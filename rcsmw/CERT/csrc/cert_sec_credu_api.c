/* ----------------------------------------------------------------------
 * %CCaseFile:	cert_sec_credu_api.c %
 * %CCaseRev:	/main/R11A/17 %
 * %CCaseDate:	2017-10-19 %
 * %CCaseDocNo: %
 * Author:      ekurnik
 *
 * ----------------------------------------------------------------------
 * %CCaseTemplateFile:  template.h %
 * %CCaseTemplateId: CCver: /main/1 %
 *
 * %CCaseCopyrightBegin%
 * Copyright (c) Ericsson AB 2017 All rights reserved.
 * 
 * The information in this document is the property of Ericsson.
 * 
 * Except as specifically authorized in writing by Ericsson, the 
 * receiver of this document shall keep the information contained 
 * herein confidential and shall protect the same in whole or in 
 * part from disclosure and dissemination to third parties.
 * 
 * Disclosure and disseminations to the receivers employees shall 
 * only be made on a strict need to know basis.
 * %CCaseCopyrightEnd%
 *
 * ----------------------------------------------------------------------
 *
 * Revision history:
 *
 * Rev     Date         Name     What
 * -----   ----------   -------- ------------------------------------------
 * R11A/1  2017-09-06   ekurnik  Created
 * R11A/2  2017-09-18   estjako  Added initialize and finalize
 * R11A/4  2017-09-18   enekdav  Added subscribe and unsubscribe
 * R11A/5  2017-09-22   enekdav  Fixed a lot of issues
 * R11A/6  2017-09-27   enekdav  Code refactoring
 * R11A/7  2017-09-28   emirbos  Fixed subscribe and unsubscribe issues
 * R11A/8  2017-09-29   eivmiha  Added NC key/cert get
 * R11A/9  2017-09-29   eivmiha  Added tcat get
 * R11A/10 2017-09-29   estjako  Added trust category subscribe and unsubscribe
 * R11A/11 2017-10-03   enatdok  Added selection_object_get and dispatch
 * R11A/12 2017-10-04   eivmiha  Minor fixes in cert_get
 * R11A/14 2017-10-06   enekdav  Minor fixes in various functions
 * R11A/15 2017-10-10   enekdav  Fixed finalize
 * R11A/16 2017-10-19   estjako  Added sec_credu_status_string
 * R11A/17 2017-10-19   enekdav  Changed INFO into DEBUG
 * ----------------------------------------------------------------------
 */

#define _GNU_SOURCE /* required to remove warning for asprintf */

#include "cert_sec_credu_api_internal.h"
#include "sec_credu_api.h"
#include "cec.h"
#define TRACEPOINT_DEFINE
#include <cert_sec_credu_trace.h>

#define INFO( fmt,...)  TRACE_HELPER(info_trace, fmt, __VA_ARGS__)
#define ERROR( fmt,...) TRACE_HELPER(error_trace, fmt, __VA_ARGS__)
#define DEBUG( fmt,...) TRACE_HELPER(debug_trace, fmt, __VA_ARGS__)

#define TRACE_HELPER(type, fmt,...) do {    \
  char *err_str;\
  asprintf(&err_str,"%s:%d:%s():" fmt, __FILE__,__LINE__, __func__, __VA_ARGS__); \
  tracepoint(com_ericsson_cert_seccredu, type , err_str);\
  free(err_str);\
  } while(0)

#define TIMEOUT                           4000
#define SIGNAL_SIZE                       4
#define FUNC_SIZE                         4
#define ID_SIZE                           4
#define RC_SIZE                           1
#define MAJ_VER_SIZE                      1
#define MIN_VER_SIZE                      1
#define NC_ID_LENGTH_SIZE                 4
#define SUBSCRIPTION_ID                   4
#define FORMAT_SIZE                       4
#define SEND_PACKET_HEADER_SIZE           SIGNAL_SIZE + FUNC_SIZE
#define VERSION_SIZE                      RC_SIZE + MAJ_VER_SIZE + MIN_VER_SIZE
#define INITIALIZE_SEND_BUFFER            SEND_PACKET_HEADER_SIZE + VERSION_SIZE
#define FINALIZE_SEND_BUFFER              SEND_PACKET_HEADER_SIZE + ID_SIZE
#define GET_SELECTION_OBJ_BUFFER          SEND_PACKET_HEADER_SIZE + ID_SIZE
#define NC_SUBSCRIBE_SEND_BUFFER          SEND_PACKET_HEADER_SIZE + ID_SIZE + NC_ID_LENGTH_SIZE
#define NC_UNSUBSCRIBE_SEND_BUFFER        SEND_PACKET_HEADER_SIZE + ID_SIZE + SUBSCRIPTION_ID
#define NC_GET_BUFFER                     SEND_PACKET_HEADER_SIZE + ID_SIZE + SUBSCRIPTION_ID + FORMAT_SIZE
#define TC_GET_BUFFER                     SEND_PACKET_HEADER_SIZE + ID_SIZE + SUBSCRIPTION_ID
#define TC_SUBSCRIBE_SEND_BUFFER          SEND_PACKET_HEADER_SIZE + ID_SIZE + SUBSCRIPTION_ID
#define TC_UNSUBSCRIBE_SEND_BUFFER        SEND_PACKET_HEADER_SIZE + ID_SIZE + SUBSCRIPTION_ID

// Func declaration
char* get_resp_signal(char *ptr, uint32_t *Data);
char* get_resp_status(char *ptr, uint32_t *Data);
char* get_recv_event(char *ptr, uint32_t *Data);
char* get_id(char *ptr, uint32_t *Data);
char* get_subscription_id(char *ptr, uint32_t *Data);
char* get_version(char *ptr, SecCreduVersion *version);
char* get_length(char *ptr, uint32_t *Data);
char* get_variable_char_data(char **Data, char *ptr, uint32_t data_length);
char* get_int_data(char *ptr, uint32_t *Data);
char* get_uint8_data(char *ptr, uint8_t *Data);
char* get_char_data(char *ptr, char *Data);

char* set_signal(char *ptr, uint32_t Data);
char* set_request_type(char *ptr, uint32_t Data);
char* set_id(char *ptr, uint32_t Data);
char* set_subscription_id(char *ptr, uint32_t Data);
char* set_version(char *ptr, SecCreduVersion *version);
char* set_new_node_credential(char *ptr, const char *ncID);
char* set_new_trust_category(char *ptr, const char *tcID );
char* set_format(char *ptr, SecCreduFormat format);
char* set_variable_char_data(char *ptr, const char *Data);
char* set_int_data(char *ptr, uint32_t Data);
char* set_uint8_data(char *ptr, uint8_t Data);

cec_handle_t *get_cec_handle(int socket);
int send_recv(cec_handle_t *cec_handle, cec_packet_t *send_packet,cec_packet_t *recv_packet);
int receive_msg(cec_handle_t *cec_handle, cec_packet_t *recv_packet);

const char*
sec_credu_status_string(
        /*IN*/ SecCreduStatus status) {
    DEBUG("ENTER: sec_credu_status_string%s", "");
    switch(status) {
    case SEC_CREDU_OK:
    	return "Operation succeeded";
    case SEC_CREDU_ERROR_UNSPECIFIED:
    	return "Operation failed: reason unspecified";
    case SEC_CREDU_ERROR_SERVICE_UNAVAILABLE:
    	return "Operation failed: service is unavailable, try again";
    case SEC_CREDU_ERROR_INVALID_PARAMETER:
        return "Operation failed: an invalid parameter was given";
    case SEC_CREDU_ERROR_UNSUPPORTED_FORMAT:
    	return "Operation failed: unsupported format was given";
    default:
    	return "";
    }
}

SecCreduStatus
sec_credu_initialize(
        /*  OUT*/ SecCreduHandle           *handle,
        /*   IN*/ const SecCreduParameters *parameters,
        /*INOUT*/ SecCreduVersion          *version) {
    DEBUG("ENTER: sec_credu_initialize%s", "");

    if (parameters == NULL) {
        DEBUG("Parameters are NULL%s!", " ");
        return SEC_CREDU_ERROR_INVALID_PARAMETER;
    }
    if (version == NULL) {
        DEBUG("Version is NULL%s!", " ");
        return SEC_CREDU_ERROR_INVALID_PARAMETER;
    }


    cec_handle_t *cec_handle;
    cec_packet_t send_packet, recv_packet;
    char buffer[INITIALIZE_SEND_BUFFER];
    char *ptr;

    uint32_t resp_signal;
    uint32_t resp_status;
    uint32_t resp_id;
    char *recv_ptr = recv_packet.data;

    send_packet.length = sizeof(buffer);
    ptr = send_packet.data = buffer;

    cec_handle = get_cec_handle(0);
    if (cec_handle == NULL) {
        ERROR("ERROR: cert_sec_credu_api could not open cec %s\n", "");
        return SEC_CREDU_ERROR_SERVICE_UNAVAILABLE;
    }

    ptr = set_signal(ptr, SEC_CREDU_REQ_SIG);
    ptr = set_request_type(ptr, INITIALIZE);
    ptr = set_version(ptr, version);

    int result = send_recv(cec_handle, &send_packet, &recv_packet);
    DEBUG("Result: %d", result);
    if (result != 0) {
        ERROR("cert_sec_credu_api could not send packet %s\n", "");
        return SEC_CREDU_ERROR_SERVICE_UNAVAILABLE;
    }
    recv_ptr = get_resp_signal(recv_packet.data, &resp_signal);
    if (resp_signal !=  SEC_CREDU_RESP_SIG) {
        free(recv_packet.data);
        ERROR("cert_sec_credu_api wrong signal received %d\n", resp_signal);
        return SEC_CREDU_ERROR_SERVICE_UNAVAILABLE;
    }
    DEBUG("Response signal: %d", resp_signal);
    recv_ptr = get_resp_status(recv_ptr, &resp_status);
    if (resp_status != OK){
        free(recv_packet.data);
        ERROR("cert_sec_credu_api wrong status received %d\n", resp_status);
        return SEC_CREDU_ERROR_SERVICE_UNAVAILABLE;
    }
    DEBUG("Response status: %d", resp_status);
    recv_ptr = get_id(recv_ptr, &resp_id);
    DEBUG("Response id: %d", resp_id);
    recv_ptr = get_version(recv_ptr, version);

    SecCreduHandleStruct *handleStruct = malloc(sizeof(SecCreduHandleStruct));
    handleStruct->ID = resp_id;
    handleStruct->handle = cec_handle;
    handleStruct->nodeCredentialCallback = parameters->nodecredential_change_callback;
    handleStruct->trustCategoryCallback = parameters->trustcategory_change_callback;
    *handle = (SecCreduHandle)handleStruct;

    free(recv_packet.data);

    return SEC_CREDU_OK;

}

SecCreduStatus
sec_credu_finalize(
        /*IN*/ SecCreduHandle handle) {
    DEBUG("ENTER: sec_credu_finalize%s", "");

    if (handle == NULL) {
        DEBUG("Handle is NULL%s!", " ");
        return SEC_CREDU_ERROR_INVALID_PARAMETER;
    }

    cec_packet_t send_packet, recv_packet;
    char buffer[FINALIZE_SEND_BUFFER];
    char *ptr;
    uint32_t resp_signal;
    uint32_t resp_status;
    char *recv_ptr = recv_packet.data;

    SecCreduHandleStruct *handleStruct = (SecCreduHandleStruct *)handle;

    send_packet.length = sizeof(buffer);
    ptr = send_packet.data = buffer;

    ptr = set_signal(ptr, SEC_CREDU_REQ_SIG);
    ptr = set_request_type(ptr, FINALIZE);
    ptr = set_id(ptr, handleStruct->ID);

    int result = send_recv(handleStruct->handle, &send_packet, &recv_packet); // 4s timeout
    if (result != 0) {
        ERROR("ERROR: cert_sec_credu_api could not send packet %s\n", "");
        return SEC_CREDU_ERROR_SERVICE_UNAVAILABLE;
    }
    recv_ptr = get_resp_signal(recv_packet.data, &resp_signal);
    if (resp_signal !=  SEC_CREDU_RESP_SIG) {
        free(recv_packet.data);
        ERROR("ERROR: cert_sec_credu_api wrong response signal %d\n", resp_signal);
        return SEC_CREDU_ERROR_SERVICE_UNAVAILABLE;
    }
    recv_ptr = get_resp_status(recv_ptr, &resp_status);
    if (resp_status != OK){
        free(recv_packet.data);
        ERROR("ERROR: cert_sec_credu_api wrong response status %d\n", resp_status);
        return SEC_CREDU_ERROR_INVALID_PARAMETER ;
    }

    cec_close(handleStruct->handle);
    free(recv_packet.data);
    return SEC_CREDU_OK;
}

SecCreduStatus
sec_credu_selectionobject_get(
        /*   IN*/ SecCreduHandle handle,
        /*INOUT*/ int *fd) {
    DEBUG("ENTER: sec_credu_selectionobject_get%s", "");

    if (handle == NULL)
    {
        DEBUG("Handle is NULL%s!", " ");
        return SEC_CREDU_ERROR_INVALID_PARAMETER;
    }

    cec_handle_t *cec_handle;
    cec_packet_t send_packet, recv_packet;
    char *ptr;
    int result;
    uint32_t resp_signal;
    uint32_t resp_status;
    int buffer_size = GET_SELECTION_OBJ_BUFFER;
    char *buffer = malloc(sizeof(char) * buffer_size);

    char *recv_ptr = recv_packet.data;

    SecCreduHandleStruct *handleStruct = (SecCreduHandleStruct *)handle;

    cec_handle = get_cec_handle(0);
    if (cec_handle == NULL)
    {
        free(buffer);
        ERROR("ERROR: cert_sec_credu_api couldn't open cec %s\n", "");
        return SEC_CREDU_ERROR_SERVICE_UNAVAILABLE;
    }

    send_packet.length = buffer_size;
    ptr = send_packet.data = buffer;

    ptr = set_signal(ptr, SEC_CREDU_REQ_SIG);
    ptr = set_request_type(ptr, SELECTION_OBJECT_GET);
    ptr = set_id(ptr, handleStruct->ID);

    result = send_recv(cec_handle, &send_packet, &recv_packet);

    if (result != 0)
    {
        free(buffer);
        ERROR("ERROR: cert_sec_credu_api could not send packet %s\n", "");
        return SEC_CREDU_ERROR_SERVICE_UNAVAILABLE;
    }

    recv_ptr = get_resp_signal(recv_packet.data, &resp_signal);
    if (resp_signal !=  SEC_CREDU_RESP_SIG)
    {
        free(buffer);
        free(recv_packet.data);
        ERROR("ERROR: cert_sec_credu_api couldn't receive packet %s\n", "");
        return SEC_CREDU_ERROR_SERVICE_UNAVAILABLE;
    }

    recv_ptr = get_resp_status(recv_ptr, &resp_status);
    if (resp_status != OK)
    {
        free(buffer);
        free(recv_packet.data);
        ERROR("ERROR: cert_sec_credu_api got error, code: %d\n", resp_status);
        return SEC_CREDU_ERROR_INVALID_PARAMETER;
    }

    *fd = cec_handle->socket;

    DEBUG("EXIT: sec_credu_selectionobject_get%s", "");
    free(buffer);
    free(recv_packet.data);
    return SEC_CREDU_OK;
}

SecCreduStatus
sec_credu_dispatch(
        /*IN*/ SecCreduHandle handle,
        /*IN*/ int fd,
        /*IN*/ SecCreduDispatchFlags flags) {
    DEBUG("ENTER: sec_credu_dispatch%s", "");

    SecCreduHandleStruct *handleStruct = (SecCreduHandleStruct *)handle;
    cec_packet_t recv_packet;
    cec_handle_t *cec_handle;
    char *recv_ptr = recv_packet.data;
    int result = 0;

    uint32_t recv_signal;
    uint32_t recv_event;
    char* recv_nc_id;
    uint32_t recv_sub_nc_id;
    char* recv_tc_id;
    uint32_t recv_sub_tc_id;
    uint32_t recv_data_length;

    if (handle == NULL)
    {
        ERROR("ERROR: Handle is NULL%s!", "");
        return SEC_CREDU_ERROR_INVALID_PARAMETER;
    }
    if (flags != SEC_CREDU_DISPATCH_ONE && flags != SEC_CREDU_DISPATCH_ALL && flags != SEC_CREDU_DISPATCH_BLOCKING)
    {
        ERROR("ERROR: Flag is not set!%s", "");
        return SEC_CREDU_ERROR_INVALID_PARAMETER;
    }
    if (fd == -1)
    {
        ERROR("ERROR: File descriptor is -1%s!", "");
        return SEC_CREDU_ERROR_INVALID_PARAMETER;
    }

    cec_handle = get_cec_handle(fd);
    if (cec_handle == NULL)
    {
        ERROR("ERROR: cert_sec_credu_api couldn't open cec %s\n", "");
        return SEC_CREDU_ERROR_SERVICE_UNAVAILABLE;
    }

    do
    {
        result = receive_msg(cec_handle, &recv_packet);
        if (result != 0)
        {
            free(recv_packet.data);
            DEBUG("ERROR: Msg receival failed %s\n", "");
        }
        else {
            recv_ptr = get_resp_signal(recv_packet.data, &recv_signal);
            if (recv_signal !=  SEC_CREDU_EVENT_SIG)
            {
                free(recv_packet.data);
                ERROR("ERROR: cert_sec_credu_api wrong event signal %d\n", recv_signal);
                return SEC_CREDU_ERROR_SERVICE_UNAVAILABLE;
            }

            recv_ptr = get_recv_event(recv_ptr, &recv_event);
            if (recv_event == NC_EVENT)
            {
                DEBUG("Event is NC_EVENT!%s", "");
                recv_ptr = get_subscription_id(recv_ptr, &recv_sub_nc_id);
                recv_ptr = get_length(recv_ptr, &recv_data_length);
                recv_ptr = get_variable_char_data(&recv_nc_id, recv_ptr, recv_data_length);
                handleStruct->nodeCredentialCallback(handle, recv_sub_nc_id, recv_nc_id);
                free(recv_nc_id);
            }
            else if (recv_event == TCAT_EVENT)
            {
                DEBUG("Event is TCAT_EVENT!%s", "");
                recv_ptr = get_subscription_id(recv_ptr, &recv_sub_tc_id);
                recv_ptr = get_length(recv_ptr, &recv_data_length);
                recv_ptr = get_variable_char_data(&recv_tc_id, recv_ptr, recv_data_length);
                handleStruct->trustCategoryCallback(handle, recv_sub_tc_id, recv_tc_id);
                free(recv_tc_id);
            }
            else if (recv_event == FINALIZE_EVENT)
            {
                DEBUG("Event is FINALIZE_EVENT!%s", "");
                free(recv_packet.data);
                cec_close(cec_handle);
                return SEC_CREDU_OK;
            }
            else
            {
                free(recv_packet.data);
                ERROR("ERROR: cert_sec_credu_api wrong event %d\n", recv_event);
                return SEC_CREDU_ERROR_SERVICE_UNAVAILABLE;
            }
        }
    } while ((flags == SEC_CREDU_DISPATCH_ALL && result == 0) || flags == SEC_CREDU_DISPATCH_BLOCKING);

    DEBUG("EXIT: sec_credu_dispatch%s", "");
    return SEC_CREDU_OK;
}

SecCreduStatus
sec_credu_nodecredential_subscribe(
        /* IN*/ SecCreduHandle       handle,
        /* IN*/ const char           *nodecredential_id,
        /*OUT*/ SecCreduSubscription *nodecredential_subscription) {
    DEBUG("ENTER: sec_credu_nodecredential_subscribe%s", "");

    if (nodecredential_id == NULL) {
        DEBUG("NodeCredential ID is NULL%s!", " ");
        return SEC_CREDU_ERROR_INVALID_PARAMETER;
    }

    if (handle == NULL) {
        DEBUG("Handle is NULL%s!", " ");
        return SEC_CREDU_ERROR_INVALID_PARAMETER;
    }

    SecCreduHandleStruct *handleStruct = (SecCreduHandleStruct *)handle;
    DEBUG("PARAMS:\nID: %d\nNC_ID: %s", handleStruct->ID, nodecredential_id);

    cec_packet_t send_packet, recv_packet;
    char *ptr;
    int ncIdlength = strlen(nodecredential_id);
    int buffer_size = NC_SUBSCRIBE_SEND_BUFFER + ncIdlength;
    char *buffer = malloc(sizeof(char) * buffer_size);

    uint32_t resp_signal;
    uint32_t resp_status;
    uint32_t resp_sub_id;
    char *recv_ptr = recv_packet.data;

    send_packet.length = buffer_size;
    ptr = send_packet.data = buffer;

    ptr = set_signal(ptr, SEC_CREDU_REQ_SIG);
    ptr = set_request_type(ptr, NC_SUBSCRIBE);
    ptr = set_id(ptr, handleStruct->ID);
    ptr = set_new_node_credential(ptr, nodecredential_id);

    int result = send_recv(handleStruct->handle, &send_packet, &recv_packet); // 4s timeout
    if (result != 0) {
        free(buffer);
        ERROR("ERROR: cert_sec_credu_api couldn't send packet %s\n", "");
        return SEC_CREDU_ERROR_SERVICE_UNAVAILABLE;
    }
    recv_ptr = get_resp_signal(recv_packet.data, &resp_signal);
    if (resp_signal !=  SEC_CREDU_RESP_SIG) {
        free(buffer);
        free(recv_packet.data);
        ERROR("ERROR: cert_sec_credu_api wrong response signal %d\n", resp_signal);
        return SEC_CREDU_ERROR_SERVICE_UNAVAILABLE;
    }
    recv_ptr = get_resp_status(recv_ptr, &resp_status);
    if (resp_status != OK) {
        free(buffer);
        free(recv_packet.data);
        ERROR("ERROR: cert_sec_credu_api got error, code: %d\n", resp_status);
        return SEC_CREDU_ERROR_INVALID_PARAMETER;
    }
    recv_ptr = get_subscription_id(recv_ptr, &resp_sub_id);
    *nodecredential_subscription = resp_sub_id;
    free(buffer);
    DEBUG("EXIT: sec_credu_nodecredential_subscribe%s", "");

    free(recv_packet.data);
    return SEC_CREDU_OK;
}

SecCreduStatus
sec_credu_nodecredential_unsubscribe(
        /*IN*/ SecCreduHandle       handle,
        /*IN*/ SecCreduSubscription nodecredential_subscription) {
    DEBUG("ENTER: sec_credu_nodecredential_unsubscribe%s", "");
    if (handle == NULL) {
        DEBUG("Handle is NULL%s!", " ");
        return SEC_CREDU_ERROR_INVALID_PARAMETER;
    }

    SecCreduHandleStruct *handleStruct = (SecCreduHandleStruct *)handle;
    DEBUG("PARAMS:\nID: %d\nNC_Sub: %d", handleStruct->ID, nodecredential_subscription);

    cec_packet_t send_packet, recv_packet;
    char *ptr;
    char buffer[NC_UNSUBSCRIBE_SEND_BUFFER];

    uint32_t resp_signal;
    uint32_t resp_status;
    char *recv_ptr = recv_packet.data;

    send_packet.length = sizeof(buffer);
    ptr = send_packet.data = buffer;

    ptr = set_signal(ptr, SEC_CREDU_REQ_SIG);
    ptr = set_request_type(ptr, NC_UNSUBSCRIBE);
    ptr = set_id(ptr, handleStruct->ID);
    ptr = set_subscription_id(ptr, nodecredential_subscription);

    int result = send_recv(handleStruct->handle, &send_packet, &recv_packet); // 4s timeout
    if (result != 0) {
        ERROR("ERROR: cert_sec_credu_api couldn't send packet %s\n", "");
        return SEC_CREDU_ERROR_SERVICE_UNAVAILABLE;
    }
    recv_ptr = get_resp_signal(recv_packet.data, &resp_signal);
    if (resp_signal !=  SEC_CREDU_RESP_SIG) {
        free(recv_packet.data);
        ERROR("ERROR: cert_sec_credu_api wrong response signal %d\n", resp_signal);
        return SEC_CREDU_ERROR_SERVICE_UNAVAILABLE;
    }
    recv_ptr = get_resp_status(recv_ptr, &resp_status);
    if (resp_status != OK) {
        free(recv_packet.data);
        ERROR("ERROR: cert_sec_credu_api got error, code: %d\n", resp_status);
        return SEC_CREDU_ERROR_INVALID_PARAMETER;
    }

    DEBUG("EXIT: sec_credu_nodecredential_unsubscribe%s", "");

    free(recv_packet.data);
    return SEC_CREDU_OK;
}

SecCreduStatus
sec_credu_nodecredential_cert_get(
        /* IN*/ SecCreduHandle       handle,
        /* IN*/ SecCreduSubscription nodecredential_subscription,
        /* IN*/ SecCreduFormat       format,
        /*OUT*/ char                 **nodecredential_data) {
    DEBUG("ENTER: sec_credu_nodecredential_cert_get%s", "");
    if (handle == NULL) {
        DEBUG("Some input parameters are NULL%s!", " ");
        return SEC_CREDU_ERROR_INVALID_PARAMETER;
    }
    if (format != SEC_CREDU_FILENAME && format != SEC_CREDU_PEM){
        DEBUG("Invalid format%s!", " ");
        return SEC_CREDU_ERROR_UNSUPPORTED_FORMAT;
    }

    cec_packet_t send_packet, recv_packet;
    char buffer[NC_GET_BUFFER];
    char *ptr;
    char *recv_ptr = recv_packet.data;
    uint32_t resp_signal;
    SecCreduResponseStatus resp_status;
    uint32_t resp_data_length;
    SecCreduHandleStruct *handleStruct = (SecCreduHandleStruct *) handle;

    send_packet.length = sizeof(buffer);
    ptr = send_packet.data = buffer;

    ptr = set_signal(ptr, SEC_CREDU_REQ_SIG);
    ptr = set_request_type(ptr, NC_CERT_GET);
    ptr = set_id(ptr, handleStruct->ID);
    ptr = set_subscription_id(ptr, nodecredential_subscription);
    ptr = set_format(ptr, format);

    int result = send_recv(handleStruct->handle, &send_packet, &recv_packet);
    if (result != 0) {
        ERROR("ERROR: cert_sec_credu_api could not send packet %s\n", "");
        return SEC_CREDU_ERROR_SERVICE_UNAVAILABLE;
    }
    recv_ptr = get_resp_signal(recv_packet.data, &resp_signal);
    if (resp_signal !=  SEC_CREDU_RESP_SIG) {
    	free(recv_packet.data);
        ERROR("ERROR: cert_sec_credu_api wrong response signal %d\n", resp_signal);
        return SEC_CREDU_ERROR_SERVICE_UNAVAILABLE;
    }
    recv_ptr = get_resp_status(recv_ptr, &resp_status);
    if (resp_status != OK){
    	free(recv_packet.data);
        ERROR("ERROR: cert_sec_credu_api wrong response status %d\n", resp_status);
        return SEC_CREDU_ERROR_INVALID_PARAMETER ;
    }

    recv_ptr = get_length(recv_ptr, &resp_data_length);
    recv_ptr = get_variable_char_data(nodecredential_data, recv_ptr, resp_data_length);

    free(recv_packet.data);
    DEBUG("EXIT: sec_credu_nodecredential_cert_get%s", "");

    return SEC_CREDU_OK;
}

SecCreduStatus
sec_credu_nodecredential_key_get(
        /* IN*/ SecCreduHandle       handle,
        /* IN*/ SecCreduSubscription nodecredential_subscription,
        /* IN*/ SecCreduFormat       format,
        /*OUT*/ char                 **nodecredential_data) {
    DEBUG("ENTER: sec_credu_nodecredential_key_get%s", "");

    if (handle == NULL) {
        DEBUG("Some input parameters are NULL%s!", " ");
        return SEC_CREDU_ERROR_INVALID_PARAMETER;
    }
    if (format != SEC_CREDU_FILENAME && format != SEC_CREDU_PEM){
        DEBUG("Invalid format%s!", " ");
        return SEC_CREDU_ERROR_UNSUPPORTED_FORMAT;
    }

    cec_packet_t send_packet, recv_packet;
    char buffer[NC_GET_BUFFER];
    char *ptr;
    uint32_t resp_signal;
    SecCreduResponseStatus resp_status;
    uint32_t resp_data_length;
    char *recv_ptr = recv_packet.data;

    SecCreduHandleStruct *handleStruct = (SecCreduHandleStruct *)handle;

    send_packet.length = sizeof(buffer);
    ptr = send_packet.data = buffer;

    ptr = set_signal(ptr, SEC_CREDU_REQ_SIG);
    ptr = set_request_type(ptr, NC_KEY_GET);
    ptr = set_id(ptr, handleStruct->ID);
    ptr = set_subscription_id(ptr, nodecredential_subscription);
    ptr = set_format(ptr, format);

    int result = send_recv(handleStruct->handle, &send_packet, &recv_packet);
    if (result != 0) {
    	ERROR("ERROR: cert_sec_credu_api could not send packet %s\n", "");
        return SEC_CREDU_ERROR_SERVICE_UNAVAILABLE;
    }
    recv_ptr = get_resp_signal(recv_packet.data, &resp_signal);
    if (resp_signal !=  SEC_CREDU_RESP_SIG) {
    	free(recv_packet.data);
        ERROR("ERROR: cert_sec_credu_api wrong response signal %d\n", resp_signal);
        return SEC_CREDU_ERROR_SERVICE_UNAVAILABLE;
    }
    recv_ptr = get_resp_status(recv_ptr, &resp_status);
    if (resp_status != OK){
    	free(recv_packet.data);
        ERROR("ERROR: cert_sec_credu_api wrong response status %d\n", resp_status);
        return SEC_CREDU_ERROR_INVALID_PARAMETER ;
    }

    recv_ptr = get_length(recv_ptr, &resp_data_length);
    recv_ptr = get_variable_char_data(nodecredential_data, recv_ptr, resp_data_length);

    free(recv_packet.data);

    DEBUG("EXIT: sec_credu_nodecredential_key_get%s", "");
    return SEC_CREDU_OK;
}

SecCreduStatus
sec_credu_trustcategory_subscribe(
        /* IN*/ SecCreduHandle       handle,
        /* IN*/ const char           *trustcategory_id,
        /*OUT*/ SecCreduSubscription *trustcategory_subscription) {
    DEBUG("ENTER: sec_credu_trustcategory_subscribe%s", "");

    if (trustcategory_id== NULL) {
        DEBUG("Trustcategroyl ID is NULL%s!", " ");
        return SEC_CREDU_ERROR_INVALID_PARAMETER;
    }

    if (handle == NULL) {
        DEBUG("Handle is NULL%s!", " ");
        return SEC_CREDU_ERROR_INVALID_PARAMETER;
    }
    SecCreduHandleStruct *handleStruct = (SecCreduHandleStruct *)handle;
    DEBUG("PARAMS:\nID: %d\nTcat_ID: %s", handleStruct->ID, trustcategory_id );

    cec_packet_t send_packet, recv_packet;
    char *ptr;
    int tcatIdlength = strlen(trustcategory_id);
    int buffer_size = TC_SUBSCRIBE_SEND_BUFFER + tcatIdlength;
    char *buffer = malloc(sizeof(char) * buffer_size);

    uint32_t resp_signal;
    uint32_t resp_status;
    uint32_t resp_sub_id;
    char *recv_ptr = recv_packet.data;

    send_packet.length = buffer_size;
    ptr = send_packet.data = buffer;

    ptr = set_signal(ptr, SEC_CREDU_REQ_SIG);
    ptr = set_request_type(ptr, TCAT_SUBSCRIBE);
    ptr = set_id(ptr, handleStruct->ID);
    ptr = set_new_trust_category(ptr, trustcategory_id);

    int result = send_recv(handleStruct->handle, &send_packet, &recv_packet); // 4s timeout
    if (result != 0) {
        free(buffer);
        ERROR("ERROR: cert_sec_credu_api couldn't send packet %s\n", "");
        return SEC_CREDU_ERROR_SERVICE_UNAVAILABLE;
    }
    recv_ptr = get_resp_signal(recv_packet.data, &resp_signal);
    if (resp_signal !=  SEC_CREDU_RESP_SIG) {
        free(buffer);
        free(recv_packet.data);
        ERROR("ERROR: cert_sec_credu_api wrong response signal %d\n", resp_signal);
        return SEC_CREDU_ERROR_SERVICE_UNAVAILABLE;
    }
    recv_ptr = get_resp_status(recv_ptr, &resp_status);
    if (resp_status != OK) {
        free(buffer);
        free(recv_packet.data);
        ERROR("ERROR: cert_sec_credu_api got error, code: %d\n", resp_status);
        return SEC_CREDU_ERROR_INVALID_PARAMETER;
    }
    recv_ptr = get_subscription_id(recv_ptr, &resp_sub_id);
   *trustcategory_subscription = resp_sub_id;
    free(buffer);

    DEBUG("EXIT: sec_credu_trustcategory_subscribe%s", "");
    return SEC_CREDU_OK;
}

SecCreduStatus
sec_credu_trustcategory_unsubscribe(
        /*IN*/ SecCreduHandle       handle,
        /*IN*/ SecCreduSubscription trustcategory_subscription) {
    DEBUG("ENTER: sec_credu_trustcategory_unsubscribe%s", "");

    if (handle == NULL) {
           DEBUG("Some input parameters are NULL%s!", " ");
           return SEC_CREDU_ERROR_INVALID_PARAMETER;
       }
    SecCreduHandleStruct *handleStruct = (SecCreduHandleStruct *)handle;
    DEBUG("PARAMS:\nID: %d\nTC_Sub: %d", handleStruct->ID, trustcategory_subscription);

    cec_packet_t send_packet, recv_packet;
    char *ptr;
    char buffer[TC_UNSUBSCRIBE_SEND_BUFFER];

    uint32_t resp_signal;
    uint32_t resp_status;
    char *recv_ptr = recv_packet.data;

    send_packet.length = sizeof(buffer);
    ptr = send_packet.data = buffer;

    ptr = set_signal(ptr, SEC_CREDU_REQ_SIG);
    ptr = set_request_type(ptr, TCAT_UNSUBSCRIBE);
    ptr = set_id(ptr, handleStruct->ID);
    ptr = set_subscription_id(ptr, trustcategory_subscription);

    int result = send_recv(handleStruct->handle, &send_packet, &recv_packet);
    if (result != 0) {
        ERROR("ERROR: cert_sec_credu_api couldn't send packet %s\n", "");
        return SEC_CREDU_ERROR_SERVICE_UNAVAILABLE;
    }
    recv_ptr = get_resp_signal(recv_packet.data, &resp_signal);
    if (resp_signal !=  SEC_CREDU_RESP_SIG) {
        free(recv_packet.data);
        ERROR("ERROR: cert_sec_credu_api wrong response signal %d\n", resp_signal);
        return SEC_CREDU_ERROR_SERVICE_UNAVAILABLE;
    }
    recv_ptr = get_resp_status(recv_ptr, &resp_status);
    if (resp_status != OK) {
        free(recv_packet.data);
        ERROR("ERROR: cert_sec_credu_api got error, code: %d\n", resp_status);
        return SEC_CREDU_ERROR_INVALID_PARAMETER;
    }

    DEBUG("EXIT:sec_credu_trustcategory_unsubscribe %s", "");
    free(recv_packet.data);
    return SEC_CREDU_OK;
}

SecCreduStatus
sec_credu_trustcategory_get(
        /* IN*/ SecCreduHandle         handle,
        /* IN*/ SecCreduSubscription   trustcategory_subscription,
        /*OUT*/ SecCreduTrustCategory  **category) {
    DEBUG("ENTER: sec_credu_trustcategory_get%s", "");

    DEBUG("ENTER: sec_credu_nodecredential_cert_get%s", "");
    if (handle == NULL) {
        DEBUG("Some input parameters are NULL%s!", " ");
        return SEC_CREDU_ERROR_INVALID_PARAMETER;
    }
    cec_packet_t send_packet, recv_packet;
    char buffer[TC_GET_BUFFER];
    char *ptr;
    uint32_t resp_signal;
    SecCreduResponseStatus resp_status;
    char *recv_ptr = recv_packet.data;
    uint32_t resp_tcat_count;
    uint32_t resp_tcat_dir_length;
    uint32_t resp_data_length;
    uint32_t resp_id_length;
    uint32_t resp_tc_filename_length;
    uint32_t resp_tc_cert_length;
    char *tc_id, *filename, *pem_content, *dirname;
    SecCreduHandleStruct *handleStruct = (SecCreduHandleStruct *)handle;

    send_packet.length = sizeof(buffer);
    ptr = send_packet.data = buffer;

    ptr = set_signal(ptr, SEC_CREDU_REQ_SIG);
    ptr = set_request_type(ptr, TCAT_GET);
    ptr = set_id(ptr, handleStruct->ID);
    ptr = set_subscription_id(ptr, trustcategory_subscription);

    int result = send_recv(handleStruct->handle, &send_packet, &recv_packet);

    if (result != 0) {
    	ERROR("ERROR: cert_sec_credu_api could not send packet %s\n", "");
        return SEC_CREDU_ERROR_SERVICE_UNAVAILABLE;
    }
    recv_ptr = get_resp_signal(recv_packet.data, &resp_signal);
    if (resp_signal !=  SEC_CREDU_RESP_SIG) {
    	free(recv_packet.data);
        ERROR("ERROR: cert_sec_credu_api wrong response signal %d\n", resp_signal);
        return SEC_CREDU_ERROR_SERVICE_UNAVAILABLE;
    }
    recv_ptr = get_resp_status(recv_ptr, &resp_status);
    if (resp_status == ERROR_TCAT_EMPTY) {
    	free(recv_packet.data);
        ERROR("ERROR: cert_sec_credu_api wrong response status %d\n", resp_status);
        return SEC_CREDU_ERROR_UNSPECIFIED;
    }
    if (resp_status != OK) {
    	free(recv_packet.data);
        ERROR("ERROR: cert_sec_credu_api wrong response status %d\n", resp_status);
        return SEC_CREDU_ERROR_INVALID_PARAMETER ;
    }

    recv_ptr = get_length(recv_ptr, &resp_tcat_count);
    recv_ptr = get_length(recv_ptr, &resp_tcat_dir_length);

    SecCreduTrustCategory *tmpCategory = malloc(sizeof(SecCreduTrustCategory));
    recv_ptr = get_variable_char_data(&dirname, recv_ptr, resp_tcat_dir_length);
    tmpCategory->dirname = dirname;
    tmpCategory->trustedCertificateCount = resp_tcat_count;
    tmpCategory->trustedCertificates = malloc(resp_tcat_count * sizeof(SecCreduTrustedCertificate *));

    for(unsigned int i = 0; i < resp_tcat_count; ++i){
    	tmpCategory->trustedCertificates[i] = malloc(sizeof(SecCreduTrustedCertificate));
    	recv_ptr = get_length(recv_ptr, &resp_data_length);

    	recv_ptr = get_length(recv_ptr, &resp_id_length);
    	recv_ptr = get_variable_char_data(&tc_id, recv_ptr, resp_id_length);
    	tmpCategory->trustedCertificates[i]->id = tc_id;

    	recv_ptr = get_length(recv_ptr, &resp_tc_filename_length);
    	recv_ptr = get_variable_char_data(&filename, recv_ptr, resp_tc_filename_length);
    	tmpCategory->trustedCertificates[i]->filename = filename;

    	recv_ptr = get_length(recv_ptr, &resp_tc_cert_length);
    	recv_ptr = get_variable_char_data(&pem_content, recv_ptr, resp_tc_cert_length);
    	tmpCategory->trustedCertificates[i]->pemContent = pem_content;
    }

    free(recv_packet.data);
    *category = tmpCategory;

    DEBUG("EXIT: sec_credu_trustcategory_get%s", "");
    return SEC_CREDU_OK;

}

SecCreduStatus
sec_credu_trustcategory_free(
        /* IN*/ SecCreduHandle        handle,
        /*OUT*/ SecCreduTrustCategory **category) {
    DEBUG("ENTER: sec_credu_trustcategory_free%s", "");

    SecCreduTrustCategory *tmpCategory = *category;

    if (handle == NULL) {
            DEBUG("Some input parameters are NULL%s!", " ");
            return SEC_CREDU_ERROR_INVALID_PARAMETER;
        }

    for (unsigned int i = 0; i < tmpCategory->trustedCertificateCount; ++i) {
        free(tmpCategory->trustedCertificates[i]->id);
        free(tmpCategory->trustedCertificates[i]->filename);
        free(tmpCategory->trustedCertificates[i]->pemContent);
        free(tmpCategory->trustedCertificates[i]);
    }

    free(tmpCategory->dirname);
    free(tmpCategory);
    category = NULL;

    DEBUG("EXIT: sec_credu_trustcategory_free%s", "");
    return SEC_CREDU_OK;
}

SecCreduStatus
sec_credu_trustcategory_dirname_get(
        /* IN*/ SecCreduTrustCategory *category,
        /*OUT*/ char                  **dirname) {
    DEBUG("ENTER: sec_credu_trustcategory_dirname_get%s", "");

    if (category == NULL) {
        DEBUG("Some input parameters are NULL%s!", " ");
        return SEC_CREDU_ERROR_INVALID_PARAMETER;
    }

    get_variable_char_data(dirname, category->dirname, strlen(category->dirname));

    DEBUG("EXIT: sec_credu_trustcategory_dirname_get%s", "");
    return SEC_CREDU_OK;
}

SecCreduStatus
sec_credu_trustcategory_cert_count_get(
        /* IN*/ SecCreduTrustCategory *category,
        /*OUT*/ size_t                *trustedcertificate_count) {
    DEBUG("ENTER: sec_credu_trustcategory_cert_count_get%s", "");

    if (category == NULL) {
        DEBUG("Some input parameters are NULL%s!", " ");
        return SEC_CREDU_ERROR_INVALID_PARAMETER;
    }

    *trustedcertificate_count = category->trustedCertificateCount;

    DEBUG("EXIT: sec_credu_trustcategory_cert_count_get%s", "");
    return SEC_CREDU_OK;
}

SecCreduStatus
sec_credu_trustcategory_cert_get(
        /* IN*/ SecCreduTrustCategory *category,
        /* IN*/ SecCreduFormat        format,
        /* IN*/ size_t                index,
        /*OUT*/ char                  **trustedcertificate_data,
        /*OUT*/ char                  **trustedcertificate_id) {
    DEBUG("ENTER: sec_credu_trustcategory_cert_get%s", "");

    if (category == NULL) {
        DEBUG("Some input parameters are NULL%s!", " ");
        return SEC_CREDU_ERROR_INVALID_PARAMETER;
    }

    if(format != SEC_CREDU_FILENAME && format != SEC_CREDU_PEM){
        DEBUG("Invalid format%s!", " ");
        return SEC_CREDU_ERROR_UNSUPPORTED_FORMAT;
    }
    if(index > category->trustedCertificateCount){
        DEBUG("Invalid index%s!", " ");
        return SEC_CREDU_ERROR_INVALID_PARAMETER;
    }

    if(trustedcertificate_id != NULL){
        *trustedcertificate_id = malloc(sizeof(char) * strlen(category->trustedCertificates[index]->id));
        *trustedcertificate_id = strcpy(*trustedcertificate_id, category->trustedCertificates[index]->id);
    }

    if (format == SEC_CREDU_FILENAME && trustedcertificate_data != NULL) {
	   *trustedcertificate_data = malloc(sizeof(char) * strlen(category->trustedCertificates[index]->filename));
	   *trustedcertificate_data = strcpy(*trustedcertificate_data, category->trustedCertificates[index]->filename);
    } else if (format == SEC_CREDU_PEM && trustedcertificate_data != NULL) {
	   *trustedcertificate_data = malloc(sizeof(char) * strlen(category->trustedCertificates[index]->pemContent));
	   *trustedcertificate_data = strcpy(*trustedcertificate_data, category->trustedCertificates[index]->pemContent);
    }

    DEBUG("EXIT: sec_credu_trustcategory_cert_get%s", "");
    return SEC_CREDU_OK;
}

/********************************************************************
 * Internal functions
 ********************************************************************/

// if there is no active socket, open new one
// else just create handle and reuse the socket
cec_handle_t *get_cec_handle(int socket) {
    if (socket == 0) {
        // open new socket
        DEBUG("CEC_HANDLE: Opening new socket%s", "");
        return cec_open((void *)CEC_REGISTERED_NAME, strlen(CEC_REGISTERED_NAME));
    } else {
        // socket already exists, return handle
        DEBUG("CEC_HANDLE: Reusing existing socket%s", "");
        cec_handle_t *handle = malloc(sizeof(*handle));
        handle->socket = socket;
        return handle;
    }
}

// free handle without closing the socket
void free_cec_handle(cec_handle_t *handle) {
    DEBUG("CEC_HANDLE: Freeing cec_handle%s", "");
    free(handle);
}

// free handle and close the socket
void close_cec_handle(cec_handle_t *handle) {
    DEBUG("CEC_HANDLE: Closing cec_handle%s", "");
    cec_close(handle);
}

int send_recv(cec_handle_t *cec_handle,
	                     cec_packet_t *send_packet,
	                     cec_packet_t *recv_packet)
{
    if (cec_send_with_pid(cec_handle, send_packet) == -1) {
        ERROR("Cec_send_with_pid failed%s", "");
        return -1;
    }

    if (cec_receive_w_tmeout(cec_handle, recv_packet, TIMEOUT) == -1) {
        ERROR("Cec_receive_w_tmeout failed%s", "");
        return -1;
    }

    return 0;
}

int receive_msg(cec_handle_t *cec_handle, cec_packet_t *recv_packet)
{
    if (cec_receive(cec_handle, recv_packet) == -1)
    {
        ERROR("cec_receive failed%s", "");
        return -1;
    }
    return 0;
}


// Helper funs for get
char* get_resp_signal(char *ptr, uint32_t *Data) {
	return get_int_data(ptr, Data);
}
char* get_resp_status(char *ptr, uint32_t *Data) {
	return get_int_data(ptr, Data);
}

char* get_recv_event(char *ptr, uint32_t *Data) {
    return get_int_data(ptr, Data);
}

char* get_id(char *ptr, uint32_t *Data) {
	return get_int_data(ptr, Data);
}
char* get_subscription_id(char *ptr, uint32_t *Data) {
	return get_int_data(ptr, Data);
}
char* get_version(char *ptr, SecCreduVersion *version) {
	ptr = get_char_data(ptr, &version->release_code);
	ptr = get_uint8_data(ptr, &version->major_version);
	return get_uint8_data(ptr, &version->minor_version);
}
char* get_length(char *ptr, uint32_t *Data) {
	return get_int_data(ptr, Data);
}
char* get_variable_char_data(char ** Data, char *ptr, uint32_t data_length){
	*Data  = malloc((data_length+1)*sizeof(char));
	memset(*Data, 0, data_length + 1);
	memcpy(*Data , ptr, data_length);
	return ptr + data_length;
}
char* get_int_data(char *ptr, uint32_t *Data) {
	*Data = *(uint32_t*)ptr;
	ptr+=4;
	return ptr;
}
char* get_uint8_data(char *ptr, uint8_t *Data) {
	*Data = *(uint8_t*)ptr;
	ptr+=1;
	return ptr;
}
char* get_char_data(char *ptr, char *Data) {
	*Data = *ptr;
	ptr+=1;
	return ptr;
}

// Helper funs for set
char* set_signal(char *ptr, uint32_t Data) {
	return set_int_data(ptr, Data);
}
char* set_request_type(char *ptr, uint32_t Data) {
	return set_int_data(ptr, Data);
}
char* set_id(char *ptr, uint32_t Data) {
	return set_int_data(ptr, Data);
}
char* set_subscription_id(char *ptr, uint32_t Data) {
	return set_int_data(ptr, Data);
}
char* set_version(char *ptr, SecCreduVersion *version) {
	ptr = set_uint8_data(ptr, version->release_code);
	ptr = set_uint8_data(ptr, version->major_version);
	return set_uint8_data(ptr, version->minor_version);
}
char* set_new_node_credential(char *ptr, const char *ncID ) {
	ptr = set_int_data(ptr, strlen(ncID));
	return set_variable_char_data(ptr, ncID);
}
char* set_new_trust_category(char *ptr, const char *tcID ) {
	ptr = set_int_data(ptr, strlen(tcID));
	return set_variable_char_data(ptr, tcID);
}
char* set_variable_char_data(char *ptr, const char *Data) {
	int data_length = strlen(Data);
	ptr = memcpy(ptr, Data, data_length);
	ptr += data_length;
	return ptr;
}
char* set_format(char *ptr, SecCreduFormat Data){
	return set_int_data(ptr, Data);
}
char* set_int_data(char *ptr, uint32_t Data) {
	*(uint32_t*)ptr = Data;
	ptr+=4;
	return ptr;
}
char* set_uint8_data(char *ptr, uint8_t Data) {
	*(uint8_t*)ptr = Data;
	ptr+=1;
	return ptr;
}
