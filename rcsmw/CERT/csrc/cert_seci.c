/* ----------------------------------------------------------------------
 * %CCaseFile:	cert_seci.c %
 * %CCaseRev:	/main/R3A/R4A/R5A/R6A/R7A/4 %
 * %CCaseDate:	2016-10-12 %
 * %CCaseDocNo: %
 * Author:      ehsake
 *
 * ----------------------------------------------------------------------
 * %CCaseTemplateFile:  template.h %
 * %CCaseTemplateId: CCver: /main/1 %
 *
 * %CCaseCopyrightBegin%
 * Copyright (c) Ericsson AB 2014-2016 All rights reserved.
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
 * Rev     Date       Name     What
 * -----   ---------- -------- ------------------------------------------
 *         2016-01-25 ehsake   SECI update for eIDL encryption
 *         2016-07-13 enenteo  Coverity warnings updates
 * R7A/1   2016-09-27 etxasta  Add seci_get_vcert, seci_verify_peer_vc,
 *                             seci_encode and seci_decode
 * ----------------------------------------------------------------------
 */

#define _GNU_SOURCE /* required to remove warning for asprintf */


#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "cec.h"
#include "cert_seci.h"
#define TRACEPOINT_DEFINE
#include <cert_trace.h>
#include <cert_file_vc.h>
#include <cert_crc.h>
#include <cert_vc.h>

#define SIGNATURE {'S', 'E', 'C', 'I'};
#define CEC_REC_TMO 10000

#define SECI_GET_CERT        1
#define SECI_READ            2
#define SECI_WRITE           3
#define SECI_DELETE          4
#define SECI_LOG             5
#define SECI_SUB_INIT        6
#define SECI_ADD_SUB         7
#define SECI_DEL_SUB         8
#define SECI_GET_SUB_EVENT   9
/* SECI_GET_VC 10 */
#define SECI_GET_NC         11
#define SECI_GET_TCAT       12
#define SECI_VERIFY_PEER    13
#define SECI_GET_VCERT      14
#define SECI_VERIFY_PEER_VC 15
#define SECI_ENCODE         16
#define SECI_DECODE         17

/* checksum of well-known secret, generated with crc.h */
#define SECI_SECRET_CHECKSUM 1067826222


typedef struct
{
  cec_handle_t *handle;
} SeciProxyData;

static SeciProxyData* init_conn();
static SeciResultT send_recv(SeciProxyData *seciProxyP,
			     cec_packet_t *send_packet,
			     cec_packet_t *recv_packet,
                             int timeout);
static void term_conn(SeciProxyData *seciProxyP);

/* global connection information, only ONE connection is supported */
static SeciProxyData* theProxy = NULL;


static SeciResultT check_secret(char* secret);
static SeciResultT get_string_from_packet(cec_packet_t* recv_packet, char** resultP);

SeciResultT seci_get_cert(char *dnP, char **resultP) {
    DEBUG("seci_get_cert%s", "");
    SeciProxyData *seciProxyP = NULL;
    cec_packet_t send_packet, recv_packet;
    int dnSize = strlen(dnP) + 1;
    int send_length;
    char *ptr;
    int requestType  = SECI_GET_CERT;
    int result;
    
    /* resultP should be NULL */
    if (resultP == NULL || *resultP != NULL) {
        INFO("Parameter resultP must be NULL%s", "");
        return SECI_INVALID_PARAMETER_RESULTP;
    }
    seciProxyP = init_conn();
    /* Build the send signal */
    send_length = dnSize + 2*sizeof(uint32_t);
    ptr = send_packet.data = malloc(send_length);
    send_packet.length = send_length;
    *(uint32_t*)ptr = requestType;
    ptr+=4;
    *(uint32_t*)ptr = dnSize;
    ptr+=4;
    strcpy(ptr, dnP);
    /* Send the request and wait for the translated reply */
    result = send_recv(seciProxyP, &send_packet, &recv_packet, 43000);
    if (result == SECI_OK) {
        /* OK. Copy the result to result parameter */
        *resultP = malloc(recv_packet.length);
        result = *((int *)recv_packet.data);
        memcpy(*resultP, ((char *)recv_packet.data)+4, recv_packet.length-4);
        free(recv_packet.data);
        DEBUG("RESULT:\n  result  = %d data = %s\n", result,*resultP);
    }
    free(send_packet.data);
    /* Terminate the connection if there was no connection before */
    term_conn(seciProxyP);
    DEBUG("RESULT:\n  result  = %d \n", result);
    return result;
}

SeciResultT seci_get_vcert(char **resultP) {
    DEBUG("seci_get_vcert%s", "");
    SeciProxyData *seciProxyP = NULL;
    cec_packet_t send_packet, recv_packet;
    int send_length;
    char *ptr;
    int requestType  = SECI_GET_VCERT;
    int result;
    /* resultP should be NULL */
    if (resultP == NULL || *resultP != NULL) {
        INFO("Parameter resultP must be NULL%s", "");
        return SECI_INVALID_PARAMETER_RESULTP;
    }
    seciProxyP = init_conn();
    /* Build the send signal */
    send_length = sizeof(uint32_t);
    ptr = send_packet.data = malloc(send_length);
    send_packet.length = send_length;
    *(uint32_t*)ptr = requestType;
    /* Send the request and wait for the translated reply */
    result = send_recv(seciProxyP, &send_packet, &recv_packet, 43000);
    if (result == SECI_OK) {
        result = get_string_from_packet(&recv_packet,resultP);
        free(recv_packet.data);
        DEBUG("RESULT:\n  result  = %d data = %s\n", result,*resultP);
    }
    free(send_packet.data);
    /* Terminate the connection if there was no connection before */
    term_conn(seciProxyP);
    DEBUG("RESULT:\n  result  = %d \n", result);
    return result;
}

SeciResultT
seci_get_vc(char* secretP, char** resultP) {

  SeciResultT result;

  DEBUG("seci_get_vc%s", "");

  if (resultP == NULL || *resultP != NULL) {
         DEBUG("Parameter resultP must be NULL%s", "");
         return SECI_INVALID_PARAMETER_RESULTP;
     }

  if(check_secret(secretP) == SECI_ERROR)
  {
    result = SECI_ERROR;
  }
  else
  {
#ifdef SIM_VC
    result = get_vc_from_file(resultP);
#else
    result = get_vc(resultP);
#endif

  }

  DEBUG("RESULT:\n  result  = %d \n", result);
  return result;
}

SeciResultT seci_get_nc(char *dnP, char **resultP) {
   DEBUG("seci_get_nc%s", "");
    SeciProxyData *seciProxyP = NULL;
    cec_packet_t send_packet, recv_packet;
    int dnSize = strlen(dnP) + 1;
    int send_length;
    char *ptr;
    int requestType  = SECI_GET_NC;
    int result;
    
    /* resultP should be NULL */
    if (resultP == NULL || *resultP != NULL) {
        INFO("Parameter resultP must be NULL%s", "");
        return SECI_INVALID_PARAMETER_RESULTP;
    }
    seciProxyP = init_conn();
    /* Build the send signal */
    send_length = dnSize + 2*sizeof(uint32_t);
    ptr = send_packet.data = malloc(send_length);
    send_packet.length = send_length;
    *(uint32_t*)ptr = requestType;
    ptr+=4;
    *(uint32_t*)ptr = dnSize;
    ptr+=4;
    strcpy(ptr, dnP);
    /* Send the request and wait for the translated reply */
    result = send_recv(seciProxyP, &send_packet, &recv_packet, 43000);
    if (result == SECI_OK) {
        /* OK. Copy the result to result parameter */
        *resultP = malloc(recv_packet.length);
        result = *((int *)recv_packet.data);
        memcpy(*resultP, ((char *)recv_packet.data)+4, recv_packet.length-4);
        free(recv_packet.data);
        DEBUG("RESULT:\n  result  = %d data = %s\n", result,*resultP);
    }
    free(send_packet.data);
    /* Terminate the connection if there was no connection before */
    term_conn(seciProxyP);
    DEBUG("RESULT:\n  result  = %d \n", result);
    return result;
}

SeciResultT seci_get_tcat(char *dnP, char **resultP) {
    DEBUG("seci_get_tcat%s", "");
    SeciProxyData *seciProxyP = NULL;
    cec_packet_t send_packet, recv_packet;
    int dnSize = strlen(dnP) + 1;
    int send_length;
    char *ptr;
    int requestType  = SECI_GET_TCAT;
    int result;
    
    /* resultP should be NULL */
    if (resultP == NULL || *resultP != NULL) {
        INFO("Parameter resultP must be NULL%s", "");
        return SECI_INVALID_PARAMETER_RESULTP;
    }
    seciProxyP = init_conn();
    /* Build the send signal */
    send_length = dnSize + 2*sizeof(uint32_t);
    ptr = send_packet.data = malloc(send_length);
    send_packet.length = send_length;
    *(uint32_t*)ptr = requestType;
    ptr+=4;
    *(uint32_t*)ptr = dnSize;
    ptr+=4;
    strcpy(ptr, dnP);
    /* Send the request and wait for the translated reply */
    result = send_recv(seciProxyP, &send_packet, &recv_packet, 43000);
    if (result == SECI_OK) {
        /* OK. Copy the result to result parameter */
        *resultP = malloc(recv_packet.length);
        result = *((int *)recv_packet.data);
        memcpy(*resultP, ((char *)recv_packet.data)+4, recv_packet.length-4);
        free(recv_packet.data);
        DEBUG("RESULT:\n  result  = %d data = %s\n", result,*resultP);
    }
    free(send_packet.data);
    /* Terminate the connection if there was no connection before */
    term_conn(seciProxyP);
    DEBUG("RESULT:\n  result  = %d \n", result);
    return result;
}

SeciResultT seci_read(char *idP, char *indexP, char **resultP) {
    DEBUG("seci_read%s", "");
    SeciProxyData *seciProxyP = NULL;
    cec_packet_t send_packet, recv_packet;
    int idSize = strlen(idP) + 1;
    int indexSize = strlen(indexP) + 1;
    int send_length;
    char *ptr;
    int requestType  = SECI_READ;
    int result;
    /* resultP should be NULL */
    if (resultP == NULL || *resultP != NULL) {
        INFO("Parameter resultP must be NULL%s", "");
        return SECI_INVALID_PARAMETER_RESULTP;
    }
    seciProxyP = init_conn();
    /* Build the send signal */
    send_length = idSize + indexSize + 3*sizeof(uint32_t);
    ptr = send_packet.data = malloc(send_length);
    send_packet.length = send_length;
    *(uint32_t*)ptr = requestType;
    ptr+=4;
    *(uint32_t*)ptr = idSize;
    ptr+=4;
    strcpy(ptr, idP);
    ptr+=idSize;
    *(uint32_t*)ptr = indexSize;
    ptr+=4;
    strcpy(ptr, indexP);
    /* Send the request and wait for the translated reply */
    result = send_recv(seciProxyP, &send_packet, &recv_packet, 43000);
    if (result == SECI_OK) {
        /* OK. Copy the result to result parameter */
        *resultP = malloc(recv_packet.length);
        result = *((int *)recv_packet.data);
        //strcpy(*resultP, ((char *)recv_packet.data)+4);
        memcpy(*resultP, ((char *)recv_packet.data)+4, recv_packet.length-4);
        free(recv_packet.data);
    }
    free(send_packet.data);
    /* Terminate the connection if there was no connection before */
    term_conn(seciProxyP);
    DEBUG("RESULT:\n  result  = %d \n", result);
    return result;
}

SeciResultT seci_write(char *idP, char *indexP, char *dataP) {
    DEBUG("seci_write%s", "");
    SeciProxyData *seciProxyP = NULL;
    cec_packet_t send_packet, recv_packet;
    uint32_t idSize = strlen(idP) + 1;
    uint32_t indexSize = strlen(indexP) + 1;
    //uint32_t dataSize = 53;  
    uint32_t dataSize = *(uint32_t*)dataP;  
    int send_length;
    char *ptr;
    int requestType  = SECI_WRITE;
    int result;
    seciProxyP = init_conn();
    /* Build the send signal */
    send_length = idSize + indexSize + dataSize + 4*sizeof(uint32_t);
    ptr = send_packet.data = malloc(send_length);
    send_packet.length = send_length;
    *(int*)ptr = requestType;
    ptr+=4;
    *(uint32_t*)ptr = idSize;
    ptr+=4;
    strcpy(ptr, idP);
    ptr+=idSize;
    *(uint32_t*)ptr = indexSize;
    ptr+=4;
    strcpy(ptr, indexP);
    ptr+=indexSize;
    memcpy(ptr, ((char *)dataP), dataSize+4);
    /* Send the request and wait for the translated reply */
    result = send_recv(seciProxyP, &send_packet, &recv_packet, 43000); 
    if (result == SECI_OK)
        free(recv_packet.data);
    free(send_packet.data);
    /* Terminate the connection if there was no connection before */
    term_conn(seciProxyP);
    DEBUG("RESULT:\n  result  = %d\n", result);
    return result;
}

SeciResultT seci_delete(char *idP, char *indexP) {
    DEBUG("seci_delete%s", "");
    SeciProxyData *seciProxyP = NULL;
    cec_packet_t send_packet, recv_packet;
    int idSize = strlen(idP) + 1;
    int indexSize = strlen(indexP) + 1;
    int send_length;
    char *ptr;
    int requestType  = SECI_DELETE;
    int result;
    seciProxyP = init_conn();
    /* Build the send signal */
    send_length = idSize + indexSize + 3*sizeof(uint32_t);
    ptr = send_packet.data = malloc(send_length);
    send_packet.length = send_length;
    *(uint32_t*)ptr = requestType;
    ptr+=4;
    *(uint32_t*)ptr = idSize;
    ptr+=4;
    strcpy(ptr, idP);
    ptr+=idSize;
    *(uint32_t*)ptr = indexSize;
    ptr+=4;
    strcpy(ptr, indexP);
    /* Send the request and wait for the translated reply */
    result = send_recv(seciProxyP, &send_packet, &recv_packet, 43000);
    if (result == SECI_OK)
        free(recv_packet.data);
    free(send_packet.data);
    /* Terminate the connection if there was no connection before */
    term_conn(seciProxyP);
    DEBUG("RESULT:\n  result  = %d\n", result);
    return result;
}

SeciResultT seci_log(SeciLogTypeT type, int facility,
        SeciSeverityT severity, char *messageP) {
    DEBUG("seci_log%s", "");
    SeciProxyData *seciProxyP = NULL;
    cec_packet_t send_packet, recv_packet;
    int messageSize = strlen(messageP) + 1;
    int send_length;
    char *ptr;
    int requestType  = SECI_LOG;
    int result;
    seciProxyP = init_conn();
    /* Build the send signal */
    send_length = messageSize + 5*sizeof(uint32_t);
    ptr = send_packet.data = malloc(send_length);
    send_packet.length = send_length;
    *(uint32_t*)ptr = requestType;
    ptr+=4;
    *(uint32_t*)ptr = (uint32_t)type;
    DEBUG("type = %d\n", (uint32_t)type);
    ptr+=4;
    *(uint32_t*)ptr = (uint32_t)facility;
    DEBUG("facility = %d\n", (uint32_t)facility);
    ptr+=4;
    *(uint32_t*)ptr = (uint32_t)severity;
    DEBUG("severity = %d\n", (uint32_t)severity);
    ptr+=4;
    *(uint32_t*)ptr = messageSize;
    ptr+=4;
    strcpy(ptr, messageP);
    /* Send the request and wait for the translated reply */
    result = send_recv(seciProxyP, &send_packet, &recv_packet, 43000);
    if (result == SECI_OK)
        free(recv_packet.data);
    free(send_packet.data);
    /* Terminate the connection if there was no connection before */
    term_conn(seciProxyP);
    DEBUG("RESULT:\n  result  = %d\n", result);
    return result;
}

SeciHandleT
seci_sub_initialize(char *idP) {
    DEBUG("seci_sub_initialize%s", "");
    SeciProxyData *seciProxyP = NULL;
    cec_packet_t send_packet, recv_packet;
    int idSize = strlen(idP) + 1;
    int send_length;
    char *ptr;
    int requestType  = SECI_SUB_INIT;
    int result;
    if(theProxy){
	INFO("seci: Already initialized sub API%s", "");
	term_conn(theProxy);
	theProxy = NULL;
    }
    seciProxyP = init_conn();
    /* Build the send signal */
    send_length = idSize + 2*sizeof(uint32_t);
    ptr = send_packet.data = malloc(send_length);
    send_packet.length = send_length;
    *(uint32_t*)ptr = requestType;
    ptr+=4;
    *(uint32_t*)ptr = idSize;
    ptr+=4;
    strcpy(ptr, idP);
    /* Send the request and wait for the translated reply */
    result = send_recv(seciProxyP, &send_packet, &recv_packet, 43000);
    free(send_packet.data);
    /* Keep the connection up for polling on */
    if (result == SECI_OK) {
        theProxy = seciProxyP;
        free(recv_packet.data);
        DEBUG("RESULT:\n  handle = %d\n", theProxy->handle->socket);
        return theProxy->handle->socket;
    } else {
      DEBUG("RESULT:\n  result  = %d\n", result);
      term_conn(seciProxyP);
      return -1;
    }
}

SeciResultT seci_add_sub(char *idP, char *dnP) {
    DEBUG("seci_add_sub%s", "");
    SeciProxyData *seciProxyP = NULL;
    cec_packet_t send_packet, recv_packet;
    int idSize = strlen(idP) + 1;
    int dnSize = strlen(dnP) + 1;
    int send_length;
    char *ptr;
    int requestType  = SECI_ADD_SUB;
    int result;
    seciProxyP = init_conn();
    /* Build the send signal */
    send_length = idSize + dnSize + 3*sizeof(uint32_t);
    ptr = send_packet.data = malloc(send_length);
    send_packet.length = send_length;
    *(uint32_t*)ptr = requestType;
    ptr+=4;
    *(uint32_t*)ptr = idSize;
    ptr+=4;
    strcpy(ptr, idP);
    ptr+=idSize;
    *(uint32_t*)ptr = dnSize;
    ptr+=4;
    strcpy(ptr, dnP);
    /* Send the request and wait for the translated reply */
    result = send_recv(seciProxyP, &send_packet, &recv_packet, 43000);
    if (result == SECI_OK)
        free(recv_packet.data);
    free(send_packet.data);
    /* Terminate the connection if there was no connection before */
    term_conn(seciProxyP);
    DEBUG("RESULT:\n  result  = %d\n", result);
    return result;
}

SeciResultT seci_del_sub(char *idP, char *dnP) {
    DEBUG("seci_del_sub%s", "");
    SeciProxyData *seciProxyP = NULL;
    cec_packet_t send_packet, recv_packet;
    int idSize = strlen(idP) + 1;
    int dnSize = strlen(dnP) + 1;
    int send_length;
    char *ptr;
    int requestType  = SECI_DEL_SUB;
    int result;
    seciProxyP = init_conn();
    /* Build the send signal */
    send_length = idSize + dnSize + 3*sizeof(uint32_t);
    ptr = send_packet.data = malloc(send_length);
    send_packet.length = send_length;
    *(uint32_t*)ptr = requestType;
    ptr+=4;
    *(uint32_t*)ptr = idSize;
    ptr+=4;
    strcpy(ptr, idP);
    ptr+=idSize;
    *(uint32_t*)ptr = dnSize;
    ptr+=4;
    strcpy(ptr, dnP);
    /* Send the request and wait for the translated reply */
    result = send_recv(seciProxyP, &send_packet, &recv_packet, 43000);
    if (result == SECI_OK)
        free(recv_packet.data);
    free(send_packet.data);
    /* Terminate the connection if there was no connection before */
    term_conn(seciProxyP);
    DEBUG("RESULT:\n  result  = %d\n", result);
    return result;
}

SeciResultT seci_get_sub_event(char *idP, char **resultP) {
    DEBUG("seci_get_sub_event%s", "");
    SeciProxyData *seciProxyP = NULL;
    cec_packet_t send_packet, recv_packet, packet;
    int idSize = strlen(idP) + 1;
    int send_length;
    char *ptr;
    int requestType  = SECI_GET_SUB_EVENT;
    int result;
    /* resultP should be NULL */
    if (resultP == NULL || *resultP != NULL) {
        INFO("Parameter resultP must be NULL%s", "");
        return SECI_INVALID_PARAMETER_RESULTP;
    }
    if(!theProxy){
	INFO("seci: Error, dispatch called on uninitialized sub API%s", "");
	return SECI_UNINITIALIZED_SUB;
    }
    memset(&packet, 0, sizeof packet);
    if(cec_receive_w_tmeout(theProxy->handle, &packet, CEC_REC_TMO) == -1) {
	INFO("seci: Error, timeout reading from CEC handle%s", "");
	term_conn(theProxy);
	theProxy=NULL;
        if(packet.data) {
            free(packet.data);
        }
        return SECI_RECEIVE_ERROR;
    }
    if(packet.data) {
        free(packet.data);
    }

    seciProxyP = init_conn();

    /* Build the send signal */
    send_length = idSize + 2*sizeof(uint32_t);
    ptr = send_packet.data = malloc(send_length);
    send_packet.length = send_length;
    *(uint32_t*)ptr = requestType;
    ptr+=4;
    *(uint32_t*)ptr = idSize;
    ptr+=4;
    strcpy(ptr, idP);
    /* Send the request and wait for the translated reply */
    result = send_recv(seciProxyP, &send_packet, &recv_packet, 43000);
    if (result == SECI_OK) {
        /* OK. Copy the result to result parameter */
        *resultP = malloc(recv_packet.length);
        result = *((int *)recv_packet.data);
        memcpy(*resultP, ((char *)recv_packet.data)+4, recv_packet.length-4);
        free(recv_packet.data);
    } 
    free(send_packet.data);
    term_conn(seciProxyP);
    /* Keep the connection up for polling */
    DEBUG("RESULT:\n  result  = %d \n", result);
    return result;
}

SeciResultT seci_verify_peer(char *dnTcatP, char *peerP) {
    DEBUG("seci_verify_peer%s", "");
    SeciProxyData *seciProxyP = NULL;
    cec_packet_t send_packet, recv_packet;
    uint32_t dnTcatSize = strlen(dnTcatP) + 1;
    uint32_t peerSize = *(uint32_t*)peerP;  
    int send_length;
    char *ptr;
    int requestType  = SECI_VERIFY_PEER;
    int result;
    seciProxyP = init_conn();
    /* Build the send signal */
    send_length = dnTcatSize + peerSize + 4*sizeof(uint32_t);
    ptr = send_packet.data = malloc(send_length);
    send_packet.length = send_length;
    *(int*)ptr = requestType;
    ptr+=4;
    *(uint32_t*)ptr = dnTcatSize;
    ptr+=4;
    strcpy(ptr, dnTcatP);
    ptr+=dnTcatSize;
    memcpy(ptr, ((char *)peerP), peerSize+4);
    /* Send the request and wait for the translated reply */
    result = send_recv(seciProxyP, &send_packet, &recv_packet, 4000); // 4s timeout
    if (result == SECI_OK) {
        /* OK. Copy the result to result parameter */
        //*resultP = malloc(recv_packet.length);
        result = *((int *)recv_packet.data);
        //memcpy(*resultP, ((char *)recv_packet.data)+4, recv_packet.length-4);
        free(recv_packet.data);
    } else {
        result = SECI_VERIFY_PEER_UNKNOWN; 
    }
    free(send_packet.data);
    /* Terminate the connection if there was no connection before */
    term_conn(seciProxyP);
    DEBUG("RESULT:\n  result  = %d\n", result);
    return result;
}

SeciResultT seci_verify_peer_vc(char *peerP) {
    DEBUG("seci_verify_peer_vc%s", "");
    SeciProxyData *seciProxyP = NULL;
    cec_packet_t send_packet, recv_packet;
    uint32_t peerSize = strlen(peerP);  
    int send_length;
    char *ptr;
    int requestType  = SECI_VERIFY_PEER_VC;
    int result;
    seciProxyP = init_conn();
    /* Build the send signal */
    send_length = peerSize + 2*sizeof(uint32_t);
    ptr = send_packet.data = malloc(send_length);
    send_packet.length = send_length;
    *(int*)ptr = requestType;
    ptr+=4;
    *(uint32_t*)ptr = peerSize;
    ptr+=4;
    memcpy(ptr, ((char *)peerP), peerSize);
    /* Send the request and wait for the translated reply */
    result = send_recv(seciProxyP, &send_packet, &recv_packet, 43000);
    if (result == SECI_OK) {
        /* OK. Copy the result to result parameter */
        //*resultP = malloc(recv_packet.length);
        result = *((int *)recv_packet.data);
        //memcpy(*resultP, ((char *)recv_packet.data)+4, recv_packet.length-4);
        free(recv_packet.data);
    } else {
        result = SECI_VERIFY_PEER_UNKNOWN; 
    }
    free(send_packet.data);
    /* Terminate the connection if there was no connection before */
    term_conn(seciProxyP);
    DEBUG("RESULT:\n  result  = %d\n", result);
    return result;
}

SeciResultT seci_encode(char *peerVcP, int dataLen, char *dataP,
        char **resultP) {
    DEBUG("seci_encode%s", "");
    SeciProxyData *seciProxyP = NULL;
    cec_packet_t send_packet, recv_packet;
    int peerVcSize = strlen(peerVcP);
    int send_length;
    char *ptr;
    int requestType  = SECI_ENCODE;
    int result;
    /* resultP should be NULL */
    if (resultP == NULL || *resultP != NULL) {
        INFO("Parameter resultP must be NULL%s", "");
        return SECI_INVALID_PARAMETER_RESULTP;
    }
    seciProxyP = init_conn();
    /* Build the send signal */
    send_length = peerVcSize + dataLen + 3*sizeof(uint32_t);
    ptr = send_packet.data = malloc(send_length);
    send_packet.length = send_length;
    *(uint32_t*)ptr = requestType;
    ptr+=4;
    *(uint32_t*)ptr = peerVcSize;
    ptr+=4;
    memcpy(ptr, ((char *)peerVcP), peerVcSize);
    ptr+=peerVcSize;
    *(uint32_t*)ptr = dataLen;
    ptr+=4;
    memcpy(ptr, ((char *)dataP), dataLen);
    /* Send the request and wait for the translated reply */
    result = send_recv(seciProxyP, &send_packet, &recv_packet, 43000);
    if (result == SECI_OK) {
      result = get_string_from_packet(&recv_packet,resultP);
      free(recv_packet.data);
    }
    free(send_packet.data);
    /* Terminate the connection if there was no connection before */
    term_conn(seciProxyP);
    DEBUG("RESULT:\n  result  = %d \n", result);
    return result;
}

SeciResultT seci_decode(char *peerVcP, int dataLen, char *dataP,
        char **resultP) {
    DEBUG("seci_decode%s", "");
    SeciProxyData *seciProxyP = NULL;
    cec_packet_t send_packet, recv_packet;
    int peerVcSize = strlen(peerVcP);
    int send_length;
    char *ptr;
    int requestType  = SECI_DECODE;
    int result;
    /* resultP should be NULL */
    if (resultP == NULL || *resultP != NULL) {
        INFO("Parameter resultP must be NULL%s", "");
        return SECI_INVALID_PARAMETER_RESULTP;
    }
    seciProxyP = init_conn();
    /* Build the send signal */
    send_length = peerVcSize + dataLen + 3*sizeof(uint32_t);
    ptr = send_packet.data = malloc(send_length);
    send_packet.length = send_length;
    *(uint32_t*)ptr = requestType;
    ptr+=4;
    *(uint32_t*)ptr = peerVcSize;
    ptr+=4;
    memcpy(ptr, ((char *)peerVcP), peerVcSize);
    ptr+=peerVcSize;
    *(uint32_t*)ptr = dataLen;
    ptr+=4;
    memcpy(ptr, ((char *)dataP), dataLen);
    /* Send the request and wait for the translated reply */
    result = send_recv(seciProxyP, &send_packet, &recv_packet, 43000);
    if (result == SECI_OK) {
        result = get_string_from_packet(&recv_packet,resultP);
        free(recv_packet.data);
    }
    free(send_packet.data);
    /* Terminate the connection if there was no connection before */
    term_conn(seciProxyP);
    DEBUG("RESULT:\n  result  = %d \n", result);
    return result;
}

void seci_free(void* p) {
    DEBUG("seci_free%s", "");
    free(p);
    return;
}

static SeciProxyData * init_conn() {
    char signature[] = SIGNATURE;
    SeciProxyData *seciProxyP;
    seciProxyP = malloc(sizeof *seciProxyP);
    seciProxyP->handle = cec_open(signature, sizeof(signature));
    DEBUG("call: cec_open  %lx\n", (unsigned long)seciProxyP->handle);
    return seciProxyP;
}

static SeciResultT send_recv(SeciProxyData *seciProxyP,
	                     cec_packet_t *send_packet,
	                     cec_packet_t *recv_packet,
                             int timeout) {
    //int timeout = 43000; // milliseconds
    
    if (cec_send_with_pid(seciProxyP->handle, send_packet) == -1) {
        INFO("ErrorCode: %d (SECI_SEND_ERROR)", 3);
        return SECI_SEND_ERROR;
    }
    
    if (cec_receive_w_tmeout(seciProxyP->handle, recv_packet, timeout) == -1) {
        INFO("ErrorCode: %d (SECI_RECEIVE_ERROR)",4);
        return SECI_RECEIVE_ERROR;
    }
    
    return SECI_OK;
}

static void term_conn(SeciProxyData *seciProxyP) {
    DEBUG("call: cec_close %lx\n", (unsigned long)seciProxyP->handle);
    cec_close(seciProxyP->handle);
    free(seciProxyP);
}

/*
 * Utility method that extracts and allocates a null terminated string
 * from a CEC response.
 * Can be used when response from certSeci is in the format:
 * <result code><length><data>
 */
static SeciResultT
get_string_from_packet(cec_packet_t* recv_packet, char** resultP) {

  int result;
  int dataLen;
  char* ptr = recv_packet->data;

  result = *((int *)ptr);
  ptr += 4;
  dataLen = *((int *)ptr);
  *resultP = malloc(dataLen +1);
  memset(*resultP,0,dataLen +1);
  ptr += 4;
  memcpy(*resultP,ptr,dataLen);
  return result;

}



static SeciResultT
check_secret(char* secret)
{
  if(secret == NULL) {
    DEBUG("no secret provided%s","");
    return SECI_ERROR;
  }
  if(crc(secret,strlen(secret)) == SECI_SECRET_CHECKSUM)
  {
    DEBUG("secret is ok%s","");
    return SECI_OK;
  }

  DEBUG("bad secret%s","");
  return SECI_ERROR;

}

