/**
 * Copyright (C) 2015 by Ericsson AB. All rights reserved. The
 * information in this document is the property of Ericsson. Except
 * as specifically authorized in writing by Ericsson, the receiver
 * of this document shall keep the information contained herein
 * confidential and shall protect the same in whole or in part from
 * disclosure and dissemination to third parties. Disclosure and
 * disseminations to the receiver's employees shall only be made on
 * a strict need to know basis.
 */

#include <itc.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <arpa/inet.h>

#include "conn-establish-helper.h"
#include "log.h"

union itc_msg {
	uint32_t msgno;
	conn_any_msg_t         conn_any_msg;
	conn_establish_req_t   conn_establish_req;
	conn_establish_cfm_t   conn_establish_cfm;
	conn_establish_rej_t   conn_establish_rej;
	conn_disconnect_req_t  conn_disconnect_req;
	conn_disconnect_cfm_t  conn_disconnect_cfm;
	conn_disconnect_rej_t  conn_disconnect_rej;
};

/********************************************************************
 * Function: conn_establish()
 *
 ********************************************************************/

uint32_t conn_establish(
        /*input parameters*/
        itc_mbox_id_t server_mailbox,
        uint32_t procedure_ref,
        uint32_t client_ref,
        uint32_t requested_version_count,
        uint32_t *requested_versions,
        struct conn_establish_msg_numbers *msg_numbers,
        uint32_t time_out,
        /*returned values*/
        uint32_t *server_ref,
        uint32_t *selected_version)
{
	uint32_t filter[] = {2, msg_numbers->establish_cfm,
	                     msg_numbers->establish_rej
	                    };
	union itc_msg *msg = itc_alloc(sizeof(conn_establish_req_t) +
	                               (requested_version_count - 1) *
	                               sizeof(uint32_t),
	                               msg_numbers->establish_req);
	int i, pos;
	char *requested_ver_str = NULL;
	char *supported_ver_str = NULL;
	uint32_t reason = CONN_ESTABLISH_SUCCESS;

	msg->conn_establish_req.procedure_ref = procedure_ref;
	msg->conn_establish_req.connection_ref = client_ref;
	msg->conn_establish_req.protocol_count = htonl(requested_version_count);
	for(i = 0; i < requested_version_count; i++) {
		msg->conn_establish_req.protocol_versions[i] =
		        htonl(requested_versions[i]);
	}
	itc_send(&msg, server_mailbox, ITC_MY_MBOX);
	while(1) {
		msg = itc_receive(filter,
		                  time_out == 0 ? ITC_NO_TMO : time_out,
		                  server_mailbox);
		if(!msg) {
			reason = CONN_ESTABLISH_REJ_TIME_OUT;
			goto connection_error;
		}
		if(msg->conn_any_msg.procedure_ref != procedure_ref) {
			log_info("ABN: Server replied using wrong procedure_ref "
			         "(expected 0x%08x, received 0x%08x) "
			         "Message ignored.",
			         procedure_ref, msg->conn_any_msg.procedure_ref);
			itc_free(&msg);
			continue;
		}
		if(msg->msgno == msg_numbers->establish_cfm) {
			*selected_version =
			        ntohl(msg->conn_establish_cfm.selected_protocol_version);
			*server_ref = msg->conn_establish_cfm.connection_ref;
			itc_free(&msg);
			return CONN_ESTABLISH_SUCCESS;
		}
		break; /* if we get here the we got a reject. */
	}
	/* Handle reject */
	requested_ver_str = malloc(sizeof("0x12345678, ") *
	                           requested_version_count);
	supported_ver_str = malloc(sizeof("0x12345678, ") *
	                           ntohl(msg->conn_establish_rej.supported_protocol_count));
	if( requested_ver_str == NULL || supported_ver_str == NULL ) {
		log_err("Memory allocation failed for "
		        "requested_ver_str or supported_ver_str.");
		reason = CONN_ESTABLISH_REJ_OUT_OF_MEMORY;
		goto connection_error;
	}
	reason = ntohl(msg->conn_establish_rej.reason);
	for(i = 0, pos = 0; i < requested_version_count; i++)
		pos += sprintf(&requested_ver_str[pos], "0x%08x, ",
		               requested_versions[i]);
	requested_ver_str[pos - 2] = '\0'; /*remove last comma and space*/

	for(i = 0, pos = 0;
	    i < ntohl(msg->conn_establish_rej.supported_protocol_count);
	    i++)
		pos += sprintf(&supported_ver_str[pos], "0x%08x, ",
		               ntohl(msg->conn_establish_rej.supported_protocol_versions[i]));
	supported_ver_str[pos - 2] = '\0';

	log_info("ABN: Connection establish rejected. Reason %d; "
	         "Requested protocol versions: %s; "
	         "Supported protocol versions: %s",
	         reason, requested_ver_str, supported_ver_str);

connection_error:
	*selected_version = 0;
	*server_ref = 0;
	free(requested_ver_str);
	free(supported_ver_str);
	if(msg) itc_free(&msg);
	return reason;
}

/********************************************************************
 * Function: conn_disconnect()
 *
 ********************************************************************/
uint32_t conn_disconnect(
        /*input parameters*/
        itc_mbox_id_t server_mailbox,
        uint32_t procedure_ref,
        uint32_t server_ref,
        struct conn_establish_msg_numbers *msg_numbers,
        uint32_t time_out)
{
	uint32_t filter[] = {2, msg_numbers->disconnect_cfm,
	                     msg_numbers->disconnect_rej
	                    };
	union itc_msg *msg = itc_alloc(sizeof(conn_disconnect_req_t),
			                               msg_numbers->disconnect_req);

	msg->conn_disconnect_req.procedure_ref = procedure_ref;
	msg->conn_disconnect_req.connection_ref = server_ref;
	itc_send(&msg, server_mailbox, ITC_MY_MBOX);
	while(1) {
		msg = itc_receive(filter,
		                  time_out == 0 ? ITC_NO_TMO : time_out,
		                  server_mailbox);
		if(!msg)
			return CONN_ESTABLISH_REJ_TIME_OUT;

		if(msg->conn_any_msg.procedure_ref != procedure_ref) {
			log_info("ABN: Server replied using wrong procedure_ref "
			         "(expected 0x%08x, received 0x%08x) "
			         "Message ignored.",
			         procedure_ref, msg->conn_any_msg.procedure_ref);
			itc_free(&msg);
			continue;
		}

		if(msg->msgno == msg_numbers->disconnect_cfm) {
			itc_free(&msg);
			return CONN_ESTABLISH_SUCCESS;
		}

		uint32_t reason = ntohl(msg->conn_establish_rej.reason);
		itc_free(&msg);
		return reason;
	}
}
