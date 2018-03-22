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
#include <string.h>
#include <stdbool.h>
#include <arpa/inet.h>
#include <sys/queue.h>
#include "conn-establish-helper.h"
#include "log.h"

typedef conn_any_msg_t conn_monitor_fwd_t;

union itc_msg {
	uint32_t msgno;
	conn_any_msg_t         conn_any_msg;
	conn_establish_req_t   conn_establish_req;
	conn_establish_cfm_t   conn_establish_cfm;
	conn_establish_rej_t   conn_establish_rej;
	conn_disconnect_req_t  conn_disconnect_req;
	conn_disconnect_cfm_t  conn_disconnect_cfm;
	conn_disconnect_rej_t  conn_disconnect_rej;
	conn_monitor_fwd_t     conn_monitor_fwd;
};

typedef struct client_t {
	STAILQ_ENTRY(client_t) next;
	uint32_t client_ref;
	uint32_t server_ref;
	itc_mbox_id_t client_mailbox;
	itc_monitor_id_t monitor_id;
	uint32_t selected_protocol;
	void *client_data;
} client_t;

STAILQ_HEAD(client_list_t, client_t);

typedef struct conn_server_data_internal {
	struct client_list_t client_list;
	struct conn_establish_msg_numbers msgno;
	uint32_t version_count;
	uint32_t *versions;
	uint32_t max_clients;
	uint32_t num_clients;
	conn_event_connect_cb     connecting_cb;
	conn_event_disconnect_cb  disconnecting_cb;
	conn_event_disconnect_cb  died_cb;
	conn_event_dropped_msg_cb dropped_msg_cb;
} server_data_t;

#ifdef DEBUG
void DUMP_ALL_CLIENTS(char *text, server_data_t *sd)
{
	client_t *client = NULL;
	int client_num = 1;
	log_info("%s: Clients %d/%d", text, sd->num_clients, sd->max_clients);
	STAILQ_FOREACH(client, &(sd->client_list), next) {
		log_info("%03d: mb %08x, cr %08x, sr %08x",
		         client_num++, client->client_mailbox,
		         client->client_ref, client->server_ref);
	}
}
#else
#define DUMP_ALL_CLIENTS(a,b)
#endif

#define CHECK_MALLOC(pointer) if(pointer == NULL){   \
		log_err("Memory allocation failed"); \
		exit(1);                             \
	}

static void clear_client_info(struct conn_client_info *client_info)
{
	memset(client_info, 0, sizeof(struct conn_client_info));
}
static void get_client_info(server_data_t *sd,
                            struct conn_client_info *client_info,
                            client_t *client,
                            enum conn_client_state state,
                            union itc_msg *msg)
{
	client_info->client_ref        = client->client_ref;
	client_info->server_ref        = client->server_ref;
	client_info->protocol_version  = client->selected_protocol;
	client_info->client_data       = client->client_data;
	client_info->state             = state;
	client_info->connected_mailbox = client->client_mailbox;
	client_info->server_handle     = sd;
	if(msg && (itc_size(msg) >= sizeof(conn_any_msg_t))) {
		client_info->procedure_ref = msg->conn_any_msg.procedure_ref;
		client_info->sender        = itc_sender(msg);
	} else {
		client_info->procedure_ref = 0;
		client_info->sender        = 0;
	}
}


/*
 * match_protocols
 *  Find the most wanted protocol common to the client and the server.
 */
static uint32_t match_protocols(server_data_t *sd,
                                uint32_t count,
                                uint32_t *versions)
{
	int i, j;
	for(i = 0; i < count; i++) {
		for(j = 0; j < sd->version_count; j++) {
			if(versions[i] == sd->versions[j])
				return sd->versions[j];
		}
	}
	return 0;
}

/*
 * handle_conn_establish
 */
static void handle_conn_establish(server_data_t *sd,
                                  union itc_msg **message)
{
	static uint32_t last_server_ref = 0;
	conn_establish_req_t *msg = (conn_establish_req_t *) *message;
	itc_mbox_id_t sender = itc_sender(*message);
	union itc_msg *reply = NULL;
	union itc_msg *monitor_fwd = NULL;
	uint32_t reject_reason = 0;
	client_t *client = NULL;
	client_t *new_client = NULL;
	uint32_t connection_ref;
	uint32_t selected_protocol;

	if( (sd->max_clients != 0) &&
	    (sd->num_clients >= sd->max_clients) ) {
		log_info("ABN: Maximum number of clients reached "
		         "(mailbox_id=0x%08x / client_ref=0x%08x) ",
		         sender, msg->connection_ref);
		reject_reason = CONN_ESTABLISH_REJ_MAX_CLIENTS_REACHED;
		connection_ref = msg->connection_ref;
		goto reject;
	}

	/* checking if valid message */
	msg->protocol_count = ntohl(msg->protocol_count);
	if( (msg->protocol_count == 0) ||
	    (itc_size((union itc_msg *) msg) <  sizeof(conn_establish_req_t) +
	     (msg->protocol_count - 1) * sizeof(uint32_t)) ) {
		log_info("ABN: Received invalid connection establish request "
		         "from (mailbox_id=0x%08x / client_ref=0x%08x)",
		         sender, msg->connection_ref);
		reject_reason = CONN_ESTABLISH_REJ_INVALID_PARAMETER;
		connection_ref = msg->connection_ref;
		goto reject;
	}

	/* fixing endianess, of protocol versions */
	for(int i = 0; i < msg->protocol_count; i++)
		msg->protocol_versions[i] = ntohl(msg->protocol_versions[i]);

	/* checking if already a client. */
	STAILQ_FOREACH(client, &(sd->client_list), next) {
		if(msg->connection_ref == client->client_ref &&
		    sender == client->client_mailbox)
			break;
	}

	if(client) {
		log_info("ABN: Client (mailbox_id=0x%08x / client_ref=0x%08x / "
		         "server_ref=0x%08x) is already connected.",
		         client->client_mailbox, client->client_ref,
		         client->server_ref);
		reject_reason = CONN_ESTABLISH_REJ_ALREADY_CONNECTED;
		connection_ref = client->server_ref;
		goto reject;
	}
	/* checking for compatible protocol version */
	selected_protocol = match_protocols(sd, msg->protocol_count,
	                                    msg->protocol_versions);
	if(selected_protocol == 0) {
		log_info("ABN: Client (client_ref=0x%08x, "
		         "sender=0x%08x) is requesting an "
		         "unsupported protocol version.",
		         msg->connection_ref, sender);
		reject_reason = CONN_ESTABLISH_REJ_UNSUPPORTED_VERSION;
		connection_ref = 0;
		goto reject;
	}

	/* everything is OK, register new client.*/
	new_client = malloc(sizeof(client_t));
	CHECK_MALLOC(new_client);
	new_client->client_ref = msg->connection_ref;
	new_client->server_ref = ++last_server_ref;
	new_client->client_mailbox = sender;
	new_client->selected_protocol = selected_protocol;
	new_client->client_data = NULL;

	STAILQ_INSERT_HEAD(&(sd->client_list), new_client, next);

	/* If there is a connection callback then check if the server server
	   wants us to confirm this connection*/
	if(sd->connecting_cb) {
		struct conn_client_info ci;
		get_client_info( sd, &ci, new_client,
		                 CONN_ESTABLISH_STATUS_CONNECTING,
		                 *message );
		uint32_t res = sd->connecting_cb(&ci);
		if(res > 0) { /* callback want to reject connection */
			STAILQ_REMOVE_HEAD(&(sd->client_list), next);
			connection_ref = 0;
			reject_reason = res;
			free(new_client);
			goto reject;
		}
	}

	monitor_fwd = itc_alloc(sizeof(conn_monitor_fwd_t),
	                        sd->msgno.monitor_fwd);
	monitor_fwd->conn_monitor_fwd.connection_ref = new_client->server_ref;
	new_client->monitor_id = itc_monitor(sender, &monitor_fwd);

	sd->num_clients++;

	reply = itc_alloc(sizeof(conn_establish_cfm_t),
	                  sd->msgno.establish_cfm);
	reply->conn_establish_cfm.procedure_ref = msg->procedure_ref;
	reply->conn_establish_cfm.connection_ref = new_client->server_ref;
	reply->conn_establish_cfm.selected_protocol_version = htonl(selected_protocol);
	itc_send(&reply, sender, ITC_MY_MBOX);
	log_trace("New client connected. (mailbox_id=0x%08x / client_ref=0x%x / "
	          "server_ref=0x%x). Now %d clients",
	          new_client->client_mailbox, new_client->client_ref,
	          new_client->server_ref, sd->num_clients);
	DUMP_ALL_CLIENTS("After connect: ", sd);
	return;

reject:
	reply = itc_alloc(sizeof(conn_establish_rej_t) +
	                  (sd->version_count - 1) * sizeof(uint32_t),
	                  sd->msgno.establish_rej);
	reply->conn_establish_rej.procedure_ref = msg->procedure_ref;
	reply->conn_establish_rej.connection_ref = connection_ref;
	reply->conn_establish_rej.reason = htonl(reject_reason);
	reply->conn_establish_rej.supported_protocol_count =
	        htonl(sd->version_count);
	for(int i = 0; i < sd->version_count; i++) {
		reply->conn_establish_rej.supported_protocol_versions[i] =
		        htonl(sd->versions[i]);
	}
	itc_send(&reply, sender, ITC_MY_MBOX);
	return;
}

/*
 * handle_conn_disconnect
 */
static void handle_conn_disconnect(server_data_t *sd,
                                   union itc_msg **message)
{
	conn_disconnect_req_t *msg = (conn_disconnect_req_t *) *message;
	itc_mbox_id_t sender = itc_sender(*message);
	union itc_msg *reply = NULL;
	client_t *client = NULL;
	union itc_msg *monitor_fwd = NULL;

	DUMP_ALL_CLIENTS("Before disconnect: ", sd);
	/* check if it is a known client */
	STAILQ_FOREACH(client, &(sd->client_list), next) {
		if(msg->connection_ref == client->server_ref)
			break;
	}
	if(!client) {
		log_info("ABN: Received a %s message from unknown "
		         "client (mbox_id=0x%08x)",
		         msg->msgno == sd->msgno.monitor_fwd ?
		         "monitor" : "disconnect",
		         itc_sender(*message));

		if(msg->msgno == sd->msgno.monitor_fwd)
			return;

		reply = itc_alloc(sizeof(conn_disconnect_rej_t),
		                  sd->msgno.disconnect_rej);
		reply->conn_disconnect_rej.procedure_ref = msg->procedure_ref;
		reply->conn_disconnect_rej.connection_ref = 0;
		reply->conn_disconnect_rej.reason =
		        htonl(CONN_ESTABLISH_REJ_NOT_A_CLIENT);
		itc_send(&reply, sender, ITC_MY_MBOX);
		return;
	}

	if(msg->msgno == sd->msgno.disconnect_req && sd->disconnecting_cb) {
		struct conn_client_info ci;
		get_client_info( sd, &ci, client,
		                 CONN_ESTABLISH_STATUS_DISCONNECTING,
		                 *message );
		sd->disconnecting_cb(&ci);
	}

	if(msg->msgno == sd->msgno.monitor_fwd && sd->died_cb) {
		struct conn_client_info ci;
		get_client_info( sd, &ci, client,
		                 CONN_ESTABLISH_STATUS_DEAD,
		                 *message );
		sd->died_cb(&ci);
	}

	STAILQ_REMOVE(&(sd->client_list), client, client_t, next);

	if(msg->msgno == sd->msgno.disconnect_req) {
		itc_unmonitor(client->monitor_id);
		reply = itc_alloc( sizeof(conn_disconnect_cfm_t),
		                   sd->msgno.disconnect_cfm );
		reply->conn_disconnect_cfm.procedure_ref = msg->procedure_ref;
		reply->conn_disconnect_cfm.connection_ref = client->client_ref;
		itc_send(&reply, sender, ITC_MY_MBOX);
	}

	if(sd->num_clients) /*safety - avoid going below zero*/
		sd->num_clients--;

	log_trace("Client %s. (mailbox_id=0x%08x / client_ref=0x%x / "
	          "server_ref=0x%x). Now %d clients",
	          msg->msgno == sd->msgno.disconnect_req ?
	          "dis-connected" : "died",
	          client->client_mailbox, client->client_ref,
	          client->server_ref, sd->num_clients);

	free(client);

	itc_free(message);
	*message = monitor_fwd;
	DUMP_ALL_CLIENTS("After disconnect: ", sd);
	return;
}


#ifdef DEBUG
#include <stdio.h> /*snprintf()*/
static const char *get_msg_name(server_data_t *sd, conn_any_msg_t *msg)
{
	static char reply[30];
	if(msg->msgno == sd->msgno.establish_req)
		return "CONN_ESTABLISH_REQ";
	if(msg->msgno == sd->msgno.establish_cfm)
		return "CONN_ESTABLISH_CFM";
	if(msg->msgno == sd->msgno.establish_rej)
		return "CONN_ESTABLISH_REJ";
	if(msg->msgno == sd->msgno.disconnect_req)
		return "CONN_DISCONNECT_REQ";
	if(msg->msgno == sd->msgno.disconnect_cfm)
		return "CONN_DISCONNECT_CFM";
	if(msg->msgno == sd->msgno.disconnect_rej)
		return "CONN_DISCONNECT_REJ";
	if(msg->msgno == sd->msgno.monitor_fwd)
		return "CONN_MONITOR_FWD";
	snprintf(reply, sizeof(reply), "User message 0x%08x", msg->msgno);
	return reply;
}

static void DUMP_MSG(server_data_t *sd, conn_any_msg_t *msg, char *heading)
{
	log_info("%s %s proc_ref=0x%08x, conn_ref=0x%08x",
	         heading, get_msg_name(sd, msg),
	         msg->procedure_ref, msg->connection_ref);
}
#else
#define DUMP_MSG(a,b,c)
#endif


static void drop_msg(server_data_t *sd, union itc_msg **message, char *reason)
{
	if(sd->dropped_msg_cb)
		sd->dropped_msg_cb(message);
	/* If callback has not handled/freed the message then
	   log that we will drop it.*/
	if(*message) {
		log_info("ABN: Dropping message msgno=0x%08x "
		         "from mbox_id=0x%08x (%s).",
		         (*message)->msgno, itc_sender(*message), reason);
	}
}
/********************************************************************
 * Function: conn_check_client()
 *
 ********************************************************************/
bool conn_check_client(conn_server_handle_t handle,
                       union itc_msg **message,
                       struct conn_client_info *client_info)
{
	server_data_t *sd    = (server_data_t *) handle;
	conn_any_msg_t *msg  = (conn_any_msg_t *)*message;
	client_t *client     = NULL;

	DUMP_MSG(sd, msg, "Received:");

	clear_client_info(client_info);

	if(msg->msgno == sd->msgno.establish_req) {
		if(itc_size(*message) < sizeof(conn_establish_req_t)) {
			drop_msg(sd, message, "invalid size");
			goto finish;
		}
		log_trace("Received a establish_req from 0x%08x",
		          itc_sender(*message));
		handle_conn_establish(sd, message);
		goto finish;
	}

	/*Verify that the received message is large enough to have
	  the necessary fields?
	  This check covers all messages except establish_req. */
	if(itc_size(*message) < sizeof(conn_any_msg_t)) {
		drop_msg(sd, message, "too small");
		goto finish;
	}

	if(msg->msgno == sd->msgno.monitor_fwd ||
	    msg->msgno == sd->msgno.disconnect_req) {
		log_trace("Received a %s from 0x%08x",
		          msg->msgno == sd->msgno.disconnect_req ?
		          "disconnect_req" : "monitor_fwd",
		          itc_sender(*message));
		handle_conn_disconnect(sd, message);
		goto finish;
	}

	/* Handle all user messages */
	STAILQ_FOREACH(client, &(sd->client_list), next) {
		if(msg->connection_ref == client->server_ref)
			break;
	}
	if(client) {
		get_client_info(sd, client_info, client,
		                CONN_ESTABLISH_STATUS_CONNECTED,
		                *message);
		return true;
	}
	drop_msg(sd, message, "unknown client");

finish:
	if(*message) itc_free(message);
	return false;
}

/********************************************************************
 * Function: conn_set_client_data()
 *
 ********************************************************************/
int conn_set_client_data(conn_server_handle_t handle,
                         uint32_t server_ref, void *client_data)
{
	server_data_t *sd = (server_data_t *) handle;
	client_t *client = NULL;


	if(!sd->disconnecting_cb || !sd->died_cb) {
		log_info("ABN: Set of client_data is not allowed unless the "
		         "conn_event_disconnecting_cb and "
		         "client_disconnecting_cb callbacks are defined.");
		log_trace("returning CONN_ESTABLISH_CLIENT_MISSING_CALLBACKS");
		return CONN_ESTABLISH_CLIENT_MISSING_CALLBACKS;
	}

	STAILQ_FOREACH(client, &(sd->client_list), next) {
		if(server_ref == client->server_ref)
			break;
	}

	if(client) {
		if(client->client_data != NULL) {
			log_info("ABN: client_data was already set for "
			         "server_ref 0x%08x. "
			         "Ignoring request to set client_data.",
			         server_ref);
			log_trace("returning CONN_ESTABLISH_CLIENT_DATA_ALREADY_SET");
			return CONN_ESTABLISH_CLIENT_DATA_ALREADY_SET;
		}
		client->client_data = client_data;
		log_trace("returning CONN_ESTABLISH_CLIENT_DATA_OK");
		return CONN_ESTABLISH_CLIENT_DATA_OK;
	}
	log_info("ABN: Tried to set client data on non-existing client "
	         "(server_ref=0x%08x)", server_ref);
	log_trace("returning CONN_ESTABLISH_CLIENT_DOES_NOT_EXIST");
	return CONN_ESTABLISH_CLIENT_DOES_NOT_EXIST;
}

/********************************************************************
 * Function: conn_clear_client_data()
 *
 ********************************************************************/
int conn_clear_client_data(conn_server_handle_t handle,
                           uint32_t server_ref, void **client_data)
{
	server_data_t *sd = (server_data_t *) handle;
	client_t *client = NULL;

	STAILQ_FOREACH(client, &(sd->client_list), next) {
		if(server_ref == client->server_ref)
			break;
	}
	if(client) {
		*client_data = client->client_data;
		client->client_data = NULL;
		return CONN_ESTABLISH_CLIENT_DATA_OK;
	}
	log_info("ABN: Tried to clear client data on non-existing client "
	         "(server_ref=0x%08x)", server_ref);
	return CONN_ESTABLISH_CLIENT_DOES_NOT_EXIST;
}
/********************************************************************
 * Function: conn_get_client_data()
 *
 ********************************************************************/
int conn_get_client_info(conn_server_handle_t handle,
                         uint32_t server_ref,
                         struct conn_client_info *client_info)
{
	server_data_t *sd = (server_data_t *) handle;
	client_t *client = NULL;

	STAILQ_FOREACH(client, &(sd->client_list), next) {
		if(server_ref == client->server_ref)
			break;
	}

	if(!client) {
		return CONN_ESTABLISH_CLIENT_DOES_NOT_EXIST;
	}
	get_client_info(handle, client_info, client,
	                CONN_ESTABLISH_STATUS_CONNECTED,
	                NULL);

	return CONN_ESTABLISH_CLIENT_INFO_OK;
}

/********************************************************************
 * Function: init_conn_establish_server()
 *
 ********************************************************************/
int conn_establish_server_init( conn_server_handle_t *handle,
                                uint32_t supported_version_count,
                                uint32_t *supported_versions,
                                struct conn_establish_msg_numbers *msg_numbers,
                                uint32_t max_client_count,
                                struct conn_event_callbacks *callbacks)
{
	server_data_t *sd = NULL;
	if(!handle) {
		log_info("ABN: %s called with NULL pointer as handle",
		         __func__);
		return CONN_INIT_INVALID_PARAMETER;
	}
	*handle = NULL;
	if(supported_version_count == 0) {
		log_info("ABN: %s called with 0 supported versions", __func__);
		return CONN_INIT_INVALID_PARAMETER;
	}
	if(!supported_versions || supported_version_count == 0) {
		log_info("ABN: %s called with NULL pointer as supported_versions",
		         __func__);
		return CONN_INIT_INVALID_PARAMETER;
	}
	if(!msg_numbers) {
		log_info("ABN: %s called with NULL pointer as msg_numbers",
		         __func__);
		return CONN_INIT_INVALID_PARAMETER;
	}
#define NUM_MSG_NUMBERS sizeof(sd->msgno)/sizeof(sd->msgno.establish_req)
	for(int i = 0; i < NUM_MSG_NUMBERS; i++) {
		for(int j = i + 1; j < NUM_MSG_NUMBERS; j++) {
			if( ((uint32_t *)msg_numbers)[i] ==
			    ((uint32_t *)msg_numbers)[j] ) {
				log_info("ABN: conn_establish_server_init() "
				         " numbers in msg_numbers are not unique");
				return CONN_INIT_INVALID_PARAMETER;
			}
		}
	}

	sd = malloc(sizeof(server_data_t));
	CHECK_MALLOC(sd);
	memset(sd, 0, sizeof(server_data_t));
	sd->versions = malloc(supported_version_count * sizeof(uint32_t));
	CHECK_MALLOC(sd->versions);
	memcpy(sd->versions, supported_versions,
	       supported_version_count * sizeof(uint32_t));
	sd->version_count = supported_version_count;
	memcpy(&(sd->msgno), msg_numbers, sizeof(sd->msgno));
	sd->max_clients      = max_client_count;

	if(callbacks) {
		sd->connecting_cb    = callbacks->client_connecting_cb;
		sd->disconnecting_cb = callbacks->client_disconnecting_cb;
		sd->died_cb          = callbacks->client_died_cb;
		sd->dropped_msg_cb   = callbacks->dropped_msg_cb;
	} /*else { callbacks set to NULL implies all callbacks are disabled. }*/

	STAILQ_INIT(&(sd->client_list));
	*handle = (conn_server_handle_t) sd;
	return CONN_INIT_OK;
}
