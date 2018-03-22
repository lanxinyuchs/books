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

#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <stdbool.h>
#include <getopt.h>
#include <unistd.h>
#include <signal.h>
#include <itc.h>

#include "test.h"

#ifdef LOG_LTTNG
#define TRACEPOINT_DEFINE
#define TRACEPOINT_PROBE_DYNAMIC_LINKAGE
#endif
#include "log.h"

#define EXIT_SIGNAL     0xdeadbeef

#if __SIZEOF_POINTER__ == 8
#    define POINTER_TO_UINT_CAST uint64_t
#else
#    define POINTER_TO_UINT_CAST uint32_t
#endif

#define MAILBOX_SIZE 5

union itc_msg {
	uint32_t msgno;
	conn_any_msg_t any_msg;
	TEST_MESSAGES;
};

typedef struct {
	itc_mbox_id_t mbox;
	uint32_t data;
} cd_t;

static itc_mbox_id_t my_mbox;
static itc_mbox_id_t client_master_mbox;
static uint32_t reject_next_conn = 0;
static uint32_t connect_cb(struct conn_client_info *ci)
{
	uint32_t reject_reason;
	log_trace("Connect callback called");
	if(reject_next_conn) {
		log_trace("Connection rejected %d", reject_next_conn);
		reject_reason = reject_next_conn;
		reject_next_conn = 0; /*just reject once*/
		return reject_reason;
	}
	return 0;
}
static void disconnect_cb(struct conn_client_info *ci)
{
	union itc_msg *reply;
	log_trace(" Client 0x%08x has disconnected",
	     ci->sender);
	if(client_master_mbox) {
		reply = itc_alloc(sizeof(struct test_client_died_ind),
		                  TEST_CLIENT_DISCONNECT_IND);
		reply->test_client_died_ind.mbox = ci->sender;
		reply->test_client_died_ind.client_ref = ci->client_ref;
		reply->test_client_died_ind.server_ref = ci->server_ref;
		itc_send(&reply, client_master_mbox, ITC_MY_MBOX);
		log_trace("Sent a TEST_CLIENT_DISCONNECT_IND");
	}
	/*Handle cleanup after client has disconnected*/
	if(ci->client_data) {
		reply = itc_alloc(sizeof(struct test_cd_clear_cfm),
		                  TEST_CD_CLEAR_CFM);
		reply->test_cd_clear_cfm.data = ((cd_t *)ci->client_data)->data;
		reply->any_msg.procedure_ref = ci->procedure_ref;
		reply->any_msg.connection_ref = ci->client_ref;
		log_trace("Replying with msgno 0x%08x to client 0x%08x",
		     reply->msgno, ci->sender);
		itc_send(&reply, ci->sender, ITC_MY_MBOX);
		free(ci->client_data);
	}
}
static void died_cb(struct conn_client_info *ci)
{
	union itc_msg *reply;
	log_trace("Client 0x%08x has died",
	     ci->sender);
	if(client_master_mbox) {
		reply = itc_alloc(sizeof(struct test_client_died_ind),
		                  TEST_CLIENT_DIED_IND);
		reply->test_client_died_ind.mbox = ci->sender;
		reply->test_client_died_ind.client_ref = ci->client_ref;
		reply->test_client_died_ind.server_ref = ci->server_ref;
		itc_send(&reply, client_master_mbox, ITC_MY_MBOX);
		log_trace("Sent a TEST_CLIENT_DIED_IND");
	}
	if(ci->client_data)
		free(ci->client_data);
}
static void dropped_msg_cb(union itc_msg **msg)
{
	log_trace("Dropped msg callback called");
	if((*msg)->msgno == TEST_ECHO_REQ) {
		(*msg)->msgno = TEST_ECHO_RSP;
		itc_send(msg, itc_sender(*msg), ITC_MY_MBOX);
		log_trace("Sent a TEST_ECHO_RSP");
	}
}

static void main_loop(void)
{
	union itc_msg *msg;
	union itc_msg *reply = NULL;
	int res;
	cd_t *cd = NULL;
	CONN_ESTABLISH_MESSAGES_STRUCT(conn_messages);
	uint32_t supported_versions[] = {TEST_SERVER_VERSIONS};
	conn_server_handle_t handle;
	uint32_t tmo_test_tmo = 0;
	struct conn_client_info ci;
#define CLIENT_DATA ((cd_t *) ci.client_data)
	struct conn_event_callbacks cb = {connect_cb, disconnect_cb,
		       died_cb, dropped_msg_cb
	};

	res = conn_establish_server_init(&handle,
	                                 sizeof(supported_versions) /
	                                 sizeof(supported_versions[0]),
	                                 supported_versions,
	                                 &conn_messages, 0, &cb);
	if(res) {
		log_err("Failed to init conn_establish server");
		exit(1);
	}

	log_trace("Entering main loop");
	for (;;) {
		msg = itc_receive(ITC_NOFILTER,
		                  ITC_NO_TMO,
		                  ITC_FROM_ALL);
		if(tmo_test_tmo)
			usleep(tmo_test_tmo * 1000);

		if (msg->msgno == EXIT_SIGNAL) {
			log_info("Server exiting as ordered");
			itc_free(&msg);
			return;
		}

		if(msg->msgno == TEST_PRINT_TEST_INFO_REQ) {
			log_trace("*** %s", msg->test_print_test_info_req.str );
			reply = itc_alloc(sizeof( struct test_print_test_info_cfm),
			                  TEST_PRINT_TEST_INFO_CFM);
			itc_send(&reply, itc_sender(msg), ITC_MY_MBOX);
			itc_free(&msg);
			continue;
		}

		if(msg->msgno == TEST_RE_INIT_REQ) {
			log_trace("Received TEST_RE_INIT_REQ ");
			/*It's totally unacceptable in real code to re-init
			  the server. This is however test code....*/
			struct conn_event_callbacks cb2 = {NULL, NULL, NULL, NULL};
			conn_server_handle_t my_handle ;
			res = conn_establish_server_init(&my_handle,
			                                 sizeof(supported_versions) /
			                                 sizeof(supported_versions[0]),
			                                 supported_versions,
			                                 &conn_messages,
			                                 msg->test_re_init_req.max_clients,
			                                 msg->test_re_init_req.use_callbacks ? &cb : &cb2);
			reply = itc_alloc(sizeof(struct test_re_init_cfm),
			                  TEST_RE_INIT_CFM);
			if(res) {
				log_trace("Failed to init conn_establish server");
				reply->msgno = TEST_RE_INIT_REJ;
			} else {
				handle = my_handle;
			}

			itc_send(&reply, itc_sender(msg), ITC_MY_MBOX);
			itc_free(&msg);
			continue;
		}
		if(msg->msgno ==  TEST_SET_TMO_REQ) {
			log_trace("Received TEST_SET_TMO_REQ ");
			tmo_test_tmo = msg->test_set_tmo_req.time_out;
			log_trace("Setting test_tmo to %d ", tmo_test_tmo );
			reply = itc_alloc(sizeof(struct test_set_tmo_rsp),
			                  TEST_SET_TMO_RSP);
			itc_send(&reply, itc_sender(msg), ITC_MY_MBOX);
			itc_free(&msg);
			continue;
		}


		if(!conn_check_client(handle, &msg, &ci))
			continue;

		if(handle != ci.server_handle) {
			log_err("Incorrect server_handle in "
			        "client_info!");
			exit(1);
		}

		switch(msg->msgno) {
		case TEST_MESSAGE_REQ:
			log_trace("Received TEST_MESSAGE_REQ ");
			log_trace("Wanted reply is 0x%08x",
			     msg->test_message_req.wanted_reply_msgno);
			if(msg->test_message_req.wanted_reply_msgno ==
			    TEST_MESSAGE_CFM) {
				reply = itc_alloc(sizeof(struct test_message_cfm),
				                  TEST_MESSAGE_CFM);
				reply->test_message_cfm.client_data =
				        CLIENT_DATA ? CLIENT_DATA->data : 0;
				reply->test_message_cfm.protocol_version =
				        ci.protocol_version;
			}
			if(msg->test_message_req.wanted_reply_msgno ==
			    TEST_MESSAGE_REJ) {
				reply = itc_alloc(sizeof(struct test_message_rej),
				                  TEST_MESSAGE_REJ);
				reply->test_message_rej.error_code = ERROR_1;
			}
			break;

		case TEST_CD_SET_REQ:
			log_trace("Received TEST_CD_SET_REQ ");
			cd = malloc(sizeof(cd_t));
			if( cd == NULL ) {
				log_err("Can't allocate memory for cd.");
				break;
			}
			cd->data = msg->test_cd_set_req.data;
			cd->mbox = ci.sender;

			res = conn_set_client_data(handle,
			                           msg->test_cd_set_req.server_ref,
			                           cd);
			if(res) { /*cd data have not been stored.*/
				free(cd);
			}
			/*cd is either stored or freed, let's forget it.*/
			cd = NULL;

			if(res == CONN_ESTABLISH_CLIENT_DATA_ALREADY_SET &&
			    msg->test_cd_set_req.server_ref ==
			    msg->test_cd_set_req.connection_ref) {
				reply = itc_alloc(sizeof(struct test_cd_set_rej),
				                  TEST_CD_SET_REJ);
				if(CLIENT_DATA) {
					snprintf(reply->test_cd_set_rej.text, 100,
					         "Tried to set client_data that was "
					         "already set.");
				} else {
					snprintf(reply->test_cd_set_rej.text, 100,
					         "Try to set client_data failed. "
					         "CONN_ESTABLISH_CLIENT_DATA_ALREADY_SET");
				}
				reply->test_cd_set_rej.error = CONN_ESTABLISH_CLIENT_DATA_ALREADY_SET;
				break;
			}
			if(res == CONN_ESTABLISH_CLIENT_DOES_NOT_EXIST) {
				reply = itc_alloc(sizeof(struct test_cd_set_rej),
				                  TEST_CD_SET_REJ);

				if(msg->test_cd_set_req.server_ref ==
				    msg->test_cd_set_req.connection_ref) {
					snprintf(reply->test_cd_set_rej.text, 100,
					         "set_client_data claims CONN_ESTABLISH_CLIENT_DOES_NOT_EXIST, "
					         "but it does!");
					reply->test_cd_set_rej.error = CONN_ESTABLISH_CLIENT_DOES_NOT_EXIST;
				} else {
					snprintf(reply->test_cd_set_rej.text, 100,
					         "set_client_data correctly claims CONN_ESTABLISH_CLIENT_DOES_NOT_EXIST");
					reply->test_cd_set_rej.error = CONN_ESTABLISH_CLIENT_DOES_NOT_EXIST;
				}
				break;
			}

			if(res == CONN_ESTABLISH_CLIENT_DATA_OK) {
				if(msg->test_cd_set_req.server_ref !=
				    msg->test_cd_set_req.connection_ref) {
					reply = itc_alloc(sizeof(struct test_cd_set_rej),
					                  TEST_CD_SET_REJ);
					snprintf(reply->test_cd_set_rej.text, 100,
					         "set_client_data returned CONN_ESTABLISH_CLIENT_DATA_OK "
					         "for non-existing client");
					reply->test_cd_set_rej.error = CONN_ESTABLISH_CLIENT_DATA_OK;
					break;
				}

				reply = itc_alloc(sizeof(struct test_cd_set_cfm),
				                  TEST_CD_SET_CFM);
				break;
			}
			reply = itc_alloc(sizeof(struct test_cd_set_rej),
			                  TEST_CD_SET_REJ);
			snprintf(reply->test_cd_set_rej.text, 100,
			         "set_client_data returned unecpected reply (%d)",
			         res);
			reply->test_cd_set_rej.error = res;
			break;

		case TEST_CD_CLEAR_REQ:
			log_trace("Received TEST_CD_CLEAR_REQ ");

			res = conn_clear_client_data(handle,
			                             msg->test_cd_clear_req.server_ref,
			                             (void **) &cd);

			if(res == CONN_ESTABLISH_CLIENT_DOES_NOT_EXIST) {
				reply = itc_alloc(sizeof(struct test_cd_clear_rej),
				                  TEST_CD_CLEAR_REJ);

				if(msg->test_cd_clear_req.server_ref ==
				    msg->test_cd_clear_req.connection_ref) {
					snprintf(reply->test_cd_clear_rej.text, 100,
					         "clear_client_data claims "
					         "CONN_ESTABLISH_CLIENT_DOES_NOT_EXIST, "
					         "but it does!");
					reply->test_cd_clear_rej.error =
					        CONN_ESTABLISH_CLIENT_DOES_NOT_EXIST;
					reply->test_cd_clear_rej.data =
					        ((POINTER_TO_UINT_CAST) cd);
				} else {
					snprintf(reply->test_cd_clear_rej.text, 100,
					         "clear_client_data correctly claims "
					         "CONN_ESTABLISH_CLIENT_DOES_NOT_EXIST");
					reply->test_cd_clear_rej.error =
					        CONN_ESTABLISH_CLIENT_DOES_NOT_EXIST;
					reply->test_cd_clear_rej.data =
					        ((POINTER_TO_UINT_CAST) cd);

				}
				break;
			}

			if(res == CONN_ESTABLISH_CLIENT_DATA_OK) {
				if(msg->test_cd_clear_req.server_ref !=
				    msg->test_cd_clear_req.connection_ref) {
					reply = itc_alloc(sizeof(struct test_cd_clear_rej),
					                  TEST_CD_CLEAR_REJ);
					snprintf(reply->test_cd_clear_rej.text, 100,
					         "clear_client_data returned "
					         "CONN_ESTABLISH_CLIENT_DATA_OK "
					         "for non-existing client");
					reply->test_cd_clear_rej.error =
					        CONN_ESTABLISH_CLIENT_DATA_OK;
					reply->test_cd_clear_rej.data =
					        cd ? cd->data : 0;
					break;
				}

				reply = itc_alloc(sizeof(struct test_cd_clear_cfm),
				                  TEST_CD_CLEAR_CFM);
				reply->test_cd_clear_cfm.data =
				        cd ? cd->data : 0;
				break;
			}
			reply = itc_alloc(sizeof(struct test_cd_clear_rej),
			                  TEST_CD_CLEAR_REJ);
			snprintf(reply->test_cd_clear_rej.text, 100,
			         "clear_client_data returned unecpected reply (%d)",
			         res);
			reply->test_cd_clear_rej.error = res;
			break;

		case TEST_SET_CONN_REJECT_CAUSE_REQ:
			log_trace("Received TEST_SET_CONN_REJECT_CAUSE_REQ");
			reject_next_conn = msg->test_set_conn_reject_cause_req.reject_cause;
			reply = itc_alloc(sizeof(struct test_set_conn_reject_cause_cfm),
			                  TEST_SET_CONN_REJECT_CAUSE_CFM);
			log_trace("Next conn establish req wil be rejectes with "
			     "reason %d ", reject_next_conn);
			break;
		case TEST_SET_MASTER_MBOX_REQ:
			log_trace("Received TEST_SET_MASTER_MBOX_REQ");
			client_master_mbox = msg->test_set_master_mbox_req.mbox;
			reply = itc_alloc(sizeof(struct test_set_master_mbox_cfm),
			                  TEST_SET_MASTER_MBOX_CFM);
			log_trace("Messages unrelated to existing client will be "
			     "sent to mbox 0x%08x", client_master_mbox );
			break;
		case TEST_GET_CLIENT_INFO_REQ: {
			struct conn_client_info my_ci;
			memset(&my_ci, 0, sizeof(my_ci));
			reply = itc_alloc(sizeof(struct test_get_client_info_rsp),
			                  TEST_GET_CLIENT_INFO_RSP);
			reply->test_get_client_info_rsp.result =
			        conn_get_client_info(handle,
			                             msg->test_get_client_info_req.server_ref,
			                             &my_ci);
			reply->test_get_client_info_rsp.client_ref = my_ci.client_ref;
			break;
		}
		default: {
			char name[64];
			itc_get_name(ci.sender, name, sizeof(name));
			log_trace("Received an unknown message "
			     "(msgno:0x%08x) from client (name:\"%s\", "
			     "mbox_id:0x%08x, "
			     "client_ref:0x%08x, server_ref:0x%08x)",
			     msg->msgno, name, ci.sender,
			     ci.client_ref, msg->any_msg.connection_ref);
			break;
		}
		}
		if(reply) {
			reply->any_msg.procedure_ref = ci.procedure_ref;
			reply->any_msg.connection_ref = ci.client_ref;
			log_trace("Replying with msgno 0x%08x to client 0x%08x",
			     reply->msgno, ci.sender);
			itc_send(&reply, ci.sender, ITC_MY_MBOX);
		}
		if(cd) {
			free(cd);
			cd = NULL;
		}
		itc_free(&msg);
	}
}

static void exit_handler(int sig)
{
	union itc_msg *msg;

	log_info("Received signal 0x%X, terminating", sig);
	msg = itc_alloc(sizeof(uint32_t), EXIT_SIGNAL);
	itc_send(&msg, my_mbox, ITC_MY_MBOX);
}

const char* itc_err_str(int err)
{
	static char unknown_err[30];

	if (err == ITC_EINTERNAL_ERROR)
		return "ITC_EINTERNAL_ERROR";
	if (err == ITC_EALREADY_INITIALISED)
		return "ITC_EALREADY_INITIALISED";
	if (err == ITC_ENS_TO_LONG)
		return "ITC_ENS_TO_LONG";
	if (err == ITC_EOUT_OF_MEMORY)
		return "ITC_EOUT_OF_MEMORY";
	if (err == ITC_ENO_WORLD)
		return "ITC_ENO_WORLD";
	if (err == ITC_EILLEGAL_ALLOC_CFG)
		return "ITC_EILLEGAL_ALLOC_CFG";
	snprintf(unknown_err, sizeof(unknown_err),
		         "<unknown error %d>", err);
	return unknown_err;
}

int main( int argc, char **argv )
{
	int res = 0;
	log_trace("Starting test server");

	res = itc_init(MAILBOX_SIZE, ITC_MALLOC, NULL, ITC_NO_NAMESPACE, 0);
	if(res) {
		const char* err_txt = itc_err_str(res);
		printf("Unable to inizalize ITC!, reason: %s",err_txt);
		log_err("Unable to inizalize ITC!, reason: %s",err_txt);
		exit(1);
	}
	/**
	 * Create our mailbox.
	 */
	my_mbox = itc_create_mailbox(TEST_SERVER_NAME, 0);
	if (my_mbox == ITC_NO_ID) {
		log_trace("Unable to create ITC mailbox \"%s\"!", TEST_SERVER_NAME);
	}

	if (signal(SIGTERM, exit_handler) == SIG_ERR) {
		log_err("Failed to install signal exit handler");
		exit(1);
	}

	main_loop();

	exit(0);
}
