/*
 * Copyright (C) 2015 by Ericsson AB. All rights reserved. The
 * information in this document is the property of Ericsson. Except
 * as specifically authorized in writing by Ericsson, the receiver
 * of this document shall keep the information contained herein
 * confidential and shall protect the same in whole or in part from
 * disclosure and dissemination to third parties. Disclosure and
 * disseminations to the receiver's employees shall only be made on
 * a strict need to know basis.
 */
/*----------------------------  Include files  ------------------------------*/

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <unistd.h>
#include <signal.h>
#include <itc.h>
#include "xcbc_fault_if.h"
#include "evti.h"
#include "conn-establish-helper.h"

#define TRACEPOINT_PROVIDER com_ericsson_xcs_fault_server
#include <tpt_create.h>
#include <tpt.h>

#define MAX_MAILBOX_SIZE 32
#define MAX_NAME_LEN     20
#define EXIT_SIGNAL     0xdeadbeef

/* Fault list element */
struct faults {
	struct faults *next;          /* Next fault */
	struct faults *prev;
	struct xcbc_report_fault data;/* The fault data */
};

struct clients {
	struct clients *next;
	struct clients *prev;
	itc_mbox_id_t   mbox;
};

static struct conn_establish_msg_numbers xcbc_conn_messages = {
	XCBC_FAULT_CONN_ESTABLISH_REQ,
	XCBC_FAULT_CONN_ESTABLISH_CFM,
	XCBC_FAULT_CONN_ESTABLISH_REJ,
	XCBC_FAULT_CONN_DISCONNECT_REQ,
	XCBC_FAULT_CONN_DISCONNECT_CFM,
	XCBC_FAULT_CONN_DISCONNECT_REJ,
	XCBC_FAULT_CONN_MONITOR_FWD
};

static struct faults *fault_list = NULL;
static struct clients *client_list = NULL;
static itc_mbox_id_t fault_server_mbox = ITC_NO_ID;

union itc_msg {
	uint32_t                   msgno;
	conn_any_msg_t             any_msg;
	struct EVTI_DistributeReqS distribute_req;
	struct EVTI_DistributeCfmS distribute_cfm;
	struct EVTI_DistributeRejS distribute_rej;
	XCBC_FAULT_STRUCTS
};

/**
 * Function create_fault_element
 */
static struct faults *create_fault_element(void)
{
	struct faults *new;

	new = (struct faults *) malloc(sizeof(struct faults));
	if (new) {
		new->next = NULL;
		new->prev = NULL;
	}
	return new;
}

/**
 * Function create_client_element
 */
static struct clients *create_client_element(void)
{
	struct clients *new;

	new = (struct clients *) malloc(sizeof(struct clients));
	if (new) {
		new->next = NULL;
		new->prev = NULL;
	}
	return new;
}

/**
 * Function release_fault_list
 */
static void release_fault_list(void)
{
	struct faults *tmp = fault_list;

	while (fault_list != NULL) {
		fault_list = fault_list->next;
		free(tmp);
		tmp = fault_list;
	}
}

/**
 * Function release_fault_element
 */
static void release_fault_element(struct faults *item)
{
	if (item->prev)
		item->prev->next = item->next;
	else
		fault_list = item->next;

	if (item->next)
		item->next->prev = item->prev;

	free(item);
}

/**
 * Function release_client_element
 */
static void release_client_element(struct clients *item)
{
	if (item->prev)
		item->prev->next = item->next;
	else
		client_list = item->next;

	if (item->next)
		item->next->prev = item->prev;

	free(item);
}

/**
 * Function send_fault
 */
static void send_fault(struct faults *fault, itc_mbox_id_t client_mbox)
{
	union itc_msg *msg;

	/* Indication. */
	msg = itc_alloc(sizeof(struct xcbc_fault_ind), XCBC_FAULT_IND);
	msg->fault_ind.faultType = fault->data.fault_type;
	msg->fault_ind.recoveryAction = fault->data.recov_act;
	strncpy(msg->fault_ind.faultDescription,
	        fault->data.fault_description,
	        XCBC_MAX_FAULT_DESCR_LEN);
	msg->fault_ind.faultDescription[XCBC_MAX_FAULT_DESCR_LEN - 1] = '\0';
	TPT_SEND_SIG(msg->msgno, client_mbox,
	             STR("XCBC_FAULT_IND: type=%d recovAct=%d faultDesc=%s",
	                 msg->fault_ind.faultType,
	                 msg->fault_ind.recoveryAction,
	                 msg->fault_ind.faultDescription));

	itc_send(&msg, client_mbox, ITC_MY_MBOX);
}

/**
 * Function handle_attach
 */
static void handle_attach(itc_mbox_id_t sender_mbox)
{
	struct clients *client;

	for (client = client_list; client != NULL; client = client->next) {
		if (client->mbox == sender_mbox) {
			release_client_element(client);
			break;
		}
	}
}
/**
 * Function handle_purge_fault_ind
 * Purge fault list, test only
 */
static void handle_purge_fault_ind(void)
{
	TPT_TRACE(1, "Receive purge fault ind.");
	release_fault_list();
}

/**
 * Function handle_report_fault
 * Forward fault to subscribers
 */
static void handle_report_fault(union itc_msg *msg)
{
	struct clients *client;
	struct faults *fault;
	char client_name[MAX_NAME_LEN];
	int already_present = 0;

	if (itc_size(msg) < sizeof(struct xcbc_report_fault)) {
		TPT_ERROR("corrupt size of report fault message");
		return;
	}

	TPT_REC_SIG(msg->msgno,
	            STR("XCBC_REPORT_FAULT"
	                "FaultInd: type=%d, recovAct=%d, faultDesc=%s",
	                msg->report_fault.fault_type,
	                msg->report_fault.recov_act,
	                msg->report_fault.fault_description));

	/* Check if fault is already present in list */
	for (fault = fault_list; fault != NULL; fault = fault->next) {
		/* Check type and recovery action */
		if (fault->data.fault_type == msg->report_fault.fault_type &&
		    fault->data.recov_act == msg->report_fault.recov_act)
			already_present = 1;
	}

	/* Save fault (if not already present) */
	if (!already_present) {
		/* Create fault element and save in list */
		fault = create_fault_element();
		if (!fault) {
			TPT_INFO("Create fault element failed");
			return;
		}
		fault->data = msg->report_fault;
		fault->next = fault_list;
		if (fault_list != NULL)
			fault_list->prev = fault;
		fault_list = fault;

		/* Trace fault */
		if (itc_get_name(itc_sender(msg), client_name, MAX_NAME_LEN)) {
			TPT_TRACE(1, STR("Warning: FAULT!"
			                 " FaultType: %d. RecovAction: %d."
			                 " Reported by: %s.",
			                 msg->report_fault.fault_type,
			                 msg->report_fault.recov_act,
			                 client_name));
		}
		/* Send fault */
		for (client = client_list; client != NULL; client = client->next) {
			send_fault(fault, client->mbox);
		}
	} else {
		TPT_TRACE(1, "Fault already present in list. Fault discarded!");
	}
}

/**
 * Function handle_subscribe_faults
 * Register the client as a fault subscriber
 */
static void handle_subscribe_faults(union itc_msg *msg, uint32_t client_ref)
{
	itc_mbox_id_t client_mbox = itc_sender(msg);
	union itc_msg *out_msg;
	uint32_t result = XCBC_SUBSCRIBE_FAULTS_OK;
	int already_subscribed = 0;
	struct clients *client;
	struct faults *fault;
	itc_mbox_id_t subscriber_mbox;
	uint32_t counter = 0;

	if (itc_size(msg) < sizeof(struct subscribe_faults_req)) {
		TPT_ERROR("corrupt size of subscribe faults message");
		out_msg = itc_alloc(sizeof(struct subscribe_faults_rej),
		                    XCBC_SUBSCRIBE_FAULTS_REJ);
		out_msg->sub_faults_rej.err_code = XCBC_SUBSCRIBE_FAULTS_NOK_OTHER;
		out_msg->any_msg.connection_ref = client_ref;
		itc_send(&out_msg, client_mbox, ITC_MY_MBOX);
		return;
	}
	TPT_REC_SIG(msg->msgno,
	            STR("subscribe_faults_req: pid=0x%x",
	                msg->sub_faults_req.mbox));

	subscriber_mbox = msg->sub_faults_req.mbox;

	/* Check if client is already a fault subscriber */
	for (client = client_list; client != NULL; client = client->next) {
		if (client->mbox == subscriber_mbox) {
			already_subscribed = 1;
		}
		counter++;
	}

	if (!already_subscribed) {
		if (counter >= XCBC_MAX_NOF_FAULT_SUBSCRIBERS) {
			/* Max number of subscribers reached */
			result = XCBC_SUBSCRIBE_FAULTS_NOK_UNSUPPORTED_CAPABILITY;
		} else {
			/* Register client as subscriber */
			client = create_client_element();
			if (client) {
				client->mbox = subscriber_mbox;
				client->next = client_list;
				if (client_list != NULL)
					client_list->prev = client;
				client_list = client;
				/* Attach to the subscriber if no connection */
				if ((subscriber_mbox != itc_current_mbox()) &&
				    (subscriber_mbox != client_mbox))
					(void) itc_monitor(subscriber_mbox, NULL);
			} else {
				result = XCBC_SUBSCRIBE_FAULTS_NOK_OTHER;
			}
		}
	}

	/* Send result */
	if (result == XCBC_SUBSCRIBE_FAULTS_OK) {
		/* Confirmation. */
		out_msg = itc_alloc(sizeof(struct subscribe_faults_cfm),
		                    XCBC_SUBSCRIBE_FAULTS_CFM);
		TPT_TRACE(1, "subscribe_faults_cfm");
	} else {
		TPT_TRACE(1, "Warning: Subscribe faults failed,"
		             " reach max NO of clients");

		/* Reject. */
		out_msg = itc_alloc(sizeof(struct subscribe_faults_rej),
		                    XCBC_SUBSCRIBE_FAULTS_REJ);
		out_msg->sub_faults_rej.err_code = result;
	}
	out_msg->any_msg.connection_ref = client_ref;
	itc_send(&out_msg, client_mbox, ITC_MY_MBOX);

	/* Send all faults to subsciber if subscription succeded */
	if (result == XCBC_SUBSCRIBE_FAULTS_OK) {
		for (fault = fault_list; fault != NULL; fault = fault->next) {
			send_fault(fault, client_mbox);
		}
	}
}

/**
 * Function handle_fault_clear
 */
static void handle_fault_clear(union itc_msg *msg, uint32_t client_ref)
{
	itc_mbox_id_t  client_mbox = itc_sender(msg);
	struct faults  *fault, *next_fault;
	union itc_msg  *out_msg, *in_msg;
	itc_mbox_id_t  evti_server_mbox;
	uint32_t resp_msg[] = {1, ITC_LOCATE_DEFAULT_NO};

	itc_locate_async(EVTI_SERVER_NAME, NULL, ITC_MY_MBOX);
	in_msg = itc_receive(resp_msg, ITC_NO_TMO, ITC_FROM_ALL);
	evti_server_mbox = itc_sender(in_msg);
	itc_free(&in_msg);
	TPT_TRACE(1, "Found event server mailbox");

	static uint32_t rx_filter[] = {2,
	                               EVTI_DISTRIBUTE_CFM,
	                               EVTI_DISTRIBUTE_REJ
	                              };

	if (itc_size(msg) < sizeof(struct fault_clear_req)) {
		TPT_ERROR("corrupt size of fault clear message");
		out_msg = itc_alloc(sizeof(struct fault_clear_rej),
		                    XCBC_FAULT_CLEAR_REJ);
		out_msg->clear_rej.err_code = XCBC_FAULT_CLEAR_NOK_OTHER;
		out_msg->any_msg.connection_ref = client_ref;
		itc_send(&out_msg, client_mbox, ITC_MY_MBOX);
		return;
	}
	TPT_REC_SIG(msg->msgno,
	            STR("fault_clear_req: faultType=%d recoveryAction=%d",
	                msg->clear_req.fault_type,
	                msg->clear_req.recov_act));

	/* Distribute FaultClear. */
	out_msg = itc_alloc(sizeof(struct EVTI_DistributeReqS),
	                    EVTI_DISTRIBUTE_REQ);
	strcpy(out_msg->distribute_req.tag, XCBC_XMR_TAG_FAULT_CLEAR);
	strcpy(out_msg->distribute_req.data, "");

	TPT_SEND_SIG(msg->msgno, evti_server_mbox,
	             STR("DistributeReq: tag=%s data=%s",
	                 out_msg->distribute_req.tag,
	                 out_msg->distribute_req.data));
	itc_send(&out_msg, evti_server_mbox, ITC_MY_MBOX);

	in_msg = itc_receive(rx_filter, ITC_NO_TMO, ITC_FROM_ALL);

	if (in_msg->msgno == EVTI_DISTRIBUTE_CFM) {
		TPT_REC_SIG(msg->msgno,
		            STR("DistributeCfm: noSubsc=%d result=%d",
		                in_msg->distribute_cfm.noOfSubsc,
		                in_msg->distribute_cfm.result));
	} else {
		TPT_REC_SIG(msg->msgno,
		            STR("DistributeRej: noSubsc=%d result=%d",
		                in_msg->distribute_rej.noOfSubsc,
		                in_msg->distribute_rej.result));
	}
	itc_free(&in_msg);

	/* Clear all faults with fault type and recovery action. */
	for (fault = fault_list; fault != NULL; fault = next_fault) {
		next_fault = fault->next;

		if (((msg->clear_req.fault_type == fault->data.fault_type) ||
		     (msg->clear_req.fault_type == XCBC_CLEAR_FAULT_TYPE_ALL)) &&
		    ((msg->clear_req.recov_act == fault->data.recov_act) ||
		     (msg->clear_req.recov_act == XCBC_CLEAR_RECOVERY_ACTION_ALL))) {
			release_fault_element(fault);
		}
	}

	/* Send result */
	out_msg = itc_alloc(sizeof(struct fault_clear_cfm),
	                    XCBC_FAULT_CLEAR_CFM);
	TPT_TRACE(1, "XPP_XCBC_FaultClearCfm");
	out_msg->any_msg.connection_ref = client_ref;
	itc_send(&out_msg, client_mbox, ITC_MY_MBOX);
}

/**
 * Function read_messages
 * Main loop
 */
static void read_messages(conn_server_handle_t handle)
{
	union itc_msg *msg;
	struct conn_client_info client_info;

	for(;;) {
		msg = itc_receive(ITC_NOFILTER, ITC_NO_TMO, ITC_FROM_ALL);

		if (msg->msgno == EXIT_SIGNAL) {
			TPT_INFO("fault_server exiting as ordered");
			itc_free(&msg);
			return;
		}
		/* handle attach in case there is no connection */
		if (msg->msgno == ITC_MONITOR_DEFAULT_NO) {
			handle_attach(itc_sender(msg));
			itc_free(&msg);
			continue;
		}
		/*Handle CONN_ESTABLISH... messages (and messages from
		 unknown clients.*/
		if(!conn_check_client(handle, &msg, &client_info))
			continue;

		switch(msg->msgno) {
		/* Purge fault used only by target test shell cmd*/
		case XCBC_PURGE_FAULTS_IND:
			handle_purge_fault_ind();
			break;
		case XCBC_REPORT_FAULT:
			handle_report_fault(msg);
			break;
		case XCBC_SUBSCRIBE_FAULTS_REQ:
			handle_subscribe_faults(msg, client_info.client_ref);
			break;
		case XCBC_FAULT_CLEAR_REQ:
			handle_fault_clear(msg, client_info.client_ref);
			break;
		default:
			TPT_INFO(STR("Unexpected message received, "
			             "msgno=0x%x, sender=0x%x",
			             msg->msgno, itc_sender(msg)));
			break;
		}
		itc_free(&msg);
	}
}

/**
 * Function client_disconnect
 */
static void client_disconnect(struct conn_client_info *client_info)
{
	handle_attach(client_info->connected_mailbox);
}

/**
 * Function conn_server_init
 */
static conn_server_handle_t conn_server_init(void)
{
	conn_server_handle_t handle;
	struct conn_event_callbacks cb = { NULL, client_disconnect,
		       client_disconnect, NULL
	};
	uint32_t supported_versions[] = {FAULT_SERVER_VERSIONS};
	int conn_result = conn_establish_server_init( &handle,
	                  sizeof(supported_versions) /
	                  sizeof(supported_versions[0]),
	                  supported_versions,
	                  &xcbc_conn_messages, 0, &cb);
	if( conn_result != CONN_INIT_OK)
		return NULL;

	return handle;
}


/**
 * Function print_usage
 */
static void print_usage()
{
	printf("Usage: fault_server <options>\n\n"
	       "Options:\n"
	       "    -h  Display usage information (this message).\n"
	       "    -d  Daemonize the program.\n\n");
}

/**
 * Function exit_handler
 */
static void exit_handler(int sig)
{
	union itc_msg *msg;

	TPT_INFO(STR("Receive signal 0x%X, terminating", sig));
	msg = itc_alloc(sizeof(uint32_t), EXIT_SIGNAL);
	itc_send(&msg, fault_server_mbox, ITC_MY_MBOX);
}

/**
 * Main function
 */
int main( int argc, char **argv )
{
	int daemonize = 0;
	void *handle;

	if (argc > 1) {
		if (strcmp("-d", argv[1]) == 0) {
			daemonize = 1;
		}
		else if (strcmp("-h", argv[1]) == 0) {
			print_usage();
			exit(0);
		}
		else {
			print_usage();
			exit(1);
		}
	}

	/* Initialize logging */
	TPT_INFO("Starting fault server");

	if (!daemonize || !daemon(0, 0)) {

		/* Initialize connection establish server */
		handle = conn_server_init();
		if (handle == NULL) {
			TPT_ERROR("Unable to initialize conn_establish server!");
			return -1;
		}
		/* Initialize ITC */
		if (itc_init(MAX_MAILBOX_SIZE, ITC_MALLOC,
		             NULL, ITC_NO_NAMESPACE, 0)) {
			TPT_ERROR("Unable to initialize ITC!");
			return -1;
		}
		fault_server_mbox = itc_create_mailbox(FAULT_SERVER_MAILBOX, 0);
		if (fault_server_mbox == ITC_NO_ID) {
			TPT_ERROR(STR("%s: Unable to create ITC mailbox!",
			              FAULT_SERVER_MAILBOX));
		}
		if (signal(SIGTERM, exit_handler) == SIG_ERR) {
			TPT_ERROR("Failed to install signal exit handler");
			exit(1);
		}
		read_messages(handle);
	}
	return 0;
}
