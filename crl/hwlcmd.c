/**
 *   Copyright (C) 2014 by Ericsson AB. All rights reserved. The
 *   information in this document is the property of Ericsson. Except
 *   as specifically authorized in writing by Ericsson, the receiver
 *   of this document shall keep the information contained herein
 *   confidential and shall protect the same in whole or in part from
 *   disclosure and dissemination to third parties. Disclosure and
 *   disseminations to the receiver's employees shall only be made on
 *   a strict need to know basis.
 */

#include <stdio.h>
#include <stdlib.h>
#include <itc.h>
#include <string.h>

#include "hwli.h"
#include "hwlog.h"

#define ERR(fmt, args...)				\
	fprintf(stderr, "Error: " fmt "\n", ## args)
#define MSG(fmt, args...)			\
	fprintf(stdout, fmt "\n", ## args)

#define MAILBOX_SIZE 1

union itc_msg {
	struct {
		uint32_t msgno;
		uint32_t procedure_reference;
		uint32_t connection_reference;
	};
	HWLOG_SIGNAL_STRUCTS;
};

void usage()
{
	MSG("Usage: hwlog <read|delete>\n"
	    "   read   - read entries from hwlog\n"
	    "   delete - delete all entries from hwlog (erase the log)\n");
}

void readlog()
{
	struct hwli_entry *entries;
	uint32_t size;

	if (hwli_readlog(&entries, &size) != HWLI_SUCCESS) {
		ERR("failed reading log");
		exit(1);
	}

	MSG(" no   logid  time               msg");
	MSG("--------------------------------------------");
	for (int i = 0; i < size; i++) {
		char nicetime[HWLI_TIMEDATE_SZ+5];
		struct hwli_entry *e = &entries[i];
		char *t = e->time;

		sprintf(nicetime, "%.2s-%.2s-%.2s %.2s:%.2s:%.2s",
	            &t[0], &t[2], &t[4], &t[6], &t[8], &t[10]);


		MSG("%4d  %4s   %16s  %s", i+1, e->id, nicetime, e->msg);
	}
}

void connect_to_server(itc_mbox_id_t *mbox, uint32_t *connection_reference)
{
	union itc_msg *msg, *reply;
	itc_mbox_id_t hwld_mbox = itc_locate(HWLOG_MAILBOX);

	if (hwld_mbox == ITC_NO_ID) {
		ERR("Failed to locate hwlog mailbox: %s", HWLOG_MAILBOX);
		exit(1);
	}

	msg = itc_alloc(sizeof(struct hwlog_connect_req),
	                    HWLOG_CONNECT_REQ);
	msg->hwlog_connect_req.nbr_of_supported_protocol_revisions = 1;
	msg->hwlog_connect_req.protocol_revision = HWLOG_PROTOCOL_REVISION;
	itc_send(&msg, hwld_mbox, ITC_MY_MBOX);

	reply = itc_receive(ITC_NOFILTER, ITC_NO_TMO, hwld_mbox);
	if (reply->msgno != HWLOG_CONNECT_CFM) {
		ERR("failed to connect to hwlog mailbox %s", HWLOG_MAILBOX);
		exit(1);
	}

	*mbox = hwld_mbox;
	*connection_reference = reply->connection_reference;
	itc_free(&reply);
}

void eraselog()
{
	itc_mbox_id_t mbox;
	union itc_msg *reply, *msg = itc_alloc(sizeof(struct hwlog_logerase_req),
	                                       HWLOG_LOGERASE_REQ);

	connect_to_server(&mbox, &msg->connection_reference);
	itc_send(&msg, mbox, ITC_MY_MBOX);
	reply = itc_receive(ITC_NOFILTER, ITC_NO_TMO, mbox);
	if (reply->msgno == HWLOG_LOGERASE_REJ) {
		ERR("erase log request was rejected");
		exit(1);
	}
	itc_free(&reply);
}

int main(int argc, char *argv[])
{
	if (argc < 2) {
		usage();
		exit(1);
	}

	/*
	 * The application needs to initialize itc and create a mailbox for the
	 * thread before using the hwli interface.
	 */
	if (itc_init(MAILBOX_SIZE, ITC_MALLOC, NULL, ITC_NO_NAMESPACE, 0)) {
		ERR("Failed to initialize itc");
		exit(1);
	}

	if (itc_create_mailbox("hwlcmd", 0) == ITC_NO_ID) {
		ERR("Failed to create itc mailbox");
		exit(1);
	}

	if (strcmp(argv[1], "delete") == 0) {
		eraselog();
	}
	else if (strcmp(argv[1], "read") == 0) {
		readlog();
	}
	else {
		usage();
	}

	return 0;
}
