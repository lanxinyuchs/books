/* ---------------------------------------------------------------------------
 *
 * © Ericsson AB 2014 All rights reserved.
 * The information in this document is the property of Ericsson. Except
 * as specifically authorized in writing by Ericsson, the receiver of
 * this document shall keep the information contained herein confidential
 * and shall protect the same in whole or in part from disclosure and
 * dissemination to third parties. Disclosure and disseminations to the
 * receivers employees shall only be made on a strict need to know basis.
 *
 * ---------------------------------------------------------------------------
 */

#include <stdint.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <syslog.h>
#include <ctype.h>
#include <sys/types.h>
#include <itc.h>

#include "eolc-link-api.h"
#include "eolc-link-internal.h"

#define __debug_print(...)					\
        {									\
			fprintf(stderr, __VA_ARGS__);	\
			fprintf(stderr, "\n");			\
        }


#define RIOLNH_VERBOSE
#if defined(RIOLNH_VERBOSE)
#define DBG(...) __debug_print(__VA_ARGS__)
#else
#define DBG(...)
#endif


union itc_msg {
	uint32_t         		 			msgno;

	struct eolc_create_req				ctreq;
	struct eolc_create_rsp				ctrsp;
	struct eolc_delete_req				dsreq;
	struct eolc_delete_rsp				dsrsp;
	struct eolc_linkstate_ind			lkind;
};




static void handle_create(union itc_msg *cfg)
{
	union itc_msg *msg;

	msg = itc_alloc(sizeof(msg->ctrsp), EOLC_CREATE_RSP);
	msg->ctrsp.linkid = cfg->ctreq.config.g2.emca_id;
	msg->ctrsp.result = 0;
	strncpy(msg->ctrsp.name, cfg->ctreq.name, EOLC_LINKNAME_SIZE);
	itc_send(&msg, itc_sender(cfg), ITC_MY_MBOX);
}

static void handle_destroy(union itc_msg *rem)
{
	union itc_msg *msg;

	msg = itc_alloc(sizeof(msg->dsrsp), EOLC_DELETE_RSP);
	msg->dsrsp.result = 0;
	msg->dsrsp.owner = rem->dsreq.owner;
	msg->dsrsp.linkid = rem->dsreq.linkid;
	itc_send(&msg, itc_sender(rem), ITC_MY_MBOX);
}

int main(int argc, char *argv[])
{
	union itc_msg *msg;
	itc_mbox_id_t me;
	int ret;


	ret = itc_init(32, ITC_MALLOC, NULL, NULL, 0);
	if (ret) {
		syslog(LOG_ERR, "itc_init failure: %d\n",ret);
		return -1;
	}

	me = itc_create_mailbox(EOLC_DAEMON_NAME, 0);
	if (me == ITC_NO_ID) {
		syslog(LOG_ERR, "created mbox failure: %d\n",me);
		itc_exit();
		return -1;
	}

	for(;;) {
		msg = itc_receive(ITC_NOFILTER, ITC_NO_TMO, ITC_FROM_ALL); 
		switch(msg->msgno) {
			case EOLC_CREATE_REQ:
				handle_create(msg);
				break;

			case EOLC_DELETE_REQ:
				handle_destroy(msg);
				break;

			case ITC_MONITOR_DEFAULT_NO:
				break;

			default:
				break;
		}
		itc_free(&msg);
	}
	return -1;
}


