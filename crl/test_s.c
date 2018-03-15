/* ---------------------------------------------------------------------------
 *
 * © Ericsson AB 2014 All rights reserved.
 * The information in this document is the property of Ericsson.  Except
 * as specifically authorized in writing by Ericsson, the receiver of
 * this document shall keep the information contained herein confidential
 * and shall protect the same in whole or in part from disclosure and
 * dissemination to third parties.  Disclosure and disseminations to the
 * receivers employees shall only be made on a strict need to know basis.
 *
 * ---------------------------------------------------------------------------
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdarg.h>
#include <errno.h>
#include <string.h>
#include <time.h>
#include <pthread.h>
#include <unistd.h>
#include <arpa/inet.h>
#include <itc.h>
#include <itc_system.h>

#include "rolc-link-api.h"
#include "rolc-link-int.h"


union itc_msg {
	uint32_t						msgno;
	struct rolc_linkstate_ind		lkind;
	struct rolc_test_done			tdone;
	struct rolc_test_create_link	clink;
	struct rolc_test_destroy_link	dlink;
	struct rolc_test_create_mbox	cmbox;
	struct rolc_test_destroy_mbox	dmbox;
	struct rolc_test_locate_mbox	lcate;
};

void handle_rolc_create_link(struct test_args *targ, union itc_msg *msg)
{
	struct rolc_link_config cfg;
	int ret;

	memset(&cfg, 0, sizeof(cfg));
	cfg.ri_addr = msg->clink.addr;
	cfg.mode 	= msg->clink.mode;

	ret = rolc_link_setup(	targ->rolc_handle,
							msg->clink.link_name,
							&cfg,
							&msg->clink.linkid
						 );
	if (ret)
		msg->clink.linkid = (uint32_t)-1;

	msg->msgno = ROLC_TEST_CREATE_LINK_RSP;
	itc_send(&msg, itc_sender(msg), ITC_MY_MBOX);
}

void handle_rolc_destroy_link(struct test_args *targ, union itc_msg *msg)
{
	msg->dlink.result = rolc_link_destroy(	targ->rolc_handle,
											msg->dlink.linkid
										 );

	msg->msgno = ROLC_TEST_DESTROY_LINK_RSP;
	itc_send(&msg, itc_sender(msg), ITC_MY_MBOX);
}

void handle_rolc_create_mbox(struct test_args *targ, union itc_msg *msg)
{
	msg->cmbox.mail_box = itc_clone_mailbox(ITC_MY_MBOX, msg->cmbox.mbox_name);
	msg->msgno = ROLC_TEST_CREATE_MBOX_RSP;
	itc_send(&msg, itc_sender(msg), ITC_MY_MBOX);
}

void handle_rolc_destroy_mbox(struct test_args *targ, union itc_msg *msg)
{
	if (msg->dmbox.mail_box != ITC_NO_ID)
		itc_delete_mailbox(msg->dmbox.mail_box);

	msg->msgno = ROLC_TEST_DESTROY_MBOX_RSP;
	itc_send(&msg, itc_sender(msg), ITC_MY_MBOX);
}

void handle_rolc_locate_mbox(struct test_args *targ, union itc_msg *msg)
{
	msg->msgno = ROLC_TEST_LOCATE_MBOX_RSP;
	itc_locate_async(msg->lcate.mbox_name, &msg, itc_sender(msg));
}

void *test_prog_s(void *ctx)
{
	uint32_t select[2] = {1, ITC_LOCATE_DEFAULT_NO};
	struct test_args *targ = ctx;
	struct rolc_link_config cfg;
	struct rolc_if *handle;
	union itc_msg *msg;
	itc_mbox_id_t me;
	uint32_t link_id;
	int ret = -1;

	memset(&cfg, 0, sizeof(cfg));

	me = itc_create_mailbox("rolc_test_slave", 0);
	if (me == ITC_NO_ID) {
		printf("created mbox failure: %d\n",me);
		goto get_out;
	}

	ret = rolc_init(&handle);
	if (ret) {
		printf("rolc_link_init failed, %d\n", ret);
		goto get_out;
	}

	cfg.ri_addr = 0;
	cfg.mode = ROLC_SLAVE;

	ret = rolc_link_setup(handle, "master", &cfg, &link_id);
	if (ret) {
		printf("rolc_link_setup, %d\n", ret);
		goto get_out;
	}

	itc_locate_async("master/rolc_test_master", NULL, ITC_MY_MBOX);
	msg = itc_receive(select, 5000, ITC_FROM_ALL);

	if (msg == NULL) {
		printf("failed to locate peer: master/rolc_test_master\n");
		goto get_out;
	}

	targ->test_mbox = me;
	targ->peer_mbox = itc_sender(msg);
	targ->rolc_handle = handle;
	itc_free(&msg);

	printf("Master located (0x%x)\n", targ->peer_mbox);


	for (;;) {
		msg = itc_receive(ITC_NOFILTER, ITC_NO_TMO, ITC_FROM_ALL);
		switch (msg->msgno) {

		case ROLC_LINKSTATE_IND:
			if ((msg->lkind.link_id == link_id) &&
				(msg->lkind.available == 0)) {
				printf("Link to peer lost\n");
				goto get_out;
			}
			break;

		case ROLC_TEST_CREATE_LINK:
			handle_rolc_create_link(targ, msg);
			continue;

		case ROLC_TEST_DESTROY_LINK:
			handle_rolc_destroy_link(targ, msg);
			continue;

		case ROLC_TEST_CREATE_MBOX:
			handle_rolc_create_mbox(targ, msg);
			continue;

		case ROLC_TEST_DESTROY_MBOX:
			handle_rolc_destroy_mbox(targ, msg);
			continue;

		case ROLC_TEST_LOCATE_MBOX:
			handle_rolc_locate_mbox(targ, msg);
			continue;

		case ROLC_TEST_PING:
			msg->msgno = ROLC_TEST_PONG;
			itc_send(&msg, itc_sender(msg), me);
			continue;

		default:
			break;
		}
		itc_free(&msg);
	}

 get_out:

	rolc_shutdown(&handle);
	msg = itc_alloc(sizeof(msg->tdone), ROLC_TEST_DONE);
	msg->tdone.result = 0;
	itc_send(&msg, targ->parent_mbox, me);
	return NULL;
}
