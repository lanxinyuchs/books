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

#include <ulh_lnh_msg.h>


#include "rolc-link-api.h"
#include "rolc-link-int.h"

union itc_msg {
	uint32_t						msgno;
	struct rolc_test_done			tdone;
	struct rolc_linkstate_ind		lkind;
	struct rolc_test_create_link	clink;
	struct rolc_test_destroy_link	dlink;
	struct rolc_test_create_mbox	cmbox;
	struct rolc_test_destroy_mbox	dmbox;
	struct rolc_test_locate_mbox	lcate;
};

int remote_create_link(struct test_args *targ, char *link_name,
						uint32_t mode, uint32_t addr, uint32_t *link_id)
{
	uint32_t select[2] = {1, ROLC_TEST_CREATE_LINK_RSP};
	union itc_msg *msg;
	int ret = -1;

	msg = itc_alloc(sizeof(msg->clink), ROLC_TEST_CREATE_LINK);
	msg->clink.addr = addr;
	msg->clink.mode = mode;
	strncpy(msg->clink.link_name, link_name, ROLC_LINKNAME_SIZE);

	itc_send(&msg, targ->peer_mbox, ITC_MY_MBOX);
	msg = itc_receive(select, 3000, ITC_FROM_ALL);

	if (msg == 0)
		return ret;

	if (msg->clink.linkid != (uint32_t)-1) {
		*link_id = msg->clink.linkid;
		ret = 0;
	}
	
	itc_free(&msg);
	return ret;
}

void remote_rolc_destroy_link(struct test_args *targ, uint32_t linkid)
{
	uint32_t select[2] = {1, ROLC_TEST_DESTROY_LINK_RSP};
	union itc_msg *msg;

	msg = itc_alloc(sizeof(msg->dlink), ROLC_TEST_DESTROY_LINK);
	msg->clink.linkid = linkid;

	itc_send(&msg, targ->peer_mbox, ITC_MY_MBOX);
	msg = itc_receive(select, 3000, ITC_FROM_ALL);

	if (msg)
		itc_free(&msg);
}

itc_mbox_id_t
remote_rolc_create_mbox(struct test_args *targ, char *mbox_name)
{
	uint32_t select[2] = {1, ROLC_TEST_CREATE_MBOX_RSP};
	itc_mbox_id_t mbox = ITC_NO_ID;
	union itc_msg *msg;

	msg = itc_alloc(sizeof(msg->cmbox), ROLC_TEST_CREATE_MBOX);
	strncpy(msg->cmbox.mbox_name, mbox_name, ROLC_LINKNAME_SIZE);

	itc_send(&msg, targ->peer_mbox, ITC_MY_MBOX);
	msg = itc_receive(select, 3000, ITC_FROM_ALL);

	if (msg) {
		mbox = msg->cmbox.mail_box;
		itc_free(&msg);
	}
	return mbox;
}

void remote_rolc_destroy_mbox(struct test_args *targ, itc_mbox_id_t mbox)
{
	uint32_t select[2] = {1, ROLC_TEST_DESTROY_MBOX_RSP};
	union itc_msg *msg;

	msg = itc_alloc(sizeof(msg->dmbox), ROLC_TEST_DESTROY_MBOX);
	msg->dmbox.mail_box = mbox;

	itc_send(&msg, targ->peer_mbox, ITC_MY_MBOX);
	msg = itc_receive(select, 3000, ITC_FROM_ALL);

	if (msg)
		itc_free(&msg);
}

itc_mbox_id_t remote_rolc_locate_mbox(struct test_args *targ, char *name)
{
	uint32_t select[2] = {1, ITC_LOCATE_DEFAULT_NO};
	itc_mbox_id_t mbox = ITC_NO_ID;
	union itc_msg *msg;

	msg = itc_alloc(sizeof(msg->lcate), ROLC_TEST_LOCATE_MBOX);
	strncpy(msg->lcate.mbox_name, name, ROLC_LINKNAME_SIZE);

	itc_send(&msg, targ->peer_mbox, ITC_MY_MBOX);
	msg = itc_receive(select, 3000, ITC_FROM_ALL);

	if (msg) {
		mbox = itc_sender(msg);
		itc_free(&msg);
	}
	return mbox;
}

static void *usend_thread(void *ctx)
{
	struct test_args *targ = ctx;
	union itc_msg *msg;
	itc_mbox_id_t me;
	char mname[32];

	sprintf(mname, "usend_%d", rand());

	me = itc_create_mailbox(mname, 0);
	if (me == ITC_NO_ID) {
		printf("created mbox failure: %d\n",me);
		return NULL;
	}

	msg = itc_alloc(24, ROLC_TEST_PING);
	itc_send(&msg, targ->peer_mbox, me);
	msg = itc_receive(ITC_NOFILTER, ITC_NO_TMO, ITC_FROM_ALL);
	itc_send(&msg, targ->work_mbox, me);	
	itc_delete_mailbox(me);
	return targ;
}

static int test_unsolicited_send(struct test_args *targ)
{
	uint32_t select[2] = {1, ROLC_TEST_PONG};
	union itc_msg *msg;
	pthread_t thrd[20];
	int cnt;

	for (cnt = 0; cnt < 20; cnt++)
		if (pthread_create(&thrd[cnt], NULL, usend_thread, targ)) {
			printf("create test thread, %d\n", -errno);
			return -1;
		}

	for (cnt = 0; cnt < 20; cnt++) {
		msg = itc_receive(select, 1000, ITC_FROM_ALL);
		if (msg == NULL) {
			printf("Error @line %d\n", __LINE__);
			return -1;
		}
		itc_free(&msg);
	}

	for (cnt = 0; cnt < 20; cnt++)
		pthread_join(thrd[cnt], NULL);

	return 0;
}

static int test_locate_10(struct test_args *targ)
{
	uint32_t select[2] = {1, ITC_LOCATE_DEFAULT_NO};
	uint32_t lnkind[2] = {1, ROLC_LINKSTATE_IND};

	struct rolc_link_config cfg;
	union itc_msg *msg;
	itc_mbox_id_t mbox[5];
	char link_name[32];
	char rlink_name[32];
	char mbox_name[5][32];
	char hunt_name[64];
	uint32_t link_id, rlink_id;
	int cnt, ret = -1;

	sprintf(link_name, "link_%d", rand());
	sprintf(rlink_name, "link_%d", rand());

	sprintf(mbox_name[0], "mbox_%d", rand());
	sprintf(mbox_name[1], "mbox_%d", rand());
	sprintf(mbox_name[2], "mbox_%d", rand());
	sprintf(mbox_name[3], "mbox_%d", rand());
	sprintf(mbox_name[4], "mbox_%d", rand());


	/*
	** create link on local side
	** create link on remote end
	** Issue async locate
	** disconnect link
	** create local mbox 
	** recover link on local side
	*/

	sprintf(hunt_name, "%s/%s/%s", link_name, rlink_name, mbox_name[0]);
	itc_locate_async(hunt_name, NULL, ITC_MY_MBOX);

	memset(&cfg, 0, sizeof(cfg));
	cfg.mode = ROLC_SLAVE;
	cfg.ri_addr = 10;

	ret = rolc_link_setup(targ->rolc_handle, link_name,
							&cfg, &link_id);
	if (ret) {
		printf("Error %d @line %d\n", ret, __LINE__);
		return ret;
	}

	ret = remote_create_link(targ, rlink_name, ROLC_MASTER, 10,
								&rlink_id);
	if (ret) {
		printf("Error %d @line %d\n", ret, __LINE__);
		return ret;
	}

	sprintf(hunt_name, "%s/%s/%s", link_name, rlink_name, mbox_name[1]);
	itc_locate_async(hunt_name, NULL, ITC_MY_MBOX);

	msg = itc_receive(lnkind, 3000, ITC_FROM_ALL);
	if (msg == NULL) {
		printf("Error @line %d\n", __LINE__);
		return -1;
	}

	if (!msg->lkind.available) {
		printf("Error @line %d\n", __LINE__);
		return -1;
	}
	itc_free(&msg);

	sprintf(hunt_name, "%s/%s/%s", link_name, rlink_name, mbox_name[2]);
	itc_locate_async(hunt_name, NULL, ITC_MY_MBOX);

	mbox[1] = itc_clone_mailbox(ITC_MY_MBOX, mbox_name[1]);
	if (mbox[1] == ITC_NO_ID) {
		printf("Error @line %d\n", __LINE__);
		return ret;
	}
	mbox[2] = itc_clone_mailbox(ITC_MY_MBOX, mbox_name[2]);
	if (mbox[2] == ITC_NO_ID) {
		printf("Error @line %d\n", __LINE__);
		return ret;
	}

	set_rx_skip_mode(10, 1);	
	msg = itc_receive(lnkind, 3000, ITC_FROM_ALL);
	if (msg == NULL) {
		printf("Error @line %d\n", __LINE__);
		return -1;
	}

	if (msg->lkind.available) {
		printf("Error @line %d\n", __LINE__);
		return -1;
	}
	itc_free(&msg);

	sprintf(hunt_name, "%s/%s/%s", link_name, rlink_name, mbox_name[3]);
	itc_locate_async(hunt_name, NULL, ITC_MY_MBOX);

	mbox[0] = itc_clone_mailbox(ITC_MY_MBOX, mbox_name[0]);
	if (mbox[0] == ITC_NO_ID) {
		printf("Error @line %d\n", __LINE__);
		return ret;
	}

	mbox[3] = itc_clone_mailbox(ITC_MY_MBOX, mbox_name[3]);
	if (mbox[3] == ITC_NO_ID) {
		printf("Error @line %d\n", __LINE__);
		return ret;
	}

	set_rx_skip_mode(10, 0);
	msg = itc_receive(lnkind, 3000, ITC_FROM_ALL);
	if (msg == NULL) {
		printf("Error @line %d\n", __LINE__);
		return -1;
	}

	mbox[4] = itc_clone_mailbox(ITC_MY_MBOX, mbox_name[4]);
	if (mbox[4] == ITC_NO_ID) {
		printf("Error @line %d\n", __LINE__);
		return ret;
	}

	sprintf(hunt_name, "%s/%s/%s", link_name, rlink_name, mbox_name[4]);
	itc_locate_async(hunt_name, NULL, ITC_MY_MBOX);

	if (!msg->lkind.available) {
		printf("Error @line %d\n", __LINE__);
		return -1;
	}
	itc_free(&msg);

	for (cnt = 0; cnt < 5; cnt++) {
		msg = itc_receive(select, 3000, ITC_FROM_ALL);
		if (msg == NULL) {
			printf("Error @line %d (cnt: %d)\n", __LINE__, cnt);
			return -1;
		}
		itc_free(&msg);
	}

	itc_delete_mailbox(mbox[0]);
	itc_delete_mailbox(mbox[1]);
	itc_delete_mailbox(mbox[2]);
	itc_delete_mailbox(mbox[3]);
	itc_delete_mailbox(mbox[4]);

	remote_rolc_destroy_link(targ, rlink_id);
	ret = rolc_link_destroy(targ->rolc_handle, link_id);
	if (ret) {
		printf("Error %d @line %d\n", ret, __LINE__);
		return ret;
	}

	msg = itc_receive(lnkind, 3000, ITC_FROM_ALL);
	if (msg == NULL) {
		printf("Error @line %d\n", __LINE__);
		return -1;
	}
	if (msg->lkind.available) {
		printf("Error @line %d\n", __LINE__);
		return -1;
	}
	itc_free(&msg);
	return 0;
}

static int test_locate_9(struct test_args *targ)
{
	uint32_t select[2] = {1, ITC_LOCATE_DEFAULT_NO};
	uint32_t lnkind[2] = {1, ROLC_LINKSTATE_IND};

	struct rolc_link_config cfg;
	union itc_msg *msg;
	itc_mbox_id_t mbox;
	char link_name[32];
	char rlink_name[32];
	char mbox_name[32];
	char hunt_name[64];
	uint32_t link_id, rlink_id;
	int ret = -1;

	sprintf(link_name, "link_%d", rand());
	sprintf(rlink_name, "link_%d", rand());
	sprintf(mbox_name, "mbox_%d", rand());
	sprintf(hunt_name, "%s/%s/%s", link_name, rlink_name, mbox_name);


	/*
	** Issue async locate
	** create link on local side
	** create link on remote end
	** disconnect link
	** create local mbox 
	** recover link on local side
	*/

	itc_locate_async(hunt_name, NULL, ITC_MY_MBOX);

	memset(&cfg, 0, sizeof(cfg));
	cfg.mode = ROLC_SLAVE;
	cfg.ri_addr = 9;

	ret = rolc_link_setup(targ->rolc_handle, link_name,
							&cfg, &link_id);
	if (ret) {
		printf("Error %d @line %d\n", ret, __LINE__);
		return ret;
	}

	ret = remote_create_link(targ, rlink_name, ROLC_MASTER, 9,
								&rlink_id);
	if (ret) {
		printf("Error %d @line %d\n", ret, __LINE__);
		return ret;
	}

	msg = itc_receive(lnkind, 3000, ITC_FROM_ALL);
	if (msg == NULL) {
		printf("Error @line %d\n", __LINE__);
		return -1;
	}

	if (!msg->lkind.available) {
		printf("Error @line %d\n", __LINE__);
		return -1;
	}
	itc_free(&msg);

	set_rx_skip_mode(9, 1);	
	msg = itc_receive(lnkind, 3000, ITC_FROM_ALL);
	if (msg == NULL) {
		printf("Error @line %d\n", __LINE__);
		return -1;
	}

	if (msg->lkind.available) {
		printf("Error @line %d\n", __LINE__);
		return -1;
	}
	itc_free(&msg);

	mbox = itc_clone_mailbox(ITC_MY_MBOX, mbox_name);
	if (mbox == ITC_NO_ID) {
		printf("Error @line %d\n", __LINE__);
		return ret;
	}

	set_rx_skip_mode(9, 0);
	msg = itc_receive(lnkind, 3000, ITC_FROM_ALL);
	if (msg == NULL) {
		printf("Error @line %d\n", __LINE__);
		return -1;
	}

	if (!msg->lkind.available) {
		printf("Error @line %d\n", __LINE__);
		return -1;
	}
	itc_free(&msg);

	msg = itc_receive(select, 3000, ITC_FROM_ALL);
	if (msg == NULL) {
		printf("Error @line %d\n", __LINE__);
		return -1;
	}

	itc_free(&msg);
	itc_delete_mailbox(mbox);
	remote_rolc_destroy_link(targ, rlink_id);

	ret = rolc_link_destroy(targ->rolc_handle, link_id);
	if (ret) {
		printf("Error %d @line %d\n", ret, __LINE__);
		return ret;
	}

	msg = itc_receive(lnkind, 3000, ITC_FROM_ALL);
	if (msg == NULL) {
		printf("Error @line %d\n", __LINE__);
		return -1;
	}
	if (msg->lkind.available) {
		printf("Error @line %d\n", __LINE__);
		return -1;
	}
	itc_free(&msg);
	return 0;
}

static int test_locate_8(struct test_args *targ)
{
	uint32_t select[2] = {1, ITC_LOCATE_DEFAULT_NO};
	uint32_t lnkind[2] = {1, ROLC_LINKSTATE_IND};

	struct rolc_link_config cfg;
	union itc_msg *msg;
	itc_mbox_id_t mbox;
	char link_name[32];
	char rlink_name[32];
	char mbox_name[32];
	char hunt_name[64];
	uint32_t link_id, rlink_id;
	int ret = -1;

	sprintf(link_name, "link_%d", rand());
	sprintf(rlink_name, "link_%d", rand());
	sprintf(mbox_name, "mbox_%d", rand());
	sprintf(hunt_name, "%s/%s/%s", link_name, rlink_name, mbox_name);


	/*
	** create link on local side
	** create link on remote end
	** Issue async locate
	** create local mbox 
	*/

	memset(&cfg, 0, sizeof(cfg));
	cfg.mode = ROLC_SLAVE;
	cfg.ri_addr = 8;

	ret = rolc_link_setup(targ->rolc_handle, link_name,
							&cfg, &link_id);
	if (ret) {
		printf("Error %d @line %d\n", ret, __LINE__);
		return ret;
	}

	ret = remote_create_link(targ, rlink_name, ROLC_MASTER, 8,
								&rlink_id);
	if (ret) {
		printf("Error %d @line %d\n", ret, __LINE__);
		return ret;
	}

	msg = itc_receive(lnkind, 3000, ITC_FROM_ALL);
	if (msg == NULL) {
		printf("Error @line %d\n", __LINE__);
		return -1;
	}

	if (!msg->lkind.available) {
		printf("Error @line %d\n", __LINE__);
		return -1;
	}
	itc_free(&msg);

	itc_locate_async(hunt_name, NULL, ITC_MY_MBOX);

	mbox = itc_clone_mailbox(ITC_MY_MBOX, mbox_name);
	if (mbox == ITC_NO_ID) {
		printf("Error @line %d\n", __LINE__);
		return ret;
	}

	msg = itc_receive(select, 3000, ITC_FROM_ALL);
	if (msg == NULL) {
		printf("Error @line %d\n", __LINE__);
		return -1;
	}

	itc_free(&msg);
	itc_delete_mailbox(mbox);
	remote_rolc_destroy_link(targ, rlink_id);

	ret = rolc_link_destroy(targ->rolc_handle, link_id);
	if (ret) {
		printf("Error %d @line %d\n", ret, __LINE__);
		return ret;
	}

	msg = itc_receive(lnkind, 3000, ITC_FROM_ALL);
	if (msg == NULL) {
		printf("Error @line %d\n", __LINE__);
		return -1;
	}
	if (msg->lkind.available) {
		printf("Error @line %d\n", __LINE__);
		return -1;
	}
	itc_free(&msg);
	return 0;
}


static int test_locate_7(struct test_args *targ)
{
	uint32_t select[2] = {1, ITC_LOCATE_DEFAULT_NO};
	uint32_t lnkind[2] = {1, ROLC_LINKSTATE_IND};

	struct rolc_link_config cfg;
	union itc_msg *msg;
	itc_mbox_id_t mbox;
	char link_name[32];
	char mbox_name[32];
	char hunt_name[64];
	uint32_t link_id, rlink_id;
	int ret = -1;

	sprintf(link_name, "link_%d", rand());
	sprintf(mbox_name, "mbox_%d", rand());
	sprintf(hunt_name, "%s/%s", link_name, mbox_name);


	/*
	** create link on local side
	** create link on remote end
	** Issue async locate
	** disconnect link
	** recover link
	** disconnect link
	** recover link
	** create mbox on remote end
	*/

	memset(&cfg, 0, sizeof(cfg));
	cfg.mode = ROLC_SLAVE;
	cfg.ri_addr = 6;

	ret = rolc_link_setup(targ->rolc_handle, link_name,
							&cfg, &link_id);
	if (ret) {
		printf("Error %d @line %d\n", ret, __LINE__);
		return ret;
	}

	ret = remote_create_link(targ, link_name, ROLC_MASTER, 6,
								&rlink_id);
	if (ret) {
		printf("Error %d @line %d\n", ret, __LINE__);
		return ret;
	}

	msg = itc_receive(lnkind, 3000, ITC_FROM_ALL);
	if (msg == NULL) {
		printf("Error @line %d\n", __LINE__);
		return -1;
	}

	if (!msg->lkind.available) {
		printf("Error @line %d\n", __LINE__);
		return -1;
	}

	itc_locate_async(hunt_name, NULL, ITC_MY_MBOX);

	itc_free(&msg);
	set_rx_skip_mode(6, 1);

	msg = itc_receive(lnkind, 3000, ITC_FROM_ALL);
	if (msg == NULL) {
		printf("Error @line %d\n", __LINE__);
		return -1;
	}

	if (msg->lkind.available) {
		printf("Error @line %d\n", __LINE__);
		return -1;
	}


	itc_free(&msg);
	set_rx_skip_mode(6, 0);
	
	msg = itc_receive(lnkind, 3000, ITC_FROM_ALL);
	if (msg == NULL) {
		printf("Error @line %d\n", __LINE__);
		return -1;
	}

	if (!msg->lkind.available) {
		printf("Error @line %d\n", __LINE__);
		return -1;
	}

	itc_free(&msg);
	set_tx_skip_mode(6, 1);

	msg = itc_receive(lnkind, 3000, ITC_FROM_ALL);
	if (msg == NULL) {
		printf("Error @line %d\n", __LINE__);
		return -1;
	}

	if (msg->lkind.available) {
		printf("Error @line %d\n", __LINE__);
		return -1;
	}

	itc_free(&msg);
	set_tx_skip_mode(6, 0);
	
	msg = itc_receive(lnkind, 3000, ITC_FROM_ALL);
	if (msg == NULL) {
		printf("Error @line %d\n", __LINE__);
		return -1;
	}

	if (!msg->lkind.available) {
		printf("Error @line %d\n", __LINE__);
		return -1;
	}
	itc_free(&msg);

	mbox = remote_rolc_create_mbox(targ, mbox_name);
	if (mbox == ITC_NO_ID) {
		printf("Error @line %d\n", __LINE__);
		return ret;
	}

	msg = itc_receive(select, 3000, ITC_FROM_ALL);
	if (msg == NULL) {
		printf("Error @line %d\n", __LINE__);
		return -1;
	}
	itc_free(&msg);

	remote_rolc_destroy_mbox(targ, mbox);
	remote_rolc_destroy_link(targ, rlink_id);

	ret = rolc_link_destroy(targ->rolc_handle, link_id);
	if (ret) {
		printf("Error %d @line %d\n", ret, __LINE__);
		return ret;
	}

	msg = itc_receive(lnkind, 3000, ITC_FROM_ALL);
	if (msg == NULL) {
		printf("Error @line %d\n", __LINE__);
		return -1;
	}
	if (msg->lkind.available) {
		printf("Error @line %d\n", __LINE__);
		return -1;
	}

	itc_free(&msg);
	return 0;
}

static int test_locate_6(struct test_args *targ)
{
	uint32_t lnkind[2] = {1, ROLC_LINKSTATE_IND};
	uint32_t select[2] = {1, ITC_LOCATE_DEFAULT_NO};
	uint32_t montor[2] = {1, ITC_MONITOR_DEFAULT_NO};
	struct rolc_link_config cfg;
	union itc_msg *msg;
	itc_mbox_id_t mbox;
	char link_name[32];
	char mbox_name[32];
	char hunt_name[64];
	uint32_t link_id, rlink_id;
	int ret = -1;

	sprintf(link_name, "link_%d", rand());
	sprintf(mbox_name, "mbox_%d", rand());
	sprintf(hunt_name, "%s/%s", link_name, mbox_name);

	mbox = itc_locate(mbox_name);
	if (mbox != ITC_NO_ID) {
		printf("Local mbox %s unexpectedly found\n", mbox_name);
		return -1;
	}

	mbox = itc_locate(hunt_name);
	if (mbox != ITC_NO_ID) {
		printf("Remote mbox %s unexpectedly found\n", hunt_name);
		return -1;
	}

	/*
	** create mbox on remote end
	** create link on local side
	** create link on remote end
	** Issue async locate
	*/

	mbox = remote_rolc_create_mbox(targ, mbox_name);
	if (mbox == ITC_NO_ID) {
		printf("Error @line %d\n", __LINE__);
		return ret;
	}

	memset(&cfg, 0, sizeof(cfg));
	cfg.mode = ROLC_SLAVE;
	cfg.ri_addr = 6;

	ret = rolc_link_setup(targ->rolc_handle, link_name,
							&cfg, &link_id);
	if (ret) {
		printf("Error %d @line %d\n", ret, __LINE__);
		return ret;
	}

	ret = remote_create_link(targ, link_name, ROLC_MASTER, 6,
									&rlink_id);
	if (ret) {
		printf("Error %d @line %d\n", ret, __LINE__);
		return ret;
	}

	msg = itc_receive(lnkind, 3000, ITC_FROM_ALL);
	if (msg == NULL) {
		printf("Error @line %d\n", __LINE__);
		return -1;
	}

	if (!msg->lkind.available) {
		printf("Error @line %d\n", __LINE__);
		return -1;
	}
	itc_free(&msg);

	itc_locate_async(hunt_name, NULL, ITC_MY_MBOX);

	msg = itc_receive(select, 3000, ITC_FROM_ALL);
	if (msg == NULL) {
		printf("Error @line %d\n", __LINE__);
		return ret;
	}

	(void)itc_monitor(itc_sender(msg), NULL);
	itc_free(&msg);

	remote_rolc_destroy_mbox(targ, mbox);


	msg = itc_receive(montor, 2000, ITC_FROM_ALL);
	if (msg == NULL) {
		printf("Error @line %d\n", __LINE__);
		return ret;
	}
	itc_free(&msg);

	remote_rolc_destroy_link(targ, rlink_id);
	ret = rolc_link_destroy(targ->rolc_handle, link_id);
	if (ret) {
		printf("Error %d @line %d\n", ret, __LINE__);
		return ret;
	}

	msg = itc_receive(lnkind, 3000, ITC_FROM_ALL);
	if (msg == NULL) {
		printf("Error @line %d\n", __LINE__);
		return -1;
	}
	if (msg->lkind.available) {
		printf("Error @line %d\n", __LINE__);
		return -1;
	}
	itc_free(&msg);
	return 0;
}

static int test_locate_5(struct test_args *targ)
{
	uint32_t lnkind[2] = {1, ROLC_LINKSTATE_IND};
	uint32_t select[2] = {1, ITC_LOCATE_DEFAULT_NO};
	struct rolc_link_config cfg;
	union itc_msg *msg;
	itc_mbox_id_t mbox;
	char link_name[32];
	char mbox_name[32];
	char hunt_name[64];
	uint32_t link_id, rlink_id;
	int ret = -1;

	sprintf(link_name, "link_%d", rand());
	sprintf(mbox_name, "mbox_%d", rand());
	sprintf(hunt_name, "%s/%s", link_name, mbox_name);

	mbox = itc_locate(mbox_name);
	if (mbox != ITC_NO_ID) {
		printf("Local mbox %s unexpectedly found\n", mbox_name);
		return -1;
	}

	mbox = itc_locate(hunt_name);
	if (mbox != ITC_NO_ID) {
		printf("Remote mbox %s unexpectedly found\n", hunt_name);
		return -1;
	}

	/*
	** create link on local side
	** create link on remote end
	** Issue async locate
	** create mbox on remote end
	*/

	memset(&cfg, 0, sizeof(cfg));
	cfg.mode = ROLC_SLAVE;
	cfg.ri_addr = 5;

	ret = rolc_link_setup(targ->rolc_handle, link_name,
							&cfg, &link_id);
	if (ret) {
		printf("Error %d @line %d\n", ret, __LINE__);
		return ret;
	}

	ret = remote_create_link(targ, link_name, ROLC_MASTER, 5,
								&rlink_id);
	if (ret) {
		printf("Error %d @line %d\n", ret, __LINE__);
		return ret;
	}

	msg = itc_receive(lnkind, 3000, ITC_FROM_ALL);
	if (msg == NULL) {
		printf("Error @line %d\n", __LINE__);
		return -1;
	}

	if (!msg->lkind.available) {
		printf("Error @line %d\n", __LINE__);
		return -1;
	}
	itc_free(&msg);

	itc_locate_async(hunt_name, NULL, ITC_MY_MBOX);

	mbox = remote_rolc_create_mbox(targ, mbox_name);
	if (mbox == ITC_NO_ID) {
		printf("Error @line %d\n", __LINE__);
		return ret;
	}

	msg = itc_receive(select, 3000, ITC_FROM_ALL);
	if (msg == NULL) {
		printf("Error @line %d\n", __LINE__);
		return ret;
	}

	itc_free(&msg);
	remote_rolc_destroy_mbox(targ, mbox);
	remote_rolc_destroy_link(targ, rlink_id);


	ret = rolc_link_destroy(targ->rolc_handle, link_id);
	if (ret) {
		printf("Error %d @line %d\n", ret, __LINE__);
		return ret;
	}

	msg = itc_receive(lnkind, 3000, ITC_FROM_ALL);
	if (msg == NULL) {
		printf("Error @line %d\n", __LINE__);
		return -1;
	}
	if (msg->lkind.available) {
		printf("Error @line %d\n", __LINE__);
		return -1;
	}
	itc_free(&msg);
	return 0;
}

static int test_locate_4(struct test_args *targ)
{
	uint32_t lnkind[2] = {1, ROLC_LINKSTATE_IND};
	uint32_t select[2] = {1, ITC_LOCATE_DEFAULT_NO};
	struct rolc_link_config cfg;
	union itc_msg *msg;
	itc_mbox_id_t mbox;
	char link_name[32];
	char mbox_name[32];
	char hunt_name[64];
	uint32_t link_id, rlink_id;
	int ret = -1;

	sprintf(link_name, "link_%d", rand());
	sprintf(mbox_name, "mbox_%d", rand());
	sprintf(hunt_name, "%s/%s", link_name, mbox_name);

	mbox = itc_locate(mbox_name);
	if (mbox != ITC_NO_ID) {
		printf("Local mbox %s unexpectedly found\n", mbox_name);
		return -1;
	}

	mbox = itc_locate(hunt_name);
	if (mbox != ITC_NO_ID) {
		printf("Remote mbox %s unexpectedly found\n", hunt_name);
		return -1;
	}

	/*
	** create link on local side
	** Issue async locate
	** create mbox on remote end
	** create link on remote end
	*/

	memset(&cfg, 0, sizeof(cfg));	
	cfg.mode = ROLC_SLAVE;
	cfg.ri_addr = 4;

	ret = rolc_link_setup(targ->rolc_handle, link_name,
							&cfg, &link_id);
	if (ret) {
		printf("Error %d @line %d\n", ret, __LINE__);
		return ret;
	}

	mbox = remote_rolc_create_mbox(targ, mbox_name);
	if (mbox == ITC_NO_ID) {
		printf("Error @line %d\n", __LINE__);
		return ret;
	}

	ret = remote_create_link(targ, link_name, ROLC_MASTER, 4, &rlink_id);
	if (ret) {
		printf("Error %d @line %d\n", ret, __LINE__);
		return ret;
	}

	itc_locate_async(hunt_name, NULL, ITC_MY_MBOX);

	msg = itc_receive(lnkind, 3000, ITC_FROM_ALL);
	if (msg == NULL) {
		printf("Error @line %d\n", __LINE__);
		return -1;
	}

	if (!msg->lkind.available) {
		printf("Error @line %d\n", __LINE__);
		return -1;
	}
	itc_free(&msg);

	msg = itc_receive(select, 3000, ITC_FROM_ALL);
	if (msg == NULL) {
		printf("Error @line %d\n", __LINE__);
		return ret;
	}

	itc_free(&msg);
	remote_rolc_destroy_mbox(targ, mbox);
	remote_rolc_destroy_link(targ, rlink_id);


	ret = rolc_link_destroy(targ->rolc_handle, link_id);
	if (ret) {
		printf("Error %d @line %d\n", ret, __LINE__);
		return ret;
	}

	msg = itc_receive(lnkind, 3000, ITC_FROM_ALL);
	if (msg == NULL) {
		printf("Error @line %d\n", __LINE__);
		return -1;
	}
	if (msg->lkind.available) {
		printf("Error @line %d\n", __LINE__);
		return -1;
	}
	itc_free(&msg);
	return 0;
}

static int test_locate_3(struct test_args *targ)
{
	uint32_t lnkind[2] = {1, ROLC_LINKSTATE_IND};
	uint32_t select[2] = {1, ITC_LOCATE_DEFAULT_NO};
	struct rolc_link_config cfg;
	union itc_msg *msg;
	itc_mbox_id_t mbox;
	char link_name[32];
	char mbox_name[32];
	char hunt_name[64];
	uint32_t link_id, rlink_id;
	int ret = -1;

	sprintf(link_name, "link_%d", rand());
	sprintf(mbox_name, "mbox_%d", rand());
	sprintf(hunt_name, "%s/%s", link_name, mbox_name);

	mbox = itc_locate(mbox_name);
	if (mbox != ITC_NO_ID) {
		printf("Local mbox %s unexpectedly found\n", mbox_name);
		return -1;
	}

	mbox = itc_locate(hunt_name);
	if (mbox != ITC_NO_ID) {
		printf("Remote mbox %s unexpectedly found\n", hunt_name);
		return -1;
	}


	/*
	** Issue async locate
	** create mbox on remote end
	** create link on remote end
	** create link on local side
	*/

	itc_locate_async(hunt_name, NULL, ITC_MY_MBOX);


	mbox = remote_rolc_create_mbox(targ, mbox_name);
	if (mbox == ITC_NO_ID) {
		printf("Error @line %d\n", __LINE__);
		return ret;
	}

	ret = remote_create_link(targ, link_name, ROLC_MASTER, 3,
								&rlink_id);
	if (ret) {
		printf("Error %d @line %d\n", ret, __LINE__);
		return ret;
	}

	memset(&cfg, 0, sizeof(cfg));	
	cfg.mode = ROLC_SLAVE;
	cfg.ri_addr = 3;

	ret = rolc_link_setup(targ->rolc_handle, link_name,
							&cfg, &link_id);
	if (ret) {
		printf("Error %d @line %d\n", ret, __LINE__);
		return ret;
	}

	msg = itc_receive(lnkind, 3000, ITC_FROM_ALL);
	if (msg == NULL) {
		printf("Error @line %d\n", __LINE__);
		return -1;
	}

	if (!msg->lkind.available) {
		printf("Error @line %d\n", __LINE__);
		return -1;
	}
	itc_free(&msg);

	msg = itc_receive(select, 3000, ITC_FROM_ALL);
	if (msg == NULL) {
		printf("Error @line %d\n", __LINE__);
		return ret;
	}

	itc_free(&msg);
	remote_rolc_destroy_mbox(targ, mbox);
	remote_rolc_destroy_link(targ, rlink_id);


	ret = rolc_link_destroy(targ->rolc_handle, link_id);
	if (ret) {
		printf("Error %d @line %d\n", ret, __LINE__);
		return ret;
	}

	msg = itc_receive(lnkind, 3000, ITC_FROM_ALL);
	if (msg == NULL) {
		printf("Error @line %d\n", __LINE__);
		return -1;
	}
	if (msg->lkind.available) {
		printf("Error @line %d\n", __LINE__);
		return -1;
	}
	itc_free(&msg);
	return 0;
}

static int test_locate_2(struct test_args *targ)
{
	uint32_t lnkind[2] = {1, ROLC_LINKSTATE_IND};
	uint32_t select[2] = {1, ITC_LOCATE_DEFAULT_NO};
	struct rolc_link_config cfg;
	union itc_msg *msg;
	itc_mbox_id_t mbox;
	char link_name[32];
	char mbox_name[32];
	char hunt_name[64];
	uint32_t link_id, rlink_id;
	int ret;

	sprintf(link_name, "link_%d", rand());
	sprintf(mbox_name, "mbox_%d", rand());
	sprintf(hunt_name, "%s/%s", link_name, mbox_name);

	mbox = itc_locate(mbox_name);
	if (mbox != ITC_NO_ID) {
		printf("Local mbox %s unexpectedly found\n", mbox_name);
		return -1;
	}

	mbox = itc_locate(hunt_name);
	if (mbox != ITC_NO_ID) {
		printf("Remote mbox %s unexpectedly found\n", hunt_name);
		return -1;
	}


	/*
	** Issue async locate
	** create link on local side
	** create mbox on remote end
	** create link on remote end
	*/

	itc_locate_async(hunt_name, NULL, ITC_MY_MBOX);

	memset(&cfg, 0, sizeof(cfg));	
	cfg.mode = ROLC_SLAVE;
	cfg.ri_addr = 2;

	ret = rolc_link_setup(targ->rolc_handle, link_name,
							&cfg, &link_id);
	if (ret) {
		printf("Error %d @line %d\n", ret, __LINE__);
		return ret;
	}

	mbox = remote_rolc_create_mbox(targ, mbox_name);
	if (mbox == ITC_NO_ID) {
		printf("Error %d @line %d\n", ret, __LINE__);
		return ret;
	}

	ret = remote_create_link(targ, link_name, ROLC_MASTER, 2,
								&rlink_id);
	if (ret) {
		printf("Error %d @line %d\n", ret, __LINE__);
		return ret;
	}

	msg = itc_receive(lnkind, 3000, ITC_FROM_ALL);
	if (msg == NULL) {
		printf("Error @line %d\n", __LINE__);
		return -1;
	}

	if (!msg->lkind.available) {
		printf("Error @line %d\n", __LINE__);
		return -1;
	}
	itc_free(&msg);

	msg = itc_receive(select, 3000, ITC_FROM_ALL);
	if (msg == NULL) {
		printf("Error @line %d\n", __LINE__);
		return ret;
	}

	itc_free(&msg);
	remote_rolc_destroy_mbox(targ, mbox);
	remote_rolc_destroy_link(targ, rlink_id);


	ret = rolc_link_destroy(targ->rolc_handle, link_id);
	if (ret) {
		printf("Error %d @line %d\n", ret, __LINE__);
		return ret;
	}

	msg = itc_receive(lnkind, 3000, ITC_FROM_ALL);
	if (msg == NULL) {
		printf("Error @line %d\n", __LINE__);
		return -1;
	}
	if (msg->lkind.available) {
		printf("Error @line %d\n", __LINE__);
		return -1;
	}
	itc_free(&msg);
	return 0;
}

static int test_locate_1(struct test_args *targ)
{
	uint32_t lnkind[2] = {1, ROLC_LINKSTATE_IND};
	uint32_t select[2] = {1, ITC_LOCATE_DEFAULT_NO};
	struct rolc_link_config cfg;
	union itc_msg *msg;
	itc_mbox_id_t mbox;
	char link_name[32];
	char mbox_name[32];
	char hunt_name[64];
	uint32_t link_id, rlink_id;
	int ret;

	sprintf(link_name, "link_%d", rand());
	sprintf(mbox_name, "mbox_%d", rand());
	sprintf(hunt_name, "%s/%s", link_name, mbox_name);

	mbox = itc_locate(mbox_name);
	if (mbox != ITC_NO_ID) {
		printf("Local mbox %s unexpectedly found\n", mbox_name);
		return -1;
	}

	mbox = itc_locate(hunt_name);
	if (mbox != ITC_NO_ID) {
		printf("Remote mbox %s unexpectedly found\n", hunt_name);
		return -1;
	}


	/*
	** Issue async locate
	** create link on local side
	** create link on remote end
	** create mbox on remote end
	*/

	itc_locate_async(hunt_name, NULL, ITC_MY_MBOX);

	memset(&cfg, 0, sizeof(cfg));	
	cfg.ri_port = 1;
	cfg.mode = ROLC_MASTER;
	cfg.ri_addr = 1;

	ret = rolc_link_setup(targ->rolc_handle, link_name,
							&cfg, &link_id);
	if (ret) {
		printf("Error %d @line %d\n", ret, __LINE__);
		return ret;
	}

	ret = remote_create_link(targ, link_name, ROLC_SLAVE, 1,
								&rlink_id);
	if (ret) {
		printf("Error %d @line %d\n", ret, __LINE__);
		return ret;
	}

	msg = itc_receive(lnkind, 3000, ITC_FROM_ALL);
	if (msg == NULL) {
		printf("Error @line %d\n", __LINE__);
		return -1;
	}

	if (!msg->lkind.available) {
		printf("Error @line %d\n", __LINE__);
		return -1;
	}
	itc_free(&msg);

	mbox = remote_rolc_create_mbox(targ, mbox_name);
	if (mbox == ITC_NO_ID) {
		printf("Error %d @line %d\n", ret, __LINE__);
		return ret;
	}

	msg = itc_receive(select, 1500, ITC_FROM_ALL);
	if (msg == NULL) {
		printf("Error @line %d\n", __LINE__);
		return ret;
	}

	itc_free(&msg);
	remote_rolc_destroy_mbox(targ, mbox);
	remote_rolc_destroy_link(targ, rlink_id);

	ret = rolc_link_destroy(targ->rolc_handle, link_id);
	if (ret) {
		printf("Error %d @line %d\n", ret, __LINE__);
		return ret;
	}

	msg = itc_receive(lnkind, 3000, ITC_FROM_ALL);
	if (msg == NULL) {
		printf("Error @line %d\n", __LINE__);
		return -1;
	}

	if (msg->lkind.available) {
		printf("Error @line %d\n", __LINE__);
		return -1;
	}

	itc_free(&msg);
	return 0;
}

static int test_link_recover(struct test_args *targ)
{
	uint32_t select[2] = {1, ROLC_LINKSTATE_IND};
	struct rolc_link_config cfg;
	union itc_msg *msg;
	char link_name[32];
	uint32_t link, rink;
	int ret;

	sprintf(link_name, "link_%d", rand());

	memset(&cfg, 0, sizeof(cfg));	
	cfg.ri_port = 1;
	cfg.mode = ROLC_MASTER;
	cfg.ri_addr = 8;

	ret = rolc_link_setup(targ->rolc_handle, link_name,
							&cfg, &link);
	if (ret) {
		printf("Error %d @line %d\n", ret, __LINE__);
		return ret;
	}

	ret = remote_create_link(targ, link_name, ROLC_SLAVE, 8, &rink);
	if (ret) {
		printf("Error %d @line %d\n", ret, __LINE__);
		return ret;
	}

	msg = itc_receive(select, 3000, ITC_FROM_ALL);
	if (msg == NULL) {
		printf("Error @line %d\n", __LINE__);
		return -1;
	}

	if (!msg->lkind.available) {
		printf("Error @line %d\n", __LINE__);
		return -1;
	}

	itc_free(&msg);
	set_tx_skip_mode(8, 1);	

	msg = itc_receive(select, 3000, ITC_FROM_ALL);
	if (msg == NULL) {
		printf("Error @line %d\n", __LINE__);
		return -1;
	}

	if (msg->lkind.available) {
		printf("Error @line %d\n", __LINE__);
		return -1;
	}

	itc_free(&msg);
	set_tx_skip_mode(8, 0);
	
	msg = itc_receive(select, 3000, ITC_FROM_ALL);
	if (msg == NULL) {
		printf("Error @line %d\n", __LINE__);
		return -1;
	}

	if (!msg->lkind.available) {
		printf("Error @line %d\n", __LINE__);
		return -1;
	}

	itc_free(&msg);
	set_rx_skip_mode(8, 1);

	msg = itc_receive(select, 3000, ITC_FROM_ALL);
	if (msg == NULL) {
		printf("Error @line %d\n", __LINE__);
		return -1;
	}

	if (msg->lkind.available) {
		printf("Error @line %d\n", __LINE__);
		return -1;
	}


	itc_free(&msg);
	set_rx_skip_mode(8, 0);
	
	msg = itc_receive(select, 3000, ITC_FROM_ALL);
	if (msg == NULL) {
		printf("Error @line %d\n", __LINE__);
		return -1;
	}

	if (!msg->lkind.available) {
		printf("Error @line %d\n", __LINE__);
		return -1;
	}
	itc_free(&msg);


	remote_rolc_destroy_link(targ, rink);
	ret = rolc_link_destroy(targ->rolc_handle, link);
	if (ret) {
		printf("Error %d @line %d\n", ret, __LINE__);
		return -1;
	}

	msg = itc_receive(select, 3000, ITC_FROM_ALL);
	if (msg == NULL) {
		printf("Error @line %d\n", __LINE__);
		return -1;
	}
	if (msg->lkind.available) {
		printf("Error @line %d\n", __LINE__);
		return -1;
	}
	itc_free(&msg);
	return 0;
}

static int test_link_create(struct test_args *targ)
{
	struct rolc_link_config cfg;
	uint32_t link_id[65];
	char lname[64];
	int count, cnt, ret;

	memset(&cfg, 0, sizeof(cfg));

	for (count = 0; count < 1; count++) {
		cfg.ri_port = 1;
		cfg.mode = ROLC_MASTER;

		/* 64 links should be available currently */
		for (cnt = 0; cnt < 64; cnt++) {
			sprintf(lname, "link_#%d", cnt+1);
			cfg.ri_addr = cnt + 1;

			ret = rolc_link_setup(targ->rolc_handle, lname, &cfg,
								&link_id[cnt]);
			if (ret) {
				printf("Error %d @line %d\n", ret, __LINE__);
				return ret;
			}
		}

		cfg.ri_addr = cnt + 1;
		sprintf(lname, "link_#%d", cnt+1);
		ret = rolc_link_setup(targ->rolc_handle, lname, &cfg,
									&link_id[cnt]);

		if (ret != -ENOMEM) {
			printf("Error %d @line %d\n", ret, __LINE__);
			return -1;
		}

		for (cnt = 0; cnt < 64; cnt++) {
			ret = rolc_link_destroy(targ->rolc_handle, link_id[cnt]);
			if (ret) {
				printf("Error %d @line %d\n", ret, __LINE__);
				return ret;
			}
		}
	}
	sleep(1);
	return 0;
}

static void *test_cases(void *ctx)
{
	struct test_args *targ = ctx;
	union itc_msg *msg;
	itc_mbox_id_t me;
	int result = -1;
	int loop = 1;

	me = itc_create_mailbox("rolc_mtest_cases", 0);
	if (me == ITC_NO_ID) {
		printf("created mbox failure: %d\n",me);
		goto get_out;
	}

	srand(me);
	targ->work_mbox = me;

 do_again:

	result = test_link_create(targ);
	if (result)
		goto get_out;

	printf("TC_1 SUCCESS\n");
	result = test_link_recover(targ);
	if (result)
		goto get_out;

	printf("TC_2 SUCCESS\n");
	result = test_locate_1(targ);
	if (result)
		goto get_out;


	printf("TC_3 SUCCESS\n");
	result = test_locate_2(targ);
	if (result)
		goto get_out;


	printf("TC_4 SUCCESS\n");
	result = test_locate_3(targ);
	if (result)
		goto get_out;


	printf("TC_5 SUCCESS\n");
	result = test_locate_4(targ);
	if (result)
		goto get_out;


	printf("TC_6 SUCCESS\n");
	result = test_locate_5(targ);
	if (result)
		goto get_out;


	printf("TC_7 SUCCESS\n");
	result = test_locate_6(targ);
	if (result)
		goto get_out;


	printf("TC_8 SUCCESS\n");
	result = test_locate_7(targ);
	if (result)
		goto get_out;


	printf("TC_9 SUCCESS\n");
	result = test_locate_8(targ);
	if (result)
		goto get_out;


	printf("TC_10 SUCCESS\n");
	result = test_locate_9(targ);
	if (result)
		goto get_out;


	printf("TC_11 SUCCESS\n");
	result = test_locate_10(targ);
	if (result)
		goto get_out;


	printf("TC_12 SUCCESS\n");
	result = test_unsolicited_send(targ);
	if (result)
		goto get_out;

	printf("TC_13 SUCCESS\n");

	if (loop--)
		goto do_again;

 get_out:

	msg = itc_alloc(sizeof(msg->tdone), ROLC_TEST_DONE);
	msg->tdone.result = result;
	itc_send(&msg, targ->test_mbox, me);
	usleep(2000);
	itc_delete_mailbox(me);
	return NULL;
}

void *test_prog_m(void *ctx)
{
	uint32_t select[2] = {1, ITC_LOCATE_DEFAULT_NO};
	struct rolc_if *rolc_handle;
	struct test_args *targ = ctx;
	pthread_t test_thread;
	union itc_msg *msg;
	itc_mbox_id_t me;
	int ret = -1, result = -1, run = -1;

	me = itc_create_mailbox("rolc_test_master", 0);
	if (me == ITC_NO_ID) {
		printf("created mbox failure: %d\n",me);
		goto get_out;
	}

	ret = rolc_init(&rolc_handle);
	if (ret) {
		printf("rolc_link_init failed, %d\n", ret);
		goto get_out;
	}

	itc_locate_async("ecblink/rolc_test_slave", NULL, ITC_MY_MBOX);
	msg = itc_receive(select, 50000, ITC_FROM_ALL);

	if (msg == NULL) {
		printf("failed to locate peer: slave/rolc_test_slave\n");
		goto get_out;
	}

	targ->test_mbox = me;
	targ->peer_mbox = itc_sender(msg);
	targ->rolc_handle = rolc_handle;
	itc_free(&msg);

	printf("Slave located (0x%x)\n", targ->peer_mbox);

	if (pthread_create(&test_thread, NULL, test_cases, targ)) {
		printf("create test thread, %d\n", -errno);
		goto get_out;
	}

	for (;run;) {
		msg = itc_receive(ITC_NOFILTER, ITC_NO_TMO, ITC_FROM_ALL);

		switch (msg->msgno) {

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

		case ROLC_TEST_DONE:
			result = msg->tdone.result;
			run = 0;
			break;

		default:
			break;
		}
		itc_free(&msg);
	}

	pthread_join(test_thread, NULL);

 get_out:

	rolc_shutdown(&rolc_handle);

	msg = itc_alloc(sizeof(msg->tdone), ROLC_TEST_DONE);
	msg->tdone.result = result;
	itc_send(&msg, targ->peer_mbox, me);
	usleep(2000);

	msg = itc_alloc(sizeof(msg->tdone), ROLC_TEST_DONE);
	msg->tdone.result = result;
	itc_send(&msg, targ->parent_mbox, me);
	usleep(2000);
	itc_delete_mailbox(me);
	return NULL;
}
