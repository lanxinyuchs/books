/* ---------------------------------------------------------------------------
 *
 * © Ericsson AB 2013 All rights reserved.
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
#include <string.h>
#include <unistd.h>
#include <pthread.h>
#include <errno.h>
#include <itc.h>

#include "ecm-link-internal.h"
#include "ecm-link-api.h"

#define ECM_LINK_MAGIG			(0x134779fa)

struct ecm_if {
	int		magic;
};

union itc_msg {
	uint32_t			msgno;
	struct ecm_create_req		ctreq;
	struct ecm_create_rsp		ctrsp;
	struct ecm_delete_req		dlreq;
	struct ecm_delete_rsp		dlrsp;
};


union itc_msg *transact(union itc_msg *msg, uint32_t reply)
{
	union itc_msg *tmpmsg;
	uint32_t locmsg[] = { 1, ITC_LOCATE_DEFAULT_NO };
	itc_mbox_id_t server_mbox;
	uint32_t select[2];

	if (itc_current_mbox() == ITC_NO_ID)
		return NULL;

	itc_locate_async(ECM_DAEMON_NAME, NULL, ITC_MY_MBOX);
	tmpmsg = itc_receive(locmsg, 5000, ITC_FROM_ALL);
	if(tmpmsg == NULL) {
		return NULL;
	}
	server_mbox = itc_sender(tmpmsg);
	itc_free(&tmpmsg);

	itc_send(&msg, server_mbox, ITC_MY_MBOX);
	select[0] = 1;
	select[1] = reply;
	return itc_receive(select, 5000, server_mbox);
}

int32_t ecm_link_init(struct ecm_if **handle)
{
	struct ecm_if *p;

	if (handle == NULL)
		return -EINVAL;

	if (itc_current_mbox() == ITC_NO_ID)
		return -EIO;

	p = malloc(sizeof(*p));
	if (p == NULL)
		return -ENOMEM;

	p->magic = ECM_LINK_MAGIG;
	*handle = p;
	return 0;
}

void ecm_link_shutdown(struct ecm_if **handle)
{
	union itc_msg *msg;
	struct ecm_if *p;

	if (handle == NULL)
		return;

	if (((p = *handle) == NULL) || (p->magic != ECM_LINK_MAGIG))
		return;

	msg = itc_alloc(sizeof(msg->dlreq), ECM_DELETE_REQ);
	msg->dlreq.owner = p;
	msg->dlreq.linkid = 0;


	msg = transact(msg, ECM_DELETE_RSP);
	if (msg)
		itc_free(&msg);

	p->magic = 0;
	free(p);
	*handle = NULL;
}

int ecm_link_create(struct ecm_if *handle, const char *name,
		    struct ecm_link_config *cfg, uint32_t *link_id)
{
	union itc_msg *msg;
	int ret = -EIO;

	if ((handle == NULL) || (handle->magic != ECM_LINK_MAGIG))
		return -EINVAL;

	if (link_id == NULL)
		return -EINVAL;

	msg = itc_alloc(sizeof(msg->ctreq), ECM_CREATE_REQ);
	msg->ctreq.owner = handle;
	msg->ctreq.config = *cfg;
	strncpy(msg->ctreq.name, name, ECM_LINKNAME_SIZE);

	msg = transact(msg, ECM_CREATE_RSP);
	if (msg) {
		*link_id = msg->ctrsp.linkid;
		ret = (int)msg->ctrsp.result;
		itc_free(&msg);
	}
	return ret;
}

int ecm_link_destroy(struct ecm_if *handle, uint32_t link_id)
{
	union itc_msg *msg;
	int ret = -EIO;

	if ((handle == NULL) || (handle->magic != ECM_LINK_MAGIG))
		return -EINVAL;

	msg = itc_alloc(sizeof(msg->dlreq), ECM_DELETE_REQ);
	msg->dlreq.owner = NULL;
	msg->dlreq.linkid = link_id;

	msg = transact(msg, ECM_DELETE_RSP);
	if (msg) {
		ret = (int)msg->dlrsp.result;
		itc_free(&msg);
	}

	return ret;
}
