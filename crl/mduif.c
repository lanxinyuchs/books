/* ---------------------------------------------------------------------------
 *
 * © Ericsson AB 2015 All rights reserved.
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

#include "mdu-link-internal.h"
#include "mdu-link-api.h"

#define MDU_LINK_MAGIG			(0x134779f9)

struct mdu_if {
	int		magic;
};

union itc_msg {
	uint32_t					msgno;
	struct mdu_create_req		ctreq;
	struct mdu_create_rsp		ctrsp;
	struct mdu_delete_req		dlreq;
	struct mdu_delete_rsp		dlrsp;
};


union itc_msg *transact(union itc_msg *msg, uint32_t reply)
{
	itc_mbox_id_t server_mbox;
	uint32_t select[2];

	if (itc_current_mbox() == ITC_NO_ID)
		return NULL;

	server_mbox = itc_locate(MDU_DAEMON_NAME);
	if (server_mbox == ITC_NO_ID)
			return NULL;

	itc_send(&msg, server_mbox, ITC_MY_MBOX);
	select[0] = 1;
	select[1] = reply;
	return itc_receive(select, 5*60000, server_mbox);
}

int32_t mdu_link_init(struct mdu_if **handle)
{
	struct mdu_if *p;

	if (handle == NULL)
		return -EINVAL;

	if (itc_current_mbox() == ITC_NO_ID)
		return -EIO;

	p = malloc(sizeof(*p));
	if (p == NULL)
		return -ENOMEM;

	p->magic = MDU_LINK_MAGIG;
	*handle = p;
	return 0;
}

void mdu_link_shutdown(struct mdu_if **handle)
{
	union itc_msg *msg;
	struct mdu_if *p;

	if (handle == NULL)
		return;

	if (((p = *handle) == NULL) || (p->magic != MDU_LINK_MAGIG))
		return;

	msg = itc_alloc(sizeof(msg->dlreq), MDU_DELETE_REQ);
	msg->dlreq.owner = p;
	msg->dlreq.linkid = 0;


	msg = transact(msg, MDU_DELETE_RSP);
	if (msg)
		itc_free(&msg);

	p->magic = 0;
	free(p);
	*handle = NULL;
}

int mdu_link_create(struct mdu_if *handle, const char *name,
						struct mdu_link_config *cfg, uint32_t *link_id)
{
	union itc_msg *msg;
	int ret = -EIO;

	if ((handle == NULL) || (handle->magic != MDU_LINK_MAGIG))
		return -EINVAL;

	if (link_id == NULL)
		return -EINVAL;

	msg = itc_alloc(sizeof(msg->ctreq), MDU_CREATE_REQ);
	msg->ctreq.owner = handle;
	msg->ctreq.config = *cfg;
	strncpy(msg->ctreq.name, name, MDU_LINKNAME_SIZE);

	msg = transact(msg, MDU_CREATE_RSP);
	if (msg) {
		*link_id = msg->ctrsp.linkid;
		ret = (int)msg->ctrsp.result;
		itc_free(&msg);
	}
	return ret;
}

int mdu_link_destroy(struct mdu_if *handle, uint32_t link_id)
{
	union itc_msg *msg;
	int ret = -EIO;

	if ((handle == NULL) || (handle->magic != MDU_LINK_MAGIG))
		return -EINVAL;

	msg = itc_alloc(sizeof(msg->dlreq), MDU_DELETE_REQ);
	msg->dlreq.owner = NULL;
	msg->dlreq.linkid = link_id;

	msg = transact(msg, MDU_DELETE_RSP);
	if (msg) {
		ret = (int)msg->dlrsp.result;
		itc_free(&msg);
	}

	return ret;
}
