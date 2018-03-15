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

#include "rolc-link-int.h"
#include "rolc-link-api.h"

#define ROLC_LINK_MAGIG			(0x134779e9)

struct rolc_if {
	int		magic;
};

union itc_msg {
	uint32_t					msgno;
	struct rolc_create_req		ctreq;
	struct rolc_create_rsp		ctrsp;
	struct rolc_delete_req		dlreq;
	struct rolc_delete_rsp		dlrsp;
};


union itc_msg *transact(union itc_msg *msg, uint32_t reply)
{
	itc_mbox_id_t server_mbox;
	uint32_t select[2];

	if (itc_current_mbox() == ITC_NO_ID)
		return NULL;

	server_mbox = itc_locate(ROLC_DAEMON_NAME);
	if (server_mbox == ITC_NO_ID)
			return NULL;

	itc_send(&msg, server_mbox, ITC_MY_MBOX);
	select[0] = 1;
	select[1] = reply;
	return itc_receive(select, 5*60000, server_mbox);
}


int32_t rolc_init(struct rolc_if **handle)
{
	struct rolc_if *p;

	if (handle == NULL)
		return -EINVAL;

	p = malloc(sizeof(*p));
	if (p == NULL)
		return -ENOMEM;

	p->magic = ROLC_LINK_MAGIG;
	*handle = p;
	return 0;
}

void rolc_shutdown(struct rolc_if **handle)
{
	union itc_msg *msg;
	struct rolc_if *p;

	if (handle == NULL)
		return;

	if (((p = *handle) == NULL) || (p->magic != ROLC_LINK_MAGIG))
		return;

	msg = itc_alloc(sizeof(msg->dlreq), ROLC_DELETE_REQ);
	msg->dlreq.owner = p;
	msg->dlreq.linkid = 0;

	msg = transact(msg, ROLC_DELETE_RSP);
	if (msg)
		itc_free(&msg);

	p->magic = 0;
	free(p);
	*handle = NULL;
}

int32_t rolc_link_setup(struct rolc_if *handle, const char *name,
		struct rolc_link_config *cfg, uint32_t *link_id)
{
	union itc_msg *msg;
	int ret = -EIO;

	if ((handle == NULL) || (handle->magic != ROLC_LINK_MAGIG))
		return -EINVAL;

	if (link_id == NULL)
		return -EINVAL;

	msg = itc_alloc(sizeof(msg->ctreq), ROLC_CREATE_REQ);
	msg->ctreq.owner = handle;
	msg->ctreq.ri_port = cfg->ri_port;
	msg->ctreq.ri_addr = cfg->ri_addr;
	msg->ctreq.mode = cfg->mode;
	strncpy(msg->ctreq.name, name, ROLC_LINKNAME_SIZE);

	msg = transact(msg, ROLC_CREATE_RSP);

	if (msg) {
		*link_id = msg->ctrsp.linkid;
		ret = (int)msg->ctrsp.result;
		itc_free(&msg);
	}
	return ret;
}

int32_t rolc_link_destroy(struct rolc_if *handle, uint32_t link_id)
{
	union itc_msg *msg;
	int ret = -EIO;

	if ((handle == NULL) || (handle->magic != ROLC_LINK_MAGIG))
		return -EINVAL;

	msg = itc_alloc(sizeof(msg->dlreq), ROLC_DELETE_REQ);
	msg->dlreq.owner = NULL;
	msg->dlreq.linkid = link_id;

	msg = transact(msg, ROLC_DELETE_RSP);

	if (msg) {
		ret = (int)msg->dlrsp.result;
		itc_free(&msg);
	}

	return ret;
}
