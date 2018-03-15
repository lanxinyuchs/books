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
#include "ecb-link-api.h"
#include "ecb-link-internal.h"

#define ECB_LINK_MAGIG			(0xECB88616)

struct ecbif_data {
	int		magic;
};

union itc_msg {
	uint32_t					msgno;
	struct ecb_link_create    	ctreq;
	struct ecb_link_create    	ctrsp;
	struct ecb_link_destroy    	dsreq;
	struct ecb_link_destroy    	dsrsp;
};


int ecb_link_init(void **handle)
{
	struct ecbif_data *p;
	itc_mbox_id_t server_mbox;

	if (handle == NULL)
		return -EINVAL;

	p = malloc(sizeof(*p));
	if (p == NULL)
		return -ENOMEM;

	p->magic = ECB_LINK_MAGIG;
	*handle = p;

	for (;;) {
		server_mbox = itc_locate(ECB_DAEMON_NAME);
		if (server_mbox != ITC_NO_ID)
			break;

		sleep(1);
	}
	return 0;
}

int ecb_link_create(void *handle, struct ecb_link_config *cfg, uint32_t *inst)
{
	uint32_t select[2] = {1, ECB_LINK_CREATE_RSP};
	struct ecbif_data *p = handle;
	itc_mbox_id_t server_mbox;
	union itc_msg  *msg;
	int ret = -EIO;

	if ((p == NULL) || (p->magic != ECB_LINK_MAGIG))
		return -EINVAL;

	if ((cfg == NULL) || (inst == NULL))
		return -EINVAL;

	if (itc_current_mbox() == ITC_NO_ID)
		return -EIO;

	server_mbox = itc_locate(ECB_DAEMON_NAME);
	if (server_mbox == ITC_NO_ID)
			return -EIO;

	msg = itc_alloc(sizeof(msg->ctreq), ECB_LINK_CREATE_REQ);
	msg->ctreq.owner = handle;
	msg->ctreq.address = cfg->address;
	msg->ctreq.station = cfg->station;
	strncpy(msg->ctreq.name, cfg->name, ECB_LINKNAME_SIZE);

	itc_send(&msg, server_mbox, ITC_MY_MBOX);
	msg = itc_receive(select, 5*60000, server_mbox);

	if (msg) {
		*inst = msg->ctrsp.linkid;
		ret = msg->ctrsp.result;
		itc_free(&msg);
	}
	return ret;
}

int ecb_link_destroy(void *handle, uint32_t inst)
{
	uint32_t select[2] = {1, ECB_LINK_DESTROY_RSP};
	struct ecbif_data *p = handle;
	itc_mbox_id_t server_mbox;
	union itc_msg  *msg;
	int ret = -EIO;

	if ((p == NULL) || (p->magic != ECB_LINK_MAGIG))
		return -EINVAL;

	server_mbox = itc_locate(ECB_DAEMON_NAME);
	if (server_mbox == ITC_NO_ID)
			return -EIO;

	msg = itc_alloc(sizeof(msg->dsreq), ECB_LINK_DESTROY_REQ);
	msg->dsreq.owner = NULL;
	msg->dsreq.linkid = inst;
	itc_send(&msg, server_mbox, ITC_MY_MBOX);
	msg = itc_receive(select, 5*60000, server_mbox);

	if (msg) {
		ret = msg->dsrsp.result;
		itc_free(&msg);
	}
	return ret;
}

void ecb_link_shutdown(void *handle)
{
	uint32_t select[2] = {1, ECB_LINK_DESTROY_RSP};
	struct ecbif_data *p = handle;
	itc_mbox_id_t server_mbox;
	union itc_msg  *msg = NULL;

	if ((p == NULL) || (p->magic != ECB_LINK_MAGIG))
		return;

	server_mbox = itc_locate(ECB_DAEMON_NAME);

	if (server_mbox != ITC_NO_ID) {
		union itc_msg  *msg;
		msg = itc_alloc(sizeof(msg->dsreq), ECB_LINK_DESTROY_REQ);
		msg->dsreq.owner = handle;
		msg->dsreq.linkid = 0;
		itc_send(&msg, server_mbox, ITC_MY_MBOX);
		msg = itc_receive(select, 5*60000, server_mbox);
	}

	if (msg)
		itc_free(&msg);

	p->magic = 0;
	free(p);
}
