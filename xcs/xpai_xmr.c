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
#include <string.h>
#include <ose.h>
#include <osetypes.h>
#include <evti.h>

#include "xpai_xmr_if.h"
#include "common.h"
#include "log_tpt.h"


/* Referenced signals. */
union itc_msg {
	uint32_t                   sigNo;
	struct XPAI_SubscribeIndS  XPAI_SubscribeInd;
};

/******************************************************************************
 *
 * Global function:
 *      XPAI_XmrInit
 *
 * Description:
 *      As the event subscription mechanism for legacy reasons has no
 *      connection establish or version negotiation. There is really
 *      no need for an init function. But keep things similar for all
 *      sub interfaces it's defined and always return success.
 *
 *****************************************************************************/
inline uint32_t xpai_xmr_init(void)
{
	return INIT_OK;
}

/******************************************************************************
 *
 * Global function:
 *      XPAI_Subscribe
 *
 * Description:
 *      This function will register 'pid' as a subscriber of the 'tag' string.
 *
 * Side effects:
 *      None.
 *
 *****************************************************************************/
void XPAI_Subscribe(U32 pid /*mbox_id*/, char *tag)
{
	union itc_msg *msg = NULL;
	static itc_mbox_id_t xmr_mbox_id = ITC_NO_ID;

	/* Check the TAG string length. */
	if (strlen(tag) >= XMR_MAX_TAG_LENGTH) {
		TPT_ERROR("Subscribing TAG is too long. No subscription made!!!");
		return;
	}

	if (xmr_mbox_id == ITC_NO_ID) {
		xmr_mbox_id = itc_locate(EVTI_SERVER_NAME);
		if (xmr_mbox_id == ITC_NO_ID) {
			TPT_ERROR(STR("The process %s could not be found!!!", EVTI_SERVER_NAME));
			return;
		}
	}
	/* Send a XPAI_SUBSCRIBE_IND signal to EVENT_SERVER
	   with pid as sender. */
	msg = itc_alloc(sizeof(struct XPAI_SubscribeIndS), XPAI_SUBSCRIBE_IND);
	strcpy(msg->XPAI_SubscribeInd.tag, tag);
	TPT_SEND_SIG(msg->sigNo, xmr_mbox_id, "XPAI_SUBSCRIBE_IND");
	itc_send(&msg, xmr_mbox_id, (itc_mbox_id_t)pid);
	return;
}
