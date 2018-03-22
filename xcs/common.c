
#include <stdint.h>
#include <itc.h>

#include "common.h"
#include "log_tpt.h"


int32_t xpai_locate_mbox(const char *mbox_name, itc_mbox_id_t *mbox_id)
{
	const uint32_t filter[] = { 1, ITC_LOCATE_DEFAULT_NO };
	union itc_msg *msg;

	if (itc_current_mbox() == ITC_NO_ID) {
		TPT_ERROR("The current application thread has no ITC mailbox");
		return INIT_MBOX_NOK;
	}

	TPT_TRACE(7, STR("Locating \"%s\"", mbox_name));
	itc_locate_async(mbox_name, NULL, ITC_MY_MBOX);
	msg = itc_receive(filter, ITC_NO_TMO, ITC_FROM_ALL);
	*mbox_id = itc_sender(msg);
	itc_free(&msg);
	TPT_TRACE(7, STR("Located \"%s\" as 0x%x", mbox_name, *mbox_id));

	return INIT_OK;
}
