/**
 *   Copyright (C) 2015 by Ericsson AB. All rights reserved. The
 *   information in this document is the property of Ericsson. Except
 *   as specifically authorized in writing by Ericsson, the receiver
 *   of this document shall keep the information contained herein
 *   confidential and shall protect the same in whole or in part from
 *   disclosure and dissemination to third parties. Disclosure and
 *   disseminations to the receiver's employees shall only be made on
 *   a strict need to know basis.
 */

#include <stdio.h>
#include <itc.h>
#include "xpai_xcbc_basic_if.h"

#define TEST_MBOX "xpai_xpp_if_client"

union itc_msg {
	uint32_t msgno;
	struct XPAI_SelfTestIndS self_test_ind;
};

int main(int argc, char **argv)
{
	(void) argc; (void) argv;
	itc_mbox_id_t mbox;
	union itc_msg *rec_msg_p = NULL;
	uint32_t ret = 0;

	static uint32_t rx_filter[] = {1, XPAI_SELF_TEST_IND};
	/* Create mailbox */
	if(itc_init(1, ITC_MALLOC, NULL, ITC_NO_NAMESPACE, 0)) {
		printf("Unable to initialize ITC!\n");
		return -1;
	}

	mbox = itc_create_mailbox(TEST_MBOX, 0);
	if (mbox == ITC_NO_ID) {
		printf("Cannot create ITC mailbox \"%s\"!\n",
		       TEST_MBOX);
		return -1;

	}

	printf("Calling XPAI_SelfTest mbox 0x%x\n", mbox);

	ret = XPAI_SelfTest(mbox);

	if(ret != XPAI_SELF_TEST_OK) {
		printf("XPAI_SelfTest() returned unexpected error code: %d\n", ret);
		ret = -1;
		goto exit;
	}

	printf("waiting for XPAI_SELF_TEST_IND\n");

	rec_msg_p = itc_receive(rx_filter, 1000, ITC_FROM_ALL);

	if(rec_msg_p == NULL) {
		printf("Expected to receive XPAI_SELF_TEST_IND\n");
		ret = -1;
		goto exit;
	} else {
		if(rec_msg_p->self_test_ind.result != XPAI_SELF_TEST_RESULT_PASSED) {
			printf("Received XPAI_SELT_TEST_IND "
			       "with unexpected result: %d\n",
			       rec_msg_p->self_test_ind.result);
			ret = -1;
			itc_free(&rec_msg_p);
			goto exit;
		}
		itc_free(&rec_msg_p);
		printf("XPAI_SelfTest() test case PASSED\n");
	}

exit:
	itc_delete_mailbox(mbox);
	return ret;
}
