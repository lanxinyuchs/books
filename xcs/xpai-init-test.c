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
#include "xpp_init.h"

#define MAX_NOF_SUB 1
#define MBOX "XPAI_CLIENT"

int main(int argc, char **argv)
{
	(void) argc; (void) argv;
	itc_mbox_id_t main_mbox;


	/* Create mailbox */
	if(itc_init(MAX_NOF_SUB + 2, ITC_MALLOC, NULL, ITC_NO_NAMESPACE, 0)) {
		printf("%s: Unable to initialize ITC!\n", __func__);
		return -1;
	}

	main_mbox = itc_create_mailbox(MBOX, 0);
	if (main_mbox == ITC_NO_ID) {
		printf("%s: Cannot create ITC mailbox \"%s\"!\n",
		       __func__, MBOX);
		return -1;

	}

	/* Will never return if fails */
	XPAI_initXPPInterface();
}
