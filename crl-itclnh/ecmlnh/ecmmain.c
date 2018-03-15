/**
 *   Function for starting an ECM linkhandler.
 *
 *   @file ecmmain.c
 *
 *   Copyright (C) 2015 by Ericsson AB. All rights reserved. The
 *   information in this document is the property of Ericsson. Except
 *   as specifically authorized in writing by Ericsson, the receiver
 *   of this document shall keep the information contained herein
 *   confidential and shall protect the same in whole or in part from
 *   disclosure and dissemination to third parties. Disclosure and
 *   disseminations to the receiver's employees shall only be made on
 *   a strict need to know basis.
 *
 */

/* ========================================================================
 *   History of development:
 *   -----------------------
 *   Revised : 2015-03-20 Fredrik Skog
 *   Change  : First version
 *
 * ========================================================================
 */

#include <stdlib.h>
#include <stdio.h>
#include <pthread.h>

#include "itc.h"
#include "ecmlnh.h"

/*
 * MAX_LOCAL_MBOX: no of links i.e 24 + max local mailboxes 3.
 */
#define MAX_LOCAL_MBOX                  27
#define MAX_MBOX_COUNT                  (HTAB_SIZE + MAX_LOCAL_MBOX)

/* ==================================================================== */
/**
 *   Main function for standalone use.
 *
 *   @brief            Starts linkhandler pthread.
 *
 *   @param            -
 *   @return           -
 *
 *   @par Globals:     --
 */
/* ===================================================================== */
int main(int argc, char **argv)
{
	int ret;
	pthread_t tid;

        ret = itc_init(MAX_MBOX_COUNT, ITC_MALLOC, NULL, NULL, 0);
	if (ret) {
		printf("itc_init returned error %d\n", ret);
		abort();
	}

	ret = pthread_create(&tid, NULL, ecmlnh_thread, NULL);
	if(ret != 0) {
		abort();
	}
	ret = pthread_join(tid, NULL);
	if(ret != 0) {
		abort();
	}
	return 0;
}

