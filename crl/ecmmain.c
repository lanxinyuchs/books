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

#include "ulh_trace.h"
#ifdef LTTNG
#include "ulh_lttng.h"
#include "../libitclnh/ulh_rlnh.h"
#endif

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

    ret = itc_init(4096, ITC_MALLOC, NULL, NULL, 0);
	if (ret) {
		printf("itc_init returned error %d\n", ret);
		abort();
	}

	ret = pthread_create(&tid, NULL, ecmlnh_thread, NULL);
	if(ret != 0) {
		abort();
	}
	/* Make it easier to find the ecmlnh init thread when debugging. */
	ret = pthread_setname_np(tid, "ecmlnh_init");
	if(ret != 0) {
        ULH_TRACE_ERROR("pthread_setname_np failed:%d(%s)",
                        errno, strerror(errno));
	}
	ret = pthread_join(tid, NULL);
	if(ret != 0) {
		abort();
	}
	return 0;
}

