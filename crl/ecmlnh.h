/* ---------------------------------------------------------------------------
 *
 * © Ericsson AB 2014 All rights reserved.
 * The information in this document is the property of Ericsson. Except
 * as specifically authorized in writing by Ericsson, the receiver of
 * this document shall keep the information contained herein confidential
 * and shall protect the same in whole or in part from disclosure and
 * dissemination to third parties. Disclosure and disseminations to the
 * receivers employees shall only be made on a strict need to know basis.
 *
 * ---------------------------------------------------------------------------
 */

#define ECM_THREAD_NAME    "ecmlnh"

void *ecmlnh_thread(void *data);

/* ==================================================================== */
/**
 *   Start linkhandler.
 *
 *   @brief            Starts linkhandler pthread.
 *
 *   @param            -
 *   @return           -
 *
 *   @par Globals:     --
 */
/* ===================================================================== */
void start_ecmlnh(void);
