/*****************************************************************************
 *
 *
 * Copyright (c) Ericsson AB  2015 All rights reserved.
 *
 * The information in this document is the property of Ericsson. Except
 * as specifically authorized in writing by Ericsson,the receiver of this
 * document shall keep the information contained herein confidential
 * and shall protect the same in whole or in part from disclosure and
 * dissemination to third parties. Disclosure and disseminations to the
 * receiver's employees shall only be made on a strict need to know basis.
 *
 ****************************************************************************/
#ifndef CLH_CHI_SIG
#define CLH_CHI_SIG

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/******************************************************************************
 *
 * Message Name: CHI_OP_STATE_CHANGE_IND
 *
 * Descr      : Message to client indicating that the operational state of an MP
 *              instance has changed.
 *
 * Data       : mpId      Identifies the MP instance
 *              operState The new operational state
 *
 *****************************************************************************/
#define CHI_OP_STATE_CHANGE_IND (0x0180003)
typedef struct
{
   uint32_t         msgNo;
   uint32_t         mpId;
   ChiOpState       operState;
} ChiOpStateChangeInd;

  /*
   * The next signal is deprecated and will be removed.
   * Use CHI_OP_STATE_CHANGE_IND instead.
   */
/******************************************************************************
 *
 * Signal Name: CHI_CHANGE_STATE_IND
 *
 * Descr      : Signal to client indicating that the state of an MP
 *              instance has changed.
 *
 * Data       : mpId      Identifies the MP instance
 *              operState The new operational state
 *              role      The new role
 *
 *****************************************************************************/
#define CHI_CHANGE_STATE_IND (0x0180001) /*!- SIGNO(ChiChangeStateInd) -!*/
typedef struct
{
   SIGSELECT        sigNo;
   uint32_t         mpId;
   ChiOpState       operState;
   ChiRole          role;
} ChiChangeStateInd;

#ifdef __cplusplus
}
#endif

#endif /* CLH_CHI_SIG */
