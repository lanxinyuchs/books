/*****************************************************************************
 *
 *
 * Copyright (c) Ericsson AB  2012-2015 All rights reserved.
 *
 * The information in this document is the property of Ericsson. Except
 * as specifically authorized in writing by Ericsson,the receiver of this
 * document shall keep the information contained herein confidential
 * and shall protect the same in whole or in part from disclosure and
 * dissemination to third parties. Disclosure and disseminations to the
 * receiver's employees shall only be made on a strict need to know basis.
 *
 ****************************************************************************/
#ifndef CLH_CSI_SIG
#define CLH_CSI_SIG

#include "cello_te_ose.h"
#include "osetypes.h"
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/******************************************************************************
 *
 * Message Name: CSI_CORE_STATE_CHANGE_IND
 *
 * Descr      : Message to client indicating that the core state of an MP
 *              instance has changed.
 *
 * Data       : mpId           Identifies the MP instance
 *              coreState      The new core state
 *
 *****************************************************************************/
#define CSI_CORE_STATE_CHANGE_IND (0x0180004)
typedef struct
{
   uint32_t         msgNo;
   uint32_t         mpId;
   CsiCoreState     coreState;
} CsiCoreStateChangeInd;


/******************************************************************************
 *
 * Signal Name: CSI_CLUSTER_RESTART_IND
 *
 * Descr      : Signal to client indicating that a cluster restart
 *              has been requested.
 *
 * Data       : restartType  The cluster restart type
 *
 *****************************************************************************/
#define CSI_CLUSTER_RESTART_IND (0x0180006) /*!- SIGNO(CsiChangeRoleInd) -!*/
typedef struct
{
   uint32_t                 msgNo;
   CsiClusterRestartType    restartType;
} CsiClusterRestartInd;





  /*
   * The next signals are deprecated and will be removed.
   * Use CSI_CORE_STATE_CHANGE_IND instead.
   */
/******************************************************************************
 *
 * Signal Name: CSI_CHANGE_OPER_STATE_IND
 *
 * Descr      : Signal to client indicating that the operational state of an MP
 *              instance has changed.
 *
 * Data       : piuInstanceId  Identifies the MP instance
 *              operState      The new operational state
 *
 *****************************************************************************/
#define CSI_CHANGE_OPER_STATE_IND (0x0180000) /*!- SIGNO(CsiChangeOperStateInd) -!*/
typedef struct
{
   SIGSELECT        sigNo;
   U32              piuInstanceId;
   CsiOpState       operState;
} CsiChangeOperStateInd;

/******************************************************************************
 *
 * Signal Name: CSI_CHANGE_ROLE_IND
 *
 * Descr      : Signal to client indicating that the role of an MP
 *              instance has changed.
 *
 * Data       : mpId  Identifies the MP instance
 *              role  The new role
 *
 *****************************************************************************/
#define CSI_CHANGE_ROLE_IND (0x0180002) /*!- SIGNO(CsiChangeRoleInd) -!*/
typedef struct
{
   SIGSELECT        sigNo;
   uint32_t         mpId;
   CsiRole          role;
} CsiChangeRoleInd;





#ifdef __cplusplus
}
#endif

#endif /* CLH_CSI_SIG */
