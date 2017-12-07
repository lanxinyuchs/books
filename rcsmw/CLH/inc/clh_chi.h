/*
 *
 * Copyright (c) Ericsson AB 2014-2015 All rights reserved.
 *
 * The information in this document is the property of Ericsson. Except
 * as specifically authorized in writing by Ericsson,the receiver of this
 * document shall keep the information contained herein confidential
 * and shall protect the same in whole or in part from disclosure and
 * dissemination to third parties. Disclosure and disseminations to the
 * receiver's employees shall only be made on a strict need to know basis.
 *
 */

#ifndef CLH_CHI_H
#define CLH_CHI_H

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

  /********** EXPORTED TYPES ****************/

  typedef uint32_t ChiResult;
#define CHI_RESULT_OK                         0
#define CHI_RESULT_ERROR_SERVER_NOT_AVAILABLE 1
#define CHI_RESULT_NO_EXIST                   2
#define CHI_RESULT_BAD_PARM                   3
#define CHI_RESULT_NO_ITC_MBOX                4

  typedef uint32_t ChiMpRole;
#define CHI_MP_ROLE_CORE    0
#define CHI_MP_ROLE_REGULAR 1

  typedef uint32_t ChiOpState;
#define CHI_OPSTATE_DISABLED 0
#define CHI_OPSTATE_ENABLED  1

  /*
   * The CoreRank is only applicable for CORE MPs, which implicitly gives that
   * an MP associated with a CoreRank does in fact have the Role = CORE.
   * If CoreRank is 'UNDEFINED', it also implicitly means that the MP:s Role =
   * REGULAR.
   */
  typedef uint32_t ChiCoreRank;
#define CHI_CORE_RANK_PRIMARY    0
#define CHI_CORE_RANK_SECONDARY  1
#define CHI_CORE_RANK_UNDEFINED  2

  typedef uint32_t ChiRestartType;
#define CHI_RESTART_TYPE_HARD 0
#define CHI_RESTART_TYPE_SOFT 1

  typedef uint32_t ChiRestartRank;
#define CHI_RESTART_RANK_COLD            0
#define CHI_RESTART_RANK_COLD_WITH_TEST  1
#define CHI_RESTART_RANK_WARM            2

#define CHI_MAX_RESTART_CAUSE_LEN (60)

  ChiResult Chi_subscribeOpState(uint32_t mpId);
  ChiResult Chi_unsubscribeOpState(uint32_t mpId);

  /*
   * The association of a FruId with an MpId and a CoreRank is made for the
   * first time when the FruId is configured.
   * When the Cluster restarts, the association is made again. Note that the
   * FruId may be assigned to another MpId after each restart depending on the
   * algorithm in NC to calculate MpIds. However, NC should have an algorithm
   * that increases the odds for getting the same MpId as often as possible,
   * because RCS must escalate to re-installation of the MP when there is a
   * mismatch in the association compared to the previous association. When that
   * happens, the restart time will be significantly longer.
   */
  ChiResult Chi_associateMp(uint32_t mpId,
			    const char *fruId,
			    ChiCoreRank coreRank);
  /*
   * Disassociasion of an MP is made when a change of configuration is made that
   * leads to a removal of the MP.
   */
  ChiResult Chi_disassociateMp(uint32_t mpId);

  ChiResult Chi_restartCluster(ChiRestartType restartType,
			       ChiRestartRank restartRank,
			       const char *restartCause);

  /*
   * The next definitions are deprecated and will be removed.
   */
  typedef uint32_t ChiRole;
#define CHI_ROLE_ACTIVE  0
#define CHI_ROLE_STANDBY 1
#define CHI_ROLE_REGULAR 2

  typedef uint32_t ChiCoreRole;
#define CHI_CORE_ROLE_PRIMARY    0
#define CHI_CORE_ROLE_SECONDARY  1
#define CHI_CORE_ROLE_REGULAR    2

  /*
   * The next function is deprecated and will be removed.
   */
  ChiResult Chi_getState(uint32_t mpId, ChiOpState *operState, ChiRole *role);
  /*
   * The next functions are deprecated and will be removed.
   * Use Chi_subscribeOpState, Csi_subscribeCoreState and
   * Chi_unsubscribeOpState, Csi_unsubscribeCoreState instead.
   */
  ChiResult Chi_subscribe(uint32_t mpId);
  ChiResult Chi_unsubscribe(uint32_t mpId);
  /*
   * The next functions are deprecated and will be removed.
   * Use Chi_associateMp and Chi_disassociateMp instead.
   */
  ChiResult Chi_addMp(uint32_t mpId, const char *fruId, ChiCoreRole coreRole);
  ChiResult Chi_removeMp(uint32_t mpId);

#ifdef __cplusplus
}
#endif

#endif   /* CLH_CHI_H */
