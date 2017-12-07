#ifndef CLH_CSI_H
#define CLH_CSI_H
/*
 *
 * Copyright (c) Ericsson AB 2012-2015 All rights reserved.
 *
 * The information in this document is the property of Ericsson. Except
 * as specifically authorized in writing by Ericsson,the receiver of this
 * document shall keep the information contained herein confidential
 * and shall protect the same in whole or in part from disclosure and
 * dissemination to third parties. Disclosure and disseminations to the
 * receiver's employees shall only be made on a strict need to know basis.
 *
 */

#ifdef __cplusplus
extern "C" {
#endif

  /********** EXPORTED TYPES ****************/

  /*
   * The CoreState is only applicable for CORE MP:s.
   * If CoreState is requested for a REGULAR MP, 'UNDEFINED' is returned since
   * the REGULAR MP does not have a CoreState. The CoreState of a CORE MP is
   * never UNDEFINED regardless of the MP:s OperationalState.
   * 
   * MpRole can be either 'CORE' or 'REGULAR'. The MpRole is not defined in this
   * interface since the CoreState gives that information indirectly:
   * CoreState = ACTIVE / STANDBY -> MpRole = CORE
   * CoreState = UNDEFINED        -> MpRole = REGULAR
   */
  typedef U32 CsiCoreState;
#define CSI_CORE_STATE_ACTIVE    0
#define CSI_CORE_STATE_STANDBY   1
#define CSI_CORE_STATE_UNDEFINED 2

  typedef U16 CsiResult;
#define CSI_OK                         0
#define CSI_ERROR_SERVER_NOT_AVAILABLE 1
#define CSI_NO_EXIST                   2
#define CSI_NO_ITC_MBOX                3

  typedef U16 CsiClusterRestartType;
#define CSI_RESTART_TYPE_HARD 0
#define CSI_RESTART_TYPE_SOFT 1

/* Maximum size of hunt path */
#define CSI_HUNT_PATH_SIZE             80

  CsiResult Csi_subscribeCoreState(U32 mpId);
  CsiResult Csi_unsubscribeCoreState(U32 mpId);

  CsiResult Csi_subscribeClusterRestart();
  CsiResult Csi_unsubscribeClusterRestart();
  CsiResult Csi_clusterRestartReply();

  CsiResult Csi_getOwnMpid(U32 *mpId);
  CsiResult Csi_getHuntPathPrefix(U32 mpId,
				  char *huntPathPrefix);

  /*
   * The next definitions are deprecated and will be removed.
   */
  typedef U32 CsiRole;
#define CSI_ROLE_ACTIVE	 0
#define CSI_ROLE_STANDBY 1
#define CSI_ROLE_REGULAR 2

  typedef U16 CsiOpState;
#define CSI_DISABLED 0
#define CSI_ENABLED  1

  /*
   * The next functions are deprecated and will be removed.
   * Use Csi_subscribeCoreState, Csi_unsubscribeCoreState, Csi_getHuntPathPrefix
   * instead.
   */
  CsiResult Csi_getRole(U32 mpId, CsiRole *role);
  CsiResult Csi_subscribeRole(U32 mpId);
  CsiResult Csi_unsubscribeRole(U32 mpId);
  CsiResult Csi_getHuntPath(U32 mpId,
			    char *huntPath);

  /*
   * The next functions are deprecated and will be removed
   * Operational state is not applicable for this interface.
   */
  CsiResult Csi_get_state(U32 mpId, CsiOpState *opState);
  CsiResult Csi_subscribe(U32 mpId);
  CsiResult Csi_unsubscribe(U32 mpId);

#ifdef __cplusplus
}
#endif

#endif   /* CLH_CSI_H */
