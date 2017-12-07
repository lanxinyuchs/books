/*
 ****************************************************************************
 *
 * 
 * Copyright (c) Ericsson AB  2006 All rights reserved.
 *
 * The information in this document is the property of Ericsson. Except
 * as specifically authorized in writing by Ericsson,the receiver of this
 * document shall keep the information contained herein confidential
 * and shall protect the same in whole or in part from disclosure and
 * dissemination to third parties. Disclosure and disseminations to the
 * receiver's employees shall only be made on a strict need to know basis.
 *
 *
 *
 * @(#) ClearCase ID: /vobs/cello/osa_src/OSA_CRX90149_1/LICI_CNX9011181/inc/cppext/cello_lici.sig /main/cppdev/3 09-08-27 07:57 xcssrba #
 *
 * REVISION HISTORY
 * ----------------
 *
 * Revided: LMF/TT/L/N  Taru Koivisto  20-September-02.
 * Change:  Created
 *          2002-02-24 LMFJUS Signals CELLO_LICI_FEATURE_SUBSCRIBE_CFM
 *                     and CELLO_LICI_FEATURE_SUBSCRIBE_CFM: field
 *                     changeReason added
 *         
 * Revised: Ingmar Lanner Ekstrand 2009-03-10
 * Change:  Protocol version 3 (UABtr70302)
 *
 * Revised: Srilakshmi 2009-08-26
 * Change:  Changed comments  (WRNae25543) 
 *                                                                
 ****************************************************************************/
#ifndef CELLO_LICI_SIG
#define CELLO_LICI_SIG

#include "osetypes.h"
#include "ose.h"
#include "cello_lici.h"

#ifdef __cplusplus
extern "C" {
#endif

/*****************************************************************************
 *
 * Signal Name: CELLO_LICI_FEATURE_SUBSCRIBE_CFM
 *
 * Descr      : This signal confirms if the license controlled feature is
 *              enabled or disabled.
 *
 ****************************************************************************/
#define CELLO_LICI_FEATURE_SUBSCRIBE_CFM (0x1066E) /* !-SIGNO(CelloLiciFeatureSubscribeCfm)-! */
typedef struct
{
  SIGSELECT              sigNo;
  CelloLiciFeatureId     featureId;
  CelloLiciFeatureStatus featureStatus;
  CelloLiciChangeReason  changeReason;
} CelloLiciFeatureSubscribeCfm;

/*****************************************************************************
 *
 * Signal Name: CELLO_LICI_FEATURE_SUBSCRIBE_REJ
 *
 * Descr      : This signal rejects the subscription of the license
 *              controlled feature.
 *
 ****************************************************************************/
#define CELLO_LICI_FEATURE_SUBSCRIBE_REJ (0x1066F) /* !-SIGNO(CelloLiciFeatureSubscribeRej)-! */
typedef struct
{
   SIGSELECT             sigNo;
   CelloLiciFeatureId    featureId;
   CelloLiciRejectReason rejectReason;
} CelloLiciFeatureSubscribeRej;

/*****************************************************************************
 *
 * Signal Name: CELLO_LICI_CAPACITY_SUBSCRIBE_CFM
 *
 * Descr      : This signal confirms the value of the licensed level and the
 *		value of the hard limit.
 *
 ****************************************************************************/
#define CELLO_LICI_CAPACITY_SUBSCRIBE_CFM (0x10670) /* !-SIGNO(CelloLiciCapacitySubscribeCfm)-! */
typedef struct
{
  SIGSELECT              sigNo;
  CelloLiciCapacityId    capacityId;
  CelloLiciLicensedLevel licensedLevel;
  CelloLiciHardLimit     hardLimit;		
  CelloLiciChangeReason  changeReason;
} CelloLiciCapacitySubscribeCfm;

/*****************************************************************************
 *
 * Signal Name: CELLO_LICI_CAPACITY_SUBSCRIBE_REJ
 *
 * Descr      : This signal rejects the subscription of the license controlled
 *		capacity.
 *
 ****************************************************************************/
#define CELLO_LICI_CAPACITY_SUBSCRIBE_REJ (0x10671) /* !-SIGNO(CelloLiciCapacitySubscribeRej)-! */
typedef struct
{
  SIGSELECT             sigNo;
  CelloLiciCapacityId   capacityId;
  CelloLiciRejectReason rejectReason;
} CelloLiciCapacitySubscribeRej;

/*****************************************************************************
 *
 * Signal Name: CELLO_LICI_FEATURE_CHANGE_IND
 *
 * Descr      : This signal indicates that the value (enabled/disabled) of
 *		the license controlled feature has changed.
 *
 ****************************************************************************/
#define CELLO_LICI_FEATURE_CHANGE_IND (0x10672) /* !-SIGNO(CelloLiciFeatureChangeInd)-! */
typedef struct
{
  SIGSELECT              sigNo;
  CelloLiciFeatureId     featureId;
  CelloLiciFeatureStatus featureStatus;
  CelloLiciChangeReason  changeReason;
} CelloLiciFeatureChangeInd;

/*****************************************************************************
 *
 * Signal Name: CELLO_LICI_CAPACITY_CHANGE_IND
 *
 * Descr      : This signal indicates that the value of the licensed level/
 *		hard limit of the license controlled capacity has changed.
 *
 ****************************************************************************/
#define CELLO_LICI_CAPACITY_CHANGE_IND (0x10673) /* !-SIGNO(CelloLiciCapacityChangeInd)-! */
typedef struct
{
  SIGSELECT              sigNo;
  CelloLiciCapacityId    capacityId;
  CelloLiciLicensedLevel licensedLevel;
  CelloLiciHardLimit     hardLimit;	
  CelloLiciChangeReason  changeReason;	
} CelloLiciCapacityChangeInd;

/*****************************************************************************
 *
 * Signal Name: CELLO_LICI_STATUS_SUBSCRIBE_CFM
 *
 * Descr      : This signal confirms the license manager status.
 *
 ****************************************************************************/
#define CELLO_LICI_STATUS_SUBSCRIBE_CFM (0x10674) /* !-SIGNO(CelloLiciStatusSubscribeCfm)-! */
typedef struct
{
  SIGSELECT                 sigNo;
  CelloLiciLicMgrStatus     licenseManagerStatus;
  CelloLiciEmergencyCounter emergencyCounter;
} CelloLiciStatusSubscribeCfm;

/*****************************************************************************
 *
 * Signal Name: CELLO_LICI_STATUS_CHANGE_IND
 *
 * Descr      : This signal indicates the license manager status.
 *
 ****************************************************************************/
#define CELLO_LICI_STATUS_CHANGE_IND (0x10675) /* !-SIGNO(CelloLiciStatusChangeInd)-! */
typedef struct
{
  SIGSELECT                 sigNo;
  CelloLiciLicMgrStatus     licenseManagerStatus;
  CelloLiciEmergencyCounter emergencyCounter;
} CelloLiciStatusChangeInd;

/*****************************************************************************
 *
 * Signal Name: CELLO_LICI_SERVER_UP_IND
 *
 * Descr      : This signal should be received in the clients receive loop and  
 *              be forwarded to the proxy as parameter in a 
 *              Cello_Lici_internal(sig_p) call.
 *
 ****************************************************************************/
#define CELLO_LICI_SERVER_UP_IND (0x10676) /* !-SIGNO(CelloLiciServerUpInd)-! */
typedef struct
{
  SIGSELECT sigNo;
} CelloLiciServerUpInd;

/*****************************************************************************
 *
 * Signal Name: CELLO_LICI_SERVER_DOWN_IND
 *
 * Descr      : This signal should be received in the clients receive loop and  
 *              be forwarded to the proxy as parameter in a 
 *              Cello_Lici_internal (sig_p) call.
 *
 ****************************************************************************/
#define CELLO_LICI_SERVER_DOWN_IND (0x10677) /* !-SIGNO(CelloLiciServerDownInd)-! */
typedef struct
{
  SIGSELECT sigNo;
} CelloLiciServerDownInd;

/*****************************************************************************
 *
 * Signal Name: CELLO_LICI_SERVER_UNPUBLISH_IND
 *
 * Descr      : This signal should be received in the clients receive loop and  
 *              be forwarded to the proxy as parameter in a 
 *              Cello_Lici_internal(sig_p) call.
 *
 ****************************************************************************/
#define CELLO_LICI_SERVER_UNPUBLISH_IND (0x10678) /* !-SIGNO(CelloLiciServerUnpublishInd)-! */
typedef struct
{
  SIGSELECT sigNo;
} CelloLiciServerUnpublishInd;

/*****************************************************************************
 *
 * Signal Name: CELLO_LICI_INITIATE_SERVICE_CFM
 *
 * Descr      : This signal should be received in the clients receive loop and  
 *              be forwarded to the proxy as parameter in a 
 *              Cello_Lici_internal(sig_p) call.
 *
 ****************************************************************************/
#define CELLO_LICI_INITIATE_SERVICE_CFM (0x10679) /* !-SIGNO(CelloLiciInitiateServiceCfm)-! */
typedef struct
{
  SIGSELECT sigNo;
  CelloLiciSignalRevision   signalRevision;
  CelloLiciProtocolVersion  protocolVersion;
} CelloLiciInitiateServiceCfm;

/*****************************************************************************
 *
 * Signal Name: CELLO_LICI_INITIATE_SERVICE_SUS
 *
 * Descr      : This signal should be received in the clients receive loop and 
 *              be forwarded to the proxy as parameter in a 
 *              Cello_Lici_internal(sig_p) call.
 *
 ****************************************************************************/
#define CELLO_LICI_INITIATE_SERVICE_SUS (0x1067A) /* !-SIGNO(CelloLiciInitiateServiceSus)-! */
typedef struct
{
  SIGSELECT                sigNo;
  CelloLiciSignalRevision  signalRevision;
  CelloLiciProtocolVersion protocolVersion;
} CelloLiciInitiateServiceSus;

/*****************************************************************************
 *
 * Signal Name: CELLO_LICI_INITIATE_SERVICE_REJ
 *
 * Descr      : This signal should be received in the clients receive loop and  
 *              be forwarded to the proxy as parameter in a 
 *              Cello_Lici_internal(sig_p) call.
 *
 ****************************************************************************/
#define CELLO_LICI_INITIATE_SERVICE_REJ (0x1067B) /* !-SIGNO(CelloLiciInitiateServiceRej)-! */
typedef struct
{
  SIGSELECT                sigNo;
  CelloLiciSignalRevision  signalRevision;
  CelloLiciProtocolVersion protocolVersion;
  CelloLiciRejectReason    rejectReason;
} CelloLiciInitiateServiceRej;
  
/*****************************************************************************
 *
 * Signal Name: CELLO_LICI_TERMINATE_SERVICE_CFM
 *
 * Descr      : This signal should be received in the clients receive loop and  
 *              be forwarded to the proxy as parameter in a 
 *              Cello_Lici_internal(sig_p) call.
 *
 ****************************************************************************/
#define CELLO_LICI_TERMINATE_SERVICE_CFM (0x1067C) /* !-SIGNO(CelloLiciTerminateServiceCfm)-! */
typedef struct
{
  SIGSELECT sigNo;
} CelloLiciTerminateServiceCfm;
 
/*****************************************************************************
 *
 * Signal Name: CELLO_LICI_STATUS_SUBSCRIBE_REJ
 *
 * Descr      : This signal rejects subscription to license manager status.
 *
 ****************************************************************************/
#define CELLO_LICI_STATUS_SUBSCRIBE_REJ (0x106FB) /* !-SIGNO(CelloLiciStatusSubscribeRej)-! */
typedef struct
{
  SIGSELECT             sigNo;
  CelloLiciRejectReason rejectReason;
} CelloLiciStatusSubscribeRej;


/*****************************************************************************
 *
 * Signal Name: CELLO_LICI_IS_LKF_INSTALLED_RSP
 *
 * Descr      : 
 *
 ****************************************************************************/
#define CELLO_LICI_IS_LKF_INSTALLED_RSP (0x10AC6) /* !-SIGNO(CelloLiciIsLKFInstalledRsp)-! */
typedef struct
{
  SIGSELECT sigNo;
  CelloLiciSignalRevision  signalRevision;
  U32                      result;
} CelloLiciIsLKFInstalledRsp;

#ifdef __cplusplus
}

#endif

#endif
