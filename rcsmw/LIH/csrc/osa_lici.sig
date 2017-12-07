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
 * @(#) ClearCase ID: /vobs/cello/osa_src/OSA_CRX90149_1/LICI_CNX9011181/inc/cppint/osa_lici.sig /main/cppdev/2 09-03-30 12:22 uabinle #
 *
 * REVISION HISTORY
 * ----------------
 *
 * Revided: LMF/TT/L/N  Taru Koivisto  2002-08-06
 * Change:  Created
 *         
 *
 *                                                                         
 ****************************************************************************/


#ifndef OSA_LICI_SIG
#define OSA_LICI_SIG

/*
 ****************************************************************************
 *   INCLUDE FILES
 ****************************************************************************
 */
#include "ose.h"
#include "osetypes.h"
#include "cello_lici.h"

#ifdef __cplusplus
extern "C" {
#endif

/****************************************************************************
 *
 * Signal Name: OSA_LICI_FEATURE_SUBSCRIBE_REQ
 *
 * Descr      : Signal sent to the lici_server from the lici proxy to request
 *              subscription to a licensed controlled feature.
 *
 * Data       : featureId : Feature key id
 *
 ***************************************************************************/
#define OSA_LICI_FEATURE_SUBSCRIBE_REQ (0x10669) /* !-SIGNO(struct OsaLiciFeatureSubscribeReq)-! */
typedef struct
{
  SIGSELECT          sigNo;  
  CelloLiciFeatureId featureId;
} OsaLiciFeatureSubscribeReq;

/****************************************************************************
 *
 * Signal Name: OSA_LICI_CAPACITY_SUBSCRIBE_REQ
 *
 * Descr      : Signal sent to the lici_server from the lici proxy to request
 *              subscription to a licensed controlled capacity.
 *
 * Data       : capacityId : Capacity key id
 *
 ***************************************************************************/
#define OSA_LICI_CAPACITY_SUBSCRIBE_REQ (0x1066A) /* !-SIGNO(struct OsaLiciCapacitySubscribeReq)-! */
typedef struct
{
  SIGSELECT           sigNo;  
  CelloLiciCapacityId capacityId;
} OsaLiciCapacitySubscribeReq;
				
/****************************************************************************
 *
 * Signal Name: OSA_LICI_STATUS_SUBSCRIBE_REQ
 *
 * Descr      : Signal sent to the lici_server from the lici proxy to request
 *              subscription to the license manager status.
 *
 * Data       : -
 *
 ***************************************************************************/
#define OSA_LICI_STATUS_SUBSCRIBE_REQ (0x1066B) /* !-SIGNO(struct OsaLiciStatusSubscribeReq)-! */
typedef struct
{
  SIGSELECT sigNo;  
} OsaLiciStatusSubscribeReq;
				
/****************************************************************************
 *
 * Signal Name: OSA_LICI_INITIATE_SERVICE_REQ
 *
 * Descr      : Signal sent to the lici_server from the lici proxy to request
 *              initiation of the service and negotiation of the protocol 
 *              version.
 *
 * Data       : signalRevision : signal revision
 *            : firstWantedPV : first wanted protocol version
 *            : secondWantedPV : second wanted protocol version
 *	      : thirdWantedPV : third wanted protocol version
 *
 ***************************************************************************/
#define OSA_LICI_INITIATE_SERVICE_REQ (0x1066C) /* !-SIGNO(struct OsaLiciInitiateServiceReq)-! */
typedef struct
{
  SIGSELECT                sigNo; 
  CelloLiciSignalRevision  signalRevision; 
  CelloLiciProtocolVersion firstWantedPV;
  CelloLiciProtocolVersion secondWantedPV;
  CelloLiciProtocolVersion thirdWantedPV;
} OsaLiciInitiateServiceReq;

/****************************************************************************
 *
 * Signal Name: OSA_LICI_TERMINATE_SERVICE_REQ
 *
 * Descr      : Signal sent to the lici_server from the lici proxy to request
 *              termination of the service and disconnection from the server.
 *
 * Data       : -
 *
 ***************************************************************************/
#define OSA_LICI_TERMINATE_SERVICE_REQ (0x1066D) /* !-SIGNO(struct OsaLiciTerminateServiceReq)-! */
typedef struct
{
  SIGSELECT sigNo;  
} OsaLiciTerminateServiceReq;

/****************************************************************************
 *
 * Signal Name: OSA_LICI_CLIENT_DOWN_IND
 *
 * Descr      : Signal sent to the lici_server to indicate the client is down.
 *
 * Data       : -
 *
 ***************************************************************************/
#define OSA_LICI_CLIENT_DOWN_IND (0x106D1) /* !-SIGNO(struct OsaLiciClientDownInd)-! */
typedef struct
{
  SIGSELECT sigNo;  
} OsaLiciClientDownInd;

/****************************************************************************
 *
 * Signal Name: OSA_LICI_IS_LKF_INSTALLED_REQ
 *
 * Descr      : Check if the LKF is installed or not.
 *
 * Data       : -
 *
 ***************************************************************************/
#define OSA_LICI_IS_LKF_INSTALLED_REQ (0x10AC5) /* !-SIGNO(struct OsaLiciIsLKFInstalledReq)-! */
typedef struct
{
  SIGSELECT sigNo;  
  CelloLiciSignalRevision  signalRevision;
} OsaLiciIsLKFInstalledReq;

	
#ifdef __cplusplus
}
#endif

#endif   /* OSA_LICI_SIG */
