/*****************************************************************************
 *
 *
 * Copyright (c) Ericsson AB  2006-2016 All rights reserved.
 *
 * The information in this document is the property of Ericsson. Except
 * as specifically authorized in writing by Ericsson,the receiver of this
 * document shall keep the information contained herein confidential
 * and shall protect the same in whole or in part from disclosure and
 * dissemination to third parties. Disclosure and disseminations to the
 * receiver's employees shall only be made on a strict need to know basis.
 *
 ****************************************************************************
 */
#include <stdlib.h>
#include <string.h>

#include "cello_te_trace.h"
#include "osetypes.h"
#include "ose.h"
#include "cello_lici.h"

/*
****************************************************************************
* 3  SIGNAL INCLUDE FILES
****************************************************************************
*/
#include "cello_lici.sig"
#include "osa_lici.sig"

/*
****************************************************************************
* 4  MACROS
****************************************************************************
*/

/*
****************************************************************************
* 5  TYPES
****************************************************************************
*/

/* Define values for availabilityState */
#define UNAVAILABLE (0)
#define INITIATING  (1)
#define AVAILABLE   (2)
#define SUSPENDED   (3)
#define TERMINATING (4)

typedef struct
{
  SIGSELECT sigNo;
  U32 availabilityState;     /* availability state           */
  PROCESS factoryPid;        /* server pid for management    */
  PROCESS servicePid;        /* server pid for service usage */
  OSATTREF serviceAttachRef; /* attach reference             */
  U32 pvFirstWanted;         /* data from the client         */
  U32 pvSecondWanted;        /* data from the client         */
  U32 pvThirdWanted;         /* data from the client         */
  U32 protocolVersion;       /* Selected protocol version    */
} ProxyData;

union SIGNAL
{
  SIGSELECT sigNo;
  CelloLiciInitiateServiceCfm celloLiciInitiateServiceCfm;
  OsaLiciInitiateServiceReq   osaLiciInitiateServiceReq;
  OsaLiciFeatureSubscribeReq  osaLiciFeatureSubscribeReq;
  OsaLiciCapacitySubscribeReq osaLiciCapacitySubscribeReq;
};

/************************************************************************
 *
 *  Name        : CelloLici_initiateMemory
 *  Description : This function should be called once in each process
 *                in connection with start of the client SW in the
 *                process
 *
 *  Arguments   : -
 *
 *
 *  Return      : pointer to allocated data area for licensing in
 *                this process
 *
 ***********************************************************************
 */
void*
CelloLici_initiateMemory(void)
{
  ProxyData *liciProxy_p;
  void* liciBuffer_p;

  ENTER("CelloLici_initiateMemory");

  liciBuffer_p = alloc(sizeof(ProxyData), 0);
  liciProxy_p = liciBuffer_p;

  /* Initiate default values for the data. */
  liciProxy_p->availabilityState = UNAVAILABLE;
  liciProxy_p->factoryPid = 0;
  liciProxy_p->servicePid = 0;

  /* Return the pointer to the buffer. This pointer must be kept by
   * the client and supplied in every subsequent call to the proxy. */

  RETURN liciBuffer_p;
}

/************************************************************************
 *
 *  Name        : CelloLici_initiateService
 *  Description : This function should be called when the application
 *                wants to connect (or reconnect) to the server.
 *                Protocol version negotiation is included in the request.
 *
 *  Arguments   : liciBuffer_p   : pointer to data area
 *                allocated and returned from  CelloLici_initiateMemory
 *                firstWantedPV  : first wanted protocol version
 *                secondWantedPV : second wanted protocol version
 *                thirdWantedPV  : third wanted protocol version
 *
 *  Return      : result of the operation
 *
 ***********************************************************************
 */
CelloLiciResult
CelloLici_initiateService(void *liciBuffer_p,
			  CelloLiciProtocolVersion firstWantedPV,
			  CelloLiciProtocolVersion secondWantedPV,
			  CelloLiciProtocolVersion thirdWantedPV)
{
  union SIGNAL *serverUpIndSig_p;

  ProxyData *liciProxy_p = liciBuffer_p;

  ENTER("CelloLici_initiateService");

  /* The server must be unavailable to accept an initiation */
  if (liciProxy_p->availabilityState != UNAVAILABLE) {
    RETURN CELLO_LICI_SERVER_NOT_UNAVAIL;
  }

  /* Store input data to be used later in OSA_LICI_INITIATE_SERVICE_REQ */
  liciProxy_p->pvFirstWanted  = firstWantedPV;
  liciProxy_p->pvSecondWanted = secondWantedPV;
  liciProxy_p->pvThirdWanted  = thirdWantedPV;

  serverUpIndSig_p = alloc(sizeof(CelloLiciServerUpInd),
			   CELLO_LICI_SERVER_UP_IND);

  hunt("LICI", 0, NULL, &serverUpIndSig_p);

  liciProxy_p->availabilityState = INITIATING;
  liciProxy_p->factoryPid = 0;
  liciProxy_p->servicePid = 0;

  RETURN CELLO_LICI_SUCCESS;
}

/************************************************************************
 *
 *  Name        : CelloLici_internal
 *
 *  Description : This function is called when the application forwards
 *                interface management signals that are received in the
 *                client's receive loop.
 *
 *  Arguments   : liciBuffer_p : pointer to data area
 *                allocated and returned from  CelloLici_initiateMemory
 *
 *                sig_p :signal received by the client
 *
 *  Return      : result of operation
 *
 ***********************************************************************
 */
CelloLiciResult
CelloLici_internal(void *liciBuffer_p, union SIGNAL* sig_p)
{
  union SIGNAL *serverDownIndSig_p;
  union SIGNAL *initServiceReqSig_p;

  ProxyData *liciProxy_p = liciBuffer_p;

  ENTER("CelloLici_internal");

  switch(sig_p->sigNo)
    {
    case CELLO_LICI_SERVER_UP_IND:
      /* Service factory is found, initiate the service.           */
      /* Store the pid of the service factory. It will be used for */
      /* the interface management signals.                         */
      liciProxy_p->factoryPid = sender(&sig_p);

      /* Supervise the service factory process by attaching to it */
      serverDownIndSig_p = alloc(sizeof(CelloLiciServerDownInd),
				 CELLO_LICI_SERVER_DOWN_IND);

      /* Save the reference to be able to detach in case of unpublish of */
      /* the server or terminating of the service                        */
      liciProxy_p->serviceAttachRef = attach(&serverDownIndSig_p,
					     liciProxy_p->factoryPid);

      /* Initiate the service by sending the signal               */
      /* OSA_LICI_INITIATE_SERVICE_REQ to the service factory.    */
      /* This implies protocol version negotiation and request on */
      /* subscription to "server unpublish indication"            */
      /* (when applicable).                                       */

      /* Allocate the signal buffer */
      initServiceReqSig_p = alloc(sizeof(OsaLiciInitiateServiceReq),
				  OSA_LICI_INITIATE_SERVICE_REQ);

      /* Write signal revision and previously stored data into the signal */
      initServiceReqSig_p->osaLiciInitiateServiceReq.signalRevision = 1;
      initServiceReqSig_p->osaLiciInitiateServiceReq.firstWantedPV  =
	liciProxy_p->pvFirstWanted;
      initServiceReqSig_p->osaLiciInitiateServiceReq.secondWantedPV =
	liciProxy_p->pvSecondWanted;
      initServiceReqSig_p->osaLiciInitiateServiceReq.thirdWantedPV  =
	liciProxy_p->pvThirdWanted;

      /* Send the signal to the service factory */
      itc_send((union itc_msg **)&initServiceReqSig_p, liciProxy_p->factoryPid,
	       ITC_MY_MBOX);
      break;
    case CELLO_LICI_INITIATE_SERVICE_CFM:
      /* Confirmation when the service is ready to be used */
      /* Store the pid of the service provider.            */
      /* It will be used for service usage signals.        */

      liciProxy_p->servicePid = sender(&sig_p);
      liciProxy_p->protocolVersion =
	sig_p->celloLiciInitiateServiceCfm.protocolVersion;

      /* No need to supervise the service providing process by
       * attaching to it, the attach to the service factory should
       * be enough */
      liciProxy_p->availabilityState = AVAILABLE;
      break;
    case CELLO_LICI_INITIATE_SERVICE_SUS:
      /* Service is suspended when wanted PV(s) is/are not available  */
      /* The "implied" subscription of "server unpublish indication"  */
      /* is provided by the server (when applicable). The proxy shall */
      /* keep the supervision (attach) of the service factory. */

      liciProxy_p->servicePid = 0;
      liciProxy_p->availabilityState = SUSPENDED;
      break;
    case CELLO_LICI_INITIATE_SERVICE_REJ:
      /* Total reject from the service */

    case CELLO_LICI_SERVER_UNPUBLISH_IND:
      /* The service has unpublished itself                         */
      /* Confirmation of the request for termination of the service */

    case CELLO_LICI_TERMINATE_SERVICE_CFM:
      /* This part is common for all three signals above. */
      /* Detach from the service factory.                 */
      detach(&(liciProxy_p->serviceAttachRef));
      /*fall thru */
    case CELLO_LICI_SERVER_DOWN_IND:
      /* Indication when the server is killed            */
      /* This part is common for all four signals above. */
      /* Update proxy internal data.                     */
      liciProxy_p->factoryPid = 0;
      liciProxy_p->servicePid = 0;
      liciProxy_p->availabilityState = UNAVAILABLE;
      break;
    default:
      RETURN CELLO_LICI_ILLEGAL_SIGNAL;
    }

  RETURN CELLO_LICI_SUCCESS;
}

/************************************************************************
 *
 *  Name        : CelloLici_terminateService
 *  Description : This function is called when the application wants to
 *                terminate the service.
 *
 *  Arguments   : liciBuffer_p : pointer to data area allocated and
 *                               returned from  CelloLici_initiateMemory
 *
 *  Return      : result of operation
 *
 ***********************************************************************
 */
CelloLiciResult
CelloLici_terminateService(void *liciBuffer_p)
{
  union SIGNAL *sig_p;

  ProxyData *liciProxy_p = liciBuffer_p;

  ENTER("CelloLici_terminateService");

  /* Check the server availability */
  if (liciProxy_p->availabilityState != AVAILABLE &&
      liciProxy_p->availabilityState != SUSPENDED) {
    RETURN CELLO_LICI_SERVICE_UNAVAIL;
  }

  /* Allocate the signal buffer */
  sig_p = alloc(sizeof(OsaLiciTerminateServiceReq),
		OSA_LICI_TERMINATE_SERVICE_REQ);

  /* Send the signal to the service factory */
  itc_send((union itc_msg **)&sig_p, liciProxy_p->factoryPid, ITC_MY_MBOX);

  /* Update the availability state variables */
  liciProxy_p->availabilityState = TERMINATING;

  RETURN CELLO_LICI_SUCCESS;
}

/************************************************************************
 *
 *  Name        : CelloLici_featureSubscription
 *  Description : This function is called when the application wants to
 *                request subscription to a license controlled feature.
 *
 *  Arguments   : liciBuffer_p : pointer to data area allocated and
 *                               returned from  CelloLici_initiateMemory
 *                featureId    : feature key id
 *
 *  Return      : result of operation
 *
 ***********************************************************************
 */
CelloLiciResult
CelloLici_featureSubscription(void *liciBuffer_p,
			      CelloLiciFeatureId featureId)
{
  union SIGNAL *sig_p;
  char *featureId_p;

  ProxyData *liciProxy_p = liciBuffer_p;

  ENTER("CelloLici_featureSubscription");

  /* Check the server availability */
  if (liciProxy_p->availabilityState != AVAILABLE) {
    RETURN CELLO_LICI_SERVICE_UNAVAIL;
  }

  /* Allocate the signal buffer */
  sig_p = alloc(sizeof(OsaLiciFeatureSubscribeReq),
		OSA_LICI_FEATURE_SUBSCRIBE_REQ);

  /* Write signal data */
  featureId_p = (char *) sig_p->osaLiciFeatureSubscribeReq.featureId;
  strncpy(featureId_p, (const char *) featureId, CELLO_LICI_MAX_ID_LENGTH);
  featureId_p[CELLO_LICI_MAX_ID_LENGTH - 1] = 0;

  /* Send the signal to the service provider */
  itc_send((union itc_msg **)&sig_p, liciProxy_p->servicePid, ITC_MY_MBOX);

  RETURN CELLO_LICI_SUCCESS;
}

CelloLiciResult
CelloLici_capacitySubscription(void *liciBuffer_p,
			       CelloLiciCapacityId capacityId)
{
  union SIGNAL *sig_p;
  char *capacityId_p;

  ProxyData *liciProxy_p = liciBuffer_p;

  ENTER("CelloLici_capacitySubscription");

  /* Check the server availability */
  if (liciProxy_p->availabilityState != AVAILABLE) {
    RETURN CELLO_LICI_SERVICE_UNAVAIL;
  }

  /* Allocate the signal buffer */
  sig_p = alloc(sizeof(OsaLiciCapacitySubscribeReq),
		OSA_LICI_CAPACITY_SUBSCRIBE_REQ);

  /* Write the signal data */
  capacityId_p = (char *) sig_p->osaLiciCapacitySubscribeReq.capacityId;
  strncpy(capacityId_p, (const char *) capacityId, CELLO_LICI_MAX_ID_LENGTH);
  capacityId_p[CELLO_LICI_MAX_ID_LENGTH - 1] = 0;

  /* Send the signal to the service provider */
  itc_send((union itc_msg **)&sig_p, liciProxy_p->servicePid, ITC_MY_MBOX);

  RETURN CELLO_LICI_SUCCESS;
}

/************************************************************************
 *
 *  Name        : CelloLici_statusSubscription
 *  Description : This function is called when the application wants to
 *                request subscription to the license manager status.
 *
 *  Arguments   : liciBuffer_p : pointer to data area allocated and
 *                               returned from  CelloLici_initiateMemory
 *
 *  Return      : result of operation
 *
 ***********************************************************************
 */
CelloLiciResult
CelloLici_statusSubscription(void *liciBuffer_p)
{
  union SIGNAL *sig_p;

  ProxyData *liciProxy_p = liciBuffer_p;

  ENTER("CelloLici_statusSubscription");

  /* Check the server availability */
  if (liciProxy_p->availabilityState != AVAILABLE) {
    RETURN CELLO_LICI_SERVICE_UNAVAIL;
  }

  /* Allocate the signal buffer */
  sig_p = alloc(sizeof(OsaLiciStatusSubscribeReq),
		OSA_LICI_STATUS_SUBSCRIBE_REQ);

  /* Send the signal to the service provider */
  itc_send((union itc_msg **)&sig_p, liciProxy_p->servicePid, ITC_MY_MBOX);

  RETURN CELLO_LICI_SUCCESS;
}

/************************************************************************
 *
 *  Name        : CelloLici_isLKFInstalled
 *  Description : This function is called when the application wants to
 *                check if a license key file is installed or not.
 *
 *  Arguments   : liciBuffer_p : pointer to data area allocated and
 *                               returned from  CelloLici_initiateMemory
 *
 *  Return      : CELLO_LICI_SUCCESS
 *                CELLO_LICI_SERVICE_UNAVAIL
 *                CELLO_LICI_NOT_SUPPORTED_BY_SELECTED_PV
 *
 ************************************************************************/
CelloLiciResult
CelloLici_isLKFInstalled(void *liciBuffer_p)
{
  union SIGNAL *sig_p;

  ProxyData *liciProxy_p = liciBuffer_p;

  ENTER("CelloLici_isLKFInstalled");

  /* Check the server availability */
  if (liciProxy_p->availabilityState != AVAILABLE) {
    RETURN CELLO_LICI_SERVICE_UNAVAIL;
  }

  /* Introduced in protocol version CELLO_LICI_PV3 */
  if (liciProxy_p->protocolVersion < CELLO_LICI_PV3) {
    RETURN CELLO_LICI_NOT_SUPPORTED_BY_SELECTED_PV;
  }

  /* Allocate the signal buffer */
  sig_p = alloc(sizeof(OsaLiciIsLKFInstalledReq),
		OSA_LICI_IS_LKF_INSTALLED_REQ);

  /* Send the signal to the service provider */
  itc_send((union itc_msg **)&sig_p, liciProxy_p->servicePid, ITC_MY_MBOX);

  RETURN CELLO_LICI_SUCCESS;
}
