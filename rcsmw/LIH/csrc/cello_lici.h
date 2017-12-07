#ifndef CELLO_LICI_H
#define CELLO_LICI_H
/*
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
 * IDENTIFICATION
 * --------------
 * Description: License Control Support Interface
 *
 * @(#) ClearCase ID: This line should be automatically created at checkin
 * 
 *
 * REVISION HISTORY
 * ----------------
 *
 * Revided: LMF/TT/X/J Taru Koivisto 12-September-02. 
 * Change:  Created
 *          2002-02-24 LMFJUS changeReason named values renamed
 *
 * Revised: Ingmar Lanner Ekstrand 2007-02-12
 * Change:  Protocol version 2
 *
 * Revised: Ingmar Lanner Ekstrand 2009-03-10
 * Change:  Protocol version 3 (UABtr70302)
 */

#ifdef __cplusplus
extern "C" {
#endif

/********** EXPORTED TYPES ****************/

typedef U16 CelloLiciResult;	
typedef U16 CelloLiciRejectReason;
	
#define CELLO_LICI_SUCCESS                      (0)
#define CELLO_LICI_SERVICE_UNAVAIL              (1)
#define CELLO_LICI_ILLEGAL_PARAM                (2)
#define CELLO_LICI_UNEXPECTED_ERROR             (3)
#define CELLO_LICI_ILLEGAL_SIGNAL               (4)
#define CELLO_LICI_ALREADY_SUBSCRIBED           (5)
#define CELLO_LICI_INVALID_PROTOCOLVERSION      (6)
#define CELLO_LICI_SERVER_NOT_UNAVAIL           (7)
#define CELLO_LICI_NOT_SUPPORTED_BY_SELECTED_PV (8)

#define CELLO_LICI_SERVER_NAME "CELLO_LICI_LICENSE_MANAGER_SERVER"

#define CELLO_LICI_PV1                          (1)
#define CELLO_LICI_PV2                          (2)
#define CELLO_LICI_PV3                          (3)
 
typedef struct CelloLiciLicensedLevel
{		
	U32 noLimit;
	U32 value;
} CelloLiciLicensedLevel;
	
typedef struct CelloLiciHardLimit
{		
	U32 noLimit;
	U32 value;
} CelloLiciHardLimit;	

typedef U32 CelloLiciChangeReason;
#define CELLO_LICI_LICENSED_VALUE               (0)
#define CELLO_LICI_EMERGENCY_REASON             (1)
#define CELLO_LICI_NOT_ACTIVATED                (2)

typedef U32 CelloLiciFeatureStatus;				    
#define CELLO_LICI_FEATURE_ENABLED              (0)
#define CELLO_LICI_FEATURE_DISABLED             (1)

typedef U32 CelloLiciLicMgrStatus;				    
#define CELLO_LICI_EMERGENCY_ACTIVATED          (0)
#define CELLO_LICI_EMERGENCY_DEACTIVATED        (1)

typedef U32 CelloLiciEmergencyCounter;	
#define CELLO_LICI_NO_EMERGENCY                 (0)			    
#define CELLO_LICI_EMERGENCY_ONCE               (1)
#define CELLO_LICI_EMERGENCY_TWICE              (2)

#define CELLO_LICI_LKF_INSTALLED                (0)
#define CELLO_LICI_LKF_NOT_INSTALLED            (1)


#define CELLO_LICI_MAX_ID_LENGTH               (24)

typedef U8 CelloLiciFeatureId[CELLO_LICI_MAX_ID_LENGTH];     
typedef U8 CelloLiciCapacityId[CELLO_LICI_MAX_ID_LENGTH];     
typedef U32 CelloLiciSignalRevision;
typedef U32 CelloLiciProtocolVersion;

/************************************************************************
 *
 *  Name        : CelloLici_initiateMemory
 *
 *  Description : This function should be called once in each process
 *                in connection with start of the client SW in the
 *                process
 *                
 *  Arguments   : -
 *
 *  Return      : pointer to allocated data area for licensing in
 *                this process
 *                
 ************************************************************************/
void*
CelloLici_initiateMemory(void);

/************************************************************************
 *
 *  Name        : CelloLici_initiateService
 *
 *  Description : This function should be called once in each process
 *                in connection with start of the client SW in the
 *                process
 *                
 *  Arguments   : liciBuffer_p : pointer to data area allocated and
 *                               returned from  CelloLici_initiateMemory
 *		: firstWantedPV : first wanted protocol version
 *              : secondWantedPV : second wanted protocol version
 *              : thirdWantedPV : third wanted protocol version 
 *
 *  Return      : result of the operation
 *                
 ************************************************************************/
CelloLiciResult
CelloLici_initiateService(void *liciBuffer_p,
                          CelloLiciProtocolVersion firstWantedPV,
                          CelloLiciProtocolVersion secondWantedPV,
                          CelloLiciProtocolVersion thirdWantedPV);

/************************************************************************
 *
 *  Name        : CelloLici_terminateService
 *
 *  Description : This function terminates the service and requests 
 *                disconnection from the server
 *                
 *  Arguments   : liciBuffer_p : pointer to data area
 *                allocated and returned from  CelloLici_initiateMemory
 *
 *  Return      : result of the operation
 *                
 ************************************************************************/
CelloLiciResult
CelloLici_terminateService(void *liciBuffer_p);

/************************************************************************
 *
 *  Name        : CelloLici_internal
 *
 *  Description : This function is a wrapper to be called by the client
 *                when any of the signals CELLO_LICI_SEVER_UP_IND,
 *                CELLO_LICI_SERVER_DOWN_IND, CELLO_LICI_UNPUBLISH_IND 
 *                CELLO_LICI_INITIATE_SERVICE_CFM, 
 *                CELLO_LICI_INITIATE_SERVICE_SUS, 
 *                CELLO_LICI_INITIATE_SERVICE_REJ or 
 *                CELLO_LICI_TERMINATE_SERVICE_CFM is
 *                received in the clients receive loop.
 *                
 *  Arguments   : liciBuffer_p : pointer to data area
 *                allocated and returned from  CelloLici_initiateMemory
 *
 *                sig_p :signal received by the client
 *
 *  Return      : result of the operation
 *                
 ************************************************************************/
CelloLiciResult
CelloLici_internal(void *liciBuffer_p, union SIGNAL* sig_p);
  
/************************************************************************
 *
 *  Name        : CelloLici_featureSubscription
 *
 *  Description : This function will request subscription to a license
 *                controlled feature.
 *                
 *  Arguments   : liciBuffer_p : pointer to data area allocated and
 *                               returned from  CelloLici_initiateMemory
 *		  featureId    : feature key id 
 *                
 *  Return      : result of the operation
 *                
 ************************************************************************/
CelloLiciResult
CelloLici_featureSubscription(void *liciBuffer_p,
                              CelloLiciFeatureId featureId);

/************************************************************************
 *
 *  Name        : CelloLici_capacitySubscription
 *
 *  Description : This function will request subscription to a license
 *                controlled capacity.
 *                
 *  Arguments   : liciBuffer_p : pointer to data area allocated and
 *                               returned from  CelloLici_initiateMemory
 *		  capacityId   : capacity key id	 
 *                
 *  Return      : result of the operation
 *                
 ************************************************************************/
CelloLiciResult
CelloLici_capacitySubscription(void *liciBuffer_p,
                               CelloLiciCapacityId capacityId);
                                                 
/************************************************************************
 *
 *  Name        : CelloLici_statusSubscription
 *
 *  Description : This function will request subscription to the license
 *                manager status.
 *                
 *  Arguments   : liciBuffer_p : pointer to data area allocated and
 *                               returned from  CelloLici_initiateMemory
 *                
 *  Return      : result of the operation
 *                
 ************************************************************************/
CelloLiciResult
CelloLici_statusSubscription (void *liciBuffer_p);

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
CelloLici_isLKFInstalled (void *liciBuffer_p);
                             
#ifdef __cplusplus
}
#endif

#endif   /* CELLO_LICI_H */

