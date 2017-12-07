#ifndef CELLO_AVLI_H
#define CELLO_AVLI_H
/*
 * 
 * Copyright (c) Ericsson AB  2014-2016 All rights reserved.
 *
 * The information in this document is the property of Ericsson. Except
 * as specifically authorized in writing by Ericsson,the receiver of this
 * document shall keep the information contained herein confidential
 * and shall protect the same in whole or in part from disclosure and
 * dissemination to third parties. Disclosure and disseminations to the
 * receiver's employees shall only be made on a strict need to know basis.
 */
 
/********** IMPORT ************************/

#ifdef NO_LITS
#include "alh_lits_types.h"
#else
#include "cello_control_commontypes.h"
#endif

#ifdef __cplusplus
extern "C" {
#endif

#define CELLO_AVLI_NO_PV     (0)
#define CELLO_AVLI_PV2 	     (2)
#define CELLO_AVLI_PV3 	     (3)
#define CELLO_AVLI_PV4 	     (4)
#define CELLO_AVLI_PV5 	     (5)
#define CELLO_AVLI_LATEST_PV (CELLO_AVLI_PV5)

/********** EXPORTED TYPES ****************/

/* String lengths including \0-termination. */
#define CELLO_AVLI_MAX_ADD_INFO_LEN           (1500)
#define CELLO_AVLI_MAX_HW_TYPE_LEN              (60)
#define CELLO_AVLI_MAX_HW_ADDRESS_LEN           (60)
#define CELLO_AVLI_MAX_SERVICE_TYPE_LEN         (60)
#define CELLO_AVLI_MAX_SERVICE_INSTANCE_LEN    (100)
#define CELLO_AVLI_MAX_AVAILABILITY_INFO_LEN (15000)

/* Incresased CELLO_MAX_PRODUCT_NAME_LEN limit to support
 * new RadioSystem market names.
 */
#define CELLO_AVLI_MAX_PRODUCT_NAME_LEN         (33)

/* Introduced in CELLO_AVLI_PV5
 * to support new RadioSystem market names.
 */
typedef struct
{
  U8 productNumber[CELLO_MAX_PRODUCT_NUMBER_LEN];
  U8 productRevision[CELLO_MAX_PRODUCT_REVISION_LEN];
  U8 productName[CELLO_AVLI_MAX_PRODUCT_NAME_LEN];
  U8 productDate[CELLO_MAX_PRODUCT_DATE_LEN];
  U8 serialNumber[CELLO_MAX_SERIAL_NUMBER_LEN];
} Cello_Avli_PidInHW;

#define CELLO_AVLI_LOG_SERVER_NAME "CELLO_AVLI_AVAILABILITY_LOG_SERVER"

#define CELLO_AVLI_TIME_BY_AVLI           (0xFFFFFFFF)


typedef U16 Cello_AvliResult;
#define CELLO_AVLI_SUCCESS                      (0)
#define CELLO_AVLI_SERVICE_UNAVAIL              (1)
#define CELLO_AVLI_TOO_LONG_STRING              (2)
#define CELLO_AVLI_OUT_OF_MEMORY                (3)
#define CELLO_AVLI_ILLEGAL_SIGNAL               (4)
#define CELLO_AVLI_ILLEGAL_EVENT                (5)
#define CELLO_AVLI_LOG_NOT_CREATED              (6)
#define CELLO_AVLI_ILLEGAL_PARAM                (7)
#define CELLO_AVLI_MEMORY_NOT_INITIATED         (8)
/* Introduced in CELLO_AVLI_PV3 */
#define CELLO_AVLI_NOT_SUPPORTED_BY_SELECTED_PV (9)

typedef enum {
  CELLO_AVLI_WRITE_NODE_EVENT,
  CELLO_AVLI_WRITE_PIU_EVENT,
  CELLO_AVLI_WRITE_HW_EVENT,
  CELLO_AVLI_WRITE_SERVICE_EVENT,
  CELLO_AVLI_WRITE_OTHER_EVENT, 
  CELLO_AVLI_WRITE_PGM_EVENT,
  CELLO_AVLI_WRITE_HW5_EVENT
} CelloAvliCmd;

typedef enum {
  CELLO_AVLI_EVENT_NOT_USED,
  CELLO_AVLI_IN_SERVICE,
  CELLO_AVLI_OUT_OF_SERVICE,
  CELLO_AVLI_PARTIALLY_OUT_OF_SERVICE
} CelloAvliServiceStatus;

typedef enum {
  CELLO_AVLI_REASON_NOT_USED,
  CELLO_AVLI_SHUTDOWN_COMMAND,
  CELLO_AVLI_UNOPERATIONAL,
  CELLO_AVLI_STARTING,
  CELLO_AVLI_OPERATIONAL
} CelloAvliReason;

typedef enum {
  CELLO_AVLI_NONE,
  CELLO_AVLI_MP,
  CELLO_AVLI_BP
} CelloAvliPiuType;


typedef char* CelloAvliHwType;
typedef char* CelloAvliHwAddress;
typedef char* CelloAvliServiceType;
typedef char* CelloAvliServiceInstance;
typedef char* CelloAvliAddInfo;
typedef char* CelloAvliAvailabilityInfo;
typedef U32 CelloAvliClientRef;

/********** EXPORTED CONSTANTS ************/

/********** EXPORTED FUNCTIONS ************/

/*
********* FUNCTIONS DEFINED IN PROTOCOL VERSION 1 **********
*/

/************************************************************************
 *
 *  Name        : Cello_Avli_initiateMemory
 *
 *  Description : This function should be called once in each process
 *                in connection with start of the client SW in the
 *                process
 *                
 *  Arguments   : -
 *
 *  Return      : Pointer to allocated data area for avaliability log in
 *                this process
 *                
 ************************************************************************/
union SIGNAL*
Cello_Avli_initiateMemory(void);


/************************************************************************
 *
 *  Name        : Cello_Avli_initiateService
 *
 *  Description : This function should be called once in each process
 *                in connection with start of the client SW in the
 *                process
 *                
 *  Arguments   : avliBuffer_p : Pointer to data area allocated and 
 *                               returned from  Cello_Avli_initiateMemory
 *
 *  Return      : CELLO_AVLI_SUCCESS
 *                CELLO_AVLI_MEMORY_NOT_INITIATED
 *                
 ************************************************************************/
Cello_AvliResult
Cello_Avli_initiateService( union SIGNAL* avliBuffer_p );


/************************************************************************
 *
 *  Name        : Cello_Avli_internal
 *
 *  Description : This function is a wrapper to be called by the client
 *                when any of the signals CELLO_AVLI_HUNT_NS_IND,
 *                CELLO_AVLI_ATTACH_NS_IND, CELLO_AVLI_SERVER_UP_IND,
 *                CELLO_AVLI_SERVER_DOWN_IND or CELLO_AVLI_UNPUBLISH_IND is
 *                received in the clients receive loop.
 *                
 *  Arguments   : avliBuffer_p : Pointer to data area allocated and
 *                               returned from Cello_Avli_initiateMemory
 *
 *                sig_p        : Signal received by the client
 *
 *  Return      : CELLO_AVLI_SUCCESS
 *                CELLO_AVLI_ILLEGAL_SIGNAL
 *                CELLO_AVLI_MEMORY_NOT_INITIATED
 *                
 ************************************************************************/
Cello_AvliResult
Cello_Avli_internal( union SIGNAL* avliBuffer_p,
                     union SIGNAL* sig_p );


/************************************************************************
 *
 *  Name        : Cello_Avli_writeNodeEvent
 *
 *  Description : This function will write one node availability event
 *                record to the availability log.
 *                
 *  Arguments   : avliBuffer_p : Pointer to data area allocated and
 *                               returned from  Cello_Avli_initiateMemory
 *
 *                serviceStatus: CELLO_AVLI_EVENT_NOT_USED
 *                               CELLO_AVLI_IN_SERVICE
 *                               CELLO_AVLI_OUT_OF_SERVICE
 *                               CELLO_AVLI_PARTIALLY_OUT_OF_SERVICE
 *
 *                reason       : CELLO_AVLI_REASON_NOT_USED
 *                               CELLO_AVLI_SHUTDOWN_COMMAND
 *                               CELLO_AVLI_UNOPERATIONAL
 *                               CELLO_AVLI_STARTING
 *                               CELLO_AVLI_OPERATIONAL
 *
 *                addInfo      : Character string defined by client. Max 
 *                               1500 chars.
 *
 *                clientRef    : Not stored in log. Can be used by client
 *                               to match indications with requests.
 *                
 *  Return      : CELLO_AVLI_SUCCESS
 *                CELLO_AVLI_SERVICE_UNAVAIL
 *                CELLO_AVLI_TOO_LONG_STRING
 *                CELLO_AVLI_MEMORY_NOT_INITIATED
 *                
 ************************************************************************/
Cello_AvliResult
Cello_Avli_writeNodeEvent( union SIGNAL* avliBuffer_p,
                           CelloAvliServiceStatus serviceStatus,
                           CelloAvliReason reason,
                           CelloAvliAddInfo addInfo,
                           CelloAvliClientRef clientRef );


/************************************************************************
 *
 *  Name        : Cello_Avli_writePiuEvent
 *
 *  Description : This function will write one PIU availability event
 *                record to the availability log.
 *                
 *  Arguments   : avliBuffer_p : Pointer to data area allocated and
 *                               returned from  Cello_Avli_initiateMemory
 *
 *                serviceStatus: CELLO_AVLI_EVENT_NOT_USED
 *                               CELLO_AVLI_IN_SERVICE
 *                               CELLO_AVLI_OUT_OF_SERVICE
 *                               CELLO_AVLI_PARTIALLY_OUT_OF_SERVICE
 *
 *                reason       : CELLO_AVLI_REASON_NOT_USED
 *                               CELLO_AVLI_SHUTDOWN_COMMAND
 *                               CELLO_AVLI_UNOPERATIONAL
 *                               CELLO_AVLI_STARTING
 *                               CELLO_AVLI_OPERATIONAL
 *               
 *                piuType      : CELLO_AVLI_NONE
 *                               CELLO_AVLI_MP
 *                               CELLO_AVLI_BP
 *
 *                piuHwAddr    : Hw addr (smn, apn) (CelloPiuHwAddr).
 *
 *                hwPid        : Pointer to struct with hardware product
 *                               information data
 *
 *                addInfo      : Character string defined by client. Max 
 *                               1500 chars.
 *
 *                clientRef    : Not stored in log. Can be used by client
 *                               to match indications with requests.
 *                
 *  Return      : CELLO_AVLI_SUCCESS
 *                CELLO_AVLI_SERVICE_UNAVAIL
 *                CELLO_AVLI_TOO_LONG_STRING
 *                CELLO_AVLI_MEMORY_NOT_INITIATED
 *                
 ************************************************************************/
Cello_AvliResult
Cello_Avli_writePiuEvent(union SIGNAL* avliBuffer_p,
                         CelloAvliServiceStatus serviceStatus,
                         CelloAvliReason reason,
                         CelloAvliPiuType piuType,
                         CelloPiuHwAddr *piuHwAddr,
                         Cello_PidInHW *hwPid,
                         CelloAvliAddInfo addInfo,
                         CelloAvliClientRef clientRef);


/************************************************************************
 *
 *  Name        : Cello_Avli_writeHwEvent
 *
 *  Description : This function will write one HW availability event
 *                record to the availability log.
 *                
 *  Arguments   : avliBuffer_p : Pointer to data area allocated and
 *                               returned from  Cello_Avli_initiateMemory
 *
 *                serviceStatus: CELLO_AVLI_EVENT_NOT_USED
 *                               CELLO_AVLI_IN_SERVICE
 *                               CELLO_AVLI_OUT_OF_SERVICE
 *                               CELLO_AVLI_PARTIALLY_OUT_OF_SERVICE
 *
 *                reason       : CELLO_AVLI_REASON_NOT_USED
 *                               CELLO_AVLI_SHUTDOWN_COMMAND
 *                               CELLO_AVLI_UNOPERATIONAL
 *                               CELLO_AVLI_STARTING
 *                               CELLO_AVLI_OPERATIONAL
 *
 *                hwType       : Character string defined by client. Max 
 *                               60 chars.
 *
 *                hwAddress    : Character string defined by client. Max 
 *                               60 chars.
 *
 *                hwPid        : Pointer to struct with hardware product
 *                               information data
 *
 *                addInfo      : Character string defined by client. Max 
 *                               1500 chars. If not used NULL.
 *
 *                clientRef    : Not stored in log. Can be used by client
 *                               to match indications with requests.
 *                
 *  Return      : CELLO_AVLI_SUCCESS
 *                CELLO_AVLI_SERVICE_UNAVAIL
 *                CELLO_AVLI_TOO_LONG_STRING
 *                CELLO_AVLI_MEMORY_NOT_INITIATED
 *                
 ************************************************************************/
Cello_AvliResult
Cello_Avli_writeHwEvent(union SIGNAL* avliBuffer_p,
                        CelloAvliServiceStatus serviceStatus,
                        CelloAvliReason reason,
                        CelloAvliHwType hwType,
                        CelloAvliHwAddress hwAddress,
                        Cello_PidInHW  *hwPid,
                        CelloAvliAddInfo addInfo,
                        CelloAvliClientRef clientRef);


/************************************************************************
 *
 *  Name        : Cello_Avli_writeServiceEvent
 *
 *  Description : This function will write one service availability event
 *                record to the availability log.
 *                
 *  Arguments   : avliBuffer_p : Pointer to data area allocated and
 *                               returned from  Cello_Avli_initiateMemory
 *
 *                serviceStatus: CELLO_AVLI_EVENT_NOT_USED
 *                               CELLO_AVLI_IN_SERVICE
 *                               CELLO_AVLI_OUT_OF_SERVICE
 *                               CELLO_AVLI_PARTIALLY_OUT_OF_SERVICE
 *
 *                reason       : CELLO_AVLI_REASON_NOT_USED
 *                               CELLO_AVLI_SHUTDOWN_COMMAND
 *                               CELLO_AVLI_UNOPERATIONAL
 *                               CELLO_AVLI_STARTING
 *                               CELLO_AVLI_OPERATIONAL
 *
 *                serviceType  : Character string defined by client. Max 
 *                               60 chars.
 *
 *                service-     : Character string defined by client. Max 
 *                Instance       60 chars.
 *
 *                addInfo      : Character string defined by client. Max 
 *                               1500 chars. If not used NULL.
 *
 *                clientRef    : Not stored in log. Can be used by client
 *                               to match indications with requests.
 *                
 *  Return      : CELLO_AVLI_SUCCESS
 *                CELLO_AVLI_SERVICE_UNAVAIL
 *                CELLO_AVLI_TOO_LONG_STRING
 *                CELLO_AVLI_MEMORY_NOT_INITIATED
 *                
 ************************************************************************/
Cello_AvliResult
Cello_Avli_writeServiceEvent(union SIGNAL* avliBuffer_p,
                             CelloAvliServiceStatus serviceStatus,
                             CelloAvliReason reason,
                             CelloAvliServiceType serviceType,
                             CelloAvliServiceInstance serviceInstance,
                             CelloAvliAddInfo addInfo,
                             CelloAvliClientRef clientRef);


/************************************************************************
 *
 *  Name        : Cello_Avli_writeOtherEvent
 *
 *  Description : This function will write one availability event
 *                record to the availability log.
 *
 *  Arguments   : avliBuffer_p    : Pointer to data area allocated and
 *                                  returned from  Cello_Avli_initiateMemory
 *
 *                serviceStatus   : CELLO_AVLI_EVENT_NOT_USED
 *                                  CELLO_AVLI_IN_SERVICE
 *                                  CELLO_AVLI_OUT_OF_SERVICE
 *                                  CELLO_AVLI_PARTIALLY_OUT_OF_SERVICE
 *
 *                reason          : CELLO_AVLI_REASON_NOT_USED
 *                                  CELLO_AVLI_SHUTDOWN_COMMAND
 *                                  CELLO_AVLI_UNOPERATIONAL
 *                                  CELLO_AVLI_STARTING
 *                                  CELLO_AVLI_OPERATIONAL
 *
 *                availabilityInfo: Character string defined by client. Max 
 *                                  15000 chars, must be provided.
 *
 *                clientRef       : Not stored in log. Can be used by client
 *                                  to match indications with requests.
 *                
 *  Return      : CELLO_AVLI_SUCCESS
 *                CELLO_AVLI_SERVICE_UNAVAIL
 *                CELLO_AVLI_TOO_LONG_STRING
 *                CELLO_AVLI_ILLEGAL_PARAM
 *                CELLO_AVLI_MEMORY_NOT_INITIATED
 *                
 ************************************************************************/
Cello_AvliResult
Cello_Avli_writeOtherEvent(union SIGNAL* avliBuffer_p,
                           CelloAvliServiceStatus serviceStatus,
                           CelloAvliReason reason,
                           CelloAvliAvailabilityInfo availabilityInfo,
                           CelloAvliClientRef clientRef);


/*
********* FUNCTIONS DEFINED IN PROTOCOL VERSION 2 **********
*/

/************************************************************************
 *
 *  Name        : CelloAvli2_freeMemory
 *
 *  Description : This function is called to free memory allocated by 
 *                Cello_Avli_initiateMemory.
 *                
 *                
 *  Arguments   : avliBuffer_pp : Pointer to a pointer to allocated memory.
 *
 *  Return      : -
 *                
 *                
 ************************************************************************/
void
CelloAvli2_freeMemory(union SIGNAL** avliBuffer_pp);


/************************************************************************
 *
 *  Name        : CelloAvli2_initiateService
 *
 *  Description : This function should be called once in each process
 *                in connection with start of the client SW in the
 *                process
 *                
 *  Arguments   : avliBuffer_p  : Pointer to data area allocated and
 *                                returned from  Cello_Avli_initiateMemory
 *
 *                pvFirstWanted : Protocol version first wanted
 *
 *                pvSecondWanted: Protocol version second wanted
 *
 *                pvThirdWanted : Protocol version third wanted
 *
 *  Return      : CELLO_AVLI_SUCCESS
 *                CELLO_AVLI_MEMORY_NOT_INITIATED
 *                
 ************************************************************************/
Cello_AvliResult
CelloAvli2_initiateService( union SIGNAL* avliBuffer_p,
                            U32 pvFirstWanted,
                            U32 pvSecondWanted, 
                            U32 pvThirdWanted);


/************************************************************************
 *
 *  Name        : CelloAvli2_writeNodeEvent
 *
 *  Description : This function will write one node availability event
 *                record to the availability log.
 *                
 *  Arguments   : avliBuffer_p : Pointer to data area allocated and
 *                               returned from  Cello_Avli_initiateMemory
 *
 *                timeStamp    : Seconds since Jan 1 1970 00:00:00, if time for
 *                               the event is submitted by client, or set to
 *                               CELLO_AVLI_TIME_BY_AVLI if the AVLI Service
 *                               shall set the time stamp.
 *
 *                serviceStatus: CELLO_AVLI_EVENT_NOT_USED
 *                               CELLO_AVLI_IN_SERVICE
 *                               CELLO_AVLI_OUT_OF_SERVICE
 *                               CELLO_AVLI_PARTIALLY_OUT_OF_SERVICE
 *
 *                reason       : CELLO_AVLI_REASON_NOT_USED
 *                               CELLO_AVLI_SHUTDOWN_COMMAND
 *                               CELLO_AVLI_UNOPERATIONAL
 *                               CELLO_AVLI_STARTING
 *                               CELLO_AVLI_OPERATIONAL
 *
 *                addInfo      : Character string defined by client. Max 
 *                               1500 chars. If not used NULL.
 *
 *                clientRef    : Not stored in log. Can be used by client
 *                               to match indications with requests.
 *                
 *  Return      : CELLO_AVLI_SUCCESS
 *                CELLO_AVLI_SERVICE_UNAVAIL
 *                CELLO_AVLI_TOO_LONG_STRING
 *                CELLO_AVLI_MEMORY_NOT_INITIATED
 *                
 ************************************************************************/
Cello_AvliResult
CelloAvli2_writeNodeEvent( union SIGNAL* avliBuffer_p,
			   U32 timestamp,
                           CelloAvliServiceStatus serviceStatus,
                           CelloAvliReason reason,
                           CelloAvliAddInfo addInfo,
                           CelloAvliClientRef clientRef );


/************************************************************************
 *
 *  Name        : CelloAvli2_writePiuEvent
 *
 *  Description : This function will write one PIU availability event
 *                record to the availability log.
 *                
 *  Arguments   : avliBuffer_p : Pointer to data area allocated and
 *                               returned from  Cello_Avli_initiateMemory
 *
 *                timeStamp    : Seconds since Jan 1 1970 00:00:00, if time for
 *                               the event is submitted by client, or set to
 *                               CELLO_AVLI_TIME_BY_AVLI if the AVLI Service
 *                               shall set the time stamp.
 *
 *                serviceStatus: CELLO_AVLI_EVENT_NOT_USED
 *                               CELLO_AVLI_IN_SERVICE
 *                               CELLO_AVLI_OUT_OF_SERVICE
 *                               CELLO_AVLI_PARTIALLY_OUT_OF_SERVICE
 *
 *                reason       : CELLO_AVLI_REASON_NOT_USED
 *                               CELLO_AVLI_SHUTDOWN_COMMAND
 *                               CELLO_AVLI_UNOPERATIONAL
 *                               CELLO_AVLI_STARTING
 *                               CELLO_AVLI_OPERATIONAL
 *
 *                piuType      : CELLO_AVLI_NONE
 *                               CELLO_AVLI_MP
 *                               CELLO_AVLI_BP
 *
 *                piuHwAddr    : Hw addr (smn, apn) (CelloPiuHwAddr).
 *
 *                hwPid        : Pointer to struct with hardware product
 *                               information data
 *
 *                addInfo      : Character string defined by client. Max 
 *                               1500 chars. If not used NULL.
 *
 *                clientRef    : Not stored in log. Can be used by client
 *                               to match indications with requests.
 *                
 *  Return      : CELLO_AVLI_SUCCESS
 *                CELLO_AVLI_SERVICE_UNAVAIL
 *                CELLO_AVLI_TOO_LONG_STRING
 *                CELLO_AVLI_MEMORY_NOT_INITIATED
 *                
 ************************************************************************/
Cello_AvliResult
CelloAvli2_writePiuEvent(union SIGNAL* avliBuffer_p,
			 U32 timestamp,
                         CelloAvliServiceStatus serviceStatus,
                         CelloAvliReason reason,
                         CelloAvliPiuType piuType,
                         CelloPiuHwAddr *piuHwAddr,
                         Cello_PidInHW *hwPid,
                         CelloAvliAddInfo addInfo,
                         CelloAvliClientRef clientRef);


/************************************************************************
 *
 *  Name        : CelloAvli2_writeHwEvent
 *
 *  Description : This function will write one HW availability event
 *                record to the availability log.
 *                
 *  Arguments   : avliBuffer_p : Pointer to data area allocated and
 *                               returned from  Cello_Avli_initiateMemory
 *
 *                timeStamp    : Seconds since Jan 1 1970 00:00:00, if time for
 *                               the event is submitted by client, or set to
 *                               CELLO_AVLI_TIME_BY_AVLI if the AVLI Service
 *                               shall set the time stamp.
 *
 *                serviceStatus: CELLO_AVLI_EVENT_NOT_USED
 *                               CELLO_AVLI_IN_SERVICE
 *                               CELLO_AVLI_OUT_OF_SERVICE
 *                               CELLO_AVLI_PARTIALLY_OUT_OF_SERVICE
 *
 *                reason       : CELLO_AVLI_REASON_NOT_USED
 *                               CELLO_AVLI_SHUTDOWN_COMMAND
 *                               CELLO_AVLI_UNOPERATIONAL
 *                               CELLO_AVLI_STARTING
 *                               CELLO_AVLI_OPERATIONAL
 *
 *                hwType       : Character string defined by client. Max 
 *                               60 chars.
 *
 *                hwAddress    : Character string defined by client. Max 
 *                               60 chars.
 *
 *                hwPid        : Pointer to struct with hardware product
 *                               information data
 *
 *                addInfo      : Character string defined by client. Max 
 *                               1500 chars. If not used NULL.
 *
 *                clientRef    : Not stored in log. Can be used by client
 *                               to match indications with requests.
 *                
 *  Return      : CELLO_AVLI_SUCCESS
 *                CELLO_AVLI_SERVICE_UNAVAIL
 *                CELLO_AVLI_TOO_LONG_STRING
 *                CELLO_AVLI_MEMORY_NOT_INITIATED
 *                
 ************************************************************************/
Cello_AvliResult
CelloAvli2_writeHwEvent(union SIGNAL* avliBuffer_p,
			U32 timestamp,
                        CelloAvliServiceStatus serviceStatus,
                        CelloAvliReason reason,
                        CelloAvliHwType hwType,
                        CelloAvliHwAddress hwAddress,
                        Cello_PidInHW  *hwPid,
                        CelloAvliAddInfo addInfo,
                        CelloAvliClientRef clientRef);


/************************************************************************
 *
 *  Name        : CelloAvli5_writeHwEvent
 *
 *  Description : This function will write one HW availability event
 *                record to the availability log.
 *                
 *  Arguments   : avliBuffer_p : Pointer to data area allocated and
 *                               returned from  Cello_Avli_initiateMemory
 *
 *                timeStamp    : Seconds since Jan 1 1970 00:00:00, if time for
 *                               the event is submitted by client, or set to
 *                               CELLO_AVLI_TIME_BY_AVLI if the AVLI Service
 *                               shall set the time stamp.
 *
 *                serviceStatus: CELLO_AVLI_EVENT_NOT_USED
 *                               CELLO_AVLI_IN_SERVICE
 *                               CELLO_AVLI_OUT_OF_SERVICE
 *                               CELLO_AVLI_PARTIALLY_OUT_OF_SERVICE
 *
 *                reason       : CELLO_AVLI_REASON_NOT_USED
 *                               CELLO_AVLI_SHUTDOWN_COMMAND
 *                               CELLO_AVLI_UNOPERATIONAL
 *                               CELLO_AVLI_STARTING
 *                               CELLO_AVLI_OPERATIONAL
 *
 *                hwType       : Character string defined by client. Max 
 *                               60 chars.
 *
 *                hwAddress    : Character string defined by client. Max 
 *                               60 chars.
 *
 *                hwPid        : Pointer to struct with hardware product
 *                               information data
 *
 *                addInfo      : Character string defined by client. Max 
 *                               1500 chars. If not used NULL.
 *
 *                clientRef    : Not stored in log. Can be used by client
 *                               to match indications with requests.
 *                
 *  Return      : CELLO_AVLI_SUCCESS
 *                CELLO_AVLI_SERVICE_UNAVAIL
 *                CELLO_AVLI_TOO_LONG_STRING
 *                CELLO_AVLI_MEMORY_NOT_INITIATED
 *                
 ************************************************************************/
Cello_AvliResult
CelloAvli5_writeHwEvent(union SIGNAL* avliBuffer_p,
			U32 timestamp,
                        CelloAvliServiceStatus serviceStatus,
                        CelloAvliReason reason,
                        CelloAvliHwType hwType,
                        CelloAvliHwAddress hwAddress,
                        Cello_Avli_PidInHW  *hwPid,
                        CelloAvliAddInfo addInfo,
                        CelloAvliClientRef clientRef);


/************************************************************************
 *
 *  Name        : CelloAvli2_writeServiceEvent
 *
 *  Description : This function will write one service availability event
 *                record to the availability log.
 *                
 *  Arguments   : avliBuffer_p : Pointer to data area allocated and
 *                               returned from Cello_Avli_initiateMemory
 *
 *                timeStamp    : Seconds since Jan 1 1970 00:00:00, if time
 *                               for the event is submitted by client, or
 *                               set to CELLO_AVLI_TIME_BY_AVLI if the AVLI
 *                               service shall set the time stamp.
 *
 *                serviceStatus: CELLO_AVLI_EVENT_NOT_USED
 *                               CELLO_AVLI_IN_SERVICE
 *                               CELLO_AVLI_OUT_OF_SERVICE
 *                               CELLO_AVLI_PARTIALLY_OUT_OF_SERVICE
 *
 *                reason       : CELLO_AVLI_REASON_NOT_USED
 *                               CELLO_AVLI_SHUTDOWN_COMMAND
 *                               CELLO_AVLI_UNOPERATIONAL
 *                               CELLO_AVLI_STARTING
 *                               CELLO_AVLI_OPERATIONAL
 *
 *                serviceType  : Character string defined by client. Max 
 *                               60 chars.
 *
 *                service-     : Character string defined by client. Max 
 *                Instance       60 chars.
 *
 *                addInfo      : Character string defined by client. Max 
 *                               1500 chars. If not used NULL.
 *
 *                clientRef    : Not stored in log. Can be used by client
 *                               to match indications with requests.
 *                
 *  Return      : CELLO_AVLI_SUCCESS
 *                CELLO_AVLI_SERVICE_UNAVAIL
 *                CELLO_AVLI_TOO_LONG_STRING
 *                CELLO_AVLI_MEMORY_NOT_INITIATED
 *                
 ************************************************************************/
Cello_AvliResult
CelloAvli2_writeServiceEvent(union SIGNAL* avliBuffer_p,
			     U32 timestamp,
                             CelloAvliServiceStatus serviceStatus,
                             CelloAvliReason reason,
                             CelloAvliServiceType serviceType,
                             CelloAvliServiceInstance serviceInstance,
                             CelloAvliAddInfo addInfo,
                             CelloAvliClientRef clientRef);


/************************************************************************
 *
 *  Name        : CelloAvli2_writeOtherEvent
 *
 *  Description : This function will write one availability event
 *                record to the availability log.
 *
 *  Arguments   : avliBuffer_p    : Pointer to data area allocated and
 *                                  returned from  Cello_Avli_initiateMemory
 *
 *                timeStamp       : Seconds since Jan 1 1970 00:00:00, if 
 *                                  time for the event is submitted by client,
 *                                  or set to CELLO_AVLI_TIME_BY_AVLI if the AVLI
 *                                  Service shall set the time stamp.
 *
 *                serviceStatus   : CELLO_AVLI_EVENT_NOT_USED
 *                                  CELLO_AVLI_IN_SERVICE
 *                                  CELLO_AVLI_OUT_OF_SERVICE
 *                                  CELLO_AVLI_PARTIALLY_OUT_OF_SERVICE
 *
 *                reason          : CELLO_AVLI_REASON_NOT_USED
 *                                  CELLO_AVLI_SHUTDOWN_COMMAND
 *                                  CELLO_AVLI_UNOPERATIONAL
 *                                  CELLO_AVLI_STARTING
 *                                  CELLO_AVLI_OPERATIONAL
 *
 *                availabilityInfo: Character string defined by client. Max 
 *                                  15000 chars, must be provided.
 *
 *                clientRef       : Not stored in log. Can be used by client
 *                                  to match indications with requests.
 *                
 *  Return      : CELLO_AVLI_SUCCESS
 *                CELLO_AVLI_SERVICE_UNAVAIL
 *                CELLO_AVLI_TOO_LONG_STRING
 *                CELLO_AVLI_ILLEGAL_PARAM
 *                CELLO_AVLI_MEMORY_NOT_INITIATED
 *                
 ************************************************************************/
Cello_AvliResult
CelloAvli2_writeOtherEvent(union SIGNAL* avliBuffer_p,
			   U32 timestamp,
                           CelloAvliServiceStatus serviceStatus,
                           CelloAvliReason reason,
                           CelloAvliAvailabilityInfo availabilityInfo,
                           CelloAvliClientRef clientRef);

/*
********* FUNCTIONS DEFINED IN PROTOCOL VERSION 3 **********
*/

/************************************************************************
 *
 *  Name        : CelloAvli3_writePgmEvent
 *
 *  Description : This function will write one PGM availability event
 *                record to the availability log.
 *                
 *  Arguments   : avliBuffer_p : Pointer to data area allocated and
 *                               returned from  Cello_Avli_initiateMemory
 *
 *                timeStamp    : Seconds since Jan 1 1970 00:00:00, if time for
 *                               the event is submitted by client, or set to
 *                               CELLO_AVLI_TIME_BY_AVLI if the AVLI Service
 *                               shall set the time stamp.
 *
 *                serviceStatus: CELLO_AVLI_EVENT_NOT_USED
 *                               CELLO_AVLI_IN_SERVICE
 *                               CELLO_AVLI_OUT_OF_SERVICE
 *                               CELLO_AVLI_PARTIALLY_OUT_OF_SERVICE
 *
 *                reason       : CELLO_AVLI_REASON_NOT_USED
 *                               CELLO_AVLI_SHUTDOWN_COMMAND
 *                               CELLO_AVLI_UNOPERATIONAL
 *                               CELLO_AVLI_STARTING
 *                               CELLO_AVLI_OPERATIONAL
 *
 *                piuType      : CELLO_AVLI_NONE
 *                               CELLO_AVLI_MP
 *                               CELLO_AVLI_BP
 *
 *                piuHwAddr    : Hw addr (smn, apn) (CelloPiuHwAddr).
 *
 *                swPid        : Pointer to struct with software product
 *                               information data (Cello_PidInSW).
 *
 *                addInfo      : Character string defined by client. Max 
 *                               1500 chars. If not used NULL.
 *
 *                clientRef    : Not stored in log. Can be used by client
 *                               to match indications with requests.
 *                
 *  Return      : CELLO_AVLI_SUCCESS
 *                CELLO_AVLI_SERVICE_UNAVAIL
 *                CELLO_AVLI_TOO_LONG_STRING
 *                CELLO_AVLI_MEMORY_NOT_INITIATED
 *                CELLO_AVLI_NOT_SUPPORTED_BY_SELECTED_PV
 *                
 ************************************************************************/
Cello_AvliResult
CelloAvli3_writePgmEvent( union SIGNAL* avliBuffer_p,
                          U32 timeStamp,
                          CelloAvliServiceStatus serviceStatus,
                          CelloAvliReason    reason,
                          CelloAvliPiuType   piuType,
                          CelloPiuHwAddr    *piuHwAddr,
                          Cello_PidInSW      *swPid,
                          CelloAvliAddInfo   addInfo,
                          CelloAvliClientRef clientRef );

/*
********* FUNCTIONS DEFINED IN PROTOCOL VERSION 4 **********
*/

/************************************************************************
 *
 *  Name        : CelloAvli4_writeNodeEvent
 *
 *  Description : This function will write one node availability event
 *                record to the availability log.
 *                
 *  Arguments   : avliBuffer_p : Pointer to data area allocated and
 *                               returned from  Cello_Avli_initiateMemory
 *
 *                timeStamp    : Seconds since Jan 1 1970 00:00:00, if time for
 *                               the event is submitted by client, or set to
 *                               CELLO_AVLI_TIME_BY_AVLI if the AVLI Service
 *                               shall set the time stamp.
 *
 *                serviceStatus: CELLO_AVLI_EVENT_NOT_USED
 *                               CELLO_AVLI_IN_SERVICE
 *                               CELLO_AVLI_OUT_OF_SERVICE
 *                               CELLO_AVLI_PARTIALLY_OUT_OF_SERVICE
 *
 *                reason       : CELLO_AVLI_REASON_NOT_USED
 *                               CELLO_AVLI_SHUTDOWN_COMMAND
 *                               CELLO_AVLI_UNOPERATIONAL
 *                               CELLO_AVLI_STARTING
 *                               CELLO_AVLI_OPERATIONAL
 *
 *                eventId      : Unique Id for a log event.
 *
 *                addInfo      : Character string defined by client. Max 
 *                               1500 chars. If not used NULL.
 *
 *                clientRef    : Not stored in log. Can be used by client
 *                               to match indications with requests.
 *                
 *  Return      : CELLO_AVLI_SUCCESS
 *                CELLO_AVLI_SERVICE_UNAVAIL
 *                CELLO_AVLI_TOO_LONG_STRING
 *                CELLO_AVLI_MEMORY_NOT_INITIATED
 *                
 ************************************************************************/
Cello_AvliResult
CelloAvli4_writeNodeEvent( union SIGNAL* avliBuffer_p,
			   U32 timestamp,
                           CelloAvliServiceStatus serviceStatus,
                           CelloAvliReason reason,
                           U32 eventId,
                           CelloAvliAddInfo addInfo,
                           CelloAvliClientRef clientRef );

#ifdef __cplusplus
}
#endif

#endif   /* CELLO_AVLI_H */

