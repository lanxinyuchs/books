/* ----------------------------------------------------------------------
 * %CCaseFile:	avli_proxy.c %
 * %CCaseRev:	/main/R2A/R3A/R4A/R5A/R6A/R8A/1 %
 * %CCaseDate:	2016-11-14 %
 * %CCaseDocNo: %
 * Author:	erarafo
 * Author: <name>, <e-mail address>
 *
 * Short description: AVLI interface.
 * ----------------------------------------------------------------------
 * %CCaseTemplateFile:	template.c %
 * %CCaseTemplateId: CCver: /main/1 %
 *
 * %CCaseCopyrightBegin%
 * Copyright (c) Ericsson AB 2012-2016 All rights reserved.
 * 
 * The information in this document is the property of Ericsson.
 * 
 * Except as specifically authorized in writing by Ericsson, the 
 * receiver of this document shall keep the information contained 
 * herein confidential and shall protect the same in whole or in 
 * part from disclosure and dissemination to third parties.
 * 
 * Disclosure and disseminations to the receivers employees shall 
 * only be made on a strict need to know basis.
 * %CCaseCopyrightEnd%
 *
 * ----------------------------------------------------------------------
 *
 * Revision history:
 *
 * Rev        Date       Name        What
 * -----      -------    --------    --------------------------
 * R5A/1      2015-12-01 erarafo     No use of TRI
 * R5A/2      2015-12-23 etomist     LITS removed, ITC added
 * R6A/1      2016-07-07 etxpeno     Coverity fixes
 * R6A/2      2016-07-12 etxarnu     Adjusted EVENT_SIZE for events containing
 *                                   PRODUCT_NAME (added 20) (MR2230)
 * R6A/3      2016-08-22 etxarnu     Upadated for PV5 version of AVLI (MR2230)
 * R8A/1      2016-11-14 etomist     HV40375, fix for SwPid == NULL in writePgmEvent
 * ----------------------------------------------------------------------
 ****************************************************************************

	CONTENTS
	--------

	1  Description
	2  Include files
	3  Signal include files
	4  Macros
	5  Types
	6  Function prototypes
	7  Signal definitions
	8  Constants
	9  Variables
	10 Functions
	11 Processes

 ****************************************************************************
 */

/*
 ****************************************************************************
 * 1  DESCRIPTION
 *
 *
 ****************************************************************************
 * This file contains the availability log proxy function. It provides an
 * access interface to the RBS CS availability log server.
 */

/*
 ****************************************************************************
 * 2  INCLUDE FILES
 ****************************************************************************
 */

#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif

#include <stddef.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>
#include <unistd.h>
#include "cello_avli.h"
#include "cec.h"
#include "itc.h"

#define TRACEPOINT_DEFINE
#include "alh_trace.h"

/*
 ****************************************************************************
 * 3  SIGNAL INCLUDE FILES
 ****************************************************************************
 */
#include "cello_avli.sig"
/*
 ****************************************************************************
 * 4  MACROS
 ****************************************************************************
 */

/* Tracing */

// INFO and ERROR trace are enabled by default.
// To enable ENTER trace as well do
//
//     te enable enter_trace '*'
//
#define ENTER(msg) TRACE_HELPER(enter_trace, "%s", msg)
#define INFO(fmt, ...)  TRACE_HELPER(info_trace, fmt, __VA_ARGS__)
#define ERROR(fmt, ...) TRACE_HELPER(error_trace, fmt, __VA_ARGS__)

#define TRACE_HELPER(type, fmt, ...) {				        \
    char *err_str;							\
    asprintf(&err_str,"%s:%d:%s():" fmt, __FILE__,__LINE__, __func__, __VA_ARGS__); \
    tracepoint(com_ericsson_rcs_alh, type, err_str);			\
    free(err_str);							\
}

/* DECLARE_INTERFACE(OSA_AVLI_IF) */

/* Availability State */
#define UNAVAILABLE (0)
#define INITIATING  (1)
#define AVAILABLE   (2)
#define SUSPENDED   (3)
#define TERMINATING (4)

/* Erlang i/f */

#define AVLI2_INIT_SERVICE         (7)
#define AVLI2_WRITE_PIU_EVENT      (9)
#define AVLI2_WRITE_HW_EVENT      (10)
#define AVLI2_WRITE_SERVICE_EVENT (11)
#define AVLI2_WRITE_OTHER_EVENT   (12)
#define AVLI3_WRITE_PGM_EVENT     (13)
#define AVLI4_WRITE_NODE_EVENT    (14)
#define AVLI5_WRITE_HW_EVENT      (15)

#define AVLI2_INIT_SERVICE_SIZE           (20)
#define AVLI2_WRITE_PIU_EVENT_SIZE      (1611)
#define AVLI2_WRITE_HW_EVENT_SIZE       (1713)
#define AVLI2_WRITE_SERVICE_EVENT_SIZE  (1682)
#define AVLI2_WRITE_OTHER_EVENT_SIZE   (15022)
#define AVLI3_WRITE_PGM_EVENT_SIZE      (1597)
#define AVLI4_WRITE_NODE_EVENT_SIZE     (1526)
#define AVLI5_WRITE_HW_EVENT_SIZE       (1733)


/*
 ****************************************************************************
 * 5  TYPES
 ****************************************************************************
 */
typedef struct
{
  U32 sigNo;
  itc_mbox_id_t spid;
  U32 initiatePV1;
  U32 state;
  U32 pvFirstWanted;
  U32 pvSecondWanted;
  U32 pvThirdWanted;
  U32 selectedPV;
  cec_handle_t *handle;

} AvliProxyData;

union SIGNAL
{
  U32 sigNo;
  AvliProxyData                    celloAvliData;
  CelloAvliWriteCfm                celloAvliWriteCfm;
  CelloAvliServerUpInd             celloAvliServerUpInd;
  CelloAvliServerDownInd           celloAvliServerDownInd;
  CelloAvli2InitiateServiceCfm     celloAvli2InitiateServiceCfm;
};


/*
 ****************************************************************************
 * 6  FUNCTION PROTOTYPES
 ****************************************************************************
 */

/*
 ****************************************************************************
 * 7  SIGNAL DEFINITIONS
 ****************************************************************************
 */

/*
 ****************************************************************************
 * 8  CONSTANTS
 ****************************************************************************
 */

/********** GLOBAL CONSTANTS ***********/

/********** LOCAL CONSTANTS ***********/


/*
 ****************************************************************************
 * 9  VARIABLES
 ****************************************************************************
 */


/********** GLOBAL VARIABLES ***********/

/********** LOCAL VARIABLES ***********/

/*
 ****************************************************************************
 * 10  FUNCTIONS
 ****************************************************************************
 */

/********** GLOBAL FUNCTIONS ***********/

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
 *  Return      : Pointer to allocated data area for availability log in
 *                this process
 *
 ************************************************************************/
union SIGNAL*
Cello_Avli_initiateMemory(void)
{
  union SIGNAL *avliBuffer_p;

  ENTER("Cello_Avli_initiateMemory");

  /* allocate data area for this service in this process in a signal buffer. */
  avliBuffer_p = (union SIGNAL*)itc_alloc(sizeof(AvliProxyData), 0);
  avliBuffer_p->celloAvliData.spid = itc_current_mbox();
  avliBuffer_p->celloAvliData.initiatePV1 = 0;
  avliBuffer_p->celloAvliData.state = UNAVAILABLE;
  avliBuffer_p->celloAvliData.pvFirstWanted = 0;
  avliBuffer_p->celloAvliData.pvSecondWanted = 0;
  avliBuffer_p->celloAvliData.pvThirdWanted = 0;
  avliBuffer_p->celloAvliData.selectedPV = 0;

  /* Return the pointer to the buffer. This pointer must be kept by
   * the client and supplied in every subsequent call to the proxy. */

  return avliBuffer_p;
}

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
Cello_Avli_initiateService( union SIGNAL* avliBuffer_p )
{

  ENTER("Cello_Avli_initiateService");

  (void) avliBuffer_p;

  return CELLO_AVLI_NOT_SUPPORTED_BY_SELECTED_PV;

}


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
                     union SIGNAL* sig_p )
{
  ENTER("Cello_Avli_internal");

  if (avliBuffer_p)
    {
      switch (sig_p->sigNo)
	{
	case CELLO_AVLI_SERVER_UP_IND:
	  break;
	case CELLO_AVLI_SERVER_DOWN_IND:
	  avliBuffer_p->celloAvliData.state = UNAVAILABLE;
	  break;
	case CELLO_AVLI2_INITIATE_SERVICE_REJ:
	  avliBuffer_p->celloAvliData.state = UNAVAILABLE;
	  break;
	case CELLO_AVLI2_INITIATE_SERVICE_CFM:
	  avliBuffer_p->celloAvliData.state = AVAILABLE;
	  avliBuffer_p->celloAvliData.selectedPV =
	    sig_p->celloAvli2InitiateServiceCfm.selectedPV;
	  break;
	default:
	  return CELLO_AVLI_ILLEGAL_SIGNAL;
	}
      return CELLO_AVLI_SUCCESS;
    }
  else
    {
      return CELLO_AVLI_MEMORY_NOT_INITIATED;
    }
}


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
                           CelloAvliReason    reason,
                           CelloAvliAddInfo   addInfo,
                           CelloAvliClientRef clientRef )
{

  ENTER( "Cello_Avli_writeNodeEvent" );

  (void) avliBuffer_p;
  (void) serviceStatus;
  (void) reason;
  (void) addInfo;
  (void) clientRef;

  return CELLO_AVLI_NOT_SUPPORTED_BY_SELECTED_PV;


}   /* Cello_Avli_writeNodeEvent() */


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
Cello_Avli_writePiuEvent( union SIGNAL* avliBuffer_p,
                          CelloAvliServiceStatus serviceStatus,
                          CelloAvliReason    reason,
                          CelloAvliPiuType   piuType,
                          CelloPiuHwAddr    *piuHwAddr,
                          Cello_PidInHW     *hwPid,
                          CelloAvliAddInfo   addInfo,
                          CelloAvliClientRef clientRef )
{

  ENTER( "Cello_Avli_writePiuEvent" );

  (void) avliBuffer_p;
  (void) serviceStatus;
  (void) reason;
  (void) piuType;
  (void) piuHwAddr;
  (void) hwPid;
  (void) addInfo;
  (void) clientRef;

  return CELLO_AVLI_NOT_SUPPORTED_BY_SELECTED_PV;

}


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
Cello_Avli_writeHwEvent( union SIGNAL* avliBuffer_p,
                         CelloAvliServiceStatus serviceStatus,
                         CelloAvliReason    reason,
                         CelloAvliHwType    hwType,
                         CelloAvliHwAddress hwAddress,
                         Cello_PidInHW     *hwPid,
                         CelloAvliAddInfo   addInfo,
                         CelloAvliClientRef clientRef )
{

  ENTER( "Cello_Avli_writeHwEvent" );

  (void) avliBuffer_p;
  (void) serviceStatus;
  (void) reason;
  (void) hwType;
  (void) hwAddress;
  (void) hwPid;
  (void) addInfo;
  (void) clientRef;

  return CELLO_AVLI_NOT_SUPPORTED_BY_SELECTED_PV;

}


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
Cello_Avli_writeServiceEvent( union SIGNAL* avliBuffer_p,
                              CelloAvliServiceStatus    serviceStatus,
                              CelloAvliReason           reason,
                              CelloAvliServiceType      serviceType,
                              CelloAvliServiceInstance  serviceInstance,
                              CelloAvliAddInfo          addInfo,
                              CelloAvliClientRef        clientRef)
{

  ENTER( "Cello_Avli_writeServiceEvent" );

  (void) avliBuffer_p;
  (void) serviceStatus;
  (void) reason;
  (void) serviceType;
  (void) serviceInstance;
  (void) addInfo;
  (void) clientRef;

  return CELLO_AVLI_NOT_SUPPORTED_BY_SELECTED_PV;

}


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
                           CelloAvliClientRef clientRef)
{

  ENTER("Cello_Avli_writeOtherEvent");

  (void) avliBuffer_p;
  (void) serviceStatus;
  (void) reason;
  (void) availabilityInfo;
  (void) clientRef;

  return CELLO_AVLI_NOT_SUPPORTED_BY_SELECTED_PV;

}

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
CelloAvli2_freeMemory(union SIGNAL** avliBuffer_pp)
{

  ENTER("CelloAvli2_freeMemory");

  if(*avliBuffer_pp)
    {
      cec_close(*(&(*avliBuffer_pp)->celloAvliData.handle)),
      itc_free((union itc_msg**)avliBuffer_pp);
    }

  return;

}


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
 *                CELLO_AVLI_SERVICE_UNAVAIL
 *                CELLO_AVLI_MEMORY_NOT_INITIATED
 *
 ************************************************************************/
Cello_AvliResult
CelloAvli2_initiateService( union SIGNAL* avliBuffer_p,
                            U32 pvFirstWanted,
                            U32 pvSecondWanted,
                            U32 pvThirdWanted )
{
  cec_packet_t packet;
  char buffer[AVLI2_INIT_SERVICE_SIZE];
  char *ptr;
  char signature[4] = {'A', 'V', 'L', 'I'} ;
  pid_t pid = getpid();

  ENTER("CelloAvli2_initiateService");

  if (avliBuffer_p)
    {
      INFO("Enter. Pid: %d, SPid: %d, State: %d",
	   (int)pid,
	   (int)avliBuffer_p->celloAvliData.spid,
	   avliBuffer_p->celloAvliData.state );

      avliBuffer_p->celloAvliData.initiatePV1 = False;
      avliBuffer_p->celloAvliData.pvFirstWanted = pvFirstWanted;
      avliBuffer_p->celloAvliData.pvSecondWanted = pvSecondWanted;
      avliBuffer_p->celloAvliData.pvThirdWanted = pvThirdWanted;
      avliBuffer_p->celloAvliData.selectedPV = 0;

      switch (avliBuffer_p->celloAvliData.state) {
      case UNAVAILABLE:

	avliBuffer_p->celloAvliData.handle = cec_open(signature,
						      sizeof(signature));

	if (avliBuffer_p->celloAvliData.handle == NULL) {
	  ERROR("cec_open failed, Pid: %d, SPid: %d, State: %d",
		(int)pid,
		(int)avliBuffer_p->celloAvliData.spid,
		avliBuffer_p->celloAvliData.state );
	  return CELLO_AVLI_SERVICE_UNAVAIL;
	}

	packet.length = sizeof(buffer);
	ptr = packet.data = buffer;

	*(U32*)ptr = AVLI2_INIT_SERVICE;
	ptr+=4;
	*(U32*)ptr = avliBuffer_p->celloAvliData.spid;
	ptr+=4;
	*(U32*)ptr = pvFirstWanted;
	ptr+=4;
	*(U32*)ptr = pvSecondWanted;
	ptr+=4;
	*(U32*)ptr = pvThirdWanted;

	if (cec_send_with_pid(avliBuffer_p->celloAvliData.handle, &packet) < 0)
	  {
	    ERROR("cec_send_with_pid failed, Pid: %d, SPid: %d, State: %d",
		  (int)pid,
		  (int)avliBuffer_p->celloAvliData.spid,
		  avliBuffer_p->celloAvliData.state );
	    return CELLO_AVLI_SERVICE_UNAVAIL;
	  }

	/* Update the availability state variables */
	avliBuffer_p->celloAvliData.state = INITIATING;

	INFO("Success. Pid: %d, SPid: %d, State: %d",
	     (int)pid,
	     (int)avliBuffer_p->celloAvliData.spid,
	     avliBuffer_p->celloAvliData.state );

	return CELLO_AVLI_SUCCESS;
      default:
	ERROR("Wrong state. Pid: %d, SPid: %d, State: %d",
	      (int)pid,
	      (int)avliBuffer_p->celloAvliData.spid,
	      avliBuffer_p->celloAvliData.state );
	return CELLO_AVLI_SERVICE_UNAVAIL;
      }

    }

  ERROR("Not using initiated memory, Pid: %d", (int)pid);
  return CELLO_AVLI_MEMORY_NOT_INITIATED;

}


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
                           U32 timeStamp,
                           CelloAvliServiceStatus serviceStatus,
                           CelloAvliReason    reason,
                           CelloAvliAddInfo   addInfo,
                           CelloAvliClientRef clientRef )
{
  ENTER( "CelloAvli2_writeNodeEvent" );

  (void) avliBuffer_p;
  (void) timeStamp;
  (void) serviceStatus;
  (void) reason;
  (void) addInfo;
  (void) clientRef;

  return CELLO_AVLI_NOT_SUPPORTED_BY_SELECTED_PV;

}   /* CelloAvli2_writeNodeEvent() */


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
CelloAvli2_writePiuEvent( union SIGNAL* avliBuffer_p,
                          U32 timeStamp,
                          CelloAvliServiceStatus serviceStatus,
                          CelloAvliReason    reason,
                          CelloAvliPiuType   piuType,
                          CelloPiuHwAddr    *piuHwAddr,
                          Cello_PidInHW     *hwPid,
                          CelloAvliAddInfo   addInfo,
                          CelloAvliClientRef clientRef )
{
  Cello_AvliResult avliResult;
  /* union SIGNAL *sendSig; */
  U32 addInfoLen;
  cec_packet_t packet;
  /* char buffer[AVLI2_WRITE_PIU_EVENT_SIZE]; */
  char *ptr;
  char *buffer;

  ENTER( "CelloAvli2_writePiuEvent" );

  if (!avliBuffer_p)
  {
    return CELLO_AVLI_MEMORY_NOT_INITIATED;
  }

  if ((avliBuffer_p->celloAvliData.state != AVAILABLE) ||
      avliBuffer_p->celloAvliData.initiatePV1)
  {
     return CELLO_AVLI_SERVICE_UNAVAIL;
  }

  avliResult = CELLO_AVLI_SUCCESS;


  if ( addInfo == NULL ) {
    addInfoLen = 0;
  } else {
    addInfoLen = strlen( addInfo );
    if ( addInfoLen > CELLO_AVLI_MAX_ADD_INFO_LEN - 1 ) {
      addInfoLen = CELLO_AVLI_MAX_ADD_INFO_LEN - 1;
      avliResult = CELLO_AVLI_TOO_LONG_STRING;
    }
  }

  packet.length = AVLI2_WRITE_PIU_EVENT_SIZE - CELLO_AVLI_MAX_ADD_INFO_LEN +
    addInfoLen + 1;
  buffer = malloc(packet.length);
  ptr = packet.data = buffer;

  *(U32*)ptr = AVLI2_WRITE_PIU_EVENT;
  ptr+=4;
  *(U32*)ptr = timeStamp;
  ptr+=4;
  *(U32*)ptr = serviceStatus;
  ptr+=4;
  *(U32*)ptr = reason;
  ptr+=4;
  *(U32*)ptr = piuType;
  ptr+=4;
  if ( piuHwAddr == NULL )
    {
      *(U16*)ptr = False;
      ptr+=2;
      memset(ptr, 0, 12);
      ptr+=12;
    }
  else
    {
      *(U16*)ptr = True;
      ptr+=2;
      *(U32*)ptr = piuHwAddr->smn;
      ptr+=4;
      *(U32*)ptr = piuHwAddr->apn;
      ptr+=4;
      *(U32*)ptr = piuHwAddr->ern;
      ptr+=4;
    }
  if ( hwPid == NULL )
    {
      *(U16*)ptr = False;
      ptr+=2;
      *(U8*)ptr = 0;
      ptr+=CELLO_MAX_PRODUCT_NUMBER_LEN;
      *(U8*)ptr = 0;
      ptr+=CELLO_MAX_PRODUCT_REVISION_LEN;
      *(U8*)ptr = 0;
      ptr+=CELLO_MAX_PRODUCT_NAME_LEN;
      *(U8*)ptr = 0;
      ptr+=CELLO_MAX_PRODUCT_DATE_LEN;
      *(U8*)ptr = 0;
      ptr+=CELLO_MAX_SERIAL_NUMBER_LEN;
    }
  else
    {
      *(U16*)ptr = True;
      ptr+=2;
      memcpy(ptr, hwPid->productNumber, CELLO_MAX_PRODUCT_NUMBER_LEN);
      ptr+=CELLO_MAX_PRODUCT_NUMBER_LEN;
      memcpy(ptr, hwPid->productRevision, CELLO_MAX_PRODUCT_REVISION_LEN);
      ptr+=CELLO_MAX_PRODUCT_REVISION_LEN;
      memcpy(ptr, hwPid->productName, CELLO_MAX_PRODUCT_NAME_LEN);
      ptr+=CELLO_MAX_PRODUCT_NAME_LEN;
      memcpy(ptr, hwPid->productDate, CELLO_MAX_PRODUCT_DATE_LEN);
      ptr+=CELLO_MAX_PRODUCT_DATE_LEN;
      memcpy(ptr, hwPid->serialNumber, CELLO_MAX_SERIAL_NUMBER_LEN);
      ptr+=CELLO_MAX_SERIAL_NUMBER_LEN;
    }
  *(U16*)ptr = addInfoLen + 1;
  ptr+=2;
  if ( addInfo == NULL )
    *(U8*)ptr = 0;
  else
    memcpy(ptr, addInfo, addInfoLen + 1);
  ptr+=addInfoLen + 1;
  *(U32*)ptr = clientRef;

  if (cec_send_with_pid(avliBuffer_p->celloAvliData.handle, &packet) < 0) {
    avliResult = CELLO_AVLI_SERVICE_UNAVAIL;
  }

  free(buffer);

  return avliResult;

}


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
CelloAvli2_writeHwEvent( union SIGNAL* avliBuffer_p,
                         U32 timeStamp,
                         CelloAvliServiceStatus serviceStatus,
                         CelloAvliReason    reason,
                         CelloAvliHwType    hwType,
                         CelloAvliHwAddress hwAddress,
                         Cello_PidInHW     *hwPid,
                         CelloAvliAddInfo   addInfo,
                         CelloAvliClientRef clientRef )
{
  Cello_AvliResult avliResult;
  U16 hwTypeLen;
  U16 hwAddressLen;
  U16 addInfoLen;
  cec_packet_t packet;
  char *ptr;
  char *buffer;

  ENTER( "CelloAvli2_writeHwEvent" );

  if (!avliBuffer_p)
  {
    return CELLO_AVLI_MEMORY_NOT_INITIATED;
  }

  if ((avliBuffer_p->celloAvliData.state != AVAILABLE) ||
      avliBuffer_p->celloAvliData.initiatePV1)
  {
     return CELLO_AVLI_SERVICE_UNAVAIL;
  }

  avliResult = CELLO_AVLI_SUCCESS;


  if ( hwType == NULL ) {
    hwTypeLen = 0;
  } else {
    hwTypeLen = (U16)strlen( (char*)hwType );
    if ( hwTypeLen > CELLO_AVLI_MAX_HW_TYPE_LEN - 1 ) {
      hwTypeLen = CELLO_AVLI_MAX_HW_TYPE_LEN - 1;
      avliResult = CELLO_AVLI_TOO_LONG_STRING;
    }
  }

  if ( hwAddress == NULL ) {
    hwAddressLen = 0;
  } else {
    hwAddressLen = (U16)strlen( (char*)hwAddress );
    if ( hwAddressLen > CELLO_AVLI_MAX_HW_ADDRESS_LEN - 1 ) {
      hwAddressLen = CELLO_AVLI_MAX_HW_ADDRESS_LEN - 1;
      avliResult = CELLO_AVLI_TOO_LONG_STRING;
    }
  }

  if ( addInfo == NULL ) {
    addInfoLen = 0;
  } else {
    addInfoLen = (U16)strlen( (char*)addInfo );
    if ( addInfoLen > CELLO_AVLI_MAX_ADD_INFO_LEN - 1 ) {
      addInfoLen = CELLO_AVLI_MAX_ADD_INFO_LEN - 1;
      avliResult = CELLO_AVLI_TOO_LONG_STRING;
    }
  }

  packet.length = AVLI2_WRITE_HW_EVENT_SIZE - CELLO_AVLI_MAX_ADD_INFO_LEN
    + addInfoLen + 1;
  buffer = malloc(packet.length);
  ptr = packet.data = buffer;

  *(U32*)ptr = AVLI2_WRITE_HW_EVENT;
  ptr+=4;
  *(U32*)ptr = timeStamp;
  ptr+=4;
  *(U32*)ptr = serviceStatus;
  ptr+=4;
  *(U32*)ptr = reason;
  ptr+=4;
  if ( hwType == NULL )
    {
      *(U8*)ptr = 0;
    }
  else
    {
      memset(ptr, 0, CELLO_AVLI_MAX_HW_TYPE_LEN);
      memcpy(ptr, hwType, hwTypeLen);
    }
  ptr+=CELLO_AVLI_MAX_HW_TYPE_LEN;
  if ( hwAddress == NULL )
    {
      *(U8*)ptr = 0;
    }
  else
    {
      memset(ptr, 0, CELLO_AVLI_MAX_HW_ADDRESS_LEN);
      memcpy(ptr, hwAddress, hwAddressLen);
    }
  ptr+=CELLO_AVLI_MAX_HW_ADDRESS_LEN;
  if ( hwPid == NULL )
    {
      *(U16*)ptr = False;
      ptr+=2;
      *(U8*)ptr = 0;
      ptr+=CELLO_MAX_PRODUCT_NUMBER_LEN;
      *(U8*)ptr = 0;
      ptr+=CELLO_MAX_PRODUCT_REVISION_LEN;
      *(U8*)ptr = 0;
      ptr+=CELLO_MAX_PRODUCT_NAME_LEN;
      *(U8*)ptr = 0;
      ptr+=CELLO_MAX_PRODUCT_DATE_LEN;
      *(U8*)ptr = 0;
      ptr+=CELLO_MAX_SERIAL_NUMBER_LEN;
    }
  else
    {
      *(U16*)ptr = True;
      ptr+=2;
      memcpy(ptr, hwPid->productNumber, CELLO_MAX_PRODUCT_NUMBER_LEN);
      ptr+=CELLO_MAX_PRODUCT_NUMBER_LEN;
      memcpy(ptr, hwPid->productRevision, CELLO_MAX_PRODUCT_REVISION_LEN);
      ptr+=CELLO_MAX_PRODUCT_REVISION_LEN;
      memcpy(ptr, hwPid->productName, CELLO_MAX_PRODUCT_NAME_LEN);
      ptr+=CELLO_MAX_PRODUCT_NAME_LEN;
      memcpy(ptr, hwPid->productDate, CELLO_MAX_PRODUCT_DATE_LEN);
      ptr+=CELLO_MAX_PRODUCT_DATE_LEN;
      memcpy(ptr, hwPid->serialNumber, CELLO_MAX_SERIAL_NUMBER_LEN);
      ptr+=CELLO_MAX_SERIAL_NUMBER_LEN;
    }
  *(U16*)ptr = addInfoLen + 1;
  ptr+=2;
  if ( addInfo == NULL )
    *(U8*)ptr = 0;
  else
    memcpy(ptr, addInfo, addInfoLen + 1);
  ptr+=addInfoLen + 1;
  *(U32*)ptr = clientRef;

  if (cec_send_with_pid(avliBuffer_p->celloAvliData.handle, &packet) < 0) {
    avliResult = CELLO_AVLI_SERVICE_UNAVAIL;
  }

  free(buffer);

  return avliResult;

}

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
CelloAvli5_writeHwEvent( union SIGNAL* avliBuffer_p,
                         U32 timeStamp,
                         CelloAvliServiceStatus serviceStatus,
                         CelloAvliReason    reason,
                         CelloAvliHwType    hwType,
                         CelloAvliHwAddress hwAddress,
                         Cello_Avli_PidInHW     *hwPid,
                         CelloAvliAddInfo   addInfo,
                         CelloAvliClientRef clientRef )
{
  Cello_AvliResult avliResult;
  U16 hwTypeLen;
  U16 hwAddressLen;
  U16 addInfoLen;
  cec_packet_t packet;
  char *ptr;
  char *buffer;

  ENTER( "CelloAvli5_writeHwEvent" );

  if (!avliBuffer_p)
  {
    return CELLO_AVLI_MEMORY_NOT_INITIATED;
  }

  if ((avliBuffer_p->celloAvliData.state != AVAILABLE) ||
      avliBuffer_p->celloAvliData.initiatePV1)
  {
     return CELLO_AVLI_SERVICE_UNAVAIL;
  }

  avliResult = CELLO_AVLI_SUCCESS;


  if ( hwType == NULL ) {
    hwTypeLen = 0;
  } else {
    hwTypeLen = (U16)strlen( (char*)hwType );
    if ( hwTypeLen > CELLO_AVLI_MAX_HW_TYPE_LEN - 1 ) {
      hwTypeLen = CELLO_AVLI_MAX_HW_TYPE_LEN - 1;
      avliResult = CELLO_AVLI_TOO_LONG_STRING;
    }
  }

  if ( hwAddress == NULL ) {
    hwAddressLen = 0;
  } else {
    hwAddressLen = (U16)strlen( (char*)hwAddress );
    if ( hwAddressLen > CELLO_AVLI_MAX_HW_ADDRESS_LEN - 1 ) {
      hwAddressLen = CELLO_AVLI_MAX_HW_ADDRESS_LEN - 1;
      avliResult = CELLO_AVLI_TOO_LONG_STRING;
    }
  }

  if ( addInfo == NULL ) {
    addInfoLen = 0;
  } else {
    addInfoLen = (U16)strlen( (char*)addInfo );
    if ( addInfoLen > CELLO_AVLI_MAX_ADD_INFO_LEN - 1 ) {
      addInfoLen = CELLO_AVLI_MAX_ADD_INFO_LEN - 1;
      avliResult = CELLO_AVLI_TOO_LONG_STRING;
    }
  }

  packet.length = AVLI5_WRITE_HW_EVENT_SIZE - CELLO_AVLI_MAX_ADD_INFO_LEN
    + addInfoLen + 1;
  buffer = malloc(packet.length);
  ptr = packet.data = buffer;

  *(U32*)ptr = AVLI5_WRITE_HW_EVENT;
  ptr+=4;
  *(U32*)ptr = timeStamp;
  ptr+=4;
  *(U32*)ptr = serviceStatus;
  ptr+=4;
  *(U32*)ptr = reason;
  ptr+=4;
  if ( hwType == NULL )
    {
      *(U8*)ptr = 0;
    }
  else
    {
      memset(ptr, 0, CELLO_AVLI_MAX_HW_TYPE_LEN);
      memcpy(ptr, hwType, hwTypeLen);
    }
  ptr+=CELLO_AVLI_MAX_HW_TYPE_LEN;
  if ( hwAddress == NULL )
    {
      *(U8*)ptr = 0;
    }
  else
    {
      memset(ptr, 0, CELLO_AVLI_MAX_HW_ADDRESS_LEN);
      memcpy(ptr, hwAddress, hwAddressLen);
    }
  ptr+=CELLO_AVLI_MAX_HW_ADDRESS_LEN;
  if ( hwPid == NULL )
    {
      *(U16*)ptr = False;
      ptr+=2;
      *(U8*)ptr = 0;
      ptr+=CELLO_MAX_PRODUCT_NUMBER_LEN;
      *(U8*)ptr = 0;
      ptr+=CELLO_MAX_PRODUCT_REVISION_LEN;
      *(U8*)ptr = 0;
      ptr+=CELLO_AVLI_MAX_PRODUCT_NAME_LEN;
      *(U8*)ptr = 0;
      ptr+=CELLO_MAX_PRODUCT_DATE_LEN;
      *(U8*)ptr = 0;
      ptr+=CELLO_MAX_SERIAL_NUMBER_LEN;
    }
  else
    {
      *(U16*)ptr = True;
      ptr+=2;
      memcpy(ptr, hwPid->productNumber, CELLO_MAX_PRODUCT_NUMBER_LEN);
      ptr+=CELLO_MAX_PRODUCT_NUMBER_LEN;
      memcpy(ptr, hwPid->productRevision, CELLO_MAX_PRODUCT_REVISION_LEN);
      ptr+=CELLO_MAX_PRODUCT_REVISION_LEN;
      memcpy(ptr, hwPid->productName, CELLO_AVLI_MAX_PRODUCT_NAME_LEN);
      ptr+=CELLO_AVLI_MAX_PRODUCT_NAME_LEN;
      memcpy(ptr, hwPid->productDate, CELLO_MAX_PRODUCT_DATE_LEN);
      ptr+=CELLO_MAX_PRODUCT_DATE_LEN;
      memcpy(ptr, hwPid->serialNumber, CELLO_MAX_SERIAL_NUMBER_LEN);
      ptr+=CELLO_MAX_SERIAL_NUMBER_LEN;
    }
  *(U16*)ptr = addInfoLen + 1;
  ptr+=2;
  if ( addInfo == NULL )
    *(U8*)ptr = 0;
  else
    memcpy(ptr, addInfo, addInfoLen + 1);
  ptr+=addInfoLen + 1;
  *(U32*)ptr = clientRef;

  if (cec_send_with_pid(avliBuffer_p->celloAvliData.handle, &packet) < 0) {
    avliResult = CELLO_AVLI_SERVICE_UNAVAIL;
  }

  free(buffer);

  return avliResult;

}


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
CelloAvli2_writeServiceEvent( union SIGNAL* avliBuffer_p,
                              U32 timeStamp,
                              CelloAvliServiceStatus    serviceStatus,
                              CelloAvliReason           reason,
                              CelloAvliServiceType      serviceType,
                              CelloAvliServiceInstance  serviceInstance,
                              CelloAvliAddInfo          addInfo,
                              CelloAvliClientRef        clientRef)
{
  Cello_AvliResult avliResult;
  U16 serviceTypeLen;
  U16 serviceInstanceLen;
  U16 addInfoLen;
  cec_packet_t packet;
  char *ptr;
  char *buffer;

  ENTER( "CelloAvli2_writeServiceEvent" );

  if (!avliBuffer_p)
  {
    return CELLO_AVLI_MEMORY_NOT_INITIATED;
  }

  if ((avliBuffer_p->celloAvliData.state != AVAILABLE) ||
      avliBuffer_p->celloAvliData.initiatePV1)
  {
     return CELLO_AVLI_SERVICE_UNAVAIL;
  }

  avliResult = CELLO_AVLI_SUCCESS;


  if ( serviceType == NULL ) {
    serviceTypeLen = 0;
  } else {
    serviceTypeLen = (U16)strlen((char*)serviceType);
    if ( serviceTypeLen > CELLO_AVLI_MAX_SERVICE_TYPE_LEN - 1 ) {
      serviceTypeLen = CELLO_AVLI_MAX_SERVICE_TYPE_LEN - 1;
      avliResult = CELLO_AVLI_TOO_LONG_STRING;
    }
  }

  if ( serviceInstance == NULL ) {
    serviceInstanceLen = 0;
  } else {
    serviceInstanceLen = (U16)strlen((char*)serviceInstance);
    if ( serviceInstanceLen > CELLO_AVLI_MAX_SERVICE_INSTANCE_LEN - 1 ) {
      serviceInstanceLen = CELLO_AVLI_MAX_SERVICE_INSTANCE_LEN - 1;
      avliResult = CELLO_AVLI_TOO_LONG_STRING;
    }
  }

  if ( addInfo == NULL ) {
    addInfoLen = 0;
  } else {
    addInfoLen = (U16)strlen((char*)addInfo);
    if ( addInfoLen > CELLO_AVLI_MAX_ADD_INFO_LEN - 1 ) {
      addInfoLen = CELLO_AVLI_MAX_ADD_INFO_LEN - 1;
      avliResult = CELLO_AVLI_TOO_LONG_STRING;
    }
  }

  packet.length = AVLI2_WRITE_SERVICE_EVENT_SIZE - CELLO_AVLI_MAX_ADD_INFO_LEN
    + addInfoLen + 1;
  buffer = malloc(packet.length);
  ptr = packet.data = buffer;

  *(U32*)ptr = AVLI2_WRITE_SERVICE_EVENT;
  ptr+=4;
  *(U32*)ptr = timeStamp;
  ptr+=4;
  *(U32*)ptr = serviceStatus;
  ptr+=4;
  *(U32*)ptr = reason;
  ptr+=4;

  if ( serviceType == NULL )
    *(U8*)ptr = 0;
  else
    memcpy(ptr, serviceType, serviceTypeLen);

  ptr += serviceTypeLen;
  memset(ptr, 0, (CELLO_AVLI_MAX_SERVICE_TYPE_LEN - serviceTypeLen));
  ptr += (CELLO_AVLI_MAX_SERVICE_TYPE_LEN - serviceTypeLen);

  if ( serviceInstance == NULL )
    *(U8*)ptr = 0;
  else
    memcpy(ptr, serviceInstance, serviceInstanceLen);

  ptr += serviceInstanceLen;
  memset(ptr, 0, (CELLO_AVLI_MAX_SERVICE_INSTANCE_LEN - serviceInstanceLen));
  ptr += (CELLO_AVLI_MAX_SERVICE_INSTANCE_LEN - serviceInstanceLen);

  *(U16*)ptr = addInfoLen + 1;
  ptr+=2;

  if ( addInfo == NULL )
    *(U8*)ptr = 0;
  else
    memcpy(ptr, addInfo, addInfoLen + 1);

  ptr+=addInfoLen + 1;
  *(U32*)ptr = clientRef;

  if (cec_send_with_pid(avliBuffer_p->celloAvliData.handle, &packet) < 0) {
    avliResult = CELLO_AVLI_SERVICE_UNAVAIL;
  }

  free(buffer);

  return avliResult;

}


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
                           U32 timeStamp,
                           CelloAvliServiceStatus serviceStatus,
                           CelloAvliReason reason,
                           CelloAvliAvailabilityInfo availabilityInfo,
                           CelloAvliClientRef clientRef)
{
  Cello_AvliResult avliResult;
  U32 availabilityInfoLen;
  cec_packet_t packet;
  char *ptr;
  char *buffer;

  ENTER("CelloAvli2_writeOtherEvent");

  if (!avliBuffer_p)
  {
    return CELLO_AVLI_MEMORY_NOT_INITIATED;
  }

  if ((avliBuffer_p->celloAvliData.state != AVAILABLE) ||
      avliBuffer_p->celloAvliData.initiatePV1)
  {
     return CELLO_AVLI_SERVICE_UNAVAIL;
  }

  if (availabilityInfo == NULL)
  {
     return CELLO_AVLI_ILLEGAL_PARAM;
  }

  avliResult = CELLO_AVLI_SUCCESS;

  availabilityInfoLen = strlen( (char*)availabilityInfo );
  if ( availabilityInfoLen > CELLO_AVLI_MAX_AVAILABILITY_INFO_LEN - 1 ) {
    availabilityInfoLen = CELLO_AVLI_MAX_AVAILABILITY_INFO_LEN - 1;
    avliResult = CELLO_AVLI_TOO_LONG_STRING;
  }

  packet.length = AVLI2_WRITE_OTHER_EVENT_SIZE -
    CELLO_AVLI_MAX_AVAILABILITY_INFO_LEN + availabilityInfoLen + 1;
  buffer = malloc(packet.length);
  ptr = packet.data = buffer;

  *(U32*)ptr = AVLI2_WRITE_OTHER_EVENT;
  ptr+=4;
  *(U32*)ptr = timeStamp;
  ptr+=4;
  *(U32*)ptr = serviceStatus;
  ptr+=4;
  *(U32*)ptr = reason;
  ptr+=4;
  *(U16*)ptr = availabilityInfoLen + 1;
  ptr+=2;

  memcpy(ptr, availabilityInfo, availabilityInfoLen + 1);

  ptr+=availabilityInfoLen + 1;
  *(U32*)ptr = clientRef;

  if (cec_send_with_pid(avliBuffer_p->celloAvliData.handle, &packet) < 0) {
    avliResult = CELLO_AVLI_SERVICE_UNAVAIL;
  }

  free(buffer);

  return avliResult;
}


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
                          Cello_PidInSW     *swPid,
                          CelloAvliAddInfo   addInfo,
                          CelloAvliClientRef clientRef )
{
  Cello_AvliResult avliResult;
  U32 addInfoLen;
  cec_packet_t packet;
  char *ptr;
  char *buffer;

  ENTER( "CelloAvli3_writePgmEvent" );

  if (!avliBuffer_p)
    {
      return CELLO_AVLI_MEMORY_NOT_INITIATED;
    }

  if ((avliBuffer_p->celloAvliData.state != AVAILABLE) ||
      avliBuffer_p->celloAvliData.initiatePV1)
    {
      return CELLO_AVLI_SERVICE_UNAVAIL;
    }

  if (avliBuffer_p->celloAvliData.selectedPV < CELLO_AVLI_PV3)
    {
      return CELLO_AVLI_NOT_SUPPORTED_BY_SELECTED_PV;
    }

  avliResult = CELLO_AVLI_SUCCESS;


  if ( addInfo == NULL ) {
    addInfoLen = 0;
  } else {
    addInfoLen = strlen( addInfo );
    if ( addInfoLen > CELLO_AVLI_MAX_ADD_INFO_LEN - 1 ) {
      addInfoLen = CELLO_AVLI_MAX_ADD_INFO_LEN - 1;
      avliResult = CELLO_AVLI_TOO_LONG_STRING;
    }
  }

  packet.length = AVLI3_WRITE_PGM_EVENT_SIZE - CELLO_AVLI_MAX_ADD_INFO_LEN +
    addInfoLen + 1;
  buffer = malloc(packet.length);
  ptr = packet.data = buffer;

  *(U32*)ptr = AVLI3_WRITE_PGM_EVENT;
  ptr+=4;
  *(U32*)ptr = timeStamp;
  ptr+=4;
  *(U32*)ptr = serviceStatus;
  ptr+=4;
  *(U32*)ptr = reason;
  ptr+=4;
  *(U32*)ptr = piuType;
  ptr+=4;
  if ( piuHwAddr == NULL )
    {
      *(U16*)ptr = False;
      ptr+=2;
      memset(ptr, 0, 12);
      ptr+=12;
    }
  else
    {
      *(U16*)ptr = True;
      ptr+=2;
      *(U32*)ptr = piuHwAddr->smn;
      ptr+=4;
      *(U32*)ptr = piuHwAddr->apn;
      ptr+=4;
      *(U32*)ptr = piuHwAddr->ern;
      ptr+=4;
    }
  if ( swPid == NULL ) /* HV40375 */
    {
      *(U16*)ptr = False;
      ptr+=2;
      memset(ptr, 0, CELLO_MAX_PRODUCT_NUMBER_LEN);
      ptr+=CELLO_MAX_PRODUCT_NUMBER_LEN;
      memset(ptr, 0, CELLO_MAX_PRODUCT_REVISION_LEN);
      ptr+=CELLO_MAX_PRODUCT_REVISION_LEN;
      memset(ptr, 0, CELLO_MAX_PRODUCT_NAME_LEN);
      ptr+=CELLO_MAX_PRODUCT_NAME_LEN;
      memset(ptr, 0, CELLO_MAX_PRODUCT_DATE_LEN);
      ptr+=CELLO_MAX_PRODUCT_DATE_LEN;
    }
  else
    {
      *(U16*)ptr = True;
      ptr+=2;
      memcpy(ptr, swPid->productNumber, CELLO_MAX_PRODUCT_NUMBER_LEN);
      ptr+=CELLO_MAX_PRODUCT_NUMBER_LEN;
      memcpy(ptr, swPid->productRevision, CELLO_MAX_PRODUCT_REVISION_LEN);
      ptr+=CELLO_MAX_PRODUCT_REVISION_LEN;
      memcpy(ptr, swPid->productName, CELLO_MAX_PRODUCT_NAME_LEN);
      ptr+=CELLO_MAX_PRODUCT_NAME_LEN;
      memcpy(ptr, swPid->productDate, CELLO_MAX_PRODUCT_DATE_LEN);
      ptr+=CELLO_MAX_PRODUCT_DATE_LEN;
    }
  *(U16*)ptr = addInfoLen + 1;
  ptr+=2;
  if ( addInfo == NULL )
    *(U8*)ptr = 0;
  else
    memcpy(ptr, addInfo, addInfoLen + 1);

  ptr+=addInfoLen + 1;
  *(U32*)ptr = clientRef;

  if (cec_send_with_pid(avliBuffer_p->celloAvliData.handle, &packet) < 0) {
    avliResult = CELLO_AVLI_SERVICE_UNAVAIL;
  }

  free(buffer);

  return avliResult;
}


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
 *                eventId      : Unique Id for a log event. Not valid for
 *                               applications. Applications can set this
 *                               parameter to 0.
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
			   U32 timeStamp,
                           CelloAvliServiceStatus serviceStatus,
                           CelloAvliReason reason,
                           U32 eventId,
                           CelloAvliAddInfo addInfo,
                           CelloAvliClientRef clientRef )
{
  Cello_AvliResult avliResult;
  U32 addInfoLen;
  cec_packet_t packet;
  char *ptr;
  char *buffer;

  ENTER( "CelloAvli4_writeNodeEvent" );

  if (!avliBuffer_p)
  {
    return CELLO_AVLI_MEMORY_NOT_INITIATED;
  }

  if ((avliBuffer_p->celloAvliData.state != AVAILABLE) ||
      avliBuffer_p->celloAvliData.initiatePV1)
  {
     return CELLO_AVLI_SERVICE_UNAVAIL;
  }

  avliResult = CELLO_AVLI_SUCCESS;


  if ( addInfo == NULL ) {
    addInfoLen = 0;
  } else {
    addInfoLen = strlen( addInfo );
    if ( addInfoLen > CELLO_AVLI_MAX_ADD_INFO_LEN - 1 ) {
      addInfoLen = CELLO_AVLI_MAX_ADD_INFO_LEN - 1;
      avliResult = CELLO_AVLI_TOO_LONG_STRING;
    }
  }

  packet.length = AVLI4_WRITE_NODE_EVENT_SIZE - CELLO_AVLI_MAX_ADD_INFO_LEN +
    addInfoLen + 1;
  buffer = malloc(packet.length);
  ptr = packet.data = buffer;

  *(U32*)ptr = AVLI4_WRITE_NODE_EVENT;
  ptr+=4;
  *(U32*)ptr = timeStamp;
  ptr+=4;
  *(U32*)ptr = serviceStatus;
  ptr+=4;
  *(U32*)ptr = reason;
  ptr+=4;
  *(U32*)ptr = eventId;
  ptr+=4;
  *(U16*)ptr = addInfoLen + 1;
  ptr+=2;
  if ( addInfo != NULL )
    memcpy(ptr, addInfo, addInfoLen + 1);
  else
    *(U8*)ptr = 0;
  ptr+=addInfoLen + 1;
  *(U32*)ptr = clientRef;

  if (cec_send_with_pid(avliBuffer_p->celloAvliData.handle, &packet) < 0) {
    avliResult = CELLO_AVLI_SERVICE_UNAVAIL;
  }

  free(buffer);

  return avliResult;

}

/********** LOCAL FUNCTIONS ***********/
