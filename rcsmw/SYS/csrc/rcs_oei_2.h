/*
 *
 * Copyright (c) Ericsson AB  2017 All rights reserved.
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
 * Description: This is the interface for the object event service, from now
 *              called OEI. The purpose of the OEI is provide a unique event
 *              number for the processes within a node. And to derive 
 *              correlationId from an event number and a universally unique 
 *              identifier (UUID).
 *
 *              The event number is supposed to be connected to an error event
 *              that will be logged by the calling process. When an error is
 *              corrected, the event number must be referred in the correction
 *              log to make an association to the first error log.
 *
 *
 * @(#) ClearCase ID: #
 *
 *
 * REVISION HISTORY
 * ----------------
 *
 * Revised: elarrun
 * Change:  Created
 *
 */

#ifndef RCS_OEI_2_H
#define RCS_OEI_2_H

#ifdef __cplusplus
extern "C" {
#endif

/********** IMPORT ************************/

#include <stdint.h>

/********** EXPORTED CONSTANTS ************/

/* interface call results */
#define RCS_OEI_OK                  (RcsOeiResult_t)0
#define RCS_OEI_TIMEOUT             (RcsOeiResult_t)1
#define RCS_OEI_NO_SERVER           (RcsOeiResult_t)2
#define RCS_OEI_SERVER_ERROR        (RcsOeiResult_t)3
#define RCS_OEI_INVALID_UUID_FORMAT (RcsOeiResult_t)4

/* the oei server will never return this value */
#define RCS_OEI_UNSPECIFIED_EVENT_ID (uint32_t)0

/* it is possible to specify that calls do not timeout in this interface */
#define RCS_OEI_NO_TIMEOUT_VALUE (uint32_t)0

/* UUID length, 32 lowercase hex digits + 4 hyphens */
#define RCS_OEI_UUID_LEN 36 

/********** EXPORTED TYPES ****************/

/* return value type */
typedef uint16_t RcsOeiResult_t;

/* Container for a Universally Unique ID (UIID). */
typedef struct {
  /* UUID:s have a fixed size, added one for terminating null terminator */
  char value[RCS_OEI_UUID_LEN+1]; /* UUID:s have a fixed size. */
} RcsOeiSystemUuid_t;

/* Container for a Derived Correlation Universally Unique ID. */
typedef struct {
  /* UUID:s have a fixed size, added one for terminating null terminator */
  char value[RCS_OEI_UUID_LEN+1]; 
} RcsOeiDerivedCorrelationUuid_t;

/* Container for a Universally Unique ID. */
typedef struct {
  /* UUID:s have a fixed size, added one for terminating null terminator */
  char value[RCS_OEI_UUID_LEN+1]; 
} RcsOeiUuid_t;

/********** EXPORTED FUNCTIONS ************/


/************************************************************************
*
*  Name  : rcsOeiGetEventIdentity
*
*  Descr.: Executes a request to the object event service for an unique
*          event number.
*
*  Args. : uint32_t  timeoutValue  Timeout value for the answer
*                                  from the object event service.
*
*          uint32_t  *eventId_p    Returns the unique event number.
*
*  Return: A RcsOeiResult_t code.
*
************************************************************************/
RcsOeiResult_t
rcsOeiGetEventIdentity( uint32_t  timeoutValue,
                        uint32_t  *eventId_p );  /* Used as an output parameter */


/************************************************************************
*
*  Name  : rcsOeiGetDerivedCorrelationUUID
*
*  Descr.: Executes a request to the object event service for a
*          correlation UUID. The correlation UUID is derived by
*          merging eventId with systemUUID read from environment.
*
*  Args. : uint32_t      eventId       Unique event number received from
*                                      rcsOeiGetEventIdentity()
*
*          uint32_t      timeoutValue  Timeout value for the answer
*                                      from the object event service.
*
*          RcsOeiDerivedCorrelationUuid_t*  p_derivedCorrelationUUID  Output parameter,
*                                                                     pointer to a
*                                                                     correlation UUID struct
*  
*  Return: A RcsOeiResult_t code.
*
************************************************************************/
RcsOeiResult_t
rcsOeiGetDerivedCorrelationUUID( uint32_t      eventId,
                                 uint32_t      timeoutValue,
                                 RcsOeiDerivedCorrelationUuid_t*  derivedCorrelationUUID );    


/************************************************************************
*
*  Name  : rcsOeiGetDerivedCorrelationUUID2
*
*  Descr.: Executes a request to the object event service for a
*          correlation UUID. The correlation UUID is derived by
*          merging the input parameters eventId and systemUUID.
*
*  Args. : uint32_t      eventId        Unique event number received by
*                                       rcsOeiGetEventIdentity()
*
*          uint32_t      timeoutValue  Timeout value for the answer
*                                      from the object event service.
*
*          RcsOeiSystemUuid_t*  p_systemUUID   Pointer to a  universally unique 
*                                            identifier struct
*
*          RcsOeiDerivedCorrelationUuid_t*  p_derivedCorrelationUUID  Output parameter,
*                                                                     pointer to a
*                                                                     correlation UUID struct
*          
*  Return: A RcsOeiResult_t code.
*
************************************************************************/
RcsOeiResult_t
rcsOeiGetDerivedCorrelationUUID2( uint32_t       eventId,
                                  uint32_t       timeoutValue,
                                  RcsOeiSystemUuid_t*       p_systemUUID,
                                  RcsOeiDerivedCorrelationUuid_t*  p_derivedCorrelationUUID );


/************************************************************************
*
*  Name  : rcsOeiGetUUID
*
*  Descr.: Generates a UUID
*
*  Args:   RcsOeiUuid_t*  p_rcsOeiUUID  Output parameter, pointer to a UUID struct
*
*  Return: A RcsOeiResult_t code.
*
************************************************************************/
RcsOeiResult_t 
rcsOeiGetUUID( RcsOeiUuid_t*  p_rcsOeiUUID );


#ifdef __cplusplus
}
#endif


#endif   /* RCS_OEI_2_H */

