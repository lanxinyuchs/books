/* ----------------------------------------------------------------------
 * %CCaseFile:	cello_tzii.h %
 * %CCaseRev:	/main/R4A/12 %
 * %CCaseDate:	2015-11-05 %
 * %CCaseDocNo: %
 * Author:	erarafo
 * Author: <name>, <e-mail address>
 *
 * Short description: See DESCRIPTION further down.
 * ----------------------------------------------------------------------
 * %CCaseCopyrightBegin%
 * Copyright (c) Ericsson AB 2015 All rights reserved.
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
 * G2 revision history:
 *
 * Rev        Date       Name        What
 * -----      -------    --------    --------------------------
 * R4A/1      2015-08-27 erarafo     First version
 * R4A/4      2015-10-20 erarafo     Added function return codes
 * R4A/5      2015-10-20 erarafo     Typo in return code name
 * R4A/6      2015-10-23 erarafo     Added a function return code
 * R4A/7      2015-10-26 erarafo     Changed function return codes
 * R4A/8      2015-10-27 erarafo     Added TRUE and FALSE
 * R4A/9      2015-10-29 erarafo     Added CELLO_TZII_UNKNOWN_CLIENT
 * R4A/10     2015-11-02 erarafo     Definition of SIGNAL cannot be here
 * R4A/11     2015-11-02 erarafo     Comment only
 * R4A/12     2015-11-05 erarafo     CelloTzii_setMailbox re-inserted
 * ----------------------------------------------------------------------
 */

/*
 * DESCRIPTION
 * -----------
 * This file contains the Time Zone Information Interface (TZII),
 * which is used to distribute information about Daylight Saving Time (DST)
 * and leap second changes.
 *
 * The interface follows standard pattern so a client should first
 * use  CelloTzii_initiateMemory function to allocate memory,
 * later use CelloTzii_initiateService for protocol negotiation.
 *
 * Currently only protocol version 1 is used.
 *
 *
 * G1 REVISION HISTORY
 * -------------------
 *
 * Revised: Francisco Jares 2013-07-22
 * Change:  First version
 *
 * Revised: Daniel Lefwerth 2014-10-01
 * Change:  Added support for scalable TSL.
 */

#ifndef CELLO_TZII_H
#define CELLO_TZII_H

#ifdef __cplusplus
extern "C"
{
#endif

/*
 ******************************************************************************
 * INCLUDE FILES
 ******************************************************************************
 */

#include <stdint.h>

/*
 ******************************************************************************
 * MACROS
 ******************************************************************************
 */

/* Service name*/
#define CELLO_TZII_SERVICE_NAME "CELLO_TIMESETTING_SERVICE_NAME"

/* Protocol versions */
#define CELLO_TZII_NO_PV                         (0)
#define CELLO_TZII_PV1                           (1)
#define CELLO_TZII_HIGHEST_PV                    (1)

/* Used in return signals to indicate that the attribute is not set. */
#define CELLO_TZII_UNDEFINED            (2147483647)

/* Represents DST status */
#define FALSE                                    (0)
#define TRUE                                     (1)

/*
 ******************************************************************************
 * TYPES
 ******************************************************************************
 */

/* CelloTziiResult, returned by the proxy functions */
typedef uint32_t CelloTziiResult;
#define CELLO_TZII_OK                            (1)
#define CELLO_TZII_MEMORY_NOT_INITIATED          (2)
#define CELLO_TZII_SERVER_UNAVAILABLE            (3)
#define CELLO_TZII_ILLEGAL_SIGNAL                (4)
#define CELLO_TZII_ACTIVE_SERVICE                (5)
#define CELLO_TZII_ALREADY_INITIATED             (6)
#define CELLO_TZII_INVALID_REVISION              (7)
#define CELLO_TZII_ILLEGAL_WHEN_AVAILABLE        (8)
#define CELLO_TZII_ILLEGAL_WHEN_INITIATING       (9)
#define CELLO_TZII_ILLEGAL_WHEN_SUSPENDED       (10)
#define CELLO_TZII_ILLEGAL_WHEN_TERMINATING     (11)
#define CELLO_TZII_UNKNOWN_CLIENT               (12)

#define CELLO_TZII_INTERNAL_ERROR_INIT_SVC     (900)
#define CELLO_TZII_INTERNAL_ERROR_TERM_SVC     (910)
#define CELLO_TZII_INTERNAL_ERROR_INTERNAL_1   (920)
#define CELLO_TZII_INTERNAL_ERROR_INTERNAL_2   (930)
#define CELLO_TZII_INTERNAL_ERROR_SUBSCR_DST   (940)
#define CELLO_TZII_INTERNAL_ERROR_SUBSCR_LEAP  (950)


/* CelloTziiServiceResult, returned in signals from server */
typedef uint32_t CelloTziiServiceResult;
#define CELLO_TZII_SERVICE_OK                    (1)
#define CELLO_TZII_SERVICE_NOT_REG_CLIENT        (2)
#define CELLO_TZII_SERVICE_ALREADY_INITIATED     (3)
#define CELLO_TZII_SERVICE_INVALID_PV            (4)
#define CELLO_TZII_PREWARNING_NOK                (5)


typedef uint32_t CelloTziiIndCause;
#define CELLO_TZII_PREWARNING_START         (0)
#define CELLO_TZII_PREWARNING_CANCEL        (1)
#define CELLO_TZII_ATTRIBUTE_OR_TIME_UPDATE (2)


/**
 * Applications wanting to use the signals in cello_tzii.sig
 * need to declare SIGNAL as a union of types. This piece
 * of code may be use as raw material for such a union
 * declaration.

union SIGNAL {
  SIGSELECT sigNo;
  CelloTziiServerUpInd servUpInd;
  CelloTziiServerDownInd servDownInd;
  CelloTziiInitiateServiceCfm initServCfm;
  CelloTziiInitiateServiceRej initServRej;
  CelloTziiInitiateServiceSus initServSus;
  CelloTziiTerminateServiceCfm termServCfm;
  CelloTziiServerUnpublishInd servUnpubInd;
  CelloTziiSubscribeDaylightSavingTimeCfm subscrDstCfm;
  CelloTziiSubscribeDaylightSavingTimeRej subscrDstRej;
  CelloTziiSubscribeDaylightSavingTimeInd subscrDstInd;
  CelloTziiSubscribeLeapSecondsCfm subscrLeapSecsCfm;
  CelloTziiSubscribeLeapSecondsRej subscrLeapSecsRej;
  CelloTziiSubscribeLeapSecondsInd subscrLeapSecsInd;
};
 */


/*
 ******************************************************************************
 * FUNCTION PROTOTYPES
 ******************************************************************************
 */

/******************************************************************************
 *
 * Allocates the TZII data.
 * This function should be called once in each process.
 *
 * @return an TZII interface handle
 *
 *****************************************************************************/
void *
CelloTzii_initiateMemory(void);


/******************************************************************************
 *
 * Initiates the TZII service.
 *
 * @param tziiMemory_p   - TZII handle
 * @param pvFirstWanted  - Protocol version first wanted
 * @param pvSecondWanted - Protocol version second wanted
 * @param pvThirdWanted  - Protocol version third wanted
 *
 * @return CELLO_TZII_OK,
 *         CELLO_TZII_ALREADY_INITIATED,
 *         CELLO_TZII_MEMORY_NOT_INITIATED
 *
 *****************************************************************************/
CelloTziiResult
CelloTzii_initiateService(void *tziiMemory_p,
                          uint32_t pvFirstWanted,
                          uint32_t pvSecondWanted,
                          uint32_t pvThirdWanted);

/******************************************************************************
 *
 * This function terminates the service and requests disconnection
 * from the server
 *
 * @param TZII interface handle
 *
 * @return CELLO_TZII_OK,
 *         CELLO_TZII_MEMORY_NOT_INITIATED
 *
 *****************************************************************************/
CelloTziiResult
CelloTzii_terminateService(void *tziiMemory_p);

/******************************************************************************
 *
 * This function is a wrapper to be called by the client when any of
 * the signals
 *   CELLO_TZII_SERVER_UP_IND
 *   CELLO_TZII_SERVER_DOWN_IND
 *   CELLO_TZII_INITIATE_SERVICE_CFM
 *   CELLO_TZII_INITIATE_SERVICE_REJ
 *   CELLO_TZII_INITIATE_SERVICE_SUS
 *   CELLO_TZII_TERMINATE_SERVICE_CFM
 * is received by the client.
 *
 * @param tziiMemory_p - TZII handle
 * @param sig_p - signal received by the client
 *
 * @return CELLO_TZII_OK,
 *         CELLO_TZII_ILLEGAL_SIGNAL,
 *         CELLO_TZII_MEMORY_NOT_INITIATED
 *
 *****************************************************************************/
CelloTziiResult
CelloTzii_internal(void *tziiMemory_p, union SIGNAL *sig_p);


/******************************************************************************
 *
 * This function is called to free memory allocated by CelloTzii_initiateMemory.
 *
 * @param tziiMemory_pp - pointer to TZII handle
 *
 * @return CELLO_TZII_OK,
 *         CELLO_TZII_ACTIVE_SERVICE,
 *         CELLO_TZII_MEMORY_NOT_INITIATED
 *
 *****************************************************************************/
CelloTziiResult
CelloTzii_freeMemory(void **tziiMemory_pp);


/*
 ****************************  PROTOCOL VERSION 1 *****************************
 */

/******************************************************************************
 *
 * Start subscription on Daylight Saving Time (DST) information.
 *
 * If client is already subscribing on DST information, the old subscription
 * is discarded and the client will receive a new confirm signal.
 *
 * @param tziiMemory_p   - TZII handle
 * @param clientInfo     - client data returned in indication signal
 * @param preWarningTime - advance notification in milliseconds (<= 23 hours)
 *
 * @return CELLO_TZII_OK
 *         CELLO_TZII_SERVER_UNAVAILABLE
 *         CELLO_TZII_MEMORY_NOT_INITIATED
 *
 * The client will receive an indication signal with cause
 * CELLO_TZII_PREWARNING_START when:
 * - the subscription is issued during the pre-warning interval
 * - time passes into the pre-warning interval
 * - the subscription enters the pre-warning interval due to:
 *   - the DST on/off date is updated
 *   - the time offset to UTC is updated
 *   - the node UTC time is updated
 *   - the client updates the pre-warning interal
 *
 * The client will receive an indication signal with cause
 * CELLO_TZII_PREWARNING_CANCEL when:
 * - the subscription leaves the pre-warning interval due to:
 *   - the DST on/off date is updated
 *   - the time offset to UTC is updated
 *   - the node UTC time is updated
 *
 * The client will receive an indication signal with cause
 * CELLO_TZII_ATTRIBUTE_OR_TIME_UPDATE when:
 * - the time offset to UTC is updated
 * - the DST on/off date is updated causing the DST on/off value to change
 *
 *****************************************************************************/
CelloTziiResult
CelloTzii_subscribeDaylightSavingTime(void *tziiMemory_p,
                                      uint32_t clientInfo,
                                      uint32_t preWarningTime);


/******************************************************************************
 *
 * Start subscription on leap seconds information.
 *
 * If client is already subscribing on DST information, the old subscription
 * is discarded and the client will receive a new confirm signal.
 *
 * @param tziiMemory_p   - TZII handle
 * @param clientInfo     - client data returned in indication signal
 * @param preWarningTime - advance notification in milliseconds (<= 23 hours)
 *
 * Return:  CELLO_TZII_OK
 *          CELLO_TZII_SERVER_UNAVAILABLE
 *          CELLO_TZII_MEMORY_NOT_INITIATED
 *
 * The client will receive an indication signal with cause
 * CELLO_TZII_PREWARNING_START when:
 * - the subscription is issued during the pre-warning interval
 * - time passes into the pre-warning interval
 * - the subscription enters the pre-warning interval due to:
 *   - the node UTC time is updated
 *   - the client updates the pre-warning interal
 *
 * The client will receive an indication signal with cause
 * CELLO_TZII_PREWARNING_CANCEL when:
 * - the subscription leaves the pre-warning interval due to:
 *   - the node UTC time is updated
 *
 * The client will receive an indication signal with cause
 * CELLO_TZII_ATTRIBUTE_OR_TIME_UPDATE when:
 * - the time offset to UTC is updated
 * - the leap seconds MO attribute is updated manually
 *
 *****************************************************************************/
CelloTziiResult
CelloTzii_subscribeLeapSeconds(void *tziiMemory_p,
                               uint32_t clientInfo,
                               uint32_t preWarningTime);

/**
 ******************************************************************************
 *
 * This function can be used to use the proxy on the behalf of another
 * thread or a mailbox.
 *
 * All signals from the server will be sent to the specified mailbox instead
 * of being sent to the thread that executes the proxy function using
 * the provided TZII handle.
 *
 * @param tziiMemory_p - pointer to TZII handle
 * @param mbox - the receiving mailbox
 *               (use 0 to revert to the default, i.e. the current thread)
 *
 *****************************************************************************/
void
CelloTzii_setMailbox(void *tziiMemory_p, PROCESS mbox);

#ifdef __cplusplus
}
#endif
#endif /* CELLO_TZII_H */
