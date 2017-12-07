/* ----------------------------------------------------------------------
 * %CCaseFile:	cello_tzii.sig %
 * %CCaseRev:	/main/R4A/3 %
 * %CCaseDate:	2015-10-20 %
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
 * R4A/2      2015-09-11 erarafo     Mirroring a G1 update
 * R4A/3      2015-10-20 erarafo     Comment only
 * ----------------------------------------------------------------------
 */

/*
 * DESCRIPTION
 * -----------
 * This file contains the signal definitions of the TZII.
 *
 *
 * G1 REVISION HISTORY
 * ----------------
 *
 * Revised: 2013-07-22 Francisco Jares Junior
 * Change:  First version
 *
 */

#ifndef CELLO_TZII_SIG
#define CELLO_TZII_SIG

#ifdef __cplusplus
extern "C" {
#endif

/*
 ******************************************************************************
 * INCLUDE FILES
 ******************************************************************************
 */

#include "stdint.h"
#include "stdbool.h"

/*
 *******************************************************************************
 * MACROS
 *******************************************************************************
 */

/* 0x10CEA == 68842 */
#define CELLO_TZII_SIG_BASE (0x10CEA)

/*
 ******************************************************************************
 * TYPES
 ******************************************************************************
 */

/*
 ******************************************************************************
 * SIGNALS
 ******************************************************************************
 */

/******************************************************************************
 *
 * Signal : CelloTziiServerDownInd
 *
 * Descr  : The server down signal.
 *
 * Data   : -
 *
 *****************************************************************************/
#define CELLO_TZII_SERVER_DOWN_IND (CELLO_TZII_SIG_BASE + 0) /*!- SIGNO(CelloTziiServerDownInd) -!*/
typedef struct
{
   SIGSELECT    sigNo;
} CelloTziiServerDownInd;



/******************************************************************************
 *
 * Signal : CelloTziiServerUpInd
 *
 * Descr  : The server up signal.
 *
 * Data   : -
 *
 *****************************************************************************/
#define CELLO_TZII_SERVER_UP_IND (CELLO_TZII_SIG_BASE + 1) /*!- SIGNO(CelloTziiServerUpInd) -!*/
typedef struct
{
   SIGSELECT    sigNo;
} CelloTziiServerUpInd;

/******************************************************************************
 *
 * Signal : CelloTziiInitiateServiceCfm
 *
 * Descr  : Confirmation for the service initiation.
 *
 * Data   : selectedPV     - Selected protocol version
 *
 *****************************************************************************/
#define CELLO_TZII_INITIATE_SERVICE_CFM (CELLO_TZII_SIG_BASE + 2) /*!- SIGNO(CelloTziiInitiateServiceCfm) -!*/
typedef struct
{
   SIGSELECT    sigNo;
   uint32_t     signalRevision;
   uint32_t     selectedPV;
} CelloTziiInitiateServiceCfm;


/******************************************************************************
 *
 * Signal : CelloTziiInitiateServiceSus
 *
 * Descr  : Incompatible protocol version between the client and the server.
 *
 * Data   : -
 *
 *****************************************************************************/
#define CELLO_TZII_INITIATE_SERVICE_SUS (CELLO_TZII_SIG_BASE + 3) /*!- SIGNO(CelloTziiInitiateServiceSus) -!*/
typedef struct
{
   SIGSELECT    sigNo;
   uint32_t     signalRevision;
   uint32_t     highestPV;
} CelloTziiInitiateServiceSus;


/******************************************************************************
 *
 * Signal : CelloTziiInitiateServiceRej
 *
 * Descr  : Rejection for the service initiation.
 *
 * Data   : -
 *
 *****************************************************************************/
#define CELLO_TZII_INITIATE_SERVICE_REJ (CELLO_TZII_SIG_BASE + 4) /*!- SIGNO(CelloTziiInitiateServiceRej) -!*/
typedef struct
{
   SIGSELECT                sigNo;
   uint32_t                 signalRevision;
   CelloTziiServiceResult   result;
   uint32_t                 highestPV;
} CelloTziiInitiateServiceRej;

/******************************************************************************
 *
 * Signal : CelloTziiServerUnpublishInd
 *
 * Descr  : Server unpublish occurence.
 *
 * Data   : -
 *
 *****************************************************************************/
#define CELLO_TZII_SERVER_UNPUBLISH_IND (CELLO_TZII_SIG_BASE + 5) /*!- SIGNO(CelloTziiServerUnpublishInd) -!*/
typedef struct
{
   SIGSELECT    sigNo;
} CelloTziiServerUnpublishInd;


/******************************************************************************
 *
 * Signal : CelloTziiTerminateServiceCfm
 *
 * Descr  : Confirmation for the termination of the service.
 *
 * Data   : -
 *
 *****************************************************************************/
#define CELLO_TZII_TERMINATE_SERVICE_CFM (CELLO_TZII_SIG_BASE + 6) /*!- SIGNO(CelloTziiTerminateServiceCfm) -!*/
typedef struct
{
   SIGSELECT    sigNo;
} CelloTziiTerminateServiceCfm;


/******************************************************************************
 *
 * Signal : CelloTziiSubscribeDaylightSavingTimeCfm
 *
 * Descr  : The confirmation for the subscription signal for daylight saving
 *          info.
 *
 * Data   : clientInfo - client data as given in subscribe request
 *          timeOffsetToUtc - in minutes
 *          daylightSavingTimeOffset - in minutes
 *          daylightSavingOn - current DST status
 *                             1 if DST is on, 0 if DST is off

 *****************************************************************************/
#define CELLO_TZII_SUBSCRIBE_DAYLIGHT_SAVING_TIME_CFM (CELLO_TZII_SIG_BASE + 9) /*!- SIGNO(CelloTziiSubscribeDaylightSavingTimeCfm) -!*/
typedef struct
{
   SIGSELECT    sigNo;
   uint32_t     clientInfo;
   int32_t      timeOffsetToUtc;
   int32_t      daylightSavingTimeOffset;
   uint32_t     daylightSavingTimeOn;
} CelloTziiSubscribeDaylightSavingTimeCfm;


/******************************************************************************
 *
 * Signal : CelloTziiSubscribeDaylightSavingTimeRej
 *
 * Descr  : Rejection cause.
 *
 * Data   : clientInfo - client data as given in subscribe request
 *          rejectCause -
 *
 *****************************************************************************/
#define CELLO_TZII_SUBSCRIBE_DAYLIGHT_SAVING_TIME_REJ (CELLO_TZII_SIG_BASE + 10) /*!- SIGNO(CelloTziiSubscribeDaylightSavingTimeRej) -!*/
typedef struct
{
   SIGSELECT    sigNo;
   uint32_t     clientInfo;
   CelloTziiServiceResult rejectCause;
} CelloTziiSubscribeDaylightSavingTimeRej;


/******************************************************************************
 *
 * Signal : CelloTziiSubscribeDaylightSavingTimeInd
 *
 * Descr  : Notification containing info about DST.
 *
 * Data   : clientInfo - client data as given in subscribe request
 *          timeOffsetToUtc - in minutes
 *          daylightSavingTimeOffset - in minutes
 *          daylightSavingOn - current DST status in case of cancel or update
 *                             future DST status in case of prewarning start
 *                             1 if DST is on, 0 if DST is off
 *          timeOfChange - minutes after midnight (in Local Standard Time)
 *          causeForChange - start, cancel or update
 *
 *****************************************************************************/
#define CELLO_TZII_SUBSCRIBE_DAYLIGHT_SAVING_TIME_IND (CELLO_TZII_SIG_BASE + 7) /*!- SIGNO(CelloTziiSubscribeDaylightSavingTimeInd) -!*/
typedef struct
{
   SIGSELECT         sigNo;
   uint32_t          clientInfo;
   int32_t           timeOffsetToUtc;
   int32_t           daylightSavingTimeOffset;
   uint32_t          daylightSavingTimeOn;
   uint32_t          timeOfChange;
   CelloTziiIndCause causeForChange;
} CelloTziiSubscribeDaylightSavingTimeInd;


/******************************************************************************
 *
 * Signal : CelloTziiSubscribeLeapCfm
 *
 * Descr  : The confirmation of the leap seconds subscription.
 *
 * Data   : clientInfo - client data as given in subscribe request
 *          leapSeconds - in seconds
 *
 *****************************************************************************/
#define CELLO_TZII_SUBSCRIBE_LEAP_SECONDS_CFM (CELLO_TZII_SIG_BASE + 11) /*!- SIGNO(CelloTziiSubscribeLeapCfm) -!*/
typedef struct
{
   SIGSELECT    sigNo;
   uint32_t     clientInfo;
   int32_t      leapSeconds;
} CelloTziiSubscribeLeapSecondsCfm;


/******************************************************************************
 *
 * Signal : CelloTziiSubscribeLeapSecondsRej
 *
 * Descr  : Rejection cause.
 *
 * Data   : clientInfo - client data as given in subscribe request
 *          rejectCause -
 *
 *****************************************************************************/
#define CELLO_TZII_SUBSCRIBE_LEAP_SECONDS_REJ (CELLO_TZII_SIG_BASE + 12) /*!- SIGNO(CelloTziiSubscribeLeapRej) -!*/
typedef struct
{
   SIGSELECT    sigNo;
   uint32_t     clientInfo;
   CelloTziiServiceResult rejectCause;
} CelloTziiSubscribeLeapSecondsRej;


/******************************************************************************
 *
 * Signal : CelloTziiSubscribeLeapInd
 *
 * Descr  : Notification containing info about the leap seconds.
 *
 * Data   : clientInfo - client data as given in subscribe request
 *          leapSeconds - in seconds. When entering a pre-warning interval,
 *                        the new, but not yet effective, value is provided.
 *          causeForChange - start, cancel or update
 *
 *****************************************************************************/
#define CELLO_TZII_SUBSCRIBE_LEAP_SECONDS_IND (CELLO_TZII_SIG_BASE + 8) /*!- SIGNO(CelloTziiSubscribeLeapInd) -!*/
typedef struct
{
   SIGSELECT    sigNo;
   uint32_t     clientInfo;
   int32_t      leapSeconds;
   CelloTziiIndCause causeForChange;
} CelloTziiSubscribeLeapSecondsInd;


#ifdef __cplusplus
}
#endif
#endif /* CELLO_TZII_SIG */
