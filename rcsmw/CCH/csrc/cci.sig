/*****************************************************************************
 *
 *
 * Copyright (c) Ericsson AB  2015-2016 All rights reserved.
 *
 * The information in this document is the property of Ericsson. Except
 * as specifically authorized in writing by Ericsson,the receiver of this
 * document shall keep the information contained herein confidential
 * and shall protect the same in whole or in part from disclosure and
 * dissemination to third parties. Disclosure and disseminations to the
 * receiver's employees shall only be made on a strict need to know basis.
 *
 ****************************************************************************/
#ifndef CCI_SIG
#define CCI_SIG

/* ----------------------------------------------------------------------
 * %CCaseFile:	cci.sig %
 * %CCaseRev:	/main/R5A/R10A/5 %
 * %CCaseDate:	2017-06-29 %
 * %CCaseDocNo: %
 * Author:	ekurnik
 * Author: <name>, <e-mail address>
 *
 * Short description:
 * <Some rows here>
 * ----------------------------------------------------------------------
 * %CCaseTemplateFile:	template.c %
 * %CCaseTemplateId: CCver: /main/1 %
 *
 * %CCaseCopyrightBegin%
 * Copyright (c) Ericsson AB 2016-2017 All rights reserved.
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
 * R5A/1      2016-02-17 ekurnik     Created
 * R10A/1     2017-06-26 estjako     Added CCI2_SUBSCRIBE_REQ
 * R10A/3     2017-06-27 estjako     Added CCI2_NTP_STATE_IND
 * R10A/4     2017-06-28 estjako     Fixed comments
 * R10A/5     2017-06-29 ekurnik     Added include for pid_t type
 * ----------------------------------------------------------------------
 */

#include <stdint.h>
#include <sys/types.h>

#ifdef __cplusplus
extern "C" {
#endif

#include "cci.h"

#define CCI_SIG_BASE 0x018A4FD0

/******************************************************************************
 * Signal range: 0x018A4FD0 - 0x018A4FEF
 ******************************************************************************/

/******************************************************************************
 * REQUESTS
 ******************************************************************************/

/******************************************************************************
 *
 * Message Name: CCI_SUBSCRIBE_REQ
 *
 * Descr      : Subscribe request to service
 *
 * Data		  : PID of the subscribing process, used for logging
 *
 *****************************************************************************/
#define CCI_SUBSCRIBE_REQ (CCI_SIG_BASE + 0) /* !-SIGNO(CciSubscribeReq)-! */
typedef struct
{
   uint32_t  msg_no;
   pid_t pid;
} CciSubscribeReq;

/******************************************************************************
 *
 * Message Name: CCI_UNSUBSCRIBE_REQ
 *
 * Descr      : Unsubscribe request to service
 *
 *****************************************************************************/
#define CCI_UNSUBSCRIBE_REQ (CCI_SIG_BASE + 1) /* !-SIGNO(CciUnsubscribeReq)-! */
typedef struct
{
   uint32_t  msg_no;
} CciUnsubscribeReq;

/******************************************************************************
 * RESPONSES
 ******************************************************************************/

/******************************************************************************
 *
 * Message Name: CCI_TIME_UPDATE_IND
 *
 * Descr      : Message from service which indicates the clock needs to be updated
 *
 * Data		  : Update reason, can be initial (after subscribe) or due to timestep
 * 				Timediff represents time change in us - will be 0 when reason is initial
 *
 *****************************************************************************/
#define CCI_TIME_UPDATE_IND (CCI_SIG_BASE + 2) /* !-SIGNO(CciTimeUpdateInd)-! */
typedef struct
{
   uint32_t  msg_no;
   CciTimeUpdateReason reason;
   uint32_t timediff;
} CciTimeUpdateInd;

/******************************************************************************
 *
 * Message Name: CCI_SUBSCRIBE_CFM
 *
 * Descr      : Message from service which indicates successful subscription
 *
 *****************************************************************************/
#define CCI_SUBSCRIBE_CFM (CCI_SIG_BASE + 3) /* !-SIGNO(CciSubscribeCfm)-! */
typedef struct
{
   uint32_t  msg_no;
} CciSubscribeCfm;

/******************************************************************************
 *
 * Message Name: CCI_SUBSCRIBE_REJ
 *
 * Descr      : Message from service which indicates unsuccessful subscription
 *
 * Data		  : Reason for unsuccessful subscription
 *
 *****************************************************************************/
#define CCI_SUBSCRIBE_REJ (CCI_SIG_BASE + 4) /* !-SIGNO(CciSubscribeRej)-! */
typedef struct
{
   uint32_t  msg_no;
   CciRejectReason rejectReason;
} CciSubscribeRej;

/******************************************************************************
 *
 * Message Name: CCI_UNSUBSCRIBE_CFM
 *
 * Descr      : Message from service which indicates successful cancellation
 * 				of subscription
 *
 *****************************************************************************/
#define CCI_UNSUBSCRIBE_CFM (CCI_SIG_BASE + 5) /* !-SIGNO(CciUnsubscribeCfm)-! */
typedef struct
{
   uint32_t  msg_no;
} CciUnsubscribeCfm;

/******************************************************************************
 *
 * Message Name: CCI_UNSUBSCRIBE_REJ
 *
 * Descr      : Message from service which indicates unsuccessful cancellation
 * 				of subscription
 *
 * Data		  : Reason for unsuccessful cancellation of subscription
 *
 *****************************************************************************/
#define CCI_UNSUBSCRIBE_REJ (CCI_SIG_BASE + 6) /* !-SIGNO(CciUnsubscribeRej)-! */
typedef struct
{
   uint32_t  msg_no;
   CciRejectReason rejectReason;
} CciUnsubscribeRej;

/******************************************************************************
 *
 * Message Name: CCI_SERVER_DOWN_IND
 *
 * Descr      : Message from service which indicates service is down.
 * 				The client should re-subscribe to the service.
 *
 *****************************************************************************/
#define CCI_SERVER_DOWN_IND (CCI_SIG_BASE + 7) /* !-SIGNO(CciServerDownInd)-! */
typedef struct
{
   uint32_t  msg_no;
} CciServerDownInd;

/******************************************************************************
 *
 * Message Name: CCI2_SUBSCRIBE_REQ
 *
 * Descr      : Subscribe request to service with protocol version
 *
 * Data		  : PID of the subscribing process, used for logging
 * 			  : PV is protocol version (PV1 and PV2 currently supported)
 *
 *****************************************************************************/
#define CCI2_SUBSCRIBE_REQ (CCI_SIG_BASE + 8) /* !-SIGNO(Cci2SubscribeReq)-! */
typedef struct
{
   uint32_t  msg_no;
   pid_t pid;
   CciProtocolVersion PV;
} Cci2SubscribeReq;

/******************************************************************************
 *
 * Message Name: CCI2_NTP_STATE_IND
 *
 * Descr      : This signal is sent for PV 2 subscribers only. It is sent
 *              on subscribe and on every state or offset change. Offset indicates
 *              difference between ntpd and current state (in ms).
 *
 *****************************************************************************/
#define CCI2_NTP_STATE_IND (CCI_SIG_BASE + 9) /* !-SIGNO(Cci2NtpStateInd)-! */
typedef struct
{
	uint32_t  msg_no;
	CciNtpState cci_ntp_state;
	int32_t offset;
} Cci2NtpStateInd;


#ifdef __cplusplus
}
#endif

#endif
