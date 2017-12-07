#ifndef CCI_H
#define CCI_H

/* ----------------------------------------------------------------------
 * %CCaseFile:	cci.h %
 * %CCaseRev:	/main/R5A/R10A/5 %
 * %CCaseDate:	2017-07-10 %
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
 * R10A/1     2017-06-26 estjako     Added enums
 * R10A/3     2017-06-27 estjako     Added enums
 * R10A/4     2017-06-28 edartop     Added CciRejectReason
 * R10A/5     2017-07-10 evadumb     Added CCI_REASON_SLEW, CCI_REASON_DISCREPANCY
 * ----------------------------------------------------------------------
 */

/********** IMPORT ************************/

#ifdef __cplusplus
extern "C" {
#endif

/********** EXPORTED TYPES ****************/

#define CCI_MBOX_NAME "CCI_mbox"

typedef enum {
	CCI_REASON_INITIAL = 0,
	CCI_REASON_STEP = 1,
	CCI_REASON_SLEW = 2,
	CCI_REASON_SLEW_DISCREPANCY = 3
} CciTimeUpdateReason;

typedef enum {
	CCI_REJ_SERVICE_UNAVAIL = 0,
	CCI_REJ_ALREADY_SUBSCRIBED = 1,
	CCI_REJ_ALREADY_UNSUBSCRIBED = 2,
	CCI_REJ_MAX_SUBSCRIBERS = 3,
	CCI_REJ_UNSUPPORTED_PV = 4
} CciRejectReason;

typedef enum {
	CCI_PV1 = 1,
	CCI_PV2 = 2
} CciProtocolVersion;

typedef enum {
	NTP_OUT_OF_SYNC = 0,
	NTP_IN_SYNC = 1
} CciNtpState;

#ifdef __cplusplus
}
#endif

#endif   /* CCI_H */
