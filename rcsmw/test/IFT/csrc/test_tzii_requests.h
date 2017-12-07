/* ----------------------------------------------------------------------
 * %CCaseFile:	test_tzii_requests.h %
 * %CCaseRev:	/main/R4A/R7A/1 %
 * %CCaseDate:	2016-09-19 %
 * %CCaseDocNo: %
 * Author:	erarafo
 * Author: <name>, <e-mail address>
 *
 * Short description: Integer codes representing TZII client requests.
 * Used by: TIM_CNX.../test/suites/tzii_c_SUITE, IFT_CAX.../csrc/test_tzii.c
 *
 * NOTE: When updating this file, also check out
 * TIM_CNX.../test/suites/....hrl and run `clearmake -u test-headers' in the
 * TIM_CAX... directory.
 *
 * ----------------------------------------------------------------------
 * %CCaseTemplateFile:	template.h %
 * %CCaseTemplateId: CCver: /main/1 %
 *
 * %CCaseCopyrightBegin%
 * Copyright (c) Ericsson AB 2015-2016 All rights reserved.
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
 * R4A/1      2015-09-06 erarafo     First version
 * R4A/2      2015-09-28 erarafo     Added 'getPid'
 * R4A/3      2015-10-13 erarafo     Negative tests support
 * R4A/4      2015-10-15 erarafo     Version request added
 * R4A/5      2015-11-06 erarafo     setMailbox support
 * R4A/6      2015-11-16 erarafo     internal call using proxy of peer
 * R7A/1      2016-09-19 erarafo     Support for fetching board time
 * ----------------------------------------------------------------------
 */

#define iftRequest_initiateMemory               1
#define iftRequest_freeMemory                   2
#define iftRequest_initiateService              3
#define iftRequest_terminateService             4
#define iftRequest_internal                     5
#define iftRequest_subscribeDaylightSavingTime  6
#define iftRequest_subscribeLeapSeconds         7
#define iftRequest_startExample                 8
#define iftRequest_stopExample                  9
#define iftRequest_getPid                      10
#define iftRequest_setMailbox                  11
#define iftRequest_internalForPeer             12

#define iftRequest_internalBad                 91
#define iftRequest_version                     92

#define iftRequest_boardTime                   93
