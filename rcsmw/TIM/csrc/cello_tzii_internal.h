/* ----------------------------------------------------------------------
 * %CCaseFile:	cello_tzii_internal.h %
 * %CCaseRev:	/main/R4A/3 %
 * %CCaseDate:	2015-11-05 %
 * %CCaseDocNo: %
 * Author:	erarafo
 * Author: <name>, <e-mail address>
 *
 * Short description: Internal definitions, also used by timServer.erl
 * ----------------------------------------------------------------------
 * %CCaseTemplateFile:	template.h %
 * %CCaseTemplateId: CCver: /main/1 %
 *
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
 * Revision history:
 *
 * Rev        Date       Name        What
 * -----      -------    --------    --------------------------
 * R4A/1      2015-09-08 erarafo     First version
 * R4A/2      2015-10-29 erarafo     CEC result codes added
 * R4A/3      2015-11-05 erarafo     setMailbox support
 * ----------------------------------------------------------------------
 */

/* Server state */
#define SERVER_UNAVAILABLE (0)
#define SERVER_INITIATING  (1)
#define SERVER_AVAILABLE   (2)
#define SERVER_TERMINATING (3)
#define SERVER_SUSPENDED   (4)

static char *stateNames[] = {
    "unavailable",
    "initiating",
    "available",
    "terminating",
    "suspended"};


/* CEC signature and other strings */
#define CEC_SIGNATURE "TZII"
#define CEC_RESPONSE_OK "ok"
#define CEC_RESPONSE_UNKNOWN "unknown"


/* CEC operations result codes */
#define CEC_OK                               (0)
#define CEC_UNKNOWN                          (1)
#define CEC_UNEXPECTED                       (2)
#define CEC_ERROR_OPEN                       (3)
#define CEC_ERROR_SEND                       (4)
#define CEC_ERROR_RECEIVE                    (5)
#define CEC_ERROR_CLOSE_A                    (6)
#define CEC_ERROR_CLOSE_B                    (7)
#define CEC_ERROR_CLOSE_C                    (8)


/* CEC requests */
#define CEC_REQUEST_INIT_SERVICE           (100)
#define CEC_REQUEST_TERM_SERVICE           (101)
#define CEC_REQUEST_SUBSCRIBE_DST          (102)
#define CEC_REQUEST_SUBSCRIBE_LEAP_SEC     (103)
#define CEC_REQUEST_CHANGE_FEEDBACK        (104)
#define CEC_REQUEST_PROCEED                (105)
#define CEC_REQUEST_SET_MAILBOX            (106)
