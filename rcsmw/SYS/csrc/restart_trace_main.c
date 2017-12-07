/* ----------------------------------------------------------------------
 * %CCaseFile:	restart_trace_main.c %
 * %CCaseRev:	/main/R3A/2 %
 * %CCaseDate:	2014-11-11 %
 * %CCaseDocNo: %
 * Author:	etxarnu
 * Author: <name>, <e-mail address>
 *
 * Short description:
 * To do a logging in restart_log
 * ----------------------------------------------------------------------
 * %CCaseTemplateFile:  template.c %
 * %CCaseTemplateId: CCver: /main/1 %
 *
 * %CCaseCopyrightBegin%
 * Copyright (c) Ericsson AB 2014 All rights reserved.
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
 * R3A/1      2014-11-11   etxarnu     Created
 * ----------------------------------------------------------------------
 */

#define TRACEPOINT_DEFINE
#include "com_ericsson_system_start.h"

int main(int argc, char **argv) {
 	event_system_start(argv[1]);
        return 0;
} 


