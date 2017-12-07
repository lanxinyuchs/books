/* ----------------------------------------------------------------------
 * %CCaseFile:	test_trace_probe.c %
 * %CCaseRev:	/main/R2A/2 %
 * %CCaseDate:	2014-02-19 %
 * %CCaseDocNo: %
 * Author:	erarafo
 * Author: <name>, <e-mail address>
 *
 * Short description:
 * "Probe", aka "tracepoint provider" for test_trace.
 * ----------------------------------------------------------------------
 * %CCaseTemplateFile:	template.c %
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
 * R2A/1      2014-02-18 erarafo     Created
 * R2A/2      2014-02-19 erarafo     Added a "version" function
 * ----------------------------------------------------------------------
 */

const char *testTraceProbeVersion() {
  return "R2A/2";
}

#define TRACEPOINT_CREATE_PROBES
#include "test_trace.h"
