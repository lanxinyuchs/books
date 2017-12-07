/* ----------------------------------------------------------------------
 * %CCaseFile:	test_trace.h %
 * %CCaseRev:	/main/R2A/3 %
 * %CCaseDate:	2014-02-19 %
 * %CCaseDocNo: %
 * Author:	erarafo
 * Author: <name>, <e-mail address>
 *
 * Short description: "Tracepoint provider" for the test_trace program
 * <Some rows here>
 * ----------------------------------------------------------------------
 * %CCaseTemplateFile:	template.h %
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
 * R2A/2      2014-02-18 erarafo     Added another tracepoint
 * R2A/3      2014-02-19 erarafo     Trace at level INFO
 * ----------------------------------------------------------------------
 */

#undef TRACEPOINT_PROVIDER
#define TRACEPOINT_PROVIDER com_ericsson_rcs_test


#if !defined(_TRACEPOINT_TEST_TRACE_H) || defined(TRACEPOINT_HEADER_MULTI_READ)
#define _TRACEPOINT_TEST_TRACE_H

#include <lttng/tracepoint.h>

TRACEPOINT_EVENT(com_ericsson_rcs_test, starting, TP_ARGS(int, value), TP_FIELDS(ctf_integer(int, value, value)))
TRACEPOINT_LOGLEVEL(com_ericsson_rcs_test, starting, TRACE_INFO)

TRACEPOINT_EVENT(com_ericsson_rcs_test, idle, TP_ARGS(int, value), TP_FIELDS(ctf_integer(int, value, value)))
TRACEPOINT_LOGLEVEL(com_ericsson_rcs_test, idle, TRACE_INFO)

#endif /* _TRACEPOINT_TEST_TRACE_H */

#undef TRACEPOINT_INCLUDE_FILE
#define TRACEPOINT_INCLUDE_FILE ./test_trace.h

#include <lttng/tracepoint-event.h>
