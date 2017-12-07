/* ----------------------------------------------------------------------
 * %CCaseFile:	pes_trace.h %
 * %CCaseRev:	/main/R3A/2 %
 * %CCaseDate:	2015-05-13 %
 * %CCaseDocNo: %
 * Author:	etxarnu
 *
 * Short description: "Tracepoint provider" for the PES PEI program
 * 
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
 * R2A/1      2014-10-09 etxarnu     Created
 * ----------------------------------------------------------------------
 */

#undef TRACEPOINT_PROVIDER
#define TRACEPOINT_PROVIDER com_ericsson_pes

#if !defined(_TRACEPOINT_PES_TRACE_H) || defined(TRACEPOINT_HEADER_MULTI_READ)
#define _TRACEPOINT_PES_TRACE_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>
#include <lttng/tracepoint.h>

TRACEPOINT_EVENT(com_ericsson_pes, info_trace,
                 TP_ARGS(char *, trace),
                 TP_FIELDS(ctf_string(trace, trace))
)
TRACEPOINT_LOGLEVEL(com_ericsson_pes, info_trace, TRACE_INFO)

TRACEPOINT_EVENT(com_ericsson_pes, error_trace,
                 TP_ARGS(char *, trace),
                 TP_FIELDS(ctf_string(trace, trace))
)
TRACEPOINT_LOGLEVEL(com_ericsson_pes, error_trace, TRACE_ERR)

TRACEPOINT_EVENT(com_ericsson_pes, debug_trace,
                 TP_ARGS(char *, trace),
                 TP_FIELDS(ctf_string(trace, trace))
)
TRACEPOINT_LOGLEVEL(com_ericsson_pes, debug_trace, TRACE_DEBUG)


#endif /* _TRACEPOINT_PES_TRACE_H */

#undef TRACEPOINT_INCLUDE_FILE
#define TRACEPOINT_INCLUDE_FILE ./pes_trace.h

#include <lttng/tracepoint-event.h>
