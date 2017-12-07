/* ----------------------------------------------------------------------
 * %CCaseFile:	cstnn_trace.h %
 * %CCaseRev:	/main/R10A/1 %
 * %CCaseDate:	2017-05-16 %
 * %CCaseDocNo: %
 * Author:      etxpeno
 *
 * Short description: "Tracepoint provider" for the oot cstn library
 *
 * ----------------------------------------------------------------------
 * %CCaseTemplateFile:  template.h %
 * %CCaseTemplateId: CCver: /main/1 %
 *
 * %CCaseCopyrightBegin%
 * Copyright (c) Ericsson AB 2017 All rights reserved.
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
 * R10A/1     2017-05-12 etxpeno     Created
 * ----------------------------------------------------------------------
 */

#undef TRACEPOINT_PROVIDER
#define TRACEPOINT_PROVIDER com_ericsson_rcs_cstn

#if !defined(_TRACEPOINT_CSTNN_TRACE_H) || defined(TRACEPOINT_HEADER_MULTI_READ)
#define _TRACEPOINT_CSTNN_TRACE_H

#include <stdint.h>
#include <lttng/tracepoint.h>

TRACEPOINT_EVENT(com_ericsson_rcs_cstn, info_trace,
		 TP_ARGS(char *, trace),
		 TP_FIELDS(ctf_string(trace, trace))
		 )

TRACEPOINT_LOGLEVEL(com_ericsson_rcs_cstn, info_trace, TRACE_INFO)

TRACEPOINT_EVENT(com_ericsson_rcs_cstn, error_trace,
                 TP_ARGS(char *, trace),
                 TP_FIELDS(ctf_string(trace, trace))
		 )

TRACEPOINT_LOGLEVEL(com_ericsson_rcs_cstn, error_trace, TRACE_ERR)

TRACEPOINT_EVENT(com_ericsson_rcs_cstn, debug_trace,
                 TP_ARGS(char *, trace),
                 TP_FIELDS(ctf_string(trace, trace))
)
TRACEPOINT_LOGLEVEL(com_ericsson_rcs_cstn, debug_trace, TRACE_DEBUG)

#endif /* _TRACEPOINT_CSTNN_TRACE_H */

#undef TRACEPOINT_INCLUDE_FILE
#define TRACEPOINT_INCLUDE_FILE ./cstnn_trace.h

#include <lttng/tracepoint-event.h>
