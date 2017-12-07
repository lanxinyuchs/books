/* ----------------------------------------------------------------------
 * %CCaseFile:	alh_trace.h %
 * %CCaseRev:	/main/R4A/R5A/1 %
 * %CCaseDate:	2015-12-01 %
 * %CCaseDocNo: %
 * Author:      etxberb
 *
 * Short description: "Tracepoint provider" for the ALH library
 *
 * ----------------------------------------------------------------------
 * %CCaseTemplateFile:  template.h %
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
 * R4A/1      2015-09-29 etxberb     Created
 * R5A/1      2015-12-01 erarafo     Added support for 'ENTER' trace
 * ----------------------------------------------------------------------
 */

#undef TRACEPOINT_PROVIDER
#define TRACEPOINT_PROVIDER com_ericsson_rcs_alh

#if !defined(_TRACEPOINT_ALH_TRACE_H) || defined(TRACEPOINT_HEADER_MULTI_READ)
#define _TRACEPOINT_ALH_TRACE_H

#include <stdint.h>
#include <lttng/tracepoint.h>

TRACEPOINT_EVENT (com_ericsson_rcs_alh, info_trace,
		  TP_ARGS (char *, trace),
		  TP_FIELDS (ctf_string (trace, trace)))
TRACEPOINT_LOGLEVEL (com_ericsson_rcs_alh, info_trace, TRACE_INFO)

TRACEPOINT_EVENT (com_ericsson_rcs_alh, error_trace,
		  TP_ARGS (char *, trace),
		  TP_FIELDS (ctf_string (trace, trace)))
TRACEPOINT_LOGLEVEL (com_ericsson_rcs_alh, error_trace, TRACE_ERR)

TRACEPOINT_EVENT (com_ericsson_rcs_alh, enter_trace,
                  TP_ARGS (char *, trace),
                  TP_FIELDS (ctf_string (trace, trace)))
TRACEPOINT_LOGLEVEL (com_ericsson_rcs_alh, enter_trace, TRACE_DEBUG_FUNCTION)

#endif				/* _TRACEPOINT_ALH_TRACE_H */
#undef TRACEPOINT_INCLUDE_FILE
#define TRACEPOINT_INCLUDE_FILE ./alh_trace.h
#include <lttng/tracepoint-event.h>
