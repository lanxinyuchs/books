/* ----------------------------------------------------------------------
 * %CCaseFile:	sys_trace.h %
 * %CCaseRev:	/main/R4A/1 %
 * %CCaseDate:	2015-09-18 %
 * %CCaseDocNo: %
 * Author:      etxpeno
 *
 * Short description: "Tracepoint provider" for SYS
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
 * R3A/1      2015-03-20 etxpeno     Created
 * ----------------------------------------------------------------------
 */

#undef TRACEPOINT_PROVIDER
#define TRACEPOINT_PROVIDER com_ericsson_rcs_sys

#if !defined(_TRACEPOINT_SYS_TRACE_H) || defined(TRACEPOINT_HEADER_MULTI_READ)
#define _TRACEPOINT_SYS_TRACE_H

#include <stdint.h>
#include <lttng/tracepoint.h>
TRACEPOINT_EVENT (com_ericsson_rcs_sys, info_trace,
		  TP_ARGS (char *, trace),
		  TP_FIELDS (ctf_string (trace, trace)))
TRACEPOINT_LOGLEVEL (com_ericsson_rcs_sys, info_trace, TRACE_INFO)
TRACEPOINT_EVENT (com_ericsson_rcs_sys, error_trace,
		  TP_ARGS (char *, trace),
		  TP_FIELDS (ctf_string (trace, trace)))
TRACEPOINT_LOGLEVEL (com_ericsson_rcs_sys, error_trace, TRACE_ERR)
#endif				/* _TRACEPOINT_SYS_TRACE_H */
#undef TRACEPOINT_INCLUDE_FILE
#define TRACEPOINT_INCLUDE_FILE ./sys_trace.h
#include <lttng/tracepoint-event.h>
