/* ----------------------------------------------------------------------
 * %CCaseFile:	cec_trace.h %
 * %CCaseRev:	/main/R3A/R8A/1 %
 * %CCaseDate:	2016-11-10 %
 * %CCaseDocNo: %
 * Author:      etxpeno
 *
 * Short description: "Tracepoint provider" for the CEC library
 *
 * ----------------------------------------------------------------------
 * %CCaseTemplateFile:  template.h %
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
 * R3A/1      2015-03-20 etxpeno     Created
 * R8A/1      2015-11-10 etxpeno     improve tracing
 * ----------------------------------------------------------------------
 */

#undef TRACEPOINT_PROVIDER
#define TRACEPOINT_PROVIDER com_ericsson_rcs_cec

#if !defined(_TRACEPOINT_CEC_TRACE_H) || defined(TRACEPOINT_HEADER_MULTI_READ)
#define _TRACEPOINT_CEC_TRACE_H

#include <stdint.h>
#include <lttng/tracepoint.h>

TRACEPOINT_EVENT(com_ericsson_rcs_cec, error_trace,
                 TP_ARGS(char *, file, int, line, const char *, func,
			 char *, trace),
                 TP_FIELDS(ctf_string(file, file)
			   ctf_integer(int, line, line)
			   ctf_string(func, func)
			   ctf_string(trace, trace))
		 )

TRACEPOINT_LOGLEVEL(com_ericsson_rcs_cec, error_trace, TRACE_ERR)

TRACEPOINT_EVENT(com_ericsson_rcs_cec, error_trace_w_int_arg,
                 TP_ARGS(char *, file, int, line, const char *, func,
			 char *, trace, int, arg1),
                 TP_FIELDS(ctf_string(file, file)
			   ctf_integer(int, line, line)
			   ctf_string(func, func)
			   ctf_string(trace, trace)
                           ctf_integer(int, arg1, arg1))
		 )
TRACEPOINT_LOGLEVEL(com_ericsson_rcs_cec, error_trace_w_int_arg, TRACE_ERR)

TRACEPOINT_EVENT(com_ericsson_rcs_cec, error_trace_w_str_arg,
                 TP_ARGS(char *, file, int, line, const char *, func,
			 char *, trace, char *, arg1),
                 TP_FIELDS(ctf_string(file, file)
			   ctf_integer(int, line, line)
			   ctf_string(func, func)
			   ctf_string(trace, trace)
                           ctf_string(arg1, arg1))
		 )
TRACEPOINT_LOGLEVEL(com_ericsson_rcs_cec, error_trace_w_str_arg, TRACE_ERR)

TRACEPOINT_EVENT (com_ericsson_rcs_cec, warning_trace,
		  TP_ARGS (char *, file, int, line, const char *, func,
			   char *, trace),
		  TP_FIELDS (ctf_string(file, file)
			     ctf_integer(int, line, line)
			     ctf_string(func, func)
			     ctf_string(trace, trace)))
TRACEPOINT_LOGLEVEL (com_ericsson_rcs_cec, warning_trace, TRACE_WARNING)

TRACEPOINT_EVENT(com_ericsson_rcs_cec, warning_trace_w_str_arg,
                 TP_ARGS(char *, file, int, line, const char *, func,
			 char *, trace, char *, arg1),
                 TP_FIELDS(ctf_string(file, file)
			   ctf_integer(int, line, line)
			   ctf_string(func, func)
			   ctf_string(trace, trace)
                           ctf_string(arg1, arg1))
		 )
TRACEPOINT_LOGLEVEL(com_ericsson_rcs_cec, warning_trace_w_str_arg,
		    TRACE_WARNING)

TRACEPOINT_EVENT(com_ericsson_rcs_cec, enter,
		 TP_ARGS(const char *, func),
		 TP_FIELDS(ctf_string(func, func))
		 )
TRACEPOINT_LOGLEVEL(com_ericsson_rcs_cec, enter, TRACE_DEBUG)

#endif				/* _TRACEPOINT_CEC_TRACE_H */
#undef TRACEPOINT_INCLUDE_FILE
#define TRACEPOINT_INCLUDE_FILE ./cec_trace.h
#include <lttng/tracepoint-event.h>
