/**
 *   Trace utility trace points
 *
 *   @file com_ericsson_plf_trace_util.h
 *
 *   Copyright (C) 2015 by Ericsson AB. All rights reserved. The
 *   information in this document is the property of Ericsson. Except
 *   as specifically authorized in writing by Ericsson, the receiver
 *   of this document shall keep the information contained herein
 *   confidential and shall protect the same in whole or in part from
 *   disclosure and dissemination to third parties. Disclosure and
 *   disseminations to the receiver's employees shall only be made on
 *   a strict need to know basis.
 */
/* ========================================================================
 *   History of development:
 *   -----------------------
 *   Revised : 2015-01-27 Nils Carlson <nils.carlson@ericsson.com>
 *   Change  : First revision. ts_ and te_restart and tracelogger tracepoints
 *
 *   Revised : 2015-03-05 Niranjan
 *   Change  : Added provider tsDataStream for data streaming.
 *   -----------------------
 */

#undef TRACEPOINT_PROVIDER
#define TRACEPOINT_PROVIDER com_ericsson_plf_trace_util

#undef TRACEPOINT_INCLUDE
#define TRACEPOINT_INCLUDE "./com_ericsson_plf_trace_util.h"

#if !defined(TRACE_H) || defined(TRACEPOINT_HEADER_MULTI_READ)
#define TRACE_H

#include <lttng/tracepoint.h>

TRACEPOINT_EVENT(
	com_ericsson_plf_trace_util,
	traceLogger,
	TP_ARGS(
		char *, logger_string_arg
		),
	TP_FIELDS(
		ctf_string(logger_entry, logger_string_arg)
		)
	)
TRACEPOINT_LOGLEVEL(com_ericsson_plf_trace_util, traceLogger, TRACE_INFO)

TRACEPOINT_EVENT(
	com_ericsson_plf_trace_util,
	teRestart,
	TP_ARGS(
		int, te_restart_count_arg
		),
	TP_FIELDS(
		ctf_integer(int, te_restart_count, te_restart_count_arg)
		)
	)

TRACEPOINT_LOGLEVEL(com_ericsson_plf_trace_util, teRestart, TRACE_INFO)

TRACEPOINT_EVENT(
	com_ericsson_plf_trace_util,
	tsRestart,
	TP_ARGS(
		int, ts_restart_count_arg
		),
	TP_FIELDS(
		ctf_integer(int, ts_restart_count, ts_restart_count_arg)
		)
	)
TRACEPOINT_LOGLEVEL(com_ericsson_plf_trace_util, tsRestart, TRACE_INFO)

TRACEPOINT_EVENT(
	com_ericsson_plf_trace_util,
	testLog,
	TP_ARGS(
		char *, logger_string_arg
		),
	TP_FIELDS(
		ctf_string(logger_entry, logger_string_arg)
		)
	)
TRACEPOINT_LOGLEVEL(com_ericsson_plf_trace_util, testLog, TRACE_DEBUG)


TRACEPOINT_EVENT(com_ericsson_plf_trace_util, tsDataStream,
                 TP_ARGS(const char *,  msg,
                         char*, data, size_t, datalen),
                 TP_FIELDS(
                           ctf_string(msg, msg)
                           ctf_sequence(char, dataBusSend, data, size_t, datalen)
        )
)
TRACEPOINT_LOGLEVEL(com_ericsson_triobjif, tsDataStream, TRACE_DEBUG)

#endif /* TRACE_H */

#include <lttng/tracepoint-event.h>
