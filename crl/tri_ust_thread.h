#undef TRACEPOINT_PROVIDER
#define TRACEPOINT_PROVIDER com_ericsson_trithread

#if !defined(_TRACEPOINT_TRI_UST_THREAD_H) || defined(TRACEPOINT_HEADER_MULTI_READ)
#define _TRACEPOINT_TRI_UST_THREAD_H

#ifdef __cplusplus
extern "C" {
#endif

/*
 * Copyright (C) 2011  Mathieu Desnoyers <mathieu.desnoyers@efficios.com>
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 */

#include <lttng/tracepoint.h>

TRACEPOINT_EVENT(com_ericsson_trithread, CHECK,
		 TP_ARGS(char *, process, size_t, processlen,
                         const char *, fileAndLine, size_t, fileAndLinelen,
                         const char*, msg, size_t, msglen),
                 TP_FIELDS(
                           ctf_string(process, process)
                           ctf_string(fileAndLine, fileAndLine)
                           ctf_string(msg, msg)
	)
)
TRACEPOINT_LOGLEVEL(com_ericsson_trithread, CHECK, TRACE_CRIT)

TRACEPOINT_EVENT(com_ericsson_trithread, ERROR,
		 TP_ARGS(char *, process, size_t, processlen,
                         const char *, fileAndLine, size_t, fileAndLinelen,
                         const char*, msg, size_t, msglen),
                 TP_FIELDS(
                           ctf_string(process, process)
                           ctf_string(fileAndLine, fileAndLine)
                           ctf_string(msg, msg)
	)
)
TRACEPOINT_LOGLEVEL(com_ericsson_trithread, ERROR, TRACE_ERR)

TRACEPOINT_EVENT(com_ericsson_trithread, ENTER,
		 TP_ARGS(char *, process, size_t, processlen,
                         const char *, fileAndLine, size_t, fileAndLinelen,
                         const char*, msg, size_t, msglen),
                 TP_FIELDS(
                           ctf_string(process, process)
                           ctf_string(fileAndLine, fileAndLine)
                           ctf_string(msg, msg)
	)
)
TRACEPOINT_LOGLEVEL(com_ericsson_trithread, ENTER, TRACE_NOTICE)

TRACEPOINT_EVENT(com_ericsson_trithread, RETURN,
		 TP_ARGS(char *, process, size_t, processlen,
                         const char *, fileAndLine, size_t, fileAndLinelen),
                 TP_FIELDS(
                           ctf_string(process, process)
                           ctf_string(fileAndLine, fileAndLine)
	)
)
TRACEPOINT_LOGLEVEL(com_ericsson_trithread, RETURN, TRACE_NOTICE)

TRACEPOINT_EVENT(com_ericsson_trithread, INFO,
		 TP_ARGS(char *, process, size_t, processlen,
                         const char *, fileAndLine, size_t, fileAndLinelen,
                         const char*, msg, size_t, msglen),
                 TP_FIELDS(
                           ctf_string(process, process)
                           ctf_string(fileAndLine, fileAndLine)
                           ctf_string(msg, msg)
	)
)
TRACEPOINT_LOGLEVEL(com_ericsson_trithread, INFO, TRACE_INFO)

TRACEPOINT_EVENT(com_ericsson_trithread, TRACE1,
		 TP_ARGS(char *, process, size_t, processlen,
                         const char *, fileAndLine, size_t, fileAndLinelen,
                         const char*, msg, size_t, msglen),
                 TP_FIELDS(
                           ctf_string(process, process)
                           ctf_string(fileAndLine, fileAndLine)
                           ctf_string(msg, msg)
	)
)
TRACEPOINT_LOGLEVEL(com_ericsson_trithread, TRACE1, TRACE_NOTICE)

TRACEPOINT_EVENT(com_ericsson_trithread, TRACE2,
		 TP_ARGS(char *, process, size_t, processlen,
                         const char *, fileAndLine, size_t, fileAndLinelen,
                         const char*, msg, size_t, msglen),
                 TP_FIELDS(
                           ctf_string(process, process)
                           ctf_string(fileAndLine, fileAndLine)
                           ctf_string(msg, msg)
	)
)
TRACEPOINT_LOGLEVEL(com_ericsson_trithread, TRACE2, TRACE_NOTICE)

TRACEPOINT_EVENT(com_ericsson_trithread, TRACE3,
		 TP_ARGS(char *, process, size_t, processlen,
                         const char *, fileAndLine, size_t, fileAndLinelen,
                         const char*, msg, size_t, msglen),
                 TP_FIELDS(
                           ctf_string(process, process)
                           ctf_string(fileAndLine, fileAndLine)
                           ctf_string(msg, msg)
	)
)
TRACEPOINT_LOGLEVEL(com_ericsson_trithread, TRACE3, TRACE_NOTICE)

TRACEPOINT_EVENT(com_ericsson_trithread, TRACE4,
		 TP_ARGS(char *, process, size_t, processlen,
                         const char *, fileAndLine, size_t, fileAndLinelen,
                         const char*, msg, size_t, msglen),
                 TP_FIELDS(
                           ctf_string(process, process)
                           ctf_string(fileAndLine, fileAndLine)
                           ctf_string(msg, msg)
	)
)
TRACEPOINT_LOGLEVEL(com_ericsson_trithread, TRACE4, TRACE_NOTICE)

TRACEPOINT_EVENT(com_ericsson_trithread, TRACE5,
		 TP_ARGS(char *, process, size_t, processlen,
                         const char *, fileAndLine, size_t, fileAndLinelen,
                         const char*, msg, size_t, msglen),
                 TP_FIELDS(
                           ctf_string(process, process)
                           ctf_string(fileAndLine, fileAndLine)
                           ctf_string(msg, msg)
	)
)
TRACEPOINT_LOGLEVEL(com_ericsson_trithread, TRACE5, TRACE_NOTICE)

TRACEPOINT_EVENT(com_ericsson_trithread, TRACE6,
		 TP_ARGS(char *, process, size_t, processlen,
                         const char *, fileAndLine, size_t, fileAndLinelen,
                         const char*, msg, size_t, msglen),
                 TP_FIELDS(
                           ctf_string(process, process)
                           ctf_string(fileAndLine, fileAndLine)
                           ctf_string(msg, msg)
	)
)
TRACEPOINT_LOGLEVEL(com_ericsson_trithread, TRACE6, TRACE_NOTICE)

TRACEPOINT_EVENT(com_ericsson_trithread, TRACE7,
                 TP_ARGS(char *, process, size_t, processlen,
                         const char *, fileAndLine, size_t, fileAndLinelen,
                         const char*, msg, size_t, msglen),
                 TP_FIELDS(
                           ctf_string(process, process)
                           ctf_string(fileAndLine, fileAndLine)
                           ctf_string(msg, msg)
	)
)
TRACEPOINT_LOGLEVEL(com_ericsson_trithread, TRACE7, TRACE_NOTICE)

TRACEPOINT_EVENT(com_ericsson_trithread, TRACE8,
                 TP_ARGS(char *, process, size_t, processlen,
                         const char *, fileAndLine, size_t, fileAndLinelen,
                         const char*, msg, size_t, msglen),
                 TP_FIELDS(
                           ctf_string(process, process)
                           ctf_string(fileAndLine, fileAndLine)
                           ctf_string(msg, msg)
	)
)
TRACEPOINT_LOGLEVEL(com_ericsson_trithread, TRACE8, TRACE_NOTICE)

TRACEPOINT_EVENT(com_ericsson_trithread, TRACE9,
                 TP_ARGS(char *, process, size_t, processlen,
                         const char *, fileAndLine, size_t, fileAndLinelen,
                         const char*, msg, size_t, msglen),
                 TP_FIELDS(
                           ctf_string(process, process)
                           ctf_string(fileAndLine, fileAndLine)
                           ctf_string(msg, msg)
	)
)
TRACEPOINT_LOGLEVEL(com_ericsson_trithread, TRACE9,  TRACE_NOTICE)

TRACEPOINT_EVENT(com_ericsson_trithread, STATE_CHANGE,
                 TP_ARGS(char *, process, size_t, processlen,
                         const char *, fileAndLine, size_t, fileAndLinelen,
                         const char*, msg, size_t, msglen),
                 TP_FIELDS(
                           ctf_string(process, process)
                           ctf_string(fileAndLine, fileAndLine)
                           ctf_string(msg, msg)
	)
)
TRACEPOINT_LOGLEVEL(com_ericsson_trithread, STATE_CHANGE, TRACE_NOTICE)

TRACEPOINT_EVENT(com_ericsson_trithread, BUS_SEND,
                 TP_ARGS(char*, process, size_t, processlen,
                         const char *, fileAndLine, size_t, fileAndLinelen,
                         const char*, msg, size_t, msglen, 
                         char*, data, size_t, datalen),
                 TP_FIELDS(
                           ctf_string(process, process)
                           ctf_string(fileAndLine, fileAndLine)
                           ctf_string(msg, msg)
                           ctf_sequence(char, dataBusSend, data, size_t, datalen)
	)
)
TRACEPOINT_LOGLEVEL(com_ericsson_trithread, BUS_SEND, TRACE_NOTICE)

TRACEPOINT_EVENT(com_ericsson_trithread, BUS_RECEIVE,
                 TP_ARGS(char *, process, size_t, processlen,
                         const char *, fileAndLine, size_t, fileAndLinelen,
                         const char*, msg, size_t, msglen, 
                         char*, data, size_t, datalen),
                 TP_FIELDS(
                           ctf_string(process, process)
                           ctf_string(fileAndLine, fileAndLine)
                           ctf_string(msg, msg)
                           ctf_sequence(char, dataBusRec, data, size_t, datalen)
	)
)
TRACEPOINT_LOGLEVEL(com_ericsson_trithread, BUS_RECEIVE, TRACE_NOTICE)

TRACEPOINT_EVENT(com_ericsson_trithread, REC_SIG,
                 TP_ARGS(char *, process, size_t, processlen,
                         const char *, fileAndLine, size_t, fileAndLinelen,
                         const char*, msg, size_t, msglen),
                 TP_FIELDS(
                           ctf_string(process, process)
                           ctf_string(fileAndLine, fileAndLine)
                           ctf_string(msg, msg)
	)
)
TRACEPOINT_LOGLEVEL(com_ericsson_trithread, REC_SIG, TRACE_NOTICE)

TRACEPOINT_EVENT(com_ericsson_trithread, TRACE_UTS,
		 TP_ARGS(int, sec, int, usec,
                         char *, process, size_t, processlen,
                         const char *, fileAndLine, size_t, fileAndLinelen,
                         const char *,  msg, size_t, msglen,
                         char*, data, size_t, datalen),
                 TP_FIELDS(
                           ctf_integer(int, sec, sec)
                           ctf_integer(int, usec, usec)
                           ctf_string(process, process)
                           ctf_string(fileAndLine, fileAndLine)
                           ctf_string(msg, msg)
                           ctf_sequence(char, dataUts, data, size_t, datalen)
	)
)
TRACEPOINT_LOGLEVEL(com_ericsson_trithread, TRACE_UTS, TRACE_NOTICE)

TRACEPOINT_EVENT(com_ericsson_trithread, SEND_SIG,
                 TP_ARGS(char *, process, size_t, processlen,
                         const char *, fileAndLine, size_t, fileAndLinelen,
                         const char*, msg, size_t, msglen),
                 TP_FIELDS(
                           ctf_string(process, process)
                           ctf_string(fileAndLine, fileAndLine)
                           ctf_string(msg, msg)
	)
)
TRACEPOINT_LOGLEVEL(com_ericsson_trithread, SEND_SIG, TRACE_NOTICE)

TRACEPOINT_EVENT(com_ericsson_trithread, PARAM,
                 TP_ARGS(char *, process, size_t, processlen,
                         const char *, fileAndLine, size_t, fileAndLinelen,
                         const char*, msg, size_t, msglen),
                 TP_FIELDS(
                           ctf_string(process, process)
                           ctf_string(fileAndLine, fileAndLine)
                           ctf_string(msg, msg)
	)
)
TRACEPOINT_LOGLEVEL(com_ericsson_trithread, PARAM, TRACE_NOTICE)

TRACEPOINT_EVENT(com_ericsson_trithread, INTERFACE,
                 TP_ARGS(char *, process, size_t, processlen,
                         const char *, fileAndLine, size_t, fileAndLinelen,
                         const char*, msg, size_t, msglen),
                 TP_FIELDS(
                           ctf_string(process, process)
                           ctf_string(fileAndLine, fileAndLine)
                           ctf_string(msg, msg)
	)
)
TRACEPOINT_LOGLEVEL(com_ericsson_trithread, INTERFACE, TRACE_NOTICE)

TRACEPOINT_EVENT(com_ericsson_trithread, TRACE_OBJ,
                 TP_ARGS(char *, process, size_t, processlen,
                         const char *, fileAndLine, size_t, fileAndLinelen,
                         const char*, msg, size_t, msglen),
                 TP_FIELDS(
                           ctf_string(process, process)
                           ctf_string(fileAndLine, fileAndLine)
                           ctf_string(msg, msg)
	)
)
TRACEPOINT_LOGLEVEL(com_ericsson_trithread, TRACE_OBJ, TRACE_NOTICE)

TRACEPOINT_EVENT(com_ericsson_trithread, USER1,
                 TP_ARGS(char *, process, size_t, processlen,
                         const char *, fileAndLine, size_t, fileAndLinelen,
                         const char*, msg, size_t, msglen),
                 TP_FIELDS(
                           ctf_string(process, process)
                           ctf_string(fileAndLine, fileAndLine)
                           ctf_string(msg, msg)
	)
)
TRACEPOINT_LOGLEVEL(com_ericsson_trithread, USER1, TRACE_NOTICE)

TRACEPOINT_EVENT(com_ericsson_trithread, USER2,
                 TP_ARGS(char *, process, size_t, processlen,
                         const char *, fileAndLine, size_t, fileAndLinelen,
                         const char*, msg, size_t, msglen),
                 TP_FIELDS(
                           ctf_string(process, process)
                           ctf_string(fileAndLine, fileAndLine)
                           ctf_string(msg, msg)
	)
)
TRACEPOINT_LOGLEVEL(com_ericsson_trithread, USER2, TRACE_NOTICE)

TRACEPOINT_EVENT(com_ericsson_trithread, USER3,
                 TP_ARGS(char *, process, size_t, processlen,
                         const char *, fileAndLine, size_t, fileAndLinelen,
                         const char*, msg, size_t, msglen),
                 TP_FIELDS(
                           ctf_string(process, process)
                           ctf_string(fileAndLine, fileAndLine)
                           ctf_string(msg, msg)
	)
)
TRACEPOINT_LOGLEVEL(com_ericsson_trithread, USER3, TRACE_NOTICE)

TRACEPOINT_EVENT(com_ericsson_trithread, USER4,
                 TP_ARGS(char *, process, size_t, processlen,
                         const char *, fileAndLine, size_t, fileAndLinelen,
                         const char*, msg, size_t, msglen),
                 TP_FIELDS(
                           ctf_string(process, process)
                           ctf_string(fileAndLine, fileAndLine)
                           ctf_string(msg, msg)
	)
)
TRACEPOINT_LOGLEVEL(com_ericsson_trithread, USER4, TRACE_NOTICE)

TRACEPOINT_EVENT(com_ericsson_trithread, DEFAULT,
                 TP_ARGS(int, value),
                 TP_FIELDS(
                           ctf_integer(int, line, value)
	)
)
TRACEPOINT_LOGLEVEL(com_ericsson_trithread, DEFAULT, TRACE_NOTICE)

#endif /* _TRACEPOINT_TRI_UST_THREAD_H */

#undef TRACEPOINT_INCLUDE_FILE
#define TRACEPOINT_INCLUDE_FILE tri_ust_thread.h

/* This part must be outside ifdef protection */
#include <lttng/tracepoint-event.h>

#ifdef __cplusplus 
}
#endif
