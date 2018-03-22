#undef TRACEPOINT_PROVIDER
#define TRACEPOINT_PROVIDER com_ericsson_triobjif

#if !defined(_TRACEPOINT_TRI_UST_OBJ_IF_H) || defined(TRACEPOINT_HEADER_MULTI_READ)
#define _TRACEPOINT_TRI_UST_OBJ_IF_H

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

TRACEPOINT_EVENT(com_ericsson_triobjif, CHECK,
                 TP_ARGS(char *, processAndObjIf, size_t, processAndObjIfLen,
                         const char *, fileAndLine, size_t, fileAndLinelen,
                         const char *,  msg, size_t, msglen),
                 TP_FIELDS(
                           ctf_string(processAndObjIf, processAndObjIf)
                           ctf_string(fileAndLine, fileAndLine)
                           ctf_string(msg, msg)
        )
)
TRACEPOINT_LOGLEVEL(com_ericsson_triobjif, CHECK, TRACE_CRIT)

TRACEPOINT_EVENT(com_ericsson_triobjif, ERROR,
                 TP_ARGS(char *, processAndObjIf, size_t, processAndObjIfLen,
                         const char *, fileAndLine, size_t, fileAndLinelen,
                         const char *,  msg, size_t, msglen),
                 TP_FIELDS(
                           ctf_string(processAndObjIf, processAndObjIf)
                           ctf_string(fileAndLine, fileAndLine)
                           ctf_string(msg, msg)
        )
)
TRACEPOINT_LOGLEVEL(com_ericsson_triobjif, ERROR, TRACE_ERR)

TRACEPOINT_EVENT(com_ericsson_triobjif, ENTER,
                 TP_ARGS(char *, processAndObjIf, size_t, processAndObjIfLen,
                         const char *, fileAndLine, size_t, fileAndLinelen,
                         const char *,  msg, size_t, msglen),
                 TP_FIELDS(
                           ctf_string(processAndObjIf, processAndObjIf)
                           ctf_string(fileAndLine, fileAndLine)
                           ctf_string(msg, msg)
        )
)
TRACEPOINT_LOGLEVEL(com_ericsson_triobjif, ENTER, TRACE_NOTICE)

TRACEPOINT_EVENT(com_ericsson_triobjif, RETURN,
                 TP_ARGS(char *, processAndObjIf, size_t, processAndObjIfLen,
                         const char *, fileAndLine, size_t, fileAndLinelen),
                 TP_FIELDS(
                           ctf_string(processAndObjIf, processAndObjIf)
                           ctf_string(fileAndLine, fileAndLine)
        )
)
TRACEPOINT_LOGLEVEL(com_ericsson_triobjif, RETURN, TRACE_NOTICE)

TRACEPOINT_EVENT(com_ericsson_triobjif, INFO,
                 TP_ARGS(char *, processAndObjIf, size_t, processAndObjIfLen,
                         const char *, fileAndLine, size_t, fileAndLinelen,
                         const char *,  msg, size_t, msglen),
                 TP_FIELDS(
                           ctf_string(processAndObjIf, processAndObjIf)
                           ctf_string(fileAndLine, fileAndLine)
                           ctf_string(msg, msg)
        )
)
TRACEPOINT_LOGLEVEL(com_ericsson_triobjif, INFO, TRACE_INFO)

TRACEPOINT_EVENT(com_ericsson_triobjif, TRACE1,
                 TP_ARGS(char *, processAndObjIf, size_t, processAndObjIfLen,
                         const char *, fileAndLine, size_t, fileAndLinelen,
                         const char*, msg, size_t, msglen),
                 TP_FIELDS(
                           ctf_string(processAndObjIf, processAndObjIf)
                           ctf_string(fileAndLine, fileAndLine)
                           ctf_string(msg, msg)
        )
)
TRACEPOINT_LOGLEVEL(com_ericsson_triobjif, TRACE1, TRACE_NOTICE)

TRACEPOINT_EVENT(com_ericsson_triobjif, TRACE2,
                 TP_ARGS(char *, processAndObjIf, size_t, processAndObjIfLen,
                         const char *, fileAndLine, size_t, fileAndLinelen,
                         const char*, msg, size_t, msglen),
                 TP_FIELDS(
                           ctf_string(processAndObjIf, processAndObjIf)
                           ctf_string(fileAndLine, fileAndLine)
                           ctf_string(msg, msg)
        )
)
TRACEPOINT_LOGLEVEL(com_ericsson_triobjif, TRACE2, TRACE_NOTICE)

TRACEPOINT_EVENT(com_ericsson_triobjif, TRACE3,
                 TP_ARGS(char *, processAndObjIf, size_t, processAndObjIfLen,
                         const char *, fileAndLine, size_t, fileAndLinelen,
                         const char*, msg, size_t, msglen),
                 TP_FIELDS(
                           ctf_string(processAndObjIf, processAndObjIf)
                           ctf_string(fileAndLine, fileAndLine)
                           ctf_string(msg, msg)
        )
)
TRACEPOINT_LOGLEVEL(com_ericsson_triobjif, TRACE3, TRACE_NOTICE)

TRACEPOINT_EVENT(com_ericsson_triobjif, TRACE4,
                 TP_ARGS(char *, processAndObjIf, size_t, processAndObjIfLen,
                         const char *, fileAndLine, size_t, fileAndLinelen,
                         const char*, msg, size_t, msglen),
                 TP_FIELDS(
                           ctf_string(processAndObjIf, processAndObjIf)
                           ctf_string(fileAndLine, fileAndLine)
                           ctf_string(msg, msg)
       )
)
TRACEPOINT_LOGLEVEL(com_ericsson_triobjif, TRACE4, TRACE_NOTICE)

TRACEPOINT_EVENT(com_ericsson_triobjif, TRACE5,
                 TP_ARGS(char *, processAndObjIf, size_t, processAndObjIfLen,
                         const char *, fileAndLine, size_t, fileAndLinelen,
                         const char*, msg, size_t, msglen),
                 TP_FIELDS(
                           ctf_string(processAndObjIf, processAndObjIf)
                           ctf_string(fileAndLine, fileAndLine)
                           ctf_string(msg, msg)
        )
)
TRACEPOINT_LOGLEVEL(com_ericsson_triobjif, TRACE5, TRACE_NOTICE)

TRACEPOINT_EVENT(com_ericsson_triobjif, TRACE6,
                 TP_ARGS(char *, processAndObjIf, size_t, processAndObjIfLen,
                         const char *, fileAndLine, size_t, fileAndLinelen,
                         const char*, msg, size_t, msglen),
                 TP_FIELDS(
                           ctf_string(processAndObjIf, processAndObjIf)
                           ctf_string(fileAndLine, fileAndLine)
                           ctf_string(msg, msg)
        )
)
TRACEPOINT_LOGLEVEL(com_ericsson_triobjif, TRACE6, TRACE_NOTICE)

TRACEPOINT_EVENT(com_ericsson_triobjif, TRACE7,
                 TP_ARGS(char *, processAndObjIf, size_t, processAndObjIfLen,
                         const char *, fileAndLine, size_t, fileAndLinelen,
                         const char*, msg, size_t, msglen),
                 TP_FIELDS(
                           ctf_string(processAndObjIf, processAndObjIf)
                           ctf_string(fileAndLine, fileAndLine)
                           ctf_string(msg, msg)
        )
)
TRACEPOINT_LOGLEVEL(com_ericsson_triobjif, TRACE7, TRACE_NOTICE)

TRACEPOINT_EVENT(com_ericsson_triobjif, TRACE8,
                 TP_ARGS(char *, processAndObjIf, size_t, processAndObjIfLen,
                         const char *, fileAndLine, size_t, fileAndLinelen,
                         const char*, msg, size_t, msglen),
                 TP_FIELDS(
                           ctf_string(processAndObjIf, processAndObjIf)
                           ctf_string(fileAndLine, fileAndLine)
                           ctf_string(msg, msg)
        )
)
TRACEPOINT_LOGLEVEL(com_ericsson_triobjif, TRACE8, TRACE_NOTICE)

TRACEPOINT_EVENT(com_ericsson_triobjif, TRACE9,
                 TP_ARGS(char *, processAndObjIf, size_t, processAndObjIfLen,
                         const char *, fileAndLine, size_t, fileAndLinelen,
                         const char*, msg, size_t, msglen),
                 TP_FIELDS(
                           ctf_string(processAndObjIf, processAndObjIf)
                           ctf_string(fileAndLine, fileAndLine)
                           ctf_string(msg, msg)
        )
)
TRACEPOINT_LOGLEVEL(com_ericsson_triobjif, TRACE9, TRACE_NOTICE)

TRACEPOINT_EVENT(com_ericsson_triobjif, STATE_CHANGE,
                 TP_ARGS(char *, processAndObjIf, size_t, processAndObjIfLen,
                         const char *, fileAndLine, size_t, fileAndLinelen, 
                         const char*, msg, size_t, msglen),
                 TP_FIELDS(
                           ctf_string(processAndObjIf, processAndObjIf)
                           ctf_string(fileAndLine, fileAndLine)
                           ctf_string(msg, msg)
        )
)
TRACEPOINT_LOGLEVEL(com_ericsson_triobjif, STATE_CHANGE, TRACE_NOTICE)

TRACEPOINT_EVENT(com_ericsson_triobjif, BUS_SEND,
                 TP_ARGS(char *, processAndObjIf, size_t, processAndObjIfLen,
                         const char *, fileAndLine, size_t, fileAndLinelen,
                         const char *,  msg, size_t, msglen,
                         char*, data, size_t, datalen),
                 TP_FIELDS(
                           ctf_string(processAndObjIf, processAndObjIf)
                           ctf_string(fileAndLine, fileAndLine)
                           ctf_string(msg, msg)
                           ctf_sequence(char, dataBusSend, data, size_t, datalen)
        )
)
TRACEPOINT_LOGLEVEL(com_ericsson_triobjif, BUS_SEND, TRACE_NOTICE)

TRACEPOINT_EVENT(com_ericsson_triobjif, BUS_RECEIVE,
                 TP_ARGS(char *, processAndObjIf, size_t, processAndObjIfLen,
                         const char *, fileAndLine, size_t, fileAndLinelen, 
                         const char *,  msg, size_t, msglen,
                         char*, data, size_t, datalen),
                 TP_FIELDS(
                           ctf_string(processAndObjIf, processAndObjIf)
                           ctf_string(fileAndLine, fileAndLine)
                           ctf_string(msg, msg)
                           ctf_sequence(char, dataBusRec, data, size_t, datalen)
        )
)
TRACEPOINT_LOGLEVEL(com_ericsson_triobjif, BUS_RECEIVE, TRACE_NOTICE)

TRACEPOINT_EVENT(com_ericsson_triobjif, REC_SIG,
                 TP_ARGS(char *, processAndObjIf, size_t, processAndObjIfLen,
                         const char *, fileAndLine, size_t, fileAndLinelen,
                         const char*, msg, size_t, msglen),
                 TP_FIELDS(
                           ctf_string(processAndObjIf, processAndObjIf)
                           ctf_string(fileAndLine, fileAndLine)
                           ctf_string(msg, msg)
        )
)
TRACEPOINT_LOGLEVEL(com_ericsson_triobjif, REC_SIG, TRACE_NOTICE)

TRACEPOINT_EVENT(com_ericsson_triobjif, SEND_SIG,
                 TP_ARGS(char *, processAndObjIf, size_t, processAndObjIfLen,
                         const char *, fileAndLine, size_t, fileAndLinelen,
                         const char*, msg, size_t, msglen),
                 TP_FIELDS(
                           ctf_string(processAndObjIf, processAndObjIf)
                           ctf_string(fileAndLine, fileAndLine)
                           ctf_string(msg, msg)
        )
)
TRACEPOINT_LOGLEVEL(com_ericsson_triobjif, SEND_SIG, TRACE_NOTICE)

TRACEPOINT_EVENT(com_ericsson_triobjif, PARAM,
                 TP_ARGS(char *, processAndObjIf, size_t, processAndObjIfLen,
                         const char *, fileAndLine, size_t, fileAndLinelen,
                         const char*, msg, size_t, msglen),
                 TP_FIELDS(
                           ctf_string(processAndObjIf, processAndObjIf)
                           ctf_string(fileAndLine, fileAndLine)
                           ctf_string(msg, msg)
        )
)
TRACEPOINT_LOGLEVEL(com_ericsson_triobjif, PARAM, TRACE_NOTICE)

TRACEPOINT_EVENT(com_ericsson_triobjif, TRACE_UTS,
                 TP_ARGS(char *, processAndObjIf, size_t, processAndObjIfLen,
                         long, sec, int, usec,
                         const char *, fileAndLine, size_t, fileAndLinelen,
                         const char *,  msg, size_t, msglen,
                         char*, data, size_t, datalen),
                 TP_FIELDS(
                           ctf_string(processAndObjIf, processAndObjIf)
                           ctf_integer(long, sec, sec)
                           ctf_integer(int, usec, usec)
                           ctf_string(fileAndLine, fileAndLine)
                           ctf_string(msg, msg)
                           ctf_sequence(char, dataUts, data, size_t, datalen)
        )
)
TRACEPOINT_LOGLEVEL(com_ericsson_triobjif, TRACE_UTS, TRACE_NOTICE)

TRACEPOINT_EVENT(com_ericsson_triobjif, INTERFACE,
                 TP_ARGS(char *, processAndObjIf, size_t, processAndObjIfLen,
                         const char *, fileAndLine, size_t, fileAndLinelen,
                         const char*, msg, size_t, msglen),
                 TP_FIELDS(
                           ctf_string(processAndObjIf, processAndObjIf)
                           ctf_string(fileAndLine, fileAndLine)
                           ctf_string(msg, msg)
        )
)
TRACEPOINT_LOGLEVEL(com_ericsson_triobjif, INTERFACE, TRACE_NOTICE)

TRACEPOINT_EVENT(com_ericsson_triobjif, TRACE_OBJ,
                 TP_ARGS(char *, processAndObjIf, size_t, processAndObjIfLen,
                         const char *, fileAndLine, size_t, fileAndLinelen,
                         const char *,  msg, size_t, msglen),
                 TP_FIELDS(
                           ctf_string(processAndObjIf, processAndObjIf)
                           ctf_string(fileAndLine, fileAndLine)
                           ctf_string(msg, msg)
         )
)
TRACEPOINT_LOGLEVEL(com_ericsson_triobjif, TRACE_OBJ, TRACE_NOTICE)

TRACEPOINT_EVENT(com_ericsson_triobjif, USER1,
                 TP_ARGS(char *, processAndObjIf, size_t, processAndObjIfLen,
                         const char *, fileAndLine, size_t, fileAndLinelen,
                         const char *,  msg, size_t, msglen),
                 TP_FIELDS(
                           ctf_string(processAndObjIf, processAndObjIf)
                           ctf_string(fileAndLine, fileAndLine)
                           ctf_string(msg, msg)
         )
)
TRACEPOINT_LOGLEVEL(com_ericsson_triobjif, USER1, TRACE_NOTICE)

TRACEPOINT_EVENT(com_ericsson_triobjif, USER2,
                 TP_ARGS(char *, processAndObjIf, size_t, processAndObjIfLen,
                         const char *, fileAndLine, size_t, fileAndLinelen,
                         const char *,  msg, size_t, msglen),
                 TP_FIELDS(
                           ctf_string(processAndObjIf, processAndObjIf)
                           ctf_string(fileAndLine, fileAndLine)
                           ctf_string(msg, msg)
         )
)
TRACEPOINT_LOGLEVEL(com_ericsson_triobjif, USER2, TRACE_NOTICE)

TRACEPOINT_EVENT(com_ericsson_triobjif, USER3,
                 TP_ARGS(char *, processAndObjIf, size_t, processAndObjIfLen,
                         const char *, fileAndLine, size_t, fileAndLinelen,
                         const char *,  msg, size_t, msglen),
                 TP_FIELDS(
                           ctf_string(processAndObjIf, processAndObjIf)
                           ctf_string(fileAndLine, fileAndLine)
                           ctf_string(msg, msg)
         )
)
TRACEPOINT_LOGLEVEL(com_ericsson_triobjif, USER3, TRACE_NOTICE)

TRACEPOINT_EVENT(com_ericsson_triobjif, USER4,
                 TP_ARGS(char *, processAndObjIf, size_t, processAndObjIfLen,
                         const char *, fileAndLine, size_t, fileAndLinelen,
                         const char *,  msg, size_t, msglen),
                 TP_FIELDS(
                           ctf_string(processAndObjIf, processAndObjIf)
                           ctf_string(fileAndLine, fileAndLine)
                           ctf_string(msg, msg)
         )
)
TRACEPOINT_LOGLEVEL(com_ericsson_triobjif, USER4, TRACE_NOTICE)

TRACEPOINT_EVENT(com_ericsson_triobjif, DEFAULT,
                 TP_ARGS(int, value),
                 TP_FIELDS(
                           ctf_integer(int, value, value)
         )
)
TRACEPOINT_LOGLEVEL(com_ericsson_triobjif, DEFAULT, TRACE_NOTICE)

#endif /* _TRACEPOINT_TRI_UST_OBJ_IF_H */

#undef TRACEPOINT_INCLUDE_FILE
#define TRACEPOINT_INCLUDE_FILE tri_ust_obj_if.h

/* This part must be outside ifdef protection */
#include <lttng/tracepoint-event.h>

#ifdef __cplusplus 
}
#endif
