/* ---------------------------------------------------------------------------
 *
 * © Ericsson AB 2015 All rights reserved.
 * The information in this document is the property of Ericsson.  Except
 * as specifically authorized in writing by Ericsson, the receiver of
 * this document shall keep the information contained herein confidential
 * and shall protect the same in whole or in part from disclosure and
 * dissemination to third parties.  Disclosure and disseminations to the
 * receivers employees shall only be made on a strict need to know basis.
 *
 * ---------------------------------------------------------------------------
 */

#undef TRACEPOINT_PROVIDER
#define TRACEPOINT_PROVIDER com_ericsson_xcs_shell_trace

#undef TRACEPOINT_INCLUDE_FILE
#define TRACEPOINT_INCLUDE_FILE com_ericsson_xcs_shell_trace.h

#if !defined( _COM_ERICSSON_XCS_SHELL_TRACE_H) || defined(TRACEPOINT_HEADER_MULTI_READ)
#define _COM_ERICSSON_XCS_SHELL_TRACE_H

#ifdef __cplusplus
extern "C" {
#endif

#include <lttng/tracepoint.h>

/* This is done, so macro TRACEPOINT_PROVIDER can be
 * expanded before macro TRACEPOINT_LOGLEVEL is used*/
#define COM_ERICSSON_XCS_SHELL_TRACE_LOGLEVEL(a, b, c) TRACEPOINT_LOGLEVEL(a, b, c)

/* MSG */
#define MSG_ARGS \
	TP_ARGS( \
	         const char*, file, \
	         unsigned int, line, \
	         const char*, msg \
	       )
#define MSG_FIELDS \
	TP_FIELDS( \
	           ctf_string(file, file) \
	           ctf_integer(unsigned int, line, line) \
	           ctf_string(msg, msg) \
	         )
TRACEPOINT_EVENT(
        TRACEPOINT_PROVIDER,
        ERROR,
        MSG_ARGS,
        MSG_FIELDS
)
COM_ERICSSON_XCS_SHELL_TRACE_LOGLEVEL(TRACEPOINT_PROVIDER, ERROR, TRACE_ERR)
TRACEPOINT_EVENT(
        TRACEPOINT_PROVIDER,
        INFO,
        MSG_ARGS,
        MSG_FIELDS
)
COM_ERICSSON_XCS_SHELL_TRACE_LOGLEVEL(TRACEPOINT_PROVIDER, INFO, TRACE_INFO)

/* MSG + OBJ */
#define OBJ_MSG_ARGS \
	TP_ARGS( \
	         const char*, file, \
	         unsigned int, line, \
	         const char*, obj, \
	         const char*, msg \
	       )
#define OBJ_MSG_FIELDS \
	TP_FIELDS( \
	           ctf_string(file, file) \
	           ctf_integer(unsigned int, line, line) \
	           ctf_string(obj, obj) \
	           ctf_string(msg, msg) \
	         )
TRACEPOINT_EVENT(
        TRACEPOINT_PROVIDER,
        TRACE1,
        OBJ_MSG_ARGS,
        OBJ_MSG_FIELDS
)
COM_ERICSSON_XCS_SHELL_TRACE_LOGLEVEL(TRACEPOINT_PROVIDER, TRACE1,
                                      TRACE_DEBUG_PROGRAM)
TRACEPOINT_EVENT(
        TRACEPOINT_PROVIDER,
        TRACE2,
        OBJ_MSG_ARGS,
        OBJ_MSG_FIELDS
)
COM_ERICSSON_XCS_SHELL_TRACE_LOGLEVEL(TRACEPOINT_PROVIDER, TRACE2,
                                      TRACE_DEBUG_PROCESS)
TRACEPOINT_EVENT(
        TRACEPOINT_PROVIDER,
        TRACE3,
        OBJ_MSG_ARGS,
        OBJ_MSG_FIELDS
)
COM_ERICSSON_XCS_SHELL_TRACE_LOGLEVEL(TRACEPOINT_PROVIDER, TRACE3,
                                      TRACE_DEBUG_MODULE)
TRACEPOINT_EVENT(
        TRACEPOINT_PROVIDER,
        TRACE4,
        OBJ_MSG_ARGS,
        OBJ_MSG_FIELDS
)
COM_ERICSSON_XCS_SHELL_TRACE_LOGLEVEL(TRACEPOINT_PROVIDER, TRACE4,
                                      TRACE_DEBUG_UNIT)
TRACEPOINT_EVENT(
        TRACEPOINT_PROVIDER,
        TRACE5,
        OBJ_MSG_ARGS,
        OBJ_MSG_FIELDS
)
COM_ERICSSON_XCS_SHELL_TRACE_LOGLEVEL(TRACEPOINT_PROVIDER, TRACE5,
                                      TRACE_DEBUG_FUNCTION)
TRACEPOINT_EVENT(
        TRACEPOINT_PROVIDER,
        TRACE6,
        OBJ_MSG_ARGS,
        OBJ_MSG_FIELDS
)
COM_ERICSSON_XCS_SHELL_TRACE_LOGLEVEL(TRACEPOINT_PROVIDER, TRACE6,
                                      TRACE_DEBUG_LINE)
TRACEPOINT_EVENT(
        TRACEPOINT_PROVIDER,
        TRACE7,
        OBJ_MSG_ARGS,
        OBJ_MSG_FIELDS
)
COM_ERICSSON_XCS_SHELL_TRACE_LOGLEVEL(TRACEPOINT_PROVIDER, TRACE7, TRACE_DEBUG)

#ifdef __cplusplus
}
#endif

#endif /* _COM_ERICSSON_XCS_SHELL_TRACE_H */

#include <lttng/tracepoint-event.h>
