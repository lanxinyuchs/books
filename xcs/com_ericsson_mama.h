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
#define TRACEPOINT_PROVIDER com_ericsson_mama

#undef TRACEPOINT_INCLUDE_FILE
#define TRACEPOINT_INCLUDE_FILE com_ericsson_mama.h

#if !defined(_COM_ERICSSON_MAMA_H) || defined(TRACEPOINT_HEADER_MULTI_READ)
#define _COM_ERICSSON_MAMA_H

#include <lttng/tracepoint.h>

#ifdef __cplusplus
extern "C" {
#endif

/*
 * This is done, so macro TRACEPOINT_PROVIDER can be
 * expanded before macro TRACEPOINT_LOGLEVEL is used.
 */
#define COM_ERICSSON_MAMA_LOGLEVEL(a, b, c) TRACEPOINT_LOGLEVEL(a, b, c)

#define COM_ERICSSON_MAMA_ARGS \
        TP_ARGS( \
                const char*, file, \
                unsigned int, line, \
                const char*, msg \
        )
#define COM_ERICSSON_MAMA_FIELDS \
        TP_FIELDS(\
                ctf_string(file, file) \
                ctf_integer(unsigned int, line, line) \
                ctf_string(msg, msg) \
        )

/* coverity[unsigned_compare] error inside lttng macros */
TRACEPOINT_EVENT(
    TRACEPOINT_PROVIDER,
    ERROR,
    COM_ERICSSON_MAMA_ARGS,
    COM_ERICSSON_MAMA_FIELDS
)
COM_ERICSSON_MAMA_LOGLEVEL(
	TRACEPOINT_PROVIDER,
	ERROR,
	TRACE_ERR
)

/* coverity[unsigned_compare] error inside lttng macros */
TRACEPOINT_EVENT(
    TRACEPOINT_PROVIDER,
    INFO,
    COM_ERICSSON_MAMA_ARGS,
    COM_ERICSSON_MAMA_FIELDS
)
COM_ERICSSON_MAMA_LOGLEVEL(
	TRACEPOINT_PROVIDER,
	INFO,
	TRACE_NOTICE
)

/* coverity[unsigned_compare] error inside lttng macros */
TRACEPOINT_EVENT(
    TRACEPOINT_PROVIDER,
    TRACE2,
    COM_ERICSSON_MAMA_ARGS,
    COM_ERICSSON_MAMA_FIELDS
)
COM_ERICSSON_MAMA_LOGLEVEL(
	TRACEPOINT_PROVIDER,
	TRACE2,
	TRACE_DEBUG_PROGRAM
)

/* This is done, so macro TRACEPOINT_PROVIDER can be
 * expanded before macro tracepoint is used*/
#define com_ericsson_mama_tracepoint(provider, ...) \
	tracepoint(provider,  ## __VA_ARGS__)

#ifdef __cplusplus
}
#endif

#endif /* _COM_ERICSSON_MAMA_H */

#include <lttng/tracepoint-event.h>
