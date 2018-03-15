#undef TRACEPOINT_PROVIDER
#define TRACEPOINT_PROVIDER com_ericsson_ecb

#undef TRACEPOINT_INCLUDE_FILE
#define TRACEPOINT_INCLUDE_FILE ./ecb_lttng.h

#ifdef __cplusplus
extern "C"{
#endif /* __cplusplus */

#if !defined(_ECB_LTTNG_H) || defined(TRACEPOINT_HEADER_MULTI_READ)
#define _ECB_LTTNG_H

#include "lttng/tracepoint.h"

#include <stdint.h>


TRACEPOINT_EVENT(
        com_ericsson_ecb,
        a4ci_dbg, // Comment
        TP_ARGS(char *, dbg_msg),

        /* Next are the fields */
        TP_FIELDS(
                ctf_string(dbg, dbg_msg)
        )
)

TRACEPOINT_EVENT(
        com_ericsson_ecb,
        a4ci_info, // Comment
        TP_ARGS(char *, info_msg),

        /* Next are the fields */
        TP_FIELDS(
                ctf_string(info, info_msg)
        )
)

TRACEPOINT_EVENT(
        com_ericsson_ecb,
        a4ci_error, // Comment
        TP_ARGS(char *, err_msg),

        /* Next are the fields */
        TP_FIELDS(
                ctf_string(error, err_msg)
        )
)

TRACEPOINT_EVENT(
        com_ericsson_ecb,
        ecblnh_dbg, // Comment
        TP_ARGS(char *, dbg_msg),

        /* Next are the fields */
        TP_FIELDS(
                ctf_string(dbg, dbg_msg)
        )
)

TRACEPOINT_EVENT(
        com_ericsson_ecb,
        ecblnh_info, // Comment
        TP_ARGS(char *, info_msg),

        /* Next are the fields */
        TP_FIELDS(
                ctf_string(info, info_msg)
        )
)

TRACEPOINT_EVENT(
        com_ericsson_ecb,
        ecblnh_error, // Comment
        TP_ARGS(char *, err_msg),

        /* Next are the fields */
        TP_FIELDS(
                ctf_string(error, err_msg)
        )
)


TRACEPOINT_LOGLEVEL(com_ericsson_ecb, a4ci_dbg,  	TRACE_DEBUG)
TRACEPOINT_LOGLEVEL(com_ericsson_ecb, a4ci_info,  	TRACE_INFO)
TRACEPOINT_LOGLEVEL(com_ericsson_ecb, a4ci_error,	TRACE_ERR)
TRACEPOINT_LOGLEVEL(com_ericsson_ecb, ecblnh_dbg,  	TRACE_DEBUG)
TRACEPOINT_LOGLEVEL(com_ericsson_ecb, ecblnh_info,  TRACE_INFO)
TRACEPOINT_LOGLEVEL(com_ericsson_ecb, ecblnh_error,	TRACE_ERR)

#endif /* __ECB_LTTNG_H */

#include <lttng/tracepoint-event.h>

#ifdef __cplusplus
}
#endif /* __cplusplus */
