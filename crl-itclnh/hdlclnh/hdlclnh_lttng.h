/* ---------------------------------------------------------------------------
 *
 * � Ericsson AB 2015 All rights reserved.
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
#define TRACEPOINT_PROVIDER com_ericsson_hdlclnh

#undef TRACEPOINT_INCLUDE_FILE
#define TRACEPOINT_INCLUDE_FILE ./hdlclnh_lttng.h

#ifdef __cplusplus
extern "C"{
#endif /* __cplusplus */

#ifdef LTTNG
#if !defined(_HDLCLNH_LTTNG_H_) || defined(TRACEPOINT_HEADER_MULTI_READ)
#define _HDLCLNH_LTTNG_H_

#include "lttng/tracepoint.h"
#include <stdint.h>


TRACEPOINT_EVENT(
        com_ericsson_hdlclnh,
        hdlclnh_dbg, // Comment
        TP_ARGS(int,    line,
                char *, info),

        /* Next are the fields */
        TP_FIELDS(
                ctf_integer(int, line, line)
                ctf_string(info, info)
        )
)

TRACEPOINT_EVENT(
        com_ericsson_hdlclnh,
        hdlclnh_info, // Comment
        TP_ARGS(int,    line,
                char *, info),

        /* Next are the fields */
        TP_FIELDS(
                ctf_integer(int, line, line)
                ctf_string(info, info)
        )
)

TRACEPOINT_EVENT(
        com_ericsson_hdlclnh,
        hdlclnh_error, // Comment
        TP_ARGS(int,    line,
                char *, error),

        /* Next are the fields */
        TP_FIELDS(
                ctf_integer(int, line, line)
                ctf_string(error, error)
        )
)



TRACEPOINT_LOGLEVEL(com_ericsson_hdlclnh, hdlclnh_dbg,        TRACE_DEBUG)
TRACEPOINT_LOGLEVEL(com_ericsson_hdlclnh, hdlclnh_info,       TRACE_INFO)
TRACEPOINT_LOGLEVEL(com_ericsson_hdlclnh, hdlclnh_error,      TRACE_ERR)

#include <lttng/tracepoint-event.h>

#endif /* _HDLCLNH_LTTNG_H_ */
#endif /* LTTNG */

#ifdef __cplusplus
}
#endif /* __cplusplus */