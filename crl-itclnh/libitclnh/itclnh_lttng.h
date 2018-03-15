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
#define TRACEPOINT_PROVIDER com_ericsson_itclnh

#undef TRACEPOINT_INCLUDE_FILE
#define TRACEPOINT_INCLUDE_FILE ./itclnh_lttng.h

#ifdef __cplusplus
extern "C"{
#endif /* __cplusplus */

#ifdef LTTNG
#if !defined(_ITCLNH_LTTNG_H_) || defined(TRACEPOINT_HEADER_MULTI_READ)
#define _ITCLNH_LTTNG_H_

#include "lttng/tracepoint.h"
#endif

#include <stdint.h>


TRACEPOINT_EVENT(
        com_ericsson_itclnh,
        rx_rlnh_query,
        TP_ARGS(char *,   query_name,
                char *,   link_name),

        TP_FIELDS(
                ctf_string(query_name, query_name)
                ctf_string(link_name, link_name)
        )
)

TRACEPOINT_EVENT(
        com_ericsson_itclnh,
        rx_rlnh_init,
        TP_ARGS(char *, link_name),

        TP_FIELDS(
                ctf_string(link_name, link_name)
        )
)

TRACEPOINT_EVENT(
        com_ericsson_itclnh,
        shutdown_link,
        TP_ARGS(char *, link_name),

        TP_FIELDS(
                ctf_string(link_name, link_name)
        )
)

TRACEPOINT_EVENT(
        com_ericsson_itclnh,
        tx_rlnh_query,
        TP_ARGS(char *,   query_name,
                char *,   link_name),

        TP_FIELDS(
                ctf_string(query_name, query_name)
                ctf_string(link_name, link_name)
        )
)

TRACEPOINT_EVENT(
        com_ericsson_itclnh,
        tx_rlnh_publish,
        TP_ARGS(char *,   thread_name,
                char *,   link_name,
                 uint32_t , local_addr),

        TP_FIELDS(
                ctf_string(thread_name, thread_name)
                ctf_string(link_name, link_name)
                ctf_integer(uint32_t, local_addr, local_addr)
        )
)

TRACEPOINT_EVENT(
        com_ericsson_itclnh,
        rx_rlnh_publish,
        TP_ARGS(char *,   proc_name,
                uint32_t, local_addr,
                char *,   link_name),

        TP_FIELDS(
                ctf_string(proc_name, proc_name)
                ctf_integer(uint32_t, local_addr, local_addr)
                ctf_string(link_name, link_name)
        )
)

TRACEPOINT_EVENT(
        com_ericsson_itclnh,
        tx_rlnh_unpublish,
        TP_ARGS(char *,   link_name,
                uint32_t, local_addr),

        TP_FIELDS(
                ctf_string(link_name, link_name)
                ctf_integer(uint32_t, local_addr, local_addr)
        )
)

TRACEPOINT_EVENT(
        com_ericsson_itclnh,
        rx_rlnh_unpublish,
        TP_ARGS(char *,   link_name,
                uint32_t, local_addr),

        TP_FIELDS(
                ctf_string(link_name, link_name)
                ctf_integer(uint32_t, local_addr, local_addr)
        )
)

TRACEPOINT_EVENT(
        com_ericsson_itclnh,
        handle_locate,
        TP_ARGS(char *,   lnh_name,
                char *,   locate_name,
                uint32_t, locate_from_mbox_id),

        TP_FIELDS(
                ctf_string(lnh_name, lnh_name)
                ctf_string(locate_name, locate_name)
                ctf_integer(uint32_t, locate_from_mbox_id, locate_from_mbox_id)
        )
)

TRACEPOINT_EVENT(
        com_ericsson_itclnh,
        add_locate,
        TP_ARGS(char *,   lnh_name,
                char *,   locate_name,
                uint32_t, locate_from_mbox_id),

        TP_FIELDS(
                ctf_string(lnh_name, lnh_name)
                ctf_string(locate_name, locate_name)
                ctf_integer(uint32_t, locate_from_mbox_id, locate_from_mbox_id)
        )
)

TRACEPOINT_EVENT(
        com_ericsson_itclnh,
        remove_locate,
        TP_ARGS(char *,   lnh_name,
                char *,   locate_name,
                uint32_t, locate_from_mbox_id),

        TP_FIELDS(
                ctf_string(lnh_name, lnh_name)
                ctf_string(locate_name, locate_name)
                ctf_integer(uint32_t, locate_from_mbox_id, locate_from_mbox_id)
        )
)

TRACEPOINT_EVENT(
        com_ericsson_itclnh,
        lnh_shutdown_req,
        TP_ARGS(char *,   lnh_name,
                uint32_t , lnh_mbox_id),

        TP_FIELDS(
                ctf_string(lnh_name, lnh_name)
                ctf_integer(uint32_t, lnh_mbox_id, lnh_mbox_id)
        )
)

TRACEPOINT_EVENT(
        com_ericsson_itclnh,
        link_connect_state,
        TP_ARGS(char *,  link_name,
                uint32_t, state),

        TP_FIELDS(
                ctf_string(link_name, link_name)
                ctf_integer(uint32_t, state, state)
        )
)

TRACEPOINT_EVENT(
        com_ericsson_itclnh,
        link_disconnect_state,
        TP_ARGS(char *,  link_name,
                uint32_t, state),

        TP_FIELDS(
                ctf_string(link_name, link_name)
                ctf_integer(uint32_t, state, state)
        )
)

TRACEPOINT_EVENT(
        com_ericsson_itclnh,
        link_drop,
        TP_ARGS(char *,  link_name,
                uint32_t, state),

        TP_FIELDS(
                ctf_string(link_name, link_name)
                ctf_integer(uint32_t, state, state)
        )
)


TRACEPOINT_LOGLEVEL(com_ericsson_itclnh, rx_rlnh_init, TRACE_DEBUG)
TRACEPOINT_LOGLEVEL(com_ericsson_itclnh, shutdown_link, TRACE_DEBUG)
TRACEPOINT_LOGLEVEL(com_ericsson_itclnh, tx_rlnh_unpublish, TRACE_DEBUG)
TRACEPOINT_LOGLEVEL(com_ericsson_itclnh, rx_rlnh_unpublish, TRACE_DEBUG)
TRACEPOINT_LOGLEVEL(com_ericsson_itclnh, tx_rlnh_publish, TRACE_DEBUG)
TRACEPOINT_LOGLEVEL(com_ericsson_itclnh, rx_rlnh_publish, TRACE_DEBUG)
TRACEPOINT_LOGLEVEL(com_ericsson_itclnh, rx_rlnh_query, TRACE_DEBUG)
TRACEPOINT_LOGLEVEL(com_ericsson_itclnh, tx_rlnh_query, TRACE_DEBUG)
TRACEPOINT_LOGLEVEL(com_ericsson_itclnh, handle_locate, TRACE_DEBUG)
TRACEPOINT_LOGLEVEL(com_ericsson_itclnh, add_locate, TRACE_DEBUG)
TRACEPOINT_LOGLEVEL(com_ericsson_itclnh, remove_locate, TRACE_DEBUG)
TRACEPOINT_LOGLEVEL(com_ericsson_itclnh, lnh_shutdown_req, TRACE_DEBUG)
TRACEPOINT_LOGLEVEL(com_ericsson_itclnh, link_connect_state, TRACE_DEBUG)
TRACEPOINT_LOGLEVEL(com_ericsson_itclnh, link_disconnect_state, TRACE_DEBUG)
TRACEPOINT_LOGLEVEL(com_ericsson_itclnh, link_drop, TRACE_DEBUG)

#endif /* _ITCLNH_LTTNG_H */

#include <lttng/tracepoint-event.h>

#ifdef __cplusplus
}
#endif /* __cplusplus */
