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
#define TRACEPOINT_PROVIDER com_ericsson_hdlc

#undef TRACEPOINT_INCLUDE_FILE
#define TRACEPOINT_INCLUDE_FILE ./hdlc_lttng.h

#ifdef __cplusplus
extern "C"{
#endif /* __cplusplus */

#ifdef LTTNG
#if !defined(_HDLC_LTTNG_H_) || defined(TRACEPOINT_HEADER_MULTI_READ)
#define _HDLC_LTTNG_H_

#include "lttng/tracepoint.h"
#include <stdint.h>


TRACEPOINT_EVENT(
        com_ericsson_hdlc,
        rlnh_init,
        TP_ARGS(char *,   linkname,
                char *,   tag,
                uint16_t, reserved,
                uint16_t, type,
                uint32_t, version),
        
        TP_FIELDS(
                ctf_string(linkname, linkname)
                ctf_string(tag, tag)
                ctf_integer_network_hex(uint16_t, reserved, reserved)
                ctf_integer_network_hex(uint16_t, type, type)
                ctf_integer_network_hex(uint32_t, version, version)
        )
)

TRACEPOINT_EVENT(
        com_ericsson_hdlc,
        rlnh_init_reply,
        TP_ARGS(char *,   linkname,
                char *,   tag,
                uint16_t, reserved,
                uint16_t, type,
                uint32_t, status,
                char *, features),
        
        TP_FIELDS(
                ctf_string(linkname, linkname)
                ctf_string(tag, tag)
                ctf_integer_network_hex(uint16_t, reserved, reserved)
                ctf_integer_network_hex(uint16_t, type, type)
                ctf_integer_network_hex(uint32_t, status, status)
                ctf_string(features, features)
        )
)

TRACEPOINT_EVENT(
        com_ericsson_hdlc,
        rlnh_publish,
        TP_ARGS(char *,   linkname,
                char *,   tag,
                uint16_t, reserved,
                uint16_t, type,
                uint32_t, laddr,
                char *, name),
        
        TP_FIELDS(
                ctf_string(linkname, linkname)
                ctf_string(tag, tag)
                ctf_integer_network_hex(uint16_t, reserved, reserved)
                ctf_integer_network_hex(uint16_t, type, type)
                ctf_integer_network_hex(uint32_t, laddr, laddr)
                ctf_string(name, name)
        )
)

TRACEPOINT_EVENT(
        com_ericsson_hdlc,
        rlnh_unpublish,
        TP_ARGS(char *,   linkname,
                char *,   tag,
                uint16_t, reserved,
                uint16_t, type,
                uint32_t, laddr),
        
        TP_FIELDS(
                ctf_string(linkname, linkname)
                ctf_string(tag, tag)
                ctf_integer_network_hex(uint16_t, reserved, reserved)
                ctf_integer_network_hex(uint16_t, type, type)
                ctf_integer_network_hex(uint32_t, laddr, laddr)
        )
)

TRACEPOINT_EVENT(
        com_ericsson_hdlc,
        rlnh_unpublish_ack,
        TP_ARGS(char *,   linkname,
                char *,   tag,
                uint16_t, reserved,
                uint16_t, type,
                uint32_t, laddr),
        
        TP_FIELDS(
                ctf_string(linkname, linkname)
                ctf_string(tag, tag)
                ctf_integer_network_hex(uint16_t, reserved, reserved)
                ctf_integer_network_hex(uint16_t, type, type)
                ctf_integer_network_hex(uint32_t, laddr, laddr)
        )
)

TRACEPOINT_EVENT(
        com_ericsson_hdlc,
        rlnh_query,
        TP_ARGS(char *,   linkname,
                char *,   tag,
                uint16_t, reserved,
                uint16_t, type,
                uint32_t, laddr,
                char *,   name),
        
        TP_FIELDS(
                ctf_string(linkname, linkname)
                ctf_string(tag, tag)
                ctf_integer_network_hex(uint16_t, reserved, reserved)
                ctf_integer_network_hex(uint16_t, type, type)
                ctf_integer_network_hex(uint32_t, laddr, laddr)
                ctf_string(name, name)
        )
)

TRACEPOINT_EVENT(
        com_ericsson_hdlc,
        rlnh_data,
        TP_ARGS(char *,   linkname,
                char *,   tag,
                uint32_t, src,
                uint32_t, dst,
                uint32_t, size,
                uint32_t, signo),
        
        TP_FIELDS(
                ctf_string(linkname, linkname)
                ctf_string(tag, tag)
                ctf_integer_hex(uint32_t, src, src)
                ctf_integer_hex(uint32_t, dst, dst)
                ctf_integer_hex(uint32_t, size, size)
                ctf_integer_network_hex(uint32_t, signo, signo)
        )
)

TRACEPOINT_EVENT(
        com_ericsson_hdlc,
        hdlc_data, // Comment
        TP_ARGS(char *, info),
        
        /* Next are the fields */
        TP_FIELDS(
                ctf_string(info, info)
        )
)

TRACEPOINT_EVENT(
        com_ericsson_hdlc,
        hdlc_dbg, // Comment
        TP_ARGS(int,    line,
                char *, info),
        
        /* Next are the fields */
        TP_FIELDS(
                ctf_integer(int, line, line)
                ctf_string(info, info)
        )
)

TRACEPOINT_EVENT(
        com_ericsson_hdlc,
        hdlc_info, // Comment
        TP_ARGS(int,    line,
                char *, info),
        
        /* Next are the fields */
        TP_FIELDS(
                ctf_integer(int, line, line)
                ctf_string(info, info)
        )
)

TRACEPOINT_EVENT(
        com_ericsson_hdlc,
        hdlc_error, // Comment
        TP_ARGS(int,    line,
                char *, error),
        
        /* Next are the fields */
        TP_FIELDS(
                ctf_integer(int, line, line)
                ctf_string(error, error)
        )
)



TRACEPOINT_LOGLEVEL(com_ericsson_hdlc, hdlc_hdr,        TRACE_DEBUG)
TRACEPOINT_LOGLEVEL(com_ericsson_hdlc, hdlc_connect,    TRACE_DEBUG)
TRACEPOINT_LOGLEVEL(com_ericsson_hdlc, hdlc_disconnect, TRACE_DEBUG)
TRACEPOINT_LOGLEVEL(com_ericsson_hdlc, hdlc_data,       TRACE_DEBUG)
TRACEPOINT_LOGLEVEL(com_ericsson_hdlc, hdlc_dbg,        TRACE_DEBUG)
TRACEPOINT_LOGLEVEL(com_ericsson_hdlc, hdlc_info,       TRACE_INFO)
TRACEPOINT_LOGLEVEL(com_ericsson_hdlc, hdlc_error,      TRACE_ERR)

#include <lttng/tracepoint-event.h>

#endif /* _HDLC_LTTNG_H_ */
#endif /* LTTNG */

#ifdef __cplusplus
}
#endif /* __cplusplus */
