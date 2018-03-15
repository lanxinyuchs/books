
#undef TRACEPOINT_PROVIDER
#define TRACEPOINT_PROVIDER com_ericsson_ulh

#undef TRACEPOINT_INCLUDE_FILE
#define TRACEPOINT_INCLUDE_FILE ./lh_lttng.h

#ifdef __cplusplus
extern "C"{
#endif /* __cplusplus */

#ifdef LTTNG
#if !defined(_MHP_LTTNG_H_) || defined(TRACEPOINT_HEADER_MULTI_READ)
#define _MHP_LTTNG_H_

#include "lttng/tracepoint.h"
#endif

#include <stdint.h>

TRACEPOINT_EVENT(
        com_ericsson_ulh,
        eth_hdr,
        TP_ARGS(char *,    text,
                uint8_t *, dmac,
                uint8_t *, smac,
                uint16_t,  ethtype),
        
        TP_FIELDS(
                ctf_string(text, text)
                ctf_array(uint8_t, dmac, dmac, 6)
                ctf_array(uint8_t, smac, smac, 6)
                ctf_integer_network_hex(uint16_t, ethtype, ethtype)
        )
)

TRACEPOINT_EVENT(
        com_ericsson_ulh,
        mhp_hdr,
        TP_ARGS(char *,   name,
                char *,   msg,
                uint32_t, len,
                char *,   type,
                uint16_t, ns,
                uint16_t, nr),
        
        TP_FIELDS(
                ctf_string(name, name)
                ctf_string(msg, msg)
                ctf_integer(uint32_t, len, len)
                ctf_string(typ, type)
                ctf_integer_network(uint16_t, ns, ns)
                ctf_integer_network(uint16_t, nr, nr)
        )
)

TRACEPOINT_EVENT(
        com_ericsson_ulh,
        mhp_connect,
        TP_ARGS(uint16_t, lconnid,
                uint16_t, rconnid),
        
        TP_FIELDS(
                ctf_integer_network_hex(uint16_t, lconnid, lconnid)
                ctf_integer_network_hex(uint16_t, rconnid, rconnid)
        )
)

TRACEPOINT_EVENT(
        com_ericsson_ulh,
        mhp_disconnect,
        TP_ARGS(uint16_t, lconnid,
                uint16_t, rconnid),
        
        TP_FIELDS(
                ctf_integer_network_hex(uint16_t, lconnid, lconnid)
                ctf_integer_network_hex(uint16_t, rconnid, rconnid)
        )
)

TRACEPOINT_EVENT(
        com_ericsson_ulh,
        mhp_data,
        TP_ARGS(uint16_t, size,
                uint16_t, frag,
                uint32_t, src,
                uint32_t, dst,
                uint32_t, extra),
        
        TP_FIELDS(
                ctf_integer_network_hex(uint16_t, size, size)
                ctf_integer_network_hex(uint16_t, frag, frag)
                ctf_integer_network_hex(uint32_t, src, src)
                ctf_integer_network_hex(uint32_t, dst, dst)
                ctf_integer_network_hex(uint32_t, extra, extra)
        )
)


TRACEPOINT_EVENT(
        com_ericsson_ulh,
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
        com_ericsson_ulh,
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
        com_ericsson_ulh,
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
        com_ericsson_ulh,
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
        com_ericsson_ulh,
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
        com_ericsson_ulh,
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
        com_ericsson_ulh,
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
        com_ericsson_ulh,
        mhp_info, // Comment
        TP_ARGS(char *, file,
                int,    line,
                char *, info),
        
        /* Next are the fields */
        TP_FIELDS(
                ctf_string(file, file)
                ctf_integer(int, line, line)
                ctf_string(info, info)
        )
)

TRACEPOINT_EVENT(
        com_ericsson_ulh,
        mhp_error, // Comment
        TP_ARGS(char *, file,
                int,    line,
                char *, error),
        
        /* Next are the fields */
        TP_FIELDS(
                ctf_string(file, file)
                ctf_integer(int, line, line)
                ctf_string(error, error)
        )
)

TRACEPOINT_EVENT(
        com_ericsson_ulh,
        mhp_dbg, // Comment
        TP_ARGS(char *, file,
                int,    line,
                char *, dbg),
        
        /* Next are the fields */
        TP_FIELDS(
                ctf_string(file, file)
                ctf_integer(int, line, line)
                ctf_string(dbg, dbg)
        )
)


TRACEPOINT_LOGLEVEL(com_ericsson_ulh, mhp_hdr,        TRACE_DEBUG)
TRACEPOINT_LOGLEVEL(com_ericsson_ulh, mhp_connect,    TRACE_DEBUG)
TRACEPOINT_LOGLEVEL(com_ericsson_ulh, mhp_disconnect, TRACE_DEBUG)
TRACEPOINT_LOGLEVEL(com_ericsson_ulh, mhp_data,       TRACE_DEBUG)
TRACEPOINT_LOGLEVEL(com_ericsson_ulh, mhp_dbg,        TRACE_DEBUG)
TRACEPOINT_LOGLEVEL(com_ericsson_ulh, mhp_info,       TRACE_INFO)
TRACEPOINT_LOGLEVEL(com_ericsson_ulh, mhp_error,      TRACE_ERR)

#endif /* _MHP_EBCOM_LTTNG_H */

#include <lttng/tracepoint-event.h>

#ifdef __cplusplus
}
#endif /* __cplusplus */
