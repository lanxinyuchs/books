
#undef TRACEPOINT_PROVIDER
#define TRACEPOINT_PROVIDER com_ericsson_ulh

#undef TRACEPOINT_INCLUDE_FILE
#define TRACEPOINT_INCLUDE_FILE ./ulh_lttng.h

#ifdef __cplusplus
extern "C"{
#endif /* __cplusplus */

#ifdef LTTNG
#if !defined(_ULH_EBCOM_LTTNG_H) || defined(TRACEPOINT_HEADER_MULTI_READ)
#define _ULH_EBCOM_LTTNG_H

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
        eth_vlan_hdr,
        TP_ARGS(char *,    text,
                uint8_t *, dmac,
                uint8_t *, smac,
                uint16_t,  vlantype,
                uint16_t,  vid,
                uint16_t,  ethtype),
        
        TP_FIELDS(
                ctf_string(text, text)
                ctf_array(uint8_t, dmac, dmac, 6)
                ctf_array(uint8_t, smac, smac, 6)
                ctf_integer_network_hex(uint16_t, vlantype, vlantype)
                ctf_integer_network_hex(uint16_t, vid, vid)
                ctf_integer_network_hex(uint16_t, ethtype, ethtype)
        )
)

TRACEPOINT_EVENT(
        com_ericsson_ulh,
        ebcom_cep,
        TP_ARGS(char *,   text,
                uint32_t, cep,
                uint32_t, qix),
        
        TP_FIELDS(
                ctf_string(text, text)
                ctf_integer(uint32_t, cep, cep)
                ctf_integer(uint8_t, qix, qix)
        )
)

TRACEPOINT_EVENT(
        com_ericsson_ulh,
        mhp_hdr,
        TP_ARGS(char *,   name,
                char *,   prfx,
                uint32_t, len,
                char *,   typename,
                uint8_t,  type,
                uint8_t,  prio,
                uint16_t, ns,
                uint16_t, nr),
        
        TP_FIELDS(
                ctf_string(name, name)
                ctf_string(prfx, prfx)
                ctf_integer(uint32_t, len, len)
                ctf_string(typename, typename)
                ctf_integer(uint8_t, type, type)
                ctf_integer(uint8_t, prio, prio)
                ctf_integer_network_hex(uint16_t, ns, ns)
                ctf_integer_network_hex(uint16_t, nr, nr)
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
        ulh_info, // Comment
        TP_ARGS(char *, file,
                int,    line,
                char *, info_msg),
        
        /* Next are the fields */
        TP_FIELDS(
                ctf_string(file, file)
                ctf_integer(int, line, line)
                ctf_string(info_msg, info_msg)
        )
)

TRACEPOINT_EVENT(
        com_ericsson_ulh,
        ulh_error, // Comment
        TP_ARGS(char *, file,
                int,    line,
                char *, err_msg),
        
        /* Next are the fields */
        TP_FIELDS(
                ctf_string(file, file)
                ctf_integer(int, line, line)
                ctf_string(err_msg, err_msg)
        )
)



TRACEPOINT_LOGLEVEL(com_ericsson_ulh, eth_hdr,        TRACE_DEBUG)
TRACEPOINT_LOGLEVEL(com_ericsson_ulh, eth_vlan_hdr,   TRACE_DEBUG)
TRACEPOINT_LOGLEVEL(com_ericsson_ulh, ebcom_cep,      TRACE_DEBUG)
TRACEPOINT_LOGLEVEL(com_ericsson_ulh, mhp_hdr,        TRACE_DEBUG)
TRACEPOINT_LOGLEVEL(com_ericsson_ulh, mhp_connect,    TRACE_DEBUG)
TRACEPOINT_LOGLEVEL(com_ericsson_ulh, mhp_disconnect, TRACE_DEBUG)
TRACEPOINT_LOGLEVEL(com_ericsson_ulh, mhp_data,       TRACE_DEBUG)
TRACEPOINT_LOGLEVEL(com_ericsson_ulh, ulh_info,       TRACE_INFO)
TRACEPOINT_LOGLEVEL(com_ericsson_ulh, ulh_error,      TRACE_ERR)

#endif /* _ULH_EBCOM_LTTNG_H */

#include <lttng/tracepoint-event.h>

#ifdef __cplusplus
}
#endif /* __cplusplus */
