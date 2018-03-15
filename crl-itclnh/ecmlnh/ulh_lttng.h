
#undef TRACEPOINT_PROVIDER
#define TRACEPOINT_PROVIDER com_ericsson_ulh_ecm

#undef TRACEPOINT_INCLUDE_FILE
#define TRACEPOINT_INCLUDE_FILE ./ulh_lttng.h

#ifdef __cplusplus
extern "C"{
#endif /* __cplusplus */

#ifdef LTTNG
#if !defined(_ULH_ECMT_LTTNG_H) || defined(TRACEPOINT_HEADER_MULTI_READ)
#define _ULH_ECMT_LTTNG_H

#include "lttng/tracepoint.h"
#endif

#include <stdint.h>

TRACEPOINT_EVENT(
        com_ericsson_ulh_ecm,
        eth_hdr,
        TP_ARGS(char *,    text,
		char *,    devname,
                uint8_t *, dmac,
                uint8_t *, smac,
                uint16_t,  ethtype),

        TP_FIELDS(
                ctf_string(text, text)
                ctf_string(devname, devname)
                ctf_array(uint8_t, dmac, dmac, 6)
                ctf_array(uint8_t, smac, smac, 6)
                ctf_integer_network_hex(uint16_t, ethtype, ethtype)
        )
)

TRACEPOINT_EVENT(
        com_ericsson_ulh_ecm,
        eth_vlan_hdr,
        TP_ARGS(char *,    text,
		char *,    devname,
                uint8_t *, dmac,
                uint8_t *, smac,
                uint16_t,  vlantype,
                uint16_t,  vid,
                uint16_t,  ethtype),

        TP_FIELDS(
                ctf_string(text, text)
                ctf_string(devname, devname)
                ctf_array(uint8_t, dmac, dmac, 6)
                ctf_array(uint8_t, smac, smac, 6)
                ctf_integer_network_hex(uint16_t, vlantype, vlantype)
                ctf_integer_network_hex(uint16_t, vid, vid)
                ctf_integer_network_hex(uint16_t, ethtype, ethtype)
        )
)

TRACEPOINT_EVENT(
        com_ericsson_ulh_ecm,
        ecmt_packet_not_found,
        TP_ARGS(char *,    device,
                uint8_t *, mac,
                uint16_t,  vid),

        TP_FIELDS(
                ctf_string(device, device)
                ctf_array(uint8_t, mac, mac, 6)
                ctf_integer_network_hex(uint16_t, vid, vid)
        )
)

TRACEPOINT_EVENT(
        com_ericsson_ulh_ecm,
        ecm_main,
        TP_ARGS(char *,   name,
		char *,   prfx,
                uint8_t,  next,
                uint8_t,  ver,
                uint8_t,  connid,
		uint16_t, pktsize),

        TP_FIELDS(
                ctf_string(name, name)
                ctf_string(prfx, prfx)
                ctf_integer_hex(uint8_t,  next,    next)
                ctf_integer_hex(uint8_t,  ver,     ver)
                ctf_integer_hex(uint8_t,  connid,  connid)
                ctf_integer_hex(uint16_t, pktsize, pktsize)
        )
)

TRACEPOINT_EVENT(
        com_ericsson_ulh_ecm,
        ecm_conn,
        TP_ARGS(uint8_t,   next,
                uint8_t,   type,
                uint8_t,   size,
                uint8_t,   win,
                uint8_t *, dmac,
                uint8_t *, smac,
		char *,    features),

        TP_FIELDS(
                ctf_integer_hex(uint8_t, next, next)
                ctf_integer_hex(uint8_t, type, type)
                ctf_integer_hex(uint8_t, size, size)
                ctf_integer_hex(uint8_t, win,  win)
                ctf_array(uint8_t, dmac, dmac, 6)
                ctf_array(uint8_t, smac, smac, 6)
                ctf_string(features, features)
        )
)

TRACEPOINT_EVENT(
        com_ericsson_ulh_ecm,
        ecm_udata,
        TP_ARGS(uint8_t,  next,
                uint8_t,  o,
                uint8_t,  m,
                uint16_t, fragno,
                uint32_t, dst,
                uint32_t, src),

        TP_FIELDS(
                ctf_integer_hex(uint8_t,  next,   next)
                ctf_integer_hex(uint8_t,  o,      o)
                ctf_integer_hex(uint8_t,  m,      m)
                ctf_integer_hex(uint16_t, fragno, fragno)
                ctf_integer_network_hex(uint32_t, dst, dst)
                ctf_integer_network_hex(uint32_t, src, src)
        )
)

TRACEPOINT_EVENT(
        com_ericsson_ulh_ecm,
        ecm_frag,
        TP_ARGS(uint8_t,  next,
                uint8_t,  m,
                uint16_t, fragno),

        TP_FIELDS(
                ctf_integer_hex(uint8_t,  next,   next)
                ctf_integer_hex(uint8_t,  m,      m)
                ctf_integer_hex(uint16_t, fragno, fragno)
        )
)

TRACEPOINT_EVENT(
        com_ericsson_ulh_ecm,
        ecm_ack,
        TP_ARGS(uint8_t,  next,
                uint8_t,  r,
                uint16_t, ackno,
		uint16_t, seqno),

        TP_FIELDS(
                ctf_integer_hex(uint8_t,  next,  next)
                ctf_integer_hex(uint8_t,  r,     r)
                ctf_integer_hex(uint16_t, ackno, ackno)
                ctf_integer_hex(uint16_t, seqno, seqno)
        )
)

TRACEPOINT_EVENT(
        com_ericsson_ulh_ecm,
        ecm_nack,
        TP_ARGS(uint8_t,  next,
                uint8_t,  count,
		uint16_t, seqno),

        TP_FIELDS(
                ctf_integer_hex(uint8_t,  next,  next)
                ctf_integer_hex(uint8_t,  count, count)
                ctf_integer_hex(uint16_t, seqno, seqno)
        )
)

TRACEPOINT_EVENT(
        com_ericsson_ulh_ecm,
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
        com_ericsson_ulh_ecm,
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
        com_ericsson_ulh_ecm,
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
        com_ericsson_ulh_ecm,
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
        com_ericsson_ulh_ecm,
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
        com_ericsson_ulh_ecm,
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
        com_ericsson_ulh_ecm,
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
        com_ericsson_ulh_ecm,
        ecmt_rx_ctrlmsg,
        TP_ARGS(char *,    device,
                int,       add,
                uint8_t *, dmac,
                uint16_t,  vid,
		uint32_t,  cid),

        TP_FIELDS(
                ctf_string(device, device)
                ctf_integer(int, add, add)
                ctf_array(uint8_t, dmac, dmac, 6)
                ctf_integer_network_hex(uint16_t, vid, vid)
                ctf_integer(uint32_t, cid, cid)
        )
)

TRACEPOINT_EVENT(
        com_ericsson_ulh_ecm,
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
        com_ericsson_ulh_ecm,
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

TRACEPOINT_EVENT(
        com_ericsson_ulh_ecm,
        ulh_trace_sig, // Comment
        TP_ARGS(char *, file,
                int,    line,
                uint32_t, msg_no,
                char*, function),

        /* Next are the fields */
        TP_FIELDS(
                ctf_string(file, file)
                ctf_integer(int, line, line)
                ctf_integer(uint32_t, msg_no, msg_no)
                ctf_string(function, function)
        )
)

TRACEPOINT_LOGLEVEL(com_ericsson_ulh_ecm, eth_hdr,               TRACE_DEBUG)
TRACEPOINT_LOGLEVEL(com_ericsson_ulh_ecm, eth_vlan_hdr,          TRACE_DEBUG)
TRACEPOINT_LOGLEVEL(com_ericsson_ulh_ecm, ecmt_packet_not_found, TRACE_DEBUG)
TRACEPOINT_LOGLEVEL(com_ericsson_ulh_ecm, mhp_hdr,               TRACE_DEBUG)
TRACEPOINT_LOGLEVEL(com_ericsson_ulh_ecm, mhp_connect,           TRACE_DEBUG)
TRACEPOINT_LOGLEVEL(com_ericsson_ulh_ecm, mhp_disconnect,        TRACE_DEBUG)
TRACEPOINT_LOGLEVEL(com_ericsson_ulh_ecm, mhp_data,              TRACE_DEBUG)
TRACEPOINT_LOGLEVEL(com_ericsson_ulh_ecm, ulh_info,              TRACE_DEBUG)
TRACEPOINT_LOGLEVEL(com_ericsson_ulh_ecm, ulh_error,             TRACE_ERR)

#endif /* _ULH_ECMT_LTTNG_H */

#include <lttng/tracepoint-event.h>

#ifdef __cplusplus
}
#endif /* __cplusplus */
