
#undef TRACEPOINT_PROVIDER
#define TRACEPOINT_PROVIDER com_ericsson_mdu

#undef TRACEPOINT_INCLUDE_FILE
#define TRACEPOINT_INCLUDE_FILE ./mdu_lttng.h

#ifdef __cplusplus
extern "C"{
#endif /* __cplusplus */

#ifdef LTTNG
#if !defined(_MDU_LTTNG_H_) || defined(TRACEPOINT_HEADER_MULTI_READ)
#define _MDU_LTTNG_H_

#include "lttng/tracepoint.h"
#endif

#include <stdint.h>

TRACEPOINT_EVENT(
        com_ericsson_mdu,
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
        com_ericsson_mdu,
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
        com_ericsson_mdu,
        mhp_connect,
        TP_ARGS(uint16_t, lconnid,
                uint16_t, rconnid),
        
        TP_FIELDS(
                ctf_integer_network_hex(uint16_t, lconnid, lconnid)
                ctf_integer_network_hex(uint16_t, rconnid, rconnid)
        )
)

TRACEPOINT_EVENT(
        com_ericsson_mdu,
        mhp_disconnect,
        TP_ARGS(uint16_t, lconnid,
                uint16_t, rconnid),
        
        TP_FIELDS(
                ctf_integer_network_hex(uint16_t, lconnid, lconnid)
                ctf_integer_network_hex(uint16_t, rconnid, rconnid)
        )
)

TRACEPOINT_EVENT(
        com_ericsson_mdu,
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
        com_ericsson_mdu,
        mdu_info, // Comment
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
        com_ericsson_mdu,
        mdu_error, // Comment
        TP_ARGS(char *, file,
                int,    line,
                char *, err_msg),
        
        /* Next are the fields */
        TP_FIELDS(
                ctf_string(file, file)
                ctf_integer(int, line, line)
                ctf_string(error, err_msg)
        )
)



TRACEPOINT_LOGLEVEL(com_ericsson_mdu, eth_hdr,        TRACE_DEBUG)
TRACEPOINT_LOGLEVEL(com_ericsson_mdu, eth_vlan_hdr,   TRACE_DEBUG)
TRACEPOINT_LOGLEVEL(com_ericsson_mdu, ebcom_cep,      TRACE_DEBUG)
TRACEPOINT_LOGLEVEL(com_ericsson_mdu, mhp_hdr,        TRACE_DEBUG)
TRACEPOINT_LOGLEVEL(com_ericsson_mdu, mhp_connect,    TRACE_DEBUG)
TRACEPOINT_LOGLEVEL(com_ericsson_mdu, mhp_disconnect, TRACE_DEBUG)
TRACEPOINT_LOGLEVEL(com_ericsson_mdu, mhp_data,       TRACE_DEBUG)
TRACEPOINT_LOGLEVEL(com_ericsson_mdu, mdu_info,       TRACE_INFO)
TRACEPOINT_LOGLEVEL(com_ericsson_mdu, mdu_error,      TRACE_ERR)

#endif /* _MDU_EBCOM_LTTNG_H */

#include <lttng/tracepoint-event.h>

#ifdef __cplusplus
}
#endif /* __cplusplus */
