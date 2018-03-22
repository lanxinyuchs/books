
#undef TRACEPOINT_PROVIDER
#define TRACEPOINT_PROVIDER com_ericsson_itc_sysv

#undef TRACEPOINT_INCLUDE_FILE
#define TRACEPOINT_INCLUDE_FILE ./itc_sysv_lttng.h

#ifdef __cplusplus
extern "C"{
#endif /* __cplusplus */


#if !defined(_ITC_LTTNG_H) || defined(TRACEPOINT_HEADER_MULTI_READ)
#define _ITC_LTTNG_H

#include <lttng/tracepoint.h>
#include <stdint.h>

TRACEPOINT_EVENT(
    com_ericsson_itc_sysv,
    itc_sysv_send, // Comment
    TP_ARGS(int, mqid,
            long, pid,
            uint32_t, from_mbox_id,
            int, size,
            unsigned long, addr,
            uint32_t, to_mbox_id),

    /* Next are the fields */
    TP_FIELDS(
	ctf_integer_hex(int, mqid, mqid)
	ctf_integer_hex(long, pid, pid)
	ctf_integer_hex(uint32_t, from_mbox_id, from_mbox_id)
	ctf_integer_hex(int, size, size)
	ctf_integer_hex(unsigned long, addr, addr)
	ctf_integer_hex(uint32_t, to_mbox_id, to_mbox_id)
    )
)

TRACEPOINT_EVENT(
    com_ericsson_itc_sysv,
    itc_sysv_send_free, // Comment
    TP_ARGS(int, mqid,
            uint32_t, my_mbox_id,
            unsigned long, addr,
            uint32_t, to_mbox_id),

    /* Next are the fields */
    TP_FIELDS(
	ctf_integer_hex(int, mqid, mqid)
	ctf_integer_hex(uint32_t, my_mbox_id, my_mbox_id)
	ctf_integer_hex(unsigned long, addr, addr)
	ctf_integer_hex(uint32_t, to_mbox_id, to_mbox_id)
    )
)

TRACEPOINT_EVENT(
    com_ericsson_itc_sysv,
    itc_sysv_deliver_packet, // Comment
    TP_ARGS(int, mqid,
            uint32_t, my_mbox_id,
            long, pid,
            uint32_t, from_mbox_id,
            int, size,
            unsigned long, addr),

    /* Next are the fields */
    TP_FIELDS(
	ctf_integer_hex(int, mqid, mqid)
	ctf_integer_hex(uint32_t, my_mbox_id, my_mbox_id)
	ctf_integer_hex(long, pid, pid)
	ctf_integer_hex(uint32_t, from_mbox_id, from_mbox_id)
	ctf_integer_hex(int, size, size)
	ctf_integer_hex(unsigned long, addr, addr)
    )
)

TRACEPOINT_LOGLEVEL(com_ericsson_itc_sysv, itc_sysv_send, TRACE_DEBUG)
TRACEPOINT_LOGLEVEL(com_ericsson_itc_sysv, itc_sysv_send_free, TRACE_DEBUG)
TRACEPOINT_LOGLEVEL(com_ericsson_itc_sysv, itc_sysv_deliver_packet, TRACE_DEBUG)

#endif /* _ITC_LTTNG_H */

#include <lttng/tracepoint-event.h>

#ifdef __cplusplus
}
#endif /* __cplusplus */

