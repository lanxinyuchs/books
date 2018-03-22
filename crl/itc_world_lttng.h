
#undef TRACEPOINT_PROVIDER
#define TRACEPOINT_PROVIDER com_ericsson_itc_world

#undef TRACEPOINT_INCLUDE_FILE
#define TRACEPOINT_INCLUDE_FILE ./itc_world_lttng.h

#ifdef __cplusplus
extern "C"{
#endif /* __cplusplus */


#if !defined(_ITC_LTTNG_H) || defined(TRACEPOINT_HEADER_MULTI_READ)
#define _ITC_WORLD_LTTNG_H

#include <lttng/tracepoint.h>
#include <stdint.h>

TRACEPOINT_EVENT(
    com_ericsson_itc_world,
    itc_world_ITC_LOCATE_WORLD, // Comment
    TP_ARGS(int, sd),

    /* Next are the fields */
    TP_FIELDS(
            ctf_integer(int, sd, sd)
    )
)

TRACEPOINT_EVENT(
    com_ericsson_itc_world,
    itc_world_ITC_LOCATE_WORLD_REPL, // Comment
    TP_ARGS(uint32_t, my_id,
            uint32_t, world_mask,
            uint32_t, world_mbox_id),

    /* Next are the fields */
    TP_FIELDS(
	ctf_integer_hex(uint32_t, my_id, my_id)
	ctf_integer_hex(uint32_t, world_mask, world_mask)
	ctf_integer_hex(uint32_t, world_mbox_id, world_mbox_id)
    )
)

TRACEPOINT_EVENT(
    com_ericsson_itc_world,
    itc_world_CONNECT_PROC, // Comment
    TP_ARGS(int, sd,
            uint32_t, proc_mbox_id),

    /* Next are the fields */
    TP_FIELDS(
        ctf_integer(int, sd, sd)
        ctf_integer_hex(uint32_t, proc_mbox_id, proc_mbox_id)
    )
)

TRACEPOINT_EVENT(
    com_ericsson_itc_world,
    itc_world_DISCONNECT_PROC, // Comment
    TP_ARGS(int, sd,
            uint32_t, proc_mbox_id),

    /* Next are the fields */
    TP_FIELDS(
        ctf_integer(int, sd, sd)
        ctf_integer_hex(uint32_t, proc_mbox_id, proc_mbox_id)
    )
)

TRACEPOINT_EVENT(
    com_ericsson_itc_world,
    itc_world_ADD_MBOX, // Comment
    TP_ARGS(uint32_t, msgno,
            uint32_t, mbox_id,
            char *, mbox_name),

    /* Next are the fields */
    TP_FIELDS(
	ctf_integer_hex(uint32_t, msgno, msgno)
	ctf_integer_hex(uint32_t, mbox_id, mbox_id)
        ctf_string(mbox_name, mbox_name)
    )
)

TRACEPOINT_EVENT(
    com_ericsson_itc_world,
    itc_world_REM_MBOX, // Comment
    TP_ARGS(uint32_t, msgno,
            uint32_t, mbox_id,
            char *, mbox_name),

    /* Next are the fields */
    TP_FIELDS(
	ctf_integer_hex(uint32_t, msgno, msgno)
	ctf_integer_hex(uint32_t, mbox_id, mbox_id)
        ctf_string(stringfield, mbox_name)
    )
)

TRACEPOINT_EVENT(
    com_ericsson_itc_world,
    itc_world_ITC_LOCATE, // Comment
    TP_ARGS(uint32_t, msgno,
            uint32_t, sender,
            uint32_t, from_mbox_id,
            char *, mbox_name),

    /* Next are the fields */
    TP_FIELDS(
	ctf_integer_hex(uint32_t, msgno, msgno)
	ctf_integer_hex(uint32_t, sender, sender)
	ctf_integer_hex(uint32_t, from_mbox_id, from_mbox_id)
        ctf_string(mbox_name, mbox_name)
    )
)

TRACEPOINT_EVENT(
    com_ericsson_itc_world,
    itc_world_ITC_LOCATE_REPL, // Comment
    TP_ARGS(uint32_t, msgno,
            uint32_t, mbox_id,
            int, transport,
            char *, mbox_name),

    /* Next are the fields */
    TP_FIELDS(
	ctf_integer_hex(uint32_t, msgno, msgno)
	ctf_integer_hex(uint32_t, mbox_id, mbox_id)
	ctf_integer_hex(uint32_t, transport, transport)
        ctf_string(mbox_name, mbox_name)
    )
)

TRACEPOINT_EVENT(
    com_ericsson_itc_world,
    itc_world_ITC_LOCATE_ASYNC, // Comment
    TP_ARGS(uint32_t, msgno,
            uint32_t, sender,
            uint32_t, from_mbox_id,
            unsigned long, data,
            char *, mbox_name),

    /* Next are the fields */
    TP_FIELDS(
	ctf_integer_hex(uint32_t, msgno, msgno)
	ctf_integer_hex(uint32_t, sender, sender)
	ctf_integer_hex(uint32_t, from_mbox_id, from_mbox_id)
	ctf_integer_hex(unsigned long, data, data)
        ctf_string(mbox_name, mbox_name)
    )
)

TRACEPOINT_EVENT(
    com_ericsson_itc_world,
    itc_world_ITC_LOCATE_ASYNC_REPL, // Comment
    TP_ARGS(uint32_t, msgno,
            uint32_t, from_mbox_id,
            unsigned long, data,
            uint32_t, mbox_id,
            int, transport,
            char *, mbox_name),

    /* Next are the fields */
    TP_FIELDS(
	ctf_integer_hex(uint32_t, msgno, msgno)
	ctf_integer_hex(uint32_t, from_mbox_id, from_mbox_id)
	ctf_integer_hex(unsigned long, data, data)
	ctf_integer_hex(uint32_t, mbox_id, mbox_id)
	ctf_integer(int, transport, transport)
        ctf_string(mbox_name, mbox_name)
    )
)

TRACEPOINT_EVENT(
    com_ericsson_itc_world,
    itc_error, // Comment
    TP_ARGS(char *, file,
            int, line,
            char *, err_msg),

    /* Next are the fields */
    TP_FIELDS(
        ctf_string(file, file)
	ctf_integer(int, line, line)
        ctf_string(err_msg, err_msg)
    )
)

TRACEPOINT_LOGLEVEL(com_ericsson_itc_world, itc_world_ITC_LOCATE_WORLD, TRACE_DEBUG)
TRACEPOINT_LOGLEVEL(com_ericsson_itc_world, itc_world_ITC_LOCATE_WORLD_REPL, TRACE_DEBUG)
TRACEPOINT_LOGLEVEL(com_ericsson_itc_world, itc_world_CONNECT_PROC, TRACE_DEBUG)
TRACEPOINT_LOGLEVEL(com_ericsson_itc_world, itc_world_DISCONNECT_PROC, TRACE_DEBUG)
TRACEPOINT_LOGLEVEL(com_ericsson_itc_world, itc_world_ADD_MBOX, TRACE_DEBUG)
TRACEPOINT_LOGLEVEL(com_ericsson_itc_world, itc_world_REM_MBOX, TRACE_DEBUG)
TRACEPOINT_LOGLEVEL(com_ericsson_itc_world, itc_world_ITC_LOCATE, TRACE_DEBUG)
TRACEPOINT_LOGLEVEL(com_ericsson_itc_world, itc_world_ITC_LOCATE_REPL, TRACE_DEBUG)
TRACEPOINT_LOGLEVEL(com_ericsson_itc_world, itc_world_ITC_LOCATE_ASYNC, TRACE_DEBUG)
TRACEPOINT_LOGLEVEL(com_ericsson_itc_world, itc_world_ITC_LOCATE_ASYNC_REPL, TRACE_DEBUG)

TRACEPOINT_LOGLEVEL(com_ericsson_itc_world, itc_error, TRACE_ERR)

#endif /* _ITC_WORLD_LTTNG_H */

#include <lttng/tracepoint-event.h>

#ifdef __cplusplus
}
#endif /* __cplusplus */
