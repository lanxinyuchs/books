
#undef TRACEPOINT_PROVIDER
#define TRACEPOINT_PROVIDER com_ericsson_itc_if

#undef TRACEPOINT_INCLUDE_FILE
#define TRACEPOINT_INCLUDE_FILE ./itc_lttng.h

#ifdef __cplusplus
extern "C"{
#endif /* __cplusplus */


#if !defined(_ITC_LTTNG_H) || defined(TRACEPOINT_HEADER_MULTI_READ)
#define _ITC_LTTNG_H

#include <lttng/tracepoint.h>

#include <stdint.h>

TRACEPOINT_EVENT(
    com_ericsson_itc_if,
    itc_init, // Comment
    TP_ARGS(uint32_t, own_mbox_id,
            int, mailbox_count,
            int, alloc_scheme,
            uint32_t, flags),

    /* Next are the fields */
    TP_FIELDS(
	ctf_integer_hex(uint32_t, own_mbox_id, own_mbox_id)
	ctf_integer(int, mailbox_count, mailbox_count)
	ctf_integer(int, alloc_scheme, alloc_scheme)
	ctf_integer_hex(uint32_t, flags, flags)
    )
)

TRACEPOINT_EVENT(
    com_ericsson_itc_if,
    itc_init_wname, // Comment
    TP_ARGS(uint32_t, own_mbox_id,
            int, mailbox_count,
            int, alloc_scheme,
            char *, namespace,
            uint32_t, flags),

    /* Next are the fields */
    TP_FIELDS(
	ctf_integer_hex(uint32_t, own_mbox_id, own_mbox_id)
	ctf_integer(int, mailbox_count, mailbox_count)
	ctf_integer(int, alloc_scheme, alloc_scheme)
        ctf_string(namespace, namespace)
	ctf_integer_hex(uint32_t, flags, flags)
    )
)

TRACEPOINT_EVENT(
    com_ericsson_itc_if,
    itc_exit, // Comment
    TP_ARGS(uint32_t, own_mbox_id,
            int, dummy),

    /* Next are the fields */
    TP_FIELDS(
	ctf_integer_hex(uint32_t, own_mbox_id, own_mbox_id)
    )
)

TRACEPOINT_EVENT(
    com_ericsson_itc_if,
    itc_alloc, // Comment
    TP_ARGS(uint32_t, own_mbox_id,
            size_t, size,
	    int, msg_no,
	    long, retmsg),

    /* Next  are the fields */
    TP_FIELDS(
	ctf_integer_hex(uint32_t, own_mbox_id, own_mbox_id)
	ctf_integer(size_t, size, size)
	ctf_integer_hex(uint32_t, msg_no, msg_no)
	ctf_integer_hex(uint32_t, msg_no_hex, msg_no)
	ctf_integer_hex(unsigned long, retmsg, retmsg)
    )
)

TRACEPOINT_EVENT(
    com_ericsson_itc_if,
    itc_free, // Comment
    TP_ARGS(uint32_t, own_mbox_id,
            unsigned long, msg),

    /* Next are the fields */
    TP_FIELDS(
	ctf_integer_hex(uint32_t, own_mbox_id, own_mbox_id)
	ctf_integer_hex(unsigned long, msg, msg)
    )
)

TRACEPOINT_EVENT(
    com_ericsson_itc_if,
    itc_sender, // Comment
    TP_ARGS(uint32_t, own_mbox_id,
            unsigned long, msg,
	    uint32_t, sender),

    /* Next are the fields */
    TP_FIELDS(
	ctf_integer_hex(uint32_t, own_mbox_id, own_mbox_id)
	ctf_integer_hex(unsigned long, msg, msg)
	ctf_integer_hex(uint32_t, sender, sender)
    )
)

TRACEPOINT_EVENT(
    com_ericsson_itc_if,
    itc_receiver, // Comment
    TP_ARGS(uint32_t, own_mbox_id,
            unsigned long, msg,
	    uint32_t, receiver),

    /* Next are the fields */
    TP_FIELDS(
	ctf_integer_hex(uint32_t, own_mbox_id, own_mbox_id)
	ctf_integer_hex(unsigned long, msg, msg)
	ctf_integer_hex(uint32_t, receiver, receiver)
    )
)

TRACEPOINT_EVENT(
    com_ericsson_itc_if,
    itc_size, // Comment
    TP_ARGS(uint32_t, own_mbox_id,
            unsigned long, msg,
	    size_t, size),

    /* Next are the fields */
    TP_FIELDS(
	ctf_integer_hex(uint32_t, own_mbox_id, own_mbox_id)
	ctf_integer_hex(unsigned long, msg, msg)
	ctf_integer_hex(size_t, size, size)
    )
)

TRACEPOINT_EVENT(
    com_ericsson_itc_if,
    itc_create_mailbox_enter, // Comment
    TP_ARGS(uint32_t, own_mbox_id,
            const char *, name,
	    uint32_t, flags),

    /* Next are the fields */
    TP_FIELDS(
	ctf_integer_hex(uint32_t, own_mbox_id, own_mbox_id)
        ctf_string(name, name)
	ctf_integer_hex(uint32_t, flags, flags)
    )
)

TRACEPOINT_EVENT(
    com_ericsson_itc_if,
    itc_create_mailbox_exit,
    TP_ARGS(uint32_t, own_mbox_id,
            const char *, name,
	    uint32_t, flags,
	    uint32_t, mbox_id),

    /* Next are the fields */
    TP_FIELDS(
	ctf_integer_hex(uint32_t, own_mbox_id, own_mbox_id)
        ctf_string(name, name)
	ctf_integer_hex(uint32_t, flags, flags)
	ctf_integer_hex(uint32_t, mbox_id, mbox_id)
    )
)

TRACEPOINT_EVENT(
    com_ericsson_itc_if,
    itc_clone_mailbox, // Comment
    TP_ARGS(uint32_t, own_mbox_id,
            const char *, name,
	    uint32_t, clone_mbox_id),

    /* Next are the fields */
    TP_FIELDS(
	ctf_integer_hex(uint32_t, own_mbox_id, own_mbox_id)
        ctf_string(name, name)
	ctf_integer_hex(uint32_t, clone_mbox_id, clone_mbox_id)
    )
)

TRACEPOINT_EVENT(
    com_ericsson_itc_if,
    itc_delete_mailbox, // Comment
    TP_ARGS(uint32_t, own_mbox_id,
            uint32_t, mbox_id),

    /* Next are the fields */
    TP_FIELDS(
	ctf_integer_hex(uint32_t, own_mbox_id, own_mbox_id)
	ctf_integer_hex(uint32_t, mbox_id, mbox_id)
    )
)

TRACEPOINT_EVENT(
    com_ericsson_itc_if,
    itc_add_name, // Comment
    TP_ARGS(uint32_t, own_mbox_id,
            uint32_t, mbox_id,
	    const char *, name),

    /* Next are the fields */
    TP_FIELDS(
	ctf_integer_hex(uint32_t, own_mbox_id, own_mbox_id)
	ctf_integer_hex(uint32_t, mbox_id, mbox_id)
        ctf_string(name, name)
    )
)

TRACEPOINT_EVENT(
    com_ericsson_itc_if,
    itc_locate, // Comment
    TP_ARGS(uint32_t, own_mbox_id,
            const char *, name,
	    uint32_t, mbox_id),

    /* Next are the fields */
    TP_FIELDS(
	ctf_integer_hex(uint32_t, own_mbox_id, own_mbox_id)
        ctf_string(name, name)
	ctf_integer_hex(uint32_t, mbox_id, mbox_id)
    )
)

TRACEPOINT_EVENT(
    com_ericsson_itc_if,
    itc_locate_async, // Comment
    TP_ARGS(uint32_t, own_mbox_id,
            const char *, name,
	    unsigned long, msg),

    /* Next are the fields */
    TP_FIELDS(
	ctf_integer_hex(uint32_t, own_mbox_id, own_mbox_id)
        ctf_string(name, name)
	ctf_integer_hex(unsigned long, msg, msg)
    )
)

TRACEPOINT_EVENT(
    com_ericsson_itc_if,
    itc_getname, // Comment
    TP_ARGS(uint32_t, own_mbox_id,
            uint32_t, mbox_id,
	    char *, name,
	    int, max_name_len,
	    int, found),

    /* Next are the fields */
    TP_FIELDS(
	ctf_integer_hex(uint32_t, own_mbox_id, own_mbox_id)
	ctf_integer_hex(uint32_t, mbox_id, mbox_id)
        ctf_string(name, name)
	ctf_integer(int, max_name_len, max_name_len)
	ctf_integer(int, found, found)
    )
)

TRACEPOINT_EVENT(
    com_ericsson_itc_if,
    itc_send, // Comment
    TP_ARGS(uint32_t, own_mbox_id,
            unsigned long, msg,
            uint32_t, msgno,
	    uint32_t, to,
	    uint32_t, from),

    /* Next are the fields */
    TP_FIELDS(
	ctf_integer_hex(uint32_t, own_mbox_id, own_mbox_id)
	ctf_integer_hex(unsigned long, msg, msg)
	ctf_integer_hex(uint32_t, msgno, msgno)
	ctf_integer_hex(uint32_t, to, to)
	ctf_integer_hex(uint32_t, from, from)
    )
)

TRACEPOINT_EVENT(
    com_ericsson_itc_if,
    itc_receive_enter, // Comment
    TP_ARGS(uint32_t, own_mbox_id,
            uint32_t, no_filter,
	    long, tmo,
	    uint32_t, from),

    /* Next are the fields */
    TP_FIELDS(
	ctf_integer_hex(uint32_t, own_mbox_id, own_mbox_id)
	ctf_integer(uint32_t, no_filter, no_filter)
	ctf_integer(long, tmo, tmo)
	ctf_integer_hex(uint32_t, from, from)
    )
)

TRACEPOINT_EVENT(
    com_ericsson_itc_if,
    itc_receive_exit_empty, // Comment
    TP_ARGS(uint32_t, own_mbox_id,
	    long, tmo,
	    uint32_t, from,
            unsigned long, msg),

    /* Next are the fields */
    TP_FIELDS(
	ctf_integer_hex(uint32_t, own_mbox_id, own_mbox_id)
	ctf_integer(long, tmo, tmo)
	ctf_integer_hex(uint32_t, from, from)
	ctf_integer_hex(unsigned long, msg, msg)
    )
)

TRACEPOINT_EVENT(
    com_ericsson_itc_if,
    itc_receive_exit, // Comment
    TP_ARGS(uint32_t, own_mbox_id,
	    long, tmo,
	    uint32_t, from,
            unsigned long, msg,
            uint32_t, msgno),

    /* Next are the fields */
    TP_FIELDS(
	ctf_integer_hex(uint32_t, own_mbox_id, own_mbox_id)
	ctf_integer(long, tmo, tmo)
	ctf_integer_hex(uint32_t, from, from)
	ctf_integer_hex(unsigned long, msg, msg)
	ctf_integer_hex(uint32_t, msgno, msgno)
    )
)

TRACEPOINT_EVENT(
    com_ericsson_itc_if,
    itc_receive_mbox_enter, // Comment
    TP_ARGS(uint32_t, own_mbox_id,
            uint32_t, mbox_id,
            uint32_t, no_filter,
	    long, tmo),

    /* Next are the fields */
    TP_FIELDS(
	ctf_integer_hex(uint32_t, own_mbox_id, own_mbox_id)
	ctf_integer_hex(uint32_t, mbox_id, mbox_id)
	ctf_integer(uint32_t, no_filter, no_filter)
	ctf_integer(long, tmo, tmo)
    )
)

TRACEPOINT_EVENT(
    com_ericsson_itc_if,
    itc_receive_mbox_exit_empty, // Comment
    TP_ARGS(uint32_t, own_mbox_id,
            uint32_t, mbox_id,
	    long, tmo,
            unsigned long, msg),

    /* Next are the fields */
    TP_FIELDS(
	ctf_integer_hex(uint32_t, own_mbox_id, own_mbox_id)
	ctf_integer_hex(uint32_t, mbox_id, mbox_id)
	ctf_integer(long, tmo, tmo)
	ctf_integer_hex(unsigned long, msg, msg)
    )
)

TRACEPOINT_EVENT(
    com_ericsson_itc_if,
    itc_receive_mbox_exit, // Comment
    TP_ARGS(uint32_t, own_mbox_id,
            uint32_t, mbox_id,
	    long, tmo,
            unsigned long, msg,
            uint32_t, msgno),

    /* Next are the fields */
    TP_FIELDS(
	ctf_integer_hex(uint32_t, own_mbox_id, own_mbox_id)
	ctf_integer_hex(uint32_t, mbox_id, mbox_id)
	ctf_integer(long, tmo, tmo)
	ctf_integer_hex(unsigned long, msg, msg)
	ctf_integer_hex(uint32_t, msgno, msgno)
    )
)

TRACEPOINT_EVENT(
    com_ericsson_itc_if,
    itc_monitor, // Comment
    TP_ARGS(uint32_t, own_mbox_id,
            uint32_t, who,
	    unsigned long, msg,
	    unsigned long, monitor_id),

    /* Next are the fields */
    TP_FIELDS(
	ctf_integer_hex(uint32_t, own_mbox_id, own_mbox_id)
	ctf_integer_hex(uint32_t, who, who)
	ctf_integer_hex(unsigned long, msg, msg)
	ctf_integer_hex(unsigned long, monitor_id, monitor_id)
    )
)

TRACEPOINT_EVENT(
    com_ericsson_itc_if,
    itc_unmonitor, // Comment
    TP_ARGS(uint32_t, own_mbox_id,
            unsigned long, monitor_id),

    /* Next are the fields */
    TP_FIELDS(
	ctf_integer_hex(uint32_t, own_mbox_id, own_mbox_id)
	ctf_integer_hex(unsigned long, monitor_id, monitor_id)
    )
)

TRACEPOINT_EVENT(
    com_ericsson_itc_if,
    itc_register_errorhandler, // Comment
    TP_ARGS(uint32_t, own_mbox_id,
            unsigned long, errh),

    /* Next are the fields */
    TP_FIELDS(
	ctf_integer_hex(uint32_t, own_mbox_id, own_mbox_id)
	ctf_integer_hex(unsigned long, errh, errh)
    )
)

TRACEPOINT_EVENT(
    com_ericsson_itc_if,
    itc_send_locate_found, // Comment
    TP_ARGS(uint32_t, own_mbox_id,
           uint32_t, mbox_id,
            uint32_t, from_mbox_id,
            unsigned long, msg,
            uint32_t, msgno),

    /* Next are the fields */
    TP_FIELDS(
	ctf_integer_hex(uint32_t, own_mbox_id, own_mbox_id)
	ctf_integer_hex(uint32_t, mbox_id, mbox_id)
	ctf_integer_hex(uint32_t, from_mbox_id, from_mbox_id)
	ctf_integer_hex(unsigned long, msg, msg)
	ctf_integer_hex(uint32_t, msgno, msgno)
    )
)

TRACEPOINT_EVENT(
    com_ericsson_itc_if,
    itc_coord_ITC_LOCATE_ASYNC_REPL, // Comment
    TP_ARGS(uint32_t, msgno,
            uint32_t, my_mbox_id,
            uint32_t, from_mbox_id,
            unsigned long, data,
            uint32_t, mbox_id,
            int, transport,
            char *, name),

    /* Next are the fields */
    TP_FIELDS(
	ctf_integer_hex(uint32_t, msgno, msgno)
	ctf_integer_hex(uint32_t, my_mbox_id, my_mbox_id)
	ctf_integer_hex(uint32_t, from_mbox_id, from_mbox_id)
	ctf_integer_hex(unsigned long, data, data)
	ctf_integer_hex(uint32_t, mbox_id, mbox_id)
	ctf_integer_hex(int, transport, transport)
        ctf_string(name, name)
    )
)

TRACEPOINT_EVENT(
    com_ericsson_itc_if,
    itc_coord_send_locate_found, // Comment
    TP_ARGS(uint32_t, mbox_id,
            uint32_t, from_mbox_id,
            unsigned long, msg,
            uint32_t, msgno),

    /* Next are the fields */
    TP_FIELDS(
	ctf_integer_hex(uint32_t, mbox_id, mbox_id)
	ctf_integer_hex(uint32_t, from_mbox_id, from_mbox_id)
	ctf_integer_hex(unsigned long, msg, msg)
	ctf_integer_hex(uint32_t, msgno, msgno)
    )
)
TRACEPOINT_EVENT(
    com_ericsson_itc_if,
    itc_send_to_deleted, // Comment
    TP_ARGS(uint32_t, mbox_id,
	    uint32_t, from_mbox_id,
            uint32_t, to_mbox_id),

    /* Next are the fields */
    TP_FIELDS(
	ctf_integer_hex(uint32_t, mbox_id, mbox_id)
	ctf_integer_hex(uint32_t, from_mbox_id, from_mbox_id)
	ctf_integer_hex(uint32_t, to_mbox_id, to_mbox_id)
    )
)

TRACEPOINT_EVENT(
    com_ericsson_itc_if,
    itc_error, // Comment
    TP_ARGS(uint32_t, own_mbox_id,
            char *, file,
            int, line,
            char *, err_msg),

    /* Next are the fields */
    TP_FIELDS(
	ctf_integer_hex(uint32_t, own_mbox_id, own_mbox_id)
        ctf_string(file, file)
	ctf_integer(int, line, line)
        ctf_string(err_msg, err_msg)
    )
)

TRACEPOINT_LOGLEVEL(com_ericsson_itc_if, itc_init, TRACE_DEBUG)
TRACEPOINT_LOGLEVEL(com_ericsson_itc_if, itc_init_wname, TRACE_DEBUG)
TRACEPOINT_LOGLEVEL(com_ericsson_itc_if, itc_exit, TRACE_DEBUG)
TRACEPOINT_LOGLEVEL(com_ericsson_itc_if, itc_alloc, TRACE_DEBUG)
TRACEPOINT_LOGLEVEL(com_ericsson_itc_if, itc_free, TRACE_DEBUG)
TRACEPOINT_LOGLEVEL(com_ericsson_itc_if, itc_sender, TRACE_DEBUG)
TRACEPOINT_LOGLEVEL(com_ericsson_itc_if, itc_receiver, TRACE_DEBUG)
TRACEPOINT_LOGLEVEL(com_ericsson_itc_if, itc_size, TRACE_DEBUG)
TRACEPOINT_LOGLEVEL(com_ericsson_itc_if, itc_create_mailbox_enter, TRACE_DEBUG)
TRACEPOINT_LOGLEVEL(com_ericsson_itc_if, itc_create_mailbox_exit, TRACE_DEBUG)
TRACEPOINT_LOGLEVEL(com_ericsson_itc_if, itc_delete_mailbox, TRACE_DEBUG)
TRACEPOINT_LOGLEVEL(com_ericsson_itc_if, itc_clone_mailbox, TRACE_DEBUG)
TRACEPOINT_LOGLEVEL(com_ericsson_itc_if, itc_add_name, TRACE_DEBUG)
TRACEPOINT_LOGLEVEL(com_ericsson_itc_if, itc_locate, TRACE_DEBUG)
TRACEPOINT_LOGLEVEL(com_ericsson_itc_if, itc_locate_async, TRACE_DEBUG)
TRACEPOINT_LOGLEVEL(com_ericsson_itc_if, itc_getname, TRACE_DEBUG)
TRACEPOINT_LOGLEVEL(com_ericsson_itc_if, itc_send, TRACE_DEBUG)
TRACEPOINT_LOGLEVEL(com_ericsson_itc_if, itc_receive_enter, TRACE_DEBUG)
TRACEPOINT_LOGLEVEL(com_ericsson_itc_if, itc_receive_exit, TRACE_DEBUG)
TRACEPOINT_LOGLEVEL(com_ericsson_itc_if, itc_receive_exit_empty, TRACE_DEBUG)
TRACEPOINT_LOGLEVEL(com_ericsson_itc_if, itc_receive_mbox_enter, TRACE_DEBUG)
TRACEPOINT_LOGLEVEL(com_ericsson_itc_if, itc_receive_mbox_exit, TRACE_DEBUG)
TRACEPOINT_LOGLEVEL(com_ericsson_itc_if, itc_receive_mbox_exit_empty, TRACE_DEBUG)
TRACEPOINT_LOGLEVEL(com_ericsson_itc_if, itc_monitor, TRACE_DEBUG)
TRACEPOINT_LOGLEVEL(com_ericsson_itc_if, itc_unmonitor, TRACE_DEBUG)
TRACEPOINT_LOGLEVEL(com_ericsson_itc_if, itc_register_errorhandler, TRACE_DEBUG)

TRACEPOINT_LOGLEVEL(com_ericsson_itc_if, itc_send_locate_found, TRACE_DEBUG)
TRACEPOINT_LOGLEVEL(com_ericsson_itc_if, itc_coord_ITC_LOCATE_ASYNC_REPL, TRACE_DEBUG)
TRACEPOINT_LOGLEVEL(com_ericsson_itc_if, itc_coord_send_locate_found, TRACE_DEBUG)
TRACEPOINT_LOGLEVEL(com_ericsson_itc_if, itc_send_to_deleted, TRACE_DEBUG)

TRACEPOINT_LOGLEVEL(com_ericsson_itc_if, itc_error, TRACE_ERR)
#endif /* _ITC_LTTNG_H */

#include <lttng/tracepoint-event.h>

#ifdef __cplusplus
}
#endif /* __cplusplus */

