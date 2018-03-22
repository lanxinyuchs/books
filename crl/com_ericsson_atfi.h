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
#define TRACEPOINT_PROVIDER com_ericsson_atfi

#undef TRACEPOINT_INCLUDE_FILE
#define TRACEPOINT_INCLUDE_FILE ./com_ericsson_atfi.h

#if !defined(_COM_ERICSSON_ATFI_H) || defined(TRACEPOINT_HEADER_MULTI_READ)
#define _COM_ERICSSON_ATFI_H

#include <lttng/tracepoint.h>

#ifdef __cplusplus
extern "C" {
#endif

/*
 * This is done, so macro TRACEPOINT_PROVIDER can be
 * expanded before macro TRACEPOINT_LOGLEVEL is used.
 */
#define COM_ERICSSON_ATFI_LOGLEVEL(a, b, c) TRACEPOINT_LOGLEVEL(a, b, c)

#define COM_ERICSSON_ATFI_ARGS \
	TP_ARGS( \
		const char*, file, \
		unsigned int, line, \
		const char*, msg \
	)
#define COM_ERICSSON_ATFI_FIELDS \
	TP_FIELDS( \
		ctf_string(file, file) \
		ctf_integer(unsigned int, line, line) \
		ctf_string(msg, msg) \
	)

#define COM_ERICSSON_ATFI_HDLC_ARGS \
	TP_ARGS( \
		const char*, file, \
		unsigned int, line, \
		uint8_t, addr, \
		const char*, ctrl, \
		const void*, info, \
		uint32_t*, info_size, \
		const char*, msg \
	)
#define COM_ERICSSON_ATFI_HDLC_FIELDS \
	TP_FIELDS( \
		ctf_string(file, file) \
		ctf_integer(unsigned int, line, line) \
		ctf_string(msg, msg) \
		ctf_integer(uint8_t, addr, addr) \
		ctf_string(ctrl, ctrl) \
		ctf_sequence_hex(uint8_t, info, info, \
		                 uint32_t, info_size == NULL ? 0 : *info_size) \
	)

#define COM_ERICSSON_ATFI_BUS_ARGS \
	TP_ARGS( \
		const char*, file, \
		unsigned int, line, \
		const uint8_t*, data, \
		uint32_t, size, \
		const char*, msg \
	)
#define COM_ERICSSON_ATFI_BUS_FIELDS \
	TP_FIELDS( \
		ctf_string(file, file) \
		ctf_integer(unsigned int, line, line) \
		ctf_string(msg, msg) \
		ctf_sequence_hex(uint8_t, data, data, uint32_t, size) \
	)

TRACEPOINT_EVENT(
	TRACEPOINT_PROVIDER,
	ERROR,
	COM_ERICSSON_ATFI_ARGS,
	COM_ERICSSON_ATFI_FIELDS
)
COM_ERICSSON_ATFI_LOGLEVEL(
	TRACEPOINT_PROVIDER,
	ERROR,
	TRACE_ERR
)

TRACEPOINT_EVENT(
	TRACEPOINT_PROVIDER,
	INFO,
	COM_ERICSSON_ATFI_ARGS,
	COM_ERICSSON_ATFI_FIELDS
)
COM_ERICSSON_ATFI_LOGLEVEL(
	TRACEPOINT_PROVIDER,
	INFO,
	TRACE_NOTICE
)

TRACEPOINT_EVENT(
	TRACEPOINT_PROVIDER,
	TRACE2,
	COM_ERICSSON_ATFI_ARGS,
	COM_ERICSSON_ATFI_FIELDS
)
COM_ERICSSON_ATFI_LOGLEVEL(
	TRACEPOINT_PROVIDER,
	TRACE2,
	TRACE_DEBUG_PROGRAM
)

TRACEPOINT_EVENT(
	TRACEPOINT_PROVIDER,
	HDLC,
	COM_ERICSSON_ATFI_HDLC_ARGS,
	COM_ERICSSON_ATFI_HDLC_FIELDS
)
COM_ERICSSON_ATFI_LOGLEVEL(
	TRACEPOINT_PROVIDER,
	HDLC,
	TRACE_DEBUG_PROGRAM
)

TRACEPOINT_EVENT(
	TRACEPOINT_PROVIDER,
	BUS_SEND,
	COM_ERICSSON_ATFI_BUS_ARGS,
	COM_ERICSSON_ATFI_BUS_FIELDS
)
COM_ERICSSON_ATFI_LOGLEVEL(
	TRACEPOINT_PROVIDER,
	BUS_SEND,
	TRACE_DEBUG_PROGRAM
)

TRACEPOINT_EVENT(
	TRACEPOINT_PROVIDER,
	BUS_RECEIVE,
	COM_ERICSSON_ATFI_BUS_ARGS,
	COM_ERICSSON_ATFI_BUS_FIELDS
)
COM_ERICSSON_ATFI_LOGLEVEL(
	TRACEPOINT_PROVIDER,
	BUS_RECEIVE,
	TRACE_DEBUG_PROGRAM
)

/* This is done, so macro TRACEPOINT_PROVIDER can be
 * expanded before macro tracepoint is used*/
#define com_ericsson_atfi_tracepoint(provider, ...) \
	tracepoint(provider,  ##__VA_ARGS__)

#ifdef __cplusplus
}
#endif

#endif /* _COM_ERICSSON_ATFI_H */

#include <lttng/tracepoint-event.h>
