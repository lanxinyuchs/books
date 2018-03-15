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
#undef TRACEPOINT_INCLUDE_FILE
#define TRACEPOINT_INCLUDE_FILE tpt_lttng.h

#if !defined(_TPT_LTTNG_H) || defined(TRACEPOINT_HEADER_MULTI_READ)
#define _TPT_LTTNG_H

#ifdef __cplusplus
extern "C" {
#endif

#include <lttng/tracepoint.h>

/* This is done, so macro TRACEPOINT_PROVIDER can be
 * expanded before macro TRACEPOINT_LOGLEVEL is used*/
#define TPT_LOGLEVEL(a, b, c) TRACEPOINT_LOGLEVEL(a, b, c)

/* NO MSG*/
#define TPT_ARGS \
		TP_ARGS( \
				const char*, file, \
				unsigned int, line \
		)
#define TPT_FIELDS \
	    TP_FIELDS(\
				ctf_string(file, file)\
				ctf_integer(unsigned int, line, line)\
	    )

/* MSG */
#define TPT_MSG_ARGS \
		TP_ARGS( \
				const char*, file, \
				unsigned int, line, \
				const char*, msg \
		)
#define TPT_MSG_FIELDS \
	    TP_FIELDS(\
				ctf_string(file, file)\
				ctf_integer(unsigned int, line, line)\
				ctf_string(msg, msg)\
	    )
TRACEPOINT_EVENT(
	TRACEPOINT_PROVIDER,
	ERROR,
	TPT_MSG_ARGS,
	TPT_MSG_FIELDS
)
TPT_LOGLEVEL(TRACEPOINT_PROVIDER, ERROR, TRACE_ERR)
TRACEPOINT_EVENT(
	TRACEPOINT_PROVIDER,
	INFO,
	TPT_MSG_ARGS,
	TPT_MSG_FIELDS
)
TPT_LOGLEVEL(TRACEPOINT_PROVIDER, INFO, TRACE_INFO)

/* MSG + OBJ */
#define TPT_OBJ_MSG_ARGS \
		TP_ARGS( \
				const char*, file, \
				unsigned int, line, \
				const char*, obj, \
				const char*, msg \
		)
#define TPT_OBJ_MSG_FIELDS \
	    TP_FIELDS(\
				ctf_string(file, file)\
				ctf_integer(unsigned int, line, line)\
				ctf_string(obj, obj)\
				ctf_string(msg, msg)\
	    )
TRACEPOINT_EVENT(
	TRACEPOINT_PROVIDER,
	STATE_CHANGE,
	TPT_OBJ_MSG_ARGS,
	TPT_OBJ_MSG_FIELDS
)
TPT_LOGLEVEL(TRACEPOINT_PROVIDER, STATE_CHANGE, TRACE_DEBUG_SYSTEM)
TRACEPOINT_EVENT(
	TRACEPOINT_PROVIDER,
	TRACE1,
	TPT_OBJ_MSG_ARGS,
	TPT_OBJ_MSG_FIELDS
)
TPT_LOGLEVEL(TRACEPOINT_PROVIDER, TRACE1, TRACE_DEBUG_PROGRAM)
TRACEPOINT_EVENT(
	TRACEPOINT_PROVIDER,
	TRACE2,
	TPT_OBJ_MSG_ARGS,
	TPT_OBJ_MSG_FIELDS
)
TPT_LOGLEVEL(TRACEPOINT_PROVIDER, TRACE2, TRACE_DEBUG_PROCESS)
TRACEPOINT_EVENT(
	TRACEPOINT_PROVIDER,
	TRACE3,
	TPT_OBJ_MSG_ARGS,
	TPT_OBJ_MSG_FIELDS
)
TPT_LOGLEVEL(TRACEPOINT_PROVIDER, TRACE3, TRACE_DEBUG_MODULE)
TRACEPOINT_EVENT(
	TRACEPOINT_PROVIDER,
	TRACE4,
	TPT_OBJ_MSG_ARGS,
	TPT_OBJ_MSG_FIELDS
)
TPT_LOGLEVEL(TRACEPOINT_PROVIDER, TRACE4, TRACE_DEBUG_UNIT)
TRACEPOINT_EVENT(
	TRACEPOINT_PROVIDER,
	TRACE5,
	TPT_OBJ_MSG_ARGS,
	TPT_OBJ_MSG_FIELDS
)
TPT_LOGLEVEL(TRACEPOINT_PROVIDER, TRACE5, TRACE_DEBUG_FUNCTION)
TRACEPOINT_EVENT(
	TRACEPOINT_PROVIDER,
	TRACE6,
	TPT_OBJ_MSG_ARGS,
	TPT_OBJ_MSG_FIELDS
)
TPT_LOGLEVEL(TRACEPOINT_PROVIDER, TRACE6, TRACE_DEBUG_LINE)
TRACEPOINT_EVENT(
	TRACEPOINT_PROVIDER,
	TRACE7,
	TPT_OBJ_MSG_ARGS,
	TPT_OBJ_MSG_FIELDS
)
TPT_LOGLEVEL(TRACEPOINT_PROVIDER, TRACE7, TRACE_DEBUG)

/* OBJ, SIG + MSG */
#define TPT_OBJ_SIG_MSG_ARGS \
		TP_ARGS( \
				const char*, file, \
				unsigned int, line, \
				const char*, obj, \
				unsigned int, sig_no, \
				const char*, msg \
		)
#define TPT_OBJ_SIG_MSG_FIELDS \
		TP_FIELDS(\
				ctf_string(file, file)\
				ctf_integer(unsigned int, line, line)\
				ctf_string(obj, obj)\
				ctf_integer_hex(unsigned int, sig_no, sig_no)\
				ctf_string(msg, msg)\
		)
TRACEPOINT_EVENT(
	TRACEPOINT_PROVIDER,
	REC_SIG,
	TPT_OBJ_SIG_MSG_ARGS,
	TPT_OBJ_SIG_MSG_FIELDS
)
TPT_LOGLEVEL(TRACEPOINT_PROVIDER, REC_SIG, TRACE_DEBUG_SYSTEM)

/* SIG, PID, OBJ + MSG */
#define TPT_OBJ_SIG_PID_MSG_ARGS \
		TP_ARGS( \
				const char*, file, \
				unsigned int, line, \
				const char*, obj, \
				unsigned int, sig_no, \
				unsigned int, pid, \
				const char*, msg \
		)
#define TPT_OBJ_SIG_PID_MSG_FIELDS \
	TP_FIELDS(\
			ctf_string(file, file)\
			ctf_integer(unsigned int, line, line)\
			ctf_string(obj, obj)\
			ctf_integer_hex(unsigned int, sig_no, sig_no)\
			ctf_integer(unsigned int, pid, pid)\
			ctf_string(msg, msg)\
	)
TRACEPOINT_EVENT(
	TRACEPOINT_PROVIDER,
	SEND_SIG,
	TPT_OBJ_SIG_PID_MSG_ARGS,
	TPT_OBJ_SIG_PID_MSG_FIELDS
)
TPT_LOGLEVEL(TRACEPOINT_PROVIDER, SEND_SIG, TRACE_DEBUG_SYSTEM)

/* OBJ, MSG + DATA*/
#define TPT_OBJ_MSG_DATA_ARGS \
		TP_ARGS( \
				const char*, file, \
				unsigned int, line, \
				const char*, obj, \
				const char*, msg, \
				const char*, data, size_t, datalen \
		)
#define TPT_OBJ_MSG_DATA_FIELDS \
		TP_FIELDS( \
				ctf_string(file, file)\
				ctf_integer(unsigned int, line, line)\
				ctf_string(obj, obj)\
				ctf_string(msg, msg)\
				ctf_sequence(char, data, data, size_t, datalen)\
		)
TRACEPOINT_EVENT(
	TRACEPOINT_PROVIDER,
	DATA,
	TPT_OBJ_MSG_DATA_ARGS,
	TPT_OBJ_MSG_DATA_FIELDS
)
TPT_LOGLEVEL(TRACEPOINT_PROVIDER, DATA, TRACE_DEBUG)

/* SEC, USEC, PROCESS, MSG + DATA */
#define TPT_UTS_ARGS \
		TP_ARGS( \
				const char*, file, \
				unsigned int, line, \
				unsigned int, sec, \
				unsigned int, usec, \
				const char*, process, \
				const char*, msg, \
				const char*, data, size_t, datalen \
		)
#define TPT_UTS_FIELDS \
		TP_FIELDS( \
				ctf_string(file, file)\
				ctf_integer(unsigned int, line, line)\
				ctf_integer(unsigned int, sec, sec)\
				ctf_integer(unsigned int, usec, usec)\
				ctf_string(process, process)\
				ctf_string(msg, msg)\
				ctf_sequence(char, data, data, size_t, datalen)\
		)
TRACEPOINT_EVENT(
	TRACEPOINT_PROVIDER,
	TRACE_UTS1,
	TPT_UTS_ARGS,
	TPT_UTS_FIELDS
)
TPT_LOGLEVEL(TRACEPOINT_PROVIDER, TRACE_UTS1, TRACE_DEBUG_PROGRAM)
TRACEPOINT_EVENT(
	TRACEPOINT_PROVIDER,
	TRACE_UTS2,
	TPT_UTS_ARGS,
	TPT_UTS_FIELDS
)
TPT_LOGLEVEL(TRACEPOINT_PROVIDER, TRACE_UTS2, TRACE_DEBUG_PROCESS)
TRACEPOINT_EVENT(
	TRACEPOINT_PROVIDER,
	TRACE_UTS3,
	TPT_UTS_ARGS,
	TPT_UTS_FIELDS
)
TPT_LOGLEVEL(TRACEPOINT_PROVIDER, TRACE_UTS3, TRACE_DEBUG_MODULE)
TRACEPOINT_EVENT(
	TRACEPOINT_PROVIDER,
	TRACE_UTS4,
	TPT_UTS_ARGS,
	TPT_UTS_FIELDS
)
TPT_LOGLEVEL(TRACEPOINT_PROVIDER, TRACE_UTS4, TRACE_DEBUG_UNIT)
TRACEPOINT_EVENT(
	TRACEPOINT_PROVIDER,
	TRACE_UTS5,
	TPT_UTS_ARGS,
	TPT_UTS_FIELDS
)
TPT_LOGLEVEL(TRACEPOINT_PROVIDER, TRACE_UTS5, TRACE_DEBUG_FUNCTION)
TRACEPOINT_EVENT(
	TRACEPOINT_PROVIDER,
	TRACE_UTS6,
	TPT_UTS_ARGS,
	TPT_UTS_FIELDS
)
TPT_LOGLEVEL(TRACEPOINT_PROVIDER, TRACE_UTS6, TRACE_DEBUG_LINE)
TRACEPOINT_EVENT(
	TRACEPOINT_PROVIDER,
	TRACE_UTS7,
	TPT_UTS_ARGS,
	TPT_UTS_FIELDS
)
TPT_LOGLEVEL(TRACEPOINT_PROVIDER, TRACE_UTS7, TRACE_DEBUG)

/* This is done, so macro TRACEPOINT_PROVIDER can be
 * expanded before macro tracepoint is used*/
#define tpt_tracepoint(provider, ...) tracepoint(provider,  ## __VA_ARGS__)

#ifdef __cplusplus
}
#endif

#endif /* _TPT_LTTNG_H */
