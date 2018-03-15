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

#ifndef _TPT_H
#define _TPT_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stdarg.h>
#ifndef __COVERITY__
#include "tpt_lttng.h"

#include <lttng/tracepoint-event.h>

/**
 * Macro: TPT_INIT()
 * Description:
 *   Makes necessary initializations for trace
 *
 */
#define TPT_INIT()

/* Macros for using this trace point provider */

/**
 * Macro: TPT_ERROR()
 * Description:
 *   Used for tracing errors.
 *
 */
#define TPT_ERROR(msg) \
		tpt_tracepoint(TRACEPOINT_PROVIDER, ERROR, tpt_short_fname(__FILE__), __LINE__, msg);
/**
 * Macro: TPT_INFO()
 * Description:
 *   Used for tracing informational messages. A call to INFO always
 *   generates a logged message, and thus it should be used with care.
 *
 */
#define TPT_INFO(msg) \
		tpt_tracepoint(TRACEPOINT_PROVIDER, INFO, tpt_short_fname(__FILE__), __LINE__, msg);
/**
 * Macro: TPT_STATE()/TPT_OBJ_STATE()
 * Description:
 *   Used for tracing state changes in finite state machines. Preferably
 *   the message shall state what the actual state is that has occurred.
 *
 */
#define TPT_STATE(msg) \
		tpt_tracepoint(TRACEPOINT_PROVIDER, STATE_CHANGE, tpt_short_fname(__FILE__), __LINE__, "-", msg);
#define TPT_OBJ_STATE(obj, msg) \
		tpt_tracepoint(TRACEPOINT_PROVIDER, STATE_CHANGE, tpt_short_fname(__FILE__), __LINE__, obj, msg);
/**
 * Macro: TPT_TRACE()/TPT_TRACE_OBJ()
 * Description:
 *   This is the general trace macro. Used for tracing other kinds of
 *   messages that needs to be switched on or off. The group parameter
 *   indicates which trace group this trace belongs to (1-7).
 *
 */
#define TPT_TRACE(group, msg) \
		tpt_tracepoint(TRACEPOINT_PROVIDER, TRACE ##group, tpt_short_fname(__FILE__), __LINE__, "-", msg);
#define TPT_TRACE_OBJ(group, obj, msg) \
		tpt_tracepoint(TRACEPOINT_PROVIDER, TRACE ##group, tpt_short_fname(__FILE__), __LINE__, obj, msg);
/**
 * Macro: TPT_REC_SIG()/TPT_OBJ_REC_SIG()
 * Description:
 *   Used for tracing of received signals.
 *
 */
#define TPT_REC_SIG(sig, msg) \
		tpt_tracepoint(TRACEPOINT_PROVIDER, REC_SIG, tpt_short_fname(__FILE__), __LINE__, "-", sig, msg);
#define TPT_OBJ_REC_SIG(obj, sig, msg) \
		tpt_tracepoint(TRACEPOINT_PROVIDER, REC_SIG, tpt_short_fname(__FILE__), __LINE__, obj, sig, msg);
/**
 * Macro: TPT_SEND_SIG()/TPT_OBJ_SEND_SIG()
 * Description:
 *   Used for tracing of sent signals.
 *
 */
#define TPT_SEND_SIG(sig, pid, msg) \
		tpt_tracepoint(TRACEPOINT_PROVIDER, SEND_SIG, tpt_short_fname(__FILE__), __LINE__, "-", sig, pid, msg);
#define TPT_OBJ_SEND_SIG(obj, sig, pid, msg) \
		tpt_tracepoint(TRACEPOINT_PROVIDER, SEND_SIG, tpt_short_fname(__FILE__), __LINE__, obj, sig, pid, msg);
/**
 * Macro: TPT_DATA()/TPT_OBJ_DATA()
 * Description:
 *   Used for tracing data.
 *
 */

#define TPT_DATA(msg, data, data_len) \
		tpt_tracepoint(TRACEPOINT_PROVIDER, DATA, tpt_short_fname(__FILE__), __LINE__, "-", msg, data, data_len);
#define TPT_OBJ_DATA(obj, msg, data, data_len) \
		tpt_tracepoint(TRACEPOINT_PROVIDER, DATA, tpt_short_fname(__FILE__), __LINE__, obj, msg, data, data_len);
/**
 * Macro: TPT_UTS()
 * Description:
 *   Used for tracing any trace group (1-7), but it gives an option
 *   to the user to input its own time stamp ( seconds and microseconds),
 *   process name, file and line information.
 *
 */
#define TPT_UTS(group, tsec, tusec, file, line, proc_name, msg, data, data_len) \
		tpt_tracepoint(TRACEPOINT_PROVIDER, TRACE_UTS ##group, file, line, tsec, tusec, proc_name, msg, data, data_len);


#else
/* Coverity seems to have problems with LTTng macros, so we stub them out */
#define TPT_INIT()
#define TPT_ERROR(msg) printf("%s", msg)
#define TPT_INFO(msg) printf("%s", msg)
#define TPT_STATE(msg) printf("%s", msg)
#define TPT_OBJ_STATE(obj, msg) printf("%s %s", obj, msg)
#define TPT_TRACE(group, msg) printf("%u %s", group, msg)
#define TPT_TRACE_OBJ(group, obj, msg) printf("%u %s %s", group, obj, msg)
#define TPT_REC_SIG(sig, msg) printf("%u %s", sig, msg)
#define TPT_OBJ_REC_SIG(obj, sig, msg) printf("%s %u %s", obj, sig, msg)
#define TPT_SEND_SIG(sig, pid, msg) printf("%u %u %s", sig, pid, msg)
#define TPT_OBJ_SEND_SIG(obj, sig, pid, msg) printf("%s %u %d %s", obj, sig, pid, msg)
#define TPT_DATA(msg, data, data_len) printf("%s %u %u", msg, *data, data_len)
#define TPT_OBJ_DATA(obj, msg, data, data_len) printf("%s %s %u %u", obj, msg, *data, data_len)
#define TPT_UTS(group, tsec, tusec, file, line, proc_name, msg, data, data_len)  \
	printf("%u %u %u %s %u %s %s %u %u", group, tsec, tusec, file, line, proc_name, msg, *data, data_len)
#endif

/**
 * Macro: STR()
 * Description:
 *   STR(char *format, ...)
 *
 *   The format string has the same syntax as in the sprintf function,
 *   There are however some limitations in the maximum length of the
 *   formatted string.
 */
#define STR (tpt_format_str)
/*
 * Macro: STR2()
 * Description:
 *   STR2(char *format, ...)
 *
 *   This is a simplified and hence faster implementation of format string.
 *   The format string has the same syntax as in the sprintf function,
 *   There are however some limitations in the maximum length of the
 *   formatted string as well as in the conversion specifications. If
 *   any of these limitations are too limited then STR() should be used.
 *
 *   A conversion specification must have the following syntax:
 *
 *      %[<width>][.<precision>]<conversion character>
 *
 *      ([] specifies an optional item)
 *
 *   The conversion character must be one of:
 *
 *      d   Signed decimal
 *      u   Unsigned decimal
 *      x   Hexadecimal (uses the characters 'abcdef')
 *      X   Hexadecimal (uses the characters 'ABCDEF')
 *      f   Float (Limited formatting capability. Use with care.)
 *      s   String
 *      c   Character
 *      %   The '%' character itself
 *
 * Example:
 *   STR2("Decimal value = %d", value)
 *   STR2("Hex value 1 = %x, Hex value 2 = %X", hex1, hex2)
 *   STR2("The string = %s", str)
 */
#define STR2 (tpt_format_str2)

char *tpt_format_str(const char *format, ...) __attribute__((format(printf, 1, 2)));
char *tpt_format_str2(const char *format, ...) __attribute__((format(printf, 1, 2)));

/**
 * tpt_short_fname
 *
 * Removes base directory of fname
 */
const char *tpt_short_fname(const char *fname);


#ifdef __cplusplus
}
#endif

#endif /* _TPT_H */


