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

#ifndef LOG_H
#define LOG_H

#ifdef __cplusplus
extern "C" {
#endif

#ifdef _LTTNG_
extern char *log_lttng_format_str(const char *format, ...);
extern const char *log_lttng_short_fname(const char *fname);

#include "com_ericsson_libecp.h"

#define log_err(fmt, ...) com_ericsson_libecp_tracepoint( \
	TRACEPOINT_PROVIDER, ERROR, \
	log_lttng_short_fname(__FILE__), __LINE__, \
	log_lttng_format_str(fmt, ##__VA_ARGS__));

#define log_info(fmt, ...) com_ericsson_libecp_tracepoint( \
	TRACEPOINT_PROVIDER, INFO, \
	log_lttng_short_fname(__FILE__), __LINE__, \
	log_lttng_format_str(fmt, ##__VA_ARGS__));

#define log_trace2(fmt, ...) com_ericsson_libecp_tracepoint( \
	TRACEPOINT_PROVIDER, TRACE2, \
	log_lttng_short_fname(__FILE__), __LINE__, \
	log_lttng_format_str(fmt, ##__VA_ARGS__));

#else /* syslog */
extern void log_err(const char * fmt, ...);
extern void log_info(const char * fmt, ...);
#define log_trace2(fmt, ...)
#endif

#ifdef __cplusplus
}
#endif

#endif /* LOG_H */
