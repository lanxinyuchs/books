#ifndef __LOG_H
#define __LOG_H

#ifdef __LTTNG__
extern char *log_lttng_format_str(const char *format, ...);
extern const char *log_lttng_short_fname(const char *fname);

#include "com_ericsson_mama.h"

#define log_init()

#define log_err(fmt, ...) com_ericsson_mama_tracepoint( \
	TRACEPOINT_PROVIDER, ERROR, \
	log_lttng_short_fname(__FILE__), __LINE__, \
	log_lttng_format_str(fmt, ##__VA_ARGS__));

#define log_info(fmt, ...) com_ericsson_mama_tracepoint( \
	TRACEPOINT_PROVIDER, INFO, \
	log_lttng_short_fname(__FILE__), __LINE__, \
	log_lttng_format_str(fmt, ##__VA_ARGS__));

#define log_trace2(fmt, ...) com_ericsson_mama_tracepoint( \
    TRACEPOINT_PROVIDER, TRACE2, \
    log_lttng_short_fname(__FILE__), __LINE__, \
    log_lttng_format_str(fmt, ##__VA_ARGS__));

#else /* syslog */

extern void log_init();
extern void log_err(const char *fmt, ...);
extern void log_info(const char *fmt, ...);
#define log_trace2(fmt, ...)

#endif

#endif /* __LOG_H */
