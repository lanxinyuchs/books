#ifndef _LOG_H_
#define _LOG_H_

extern const char *log_short_fname(const char *fname);

/* Coverity seems to have problems with LTTng macros, so
   for coverity check we use printf as with LOG_CONSOLE */

#if defined(LOG_LTTNG) && !defined(__COVERITY__)
extern char *log_lttng_format_str(const char *format, ...);

#    include "com_ericsson_conn_establish.h"

#    define log_init()

#    define log_err(fmt, ...) com_ericsson_conn_establish_tracepoint(   \
		TRACEPOINT_PROVIDER, ERROR, \
		log_short_fname(__FILE__), __LINE__, \
		log_lttng_format_str(fmt, ##__VA_ARGS__))

#    define log_info(fmt, ...) com_ericsson_conn_establish_tracepoint(  \
		TRACEPOINT_PROVIDER, INFO, \
		log_short_fname(__FILE__), __LINE__, \
		log_lttng_format_str(fmt, ##__VA_ARGS__))
#    define log_trace(fmt, ...) com_ericsson_conn_establish_tracepoint( \
		TRACEPOINT_PROVIDER, TRACE1, \
		log_short_fname(__FILE__), __LINE__, \
		log_lttng_format_str(fmt, ##__VA_ARGS__))
#endif

#if LOG_SYSLOG
#    include <syslog.h>

#    define log_init() openlog("conn_establish", LOG_PERROR | LOG_PID, LOG_USER)
#    define log_err(fmt, ...)  syslog(LOG_ERR, "ERROR: %s;%d: " fmt "\n", \
                                       log_short_fname(__FILE__), __LINE__, \
                                       ##__VA_ARGS__)
#    define log_info(fmt, ...) syslog(LOG_INFO, "INFO: %s;%d: " fmt "\n", \
                                       log_short_fname(__FILE__), __LINE__, \
                                       ##__VA_ARGS__)
#    ifdef DEBUG
#        define log_trace(fmt, ...) syslog(LOG_DEBUG, "TRACE1: %s;%d: " fmt "\n", \
                                           log_short_fname(__FILE__), __LINE__, \
                                           ##__VA_ARGS__)
#    else
#        define log_trace(...)
#    endif
#endif

#if defined(LOG_CONSOLE) || defined(__COVERITY__)
#    include <stdio.h>
#    define log_init()
#    define log_err(fmt,...)  printf("ERROR: %s;%d: " fmt "\n",  \
                                      log_short_fname(__FILE__), __LINE__, \
                                      ##__VA_ARGS__)
#    define log_info(fmt,...) printf("INFO: %s;%d: " fmt "\n",  \
                                      log_short_fname(__FILE__), __LINE__, \
                                      ##__VA_ARGS__)
#    ifdef DEBUG
#        define log_trace(fmt,...) printf("TRACE: %s;%d: " fmt "\n",  \
                                          log_short_fname(__FILE__), __LINE__, \
                                          ##__VA_ARGS__)
#    else
#        define log_trace(...)
#    endif
#endif

#endif /* __LOG_H */
