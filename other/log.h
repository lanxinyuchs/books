/* ---------------------------------------------------------------------------
 *
 * © Ericsson AB 2017 All rights reserved.
 * The information in this document is the property of Ericsson.  Except
 * as specifically authorized in writing by Ericsson, the receiver of
 * this document shall keep the information contained herein confidential
 * and shall protect the same in whole or in part from disclosure and
 * dissemination to third parties.  Disclosure and disseminations to the
 * receivers employees shall only be made on a strict need to know basis.
 *
 * ---------------------------------------------------------------------------
 */

#ifndef __LOG_H
#define __LOG_H

#ifdef __cplusplus
extern "C" {
#endif

#ifdef __LTTNG__
extern char *log_lttng_format_str(const char *format, ...);
extern const char *log_lttng_short_fname(const char *fname);

#include "com_ericsson_ftx_d.h"

#define log_err(fmt, ...) com_ericsson_ftx_d_tracepoint( \
    TRACEPOINT_PROVIDER, ERROR, \
    log_lttng_short_fname(__FILE__), __LINE__, \
    log_lttng_format_str(fmt, ##__VA_ARGS__));

#define log_info(fmt, ...) com_ericsson_ftx_d_tracepoint( \
    TRACEPOINT_PROVIDER, INFO, \
    log_lttng_short_fname(__FILE__), __LINE__, \
    log_lttng_format_str(fmt, ##__VA_ARGS__));

#define log_trace2(fmt, ...) com_ericsson_ftx_d_tracepoint( \
    TRACEPOINT_PROVIDER, TRACE2, \
    log_lttng_short_fname(__FILE__), __LINE__, \
    log_lttng_format_str(fmt, ##__VA_ARGS__));
#else
#ifdef HAVE_SYSLOG_H
#include <syslog.h>
#define log_err(fmt, ...) syslog(LOG_ERR, fmt, ##__VA_ARGS__)
#define log_info(fmt, ...) syslog(LOG_INFO, fmt, ##__VA_ARGS__)
#define log_trace2(fmt, ...)
#else
#define log_err(fmt, ...) do { printf("ERROR:"); printf(fmt, ##__VA_ARGS__); printf("\n"); } while (0)
#define log_info(fmt, ...) do { printf("INFO:"); printf(fmt, ##__VA_ARGS__); printf("\n"); } while (0)
#define log_trace2(fmt, ...) do { printf("TRACE2:"); printf(fmt, ##__VA_ARGS__); printf("\n"); } while (0)
#endif
#endif

#ifdef __cplusplus
}
#endif

#endif /* __LOG_H */
