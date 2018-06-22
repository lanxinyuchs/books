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

#include <stdarg.h>
#include <stdio.h>
#include <string.h>

#define TRACEPOINT_DEFINE
#define TRACEPOINT_CREATE_PROBES

#include "log.h"

#define MAX_TRACE_LEN 512

char *log_lttng_format_str(const char *format, ...)
{
    static __thread char internal_trace_buf[MAX_TRACE_LEN] = {0};
    va_list args;

    va_start(args, format);
    vsnprintf(internal_trace_buf, MAX_TRACE_LEN, format, args);
    va_end(args);

    return internal_trace_buf;
}

const char *log_lttng_short_fname(const char *fname)
{
    const char *fileName = strrchr(fname,'/');
    return fileName ? fileName + 1 : fname;
}
