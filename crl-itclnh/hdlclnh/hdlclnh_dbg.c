/* ---------------------------------------------------------------------------
 *
 * © Ericsson AB 2015 All rights reserved.
 * The information in this document is the property of Ericsson. Except
 * as specifically authorized in writing by Ericsson, the receiver of
 * this document shall keep the information contained herein confidential
 * and shall protect the same in whole or in part from disclosure and
 * dissemination to third parties. Disclosure and disseminations to the
 * receivers employees shall only be made on a strict need to know basis.
 *
 * ---------------------------------------------------------------------------
 */

#include <stdio.h>
#include <stdint.h>
#include <stdarg.h>

#ifdef LTTNG

#include "hdlclnh_lttng.h"
#include "trace.h"

void hdlclnh_trace_dbg(int line, const char *format, ...)
{
        va_list args;
        char buffer[256];

        va_start(args, format);
        vsnprintf(buffer, sizeof(buffer), format, args);
        va_end(args);

        tracepoint(com_ericsson_hdlclnh, hdlclnh_dbg, line, buffer);
}

void hdlclnh_trace_info(int line, const char *format, ...)
{
        va_list args;
        char buffer[256];

        va_start(args, format);
        vsnprintf(buffer, sizeof(buffer), format, args);
        va_end(args);

        tracepoint(com_ericsson_hdlclnh, hdlclnh_info, line, buffer);
}

void hdlclnh_trace_error(int line, const char *format, ...)
{
        va_list args;
        char buffer[256];

        va_start(args, format);
        vsnprintf(buffer, sizeof(buffer), format, args);
        va_end(args);

        tracepoint(com_ericsson_hdlclnh, hdlclnh_error, line, buffer);
}

#endif
