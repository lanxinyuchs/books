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

#ifdef LTTNG

void hdlclnh_trace_error(int line, const char *format, ...);
void hdlclnh_trace_info(int line, const char *format, ...);
void hdlclnh_trace_dbg(int line, const char *format, ...);

#define HDLCLNH_TRACE(trc, ...)											\
        do {														\
			tracepoint(com_ericsson_hdlclnh, trc, __VA_ARGS__);			\
        } while (0)


#define HDLCLNH_ERROR(txt, ...) \
        hdlclnh_trace_error(__LINE__, txt, ##__VA_ARGS__)

#define HDLCLNH_INFO(txt, ...) \
        hdlclnh_trace_info(__LINE__, txt, ##__VA_ARGS__)

#define HDLCLNH_DBG(txt, ...) \
        hdlclnh_trace_dbg(__LINE__, txt, ##__VA_ARGS__)

#else
#define HDLCLNH_TRACE(...)
#define HDLCLNH_ERROR(...)
#define HDLCLNH_INFO(...)
#define HDLCLNH_DBG(...)

#endif
