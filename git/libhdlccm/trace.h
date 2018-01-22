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

void hdlc_trace_error(char *file, int line, const char *format, ...);
void hdlc_trace_info(char *file, int line, const char *format, ...);
void hdlc_trace_dbg(const char *format, ...);
void hdlc_print_header(char *name, char *pfx, uint8_t *p, int len);
void lnh_print_header(char *name, char *prfx, struct ulh_cm_msghdr*, uint8_t*);



#define HDLC_TRACE(trc, ...)											\
        do {														\
			tracepoint(com_ericsson_hdlc, trc, __VA_ARGS__);			\
        } while (0)



#define HDLC_ERROR(txt, ...) \
        hdlc_trace_error(__FILE__, __LINE__, txt, ##__VA_ARGS__)

#define HDLC_INFO(txt, ...) \
        hdlc_trace_info(__FILE__, __LINE__, txt, ##__VA_ARGS__)

#define HDLC_DBG(txt, ...) \
        hdlc_trace_dbg(__FILE__, __LINE__, txt, ##__VA_ARGS__)

#define HDLC_HEADER(n, p, d, l) \
		hdlc_print_header(n, p, (uint8_t*)d, (uint32_t)l);

#define LNH_HEADER(n, p, h, b) \
		lnh_print_header(n, p, (struct ulh_cm_msghdr*)h, (uint8_t*)b);

#else
#define HDLC_TRACE(...)
#define HDLC_ERROR(...)
#define HDLC_INFO(...)
#define HDLC_DBG(...)
#define HDLC_HEADER(...)
#define LNH_HEADER(...)
#endif

