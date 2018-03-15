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

#ifndef _ECB_TRCAE_H_
#define _ECB_TRCAE_H_

extern void hdlc_print_header(char*, char*, uint8_t*, int);

extern void ecb_trace_dbg(const char *format, ...);
extern void ecb_trace_info(const char *format, ...);
extern void ecb_trace_error(const char *format, ...);

extern void a4c_trace_debug(char *text, uint8_t *p, int length);
extern void a4c_trace_info(const char *format, ...);
extern void a4c_trace_error(const char *format, ...);


#define ECB_ERROR(...) ecb_trace_error(__VA_ARGS__);
#define ECB_TRACE(...) ecb_trace_info(__VA_ARGS__);
#define ECB_DEBUG(...) ecb_trace_dbg(__VA_ARGS__);

#define A4C_ERROR(...) a4c_trace_error(__VA_ARGS__);
#define A4C_TRACE(...) a4c_trace_info(__VA_ARGS__);
#define A4C_DEBUG(...) a4c_trace_debug(__VA_ARGS__);

#endif
