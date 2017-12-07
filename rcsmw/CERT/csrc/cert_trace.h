/* ----------------------------------------------------------------------
 * %CCaseFile:	cert_trace.h %
 * %CCaseRev:	/main/R3A/R5A/R6A/1 %
 * %CCaseDate:	2016-05-04 %
 * %CCaseDocNo: %
 * Author:	etxasta
 *
 * Short description: "Tracepoint provider" for the cert SECI program
 * 
 * ----------------------------------------------------------------------
 * %CCaseTemplateFile:	template.h %
 * %CCaseTemplateId: CCver: /main/1 %
 *
 * %CCaseCopyrightBegin%
 * Copyright (c) Ericsson AB 2014-2016 All rights reserved.
 * 
 * The information in this document is the property of Ericsson.
 * 
 * Except as specifically authorized in writing by Ericsson, the 
 * receiver of this document shall keep the information contained 
 * herein confidential and shall protect the same in whole or in 
 * part from disclosure and dissemination to third parties.
 * 
 * Disclosure and disseminations to the receivers employees shall 
 * only be made on a strict need to know basis.
 * %CCaseCopyrightEnd%
 *
 * ----------------------------------------------------------------------
 *
 * Revision history:
 *
 * Rev        Date        Name        What
 * -----      ----------  --------    --------------------------
 * R3A/1      2014-11-07  etxasta     Created, copied from appm
 * ----------------------------------------------------------------------
 */

#undef TRACEPOINT_PROVIDER
#define TRACEPOINT_PROVIDER com_ericsson_cert

#if !defined(_TRACEPOINT_CERT_TRACE_H) || defined(TRACEPOINT_HEADER_MULTI_READ)
#define _TRACEPOINT_CERT_TRACE_H

#ifdef __cplusplus
extern "C" {
#endif


#include <stdint.h>
#include <lttng/tracepoint.h>

TRACEPOINT_EVENT(com_ericsson_cert, info_trace,
                 TP_ARGS(char *, trace),
                 TP_FIELDS(ctf_string(trace, trace))
)
TRACEPOINT_LOGLEVEL(com_ericsson_cert, info_trace, TRACE_INFO)

TRACEPOINT_EVENT(com_ericsson_cert, error_trace,
                 TP_ARGS(char *, trace),
                 TP_FIELDS(ctf_string(trace, trace))
)
TRACEPOINT_LOGLEVEL(com_ericsson_cert, error_trace, TRACE_ERR)

TRACEPOINT_EVENT(com_ericsson_cert, debug_trace,
                 TP_ARGS(char *, trace),
                 TP_FIELDS(ctf_string(trace, trace))
)
TRACEPOINT_LOGLEVEL(com_ericsson_cert, debug_trace, TRACE_DEBUG)


#endif /* _TRACEPOINT_CERT_TRACE_H */

#undef TRACEPOINT_INCLUDE_FILE
#define TRACEPOINT_INCLUDE_FILE ./cert_trace.h

#include <lttng/tracepoint-event.h>

#define INFO( fmt,...)  TRACE_HELPER(info_trace, fmt, __VA_ARGS__)
#define ERROR( fmt,...) TRACE_HELPER(error_trace, fmt, __VA_ARGS__)
#define DEBUG( fmt,...) TRACE_HELPER(debug_trace, fmt, __VA_ARGS__)

#define TRACE_HELPER(type, fmt,...) do {	\
  char *err_str;\
  asprintf(&err_str,"%s:%d:%s():" fmt, __FILE__,__LINE__, __func__, __VA_ARGS__); \
  tracepoint(com_ericsson_cert, type , err_str);\
  free(err_str);\
  } while(0)


