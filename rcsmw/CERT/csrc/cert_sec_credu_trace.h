/* ----------------------------------------------------------------------
 * %CCaseFile:	cert_sec_credu_trace.h %
 * %CCaseRev:	/main/R11A/1 %
 * %CCaseDate:	2017-09-12 %
 * %CCaseDocNo: %
 * Author:	ekurnik
 *
 * Short description: "Tracepoint provider" for the cert sec_credu_api interface
 * 
 * ----------------------------------------------------------------------
 * %CCaseTemplateFile:	template.h %
 * %CCaseTemplateId: CCver: /main/1 %
 *
 * %CCaseCopyrightBegin%
 * Copyright (c) Ericsson AB 2017 All rights reserved.
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
 * Rev          Date        Name        What
 * -----      ----------  --------    --------------------------
 * R11A/1      2017-09-12  ekurnik     Created
 * ----------------------------------------------------------------------
 */

#undef TRACEPOINT_PROVIDER
#define TRACEPOINT_PROVIDER com_ericsson_cert_seccredu

#if !defined(_TRACEPOINT_CERT_SEC_CREDU_TRACE_H) || defined(TRACEPOINT_HEADER_MULTI_READ)
#define _TRACEPOINT_CERT_SEC_CREDU_TRACE_H

#ifdef __cplusplus
extern "C" {
#endif


#include <stdint.h>
#include <lttng/tracepoint.h>

TRACEPOINT_EVENT(com_ericsson_cert_seccredu, info_trace,
                 TP_ARGS(char *, trace),
                 TP_FIELDS(ctf_string(trace, trace))
)
TRACEPOINT_LOGLEVEL(com_ericsson_cert_seccredu, info_trace, TRACE_INFO)

TRACEPOINT_EVENT(com_ericsson_cert_seccredu, error_trace,
                 TP_ARGS(char *, trace),
                 TP_FIELDS(ctf_string(trace, trace))
)
TRACEPOINT_LOGLEVEL(com_ericsson_cert_seccredu, error_trace, TRACE_ERR)

TRACEPOINT_EVENT(com_ericsson_cert_seccredu, debug_trace,
                 TP_ARGS(char *, trace),
                 TP_FIELDS(ctf_string(trace, trace))
)
TRACEPOINT_LOGLEVEL(com_ericsson_cert_seccredu, debug_trace, TRACE_DEBUG)


#endif /* _TRACEPOINT_CERT_SEC_CREDU_TRACE_H */

#undef TRACEPOINT_INCLUDE_FILE
#define TRACEPOINT_INCLUDE_FILE ./cert_sec_credu_trace.h

#include <lttng/tracepoint-event.h>


