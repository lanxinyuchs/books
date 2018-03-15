/**
 *   Tracepoint for tracking time of system start up.
 *
 *   @file com_ericsson_system_start.h
 *
 *   [Enter a longer description about the file. More than one line
 *   can be used, but leave an empty line before the copyright line.]
 *
 *   [All text within [...] should be changed or removed.]
 *
 *   Copyright (C) 2014 by Ericsson AB. All rights reserved. The
 *   information in this document is the property of Ericsson. Except
 *   as specifically authorized in writing by Ericsson, the receiver
 *   of this document shall keep the information contained herein
 *   confidential and shall protect the same in whole or in part from
 *   disclosure and dissemination to third parties. Disclosure and
 *   disseminations to the receiver's employees shall only be made on
 *   a strict need to know basis.
 */

#undef TRACEPOINT_PROVIDER
#define TRACEPOINT_PROVIDER com_ericsson_system_start

#undef TRACEPOINT_INCLUDE
#define TRACEPOINT_INCLUDE "./com_ericsson_system_start.h"

#if !defined(_COM_ERICSSON_SYSTEM_START_H) || defined(TRACEPOINT_HEADER_MULTI_READ)
#define _COM_ERICSSON_SYSTEM_START_H

#ifdef __cplusplus
extern "C" {
#endif

#include <lttng/tracepoint.h>

/**
 * Event to track start up time of different sub-systems,
 * componets, etc. Always enabled - not for excessive
 * usage.
 * NOTE: It is recommended to call macro below, event_system_start(),
 *       instead of calling tracepoint() directly. Then it is much
 *       easier to modify tracepoint if required so.
 */
TRACEPOINT_EVENT(com_ericsson_system_start, boot_time,
	TP_ARGS( char *, text ),
	TP_FIELDS( ctf_string(msg, text) )
)
TRACEPOINT_LOGLEVEL(com_ericsson_system_start, boot_time, TRACE_INFO)

#endif /* _COM_ERICSSON_SYSTEM_START_H */

#include <lttng/tracepoint-event.h>

/**
 * event_system_start
 *
 * This macro should be used to track start up time. Insert it
 * at points where a time tag is wanted.
 *
 * @__str - A final message(string). If needed use sprintf prior this call
 *         to insert dynamic values.
 */
#define event_system_start(__str)				\
	tracepoint(com_ericsson_system_start, boot_time, __str)

#ifdef __cplusplus
}
#endif
