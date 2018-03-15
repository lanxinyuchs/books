#undef TRACEPOINT_PROVIDER
#define TRACEPOINT_PROVIDER com_ericsson_my_provider

#if !defined(_TRACEPOINT_COM_ERICSSON_MY_PROVIDER_H) || defined(TRACEPOINT_HEADER_MULTI_READ)
#define _TRACEPOINT_COM_ERICSSON_MY_PROVIDER_H

#ifdef __cplusplus
extern "C" {
#endif

/*
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

#include <lttng/tracepoint.h>

TRACEPOINT_EVENT(com_ericsson_my_provider, ERROR,
		 TP_ARGS(char *, file,
                         int, line,
                         const char *,  msg),
                 TP_FIELDS(
                           ctf_string(file, file)
                           ctf_integer(int, line, line)
                           ctf_string(msg, msg)
	)
)

TRACEPOINT_LOGLEVEL(com_ericsson_my_provider, ERROR,TRACE_ERR)

TRACEPOINT_EVENT(com_ericsson_my_provider, ENTER,
		 TP_ARGS(const char *, file,
                         int, line,
                         const char *,  msg),
                 TP_FIELDS(
                           ctf_string(file, file)
                           ctf_integer(int, line, line)
                           ctf_string(msg, msg)
			   )
)
TRACEPOINT_LOGLEVEL(com_ericsson_my_provider, ENTER, TRACE_DEBUG)

TRACEPOINT_EVENT(com_ericsson_my_provider, RETURN,
		 TP_ARGS(const char *, file,
                         int, line),
                 TP_FIELDS(
                           ctf_string(file, file)
                           ctf_integer(int, line, line)
	)
)
TRACEPOINT_LOGLEVEL(com_ericsson_my_provider, RETURN, TRACE_DEBUG)

TRACEPOINT_EVENT(com_ericsson_my_provider, INFO,
		 TP_ARGS(const char *, file,
                         int, line,
                         const char *, msg),
                 TP_FIELDS(
                           ctf_string(file, file)
                           ctf_integer(int, line, line)
                           ctf_string(msg, msg)
	)
)
TRACEPOINT_LOGLEVEL(com_ericsson_my_provider, INFO, TRACE_INFO)

TRACEPOINT_EVENT(com_ericsson_my_provider, TRACE1,
		 TP_ARGS(const char *, file,
                         int, line,
                         const char *,  msg),
                 TP_FIELDS(
                           ctf_string(file, file)
                           ctf_integer(int, line, line)
                           ctf_string(msg, msg)
	)
)
TRACEPOINT_LOGLEVEL(com_ericsson_my_provider, TRACE1, TRACE_DEBUG)

TRACEPOINT_EVENT(com_ericsson_my_provider, TRACE2,
		 TP_ARGS(const char *, file,
                         int, line,
                         const char *,  msg),
                 TP_FIELDS(
                           ctf_string(file, file)
                           ctf_integer(int, line, line)
                           ctf_string(msg, msg)
	)
)
TRACEPOINT_LOGLEVEL(com_ericsson_my_provider, TRACE2, TRACE_DEBUG)

TRACEPOINT_EVENT(com_ericsson_my_provider, TRACE3,
		 TP_ARGS(const char *, file,
                         int, line,
                         const char *, msg),
                 TP_FIELDS(
                           ctf_string(file, file)
                           ctf_integer(int, line, line)
                           ctf_string(msg, msg)
	)
)

TRACEPOINT_LOGLEVEL(com_ericsson_my_provider, TRACE3, TRACE_DEBUG)


TRACEPOINT_EVENT(com_ericsson_my_provider, TRACE4,
		 TP_ARGS(const char *, file,
                         int, line,
                         const char *,  msg),
                 TP_FIELDS(
                           ctf_string(file, file)
                           ctf_integer(int, line, line)
                           ctf_string(msg, msg)
       )
)
TRACEPOINT_LOGLEVEL(com_ericsson_my_provider, TRACE4, TRACE_DEBUG)

TRACEPOINT_EVENT(com_ericsson_my_provider, TRACE5,
		 TP_ARGS(const char *, file,
                         int, line,
                         const char *,  msg),
                 TP_FIELDS(
                           ctf_string(file, file)
                           ctf_integer(int, line, line)
                           ctf_string(msg, msg)
	)
)
TRACEPOINT_LOGLEVEL(com_ericsson_my_provider, TRACE5, TRACE_DEBUG)

TRACEPOINT_EVENT(com_ericsson_my_provider, TRACE6,
		 TP_ARGS(const char *, file,
                         int, line,
                         const char *,  msg),
                 TP_FIELDS(
                           ctf_string(file, file)
                           ctf_integer(int, line, line)
                           ctf_string(msg, msg)
	)
)
TRACEPOINT_LOGLEVEL(com_ericsson_my_provider, TRACE6, TRACE_DEBUG)

TRACEPOINT_EVENT(com_ericsson_my_provider, TRACE7,
		 TP_ARGS(const char *, file,
                         int, line,
                         const char *,  msg),
                 TP_FIELDS(
                           ctf_string(file, file)
                           ctf_integer(int, line, line)
                           ctf_string(msg, msg)
	)
)
TRACEPOINT_LOGLEVEL(com_ericsson_my_provider, TRACE7, TRACE_DEBUG)

TRACEPOINT_EVENT(com_ericsson_my_provider, TRACE8,
		 TP_ARGS(const char *, file,
                         int, line,
                         const char *,  msg),
                 TP_FIELDS(
                           ctf_string(file, file)
                           ctf_integer(int, line, line)
                           ctf_string(msg, msg)
	)
)
TRACEPOINT_LOGLEVEL(com_ericsson_my_provider, TRACE8, TRACE_DEBUG)

TRACEPOINT_EVENT(com_ericsson_my_provider, TRACE9,
		 TP_ARGS(const char *, file,
                         int, line,
                         const char *,  msg),
                 TP_FIELDS(
                           ctf_string(file, file)
                           ctf_integer(int, line, line)
                           ctf_string(msg, msg)
	)
)
TRACEPOINT_LOGLEVEL(com_ericsson_my_provider, TRACE9, TRACE_DEBUG)

TRACEPOINT_EVENT(com_ericsson_my_provider, STATE_CHANGE,
		 TP_ARGS(const char *, file,
                         int, line,
                         const char *,  msg),
                 TP_FIELDS(
                           ctf_string(file, file)
                           ctf_integer(int, line, line)
                           ctf_string(msg, msg)
	)
)
TRACEPOINT_LOGLEVEL(com_ericsson_my_provider, STATE_CHANGE, TRACE_DEBUG)

TRACEPOINT_EVENT(com_ericsson_my_provider, BUS_SEND,
		 TP_ARGS(const char *, file,
                         int, line,
                         const char *,msg,
			 char*, data, size_t, datalen),
                 TP_FIELDS(
                           ctf_string(file, file)
                           ctf_integer(int, line, line)
                           ctf_string(msg, msg)
                           ctf_sequence(char, dataBusSend, data, size_t,
                                        datalen)
	)
)
TRACEPOINT_LOGLEVEL(com_ericsson_my_provider, BUS_SEND, TRACE_DEBUG)

TRACEPOINT_EVENT(com_ericsson_my_provider, BUS_RECEIVE,
		 TP_ARGS(char *, file,
                         int, line,
                         const char *,  msg,
			 const char*, data, size_t, datalen),
                 TP_FIELDS(
                           ctf_string(file, file)
                           ctf_integer(int, line, line)
                           ctf_string(msg, msg)
                           ctf_sequence(char, dataBusRec, data, size_t, datalen)
	)
)
TRACEPOINT_LOGLEVEL(com_ericsson_my_provider, BUS_RECIEVE, TRACE_DEBUG)


TRACEPOINT_EVENT(com_ericsson_my_provider, PARAM,
		 TP_ARGS(char *, file,
                         int, line,
                         const char *,  msg),
                 TP_FIELDS(
                           ctf_string(file, file)
                           ctf_integer(int, line, line)
                           ctf_string(msg, msg)
	)
)
TRACEPOINT_LOGLEVEL(com_ericsson_my_provider, PARAM, TRACE_DEBUG)

/********************************************************************/
/*    Add provider specific events and loglevels after this line    */

/*    End  provider specific events events                          */
/********************************************************************/

#endif /* _TRACEPOINT_COM_ERICSSON_MY_PROVIDER_H  */

#undef TRACEPOINT_INCLUDE_FILE
#define TRACEPOINT_INCLUDE_FILE com_ericsson_my_provider.h

/* This part must be outside ifdef protection */
#include <lttng/tracepoint-event.h>

/********************************************************************/
/*   Simplified trace macros starts here                           */

#define __SHORT_FILE__ __FILE__
#define MY_PROVIDER_ERROR(msg) tracepoint(com_ericsson_my_provider, ERROR,\
                                          __SHORT_FILE__, __LINE__, msg)

#define MY_PROVIDER_ENTER(msg) tracepoint(com_ericsson_my_provider, ENTER,\
					  __SHORT_FILE__, __LINE__, msg)

#define MY_PROVIDER_RETURN tracepoint(com_ericsson_my_provider, RETURN,\
				      __SHORT_FILE__, __LINE__);\
                           return

#define MY_PROVIDER_INFO(msg) tracepoint(com_ericsson_my_provider, INFO,\
					  __SHORT_FILE__, __LINE__, msg)

#define MY_PROVIDER_TRACE(group, msg) tracepoint(com_ericsson_my_provider,\
						 TRACE ## group,\
						 __SHORT_FILE__, __LINE__, msg)


#define MY_PROVIDER_TRACE_STATE(msg) tracepoint(com_ericsson_my_provider,\
						STATE_CHANGE,\
						__SHORT_FILE__, __LINE__, msg)

#define MY_PROVIDER_BUS_SEND(msg, data, len)\
 tracepoint(com_ericsson_my_provider, BUS_SEND,	__SHORT_FILE__, __LINE__,\
	    msg, data, len)

#define MY_PROVIDER_BUS_RECEIVE(msg, data, len)\
 tracepoint(com_ericsson_my_provider, BUS_RECEIVE, __SHORT_FILE__, __LINE__,\
	    msg, data, len)

#define MY_PROVIDER_PARAM(msg) tracepoint(com_ericsson_my_provider, PARAM,\
					  __SHORT_FILE__, __LINE__, msg)

/********************************************************************/
/*    Add provider specific macros after this line                  */


/*    End of macros                                                 */
/********************************************************************/

#ifdef __cplusplus 
}
#endif
