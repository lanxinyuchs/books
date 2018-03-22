#undef TRACEPOINT_PROVIDER
#define TRACEPOINT_PROVIDER com_ericsson_plf_lttng_test1

#undef TRACEPOINT_INCLUDE_FILE
#define TRACEPOINT_INCLUDE_FILE ./com_ericsson_plf_lttng_test1.h

#ifdef __cplusplus
extern "C" {
#endif

#if !defined(_COM_ERICSSON_PLF_LTTNG_TEST1_H) || defined(TRACEPOINT_HEADER_MULTI_READ)
#define _COM_ERICSSON_PLF_LTTNG_TEST1_H

#include <lttng/tracepoint.h>

     TRACEPOINT_EVENT(com_ericsson_plf_lttng_test1, EMERG_Test, 
		      TP_ARGS( const char *, trace_string ),
		      TP_FIELDS( ctf_string(string, trace_string) )
	  )
     TRACEPOINT_LOGLEVEL(com_ericsson_plf_lttng_test1, EMERG_Test, TRACE_EMERG)

     TRACEPOINT_EVENT(com_ericsson_plf_lttng_test1, ALERT_Test, 
		      TP_ARGS( const char *, trace_string ),
		      TP_FIELDS( ctf_string(string, trace_string) )
	  )
     TRACEPOINT_LOGLEVEL(com_ericsson_plf_lttng_test1, ALERT_Test, TRACE_ALERT)

     TRACEPOINT_EVENT(com_ericsson_plf_lttng_test1, CRIT_Test, 
		      TP_ARGS( const char *, trace_string ),
		      TP_FIELDS( ctf_string(string, trace_string) )
	  )
     TRACEPOINT_LOGLEVEL(com_ericsson_plf_lttng_test1, CRIT_Test, TRACE_CRIT)

     TRACEPOINT_EVENT(com_ericsson_plf_lttng_test1, ERR_Test, 
		      TP_ARGS( const char *, trace_string ),
		      TP_FIELDS( ctf_string(string, trace_string) )
	  )
     TRACEPOINT_LOGLEVEL(com_ericsson_plf_lttng_test1, ERR_Test, TRACE_ERR)

     TRACEPOINT_EVENT(com_ericsson_plf_lttng_test1, WARNING_Test, 
		      TP_ARGS( const char *, trace_string ),
		      TP_FIELDS( ctf_string(string, trace_string) )
	  )
     TRACEPOINT_LOGLEVEL(com_ericsson_plf_lttng_test1, WARNING_Test, TRACE_WARNING)

     TRACEPOINT_EVENT(com_ericsson_plf_lttng_test1, NOTICE_Test, 
		      TP_ARGS( const char *, trace_string ),
		      TP_FIELDS( ctf_string(string, trace_string) )
	  )
     TRACEPOINT_LOGLEVEL(com_ericsson_plf_lttng_test1, NOTICE_Test, TRACE_NOTICE)

     TRACEPOINT_EVENT(com_ericsson_plf_lttng_test1, INFO_Test, 
		      TP_ARGS( const char *, trace_string ),
		      TP_FIELDS( ctf_string(string, trace_string) )
	  )
     TRACEPOINT_LOGLEVEL(com_ericsson_plf_lttng_test1, INFO_Test, TRACE_INFO)

     TRACEPOINT_EVENT(com_ericsson_plf_lttng_test1, DEBUG_Test, 
		      TP_ARGS( const char *, trace_string ),
		      TP_FIELDS( ctf_string(string, trace_string) )
	  )
     TRACEPOINT_LOGLEVEL(com_ericsson_plf_lttng_test1, DEBUG_Test, TRACE_DEBUG)

     TRACEPOINT_EVENT(com_ericsson_plf_lttng_test1, Overload_Test, 
		      TP_ARGS(
			   const char *, trace_string,
			   int, current_iter,
			   int, max_iter,
			   int, current_trace,
			   int, max_trace
			   ),
		      TP_FIELDS(
			   ctf_string(string, trace_string)
			   ctf_integer(int, current_iter, current_iter)
			   ctf_integer(int, max_iter, max_iter)
			   ctf_integer(int, current_trace, current_trace)
			   ctf_integer(int, max_trace, max_trace)
			   )
	  )
     TRACEPOINT_LOGLEVEL(com_ericsson_plf_lttng_test1, Overload_Test, TRACE_INFO)

     TRACEPOINT_EVENT(com_ericsson_plf_lttng_test1, Multi_Seg_Test, 
		      TP_ARGS( const char *, trace_string ),
		      TP_FIELDS( ctf_string(string, trace_string) )
	  )
     TRACEPOINT_LOGLEVEL(com_ericsson_plf_lttng_test1, Multi_Seg_Test, TRACE_INFO)

     TRACEPOINT_EVENT(com_ericsson_plf_lttng_test1, Binary_Test, 
		      TP_ARGS(
			   const char *, trace_string,
			   char *, binary_data,
			   size_t, binary_len
			   ),
		      TP_FIELDS(
			   ctf_string(string, trace_string)
			   ctf_sequence(char, sequence, binary_data, size_t, binary_len)
			   )
	  )
     TRACEPOINT_LOGLEVEL(com_ericsson_plf_lttng_test1, Binary_Test, TRACE_INFO)

#endif /* _COM_ERICSSON_PLF_LTTNG_TEST1_H */

#include <lttng/tracepoint-event.h>

#ifdef __cplusplus
}
#endif
