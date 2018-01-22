#ifdef LTTNG

void ulh_trace_error(char *file, int line, const char *format, ...);
void ulh_trace_info(char *file, int line, const char *format, ...);

#define ULH_TRACE(trc, ...)                                             \
        do {                                                            \
                tracepoint(com_ericsson_ulh_ecm, trc, __VA_ARGS__);     \
        } while (0)



#define ULH_TRACE_ERROR(txt, ...) \
        ulh_trace_error(__FILE__, __LINE__, txt, ##__VA_ARGS__)
#define ULH_TRACE_INFO(txt, ...) \
        ulh_trace_info(__FILE__, __LINE__, txt, ##__VA_ARGS__)

#else
#define ULH_TRACE(...)
#define ULH_TRACE_ERROR(...)
#define ULH_TRACE_INFO(...)
#endif
