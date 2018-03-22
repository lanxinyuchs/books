#ifdef LTTNG

void mdu_trace_error(char *file, int line, const char *format, ...);
void mdu_trace_info(char *file, int line, const char *format, ...);

#define MDU_TRACE(trc, ...)											\
        do {														\
			tracepoint(com_ericsson_mdu, trc, __VA_ARGS__);			\
        } while (0)



#define MDU_ERROR(txt, ...) \
        mdu_trace_error(__FILE__, __LINE__, txt, ##__VA_ARGS__) 
#define MDU_INFO(txt, ...) \
        mdu_trace_info(__FILE__, __LINE__, txt, ##__VA_ARGS__) 

/*
static void mhp_print_header(char *name, const char *prfx,
                             char *data, uint32_t length)
*/

#define MDU_HEADER(n, p, d, l) mdu_print_header(n, p, (char*)d, (uint32_t)l)

#else
#define MDU_TRACE(...)
#define MDU_ERROR(...)
#define MDU_INFO(...)
#define MDU_HEADER(...)
#endif

