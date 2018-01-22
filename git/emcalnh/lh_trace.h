#ifdef LTTNG

void mhp_trace_error(char *file, int line, const char *format, ...);
void mhp_trace_info(char *file, int line, const char *format, ...);
void mhp_trace_dbg(char *file, int line, const char *format, ...);


#define MHP_TRACE(trc, ...)											\
        do {														\
			tracepoint(com_ericsson_ulh, trc, __VA_ARGS__);			\
        } while (0)



#define MHP_ERROR(txt, ...) \
        mhp_trace_error(__FILE__, __LINE__, txt, ##__VA_ARGS__) 
#define MHP_INFO(txt, ...) \
        mhp_trace_info(__FILE__, __LINE__, txt, ##__VA_ARGS__) 
#define MHP_DBG(txt, ...) \
        mhp_trace_dbg(__FILE__, __LINE__, txt, ##__VA_ARGS__) 

#define MHP_HEADER(n, p, d, l) \
				mhp_print_header(n, p, (char*)d, (uint32_t)l);
#define LNH_HEADER(n, p, h, b) \
				lnh_print_header(n, p, (struct mhp_msghdr*)h, (uint8_t*)b);

#else
#define MHP_TRACE(...)
#define MHP_ERROR(...)
#define MHP_INFO(...)
#define MHP_DBG(...)
#define MHP_HEADER(...)
#define LNH_HEADER(...)
#endif

