#ifndef RBS_ERROR_H__
#define RBS_ERROR_H__

#include <uapi/linux/rbs/rbs-error.h>

extern int rbs_error_t(unsigned int source, unsigned int code,
		       const char *fm, ...);
extern int rbs_error_b(unsigned int source, unsigned int code,
		       void *data, unsigned int data_len);


#endif /* RBS_ERROR_H__ */
