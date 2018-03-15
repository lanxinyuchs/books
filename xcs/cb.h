#ifndef _CB_H
#define _CB_H

#include <stdio.h>

#include "libmama.h"

extern struct mama_callbacks cb;
extern void *cb_user_data;

#define CB(x, ...) do { \
	if (cb.x) (*cb.x)(cb_user_data, __VA_ARGS__); \
} while (0)

#define reboot_fmt(fmt, ...) do {                          \
	char *_s;                                              \
	if (asprintf(&_s, fmt, ##__VA_ARGS__) < 0)             \
		CB(reboot, "Unknown reason");                      \
	CB(reboot, _s);                                        \
} while (0)


#endif /* _CB_H */
