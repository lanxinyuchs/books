#include <syslog.h>
#include <stdio.h>
#include <sys/types.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdarg.h>

#include "log.h"

void log_init()
{
	openlog("mama", LOG_PERROR | LOG_PID, LOG_USER);
}

void log_err(const char *fmt, ...)
{
	va_list args;

	va_start(args, fmt);
	vsyslog(LOG_ERR, fmt, args);
	va_end(args);
}

void log_info(const char *fmt, ...)
{
	va_list args;

	va_start(args, fmt);
	vsyslog(LOG_INFO, fmt, args);
	va_end(args);
}
