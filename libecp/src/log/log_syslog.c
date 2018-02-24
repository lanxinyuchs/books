/**
 *   Copyright (C) 2014 by Ericsson AB. All rights reserved. The
 *   information in this document is the property of Ericsson. Except
 *   as specifically authorized in writing by Ericsson, the receiver
 *   of this document shall keep the information contained herein
 *   confidential and shall protect the same in whole or in part from
 *   disclosure and dissemination to third parties. Disclosure and
 *   disseminations to the receiver's employees shall only be made on
 *   a strict need to know basis.
 */

#include <syslog.h>
#include <stdarg.h>

#include "log.h"

#define DAEMON_NAME "libecp"
#define LOGLEVEL    LOG_INFO

static int _initialized;

void log_err(const char * fmt, ...)
{
	va_list arg;

	if (!_initialized) {
		setlogmask(LOG_UPTO(LOGLEVEL));
		openlog(DAEMON_NAME, LOG_CONS | LOG_PID, LOG_USER);
		_initialized = 1;
	}

	va_start(arg, fmt);
	vsyslog(LOG_ERR, fmt, arg);
	va_end(arg);
}

void log_info(const char * fmt, ...)
{
	va_list arg;

	if (!_initialized) {
		setlogmask(LOG_UPTO(LOGLEVEL));
		openlog(DAEMON_NAME, LOG_CONS | LOG_PID, LOG_USER);
		_initialized = 1;
	}

	va_start(arg, fmt);
	vsyslog(LOG_INFO, fmt, arg);
	va_end(arg);
}
