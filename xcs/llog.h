/**
 * @file llog.h
 * @brief Llog API
 */
/* ---------------------------------------------------------------------------
 *
 * © Ericsson AB 2014 All rights reserved.
 * The information in this document is the property of Ericsson.  Except
 * as specifically authorized in writing by Ericsson, the receiver of
 * this document shall keep the information contained herein confidential
 * and shall protect the same in whole or in part from disclosure and
 * dissemination to third parties.  Disclosure and disseminations to the
 * receivers employees shall only be made on a strict need to know basis.
 *
 * ---------------------------------------------------------------------------
 */
#ifndef LLOG_H
#define LLOG_H

#include <stdio.h>
#include <syslog.h>
#include <sys/types.h>
#include <unistd.h>
#include <time.h>
#include <string.h>

/**
 * Write one entry to llog.
 *
 * If a param is not applicable, pass NULL unless otherwise stated.
 *
 * @param reason - The reason for the reboot.
 * @param program - The program which ordered or caused the reboot.
 * @param pid - The pid of the program which ordered or caused the reboot.
 * @param rank - warm or cold restart
 * @param signal - The signal which caused the crash if applicable, else 0.
 * @param pmd - Where the pmd of the crash program is available, if applicable.
 * @param extra - Extra information.
 *
 */
static inline void llog_write(char *reason, char *program, pid_t pid,
                              char *rank, int signal, char *pmd, char *extra)
{
	char *notset = "-";
	char datestring[4+1+2+1+2+1+2+1+2+1+2+1];
	time_t now = time(NULL);
	struct tm *tm = localtime(&now);
	char sig_string[3];

	if (signal) {
		snprintf(sig_string, 3, "%d", signal);
	}
	else {
		sig_string[0] = '-';
		sig_string[1] = '\0';
	}

	if (tm) {
		sprintf(datestring, "%.04d-%.02d-%.02d %.02d:%.02d:%.02d",
		        tm->tm_year + 1900, tm->tm_mon + 1, tm->tm_mday,
	            tm->tm_hour, tm->tm_min, tm->tm_sec);
	}
	else {
		snprintf(datestring, 4+1+2+1+2+1+2+1+2+1+2+1, "%d", now);
	}

	if (!reason) reason = notset;
	if (!program) program = notset;
	if (!rank) rank = notset;
	if (!pmd) pmd = notset;
	if (!extra) extra = notset;

	openlog("elog", LOG_NDELAY, LOG_LOCAL0);
	syslog(LOG_ERR, "$ %s $ %s $ %s $ %d $ %s $ %s $ %s $ %s",
	       reason, datestring, program, pid, rank, sig_string, pmd, extra);
	closelog();
}

#endif /* LLOG_H */
