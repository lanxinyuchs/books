#include <stdio.h>
#include <signal.h>
#include <errno.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <fcntl.h>
#include <getopt.h>
#include <libgen.h>
#include <sys/file.h>
#include "rhd-common.h"


int rhd_try_lock(const char *name)
{
	int fd = -1;
	FILE *fp = NULL;
	char *lock_file;

	lock_file = malloc(strlen(name) + 16); /* /var/run + .pid */
	if (!lock_file)
		goto try_lock_error;
	snprintf(lock_file, strlen(name) + 16, "/var/run/%s.pid", name);

	/* create/check lock file */
	fd = open(lock_file, O_WRONLY | O_CREAT, S_IWUSR);
	if (fd == -1)
		goto try_lock_error;
	if (flock(fd, LOCK_EX | LOCK_NB))
		goto try_lock_error;
	fp = fdopen(fd, "w");
	if (!fp)
		goto try_lock_error;

	/* dump the pid */
	fprintf(fp, "%10d\n", getpid());
	fflush(fp);

	free(lock_file);

	return 0;

try_lock_error:

	if (lock_file)
		free(lock_file);
	if (fd != -1)
		close(fd);
	return -1;
}
