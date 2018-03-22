/**
 *   Handler for startup log snapshots.
 *
 *   @file trace_sup.c
 *
 *   Copyright (C) 2014-2015 by Ericsson AB. All rights reserved. The
 *   information in this document is the property of Ericsson. Except
 *   as specifically authorized in writing by Ericsson, the receiver
 *   of this document shall keep the information contained herein
 *   confidential and shall protect the same in whole or in part from
 *   disclosure and dissemination to third parties. Disclosure and
 *   disseminations to the receiver's employees shall only be made on
 *   a strict need to know basis.
 */

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <dirent.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <time.h>
#include <string.h>
#include <ftw.h>
#include <lttng/lttng.h>
#include <lttng/snapshot.h>
#include <signal.h>
#include <errno.h>
#include <assert.h>

#define TRACEPOINT_DEFINE
#include "com_ericsson_system_start.h"


#define OUTPUT_NAME    "startup"
#define SL2CURRENTSES  "bootlog"
#define LTTNG_SES_NAME "startuplog"
#define ENTRIES2KEEP   10

#define INFO(fmt, args...)				\
	fprintf(stderr, "Info: " fmt "\n", ## args)
#define ERR(fmt, args...)				\
	fprintf(stderr, "Error: " fmt "\n", ## args)
#define DBG(fmt, args...)				\
	fprintf(stderr, "DBG: " fmt "\n", ## args)

/* Path to stored restart logs */
static char output_path[PATH_MAX] = {0};

/* Index of current session output dir */
static int cur_dir_index = 0;


static int file_select(const struct dirent *d)
{
	char buf[PATH_MAX] = {0};
	struct stat st;

	if (strcmp(d->d_name, ".") == 0 ||
	    strcmp(d->d_name, "..") == 0) {
		return 0;
	} else {
		snprintf(buf, PATH_MAX, "%s/%s", output_path, d->d_name);
		if (lstat(buf, &st) < 0) {
			return 0;
		}
		if (!S_ISDIR(st.st_mode))
			return 0;
	}
	return 1;
}


static int myrm(const char *path, const struct stat *sb,
		int flag, struct FTW *ftwbuf) {

	int (*rm_func)(const char *);
	int rc;

	switch(flag) {
	case FTW_DP:
		rm_func = rmdir;
		break;
	default:
		rm_func = unlink;
	}
	if ((rc = rm_func(path)) != 0) {
		ERR("Not removed: %s\n", path);
	}
	return rc;
}


static int clean_outputs()
{
	int size, i, to_del, index, m;
	struct dirent **namelist;
	char buf[64] = {0};

	/* Scan dir, sort by time, lowest first in array */
	size = scandir(output_path, &namelist, &file_select, &versionsort);
	if (size < 0) {
		ERR("scandir failed, %m");
	} else {
		to_del = size - ENTRIES2KEEP;
		for (i = 0; i < size; i++) {
			index = (int) strtol(strrchr(namelist[i]->d_name, '-') + 1, NULL, 10);
			if (cur_dir_index == index)
				break;
		}
		m = i;
		if (i + 1 >= size && to_del > 0) {
			/* We are at the end of the array */
			/* delete overrun dirs */
			for (i = 0; i < to_del; i++) {
				sprintf(buf, "%s/%s", output_path, namelist[i]->d_name);
				if (nftw(buf, myrm, FOPEN_MAX, FTW_DEPTH) < 0) {
					ERR("Failed to clean path, %s", buf);
				}
			}
		}
		for (i = m+1; i < size; i++) {
			if (to_del > 0) {
				sprintf(buf, "%s/%s", output_path, namelist[i]->d_name);
				if (nftw(buf, myrm, FOPEN_MAX, FTW_DEPTH) < 0) {
					ERR("Failed to clean path, %s", buf);
				}
				to_del--;
			}
		}
		/* Free the array */
		for (i = 0; i < size; i++)
			free(namelist[i]);
	}
	free(namelist);
	return 0;
}


static char * create_output_dir(char *ses)
{
	char *buf, *p, *ptr, *buf2, *env;
	char sname[PATH_MAX] = {0};
	int index = 0, index1, index2;
	int size, i;
	struct dirent **namelist;

	/* Check that BOOTLOG_PATH env is set */
	if ((env = getenv("BOOTLOG_PATH")) == NULL) {
		fprintf(stderr, "ERROR: BOOTLOG_PATH is not set\n");
		ERR("BOOTLOG_PATH is not set");
		return NULL;
	}
	/* Trailing '/' is added for strchr purpose */
	snprintf(output_path, PATH_MAX, "%s/%s/", env, OUTPUT_NAME);

	/* Check that full path exists */
	p = strdup(output_path);
	ptr = strchr(p, '/');
	while(ptr != NULL) {
		*ptr = 0;
		/* skip first '/' */
		if (strlen(p) != 0) {
			if (access(p, F_OK) == -1 &&
			    mkdir(p, 0755)) {
				ERR("mkdir(%s) failed, %m", p);
			}
		}
		*ptr = '/';
		ptr = strchr(ptr + 1, '/');
	}
	free(p);

	/* Remove trailing '/' */
	output_path[strlen(output_path) - 1] = 0;

	/* Sort all dirs using versionsort -> dir1, dir2, ..., dir10,dir11..., dir20 */
	size = scandir(output_path, &namelist, &file_select, &versionsort);
	if (size < 0) {
		ERR("scandir failed, %m");
	} else if (size == 0) {
		/* No directories */
		index = 0;
	} else {
		for (i = 0; i < size; i++) {
			index1 = (int) strtol(strrchr(namelist[i]->d_name, '-') + 1, NULL, 10);
			if (i + 1 == size) {
				if (index1 >= ENTRIES2KEEP * 3)
					index = 0;
					else index = index1;
				break;
			}
			index2 = (int) strtol(strrchr(namelist[i + 1]->d_name, '-') + 1, NULL, 10);
			/* check if we have a gap,
			 * index2 is oldest
			 */
			if (index1 + 1 != index2) {
				index = index1;
				break;
			}
		}
		/* free array */
		for (i = 0; i < size; i++)
			free(namelist[i]);
	}
	free(namelist);

	asprintf(&buf, "%s/%s-%d", output_path, ses, index + 1);
	if (mkdir(buf, 0755) < 0) {
		ERR("mkdir(%s) failed, %m", buf);
		return NULL;
	}
	cur_dir_index = index + 1;

	/* create a symlink to current session output */
	asprintf(&buf2, "%s/" SL2CURRENTSES, output_path);
	if (access(buf2, F_OK) != -1)
		unlink(buf2);

	strcpy(sname, strrchr(buf, '/') - 1);
	sname[0] = '.';
	symlink(sname, buf2);
	free(buf2);

	return buf;
}


static int mychmod(const char *path, const struct stat *sb,
		   int flag, struct FTW *ftwbuf) {

	int rc;
	struct stat st;

	if (ftwbuf->level == 0)
		return 0;

	if (stat(path, &st) < 0)
		return -1;

	if ((rc = chmod(path, st.st_mode | S_IROTH | S_IXOTH)) < 0) {
		INFO("Permissions change failed: %s, err: %s\n",
		     path, strerror(errno));
	}
	return rc;
}


/* ===================================================================== */
/**
 *  Retry lttng_enable_channel, if needed.
 *
 *   @param handle   LTTng session handle
 *   @param channel  LTTng session channel
 *   @param retries  Number of times to retry, zero or greater.
 *
 *   @return         0 on successful execution, or LTTng error.
 *
 *   @par Globals:   --
 *
 *   @note lttng_enable_channel sometimes fails to begin with, but if
 *         one tries again it will succeed after some time.
 *         The root cause is not known, so this is a work-around.
 */
/* ===================================================================== */
static int retry_lttng_enable_channel(struct lttng_handle *handle,
                                      struct lttng_channel *channel,
                                      int retries)
{
   int i, loops, ret = 0;

   assert(retries >= 0);

   /* Run the loop at least once if the caller request no retries. */
   loops = (retries == 0) ? 1 : retries;

   for (i = 1; i <= loops; i++) {
      ret = lttng_enable_channel(handle, channel);
      if (ret == 0) {
         break;
      }
      else {
         INFO("Trying to enable channel %s, attempt %d of %d: %s (%d).",
              channel->name, i, loops, lttng_strerror(ret), ret);
         if (i == loops) {
            return ret;
         }
         else {
            /* Progressive delay. */
            usleep(50000 * i);
            continue;
         }
      }
   }

   return ret;
}


static int create_lttng_session()
{
	char ses[] = LTTNG_SES_NAME;
	int ret = 0;
	char *path = NULL;
	int i;
	struct lttng_handle *handle = NULL;
	struct lttng_channel channel;
	struct lttng_domain domain;
	struct lttng_event event;
	struct lttng_event_context ctx;

	memset(&event, 0, sizeof(event));
	memset(&domain, 0, sizeof(domain));
	domain.type = LTTNG_DOMAIN_UST;
	domain.buf_type = LTTNG_BUFFER_PER_UID;

	/* Wait for sessiond */
	for (i = 0; i < 100; i++) {
		if (lttng_session_daemon_alive())
			break;
		if (i + 1 == 100) {
			ret = -1;
			ERR("LTTng session daemon is dead");
			goto out;
		}
		usleep(500 * i);
	}

	/* Create dir for output */
	path = create_output_dir(ses);
	if (!path) {
		ret = -1;
		goto out;
	}
	/* Create LTTng session */
	ret = lttng_create_session(ses, path);
	if (ret < 0) {
		ERR("lttng_create_session failed for session:%s path:%s, %s (%d)",
		    ses, path, lttng_strerror(ret), ret);
		goto out;
	}


	handle = lttng_create_handle(ses, &domain);
	if (handle == NULL) {
		ret = -1;
		ERR("lttng_create_handle failed for session:%s", ses);
		goto out;
	}

	/* Channel for periodical flush of trace data */
	memset(&channel, 0, sizeof(channel));
	lttng_channel_set_default_attr(&domain, &channel.attr);
	channel.attr.subbuf_size = 32768; /* 32k */
	channel.attr.switch_timer_interval = 2000000; /* 2 sec */
	strcpy(channel.name, "ch-1");
	/* Restrict output in size */
	channel.attr.tracefile_size = 524288;
	channel.attr.tracefile_count = 1;

   ret = retry_lttng_enable_channel(handle, &channel, 6);
   if (ret < 0) {
      ERR("lttng_enable_channel failed for session %s, channel %s: %s",
               ses, channel.name, lttng_strerror(ret));
      goto out;
   }

	/* Add context, procname */
	memset(&ctx, 0, sizeof(ctx));
	ctx.ctx = LTTNG_EVENT_CONTEXT_PROCNAME;
	ret = lttng_add_context(handle, &ctx, NULL, channel.name);
	if (ret < 0) {
		ERR("Failed to add context to lttng channel, %s, %s (%d)",
		    event.name, lttng_strerror(ret), ret);
		goto out;
	}

	/* Enable events */
	strncpy(event.name, "com_ericsson_system_start:boot_time", LTTNG_SYMBOL_NAME_LEN);
	event.name[LTTNG_SYMBOL_NAME_LEN - 1] = 0;
	event.type = LTTNG_EVENT_TRACEPOINT;
	ret = lttng_enable_event(handle, &event, channel.name);
	if (ret < 0) {
		ERR("Failed to enable event, %s, %s (%d)",
		    event.name, lttng_strerror(ret), ret);
		goto out;
	}
	memset(&event, 0, sizeof(event));
	strncpy(event.name, "com_ericsson_trithread:TRACE9", LTTNG_SYMBOL_NAME_LEN);
	event.name[LTTNG_SYMBOL_NAME_LEN - 1] = 0;
	event.type = LTTNG_EVENT_TRACEPOINT;
	ret = lttng_enable_event(handle, &event, channel.name);
	if (ret < 0) {
		ERR("Failed to enable event, %s, %s (%d)",
		    event.name, lttng_strerror(ret), ret);
		goto out;
	}
	lttng_start_tracing(ses);

	/* Chmod output file structure, read and execute for others. */
	if (nftw(path, mychmod, FOPEN_MAX, FTW_DEPTH) < 0) {
		goto out;
	}

 out:
	if (handle)
		lttng_destroy_handle(handle);
	if (path)
		free(path);
	return ret;
}


int main()
{
	int ret = 0;

   /* Assign permissions so that only you have read/write access for files,
    * and read/write/search for directories you own. All others have read
    * access only to your files, and read/search access to your directories.
    */
	umask(0022);

	/* Create LTTng session, create dir with indexed name, symlink to dir */
	if ((ret = create_lttng_session()) < 0) {
		goto out;
	}
	event_system_start("cs-ee: startup event log ready");

	/* Count lttng outputs, if too many delete the oldest */
	if ((ret = clean_outputs()) < 0) {
		goto out;
	}

	pause();

out:
	return ret;
}
