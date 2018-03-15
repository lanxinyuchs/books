/******************************************************************************
 *
 *      COPYRIGHT (C)                 Ericsson Radio Systems AB, Sweden
 *
 *      The copyright to the computer program(s) herein is the property
 *      of Ericsson Radio Systems AB.
 *
 *      The program(s) may be used and/or copied only with the written
 *      permission from Ericsson Radio Systems AB or in accordance with
 *      the terms and conditions stipulated in the agreement/contract
 *      under which the program(s) have been supplied.
 *
 *****************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <termios.h>
#include <errno.h>
#include <time.h>
#include <pthread.h>
#include <sys/inotify.h>

#include <sys/prctl.h>

#include "ecb_dev.h"
#include "ecb_mux.h"

#include "ecb.h"

#include "log.h"

#ifndef CLOCK_MONOTONIC_RAW
#define CLOCK_MONOTONIC_RAW CLOCK_MONOTONIC
#warning "CLOCK_MONOTONIC_RAW not defined using CLOCK_MONOTONIC"
#endif


static pthread_mutex_t local_lock = PTHREAD_MUTEX_INITIALIZER;


int ecb_dev_init(void **handle, const char *name)
{
	struct ecb_ctx *ctx;
	struct stat     st;
	char proc_name_buffer[17] = { 0 };

	/* Allocate memory for context. */
	ctx = (struct ecb_ctx*) calloc(1, sizeof(struct ecb_ctx));
	if (!ctx) {
		log_err(ECB_DEFAULT_PREFIX, "calloc() failed");
		goto err_calloc;
	}

	ctx->log_prefix = calloc(1, ECB_MAX_PREFIX_SIZE * sizeof(char));
	if(!ctx->log_prefix) {
		log_err(ECB_DEFAULT_PREFIX, "prefix calloc() failed");
	}

	if(prctl(PR_GET_NAME, proc_name_buffer, 0, 0, 0) != 0) {
		if(ctx->log_prefix)
			free(ctx->log_prefix);
		ctx->log_prefix = NULL;
		log_info(ctx->log_prefix,
		        "unable to get process name errno: %d", errno);
	}
#ifdef _LTTNG_
	if (snprintf(ctx->log_prefix, ECB_MAX_PREFIX_SIZE,
	             "{%s/%d} {%s}", proc_name_buffer, getpid(), name) < 0) {
		if(ctx->log_prefix)
			free(ctx->log_prefix);
		ctx->log_prefix = NULL;
		log_info(ctx->log_prefix,
		        "unable to concatenate log prefix");
	}
#else /*syslog*/

	if (snprintf(ctx->log_prefix, ECB_MAX_PREFIX_SIZE,
	             "{%s} {%s}", proc_name_buffer, name) < 0) {
		if(ctx->log_prefix)
			free(ctx->log_prefix);
		ctx->log_prefix = NULL;
		log_info(ctx->log_prefix,
		        "unable to concatenate log prefix");
	}
        /* TODO check if thread already created */
#endif
	/* Open the UART device. */
	if (stat(name, &st) == -1) {
		log_err(ctx->log_prefix, "stat(\"%s\") failed, errno: %d",
		       name, errno);
		goto err_open;
	}
	ctx->dev.fd = open(name, O_RDWR | O_NOCTTY | O_NONBLOCK | O_SYNC);
	if (ctx->dev.fd < 0) {
		log_err(ctx->log_prefix, "open(\"%s\") failed, errno: %d",
		       name, errno);
		goto err_open;
	}

	/* Use default configuration. */
	if (tcgetattr(ctx->dev.fd, &ctx->dev.ts) == -1) {
		log_err(ctx->log_prefix, "tcgetattr() failed, errno: %d", errno);
		goto err_cfg;
	}
	cfmakeraw(&ctx->dev.ts);
	if (cfsetispeed(&ctx->dev.ts, B115200) == -1) {
		log_err(ctx->log_prefix, "cfsetispeed() failed, errno: %d", errno);
		goto err_cfg;
	}
	if (cfsetospeed(&ctx->dev.ts, B115200) == -1) {
		log_err(ctx->log_prefix, "cfsetospeed() failed, errno: %d", errno);
		goto err_cfg;
	}
	ctx->dev.ts.c_cflag &= ~CSIZE;
	ctx->dev.ts.c_cflag |= CS8;
	ctx->dev.ts.c_cflag &= ~PARENB;
	ctx->dev.ts.c_cflag &= ~CSTOPB;
	ctx->dev.ts.c_cflag |= CLOCAL | CREAD;

	ctx->unc.fcs_be = 0;
	ctx->unc.n_oflags = 1;
	ctx->unc.prefix.size = 0;
	ctx->unc.prefix.data = 0;

	*handle = ctx;
	return 0;

err_cfg:
	close(ctx->dev.fd);
err_open:
	if(ctx->log_prefix) {
		free(ctx->log_prefix);
	}
	free(ctx);
err_calloc:
	*handle = 0;
	return -1;
}

int ecb_dev_shutdown(void *handle)
{
	struct ecb_ctx *ctx = (struct ecb_ctx*) handle;

	if (ctx) {

		close(ctx->dev.fd);
		if(ctx->log_prefix) {
			free(ctx->log_prefix);
		}
		free(ctx);
	}

	return 0;
}


int ecb_dev_begin(void *handle)
{
	struct ecb_ctx *ctx = (struct ecb_ctx*) handle;

	if (!ctx) {
		log_err(ECB_DEFAULT_PREFIX,
		        "ecb_dev_begin() called with NULL pointer");
		return -1;
	}

	/* Lock the local lock for threads in this process. */
	if (pthread_mutex_lock(&local_lock) != 0) {
		log_err(ctx->log_prefix, "pthread_mutex_lock() failed, errno: %d",
		       errno);
		return -1;
	}

	/* Lock the global lock for processes in this system. */
	if (lockf(ctx->dev.fd, F_LOCK, 0) == -1) {
		log_err(ctx->log_prefix, "lockf(F_LOCK) failed, errno: %d", errno);
		pthread_mutex_unlock(&local_lock);
		return -1;
	}

	/* Set MUX according to program. */
	if (ecb_mux_set(handle) == -1) {
		ecb_dev_end(handle);
		return -1;
	}

	/* Set device parameter according to program. */
	if (ecb_dev_set(handle) == -1) {
		ecb_dev_end(handle);
		return -1;
	}

	/* Flush any garbage. */
	if (ecb_dev_flush(handle) == -1) {
		ecb_dev_end(handle);
		return -1;
	}

	return 0;
}

int ecb_dev_end(void *handle)
{
	struct ecb_ctx *ctx = (struct ecb_ctx*) handle;

	if (!ctx) {
		log_err(ECB_DEFAULT_PREFIX,
		        "ecb_dev_end() called with NULL pointer");
		return -1;
	}

	/* Unlock the global lock for processes in this system. */
	if (lockf(ctx->dev.fd, F_ULOCK, 0) == -1) {
		log_err(ctx->log_prefix, "lockf(F_ULOCK) failed, errno: %d", errno);
		pthread_mutex_unlock(&local_lock);
		return -1;
	}

	/* Unlock the local lock for threads in this process. */
	if (pthread_mutex_unlock(&local_lock) != 0) {
		log_err(ctx->log_prefix, "pthread_mutex_unlock() failed, errno: %d",
		       errno);
		return -1;
	}

	return 0;
}

#ifndef ECB_DEBUG
#define ecb_hex_dump(A, B, C)
#else
static void ecb_hex_dump(char *prefix, const void *data, uint32_t size)
{
	uint32_t idx;

	printf("%s: ", prefix);
	for (idx = 0; idx < size; idx++) {
		printf("%02x ", ((uint8_t*) data)[idx]);
	}
	printf("\n");
}
#endif

int ecb_dev_read(void *handle, void *data, uint32_t *size)
{
	struct ecb_ctx *ctx = (struct ecb_ctx*) handle;
	int             len;

	if (!ctx) {
		log_err(ECB_DEFAULT_PREFIX,
		        "ecb_dev_read() called with NULL pointer");
		return -1;
	}

	len = read(ctx->dev.fd, data, (size_t) *size);
	if (len == -1) {
		if (errno == EAGAIN || errno == EWOULDBLOCK) {
			*size = 0; /* Let the caller decide what to do now. */
			return 0;
		}
		log_info(ctx->log_prefix, "read() failed, errno: %d", errno);
		return -1;
	}
	*size = len;
	ecb_hex_dump("RX", data, *size);

	return 0;
}

int ecb_dev_write(void *handle, const void *data, uint32_t size)
{
	struct ecb_ctx *ctx = (struct ecb_ctx*) handle;

	if (!ctx) {
		log_err(ECB_DEFAULT_PREFIX,
		        "ecb_dev_write() called with NULL pointer");
		return -1;
	}

	ecb_hex_dump("TX", data, size);
	if (write(ctx->dev.fd, data, (size_t) size) == -1) {
		log_info(ctx->log_prefix, "write() failed, errno: %d", errno);
		return -1;
	}

	return 0;
}

static int ecb_rx(int fd, uint8_t term, ecb_dev_rx rx, void *obj,
		  uint8_t *data, uint32_t *size, const char* log_prefix)
{
	uint8_t buffer[ECB_DEV_MRU];
	int     len, idx, ret;

	len = read(fd, buffer, sizeof(buffer));
	if (len == -1) {
		log_info(log_prefix, "read() failed, errno: %d", errno);
		return -1;
	}

	for (idx = 0; idx < len; idx++) {
		data[(*size)++] = buffer[idx];
		if (buffer[idx] == term) {
			ecb_hex_dump("RX", data, *size);
			if (rx) {
				ret = rx(obj, data, *size);
				if (ret != 0) {
					return ret;
				}
			}
			*size = 0;
		} else if (*size >= ECB_DEV_MRU) {
			*size = 0;
		}
	}

	return 0;
}

int ecb_dev_receive(void *handle,
                    const uint8_t term, ecb_dev_rx rx, void *obj,
                    struct timespec *const tmo)
{
	struct ecb_ctx *ctx = (struct ecb_ctx*) handle;
	uint8_t         data[ECB_DEV_MRU];
	int             ret;
	uint32_t        size = 0;
	struct timespec t, t1;
	struct timeval  tv;
	fd_set          fds;

	if (!ctx) {
		log_err(ECB_DEFAULT_PREFIX,
		        "ecb_dev_receive() called with NULL pointer");
		return -1;
	}

	/* Calculate when to return on time-out. */
	clock_gettime(CLOCK_MONOTONIC_RAW, &t1);
	t1.tv_sec += tmo->tv_sec;
	t1.tv_nsec += tmo->tv_nsec;
	if (t1.tv_nsec >= 1000000000) {
		t1.tv_nsec -= 1000000000;
		t1.tv_sec++;
	}

	do {
		FD_ZERO(&fds);
		FD_SET(ctx->dev.fd, &fds);
		tv.tv_sec = 0;
		tv.tv_usec = 10000;
		ret = select(ctx->dev.fd + 1, &fds, 0, 0, &tv);
		if (ret == -1) {
			log_info(ctx->log_prefix, "select() failed, errno: %d",
			       errno);
			return -1;
		} else if (ret != 0) {
			ret = ecb_rx(ctx->dev.fd, term, rx, obj, data,
			             &size, ctx->log_prefix);
			if (ret != 0) {
				return ret;
			}
		}

		clock_gettime(CLOCK_MONOTONIC_RAW, &t);

	} while (t.tv_sec < t1.tv_sec ||
		 (t.tv_sec == t1.tv_sec && t.tv_nsec < t1.tv_nsec));

	return 0;
}

int ecb_dev_flush(void *handle)
{
	struct ecb_ctx *ctx = (struct ecb_ctx*) handle;

	if (!ctx) {
		log_err(ECB_DEFAULT_PREFIX,
		        "ecb_dev_flush() called with NULL pointer");
		return -1;
	}

	/* Flush any garbage. */
	if (tcflush(ctx->dev.fd, TCIOFLUSH) == -1) {
		log_err(ctx->log_prefix, "tcflush() failed, errno: %d", errno);
		return -1;
	}

	return 0;
}

int ecb_dev_drain(void *handle)
{
	struct ecb_ctx *ctx = (struct ecb_ctx*) handle;

	if (!ctx) {
		log_err(ECB_DEFAULT_PREFIX,
		        "ecb_dev_drain() called with NULL pointer");
		return -1;
	}

	/* Wait until all data transmitted. */
	if (tcdrain(ctx->dev.fd) == -1) {
		log_err(ctx->log_prefix, "tcdrain() failed, errno: %d", errno);
		return -1;
	}

	return 0;
}

int ecb_dev_prog(void *handle, enum ecb_dev_param param, const void *value)
{
	struct ecb_ctx *ctx = (struct ecb_ctx*) handle;
	uint32_t        u32;

	if (!ctx) {
		log_err(ECB_DEFAULT_PREFIX,
		        "ecb_dev_prog() called with NULL pointer");
		return -1;
	}

	if (param == ECB_DEV_PARAM_BITRATE) {

		speed_t bitrate;

		memcpy(&u32, value, sizeof(u32));
		switch (u32) {
		case 9600:
			bitrate = B9600;
			break;
		case 38400:
			bitrate = B38400;
			break;
		case 115200:
			bitrate = B115200;
			break;
		default:
			log_err(ctx->log_prefix, "Unsupported bit rate: %u", u32);
			return -1;
		}
		if (cfsetispeed(&ctx->dev.ts, bitrate) == -1) {
			log_err(ctx->log_prefix, "cfsetispeed() failed, errno: %d",
			       errno);
			return -1;
		}
		if (cfsetospeed(&ctx->dev.ts, bitrate) == -1) {
			log_err(ctx->log_prefix, "cfsetospeed() failed, errno: %d",
			       errno);
			return -1;
		}

	} else if (param == ECB_DEV_PARAM_STOP_BITS) {

		memcpy(&u32, value, sizeof(u32));
		if (u32 == 1) {
			ctx->dev.ts.c_cflag &= ~CSTOPB;
		} else if (u32 == 2) {
			ctx->dev.ts.c_cflag |= CSTOPB;
		} else {
			log_err(ctx->log_prefix, "Unsupported stop bits: %u", u32);
			return -1;
		}

	} else if (param == ECB_DEV_PARAM_MARK_ERRORS) {

		memcpy(&u32, value, sizeof(u32));
		if (u32 == 0) {
			ctx->dev.ts.c_iflag &= ~(INPCK | PARMRK);
		} else if (u32 == 1) {
			ctx->dev.ts.c_iflag |= INPCK | PARMRK;
		} else {
			log_err(ctx->log_prefix, "Unsupported value: %u", u32);
			return -1;
		}

	} else {
		log_err(ctx->log_prefix, "Unsupported parameter: %d", param);
		return -1;
	}

	return 0;
}

int ecb_dev_set(void *handle)
{
	struct ecb_ctx *ctx = (struct ecb_ctx*) handle;

	if (!ctx) {
		log_err(ECB_DEFAULT_PREFIX,
		        "ecb_dev_set() called with NULL pointer");
		return -1;
	}

	if (tcsetattr(ctx->dev.fd, TCSANOW, &ctx->dev.ts) == -1) {
		log_err(ctx->log_prefix, "tcsetattr() failed, errno: %d", errno);
		return -1;
	}

	return 0;
}

#define IBUF_LEN (10 * (sizeof(struct inotify_event)))

static void* ecb_thread_monitor(void *handle)
{
	int i_fd, wd,flags;
	ssize_t num_read;
	char buf[IBUF_LEN];
	char *p;
	struct inotify_event *event;
	struct ecb_monitor_ctx *ctx = (struct ecb_monitor_ctx*) handle;

	i_fd = inotify_init();

	if (i_fd == -1) {
		log_err(ECB_DEFAULT_PREFIX,"inotify init failed.");
		return NULL;
	}

	wd = inotify_add_watch(i_fd, ctx->file_path, IN_CREATE | IN_DELETE);

        if (wd == -1) {
		log_err(ECB_DEFAULT_PREFIX,"Unable to setup watching descriptor.");
		return NULL;
	}

        if (-1 == (flags = fcntl(i_fd, F_GETFL, 0)))
	  flags = 0;

        if ( -1 == fcntl(i_fd, F_SETFL, flags & ~O_NONBLOCK)) {
		log_err(ECB_DEFAULT_PREFIX,"Setting O_NONBLOCK flag failed.");
		return NULL;
	}

	for (;;) {
		num_read = read(i_fd, buf, IBUF_LEN);
		if ((num_read == 0) || (num_read == -1))
			continue;

		for (p = buf; p < buf + num_read; ) {
			event = (struct inotify_event *) p;
			if(event->len > 0) {
				if(strcmp(ctx->file_name, event->name) == 0)
					(ctx->monitor)(event->mask,
						       ctx->client_ref);
			}
			p += sizeof(struct inotify_event) + event->len;
		}
	}
	return NULL;
}

static int parse_file_path (char *path, char* file_path, char* file_name) {

	char *name;
	int pos;

	name = strrchr(path, '/');

	if (name !=NULL && strlen(name) > 1 ) {

		pos = (name - path)/sizeof(char);

		if (pos > 0) {
			strncpy(file_path, path, (name-path)/sizeof(char));
			strcpy (file_name,++name);
		} else if (pos == 0) {
			sprintf(file_path,"/");
			strcpy (file_name,++name);
		} else {
			log_err(ECB_DEFAULT_PREFIX,
				"Internal error pos %d name %p = %s, "
				"path %p = %s!", pos, name,
				name, path, path);
			return -1;
		}
	} else if (name !=NULL && strlen(name) == 1) {
		log_err(ECB_DEFAULT_PREFIX,
			"No file name given %s!", name);
		return -1;
	} else if (name == NULL && strlen(path) > 0) {
		/* assuming current directory! */
		sprintf(file_path,"./");
		strcpy (file_name,path);;
	} else {
		log_err(ECB_DEFAULT_PREFIX,
			"Bad path given %s!", path);
		return -1;
	}

	return 0;
}

int ecb_dev_monitor(void **handle, ecb_dev_mon mon, char *name, void *client_ref)
{
	struct ecb_monitor_ctx *ctx;
	pthread_t mon_thread;
	int res;

	if (!mon) {
		log_err(ECB_DEFAULT_PREFIX,
			"ecb_dev_monitor() called with bad monitor callback!");
		goto err;
	}

	if (!name) {
		log_err(ECB_DEFAULT_PREFIX,
			"Device name not valid!");
		goto err;
	}

	ctx = (struct ecb_monitor_ctx*) calloc(1, sizeof(struct ecb_monitor_ctx));
	if (!ctx) {
		log_err(ECB_DEFAULT_PREFIX, "calloc() failed!");
		goto err;
	}

	ctx->file_name = (char*)calloc(strlen(name)+1, sizeof(char));
	if (!ctx->file_name) {
		log_err(ECB_DEFAULT_PREFIX, "calloc() failed!");
		goto err_fname;
	}

	ctx->file_path = (char*)calloc(strlen(name)+1, sizeof(char));
	if (!ctx->file_path) {
		log_err(ECB_DEFAULT_PREFIX, "calloc() failed!");
		goto err_fpath;
	}

	if (parse_file_path(name, ctx->file_path, ctx->file_name) != 0) {
		goto err_pthread;
	}

	ctx->monitor = mon;

	res = pthread_create( &mon_thread, NULL, ecb_thread_monitor, (void *)ctx);
	if (res != 0) {
		log_err(ECB_DEFAULT_PREFIX,
			"Creation of monitoring thread failed! Errno: %d", res);
		goto err_pthread;
	}

	ctx->mon_thread = mon_thread;
	ctx->client_ref = client_ref;

	*handle = ctx;

	return 0;

 err_pthread:
	free(ctx->file_path);
 err_fpath:
	free(ctx->file_name);
 err_fname:
	free(ctx);
 err:
	return -1;
}

int ecb_dev_unmonitor(void *handle)
{
	int res;
	struct ecb_monitor_ctx *ctx = (struct ecb_monitor_ctx*) handle;
	if (!ctx) {
		log_err(ECB_DEFAULT_PREFIX,
			"ecb_dev_unmonitor() called with NULL pointer");
		return -1;
	}

	if (ctx->monitor) {
		res = pthread_cancel(ctx->mon_thread);
		if (res == 0) {
			res = pthread_join(ctx->mon_thread, NULL);
			if (res != 0) {
				log_info(ECB_DEFAULT_PREFIX, "pthread_join for "
					 "monitoring failed, errno: %d", res);
			}
		} else {
			log_info(ECB_DEFAULT_PREFIX, "pthread_cancel for "
				 "monitoring failed, errno: %d", res);
		}
		ctx->monitor = NULL;
		ctx->mon_thread = 0;
	}
	if (ctx->file_path)
		free(ctx->file_path);

	if (ctx->file_name)
		free(ctx->file_name);

	free(ctx);

	return 0;
}
