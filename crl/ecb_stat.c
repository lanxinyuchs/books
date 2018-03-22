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

#include <errno.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

#include "ecb_dev.h"
#include "ecb_stat.h"

#include "log.h"

#define ECB_STAT_SHMEM_PREFIX "/ecb-shmem-"

struct ecb_stat_info {
	char *name; /**> shared memory file name */
	int fd;
	void *addr;
};

static char *ecb_stat_make_shm_name(const char *devname) {
	char *name, *ptr;

	name = malloc(strlen(devname) + sizeof(ECB_STAT_SHMEM_PREFIX));
	if (!name) {
		log_err(ECB_DEFAULT_PREFIX,
			"Can't allocate memory for shared memory filename.");
		return name;
	}

	strcpy(name, ECB_STAT_SHMEM_PREFIX);
	strcat(name, devname);

	ptr = name + strlen(ECB_STAT_SHMEM_PREFIX);
	while (*ptr) {
		if (*ptr == '/')
			*ptr = '-';
		ptr++;
	}
	return name;
}

struct ecb_stat_info * ecb_stat_create(const char *dev_fname)
{
	int flag = O_RDWR;
	struct ecb_stat_info *info;

	info = (struct ecb_stat_info *)malloc(sizeof(struct ecb_stat_info));
	if (!info) {
		log_err(ECB_DEFAULT_PREFIX,
			"Can't allocate memory for info structure.");
		return info;
	}
	memset(info, 0, sizeof(struct ecb_stat_info));
	info->fd = -1;

	info->name = ecb_stat_make_shm_name(dev_fname);
	if (!info->name) {
		goto cleanup;
	}

	/* try to open first or create */
	info->fd = shm_open(info->name, flag, S_IRUSR | S_IWUSR);
	if (info->fd == -1) {
		if (errno != ENOENT) {
			log_err(ECB_DEFAULT_PREFIX,
				"shm_open() failed: %s\n", strerror(errno));
			goto cleanup;
		} else {
			flag |= O_CREAT;
		}
	}

	if (flag & O_CREAT) {
		info->fd = shm_open(info->name, flag, S_IRUSR | S_IWUSR);
		if (info->fd == -1) {
			log_err(ECB_DEFAULT_PREFIX,
				"shm_open(O_CREAT) failed: %s\n",
				strerror(errno));
			goto cleanup;
		}
	}

	if (ftruncate(info->fd, sizeof(struct ecb_dev_stat_hdlc))) {
		log_err(ECB_DEFAULT_PREFIX,
			"ftruncate() failed: %s\n", strerror(errno));
		goto cleanup;
	}

	/* Map shared memory object */
	info->addr = mmap(NULL, sizeof(struct ecb_dev_stat_hdlc),
			  PROT_READ | PROT_WRITE, MAP_SHARED, info->fd, 0);
	if (info->addr == MAP_FAILED) {
		log_err(ECB_DEFAULT_PREFIX, "mmap() failed: %s\n",
			strerror(errno));
		goto cleanup;
	}

	return info;
cleanup:
	if (info->fd != -1)
		close(info->fd);

	if (info->name)
		shm_unlink(info->name);

	free(info->name);
	free(info);

	return NULL;
}

int ecb_stat_destroy(struct ecb_stat_info *info) {
	if (!info)
		return -1;

	munmap(info->addr, sizeof(struct ecb_dev_stat_hdlc));

	close(info->fd);

	free(info->name);
	free(info);

	return 0;
}

int ecb_stat_update(struct ecb_stat_info *info, struct ecb_dev_stat_hdlc *stat)
{
	struct ecb_dev_stat_hdlc *shared_stat;

	if (!info)
		return -1;
	if (!stat)
		return -1;

	shared_stat = (struct ecb_dev_stat_hdlc *)info->addr;

	if (lockf(info->fd, F_LOCK, 0)) {
		log_err(ECB_DEFAULT_PREFIX, "lockf() failed: %s\n",
			strerror(errno));
		return -1;
	}

	shared_stat->n_tx        += stat->n_tx;
	shared_stat->n_rx        += stat->n_rx;
	shared_stat->n_tx_frames += stat->n_tx_frames;
	shared_stat->n_rx_frames += stat->n_rx_frames;
	shared_stat->n_rsp_tmo   += stat->n_rsp_tmo;
	shared_stat->err_addr    += stat->err_addr;
	shared_stat->err_ctrl    += stat->err_ctrl;
	shared_stat->err_crc     += stat->err_crc;
	shared_stat->err_size    += stat->err_size;

	if (lockf(info->fd, F_ULOCK, 0)) {
		log_err(ECB_DEFAULT_PREFIX, "lockf() failed: %s\n",
			strerror(errno));
		return -1;
	}
	return 0;
}

int ecb_stat_read(struct ecb_stat_info *info, struct ecb_dev_stat_hdlc *stat)
{
	struct ecb_dev_stat_hdlc *shared_stat;

	if (!info)
		return -1;
	if (!stat)
		return -1;

	if (lockf(info->fd, F_LOCK, 0)) {
		log_err(ECB_DEFAULT_PREFIX, "lockf() failed: %s\n",
			strerror(errno));
		return -1;
	}

	shared_stat = (struct ecb_dev_stat_hdlc *)info->addr;
	memcpy(stat, shared_stat, sizeof(struct ecb_dev_stat_hdlc));

	if (lockf(info->fd, F_ULOCK, 0)) {
		log_err(ECB_DEFAULT_PREFIX, "lockf() failed: %s\n",
			strerror(errno));
		return -1;
	}

	return 0;
}

int ecb_stat_reset(struct ecb_stat_info *info)
{
	if (!info)
		return -1;

	if (lockf(info->fd, F_LOCK, 0)) {
		log_err(ECB_DEFAULT_PREFIX, "lockf() failed: %s\n",
			strerror(errno));
		return -1;
	}

	memset(info->addr, 0, sizeof(struct ecb_dev_stat_hdlc));

	if (lockf(info->fd, F_ULOCK, 0)) {
		log_err(ECB_DEFAULT_PREFIX, "lockf() failed: %s\n",
			strerror(errno));
		return -1;
	}

	return 0;
}
