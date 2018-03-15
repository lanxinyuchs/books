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

#include <fcntl.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/ioctl.h>
#include <mtd/mtd-user.h>

#include "hwld_io.h"

static int init(char *dev, mtd_info_t *pinfo)
{
	int fd = open(dev, O_RDWR, O_SYNC | O_NONBLOCK);
	if (fd < 0)
		return -1;

	if (ioctl(fd, MEMGETINFO, pinfo)) {
		close(fd);
		return -1;
	}

	return fd;
}

static int eraselog(int fd, uint32_t erasesize, uint32_t size)
{
	erase_info_t ei;

	ei.length = erasesize;

	for (ei.start = 0; ei.start < size; ei.start += erasesize) {
		ioctl(fd, MEMUNLOCK, &ei);
		ioctl(fd, MEMERASE, &ei);
	}

	return 0;
}

static int erasesector(int fd, uint32_t start, uint32_t erasesize)
{
	erase_info_t ei;

	ei.start = start;
	ei.length = erasesize;
	if (ioctl(fd, MEMUNLOCK, &ei) == -1)
		return -1;
	if (ioctl(fd, MEMERASE, &ei) == -1)
		return -1;

	return 0;
}

hwld_io hwld_io_mtd = {
	init,
	eraselog,
	erasesector
};
