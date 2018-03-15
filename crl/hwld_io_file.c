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
#include <errno.h>
#include <unistd.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <mtd/mtd-user.h>

#include "hwld_io.h"

/* Modeled after xmu03 flash */
#define HWLD_ERASE_SIZE 	0x80000
#define HWLD_WRITE_SIZE 	0x1
#define HWLD_FILE_SIZE		0x80000

static int init(char *dev, mtd_info_t *pinfo)
{
	struct stat st;
	char b = ERASED_FLASH_CONTENT;
	int fd;

	memset(pinfo, 0, sizeof(mtd_info_t));

	if (stat(dev, &st) == 0) {
		fd = open(dev, O_RDWR);
		if (fd < 0)
			return -1;
		pinfo->size = st.st_size;
	}
	else {
		if (errno != ENOENT)
			return -1;

		/* File doesn't exist, create an initialize it */
		fd = open(dev, O_RDWR | O_CREAT, S_IRWXU);
		if (fd < 0)
			return -1;

		pinfo->size = HWLD_FILE_SIZE;
		for (int i = 0; i < pinfo->size; i++) {
			if (write(fd, &b, 1) == -1) {
				close(fd);
				return -1;
			}
		}
	}

	pinfo->erasesize = HWLD_ERASE_SIZE;
	pinfo->writesize = HWLD_WRITE_SIZE;

	return fd;
}

static int eraselog(int fd, uint32_t erasesize, uint32_t size)
{
	char b = ERASED_FLASH_CONTENT;

	if (lseek(fd, 0, SEEK_SET) == -1)
		return -1;

	for (int i = 0; i < size; i++) {
		if (write(fd, &b, 1) == -1)
			return -1;
	}

	return 0;
}

static int erasesector(int fd, uint32_t start, uint32_t erasesize)
{
	char b = ERASED_FLASH_CONTENT;
	off_t pos = lseek(fd, 0, SEEK_CUR);

	if (pos == -1)
		return -1;

	if (lseek(fd, start, SEEK_SET) == -1)
		return -1;
	for (int i = 0; i < erasesize; i++) {
		if (write(fd, &b, 1) == -1)
			return -1;
	}

	/* Restore fd location to before the erase */
	if (lseek(fd, pos, SEEK_SET) == -1)
		return -1;

	return 0;
}

hwld_io hwld_io_file = {
	init,
	eraselog,
	erasesector
};
