/**
 * Copyright (C) 2015 by Ericsson AB. All rights reserved. The
 * information in this document is the property of Ericsson. Except
 * as specifically authorized in writing by Ericsson, the receiver
 * of this document shall keep the information contained herein
 * confidential and shall protect the same in whole or in part from
 * disclosure and dissemination to third parties. Disclosure and
 * disseminations to the receiver's employees shall only be made on
 * a strict need to know basis.
 */

/* Read & write uio device command for board bring-up */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdint.h>
#include <unistd.h>
#include <errno.h>
#include <dirent.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/mman.h>

#include <inttypes.h>


struct uio {
	void                *map_addr;
	uint32_t             map_size;
	volatile uint32_t   *addr;
};

/*
** Internal UIO helper functions
*/

#define UIO_ROOT "/sys/class/uio"

static const char *get_dev_name(char (*dev)[], const char *name)
{
	DIR           *dir;
	FILE          *fdd;
	struct dirent *dir_e;
	char           name_path[256], name_buf[64];

	dir = opendir(UIO_ROOT);
	dir_e = readdir(dir);
	while ((dir_e = readdir(dir))) {
		memset(name_path, 0, sizeof(name_path));
		memset(name_buf, 0, sizeof(name_buf));
		if (dir_e) {
			if (strstr(dir_e->d_name, "."))
				continue;
			sprintf(name_path, UIO_ROOT "/%s/name",
			        dir_e->d_name);
			fdd = fopen(name_path, "r");
			if (fdd == NULL) {
				closedir(dir);
				return NULL;
			}
			if (fscanf(fdd, "%s\n", name_buf) == 1 &&
			    !strcmp(name, name_buf)) {
				sprintf(*dev, "%s", dir_e->d_name);
				closedir(dir);
				fclose(fdd);
				return *dev;
			}
			fclose(fdd);
		}
	}
	closedir(dir);

	return NULL;
}

static const char *get_map_property(char (*prop_out)[], const char *prop,
                                    const char *dev, int map_no)
{
	FILE *fp;
	char  path[256];

	snprintf(path, sizeof(path), UIO_ROOT "/%s/maps/map%u/%s", dev,
	         map_no, prop);
	fp = fopen(path, "r");
	if (fp == NULL)
		return NULL;
	fgets(*prop_out, 32, fp);
	fclose(fp);

	return *prop_out;
}

static int uio_init(struct uio *uio, char *name, uint32_t offset)
{
	char      dev_name[32], dev_path[32], tmp[32];
	int       fd, page_size, page_offset;
	uint32_t  base, size;
	void     *map_addr;
	size_t    map_size;

	if (get_dev_name(&dev_name, name) == NULL) {
		printf("Failed to get device name for \"%s\"\n", name);
		return -1;
	}
	snprintf(dev_path, sizeof(dev_path), "/dev/%s", dev_name);

	if (get_map_property(&tmp, "addr", dev_name, 0) == NULL) {
		printf("Failed get map property for \"%s\"\n", name);
		return -1;
	}
	base = strtoul(tmp, NULL, 16);
	if (get_map_property(&tmp, "size", dev_name, 0) == NULL) {
		printf("Failed get map property for \"%s\"\n", name);
		return -1;
	}
	size = strtoul(tmp, NULL, 16);

	page_size = getpagesize();
	page_offset = base % page_size;

	/* Calculate actual size spanning over the whole required page range */
	map_size = (page_offset + size + page_size - 1) & ~(page_size - 1);
	map_addr = (void*) (base - page_offset);

	fd = open(dev_path, O_RDWR | O_SYNC);
	if (fd == -1) {
		printf("Failed (%d) to open \"%s\"\n", errno, dev_path);
		return -1;
	}

	uio->map_size = map_size;
	uio->map_addr = mmap(map_addr, map_size,
	                     PROT_READ | PROT_WRITE, MAP_SHARED, fd, 0);
	close(fd);
	if (uio->map_addr == MAP_FAILED) {
		printf("Failed (%d) to map %u bytes for UIO at 0x%x\n",
		       errno, map_size, (int) map_addr);
		return -1;
	}

	uio->addr = &((uint32_t*) map_addr)[offset >> 2];

	return 0;
}

static int uio_read(struct uio *uio)
{
	printf("ADDR: 0x%08"PRIx32"  DATA: 0x%08"PRIx32"\n",
	       (uint32_t) uio->addr, (uint32_t) *uio->addr);
	return 0;
}

static int uio_write(struct uio *uio, uint32_t value)
{
	printf("ADDR: 0x%08"PRIx32"  DATA: 0x%08"PRIx32" (pre-write)\n",
	       (uint32_t) uio->addr, (uint32_t) *uio->addr);
	*uio->addr = value;
	printf("ADDR: 0x%08"PRIx32"  DATA: 0x%08"PRIx32" (post-write)\n",
	       (uint32_t) uio->addr, (uint32_t) *uio->addr);
	return 0;
}

int main(int argc, char *argv[])
{
	struct uio uio;
	uint32_t   offset, value;

	if (argc == 4 &&
	    strcmp(argv[1], "-r") == 0 &&
	    sscanf(argv[3], "%"SCNx32"", &offset) == 1) {

		if (uio_init(&uio, argv[2], offset) == 0) {
			uio_read(&uio);
			munmap(uio.map_addr, uio.map_size);
		} else {
			printf("Failed to init UIO\n");
		}

	} else if (argc == 5 &&
	           strcmp(argv[1], "-w") == 0 &&
	           sscanf(argv[3], "%"SCNx32"", &offset) == 1 &&
	           sscanf(argv[4], "%"SCNx32"", &value) == 1) {

		if (uio_init(&uio, argv[2], offset) == 0) {
			uio_write(&uio, value);
			munmap(uio.map_addr, uio.map_size);
		} else {
			printf("Failed to init UIO\n");
		}

	} else {
		printf("READ : %s -r <uio device> <offset hex>\n", argv[0]);
		printf("WRITE: %s -w <uio device> <offset hex> <value hex>\n", argv[0]);
		exit(EXIT_FAILURE);
	}

	return 0;
}
