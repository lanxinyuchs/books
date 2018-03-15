/**
 *   Copyright (C) 2015 by Ericsson AB. All rights reserved. The
 *   information in this document is the property of Ericsson. Except
 *   as specifically authorized in writing by Ericsson, the receiver
 *   of this document shall keep the information contained herein
 *   confidential and shall protect the same in whole or in part from
 *   disclosure and dissemination to third parties. Disclosure and
 *   disseminations to the receiver's employees shall only be made on
 *   a strict need to know basis.
 */
#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <errno.h>
#include <string.h>
#include <mtd/mtd-user.h>
#include <sys/ioctl.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <unistd.h>
#include "nvpi3_io.h"
#include "log_tpt.h"

static int mtd_read(const char *name, void *buf, uint32_t size,
                    uint32_t offset);
static int mtd_write(const char *name, const void *buf, uint32_t size,
                     uint32_t offset);
static int file_read(const char *name, void *buf, uint32_t size,
                     uint32_t offset);
static int file_write(const char *name, const void *buf, uint32_t size,
                      uint32_t offset);

nvpi3_result_t nvpi3_get_storage_size(const union nvpi3_db_storage *storage,
                                      uint32_t *size)
{
	struct stat st;
	*size = 0;

	if (storage->type == NVPI3_DB_STORAGE_TYPE_MTD && storage->mtd.size) {
		*size = storage->mtd.size;
	} else {
		if(stat((storage->type == NVPI3_DB_STORAGE_TYPE_FILE) ?
		        storage->file.name : storage->mtd.name, &st) == -1) {
			TPT_ERROR(STR("stat failed with errno %d", errno));
			return NVPI3_RESULT_OTHER_ERROR;
		}
		*size = st.st_size;
	}

	return NVPI3_RESULT_SUCCESS;
}

nvpi3_result_t nvpi3_read(void *buf, uint32_t size,
                          const union nvpi3_db_storage *storage)
{
	nvpi3_result_t result = NVPI3_RESULT_SUCCESS;

	if(storage->type == NVPI3_DB_STORAGE_TYPE_FILE) {
		if (file_read(storage->file.name, buf, size,
		              storage->file.offset)) {
			result = NVPI3_RESULT_OTHER_ERROR;
		}

	} else if(storage->type == NVPI3_DB_STORAGE_TYPE_MTD) {
		if (mtd_read(storage->mtd.name, buf, size,
		             storage->mtd.offset)) {
			result = NVPI3_RESULT_OTHER_ERROR;
		}

	} else {
		TPT_ERROR(STR("storage type %d is not supported.",
		              storage->type));
		result = NVPI3_RESULT_INVALID_PARAM;
	}

	return result;
}


nvpi3_result_t nvpi3_write(const void *buf, uint32_t size,
                           const union nvpi3_db_storage *storage)
{
	nvpi3_result_t result = NVPI3_RESULT_SUCCESS;

	if(storage->type == NVPI3_DB_STORAGE_TYPE_FILE) {
		if(size > storage->file.max_size) {
			TPT_ERROR(STR("size %d exceeds maximum size %d.", size,
			              storage->file.max_size));
			result = NVPI3_RESULT_ACCESS_DENIED;
			goto  nvpi3_write_end;
		}

		if (file_write(storage->file.name, buf, size,
		               storage->file.offset)) {
			result = NVPI3_RESULT_OTHER_ERROR;
		}
	} else if (storage->type == NVPI3_DB_STORAGE_TYPE_MTD) {
		if (size > storage->mtd.max_size) {
			TPT_ERROR(STR("size %d exceeds maximum size %d.", size,
			              storage->mtd.max_size));
			result = NVPI3_RESULT_ACCESS_DENIED;
			goto  nvpi3_write_end;
		}

		if (mtd_write(storage->mtd.name, buf, size,
		              storage->mtd.offset)) {
			result = NVPI3_RESULT_OTHER_ERROR;
		}
	} else {
		TPT_ERROR(STR("storage type %d is not supported.",
		              storage->type));
		result = NVPI3_RESULT_INVALID_PARAM;
	}

nvpi3_write_end:
	return result;
}
static int mtd_erase_partition(mtd_info_t *mtd_info, int fd)
{
	erase_info_t ei;
	int ret = 0;
	ei.length = mtd_info->erasesize;

	for(ei.start = 0; ei.start < mtd_info->size;
	    ei.start += mtd_info->erasesize) {
		if(ioctl(fd, MEMUNLOCK, &ei) == -1) {
			TPT_ERROR(STR("ioctl failed with errno %d", errno));
			ret = -1;
			break;
		}
		if(ioctl(fd, MEMERASE, &ei) == -1) {
			TPT_ERROR(STR("ioctl failed with errno %d", errno));
			ret = -1;
			break;
		}
	}
	return ret;
}

static int mtd_read(const char *name, void *buf, uint32_t size,
                    uint32_t offset)
{
	mtd_info_t mtd_info;
	int ret = -1;
	int lseek_ret, read_ret;
	int fd = open(name, O_RDONLY | O_SYNC | O_NONBLOCK);
	if (fd < 0) {
		TPT_ERROR(STR("open %s failed with errno %d", name, errno));
		return -1;
	}

	if (ioctl(fd, MEMGETINFO, &mtd_info) == -1) {
		TPT_ERROR(STR("ioctl %s failed with errno %d", name, errno));
		goto mtd_read_end;
	}

	if(offset >= mtd_info.size) {
		TPT_ERROR(STR("offset %d is outside %s partition",
		              offset, name));
		goto mtd_read_end;
	}

	lseek_ret = lseek(fd, offset, SEEK_SET);
	if(lseek_ret == -1) {
		TPT_ERROR(STR("lseek failed with errno %d", errno));
		goto mtd_read_end;
	} else if(lseek_ret != offset) {
		TPT_ERROR(STR("lseek return value %d differs from expected %d",
		              lseek_ret, offset));
		goto mtd_read_end;
	}

	read_ret = read(fd, buf, size);
	if(read_ret == -1) {
		TPT_ERROR(STR("read failed with errno %d", errno));
		goto mtd_read_end;
	} else if(read_ret != size) {
		TPT_INFO(STR("warning: read return value %d differs from "
		             "expected %d", read_ret, size));
		goto mtd_read_end;
	}

	ret = 0;

mtd_read_end:
	if(close(fd) == -1) {
		TPT_ERROR(STR("close %s failed with errno %d", name, errno));
		ret = -1;
	}
	return ret;
}

static int mtd_write(const char *name, const void *buf, uint32_t size,
                     uint32_t offset)
{
	mtd_info_t mtd_info;
	int ret = -1;
	int lseek_ret, write_ret;

	int fd = open(name, O_WRONLY | O_SYNC | O_NONBLOCK);
	if (fd < 0) {
		TPT_ERROR(STR("open %s failed with errno %d", name, errno));
		return -1;
	}
	if (ioctl(fd, MEMGETINFO, &mtd_info) == -1) {
		TPT_ERROR(STR("ioctl %s failed with errno %d", name, errno));
		goto mtd_write_end;
	}

	if((size + offset)  > mtd_info.size) {
		TPT_ERROR(STR("requested size 0x%x @ offset: 0x%x exceeds "
		              "partition size 0x%x", size, offset,
		              mtd_info.size));
		goto mtd_write_end;
	}

	if(mtd_erase_partition(&mtd_info, fd) == -1) {
		TPT_ERROR(STR("erase %s partition failed", name));
		goto mtd_write_end;
	}

	lseek_ret = lseek(fd, offset, SEEK_SET);
	if(lseek_ret == -1) {
		TPT_ERROR(STR("lseek failed with errno %d", errno));
		goto mtd_write_end;
	} else if(lseek_ret != offset) {
		TPT_ERROR(STR("lseek return value %d differs from expected %d",
		              lseek_ret, offset));
		goto mtd_write_end;
	}

	write_ret = write(fd, buf, size);
	if(write_ret == -1) {
		TPT_ERROR(STR("write failed with errno %d", errno));
		goto mtd_write_end;
	} else if(write_ret != size) {
		TPT_ERROR(STR("warning: write return value %d differs from "
		             "expected %d", write_ret, size));
		goto mtd_write_end;
	}

	ret = 0;

mtd_write_end:
	if(close(fd) == -1) {
		TPT_ERROR(STR("close %s failed with errno %d", name, errno));
		ret = -1;
	}
	return ret;
}

static int file_read(const char *name, void *buf, uint32_t size,
                     uint32_t offset)
{
	int ret = -1;
	int lseek_ret, read_ret;
	int fd = open(name, O_RDONLY, S_IRWXU);
	if (fd < 0) {
		TPT_ERROR(STR("open %s failed with errno %d", name, errno));
		return -1;
	}

	lseek_ret = lseek(fd, offset, SEEK_SET);
	if(lseek_ret == -1) {
		TPT_ERROR(STR("lseek failed with errno %d", errno));
		goto file_read_end;
	} else if(lseek_ret != offset) {
		TPT_ERROR(STR("lseek ret %d differs from expected %d",
		              lseek_ret, offset));
		goto file_read_end;
	}

	read_ret = read(fd, buf, size);
	if(read_ret == -1) {
		TPT_ERROR(STR("read failed with errno %d", errno));
		goto file_read_end;
	} else if(read_ret != size) {
		TPT_INFO(STR("warning: read return value %d differs from "
		             "expected %d", read_ret, size));
		goto file_read_end;
	}

	ret = 0;

file_read_end:
	if(close(fd) == -1) {
		TPT_ERROR(STR("close %s failed with errno %d", name, errno));
		ret = -1;
	}
	return ret;
}



static int file_write(const char *name, const void *buf, uint32_t size,
                      uint32_t offset)
{
	int ret = -1;
	int lseek_ret, write_ret;
	int fd = open(name, O_WRONLY | O_CREAT | O_TRUNC, S_IRWXU);
	if (fd < 0) {
		TPT_ERROR(STR("open %s failed with errno %d", name, errno));
		return -1;
	}
	lseek_ret = lseek(fd, offset, SEEK_SET);
	if(lseek_ret == -1) {
		TPT_ERROR(STR("lseek failed with errno %d", errno));
		goto file_write_end;
	} else if(lseek_ret != offset) {
		TPT_ERROR(STR("lseek return value %d differs from expected %d",
		              lseek_ret, offset));
		goto file_write_end;
	}

	write_ret = write(fd, buf, size);
	if(write_ret == -1) {
		TPT_ERROR(STR("write failed with errno %d", errno));
		goto file_write_end;
	} else if(write_ret != size) {
		TPT_ERROR(STR("warning: write return value %d differs from "
		             "expected %d", write_ret, size));
		goto file_write_end;
	}

	ret = 0;

file_write_end:
	if(close(fd) == -1) {
		TPT_ERROR(STR("close %s failed with errno %d", name, errno));
		ret = -1;
	}
	return ret;
}
