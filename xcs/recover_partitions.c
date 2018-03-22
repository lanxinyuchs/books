/**
 *   Copyright (C) 2016 by Ericsson AB. All rights reserved. The
 *   information in this document is the property of Ericsson. Except
 *   as specifically authorized in writing by Ericsson, the receiver
 *   of this document shall keep the information contained herein
 *   confidential and shall protect the same in whole or in part from
 *   disclosure and dissemination to third parties. Disclosure and
 *   disseminations to the receiver's employees shall only be made on
 *   a strict need to know basis.
 */
#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/ioctl.h>
#include <fcntl.h>
#include <errno.h>
#include <unistd.h>
#include <arpa/inet.h>
#include <mtd/mtd-user.h>
#include "xlfi_api.h"

#define TRACEPOINT_PROVIDER com_ericsson_xcs_recover_partitions
#include "tpt_create.h"
#include "tpt.h"

enum partition {
	PARTITION_TYPE2,
	PARTITION_SPARE,
	PARTITION_BACKUP,
	NUM_OF_PARTITIONS
};

enum xlf_state {
	XLF_INVALID,
	XLF_VALID,
	XLF_VALID_AND_WORKING
};

enum recover_action {
	RECOVER_ACTION_NONE,
	RECOVER_ACTION_RESTORE,
	RECOVER_ACTION_SAVE
};

struct xlf_info {
	enum xlf_state state;
	uint32_t size;
};

static const char *boot_partition_env_var[NUM_OF_PARTITIONS] = {
	"sys_boot_default_dev",
	"sys_boot_dev",
	"sys_boot_backup_dev"
};

static const char *bootenv_partition_env_var[NUM_OF_PARTITIONS] = {
	"sys_bootenv_default_dev",
	"sys_bootenv_dev",
	"sys_bootenv_backup_dev"
};

static const char *bpar_partition_env_var[NUM_OF_PARTITIONS] = {
	"sys_bpar_default_dev",
	"sys_bpar_dev",
	"sys_bpar_backup_dev"
};

static struct xlf_info boot_xlf_info[NUM_OF_PARTITIONS];
static struct xlf_info bootenv_xlf_info[NUM_OF_PARTITIONS];
static struct xlf_info bpar_xlf_info[NUM_OF_PARTITIONS];

static int mtd_read(const char *name, void *buf, uint32_t size)
{
	int ret = -1;
	int read_ret;

	int fd = open(name, O_RDONLY | O_SYNC | O_NONBLOCK);
	if (fd < 0) {
		TPT_ERROR(STR("open %s failed with errno %d", name, errno));
		return -1;
	}

	read_ret = read(fd, buf, size);
	if(read_ret == -1) {
		TPT_ERROR(STR("read failed with errno %d", errno));
		goto mtd_read_end;
	} else if(read_ret != size) {
		TPT_ERROR(STR("read return value %d differs from "
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

static int get_mtd_size(char *name, uint32_t *size)
{
	mtd_info_t mtd_info;
	int ret = -1;
	int fd;

	*size = 0;
	fd = open(name, O_RDONLY | O_SYNC | O_NONBLOCK);
	if (fd < 0) {
		TPT_ERROR(STR("open %s failed with errno %d", name, errno));
		return -1;
	}
	if (ioctl(fd, MEMGETINFO, &mtd_info) == -1) {
		TPT_ERROR(STR("ioctl %s failed with errno %d", name, errno));
		goto get_mtd_size_end;
	}
	*size = mtd_info.size;
	ret = 0;

get_mtd_size_end:
	if(close(fd) == -1) {
		TPT_ERROR(STR("close %s failed with errno %d", name, errno));
		ret = -1;
	}

	return ret;
}

static int mtd_erase_partition(const char *name)
{
	mtd_info_t mtd_info;
	erase_info_t ei;
	int ret = -1;

	int fd = open(name, O_WRONLY | O_SYNC | O_NONBLOCK);
	if (fd < 0) {
		TPT_ERROR(STR("open %s failed with errno %d", name, errno));
		return -1;
	}

	if (ioctl(fd, MEMGETINFO, &mtd_info) == -1) {
		TPT_ERROR(STR("ioctl MEMGETINFO %s failed with errno %d",
		              name, errno));
		goto mtd_erase_partition_end;
	}

	ei.length = mtd_info.erasesize;

	for(ei.start = 0; ei.start < mtd_info.size;
	    ei.start += mtd_info.erasesize) {
		if(ioctl(fd, MEMUNLOCK, &ei) == -1) {
			TPT_ERROR(STR("ioctl MEMUNLOCK failed with errno %d",
			              errno));
			goto mtd_erase_partition_end;
		}
		if(ioctl(fd, MEMERASE, &ei) == -1) {
			TPT_ERROR(STR("ioctl MEMERASE failed with errno %d",
			              errno));
			goto mtd_erase_partition_end;
		}
	}
	ret = 0;

mtd_erase_partition_end:
	if(close(fd) == -1) {
		TPT_ERROR(STR("close %s failed with errno %d", name, errno));
		ret = -1;
	}
	return ret;
}

static int mtd_write(const char *name, const void *buf, uint32_t size,
                     uint32_t offset)
{
	int ret = -1;
	int lseek_ret, write_ret;

	int fd = open(name, O_WRONLY | O_SYNC | O_NONBLOCK);
	if (fd < 0) {
		TPT_ERROR(STR("open %s failed with errno %d", name, errno));
		return -1;
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

static int copy_partition(const char *src_path, const char *dest_path,
                          uint32_t size)
{
	int ret = -1;

	uint8_t *data = malloc(size);
	if(data == NULL) {
		TPT_ERROR(STR("malloc %d size failed", size));
		goto copy_partition_end;
	}

	if(mtd_read(src_path, data, size)) {
		goto copy_partition_end;
	}

	if(mtd_erase_partition(dest_path)) {
		goto copy_partition_end;
	}

	/* copy xlf file except magic number. */
	if(mtd_write(dest_path,
	             data + offsetof(struct xlf_iboot_header, type),
	             size - offsetof(struct xlf_iboot_header, type),
	             offsetof(struct xlf_iboot_header, type))) {
		goto copy_partition_end;
	}
	/* copy magic number */
	if(mtd_write(dest_path, data, offsetof(struct xlf_iboot_header, type),
	             0)) {
		goto copy_partition_end;
	}

	ret = 0;

copy_partition_end:
	free(data);
	return ret;

}

static int validate_partition(const char *path, uint32_t *size,
                              enum xlf_state *xlf_state)
{
	xlfi_result_t xlfi_res;
	struct xlfi_parse_state state;
	const char *error;
	struct xlf_iboot_header header;
	uint32_t num_of_files;
	struct xlfi_file_info *file = NULL;
	int done;
	uint8_t *data = NULL;
	int ret = -1;

	*xlf_state = XLF_INVALID;
	*size = 0;

	if(mtd_read(path, &header, sizeof(struct xlf_iboot_header))) {
		goto validate_partition_end;
	}

	xlfi_res = xlfi_validate_header(&state, &header, &error);
	if (xlfi_res != XLFI_RESULT_SUCCESS) {
		goto validate_partition_end;
	}

	*size = XLFI_GET_XLF_SIZE((&header));
	num_of_files = XLFI_GET_NUM_OF_FILES((&header));
	/*
	 * Supply non-null file handle so that xlfi_validate can perform more
	 * extensive validation
	 */
	file = malloc(sizeof(struct xlfi_file_info) * num_of_files);
	if(file == NULL) {
		TPT_ERROR(STR("malloc %d size failed.",
		              sizeof(struct xlfi_file_info) * num_of_files));
		goto validate_partition_end;
	}

	data = malloc(*size);
	if(data == NULL) {
		TPT_ERROR(STR("malloc %d size failed.", *size));
		goto validate_partition_end;
	}

	if(mtd_read(path, data, *size)) {
		goto validate_partition_end;
	}

	xlfi_res = xlfi_validate(&state, &header, data +
	                         sizeof(struct xlf_iboot_header),
	                         *size - sizeof(struct xlf_iboot_header),
	                         &num_of_files, file, &done, &error);

	if(xlfi_res != XLFI_RESULT_SUCCESS) {
		goto validate_partition_end;
	}

	*xlf_state = XLFI_XLF_HAS_WORKING_STATUS((&header)) ?
		XLF_VALID_AND_WORKING : XLF_VALID;

	ret = 0;

validate_partition_end:
	free(file);
	free(data);
	return ret;
}

static int compare_partition(const char *spare_path, const char *backup_path)
{
	struct xlf_iboot_header spare_header;
	struct xlf_iboot_header backup_header;

	if(mtd_read(backup_path, &backup_header,
	            sizeof(struct xlf_iboot_header))) {
		return 1;
	}

	if(mtd_read(spare_path, &spare_header,
	            sizeof(struct xlf_iboot_header))) {
		return 1;
	}

	if(memcmp(&spare_header, &backup_header,
	          sizeof(struct xlf_iboot_header))) {
		return 1;
	}

	return 0;
}


static enum recover_action
get_recover_action(
	const char *spare_path,
	const char *backup_path,
	struct xlf_info *spare_xlf_info,
	int *diff)
{
	enum recover_action action = RECOVER_ACTION_NONE;

	*diff = compare_partition(spare_path, backup_path);

	if(!validate_partition(spare_path, &spare_xlf_info->size,
	                       &spare_xlf_info->state)) {
		if(*diff) {
			if(spare_xlf_info->state == XLF_VALID_AND_WORKING) {
				action = RECOVER_ACTION_SAVE;
				goto boot_spare_end;
			}

			char *value = getenv("SYS_BOOT_MODE");
			if(value != NULL && !strcmp(value, "WORKING")) {
				action = RECOVER_ACTION_RESTORE;
			}
		}
	} else if(*diff) {
		action = RECOVER_ACTION_RESTORE;
	}
boot_spare_end:
	return action;
}

static void recover_partitions(void)
{
	uint32_t i;
	enum recover_action boot_spare_action;
	enum recover_action bootenv_spare_action;
	enum recover_action bpar_spare_action;

	char *boot_dev_name[NUM_OF_PARTITIONS];
	char *bootenv_dev_name[NUM_OF_PARTITIONS];
	char *bpar_dev_name[NUM_OF_PARTITIONS];

	int boot_diff, bootenv_diff, bpar_diff;

	for(i = 0; i < NUM_OF_PARTITIONS; i++) {
		boot_dev_name[i] = getenv(boot_partition_env_var[i]);
		bootenv_dev_name[i] = getenv(bootenv_partition_env_var[i]);
		bpar_dev_name[i] = getenv(bpar_partition_env_var[i]);
	}

	boot_spare_action = get_recover_action(
		boot_dev_name[PARTITION_SPARE],
		boot_dev_name[PARTITION_BACKUP],
		&boot_xlf_info[PARTITION_SPARE],
		&boot_diff);

	bootenv_spare_action = get_recover_action(
		bootenv_dev_name[PARTITION_SPARE],
		bootenv_dev_name[PARTITION_BACKUP],
		&bootenv_xlf_info[PARTITION_SPARE],
		&bootenv_diff);

	bpar_spare_action = get_recover_action(
		bpar_dev_name[PARTITION_SPARE],
		bpar_dev_name[PARTITION_BACKUP],
		&bpar_xlf_info[PARTITION_SPARE],
		&bpar_diff);

	if(boot_spare_action == RECOVER_ACTION_RESTORE ||
	   bootenv_spare_action == RECOVER_ACTION_RESTORE ||
	   bpar_spare_action == RECOVER_ACTION_RESTORE) {
		if(boot_diff) {
			if (boot_spare_action != RECOVER_ACTION_RESTORE) {
				TPT_INFO("Need to restore boot spare to avoid "
				         "incosistent state");
			}
			boot_spare_action = RECOVER_ACTION_RESTORE;
		}
		if(bootenv_diff) {
			if (bootenv_spare_action != RECOVER_ACTION_RESTORE) {
				TPT_INFO("Need to restore bootenv spare to "
				         "avoid incosistent state");
			}
			bootenv_spare_action = RECOVER_ACTION_RESTORE;
		}
		if(bpar_diff) {
			if (bpar_spare_action != RECOVER_ACTION_RESTORE) {
				TPT_INFO("Need to restore bpar spare to avoid "
				         "incosistent state");
			}
			bpar_spare_action = RECOVER_ACTION_RESTORE;
		}
	}

	if(boot_spare_action == RECOVER_ACTION_SAVE) {
		if(!copy_partition(boot_dev_name[PARTITION_SPARE],
		                   boot_dev_name[PARTITION_BACKUP],
		                   boot_xlf_info[PARTITION_SPARE].size)) {
			TPT_INFO("boot spare copied to backup.");
		}
	} else if(boot_spare_action == RECOVER_ACTION_RESTORE) {
		if(!get_mtd_size(boot_dev_name[PARTITION_BACKUP],
		                 &boot_xlf_info[PARTITION_BACKUP].size) &&
		   !copy_partition(boot_dev_name[PARTITION_BACKUP],
		                   boot_dev_name[PARTITION_SPARE],
		                   boot_xlf_info[PARTITION_BACKUP].size)) {
			TPT_INFO("boot spare restored from backup.");
		}
	}

	if(bootenv_spare_action == RECOVER_ACTION_SAVE) {
		if(!copy_partition(bootenv_dev_name[PARTITION_SPARE],
		                   bootenv_dev_name[PARTITION_BACKUP],
		                   bootenv_xlf_info[PARTITION_SPARE].size)) {
			TPT_INFO("bootenv spare copied to backup.");
		}
	} else if(bootenv_spare_action == RECOVER_ACTION_RESTORE) {
		if(!get_mtd_size(bootenv_dev_name[PARTITION_BACKUP],
		                 &bootenv_xlf_info[PARTITION_BACKUP].size) &&
		   !copy_partition(bootenv_dev_name[PARTITION_BACKUP],
		                   bootenv_dev_name[PARTITION_SPARE],
		                   bootenv_xlf_info[PARTITION_BACKUP].size)) {
			TPT_INFO("bootenv spare restored from backup.");
		}
	}

	if(bpar_spare_action == RECOVER_ACTION_SAVE) {
		if(!copy_partition(bpar_dev_name[PARTITION_SPARE],
		                   bpar_dev_name[PARTITION_BACKUP],
		                   bpar_xlf_info[PARTITION_SPARE].size)) {
			TPT_INFO("bpar spare copied to backup.");
		}
	} else if(bpar_spare_action == RECOVER_ACTION_RESTORE) {
		if(!get_mtd_size(bpar_dev_name[PARTITION_BACKUP],
		                 &bpar_xlf_info[PARTITION_BACKUP].size) &&
		   !copy_partition(bpar_dev_name[PARTITION_BACKUP],
		                   bpar_dev_name[PARTITION_SPARE],
		                   bpar_xlf_info[PARTITION_BACKUP].size)) {
			TPT_INFO("bpar spare restored from backup.");
		}
	}
}

int main(int argc, char *argv[])
{
	if(argc == 1) {
		recover_partitions();
		return 0;
	} else {
		fprintf(stdout, "Usage: recover_partitions\n");
		return 1;
	}
}
