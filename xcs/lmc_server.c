/*
 * Copyright (C) 2015 by Ericsson AB. All rights reserved. The
 * information in this document is the property of Ericsson. Except
 * as specifically authorized in writing by Ericsson, the receiver
 * of this document shall keep the information contained herein
 * confidential and shall protect the same in whole or in part from
 * disclosure and dissemination to third parties. Disclosure and
 * disseminations to the receiver's employees shall only be made on
 * a strict need to know basis.
 */
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <stdbool.h>
#include <signal.h>
#include <itc.h>
#include <ctype.h>
#include <arpa/inet.h>
#include <sys/ioctl.h>
#include <sys/syscall.h>
#include <sys/resource.h>
#include <fcntl.h>
#include <time.h>
#include <pthread.h>
#include "lmc_server.h"
#include <mtd/mtd-user.h>
#include <unistd.h>
#include "xlfi_api.h"
#include <libresetmem.h>

#define TRACEPOINT_PROVIDER com_ericsson_xcs_lmc_server
#include <tpt_create.h>
#include <tpt.h>

#define MAILBOX_SIZE 5
#define LMC_SLAVE_MBOX  "LMC_SLAVE"
#define MSG_SLAVE_MBOX  "MSG_SLAVE"
#define TIME_SLAVE_MBOX "TIME_SLAVE"

#define MTD_MAX_STR_LEN 32
#define LMC_MAX_LOADFILES 4
#define LMC_MAX_SUBFILES     16
#define CMP_BUF_LEN     4096
#define BOOT_DEFAULT    "boot_default"
#define BOOTENV_DEFAULT "bootenv_default"

#define CHECK_SUM_OK      (0)
#define INVALID_MAGIC_ERROR (-10)
#define CHECK_SUM_ERROR_1   (-11)
#define CHECK_SUM_ERROR_2   (-12)

#define LMC_MAX_LM_BLOCKSIZE  0x1000
#define HANDLE_USED      1
#define HANDLE_UNUSED    0
#define WRITE_MODE       1
#define READ_MODE        0

#define _UNUSED_ __attribute__((__unused__))
#define MEMBER_SIZE(type, member) sizeof(((type*)0)->member)
#define TIMESPEC_TO_MS(x) (x.tv_sec * 1000ULL + x.tv_nsec / 1000000)

/* internal message defination */
#define DELETE_FILE_FWD     (LMC_MSG_BASE + 0xFE)
#define SET_WORKING_BIT_FWD (LMC_MSG_BASE + 0xFD)
#define ERASE_SLOT_FWD      (LMC_MSG_BASE + 0xFC)
#define EXIT_SIGNAL     0xdeadbeef
/* Preloading state */
static enum {
	IDLE,		/* No background preloading active */
	PRE_LOADING     /* Background preloading active */
} pre_load_state = IDLE;

struct lmc_info {
	char mtd_name[MTD_MAX_STR_LEN];
	struct lmc_load_file_entry info;
};

struct handle_entry {
	uint8_t  used;            /* HANDLE_USED or HANDLE_UNUSED */
	uint8_t  *start_addr;     /* Start address of loadfile  */
	uint32_t slot;            /* The loadfile opened 0-3    */
	uint32_t length;          /* Length of loadfile         */
	uint32_t magic;           /* Saved magicno when writing */
	uint32_t server_ref;      /* connection reference for client */
};

struct load_file_data {
	uint32_t magic;        /* lmc magic number */
	uint32_t slot;         /* current writing slot */
	uint16_t seq_nr;       /* current sequence number */
	uint32_t pos;          /* current relative position in flash */
	uint32_t server_ref;   /* connection reference */
};

struct delete_file_fwd {
	BOARD_MSG_HEADER
	int32_t  slot;
};

struct set_working_bit_fwd {
	uint32_t msgno;
};

struct erase_slot_fwd {
	uint32_t msgno;
	uint32_t slot;
};

static uint32_t max_slot;
static struct lmc_info lmc_list[LMC_MAX_LOADFILES];
static int current_slot = -1;
static struct handle_entry lm_handlers[LMC_MAX_SUBFILES];
static struct load_file_data write_data;
static itc_mbox_id_t lmc_server_mbox = ITC_NO_ID;
static itc_mbox_id_t lmc_slave_mbox_id = ITC_NO_ID;
static itc_mbox_id_t msg_slave_mbox_id = ITC_NO_ID;
static itc_mbox_id_t time_slave_mbox_id = ITC_NO_ID;

static 	uint32_t erase_invalid_lmc;
static int build_nof_auboot;

static char *static_partition_name[] = {
	"pboot",
	"boot_default",
	"boot",
	"boot_backup",
	"bootenv_default",
	"bootenv",
	"bootenv_backup",
	"bpar_default",
	"bpar",
	"bpar_backup"};

static void *lmc_slave(__attribute__((unused)) void *arg);
static int mtd_read(char *devname, char *buf, uint32_t size, uint32_t offset);
static int mtd_write(char *devname, const char *buf,
                     uint32_t size, uint32_t offset);

union itc_msg {
	uint32_t msgno;
	LMC_SERVER_MESSAGES;
	/* Internal message structure */
	struct delete_file_fwd delete_fwd;
	struct erase_slot_fwd  erase_fwd;
};

/**
 * Function lmc_list_clear
 * Remove all the elements in the list
 */
static void lmc_list_clear(void)
{
	memset(lmc_list, 0, sizeof(lmc_list));
	for(int i = 0; i < LMC_MAX_LOADFILES; i++) {
		lmc_list[i].info.state = LMC_STATE_ERROR;
		lmc_list[i].info.file_type = XLF_IBOOT_HDR_TYPE_AU_BOOT;
	}
	current_slot = -1;
	return;
}

/**
 * Function pid2lmid
 */
static void pid2lmid(char *lmid, char *pid)
{
	int i;

	for (i = 0; i < XLF_SUID_ID_LEN; i++) {
		if (!isspace(pid[i])) {
			*lmid++ = (char)pid[i];
		}
	}

	*lmid++ = '_';

	for (i = XLF_SUID_ID_LEN;
	     i < XLF_SUID_ID_LEN + XLF_SUID_REV_LEN;
	     i++) {
		if (!isspace(pid[i])) {
			*lmid++ = (char)pid[i];
		}
	}
	*lmid = '\0';
}

/**
 * Function get_max_slot
 * get sys_lmc_number
 */
static void get_max_slot(void)
{
	char lmc_env[15];
	uint32_t i;

	/* reset max slot */
	max_slot = 0;

	for (i = 0; i < LMC_MAX_LOADFILES; i++) {
		sprintf(lmc_env, "sys_lmc%d_dev", i);
		if (getenv(lmc_env) == NULL)
			break;
	}
	max_slot = i;
	return;
}

/**
 * Function get_slot_from_pid
 */
static int get_slot_from_pid(char *pid)
{
	int slot = -1;
	int i;

	for (i = 0; i < max_slot; i++) {
		if (lmc_list[i].info.state != LMC_STATE_VALID)
			continue;
		if (!strcmp(pid, lmc_list[i].info.lmid)) {
			slot = i;
			break;
		}
	}

	return slot;
}

/**
 * Function get_current_slot
 */
static void get_current_slot(void)
{
	char *slot_env = NULL;
	if (current_slot < 0) { /* if uninitialized then get it from env */
		slot_env = getenv("SYS_CUR_SLOT");
		if (slot_env)
			current_slot = atoi(slot_env);
		else
			TPT_ERROR("SYS_CUR_SLOT is not set");
	}
	if (current_slot >= LMC_MAX_LOADFILES) {
		TPT_ERROR(STR("SYS_CUR_SLOT(%d) > LMC_MAX_LOADFILES(%u)",
		              current_slot, LMC_MAX_LOADFILES));
		current_slot = -1;
	} else if (current_slot >= 0) {
		if (lmc_list[current_slot].info.state != LMC_STATE_VALID)
			current_slot = -1;
	}
	return;
}

/**
 * Function check_if_current
 */
static uint32_t check_if_current(char *pid)
{
	uint32_t is_current = 0;

	if (!pid)
		return is_current;

	if (current_slot < 0)
		get_current_slot();

	if (current_slot >= 0) {
		if (!strcmp(pid, lmc_list[current_slot].info.lmid))
			is_current = 1;
	}

	return is_current;
}

/**
 * Function check_slot_locked
 */
static uint32_t check_slot_locked(uint32_t index)
{
	char *lock;

	if (index != 1)
		return 0;

	lock = getenv("SYS_LOCK_SLOT");
	if (lock != NULL) {
		if (lock[0] == 'Y' || lock[0] == 'y')
			return 1;
		else if (lock[0] != 'N' && lock[0] != 'n')
			TPT_ERROR(STR("SYS_LOCK_SLOT has invalid value %s",
			              lock));
	}

	return 0;
}

static uint32_t partition_writeable(char *devname)
{
	uint32_t flag = 0;
	mtd_info_t mtd_info;
	int fd;

	fd = open(devname, O_RDONLY | O_SYNC | O_NONBLOCK);
	if (fd < 0) {
		TPT_ERROR(STR("open %s failed with errno %d",
		              devname, errno));
		return 0;
	} else if (ioctl(fd, MEMGETINFO, &mtd_info) == -1) {
		TPT_ERROR(STR("ioctl MEMGETINFO %s failed with errno %d",
		              devname, errno));
	} else {
		flag = mtd_info.flags;
	}

	if (close(fd) == -1) {
		TPT_ERROR(STR("close %s failed with errno %d", devname, errno));
		return 0;
	}

	if(flag & MTD_WRITEABLE) {
		return 1;
	} else {
		return 0;
	}
}
/**
 * Function calc_checksum
 * Checksum algorithm is described in 1/1551-CEH10190/1
 */
static uint32_t calc_checksum(uint8_t *p, uint32_t size)
{
	uint32_t sum = 0;
	while(size--) {
		sum += (uint32_t) * p++;
		if (sum > 0xffffL) {
			sum++;
			sum &= 0xffffL;
		}
	}
	return sum;
}

static int32_t validata_crc2(char *dev_name,
                              struct xlf_iboot_header *head,
                              uint16_t crc2)
{
	uint32_t size;
	uint32_t start_addr;
	uint32_t end_addr;
	int32_t  result = -1;
	char    *buf;
	uint16_t cal_crc2;

	if (!head)
		return result;
	start_addr = offsetof(struct xlf_iboot_header, file_count);
	end_addr = ntohl(head->crc2_offset);
	size = end_addr - start_addr;
	buf = malloc(size);
	if (!buf)
		goto validate_crc2_end;
	result = mtd_read(dev_name, buf, size, start_addr);
	if (result)
		goto validate_crc2_end;

	cal_crc2 = calc_checksum((uint8_t *)buf, size);
	if (cal_crc2 != crc2) {
		TPT_TRACE(1, STR("crc2 check failed, crc2 in header = 0x%x"
		                 " crc2_calc = 0x%x", crc2, cal_crc2));
		result = -1;
		goto validate_crc2_end;
	}
	result = 0;

validate_crc2_end:
	free(buf);
	return result;
}

/**
 * Function get_mtd_size
 * common mtd partition size
 */
static uint32_t get_mtd_size(char *devname)
{
	uint32_t size = 0;
	mtd_info_t mtd_info;
	int fd;

	fd = open(devname, O_RDONLY | O_SYNC | O_NONBLOCK);
	if (fd < 0) {
		TPT_ERROR(STR("open %s failed with errno %d",
		              devname, errno));
		return 0;
	}
	else if (ioctl(fd, MEMGETINFO, &mtd_info) == -1) {
		TPT_ERROR(STR("ioctl %s failed with errno %d",
		              devname, errno));
	}
	else {
		size = mtd_info.size;
	}
	if (close(fd) == -1) {
		TPT_ERROR(STR("close %s failed with errno %d", devname, errno));
		return 0;
	}
	return size;
}

/**
 * Function mtd_read
 * common mtd read function
 */
static int mtd_read(char *devname,
                    char *buf,
                    uint32_t size,
                    uint32_t offset)
{
	mtd_info_t mtd_info;
	int ret = -1;
	int lseek_ret, read_ret;

	int fd = open(devname, O_RDONLY | O_SYNC | O_NONBLOCK);
	if (fd < 0) {
		TPT_ERROR(STR("open %s failed with errno %d", devname, errno));
		return ret;
	}

	if (ioctl(fd, MEMGETINFO, &mtd_info) == -1) {
		TPT_ERROR(STR("ioctl %s failed with errno %d", devname, errno));
		goto mtd_read_end;
	}

	if (offset >= mtd_info.size) {
		TPT_ERROR(STR("read with offset:0x%x is out of %s partition",
		              offset, devname));
		goto mtd_read_end;
	}

	lseek_ret = lseek(fd, offset, SEEK_SET);
	if (lseek_ret == -1) {
		TPT_ERROR(STR("lseek failed with errno %d", errno));
		goto mtd_read_end;
	} else if (lseek_ret != offset) {
		TPT_ERROR(STR("lseek return %d is not as expected %d",
		              lseek_ret, offset));
		goto mtd_read_end;
	}


	uint32_t rd_chunk_size = mtd_info.erasesize;
	uint32_t bytes_remain = size;
	char * buf_p = buf;
	while(bytes_remain > 0) {
		if (bytes_remain < rd_chunk_size) {
			read_ret = read(fd, buf_p, bytes_remain);
		} else {
			read_ret = read(fd, buf_p, rd_chunk_size);
		}
		//printf ("read %d (total %d of %u) bytes\n", read_ret, (int) (buf_p - buf) + read_ret, size);
		if (read_ret == -1) {
			TPT_ERROR(STR(" MTD read failed with errno %d", errno));
			goto mtd_read_end;
		} else if (read_ret == 0) {
			//eof met
			TPT_INFO(STR("warning: unexpected EOF on [%s], bytes_remain = %d; total_requested=%d", devname, bytes_remain, size));
			bytes_remain = 0;
		} else {
			buf_p += read_ret;
			if (bytes_remain > read_ret) bytes_remain -= read_ret;
			else bytes_remain = 0; //I hate unsigned vlaues
		}
	}
	ret = 0;

mtd_read_end:
	if (close(fd) == -1) {
		TPT_ERROR(STR("close %s failed with errno %d", devname, errno));
		return -1;
	}
	return ret;
}

/**
 * Function mtd_write
 * common mtd write function
 */
static int mtd_write(char *devname,
                     const char *buf,
                     uint32_t size,
                     uint32_t offset)
{
	mtd_info_t mtd_info;
	int ret = -1;
	int lseek_ret, write_ret;

	int fd = open(devname, O_WRONLY | O_SYNC | O_NONBLOCK);
	if (fd < 0) {
		TPT_ERROR(STR("open %s failed with errno %d",
		              devname, errno));
		return -1;
	}
	if (ioctl(fd, MEMGETINFO, &mtd_info) == -1) {
		TPT_ERROR(STR("ioctl %s failed with errno %d",
		              devname, errno));
		goto mtd_write_end;
	}

	if((size + offset)  > mtd_info.size) {
		TPT_ERROR(STR("requested size 0x%x @ offset: 0x%x exceeds "
		              "partition size 0x%x",
		              size, offset, mtd_info.size));
		goto mtd_write_end;
	}

	lseek_ret = lseek(fd, offset, SEEK_SET);
	if(lseek_ret == -1) {
		TPT_ERROR(STR("lseek failed with errno %d", errno));
		goto mtd_write_end;
	} else if(lseek_ret != offset) {
		TPT_ERROR(STR("lseek return %d is not as expected %d",
		              lseek_ret, offset));
		goto mtd_write_end;
	}

	write_ret = write(fd, buf, size);
	if(write_ret == -1) {
		TPT_ERROR(STR("write failed with errno %d", errno));
		goto mtd_write_end;
	} else if(write_ret != size) {
		TPT_INFO(STR("warning:write %d bytes"
		             " which is not as expected %d bytes",
		             write_ret, size));
	}

	ret = 0;

mtd_write_end:
	if(close(fd) == -1) {
		TPT_ERROR(STR("close %s failed with errno %d",
		              devname, errno));
		ret = -1;
	}

	return ret;

}

/*FIXME: Fix the lock problem in the flash driver*/
#if 0
/**
 * Function mtd_lock
 * lock flash partition
 */
static int mtd_lock(char *devname)
{
	mtd_info_t mtd_info;
	erase_info_t erase_info;
	int ret = -1;
	int fd;

	fd = open(devname, O_WRONLY | O_SYNC | O_NONBLOCK);
	if (fd < 0) {
		TPT_ERROR(STR("open %s failed with errno %d", devname, errno));
		return ret;
	}
	if (ioctl(fd, MEMGETINFO, &mtd_info) == -1) {
		TPT_ERROR(STR("get info %s failed with errno %d",
		          devname, errno));
		goto mtd_lock_end;
	}
	erase_info.length = mtd_info.erasesize;
	for (erase_info.start = 0;
	     erase_info.start < mtd_info.size;
	     erase_info.start += mtd_info.erasesize) {

		if (ioctl(fd, MEMLOCK, &erase_info) == -1) {
			TPT_ERROR(STR("lock %s failed with errno %d",
			              devname, errno));
			goto mtd_lock_end;
		}
	}

	ret = 0;

mtd_lock_end:
	if (close(fd) == -1) {
		TPT_ERROR(STR("close %s failed with errno %d",
		              devname, errno));
		return -1;
	}
	return ret;
}
#endif
/**
 * Function mtd_erase
 * erase flash partition
 */
static int mtd_erase(char *devname)
{
	mtd_info_t mtd_info;
	erase_info_t erase_info;
	int ret = -1;
	int fd;

	fd = open(devname, O_WRONLY | O_SYNC | O_NONBLOCK);
	if (fd < 0) {
		TPT_ERROR(STR("open %s failed with errno %d",
		              devname, errno));
		return ret;
	}
	if (ioctl(fd, MEMGETINFO, &mtd_info) == -1) {
		TPT_ERROR(STR("get info %s failed with errno %d",
		              devname, errno));
		goto mtd_erase_end;
	}
	erase_info.length = mtd_info.erasesize;
	for (erase_info.start = 0;
	     erase_info.start < mtd_info.size;
	     erase_info.start += mtd_info.erasesize) {
#if 0
		if (ioctl(fd, MEMUNLOCK, &erase_info) == -1) {
			TPT_ERROR(STR("Unlock %s failed with errno %d",
			              devname, errno));
			goto mtd_erase_end;
		}
#endif
		if (ioctl(fd, MEMERASE, &erase_info) == -1) {
			TPT_ERROR(STR("Erase %s failed with errno %d",
			              devname, errno));
			goto mtd_erase_end;
		}
	}

	ret = 0;

mtd_erase_end:
	if (close(fd) == -1) {
		TPT_ERROR(STR("close %s failed with errno %d",
		              devname, errno));
		return -1;
	}
	return ret;
}

/**
 * Function flash_blank_check
 */
static uint32_t flash_blank_check(char *lmc_dev, uint32_t size)
{
#if CMP_BUF_LEN % 4
	#error CMP_BUF_LEN must be aligned to 4 byte boundary
#endif
	uint32_t i;
	uint32_t len, len_left = size;
	uint32_t res = 1;
	int32_t  fd = -1;
	uint32_t *buf = NULL;
	int32_t  read_ret;

	if (size & (sizeof(uint32_t) - 1)) {
		TPT_ERROR(STR("size %d not aligned to %d byte boundary",
		              size, sizeof(uint32_t)));
		goto blank_check_end;
	}

	buf = malloc(CMP_BUF_LEN);
	if (buf == NULL) {
		TPT_ERROR(STR("malloc failed for size %d", CMP_BUF_LEN));
		goto blank_check_end;
	}

	fd = open(lmc_dev, O_RDONLY | O_SYNC | O_NONBLOCK);
	if (fd < 0) {
		TPT_ERROR(STR("open %s failed with errno %d", lmc_dev, errno));
		goto blank_check_end;
	}

	while (len_left) {
		len = (len_left > CMP_BUF_LEN) ? CMP_BUF_LEN : len_left;
		read_ret = read(fd, buf, len);
		if (read_ret == -1) {
			TPT_ERROR(STR("MTD read failed with errno %d", errno));
			goto blank_check_end;
		} else if (read_ret != len) {
			TPT_ERROR(STR("warning: read %d bytes while expected %d"
			              " bytes", read_ret, len));
			goto blank_check_end;
		}
		for (i = 0; i < len >> 2; i++) {
			if (buf[i] != 0xFFFFFFFF)
				goto blank_check_end;
		}
		len_left -= len;
	}
	res = 0;
blank_check_end:
	free(buf);
	if (fd >= 0 && close(fd) == -1) {
		TPT_ERROR(STR("close %s failed with errno %d", lmc_dev, errno));
		res = 1;
	}
	return res;
}

/**
 * Function read_lmc_header
 */
static int read_lmc_header(struct xlf_iboot_header **header_p, char *lmc_dev)
{
	int retval;
	uint32_t subfile_counter;
	uint32_t header_size;
	uint32_t offset = 0;
	uint32_t magic;

	/* First we read out magic from LMC header */
	retval = mtd_read(lmc_dev, (char *)&magic,
	                  sizeof(uint32_t), offset);
	if (retval) {
		TPT_ERROR(STR("Read LMC header failed on %s", lmc_dev));
		goto read_lmc_header_end;
	}

	magic = ntohl(magic);
        TPT_INFO(STR("read lmc header first read magic is 0x%x",magic));
	if (magic != XLF_IBOOT_HDR_MAGIC &&
	    magic != 0xffffffff) { /* Erased */
		TPT_TRACE(1, STR("Invalid LMC header \"magic=0x%x\" in %s\n",
		                 magic, lmc_dev));
		retval = -1;
		goto read_lmc_header_end;
	}

	if (magic == 0xffffffff) {
		header_size = sizeof(struct xlf_iboot_header);
		*header_p = malloc(header_size);
		if (*header_p == NULL) {
			TPT_ERROR(STR("malloc failed for %s header",
			              lmc_dev));
			retval = -1;
			goto read_lmc_header_end;
		}
		retval = mtd_read(lmc_dev, (char *)*header_p, header_size, 0);
		goto read_lmc_header_end;
	}

	offset = offsetof(struct xlf_iboot_header, file_count);

	retval = mtd_read(lmc_dev, (char *)&subfile_counter,
	                  sizeof(uint32_t), offset);
	if (retval) {
		TPT_ERROR(STR("Read LMC header failed on %s", lmc_dev));
		goto read_lmc_header_end;
	}

	subfile_counter = ntohl(subfile_counter);
	if (subfile_counter > LMC_MAX_SUBFILES) {
		TPT_TRACE(1, STR("%s has too many(%u) subfiles",
		          lmc_dev, subfile_counter));
		retval = -1;
		goto read_lmc_header_end;
	}
	header_size = sizeof(struct xlf_iboot_header) +
	              subfile_counter * sizeof(uint32_t);
	*header_p = malloc(header_size);
	if (*header_p == NULL) {
		TPT_ERROR(STR("malloc failed for %s header", lmc_dev));
		retval = -1;
		goto read_lmc_header_end;
	}

	retval = mtd_read(lmc_dev, (char *)*header_p, header_size, 0);
	if (retval) {
		TPT_ERROR(STR("read LMC full header failed on %s", lmc_dev));
		goto read_lmc_header_end;
	}
	retval = 0;

read_lmc_header_end:
	return retval;
}

/**
 * Function check_boot_partitions
 * Returns:
 * - < 0 if fatal error that should result in reboot
 * - 0 on success.
 * - > 0 if partitions are valid but not in working state, which means that
 *     automatic erase of invalid slots should not be performs at startup.
 */
static int check_boot_partitions()
{
	uint32_t i, res = -1, non_working_count = 0;
	char *env_name = NULL;
	struct xlf_iboot_header *header = NULL;
	const struct {
		char *env_name;
		char *default_partition;
	} boot[] =
	{
		{"SYS_CUR_BOOT", "boot_default"},
		{"SYS_CUR_BOOTENV", "bootenv_default"}
	};

	for (i = 0; i < sizeof(boot) / sizeof(boot[0]); i++) {
		char *env_value;
		uint32_t size;

		env_value = getenv(boot[i].env_name);
		if (env_value == NULL) {
			TPT_ERROR(STR("%s not found, aborting",
			              boot[i].env_name));
			goto check_boot_partitions_end;
		}

		if (!strcmp(env_value, boot[i].default_partition)) {
			continue;
		}

		size = strlen(env_value) + sizeof("sys__dev");
		env_name = malloc(size);
		if (env_name == NULL) {
			TPT_ERROR(STR("malloc failed for size %d", size));
			goto check_boot_partitions_end;
		}

		sprintf(env_name, "sys_%s_dev", env_value);
		env_value = getenv(env_name);
		if (env_value == NULL) {
			TPT_ERROR(STR("%s not found, aborting", env_name));
			goto check_boot_partitions_end;
		}

		free(env_name);
		env_name = NULL;

		if (read_lmc_header(&header, env_value)) {
			TPT_TRACE(1, STR("Failed to read %s", env_value));
			goto check_boot_partitions_end;
		}

		if (!XLFI_XLF_HAS_WORKING_STATUS(header)) {
			non_working_count++;
		}

		free(header);
		header = NULL;
	}

	res = non_working_count;

check_boot_partitions_end:
	free(env_name);
	free(header);
	return res;
}

/**
 * Function extract_lmc_info
 */
static int extract_lmc_info(struct lmc_load_file_entry *lmc_info,
                            char *lmc_dev)
{
	struct xlf_iboot_header *header = NULL;
	int res;
	char    *error;
	uint16_t crc2;
	struct xlfi_parse_state state;

	res = read_lmc_header(&header, lmc_dev);
	if (res) {
		TPT_TRACE(1, STR("Failed to read %s", lmc_dev));
		goto extract_lmc_end;
	}

	if (ntohl(header->magic) == 0xffffffff) {
		if (flash_blank_check(lmc_dev, lmc_info->slot_size))
			res = 1;
		else
			lmc_info->state = LMC_STATE_ERASED;
		goto empty_slot;
	}
	/* validate lmc */
	res = xlfi_validate_header(&state, header, (const char **)&error);
	if (res) {
		TPT_TRACE(1, STR("LMC header verification failed: %s", error));
		goto extract_lmc_end;
	}
	res = mtd_read(lmc_dev, (char *)&crc2, sizeof(uint16_t),
	               ntohl(header->crc2_offset));
	if (res) {
		TPT_ERROR("Read crc2 from lmc failed");
		goto extract_lmc_end;
	}
	res = validata_crc2(lmc_dev, header, ntohs(crc2));
	if (res) {
		TPT_TRACE(1, "Verify crc2 failed");
		goto extract_lmc_end;
	}
	res = 0;
	lmc_info->state = LMC_STATE_VALID;
empty_slot:
	lmc_info->lmc_size = XLFI_GET_XLF_SIZE(header);
	lmc_info->seq_number = ntohl(header->seq_number);
	/* lmc_info->slot_size is set by get_mtd_size() */
	lmc_info->file_type = ntohl(header->type);
	lmc_info->time = ntohl(header->time);
	lmc_info->subfile_counter = ntohl(header->file_count);
	lmc_info->working = XLFI_XLF_HAS_WORKING_STATUS(header) ? 1 : 0;
	pid2lmid(lmc_info->lmid, header->suid);

extract_lmc_end:
	free(header);
	return res;
}

/**
 * Function set_lmc_working
 */
static void set_lmc_working(int32_t slot)
{
	int32_t retval;
	uint32_t offset = offsetof(struct xlf_iboot_header, update);
	uint32_t update;
	char *dev_name;

	if (slot < 0)
		return;

	dev_name = lmc_list[slot].mtd_name;
	retval = mtd_read(dev_name, (char *)&update, sizeof(uint32_t), offset);
	if (retval) {
		TPT_ERROR("Read working bit failed");
		return;
	}
	update = ntohl(update);
	if ((update & XLF_IBOOT_HDR_UPDATE_WORKING_BIT) != 0) {
		update = ~XLF_IBOOT_HDR_UPDATE_WORKING_BIT;
		update = htonl(update);
		retval = mtd_write(dev_name, (char *)&update,
		                   sizeof(uint32_t), offset);
		if (retval) {
			TPT_ERROR("Write working bit failed");
			return;
		}
		/* check if working*/
		retval = mtd_read(dev_name, (char *)&update,
		                  sizeof(uint32_t), offset);
		if (retval) {
			TPT_ERROR("Check working bit failed");
			return;
		}
		update = ntohl(update);
		if ((update & XLF_IBOOT_HDR_UPDATE_WORKING_BIT) != 0) {
			TPT_ERROR("Set working bit failed");
			return;
		}
	}
	lmc_list[slot].info.working = 1;
	return;
}

/**
 * Function read_lm_header
 */
static int read_lm_header(struct xlf_blob_header *header, char *lmc_dev,
                          uint32_t offset)
{
	int retval;
	uint32_t header_size = sizeof(struct xlf_blob_header);

	retval = mtd_read(lmc_dev, (char *)header, header_size, offset);
	if (retval) {
		TPT_ERROR(STR("read LM header failed on %s", lmc_dev));
		goto read_lm_header_end;
	}
	retval = 0;

read_lm_header_end:
	return retval;
}

/**
 * Function extract_subfile_info
 */
static int extract_subfile_info(union lmc_subfile_info *lm_info,
                                struct xlf_iboot_header *lmc_header,
				uint32_t index,
				char *dev_name)
{
	uint32_t offset;
	uint32_t magic;
	int32_t  retval = -1;

	offset = ntohl(lmc_header->file_offset[index]);
	/* Read subfile magic */
	if (mtd_read(dev_name, (char *)&magic, sizeof(uint32_t), offset)) {
		TPT_ERROR(STR("read subfile %u magic failed", index));
		goto extract_lm_end;
	}
	magic = ntohl(magic);
	lm_info->magic = magic;

	if (magic == XLF_IBOOT_WPR_MAGIC_BLOB) {

		struct xlf_blob_header header;
		/* After IBOOT wrapper*/
		offset = offset + MEMBER_SIZE(struct xlf_iboot_wrapper,
		                              magic);

		if (read_lm_header(&header, dev_name, offset)) {
			TPT_TRACE(1, STR("read subfile %u header failed", index));
			goto extract_lm_end;
		}

		lm_info->blob.info.crc32 = ntohl(header.crc32);
		lm_info->blob.info.header_size = ntohl(header.header_size);
		lm_info->blob.info.type = ntohl(header.type);
		lm_info->blob.info.major_version = ntohl(header.major_version);
		lm_info->blob.info.minor_version = ntohl(header.minor_version);
		pid2lmid(lm_info->blob.info.lmid, header.suid);
		lm_info->blob.info.time = ntohl(header.time);
		strncpy(lm_info->blob.info.name,
		        header.name, XLF_BLOB_NAME_SIZE);
		lm_info->blob.info.name[XLF_BLOB_NAME_SIZE - 1] = '\0';
	} else {
		TPT_TRACE(1, STR("User subfile type with magic no:0x%x",
		                 magic));
	}

	retval = 0;
extract_lm_end:
	return retval;
}

/**
 * Function update_seq_number
 */
static int update_seq_number(void)
{
	uint32_t i;
	uint32_t seq_nr = 0;
	int nof_auboot = 0;
	uint32_t nof_applic = 0;
	uint32_t auboot_index = 0;
	uint32_t applic_index = 0;

	if (max_slot == 0)
		return nof_auboot;
	for (i = 0; i < max_slot; i++) {
		if (lmc_list[i].info.state != LMC_STATE_VALID)
			continue;
		if (lmc_list[i].info.file_type == XLF_IBOOT_HDR_TYPE_AU_BOOT) {
			nof_auboot++;
			auboot_index = i;
		}

		if (lmc_list[i].info.file_type ==
		    XLF_IBOOT_HDR_TYPE_AU_APPLIC) {
			nof_applic++;
			applic_index = i;
		}
	}

	if (nof_auboot == 1) {
		set_lmc_working(auboot_index);
		if (lmc_list[auboot_index].info.seq_number == 0xFFFFFFFF) {
			(void)mtd_write(lmc_list[auboot_index].mtd_name,
			                (char *)&seq_nr, sizeof(uint32_t),
			                offsetof(struct xlf_iboot_header,
			                seq_number));
			lmc_list[auboot_index].info.seq_number = seq_nr;
		}
	}
	if ((nof_applic == 1) &&
	    (lmc_list[applic_index].info.seq_number == 0xFFFFFFFF)) {
		(void)mtd_write(lmc_list[applic_index].mtd_name,
		                (char *)&seq_nr, sizeof(uint32_t),
		                offsetof(struct xlf_iboot_header,
		                seq_number));
		lmc_list[applic_index].info.seq_number = seq_nr;
	}
	return nof_auboot;
}

/**
 * Function erase_slot
 */
static uint32_t erase_slot(uint32_t slot)
{
	uint32_t result;
	if (mtd_erase(lmc_list[slot].mtd_name)) {
		TPT_ERROR(STR("Delete failed: erase lmc%d failed", slot));
		lmc_list[slot].info.state = LMC_STATE_ERROR;
		result = LMC_RESULT_OTHER_ERROR;
		return result;
	}
	/* Update lmc list */
	lmc_list[slot].info.state = LMC_STATE_ERASED;
	lmc_list[slot].info.slot_size = get_mtd_size(lmc_list[slot].mtd_name);
	result = LMC_RESULT_SUCCESS;
	return result;
}

/**
 * Function build_lmc_list
 */
static int build_lmc_list(void)
{
	char *dev_name;
	int nof_auboot;
	char lmc_str[sizeof"sys_lmc4294967295_dev"];
	int i;

	for (i = 0; i < max_slot; i++) {

		sprintf(lmc_str, "sys_lmc%d_dev", i);
		dev_name = getenv(lmc_str);
		if (!dev_name) {
			TPT_ERROR(STR("%s is not set", lmc_str));
			continue;
		}
		strncpy(lmc_list[i].mtd_name, dev_name,
		        sizeof(lmc_list[i].mtd_name));
		lmc_list[i].mtd_name[MTD_MAX_STR_LEN -1] = '\0';

		lmc_list[i].info.slot_size = get_mtd_size(dev_name);

		if(extract_lmc_info(&lmc_list[i].info, dev_name)) {
			lmc_list[i].info.state = LMC_STATE_ERROR;
		}
		lmc_list[i].info.is_current = 0;
		sprintf(lmc_list[i].info.partition_name, "lmc%d", i);
		lmc_list[i].info.permissions = 0;
		if(!partition_writeable(dev_name)) {
			lmc_list[i].info.permissions |=
				LMC_LOAD_FILE_PERMISSION_RO;
		}
		lmc_list[i].info.permissions |= (check_slot_locked(i) ?
		                             LMC_LOAD_FILE_PERMISSION_LOCKED :
		                             LMC_LOAD_FILE_PERMISSION_UNLOCKED);
	}

	get_current_slot();
	if (current_slot >= 0)
		lmc_list[current_slot].info.is_current = 1;

	nof_auboot = update_seq_number();

	return nof_auboot;
}

static uint32_t is_current_boot(char * env_name, char * partition_name)
{
	char *cur_boot, *cur_bootenv;

	if(!strcmp(env_name, "sys_pboot_dev")) {
		return 1;
	}

	cur_boot = getenv("SYS_CUR_BOOT");
	if(cur_boot != NULL &&
	   !strcmp(partition_name, cur_boot)) {
		return 1;
	}

	cur_bootenv = getenv("SYS_CUR_BOOTENV");
	if(cur_bootenv != NULL &&
	   !strcmp(partition_name, cur_bootenv)) {
		return 1;
	}
	return 0;
}

/**
 * Function get_static_partition_info
 */
static int get_static_partition_info(char *partition_name,
                                     struct lmc_load_file_entry *info)
{
	char *env_name = NULL, *dev_name = NULL;
	int ret = -1;

	info->state = LMC_STATE_NON_EXISTENT;

	env_name = malloc(strlen(partition_name) + sizeof("sys__dev"));
	if(env_name == NULL) {
		TPT_ERROR(STR("malloc %d failed",
			      strlen(partition_name) + sizeof("sys__dev")));
		goto get_static_partition_info_end;
	}

	sprintf(env_name, "sys_%s_dev", partition_name);

	dev_name = getenv(env_name);
	if (dev_name == NULL) {
		TPT_ERROR(STR("%s doesn't exist", env_name));
		goto get_static_partition_info_end;
	}

	info->slot_size = get_mtd_size(dev_name);

	if(extract_lmc_info(info, dev_name)) {
		info->state = LMC_STATE_ERROR;
	}
	strncpy(info->partition_name, partition_name, strlen(partition_name));
	info->partition_name[strlen(partition_name)] = '\0';
	info->permissions = 0;
	if(!partition_writeable(dev_name)) {
		info->permissions |=
			LMC_LOAD_FILE_PERMISSION_RO;
	}
	info->is_current = is_current_boot(env_name, partition_name);
	ret = 0;

get_static_partition_info_end:
	free(env_name);
	return ret;
}

/**
 * Function get_lmc_list
 */
static void get_lmc_list(union itc_msg *msg, struct conn_client_info *ci)
{
	int i = 0;
	int j;
	uint32_t size;
	uint32_t nof_lmcs = max_slot;
	struct lmc_get_load_file_info_cfm *lmc_info;

	if(msg->get_load_file_info_req.flags &
	   LMC_GET_LOAD_FILE_INFO_REQ_FLAGS_ALL) {
		nof_lmcs = max_slot +
		        sizeof(static_partition_name)/
		        sizeof(static_partition_name[0]);
	}
	size = sizeof(struct lmc_get_load_file_info_cfm) +
		nof_lmcs * sizeof(struct lmc_load_file_entry);

	lmc_info = (void *) itc_alloc(size, LMC_GET_LOAD_FILE_INFO_CFM);
	lmc_info->procedure_ref = msg->get_load_file_info_req.procedure_ref;
	lmc_info->connection_ref = ci->client_ref;
	lmc_info->lmc_count = nof_lmcs;

	/*Set whole array to 0, in case we have a gap in the LMC list below */
	memset(lmc_info->info, 0,
	       nof_lmcs * sizeof(struct lmc_load_file_entry));

	if(msg->get_load_file_info_req.flags &
	   LMC_GET_LOAD_FILE_INFO_REQ_FLAGS_ALL) {
		for(i = 0;
		    i < sizeof(static_partition_name) /
		            sizeof(static_partition_name[0]);
		    i++) {
			get_static_partition_info(static_partition_name[i],
			                          &lmc_info->info[i]);
		}
	}

	for (j = 0; i < nof_lmcs; i++, j++) {
		lmc_info->info[i].state = LMC_STATE_NON_EXISTENT;
		memcpy(&lmc_info->info[i], &lmc_list[j].info,
		       sizeof(struct lmc_load_file_entry));
	}

	itc_send((union itc_msg **) &lmc_info, itc_sender(msg),
	         lmc_server_mbox);
	return;
}

/**
 * Function get_lm_list
 */
static void get_lm_list(union itc_msg *msg, struct conn_client_info *ci)
{
	int index, retval;
	int i;
	char *lmc_dev;
	uint32_t counter;
	struct xlf_iboot_header *lmc_header = NULL;
	union itc_msg *reply = NULL;

	/* get lmc index from req */
	if (msg->get_subfile_info_req.use_current == 1) {
		get_current_slot();
		if (current_slot < 0) {
			TPT_ERROR("No current LMC exists!");
			retval = LMC_RESULT_NOT_FOUND;
			goto get_lm_list_error;
		}
		index = current_slot;
	} else {
		index = msg->get_subfile_info_req.index;
	}
	/* check index */
	if (index < 0 || index >= max_slot) {
		retval = LMC_RESULT_INVALID_PARAM;
		goto get_lm_list_error;
	}
	/* check LMC */
	lmc_dev = lmc_list[index].mtd_name;
	if (lmc_list[index].info.state != LMC_STATE_VALID) {
		TPT_ERROR(STR("LMC%d is not valid", index));
		retval = LMC_RESULT_NOT_FOUND;
		goto get_lm_list_error;
	}

	retval = read_lmc_header(&lmc_header, lmc_dev);
	if (retval) {
		TPT_TRACE(1, STR("Read lmc%d header failed", index));
		retval = LMC_RESULT_OTHER_ERROR;
		goto get_lm_list_error;
	}
	counter = lmc_list[index].info.subfile_counter;
	reply = itc_alloc(sizeof(struct lmc_get_subfile_info_cfm) +
	                  counter * sizeof(union lmc_subfile_info),
	                  LMC_GET_SUBFILE_INFO_CFM);
	reply->get_subfile_info_cfm.counter = 0;

	for (i = 0; i < lmc_list[index].info.subfile_counter; i++) {
		if (i >= LMC_MAX_SUBFILES) {
			retval = LMC_TOO_MANY_SUBFILES;
			goto get_lm_list_error;
		}

		reply->get_subfile_info_cfm.counter += 1;
		if (extract_subfile_info(&reply->get_subfile_info_cfm.list[i],
		                         lmc_header, i, lmc_dev)) {
			retval = LMC_RESULT_NOT_FOUND;
			goto get_lm_list_error;
		}
	}

	free(lmc_header);
	reply->get_subfile_info_cfm.procedure_ref = ci->procedure_ref;
	reply->get_subfile_info_cfm.connection_ref = ci->client_ref;
	itc_send(&reply, itc_sender(msg), lmc_server_mbox);
	return;

get_lm_list_error:
	if (reply)
		itc_free(&reply);
	reply = itc_alloc(sizeof(struct lmc_get_subfile_info_rej),
			  LMC_GET_SUBFILE_INFO_REJ);
	reply->get_subfile_info_rej.procedure_ref = ci->procedure_ref;
	reply->get_subfile_info_rej.connection_ref = ci->client_ref;
	reply->get_subfile_info_rej.error_code = retval;
	free(lmc_header);
	itc_send(&reply, itc_sender(msg), lmc_server_mbox);

	return;
}

/**
 * Function lm_open
 */
static void lm_open(union itc_msg *msg, struct conn_client_info *ci)
{
	union itc_msg *reply;
	struct xlf_iboot_header *lmc_header = NULL;
	struct xlf_iboot_wrapper iboot_wrapper;
	uint8_t  *lm_start;
	uint32_t lm_len;
	uint32_t server_ref;
	uint32_t rej_size = sizeof(struct lmc_load_subfile_open_rej);
	uint32_t cfm_size = sizeof(struct lmc_load_subfile_open_cfm);
	uint32_t wrapper_size = sizeof(struct xlf_iboot_wrapper);
	uint32_t offset;
	uint32_t lmc_index, lm_index;
	int32_t i, handle, retval;
	char *lmc_dev;

	lmc_index = msg->load_subfile_open_req.lmc_index;
	lm_index = msg->load_subfile_open_req.lm_index;
	server_ref = msg->load_subfile_open_req.connection_ref;

	/* check LMC info */
	if (lmc_index >= max_slot) {
		TPT_ERROR(STR("LMC index %d is not valid", lmc_index));
		retval = LMC_RESULT_INVALID_PARAM;
		goto lm_open_end;
	}
	if (lmc_list[lmc_index].info.state != LMC_STATE_VALID) {
		TPT_ERROR(STR("LMC%d is not valid", lmc_index));
		retval = LMC_RESULT_NOT_FOUND;
		goto lm_open_end;
	}
	/* Get LM info */
	if (lm_index >= lmc_list[lmc_index].info.subfile_counter) {
		TPT_ERROR(STR("LM index %d >= subfile counter %d",
		              lm_index,
		              lmc_list[lmc_index].info.subfile_counter));
		retval = LMC_RESULT_INVALID_PARAM;
		goto lm_open_end;
	}
	/* read header to get lm offset*/
	lmc_dev = lmc_list[lmc_index].mtd_name;

	retval = read_lmc_header(&lmc_header, lmc_dev);
	if (retval) {
		TPT_TRACE(1, STR("Failed to read LMC%d header", lmc_index));
		retval = LMC_RESULT_OTHER_ERROR;
		goto lm_open_end;
	}

	offset = ntohl(lmc_header->file_offset[lm_index]);
	/*start of iboot wrapper*/
	offset = offset - sizeof(iboot_wrapper.end_offset);
	/* After IBOOT wrapper*/
	lm_start = (uint8_t *)(offset + wrapper_size);

	retval = mtd_read(lmc_dev, (char *)&iboot_wrapper, wrapper_size, offset);
	if (retval) {
		TPT_ERROR(STR("Read iboot wrapper failed on %s", lmc_dev));
		retval = LMC_RESULT_OTHER_ERROR;
		goto lm_open_end;
	}

	lm_len = ntohl(iboot_wrapper.end_offset) + 1 - wrapper_size;
	/* find a entry in the handle table */
	handle = -1;
	for (i = 0; i < LMC_MAX_SUBFILES; i++) {
		if (lm_handlers[i].used == HANDLE_UNUSED) {
			handle = i;
			lm_handlers[i].used = HANDLE_USED;
			lm_handlers[i].length = lm_len;
			lm_handlers[i].start_addr = lm_start;
			lm_handlers[i].slot = lmc_index;
			lm_handlers[i].server_ref = server_ref;
			break;
		}
	}

	if (handle >= 0) {
		reply = itc_alloc(cfm_size, LMC_LOAD_SUBFILE_OPEN_CFM);
		reply->load_subfile_open_cfm.procedure_ref = ci->procedure_ref;
		reply->load_subfile_open_cfm.connection_ref = ci->client_ref;
		reply->load_subfile_open_cfm.handle = handle;
		retval = LMC_RESULT_SUCCESS;
	} else {
		retval = LMC_RESULT_RESOURCE_SHORTAGE;
	}

lm_open_end:
	if (retval != LMC_RESULT_SUCCESS) {
		reply = itc_alloc(rej_size, LMC_LOAD_SUBFILE_OPEN_REJ);
		reply->load_subfile_open_rej.procedure_ref = ci->procedure_ref;
		reply->load_subfile_open_rej.connection_ref = ci->client_ref;
		reply->load_subfile_open_rej.error_code = retval;
	}
	free(lmc_header);
	itc_send(&reply, itc_sender(msg), lmc_server_mbox);
	return;

}

/**
 * Function lm_read
 */
static void lm_read(union itc_msg *msg, struct conn_client_info *ci)
{
	union itc_msg *reply;
	uint8_t *lm_start;
	uint32_t retval = LMC_RESULT_OTHER_ERROR;
	int32_t  handle;
	uint32_t pos;
	uint32_t size;
	uint32_t slot;
	char    *buf = NULL;
	char    *lmc_dev;
	uint32_t nof_left_bytes;
	uint32_t nof_read_bytes;
	uint32_t rej_size = sizeof(struct lmc_load_subfile_open_rej);

	handle = msg->load_subfile_read_req.handle;

	/* check handle */
	if ((handle < 0) || (handle >= LMC_MAX_SUBFILES)) {
		TPT_ERROR(STR("handle %d is not valid", handle));
		retval = LMC_RESULT_INVALID_PARAM;
		goto lm_read_end;
	}

	if (lm_handlers[handle].used != HANDLE_USED) {
		TPT_ERROR(STR("handle %d is not used", handle));
		retval = LMC_RESULT_INVALID_PARAM;
		goto lm_read_end;
	}
	slot = lm_handlers[handle].slot;
	if (lmc_list[slot].info.state != LMC_STATE_VALID) {
		TPT_ERROR(STR("LMC%d is not valid", slot));
		retval = LMC_RESULT_NOT_FOUND;
		goto lm_read_end;
	}
	pos = msg->load_subfile_read_req.pos;
	size = msg->load_subfile_read_req.size;

	lmc_dev = lmc_list[slot].mtd_name;
	/* Perform read */
	if (pos < lm_handlers[handle].length) {
		nof_left_bytes = lm_handlers[handle].length - pos;
		if (nof_left_bytes >= size)
			nof_read_bytes = size;
		else
			nof_read_bytes = nof_left_bytes;
	} else {
		TPT_ERROR(STR("Invalid pos %d, while LM length is %d",
		              pos, lm_handlers[handle].length));
		nof_read_bytes = 0;
	}

	if (nof_read_bytes > 0) {
		buf = malloc(nof_read_bytes);
		if (buf == NULL) {
			TPT_ERROR("malloc failed for LM read");
			retval = LMC_RESULT_OTHER_ERROR;
			goto lm_read_end;
		}
		lm_start = lm_handlers[handle].start_addr;
		retval = mtd_read(lmc_dev, buf, nof_read_bytes,
				  ((uint32_t )lm_start + pos));
	}

	if (retval == 0) {
		reply = itc_alloc(sizeof(struct lmc_load_subfile_read_cfm) +
				  nof_read_bytes,
				  LMC_LOAD_SUBFILE_READ_CFM);
		reply->load_subfile_read_cfm.procedure_ref = ci->procedure_ref;
		reply->load_subfile_read_cfm.connection_ref = ci->client_ref;
		reply->load_subfile_read_cfm.nof_read_bytes = nof_read_bytes;
		memcpy(reply->load_subfile_read_cfm.buf, buf, nof_read_bytes);
	}

lm_read_end:
	if (retval != LMC_RESULT_SUCCESS) {
		reply = itc_alloc(rej_size, LMC_LOAD_SUBFILE_READ_REJ);
		reply->load_subfile_read_rej.procedure_ref = ci->procedure_ref;
		reply->load_subfile_read_rej.connection_ref = ci->client_ref;
		reply->load_subfile_read_rej.error_code = retval;
	}
	free(buf);
	itc_send(&reply, itc_sender(msg), lmc_server_mbox);

	return;
}

/**
 * Function lm_close
 */
static void lm_close(union itc_msg *msg, struct conn_client_info *ci)
{
	union itc_msg *reply;
	int32_t handle;
	uint32_t retval;
	uint32_t cfm_size = sizeof(struct lmc_load_subfile_close_cfm);
	uint32_t rej_size = sizeof(struct lmc_load_subfile_close_rej);

	handle = msg->load_subfile_close_req.handle;

	/* check handle */
	if ((handle < 0) || (handle >= LMC_MAX_SUBFILES)) {
		TPT_ERROR(STR("handle %d is not valid", handle));
		retval = LMC_RESULT_INVALID_PARAM;
		goto lm_close_end;
	}

	if (lm_handlers[handle].used != HANDLE_USED) {
		TPT_ERROR(STR("handle %d is not used", handle));
		retval = LMC_RESULT_INVALID_PARAM;
		goto lm_close_end;
	}

	reply = itc_alloc(cfm_size, LMC_LOAD_SUBFILE_CLOSE_CFM);
	reply->load_subfile_close_cfm.procedure_ref = ci->procedure_ref;
	reply->load_subfile_close_cfm.connection_ref = ci->client_ref;
	lm_handlers[handle].used = HANDLE_UNUSED;
	lm_handlers[handle].length = 0;
	lm_handlers[handle].start_addr = 0;
	lm_handlers[handle].server_ref = 0;
	retval = LMC_RESULT_SUCCESS;

lm_close_end:
	if (retval != LMC_RESULT_SUCCESS) {
		reply = itc_alloc(rej_size, LMC_LOAD_SUBFILE_CLOSE_REJ);
		reply->load_subfile_close_rej.procedure_ref = ci->procedure_ref;
		reply->load_subfile_close_rej.connection_ref = ci->client_ref;
		reply->load_subfile_close_rej.error_code = retval;
	}
	itc_send(&reply, itc_sender(msg), lmc_server_mbox);
	return;
}

/**
 * Function client_disconnect
 */
static void client_disconnect(struct conn_client_info *client_info)
{
	uint32_t i;
	union itc_msg *msg;

	for (i = 0; i < LMC_MAX_SUBFILES; i++) {
		if (lm_handlers[i].server_ref == client_info->server_ref) {
			lm_handlers[i].used = HANDLE_UNUSED;
			lm_handlers[i].length = 0;
			lm_handlers[i].start_addr = 0;
			lm_handlers[i].server_ref = 0;
		}
	}
	if ((pre_load_state == PRE_LOADING) &&
	    (client_info->server_ref == write_data.server_ref)) {
		pre_load_state = IDLE;
		msg = itc_alloc(sizeof(struct erase_slot_fwd), ERASE_SLOT_FWD);
		msg->erase_fwd.slot = write_data.slot;
		itc_send(&msg, lmc_slave_mbox_id, ITC_MY_MBOX);
	}

}

/**
 * Function handle_load_file_init
 * Finish the load init procedure
 */
static void handle_load_file_init(union itc_msg * msg,
                                  struct conn_client_info *ci)
{
	union itc_msg *reply;
	uint32_t i;
	uint32_t slot;
	uint32_t size = 0xFFFFFFFF;
	char *lmc;
	uint32_t error_code;
	/* check loading state */
	if (pre_load_state != IDLE) {
		error_code = LMC_RESULT_WRONG_STATE;
		goto load_init_error;
	}

	lmc = msg->load_file_init_req.loadmodule;
	/* check if loadmodule is loaded already */
	for (i = 0; i < max_slot; i++) {
		if (lmc_list[i].info.state == LMC_STATE_VALID) {
			if (!strcmp(lmc, lmc_list[i].info.lmid)) {
				error_code = LMC_RESULT_WRONG_CONFIG_DATA;
				goto load_init_error;
			}
		}
	}

	/* check that there is space left in flash */
	/* Select slot, use smallest one, make sure no one is using it */
	for (i = 0; i < max_slot; i++) {
		if ((lmc_list[i].info.state == LMC_STATE_ERASED) &&
		    (lmc_list[i].info.slot_size < size)) {
			size = lmc_list[i].info.slot_size;
			slot = i;
		}
	}

	if (size == 0xFFFFFFFF) {
		error_code = LMC_RESULT_RESOURCE_SHORTAGE;
		goto load_init_error;
	}

	write_data.slot = slot;
	write_data.seq_nr = 0;
	write_data.pos = 0;
	write_data.server_ref = msg->load_file_init_req.connection_ref;
	TPT_INFO("loading state change: IDLE -> PRE_LOADING");
	pre_load_state = PRE_LOADING;
	lmc_list[slot].info.state = LMC_STATE_WRPROG;

	reply = itc_alloc(sizeof(struct lmc_load_file_init_cfm),
	                  LMC_LOAD_FILE_INIT_CFM);
	reply->load_file_init_cfm.max_block_size = LMC_MAX_LM_BLOCKSIZE;
	reply->load_file_init_cfm.procedure_ref = ci->procedure_ref;
	reply->load_file_init_cfm.connection_ref = ci->client_ref;
	itc_send(&reply, itc_sender(msg), lmc_server_mbox);
	return;

load_init_error:
	reply = itc_alloc(sizeof(struct lmc_load_file_init_rej),
		          LMC_LOAD_FILE_INIT_REJ);
	reply->load_file_init_rej.error_code = error_code;
	reply->load_file_init_rej.procedure_ref = ci->procedure_ref;
	reply->load_file_init_rej.connection_ref = ci->client_ref;
	itc_send(&reply, itc_sender(msg), lmc_server_mbox);
	return;
}

/**
 * Function handle_load_file_data
 * Finish the load data procedure
 */
static void handle_load_file_data(union itc_msg **msg,
                                  struct conn_client_info *ci)
{
	union itc_msg *reply;
	uint32_t block_size;
	uint16_t seq_nr;
	uint32_t error_code;
	uint32_t end_pos;

	block_size = (*msg)->load_file_data_req.lm_block_size;
	seq_nr = (*msg)->load_file_data_req.lm_seq_nr;

	/* check if send from correct user */
	if ((*msg)->load_file_data_req.connection_ref != write_data.server_ref) {
		char name[20];
		itc_get_name(itc_sender(*msg), name, sizeof(name));
		TPT_ERROR(STR("Received unexpected load request from %s!",
		              name));
		error_code = LMC_RESULT_OTHER_ERROR;
		goto load_data_error;
	}

	/* check state */
	if (pre_load_state != PRE_LOADING) {
		error_code = LMC_RESULT_WRONG_STATE;
		goto load_data_error;
	}

	/* check sequence number */
	if (seq_nr != write_data.seq_nr) {
		TPT_ERROR(STR("Wrong seq nr, received %d while have %d",
		              seq_nr, write_data.seq_nr));
		error_code = LMC_RESULT_WRONG_SEQ_NR;
		goto load_data_error;
	}

	/* check loadmodule block size */
	if (block_size > LMC_MAX_LM_BLOCKSIZE) {
		TPT_ERROR(STR("Loadmodule block %d > max block size %d",
		              block_size, LMC_MAX_LM_BLOCKSIZE));
		error_code = LMC_RESULT_INVALID_PARAM;
		goto load_data_error;
	} else if (block_size == 0) {
		TPT_ERROR("Loadmodule block size is 0");
		error_code = LMC_RESULT_INVALID_PARAM;
		goto load_data_error;
	}

	/* check slot */
	end_pos = write_data.pos + block_size -1;
	if (end_pos >= lmc_list[write_data.slot].info.slot_size) {
		TPT_ERROR(STR("End pos %d > slot size %d",
		              end_pos,
		              lmc_list[write_data.slot].info.slot_size));
		error_code = LMC_RESULT_OTHER_ERROR;
		goto load_data_error;
	}

	/* send message to slave thread for loading */
	if (lmc_slave_mbox_id == ITC_NO_ID) {
		TPT_ERROR("Cannot find lmc slave thread when loading");
		error_code = LMC_RESULT_OTHER_ERROR;
		goto load_data_error;
	}
	reply = itc_alloc(sizeof(struct lmc_load_file_data_cfm),
	                  LMC_LOAD_FILE_DATA_CFM);
	reply->load_file_data_cfm.procedure_ref = ci->procedure_ref;
	reply->load_file_data_cfm.connection_ref = ci->client_ref;
	itc_send(&reply, itc_sender(*msg), lmc_server_mbox);
	itc_send(msg, lmc_slave_mbox_id, itc_sender(*msg));
	return;

load_data_error:
	reply = itc_alloc(sizeof(struct lmc_load_file_data_rej),
			  LMC_LOAD_FILE_DATA_REJ);
	reply->load_file_data_rej.error_code = error_code;
	reply->load_file_data_rej.procedure_ref = ci->procedure_ref;
	reply->load_file_data_rej.connection_ref = ci->client_ref;
	itc_send(&reply, itc_sender(*msg), lmc_server_mbox);
	return;
}

/**
 * Function handle_load_file_data_get_seq
 * Return the data sequence number.
 */
static void handle_load_file_data_get_seq(union itc_msg *msg,
                                 struct conn_client_info *ci)
{
	union itc_msg *reply = NULL;
	uint32_t error_code;

	/* check if send from correct user */
	if (msg->load_file_data_get_seq_req.connection_ref != write_data.server_ref) {
		char name[20];
		itc_get_name(itc_sender(msg), name, sizeof(name));
		TPT_ERROR(STR("Received unexpected load file data get seq request from %s!",
		              name));
		error_code = LMC_RESULT_OTHER_ERROR;
		goto load_file_data_get_seq_error;
	}
	/* check state */
	if (pre_load_state != PRE_LOADING) {
		error_code = LMC_RESULT_WRONG_STATE;
		goto load_file_data_get_seq_error;
	}
	reply = itc_alloc(sizeof(struct lmc_load_file_data_get_seq_cfm),
	                  LMC_LOAD_FILE_DATA_GET_SEQ_CFM);
	reply->load_file_data_get_seq_cfm.result = write_data.seq_nr;
	reply->load_file_data_get_seq_cfm.procedure_ref = ci->procedure_ref;
	reply->load_file_data_get_seq_cfm.connection_ref = ci->client_ref;
	itc_send(&reply, itc_sender(msg), lmc_server_mbox);
	return;

load_file_data_get_seq_error:
	reply = itc_alloc(sizeof(struct lmc_load_file_data_get_seq_rej),
			  LMC_LOAD_FILE_DATA_GET_SEQ_REJ);
	reply->load_file_data_get_seq_rej.error_code = error_code;
	reply->load_file_data_get_seq_rej.procedure_ref = ci->procedure_ref;
	reply->load_file_data_get_seq_rej.connection_ref = ci->client_ref;
	itc_send(&reply, itc_sender(msg), lmc_server_mbox);

	return;
}

/**
 * Function handle_load_file_end
 * Fnish the load lm end procedure and complete preloading procedure
 */
static void handle_load_file_end(union itc_msg **msg,
                                 struct conn_client_info *ci)
{
	union itc_msg *reply = NULL;
	uint32_t error_code;

	/* check if send from correct user */
	if ((*msg)->load_file_end_req.connection_ref != write_data.server_ref) {
		char name[20];
		itc_get_name(itc_sender(*msg), name, sizeof(name));
		TPT_ERROR(STR("Received unexpected end load request from %s!",
		              name));
		error_code = LMC_RESULT_OTHER_ERROR;
		goto load_file_end_error;
	}
	/* check state */
	if (pre_load_state != PRE_LOADING) {
		error_code = LMC_RESULT_WRONG_STATE;
		goto load_file_end_error;
	}
	/* close file */
	if (lmc_slave_mbox_id == ITC_NO_ID) {
		TPT_ERROR("Cannot find lmc slave thread mailbox");
		error_code = LMC_RESULT_OTHER_ERROR;
		goto load_file_end_error;
	}
	itc_send(msg, lmc_slave_mbox_id, itc_sender(*msg));
	return;

load_file_end_error:
	reply = itc_alloc(sizeof(struct lmc_load_file_end_rej),
			  LMC_LOAD_FILE_END_REJ);
	reply->load_file_end_rej.error_code = error_code;
	reply->load_file_end_rej.procedure_ref = ci->procedure_ref;
	reply->load_file_end_rej.connection_ref = ci->client_ref;
	itc_send(&reply, itc_sender(*msg), lmc_server_mbox);

	return;
}

/**
 * Function handle_load_file_delete
 * Finish the delete lmc procedure
 */
static void handle_load_file_delete(union itc_msg *msg,
                                    struct conn_client_info *ci)
{
	char    *lmc;
	int32_t slot;
	uint32_t i;
	uint32_t error_code;
	union itc_msg *req, *reply;
	uint32_t nof_working_auboot = 0;
	uint32_t delete_auboot = 0;

	lmc = msg->load_file_delete_req.loadmodule;
	/* search for load module */
	if (strlen(lmc) == 0) {
		TPT_INFO("Empty pid, return");
		/* Return without action */
		error_code = LMC_RESULT_SUCCESS;
		reply = itc_alloc(sizeof(struct lmc_load_file_delete_cfm),
		                  LMC_LOAD_FILE_DELETE_CFM);
		reply->load_file_delete_cfm.procedure_ref = ci->procedure_ref;
		reply->load_file_delete_cfm.connection_ref = ci->client_ref;
		itc_send(&reply, itc_sender(msg), lmc_server_mbox);
		/* send indication as well */
		reply = itc_alloc(sizeof(struct lmc_load_file_delete_ind),
		                  LMC_LOAD_FILE_DELETE_IND);
		reply->load_file_delete_ind.result = error_code;
		reply->load_file_delete_ind.procedure_ref = ci->procedure_ref;
		reply->load_file_delete_ind.connection_ref = ci->client_ref;
		itc_send(&reply, itc_sender(msg), lmc_server_mbox);
		return;
	}
	slot = get_slot_from_pid(lmc);
	if (slot < 0) {
		TPT_ERROR(STR("Cannot find lmc %s", lmc));
		error_code = LMC_RESULT_INVALID_PARAM;
		goto load_file_delete_error;
	}
	/* Cannot delete the writing slot */
	if ((pre_load_state != IDLE) && (slot == write_data.slot)) {
		TPT_ERROR("Cannot delete writing slot");
		error_code = LMC_RESULT_WRONG_STATE;
		goto load_file_delete_error;
	}
	/* Cannot delete the locked slot */
	if (lmc_list[slot].info.permissions & LMC_LOAD_FILE_PERMISSION_LOCKED) {
		TPT_INFO(STR("Cannot delete locked slot %d", slot));
		error_code = LMC_RESULT_ACCESS_DENIED;
		goto load_file_delete_error;
	}
	/* It is not allowed to delete the currently running loadmodule */
	if (check_if_current(lmc)) {
		TPT_INFO("Cannot delete current lmc");
		error_code = LMC_RESULT_ACCESS_DENIED;
		goto load_file_delete_error;
	}
	/* It is not allowed to remove the last AUBOOT file */
	for (i = 0; i < max_slot; i++) {
		if ((lmc_list[i].info.state == LMC_STATE_VALID) &&
		    (lmc_list[i].info.file_type == XLF_IBOOT_HDR_TYPE_AU_BOOT)) {
			if (slot == i) {
				delete_auboot = 1;
				continue;
			}
			if (lmc_list[i].info.working == 1) {
				nof_working_auboot++;
			}
		}
	}
	if (nof_working_auboot < 1 && delete_auboot) {
		TPT_INFO(STR("Cannot delete last auboot lmc%d", slot));
		error_code = LMC_RESULT_ACCESS_DENIED;
		goto load_file_delete_error;
	}
	if (lmc_slave_mbox_id == ITC_NO_ID) {
		TPT_ERROR("Cannot find lmc slave thread when deleting");
		error_code = LMC_RESULT_OTHER_ERROR;
		goto load_file_delete_error;
	}
	lmc_list[slot].info.state = LMC_STATE_ERPROG;
	/* Send reply to client */
	reply = itc_alloc(sizeof(struct lmc_load_file_delete_cfm),
	                  LMC_LOAD_FILE_DELETE_CFM);
	reply->load_file_delete_cfm.procedure_ref = ci->procedure_ref;
	reply->load_file_delete_cfm.connection_ref = ci->client_ref;
	itc_send(&reply, itc_sender(msg), lmc_server_mbox);

	req = itc_alloc(sizeof(struct delete_file_fwd), DELETE_FILE_FWD);
	req->delete_fwd.slot = slot;
	req->delete_fwd.procedure_ref = ci->procedure_ref;
	req->delete_fwd.connection_ref = ci->server_ref;
	itc_send(&req, lmc_slave_mbox_id, itc_sender(msg));
	return;

load_file_delete_error:
	reply = itc_alloc(sizeof(struct lmc_load_file_delete_rej),
			  LMC_LOAD_FILE_DELETE_REJ);
	reply->load_file_delete_rej.error_code = error_code;
	reply->load_file_delete_rej.procedure_ref = ci->procedure_ref;
	reply->load_file_delete_rej.connection_ref = ci->client_ref;
	itc_send(&reply, itc_sender(msg), lmc_server_mbox);
	return;
}
/**
 * Function handle_read_load_file_data
 *
 */
static void handle_read_load_file_data(union itc_msg * msg,
                                  struct conn_client_info *ci)
{
        union itc_msg *reply;
	uint32_t addroffset;
	uint32_t lmc_index;
        uint32_t data_length;
        char *lmc_dev;
        char    *buf = NULL;
        int32_t  retval = LMC_RESULT_OTHER_ERROR;

        lmc_index = msg->read_load_file_data_req.lmc_index;
        addroffset = msg->read_load_file_data_req.addroffset;
        data_length = msg->read_load_file_data_req.length;

        /* check LMC info */
	if (lmc_index >= max_slot) {
		TPT_ERROR(STR("LMC index %d is not valid", lmc_index));
		retval = LMC_RESULT_INVALID_PARAM;
		goto read_load_file_data_end;
	}
        lmc_dev = lmc_list[lmc_index].mtd_name;
        if (data_length > 0) {
		buf = malloc(data_length);
		if (buf == NULL) {
			TPT_ERROR("malloc failed for LM read");
			retval = LMC_RESULT_OTHER_ERROR;
			goto read_load_file_data_end;
		}

        	retval = mtd_read(lmc_dev, (char *)buf, data_length, addroffset);

        	if (retval == 0)
        	{
                	reply = itc_alloc(sizeof(struct lmc_read_load_file_data_cfm) +
				  data_length,
				  LMC_READ_LOAD_FILE_DATA_CFM);
                	reply->read_load_file_data_cfm.procedure_ref = ci->procedure_ref;
			reply->read_load_file_data_cfm.connection_ref = ci->client_ref;
			reply->read_load_file_data_cfm.nof_read_bytes = data_length;
			memcpy(reply->read_load_file_data_cfm.lmc_data, buf, data_length);
        	}
	}

read_load_file_data_end:
       if (retval != LMC_RESULT_SUCCESS)
       {
                reply = itc_alloc(sizeof(struct lmc_read_load_file_data_rej), LMC_READ_LOAD_FILE_DATA_REJ);
		reply->read_load_file_data_rej.procedure_ref = ci->procedure_ref;
		reply->read_load_file_data_rej.connection_ref = ci->client_ref;
                reply->read_load_file_data_rej.error_code = retval;
       }
       free(buf);
       itc_send(&reply, itc_sender(msg), lmc_server_mbox);
}
/**
 * Function write_file
 * Load data to flash
 */
static void write_file(union itc_msg *msg, struct conn_client_info *ci)
{
	uint32_t retval;
	union itc_msg *reply;
	uint32_t size, offset;
	uint8_t *buf;
	char    *dev_name;
	uint32_t b0, b1, b2, b3; /* magic number bytes */

	/* Always acknowledge to the client and write to the flash
	* (regardless of earlier writing result).
	* We want the preloading procedure to finish and if the
	* loadfile has any faults it is erased when closed.
	*/
	reply = itc_alloc(sizeof(struct lmc_load_file_data_ind),
	                  LMC_LOAD_FILE_DATA_IND);
	reply->load_file_data_ind.lm_seq_nr = write_data.seq_nr;
	reply->load_file_data_ind.connection_ref = ci->client_ref;
	reply->load_file_data_ind.procedure_ref = ci->procedure_ref;

	/* Update sequence number first */
	write_data.seq_nr++;
	itc_send(&reply, itc_sender(msg), lmc_server_mbox);

	/* write load file */
	size = msg->load_file_data_req.lm_block_size;
	buf = msg->load_file_data_req.lm_block;
	dev_name = lmc_list[write_data.slot].mtd_name;
	/* If this was the first block written invalidate the header */
	if (write_data.pos == 0) {
		/* save old magic number */
		b0 = buf[0] << 24; /* Most significant BYTE of magic number */
		b1 = buf[1] << 16;
		b2 = buf[2] << 8;
		b3 = buf[3];/* Least significant BYTE of magic number */
		/* build magic number */
		write_data.magic = b0 + b1 + b2 + b3;
		/* write with the magic number offset */
		offset = sizeof(uint32_t);
		retval = mtd_write(dev_name, (char *)(buf + offset),
		                   size - offset, write_data.pos + offset);
	} else {
		/* write without offset */
		retval = mtd_write(dev_name, (char *)buf, size, write_data.pos);
	}

	if (retval)
		TPT_ERROR(STR("write failed, seq no %d", write_data.seq_nr - 1));

	write_data.pos += size;

	return;
}

/**
 * Function close_file
 * Close LM file
 */
static void close_file(union itc_msg *msg, struct conn_client_info *ci)
{
	uint32_t retval;
	uint32_t seq_nr = 0xFFFFFFFF;
	uint32_t read_out;
	uint32_t file_type;
	uint32_t offset;
	uint32_t magic;
	uint16_t crc2;
	uint32_t slot = write_data.slot;
	uint32_t result = LMC_RESULT_OTHER_ERROR;
	char    *dev_name;
	char    *error;
	uint32_t i;
	union itc_msg *reply;
	struct xlf_iboot_header *lmc_header = NULL;
	struct xlfi_parse_state state;

	uint8_t    *buf = NULL;
	struct xlf_iboot_header *lmc_header2 = NULL;
        int fd;
        erase_info_t erase_info;
	mtd_info_t mtd_info;

	dev_name = lmc_list[slot].mtd_name;
	offset = offsetof(struct xlf_iboot_header, type);
	retval = mtd_read(dev_name, (char *)&file_type, sizeof(uint32_t), offset);
	if (retval) {
		TPT_ERROR(STR("Failed to read %s", dev_name));
		goto close_file_end;
	}
	file_type = ntohl(file_type);
	if ((file_type != XLF_IBOOT_HDR_TYPE_AU_APPLIC) &&
	    (file_type != XLF_IBOOT_HDR_TYPE_AU_BOOT)) {
		TPT_TRACE(1, STR("Unknown file type 0x%x", file_type));
		goto close_file_end;
	}
	/* Check if it's the first time we load this file type */
	for (i = 0; i < max_slot; i++) {
		if (lmc_list[i].info.state != LMC_STATE_VALID)
			continue;
		if (lmc_list[i].info.file_type == file_type) {
			if ((seq_nr == 0xFFFFFFFF) ||
			    (seq_nr < lmc_list[i].info.seq_number))
				seq_nr = lmc_list[i].info.seq_number;
		}
	}
	if (seq_nr < 0xFFFFFFFE) /* cannot be bigger than 0xFFFFFFFF */
		seq_nr++;
	else
		seq_nr = 0;
	offset = offsetof(struct xlf_iboot_header, seq_number);
	seq_nr = htonl(seq_nr);
	retval = mtd_write(dev_name, (char *)&seq_nr, sizeof(uint32_t), offset);
	if (retval) {
		TPT_ERROR("Cannot write seq nr at closing");
		goto close_file_end;
	}
	/* Mark file as valid by writing magic number */
	magic = htonl(write_data.magic);
	retval = mtd_write(dev_name, (char *)&magic, 4, 0);
	if (retval) {
		TPT_ERROR("Cannot write magic no at closing");
		goto close_file_end;
	}
	/* workround for write seq number back to slot */
	//get mtdinfo
	fd = open(dev_name, O_WRONLY | O_SYNC | O_NONBLOCK);
	if (fd < 0) {
		TPT_ERROR(STR("function:%s,open %s failed with errno %d",
		              __func__,dev_name, errno));
		goto close_file_end;
	}
	if (ioctl(fd, MEMGETINFO, &mtd_info) == -1) {
		TPT_ERROR(STR("function:%s,get info %s failed with errno %d",
		              __func__,dev_name, errno));
		if (close(fd) == -1) {
			TPT_ERROR(STR("function:%s,close %s failed with errno %d",
				__func__,dev_name, errno));

		}
		goto close_file_end;
	}
	if (close(fd) == -1) {
		TPT_ERROR(STR("function:%s,close %s failed with errno %d",
				__func__,dev_name, errno));
		goto close_file_end;

	}
	buf = malloc(mtd_info.erasesize);
	if (buf == NULL) {
		TPT_ERROR(STR("malloc failed for size %d", mtd_info.erasesize));
		goto close_file_end;
	}
	lmc_header2 = malloc(sizeof(struct xlf_iboot_header));
	if (lmc_header2 == NULL) {
		TPT_ERROR("malloc failed for sizeof xlf_iboot_header");
		goto close_file_end;
	}
	retval = mtd_read(dev_name, (char *)buf, mtd_info.erasesize, 0);
	if (retval) {
		TPT_TRACE(1, STR("Failed to read %s", dev_name));
		goto close_file_end;
	}
	memcpy(lmc_header2,buf,sizeof(struct xlf_iboot_header));
	lmc_header2 -> seq_number = seq_nr;
	lmc_header2 -> magic = magic;
        memcpy(buf,lmc_header2, sizeof(struct xlf_iboot_header));

        erase_info.length = mtd_info.erasesize;
	erase_info.start = 0;
	fd = open(dev_name, O_WRONLY | O_SYNC | O_NONBLOCK);
	if (fd < 0) {
		TPT_ERROR(STR("function:%s,open %s failed with errno %d",
		              __func__,dev_name, errno));
		goto close_file_end;
	}
	if (ioctl(fd, MEMERASE, &erase_info) == -1) {
		TPT_ERROR(STR("Erase %s failed with errno %d",
			              dev_name, errno));
		if (close(fd) == -1) {
			TPT_ERROR(STR("function:%s,close %s failed with errno %d",
				__func__,dev_name, errno));
			goto close_file_end;
		}
	}
	if (close(fd) == -1) {
		TPT_ERROR(STR("function:%s,close %s failed with errno %d",
				__func__,dev_name, errno));
		goto close_file_end;

	}
	TPT_INFO("WRITE first block data back");
	retval = mtd_write(dev_name,(char*)buf,mtd_info.erasesize,0);
	if (retval) {
		TPT_ERROR("Cannot write first block data back at closing");
		goto close_file_end;
	}
	free(lmc_header2);
	free(buf);
	/* End of workround */
	/* verify load file */
	retval = read_lmc_header(&lmc_header, dev_name);
	if (retval) {
		TPT_TRACE(1, STR("Failed to read %s", dev_name));
		goto close_file_end;
	}
	retval = xlfi_validate_header(&state, lmc_header,
	                              (const char **)&error);
	if (retval) {
		TPT_TRACE(1, STR("LMC header verification failed: %s", error));
		goto close_file_end;
	}
	/* check sequence number */
	retval = mtd_read(dev_name, (char *)&read_out, sizeof(uint32_t),
	                  offset);
	if (retval) {
		TPT_ERROR("Read sequence number from lmc failed");
		goto close_file_end;
	}
	if (read_out != seq_nr) {
		TPT_ERROR(STR("Seq number update check failed"
			      " Got %d while writing %d",
			      ntohl(read_out), ntohl(seq_nr)));
		retval = 1;
		goto close_file_end;
	}
	/* check crc2 */
	retval = mtd_read(dev_name, (char *)&crc2, sizeof(uint16_t),
	                  ntohl(lmc_header->crc2_offset));
	if (retval) {
		TPT_ERROR("Read crc2 from lmc failed");
		goto close_file_end;
	}
	retval = validata_crc2(dev_name, lmc_header, ntohs(crc2));
	if (retval) {
		TPT_TRACE(1, "Verify crc2 failed");
		goto close_file_end;
	}
	/* FIXME:lock flash */
#if 0
	retval = mtd_lock(dev_name);
	if (retval)
		TPT_ERROR(STR("lock flash failed on %s", dev_name));
#endif
	retval = 0;
	/* set to work */
	set_lmc_working(current_slot);
	/* Update lmclist */
	lmc_list[slot].info.state = LMC_STATE_VALID;
	lmc_list[slot].info.lmc_size = XLFI_GET_XLF_SIZE(lmc_header);
	lmc_list[slot].info.seq_number = ntohl(lmc_header->seq_number);
	lmc_list[slot].info.file_type = ntohl(lmc_header->type);
	lmc_list[slot].info.time = ntohl(lmc_header->time);
	lmc_list[slot].info.subfile_counter = ntohl(lmc_header->file_count);
	lmc_list[slot].info.working = 0;
	lmc_list[slot].info.slot_size = get_mtd_size(dev_name);
	lmc_list[slot].info.is_current = 0;
	pid2lmid(lmc_list[slot].info.lmid, lmc_header->suid);
	result = LMC_RESULT_SUCCESS;
close_file_end:
	TPT_INFO("loading state change: PRE_LOADING -> IDLE");
	pre_load_state = IDLE;
	reply = itc_alloc(sizeof(struct lmc_load_file_end_cfm),
	                  LMC_LOAD_FILE_END_CFM);
	reply->load_file_end_cfm.procedure_ref = ci->procedure_ref;
	reply->load_file_end_cfm.connection_ref = ci->client_ref;
	reply->load_file_end_cfm.result = result;
	itc_send(&reply, itc_sender(msg), lmc_server_mbox);

	free(lmc_header);
	if (retval) {
		TPT_INFO("File is not valid when reread, erasing");
		lmc_list[slot].info.state = LMC_STATE_ERROR;
		(void)erase_slot(slot);
	}
	return;
}

/**
 * Function delete_file
 * Erase slot
 */
static void delete_file(union itc_msg *msg, struct conn_client_info *ci)
{
	int32_t slot;
	uint32_t result;
	union itc_msg *reply;

	slot = msg->delete_fwd.slot;
	if ((slot < 0) || (slot >= max_slot)) {
		TPT_ERROR(STR("Delete failed: Invalid slot %d", slot));
		result = LMC_RESULT_INVALID_PARAM;
		goto delete_file_end;
	}
	result = erase_slot(slot);

delete_file_end:
	reply = itc_alloc(sizeof(struct lmc_load_file_delete_ind),
	                  LMC_LOAD_FILE_DELETE_IND);
	reply->load_file_delete_ind.result = result;
	reply->load_file_delete_ind.procedure_ref = ci->procedure_ref;
	reply->load_file_delete_ind.connection_ref = ci->client_ref;
	itc_send(&reply, itc_sender(msg), lmc_server_mbox);
	return;
}

/**
 * Function find_invalid_lmc
 * Find invalid lmc and send erase request
 */
static void find_invalid_lmc(int nof_auboot)
{
	union itc_msg *msg;
	uint32_t resp_msg[] = {1, ITC_LOCATE_DEFAULT_NO};

	itc_locate_async(LMC_SLAVE_MBOX, NULL, ITC_MY_MBOX);
	msg = itc_receive(resp_msg, ITC_NO_TMO, ITC_FROM_ALL);
	lmc_slave_mbox_id = itc_sender(msg);
	itc_free(&msg);

	for (int i = 0; i < max_slot; i++) {
		if (lmc_list[i].info.state != LMC_STATE_ERROR) {
			continue;
		}
		if (lmc_list[i].info.file_type == XLF_IBOOT_HDR_TYPE_AU_BOOT) {
			if (nof_auboot == 0) {
				/*
				 * Protect the first invalid LMC that possibly
				 * could be an auboot from deletion
				 */
				nof_auboot--;
				continue;
			} else if (nof_auboot < 0) {
				nof_auboot--;
			}
		}

		lmc_list[i].info.state = LMC_STATE_ERPROG;
		msg = itc_alloc(sizeof(struct erase_slot_fwd),
		                ERASE_SLOT_FWD);
		msg->erase_fwd.slot = i;
		itc_send(&msg, lmc_slave_mbox_id, ITC_MY_MBOX);
	}
}

/**
 * Function time_slave
 * Count down 30 min
 */
static void *time_slave(void _UNUSED_ *arg)
{
	union itc_msg *msg = NULL;
	sigset_t mask;
	uint32_t tmo = 30/*min*/ * 60/*s*/ * 1000/*ms*/;
	struct timespec ts_now;
	unsigned long long now;

	/* Initiate. */
	time_slave_mbox_id = itc_create_mailbox(TIME_SLAVE_MBOX, 0);
	if (time_slave_mbox_id == ITC_NO_ID) {
		TPT_ERROR(STR("%s: Unable to create ITC mailbox!",
		              TIME_SLAVE_MBOX));
		return NULL;
	}

	/* block exit signal */
	sigemptyset(&mask);
	sigaddset(&mask, SIGTERM);
	if (pthread_sigmask(SIG_BLOCK, &mask, NULL) == -1) {
		TPT_ERROR("pthread_sigmask failed");
		return NULL;
	}
	for (;;) {
		clock_gettime(CLOCK_MONOTONIC, &ts_now);
		now = TIMESPEC_TO_MS(ts_now);
		if (now >= tmo)
			break;
		msg = itc_receive(ITC_NOFILTER,
		                  (uint32_t)(tmo - now),
		                  ITC_FROM_ALL);
		if (msg == NULL)
			continue;
		if (msg->msgno == EXIT_SIGNAL) {
			itc_free(&msg);
			TPT_INFO("time_slave exiting as ordered");
			return NULL;
		} else {
			TPT_ERROR(STR("Unexpected message received, "
			              "msgno = 0x%x", msg->msgno));
			itc_free(&msg);
		}
	}
	TPT_TRACE(1, "System is stable; setting working bit and clearing reset counter");
	msg = itc_alloc(sizeof(struct set_working_bit_fwd),
	                SET_WORKING_BIT_FWD);
	itc_send(&msg, lmc_slave_mbox_id, ITC_MY_MBOX);
	if(resetmem_clear_restart_counter()) {
		TPT_ERROR("Failed to clear restart counter.");
	}
	return NULL;
}

/**
 * Function lmc_slave
 * Slave thread for writing and deleting LMC
 */
static void *lmc_slave(void *arg)
{
	int  res;
	bool timer_running = false;
	sigset_t mask;
	pthread_t time_thread;
	struct conn_client_info ci;
	union itc_msg *msg = NULL;
	conn_server_handle_t *conn_handle = arg;

	int tid = syscall(SYS_gettid);

	/* set thread to low priority */
	if (setpriority(PRIO_PROCESS, tid, 19) < 0) {
		TPT_ERROR("Failed to set priority");
		return NULL;
	}
	/* Initiate. */
	lmc_slave_mbox_id = itc_create_mailbox(LMC_SLAVE_MBOX, 0);
	if (lmc_slave_mbox_id == ITC_NO_ID) {
		TPT_ERROR(STR("%s: Unable to create ITC mailbox!",
		              LMC_SLAVE_MBOX));
		return NULL;
	}

	/* block exit signal */
	sigemptyset(&mask);
	sigaddset(&mask, SIGTERM);
	if (pthread_sigmask(SIG_BLOCK, &mask, NULL) == -1) {
		TPT_ERROR("pthread_sigmask failed");
		return NULL;
	}

	res = pthread_create(&time_thread, NULL, &time_slave, NULL);
	if (res) {
		TPT_ERROR(STR("Failed to create time thread (error:%d)",
		              res));
		return NULL;
	}
	timer_running = true;

	for (;;) {
		msg = itc_receive(ITC_NOFILTER, ITC_NO_TMO, ITC_FROM_ALL);

		switch (msg->msgno) {
		case EXIT_SIGNAL:
			if (timer_running) {
				itc_send(&msg, time_slave_mbox_id, ITC_MY_MBOX);
				(void)pthread_join(time_thread, NULL);
			} else {
				itc_free(&msg);
			}
			TPT_INFO("lmc_slave exiting as ordered");
			return NULL;
		case ERASE_SLOT_FWD:
			(void)erase_slot(msg->erase_fwd.slot);
			itc_free(&msg);
			continue;
		case SET_WORKING_BIT_FWD:
			if ((current_slot >= 0) &&
			    (lmc_list[current_slot].info.working == 0)) {
				set_lmc_working(current_slot);
			}
			timer_running = false;
			itc_free(&msg);
			continue;
		}

		if (!conn_check_client(*conn_handle, &msg, &ci))
			continue;
		switch (msg->msgno) {
		case LMC_LOAD_FILE_DATA_REQ:
			write_file(msg, &ci);
			break;
		case DELETE_FILE_FWD:
			delete_file(msg, &ci);
			break;
		case LMC_LOAD_FILE_END_REQ:
			close_file(msg, &ci);
			break;
		default:
			TPT_ERROR(STR("Unexpected message received, "
			              "msgno = 0x%x, sender = 0x%x",
			              msg->msgno, ci.sender));
			break;
		}

		itc_free(&msg);
	}
	return NULL;
}

/**
 * Function message_slave
 * Slave thread for lmc server main loop
 */
static void *message_slave(void *arg)
{
	int res;
	union itc_msg *msg = NULL;
	sigset_t mask;
	pthread_t lmc_thread;
	struct conn_client_info ci;
	conn_server_handle_t *conn_handle = arg;

	int tid = syscall(SYS_gettid);

	/* set thread to low priority */
	if (setpriority(PRIO_PROCESS, tid, 19) < 0) {
		TPT_ERROR("Failed to set priority");
		return NULL;
	}

	/* Initiate. */
	msg_slave_mbox_id = itc_create_mailbox(MSG_SLAVE_MBOX, 0);
	if (msg_slave_mbox_id == ITC_NO_ID) {
		TPT_ERROR(STR("%s: Unable to create ITC mailbox!",
		              MSG_SLAVE_MBOX));
		return NULL;
	}

	 /* block exit signal */
	sigemptyset(&mask);
	sigaddset(&mask, SIGTERM);
	if (pthread_sigmask(SIG_BLOCK, &mask, NULL) == -1) {
		TPT_ERROR("pthread_sigmask failed");
		return NULL;
	}

	res = pthread_create(&lmc_thread, NULL, &lmc_slave, conn_handle);
	if (res) {
		TPT_ERROR(STR("Failed to create slave thread (error:%d)", res));
		exit(res);
	}

	if (erase_invalid_lmc) {
		find_invalid_lmc(build_nof_auboot);
	} else {
		TPT_INFO("Warning: Faulty LMC is not erased");
	}
	for (;;) {
		msg = itc_receive(ITC_NOFILTER, ITC_NO_TMO, ITC_FROM_ALL);

		if (msg->msgno == EXIT_SIGNAL) {
			itc_send(&msg, lmc_slave_mbox_id, ITC_MY_MBOX);
			(void) pthread_join(lmc_thread, NULL);
			TPT_INFO("message_slave exiting as ordered");
			return NULL;
		}
		if (!conn_check_client(*conn_handle, &msg, &ci))
			continue;

		switch(msg->msgno) {
		/* Flash read */
		case LMC_GET_LOAD_FILE_INFO_REQ:
			TPT_REC_SIG(msg->msgno,
			            "Received LMC_GET_LOAD_FILE_INFO_REQ");
			get_lmc_list(msg, &ci);
			break;

		case LMC_GET_SUBFILE_INFO_REQ:
			TPT_REC_SIG(msg->msgno,
			            "Received LMC_GET_SUBFILE_INFO_REQ");
			get_lm_list(msg, &ci);
			break;

		case LMC_LOAD_SUBFILE_OPEN_REQ:
			TPT_REC_SIG(msg->msgno,
			            "Received LMC_LOAD_SUBFILE_OPEN_REQ");
			lm_open(msg, &ci);
			break;

		case LMC_LOAD_SUBFILE_READ_REQ:
			TPT_REC_SIG(msg->msgno,
			            "Received LMC_LOAD_SUBFILE_READ_REQ");
			lm_read(msg, &ci);
			break;

		case LMC_LOAD_SUBFILE_CLOSE_REQ:
			TPT_REC_SIG(msg->msgno,
			            "Received LMC_LOAD_SUBFILE_CLOSE_REQ");
			lm_close(msg, &ci);
			break;
		/* Flash write and delete */
		case LMC_LOAD_FILE_INIT_REQ:
			TPT_REC_SIG(msg->msgno,
			            "Received LMC_LOAD_FILE_INIT_REQ");
			handle_load_file_init(msg, &ci);
			break;

		case LMC_LOAD_FILE_DATA_REQ:
			TPT_REC_SIG(msg->msgno,
			            "Received LMC_LOAD_FILE_DATA_REQ");
			handle_load_file_data(&msg, &ci);
			break;

		case LMC_LOAD_FILE_END_REQ:
			TPT_REC_SIG(msg->msgno,
			            "Received LMC_LOAD_FILE_END_REQ");
			handle_load_file_end(&msg, &ci);
			break;

		case LMC_LOAD_FILE_DELETE_REQ:
			TPT_REC_SIG(msg->msgno,
			            "Received LMC_LOAD_FILE_DELETE_REQ");
			handle_load_file_delete(msg, &ci);
			break;

        case LMC_READ_LOAD_FILE_DATA_REQ:
                        TPT_REC_SIG(msg->msgno,
			            "Received LMC_READLOAD_FILE_DATA_REQ");
			handle_read_load_file_data(msg, &ci);
			break;
        case LMC_LOAD_FILE_DATA_GET_SEQ_REQ:
            TPT_REC_SIG(msg->msgno,
                        "Received LMC_LOAD_FILE_DATA_GET_SEQ_REQ");
            handle_load_file_data_get_seq(msg, &ci);
            break;

		default: {
			char name[64];
			itc_get_name(itc_sender(msg), name, sizeof(name));
			TPT_ERROR(STR("%s: Received an unknown message "
			              "(msgno:0x%08x) from client (name:\"%s\","
			              " mbox_id:0x%08x, client_ref:0x%08x, "
			              "server_ref:0x%08x)",
			              LMC_SERVER_NAME, msg->msgno, name,
			              itc_sender(msg), ci.client_ref,
			              ci.server_ref));
			break;
		}
		}
		if (msg != NULL)
			itc_free(&msg);
	}
	return NULL;
}

/**
 * Function main_loop
 */
static int main_loop(void)
{
	int res;
	pthread_t message_thread;
	union itc_msg *msg;
	conn_server_handle_t conn_handle;
	struct conn_client_info ci;
	LMC_CONN_ESTABLISH_MESSAGES_STRUCT(lmc_conn_messages);
	uint32_t supported_versions[] = {LMC_SERVER_VERSIONS};

	struct conn_event_callbacks cb = {NULL, client_disconnect,
	                                  client_disconnect, NULL};
	int conn_result = conn_establish_server_init(&conn_handle,
	                  sizeof(supported_versions) /
	                  sizeof(supported_versions[0]),
	                  supported_versions,
	                  &lmc_conn_messages, 0, &cb);

	if (conn_result != CONN_INIT_OK) {
		TPT_ERROR("Initalization of conn_establish mechanism failed.");
		return -1;
	}

	/* build lmclist */
	lmc_list_clear();
	get_max_slot();
	res = check_boot_partitions();
	if (res < 0) {
		/* Fatal error exit */
		exit(res);
	}
	erase_invalid_lmc = (res) ? 0 : 1;
	build_nof_auboot = build_lmc_list();
	res = pthread_create(&message_thread, NULL,
	                     &message_slave, &conn_handle);
	if(res) {
		TPT_ERROR(STR("Failed to create slave thread (error:%d)", res));
		return -1;
	}

	TPT_INFO(STR("%s: Entering main loop", LMC_SERVER_NAME));
	for (;;) {
		msg = itc_receive(ITC_NOFILTER,
		                  ITC_NO_TMO,
		                  ITC_FROM_ALL);

		if (msg->msgno == EXIT_SIGNAL) {
			itc_send(&msg, msg_slave_mbox_id, ITC_MY_MBOX);
			(void) pthread_join(message_thread, NULL);
			TPT_INFO("lmc_server exiting as ordered");
			return 0;
		}
		if (!conn_check_client(conn_handle, &msg, &ci))
			continue;
 
                 /* Wait for lmc_slave thread to start */
                int i;
                for (i = 0; (lmc_slave_mbox_id == ITC_NO_ID) && (i < 1000); i++)
                     usleep(100);

                if(lmc_slave_mbox_id == ITC_NO_ID) {
                     TPT_ERROR(STR("Cannot find lmc_slave mailbox!"));
                     return -1;
                }
                itc_send(&msg, msg_slave_mbox_id, itc_sender(msg));
	}
}

/**
 * Function print_usage
 */
static void print_usage()
{
	printf("Usage: lmc_server <options>\n\n"
	       "Options:\n"
	       "    -h  Display usage information (this message).\n"
	       "    -d  Daemonize the program.\n\n");
}

/**
 * Function exit_handler
 */
static void exit_handler(int sig)
{
	union itc_msg *msg;

	TPT_INFO(STR("Receive signal 0x%X, terminating", sig));
	msg = itc_alloc(sizeof(uint32_t), EXIT_SIGNAL);
	itc_send(&msg, lmc_server_mbox, ITC_MY_MBOX);
}

/**
 * Function main
 */
int main( int argc, char **argv )
{
	int daemonize = 0;
	int exit_code = 0;
	struct sigaction act;

	if (argc > 1) {
		if (strcmp("-d", argv[1]) == 0) {
			daemonize = 1;
		} else if (strcmp("-h", argv[1]) == 0) {
			print_usage();
			exit(0);
		} else {
			print_usage();
			exit(1);
		}
	}

	/* Initialize logging */
	TPT_INFO("Starting LMC handling server");

	if (!daemonize || !daemon(0, 0)) {
		/**
		 * Initialize ITC
		 */
		if (itc_init(MAILBOX_SIZE,
			     ITC_MALLOC,
			     NULL,
			     ITC_NO_NAMESPACE,
			     0)) {
			TPT_ERROR("Unable to inizalize ITC!");
			return -1;
		}
		/**
		* Create our mailbox.
		*/
		lmc_server_mbox = itc_create_mailbox(LMC_SERVER_NAME, 0);
		if (lmc_server_mbox == ITC_NO_ID) {
			TPT_ERROR(STR("%s: Unable to create ITC mailbox!",
			              LMC_SERVER_NAME));
			return -1;
		}
		memset(&act, '\0', sizeof(act));
		act.sa_handler = &exit_handler;
		if (sigaction(SIGTERM, &act, NULL) < 0) {
			TPT_ERROR("Failed to install signal exit handler");
			exit(1);
		}
		exit_code = main_loop();
	}

	return exit_code;
}
