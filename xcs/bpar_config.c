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
#include <errno.h>
#include <string.h>
#include <fcntl.h>
#include <itc.h>
#include <unistd.h>
#include <mtd/mtd-user.h>
#include <sys/ioctl.h>
#include <sys/stat.h>
#include <fcntl.h>
#include "xlfi_api.h"
#include "nvpi3_cfg.h"
#include "common.h"
#include "arpa/inet.h"
#include "mama.h"

#define DAEMON_NAME            "bpar_configd"
#define MAX_NUM_OF_MAILBOXES   4
#define BPAR_GROUP_NAME        "sys_bpar"
#define BPAR_WRITABLE_PDB_PATH "/var/log/pdb/"
#define BPAR_WRITABLE_DB_PATH  BPAR_WRITABLE_PDB_PATH BPAR_GROUP_NAME "/"
#define BPAR_WRITABLE_DB_FILE  BPAR_WRITABLE_DB_PATH "bpar.dtb"
#define BOOTENV_GROUP_NAME     "sys_bootenv"
#define BPAR_CFG_MAILBOX_NAME  "BPAR_CFG_MAILBOX"
#define MAX_FRAGMENT_SIZE      256
#define EXIT_SIGNAL            0xdeadbeef

#define TRACEPOINT_PROVIDER     com_ericsson_xcs_bpar_config
#include "tpt_create.h"
#include "tpt.h"

union itc_msg {
	uint32_t msgno;
};

enum partition {
	PARTITION_TYPE2, PARTITION_SPARE, PARTITION_BACKUP, NUM_OF_PARTITIONS
};

enum boot_mode {BOOT_MODE_WORKING, BOOT_MODE_LATEST};
enum xlf_state {XLF_INVALID, XLF_VALID, XLF_VALID_AND_WORKING};

/*
 * use __extension__ to disable invalid use of structure with flexible array
 * member [-Werror=pedantic] for "struct xlf_iboot_header header"
 */
__extension__ struct partition_info {
	char *name;
	enum xlf_state state;
	uint32_t num_of_files;
	struct xlfi_file_info *file;
	struct xlf_iboot_header header;
};

static const char *bpar_partition_env_var[NUM_OF_PARTITIONS] = {
	"sys_bpar_default_dev",
	"sys_bpar_dev",
	"sys_bpar_backup_dev"
};

static struct partition_info bpar_partition_info[NUM_OF_PARTITIONS];
static struct partition_info bootenv_partition_info;

struct database {
	nvpi3_db_group_handle group_handle;
	uint32_t count;
	struct nvpi3_db_definition definition[];
};

static struct database *bpar_database = NULL;
static struct database *bootenv_database = NULL;

static itc_mbox_id_t main_mbox = ITC_NO_ID;

static int get_debug_mode(void)
{
	char *env_value = getenv("SYS_DEBUG");

	if(env_value != NULL && !strcmp(env_value, "YES")) {
		return 1;
	} else {
		return 0;
	}
}

static enum boot_mode get_boot_mode(void)
{
	char *env_value = getenv("SYS_BOOT_MODE");

	if(env_value == NULL || strcmp(env_value, "LATEST")) {
		return BOOT_MODE_WORKING;
	} else {
		return BOOT_MODE_LATEST;
	}
}

static char *get_partition_name(const char *env_name)
{
	char *path;
	char *env_value;

	env_value = getenv(env_name);
	if(env_value == NULL) {
		TPT_ERROR(STR("environment variable %s not found",
		              env_name));
		return NULL;
	}

	path = malloc(strlen(env_value) + 1);
	if (!path)
		return NULL;

	strcpy(path, env_value);
	return path;
}

static const char *get_writable_db(void)
{
	int ret = -1;

	if (!get_debug_mode()) {
		goto get_writable_db_end;
	}

	ret = mkdir(BPAR_WRITABLE_PDB_PATH,
	            S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH);
	if (ret < 0 && errno != EEXIST) {
		TPT_ERROR(STR("mkdir %s failed (%d)", BPAR_WRITABLE_DB_PATH,
		              errno));
		goto get_writable_db_end;
	}

	ret = mkdir(BPAR_WRITABLE_DB_PATH,
	            S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH);
	if (ret < 0 && errno != EEXIST) {
		TPT_ERROR(STR("mkdir %s failed (%d)", BPAR_WRITABLE_DB_PATH,
		              errno));
		goto get_writable_db_end;
	}

	ret = open(BPAR_WRITABLE_DB_FILE,
	           O_CREAT | S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH);
	if (ret < 0) {
		if (errno != EEXIST) {
			TPT_ERROR(STR("open %s failed (%d)",
			              BPAR_WRITABLE_DB_FILE, errno));
		} else {
			ret = 0;
		}
	} else {
		ret = close(ret);
		if (ret) {
			TPT_ERROR(STR("close %s failed (%d)",
			              BPAR_WRITABLE_DB_FILE, errno));
		}
 	}

get_writable_db_end:
	return (ret) ? NULL : BPAR_WRITABLE_DB_FILE;
}

static enum xlf_state validate_param(char *path,
                                     struct xlf_iboot_header *header,
                                     uint32_t *num_of_files,
                                     struct xlfi_file_info **file)
{

	int done;
	uint8_t *data = NULL;
	int fd = -1;
	int first_time = 1;
	enum xlf_state xlf_state = XLF_INVALID;

	*file = NULL;
	data = malloc(MAX_FRAGMENT_SIZE);

	if(data == NULL) {
		TPT_ERROR("malloc failed");
		goto validate_param_end;
	}

	fd = open(path, O_RDONLY | O_SYNC | O_NONBLOCK);

	if (fd < 0) {
		TPT_ERROR(STR("open %s failed with errno %d", path, errno));
		goto validate_param_end;
	}

	do {
		xlfi_result_t xlfi_res;
		struct xlfi_parse_state state;
		const char *error;
		uint8_t *p = data;
		ssize_t size;

		size = read(fd, p, MAX_FRAGMENT_SIZE);

		if(size < 0) {
			TPT_ERROR(STR("read failed with errno %d", errno));
			goto validate_param_end;
		} else if(!size) {
			TPT_INFO("Unexpected zero bytes read.");
			goto validate_param_end;
		}

		if(first_time) {
			if (size < sizeof(struct xlf_iboot_header)) {
				TPT_INFO(STR("read bytes %d < expected %d.",
				             size,
				             sizeof(struct xlf_iboot_header)));
				goto validate_param_end;
			}

			xlfi_res =
				xlfi_validate_header(
					&state,
					(struct xlf_iboot_header *) p,
					&error);

			if (xlfi_res != XLFI_RESULT_SUCCESS) {
#if 0
				/* Fixme change to trace */
				TPT_INFO(STR("xlfi_validate_header failed with "
					     "%d \"%s\"", xlfi_res , error));
#endif
				goto validate_param_end;
			}


			memcpy(header, p, sizeof(struct xlf_iboot_header));

			*num_of_files = XLFI_GET_NUM_OF_FILES(header);
			*file = malloc(sizeof(struct xlfi_file_info) *
			               (*num_of_files));

			p += sizeof(struct xlf_iboot_header);
			size -= sizeof(struct xlf_iboot_header);
			first_time = 0;


		}

		xlfi_res =
			xlfi_validate(&state, header, p, size, num_of_files,
			              *file, &done, &error);

		if(xlfi_res != XLFI_RESULT_SUCCESS) {
#if 0
			/* Fixme change to trace */
			TPT_INFO(STR("xlfi_validate failed with %d \"%s\"",
			             xlfi_res , error));
#endif
			goto validate_param_end;
		}
	} while (!done);

	xlf_state = XLFI_XLF_HAS_WORKING_STATUS(header) ?
		XLF_VALID_AND_WORKING : XLF_VALID;

validate_param_end:
	if (fd >= 0) {
		if(close(fd) == -1) {
			TPT_ERROR(STR("close %s failed with errno %d",
			              path, errno));
		}
	}

	free(data);

	if (xlf_state == XLF_INVALID) {
		*num_of_files = 0;
		free(*file);
		*file = NULL;
	}

	return xlf_state;
}


static int valid_db(struct xlfi_file_info *file)
{
	if (file->info.magic != XLF_IBOOT_WPR_MAGIC_BLOB) {
		TPT_ERROR(STR("LM format %x not supported", file->info.magic));
		return 0;
	}

	if (file->info.blob.header.type != XLF_BLOB_TYPE_DTB &&
	    file->info.blob.header.type != XLF_BLOB_TYPE_ASCII_DB &&
	    file->info.blob.header.type != XLF_BLOB_TYPE_UENVIMAGE) {
		TPT_ERROR(STR("DB format %d not supported",
		              file->info.blob.header.type));
		return 0;
	}
	return 1;
}

static void cleanup(struct partition_info partition_info[NUM_OF_PARTITIONS])
{
	uint32_t i;

	for (i = 0; i < NUM_OF_PARTITIONS; i++) {
		free(partition_info[i].name);
		free(partition_info[i].file);
	}
}

static int valid_partition(enum boot_mode mode, enum partition partition,
                           enum xlf_state state)
{
	if (state == XLF_INVALID ||
	    (partition > PARTITION_TYPE2 && state == XLF_VALID &&
	     mode != BOOT_MODE_LATEST)) {
		return 0;
	}

	return 1;
}

static uint32_t validate_bpar_partitions(
        enum boot_mode mode, int validate_backup,
        struct partition_info partition_info[NUM_OF_PARTITIONS])
{
	uint32_t i, j, num_of_dbs = 0;

	memset(partition_info, 0,
	       sizeof(struct partition_info) * NUM_OF_PARTITIONS);

	for (i = 0; i < NUM_OF_PARTITIONS; i++) {
		if (i == PARTITION_BACKUP && !validate_backup) {
			continue;
		}

		partition_info[i].name =
			get_partition_name(bpar_partition_env_var[i]);
		if (partition_info[i].name == NULL) {
			num_of_dbs = 0;
			goto validate_bpar_partitions_end;
		}

		partition_info[i].state = validate_param(
			partition_info[i].name,
			&partition_info[i].header,
			&partition_info[i].num_of_files,
			&partition_info[i].file);

		if (!valid_partition(mode, i, partition_info[i].state)) {
			continue;
		}

		for (j = 0; j < partition_info[i].num_of_files; j++) {
			if (valid_db(&partition_info[i].file[j])) {
				num_of_dbs++;
			}
		}
	}

validate_bpar_partitions_end:
	if (!num_of_dbs) {
		cleanup(partition_info);
	}
	return num_of_dbs;
}

static uint32_t set_definition(nvpi3_db_permission_t permission,
                               nvpi3_db_commit_callback_func callback,
                               const char *parent_node,
                               struct partition_info *info,
                               struct nvpi3_db_definition def[])
{
	uint32_t i, j = 0, length = 0;

	for (i = 0; i < info->num_of_files; i++) {
		if (!valid_db(&info->file[i])) {
			continue;
		}
		if(parent_node != NULL) {
			length = strlen(parent_node);
			if(length) {
				strncpy(def[j].parent_node,
					parent_node,
					sizeof(def[j].parent_node));
			}
		}
		if(length < sizeof(def[j].parent_node)) {
			strncpy(&def[j].parent_node[length],
				info->file[i].info.blob.header.name,
				sizeof(def[j].parent_node) - length);
		}

		def[j].parent_node[sizeof(def[j].parent_node) - 1] = '\0';
		def[j].format = -1;
		if (info->file[i].info.blob.header.type == XLF_BLOB_TYPE_DTB) {
			def[j].format = NVPI3_DB_FORMAT_FTD;
		} else if (info->file[i].info.blob.header.type ==
		           XLF_BLOB_TYPE_ASCII_DB) {
			def[j].format = NVPI3_DB_FORMAT_ASCII_DB;
		} else if(info->file[i].info.blob.header.type ==
		          XLF_BLOB_TYPE_UENVIMAGE) {
			def[j].format = NVPI3_DB_FORMAT_UENVIMAGE;
		}

		def[j].permission = permission;
		def[j].storage.mtd.type = NVPI3_DB_STORAGE_TYPE_MTD;
		def[j].storage.mtd.offset = info->file[i].offset +
			offsetof(struct xlf_blob_header, header_size) +
			info->file[i].info.blob.header.header_size;
		def[j].storage.mtd.size = info->file[i].size -
			(def[j].storage.mtd.offset - info->file[i].offset);
		def[j].storage.mtd.max_size = def[j].storage.mtd.size;
		strncpy(def[j].storage.mtd.name, info->name,
		        sizeof(def[j].storage.mtd.name));
		def[j].storage.mtd.name[sizeof(def[j].storage.mtd.name) - 1] =
		        '\0';
		def[j].commit.user_data = &info->header;
		def[j].commit.callback = callback;
		j++;
	}

	return j;
}

int set_writable_db_definition(const char *writable_db,
                                struct nvpi3_db_definition *def)
{
	xlfi_result_t xlfi_res;
	const char *error;
	struct xlfi_file_info file;

	file.size = 0;
	xlfi_res = xlfi_get_max_file_size(256 * 1024, 0, 0, 1, &file, &error);
	if (xlfi_res != XLFI_RESULT_SUCCESS) {
		TPT_ERROR(STR("xlfi_get_max_file_size failed with %d \"%s\"",
		              xlfi_res , error));
		return -1;
	}

	def->format = NVPI3_DB_FORMAT_FTD;
	def->permission = NVPI3_DB_PERMISSION_RW;
	def->storage.file.type = NVPI3_DB_STORAGE_TYPE_FILE;
	def->storage.file.offset = 0;
	def->storage.file.max_size = file.size - sizeof(struct xlf_blob_header);
	strncpy(def->storage.file.name, writable_db,
	        sizeof(def->storage.file.name));
	def->storage.file.name[sizeof(def->storage.file.name) - 1] = '\0';
	return 0;
}

static int select_board_param(void)
{
	uint32_t num_of_dbs, size, i, j;
	nvpi3_result_t result;
	enum boot_mode mode = get_boot_mode();
	const char *writable_db;

	writable_db = get_writable_db();
	num_of_dbs = (writable_db != NULL) ? 1 : 0;
	num_of_dbs += validate_bpar_partitions(mode, 0, bpar_partition_info);
	if (!num_of_dbs) {
		TPT_ERROR("No board parameters found");
		goto select_board_param_error;
	}

	size = offsetof(struct database, definition) +
		sizeof(struct nvpi3_db_definition) * num_of_dbs;

	bpar_database = malloc(size);
	if (bpar_database == NULL) {
		TPT_ERROR(STR("malloc size 0x%x failed", size));
		goto select_board_param_error;
	}

	memset(bpar_database, 0, size);
	bpar_database->count = num_of_dbs;

	j = 0;

	if (writable_db != NULL) {
		if (set_writable_db_definition(writable_db,
		                               &bpar_database->definition[j])) {
			num_of_dbs--;
		} else {
			j++;
		}
	}

	for (i = PARTITION_SPARE; j < num_of_dbs; i--) {
		if (!valid_partition(mode, i, bpar_partition_info[i].state)) {
			continue;
		}

		j += set_definition(NVPI3_DB_PERMISSION_RO, NULL, "",
		                    &bpar_partition_info[i],
		                    &bpar_database->definition[j]);
	}

	result = nvpi3_create_db_group(BPAR_GROUP_NAME,
	                               bpar_database->count,
	                               bpar_database->definition,
	                               &bpar_database->group_handle);
	if(result != NVPI3_RESULT_SUCCESS) {
		TPT_ERROR(STR("create db group %s failed with error %d",
		              BPAR_GROUP_NAME, result));
		goto select_board_param_error;
	}
	return 0;

select_board_param_error:
	cleanup(bpar_partition_info);
	free(bpar_database);
	return -1;
}

static int get_boot_env(void)
{
	uint32_t num_of_dbs = 0;
	uint32_t size, i;
	nvpi3_result_t result;
	const char *cur_bootenv = getenv("SYS_CUR_BOOTENV");
	char *env_name = NULL;
	int ret = -1;

	memset(&bootenv_partition_info, 0, sizeof(struct partition_info));

	if(cur_bootenv == NULL) {
		TPT_ERROR("environment variable SYS_CUR_BOOTENV not found");
		goto get_boot_env_end;
	}
	size = sizeof("sys__dev") + strlen(cur_bootenv);
	env_name = malloc(size);
	if(env_name == NULL) {
		TPT_ERROR(STR("malloc size 0x%x failed", size));
		goto get_boot_env_end;
	}

	sprintf(env_name, "sys_%s_dev", cur_bootenv);

	bootenv_partition_info.name = get_partition_name(env_name);
	if(bootenv_partition_info.name == NULL) {
		TPT_ERROR(STR("Partition %s not found", env_name));
		goto get_boot_env_end;

	}

	bootenv_partition_info.state = validate_param(
		bootenv_partition_info.name,
		&bootenv_partition_info.header,
		&bootenv_partition_info.num_of_files,
		&bootenv_partition_info.file);

	if(bootenv_partition_info.state == XLF_INVALID) {
		TPT_ERROR("partition state is not valid");
		goto get_boot_env_end;
	}

	for (i = 0; i < bootenv_partition_info.num_of_files; i++) {
		if (valid_db(&bootenv_partition_info.file[i])) {
			num_of_dbs++;
		}
	}

	size = offsetof(struct database, definition) +
	       sizeof(struct nvpi3_db_definition) * num_of_dbs;

	bootenv_database = malloc(size);
	if (bootenv_database == NULL) {
		TPT_ERROR(STR("malloc size 0x%x failed", size));
		goto get_boot_env_end;
	}

	memset(bootenv_database, 0, size);
	bootenv_database->count = num_of_dbs;

	set_definition(NVPI3_DB_PERMISSION_RO, NULL, "/current_bootenv/",
	               &bootenv_partition_info,
	               &bootenv_database->definition[0]);


	result = nvpi3_create_db_group(BOOTENV_GROUP_NAME,
	                               bootenv_database->count,
	                               bootenv_database->definition,
	                               &bootenv_database->group_handle);
	if(result != NVPI3_RESULT_SUCCESS) {
		TPT_ERROR(STR("create db group %s failed with error %d",
		              BOOTENV_GROUP_NAME, result));
		goto get_boot_env_end;
	}
	free(env_name);
	return 0;

get_boot_env_end:
	free(env_name);
	free(bootenv_partition_info.name);
	free(bootenv_partition_info.file);
	free(bootenv_database);
	return ret;
}

/* Keep program running so that created group handle is not destroyed. */
static void msg_loop(void)
{
	union itc_msg *msg;
	for(;;) {
		msg = itc_receive(ITC_NOFILTER, ITC_NO_TMO, ITC_FROM_ALL);
		if (msg->msgno == EXIT_SIGNAL) {
			TPT_INFO(STR("%s exiting as ordered", DAEMON_NAME));
			itc_free(&msg);
			return;
		}

		TPT_ERROR(STR("Received Unexpected message: %d\n",
		              msg->msgno));
		itc_free(&msg);
	}
}

static int init_itc(void)
{
	int ret = itc_init(MAX_NUM_OF_MAILBOXES, ITC_MALLOC, NULL,
	                   ITC_NO_NAMESPACE, 0);
	if (ret) {
		TPT_ERROR(STR("itc_init failed (%d)", ret));
	} else {
		main_mbox = itc_create_mailbox(BPAR_CFG_MAILBOX_NAME, 0);
		if (main_mbox == ITC_NO_ID) {
			TPT_ERROR(STR("itc_create_mailbox %s",
			              BPAR_CFG_MAILBOX_NAME));
			ret = -1;
		}
	}

	return ret;
}



static void print_usage()
{
	printf("Usage: pdbcfg <options>\n\n"
	       "Options:\n"
	       "    -h  Display usage information (this message).\n"
	       "    -d  Daemonize the program.\n\n");
}

static void exit_handler(__attribute__((__unused__)) int sig)
{
	union itc_msg *msg;

	TPT_INFO(STR("Receive signal %d, terminating", sig));
	msg = itc_alloc(sizeof(uint32_t), EXIT_SIGNAL);
	itc_send((union itc_msg **)&msg, main_mbox, ITC_MY_MBOX);
}

int main(int argc, char **argv)
{
	int daemonize = 0;

	if (argc > 1) {
		if (strcmp("-d", argv[1]) == 0) {
			daemonize = 1;
		} else if (strcmp("-h", argv[1]) == 0) {
			print_usage();
			exit(0);
		} else {
			print_usage();
			exit(-EINVAL);
		}
	}


	if (lock_daemon(DAEMON_NAME)) {
		TPT_INFO(STR("failed to obtain lock: %s\n", DAEMON_NAME));
		goto main_err;
	}


	if (!daemonize || !daemon(0, 0)) {
		TPT_INFO(STR("Starting %s %s",
		             daemonize ? "daemon" : "foreground process",
		             DAEMON_NAME));

		if(init_itc()) {
			goto main_err;
		}

		if(select_board_param()) {
			TPT_ERROR("Failed to select board param");
			goto main_err;
		}
		if(get_boot_env()) {
			TPT_ERROR("Failed to get boot env");
			goto main_err;
		}
		if (signal(SIGTERM, exit_handler) == SIG_ERR) {
			TPT_ERROR("Failed to install signal exit handler");
			goto main_err;
		}

		if(mama_send_sync()) {
			TPT_ERROR("Failed to send sync to mama");
			goto main_err;
		}

		msg_loop();
		exit(0);
	} else {
		TPT_ERROR(STR("Failed to start daemon %s", DAEMON_NAME));
	}

main_err:
	exit(-EFAULT);
}
