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
#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <fcntl.h>
#include <errno.h>
#include <unistd.h>
#include <arpa/inet.h>
#include "nvpi3_srv.h"
#include "xlfi_api.h"

#define BPAR_GROUP_NAME   "sys_bpar"
#define MAX_FRAGMENT_SIZE 256

#define TRACEPOINT_PROVIDER com_ericsson_xcs_pdbpop
#include "tpt_create.h"
#include "tpt.h"

#define ERR(fmt, ...) do {					\
		char *_error_string = STR(fmt, ##__VA_ARGS__);	\
		TPT_ERROR(_error_string);			\
		puts(_error_string);				\
	} while (0)

enum partition {
	PARTITION_TYPE2, PARTITION_BACKUP, PARTITION_SPARE, NUM_OF_PARTITIONS};

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

struct enum_keys_callback_data {
	uint32_t node_name_length;
	FILE *fp;
	nvpi3_node_handle node_handle;
	nvpi3_node_handle root_node_handle;
};

struct get_value_callback_data {
	const char *env_name;
	FILE *fp;
};

static const char *partition_env_var[NUM_OF_PARTITIONS] = {
	"sys_bpar_default_dev",
	"sys_bpar_backup_dev",
	"sys_bpar_dev"
};

static enum boot_mode get_boot_mode(void)
{
	char *env_value = getenv("SYS_BOOT_MODE");

	if(env_value == NULL || strcmp(env_value, "LATEST")) {
		return BOOT_MODE_WORKING;
	} else {
		return BOOT_MODE_LATEST;
	}
}

static char *get_partition_name(enum partition type)
{
	char *path;
	char *env_value = getenv(partition_env_var[type]);

	if(env_value == NULL) {
		ERR("environment variable %s not found",
		    partition_env_var[type]);
		return NULL;
	}

	path = malloc(strlen(env_value) + 1);
	if (path != NULL) {
		strcpy(path, env_value);
	}

	return path;
}

static enum xlf_state validate_board_param(char *path,
                                           struct xlf_iboot_header *header,
                                           uint32_t *num_of_files,
                                           struct xlfi_file_info **file)
{

	int done;
	uint8_t *data;
	int fd = -1;
	int first_time = 1;
	enum xlf_state xlf_state = XLF_INVALID;

	*file = NULL;
	data = malloc(MAX_FRAGMENT_SIZE);

	if(data == NULL) {
		ERR("malloc of size %d failed", MAX_FRAGMENT_SIZE);
		goto validate_board_param_end;
	}

	fd = open(path, O_RDONLY | O_SYNC | O_NONBLOCK);

	if (fd < 0) {
		ERR("open %s failed with errno %d", path, errno);
		goto validate_board_param_end;
	}

	do {
		xlfi_result_t xlfi_res;
		struct xlfi_parse_state state;
		const char *error;
		uint8_t *p = data;
		ssize_t  size;

		size = read(fd, p, MAX_FRAGMENT_SIZE);

		if(size < 0) {
			ERR("read failed with errno %d", errno);
			goto validate_board_param_end;
		} else if(!size) {
			ERR("Unexpected zero bytes read%s", "");
			goto validate_board_param_end;
		}

		if(first_time) {
			if (size < sizeof(struct xlf_iboot_header)) {
				ERR("read bytes %d < expected %d", size,
				    sizeof(struct xlf_iboot_header));
				goto validate_board_param_end;
			}

			xlfi_res =
				xlfi_validate_header(
					&state,
					(struct xlf_iboot_header *) p,
					&error);

			if (xlfi_res != XLFI_RESULT_SUCCESS) {
#if 0
				/* Fixme change to trace */
				ERR("xlfi_validate_header failed with %d "
				    "\"%s\"", xlfi_res , error);
#endif
				goto validate_board_param_end;
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
			ERR("xlfi_validate failed with %d \"%s\"", xlfi_res ,
			    error);
#endif
			goto validate_board_param_end;
		}
	} while (!done);

	xlf_state = XLFI_XLF_HAS_WORKING_STATUS(header) ?
		XLF_VALID_AND_WORKING : XLF_VALID;

validate_board_param_end:
	if (fd >= 0) {
		if(close(fd) == -1) {
			ERR("close %s failed with errno %d", path, errno);
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
		ERR("LM format %x not supported", file->info.magic);
		return 0;
	}

	if (file->info.blob.header.type != XLF_BLOB_TYPE_DTB &&
	    file->info.blob.header.type != XLF_BLOB_TYPE_ASCII_DB) {
		ERR("DB format %d not supported", file->info.blob.header.type);
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
		partition_info[i].name = NULL;
		partition_info[i].file = NULL;
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

static uint32_t validate_partitions(
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

		partition_info[i].name = get_partition_name(i);

		if (partition_info[i].name == NULL) {
			num_of_dbs = 0;
			goto validate_partitions_end;
		}

		partition_info[i].state = validate_board_param(
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

validate_partitions_end:
	if (!num_of_dbs) {
		cleanup(partition_info);
	}
	return num_of_dbs;
}

static uint32_t set_definition(nvpi3_db_permission_t permission,
                               nvpi3_db_commit_callback_func callback,
                               struct partition_info *info,
                               struct nvpi3_db_definition def[])
{
	uint32_t i, j = 0;

	for (i = 0; i < info->num_of_files; i++) {
		if (!valid_db(&info->file[i])) {
			continue;
		}

		strncpy(def[j].parent_node,
		        info->file[i].info.blob.header.name,
		        sizeof(def[j].parent_node));
		def[j].parent_node[sizeof(def[j].parent_node)-1] = '\0';
		def[j].format = -1;
		if (info->file[i].info.blob.header.type == XLF_BLOB_TYPE_DTB) {
			def[j].format = NVPI3_DB_FORMAT_FTD;
		} else if (info->file[i].info.blob.header.type ==
		           XLF_BLOB_TYPE_ASCII_DB) {
			def[j].format = NVPI3_DB_FORMAT_ASCII_DB;
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
		def[j].storage.mtd.name[sizeof(def[j].storage.mtd.name)-1] =
			 '\0';
		def[j].commit.user_data = &info->header;
		def[j].commit.callback = callback;
		j++;
	}

	return j;
}

static const char *get_type_suffix(const char *key_name, nvpi3_key_type_t *type)
{
	static const struct {
		char *str;
		uint32_t length;
	} type_suffix[NVPI3_KEY_TYPE_BIN+1] = {{"#str", sizeof("#str")-1},
	                                       {"#u8", sizeof("#u8")-1},
	                                       {"#u32",  sizeof("#u32")-1},
	                                       {"", sizeof("")-1}};
	uint32_t length = strlen(key_name);

	for (uint32_t i = 0;
	     i < sizeof(type_suffix)/sizeof(type_suffix[0]);
	     i++) {
		const char *p;

		if (length <= type_suffix[i].length) {
			continue;
		}

		p = &key_name[length - type_suffix[i].length];
		if (!strcmp(p, type_suffix[i].str)) {
			*type = i;
			return p;
		}
	}

       *type = NVPI3_KEY_TYPE_BIN;
       return &key_name[length];
}

static nvpi3_result_t get_value_callback(void *user_data, uint32_t size,
                                         const union nvpi3_key_value *value)
{
	struct get_value_callback_data *ud =
		(struct get_value_callback_data *) user_data;

	fprintf(ud->fp, "export %s=\"%s\"\n", ud->env_name, value->str);
	return NVPI3_RESULT_SUCCESS;
}

static nvpi3_result_t enum_keys_callback(
	void *user_data,
	__attribute__((__unused__)) const char *database_name,
	const char *key_name,
	nvpi3_key_type_t type,
	__attribute__((__unused__)) const char *type_str,
	uint32_t size,
	const union nvpi3_key_value *value)
{
	nvpi3_result_t result;
	struct get_value_callback_data callback_data;
	const char *env_value_type_str;
	nvpi3_key_type_t env_value_type;
	char *env_value_key_name = NULL;
	struct enum_keys_callback_data *ud =
		(struct enum_keys_callback_data *) user_data;

	callback_data.env_name = &key_name[ud->node_name_length];
	callback_data.fp = ud->fp;

	if (type != NVPI3_KEY_TYPE_STR) {
		ERR("key %s has invalid type %d", key_name, type);
		result = NVPI3_RESULT_INVALID_PARAM;
		goto enum_keys_callback_end;
	}

	env_value_type_str = get_type_suffix(value->str, &env_value_type);
	if (env_value_type != NVPI3_KEY_TYPE_STR) {
		ERR("Value %s has unsupported type %d", value->str,
		    env_value_type);
		result = NVPI3_RESULT_UNSUPPORTED;
		goto enum_keys_callback_end;
	}

	env_value_key_name = malloc(env_value_type_str - value->str);
	if (env_value_key_name == NULL) {
		ERR("malloc of size %d failed",
		    env_value_type_str - value->str);
		result = NVPI3_RESULT_OTHER_ERROR;
		goto enum_keys_callback_end;
	}

	strncpy(env_value_key_name, &value->str[1],
	        env_value_type_str - &value->str[1]);
	env_value_key_name[env_value_type_str - &value->str[1]] = '\0';

	result = nvpi3_srv_get_value(ud->root_node_handle, env_value_key_name,
	                             env_value_type, get_value_callback,
	                             &callback_data);
	if(result != NVPI3_RESULT_SUCCESS) {
		ERR("nvpi3_srv_get_value for key %s of type %d with error %d",
		    env_value_key_name, env_value_type, result);
	}

enum_keys_callback_end:
	free(env_value_key_name);
	return result;
}

static int db_to_env(char *env_node, char *outfilename)
{
	uint32_t num_of_dbs, i, j;
	nvpi3_result_t result;
	struct partition_info partition_info[NUM_OF_PARTITIONS];
	struct enum_keys_callback_data  callback_data;
	nvpi3_db_group_handle group_handle = NULL;
	enum boot_mode mode = get_boot_mode();
	struct nvpi3_db_definition *definition = NULL;

	callback_data.node_name_length = strlen(env_node);
	callback_data.fp = NULL;
	callback_data.node_handle = NULL;
	callback_data.root_node_handle = NULL;

	num_of_dbs = validate_partitions(mode, 1, partition_info);
	if (!num_of_dbs) {
		ERR("No board parameters found%s","");
		goto db_to_env_end;
	}

	definition = malloc(sizeof(*definition) * num_of_dbs);
	if (definition == NULL) {
		ERR("malloc of size %d failed",
		    sizeof(*definition) * num_of_dbs);
		goto db_to_env_end;
	}

	memset(definition, 0, sizeof(*definition) * num_of_dbs);

	for (i = PARTITION_SPARE, j = 0; j < num_of_dbs; i--) {
		if (!valid_partition(mode, i, partition_info[i].state)) {
			continue;
		}

		j += set_definition(NVPI3_DB_PERMISSION_RO, NULL,
		                    &partition_info[i], &definition[j]);
	}

	result = nvpi3_srv_create_db_group(NULL, BPAR_GROUP_NAME, num_of_dbs,
	                                   definition, NULL, &group_handle);
	if(result != NVPI3_RESULT_SUCCESS) {
		ERR("nvpi3_srv_create_db_group %s failed with error %d",
		    BPAR_GROUP_NAME, result);
		goto db_to_env_end;
	}

	result = nvpi3_srv_open_node(group_handle, env_node,
	                             &callback_data.node_handle);
	if(result != NVPI3_RESULT_SUCCESS) {
		ERR("nvpi3_srv_open_node %s failed with error %d", env_node,
		    result);
		goto db_to_env_end;
	}

	result = nvpi3_srv_open_node(group_handle, "/",
	                             &callback_data.root_node_handle);
	if(result != NVPI3_RESULT_SUCCESS) {
		ERR("nvpi3_srv_open_node %s failed with error %d", "/", result);
		goto db_to_env_end;
	}

	callback_data.fp = fopen(outfilename, "w");
	if (callback_data.fp == NULL ) {
		ERR("Failed to open file %s for writing", outfilename);
		goto db_to_env_end;
	}

	result = nvpi3_srv_enum_keys(callback_data.node_handle, 0,
	                             enum_keys_callback, &callback_data);
	if(result != NVPI3_RESULT_SUCCESS) {
		ERR("nvpi3_srv_enum_keys %p failed with error %d",
		    (void *) callback_data.node_handle, result);
		goto db_to_env_end;
	}

db_to_env_end:
	if (callback_data.node_handle != NULL) {
		result =  nvpi3_srv_close_node(callback_data.node_handle);
		if(result != NVPI3_RESULT_SUCCESS) {
			ERR("nvpi3_srv_close_node %p failed with error %d",
			    (void *) callback_data.node_handle, result);
		}
	}


	if (callback_data.root_node_handle != NULL) {
		result =  nvpi3_srv_close_node(callback_data.root_node_handle);
		if(result != NVPI3_RESULT_SUCCESS) {
			ERR("nvpi3_srv_close_node %p failed with error %d",
			    (void *) callback_data.root_node_handle, result);
		}
	}


	if (group_handle != NULL) {
		result =  nvpi3_srv_destroy_db_group(group_handle);
		if(result != NVPI3_RESULT_SUCCESS) {
			ERR("nvpi3_srv_destroy_db_group %p failed with error "
			    "%d", (void *) group_handle, result);
		}
	}

	if (callback_data.fp != NULL && fclose(callback_data.fp)) {
		ERR("fclose %p failed", (void *) callback_data.fp);
	}
	cleanup(partition_info);
	free(definition);
	return 0;
}

int main(int argc, char *argv[])
{
	if (argc < 3) {
		fprintf(stderr, "Usage: pdbpop <environment node> "
		        "<output file>\n");
		return 1;
	}

	fprintf(stderr, "Generating environment variables to %s from node %s\n",
	        argv[2], argv[1]);

	return db_to_env(argv[1], argv[2]);
}
