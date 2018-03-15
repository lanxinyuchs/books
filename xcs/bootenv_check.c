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


#include <stdlib.h>
#include <errno.h>
#include "nvpi3.h"
#include <itc.h>
#include "mtd/mtd-user.h"
#include "errno.h"
#include "fcntl.h"
#include <sys/ioctl.h>
#include <uio_helper.h>
#include "libresetmem.h"

#define TRACEPOINT_PROVIDER com_ericsson_xcs_bootenv_check
#include "tpt_create.h"
#include "tpt.h"

#define MAX_NUM_OF_MAILBOXES   4
#define BOOTENV_GROUP_NAME "sys_bootenv"
#define BOOTENV_CHECK_MAILBOX_NAME  "BOOTENV_CHECK_MAILBOX"

#define MAX_LMC_NUMBER  4
#define MIN_LMC_NUMBER  3

#define LOCK_SLOT_ARG      "SYS_LOCK_SLOT"
#define LOCK_SLOT_ARG_YES  "YES"
#define LOCK_SLOT_ARG_NO   "NO"
#define HW_PROFILE_ARG     "SYS_HW_PROFILE"
#define FLASH_SIZE_ARG     "SYS_FLASH_SIZE"

struct uio_map {
	UIO_HANDLE_ handle;
	void *base;
} uio_map = {NULL, NULL};


static int init_itc(itc_mbox_id_t *main_mbox)
{
	int ret = itc_init(MAX_NUM_OF_MAILBOXES, ITC_MALLOC, NULL,
	                   ITC_NO_NAMESPACE, 0);
	if (ret) {
		TPT_ERROR(STR("itc_init failed (%d)", ret));
	} else {
		*main_mbox = itc_create_mailbox(BOOTENV_CHECK_MAILBOX_NAME, 0);
		if (*main_mbox == ITC_NO_ID) {
			TPT_ERROR(STR("itc_create_mailbox %s",
			              BOOTENV_CHECK_MAILBOX_NAME));
			ret = -1;
		}
	}

	return ret;
}

static int get_mtd_info(char *name, uint32_t *size, uint32_t *erasesize)
{
	mtd_info_t mtd_info;
	int ret = -1;

	int fd = open(name, O_RDONLY | O_SYNC | O_NONBLOCK);
	if (fd < 0) {
		TPT_ERROR(STR("open %s failed with errno %d", name, errno));
		return -1;
	}
	if (ioctl(fd, MEMGETINFO, &mtd_info) == -1) {
		TPT_ERROR(STR("ioctl %s failed with errno %d", name, errno));
		goto get_mtd_info_end;
	}
	*size = mtd_info.size;
	*erasesize = mtd_info.erasesize;
	ret = 0;

get_mtd_info_end:
	if(close(fd) == -1) {
		TPT_ERROR(STR("close %s failed with errno %d", name, errno));
		ret = -1;
	}

	return ret;
}

static int static_partition_check(uint32_t *size)
{
	const char* static_partition_env[] =
		{"sys_pboot_dev",
		 "sys_boot_default_dev",
		 "sys_boot_dev",
		 "sys_boot_backup_dev",
		 "sys_bootenv_default_dev",
		 "sys_bootenv_dev",
		 "sys_bootenv_backup_dev",
		 "sys_bpar_default_dev",
		 "sys_bpar_dev",
		 "sys_bpar_backup_dev",
		 "sys_nodeid_dev",
		 "sys_hwlog_dev"
		};

	char *mtd_dev = NULL;
	uint32_t mtd_size;
	uint32_t mtd_erasesize;
	uint32_t i;
	*size = 0;
	for(i = 0;
	    i < sizeof(static_partition_env)/
		    sizeof(static_partition_env[0]);
	    i++) {
		mtd_dev = getenv(static_partition_env[i]);
		if(mtd_dev == NULL) {
			TPT_ERROR(STR("%s doesn't exist",
			              static_partition_env[i]));
			return -1;

		}
		if(get_mtd_info(mtd_dev, &mtd_size, &mtd_erasesize)) {
			return -1;
		}
        /*pboot and boot partitions size changed to 1M */
                if(i < 4 && mtd_size != 1024*1024){
                        TPT_ERROR(STR("mtd size 0x%x of %s differs from expected size 1MB",
                                      mtd_size, mtd_dev));
                        return -1;
                } 
		if(i > 3 && mtd_size != 256*1024) {
			TPT_ERROR(STR("mtd size 0x%x of %s differs from expected size 256kB",
				      mtd_size, mtd_dev));
			return -1;
                } 
		*size += mtd_size;
	}
	return 0;
}

static int verify_uflash_partition(uint32_t size, char *name)
{
	uint32_t mtd_size;
	uint32_t mtd_erasesize;
	char *mtd_dev = NULL;

	/* get mtd info */
	mtd_dev = getenv(name);
	if(mtd_dev == NULL) {
		TPT_ERROR(STR("%s doesn't exist", name));
		return -1;
	}
	if(get_mtd_info(mtd_dev, &mtd_size, &mtd_erasesize)) {
		return -1;
	}

	/* verify size alignment */
	if((size & (mtd_erasesize - 1)) != 0) {
		TPT_ERROR(STR("size (0x%x) of %s(%s) is not aligned with "
		              "flash block size 0x%x.", mtd_size,
		              name, mtd_dev, mtd_erasesize));
		return -1;
	}
	/* verify size setting */
	if(size != mtd_size) {
		TPT_ERROR(STR("mtd size 0x%x is set not equal as expected size "
		              "0x%x.", mtd_size, size));
		return -1;
	}
	return 0;
}

static int get_uflash_partition_info(char *str, uint32_t *size, char *name)
{

	char unit = '\0';
	char parenthesis = '\0';
	int num_match;
	num_match = sscanf(str, "%u%[kM](%[a-zA-Z_]%c",
	                   size, &unit, name, &parenthesis);
	if(num_match != 4) {
		TPT_ERROR(STR("the (%s) format is not correct.", str));
		return -1;
	}
	if(unit != 'k' && unit != 'M') {
		TPT_ERROR(STR("the (%s) format is not correct, "
		              "%c missing k or M", str, unit));
		return -1;
	}
	if(parenthesis != ')') {
		TPT_ERROR(STR("the (%s) format is not correct, missing )",
		              str));
		return -1;
	}
/* the user flash partition changed, delete this compare */
/*
	if(strncmp(name, "uflash_", sizeof("uflash_") - 1)) {
		TPT_ERROR(STR("the (%s) format is not correct,"
		              " name %s is not uflash_xxx", str, name));
	}
*/
	unit == 'k' ? ((*size) *= 1024) : ((*size) *= (1024 * 1024));
	return 0;
}

static int check_uflash_info(char *str, uint32_t *uflash_size)
{
	uint32_t size;
	char *next, *name = NULL;
	int ret = -1;

	*uflash_size = 0;
	/*
	 * The string we allocate for one environment variable name is much
	 * bigger than needed. Still using the length of str is the best we can
	 * do without parsing the str to find max partition name that it
	 * contains.
	 */
	size = strlen(str) + sizeof("sys__dev");
	name = malloc(size);
	if(name == NULL) {
		TPT_ERROR(STR("malloc size %d failed", size));
		goto check_uflash_info_end;
	}
	memcpy(name, "sys_", sizeof("sys_") - 1);
	while((next = strtok(str, ",")) != NULL) {
		if(get_uflash_partition_info(next, &size,
		                             &name[sizeof("sys_") - 1])) {
			goto check_uflash_info_end;
		}
		strcat(name, "_dev");
		if(verify_uflash_partition(size, name)) {
			goto check_uflash_info_end;
		}
		*uflash_size += size;
		str = NULL;
	}
	ret = 0;
check_uflash_info_end:
	free(name);
	return ret;
}

static nvpi3_result_t get_bootenv_value(nvpi3_node_handle node_handle,
                                        const char *env_name, char **value)
{
	uint32_t size;
	nvpi3_result_t result;

	result = nvpi3_get_value_size(node_handle, env_name, NVPI3_KEY_TYPE_STR,
	                              &size);
	if(result == NVPI3_RESULT_NOT_FOUND) {
		TPT_TRACE(1, STR("not found %s", env_name));
		goto get_bootenv_value_end;
	} else if(result != NVPI3_RESULT_SUCCESS) {
		TPT_ERROR(STR("get %s size failed with %d", env_name, result));
		goto get_bootenv_value_end;
	}

	*value = malloc(size);
	if(*value == NULL) {
		TPT_ERROR(STR("malloc size %d failed", size));
		goto get_bootenv_value_end;
	}
	result = nvpi3_get_value(node_handle, env_name, NVPI3_KEY_TYPE_STR,
	                         &size, (union nvpi3_key_value *)*value);

	if(result == NVPI3_RESULT_NOT_FOUND) {
		TPT_TRACE(1, STR("not found %s", env_name));
		goto get_bootenv_value_end;
	} else if(result != NVPI3_RESULT_SUCCESS) {
		TPT_ERROR(STR("get %s value failed with %d", env_name, result));
		goto get_bootenv_value_end;
	}
get_bootenv_value_end:
	if(result != NVPI3_RESULT_SUCCESS) {
		free(*value);
		*value = NULL;
	}
	return result;
}

static int user_flash_check(nvpi3_node_handle node_handle,
                            uint32_t *uflash_size)
{
	char *value = NULL;
	nvpi3_result_t result;
	int ret = 0;

	result = get_bootenv_value(node_handle, "sys_user_flash_partitions",
	                           &value);
	if(result == NVPI3_RESULT_SUCCESS) {
		if(check_uflash_info(value, uflash_size)) {
			ret = -1;
		}
	} else if (result == NVPI3_RESULT_NOT_FOUND) {
		TPT_TRACE(1, "sys_user_flash_partitions not found in bootenv");
		*uflash_size = 0;
	} else {
		TPT_ERROR("Fail to get sys_user_flash_partitions value "
		          "in bootenv.");
		ret = -1;
	}

	free(value);
	return ret;
}

static int lock_slot_check(nvpi3_node_handle node_handle)
{
	char *bootenv_value = NULL;
	nvpi3_result_t result;
	char * env_value = NULL;
	int ret = -1;
	result = get_bootenv_value(node_handle, "sys_lock_slot",
	                           &bootenv_value);
	env_value = getenv(LOCK_SLOT_ARG);

	if(result == NVPI3_RESULT_SUCCESS) {
		if(env_value != NULL) {
			if(!strcmp(bootenv_value, "yes") &&
			   !strcmp(env_value, LOCK_SLOT_ARG_YES)) {
				ret = 0;
			} else if(!strcmp(bootenv_value, "no") &&
				  !strcmp(env_value, LOCK_SLOT_ARG_NO)) {
				ret = 0;
			} else {
				TPT_ERROR(STR("env %s:%s does not match with "
				              "bootenv sys_lock_slot:%s",
				              LOCK_SLOT_ARG, env_value,
				              bootenv_value));
			}
		} else {
			TPT_ERROR(STR("env %s does not exist",
			              LOCK_SLOT_ARG));
		}
	} else if(result == NVPI3_RESULT_NOT_FOUND) {
		if(env_value == NULL) {
			ret = 0;
		} else {
			TPT_ERROR(STR("bootenv sys_lock_slot not found "
			              "but env %s:%s exist",
			              LOCK_SLOT_ARG, env_value));
		}
	} else {
		TPT_ERROR(STR("Fail to get sys_lock_slot value "
		              "in bootenv with result %d.", result));
	}
	free(bootenv_value);
	return ret;
}

static int hw_profile_check(nvpi3_node_handle node_handle)
{
	char *bootenv_value = NULL;
	nvpi3_result_t result;
	char * env_value = NULL;
	int ret = -1;
	result = get_bootenv_value(node_handle, "sys_hw_profile",
	                           &bootenv_value);
	env_value = getenv(HW_PROFILE_ARG);

	if(result == NVPI3_RESULT_SUCCESS) {
		if(env_value != NULL) {
			if(!strcmp(bootenv_value, env_value)) {
				ret = 0;
			} else {
				TPT_ERROR(STR("env %s:%s does not match with "
				              "bootenv sys_hw_profile:%s",
				              HW_PROFILE_ARG, env_value,
				              bootenv_value));
			}
		} else {
			TPT_ERROR(STR("env %s does not exist",
			              HW_PROFILE_ARG));
		}
	} else if(result == NVPI3_RESULT_NOT_FOUND) {
		if(env_value == NULL) {
			ret = 0;
		} else {
			TPT_ERROR(STR("bootenv sys_hw_profile not found "
			              "but env %s:%s exist",
			              HW_PROFILE_ARG, env_value));
		}
	} else {
		TPT_ERROR(STR("Fail to get sys_hw_profile value "
		              "in bootenv with result %d.", result));
	}
	free(bootenv_value);
	return ret;
}

static int lmc_number_check(nvpi3_node_handle node_handle,
                            uint32_t *number_of_lmcs)
{
	char *bootenv_value = NULL, *end;
	nvpi3_result_t result;
	int ret = -1;

	*number_of_lmcs = 0;

	/* Verify with bootenv value of sys_lmc_number */
	result = get_bootenv_value(node_handle, "sys_lmc_number",
	                           &bootenv_value);

	if(result == NVPI3_RESULT_SUCCESS) {
		*number_of_lmcs = strtoul(bootenv_value, &end, 0);
		if (*end != '\0') {
			TPT_ERROR(STR("sys_lmc_number has invalid value %s",
			              bootenv_value));
			goto lmc_number_check_end;
		}
	} else if(result == NVPI3_RESULT_NOT_FOUND) {
		*number_of_lmcs = 4;
		TPT_TRACE(1, "sys_lmc_number not found, using default value 4");
	} else {
		TPT_ERROR("Fail to get sys_lmc_number in bootenv.");
		goto lmc_number_check_end;
	}

	if(*number_of_lmcs < MIN_LMC_NUMBER) {
		TPT_ERROR(STR("LMC number %d is less than %d",
		              *number_of_lmcs, MIN_LMC_NUMBER));
		goto lmc_number_check_end;
	} else if(*number_of_lmcs > MAX_LMC_NUMBER) {
		TPT_ERROR(STR("LMC number %d exceed %d",
		              *number_of_lmcs, MAX_LMC_NUMBER));
		goto lmc_number_check_end;
	}

	ret = 0;
lmc_number_check_end:
	free(bootenv_value);
	return ret;
}

static int get_auboot_size(nvpi3_node_handle node_handle, uint32_t *auboot_size)
{
	char *bootenv_value = NULL, *end;
	int ret = -1;
	nvpi3_result_t result;
	*auboot_size = 0;

	result = get_bootenv_value(node_handle,
	                           "sys_auboot_size",
	                           &bootenv_value);
	if(result == NVPI3_RESULT_SUCCESS) {
		*auboot_size = strtoul(bootenv_value, &end, 0);
		if(*end != '\0') {
			TPT_ERROR(STR("sys_auboot_size has invalid value %s",
			              bootenv_value));
			goto get_auboot_size_end;
		}
		if(*auboot_size == 0) {
			TPT_ERROR("Invalid zero value for sys_auboot_size");
			goto get_auboot_size_end;
		}
	} else if(result == NVPI3_RESULT_NOT_FOUND) {
		TPT_TRACE(1, "sys_auboot_size not found in bootenv");
	} else {
		TPT_ERROR("Fail to get sys_auboot_size value in bootenv.");
		goto get_auboot_size_end;
	}
	ret = 0;
get_auboot_size_end:
	free(bootenv_value);
	return ret;
}

static int check_auboot_size(nvpi3_node_handle node_handle,
                             uint32_t *auboot_size)
{

	char *mtd_dev;
	uint32_t mtd_size, mtd_erasesize;

	if(get_auboot_size(node_handle, auboot_size)) {
		return -1;
	}

	if(*auboot_size == 0) {
		return 0;
	}

	mtd_dev = getenv("sys_lmc0_dev");
	if(mtd_dev == NULL) {
		return -1;
	}

	if(get_mtd_info(mtd_dev, &mtd_size, &mtd_erasesize)) {
		return -1;
	}

	if (*auboot_size & (mtd_erasesize - 1)) {
		TPT_ERROR(STR("sys_auboot_size (0x%x) is not "
		              "aligned to flash block size "
		              "0x%x", *auboot_size,
		              mtd_erasesize));
		return -1;
	}

	if(*auboot_size != mtd_size) {
		TPT_ERROR(STR("%s size 0x%x is not equal to expected "
		              "size 0x%x.", mtd_dev, mtd_size,
		              *auboot_size));
		return -1;
	}
	return 0;
}

static int validate_slots_size(uint32_t lmc_number_start,
                               uint32_t number_of_lmcs,
                               uint32_t slots_size_left)
{
	int ret = -1;
	int i;
	for(i = lmc_number_start; i < 15; i++) {
		char *mtd_dev;
		char sys_lmc_dev[sizeof("sys_lmc99_dev")];
		uint32_t mtd_size, mtd_erasesize, slot_size;

		sprintf(sys_lmc_dev, "sys_lmc%d_dev", i);

		mtd_dev = getenv(sys_lmc_dev);
		if(mtd_dev == NULL) {
			if(i < number_of_lmcs){
				TPT_ERROR(STR("Did not find partition %s=%s",
				              sys_lmc_dev, mtd_dev));
				goto validate_slots_size_end;
			}
			continue;
		}

		if(i >= number_of_lmcs) {
			TPT_ERROR(STR("Found unexpected partition "
			              "%s=%s", sys_lmc_dev, mtd_dev));
			goto validate_slots_size_end;
		}

		if(get_mtd_info(mtd_dev, &mtd_size, &mtd_erasesize)) {
			goto validate_slots_size_end;
		}

		slot_size = (slots_size_left/(number_of_lmcs - i)) &
			~(mtd_erasesize - 1);

		if(slot_size != mtd_size) {
			TPT_ERROR(STR("%s size 0x%x is not equal to expected "
			              "size 0x%x.", sys_lmc_dev, mtd_size,
			               slot_size));
			goto validate_slots_size_end;
		}
		if (slots_size_left < slot_size) {
			TPT_ERROR(STR("%s size 0x%x exceeds size left 0x%x",
			              sys_lmc_dev, slot_size, slots_size_left));
			goto validate_slots_size_end;
		}
		slots_size_left -= slot_size;
	}
	ret = 0;
validate_slots_size_end:
	return ret;
}

static int get_flash_size(nvpi3_node_handle node_handle,
                          uint32_t *flash_size)
{
	char *sys_flash_size, *end;
	*flash_size = 0;

	sys_flash_size = getenv(FLASH_SIZE_ARG);
	if(sys_flash_size == NULL) {
		TPT_ERROR("SYS_FLASH_SIZE not found");
		return -1;
	}
	*flash_size = strtoul(sys_flash_size, &end, 0);
	if(*end != '\0') {
		TPT_ERROR(STR("SYS_FLASH_SIZE has invalid value %s",
		              sys_flash_size));
		return -1;
	}
	if(*flash_size == 0) {
		TPT_ERROR("Invalid zero value for SYS_FLASH_SIZE");
		return -1;
	}
	return 0;
}

static int lmc_check(nvpi3_node_handle node_handle,
                     uint32_t slots_size_left,
                     uint32_t auboot_size)
{
	uint32_t number_of_lmcs;
	uint32_t lmc_number_start = 0;
	if (lmc_number_check(node_handle, &number_of_lmcs)) {
		return -1;
	}
	if(auboot_size) {
		slots_size_left -= auboot_size;
		lmc_number_start++;
	}

	if(validate_slots_size(lmc_number_start,
	                       number_of_lmcs,
	                       slots_size_left)) {
		return -1;
	}
	return 0;
}

static int invalid_mtd_partition_check(void)
{
	char *value = getenv("sys_unused_dev");
	if(value != NULL) {
		TPT_ERROR("sys_unused_dev exists");
		return -1;
	}
	return 0;
}

static int bootenv_check(void)
{
	uint32_t uflash_size = 0;
	uint32_t auboot_size = 0;
	uint32_t flash_size = 0;
	uint32_t static_partition_size = 0;
	int ret = -1;
	nvpi3_db_group_handle group_handle = NULL;
	nvpi3_node_handle node_handle = NULL;
	nvpi3_result_t result;

	result = nvpi3_open_db_group(BOOTENV_GROUP_NAME, &group_handle);
	if(result != NVPI3_RESULT_SUCCESS) {
		TPT_ERROR(STR("open db group failed with %d", result));
		goto bootenv_check_end;
	}

	result = nvpi3_open_node(group_handle, "/current_bootenv/",
				 &node_handle);
	if(result != NVPI3_RESULT_SUCCESS) {
		TPT_ERROR(STR("open node failed with %d", result));
		goto bootenv_check_end;
	}

	if(get_flash_size(node_handle, &flash_size)) {
		goto bootenv_check_end;
	}

	if(check_auboot_size(node_handle, &auboot_size)) {
		goto bootenv_check_end;
	}

	if(user_flash_check(node_handle, &uflash_size)) {
		goto bootenv_check_end;
	}

	if(static_partition_check(&static_partition_size)) {
		goto bootenv_check_end;
	}

	if(lmc_check(node_handle, flash_size - uflash_size -
	             static_partition_size, auboot_size)) {
		goto bootenv_check_end;
	}

	if(invalid_mtd_partition_check()) {
		goto bootenv_check_end;
	}

	if(lock_slot_check(node_handle)) {
		goto bootenv_check_end;
	}

	if(hw_profile_check(node_handle)) {
		goto bootenv_check_end;
	}

	ret = 0;

bootenv_check_end:
	if(node_handle) {
		result = nvpi3_close_node(node_handle);
		if(result != NVPI3_RESULT_SUCCESS) {
			TPT_ERROR(STR("close node failed with %d", result));
			ret = -1;
		}
	}
	if(group_handle) {
		result = nvpi3_close_db_group(group_handle);
		if(result != NVPI3_RESULT_SUCCESS) {
			TPT_ERROR(STR("close db group failed with %d", result));
			ret = -1;
		}
	}
	return ret;
}

static void delete_itc(itc_mbox_id_t mbox)
{
	if( mbox != ITC_NO_ID) {
		itc_delete_mailbox(mbox);
	}
}

static void print_usage(void)
{
	printf("This program is checking bootenv settings.\n\n"
	       "Usage: bootenv_check <options>\n"
	       "Options:\n"
	       "    -h  Display usage information (this message).\n\n");
}

int main(int argc, char **argv)
{
	int ret = 0;
	itc_mbox_id_t main_mbox = ITC_NO_ID;

	if (argc > 1) {
		if (strcmp("-h", argv[1]) == 0) {
			print_usage();
			exit(0);
		} else {
			print_usage();
			exit(-EINVAL);
		}
	}

	if(init_itc(&main_mbox)) {
		return -1;
	}
	if( bootenv_check() || resetmem_clear_restart_in_boot_counter() ) {
		TPT_ERROR("Aborting without clearing restart in boot counter");
		ret = -1;
	}
	delete_itc(main_mbox);
	return ret;
}
