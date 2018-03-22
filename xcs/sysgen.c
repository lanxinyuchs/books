/* ---------------------------------------------------------------------------
 *
 * © Ericsson AB 2015 All rights reserved.
 * The information in this document is the property of Ericsson.  Except
 * as specifically authorized in writing by Ericsson, the receiver of
 * this document shall keep the information contained herein confidential
 * and shall protect the same in whole or in part from disclosure and
 * dissemination to third parties.  Disclosure and disseminations to the
 * receivers employees shall only be made on a strict need to know basis.
 *
 * ---------------------------------------------------------------------------
 */

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include <unistd.h>
#include <stdbool.h>
#include <string.h>
#include <uio_helper.h>
#include "booti_restart.h"

#define UIO_MISC_BLOCK_NAME         "MISC"
#define MISC_HW_ID_OFFSET           0
#define MISC_RESET_MEM_0_OFFSET     0x14
#define MISC_DEBUG_OFFSET           0x24

#define MISC_WD_DIS_MASK            0x01000000

struct uio_map {
	UIO_HANDLE_ handle;
	void *base;
} uio_map = {NULL, NULL};


static void print_usage(void)
{
	printf("Usage: sysgen [OPTION] <scriptfilename>\n\n"
	       "<scriptfilename> is the filename where this program put SYS_BOOT_MODE,"
	       " SYS_HW_TYPE, SYS_DEBUG, SYS_CUR_SLOT environment variables to\n"
	       "This program is:\n"
	       "    - Populates SYS_BOOT_MODE, SYS_HW_TYPE,"
	       " SYS_DEBUG, SYS_CUR_SLOT environment variables\n"
	       "OPTION:\n"
	       "   -h                Display usage information (this message).\n");
}

static void undo_uio_map(void)
{
	uio_close(uio_map.handle);
	uio_map.handle = NULL;
	uio_map.base = NULL;
}

static bool do_uio_map(void)
{
	UIO_HANDLE_ uio_handle = NULL;
	void *mmap_base = NULL;

	uio_handle = uio_open(UIO_MISC_BLOCK_NAME);
	if (uio_handle == UIO_OPEN_FAILED) {
		fprintf(stderr, "%s: Failed to open uio with error %d",
		        __func__, errno);
		goto do_uio_mmap_error;
	}

	mmap_base = uio_mmap(uio_handle);

	if (mmap_base == MAP_FAILED) {
		fprintf(stderr, "%s: Failed to peform UIO memory mapping",
		        __func__);
		goto do_uio_mmap_error;
	}

	uio_map.handle = uio_handle;
	uio_map.base = mmap_base;
	return true;

do_uio_mmap_error:
	uio_close(uio_handle);

	return false;
}

static void set_env_variable(char *shell_script_filename)
{
	FILE *out = fopen(shell_script_filename, "w");
	if (!out) {
		fprintf(stderr, "Failed to open file %s for writing\n", shell_script_filename);
		return;
	}

	if(*(volatile uint32_t *)(uio_map.base + MISC_HW_ID_OFFSET) == 0x10191451) {
		fprintf(out, "export SYS_HW_TYPE=XENON1.0\n");
	}

	if((*(volatile uint32_t *)(uio_map.base + MISC_DEBUG_OFFSET)) &
	    MISC_WD_DIS_MASK) {
		fprintf(out, "export SYS_DEBUG=YES\n");
	} else {
		fprintf(out, "export SYS_DEBUG=NO\n");
	}

	if((*(volatile uint32_t *)(uio_map.base + MISC_RESET_MEM_0_OFFSET)) &
	    BOOTI_RESTART_USE_LATEST) {
		fprintf(out, "export SYS_BOOT_MODE=LATEST\n");
	} else {
		fprintf(out, "export SYS_BOOT_MODE=WORKING\n");
	}

	fprintf(out, "export SYS_CUR_SLOT=%d\n",
	        (*(volatile uint32_t *)(uio_map.base + MISC_RESET_MEM_0_OFFSET)) &
	        BOOTI_RESTART_LMC_MASK);

	fclose(out);
}


int main(int argc, char *argv[])
{
	if (argc != 2) {
		print_usage();
		exit(-EINVAL);
	}
	if(!strcmp(argv[1], "-h")) {
		print_usage();
		exit(-EINVAL);
	}


	if(do_uio_map() == false) {
		exit(-EFAULT);
	}

	set_env_variable(argv[1]);
	undo_uio_map();
	exit(0);
}
