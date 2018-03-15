/* > Description **************************************************************/
/**
 * @file xlfi_get_max_size.c
 * @brief XLF interface functions
 *
 * This file contains XLF interface functions.
 *
 */

/*
 * Copyright (C) 2005 by Ericsson AB. All rights reserved. The
 * information in this document is the property of Ericsson. Except
 * as specifically authorized in writing by Ericsson, the receiver
 * of this document shall keep the information contained herein
 * confidential and shall protect the same in whole or in part from
 * disclosure and dissemination to third parties. Disclosure and
 * disseminations to the receiver's employees shall only be made on
 * a strict need to know basis.
 *
 ******************************************************************************/

/* > Includes *****************************************************************/
#include <stddef.h>
#include "xlfi.h"
#include "xlfi_api.h"


/* > Defines ******************************************************************/

#define MEMBER_SIZE(type, member)         sizeof(((type*)0)->member)

/* > Type Declarations ********************************************************/

/* > External Function Declarations *******************************************/

/* > Local Functions Declarations *********************************************/

/* > Functions ****************************************************************/

static xlfi_result_t xlfi_get_free_space(uint32_t xlf_size, uint32_t xpl_size,
                                         uint32_t num_of_files,
                                         const struct xlfi_file_info file[],
                                         uint32_t *free_space,
                                         const char **error);

xlfi_result_t xlfi_get_max_file_size(uint32_t xlf_size, uint32_t xpl_size,
                                     uint32_t file_index, uint32_t num_of_files,
                                     struct xlfi_file_info file[],
                                     const char **error)
{
	xlfi_result_t result;

	if (file_index > num_of_files) {
		if (error != NULL) {
			*error = "file_index > num_of_files";
		}
		return XLFI_RESULT_INVALID_PARAM;
	}

	file[file_index].size = 0;
	result =  xlfi_get_free_space(xlf_size, xpl_size, num_of_files, file,
	                              &file[file_index].size, error);
	if (result == XLFI_RESULT_SUCCESS) {
		file[file_index].size &= ~3;
	}
	return result;
}

xlfi_result_t xlfi_get_max_xpl_size(uint32_t xlf_size,
                                    uint32_t num_of_files,
                                    const struct xlfi_file_info file[],
                                    uint32_t *xpl_size,
                                    const char **error)
{
	return xlfi_get_free_space(xlf_size, 0, num_of_files, file, xpl_size,
	                           error);
}

static  xlfi_result_t xlfi_get_free_space(uint32_t xlf_size, uint32_t xpl_size,
                                          uint32_t num_of_files,
                                          const struct xlfi_file_info file[],
                                          uint32_t *free_space,
                                          const char **error)
{
	uint32_t i, size;

	size = (offsetof(struct xlf_iboot_header, file_offset) +
		MEMBER_SIZE(struct xlf_iboot_header, file_offset[0]) *
		num_of_files + xpl_size + 3) & ~3;

	for (i = 0; i < num_of_files; i++) {
		size += ((file[i].size + 3) & ~3);
	}

	size += sizeof(struct xlf_iboot_footer);

	if (size > xlf_size) {
		if (error != NULL) {
			*error = "size > xlf_size";
		}
		return XLFI_RESULT_NOT_ENOUGH_SPACE;
	}

	*free_space = xlf_size - size;
	return XLFI_RESULT_SUCCESS;
}
