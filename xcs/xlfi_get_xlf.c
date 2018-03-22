/* > Description **************************************************************/
/**
 * @file xlfi_get_xlf.c
 * @brief XLF interface functions
 *
 * This file contains XLF interface functions.
 *
 */

/*
 * Copyright (C) 2017 by Ericsson AB. All rights reserved. The
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

#include <stdint.h>
#include <stddef.h>

#include "xlfi_api.h"

/* > Defines ******************************************************************/

/* AXM5612 Secure Boot Block header size */
#define AXM5612_SBB_HDR_SIZE             12

/* ZynqMP Boot Header size and field offsets in words */
#define ZYNQMP_BH_WSIZE          (0x8c0 / 4)
#define ZYNQMP_BH_WORD_ID                 9

/* ZynqMP Image Header size and field offsets in words */
#define ZYNQMP_IH_WSIZE          (0x40 / 4)
#define ZYNQMP_IH_WORD_PHOFFSET          2
#define ZYNQMP_IH_WORD_CHKSUM           15

/* ZynqMP Partition Header size and field offsets in words */
#define ZYNQMP_PH_WSIZE          (0x40 / 4)
#define ZYNQMP_PH_WORD_POFFSET           8
#define ZYNQMP_PH_WORD_CHKSUM           15


/* > Type Declarations ********************************************************/

/* > External Function Declarations *******************************************/

/* > Local Functions Declarations *********************************************/

/* > Functions ****************************************************************/

static xlfi_result_t get_xlf_from_sxlf_axm5612(xlfi_read_image read_image,
                                               void *object,
                                               uint32_t size,
                                               uint32_t *offset,
                                               const char **error)
{
	const char *error_str;
	char        sbb[4];

	if (size <= AXM5612_SBB_HDR_SIZE) {
		error_str = "Image too small to contain XLF";
		goto return_err;
	}

	if (read_image(object, sbb, sizeof(sbb), 0)) {
		error_str = "Read failed";
		goto return_err;
	}
	if (sbb[0] != 'S' || sbb[1] != 'B' || sbb[2] != 'B' || sbb[3] != '!') {
		error_str = "Not an AXM5612 SBB header";
		goto return_err;
	}

	*offset = AXM5612_SBB_HDR_SIZE;
	return XLFI_RESULT_SUCCESS;

return_err:
	if (error != NULL) {
		*error = error_str;
	}
	return XLFI_RESULT_OTHER_ERROR;
}

static xlfi_result_t get_xlf_from_sxlf_zynqmp(xlfi_read_image read_image,
                                              void *object,
                                              uint32_t size,
                                              uint32_t *offset,
                                              const char **error)
{
	const char *error_str = NULL;
	uint32_t    image_hdr, partition_hdr, partition;
	uint32_t    sum, idx, tmp;
	char        id[4];

	/*
	 *   Boot Header
	 *   Image Header -> { Partition Header #1 word offset }
	 *   Partition Header #1 -> { Partition #1 word offset }
	 *   Header Authentication Certificate
	 *   Partition #1
	 *   Partition #1 Authentication Certificate
	 */

	if (size <= 4 * (ZYNQMP_BH_WSIZE + ZYNQMP_IH_WSIZE + ZYNQMP_PH_WSIZE)) {
		error_str = "Image too small to contain XLF";
		goto return_err;
	}

	/* Check the Boot Header image identification field */
	if (read_image(object, id, sizeof(id), 4 * ZYNQMP_BH_WORD_ID)) {
		error_str = "Read failed";
		goto return_err;
	}
	if (id[0] != 'X' || id[1] != 'N' || id[2] != 'L' || id[3] != 'X') {
		error_str = "Invalid boot image identifier";
		goto return_err;
	}

	/* Image Header follows on the Boot Header */
	image_hdr = 4 * ZYNQMP_BH_WSIZE;

	/* Check Image Header checksum */
	for (sum = idx = 0; idx < ZYNQMP_IH_WORD_CHKSUM; idx++) {
		if (read_image(object, &tmp, sizeof(tmp),
		               image_hdr + 4 * idx)) {
			error_str = "Read failed";
			goto return_err;
		}
		sum += tmp;
	}
	if (read_image(object, &tmp, sizeof(tmp),
	               image_hdr + 4 * ZYNQMP_IH_WORD_CHKSUM)) {
		error_str = "Read failed";
		goto return_err;
	}
	if (tmp != ~sum) {
		error_str = "Incorrect image header checksum";
		goto return_err;
	}

	/* Sanity check the 1:st partition header word offset  */
	if (read_image(object, &partition_hdr, sizeof(partition_hdr),
	               image_hdr + 4 * ZYNQMP_IH_WORD_PHOFFSET)) {
		error_str = "Read failed";
		goto return_err;
	}
	if (partition_hdr > size / 4) {
		error_str = "Invalid 1:st partition header word offset";
		goto return_err;
	}

	/* 1:st Partition Header word offset */
	partition_hdr *= 4;

	/* Check 1:st Partition Header checksum */
	for (sum = idx = 0; idx < ZYNQMP_PH_WORD_CHKSUM; idx++) {
		if (read_image(object, &tmp, sizeof(tmp),
		               partition_hdr + 4 * idx)) {
			error_str = "Read failed";
			goto return_err;
		}
		sum += tmp;
	}
	if (read_image(object, &tmp, sizeof(tmp),
	               partition_hdr + 4 * ZYNQMP_PH_WORD_CHKSUM)) {
		error_str = "Read failed";
		goto return_err;
	}
	if (tmp != ~sum) {
		error_str = "Incorrect 1:st partition header checksum";
		goto return_err;
	}

	/* Sanity check the 1:st partition word offset  */
	if (read_image(object, &partition, sizeof(partition),
	               partition_hdr + 4 * ZYNQMP_PH_WORD_POFFSET)) {
		error_str = "Read failed";
		goto return_err;
	}
	if (partition > size / 4) {
		error_str = "Invalid partition word offset";
		goto return_err;
	}
	partition *= 4;

	/* The XLF is at partition word offset from Boot Header. */
	*offset = partition;
	return XLFI_RESULT_SUCCESS;

return_err:
	if (error != NULL) {
		*error = error_str;
	}

	return XLFI_RESULT_OTHER_ERROR;
}

xlfi_result_t xlfi_get_xlf(xlfi_read_image read_image, void *object,
                           uint32_t size, int *image_type,
                           uint32_t *offset, const char **error)
{
	const char *error_str;

	if (image_type != NULL) {
		if (*image_type == XLFI_IMAGE_TYPE_AUTODETECT) {
			error_str = "Autodetect not implemented";
		} else if (*image_type == XLFI_IMAGE_TYPE_XLF) {
			*offset = 0;
			return XLFI_RESULT_SUCCESS;
		} else if (*image_type == XLFI_IMAGE_TYPE_SXLF_AXM5612) {
			return get_xlf_from_sxlf_axm5612(read_image, object,
			                                 size, offset, error);
		} else if (*image_type == XLFI_IMAGE_TYPE_SXLF_ZYNQMP) {
			return get_xlf_from_sxlf_zynqmp(read_image, object,
			                                size, offset, error);
		} else {
			error_str = "Invalid image_type";
		}
	} else {
		error_str = "image_type == NULL";
	}

	if (error != NULL) {
		*error = error_str;
	}

	return XLFI_RESULT_INVALID_PARAM;
}
