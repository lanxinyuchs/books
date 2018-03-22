/* > Description **************************************************************/
/**
 * @file xlfi_api.h
 * @brief XLF API interface
 *
 * This file defines the XLF API interface.
 *
 * @section xlf_checksum XLF Checksum
 * The checksum fields xlf_iboot_header::crc1 and xlf_iboot_footer::crc2
 * shall be calculated as:
 * @verbatim
uint16_t calc_checksum(uint8_t *p, uint32_t size)
{
        uint32_t sum = 0;
        while(size--)
        {
                sum += (uint32_t) *p++;
                if(sum > 0xffff;
                {
                        sum++;
                        sum &= 0xffff;
                }
        }
        return (uint16_t) sum;
}
@endverbatim
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

#ifdef __cplusplus
extern "C" {
#endif

#ifndef XLFI_API_H
#define XLFI_API_H

/* > Includes *****************************************************************/
#include <stdint.h>
#include "xlfi.h"

/* > Defines ******************************************************************/

/**
 * Returned @ref xlfi_result_t value when the operation completed successfully.
 */
#define XLFI_RESULT_SUCCESS                 0

/** Returned @ref xlfi_result_t value when invalid parameter supplied. */
#define XLFI_RESULT_INVALID_PARAM           1

/** Returned when the operation requested during wrong state. */
#define XLFI_RESULT_WRONG_STATE             2

/** Returned @ref xlfi_result_t value when XLF is corrupt. */
#define XLFI_RESULT_CORRUPT                 3

/* Not enough space to perform operation. */
#define XLFI_RESULT_NOT_ENOUGH_SPACE        4

/** Returned @ref xlfi_result_t value for other miscellaneous errors. */
#define XLFI_RESULT_OTHER_ERROR             5


/**
 * For use with @ref xlfi_get_xlf as the image_type argument.
 */

/** For autodetecting image headers (not implemented) */
#define XLFI_IMAGE_TYPE_AUTODETECT          0

/** Image header is XLF */
#define XLFI_IMAGE_TYPE_XLF                 1

/** Image header is AXM5612 secure boot block header */
#define XLFI_IMAGE_TYPE_SXLF_AXM5612        2

/** Image header is ZYNQMP boot image header */
#define XLFI_IMAGE_TYPE_SXLF_ZYNQMP         3


/* > Type Declarations ********************************************************/

/** @brief Parsing state information. */
struct xlfi_parse_state {
	/** Offset where parsing should start from. */
	uint32_t offset;
	/** Remaining number of bytes to parse. */
	uint32_t remaining;
	struct {
		/** Current file index, (UINT32_MAX) value reserved. */
		uint32_t index;
		/** Offset in file header. */
		uint32_t offset;
		/** Remaining number of bytes to parse of file header. */
		uint32_t remaining;
	} file;
	/** Checksum, see @ref xlf_checksum for checksum */
	uint16_t crc2;
};

/** @brief XLF LM list. */
struct xlfi_file_info {
	/**
	 * Offset from xlf_iboot_header::magic to first byte of LM, i.e. the
	 * postion after xlf_iboot_wrapper::magic.
	 */
	uint32_t offset;
	/** unpadded size of LM. */
	uint32_t size;
	union {
		/** Type of LM, see xlf_iboot_wrapper::magic. */
		uint32_t magic;
		struct {
			/** Type of LM, see xlf_iboot_wrapper::magic. */
			uint32_t magic;
			struct xlf_blob_header header;
		} blob;
		struct {
			/** Type of LM, see xlf_iboot_wrapper::magic. */
			uint32_t magic;
		} user;
	} info;
};

/**
 * @brief Callback for reading from image
 *
 * @param[in] object  Pointer to object.
 * @param[in] dst     Pointer to where to read to.
 * @param[in] size    Size of data to read.
 * @param[in] offset  Offset from where to start read.
 *
 * @return  0 on read OK. Other value means the read failed.
 */

typedef int (*xlfi_read_image)(void *object,
                               void *dst, uint32_t size, uint32_t offset);


/* > Function Declarations ****************************************************/

/**
 * @brief Validates an XLF header.
 *
 * @param[out] state  Returned initial parse state.
 * @param[in]  head   XLF IBOOT header.
 * @param[out] error  Pointer to optional returned string containing error
 *                    description on failure. Should be set to NULL if error
 *                    description should not be returned.
 *
 * @return  @ref XLFI_RESULT_SUCCESS  XLF header is valid@n
 *          @ref XLFI_RESULT_CORRUPT  XLF header is invalid@n
 */
xlfi_result_t xlfi_validate_header(struct xlfi_parse_state *state,
                                   const struct xlf_iboot_header *head,
                                   const char **error);

/**
 * @brief Validates an XLF excluding the header.
 *
 * @param[in,out] state         Parse state.
 * @param[in]     head          XLF IBOOT header.
 * @param[in]     p             Buffer containing XLF fragment.
 * @param[in]     size          Size of XLF fragment. The size must be multiple
 *                              of four bytes for fragments except the last one.
 * @param[in,out] num_of_files  Pointer to variable that the caller should set
 *                              to the number of elements that can be stored in
 *                              file array. This function will set the variable
 *                              to the actual number of elements stored in file
 *                              array.
 *                              Should be set to NULL when file parameter is
 *                              NULL.
 * @param[out     file          Optional array that enables validation of
 *                              LMs and where LM info will be returned.
 *                              Should be set to NULL validation and return of
 *                              info should not be done.
 * @param[out]     done         Returned variable that is non zero when complete
 *                              XLF has been validated.
 * @param[out]     error        Pointer to optional returned string containing
 *                              error description on failure. Should be set to
 *                              NULL if error description should not be
 *                              returned.
 *
 * @return  @ref XLFI_RESULT_SUCCESS           XLF fragment is valid@n
 *          @ref XLFI_RESULT_INVALID_PARAM     Size is not a multiple of four
 *                                             bytes.@n
 *          @ref XLFI_RESULT_WRONG_STATE       Header has not been validated,
 *                                             validation is already completed.@n
 *          @ref XLFI_RESULT_CORRUPT           XLF header is invalid@n
 *          @ref XLFI_RESULT_NOT_ENOUGH_SPACE  file is not NULL and the number
 *                                             of LMs exceeds num_of_files.
 *
 * @pre           Header must have been validated via @ref xlfi_validate_header.
 * @see           xlfi_validate_header
 */
xlfi_result_t xlfi_validate(struct xlfi_parse_state *state,
                            const struct xlf_iboot_header *head, uint8_t *p,
                            uint32_t size, uint32_t *num_of_files,
                            struct xlfi_file_info file[], int *done,
                            const char **error);

/**
 * @brief Calulates the checksum of a fragment.
 *
 *  See @ref xlf_checksum for checksum algorithm.
 *
 * @param[in] p             Buffer containing XLF fragment.
 * @param[in] size          Size of XLF fragment.
 * @param[in] previous_sum  Checksum returned from previous call.
 *
 * @return  Checksum of the fragment.
 */
uint16_t xlfi_calc_checksum(uint8_t *p, uint32_t size, uint16_t previous_sum);

/**
 * @brief Converts SW Product ID and revision field to lmid format, which
 *        means renmoving spaces, adding '_' between ID and revision and a
 *        terminatin NULL
 *
 * @param[out] lmid  SW Product ID and revision field in lmid format.
 * @param[in]  suid  SW Product ID and revision field.
 *
 * @return The length of SW Product ID and revision field.
 */
uint32_t xlfi_suid2lmid(char lmid[XLF_SUID_LEN + 2], const char *suid);

/**
 * @brief Returns maximum size of an LM.
 *
 * @param[in]     xlf_size      XLF size in bytes.
 * @param[in]     xpl_size      Size in bytes of xpl to be included in XLF.
 *                              Should be set to zero if XLF does not contain
 *                              xpl.
 * @param[in,out] file_index    Index of LM in file whose max size should be
 *                              returned.
 * @param[in]     num_of_files  The number of elements in file array.
 * @param[in]     file          Array of LM info.
 *                              file[file_index].size is set to max size of LM.
 * @param[out]    error         Pointer to optional returned string containing
 *                              error description on failure. Should be set to
 *                              NULL if error description should not be
 *                              returned.
 *
 * @return  @ref XLFI_RESULT_SUCCESS           Operation succeeded.
 *          @ref XLFI_RESULT_INVALID_PARAM     Invalid file info supplied or
 *                                             file_index exceeds num_of_files.
 *          @ref XLFI_RESULT_NOT_ENOUGH_SPACE  xlf_size is too small.
 */
xlfi_result_t xlfi_get_max_file_size(uint32_t xlf_size, uint32_t xpl_size,
                                     uint32_t file_index, uint32_t num_of_files,
                                     struct xlfi_file_info file[],
                                     const char **error);

/**
 * @brief Returns maximum size of the xpl.
 *
 * @param[in] xlf_size      XLF size in bytes.
 * @param[in] num_of_files  The number of elements in file array.
 * @param[in] file          Array of LM info.
 *                          Should be set to NULL if XLF only contains xpl.
 * @param[out] xpl_size     Returned max size of xpl.
 * @param[out] error        Pointer to optional returned string containing error
 *                          description on failure. Should be set to NULL if
 *                          error description should not be returned.
 *
 * @return  @ref XLFI_RESULT_SUCCESS           Operation succeeded.
 *          @ref XLFI_RESULT_INVALID_PARAM     Invalid file info supplied.
 *          @ref XLFI_RESULT_NOT_ENOUGH_SPACE  xlf_size is too small.
 */
xlfi_result_t xlfi_get_max_xpl_size(uint32_t xlf_size, uint32_t num_of_files,
                                    const struct xlfi_file_info file[],
                                    uint32_t *xpl_size, const char **error);

/**
 * @brief Returns offset to the XLF in an image
 *
 * @param[in] read_image     Function for reading from image
 * @param[in] object         Pointer to object passed to read_image
 * @param[in] size           Size of the image
 * @param[in,out] image_type Pointer to image type XLFI_IMAGE_TYPE_...
 *                           If the image type is set to autodetect then this
 *                           pointer will be used for returning the detected
 *                           image type.
 * @param[out] offset        Byte offset to XLF in the image
 * @param[in,out] error      Pointer to optional returned string containing
 *                           error description on failure. Set to NULL if error
 *                           description is not to be returned.
 *
 * @return  @ref XLFI_RESULT_SUCCESS           Operation succeeded.
 *          @ref XLFI_RESULT_INVALID_PARAM     Invalid parameter supplied.
 *          @ref XLFI_RESULT_OTHER_ERROR       Image could not be parsed.
 */

xlfi_result_t xlfi_get_xlf(xlfi_read_image read_image, void *object,
                           uint32_t size, int *image_type,
                           uint32_t *offset, const char **error);

#endif /* XLFI_API_H */

#ifdef __cplusplus
}
#endif
