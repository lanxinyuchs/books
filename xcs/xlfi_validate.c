/* > Description **************************************************************/
/**
 * @file xlfi_validate.c
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
#include <arpa/inet.h>
#include "xlfi.h"
#include "xlfi_api.h"

/* > Defines ******************************************************************/

/* > Type Declarations ********************************************************/

/* > External Function Declarations *******************************************/

/* > Local Functions Declarations *********************************************/
static void get_file_info(struct xlfi_parse_state *state,
                          const struct xlf_iboot_header *head, uint8_t *p,
                          uint32_t size,
                          struct xlfi_file_info file[]);

static xlfi_result_t validate_header_file_fields(
	const struct xlf_iboot_header *head,
	struct xlfi_file_info file[],
	const char **error);

/* > Functions ****************************************************************/

xlfi_result_t xlfi_validate_header(struct xlfi_parse_state *state,
                                   const struct xlf_iboot_header *head,
                                   const char **error)
{
	xlfi_result_t result = XLFI_RESULT_CORRUPT;
	const char *error_descr = "";
	uint32_t min_wrapper_offset;
	uint32_t xpl_offset = ntohl(head->xpl_offset);
	uint32_t file_count = ntohl(head->file_count);
	uint32_t crc2_offset = ntohl(head->crc2_offset);

	if (head->magic != ntohl(XLF_IBOOT_HDR_MAGIC)) {
		error_descr = "Invalid xlf_iboot_header::magic";
		goto xlfi_validate_header_end;
	}

	if (head->type > ntohl(XLF_IBOOT_HDR_TYPE_BOOT_ENV)) {
		error_descr = "Invalid xlf_iboot_header::type";
		goto xlfi_validate_header_end;
	}

	if (head->major_version) {
		error_descr = "Invalid xlf_iboot_header::major_version";
		goto xlfi_validate_header_end;
	}

	if (head->minor_version) {
		error_descr = "Invalid xlf_iboot_header::minor_version";
		goto xlfi_validate_header_end;
	}

	if (xlfi_calc_checksum((uint8_t *) head,
	                       (uint8_t *) &head->crc1 - (uint8_t *) head,
	                       0) != ntohs(head->crc1)) {
		error_descr = "Invalid xlf_iboot_header::crc1";
		goto xlfi_validate_header_end;
	}

	if (xpl_offset) {
		if (xpl_offset <
		    (offsetof(struct xlf_iboot_header, file_offset) +
		     file_count * sizeof(head->file_offset[0])) ||
		    xpl_offset >= crc2_offset) {
			error_descr = "Invalid xlf_iboot_header::xpl_offset";
			goto xlfi_validate_header_end;
		}
		min_wrapper_offset = xpl_offset;
	} else {
		min_wrapper_offset =
		        offsetof(struct xlf_iboot_header, file_offset) +
		        file_count * sizeof(head->file_offset[0]);
	}

	if ((min_wrapper_offset + file_count *
	     (sizeof(struct xlf_iboot_wrapper) + 1)) > crc2_offset) {
		/* XLF to small to hold file_count files. */
		error_descr = "Invalid xlf_iboot_header::file_count";
		goto xlfi_validate_header_end;
	}

	state->offset = offsetof(struct xlf_iboot_header, file_offset);
	state->remaining = XLFI_GET_XLF_SIZE(head) - state->offset;
	state->file.index = UINT32_MAX;
	state->crc2 = xlfi_calc_checksum((uint8_t *) &head->file_count,
	                                 sizeof(head->file_count), 0);
	result = XLFI_RESULT_SUCCESS;

xlfi_validate_header_end:
	if (error != NULL) {
		*error = error_descr;
	}

	return result;
}

xlfi_result_t xlfi_validate(struct xlfi_parse_state *state,
                            const struct xlf_iboot_header *head, uint8_t *p,
                            uint32_t size, uint32_t *num_of_files,
                            struct xlfi_file_info file[], int *done,
                            const char **error)
{
	xlfi_result_t result = XLFI_RESULT_SUCCESS;
	const char *error_descr = "";

	*done = 1;

	if (state->offset < offsetof(struct xlf_iboot_header, file_offset)) {
		error_descr = "state->offset<xlf_iboot_header::file_offset";
		result = XLFI_RESULT_WRONG_STATE;
		goto xlfi_validate_end;
	}

	if (!state->remaining) {
		error_descr = "!state->remaining";
		result = XLFI_RESULT_WRONG_STATE;
		goto xlfi_validate_end;
	}


	if (size >= state->remaining) {
		size = state->remaining;
	} else if (size & 0x3) {
		/*
		 *  The size must be multiple of four bytes for fragments except
		 * the last one.
		 */
		error_descr = "size&0x3";
		result = XLFI_RESULT_INVALID_PARAM;
		goto xlfi_validate_end;
	}

	if (file != NULL && (state->file.index == UINT32_MAX ||
	                     state->file.index < ntohl(head->file_count))) {
		if (state->file.index == UINT32_MAX) {
			if (*num_of_files < ntohl(head->file_count)) {
				*num_of_files = ntohl(head->file_count);
				error_descr = "*num_of_files<file_count";
				result = XLFI_RESULT_NOT_ENOUGH_SPACE;
				goto xlfi_validate_end;
			}
		}

		get_file_info(state, head, p, size, file);
	}

	state->offset += size;
	state->remaining -= size;

	if (state->remaining  <= sizeof(((struct xlf_iboot_footer *)0)->crc2)) {
		size -= sizeof(((struct xlf_iboot_footer *)0)->crc2);
	}

	state->crc2 = xlfi_calc_checksum(p, size, state->crc2);

	if (!state->remaining) {
		if (state->crc2 != ntohs(*((uint16_t *)(p + size)))) {
			result = XLFI_RESULT_CORRUPT;
			error_descr = "Invalid xlf_iboot_header::crc2";
			goto xlfi_validate_end;
		}

		if (file != NULL) {
			result = validate_header_file_fields(head, file,
			                                     &error_descr);
			if (result != XLFI_RESULT_SUCCESS) {
				goto xlfi_validate_end;
			}
			*num_of_files = ntohl(head->file_count);
		}
	} else {
		*done = 0;
	}


xlfi_validate_end:
	if (error != NULL) {
		*error = error_descr;
	}

	return result;
}

uint16_t xlfi_calc_checksum(uint8_t *p, uint32_t size, uint16_t previous_sum)
{
	uint32_t sum = previous_sum;

	while(size--) {
		sum += (uint32_t) * p++;
		if(sum > 0xffff) {
			sum++;
			sum &= 0xffff;
		}
	}
	return (uint16_t) sum;
}

static xlfi_result_t validate_header_file_fields(
	const struct xlf_iboot_header *head,
	struct xlfi_file_info file[],
	const char **error)
{
	uint32_t i, j, min_file_offset;
	uint32_t file_count = ntohl(head->file_count);
	uint32_t crc2_offset = ntohl(head->crc2_offset);

	min_file_offset = (head->xpl_offset) ?
	                  ntohl(head->xpl_offset) :
	                  offsetof(struct xlf_iboot_header, file_offset) +
	                  file_count * sizeof(head->file_offset[0]);

	min_file_offset += sizeof(struct xlf_iboot_wrapper);

	for (i = 0; i < file_count; i++) {
		if (file[i].offset < min_file_offset ||
		    file[i].offset + file[i].size > crc2_offset) {
			/* file offset out of range. */
			*error = "file[i].offset out of range";
			goto validate_header_file_fields_error;
		} else if (!file[i].size) {
			/* Zero file file_offset. */
			*error = "!file[i].size";
			goto validate_header_file_fields_error;
		}

		for (j = i; j < file_count; j++) {
			if (i == j) {
				continue;
			}

			if ((file[i].offset < file[j].offset &&
			     (file[j].offset - file[i].offset <
			      file[i].size +
			      sizeof(struct xlf_iboot_wrapper))) ||
			    (file[i].offset >= file[j].offset &&
			     (file[i].offset - file[j].offset <
			      file[j].size + sizeof(struct xlf_iboot_wrapper)))) {
				/* file offset overlaps. */
				*error = "file[i].offset overlaps";
				goto validate_header_file_fields_error;
			}
		}
	}

	return XLFI_RESULT_SUCCESS;

validate_header_file_fields_error:
	return XLFI_RESULT_CORRUPT;
}

static void file_info_to_host(struct xlfi_file_info *file)
{
	if (file->info.magic == XLF_IBOOT_WPR_MAGIC_BLOB) {
		file->info.blob.header.crc32 =
		        ntohl(file->info.blob.header.crc32);
		file->info.blob.header.header_size =
		        ntohl(file->info.blob.header.header_size);
		file->info.blob.header.type =
		        ntohl(file->info.blob.header.type);
		file->info.blob.header.major_version =
		        ntohl(file->info.blob.header.major_version);
		file->info.blob.header.minor_version =
		        ntohl(file->info.blob.header.minor_version);
		file->info.blob.header.time =
		        ntohl(file->info.blob.header.time);
	}
}

static void get_file_info(struct xlfi_parse_state *state,
                          const struct xlf_iboot_header *head, uint8_t *p,
                          uint32_t size,
                          struct xlfi_file_info file[])
{
	uint32_t offset, end;
	uint32_t file_count = ntohl(head->file_count);

	if (state->file.index == UINT32_MAX) {
		uint8_t *q = p;

		if (state->offset <=
		    offsetof(struct xlf_iboot_header, file_offset) +
		    file_count * sizeof(head->file_offset[0])) {
			uint32_t i =
			        (state->offset -
			         offsetof(struct xlf_iboot_header,
			                  file_offset)) /
			        sizeof(head->file_offset[0]);
			uint32_t count =
			        i + (size / sizeof(head->file_offset[0]));

			if (count > file_count) {
				count = file_count;
			}

			while (i < count) {
				/*
				 * Set file[i].offset to point to
				 * xlf_iboot_wrapper.
				 */
				file[i].offset = *q++ << 24;
				file[i].offset |= (*q++ << 16);
				file[i].offset |= (*q++ << 8);
				file[i].offset |= *q++;
				file[i].offset -=
				        offsetof(struct xlf_iboot_wrapper, magic);
				file[i].size = 0;
				i++;
			}

			if (count == file_count) {
				state->file.index = 0;
			}
		} else {
			state->file.index = 0;
		}
	}

	offset = file[state->file.index].offset;
	end = state->offset + size;

	while (state->file.index < file_count &&
	       file[state->file.index].offset < end &&
	       offset < end) {

		uint8_t *q = p + offset - state->offset;

		if (!file[state->file.index].size) {
			uint32_t end_offset;

			end_offset = *q++ << 24;
			end_offset |= (*q++ << 16);
			end_offset |= (*q++ << 8);
			end_offset |= *q++;
			file[state->file.index].size =
			        end_offset - sizeof(struct xlf_iboot_wrapper) +
			        1;
			file[state->file.index].offset += 4;
			offset += 4;
			state->file.remaining = 0;
		} else if (state->file.remaining) {
			uint8_t *r = (uint8_t *)
			             &file[state->file.index].info.magic +
			             sizeof(file[state->file.index].info.magic) +
			             state->file.offset;
			*r = *q++;
			state->file.offset++;
			state->file.remaining--;
			offset++;

			if (!state->file.remaining) {
				file_info_to_host(&file[state->file.index]);
				state->file.index++;
				offset = file[state->file.index].offset;
			}
		} else {
			file[state->file.index].info.magic = *q++ << 24;
			file[state->file.index].info.magic |= (*q++ << 16);
			file[state->file.index].info.magic |= (*q++ << 8);
			file[state->file.index].info.magic |= *q++;
			file[state->file.index].offset += 4;
			if (file[state->file.index].info.magic ==
			    XLF_IBOOT_WPR_MAGIC_BLOB) {
				state->file.offset = 0;
				state->file.remaining =
				        sizeof(struct xlf_blob_header);
				offset += 4;
			} else {
				state->file.index++;
				offset = file[state->file.index].offset;
			}
		}
	}
}

uint32_t xlfi_suid2lmid(char lmid[XLF_SUID_LEN + 2], const char *suid)
{
	uint32_t i;
	char *p = lmid;

	if (*suid != '\0') {
		for (i = 0; i < XLF_SUID_ID_LEN; i++) {
			if (suid[i] > ' ') {
				*p++ = suid[i];
			}
		}

		*p++ = '_';

		for (i = XLF_SUID_ID_LEN;
		     i < XLF_SUID_ID_LEN + XLF_SUID_REV_LEN;
		     i++) {
			if (suid[i] > ' ') {
				*p++ = suid[i];
			}
		}
	}
	*p = '\0';
	return p - lmid;
}
