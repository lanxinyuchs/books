/*****************************************************************************/
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

#ifndef XLFI_H
#define XLFI_H

/* > Includes *****************************************************************/
#include <stdint.h>

/* > Defines ******************************************************************/

/** xlf_iboot_header::magic value for an XLF. */
#define XLF_IBOOT_HDR_MAGIC                 0x58504C46

/** xlf_iboot_header::type value for an XLF that contains an AU boot. */
#define XLF_IBOOT_HDR_TYPE_AU_BOOT          0

/** xlf_iboot_header::type value for an XLF that contains an AU application. */
#define XLF_IBOOT_HDR_TYPE_AU_APPLIC        1

/** xlf_iboot_header::type value for an XLF that contains an boot. */
#define XLF_IBOOT_HDR_TYPE_BOOT             2

/**
 * xlf_iboot_header::type value for an XLF that contains a Linux AU boot.
 * The XLF contains a Linux kernel, dtb and rootfs. A more flexible way than
 * using this type is to is to use type AU_BOOT
 * (@ref XLF_IBOOT_HDR_TYPE_AU_BOOT) and package the Linux kernel, dtb and
 * rootfs as BLOB LMs (@ref XLF_IBOOT_WPR_MAGIC_BLOB).
 */
#define XLF_IBOOT_HDR_TYPE_AU_BOOT_LINUX    3

/**
 * xlf_iboot_header::type value for an XLF that contains a Linux AU application.
 * The XLF contains a Linux kernel, dtb and rootfs. A more flexible way than
 * using this type is to is to use type AU_APPLIC
 * (@ref XLF_IBOOT_HDR_TYPE_AU_APPLIC) and package the Linux kernel, dtb and
 * rootfs as BLOB LMs (@ref XLF_IBOOT_WPR_MAGIC_BLOB).
 */
#define XLF_IBOOT_HDR_TYPE_AU_APPLIC_LINUX  4

/**
 * xlf_iboot_header::type value for an XLF that contains AU boot/application
 * environment.
 */
#define XLF_IBOOT_HDR_TYPE_AU_ENV           5

/**
 * xlf_iboot_header::type value for an XLF that contains boot environment.
 */
#define XLF_IBOOT_HDR_TYPE_BOOT_ENV         6


/**
 * Bit in xlf_iboot_header::update that indicates working status of XLF.
 * A cleared bit indicates that XLF is working.
 */
#define XLF_IBOOT_HDR_UPDATE_WORKING_BIT    0x00000001


/**
 * Length of SW Product ID and revision field, see xlf_iboot_header::suid and
 * xlf_blob_header::suid fields.
 */
#define XLF_SUID_LEN                        32

/**
 * Length of ID part of SW Product ID and revision field, see
 * xlf_iboot_header::suid and xlf_blob_header::suid fields.
 */
#define XLF_SUID_ID_LEN                     23

/**
 * Length of revision part of SW Product ID and revision field, see
 * xlf_iboot_header::suid and xlf_blob_header::suid fields.
 */
#define XLF_SUID_REV_LEN                    9

/** xlf_iboot_wrapper::magic value for a RPDOUT LM. */
#define XLF_IBOOT_WPR_MAGIC_RPDOUT          0x5250444f

/** xlf_iboot_wrapper::magic value for a BLOB LM. */
#define XLF_IBOOT_WPR_MAGIC_BLOB            0x424c4f42

/**
 * xlf_blob_header::type value for a BLOB LM that contains an U-Boot environment
 * image.
 */
#define XLF_BLOB_TYPE_UENVIMAGE 0

/**
 * xlf_blob_header::type value for a BLOB LM that contains a Device Tree Blob.
 */
#define XLF_BLOB_TYPE_DTB       1

/**
 * xlf_blob_header::type value for a BLOB LM that contains a Linux root
 * filesystem.
 */
#define XLF_BLOB_TYPE_ROOTFS    2

/**
 * xlf_blob_header::type value for a BLOB LM that contains a Linux kernel in
 * U-Boot Image format.
 */
#define XLF_BLOB_TYPE_UIMAGE    3

/**
 * xlf_blob_header::type value for a BLOB LM that contains an ASCII Database.
 */
#define XLF_BLOB_TYPE_ASCII_DB  4

/** Length of the blob name string including terminating null */
#define XLF_BLOB_NAME_SIZE      32

/**
 * @brief Returns the size of an XLF.
 *
 * @param[in]   head   Pointer XLF IBOOT header.
 * @return      The XLF size.
 * @hideinitializer
 */
#define XLFI_GET_XLF_SIZE(head) \
	(uint32_t) (ntohl(head->crc2_offset) + \
	            sizeof(((struct xlf_iboot_footer *)0)->crc2))

/**
 * @brief Returns the number of LMs in an XLF.
 *
 * @param[in]   head   Pointer XLF IBOOT header.
 * @return      The number of LMs.
 * @hideinitializer
 */
#define XLFI_GET_NUM_OF_FILES(head) \
	(uint32_t) (ntohl(head->file_count))

/**
 * @brief Returns non zero if XLF has working status set.
 *
 * @param[in]   head  Pointer XLF IBOOT header.
 * @return      Non zero if XLF has working status set
 * @hideinitializer
 */
#define XLFI_XLF_HAS_WORKING_STATUS(head) \
	((head->update ^ htonl(XLF_IBOOT_HDR_UPDATE_WORKING_BIT)) & \
	 htonl(XLF_IBOOT_HDR_UPDATE_WORKING_BIT))

/**
 * @brief Sets working status of an XLF.
 *
 * @param[in]   head  Pointer XLF IBOOT header.
 * @hideinitializer
 */
#define XLFI_SET_XLF_WORKING_STATUS(head) \
	head->update &= htonl(~XLF_IBOOT_HDR_UPDATE_WORKING_BIT);

/* > Type Declarations ********************************************************/

/** Return codes. */
typedef uint32_t xlfi_result_t;

/**
 * @brief XLF IBOOT header format
 *
 * All multi byte numeric fields shall be in Big Endian byte order.
 */
struct xlf_iboot_header {
	/** Magic value equal to @ref XLF_IBOOT_HDR_MAGIC. */
	uint32_t magic;
	/**
	 * Type of XLF file: @ref XLF_IBOOT_HDR_TYPE_AU_BOOT,
	 * @ref XLF_IBOOT_HDR_TYPE_AU_APPLIC, @ref XLF_IBOOT_HDR_TYPE_BOOT,
	 * @ref XLF_IBOOT_HDR_TYPE_AU_BOOT_LINUX,
	 * @ref XLF_IBOOT_HDR_TYPE_AU_APPLIC_LINUX,
	 * @ref XLF_IBOOT_HDR_TYPE_AU_ENV or @ref XLF_IBOOT_HDR_TYPE_BOOT_ENV
	 */
	uint32_t type;
	/**
	 * Major version of XLF-format:
	 * 0xff = first version
	 * 0x00 = this version
	 * Stepped when the format is NOT backwards compatible.
	 */
	uint8_t  major_version;
	/**
	 *Minor version of XLF-format:
	 * 0xff = first version
	 * 0x00 = this version
	 * Stepped when the format is backwards compatible.
	 * Reset when major_version is stepped.
	 */
	uint8_t  minor_version;
	/** Padding, shall be 0xff, 0xff */
	uint16_t spare1;
	/**
	 * SW Product ID and revision field. The string is NOT null terminated
	 * and it is padded with spaces. The string is subdivided into a product
	 * id and a revision level. The product id starts at the first location
	 * and the revision starts at suid[23]
	 */
	char     suid[XLF_SUID_LEN];
	/**
	 * Date and time when this XLF was created. Bit 31 in description below
	 * is the MSB.
	 * Bits [31..24]: Year counted from year 1900, range 0..255
	 * Bits [23..16]:Month, range 0 (January) - 11 (December)
	 * Bits [15..8]: Day of Month, range 1 - 31
	 * Bits [7..0]: Hour of Day, range 0 - 23 where the time is the local
	 * time.
	 */
	uint32_t time;
	/**
	 * Offset from xlf_iboot_header::magic to XPL start address.
	 * Should be set to zero if XPL is not present.
	 */
	uint32_t xpl_offset;
	/** Offset from xlf_iboot_header::magic to xlf_iboot_footer::crc2. */
	uint32_t crc2_offset;
	/**
	 * Checksum of the above nine fields, see @ref xlf_checksum for checksum
	 * algorithm.
	 */
	uint16_t crc1;
	/** Padding, shall be 0xff, 0xff */
	uint16_t spare2;
	/** Update field, initially 0xff, 0xff, 0xff, 0xff. */
	uint32_t update;
	/** Sequence number, initially 0xff, 0xff, 0xff, 0xff */
	uint32_t seq_number;
	/** Number of LMs in this container. */
	uint32_t file_count;
	/**
	 * Array of offsets from xlf_iboot_header::magic to
	 * xlf_iboot_wrapper::magic field of each LM in this container
	 */
	uint32_t file_offset[];
};

/**
 * @brief XLF IBOOT wrapper format
 *
 * All multi byte numeric fields shall be in Big Endian byte order.
 */
struct xlf_iboot_wrapper {
	/**
	 * Offset from this field to the last byte of the unpadded LM
	 * (same as the unpadded LM:s length minus one).
	 */
	uint32_t end_offset;
	/**
	 * Type of LM: @ref XLF_IBOOT_WPR_MAGIC_RPDOUT,
	 * @ref XLF_IBOOT_WPR_MAGIC_BLOB or user defined types starting with
	 * 0x10 (0x01nnnnnn).
	 */
	uint32_t magic;
};

/**
 * @brief XLF IBOOT Footer format
 *
 * All multi byte numeric fields shall be in Big Endian byte order.
 */
struct xlf_iboot_footer {
	/**
	 * Checksum, from xlf_iboot_header::file_count (included) to the
	 * beginning of this field. See @ref xlf_checksum for checksum
	 * algorithm.
	 */
	uint16_t crc2;
};

/**
 * @brief XLF BLOB LM header format
 *
 * All multi byte numeric fields shall be in Big Endian byte order.
 */
struct xlf_blob_header {
	/**
	 * CRC of all following header fields and the BLOB itself. CRC
	 * algorithm is CRC-32 according to ISO 3309 and ITU-T V.42, i.e.
	 * polynomial is x32 + x26 + x23 + x 22 + x16 + x12 + x11 + x10 + x8 +
	 * x7 + x5 + x4 + x2 + x + 1, uses LSB-first order, sets the initial
	 * CRC to 0xFFFFFFFF, and complements the final CRC. This is the same
	 * CRC algorithm as UBOOT uses when checking a Kernel image.
	 */
	uint32_t crc32;
	/**
	 * Size in bytes of this field and the following header fields.
	 * The purpose of this field is to be able to extend header with
	 * additional fields without breaking backward compatibility.
	 */
	uint32_t header_size;
	/**
	 * Type of BLOB LM: @ref XLF_BLOB_TYPE_UENVIMAGE,
	 * @ref XLF_BLOB_TYPE_DTB, @ref XLF_BLOB_TYPE_ROOTFS,
	 * @ref XLF_BLOB_TYPE_UIMAGE or @ref XLF_BLOB_TYPE_ASCII_DB.
	 */
	uint32_t type;
	/**
	 * Major version field. This field has no predefined usage. It is free
	 * to use for any version tagging that the user may find appropriate.
	 */
	uint32_t major_version;
	/**
	 * Minor version field. This field has no predefined usage. It is free
	 * to use for any version tagging that the user may find appropriate.
	 */
	uint32_t minor_version;
	/**
	* SW Product ID and revision field, see xlf_iboot_header::suid for
	* description of format.
	*/
	char     suid[XLF_SUID_LEN];
	/**
	 * Date and time when this Load Module was created, see
	 * xlf_iboot_header::time for  description of format.
	 */
	uint32_t time;
	/** Name of BLOB. The string is null terminated. */
	char     name[XLF_BLOB_NAME_SIZE];
};


#endif /* XLFI_H */

#ifdef __cplusplus
}
#endif
