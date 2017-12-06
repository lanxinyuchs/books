/* ---------------------------------------------------------------------------
 *
 * © Ericsson AB 2012 All rights reserved.
 * The information in this document is the property of Ericsson.  Except
 * as specifically authorized in writing by Ericsson, the receiver of
 * this document shall keep the information contained herein confidential
 * and shall protect the same in whole or in part from disclosure and
 * dissemination to third parties.  Disclosure and disseminations to the
 * receivers employees shall only be made on a strict need to know basis.
 *
 * ---------------------------------------------------------------------------
 */

#ifndef __UAPI_RBS_SBB_H__
#define __UAPI_RBS_SBB_H__

#define SBB_IOC_MAGIC                 0x81 /* See: Documentation/ioctl/ioctl-number.txt */
#define SBB_IOC_SET_SDO               _IOWR(SBB_IOC_MAGIC, 0, int)
#define SBB_IOC_GET_SDO               _IOWR(SBB_IOC_MAGIC, 1, int)
#define SBB_IOC_GET_EFUSES            _IOWR(SBB_IOC_MAGIC, 2, int)
#define SBB_IOC_VERIFY_IMG            _IOWR(SBB_IOC_MAGIC, 3, int)
#define SBB_IOC_MAX_NUM               3

/*
  note:
  although SDO_MAX_LENGTH and IMG_MAX_LENGTH below
  are quite big, lengths greater than one page may give
  out of mem error on a non idle system.
*/

#define SDO_IV_LENGTH                 16
#define SDO_MAX_OVERHEAD              76
#define SDO_MIN_OVERHEAD              60
#define SDO_MAX_LENGTH                (1024*1024)

#define IMG_MAX_LENGTH                (1024*1024)


typedef struct sbb_ioc_sdo_set {
	void     *src_p;
	void     *dst_p;
	uint32_t data_length;
	uint32_t sdo_length; /* output, 0 on error */
	uint32_t owner_info_hi;
	uint32_t owner_info_lo;
	uint16_t object_info;
#ifdef __LP64__
	uint16_t srk_index;
#else
	uint16_t pad2;
#endif
	uint8_t  iv[SDO_IV_LENGTH];
} sbb_ioc_sdo_set_t;

typedef struct sbb_ioc_sdo_get {
	void     *src_p;
	void     *dst_p;
	uint32_t data_length; /* output, 0 on error or sdo verification faild */
	uint32_t sdo_length;
	uint32_t owner_info_hi;
	uint32_t owner_info_lo;
	uint16_t object_info; /* output */
} sbb_ioc_sdo_get_t;

typedef struct sbb_ioc_efuse_get {
	uint32_t address;
	uint32_t length;
	uint32_t fuses[8]; /* output */
} sbb_ioc_efuse_get_t;

/* note, image is only verified not changed, i.e. header
   is not removed as in the U-Boot verify function. */
typedef struct sbb_ioc_img_verify {
	void     *img_p;
	uint32_t img_length;
	uint32_t failed; /* output, 0 = verify OK, 1 = verify NOK */
} sbb_ioc_img_verify_t;

#endif // __UAPI_RBS_SBB_H__
