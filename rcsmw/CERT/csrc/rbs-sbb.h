/* ----------------------------------------------------------------------
 * %CCaseFile:  rbs_sbb.h %
 * %CCaseRev:   /main/R3A/1 %
 * %CCaseDate:  2016-01-25 %
 * %CCaseDocNo: %
 * Author:      ehsake
 *
 * ----------------------------------------------------------------------
 * %CCaseTemplateFile:  template.h %
 * %CCaseTemplateId: CCver: /main/1 %
 *
 * %CCaseCopyrightBegin%
 * Copyright (c) Ericsson AB %CCaseTemplateCopyrightYear% All rights reserved.
 *
 * The information in this document is the property of Ericsson.
 *
 * Except as specifically authorized in writing by Ericsson, the
 * receiver of this document shall keep the information contained
 * herein confidential and shall protect the same in whole or in
 * part from disclosure and dissemination to third parties.
 *
 * Disclosure and disseminations to the receivers employees shall
 * only be made on a strict need to know basis.
 * %CCaseCopyrightEnd%
 *
 * ----------------------------------------------------------------------
 *
 * Revision history:
 *
 * Rev        Date        Name        What
 * -----      ----------  --------    --------------------------
 *            20160125    ehsake      SECI update for eIDL encryption
 * ----------------------------------------------------------------------
 */


/* Copied from Network Loader */

#ifndef __UAPI_RBS_SBB_H__
#define __UAPI_RBS_SBB_H__

#define SBB_IOC_MAGIC                 0x81 /* See: Documentation/ioctl/ioctl-number.txt */
#define SBB_IOC_SET_SDO               _IOWR(SBB_IOC_MAGIC, 0, int)
#define SBB_IOC_GET_SDO               _IOWR(SBB_IOC_MAGIC, 1, int)
#define SBB_IOC_GET_EFUSES            _IOWR(SBB_IOC_MAGIC, 2, int)
#define SBB_IOC_MAX_NUM               2

#define SDO_IV_LENGTH                 16
#define SDO_MAX_OVERHEAD              76
#define SDO_MIN_OVERHEAD              60
#define SDO_MAX_LENGTH                (1024*1024)


typedef struct sbb_ioc_sdo_set{
	void     *src_p;
	void     *dst_p;
	uint32_t data_length;
	uint32_t sdo_length;  /* output */
	uint32_t owner_info_hi;
	uint32_t owner_info_lo;
	uint16_t object_info;
	uint16_t pad2;
	uint8_t  iv[SDO_IV_LENGTH];
} sbb_ioc_sdo_set_t;

typedef struct sbb_ioc_sdo_get{
	void     *src_p;
	void     *dst_p;
	uint32_t data_length; /* output */
	uint32_t sdo_length;
	uint32_t owner_info_hi;
	uint32_t owner_info_lo;
	uint16_t object_info; /* output */
} sbb_ioc_sdo_get_t;

typedef struct sbb_ioc_efuse_get{
	uint32_t address; /* output */
	uint32_t length;
	uint32_t fuses[8];
} sbb_ioc_efuse_get_t;

#endif // __UAPI_RBS_SBB_H__
