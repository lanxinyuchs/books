/* ----------------------------------------------------------------------
 * %CCaseFile:  cert_sbb.c %
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

#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/ioctl.h>
#include <fcntl.h>
#include <unistd.h>

#include "rbs-sbb.h"
/* 
#include "/repo/etxderb/rcs-ee/rcs-linux-arm/include/uapi/linux/rbs/rbs-sbb.h"
#include <linux/rbs/rbs-sbb.h>*/

#define RCS_MAGIC           0x52435300
#define SBB_DEV             "/dev/sbb"

/* Secure Boot Enable */
#define SBE_REG_ADDR        0x18 /* There are more stuff than SBE in this reg */
#define SBE_REG_LEN         4
#define SBE_BIT             (1<<3)
/* For other registers etc see the AXX5500_Security_Subsystem_MulticoreRefMan */


/*
 * This is a header that is added by the sdod daemon
 * when it creates sdo files (to be forward compatible).
 * It is not part of the secure data object that the HW expects/creates.
 */
typedef struct rcs_file_head {
	uint32_t magic;
	uint32_t version;
} rcs_file_head_t;


/*
 * Decodes secure data objects.
 *
 * object_p      (i) : Secure object to be decoded, (including RCS header)
 * object_length (i) : Length of object buffer
 * owner_info    (i) : Owners secret, "owners half of the key"
 * data_p        (o) : The decrypted data
 *                     (use same buf size as the sdo, but can be a bit smaller)
 * data_length_p (o) : Lenght of the decoded data
 * object_info_p (o) : The object_info field from the SDO
 * Return : 0 = OK, 1 = NOK
 */
int get_sdo_data(uint8_t *object_p, uint32_t object_length,
		 uint64_t owner_info,
		 void *data_p, uint32_t *data_length_p,
		 uint16_t *object_info_p)
{
	int fh;
	int err;
	sbb_ioc_sdo_get_t getcmd;
	uint8_t *sdo_p;
	uint32_t sdo_length;

	/* Skip the RCS header */
	sdo_p = object_p + sizeof(rcs_file_head_t);
	sdo_length = object_length - sizeof(rcs_file_head_t);

	/* Get the data from the SDO */
	getcmd.src_p = sdo_p;
	getcmd.dst_p = data_p;
	getcmd.sdo_length = sdo_length;
	getcmd.owner_info_hi = (uint32_t)(owner_info>>32);;
	getcmd.owner_info_lo = (uint32_t)owner_info;

	fh = open(SBB_DEV, O_RDWR);
	if (fh < 0){
		return 1;
	}

	err = ioctl(fh, SBB_IOC_GET_SDO, &getcmd);
	close(fh);

	if (err){
		return 1;
	}

	*object_info_p = getcmd.object_info;
	*data_length_p = getcmd.data_length;

	return 0;
}


/*
 * Check the eFuses to see if the Secure Boot Enable bit is set
 *
 * Return : -1 = error, 0 = SBE off, 1 = SBE on
 */
int secure_boot_is_on(void)
{
	int fh;
	int rc;
	sbb_ioc_efuse_get_t params;

	fh = open(SBB_DEV, O_RDWR);
	if (fh < 0){
		return -1;
	}
	params.address = SBE_REG_ADDR;
	params.length = SBE_REG_LEN;

	rc = ioctl(fh, SBB_IOC_GET_EFUSES, &params);
	close(fh);

	if (rc < 0){
		return -1;
	}

	return (params.fuses[0] & SBE_BIT) != 0;
}
