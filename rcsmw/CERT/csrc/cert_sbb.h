/* ----------------------------------------------------------------------
 * %CCaseFile:  cert_sbb.h %
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

#ifndef __CERT_SBB_H
#define __CERT_SBB_H

int get_sdo_data(uint8_t *object_p, uint32_t object_length,
		 uint64_t owner_info,
		 void *data_p, uint32_t *data_length_p,
		 uint16_t *object_info_p);

int secure_boot_is_on(void);

#endif /* __CERT_SBB_H */
