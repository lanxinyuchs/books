/* ----------------------------------------------------------------------
 * %CCaseFile:  cert_crc.h %
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

#ifndef __CERT_CRC_H
#define __CERT_CRC_H

#include <stdint.h>

/* ==================================================================== */
/**
 *   Calculate checksum for a buffer
 *
 *   @param   buf pointer to buffer with data
 *   @param   size Length of the buffer
 *
 *   @return  checksum value
 *
 *   @par Globals:
 *               crc32table
 *
 *   The function calculates checksum for a buffer with size data
 *
 */
/* ===================================================================== */
uint32_t crc(const char *buf, unsigned int size);

#endif
