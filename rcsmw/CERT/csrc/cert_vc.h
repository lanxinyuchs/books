/* ----------------------------------------------------------------------
 * %CCaseFile:  cert_vc.h %
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

#ifndef __CERT_VC_H
#define __CERT_VC_H

#include <stdint.h>
#include <cert_seci.h>

enum vc_state {
	VC_EXISTS = 0,
	NO_VC_EXISTS = 1,
	ERROR_READING_VC = 2
};

struct vc_hdr {
	char magic[4];
	uint8_t version[4];
	uint32_t containers_length;
	uint8_t data[0];
} __attribute__ ((packed));

struct vc_container {
	uint32_t data_len;
	char name[32];
	uint8_t data[0];
} __attribute__ ((packed));


SeciResultT get_vc(char** resultP);

#endif
