/**
 *   ECM linkhandler CM include file.
 *
 *   @file ulh_ecm.h
 *
 *   TBD
 *
 *   Copyright (C) 2010 by Ericsson AB. All rights reserved. The
 *   information in this document is the property of Ericsson. Except
 *   as specifically authorized in writing by Ericsson, the receiver
 *   of this document shall keep the information contained herein
 *   confidential and shall protect the same in whole or in part from
 *   disclosure and dissemination to third parties. Disclosure and
 *   disseminations to the receiver's employees shall only be made on
 *   a strict need to know basis.
 */

/* ========================================================================
 *   History of development:
 *   -----------------------
 *   Revised : 2014-10-17 Magnus Lindberg
 *   Change  : First version.
 * ========================================================================
 */

#ifndef __ULH_ECM_H
#define __ULH_ECM_H

#ifdef __cplusplus
extern "C" {
#endif

/* ========================================================================
 *   INCLUDE FILES
 * ========================================================================
 */
#include <stdint.h>

#include <net/ethernet.h>

/* ========================================================================
 *   DEFINITIONS
 * ========================================================================
 */

/* ========================================================================
 *   TYPE DEFINITIONS
 * ========================================================================
 */
struct ulh_cm_ecm_config {
	struct ulh_cm_config cmn;

	int                  rt_tmo;   /* Retransmission timeout (ms). */
	int                  ack_tmo;  /* Ack timeout (ms). */
        int                  kpa_tmo;  /* Keep alive timeout (ms). */
	int                  sup_tmo;  /* Supervision timeout (ms). */
	int                  tx_wnd;   /* Transmit window size (packets). */
	int                  rt_limit; /* Retransmission limit. */
	int                  conn_tmo; /* Connect timeout (ms). */

	uint16_t             prios;    /* number of priorities */

	uint8_t              dstmac[ETH_ALEN];
	uint8_t              srcmac[ETH_ALEN];
};

/* ========================================================================
 *   DATA DECLARATIONS
 * ========================================================================
 */

/* Copy header and declararion of global functions, if applicable, from
 * the C file and remove this comment.
 */

int ulh_ecm_init(const char *name);

#ifdef __cplusplus
}
#endif

#endif   /* ifndef __ULH_ECM_H */
