/* ---------------------------------------------------------------------------
 *
 * © Ericsson AB 2015 All rights reserved.
 * The information in this document is the property of Ericsson.  Except
 * as specifically authorized in writing by Ericsson, the receiver of
 * this document shall keep the information contained herein confidential
 * and shall protect the same in whole or in part from disclosure and
 * dissemination to third parties.  Disclosure and disseminations to the
 * receivers employees shall only be made on a strict need to know basis.
 *
 * ---------------------------------------------------------------------------
 */

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include "xpp_init.h"
#include "common.h"
#include "log_tpt.h"

#define XPAI_INIT_SPI_MAX_PORT 5

extern void TRI_initInterface(void);

/**
 * XPAI_initXPPInterface
 * This function shall be called before using XPAI
 * If error occurs, this function will hang and thus preventing
 * the application to fully start.
 */
void XPAI_initXPPInterface(void)
{
	static int init_done = 0;
	uint32_t client_ref = 1;

	if (!init_done) {

		TPT_INIT();
		TPT_INFO("Start initializing");

		if (xpai_fault_init(client_ref)) {
			TPT_ERROR("xpai_fault_init failed");
                } else if (xpai_i2c_init(client_ref)) {
			TPT_ERROR("xpai_i2c_init failed");
		} else if (xpai_spi_init(client_ref)) {
			TPT_ERROR("xpai_spi_init failed");
		} else if (xpai_vii_init(client_ref)) {
			TPT_ERROR("xpai_vii_init failed");
		} else if (xpai_xmr_init()) {
			TPT_ERROR("xpai_xmr_init failed");
		} else if (xpai_restart_init()) {
			TPT_ERROR("xpai_restart_init failed");
		} else if (xpai_lh_init(client_ref)) {
			TPT_ERROR("xpai_lh_init failed");
		} else if (xpai_hwlog_init()) {
			TPT_ERROR("xpai_hwlog_init failed");
		} else if (xpai_lmc_init()) {
			TPT_ERROR("xpai_lmc_init failed");
		//} else if (xdai_mmi_init(client_ref)) {
		//	TPT_ERROR("xdai_mmi_init failed");
		} else if (xpai_nodeid_init()) {
			TPT_ERROR("xpai_nodeid_init failed");
		}
		else {
			init_done = 1;
		}

	}

	if (!init_done) {
		/* Initialization failed => hang! */
		/* coverity[no_escape] */
		while (1) {
			TPT_ERROR("Failed to initialize. "
			        "Will hang to prevent boot on faulty SW");
			sleep(60);
		}
	}

	TPT_INFO("Successfully initialized");
}
