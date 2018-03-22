/**
 * Copyright (C) 2015 by Ericsson AB. All rights reserved. The
 * information in this document is the property of Ericsson. Except
 * as specifically authorized in writing by Ericsson, the receiver
 * of this document shall keep the information contained herein
 * confidential and shall protect the same in whole or in part from
 * disclosure and dissemination to third parties. Disclosure and
 * disseminations to the receiver's employees shall only be made on
 * a strict need to know basis.
 */

/* Configures the PINMUX for ANP and GPS UART using hardcoded parameters. */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdint.h>
#include <unistd.h>

#include <itc.h>

#include <pinmux.h>


static void cfg_pinmux_anp_uart(void)
{
	uint32_t        anp_uart_pins[] = { 255, 256, 257 };
	pinmux_handle_t handle;
	pinmux_status_t status;

	status = pinmux_reserve(3, anp_uart_pins, &handle);
	if (status != PINMUX_STATUS_SUCCESS) {
		printf("pinmux_reserve() failed (%d)\n", status);
		exit(EXIT_FAILURE);
	}

	status = pinmux_set_func(handle, 3, anp_uart_pins,
	                         PINMUX_FUNC_TYPE_ALTF1);
	if (status != PINMUX_STATUS_SUCCESS) {
		printf("pinmux_set_func() failed (%d)\n", status);
		exit(EXIT_FAILURE);
	}
}

static void cfg_pinmux_gps_uart(void)
{
	uint32_t        gps_uart_pins[] = { 273, 274, 276 };
	pinmux_handle_t handle;
	pinmux_status_t status;

	status = pinmux_reserve(3, gps_uart_pins, &handle);
	if (status != PINMUX_STATUS_SUCCESS) {
		printf("pinmux_reserve() failed (%d)\n", status);
		exit(EXIT_FAILURE);
	}

	status = pinmux_set_func(handle, 3, gps_uart_pins,
	                         PINMUX_FUNC_TYPE_ALTF1);
	if (status != PINMUX_STATUS_SUCCESS) {
		printf("pinmux_set_func() failed (%d)\n", status);
		exit(EXIT_FAILURE);
	}
}

int main(int argc, char *argv[])
{
	itc_mbox_id_t  mid;

	(void) argc;
	(void) argv;

	/* ITC is needed for PINMUX */
	if (itc_init(16, ITC_MALLOC, NULL, ITC_NO_NAMESPACE, 0) != 0) {
		printf("itc_init() failed\n");
		exit(EXIT_FAILURE);
	}
	mid = itc_create_mailbox("cfg_pinmux", 0);
	if (mid == ITC_NO_ID) {
		printf("itc_create_mailbox() failed\n");
		exit(EXIT_FAILURE);
	}

	cfg_pinmux_anp_uart();
	printf("Configured PINMUX for ANP UART\n");

	cfg_pinmux_gps_uart();
	printf("Configured PINMUX for GPS UART\n");

	printf("Keeping configured PINMUX pins configured...\n");
	while ((volatile int) 1) {
		sleep(1);
	}

	return 0;
}
