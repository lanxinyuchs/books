/**
 *   @copyright
 *   Copyright (C) 2015 by Ericsson AB. All rights reserved. The
 *   information in this document is the property of Ericsson. Except
 *   as specifically authorized in writing by Ericsson, the receiver
 *   of this document shall keep the information contained herein
 *   confidential and shall protect the same in whole or in part from
 *   disclosure and dissemination to third parties. Disclosure and
 *   disseminations to the receiver's employees shall only be made on
 *   a strict need to know basis.
 */

#ifndef _RHD_VII_HW_H_
#define _RHD_VII_HW_H_

enum led_index {
	LED_IDX_ZERO = 0,
	LED_FAULT_IND = LED_IDX_ZERO,
	LED_OPER_IND,
	LED_MAINT_IND,
#ifdef SPECIAL_LED
	LED_SPECIAL_IND,
#endif
	NUM_LEDS
};

enum led_mode {
	LED_OFF = 0,
	LED_STEADY,
	LED_05HZ,
	LED_2HZ,
	LED_16HZ,
	LED_DF_ON,    /* off - on - off - on - off - on  */
	LED_DF_OFF,   /* off - on - off - on - off - off */
	NUM_MODES
};


/**
 * Function vii_hw_init
 * Initialize vii device
 */
int vii_hw_init(void);

/**
 * Function vii_hw_shutdown
 * Shut down vii device
 */
void vii_hw_shutdown(void);

/**
 * Function set_led
 */
void set_led(enum led_index led, enum led_mode mode);

#endif
