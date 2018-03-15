/* ---------------------------------------------------------------------------
 *
 * © Ericsson AB 2014 All rights reserved.
 * The information in this document is the property of Ericsson.  Except
 * as specifically authorized in writing by Ericsson, the receiver of
 * this document shall keep the information contained herein confidential
 * and shall protect the same in whole or in part from disclosure and
 * dissemination to third parties.  Disclosure and disseminations to the
 * receivers employees shall only be made on a strict need to know basis.
 *
 * ---------------------------------------------------------------------------
 */

#include <errno.h>
#include <string.h>
#include <itc.h>
#include <uio_helper.h>
#include "rhd-vii-if.h"
#include "rhd-vii-hw.h"

/*----------------------------  CONSTANTS  ----------------------------------*/
#define TRACEPOINT_PROVIDER   com_ericsson_xcs_rhd_vii
#include "tpt.h"

/* Register value */
#define XENON_GREEN_LED_CTRL      (0x0)
#define XENON_RED_LED_CTRL        (0x4)
#define XENON_BLUE_LED_CTRL       (0x8)
#ifdef SPECIAL_LED
#define XENON_YELLOW_LED_CTRL     (0x30)
#endif

/* Mode for LED register */
#define LED_REG_OFF  0x0
#define LED_REG_ON   0x1
#define LED_REG_05HZ 0x2
#define LED_REG_2HZ  0x3
#define LED_REG_16HZ 0x4
#define LED_REG_DF   0x8

#define UIO_DEV_VII      "vii"

struct led_reg_s {
	volatile uint32_t *green;
	volatile uint32_t *red;
	volatile uint32_t *blue;
#ifdef SPECIAL_LED
	volatile uint32_t *yellow;
#endif
};

/*
 * Local Variable Definition
 */
static struct led_reg_s led_ctrl;

/**
 * Function init_regs
 * Initialize LED control registers
 */
static void init_regs(void *mmap_base)
{
	/* Store pointers to LED registers */
	led_ctrl.green  = (uint32_t *) (((uintptr_t)mmap_base) +
	                                XENON_GREEN_LED_CTRL);
	led_ctrl.red    = (uint32_t *) (((uintptr_t)mmap_base) +
	                                XENON_RED_LED_CTRL);
	led_ctrl.blue   = (uint32_t *) (((uintptr_t)mmap_base) +
	                                XENON_BLUE_LED_CTRL);
#ifdef SPECIAL_LED
	led_ctrl.yellow = (uint32_t *) (((uintptr_t)mmap_base) +
	                                XENON_YELLOW_LED_CTRL);
#endif
}

/**
 * Function vii_hw_init
 * Initialize vii device
 */
int vii_hw_init(void)
{
	void *mmap_base = NULL;
	UIO_HANDLE_ uio_handle = 0;

	memset(&led_ctrl, 0, sizeof(struct led_reg_s));

	/* Initialize UIO handler */
	uio_handle = uio_open(UIO_DEV_VII);
	if (uio_handle == UIO_OPEN_FAILED) {
		TPT_ERROR("Failed to open uio");
		return -EFAULT;
	}

	mmap_base = uio_mmap(uio_handle);

	if (mmap_base == MAP_FAILED) {
		TPT_ERROR("Failed to peform UIO memory mapping");
		uio_close(uio_handle);
		return -EFAULT;
	}

	init_regs(mmap_base);

	return 0;
}

/**
 * Function vii_hw_shutdown
 * Shut down vii device
 */
void vii_hw_shutdown(void)
{
	/* Nothing has to be done here on Xenon */
}

/**
 * Function set_led
 */
void set_led(enum led_index led, enum led_mode mode)
{
	const uint8_t mode_to_reg[NUM_MODES] = {LED_REG_OFF,
	                                        LED_REG_ON,
	                                        LED_REG_05HZ,
	                                        LED_REG_2HZ,
	                                        LED_REG_16HZ,
	                                        LED_REG_DF | LED_REG_ON,
	                                        LED_REG_DF | LED_REG_OFF
	                                       };
	uint8_t reg = mode_to_reg[mode];

	switch (led) {
	case LED_FAULT_IND:
		*led_ctrl.red = reg;
		break;
	case LED_OPER_IND:
		*led_ctrl.green = reg;
		break;
	case LED_MAINT_IND:
		*led_ctrl.blue = reg;
		break;
#ifdef SPECIAL_LED
	case LED_SPECIAL_IND:
		*led_ctrl.yellow = reg;
		break;
#endif
	default:
		TPT_ERROR("Unknown led index received");
		break;
	}
}

