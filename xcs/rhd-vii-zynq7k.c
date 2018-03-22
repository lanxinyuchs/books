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

#include <unistd.h>
#include <errno.h>
#include <string.h>
#include <itc.h>
#include <uio_helper.h>
#include <pthread.h>
#include "rhd-vii-if.h"
#include "rhd-vii-hw.h"
#include "timeout_server.h"
#include "gpio.h"

/*----------------------------  CONSTANTS  ----------------------------------*/
#define TRACEPOINT_PROVIDER   com_ericsson_xcs_rhd_vii
#include "tpt.h"

#define UIO_DEV_SLCR "ps7_slcr_reg"
#define MIO_PIN_28_FAULT_OFFSET 0x00000770
#define MIO_PIN_30_OPER_OFFSET  0x00000778
#define MIO_PIN_40_MAINT_OFFSET 0x000007A0
#define L3_SEL_GPIO 0

#define PIN_FAULT       17
#define PIN_OPERATIONAL 16
#define PIN_MAINTENANCE 15

#define FLASH_MAILBOX "vii_led_flash"
#define FLASH_RECEIVE_TMO 2000

#define MSG_LED_FAULT_OFFSET 0
#define MSG_LED_OPER_OFFSET  1
#define MSG_LED_MAINT_OFFSET 2

/*            ASCII    L E D */
#define MSG_LED_CHG  0x4c454400
#define MSG_LED_CHG_FAULT       (MSG_LED_CHG + MSG_LED_FAULT_OFFSET)
#define MSG_LED_CHG_OPERATIONAL (MSG_LED_CHG + MSG_LED_OPER_OFFSET)
#define MSG_LED_CHG_MAINTENANCE (MSG_LED_CHG + MSG_LED_MAINT_OFFSET)

enum onoff {
	NOW_OFF = 0,
	NOW_ON
};

#define SLOW_BLINK 1000 /* 0.5HZ */
#define MED_BLINK  250  /* 2HZ */
#define FAST_BLINK 31   /* 16HZ */
#define NUM_DF_STAGES 6
static const uint16_t double_flash_timing[NUM_DF_STAGES] = {300, 350, 300, 350, 300, 2400};

struct mmi_led_s {
	uint32_t pin;
	enum led_mode mode;
	enum onoff current_state;
	uint32_t double_flash_stage;
	void *tmo_handle;
	unsigned long long tmo;
};

union itc_msg {
	uint32_t msgno;
};

/* mio&leds array is indexed using led_index */
static const uint32_t mio_ctrl_offset[NUM_LEDS] = {MIO_PIN_28_FAULT_OFFSET,
                                                   MIO_PIN_30_OPER_OFFSET,
                                                   MIO_PIN_40_MAINT_OFFSET
                                                  };

static struct mmi_led_s leds[NUM_LEDS] = {{PIN_FAULT, LED_OFF, NOW_OFF, 0, NULL, 0},
	{PIN_OPERATIONAL, LED_OFF, NOW_OFF, 0, NULL, 0},
	{PIN_MAINTENANCE, LED_OFF, NOW_OFF, 0, NULL, 0}
};

static struct {
	pthread_t thread;
	itc_mbox_id_t mbox;
	uint8_t exit;
} flash;

static gpio_handle_t handle;
static char* boardtype;
#if 0
/**
 * Function set_l3_mux_sel
 */
static void set_l3_mux_sel(void *addr)
{
	volatile uint32_t *mio_reg = (uint32_t *)addr;

	/* bits 7:5 for L3_SEL */
	*mio_reg = (*mio_reg & ~0xe0) | L3_SEL_GPIO;
}
#endif
/**
 * Function configure_mio
 * Configure MIO pin as GPIO
 */
static int configure_mio(void)
{
#if 0
	enum led_index led;
	UIO_HANDLE_ uio_handle;
	void *mmap_base = NULL;

	uio_handle = uio_open(UIO_DEV_SLCR);
	if (uio_handle == UIO_OPEN_FAILED) {
		TPT_ERROR(STR("Failed to open uio %s",
		              UIO_DEV_SLCR));
		return -EFAULT;
	}

	mmap_base = uio_mmap(uio_handle);
	if (mmap_base == MAP_FAILED) {
		TPT_ERROR(STR("Failed to perform UIO memory mapping %s",
		              UIO_DEV_SLCR));
		uio_close(uio_handle);
		return -EFAULT;
	}

	for (led = LED_IDX_ZERO; led < NUM_LEDS; led++) {
		set_l3_mux_sel((void *)((uint32_t)mmap_base + mio_ctrl_offset[led]));

	}

	uio_close(uio_handle);
#endif
	return 0;
}

/**
 * Function flash_led
 */
static void flash_led(enum led_index led)
{
	struct timespec ts_now;
	union itc_msg *tmo_msg = NULL;
	int tmo_msg_num;
	uint8_t value;
	gpio_status_t status;

	switch (led) {
	case LED_FAULT_IND:
		tmo_msg_num = MSG_LED_CHG_FAULT;
		break;
	case LED_OPER_IND:
		tmo_msg_num = MSG_LED_CHG_OPERATIONAL;
		break;
	case LED_MAINT_IND:
		tmo_msg_num = MSG_LED_CHG_MAINTENANCE;
		break;
	default:
		TPT_ERROR("Unknown led index received");
		return;
	}

	if (leds[led].mode == LED_OFF) {
		leds[led].current_state = NOW_OFF;
		value = leds[led].current_state;
		status = gpio_write(handle, 1, &leds[led].pin, &value);
		if (status != GPIO_STATUS_SUCCESS)
			TPT_ERROR(STR("Failed to set LED to new state: %u",
			              value));
		return;
	}
	if (leds[led].mode == LED_STEADY) {
		leds[led].current_state = NOW_ON;
		value = leds[led].current_state;
		status = gpio_write(handle, 1, &leds[led].pin, &value);
		if (status != GPIO_STATUS_SUCCESS)
			TPT_ERROR(STR("Failed to set LED to new state: %u",
			              value));
		return;
	}
	if (!leds[led].tmo) { /*Start a new blink session*/
		if (!clock_gettime(CLOCK_MONOTONIC, &ts_now)) {
			leds[led].tmo = TIMESPEC_TO_MS(ts_now);
			leds[led].double_flash_stage = 0;
			leds[led].current_state = NOW_ON;
		} else {
			TPT_ERROR("Failed to execute clock_gettime");
		}
	}

	if (leds[led].current_state == NOW_ON)
		leds[led].current_state = NOW_OFF;
	else
		leds[led].current_state = NOW_ON;

	switch (leds[led].mode) {
	case LED_05HZ:
		leds[led].tmo += SLOW_BLINK;
		break;

	case LED_2HZ:
		leds[led].tmo += MED_BLINK;
		break;

	case LED_16HZ:
		leds[led].tmo += FAST_BLINK;
		break;

	case LED_DF_ON:
		leds[led].tmo += double_flash_timing[leds[led].double_flash_stage];
		leds[led].double_flash_stage = (leds[led].double_flash_stage + 1) %
		                               NUM_DF_STAGES;
		break;

	case LED_DF_OFF:
		if (leds[led].double_flash_stage == 0 ||
		    leds[led].double_flash_stage == (NUM_DF_STAGES - 1)) {
			leds[led].current_state = NOW_OFF;
		}
		leds[led].tmo += double_flash_timing[leds[led].double_flash_stage];
		leds[led].double_flash_stage = (leds[led].double_flash_stage + 1) %
		                               NUM_DF_STAGES;
		break;

	default:
		TPT_ERROR(STR("Unexpected mode %d for LED %d",
		              leds[led].mode, led));
		return;
	}

	value = leds[led].current_state;
	status = gpio_write(handle, 1, &leds[led].pin, &value);
	if (status != GPIO_STATUS_SUCCESS)
		TPT_ERROR(STR("Failed to set LED to new state: %u",
		              value));

	tmo_msg = itc_alloc(sizeof(uint32_t), tmo_msg_num);
	leds[led].tmo_handle = register_tmo_absolute_with_sender(leds[led].tmo,
	                       &tmo_msg,
	                       flash.mbox);
}

/**
 * Thread flash_thread
 */
static void *flash_thread(__attribute__((unused)) void *arg)
{
	union itc_msg *msg;
	itc_mbox_id_t sender_mbox = ITC_NO_ID;

	flash.mbox = itc_create_mailbox(FLASH_MAILBOX, 0);
	if (flash.mbox == ITC_NO_ID) {
		TPT_ERROR("Failed to create mailbox");
		pthread_exit(0);
	}

	while (!flash.exit) {
		msg = itc_receive(ITC_NOFILTER, FLASH_RECEIVE_TMO, ITC_FROM_ALL);
		if (msg == NULL)
			continue;

		sender_mbox = itc_sender(msg);

		switch (msg->msgno) {
		case MSG_LED_CHG_FAULT:
			TPT_REC_SIG(msg->msgno,
			            STR("Receive MSG_LED_CHG_FAULT, sender: %d",
			                sender_mbox));
			flash_led(LED_FAULT_IND);
			break;

		case MSG_LED_CHG_OPERATIONAL:
			TPT_REC_SIG(msg->msgno,
			            STR("Receive MSG_LED_CHG_OPERATIONAL, sender: %d",
			                sender_mbox));
			flash_led(LED_OPER_IND);
			break;

		case MSG_LED_CHG_MAINTENANCE:
			TPT_REC_SIG(msg->msgno,
			            STR("Receive MSG_LED_CHG_MAINTENANCE, sender: %d",
			                sender_mbox));
			flash_led(LED_MAINT_IND);
			break;

		default:
			TPT_ERROR(STR("Receive Unexpected message: 0x%x, sender: %d",
			              msg->msgno, sender_mbox));
			break;
		}
		itc_free(&msg);
	}
	gpio_unreserve(handle);

	itc_delete_mailbox(flash.mbox);
	pthread_exit(0);
	return NULL;
}

/**
 * Function vii_hw_init
 * Initialize vii device
 */
int vii_hw_init(void)
{
	gpio_status_t status;
	const uint32_t pins[] = {PIN_FAULT, PIN_OPERATIONAL, PIN_MAINTENANCE};
	const uint8_t dirs[] = {1, 1, 1};
	const uint8_t values[] = {NOW_OFF, NOW_OFF, NOW_OFF};
	uint32_t real_num_leds;

	memset(&flash, 0, sizeof(flash));
	flash.mbox = ITC_NO_ID;

	if (configure_mio())
		return -EFAULT;

	if ((boardtype = getenv("SYS_BOARD_TYPE")) == NULL) {
		TPT_ERROR("SYS_BOARD_TYPE not found, aborting");
			return -EFAULT;
        }

	if (strcmp(boardtype ,"BP") == 0) {
		real_num_leds = sizeof(pins) / sizeof(pins[0]); // bp
	} else if (strcmp(boardtype ,"TRXM") == 0) {
		real_num_leds = sizeof(pins) / sizeof(pins[0]) - 1; // trxm
	} else {
		TPT_ERROR("SYS_BOARD_TYPE is wrong value, aborting");
			return -EFAULT;
	}
	
	status = gpio_reserve(real_num_leds, pins, &handle);
	if (status != GPIO_STATUS_SUCCESS) {
		TPT_ERROR(STR("%s: Failed to reserve gpio pin",
			      __func__));
		return -EFAULT;
	}

	status = gpio_set_dir(handle, real_num_leds, pins, dirs);
	if (status != GPIO_STATUS_SUCCESS) {
		TPT_ERROR(STR("%s: Failed to set directions",
			      __func__));
		return -EFAULT;
	}

	status = gpio_write(handle, real_num_leds, pins, values);
	if (status != GPIO_STATUS_SUCCESS) {
		TPT_ERROR(STR("%s: Failed to set values",
			      __func__));
		return -EFAULT;
	}

	if (init_tmo_server("vii_tmo")) {
		TPT_ERROR("Failed to init timeout server.");
		return -EFAULT;
	}

	if (pthread_create(&flash.thread, NULL, flash_thread, 0) != 0) {
		TPT_ERROR("Failed to create flash thread");
		return -EFAULT;
	}

	/* Wait for flash thread to start */
	int i;
	for (i = 0; (flash.mbox == ITC_NO_ID) && (i < 5000); i++)
		usleep(1000);

	if (flash.mbox == ITC_NO_ID) {
		TPT_ERROR("Failed to find flash thread mailbox");
		return - EFAULT;
	}

	return 0;
}

/**
 * Function vii_hw_shutdown
 * Shut down vii device
 */
void vii_hw_shutdown(void)
{
	/* Cancel the flash thread */
	flash.exit = 1;
	pthread_join(flash.thread, NULL);
}

/**
 * Function set_led
 */
void set_led(enum led_index led, enum led_mode mode)
{
	if (strcmp(boardtype,"TRXM") == 0 ) {
		if (led >= (NUM_LEDS - 1)) {
			TPT_ERROR("Unknown led index received");
			return;
		}
	} else {
		if (led >= NUM_LEDS) {
			TPT_ERROR("Unknown led index received");
			return;
		}
	}

	if (leds[led].mode != mode) {
		leds[led].mode = mode;
		cancel_tmo(leds[led].tmo_handle);
		leds[led].tmo_handle = NULL;
		leds[led].tmo = 0; /*indicate re-init of flash*/
		flash_led(led);
	}
}

