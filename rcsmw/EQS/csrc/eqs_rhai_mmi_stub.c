/*
 *
 * Copyright (c) Ericsson AB 2014 All rights reserved.
 *
 * The information in this document is the property of Ericsson. Except
 * as specifically authorized in writing by Ericsson,the receiver of this
 * document shall keep the information contained herein confidential
 * and shall protect the same in whole or in part from disclosure and
 * dissemination to third parties. Disclosure and disseminations to the
 * receiver's employees shall only be made on a strict need to know basis.
 *
 */

#include <unistd.h>

#include "rhai-mmi.h"

static enum rhai_mmi_led_state operational_led_state = RHAI_MMI_LED_OFF;
static enum rhai_mmi_led_state fault_led_state = RHAI_MMI_LED_OFF;
static enum rhai_mmi_led_state status_led_state = RHAI_MMI_LED_OFF;
static enum rhai_mmi_led_state maintenance_led_state = RHAI_MMI_LED_OFF;

int
rhai_mmi_led_set_state(enum rhai_mmi_led_type led_type,
		       enum rhai_mmi_led_state led_state)
{
  if (led_type < RHAI_MMI_LED_OPERATIONAL ||
      led_type > RHAI_MMI_LED_MAINTENANCE) {
    return -RHAI_MMIERR_INVAL;
  }

  if (led_state < RHAI_MMI_LED_OFF ||
      led_state > RHAI_MMI_LED_DOUBLE_FLASH_ON) {
    return -RHAI_MMIERR_INVAL;
  }

  switch (led_type) {
  case RHAI_MMI_LED_OPERATIONAL:
    operational_led_state = led_state;
    break;
  case RHAI_MMI_LED_FAULT:
    fault_led_state = led_state;
    break;
  case RHAI_MMI_LED_STATUS:
    status_led_state = led_state;
    break;
  case RHAI_MMI_LED_MAINTENANCE:
    maintenance_led_state = led_state;
    break;
  }

  return RHAI_MMIERR_SUCCESS;
}

int
rhai_mmi_led_get_state(enum rhai_mmi_led_type led_type,
		       enum rhai_mmi_led_state *led_state)
{
  if (led_type < RHAI_MMI_LED_OPERATIONAL ||
      led_type > RHAI_MMI_LED_MAINTENANCE) {
    return -RHAI_MMIERR_INVAL;
  }

  if (led_state == NULL)
    return -RHAI_MMIERR_INVAL;

  switch (led_type) {
  case RHAI_MMI_LED_OPERATIONAL:
    *led_state = operational_led_state;
    break;
  case RHAI_MMI_LED_FAULT:
    *led_state = fault_led_state;
    break;
  case RHAI_MMI_LED_STATUS:
    *led_state = status_led_state;
    break;
  case RHAI_MMI_LED_MAINTENANCE:
    *led_state = maintenance_led_state;
    break;
  }

  return RHAI_MMIERR_SUCCESS;
}

int
rhai_mmi_button_init(void **handle, int __attribute__((__unused__)) exclusive)
{
  if (handle) {
    *handle = NULL;
    return RHAI_MMIERR_SUCCESS;
  }

  return -RHAI_MMIERR_INVAL;
}

int
rhai_mmi_button_get_event (void __attribute__((__unused__)) *handle,
			   int __attribute__((__unused__)) *button_events)
{
  char tmp;

  for (;;) {
    if (read(STDIN_FILENO, &tmp, 1) < 1)
      break;
  }

  return -RHAI_MMIERR_IO;
}

void
rhai_mmi_button_shutdown(void __attribute__((__unused__)) *handle)
{
}
