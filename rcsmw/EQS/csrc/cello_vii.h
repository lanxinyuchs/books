/*
 *
 * Copyright (c) Ericsson AB  2012-2014 All rights reserved.
 *
 * The information in this document is the property of Ericsson. Except
 * as specifically authorized in writing by Ericsson,the receiver of this
 * document shall keep the information contained herein confidential
 * and shall protect the same in whole or in part from disclosure and
 * dissemination to third parties. Disclosure and disseminations to the
 * receiver's employees shall only be made on a strict need to know basis.
 *
 */

#ifndef CELLO_VII_H
#define CELLO_VII_H

/*
 *
 * INCLUDE FILES
 *
 */

#ifdef __cplusplus
extern "C" {
#endif

  /*
   *
   * MACROS
   *
   */

  /*
   *
   * TYPES
   *
   */

  typedef enum
    {
      CELLO_VII_FAILED = -1,
      CELLO_VII_SUCCESS
    } CelloViiResult_e;

  typedef enum
    {
      CELLO_VII_FAULT = -1,
      CELLO_VII_NO_FAULT,
      CELLO_VII_LOADTEST_START,
      CELLO_VII_LOADTEST_END,
      CELLO_VII_NO_POWER,
      CELLO_VII_POWER,
      CELLO_VII_BOOTTEST_START,
      CELLO_VII_BOOTTEST_END,
      CELLO_VII_MISSING_RESOURCE_START,
      CELLO_VII_MISSING_RESOURCE_END,
      CELLO_VII_BOARD_LOCKED,
      CELLO_VII_BOARD_UNLOCKED,
      CELLO_VII_BOARD_BLOCKED,
      CELLO_VII_BOARD_UNBLOCKED,
      CELLO_VII_DISC_SYNC_START,
      CELLO_VII_DISC_SYNC_END,
      CELLO_VII_BOARD_BUSY_START,
      CELLO_VII_BOARD_BUSY_END,
      CELLO_VII_SHUTDOWN_START,
      CELLO_VII_SHUTDOWN_END,
      CELLO_VII_BACKUP_START,
      CELLO_VII_BACKUP_END,
      CELLO_VII_MEDIUM_BUTTON_PRESS_START,
      CELLO_VII_MEDIUM_BUTTON_PRESS_END,
      CELLO_VII_SHORT_BUTTON_PRESS_START,
      CELLO_VII_SHORT_BUTTON_PRESS_END,
      CELLO_VII_ALARM_SUPPRESS_START,
      CELLO_VII_ALARM_SUPPRESS_END,
      CELLO_VII_NODE_FAULT_START,
      CELLO_VII_NODE_FAULT_END,
      CELLO_VII_REMOTE_UNIT_FAULT_START,
      CELLO_VII_REMOTE_UNIT_FAULT_END
    } CelloViiCommand_e;

  typedef enum
    {
      CELLO_VII_LED_OPERATIONAL = 1,       /* Green LED  */
      CELLO_VII_LED_FAULT       = 2,       /* Red LED    */
      CELLO_VII_LED_STATUS      = 3,       /* Yellow LED */
      CELLO_VII_LED_MAINTENANCE = 4        /* Blue LED   */
    } CelloViiLed_e;

  typedef enum
    {
      CELLO_VII_LED_OFF                = 1,  /* No light */
      CELLO_VII_LED_ON                 = 2,  /* Steady light */
      CELLO_VII_LED_SLOW_BLINK         = 3,  /* Blinking 0.5 Hz */
      CELLO_VII_LED_FAST_BLINK         = 4,  /* Blinking 16 Hz */
      CELLO_VII_LED_DOUBLE_FLASH_OFF   = 5,  /* Double flash overlaying 'off' */
      CELLO_VII_LED_DOUBLE_FLASH_ON    = 6   /* Double flash overlaying 'on' */
    } CelloViiLedState_e;

  /*
   *
   * FUNCTION PROTOTYPES
   *
   */

  CelloViiResult_e
  CelloVii_visualIndRequest(CelloViiCommand_e indication);

  CelloViiResult_e
  CelloVii_visualIndGet(CelloViiLed_e led_type, CelloViiLedState_e *led_state);

#ifdef __cplusplus
}
#endif

#endif /* CELLO_VII_H */
