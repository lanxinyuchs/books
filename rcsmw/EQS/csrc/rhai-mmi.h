/**
 * @file rhai-mmi.h
 * @brief BRIEF DESCRIPTION MISSING
 */
/* ---------------------------------------------------------------------------
 *
 * © Ericsson AB 2013 All rights reserved.
 * The information in this document is the property of Ericsson. Except
 * as specifically authorized in writing by Ericsson, the receiver of
 * this document shall keep the information contained herein confidential
 * and shall protect the same in whole or in part from disclosure and
 * dissemination to third parties. Disclosure and disseminations to the
 * receivers employees shall only be made on a strict need to know basis.
 *
 * ---------------------------------------------------------------------------
 */

/**
 * @details
 * Description: This file declares the MMI Interface.
 *              The MMI interface provides functions to
 *              set or read the states of the Light Emitting Diodes
 *              and subscribe on MMI button events.
 *
 *              The following Light Emitting Diodes are supported in
 *              the LED driver interface:
 *
 *              Green LED               (Operational LED)
 *              - Red LED               (Fault LED)
 *              - Yellow LED            (Status LED)
 *              - Blue LED              (Maintenance LED)
 *
 *              The following states are supported, for all four types of
 *              Light Emitting Diodes, in the LED driver interface:
 *
 *              - Off (No light)
 *              - On  (Steady light)
 *              - Slow Blink (0.5 Hz)
 *              - Fast Blink (16 Hz)
 *              - Double flash overlaying 'Off (No light)'
 *              - Double flash overlaying 'On  (Steady light)'
 *              - Undefined (used for fault handling)
 *
 *              The state of a Light Emitting Diode is independent of the
 *              states the other Light Emitting Diodes.
 *
 *              The LED state is, for all four types of Light Emitting Diodes,
 *              default set to OFF, which means 'No light'.
 *
 *
 *              The interface also supports fetching of MMI button events. 
 *              The following MMI button events can occur:
 *
 *              - Button pressed     (someone pushed the button)
 *              - 2 seconds elapsed  (the button has been pushed for 2 seconds)
 *              - Button released    (no one is pushing the button anymore)
 *
*/
/** @cond */
#ifdef __cplusplus
extern "C" {
#endif

#ifndef _RHAI_MMI_H_
#define _RHAI_MMI_H_


/* Interface errors */
#define RHAI_MMIERR_SUCCESS     0 /* no error */
#define RHAI_MMIERR_INVAL       1 /* invalid parameter(s) */
#define RHAI_MMIERR_NOMEM       2 /* no memory */
#define RHAI_MMIERR_IO          3 /* no memory */



/*
 * This enum specifies the state/behavior of the
 * Light Emitting Diodes.
 */
enum rhai_mmi_led_state
{
    RHAI_MMI_LED_OFF                = 1,  /* No light */
    RHAI_MMI_LED_ON                 = 2,  /* Steady light */
    RHAI_MMI_LED_SLOW_BLINK         = 3,  /* Blinking 0.5 Hz */
    RHAI_MMI_LED_FAST_BLINK         = 4,  /* Blinking 16 Hz */
    RHAI_MMI_LED_DOUBLE_FLASH_OFF   = 5,  /* Double flash overlaying 'off' */
    RHAI_MMI_LED_DOUBLE_FLASH_ON    = 6   /* Double flash overlaying 'on' */
};

/*
 * This enum specifies the type (which LED) of the Light Emitting Diodes.
*/
enum rhai_mmi_led_type
{
    RHAI_MMI_LED_OPERATIONAL = 1,       /* Green LED  */
    RHAI_MMI_LED_FAULT       = 2,       /* Red LED    */
    RHAI_MMI_LED_STATUS      = 3,       /* Yellow LED */
    RHAI_MMI_LED_MAINTENANCE = 4        /* Blue LED   */
};

/*
 * These flags specify the events of the MMI button.
 */

#define RHAI_MMI_FLAG_BUTTON_PRESSED		0x1
#define RHAI_MMI_FLAG_BUTTON_2SECONDS		0x2
#define RHAI_MMI_FLAG_BUTTON_RELEASED		0x4



/** @endcond*/


/**
 * Set the state of a Light Emitting Diode
 *
 * @param[in] led_type - type of LED (Green, Red, Yellow or Blue)
 * @param[in] led_state - state of the LED (according to enum led_state)
 *
 * @return 0 on success
 * @return <0 - see RHAI_MMIERR_XXX
 */
int rhai_mmi_led_set_state(enum rhai_mmi_led_type led_type,
			   enum rhai_mmi_led_state led_state);


/**
 * Get the state of a Light Emitting Diode
 *
 * @param[in] led_type - type of LED (Green, Red, Yellow or Blue)
 * @param[out] led_state - state of the LED (according to enum led_state)
 *
 * @return 0 on success
 * @return <0 - see RHAI_MMIERR_XXX
 */
int rhai_mmi_led_get_state(enum rhai_mmi_led_type led_type,
			   enum rhai_mmi_led_state *led_state);


/**
 * Initialize MMI button interface
 *
 * @param[out] handle - MMI button interface handle, should be passed
 *                      to all further button interface calls
 * @param[in] exclusive - If nonzero, the caller will be the only client
 *						receiving MMI button events. Exclusive access
 *					  	is provided for non-disruptive testing of the
 *						MMI button. The client is supposed to shutdown
 *						the session within 60 seconds.
 *
 * @return 0 on success
 * @return <0 - see RHAI_MMIERR_XXX
 */
int rhai_mmi_button_init(void **handle, int exclusive);



/**
 * Get MMI button event. The calling thread enters a wait state until
 * woken up by a button event or by a shu
 *
 * @param[in] handle - interface handle
 * @param[out] button_events - current MMI button events
 *
 * @return 0 on success <0 on failure
 * @return -RHAI_MMIERR_XXX
 * @return -EBUSY  if multiple exclusive clients is attempted
 * @return -EUSERS  number of subscribers is limited to 16
 * @return -EBADF  the MMI session (handle) was shut down by a peer thread.
 * @return -ETIMEDOUT  an exclusive subscription is limited to 60 seconds.
 *						If not shutdown by client the server will enforce
 *						shutdown.                    
 */
int rhai_mmi_button_get_event (void *handle, int *button_events);


/**
 * Shutdown MMI button interface
 *
 * @param[in] handle - interface handle
 *
 */
void rhai_mmi_button_shutdown(void *handle);


#endif /* _RHAI_MMI_H_ */

#ifdef __cplusplus
}                               /* extern "C" */
#endif
