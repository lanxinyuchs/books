/* > Description **************************************************************/
/*
 * Copyright Ericsson AB
 *
 * The copyright to the computer programs herein is the property of Ericsson AB.
 * The programs may be used and/or copied only with the written permission from
 * Ericsson AB or in accordance with the terms conditions stipulated in the
 * agreement/contract under which the programs have been supplied.
 *
 ******************************************************************************/
/**
 * @file pinmux.h
 * @brief pin-mux handling interface
 *
 ******************************************************************************/

#ifdef __cplusplus
extern "C" {
#endif


#ifndef PINMUX_H
#define PINMUX_H

/* > Includes *****************************************************************/
#include <stdint.h>


/* > Defines ******************************************************************/


/* > Type Declarations ********************************************************/

/** @brief Type for handle that the PINMUX interface uses. */
typedef struct pinmux_handle *pinmux_handle_t;

/** @brief The function type that the PINMUX interface uses. */
typedef enum {
	PINMUX_FUNC_TYPE_GPIO = 0,  /** GPIO mode */
	PINMUX_FUNC_TYPE_ALTF1,     /** Alternate function #1 (Peripheral A) */
	PINMUX_FUNC_TYPE_ALTF2      /** Alternate function #2 (Peripheral B) */
} pinmux_func_type_t;

/** @brief The configuration type that the PINMUX interface uses. */
typedef enum {
	PINMUX_CFG_TYPE_PULLSEL = 0,
	PINMUX_CFG_TYPE_IMPSEL,
	PINMUX_CFG_TYPE_SLEW
} pinmux_cfg_type_t;

/** @brief The configuration value that the PINMUX interface uses. */
typedef enum {
	/** values for PINMUX_CFG_TYPE_PULLSEL */
	PINMUX_CFG_VALUE_PULLSEL_NONE  =  0,      /* no pull up/down */
	PINMUX_CFG_VALUE_PULLSEL_UP,              /* pull up */
	PINMUX_CFG_VALUE_PULLSEL_DOWN,            /* pull down */
	/** values for PINMUX_CFG_TYPE_IMPSEL */
	PINMUX_CFG_VALUE_IMPSEL_00,               /* 65 ohm */
	PINMUX_CFG_VALUE_IMPSEL_01,               /* 50 ohm */
	PINMUX_CFG_VALUE_IMPSEL_10,               /* 35 ohm */
	PINMUX_CFG_VALUE_IMPSEL_11,               /* 25 ohm */
	/** values for PINMUX_CFG_TYPE_SLEW */
	PINMUX_CFG_VALUE_SLEW_0,                  /* A slew rate */
	PINMUX_CFG_VALUE_SLEW_1                   /* B slew rate */
} pinmux_cfg_value_t;


/** @brief The status codes that the PINMUX interface returns to the user. */
typedef enum {
	/** Returned when the operation completed successfully */
	PINMUX_STATUS_SUCCESS = 0,
	/** Returned when Invalid parameter supplied */
	PINMUX_STATUS_INVALID_PARAM,
	/** Returned when the operation requested during wrong state */
	PINMUX_STATUS_WRONG_STATE,
	/** Returned when the service is not supported */
	PINMUX_STATUS_UNSUPPORTED,
	/** Returned for other miscellaneous errors */
	PINMUX_STATUS_OTHER
} pinmux_status_t;


/* > Function Declarations ****************************************************/

/**
* @brief Reserves a group of pins.
*
* @param[in]     number_of_pins  The number of pins in the pin array.
* @param[in]     pin             The array of pin numbers to reserve.
*
*                                For Xenon,
*                                Pin number          GPIO Nr
*                                  0 - 107       GPIO_0_0 - GPIO_0_107
*                                108 - 215       GPIO_1_0 - GPIO_1_107
*                                216 - 278       GPIO_2_0 - GPIO_2_62
*                                279 - 336       GPIO_3_0 - GPIO_3_57
*
* @param[out]    handle          Returned handle to reserved pins.
*
* @return        PINMUX_STATUS_SUCCESS       - Success to reserve.
*                PINMUX_STATUS_INVALID_PARAM - One of below causes:
*                                                - number_of_pins is 0 or > 337.
                                                 - pin number in pin[] is > 336.
*                PINMUX_STATUS_WRONG_STATE   - A pin in the array is
*                                              already reserved.
*                PINMUX_STATUS_OTHER         - Internal error.
*
* @b Example:
*
* @verbatim
    #include "stdio.h"
    #include "pinmux.h"

    pinmux_handle_t handle;
    const uint32_t my_pins[] = {290, 291, 292, 293, 294, 295};
    pinmux_status_t status;

    status = pinmux_reserve(sizeof(my_pins)/sizeof(my_pins[0]), my_pins,
                            &handle);
    if(status == PINMUX_STATUS_SUCCESS) {
          printf("My pins are reserved\n");
    }

  @endverbatim
******************************************************************************/
pinmux_status_t pinmux_reserve(uint32_t number_of_pins,
                               const uint32_t pin[],
                               pinmux_handle_t *handle);

/**
* @brief Unreserves a group of pins previously reserved using pinmux_reserve().
*
* @param[in]     handle      Handle to reserved pins.
*
* @return        PINMUX_STATUS_SUCCESS       - Success to unreserve.
*                PINMUX_STATUS_INVALID_PARAM - handle is invalid.
*                PINMUX_STATUS_OTHER         - Internal error.
*
* @pre           The pins must have been reserved by calling pinmux_reserve().
*
* @b Example:
*
* @verbatim
    #include "stdio.h"
    #include "pinmux.h"

    pinmux_handle_t handle;
    pinmux_status_t status;
    const uint32_t my_pins[] = {290, 291, 292, 293, 294, 295};

    status = pinmux_reserve(sizeof(my_pins)/sizeof(my_pins[0]), my_pins,
                            &handle);
    if(status != PINMUX_STATUS_SUCCESS) {
          // Handle the error;
    }

    status = pinmux_unreserve(handle);
    if(status ==  PINMUX_STATUS_SUCCESS){
          printf("My pins are unreserved.\n ");
    }

  @endverbatim
******************************************************************************/
pinmux_status_t pinmux_unreserve(pinmux_handle_t handle);

/**
* @brief Set function of a group of pins.
*
* @param[in]     handle          Handle to reserved pins.
* @param[in]     number_of_pins  The number of pins in the pin array.
* @param[in]     pin             The array of pin numbers whose function type
*                                should be set.
*
*                                For Xenon,
*                                Pin number          GPIO Nr
*                                  0 - 107       GPIO_0_0 - GPIO_0_107
*                                108 - 215       GPIO_1_0 - GPIO_1_107
*                                216 - 278       GPIO_2_0 - GPIO_2_62
*                                279 - 336       GPIO_3_0 - GPIO_3_57

* @param[in]     func_type       Function type for all pins in the pin array.
*
* @return        PINMUX_STATUS_SUCCESS       - Success to set pin's function.
*                PINMUX_STATUS_INVALID_PARAM - One of below causes:
*                                                - handle is invalid.
*                                                - number_of_pins is 0 or > 337.
*                                                - pin number in pin[] is > 336.
*                                                - func_type is invalid.
*                PINMUX_STATUS_UNSUPPORTED   - func_type is not supported.
*                PINMUX_STATUS_OTHER         - Internal error.
*
*
* @pre           The pins must have been reserved by calling pinmux_reserve().
*
* @b Example:
*
* @verbatim
    #include "stdio.h"
    #include "pinmux.h"

    pinmux_handle_t handle;
    pinmux_status_t status;

    const uint32_t my_pins[] = {290, 291, 292, 293, 294, 295};

    status = pinmux_reserve(sizeof(my_pins)/sizeof(my_pins[0]), my_pins,
                            &handle);
    if(status != PINMUX_STATUS_SUCCESS) {
          // Handle the error;
    }

    status = pinmux_set_func(handle, 2, &my_pins[0], PINMUX_FUNC_TYPE_ALTF2);

    if(status == PINMUX_STATUS_SUCCESS) {
         printf("Pin 290 and 291 are set to
                 PINMUX_FUNC_TYPE_ALTF2.\n");
    }

    status = pinmux_set_func(handle, 4, &my_pins[2], PINMUX_FUNC_TYPE_ALTF1);

    if(status == PINMUX_STATUS_SUCCESS) {
         printf("Pin 292, 293, 294 and 295 are set to
                 PINMUX_FUNC_TYPE_ALTF1.\n");
    }

  @endverbatim
******************************************************************************/
pinmux_status_t pinmux_set_func(pinmux_handle_t handle,
                                uint32_t number_of_pins,
                                const uint32_t pin[],
                                pinmux_func_type_t func_type);


/**
* @brief Get the pin function.
*
* @param[in]     handle     Handle to reserved pins.
* @param[in]     pin        The pin number to get function type for.
*
*                           For Xenon,
*                           Pin number             GPIO Nr
*                              0 - 107       GPIO_0_0 - GPIO_0_107
*                            108 - 215       GPIO_1_0 - GPIO_1_107
*                            216 - 278       GPIO_2_0 - GPIO_2_62
*                            279 - 336       GPIO_3_0 - GPIO_3_57
*
* @param[out]    func_type  Function type.
*
* @return        PINMUX_STATUS_SUCCESS       - Success to get the pin's
*                                              function.
*                PINMUX_STATUS_INVALID_PARAM - One of below causes:
*                                                - handle is invalid.
*                                                - number_of_pins is 0 or > 337.
*                                                - pin number is > 336
*                PINMUX_STATUS_OTHER         - Internal error.
*
* @pre           The pins must have been reserved by calling pinmux_reserve().
*
* @b Example:
*
* @verbatim
    #include "stdio.h"
    #include "pinmux.h"

    pinmux_handle_t handle;
    pinmux_status_t status;
    pinmux_func_type_t func_type;

    const uint32_t my_pins[] = {290, 291, 292, 293, 294, 295};

    status = pinmux_reserve(sizeof(my_pins)/sizeof(my_pins[0]), my_pins,
                            &handle);
    if(status != PINMUX_STATUS_SUCCESS) {
          // Handle the error;
    }

    status = pinmux_get_func(handle, my_pins[0], &func_type);
    if(status == PINMUX_STATUS_SUCCESS) {
          printf("My pin's function type is %d.\n", func_type);
    }

  @endverbatim
******************************************************************************/
pinmux_status_t pinmux_get_func(pinmux_handle_t handle,
                                const uint32_t pin,
                                pinmux_func_type_t *func_type);

/**
* @brief Set configuration of a group of pins.
*
* @param[in]     handle           Handle to reserved pins.
* @param[in]     number_of_pins   The number of pins in the pin array.
* @param[in]     pin              The array of pin numbers to configure.
*
*                                 For Xenon,
*                                 Pin number             GPIO Nr
*                                   0 - 107       GPIO_0_0 - GPIO_0_107
*                                 108 - 215       GPIO_1_0 - GPIO_1_107
*                                 216 - 278       GPIO_2_0 - GPIO_2_62
*                                 279 - 336       GPIO_3_0 - GPIO_3_57
*
* @param[in]     cfg_type         Configuration type for the pin.
* @param[in]     cfg_value        Configuration value for the pin.
*
* @return        PINMUX_STATUS_SUCCESS       - Success to set the
*                                              pin's configuration.
*                PINMUX_STATUS_INVALID_PARAM - One of below causes:
*                                                - handle is invalid.
*                                                - number_of_pins is 0 or > 337.
*                                                - pin number in pin[] is > 336.
*                                                - cfg_type is invalid.
*                                                - cfg_value is invalid.
*                PINMUX_STATUS_UNSUPPORTED   - cfg_type/cfg_value is not
*                                              supported.
*                PINMUX_STATUS_OTHER         - Internal error.
*
* @pre           The pins must have been reserved by calling pinmux_reserve().
*
* @b Example:
*
* @verbatim
    #include "stdio.h"
    #include "pinmux.h"

    pinmux_handle_t handle;
    pinmux_status_t status;

    const uint32_t my_pins[] =  {290, 291, 292, 293, 294, 295};


    status = pinmux_reserve(sizeof(my_pins)/sizeof(my_pins[0]), my_pins,
                            &handle);
    if(status != PINMUX_STATUS_SUCCESS) {
          // Handle the error;
    }

    // Configure all the pins in my_pins array pull down.
    status = pinmux_set_cfg(handle, sizeof(my_pins)/sizeof(my_pins[0]),
                            my_pins, PINMUX_CFG_TYPE_PULLSEL,
                            PINMUX_CFG_VALUE_PULLSEL_DOWN);

    if(status ==  PINMUX_STATUS_SUCCESS){
          printf("My pin's configuration is set successfully.\n");
    }

  @endverbatim
******************************************************************************/
pinmux_status_t pinmux_set_cfg(pinmux_handle_t handle,
                               uint32_t number_of_pins,
                               const uint32_t pin[],
                               pinmux_cfg_type_t cfg_type,
                               pinmux_cfg_value_t cfg_value);

#endif  /* PINMUX_H */


#ifdef __cplusplus
}                               /* extern "C" */
#endif
