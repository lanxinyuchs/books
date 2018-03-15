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
 * @file gpio.h
 * @brief GPIO interface
 *
 ******************************************************************************/

#ifdef __cplusplus
extern "C" {
#endif


#ifndef GPIO_H
#define GPIO_H

/* > Includes *****************************************************************/
#include <stdint.h>

/* > Type Declarations ********************************************************/

/** @brief The status codes that the GPIO interface returns to the user. */
typedef enum {
	/** Returned when the operation completed successfully */
	GPIO_STATUS_SUCCESS = 0,
	/** Returned when Invalid parameter supplied */
	GPIO_STATUS_INVALID_PARAM,
	/** Returned when the operation requested during wrong state */
	GPIO_STATUS_WRONG_STATE,
	/** Returned when the service is not supported */
	GPIO_STATUS_UNSUPPORTED,
	/** Returned for other miscellaneous errors */
	GPIO_STATUS_OTHER
} gpio_status_t;

/** @brief The configuration type that the GPIO interface uses. */
typedef enum {
	GPIO_CFG_TYPE_PULLSEL = 0,
	GPIO_CFG_TYPE_IMPSEL,
	GPIO_CFG_TYPE_SLEW
} gpio_cfg_type_t;

/** @brief The configuration value that the GPIO interface uses. */
typedef enum {
	/** values for GPIO_CFG_TYPE_PULLSEL */
	GPIO_CFG_VALUE_PULLSEL_NONE  =  0,      /* no pull up/down */
	GPIO_CFG_VALUE_PULLSEL_UP,              /* pull up */
	GPIO_CFG_VALUE_PULLSEL_DOWN,            /* pull down */
	/** values for GPIO_CFG_TYPE_IMPSEL */
	GPIO_CFG_VALUE_IMPSEL_00,               /* 65 ohm */
	GPIO_CFG_VALUE_IMPSEL_01,               /* 50 ohm */
	GPIO_CFG_VALUE_IMPSEL_10,               /* 35 ohm */
	GPIO_CFG_VALUE_IMPSEL_11,               /* 25 ohm */
	/** values for GPIO_CFG_TYPE_SLEW */
	GPIO_CFG_VALUE_SLEW_0,                  /* A slew rate */
	GPIO_CFG_VALUE_SLEW_1                   /* B slew rate */
} gpio_cfg_value_t;

/** @brief Type for the handle that the GPIO interface uses. */
typedef struct gpio_handle *gpio_handle_t;

/* > Defines ******************************************************************/


/* > Function Declarations ****************************************************/

/**
* @brief Reserve a group of pins in GPIO mode.
*
* @param[in]     number_of_pins  The number of pins in the pin array.
* @param[in]     pin             The array of pin numbers to reserve.
*
*                                 For Xenon,
*                                 Pin number             GPIO Nr
*                                   0 - 107       GPIO_0_0 - GPIO_0_107
*                                 108 - 215       GPIO_1_0 - GPIO_1_107
*                                 216 - 278       GPIO_2_0 - GPIO_2_62
*                                 279 - 336       GPIO_3_0 - GPIO_3_57
*
*                                 For Zynq7000,
*                                 Pin number             GPIO Nr
*                                   0 - 117       GPIO_0 - GPIO_117
*
*                                 For ZU15,
*                                 Pin number             GPIO Nr
*                                   0 - 173       GPIO_0 - GPIO_173
*                                 
* @param[out]    handle          Returned handle to reserved pins.
*
* @return        GPIO_STATUS_SUCCESS       -  Success to reserve pins as GPIO
*                                             mode.
*                GPIO_STATUS_INVALID_PARAM -  One of below causes:
*                                                - number_of_pins is 0 or > 337(Xenon)/118(Zynq7000)/174(ZU15).
*                                                - pin number in pin[] is > 336(Xenon)/117(Zynq7000)/173(ZU15).
*                GPIO_STATUS_WRONG_STATE   -  A pin in the array is already
*                                             reserved.
*                GPIO_STATUS_OTHER         -  Internal error.
*
*
* @b Example:
*
* @verbatim

    #include "gpio.h"

    gpio_status_t status;
    gpio_handle_t handle;
    const uint32_t my_pins[] = {19, 37};

    status = gpio_reserve(sizeof(my_pins)/sizeof(my_pins[0]), my_pins,
                          &handle);
    if(status != GPIO_STATUS_SUCCESS) {
          // Handle the error;
    }

  @endverbatim
******************************************************************************/
gpio_status_t gpio_reserve(uint32_t number_of_pins,
                           const uint32_t pin[],
                           gpio_handle_t *handle);

/**
* @brief Unreserve a group of pins previously reserved using gpio_reserve().
*
* @param[in]     handle         The handle to reserved GPIO pins.
*
* @return        GPIO_STATUS_SUCCESS       -  Success to unreserve.
*                GPIO_STATUS_INVALID_PARAM -  handle is invalid.
*                GPIO_STATUS_OTHER         -  Internal error.
*
* @pre           The pins must have been reserved by calling gpio_reserve().
*
* @b Example:
*
* @verbatim

    #include "gpio.h"

    gpio_status_t status;
    gpio_handle_t handle;

    uint32_t my_pins[] = {19, 37};

    status = gpio_reserve(sizeof(my_pins)/sizeof(my_pins[0]), my_pins,
                          &handle);
    if(status != GPIO_STATUS_SUCCESS) {
          // Handle the error;
    }

    status = gpio_unreserve(handle);
    if(status != GPIO_STATUS_SUCCESS) {
          // Handle the error;
    }

  @endverbatim
******************************************************************************/
gpio_status_t gpio_unreserve(gpio_handle_t handle);

/**
* @brief Set the GPIO pin direction.
*
* @param[in]     handle           Handle to reserved GPIO pins.
* @param[in]     number_of_pins   The number of pins in the pin array.
* @param[in]     pin              The array of pin numbers whose direction
*                                 should be set.
*
*                                 For Xenon,
*                                 Pin number             GPIO Nr
*                                   0 - 107       GPIO_0_0 - GPIO_0_107
*                                 108 - 215       GPIO_1_0 - GPIO_1_107
*                                 216 - 278       GPIO_2_0 - GPIO_2_62
*                                 279 - 336       GPIO_3_0 - GPIO_3_57
*
*                                 For Zynq7000,
*                                 Pin number             GPIO Nr
*                                   0 - 117       GPIO_0 - GPIO_117
*
*                                 For ZU15,
*                                 Pin number             GPIO Nr
*                                   0 - 173       GPIO_0 - GPIO_173
*
* @param[in]     value            The array of directions to set.
*                                     0 - set pin direction to input
*                                 non 0 - set pin direction to output
*
* @return        GPIO_STATUS_SUCCESS       -  Success to set pin direction.
*                GPIO_STATUS_INVALID_PARAM -  One of below causes:
*                                                - handle is invalid.
*                                                - number_of_pins is 0 or > 337/118/174.
*                                                - pin number in pin[] is > 336/117/173.
*                GPIO_STATUS_OTHER         -  Internal error.
*
*
* @pre           The pins must have been reserved by calling gpio_reserve().

* @b Example:
*
* @verbatim
    #include "gpio.h"

    gpio_status_t status;
    gpio_handle_t handle;
    const uint32_t my_pins[] = {19, 37};
    const uint8_t my_values[] = {0, 1};

    status = gpio_reserve(sizeof(my_pins)/sizeof(my_pins[0]), my_pins,
                          &handle);
    if(status != GPIO_STATUS_SUCCESS){
            // Handle the error;
    }
    // Set direction
    status = gpio_set_dir(handle, sizeof(my_pins)/sizeof(my_pins[0]),
                          my_pins, my_values);
    if(status != GPIO_STATUS_SUCCESS){
            // Handle the error;
    }


  @endverbatim
******************************************************************************/
gpio_status_t gpio_set_dir(gpio_handle_t handle,
                           uint32_t number_of_pins,
                           const uint32_t pin[],
                           const uint8_t value[]);

/**
* @brief Get the GPIO direction value.
*
* @param[in]     handle           Handle to reserved GPIO pins.
* @param[in]     number_of_pins   The number of pins in the pin array.
* @param[in]     pin              The array of pin numbers to get direction for.
*
*                                 For Xenon,
*                                 Pin number             GPIO Nr
*                                   0 - 107       GPIO_0_0 - GPIO_0_107
*                                 108 - 215       GPIO_1_0 - GPIO_1_107
*                                 216 - 278       GPIO_2_0 - GPIO_2_62
*                                 279 - 336       GPIO_3_0 - GPIO_3_57
*
*                                 For Zynq7000,
*                                 Pin number             GPIO Nr
*                                   0 - 117       GPIO_0 - GPIO_117
*
*                                 For ZU15,
*                                 Pin number             GPIO Nr
*                                   0 - 173       GPIO_0 - GPIO_173
*
* @param[out]    value            Returned array of directions.
*                                 0 - input
*                                 1 - output
*
* @return        GPIO_STATUS_SUCCESS       -  Success to get pin direction.
*                GPIO_STATUS_INVALID_PARAM -  One of below causes:
*                                                - handle is invalid.
*                                                - number_of_pins is 0 or > 337/118/174.
*                                                - pin number in pin[] is > 336/117/173.
*                GPIO_STATUS_OTHER         -  Internal error.
*
*
* @pre           The pins must have been reserved by calling gpio_reserve().

* @b Example:
*
* @verbatim
    #include "gpio.h"

    gpio_status_t status;
    gpio_handle_t handle;
    uint8_t *my_values;

    const uint32_t my_pins[] = {19, 37};

    status = gpio_reserve(sizeof(my_pins)/sizeof(my_pins[0]), my_pins,
                          &handle);
    if(status != GPIO_STATUS_SUCCESS) {
           // Handle the error;
    }
    my_values = (uint8_t *)malloc(sizeof(my_pins) / sizeof(my_pins[0]) *
                                  sizeof(uint8_t));

    status = gpio_get_dir(handle, sizeof(my_pins)/sizeof(my_pins[0]),
                           my_pins, my_values);
    if(status != GPIO_STATUS_SUCCESS) {
           // Handle the error;
    }


  @endverbatim
******************************************************************************/
gpio_status_t gpio_get_dir(gpio_handle_t handle,
                           uint32_t number_of_pins,
                           const uint32_t pin[],
                           uint8_t value[]);

/**
* @brief Write the GPIO output pin value.
*
*
* @param[in]     handle           Handle to reserved GPIO pins.
* @param[in]     number_of_pins   The number of pins in the pin array.
* @param[in]     pin              The array of pin numbers whose value should
*                                 be set.
*
*                                 For Xenon,
*                                 Pin number             GPIO Nr
*                                   0 - 107       GPIO_0_0 - GPIO_0_107
*                                 108 - 215       GPIO_1_0 - GPIO_1_107
*                                 216 - 278       GPIO_2_0 - GPIO_2_62
*                                 279 - 336       GPIO_3_0 - GPIO_3_57
*
*                                 For Zynq7000,
*                                 Pin number             GPIO Nr
*                                   0 - 117       GPIO_0 - GPIO_117
*
*                                 For ZU15,
*                                 Pin number             GPIO Nr
*                                   0 - 173       GPIO_0 - GPIO_173
*
* @param[out]    value            Value to set.
*                                     0 - set pin value to 0.
*                                 non 0 - set pin value to 1.
*
* @return        GPIO_STATUS_SUCCESS       -  Success to set gpio output pin
*                                             value.
*                GPIO_STATUS_INVALID_PARAM -  One of below causes:
*                                                - handle is invalid.
*                                                - number_of_pins is 0 or > 337/118/174.
*                                                - pin number in pin[] is > 336/117/173.
*                GPIO_STATUS_OTHER         -  Internal error.
*
* @pre           The pins must have been reserved by calling gpio_reserve().

* @b Example:
*
* @verbatim
    #include "gpio.h"

    gpio_status_t status;

    const uint32_t my_pins[] = {19, 37};
    const uint32_t my_values[] = {1, 0};

    status = gpio_reserve(sizeof(my_pins)/sizeof(my_pins[0]), my_pins,
                          &handle);
    if(status != GPIO_STATUS_SUCCESS) {
           // Handle the error;
    }

    status = gpio_write(handle, sizeof(my_pins)/sizeof(my_pins[0]),
                        my_pins, my_values);
    if(status != GPIO_STATUS_SUCCESS){
            // Handle the error;
    }

  @endverbatim
******************************************************************************/
gpio_status_t gpio_write(gpio_handle_t handle,
                         uint32_t number_of_pins,
                         const uint32_t pin[],
                         const uint8_t value[]);

/**
* @brief Read the GPIO pin value.
*
* @param[in]     handle           Handle to reserved GPIO pins.
* @param[in]     number_of_pins   The number of pins in the pin array.
* @param[in]     pin              The array of pin numbers to read.
*
*                                 For Xenon,
*                                 Pin number             GPIO Nr
*                                   0 - 107       GPIO_0_0 - GPIO_0_107
*                                 108 - 215       GPIO_1_0 - GPIO_1_107
*                                 216 - 278       GPIO_2_0 - GPIO_2_62
*                                 279 - 336       GPIO_3_0 - GPIO_3_57
*
*                                 For Zynq7000,
*                                 Pin number             GPIO Nr
*                                   0 - 117       GPIO_0 - GPIO_117
*
*                                 For ZU15,
*                                 Pin number             GPIO Nr
*                                   0 - 173       GPIO_0 - GPIO_173
*
* @param[out]    value            Returned value 0 or 1.
*
* @return        GPIO_STATUS_SUCCESS       -  Success to read GPIO pin value.
*                GPIO_STATUS_INVALID_PARAM -  One of below causes:
*                                                - handle is invalid.
*                                                - number_of_pins is 0 or > 337/118/174.
*                                                - pin number in pin[] is > 336/117/173.
*                GPIO_STATUS_OTHER         -  Internal error.
*
* @pre           The pins must have been reserved by calling gpio_reserve().

* @b Example:
*
* @verbatim
    #include "gpio.h"

    gpio_status_t status;
    const uint32_t my_pins[] = {19, 37};
    uint8_t *my_values;

    status = gpio_reserve(sizeof(my_pins)/sizeof(my_pins[0]), my_pins,
                          &handle);
    if(status != GPIO_STATUS_SUCCESS) {
           // Handle the error;
    }

    my_values = (uint8_t*)malloc(sizeof(my_pins) / sizeof(my_pins[0]) *
                                 sizeof(uint8_t));

    status = gpio_read(handle, sizeof(my_pins)/sizeof(my_pins[0]),
                       my_pins, my_values);
    if(status != GPIO_STATUS_SUCCESS){
            // Handle the error;
    }

  @endverbatim
******************************************************************************/
gpio_status_t gpio_read(gpio_handle_t handle,
                        uint32_t number_of_pins,
                        const uint32_t pin[],
                        uint8_t value[]);


/**
* @brief Set the gpio pin configuration.
*
* @param[in]     handle           Handle to reserved GPIO pins.
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
*                                 For Zynq7000,
*                                 Pin number             GPIO Nr
*                                   0 - 117       GPIO_0 - GPIO_117
*
*                                 For ZU15,
*                                 Pin number             GPIO Nr
*                                   0 - 173       GPIO_0 - GPIO_173
*
* @param[in]     cfg_type         Configuration type for the pin.
* @param[in]     cfg_value        Configuration value for the pin.
*
* @return        GPIO_STATUS_SUCCESS       - Success to set pin's configuration.
*                GPIO_STATUS_INVALID_PARAM - One of below causes:
*                                               - handle is invalid.
*                                               - number_of_pins is 0 or > 337/118/174.
*                                               - pin number in pin[] is > 336/117/173.
*                                               - cfg_type is invalid.
*                                               - cfg_value is invalid.
*                GPIO_STATUS_UNSUPPORTED   - cfg_type/cfg_value is not
*                                            supported.
*                GPIO_STATUS_OTHER         - Internal error.

*
* @pre           The pins must have been reserved by calling gpio_reserve().
*
* @b Example:
*
* @verbatim
    #include "stdio.h"
    #include "gpio.h"

    gpio_handle_t handle;
    gpio_status_t status;

    const uint32_t my_pins[] =  {19, 37};

    status = gpio_reserve(sizeof(my_pins)/sizeof(my_pins[0]), my_pins,
                          &handle);
    if(status != GPIO_STATUS_SUCCESS) {
           // Handle the error;
    }

    // Configure all the pins in my_pins array pull down.
    status = gpio_set_cfg(handle, sizeof(my_pins)/sizeof(my_pins[0]),
                          my_pins, GPIO_CFG_TYPE_PULLSEL,
                          GPIO_CFG_VALUE_PULLSEL_DOWN);

    if(status ==  GPIO_STATUS_SUCCESS){
          printf("My pin's configuration is set successfully.\n");
    }

  @endverbatim
******************************************************************************/
gpio_status_t gpio_set_cfg(gpio_handle_t handle,
                           uint32_t number_of_pins,
                           const uint32_t pin[],
                           gpio_cfg_type_t cfg_type,
                           gpio_cfg_value_t cfg_value);

#endif  /* GPIO_H */


#ifdef __cplusplus
}                               /* extern "C" */
#endif
