/******************************************************************************
 *
 *      COPYRIGHT (C)                 Ericsson Radio Systems AB, Sweden
 *
 *      The copyright to the computer program(s) herein is the property
 *      of Ericsson Radio Systems AB.
 *
 *      The program(s) may be used and/or copied only with the written
 *      permission from Ericsson Radio Systems AB or in accordance with
 *      the terms and conditions stipulated in the agreement/contract
 *      under which the program(s) have been supplied.
 *
 *****************************************************************************/

/**
 *
 * @file     ecb_mux.h
 *
 * @brief    EC-bus MUX Interface
 *
 *           The MUX control is made from a dynamically linked user defined
 *           library. The user defined library implements ecb_dl_mux_prog_fn()
 *           and ecb_dl_mux_set_fn() functions. This file provides an
 *           interface for linkage with the user defined library and the
 *           functions therein.
 *
 *           A MUX profile is a text string associated with a certain MUX HW.
 *
 *           A MUX channel is a number associated with a certain MUX setting.
 *
 *           A MUX program defines how to program a certain MUX HW for a
 *           certain MUX setting.
 *
 */

#ifndef ECB_MUX_H_
#define ECB_MUX_H_


/**
 * @brief
 *
 * Create a MUX program for the provided MUX profile and MUX channel.
 *
 * This function "ecb_dl_mux_prog" is implemented in the user defined library
 * for MUX control.
 *
 * @param prog     Pointer to MUX program pointer.
 * @param profile  MUX profile.
 * @param channel  MUX channel.
 *
 * @return 0 on success, other value on failure.
 *
 */

typedef	int (*ecb_dl_mux_prog_fn)(void **prog, const char *profile,
	                          int channel);


/**
 * @brief
 *
 * Set MUX according to MUX program.
 *
 * This function "ecb_dl_mux_set" is implemented in the user defined library
 * for MUX control.
 *
 * @param prog  Pointer to the MUX program.
 *
 * @return 0 on success, other value on failure.
 *
 */

typedef	int (*ecb_dl_mux_set_fn)(void *prog);


/**
 * @brief
 *
 * Link with users defined library for MUX control.
 *
 * @param handle   Pointer to handle.
 * @param library  Name of the library file.
 *
 * @return 0 on success, other value on failure.
 *
 */

int ecb_mux_init(void *handle, const char *library);


/**
 * @brief
 *
 * Program the MUX setting.
 *
 * @param handle   Pointer to handle.
 * @param profile  MUX profile.
 * @param channel  MUX channel.
 *
 * @return -1 on failure, 1 if function is not linked, otherwise 0.
 *
 */

int ecb_mux_prog(void *handle, const char *profile, int channel);


/**
 * @brief
 *
 * Set MUX according to program.
 *
 * @param handle   Pointer to handle.
 *
 * @return -1 on failure, 1 if function is not linked, otherwise 0.
 *
 */

int ecb_mux_set(void *handle);


#endif
