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
 * @file     ecb_fcs.h
 *
 * @brief    EC-bus HDLC Frame Check Sequence
 *
 *           This file provides an interface for HDLC FCS calculations.
 */

#ifndef ECB_FCS_H_
#define ECB_FCS_H_

#include <stdint.h>


/** Initial shift register (fcs) value. */
#define ECB_FCS_INIT     0xffff

/** Resulting good shift register (fcs) value. */
#define ECB_FCS_GOOD     0xf0b8

/** CRC-16-CCITT table lookup macro. */
#define ECB_FCS(fcs, c)                                                 \
	((uint16_t) (((fcs) >> 8) ^ ecb_fcs_tab[((fcs) ^ (c)) & 0xff]))

/** CRC-16-CCITT lookup table. */
const uint16_t ecb_fcs_tab[256];

#endif
