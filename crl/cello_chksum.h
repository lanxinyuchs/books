/**
 *   The contents of this file declares a number of general
 *   checksum calculation functions.
 * 
 *   @file
 *   @version @(#) ClearCase ID: 
 *
 *   Copyright (C) 2013 by Ericsson AB. All rights reserved. The
 *   information in this document is the property of Ericsson. Except
 *   as specifically authorized in writing by Ericsson, the receiver
 *   of this document shall keep the information contained herein
 *   confidential and shall protect the same in whole or in part from
 *   disclosure and dissemination to third parties. Disclosure and
 *   disseminations to the receiver's employees shall only be made on
 *   a strict need to know basis.
 */ 

/* ========================================================================
 *   History of development:
 *   -----------------------
 *   Revised : 2013-05-14 Anette Schött
 *   Change  : Updated according to template and added ifdef for Linux.
 * ========================================================================
 */

/* ===================================================================== */
#ifndef LNX  /* OSE implementation */
/* ===================================================================== */

#ifndef CELLO_CHKSUM_H
#define CELLO_CHKSUM_H
 
/* ========================================================================
 *   INCLUDE FILES
 * ========================================================================
 */

#include <osetypes.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ========================================================================
 *   DEFINITIONS
 * ========================================================================
 */ 

/* ========================================================================
 *   TYPE DEFINITIONS
 * ========================================================================
 */

/* ========================================================================
 *   DATA DECLARATIONS
 * ========================================================================
 */

/**
 * Public function: OMCSF_calculateCRC_8()
 * Description:
 *   This function calculates the CRC-8 checksum of the specified buffer
 *   by using a pre-calculated CRC-8 table. When calculating the checksum,
 *   the generator polynomial X^8 + X^7 + X^2 + 1 is used.
 *
 * Pre-conditions:
 *   None.
 *
 * Post-conditions:
 *   None.
 *
 * Returns:
 *   The CRC-8 checksum of the specified message.
 *
 */
extern U16
OMCSF_calculateCRC_8(
  U8  *buffer,      /* I: The buffer to calculate the CRC-8 checksum for */
  U16  bufferLen,   /* I: The length of the buffer                       */
  U16  initialCRC); /* I: The initial CRC value. This is normally 0.     */


/**
 * Public function: OMCSF_calculateCRC_16()
 * Description:
 *   This function calculates the CRC-16 checksum of the specified buffer
 *   by using a pre-calculated CRC-16 table. When calculating the checksum,
 *   the generator polynomial X^16 + X^12 + X^5 + 1 is used.
 *
 * Pre-conditions:
 *   None.
 *
 * Post-conditions:
 *   None.
 *
 * Returns:
 *   The CRC-16 checksum of the specified message.
 *
 */
extern U16
OMCSF_calculateCRC_16(
  U8  *buffer,      /* I: The buffer to calculate the CRC-16 checksum for */
  U16  bufferLen,   /* I: The length of the buffer                        */
  U16  initialCRC); /* I: The initial CRC value. This is normally 0.      */


#ifdef __cplusplus
}
#endif

#endif /* CELLO_CHKSUM_H */

/* ===================================================================== */
#endif  /* OSE implementation */
/* ===================================================================== */
