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

/******************************************************************************
 *
 * Product name:
 *      XPAI/XHT
 *
 * File:
 *      xpai_xht_if.h
 *
 * Author:
 *      Thomas Lundström  03 Jan 2001
 *
 * Description:
 *      This file defines XPAI XHT external interface functions.
 *
 * Reviewed:
 *
 * Revision history:
 *      2001-01-10 Thomas Lundström (qraluth)
 *              Added the constant XPAI_XHT_CSM_ERROR
 *              Added information about the return value from XPAI_Checksum
 *
 *      2006-04-20 Sven Löfgren (qlofsve)
 *              Updated with FUNC-tags. (Wanja).
 *
 *****************************************************************************/

#ifndef XPAI_XHT_IF_H
#define XPAI_XHT_IF_H

/*----------------------------  Include files  ------------------------------*/


#ifdef __cplusplus
extern "C" {
#endif

/*----------------------------  CONSTANTS  ----------------------------------*/

#define XPAI_XHT_CSM_ERROR      0xffff

#ifdef SOFT
#define XPAI_FNO_CHECKSUM ((U32)XPAI_Checksum)
#endif
/*----------------------------  MACROS  -------------------------------------*/

/*----------------------------  Structs and typedefs  -----------------------*/

/*----------------------------  Declaration of Global Variables  ------------*/

/*----------------------------  Declaration of Global Functions  ------------*/


/******************************************************************************
 *
 * Global function:
 *      XPAI_Checksum
 *
 * Parameters:
 *      startAddr       Address to the first byte.
 *      length          Number of bytes that shall be checksummed.
 *
 * Return value:
 *      If an error occurs (flash read error, invalid parameters ...) then
 *      XPAI_XHT_CSM_ERROR is returned.
 *      If the calculated checksum != 0xffff then the calculated value is
 *      returned, otherwise 0x1111 is returned.
 *
 * Description:
 *      This function is used to calculate a checksum. The checksum is
 *      calculated over length bytes starting at address startAddr. This
 *      function is used to checksum the ABOOT. It could also be
 *      used for checksumming the user area in flash.
 *      The checksum algorithm used is the same as the one used by the
 *      build tools. The algorithm is described in
 *      "XP Application Dependencies", 8/155 19-CEH 101 90/1 Uen
 *
 * Side effects:
 *      -
 *
 *****************************************************************************/
U16 XPAI_Checksum(U8 *startAddr, U32 length); /* !- FUNC -! */

#ifdef __cplusplus
}
#endif

#endif /* XPAI_XHT_IF_H */

