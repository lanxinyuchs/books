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
 *      XPAI/XTE
 *
 * File:
 *      xpai_xte_if.h
 *
 * Author:
 *      Sven Löfgren (xedsven)
 *
 * Description:
 *      This file defines XPAI XTE external interface functions.
 *
 * Reviewed:
 *
 * Revision history:
 *      2010-03-03 Sven Löfgren (xedsven)
 *              CR WRNae63434/78178 Add XPAI_SetUtcTime.
 *      2015-11-11 Lea Suc (eleasuc)
 *              Copied legacy XTE to XPAI, added linking with C++
 *
 *****************************************************************************/

#ifndef XPAI_XTE_IF_H
#define XPAI_XTE_IF_H

/*----------------------------  Include files  ------------------------------*/

#include <osetypes.h>

#ifdef __cplusplus
extern "C" {
#endif

/*----------------------------  Constants  ----------------------------------*/

/* Return codes for XPAI_SetUtcTime.
 * Success:
 *   XPAI_SET_UTC_TIME_OK
 * Error codes:
 *   XPAI_SET_UTC_TIME_NOK_SERVER  Server not found.
 *   XPAI_SET_UTC_TIME_NOK_OTHER   Other error.
 */
#define XPAI_SET_UTC_TIME_OK          0
#define XPAI_SET_UTC_TIME_NOK_SERVER  -1
#define XPAI_SET_UTC_TIME_NOK_OTHER   -2


/* XPSIM stub XPAI functions. */
#ifdef SOFT
#define XPAI_FNO_SET_UTC_TIME  ((S32)XPAI_SetUtcTime)
#endif


/*----------------------------  Macros  -------------------------------------*/

/*----------------------------  Structs and Typedefs  -----------------------*/

/*----------------------------  Declaration of Global Variables  ------------*/

/*----------------------------  Declaration of Global Functions  ------------*/

/******************************************************************************
 *
 * Global function:
 *      XPAI_SetUtcTime()
 *
 * Parameters:
 *      seconds           Number of seconds since Jan. 1, 1970 00:00:00.
 *      microseconds      The microseconds part of the time.
 *
 * Return value:
 *      XPAI_SET_UTC_TIME_OK
 *      XPAI_SET_UTC_TIME_NOK_SERVER
 *      XPAI_SET_UTC_TIME_NOK_OTHER
 *
 * Description:
 *      This function is used to set the OSE time and T&E time.
 *
 *****************************************************************************/
extern S32 XPAI_SetUtcTime(U32 seconds,          /* !- FUNC -! */
			   U32 microseconds);

#ifdef __cplusplus
}
#endif

#endif /* XPAI_XTE_IF_H */

