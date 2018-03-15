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
 *      XPAI/XTH
 *
 * File:
 *      xpai_xth_if.h
 *
 * Author:
 *      Thomas Lundström   12 Sep 2000
 *
 * Description:
 *      This file defines XPAI XTH external interface functions.
 *
 * Reviewed:
 *      2000-10-05 qrahsto, qrahalt
 *
 * Revision history:
 *      2000-09-12, Thomas Lundström (qraluth)
 *              Created.
 *
 *      2000-09-19, Thomas Lundström (qraluth)
 *              Renamed the interface function:
 *                xth_getOperationalTime() -> XPAI_getOperationalTime().
 *
 *      2005-02-02 Fredrik Andersson (qandfre)
 *              Did some changes to fulfill the used coding standards.
 *
 *      2005-03-01 Fredrik Andersson (qandfre)
 *              Added code for XPSIM.
 *
 *      2006-04-20 Sven Löfgren (qlofsve)
 *              Updated with FUNC-tags. (Wanja).
 *
 *****************************************************************************/

#ifndef _XPAI_XTH_H
#define _XPAI_XTH_H

/*----------------------------  Include files  ------------------------------*/

#include "osetypes.h"

#ifdef __cplusplus
extern "C" {
#endif

/*----------------------------  CONSTANTS  ----------------------------------*/

/* XPSIM stub XPAI functions. */
#ifdef SOFT
#define XPAI_FNO_GET_OPERATIONAL_TIME ((U32)XPAI_GetOperationalTime)
#endif

/*----------------------------  MACROS  -------------------------------------*/

/*----------------------------  Structs and typedefs  -----------------------*/

/*----------------------------  Declaration of Global Variables  ------------*/

/*----------------------------  Declaration of Global Functions  ------------*/

/******************************************************************************
 *
 * Global function:
 *      XPAI_GetOperationalTime
 *
 * Parameters:
 *      None.
 *
 * Return value:
 *      Returns the AUM's total operational time in hours.
 *
 * Description:
 *      Orders XTH to calculate the AUM's total operational time from its
 *      first start.
 *
 * Side effects:
 *      None.
 *
 *****************************************************************************/
extern U32 XPAI_GetOperationalTime(void); /* !- FUNC -! */

#ifdef __cplusplus
}
#endif

#endif /* _XPAI_XTH_H */
