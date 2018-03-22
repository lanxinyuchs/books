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
 *      XPAI/XMR
 *
 * File:
 *      xpai_xmr_if.h
 *
 * Author:
 *      Anders Janson, QRADAAA
 *
 * Description:
 *      This file defines XPAI XMR external interface functions.
 *
 * Reviewed:
 *      2000-10-09 Martin Tiselius (QRAMTIS)
 *
 * Revision history:
 *      2000-09-15, Anders Janson (QRADAAA)
 *              Created.
 *      2001-09-14, Anders Wallden (qraawal)
 *              Updated XPAI_DelivCfmS and XPAI_DelivRejS
 *              according to TR55057.
 *      2001-09-18, Anders Janson (QRADAAA)
 *              Added constants XPAI_XMR_TEST_PASSED/FAILED
 *      2001-11-22, Anders Hallqvist (QRAHALT)
 *              Removed dependency of xp.h
 *      2002-11-07, Peter Bergsten (QRAPEBE)
 *              Updated according to C design rule.
 *      2003-07-03, Fredrik Andersson (QANDFRE)
 *              Changed XMR's signal interface for WEGA I4 drop 1.
 *      2003-12-03, Fredrik Andersson (QANDFRE)
 *              Uppdated according to WRNac14968.
 *              Made a major overhaul of the code.
 *      2005-03-03 Fredrik Andersson (qandfre)
 *              Added code for XPSIM.
 *
 *      2006-04-20 Sven Löfgren (qlofsve)
 *              Updated with FUNC-tags. (Wanja).
 *
 *****************************************************************************/

#ifndef _XPAI_XMR_IF_H_
#define _XPAI_XMR_IF_H_

/*----------------------------  Include files  ------------------------------*/

#include "ose.h"
#include "osetypes.h"
/* The event subsription server need its own header file to avoid
   circular dependencies. */
#include "evti.h"

#ifdef __cplusplus
extern "C" {
#endif

/*----------------------------  CONSTANTS  ----------------------------------*/

/*
 * Signal numbers.
 * XMR_SIGBASE = 0x100E200
 * Hardcoded in event_subscription.h in order to remove the dependency on xp.h.
 */

#define XPAI_SUBSCRIBE_IND   EVTI_SUBSCRIBE_IND
#define XPAI_DISTRIBUTE_IND  EVTI_DISTRIBUTE_IND
#define XPAI_DISTRIBUTE_REQ  EVTI_DISTRIBUTE_REQ
#define XPAI_DISTRIBUTE_CFM  EVTI_DISTRIBUTE_CFM
#define XPAI_DISTRIBUTE_REJ  EVTI_DISTRIBUTE_REJ
#define XPAI_DELIV_IND       EVTI_DELIV_IND
#define XPAI_DELIV_REQ       EVTI_DELIV_REQ
#define XPAI_DELIV_CFM       EVTI_DELIV_CFM
#define XPAI_DELIV_REJ       EVTI_DELIV_REJ

/* Maximum length of the tag string. */

#define XMR_MAX_TAG_LENGTH EVTI_MAX_TAG_LENGTH /* = 32 */

/* Result codes from an ordered action:
 * Used by the subscriber in a XPAI_DELIV_CFM signal and
 * passed on to the client in a XPAI_DISTRIBUTE_CFM signal.
 */

#define XPAI_XMR_TEST_PASSED       EVTI_TEST_PASSED        /* 0x00 */
#define XPAI_XMR_TEST_FAILED       EVTI_TEST_FAILED        /* 0x01 */
#define XPAI_XMR_TEST_INTERRUPTED  EVTI_TEST_INTERRUPTED   /* 0x02 */


/* XPSIM stub XPAI functions. */
#ifdef SOFT
#define XPAI_FNO_SUBSCRIBE ((U32)XPAI_Subscribe)
#endif

/*----------------------------  MACROS  -------------------------------------*/

/*----------------------------  Structs and typedefs  -----------------------*/

/*
 * Signal structs.
 *
 * NOTE: To avoid circular dependancies the signal struct of this interface
 * are defined in evti.h
 */
#define  XPAI_SubscribeIndS  EVTI_SubscribeIndS
#define  XPAI_DistributeIndS EVTI_DistributeIndS
#define  XPAI_DistributeReqS EVTI_DistributeReqS
#define  XPAI_DistributeCfmS EVTI_DistributeCfmS
#define  XPAI_DistributeRejS EVTI_DistributeRejS
#define  XPAI_DelivIndS      EVTI_DelivIndS
#define  XPAI_DelivReqS      EVTI_DelivReqS
#define  XPAI_DelivCfmS      EVTI_DelivCfmS
#define  XPAI_DelivRejS      EVTI_DelivRejS

/*----------------------------  Declaration of Global Variables  ------------*/

/*----------------------------  Declaration of Global Functions  ------------*/

/******************************************************************************
 *
 * Global function:
 *      XPAI_Subscribe
 *
 * Parameters:
 *      pid,  Pid of the subscribing process.
 *      tag,  Subscription tag string. Maximum size is 32 bytes including NULL.
 *
 * Return value:
 *      None.
 *
 * Description:
 *      Register 'pid' as a subscriber to 'tag'.
 *      It sends a XPAI_SUBSCRIBE_IND signal to XMR, using 'pid' as sender.
 *
 * Side effects:
 *      None.
 *
 *****************************************************************************/
extern void XPAI_Subscribe(U32 pid, char *tag); /* !- FUNC -! */

#ifdef __cplusplus
}
#endif

#endif /* _XPAI_XMR_IF_H_ */
