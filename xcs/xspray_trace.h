/******************************************************************************
 *
 *      COPYRIGHT (C) Ericsson Radio Systems AB, Sweden
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
 *      XSPRAY,  CAH 109 2191
 *
 * File:
 *      xspray_trace.h
 *
 * Author:
 *      Göran Nordin (egornor)
 *
 * Description:
 *      This header file include trace header files.
 *
 * Revision history:
 *      2012-08-23 Göran Nordin (egornor)
 *                 HQ34850, XSPRAY often hangs shell and can,t verify high prio
 *                 message echoed to RU.
 *                 First version.
 */

#ifndef _XSPRAY_TRACE_H_
#define _XSPRAY_TRACE_H_

/*----------------------------  Include files  ------------------------------*/
//#define XPRAY_ENABLE_TRACE /*FIX ME UP. Should use libtri*/

#ifdef XPRAY_ENABLE_TRACE

#if defined XP_TARGET

#include "xpai_xte_if_ose.h"
#include "xpai_xte_if_trace.h"

#elif defined UP_TARGET

#include "upai_ute_if_ose.h"
#include "upai_ute_if_trace.h"

#else

#include "cello_te_ose.h"
#include "cello_te_trace.h"

#endif

#else

#undef ENTER
#define ENTER(MSG) ((void) 0)

#undef  RETURN
#define RETURN ((void) 0)

#undef  INFO
#define INFO(MSG) ((void) 0)

#undef TRACE
#define TRACE(GROUP,  MSG) ((void) 0)

#undef TRACE_STATE
#define TRACE_STATE(MSG) ((void) 0)

#undef TRACE_BUS_SEND
#define TRACE_BUS_SEND(MSG,DATA,LEN) ((void) 0)

#undef TRACE_BUS_RECEIVE
#define TRACE_BUS_RECEIVE(MSG,DATA,LEN) ((void) 0)

#undef TRACE_SEND_SIG
#define TRACE_SEND_SIG(SIG,RECPID,MSG) ((void) 0)

#undef TRACE_REC_SIG
#define TRACE_REC_SIG(SIG,MSG) ((void) 0)

#undef TRACE_PARAM
#define TRACE_PARAM(MSG) ((void) 0)

#undef TRACE_ERROR
#define TRACE_ERROR(MSG) ((void) 0)

#undef STR
#define STR ((void) 0)

#endif

/*----------------------------  CONSTANTS  ----------------------------------*/

/*----------------------------  MACROS  -------------------------------------*/

/*----------------------------  Structs and typedefs  -----------------------*/

/*----------------------------  Declaration of Global Variables  ------------*/

/*----------------------------  Declaration of Global Functions  ------------*/

#endif  /* _XSPRAY_TRACE_H_ */
