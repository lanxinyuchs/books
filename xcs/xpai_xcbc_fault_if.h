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
 *      XPAI/XCBC
 *
 * File:
 *      xpai_xcbc_fault_if.h
 *
 * Author:
 *      Peter Bergsten
 *
 * Description:
 *      This file defines XPAI XCBC external interface functions.
 *
 * Reviewed:
 *      2003-01-30 Anders Hallqvist.
 *              IR: 2/1776-65/FCP1034405
 *
 * Revision history:
 *      2002-11-28 Peter Bergsten
 *              Created
 *      2003-03-25 Peter Bergsten
 *              Updated for Wega I3
 *      2003-10-22 Peter Bergsten
 *              Added the X2_TestFault interface
 *
 *      2006-03-20 Sven Löfgren (qlofsve)
 *              Updated for Combined Interface (Wanja). XPAI_SubscribeFaults,
 *              XPAI_FaultClear and XPAI_FAULT_IND added.
 *
 *****************************************************************************/

#ifndef XPAI_XCBC_FAULT_IF_H
#define XPAI_XCBC_FAULT_IF_H

/*----------------------------  Include files  ------------------------------*/

#include <ose.h>
#include <osetypes.h>
#include "xcbc_fault_if.h"

#ifdef __cplusplus
extern "C" {
#endif

/*----------------------------  CONSTANTS  ----------------------------------*/

/* Fault types. */
#define XPAI_GENERAL_SW_ERROR             XCBC_GENERAL_SW_ERROR
#define XPAI_GENERAL_HW_ERROR             XCBC_GENERAL_HW_ERROR


/* Recovery actions. */
#define XPAI_NO_SUGGESTED_ACTION          XCBC_NO_SUGGESTED_ACTION
#define XPAI_ALIGN_STATES                 XCBC_ALIGN_STATES
#define XPAI_ENTITY_RESTART               XCBC_ENTITY_RESTART
#define XPAI_ENTITY_FAILURE               XCBC_ENTITY_FAILURE
#define XPAI_ENTITY_DEGRADED              XCBC_ENTITY_DEGRADED


/* Return values for XPAI_Fault and X2_TestFault. */
#define XPAI_FAULT_NOT_OK                 XCBC_FAULT_NOT_OK
#define XPAI_FAULT_OK                     XCBC_FAULT_OK


/* General subscribe faults values.
 *   XPAI_SUBSCRIBE_FAULTS_MAX_NO_SUBSCRIBERS  Maximum number of fault
 *                                             subscribers.
 */
#define XPAI_SUBSCRIBE_FAULTS_MAX_NO_SUBSCRIBERS  XCBC_MAX_NOF_FAULT_SUBSCRIBERS


/* Return codes for XPAI_SubscribeFaults.
 * Success:
 *   XPAI_SUBSCRIBE_FAULTS_OK
 * Error codes:
 *   XPAI_SUBSCRIBE_FAULTS_NOK_WRONG_PARAM             Wrong parameter.
 *   XPAI_SUBSCRIBE_FAULTS_NOK_SERVER                  Server not found.
 *   XPAI_SUBSCRIBE_FAULTS_NOK_UNSUPPORTED_CAPABILITY  Too many subscribers.
 *   XPAI_SUBSCRIBE_FAULTS_NOK_OTHER                   Other error.
 */
#define XPAI_SUBSCRIBE_FAULTS_OK                          XCBC_SUBSCRIBE_FAULTS_OK
#define XPAI_SUBSCRIBE_FAULTS_NOK_WRONG_PARAM             XCBC_SUBSCRIBE_FAULTS_NOK_WRONG_PARAM
#define XPAI_SUBSCRIBE_FAULTS_NOK_SERVER                  XCBC_SUBSCRIBE_FAULTS_NOK_SERVER
#define XPAI_SUBSCRIBE_FAULTS_NOK_UNSUPPORTED_CAPABILITY  XCBC_SUBSCRIBE_FAULTS_NOK_UNSUPPORTED_CAPABILITY
#define XPAI_SUBSCRIBE_FAULTS_NOK_OTHER                   XCBC_SUBSCRIBE_FAULTS_NOK_OTHER


/* General faults values.
 *   XPAI_FAULT_MAX_FAULT_DESCRIPTION_STRINGLENGTH  Maximum length of fault
 *                                                  description. Includes
 *                                                  null termination.
 */
#define XPAI_FAULT_MAX_FAULT_DESCRIPTION_STRINGLENGTH  XCBC_MAX_FAULT_DESCR_LEN


/* Return codes for XPAI_FaultClear.
 * Success:
 *   XPAI_FAULT_CLEAR_OK
 * Error codes:
 *   XPAI_FAULT_CLEAR_NOK_WRONG_PARAM  Wrong parameter.
 *   XPAI_FAULT_CLEAR_NOK_SERVER       Server not found.
 *   XPAI_FAULT_CLEAR_NOK_WRONG_STATE  Testing is going on.
 *   XPAI_FAULT_CLEAR_NOK_OTHER        Other error.
 */
#define XPAI_FAULT_CLEAR_OK               XCBC_FAULT_CLEAR_OK
#define XPAI_FAULT_CLEAR_NOK_WRONG_PARAM  XCBC_FAULT_CLEAR_NOK_WRONG_PARAM
#define XPAI_FAULT_CLEAR_NOK_SERVER       XCBC_FAULT_CLEAR_NOK_SERVER
#define XPAI_FAULT_CLEAR_NOK_WRONG_STATE  XCBC_FAULT_CLEAR_NOK_WRONG_STATE
#define XPAI_FAULT_CLEAR_NOK_OTHER        XCBC_FAULT_CLEAR_NOK_OTHER


/* Parameter value for parameter faultType to XPAI_FaultClear.
 *   XPAI_FAULT_CLEAR_FAULT_TYPE_ALL  All fault types.
 */
#define XPAI_FAULT_CLEAR_FAULT_TYPE_ALL  XCBC_CLEAR_FAULT_TYPE_ALL


/* Parameter value for parameter recoveryAction to XPAI_FaultClear.
 *   XPAI_FAULT_CLEAR_RECOVERY_ACTION_ALL  All recovery actions.
 */
#define XPAI_FAULT_CLEAR_RECOVERY_ACTION_ALL  XCBC_CLEAR_RECOVERY_ACTION_ALL


#ifdef SOFT
#define XPAI_FNO_FAULT             ((U32)XPAI_Fault)
#define X2_FNO_TEST_FAULT          ((U32)X2_TestFault)
#define XPAI_FNO_SUBSCRIBE_FAULTS  ((U32)XPAI_SubscribeFaults)
#define XPAI_FNO_FAULT_CLEAR       ((U32)XPAI_FaultClear)
#endif

/* The old XCBC_TestFault anf X2_TestFault are now the same as XPAI_Fault,
   since self test is moved to Uboot. In the future, use XPAI_Fault instead  */
#define X2_TestFault    XPAI_Fault
#define XCBC_TestFault  XPAI_Fault

/* Signal numbers.
 * 0x0100E000 = XCBC_SIGBASE. Redefined here to avoid dependency to xp.h.
 * Offset ranges within the XCBC offset range (0x00 - 0xFF) reserved for this
 * interface:
 *    0x90 - 0x9F
 */
#define XPAI_FAULT_IND  XCBC_FAULT_IND /* !-SIGNO( struct XPAI_FaultIndS )-! */


/*----------------------------  Macros  -------------------------------------*/

/*----------------------------  Structs and Typedefs  -----------------------*/

/******************************************************************************
 * Signal:
 *      XPAI_FAULT_IND
 *
 * Parameters:
 *      faultType            Type of fault.
 *                           Value : XPAI_GENERAL_SW_ERROR
 *                                   XPAI_GENERAL_HW_ERROR
 *      recoveryAction       Recovery action.
 *                           Value: XPAI_NO_SUGGESTED_ACTION
 *                                  XPAI_ALIGN_STATES
 *                                  XPAI_ENTITY_RESTART
 *                                  XPAI_ENTITY_FAILURE
 *                                  XPAI_ENTITY_DEGRADED
 *      faultDescription     Null terminated text string describing the fault.
 *
 * Description:
 *      This signal indicates a detected fault in the XP and is sent to
 *      all XPAI subscribers of fault indications.
 *
 *      A fault is a combination of faultType and recoveryAction.
 *
 *      A fault indication for a detected fault is only sent once, unless:
 *      - the function XPAI_SubscribeFaults is called
 *      - the fault is cleared with the function XPAI_FaultClear
 *      - the function XPAI_SelfTest (see xpai_xcbc_basic_if.h) is called
 *
 *****************************************************************************/
#define XPAI_FaultIndS  xcbc_fault_ind


/*----------------------------  Declaration of Global Variables  ------------*/

/*----------------------------  Declaration of Global Functions  ------------*/

/******************************************************************************
 *
 * Global function:
 *      XPAI_Fault()
 *
 * Parameters:
 *      FaultType            Type of fault.
 *                           Value : XPAI_GENERAL_SW_ERROR
 *                                   XPAI_GENERAL_HW_ERROR
 *      RecoveryAction       Recovery action.
 *                           Value: XPAI_NO_SUGGESTED_ACTION
 *                                  XPAI_ALIGN_STATES
 *                                  XPAI_ENTITY_RESTART
 *                                  XPAI_ENTITY_FAILURE
 *                                  XPAI_ENTITY_DEGRADED
 *      FaultDescription     Null terminated text string describing the fault.
 *
 * Return value:
 *      XPAI_FAULT_OK
 *      XPAI_FAULT_NOT_OK
 *
 * Description:
 *      Request to report a fault.
 *
 *      The fault will be stored in the platform and distributed with a fault
 *      indication, signal XPAI_FAULT_IND for XPAI and CBCI_AU_FAULT_IND for CBCI,
 *      to all subscribers of fault indications.
 *
 *      A fault is a combination of faultType and recoveryAction.
 *
 *****************************************************************************/
extern U32 XPAI_Fault(U16   FaultType,         /* !- FUNC -! */
                      U16   RecoveryAction,
                      char *FaultDescription);

/******************************************************************************
 *
 * Global function:
 *      XPAI_SubscribeFaults
 *
 * Parameters:
 *      pid  Pid (process id) of the subscriber.
 *
 * Return value:
 *      XPAI_SUBSCRIBE_FAULTS_OK
 *      XPAI_SUBSCRIBE_FAULTS_NOK_WRONG_PARAM
 *      XPAI_SUBSCRIBE_FAULTS_NOK_SERVER
 *      XPAI_SUBSCRIBE_FAULTS_NOK_UNSUPPORTED_CAPABILITY
 *      XPAI_SUBSCRIBE_FAULTS_NOK_OTHER
 *
 * Description:
 *      This function is used to make subscriptions for fault indications.
 *      XPAI will not send a fault indication unless a subscriber has made
 *      a fault subscription.
 *
 *      Maximum XPAI_SUBSCRIBE_FAULTS_MAX_NO_SUBSCRIBERS subscribers are
 *      allowed at the same time.
 *
 *      After calling this function fault indications, signal XPAI_FAULT_IND,
 *      for all active faults (faults present at the moment) will be sent to
 *      the subscriber indicated with the pid parameter.
 *
 *      A fault is a combination of faultType and recoveryAction.
 *
 *      Fault indications may be sent at any time due to errors that occurs.
 *
 *****************************************************************************/
extern U32 XPAI_SubscribeFaults(U32 pid); /* !- FUNC -! */

/******************************************************************************
 *
 * Global function:
 *      XPAI_UnsubscribeFaults
 *
 * Parameters:
 *      pid  Pid (process id) of the subscriber.
 *
 * Return value: 0: SUCCESS
 *		 1: ERROR
 *
 * Description:
 *     This function is used to delete one subscriber's nodes from the client list ,
 *     and we can find the node by the value of pid.

 *
 *****************************************************************************/
extern U32 XPAI_UnsubscribeFaults(U32 pid); /* !- FUNC -! */

/******************************************************************************
 *
 * Global function:
 *      XPAI_FaultClear
 *
 * Parameters:
 *      faultType            Type of fault.
 *                           Value : XPAI_GENERAL_SW_ERROR
 *                                   XPAI_GENERAL_HW_ERROR
 *                                   XPAI_FAULT_CLEAR_FAULT_TYPE_ALL
 *      recoveryAction       Recovery action.
 *                           Value: XPAI_NO_SUGGESTED_ACTION
 *                                  XPAI_ALIGN_STATES
 *                                  XPAI_ENTITY_RESTART
 *                                  XPAI_ENTITY_FAILURE
 *                                  XPAI_ENTITY_DEGRADED
 *                                  XPAI_FAULT_CLEAR_RECOVERY_ACTION_ALL
 *
 * Return value:
 *      XPAI_FAULT_CLEAR_OK
 *      XPAI_FAULT_CLEAR_NOK_WRONG_PARAM
 *      XPAI_FAULT_CLEAR_NOK_SERVER
 *      XPAI_FAULT_CLEAR_NOK_OTHER
 *
 * Description:
 *      This function is used to clear a fault from the fault list. It also
 *      possible to specify all fault types and/or all recovery actions.
 *
 *      A fault is a combination of faultType and recoveryAction.
 *
 *      If the fault arises after beeing cleared with a call to this function,
 *      the fault will be active again and a fault indication,
 *      signal XPAI_FAULT_IND for XPAI and CBCI_AU_FAULT_IND for CBCI,
 *      will be sent to all subscribers of fault indications.
 *
 *****************************************************************************/
extern U32 XPAI_FaultClear(U16 faultType, U16 recoveryAction); /* !- FUNC -! */

#ifdef __cplusplus
}
#endif

#endif /* XPAI_XCBC_FAULT_IF_H */
