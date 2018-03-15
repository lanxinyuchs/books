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
 *      XDP
 *
 * File:
 *      xdai_xdmmi_if.h
 *
 * Author:
 *      Magnus Lindgren (xmaglin)
 *
 * Description:
 *      Application interface functions for common MMI button subscription
 *
 * Reviewed:
 *
 * Revision history:
 *      2000-12-10, Magnus Lindgren (xmaglin)
 *              First version.
 *
 *****************************************************************************/

#ifndef _XDAI_XDMMI_IF_H
#define _XDAI_XDMMI_IF_H

/*----------------------------  Include files  ------------------------------*/
#include "ose.h"                /* OS library */
#include "osetypes.h"
#include "rhd-mmi-if.h"

#ifdef __cplusplus
extern "C" {
#endif

/*----------------------------  CONSTANTS  ----------------------------------*/


/* Return values for
 * XDAI_SubscribeMaintenanceState(PROCESS) &  XDAI_SetMaintenanceState(U32)
 */
#define XDAI_SUBSCRIBE_SUCCESS    MMI_SUBSCRIBE_SUCCESS
#define XDAI_SET_SUCCESS          MMI_SET_SUCCESS
#define XDAI_BUTTON_PRESSED       MMI_BUTTON_PRESSED
#define XDAI_SET_ERROR            MMI_SET_ERROR
#define XDAI_SUBSCRIBE_ERROR      MMI_SUBSCRIBE_ERROR

/* State values valid in
 * XDAI_MAINTENANCE_STATE_IND & XDAI_SetMaintenanceState(U32)
 */
#define XDAI_MAINTENANCE_STATE_DEACTIVATED         MMI_MAINTENANCE_STATE_DEACTIVATED
#define XDAI_MAINTENANCE_STATE_SUPPRESS_ALARMS     MMI_MAINTENANCE_STATE_SUPPRESS_ALARMS
#define XDAI_MAINTENANCE_STATE_REMOVING_TRAFFIC    MMI_MAINTENANCE_STATE_REMOVING_TRAFFIC
#define XDAI_MAINTENANCE_STATE_MAINTENANCE_MODE    MMI_MAINTENANCE_STATE_MAINTENANCE_MODE

#define XDAI_MAINTENANCE_STATE_IND  RHD_MMI_STATE_IND
/*----------------------------  MACROS  -------------------------------------*/

/*----------------------------  Structs and typedefs  -----------------------*/
#define XDAI_MaintenanceStateIndS  struct mmi_state_ind

/*----------------------------  Declaration of Global Variables  ------------*/

/*----------------------------  Declaration of Global Functions  ------------*/

/******************************************************************************
 *
 * Global function:
 *      XDAI_SubscribeMaintenanceState
 *
 * Parameters:
 *      pid - subscriber process id
 *
 * Return value:
 *      XDAI_SUBSCRIBE_SUCCESS
 *      XDAI_SUBSCRIBE_ERROR
 *
 * Description:
 *      This procedure is used to subscribe for XDP maintenance state changes
 *      caused by the maintenance button or XDAI. When someone triggers a state
 *      change, subscriber process receives a XDAI_MAINTENANCE_STATE_IND with
 *      the new maintenance state.
 *
 *      A subbscription results in a success
 *      or error code and also a XDAI_MAINTENANCE_STATE_IND with the current
 *      state.
 *
 *      There can be only one registered subscriber. A subsequent call can be
 *      used to change subscriber or to get the current maintenance state.
 *
 *      XDAI_MAINTENANCE_STATE_IND specifies one of the following states:
 *        - XDAI_MAINTENANCE_STATE_DEACTIVATED
 *        - XDAI_BUTTON_PRESSED (not a real state, i.e. action)
 *        - XDAI_MAINTENANCE_STATE_SUPPRESS_ALARMS
 *        - XDAI_MAINTENANCE_STATE_REMOVING_TRAFFIC
 *        - XDAI_MAINTENANCE_STATE_MAINTENANCE_MODE
 *
 * Side effects:
 *      -
 *
 *****************************************************************************/
extern U32 XDAI_SubscribeMaintenanceState(PROCESS pid);


/******************************************************************************
 *
 * Global function:
 *      XDAI_SetMaintenanceState
 *
 * Parameters:
 *      state - the maintenance state to set
 *
 * Return value:
 *      XDAI_SET_SUCCESS     - a successful state change
 *      XDAI_SET_ERROR       - not successful state change
 *      XDAI_BUTTON_PRESSED  - the operation was ignored since field operator
 *                             holds the maintenance button
 *
 * Description:
 *      This procedure is used when application wants to set the maintenance
 *      state. Only state XDAI_MAINTENANCE_STATE_DEACTIVATED will survive
 *      restart.
 *      Valid states are:
 *        - XDAI_MAINTENANCE_STATE_DEACTIVATED
 *        - XDAI_MAINTENANCE_STATE_SUPPRESS_ALARMS
 *        - XDAI_MAINTENANCE_STATE_REMOVING_TRAFFIC
 *        - XDAI_MAINTENANCE_STATE_MAINTENANCE_MODE
 *
 *
 * Side effects:
 *      Set maintenance state and generates XDAI_MAINTENANCE_STATE_IND if
 *      there exists a valid subscriber. If the request was rejected, no
 *      XDAI_MAINTENANCE_STATE_IND is generated.
 *
 *****************************************************************************/
extern U32 XDAI_SetMaintenanceState(U32 state);

#ifdef __cplusplus
}
#endif

#endif /* _XDAI_XDMMI_IF_H */

