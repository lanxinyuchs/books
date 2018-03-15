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
 *      XPAI/XMMI
 *
 * File:
 *      xpai_xmmi_if.h
 *
 * Author:
 *      2000-02-21 Magnus Isaksson (qramais)
 *
 * Description:
 *      This file defines XPAI XMMI external interface functions.
 *
 * Reviewed:
 *      2000-08-11 Thomas Lundström (qraluth) et al.
 *
 * Revision history:
 *      2000-02-21, Magnus Isaksson (qramais)
 *              First Draft
 *
 *      2000-08-15, Thomas Lundström (qraluth)
 *              Updated after review
 *              Renamed:
 *                xp_led.h -> xpai_xmmi_if.h
 *
 *      2000-10-19, Thomas Lundström (qraluth)
 *              TR 18011.
 *              Renamed function:
 *                xmmiControlLed -> XPAI_ControlLed
 *
 *      2000-11-27 Thomas Lundström (qraluth)
 *              XMMI is changed to be cello like (see 1/15519-CEH10190/1 Uen)
 *
 *      2004-02-17 Peter Bergsten (qrapebe)
 *              Implemented CR: WRNac00489.
 *
 *      2006-03-20 Sven Löfgren (qlofsve)
 *              Updated for Combined Interface (Wanja), XPAI_GetVii added.
 *              FUNC-tags added.
 *
 *      2009-12-06 Magnus Lindgren (xmaglin)
 *              Updated for common MMI, RBS6000 support
 *
 *      2010-01-26 Magnus Lindgren (xmaglin)
 *              WRNae76439 "Inconsistent LED state values in XPAI_DELIV_IND"
 *              Changed value of XPAI_VII_POWER_LOADING_SW
 *
 *      2010-02-12 Magnus Lindgren (emagpal)
 *              CR WRNae78089 - separate between XPAI_VII_BOARD_BUSY_START
 *                              and  XPAI_VII_M_ALARMS_SUPPRESSED
 *
 *      2014-11-11, Amir Mohammad Koosha (eamikoo)
 *              Add support for GPIO controlled LEDs for WARP3
 *****************************************************************************/

#ifndef XPAI_XMMI_H
#define XPAI_XMMI_H

/*----------------------------  Include files  ------------------------------*/
#include "ose.h"                /* OS library */
#include "osetypes.h"

#include "rhd-vii-if.h"

#ifdef __cplusplus
extern "C" {
#endif

/*----------------------------  CONSTANTS  ----------------------------------*/

/* Return values for XPAI_Vii
 * Success:
 *   XPAI_VII_OK
 * Error code:
 *   XPAI_VII_INVALID_REQ  The requested indication was not legal.
 */
#define XPAI_VII_OK               0
#define XPAI_VII_INVALID_REQ      1


/* Return values for XPAI_GetVii
 * Success:
 *      XPAI_GET_VII_OK
 * Error code:
 *      XPAI_GET_VII_NOK_SERVER  Server not found.
 *      XPAI_GET_VII_NOK_OTHER   Other error.
 */
#define XPAI_GET_VII_OK          0
#define XPAI_GET_VII_NOK_SERVER  1
#define XPAI_GET_VII_NOK_OTHER   2

/* Return values for XPAI_GetVii2
 * Success:
 *      XPAI_GET_VII2_OK
 * Error code:
 *      XPAI_GET_VII2_NOK_SERVER         Server not found.
 *      XPAI_GET_VII2_NOK_OTHER          Other error.
 *      XPAI_GET_VII2_NOK_NOT_AVAILABLE  LED ID is not
 *                                       configured/available/valid.
 */
#define XPAI_GET_VII2_OK                0
#define XPAI_GET_VII2_NOK_SERVER        1
#define XPAI_GET_VII2_NOK_OTHER         2
#define XPAI_GET_VII2_NOK_NOT_AVAILABLE 3

/* LEDs select */
#define XPAI_VII_FAULT_LED                    VII_FAULT_LED       /* Red */
#define XPAI_VII_OPERATIONAL_LED              VII_OPERATIONAL_LED /* Green */
#define XPAI_VII_INFORMATION_LED              VII_INFORMATION_LED /* RBS3000: Yellow */
#define XPAI_VII_MAINTENANCE_LED              VII_INFORMATION_LED /* RBS6000: Blue */
#define XPAI_VII_SPECIAL0_LED                 VII_SPECIAL_LED


/* Parameter values (requests) for parameter indication to XPAI_Vii.
 * Some of the values are returned in XPAI_GetVii parameters redLed, greenLed
 * and yellowLed.
 */

/* Red led indication request. */
#define XPAI_VII_FAULT                        VII_FAULT
#define XPAI_VII_ERROR                        VII_ERROR

/* Green led indication request. */
#define XPAI_VII_NO_POWER                     VII_NO_POWER
#define XPAI_VII_BOOTTEST_START               VII_BOOTTEST_START
#define XPAI_VII_O_BUSY                       VII_O_BUSY
#define XPAI_VII_LOADTEST_START               VII_LOADTEST_START
#define XPAI_VII_MISSING_RESOURCE_START       VII_MISSING_RESOURCE_START
/* enter double flash mode */
#define XPAI_VII_O_LOADING_SW                 VII_O_LOADING_SW

/* Yellow or Blue led indication request. */
#define XPAI_VII_BOARD_LOCKED                 VII_BOARD_LOCKED
#define XPAI_VII_M_FULL_MAINTENANCE_MODE      VII_M_FULL_MAINTENANCE_MODE
#define XPAI_VII_SHUTDOWN_START               VII_SHUTDOWN_START
#define XPAI_VII_M_REMOVING_TRAFFIC           VII_M_REMOVING_TRAFFIC
#define XPAI_VII_BOARD_BUSY_START             VII_BOARD_BUSY_START
#define XPAI_VII_M_ALARMS_SUPPRESSED          VII_M_ALARMS_SUPPRESSED

/* Red led cancel request. */
#define XPAI_VII_NO_FAULT                     VII_NO_FAULT
#define XPAI_VII_NO_ERROR                     VII_NO_ERROR

/* Green led cancel request. */
#define XPAI_VII_POWER                        VII_POWER
#define XPAI_VII_BOOTTEST_END                 VII_BOOTTEST_END
#define XPAI_VII_O_BUSY_END                   VII_O_BUSY_END
#define XPAI_VII_LOADTEST_END                 VII_LOADTEST_END
#define XPAI_VII_MISSING_RESOURCE_END         VII_MISSING_RESOURCE_END
/* exit double flash mode */
#define XPAI_VII_O_LOADING_SW_END             VII_O_LOADING_SW_END

/* Yellow or Blue led cancel request. */
#define XPAI_VII_BOARD_UNLOCKED               VII_BOARD_UNLOCKED
#define XPAI_VII_M_FULL_MAINTENANCE_MODE_END  VII_M_FULL_MAINTENANCE_MODE_END
#define XPAI_VII_SHUTDOWN_END                 VII_SHUTDOWN_END
#define XPAI_VII_M_REMOVING_TRAFFIC_END       VII_M_REMOVING_TRAFFIC_END
#define XPAI_VII_BOARD_BUSY_END               VII_BOARD_BUSY_END
#define XPAI_VII_M_ALARMS_SUPPRESSED_END      VII_M_ALARMS_SUPPRESSED_END

/* Red led status indicator
 * same as XPAI_Vii parameters
 */

/* Green led status indicator
 * same as XPAI_Vii + the following double flash states:
 */
#define XPAI_VII_O_BUSY_LOADING_SW                  VII_O_BUSY_LOADING_SW
#define XPAI_VII_LOADTEST_START_LOADING_SW          VII_LOADTEST_START_LOADING_SW
#define XPAI_VII_MISSING_RESOURCE_START_LOADING_SW  VII_MISSING_RESOURCE_START_LOADING_SW
#define XPAI_VII_POWER_LOADING_SW                   VII_POWER_LOADING_SW

/* Yellow or Blue led status indicator
 * same as XPAI_Vii (below mode is identical to XPAI_VII_BOARD_UNLOCKED)
*/
#define XPAI_VII_M_MAINTENANCE_DEACTIVATED    VII_M_MAINTENANCE_DEACTIVATED

/* Special LED indicator */
#define XPAI_VII_SHIFT_INDICATION             VII_SHIFT_INDICATION
#define XPAI_VII_OFF                          VII_OFF
#define XPAI_VII_0PT5HZ                       VII_05HZ
#define XPAI_VII_STEADY                       VII_STEADY

#ifdef SOFT
#define XPAI_FNO_VII     ((U32)XPAI_Vii)
#define XPAI_FNO_GET_VII ((U32)XPAI_GetVii)
#endif

/*----------------------------  MACROS  -------------------------------------*/

/*----------------------------  Structs and typedefs  -----------------------*/

/*----------------------------  Declaration of Global Variables  ------------*/

/*----------------------------  Declaration of Global Functions  ------------*/

/******************************************************************************
 *
 * Global function:
 *      <XPAI_Subscribe> (see xpai_xmr_if.h)
 *
 * Parameters:
 *      pid                     Process ID of the subscriber.
 *      tag                     "XMMI_ViiState"
 *
 * Return value:
 *      XPAI_DISTRIBUTE_IND     Signal sent to the subscriber
 *      tag                     "XMMI_ViiState"
 *      data                    "Red=<state> Green=<state> Yellow=<state>"
 *
 * Description:
 *      The Vii function can distribute the current LED-state to subscribing
 *      clients upon change. To get informed about the current LED-state, use
 *      the XPAI_Subscribe function with the tag name shown abowe. The
 *      subscribe function  is declared in xpai_xmr_if.h.
 *      When a request is received that actually changes the LED-state, the
 *      new state is distributed to all subscribing clients.
 *      The state for each LED can be extracted from the data field (string)
 *      in the received distribute indication signal. Text inside <>
 *      including the <> is replaced with decimal numbers describing the
 *      LED-state. The LED state numbers are the same as the indications
 *      (defined abowe) used when requesting a new LED state through the
 *      XPAI_Vii function.
 *
 *****************************************************************************/


/******************************************************************************
 *
 * Global function:
 *      XPAI_Vii
 *
 * Parameters:
 *      indication      The indication that is requested. Use the constants
 *                      defined above. The effects of the values are
 *                      described in 1/155 19-CEH 101 90/1 Uen.
 *                      See also XPAI_GetVii.
 *
 * Return value:
 *      XPAI_VII_OK
 *      XPAI_VII_INVALID_REQ
 *
 * Description:
 *      This function is the interface function for the LED control.
 *      It sends the requested change in a signal to the process "XPAI_XMMI"
 *      which handles the changes.
 *
 *      In RBS6000, when parameter XPAI_VII_O_LOADING_SW is given, the green
 *      led is put into double flash mode, see XPAI_GetVii
 *
 *  Side effects:
 *      The function has no side effects outside XMMI.
 *
 *****************************************************************************/
U32 XPAI_Vii(U32 indication); /* !- FUNC -! */

/******************************************************************************
 *
 * Global function: XPAI_GetVii
 *
 * Parameters:
 *
 *  redLed     Pointer to return variable indicating the red led status
 *             or null.
 *
 *   Value (RBS3000/6000)                        State                 Led
 *   -----                                       -----                 ---
 *   XPAI_VII_FAULT                              Fault                 Steady light
 *   XPAI_VII_NO_FAULT                           Default               Out
 *
 *  greenLed   Pointer to return variable indicating the green led status
 *             or null.
 *
 *   Value (RBS3000)                             State                 Led
 *   -----                                       -----                 ---
 *   XPAI_VII_NO_POWER                           Power failure         Out
 *   XPAI_VII_BOOTTEST_START                     Initial boot test     16 Hz
 *   XPAI_VII_LOADTEST_START                     Load/Test in progress 2 Hz
 *   XPAI_VII_MISSING_RESOURCE_START             Dep. resource missing 0.5 Hz
 *   XPAI_VII_POWER                              Default               Steady light
 *
 *   Value (RBS6000)                             State                 Led
 *   -----                                       -----                 ---
 *   XPAI_VII_NO_POWER                           Power failure         Out
 *   XPAI_VII_O_BUSY                             Busy                  16 Hz
 *   XPAI_VII_O_BUSY_LOADING_SW                  Busy
 *                                               + loading sw          16 Hz        + double flash
 *   XPAI_VII_MISSING_RESOURCE_START             Dep. resource missing 0.5 Hz
 *   XPAI_VII_MISSING_RESOURCE_START_LOADING_SW  Dep. resource missing
 *                                               + loading sw          0.5 Hz       + double flash
 *   XPAI_VII_POWER                              Default               Steady light
 *   XPAI_VII_POWER_LOADING_SW                   Default
 *                                               + loading sw          Steady light + double flash
 *
 *  yellowLed/blueLed  Pointer to return variable indicating the yellow led status
 *                     or null.
 *
 *   Value (RBS3000)                             State                 Led
 *   -----                                       -----                 ---
 *   XPAI_VII_BOARD_LOCKED                       Board locked          Steady light
 *   XPAI_VII_SHUTDOWN_START                     Shutdown              0.5 Hz
 *   XPAI_VII_BOARD_BUSY_START                   Board busy            16 Hz
 *   XPAI_VII_BOARD_UNLOCKED                     Default               Out
 *
 *   Value (RBS6000)                             State                 Led
 *   -----                                       -----                 ---
 *   XPAI_VII_M_FULL_MAINTENANCE_MODE            Board locked          Steady light
 *   XPAI_VII_M_REMOVING_TRAFFIC                 Shutdown              0.5 Hz
 *   XPAI_VII_M_ALARMS_SUPPRESSED                Board busy            16 Hz
 *   XPAI_VII_M_MAINTENANCE_DEACTIVATED          Default               Out
 *
 *
 * Return value:
 *      XPAI_GET_VII_OK
 *      XPAI_GET_VII_NOK_SERVER
 *      XPAI_GET_VII_NOK_OTHER
 *
 * Description:
 *      Returns the current status for each led.
 *
 *      In RBS6000 the green led can have normal blinking behaviour or be in double flash mode.
 *      Double Flash (described as <Led> + double flash above) is identified via the following
 *      repeated led sequence:
 *      | 300ms off | 350ms on | 300ms off | 350ms on | 300ms off | 2400ms <Led> |
 *
 *      If XPAI_GET_VII_OK is returned the values in the return variables are
 *      valid, indicating led status.
 *
 *      If a null pointer is passed no assignment will take place using
 *      this pointer.
 *
 *      If not XPAI_GET_VII_OK is returned the values in the return variables
 *      are unvalid.
 *
 *****************************************************************************/
extern U32 XPAI_GetVii(U32 *redLed,     /* !- FUNC -! */
                       U32 *greenLed,
                       U32 *yellowLed);

/******************************************************************************
 *
 * Global function: XPAI_GetVii2
 *
 * Parameters:
 *
 *   led: Led ID. Can be XPAI_VII_FAULT_LED (Red), XPAI_VII_OPERATIONAL_LED
 *                  (Green), XPAI_VII_MAINTENANCE_LED (Blue/Yellow) or
 *                  SPECIAL_LED0.
 *
 *   indication: Pointer to return variable indicating the LED status or null.
 *               For red, green and blue/yellow indication description refer to
 *               XPAI_GetVii.
 *               For SPECIAL_LED0, indication may be XPAI_VII_SPECIAL_OFF,
 *               XPAI_VII_SPECIAL_0PR5HZ or XPAI_VII_SPECIAL_STEADY. It is null
 *               when the function is not successful.
 *
 * Return value:
 *      0 = OK
 *      others = Not OK
 *
 * Description:
 *      Returns the current status for requested led.
 *
 *      If 0 is returned the value in the indication is valid,
 *      indicating led status.
 *
 *      If not 0 is returned the values in the indication is not valid.
 *
 *****************************************************************************/
U32 XPAI_GetVii2(U32 led, U32 *indication);   /* !- FUNC -! */

#ifdef __cplusplus
}
#endif

#endif /* XPAI_XMMI_H */

