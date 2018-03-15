/**
 *   @copyright
 *   Copyright (C) 2015 by Ericsson AB. All rights reserved. The
 *   information in this document is the property of Ericsson. Except
 *   as specifically authorized in writing by Ericsson, the receiver
 *   of this document shall keep the information contained herein
 *   confidential and shall protect the same in whole or in part from
 *   disclosure and dissemination to third parties. Disclosure and
 *   disseminations to the receiver's employees shall only be made on
 *   a strict need to know basis.
 */

#ifndef _RHD_VII_H_
#define _RHD_VII_H_
#include <stdint.h>
#include "conn-establish.h"
#include "rhd-msg-base.h"
/**
        @file rhd-vii-if.h
        @brief Message interface for LEDs control server(VII)

        ### General ###

        THis header file describes the message interface for VII server.
        This file needs to be included in any public interface which uses
        LED control service.
*/
#define VII_SERVER_VERSIONS     1
#define RHD_VII_MAILBOX         "RHD_VII"

#define VII_FAULT_LED           1              /* Red */
#define VII_OPERATIONAL_LED     2              /* Green */
#define VII_INFORMATION_LED     3              /* Blue */
#define VII_SPECIAL_LED         (0x1 << 30)    /* yellow */
#define MAX_NAME_LEN            20             /* Max length for client name */

/* Parameter values (requests) for parameter indication to XPAI_Vii.
 * Some of the values are returned in XPAI_GetVii parameters redLed, greenLed
 * and yellowLed.
 */
/* Red led indication request. */
#define VII_FAULT                               0x0001
#define VII_ERROR                               0x1000

/* Green led indication request. */
#define VII_NO_POWER                            0x0002
#define VII_BOOTTEST_START                      0x0004
#define VII_O_BUSY                              0x0004
#define VII_LOADTEST_START                      0x0008
#define VII_MISSING_RESOURCE_START              0x0010
/* enter double flash mode */
#define VII_O_LOADING_SW                        0x0100
#define VII_O_NO_POWER_LOADING_SW               0x0102
#define VII_O_BUSY_LOADING_SW                   0x0104
#define VII_LOADTEST_START_LOADING_SW           0x0108
#define VII_MISSING_RESOURCE_START_LOADING_SW   0x0110
#define VII_POWER_LOADING_SW                    0x00100100

/* Yellow or Blue led indication request. */
#define VII_BOARD_LOCKED                        0x0020
#define VII_M_FULL_MAINTENANCE_MODE             0x0020
#define VII_SHUTDOWN_START                      0x0040
#define VII_M_REMOVING_TRAFFIC                  0x0040
#define VII_BOARD_BUSY_START                    0x0080
#define VII_M_ALARMS_SUPPRESSED                 0x0200

/* Red led cancel request. */
#define VII_NO_FAULT                     (VII_FAULT                   << 16)
#define VII_NO_ERROR                     (VII_ERROR                   << 16)

/* Green led cancel request. */
#define VII_POWER                        (VII_NO_POWER                << 16)
#define VII_BOOTTEST_END                 (VII_BOOTTEST_START          << 16)
#define VII_O_BUSY_END                   (VII_O_BUSY                  << 16)
#define VII_LOADTEST_END                 (VII_LOADTEST_START          << 16)
#define VII_MISSING_RESOURCE_END         (VII_MISSING_RESOURCE_START  << 16)
/* exit double flash mode */
#define VII_O_LOADING_SW_END             (VII_O_LOADING_SW            << 16)

/* Yellow or Blue led cancel request. */
#define VII_BOARD_UNLOCKED               (VII_BOARD_LOCKED            << 16)
#define VII_M_FULL_MAINTENANCE_MODE_END  (VII_M_FULL_MAINTENANCE_MODE << 16)
#define VII_M_MAINTENANCE_DEACTIVATED    (VII_M_FULL_MAINTENANCE_MODE << 16)
#define VII_SHUTDOWN_END                 (VII_SHUTDOWN_START          << 16)
#define VII_M_REMOVING_TRAFFIC_END       (VII_M_REMOVING_TRAFFIC      << 16)
#define VII_BOARD_BUSY_END               (VII_BOARD_BUSY_START        << 16)
#define VII_M_ALARMS_SUPPRESSED_END      (VII_M_ALARMS_SUPPRESSED     << 16)

/* Special LED indicator */
#define VII_SHIFT_INDICATION             13
#define VII_OFF                          (0x0 << VII_SHIFT_INDICATION)
#define VII_05HZ                         (0x1 << VII_SHIFT_INDICATION)
#define VII_STEADY                       (0x2 << VII_SHIFT_INDICATION)
/* Special LED select fields */
#define OPTIONAL_LED_MASK              0xC0000000
/* Special LED indicator fields */
#define OPTIONAL_LED_INDICATION_MASK   0x0000E000

/**
        ### Message numbers ###

        These messages are used for conn establish mechanism.
        Messages structures can be found in conn-establish.h\n
        @verbatim
        #define VII_CONN_ESTABLISH_REQ  (RHD_VII_MSG_BASE + 1)
        #define VII_CONN_ESTABLISH_CFM  (RHD_VII_MSG_BASE + 2)
        #define VII_CONN_ESTABLISH_REJ  (RHD_VII_MSG_BASE + 3)
        #define VII_CONN_DISCONNECT_REQ (RHD_VII_MSG_BASE + 4)
        #define VII_CONN_DISCONNECT_CFM (RHD_VII_MSG_BASE + 5)
        #define VII_CONN_DISCONNECT_REJ (RHD_VII_MSG_BASE + 6)
        #define VII_CONN_MONITOR_FWD    (RHD_VII_MSG_BASE + 7)
        @endverbatim

        These messages are used for LED control.\n
        @verbatim
        #define RHD_VII_LED_CTRL_IND      (RHD_VII_MSG_BASE + 0x9)
        #define RHD_VII_INFO_REQ          (RHD_VII_MSG_BASE + 0xA)
        #define RHD_VII_INFO_CFM          (RHD_VII_MSG_BASE + 0xB)
        #define RHD_VII_DONE_IND          (RHD_VII_MSG_BASE + 0xC)
        #define RHD_VII2_INFO_REQ         (RHD_VII_MSG_BASE + 0xD)
        #define RHD_VII2_INFO_CFM         (RHD_VII_MSG_BASE + 0xE)
        #define RHD_VII2_INFO_REJ         (RHD_VII_MSG_BASE + 0xF)
        #define RHD_VII_LED_CMD_REQ       (RHD_VII_MSG_BASE + 0x10)
        @endverbatim


*/
/* Signal define */
#define RHD_VII_STRUCTS \
	struct vii_ind_req              ind_req; \
	struct vii2_info_cfm            info_cfm; \
	struct vii_info                 led_info; \
	struct vii_cmd_cfm              cmd_cfm;

#define VII_CONN_ESTABLISH_REQ    (RHD_VII_MSG_BASE + 0x1)
#define VII_CONN_ESTABLISH_CFM    (RHD_VII_MSG_BASE + 0x2)
#define VII_CONN_ESTABLISH_REJ    (RHD_VII_MSG_BASE + 0x3)
#define VII_CONN_DISCONNECT_REQ   (RHD_VII_MSG_BASE + 0x4)
#define VII_CONN_DISCONNECT_CFM   (RHD_VII_MSG_BASE + 0x5)
#define VII_CONN_DISCONNECT_REJ   (RHD_VII_MSG_BASE + 0x6)
#define VII_CONN_MONITOR_FWD      (RHD_VII_MSG_BASE + 0x7)

#define RHD_VII_LED_CTRL_IND      (RHD_VII_MSG_BASE + 0x9)
#define RHD_VII_INFO_REQ          (RHD_VII_MSG_BASE + 0xA)
#define RHD_VII_INFO_CFM          (RHD_VII_MSG_BASE + 0xB)
#define RHD_VII_DONE_IND          (RHD_VII_MSG_BASE + 0xC)
#define RHD_VII2_INFO_REQ         (RHD_VII_MSG_BASE + 0xD)
#define RHD_VII2_INFO_CFM         (RHD_VII_MSG_BASE + 0xE)
#define RHD_VII2_INFO_REJ         (RHD_VII_MSG_BASE + 0xF)
#define RHD_VII_LED_CMD_REQ       (RHD_VII_MSG_BASE + 0x10)
#define RHD_VII_LED_CMD_CFM       (RHD_VII_MSG_BASE + 0x11)



/*----------------------------  Structs and typedefs  -----------------------*/

struct clients {
	itc_mbox_id_t      mbox;
	uint32_t           ind_req;
	struct clients     *prev;
	struct clients     *next;
};

struct leds {
	uint32_t red;
	uint32_t blue;
	uint32_t green;
	uint32_t yellow; /* special led */
};

/**     @def RHD_VII_MSG_HEADER
        a macro for compulsory header fields
*/
#define RHD_VII_MSG_HEADER      \
	uint32_t msgno;         \
	uint32_t procedure_ref; \
	uint32_t connection_ref;

/**
        @brief  LED control request struct

        Send to the server when a client wants to set the LEDs
        state or retrieve a certain LED state.
*/
struct vii_ind_req {
	RHD_VII_MSG_HEADER  /**< The compulsory header for conn establish mechanism. */
	uint32_t req;       /**< Request indication. */
};

/**
        @brief Get a certain LED state confirm struct

        Is returned to client when the LED state has been retrieved.
*/
struct vii2_info_cfm {
	RHD_VII_MSG_HEADER  /**<The compulsory header for conn establish mechanism. */
	uint32_t ind;       /**<LED state indication. */
};
/**
        @brief Get all LEDs state confirm struct

        Is returned to client when the LEDs state has been retrieved.
*/
struct vii_info {
	RHD_VII_MSG_HEADER  /**<The compulsory header for conn establish mechanism. */
	struct leds req_masks;  /**<All LEDs state indication */
};

struct vii_cmd_cfm {
	RHD_VII_MSG_HEADER  /**< The compulsory header for conn establish mechanism. */
	uint32_t      ind;       /**< LED state indication. */
	itc_mbox_id_t mbox; /**< client mbox id*/
};


#endif //_RHD_VII_H_
