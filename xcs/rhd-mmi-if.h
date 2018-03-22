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


#ifndef _RHD_MMI_H
#define _RHD_MMI_H
#include <stdint.h>
#include "conn-establish.h"
#include "rhd-msg-base.h"

/**
        @file rhd-mmi-if.h
        @brief Message interface for Maintanence button server(MMI)

        ### General ###

        This header file describes the message interface for MMI server.
        This file needs to be included in any public interface which
        uses MMI service.
*/
/*----------------------------  CONSTANTS  ----------------------------------*/
#define MMI_SERVER_VERSIONS     1
#define RHD_MMI_MAILBOX         "RHD_MMI"
/* Remove dependency from xdai_xdmmi_if.h*/
/* Return values for
 * XDAI_SubscribeMaintenanceState(PROCESS) &  XDAI_SetMaintenanceState(U32)
 */
#define MMI_SUBSCRIBE_SUCCESS    0
#define MMI_SET_SUCCESS          1
#define MMI_BUTTON_PRESSED       2
#define MMI_SET_ERROR            3
#define MMI_SUBSCRIBE_ERROR      4
/* State values valid in
 * XDAI_MAINTENANCE_STATE_IND & XDAI_SetMaintenanceState(U32)
 */
#define MMI_MAINTENANCE_STATE_DEACTIVATED         5
#define MMI_MAINTENANCE_STATE_SUPPRESS_ALARMS     6
#define MMI_MAINTENANCE_STATE_REMOVING_TRAFFIC    7
#define MMI_MAINTENANCE_STATE_MAINTENANCE_MODE    8

/* Error codes for Reject signals */
#define RHD_MMI_ERROR               1
#define RHD_MMI_BUTTON_PRESSED      2

/**
        ### Message numbers ###
        These messages are used for conn establish mechanism.
        Messages structures can be found in conn-establish.h\n
        @verbatim
        #define MMI_CONN_ESTABLISH_REQ      (RHD_MMI_MSG_BASE + 0x1)
        #define MMI_CONN_ESTABLISH_CFM      (RHD_MMI_MSG_BASE + 0x2)
        #define MMI_CONN_ESTABLISH_REJ      (RHD_MMI_MSG_BASE + 0x3)
        #define MMI_CONN_DISCONNECT_REQ     (RHD_MMI_MSG_BASE + 0x4)
        #define MMI_CONN_DISCONNECT_CFM     (RHD_MMI_MSG_BASE + 0x5)
        #define MMI_CONN_DISCONNECT_REJ     (RHD_MMI_MSG_BASE + 0x6)
        #define MMI_CONN_MONITOR_FWD        (RHD_MMI_MSG_BASE + 0x7)
        @endverbatim

        These messages are used in MMI server.\n
        @verbatim
        #define RHD_MMI_SET_STATE_REQ       (RHD_MMI_MSG_BASE + 0x8)
        #define RHD_MMI_SET_STATE_CFM       (RHD_MMI_MSG_BASE + 0x9)
        #define RHD_MMI_SET_STATE_REJ       (RHD_MMI_MSG_BASE + 0xA)
        #define RHD_MMI_SUBSCRIBE_REQ       (RHD_MMI_MSG_BASE + 0xB)
        #define RHD_MMI_SUBSCRIBE_CFM       (RHD_MMI_MSG_BASE + 0xC)
        #define RHD_MMI_SUBSCRIBE_REJ       (RHD_MMI_MSG_BASE + 0xD)
        #define RHD_MMI_BUTTON_PRESS_IND    (RHD_MMI_MSG_BASE + 0xE)
        #define RHD_MMI_BUTTON_RELEASE_IND  (RHD_MMI_MSG_BASE + 0xF)
        #define RHD_MMI_TIMEOUT             (RHD_MMI_MSG_BASE + 0x10)
        #define RHD_MMI_STATE_IND           (RHD_MMI_MSG_BASE + 0x11)
        @endverbatim
*/
#define RHD_MMI_STRUCTS \
	struct mmi_set_state_req        set_state_req; \
	struct mmi_set_state_cfm        set_state_cfm; \
	struct mmi_set_state_rej        set_state_rej; \
	struct mmi_subscribe_req        subscribe_req; \
	struct mmi_subscribe_cfm        subscribe_cfm; \
	struct mmi_subscribe_rej        subscribe_rej; \
	struct mmi_button_event_ind     button_event;  \
	struct mmi_state_ind            state_ind;

/* Signal defines. */
#define MMI_CONN_ESTABLISH_REQ      (RHD_MMI_MSG_BASE + 0x1)
#define MMI_CONN_ESTABLISH_CFM      (RHD_MMI_MSG_BASE + 0x2)
#define MMI_CONN_ESTABLISH_REJ      (RHD_MMI_MSG_BASE + 0x3)
#define MMI_CONN_DISCONNECT_REQ     (RHD_MMI_MSG_BASE + 0x4)
#define MMI_CONN_DISCONNECT_CFM     (RHD_MMI_MSG_BASE + 0x5)
#define MMI_CONN_DISCONNECT_REJ     (RHD_MMI_MSG_BASE + 0x6)
#define MMI_CONN_MONITOR_FWD        (RHD_MMI_MSG_BASE + 0x7)

#define RHD_MMI_SET_STATE_REQ       (RHD_MMI_MSG_BASE + 0x8)
#define RHD_MMI_SET_STATE_CFM       (RHD_MMI_MSG_BASE + 0x9)
#define RHD_MMI_SET_STATE_REJ       (RHD_MMI_MSG_BASE + 0xA)
#define RHD_MMI_SUBSCRIBE_REQ       (RHD_MMI_MSG_BASE + 0xB)
#define RHD_MMI_SUBSCRIBE_CFM       (RHD_MMI_MSG_BASE + 0xC)
#define RHD_MMI_SUBSCRIBE_REJ       (RHD_MMI_MSG_BASE + 0xD)
#define RHD_MMI_BUTTON_PRESS_IND    (RHD_MMI_MSG_BASE + 0xE)
#define RHD_MMI_BUTTON_MT_IND       (RHD_MMI_MSG_BASE + 0xF)
#define RHD_MMI_BUTTON_RELEASE_IND  (RHD_MMI_MSG_BASE + 0x10)
#define RHD_MMI_TIMEOUT             (RHD_MMI_MSG_BASE + 0x11)
#define RHD_MMI_STATE_IND           (RHD_MMI_MSG_BASE + 0x12)


/*----------------------------  Structs and typedefs  -----------------------*/
/**
        @def RHD_MMI_MSG_HEADER
        a macro for compulsory header fields
*/
#define RHD_MMI_MSG_HEADER      \
	uint32_t msgno;         \
	uint32_t procedure_ref; \
	uint32_t connection_ref;

/**
        @brief MMI set state request struct

        Send to the server when a client wants to change the MMI state
*/
struct mmi_set_state_req {
	RHD_MMI_MSG_HEADER
	uint32_t state;
};
/**
        @brief MMI set state confirm struct

        Return to the client when the MMI state has been changed
*/
struct mmi_set_state_cfm {
	RHD_MMI_MSG_HEADER
};
/**
        @brief MMI set state reject struct

        Return to the client when the MMI state change fails
*/
struct mmi_set_state_rej {
	RHD_MMI_MSG_HEADER
	uint32_t error;
};
/**
        @brief Subscribe to the MMI state change struct

        Send to the server when a client wants to subscribe to the
        MMI state change.
*/
struct mmi_subscribe_req {
	RHD_MMI_MSG_HEADER
	itc_mbox_id_t mbox;
};
/**
        @brief Subscribe to MMI confirm struct

        Return to the client when the subscribe successed
*/
struct mmi_subscribe_cfm {
	RHD_MMI_MSG_HEADER
};
/**
        @brief Subscribe to MMI reject struct

        Return to the client when the subscribe failed
*/
struct mmi_subscribe_rej {
	RHD_MMI_MSG_HEADER
	itc_mbox_id_t error;
};
/**
        @brief MMI button event indicaiton struct

        Send to the MMI server when the button is pressed or released
*/
struct mmi_button_event_ind {
	RHD_MMI_MSG_HEADER
};
/**
        @brief MMI state indication

        Send to the subscribe client when the MMI state is changed
*/
struct mmi_state_ind {
	uint32_t sigNo;
	uint32_t state;
};
/*----------------------------  Function Definitions  -----------------------*/




#endif /* _RHD_MMI_H */

