/**
 *   This is the header file for the buti interface.
 *
 *   Copyright (C) 2010-2014 by Ericsson AB. All rights reserved. The
 *   information in this document is the property of Ericsson. Except
 *   as specifically authorized in writing by Ericsson, the receiver
 *   of this document shall keep the information contained herein
 *   confidential and shall protect the same in whole or in part from
 *   disclosure and dissemination to third parties. Disclosure and
 *   disseminations to the receiver's employees shall only be made on
 *   a strict need to know basis.
 */

/* ========================================================================
 *   History of development:
 *   -----------------------
 *   Revised : 2010-11-23 Anusha Thota xanutho
 *   Change  : First version.
 *
 *   Revised : 2011-01-21 Anusha Thota xanutho
 *   Change  : Added a constant and changed function parameter name of
 *             CelloButi_changeFeedBackMode after TC control review of BUTI UG.
 *
 *   Revised : 2011-02-02 Anusha Thota xanutho
 *   Change  : Change in  CelloButi_freeMemory function after TC control
 *             review of the BUTI UG.
 *
 *   Revised : 2014-02-27 Per Norberg etxpeno
 *   Change  : Copied from CPP git repo
 *             Removed struct CelloButi_InternalData
 * ========================================================================
 */

#ifndef __CELLO_BUTI_H
#define __CELLO_BUTI_H

#ifdef __cplusplus
extern "C" {
#endif

/* ========================================================================
 *   INCLUDE FILES
 * ========================================================================
 */
#include "ose.h"
#include "osetypes.h"

/* ========================================================================
 *   DEFINITIONS
 * ========================================================================
 */

/* Protocol version */
#define CELLO_BUTI_NO_PV                        (0)
#define CELLO_BUTI_PV1                          (1)

/* Feed Back Mode */
#define CELLO_BUTI_NO_FEEDBACK_BLINK_MODE       (0)
#define CELLO_BUTI_FEEDBACK_BLINK_MODE          (1)

/* BUTI Result */
#define CELLO_BUTI_OK                           (0)
#define CELLO_BUTI_ERROR_SERVER_NOT_UNAVAILABLE (1)
#define CELLO_BUTI_SERVER_NOT_FOUND             (2)
#define CELLO_BUTI_UNKNOWN_SIGNAL               (3)
#define CELLO_BUTI_ERROR_SERVER_NOT_AVAILABLE   (4)
#define CELLO_BUTI_INTERNAL_ERROR               (5)
#define CELLO_BUTI_INVALID_PV                   (6)
#define CELLO_BUTI_MEMORY_NOT_INITIATED         (7)
#define CELLO_BUTI_SERVICE_BUSY                 (8)

/* buttonEventType */
#define CELLO_BUTI_BUTTON_PRESSED               (0)
#define CELLO_BUTI_BUTTON_SHORT_RELEASE         (1)
#define CELLO_BUTI_BUTTON_MEDIUM_RELEASE        (2)


/* ========================================================================
 *   TYPE DEFINITIONS
 * ========================================================================
 */

typedef uint32_t CelloButiResult;

/* ========================================================================
 *   DATA DECLARATIONS
 * ========================================================================
 */

/** ==================================================================== */
/**
 *   Allocate and setup the buti proxy internal data structure.
 *
 *   @param -
 *
 *   @return A pointer to internal memory used by the buti proxy
 *           is returned. The pointer must always be used by
 *           the clients in all the calls to the buti proxy.
 *
 *   @par Globals:
 *                    --
 *
 *   Used for initiation of proxy and reservation of proxy internal memory.
 */
/* ===================================================================== */
void*
CelloButi_initiateMemory(void);


/** ==================================================================== */
/**
 *   Connect to the server.
 *
 *   @param butiMemory_p   - Pointer to data area allocated and returned from
 *                           CelloButi_initiateMemory
 *   @param pvFirstWanted  - The first wanted protocol version
 *   @param pvSecondWanted - The second wanted protocol version
 *   @param pvThirdWanted  - The thirdwanted protocol version
 *   @param clientRef      - Client reference
 *
 *   @return CelloButiResult result of the operation
 *                CELLO_BUTI_OK
 *                 - service initiation signal is sent successfully to server.
 *                CELLO_BUTI_ERROR_SERVER_NOT_UNAVAILABLE
 *                 - The internal proxy state for the client is not UNAVAILABLE
 *                CELLO_BUTI_SERVER_NOT_FOUND
 *                 - The Buti server process not found
 *
 *   @par Globals:
 *                    --
 *
 *   This function should be called when the client wants to connect
 *   to the server. Protocol version negotiation is included in the request.
*/
/* ===================================================================== */
CelloButiResult
CelloButi_initiateService(void *butiMemory_p,
                          uint32_t pvFirstWanted,
                          uint32_t pvSecondWanted,
                          uint32_t pvThirdWanted,
                          uint32_t clientRef);


/** ==================================================================== */
/**
 *  Handle the proxy internal signals.
 *
 *   @param butiMemory_p   - Pointer to data area allocated and returned from
 *                           CelloButi_initiateMemory
 *   @param sig_p          - signal received by the client.
 *
 *   @return  CelloButiResult result of the operation
 *                CELLO_BUTI_OK
 *                 - Call to the function CelloButi_internal is successful
 *                CELLO_BUTI_UNKNOWN_SIGNAL
 *                 - An unknown signal is forwarded to this function
 *
 *   @par Globals:
 *                    --
 *
 *  This function must be called when the client receives the
 *  server response signal. The forwarded signal buffer must be
 *  freed by the client after the return of this function call.
 *  The forwarded signal shall be
 *           CELLO_BUTI_INITIATE_SERVICE_CFM
 *           CELLO_BUTI_INITIATE_SERVICE_REJ
 *           CELLO_BUTI_TERMINATE_SERVICE_CFM
 */
/* ===================================================================== */
CelloButiResult
CelloButi_internal(void *butiMemory_p,
                   union SIGNAL *sig_p);


/** ==================================================================== */
/**
 *   Subscribe for Maintanance button state change events.
 *
 *   @param butiMemory_p   - Pointer to data area allocated and returned from
 *                           CelloButi_initiateMemory
 *   @param clientRef      - Client reference
 *
 *   @return CelloButiResult result of the operation
 *                CELLO_BUTI_OK
 *                 - Subcription for the button events is successful.
 *                CELLO_BUTI_ERROR_SERVER_NOT_AVAILABLE
 *                 - The internal proxy state for the client is not AVAILABLE
 *
 *   @par Globals:
 *                            --
 *
 *   This function is used to subscribe for the button events.
 **/
/* ===================================================================== */
CelloButiResult
CelloButi_subscribeButtonEvent(void *butiMemory_p,
                               uint32_t clientRef);


/** ==================================================================== */
/**
 *  Set/Reset the feedbackblink mode.
 *
 *   @param butiMemory_p   - Pointer to data area allocated and returned from
 *                           CelloButi_initiateMemory
 *   @param feedbackMode   - Feedbackblink mode ON - 1,
 *                           No feedbackblink mode - 0
 *   @param clientRef      - Client reference
 *
 *   @return CelloButiResult result of the operation
 *                CELLO_BUTI_OK
 *                 - Request for changing feedback blink mode is successful.
 *                CELLO_BUTI_ERROR_SERVER_NOT_AVAILABLE
 *                 - The internal proxy state for the client is not AVAILABLE.
 *
 *   @par Globals:
 *                            --
 *
 *  This function is used for setting the mode to either feedback blink mode
 *  or non-feedback blink mode.
 */
/* ===================================================================== */
CelloButiResult
CelloButi_changeFeedBackMode(void *butiMemory_p,
                             uint32_t feedbackMode,
                             uint32_t clientRef);


/** ==================================================================== */
/**
 *   Unsubscribe for the button events.
 *
 *   @param butiMemory_p   - Pointer to data area allocated and returned from
 *                           CelloButi_initiateMemory
 *   @param clientRef      - Client reference
 *
 *   @return CelloButiResult result of the operation
 *                CELLO_BUTI_OK
 *                 - Request for unsubcription for the button events is successfu
l.
 *                CELLO_BUTI_ERROR_SERVER_NOT_AVAILABLE
 *                 - The internal proxy state for the client is not AVAILABLE
 *
 *   @par Globals:
 *                            --
 *
 *   This function is used for unsubscribing for the button events.
 **/
/* ===================================================================== */
CelloButiResult
CelloButi_unsubscribeButtonEvent(void *butiMemory_p,
                                 uint32_t clientRef);


/** ==================================================================== */
/**
 *   Disconnect from the server.
 *
 *   @param butiMemory_p   - Pointer to data area allocated and returned from
 *                           CelloButi_initiateMemory
 *   @param clientRef      - Client reference
 *
 *   @return CelloButiResult result of the operation
 *                CELLO_BUTI_OK
 *                 - Request for Service termination is successful.
 *                CELLO_BUTI_ERROR_SERVER_NOT_AVAILABLE
 *                 - The internal proxy state for the client is not AVAILABLE
 *
 *   @par Globals:
 *                            --
 *
 *   This function is used for deregistering from the buti server.
 **/
/* ===================================================================== */
CelloButiResult
CelloButi_terminateService(void *butiMemory_p,
                           uint32_t clientRef);


/** ==================================================================== */
/**
 *   Free the buti proxy internal data structure.
 *
 *   @param butiMemory_p   - Pointer to data area allocated and
 *                           returned from CelloButi_initiateMemory
 *
 *   @return CelloButiResult result of the operation
 *                CELLO_BUTI_OK
 *                 - Memory free operation successful.
 *                CELLO_BUTI_ERROR_SERVER_NOT_UNAVAILABLE
 *                 - The internal proxy state for the client is not UNAVAILABLE.
 *
 *   @par Globals:
 *                            --
 *
 * Used for cleaning up the internal memory allocated by the proxy
 */
/* ===================================================================== */
CelloButiResult
CelloButi_freeMemory(void **butiMemory_p);


#ifdef __cplusplus
}
#endif

#endif   /* ifndef __CELLO_BUTI_H */
