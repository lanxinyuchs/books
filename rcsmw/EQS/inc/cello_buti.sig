/**
 *   This is the signal file for the buti interface.
 *
 *
 *
 *   Copyright (c) Ericsson AB  2014-2015 All rights reserved.
 *   The information in this document is the property of Ericsson. Except
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
 *   Revised : 2014-02-27 Per Norberg etxpeno
 *   Change  : Copied from CPP git repo
 * ========================================================================
 */

#ifndef __CELLO_BUTI_SIG
#define __CELLO_BUTI_SIG

#ifdef __cplusplus
extern "C" {
#endif

/* ========================================================================
 *   INCLUDE FILES
 * ========================================================================
 */
#include "ose.h"
#include "osetypes.h"
#include "cello_buti.h"

/* ========================================================================
 *   DEFINITIONS
 * ========================================================================
 */

/* ========================================================================
 *   TYPE DEFINITIONS
 * ========================================================================
 */

/** ==================================================================== */
/** @struct CelloButiInitiateServiceCfm
 *
 *   CelloButi_initiateService() is successful.
 *
 *   @param signalRevision            The revision of this signal.
 *   @param selectedProtocolVersion   The chosen protocol version.
 *   @param clientRef                 The client reference.
 *
 *   This signal is a confirmation that the service can be used.
 *   The signal must be forwarded to the proxy as an argument of
 *   the function CelloButi_internal().
 *
 */
/* ===================================================================== */
#define CELLO_BUTI_INITIATE_SERVICE_CFM (0x60D36) /* !-SIGNO(CelloButiInitiateServiceCfm)-! */

typedef struct{
  SIGSELECT       sigNo;
  uint32_t        signalRevision;
  uint32_t        selectedPV;
  uint32_t        clientRef;
}CelloButiInitiateServiceCfm;


/** ==================================================================== */
/** @struct CelloButiInitiateServiceRej
 *
 *   CelloButi_initiateService() is not successful.
 *
 *   @param signalRevision            The revision of this signal.
 *   @param highestSupportedPV        The highest supported protocol version.
 *   @param rejectReason              The reason for request rejection.
 *   @param clientRef                 The client reference.
 *
 *   This signal informs that the service has been rejected for some reason.
 *   The signal must be forwarded to the proxy as an argument of the function
 *   CelloButi_internal().
 *
 */
/* ===================================================================== */
#define CELLO_BUTI_INITIATE_SERVICE_REJ (0x60D37) /* !-SIGNO(CelloButiInitiateServiceRej)-! */
typedef struct{
  SIGSELECT      sigNo;
  uint32_t       signalRevision;
  uint32_t       highestSupportedPV;
  uint32_t       rejectReason;
  uint32_t       clientRef;
}CelloButiInitiateServiceRej;


/** ==================================================================== */
/** @struct CelloButiSubscribeButtonEventCfm
 *
 *   This signal informs that the subscription for button events is successful.
 *
 *   @param  clientRef                The client reference.
 *
 */
/* ===================================================================== */
#define CELLO_BUTI_SUBSCRIBE_BUTTON_EVENT_CFM (0x60D38) /* !-SIGNO(CelloButiSubscribeButtonEventCfm)-! */

typedef struct{
  SIGSELECT      sigNo;
  uint32_t       clientRef;
}CelloButiSubscribeButtonEventCfm;


/** ==================================================================== */
/** @struct CelloButiUnsubscribeButtonEventCfm
 *
 *   This signal informs that the unsubscription for button events is successful.
 *
 *   @param  clientRef                The client reference.
 *
 */
/* ===================================================================== */
#define CELLO_BUTI_UNSUBSCRIBE_BUTTON_EVENT_CFM (0x60D39) /* !-SIGNO(CelloButiUnsubscribeButtonEventCfm)-! */

typedef struct{
  SIGSELECT      sigNo;
  uint32_t       clientRef;
}CelloButiUnsubscribeButtonEventCfm;


/** ==================================================================== */
/** @struct CelloButiChangeFeedbackModeCfm
 *
 *   The signal informs that the mode has been changed successfully.
 *
 *   @param  clientRef                The client reference.
 *   @param  feedBackMode             The mode the client has requested.
 *                                    1 - feedback blink mode ON
 *                                    0 - feedback blink mode OFF
 *
 */
/* ===================================================================== */
#define CELLO_BUTI_CHANGE_FEEDBACK_MODE_CFM (0x60D3A) /* !-SIGNO(CelloButiChangeFeedbackModeCfm)-! */

typedef struct{
  SIGSELECT      sigNo;
  uint32_t       clientRef;
  uint32_t       feedBackMode;
}CelloButiChangeFeedbackModeCfm;


/** ==================================================================== */
/** @struct CelloButiChangeFeedbackModeRej
 *
 *   The signal informs that the mode change request has failed/rejected.
 *
 *   @param  rejectReason             The reason for rejection.
 *   @param  clientRef                The client reference.
 *   @param  feedBackMode             The mode the client has requested.
 *                                    1 - feedback blink mode ON
 *                                    0 - feedback blink mode OFF
 *
 */
/* ===================================================================== */
#define CELLO_BUTI_CHANGE_FEEDBACK_MODE_REJ (0x60D3B) /* !-SIGNO(CelloButiChangeFeedbackModeRej)-! */

typedef struct{
  SIGSELECT      sigNo;
  uint32_t       rejectReason;
  uint32_t       clientRef;
  uint32_t       feedBackMode;
}CelloButiChangeFeedbackModeRej;


/** ==================================================================== */
/** @struct CelloButiTerminateServiceCfm
 *
 *   This signal informs that the unsubscription from the buti server is successful.
 *
 *   @param  clientRef                The client reference.
 *
 */
/* ===================================================================== */
#define CELLO_BUTI_TERMINATE_SERVICE_CFM (0x60D3C) /* !-SIGNO(CelloButiTerminateServiceCfm)-! */

typedef struct{
  SIGSELECT      sigNo;
  uint32_t       clientRef;
}CelloButiTerminateServiceCfm;


/** ==================================================================== */
/** @struct CelloButiEventInd
 *
 *   This signal shall notify the button event to the subscribed client.
 *
 *   @param  buttonEventType    Used to indicate the three button events
 *                              0 - CELLO_BUTI_BUTTON_PRESSED
 *                              1 - CELLO_BUTI_BUTTON_SHORT_RELEASE
 *                              2 - CELLO_BUTI_BUTTON_MEDIUM_RELEASE
 *
 */
/* ===================================================================== */
#define CELLO_BUTI_EVENT_IND (0x60D3D) /* !-SIGNO(CelloButiEventInd)-! */

typedef struct{
  SIGSELECT      sigNo;
  uint32_t       buttonEventType;
}CelloButiEventInd;

#ifdef __cplusplus
}
#endif

#endif   /* ifndef __CELLO_BUTI_SIG */
