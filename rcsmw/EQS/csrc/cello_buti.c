/*
 *
 * Copyright (c) Ericsson AB  2014 All rights reserved.
 *
 * The information in this document is the property of Ericsson. Except
 * as specifically authorized in writing by Ericsson,the receiver of this
 * document shall keep the information contained herein confidential
 * and shall protect the same in whole or in part from disclosure and
 * dissemination to third parties. Disclosure and disseminations to the
 * receiver's employees shall only be made on a strict need to know basis.
 */

/* ========================================================================
 *   INCLUDE FILES
 * ========================================================================
 */

#include <stdlib.h>

#include "cello_te_ose.h"
#include "cello_te_trace.h"
#include "cello_te_trace_obj.h"

#include "cello_buti.h"
#include "cello_buti.sig"
#include "cec.h"

/* ========================================================================
 *   DEFINITIONS
 * ========================================================================
 */

/* Server state */
#define UNAVAILABLE (0)
#define INITIATING  (1)
#define AVAILABLE   (2)
#define TERMINATING (3)

#define INIT_SERVICE    (0)
#define TERM_SERVICE    (1)
#define SUBSCRIBE       (2)
#define UNSUBSCRIBE     (3)
#define CHANGE_FEEDBACK (4)

/* ========================================================================
 *   TYPE DEFINITIONS
 * ========================================================================
 */

typedef struct
{
  cec_handle_t *handle;
  PROCESS spid;
  uint32_t availabilityState;
}CelloButiProxyData;

union SIGNAL
{
  SIGSELECT                           sigNo;
  CelloButiInitiateServiceCfm         celloButiInitiateServiceCfm;
};

/* ========================================================================
 *   DATA DECLARATIONS
 * ========================================================================
 */

/* ========================================================================
 *   FUNCTION PROTOTYPES
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
 *           the client in all the calls to the buti proxy.
 *
 *   @par Globals:
 *                    --
 *
 *   Used for initiation of proxy and reservation of proxy internal memory.
 */
/* ===================================================================== */
void*
CelloButi_initiateMemory(void)
{
  CelloButiProxyData *buti;

  ENTER("CelloButi_initiateMemory");

  buti = malloc(sizeof *buti);

  buti->handle = NULL;
  buti->spid = current_process();
  buti->availabilityState = UNAVAILABLE;

  RETURN buti;
}

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
 *                 - Service initiation signal is sent successfully to server.
 *                CELLO_BUTI_ERROR_SERVER_NOT_UNAVAILABLE
 *                 - The internal proxy state for the client is not UNAVAILABLE
 *                CELLO_BUTI_SERVER_NOT_FOUND
 *                 - Buti server not found
 *
 *   @par Globals:
 *                    --
 *
 *   This function should be called when the client wants to connect
 *   to the server. Protocol version negotiation is included in the request.
*/
/* ===================================================================== */
CelloButiResult
CelloButi_initiateService(void    *butiMemory_p,
			  uint32_t pvFirstWanted,
			  uint32_t pvSecondWanted,
			  uint32_t pvThirdWanted,
			  uint32_t clientRef)
{
  CelloButiProxyData *buti= butiMemory_p;

  cec_handle_t *handle;
  cec_packet_t packet;
  char buffer[6*sizeof(uint32_t)];
  char *ptr;
  char signature[] = {'B', 'U', 'T', 'I'};

  ENTER("CelloButi_initiateService");

  /* The server must be unavailable to accept an initiation */
  if (buti->availabilityState != UNAVAILABLE) {
    RETURN CELLO_BUTI_ERROR_SERVER_NOT_UNAVAILABLE;
  }

  handle = cec_open(signature, sizeof(signature));

  if (handle == NULL) {
    RETURN CELLO_BUTI_SERVER_NOT_FOUND;
  }

  buti->availabilityState = INITIATING;
  buti->handle = handle;

  packet.length = sizeof(buffer);
  ptr = packet.data = buffer;

  *(uint32_t*)ptr = INIT_SERVICE;
  ptr += sizeof(uint32_t);
  *(uint32_t*)ptr = buti->spid;
  ptr += sizeof(uint32_t);
  *(uint32_t*)ptr = pvFirstWanted;
  ptr += sizeof(uint32_t);
  *(uint32_t*)ptr = pvSecondWanted;
  ptr += sizeof(uint32_t);
  *(uint32_t*)ptr = pvThirdWanted;
  ptr += sizeof(uint32_t);
  *(uint32_t*)ptr = clientRef;

  cec_send_with_pid(buti->handle, &packet);

  RETURN CELLO_BUTI_OK;
}


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
 *        CELLO_BUTI_INITIATE_SERVICE_CFM
 *        CELLO_BUTI_INITIATE_SERVICE_REJ
 *        CELLO_BUTI_TERMINATE_SERVICE_CFM
 */
/* ===================================================================== */
CelloButiResult
CelloButi_internal(void         *butiMemory_p,
                   union SIGNAL *sig_p)
{
  CelloButiProxyData *buti= butiMemory_p;

  ENTER("CelloButi_internal");

  switch(sig_p->sigNo)
    {
    case CELLO_BUTI_INITIATE_SERVICE_CFM:
      buti->availabilityState = AVAILABLE;
      break;
    case CELLO_BUTI_INITIATE_SERVICE_REJ:
      buti->availabilityState = UNAVAILABLE;
      break;
    case CELLO_BUTI_TERMINATE_SERVICE_CFM:
      buti->availabilityState = UNAVAILABLE;
      break;
    default:
      RETURN CELLO_BUTI_UNKNOWN_SIGNAL;
    }

  RETURN CELLO_BUTI_OK;
}


/** ==================================================================== */
/**
 *   Subscribe for the maintanance button state change events.
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
 *   This function is used for subscribing for the button events.
 **/
/* ===================================================================== */
CelloButiResult
CelloButi_subscribeButtonEvent(void    *butiMemory_p,
			       uint32_t clientRef)
{
  CelloButiProxyData *buti= butiMemory_p;
  cec_packet_t packet;
  char buffer[2*sizeof(uint32_t)];
  char *ptr;

  ENTER("CelloButi_subscribeButtonEvent");

  if (buti->availabilityState != AVAILABLE) {
    RETURN CELLO_BUTI_ERROR_SERVER_NOT_AVAILABLE;
  }

  packet.length = sizeof(buffer);
  ptr = packet.data = buffer;

  *(uint32_t*)ptr = SUBSCRIBE;
  ptr += sizeof(uint32_t);
  *(uint32_t*)ptr = clientRef;

  cec_send_with_pid(buti->handle, &packet);

  RETURN CELLO_BUTI_OK;
}


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
CelloButi_changeFeedBackMode(void    *butiMemory_p,
			     uint32_t feedbackMode,
			     uint32_t clientRef)
{
  CelloButiProxyData *buti= butiMemory_p;
  cec_packet_t packet;
  char buffer[3*sizeof(uint32_t)];
  char *ptr;

  ENTER("CelloButi_changeFeedBackMode");

  if (buti->availabilityState != AVAILABLE) {
    RETURN CELLO_BUTI_ERROR_SERVER_NOT_AVAILABLE;
  }

  packet.length = sizeof(buffer);
  ptr = packet.data = buffer;

  *(uint32_t*)ptr = CHANGE_FEEDBACK;
  ptr += sizeof(uint32_t);
  *(uint32_t*)ptr = feedbackMode;
  ptr += sizeof(uint32_t);
  *(uint32_t*)ptr = clientRef;

  cec_send_with_pid(buti->handle, &packet);

  RETURN CELLO_BUTI_OK;

}


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
 *                 - Request for unsubcription for the button events is successful.
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
CelloButi_unsubscribeButtonEvent(void    *butiMemory_p,
				 uint32_t clientRef)
{
  CelloButiProxyData *buti = butiMemory_p;
  cec_packet_t packet;
  char buffer[2*sizeof(uint32_t)];
  char *ptr;

  ENTER("CelloButi_unsubscribeButtonEvent");

  if (buti->availabilityState != AVAILABLE) {
    RETURN CELLO_BUTI_ERROR_SERVER_NOT_AVAILABLE;
  }

  packet.length = sizeof(buffer);
  ptr = packet.data = buffer;

  *(uint32_t*)ptr = UNSUBSCRIBE;
  ptr += sizeof(uint32_t);
  *(uint32_t*)ptr = clientRef;

  cec_send_with_pid(buti->handle, &packet);

  RETURN CELLO_BUTI_OK;
}


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
CelloButi_terminateService(void    *butiMemory_p,
			   uint32_t clientRef)
{
  CelloButiProxyData *buti = butiMemory_p;
  cec_packet_t packet;
  char buffer[2*sizeof(uint32_t)];
  char *ptr;

  ENTER("CelloButi_terminateService");

   if (buti->availabilityState != AVAILABLE) {
      RETURN CELLO_BUTI_ERROR_SERVER_NOT_AVAILABLE;
   }

   packet.length = sizeof(buffer);
   ptr = packet.data = buffer;

   *(uint32_t*)ptr = TERM_SERVICE;
   ptr += sizeof(uint32_t);
   *(uint32_t*)ptr = clientRef;

   cec_send_with_pid(buti->handle, &packet);

   buti->availabilityState = TERMINATING;

   RETURN CELLO_BUTI_OK;
}


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
CelloButi_freeMemory(void **butiMemory_p)
{
  CelloButiProxyData *buti = *butiMemory_p;

  ENTER("CelloButi_freeMemory");

  if (buti == NULL) {
    RETURN CELLO_BUTI_OK;
  }

  if (buti->availabilityState != UNAVAILABLE) {
    RETURN CELLO_BUTI_ERROR_SERVER_NOT_UNAVAILABLE;
  }

  free(buti);
  *butiMemory_p = NULL;
  RETURN CELLO_BUTI_OK;
}
