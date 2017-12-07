/*
 *
 * Copyright (c) Ericsson AB  2013 All rights reserved.
 *
 * The information in this document is the property of Ericsson. Except
 * as specifically authorized in writing by Ericsson,the receiver of this
 * document shall keep the information contained herein confidential
 * and shall protect the same in whole or in part from disclosure and
 * dissemination to third parties. Disclosure and disseminations to the
 * receiver's employees shall only be made on a strict need to know basis.
 *
 *
 */

/*
******************************************************************************
* INCLUDE FILES
******************************************************************************
*/

#include <stdlib.h>

#include "cello_te_ose.h"   /* trace and error */
#include "cello_oei.h"      /* proxy <-> server definitions - Cello visible */

#include "cec.h"

/*
******************************************************************************
* MACROS
******************************************************************************
*/

#define OEI_GET 0

/*
******************************************************************************
* TYPES
******************************************************************************
*/

union SIGNAL *
CelloOei_initiateMemory(void)
{
  return NULL;
}

CelloOeiResult
CelloOei_getEventIdentity(union SIGNAL    *oeiService_p,
                          U32              timeoutValue,
                          U32             *eventId_p)
{
  char buffer[4];
  char *ptr;
  char signature[] = {'O', 'E', 'I'};
  cec_handle_t *handle;
  cec_packet_t send_packet;
  cec_packet_t recv_packet;

  /* Avoid compiler warning that parameter is never used */
  (void) oeiService_p;

  *eventId_p = CELLO_OEI_UNSPECIFIED_EVENT_ID;

  handle = cec_open(signature, sizeof(signature));
  if (handle == NULL)
    return CELLO_OEI_NO_SERVER;

  send_packet.length = sizeof(buffer);
  ptr = send_packet.data = buffer;

  *(U32*)ptr = OEI_GET;

  if (cec_send(handle, &send_packet) < 0) {
    cec_close(handle);
    return CELLO_OEI_NO_SERVER;
  }

  if (timeoutValue == CELLO_OEI_NO_TIMEOUT_VALUE) {
    if (cec_receive(handle, &recv_packet) < 0) {
      cec_close(handle);
      return CELLO_OEI_NO_SERVER;
    }
  }
  else {
    if (cec_receive_w_tmeout(handle, &recv_packet, timeoutValue) < 0) {
      cec_close(handle);
      return CELLO_OEI_TIMEOUT;
    }
  }

  cec_close(handle);

  ptr = recv_packet.data;
  *eventId_p = *(U32*)ptr;

  free(recv_packet.data);

  /* Something went wrong in erlang
     emulate no server process
  */
  if (*eventId_p == CELLO_OEI_UNSPECIFIED_EVENT_ID)
    return CELLO_OEI_NO_SERVER;

  return CELLO_OEI_OK;
}
