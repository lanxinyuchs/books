/*
 *
 * Copyright (c) Ericsson AB  2012-2016 All rights reserved.
 *
 * The information in this document is the property of Ericsson. Except
 * as specifically authorized in writing by Ericsson,the receiver of this
 * document shall keep the information contained herein confidential
 * and shall protect the same in whole or in part from disclosure and
 * dissemination to third parties. Disclosure and disseminations to the
 * receiver's employees shall only be made on a strict need to know basis.
 */

#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include "cello_te_trace.h"

#include "cec.h"
#include "dnti.h"


#define DNTI_INIT_SERVICE   0
#define DNTI_TERM_SERVICE   1
#define DNTI_MIM2IMM        2
#define DNTI_IMM2MIM        3
#define DNTI_ILLEGAL_TYPE  99

typedef struct
{
  cec_handle_t *handle;
} DntiProxyData;

static char signature[] = {'D', 'N', 'T', 'I'};

void*
dntiInitiateService(void)
{
  ENTER("dntiInitiateService") ;

  DntiProxyData *dntiHandleP = NULL;

  cec_packet_t packet;
  char buffer[4];
  char *ptr;

  dntiHandleP = malloc(sizeof *dntiHandleP);

  dntiHandleP->handle = cec_open(signature, sizeof(signature));
  TRACE(3, STR("call: cec_open  %x\n", (unsigned long)dntiHandleP->handle));

  packet.length = sizeof(buffer);
  ptr = packet.data = buffer;

  *(uint32_t*)ptr = DNTI_INIT_SERVICE;

  cec_send(dntiHandleP->handle, &packet);


  TRACE(1, STR("RESULT:\n  *dntiHandleP = %x\n", (unsigned long)dntiHandleP));

  RETURN dntiHandleP;
}

void
dntiTerminateService(void* dntiHandleP)
{
  ENTER("dntiTerminateService") ;
  TRACE(1, STR("ENTRY PARAMS:\n  *dntiHandleP = %x\n",
	       (unsigned long)dntiHandleP));

  DntiProxyData *dntiProxyP = NULL;

  cec_packet_t packet;
  char buffer[4];
  char *ptr;

  packet.length = sizeof(buffer);
  ptr = packet.data = buffer;
  dntiProxyP = (DntiProxyData *)dntiHandleP;

  *(uint32_t*)ptr = DNTI_TERM_SERVICE;
  cec_send(dntiProxyP->handle, &packet);

  TRACE(3, STR("call: cec_close %x\n", (unsigned long)dntiProxyP->handle));
  cec_close(dntiProxyP->handle);

  free(dntiHandleP);

  RETURN;
}

DntiResultT
dntiTransform(void *dntiHandleP,
	      DntiTransformDirectionT direction,
	      char *fromDnP,
	      char **toDnP)
{
  ENTER("dntiTransform") ;

  DntiProxyData *dntiProxyP = NULL;
  cec_packet_t send_packet;
  cec_packet_t recv_packet;
  int fromDnSize = strlen(fromDnP) + 1;
  int send_length;
  char *ptr;
  int requestType  = DNTI_ILLEGAL_TYPE;
  int result       = DNTI_OK;
  int timeout = 250; // 250 milliseconds

  /* toDnP should be NULL */
  if (toDnP == NULL || *toDnP != NULL) {
    TRACE_ERROR("Parameter toDnP must be NULL \n");
    return DNTI_INVALID_PARAMETER_TODNP;
  }

  /* Check if valid direction */
  if (direction == MIM_TO_IMM)
    requestType = DNTI_MIM2IMM;
  else if (direction == IMM_TO_MIM)
    requestType = DNTI_IMM2MIM;
  else {
    TRACE_ERROR(STR("Parameter direction is invalid (%d) \n", direction));
    return DNTI_INVALID_PARAMETER_DIRECTION;
  }

  /* Valid in parameters */
  TRACE(1, STR("ENTRY PARAMS:\n  *dntiHandleP = %s\n  direction    = %s\n  *fromDnP     = %s\n  **toDnP      = null\n",
	       (dntiHandleP == NULL ? "null" : "handle"), (direction == MIM_TO_IMM ? "1 (MIM_TO_IMM)" : "2 (IMM_TO_MIM)"), fromDnP));


  /* Initialize connection if not already initialized */
  if (dntiHandleP == NULL) {
    dntiProxyP = malloc(sizeof *dntiProxyP);
    dntiProxyP->handle = cec_open(signature, sizeof(signature));
    TRACE(3, STR("call: cec_open  %x\n", (unsigned long)dntiProxyP->handle));
  }
  else {
    dntiProxyP = (DntiProxyData *)dntiHandleP;
  }


  /* Build the send signal */
  send_length = fromDnSize + 2*sizeof(uint32_t);
  ptr = send_packet.data = malloc(send_length);
  send_packet.length = send_length;


  *(uint32_t*)ptr = requestType;
  ptr+=4;
  *(uint32_t*)ptr = fromDnSize;
  ptr+=4;
  strcpy(ptr, fromDnP);

  /* Send the request and wait for the translated reply */
  if (cec_send(dntiProxyP->handle, &send_packet) == -1) {
    TRACE_ERROR("ErrorCode: 3 (DNTI_SEND_ERROR) \n");
    result = DNTI_SEND_ERROR;
  }
  else {
    if (cec_receive_w_tmeout(dntiProxyP->handle, &recv_packet, timeout) == -1) {
      TRACE_ERROR("ErrorCode: 4 (DNTI_RECEIVE_ERROR) \n");
      result = DNTI_RECEIVE_ERROR;
    }
    else {
      /* OK. Copy the result to result parameter */
      *toDnP = malloc(recv_packet.length);
      result = *((char *)recv_packet.data);
      strcpy(*toDnP, ((char *)recv_packet.data)+1);
      free(recv_packet.data);
    }
  }

  free(send_packet.data);

  /* Terminate the connection if there was no connection before */
  if (dntiHandleP == NULL) {
    TRACE(3, STR("call: cec_close %x\n", (unsigned long)dntiProxyP->handle));
    cec_close(dntiProxyP->handle);
    free(dntiProxyP);
  }

  TRACE(1, STR("RESULT:\n  result  = %d \n  **toDnP = %s \n", result, *toDnP));

  RETURN result;
}
