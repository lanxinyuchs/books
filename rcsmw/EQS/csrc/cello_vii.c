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
 *
 */

#include <stdlib.h>
#include "cello_te_ose.h"
#include "cello_te_trace.h"

#include "cello_vii.h"
#include "cec.h"

#define VISUAL_IND_REQ 0
#define VISUAL_IND_GET 1

typedef struct
{
  cec_handle_t *handle;
} ViiProxyData;

static char signature[] = {'V', 'I', 'I'} ;

static cec_handle_t *
get_handle(void)
{
  ViiProxyData* viiProxy_p;

  viiProxy_p = (ViiProxyData *)get_envp(current_process(), "VII_PROXY_MEM");

  if (viiProxy_p == NULL) {
    viiProxy_p = malloc(sizeof *viiProxy_p);
    if (viiProxy_p == NULL) {
      set_envp(current_process(), "VII_PROXY_MEM", (OSADDRESS)viiProxy_p);
      return NULL;
    }
    viiProxy_p->handle = cec_open(signature, sizeof(signature));

    set_envp(current_process(), "VII_PROXY_MEM", (OSADDRESS)viiProxy_p);
  }

  return viiProxy_p->handle;
}

static cec_handle_t *
get_new_handle(void)
{
  ViiProxyData* viiProxy_p;

  viiProxy_p = (ViiProxyData *)get_envp(current_process(), "VII_PROXY_MEM");

  if (viiProxy_p) {
    cec_close(viiProxy_p->handle);
    free(viiProxy_p);
  }

  viiProxy_p = malloc(sizeof *viiProxy_p);
  if (viiProxy_p == NULL) {
    set_envp(current_process(), "VII_PROXY_MEM", (OSADDRESS)viiProxy_p);
    return NULL;
  }
  viiProxy_p->handle = cec_open(signature, sizeof(signature));

  set_envp(current_process(), "VII_PROXY_MEM", (OSADDRESS)viiProxy_p);

  return viiProxy_p->handle;
}

static CelloViiResult_e
sendto_server(cec_packet_t *send_packet)
{
  cec_handle_t *handle;

  handle = get_handle();
  if (handle == NULL)
    return CELLO_VII_FAILED;

  if (cec_send_with_pid(handle, send_packet) < 0) {
    /*
      It was not possible to send a request on the existing TCP connection.
      Perhaps the server has terminated the TCP connection.
      Create a new and try again
    */
    handle = get_new_handle();

    if (handle == NULL)
      return CELLO_VII_FAILED;

    if (cec_send_with_pid(handle, send_packet) < 0)
      return CELLO_VII_FAILED;
  }

  return CELLO_VII_SUCCESS;
}


static CelloViiResult_e
send_recv(cec_packet_t *send_packet, cec_packet_t *recv_packet)
{
  cec_handle_t *handle;
  unsigned int timeout = 20000;

  handle = get_handle();
  if (handle == NULL)
    return CELLO_VII_FAILED;

  if (cec_send_with_pid(handle, send_packet) < 0) {
    /*
      It was not possible to send a request on the existing TCP connection.
      Perhaps the server has terminated the TCP connection.
      Create a new and try again
    */
    handle = get_new_handle();

    if (handle == NULL)
      return CELLO_VII_FAILED;

    if (cec_send_with_pid(handle, send_packet) < 0)
      return CELLO_VII_FAILED;
  }

  if (cec_receive_w_tmeout(handle, recv_packet, timeout) < 0)
    return CELLO_VII_FAILED;

  return CELLO_VII_SUCCESS;
}

CelloViiResult_e
CelloVii_visualIndRequest(CelloViiCommand_e indication)
{
  CelloViiResult_e result;
  cec_packet_t send_packet;
  char buffer[2*sizeof(U32)];
  char *ptr;

  ENTER(STR("CelloVii_visualIndRequest, indication: %d", indication));

  if (indication < CELLO_VII_FAULT ||
      indication > CELLO_VII_REMOTE_UNIT_FAULT_END) {
    RETURN CELLO_VII_FAILED;
  }

  send_packet.length = sizeof(buffer);
  ptr = send_packet.data = buffer;

  *(U32*)ptr = VISUAL_IND_REQ;
  ptr += sizeof(U32);
  *(U32*)ptr = indication;

  result = sendto_server(&send_packet);

  RETURN result;
}

CelloViiResult_e
CelloVii_visualIndGet(CelloViiLed_e led_type, CelloViiLedState_e *led_state)
{
  CelloViiResult_e result;
  cec_packet_t send_packet, recv_packet;
  char buffer[2*sizeof(U32)];
  char *ptr;

  ENTER(STR("CelloVii_visualIndGet, led_type: %d", led_type));

  if (led_state == NULL) {
    RETURN CELLO_VII_FAILED;
  }

  send_packet.length = sizeof(buffer);
  ptr = send_packet.data = buffer;

  *(U32*)ptr = VISUAL_IND_GET;
  ptr += sizeof(U32);
  *(U32*)ptr = led_type;

  result = send_recv(&send_packet, &recv_packet);
  if (result != CELLO_VII_SUCCESS) {
    RETURN result;
  }

  ptr = recv_packet.data;
  result = *(U32*)ptr;
  ptr += sizeof(U32);
  *led_state = *(U32*)ptr;

  free(recv_packet.data);

  RETURN result;
}
