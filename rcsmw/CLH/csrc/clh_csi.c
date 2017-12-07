/*
 *
 * Copyright (c) Ericsson AB 2012-2015 All rights reserved.
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
#include <cello_te_trace.h>
#include <itc.h>
#include <cec.h>

#include "clh_csi.h"
#include "clh_internal.h"

static CsiResult csi_send_recv(cec_packet_t *send_packet,
			       cec_packet_t *recv_packet);

/* -------------------------------------------------------------------------- */
CsiResult
Csi_subscribeCoreState(U32 mpId)
{
  CsiResult result;
  cec_packet_t send_packet, recv_packet;
  char buffer[3*sizeof(U32)];
  char *ptr;
  itc_mbox_id_t itc_current_mbox_id = itc_current_mbox();

  ENTER(STR("Csi_subscribeCoreState, mpId: %u", mpId));

  if (itc_current_mbox_id == ITC_NO_ID)
    return CSI_NO_ITC_MBOX;

  send_packet.length = sizeof(buffer);
  ptr = send_packet.data = buffer;

  *(U32*)ptr = CSI_SUBSCRIBE_CORE_STATE;
  ptr += sizeof(U32);
  *(U32*)ptr = mpId;
  ptr += sizeof(U32);
  *(U32*)ptr = itc_current_mbox_id;

  result = csi_send_recv(&send_packet, &recv_packet);

  if (result == CSI_OK) {
    ptr = recv_packet.data;
    result = *(U32*)ptr;
  }

  free(recv_packet.data);

  return result;
}

/* -------------------------------------------------------------------------- */
CsiResult
Csi_unsubscribeCoreState(U32 mpId)
{
  CsiResult result;
  cec_packet_t send_packet, recv_packet;
  char buffer[3*sizeof(U32)];
  char *ptr;
  itc_mbox_id_t itc_current_mbox_id = itc_current_mbox();

  ENTER(STR("Csi_unsubscribeCoreState, mpId: %u", mpId));

  if (itc_current_mbox_id == ITC_NO_ID)
    return CSI_NO_ITC_MBOX;

  send_packet.length = sizeof(buffer);
  ptr = send_packet.data = buffer;

  *(U32*)ptr = CSI_UNSUBSCRIBE_CORE_STATE;
  ptr += sizeof(U32);
  *(U32*)ptr = mpId;
  ptr += sizeof(U32);
  *(U32*)ptr = itc_current_mbox_id;

  result = csi_send_recv(&send_packet, &recv_packet);

  if (result == CSI_OK) {
    ptr = recv_packet.data;
    result = *(U32*)ptr;
  }

  free(recv_packet.data);

  return result;
}

/* -------------------------------------------------------------------------- */
CsiResult
Csi_subscribeClusterRestart()
{
  CsiResult result;
  cec_packet_t send_packet, recv_packet;
  char buffer[2*sizeof(U32)];
  char *ptr;
  itc_mbox_id_t itc_current_mbox_id = itc_current_mbox();

  ENTER(STR("Csi_subscribeClusterRestart, mbox: %d", itc_current_mbox_id));

  if (itc_current_mbox_id == ITC_NO_ID)
    return CSI_NO_ITC_MBOX;

  send_packet.length = sizeof(buffer);
  ptr = send_packet.data = buffer;

  *(U32*)ptr = CSI_SUBSCRIBE_CLUSTER_RESTART;
  ptr += sizeof(U32);
  *(U32*)ptr = itc_current_mbox_id;

  result = csi_send_recv(&send_packet, &recv_packet);

  if (result == CSI_OK) {
    ptr = recv_packet.data;
    result = *(U32*)ptr;
  }

  free(recv_packet.data);

  return result;
}

/* -------------------------------------------------------------------------- */
CsiResult
Csi_unsubscribeClusterRestart()
{
  CsiResult result;
  cec_packet_t send_packet, recv_packet;
  char buffer[2*sizeof(U32)];
  char *ptr;
  itc_mbox_id_t itc_current_mbox_id = itc_current_mbox();

  ENTER(STR("Csi_unsubscribeClusterRestart, mbox: %d", itc_current_mbox_id));

  if (itc_current_mbox_id == ITC_NO_ID)
    return CSI_NO_ITC_MBOX;

  send_packet.length = sizeof(buffer);
  ptr = send_packet.data = buffer;

  *(U32*)ptr = CSI_UNSUBSCRIBE_CLUSTER_RESTART;
  ptr += sizeof(U32);
  *(U32*)ptr = itc_current_mbox_id;

  result = csi_send_recv(&send_packet, &recv_packet);

  if (result == CSI_OK) {
    ptr = recv_packet.data;
    result = *(U32*)ptr;
  }

  free(recv_packet.data);

  return result;
}

/* -------------------------------------------------------------------------- */
CsiResult
Csi_clusterRestartReply()
{
  CsiResult result;
  cec_packet_t send_packet, recv_packet;
  char buffer[2*sizeof(U32)];
  char *ptr;
  itc_mbox_id_t itc_current_mbox_id = itc_current_mbox();

  ENTER(STR("Csi_clusterRestartReply, mbox: %d", itc_current_mbox_id));

  if (itc_current_mbox_id == ITC_NO_ID)
    return CSI_NO_ITC_MBOX;

  send_packet.length = sizeof(buffer);
  ptr = send_packet.data = buffer;

  *(U32*)ptr = CSI_CLUSTER_RESTART_REPLY;
  ptr += sizeof(U32);
  *(U32*)ptr = itc_current_mbox_id;

  result = csi_send_recv(&send_packet, &recv_packet);

  if (result == CSI_OK) {
    ptr = recv_packet.data;
    result = *(U32*)ptr;
  }

  free(recv_packet.data);

  return result;
}



/* -------------------------------------------------------------------------- */
CsiResult
Csi_getOwnMpid(U32 *mpId)
{
  CsiResult result;
  cec_packet_t send_packet, recv_packet;
  char buffer[sizeof(U32)];
  char *ptr;

  send_packet.length = sizeof(buffer);
  ptr = send_packet.data = buffer;

  *(U32*)ptr = CSI_GET_OWN_MPID;

  result = csi_send_recv(&send_packet, &recv_packet);

  if (result == CSI_OK) {
    ptr = recv_packet.data;
    result = *(U32*)ptr;
    ptr += sizeof(U32);
    *mpId = *(U32*)ptr;
  }

  free(recv_packet.data);

  return result;
}

/* -------------------------------------------------------------------------- */
CsiResult
Csi_getHuntPathPrefix(U32 mpId, char *huntPathPrefix)
{
  CsiResult result;
  cec_packet_t send_packet, recv_packet;
  char buffer[2*sizeof(U32)];
  char *ptr;
  U32 huntPathPrefixResultLen;

  ENTER(STR("Csi_getHuntPathPrefix, mpId: %u", mpId));

  send_packet.length = sizeof(buffer);
  ptr = send_packet.data = buffer;

  *(U32*)ptr = CSI_GET_HUNT_PATH_PREFIX;
  ptr += sizeof(U32);
  *(U32*)ptr = mpId;

  result = csi_send_recv(&send_packet, &recv_packet);

  if (result == CSI_OK)
    {
      ptr = recv_packet.data;
      result = *(U32*)ptr;
      ptr += sizeof(U32);
      huntPathPrefixResultLen = *(U32*)ptr;
      ptr += sizeof(U32);
      memcpy(huntPathPrefix, ptr, huntPathPrefixResultLen);
      huntPathPrefix += huntPathPrefixResultLen;
      *(char *)huntPathPrefix = 0;
    }

  free(recv_packet.data);

  return result;
}

/* -------------------------------------------------------------------------- */
static CsiResult
csi_send_recv(cec_packet_t *send_packet, cec_packet_t *recv_packet)
{
  char signature[] = {'C', 'S', 'I'};
  cec_handle_t *handle;
  CsiResult result = CSI_OK;
  int timeout = 43000; /* milliseconds */

  recv_packet->data = NULL;

  handle = cec_open(signature, sizeof(signature));

  if (handle == NULL)
    return CSI_ERROR_SERVER_NOT_AVAILABLE;

  if (cec_send_with_pid(handle, send_packet) < 0) {
    result = CSI_ERROR_SERVER_NOT_AVAILABLE;
    goto error;
  }

  if (cec_receive_w_tmeout(handle, recv_packet, timeout) < 0){
    result = CSI_ERROR_SERVER_NOT_AVAILABLE;
    goto error;
  }

 error:
  cec_close(handle);

  return result;
}

/* ############################### Deprecated ############################### */
CsiResult
Csi_getHuntPath(U32 mpId, char *huntPath)
{
  (void)mpId; (void)huntPath;
  return CSI_OK;
}

/* ############################### Deprecated ############################### */
CsiResult
Csi_get_state(U32 mpId, CsiOpState *opState)
{
  CsiResult result;
  cec_packet_t send_packet, recv_packet;
  char buffer[2*sizeof(U32)];
  char *ptr;

  ENTER(STR("Csi_get_state, mpId: %u", mpId));

  *opState = CSI_DISABLED;

  send_packet.length = sizeof(buffer);
  ptr = send_packet.data = buffer;

  *(U32*)ptr = CSI_GET_STATE;
  ptr += sizeof(U32);
  *(U32*)ptr = mpId;

  result = csi_send_recv(&send_packet, &recv_packet);

  if (result == CSI_OK) {
    ptr = recv_packet.data;
    result = *(U32*)ptr;
    ptr += sizeof(U32);
    *opState = *(U32*)ptr;
  }

  free(recv_packet.data);

  return result;
}

/* ############################### Deprecated ############################### */
CsiResult
Csi_subscribe(U32 mpId)
{
  CsiResult result;
  cec_packet_t send_packet, recv_packet;
  char buffer[3*sizeof(U32)];
  char *ptr;
  itc_mbox_id_t itc_current_mbox_id = itc_current_mbox();

  ENTER(STR("Csi_subscribe, mpId: %u", mpId));

  if (itc_current_mbox_id == ITC_NO_ID)
    return CSI_NO_ITC_MBOX;

  send_packet.length = sizeof(buffer);
  ptr = send_packet.data = buffer;

  *(U32*)ptr = CSI_SUBSCRIBE;
  ptr += sizeof(U32);
  *(U32*)ptr = mpId;
  ptr += sizeof(U32);
  *(U32*)ptr = itc_current_mbox_id;

  result = csi_send_recv(&send_packet, &recv_packet);

  if (result == CSI_OK) {
    ptr = recv_packet.data;
    result = *(U32*)ptr;
  }

  free(recv_packet.data);

  return result;
}

/* ############################### Deprecated ############################### */
CsiResult
Csi_unsubscribe(U32 mpId)
{
  CsiResult result;
  cec_packet_t send_packet, recv_packet;
  char buffer[3*sizeof(U32)];
  char *ptr;
  itc_mbox_id_t itc_current_mbox_id = itc_current_mbox();

  ENTER(STR("Csi_unsubscribe, mpId: %u", mpId));

  if (itc_current_mbox_id == ITC_NO_ID)
    return CSI_NO_ITC_MBOX;

  send_packet.length = sizeof(buffer);
  ptr = send_packet.data = buffer;

  *(U32*)ptr = CSI_UNSUBSCRIBE;
  ptr += sizeof(U32);
  *(U32*)ptr = mpId;
  ptr += sizeof(U32);
  *(U32*)ptr = itc_current_mbox_id;

  result = csi_send_recv(&send_packet, &recv_packet);

  if (result == CSI_OK) {
    ptr = recv_packet.data;
    result = *(U32*)ptr;
  }

  free(recv_packet.data);

  return result;
}

/* ############################### Deprecated ############################### */
CsiResult
Csi_getRole(U32 mpId, CsiRole *role)
{
  CsiResult result;
  cec_packet_t send_packet, recv_packet;
  char buffer[2*sizeof(U32)];
  char *ptr;

  send_packet.length = sizeof(buffer);
  ptr = send_packet.data = buffer;

  *(U32*)ptr = CSI_GET_ROLE;
  ptr += sizeof(U32);
  *(U32*)ptr = mpId;

  result = csi_send_recv(&send_packet, &recv_packet);

  if (result == CSI_OK) {
    ptr = recv_packet.data;
    result = *(U32*)ptr;
    ptr += sizeof(U32);
    *role = *(U32*)ptr;
  }

  free(recv_packet.data);

  return result;
}

/* ############################### Deprecated ############################### */
CsiResult
Csi_subscribeRole(U32 mpId)
{
  CsiResult result;
  cec_packet_t send_packet, recv_packet;
  char buffer[3*sizeof(U32)];
  char *ptr;
  itc_mbox_id_t itc_current_mbox_id = itc_current_mbox();

  ENTER(STR("Csi_subscribeRole, mpId: %u", mpId));

  if (itc_current_mbox_id == ITC_NO_ID)
    return CSI_NO_ITC_MBOX;

  send_packet.length = sizeof(buffer);
  ptr = send_packet.data = buffer;

  *(U32*)ptr = CSI_SUBSCRIBE2;
  ptr += sizeof(U32);
  *(U32*)ptr = mpId;
  ptr += sizeof(U32);
  *(U32*)ptr = itc_current_mbox_id;

  result = csi_send_recv(&send_packet, &recv_packet);

  if (result == CSI_OK) {
    ptr = recv_packet.data;
    result = *(U32*)ptr;
  }

  free(recv_packet.data);

  return result;
}

/* ############################### Deprecated ############################### */
CsiResult
Csi_unsubscribeRole(U32 mpId)
{
  CsiResult result;
  cec_packet_t send_packet, recv_packet;
  char buffer[3*sizeof(U32)];
  char *ptr;
  itc_mbox_id_t itc_current_mbox_id = itc_current_mbox();

  ENTER(STR("Csi_unsubscribe2, mpId: %u", mpId));

  if (itc_current_mbox_id == ITC_NO_ID)
    return CSI_NO_ITC_MBOX;

  send_packet.length = sizeof(buffer);
  ptr = send_packet.data = buffer;

  *(U32*)ptr = CSI_UNSUBSCRIBE2;
  ptr += sizeof(U32);
  *(U32*)ptr = mpId;
  ptr += sizeof(U32);
  *(U32*)ptr = itc_current_mbox_id;

  result = csi_send_recv(&send_packet, &recv_packet);

  if (result == CSI_OK) {
    ptr = recv_packet.data;
    result = *(U32*)ptr;
  }

  free(recv_packet.data);

  return result;
}
