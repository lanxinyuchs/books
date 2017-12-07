/*
 *
 * Copyright (c) Ericsson AB 2015 All rights reserved.
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

#include "clh_chi.h"
#include "clh_internal.h"

static ChiResult chi_send_recv(cec_packet_t *send_packet,
                               cec_packet_t *recv_packet);

/* -------------------------------------------------------------------------- */
ChiResult
Chi_subscribeOpState(uint32_t mpId)
{
  ChiResult result;
  cec_packet_t send_packet, recv_packet;
  char buffer[3*sizeof(uint32_t)];
  char *ptr;
  itc_mbox_id_t itc_current_mbox_id = itc_current_mbox();

  ENTER(STR("Chi_subscribeOpState, mpId: %u", mpId));

  if (itc_current_mbox_id == ITC_NO_ID)
    return CHI_RESULT_NO_ITC_MBOX;

  send_packet.length = sizeof(buffer);
  ptr = send_packet.data = buffer;

  *(uint32_t*)ptr = CHI_SUBSCRIBE_OP_STATE;
  ptr += sizeof(uint32_t);
  *(uint32_t*)ptr = mpId;
  ptr += sizeof(uint32_t);
  *(uint32_t*)ptr = itc_current_mbox_id;

  result = chi_send_recv(&send_packet, &recv_packet);

  if (result == CHI_RESULT_OK) {
    ptr = recv_packet.data;
    result = *(uint32_t*)ptr;
  }

  free(recv_packet.data);

  return result;
}

/* -------------------------------------------------------------------------- */
ChiResult
Chi_unsubscribeOpState(uint32_t mpId)
{
  ChiResult result;
  cec_packet_t send_packet, recv_packet;
  char buffer[3*sizeof(uint32_t)];
  char *ptr;
  itc_mbox_id_t itc_current_mbox_id = itc_current_mbox();

  ENTER(STR("Chi_unsubscribeOpState, mpId: %u", mpId));

  if (itc_current_mbox_id == ITC_NO_ID)
    return CHI_RESULT_NO_ITC_MBOX;

  send_packet.length = sizeof(buffer);
  ptr = send_packet.data = buffer;

  *(uint32_t*)ptr = CHI_UNSUBSCRIBE_OP_STATE;
  ptr += sizeof(uint32_t);
  *(uint32_t*)ptr = mpId;
  ptr += sizeof(uint32_t);
  *(uint32_t*)ptr = itc_current_mbox_id;

  result = chi_send_recv(&send_packet, &recv_packet);

  if (result == CHI_RESULT_OK) {
    ptr = recv_packet.data;
    result = *(uint32_t*)ptr;
  }

  free(recv_packet.data);

  return result;
}

/* -------------------------------------------------------------------------- */
ChiResult
Chi_associateMp(uint32_t mpId,
		const char *fruId,
		ChiCoreRank coreRank)
{
  ChiResult result;
  cec_packet_t send_packet, recv_packet;
  char *ptr;
  uint32_t fruIdLen;
  itc_mbox_id_t itc_current_mbox_id = itc_current_mbox();

  ENTER(STR("Chi_associateMp, coreRank: %u", coreRank));

  if (itc_current_mbox_id == ITC_NO_ID)
    return CHI_RESULT_NO_ITC_MBOX;

  if ( fruId == NULL )
    {
      fruIdLen = 0;
    }
  else
    {
      fruIdLen = strlen( fruId );
    }

  char buffer[(5*sizeof(uint32_t)) + fruIdLen];

  send_packet.length = sizeof(buffer);
  ptr = send_packet.data = buffer;

  *(uint32_t*)ptr = CHI_ASSOCIATE_MP;
  ptr += sizeof(uint32_t);
  *(uint32_t*)ptr = mpId;
  ptr += sizeof(uint32_t);
  *(uint32_t*)ptr = fruIdLen;
  ptr += sizeof(uint32_t);
  memcpy(ptr, fruId, fruIdLen);
  ptr += fruIdLen;
  *(uint32_t*)ptr = coreRank;
  ptr += sizeof(uint32_t);
  *(uint32_t*)ptr = itc_current_mbox_id;

  result = chi_send_recv(&send_packet, &recv_packet);

  if (result == CHI_RESULT_OK)
    {
      ptr = recv_packet.data;
      result = *(uint32_t*)ptr;
    }

  free(recv_packet.data);

  return result;
}

/* -------------------------------------------------------------------------- */
ChiResult
Chi_disassociateMp(uint32_t mpId)
{
  ChiResult result;
  cec_packet_t send_packet, recv_packet;
  char buffer[3*sizeof(uint32_t)];
  char *ptr;
  itc_mbox_id_t itc_current_mbox_id = itc_current_mbox();

  ENTER(STR("Chi_disassociateMp, mpId: %u", mpId));

  if (itc_current_mbox_id == ITC_NO_ID)
    return CHI_RESULT_NO_ITC_MBOX;

  send_packet.length = sizeof(buffer);
  ptr = send_packet.data = buffer;

  *(uint32_t*)ptr = CHI_DISASSOCIATE_MP;
  ptr += sizeof(uint32_t);
  *(uint32_t*)ptr = mpId;
  ptr += sizeof(uint32_t);
  *(uint32_t*)ptr = itc_current_mbox_id;

  result = chi_send_recv(&send_packet, &recv_packet);

  if (result == CHI_RESULT_OK)
    {
      ptr = recv_packet.data;
      result = *(uint32_t*)ptr;
    }

  free(recv_packet.data);

  return result;
}

/* -------------------------------------------------------------------------- */
ChiResult
Chi_restartCluster(ChiRestartType restartType,
		   ChiRestartRank restartRank,
		   const char *restartCause)
{
  ChiResult result;
  cec_packet_t send_packet, recv_packet;
  char *ptr;
  uint32_t restartCauseLen;
  itc_mbox_id_t itc_current_mbox_id = itc_current_mbox();

  ENTER(STR("Chi_restartCluster, restartRank: %u", restartRank));

  if (itc_current_mbox_id == ITC_NO_ID)
    return CHI_RESULT_NO_ITC_MBOX;

  if ( restartCause == NULL )
    {
      restartCauseLen = 0;
    }
  else
    {
      restartCauseLen = strlen( restartCause );
      if ( restartCauseLen > CHI_MAX_RESTART_CAUSE_LEN )
	{
	  return CHI_RESULT_BAD_PARM;
	}
    }

  char buffer[(5*sizeof(uint32_t)) + restartCauseLen];

  send_packet.length = sizeof(buffer);
  ptr = send_packet.data = buffer;

  *(uint32_t*)ptr = CHI_RESTART_CLUSTER;
  ptr += sizeof(uint32_t);
  *(uint32_t*)ptr = restartType;
  ptr += sizeof(uint32_t);
  *(uint32_t*)ptr = restartRank;
  ptr += sizeof(uint32_t);
  *(uint32_t*)ptr = restartCauseLen;
  ptr += sizeof(uint32_t);
  memcpy(ptr, restartCause, restartCauseLen);
  ptr += restartCauseLen;
  *(uint32_t*)ptr = itc_current_mbox_id;

  result = chi_send_recv(&send_packet, &recv_packet);

  if (result == CHI_RESULT_OK)
    {
      ptr = recv_packet.data;
      result = *(uint32_t*)ptr;
    }

  free(recv_packet.data);

  return result;
}

/* -------------------------------------------------------------------------- */
static ChiResult
chi_send_recv(cec_packet_t *send_packet, cec_packet_t *recv_packet)
{
  char signature[] = {'C', 'S', 'I'};
  cec_handle_t *handle;
  ChiResult result = CHI_RESULT_OK;

  recv_packet->data = NULL;

  handle = cec_open(signature, sizeof(signature));

  if (handle == NULL)
    return CHI_RESULT_ERROR_SERVER_NOT_AVAILABLE;

  if (cec_send_with_pid(handle, send_packet) < 0) {
    result = CHI_RESULT_ERROR_SERVER_NOT_AVAILABLE;
    goto error;
  }

  if (cec_receive(handle, recv_packet) < 0){
    result = CHI_RESULT_ERROR_SERVER_NOT_AVAILABLE;
    goto error;
  }

 error:
  cec_close(handle);

  return result;
}

/* ############################### Deprecated ############################### */
ChiResult
Chi_getState(uint32_t mpId, ChiOpState *operstate, ChiRole *role)
{
  ChiResult result;
  cec_packet_t send_packet, recv_packet;
  char buffer[2*sizeof(uint32_t)];
  char *ptr;

  ENTER(STR("Chi_getState, mpId: %u", mpId));

  send_packet.length = sizeof(buffer);
  ptr = send_packet.data = buffer;

  *(uint32_t*)ptr = CHI_GET_STATE;
  ptr += sizeof(uint32_t);
  *(uint32_t*)ptr = mpId;

  result = chi_send_recv(&send_packet, &recv_packet);

  if (result == CHI_RESULT_OK) {
    ptr = recv_packet.data;
    result = *(uint32_t*)ptr;
    ptr += sizeof(uint32_t);
    *operstate = *(ChiOpState *)ptr;
    ptr += sizeof(ChiOpState);
    *role = *(ChiRole *)ptr;
  }

  free(recv_packet.data);

  return result;
}

/* ############################### Deprecated ############################### */
ChiResult
Chi_subscribe(uint32_t mpId)
{
  ChiResult result;
  cec_packet_t send_packet, recv_packet;
  char buffer[3*sizeof(uint32_t)];
  char *ptr;
  itc_mbox_id_t itc_current_mbox_id = itc_current_mbox();

  ENTER(STR("Chi_subscribe, mpId: %u", mpId));

  if (itc_current_mbox_id == ITC_NO_ID)
    return CHI_RESULT_NO_ITC_MBOX;

  send_packet.length = sizeof(buffer);
  ptr = send_packet.data = buffer;

  *(uint32_t*)ptr = CHI_SUBSCRIBE;
  ptr += sizeof(uint32_t);
  *(uint32_t*)ptr = mpId;
  ptr += sizeof(uint32_t);
  *(uint32_t*)ptr = itc_current_mbox_id;

  result = chi_send_recv(&send_packet, &recv_packet);

  if (result == CHI_RESULT_OK) {
    ptr = recv_packet.data;
    result = *(uint32_t*)ptr;
  }

  free(recv_packet.data);

  return result;
}

/* ############################### Deprecated ############################### */
ChiResult
Chi_unsubscribe(uint32_t mpId)
{
  ChiResult result;
  cec_packet_t send_packet, recv_packet;
  char buffer[3*sizeof(uint32_t)];
  char *ptr;
  itc_mbox_id_t itc_current_mbox_id = itc_current_mbox();

  ENTER(STR("Chi_unsubscribe, mpId: %u", mpId));

  if (itc_current_mbox_id == ITC_NO_ID)
    return CHI_RESULT_NO_ITC_MBOX;

  send_packet.length = sizeof(buffer);
  ptr = send_packet.data = buffer;

  *(uint32_t*)ptr = CHI_UNSUBSCRIBE;
  ptr += sizeof(uint32_t);
  *(uint32_t*)ptr = mpId;
  ptr += sizeof(uint32_t);
  *(uint32_t*)ptr = itc_current_mbox_id;

  result = chi_send_recv(&send_packet, &recv_packet);

  if (result == CHI_RESULT_OK) {
    ptr = recv_packet.data;
    result = *(uint32_t*)ptr;
  }

  free(recv_packet.data);

  return result;
}

/* ############################### Deprecated ############################### */
ChiResult
Chi_addMp(uint32_t mpId, const char *fruId, ChiCoreRole coreRole)
{
  (void)mpId;(void)fruId;(void)coreRole;
  return CHI_RESULT_OK;
}

/* ############################### Deprecated ############################### */
ChiResult Chi_removeMp(uint32_t mpId)
{
  (void)mpId;
  return CHI_RESULT_OK;
}
