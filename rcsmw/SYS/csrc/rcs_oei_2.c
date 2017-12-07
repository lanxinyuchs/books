/*
 *
 * Copyright (c) Ericsson AB  2017 All rights reserved.
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
#include <string.h>
#include <stdio.h>
#include <uuid/uuid.h>

#include "rcs_oei_2.h"

#include "cec.h"

/*
******************************************************************************
* MACROS
******************************************************************************
*/

#define OEI_GET 0
#define OEI_GET_EVENT_ID 1
#define OEI_GET_CORR_ID 2
#define OEI_GET_CORR_ID_2 3

/*
******************************************************************************
* TYPES
******************************************************************************
*/


uint32_t sendReceive(char* msg, uint32_t msgLen, uint32_t timeoutValue, char** resp) {
  char signature[] = {'O', 'E', 'I'};
  cec_handle_t *handle;
  cec_packet_t send_packet;
  cec_packet_t recv_packet;

  handle = cec_open(signature, sizeof(signature));
  if (handle == NULL) {
    return RCS_OEI_NO_SERVER;
  }

  send_packet.length = msgLen;
  send_packet.data = msg;

  if (cec_send(handle, &send_packet) < 0) {
    cec_close(handle);
    return RCS_OEI_NO_SERVER;
  }

  if (timeoutValue == RCS_OEI_NO_TIMEOUT_VALUE) {
    if (cec_receive(handle, &recv_packet) < 0) {
      cec_close(handle);
      return RCS_OEI_NO_SERVER;
    }
  }
  else {
    if (cec_receive_w_tmeout(handle, &recv_packet, timeoutValue) < 0) {
      cec_close(handle);
      return RCS_OEI_TIMEOUT;
    }
  }

  cec_close(handle);

  *resp = recv_packet.data;
  return RCS_OEI_OK;
}


RcsOeiResult_t
rcsOeiGetEventIdentity( uint32_t timeoutValue,
                        uint32_t *eventId_p)
{
  char buffer[4];
  uint32_t result;
  char* resp;
  uint32_t* p_method;

  p_method = (uint32_t*)&(buffer[0]);
  *p_method = OEI_GET_EVENT_ID;

  *eventId_p = RCS_OEI_UNSPECIFIED_EVENT_ID;

  result = sendReceive(buffer, sizeof(buffer), timeoutValue, &resp);

  *eventId_p = *(uint32_t*)resp;
  free(resp);

  /* Something went wrong in erlang
     emulate no server process
  */
  if (*eventId_p == RCS_OEI_UNSPECIFIED_EVENT_ID) {
    result = RCS_OEI_NO_SERVER;
  }

  return result;
}

RcsOeiResult_t
rcsOeiGetDerivedCorrelationUUID( uint32_t  eventId,
                                 uint32_t  timeoutValue,
                                 RcsOeiDerivedCorrelationUuid_t*  p_derivedCorrelationUUID ) 
{ 
  char buffer[8]; /* 2*uint32_t */
  uint32_t result;
  char* resp;
  uint32_t* p_method;
  uint32_t* p_eventId;
  uint32_t uuidLen;

  p_method = (uint32_t*)&(buffer[0]);
  p_eventId = (uint32_t*)&(buffer[4]);

  *p_method = OEI_GET_CORR_ID;
  *p_eventId = eventId;

  result = sendReceive(buffer, sizeof(buffer), timeoutValue, &resp);

  uuidLen = *(uint32_t*)resp;
  if (result != RCS_OEI_OK) {
    p_derivedCorrelationUUID->value[0] = 0;
  } 
  else if (uuidLen != RCS_OEI_UUID_LEN) {
    result = RCS_OEI_INVALID_UUID_FORMAT;
    p_derivedCorrelationUUID->value[0] = 0;
  } 
  else {
    strncpy(p_derivedCorrelationUUID->value, resp+sizeof(uint32_t), uuidLen);
    p_derivedCorrelationUUID->value[uuidLen] = 0;
  } 
  free(resp);

  return result;
}

RcsOeiResult_t
rcsOeiGetDerivedCorrelationUUID2( uint32_t  eventId,
                                  uint32_t  timeoutValue,
                                  RcsOeiSystemUuid_t*  p_systemUUID,
                                  RcsOeiDerivedCorrelationUuid_t*  p_derivedCorrelationUUID )
{
  char buffer[12 + RCS_OEI_UUID_LEN]; /* 3*uint32_t + RCS_OEI_UUID_LEN */
  uint32_t result;
  char* resp;
  uint32_t* p_method;
  uint32_t* p_eventId;
  uint32_t* p_uuidLen;
  char* p_corrId;
  uint32_t uuidLen;

  uuidLen = strlen(p_systemUUID->value);
  if (uuidLen != RCS_OEI_UUID_LEN) {
    p_derivedCorrelationUUID->value[0] = 0;
    return RCS_OEI_INVALID_UUID_FORMAT;
  }

  p_method = (uint32_t*)&(buffer[0]);
  p_eventId = (uint32_t*)&(buffer[4]);
  p_uuidLen = (uint32_t*)&(buffer[8]);
  p_corrId = &(buffer[12]);

  *p_method = OEI_GET_CORR_ID_2;
  *p_eventId = eventId;
  *p_uuidLen = uuidLen;
  strncpy(p_corrId, p_systemUUID->value, uuidLen);

  result = sendReceive(buffer, sizeof(buffer), timeoutValue, &resp);

  uuidLen = *(uint32_t*)resp;
  if (result != RCS_OEI_OK) {
    p_derivedCorrelationUUID->value[0] = 0;
  } 
  else if (uuidLen != RCS_OEI_UUID_LEN) {
    result = RCS_OEI_INVALID_UUID_FORMAT;
    p_derivedCorrelationUUID->value[0] = 0;
  } 
  else {
    strncpy(p_derivedCorrelationUUID->value, resp+sizeof(uint32_t), uuidLen);
    p_derivedCorrelationUUID->value[uuidLen] = 0;
  } 
  free(resp);

  return result;
}

RcsOeiResult_t 
rcsOeiGetUUID( RcsOeiUuid_t*  p_rcsOeiUUID )
{
  uuid_t uuid;  

  uuid_generate_time_safe(uuid); 
  uuid_unparse_lower(uuid, p_rcsOeiUUID->value); 

  return RCS_OEI_OK;
}

