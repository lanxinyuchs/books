/*
 *
 * Copyright (c) Ericsson AB  2012-2015 All rights reserved.
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

#include "osetypes.h"
#include "cello_te_ose.h"
#include "cello_piu.h"
#include "cec.h"
#include "cello_te_trace.h"
#include "cello_te_trace_obj.h"

#include "cello_piu.sig"

DECLARE_TRACE_OBJ(OSA_PRI_IF)
DECLARE_TRACE_OBJ(OSA_PRI_IF2)
DECLARE_TRACE_OBJ(OSA_PRI_IF3)
DECLARE_TRACE_OBJ(OSA_PRI_IF4)
DECLARE_TRACE_OBJ(OSA_PRI_IF5)
DECLARE_TRACE_OBJ(OSA_PRI_IF6)
DECLARE_TRACE_OBJ(OSA_PRI_IF7)
DECLARE_TRACE_OBJ(OSA_PRI_IF8)
DECLARE_TRACE_OBJ(OSA_PRI_IF9)
DECLARE_TRACE_OBJ(OSA_PRI_IF10)
DECLARE_TRACE_OBJ(OSA_PRI_IF11)
DECLARE_TRACE_OBJ(OSA_PRI_IF12)
DECLARE_TRACE_OBJ(OSA_PRI_IF13)

/* Define values for availabilityState */
#define UNAVAILABLE (0)
#define INITIATING  (1)
#define AVAILABLE   (2)
#define TERMINATING (3)

#define INIT_SERVICE (0)
#define TERM_SERVICE (1)
#define PIU3_GET_HUNT_PATH (2)
#define PIU10_GET_PID (3)
#define PRI8_GET_OWN_IDENTITY (4)
#define PRI9_GET_OWN_IDENTITY (5)
#define PIU4_RESTART_OWN_PIU (6)
#define PIU4_RESTART_OTHER_PIU (7)
#define PIU3_GET_OWN_IDENTITY (8)
#define PRI9_GET_PIU_OR_DEVICE_IDENTITY (9)
#define PRI8_GET_PIU_OR_DEVICE_IDENTITY (10)
#define PIU3_GET_LINK_HANDLER_NAME (11)

#define PRI_MAX_RESTART_CAUSE_LEN (60)

typedef struct
{
  cec_handle_t *handle;
  PROCESS spid;
  U32 availabilityState;     /* availability state           */
} PriProxyData;


union SIGNAL
{
  SIGSELECT sigNo;
  CelloPriTerminateServiceCfm celloPriTerminateServiceCfm;
};


static CelloPriResult findMyProxyData(void *, PriProxyData **);
static CelloPriResult checkAvailability(void *, PriProxyData **);

void*
CelloPri_initiateMemory(void)
{
  PriProxyData* priProxy_p;

  ENTER("CelloPri_initiateMemory");

  priProxy_p = (PriProxyData *)get_envp(current_process(), "PRI_PROXY_MEM");

  if (priProxy_p == NULL) {
    /* Initiate default values for the data. */
    priProxy_p = malloc(sizeof *priProxy_p);
    priProxy_p->handle = NULL;
    priProxy_p->spid = current_process();
    priProxy_p->availabilityState = UNAVAILABLE;

    set_envp(current_process(), "PRI_PROXY_MEM", (OSADDRESS)priProxy_p);
  }

  RETURN priProxy_p;
}

CelloPriResult
CelloPri_initiateService(void *priMemory_p,
                         U32   pvFirstWanted,
                         U32   pvSecondWanted,
                         U32   pvThirdWanted)
{
  PriProxyData *priProxy_p = NULL;
  CelloPriResult result;

  cec_packet_t packet;
  char buffer[5*sizeof(U32)];
  char *ptr;
  char signature[] = {'P', 'R', 'I'} ;

  ENTER(STR("CelloPri_initiateService, yourRef: %p", priMemory_p));

  result = findMyProxyData(priMemory_p, &priProxy_p);
  if (result != CELLO_PRI_OK) {
    RETURN result;
  }

  TRACE_OBJ(5, OSA_PRI_IF, STR("CelloPri_initiateService: "
  			       "Call is received in state = %d",
  			       (int)priProxy_p->availabilityState));

  switch (priProxy_p->availabilityState) {
  case UNAVAILABLE:

    priProxy_p->handle = cec_open(signature, sizeof(signature));

    packet.length = sizeof(buffer);
    ptr = packet.data = buffer;

    *(U32*)ptr = INIT_SERVICE;
    ptr += sizeof(U32);

    *(U32*)ptr = priProxy_p->spid;
    ptr += sizeof(U32);

    *(U32*)ptr = pvFirstWanted;
    ptr += sizeof(U32);

    *(U32*)ptr = pvSecondWanted;
    ptr += sizeof(U32);

    *(U32*)ptr = pvThirdWanted;

    cec_send_with_pid(priProxy_p->handle, &packet);

    /* Update the availability state variables */
    priProxy_p->availabilityState = INITIATING;

    RETURN CELLO_PRI_OK;
  default:
    RETURN CELLO_PRI_ERROR_SERVER_NOT_AVAILABLE;
  }
}

CelloPriResult
CelloPri_terminateService(void *priMemory_p,
			  void *optionalData __attribute__((unused)))
{
  PriProxyData *priProxy_p = NULL;
  CelloPriResult result;
  union SIGNAL *sigSend;

  cec_packet_t packet;
  char buffer[sizeof(U32)];
  char *ptr;

  ENTER(STR("CelloPri_terminateService, yourRef: %p", priMemory_p));

  result = findMyProxyData(priMemory_p, &priProxy_p);
  if (result != CELLO_PRI_OK) {
    RETURN result;
  }

  TRACE_OBJ(5, OSA_PRI_IF, STR("CelloPri_terminateService: "
			       "Call is received in state = %d",
			       (int)priProxy_p->availabilityState));

  switch (priProxy_p->availabilityState) {
  case AVAILABLE:
  case INITIATING:
    packet.length = sizeof(buffer);
    ptr = packet.data = buffer;

    *(U32*)ptr = TERM_SERVICE;
    cec_send_with_pid(priProxy_p->handle, &packet);
    priProxy_p->availabilityState = TERMINATING;
    break;
  case TERMINATING:
    break;
  default:
    /* In all other states the server is not up so the proxy sends Cfm */
    sigSend = alloc(sizeof(CelloPriTerminateServiceCfm),
		    CELLO_PRI_TERMINATE_SERVICE_CFM);
    send(&sigSend, current_process());
  }

  RETURN CELLO_PRI_OK;
}

void
CelloPri_freeMemory(void **priMemory_p)
{
  PriProxyData *priProxy_p;

  ENTER(STR("CelloPri_freeMemory, yourRef: %p", *priMemory_p));

  priProxy_p = (PriProxyData *)get_envp(0, "PRI_PROXY_MEM");
  if (priProxy_p != NULL) {
    switch (priProxy_p->availabilityState) {
    case UNAVAILABLE:
    case TERMINATING:
      free(priProxy_p);
      set_envp(0, "PRI_PROXY_MEM", (OSADDRESS)0);
      break;
    default:
      break;
    }
  }

  RETURN;
}

CelloPriResult
CelloPri_internal(void         *priMemory_p,
		  union SIGNAL *signal)
{
  CelloPriResult  result;
  PriProxyData   *priProxy_p = NULL;

  ENTER(STR("CelloPri_internal, sigNo: 0x%08x", signal->sigNo));

  result = findMyProxyData(priMemory_p, &priProxy_p);
  if (result != CELLO_PRI_OK) {
    RETURN result;
  }

  switch(signal->sigNo)
    {
    case CELLO_PRI_SERVER_UP_IND:
      break;
    case CELLO_PRI_SERVER_DOWN_IND:
      priProxy_p->availabilityState = UNAVAILABLE;
      break;
    case CELLO_PRI_INITIATE_SERVICE_CFM:
      priProxy_p->availabilityState = AVAILABLE;
      break;
    case CELLO_PRI_INITIATE_SERVICE_REJ:
      priProxy_p->availabilityState = UNAVAILABLE;
      break;
    case CELLO_PRI_TERMINATE_SERVICE_CFM:
      priProxy_p->availabilityState = UNAVAILABLE;
      break;
    default:
      RETURN CELLO_PRI_UNKNOWN_SIGNAL;
    }

  RETURN CELLO_PRI_OK;
}

CelloPriResult
CelloPiu3_getHuntPath(void *priMemory_p,
		      U32   piuInstanceId,
		      char *processName_p,
		      U32   clientId)
{
  PriProxyData *priProxy_p = NULL;
  CelloPriResult result;

  cec_packet_t packet;
  char buffer[3*sizeof(U32) + CELLO_PIU_HUNT_PATH_SIZE];
  char *ptr;

  ENTER(STR("CelloPiu3_getHuntPath: piuInstanceId = %d, processName = %s",
	    piuInstanceId, processName_p));

  result = checkAvailability(priMemory_p, &priProxy_p);
  if (result != CELLO_PRI_OK)
    {
      RETURN result;
    }

  packet.length = sizeof(buffer);
  ptr = packet.data = buffer;

  *(U32*)ptr = PIU3_GET_HUNT_PATH;
  ptr += sizeof(U32);

  *(U32*)ptr = piuInstanceId;
  ptr += sizeof(U32);

  *(U32*)ptr = clientId;
  ptr += sizeof(U32);

  strncpy(ptr, processName_p, CELLO_PIU_HUNT_PATH_SIZE);
  ptr[CELLO_PIU_HUNT_PATH_SIZE - 1]= '\0';

  cec_send_with_pid(priProxy_p->handle, &packet);

  RETURN CELLO_PRI_OK;
}

CelloPriResult
CelloPiu10_getPid(void *priMemory_p,
		  U32   piuInstanceId,
		  U32   clientId)
{
  PriProxyData *priProxy_p = NULL;
  CelloPriResult result;

  cec_packet_t packet;
  char buffer[3*sizeof(U32)];
  char *ptr;

  ENTER(STR("CelloPiu10_getPid, yourRef: %p", (void*)priMemory_p));

  result = checkAvailability(priMemory_p, &priProxy_p);
  if (result != CELLO_PRI_OK)
    {
      RETURN result;
    }

  packet.length = sizeof(buffer);
  ptr = packet.data = buffer;

  *(U32*)ptr = PIU10_GET_PID;
  ptr += sizeof(U32);

  *(U32*)ptr = piuInstanceId;
  ptr += sizeof(U32);

  *(U32*)ptr = clientId;

  cec_send_with_pid(priProxy_p->handle, &packet);

  RETURN CELLO_PRI_OK;
}

CelloPriResult
CelloPiu3_getOwnIdentity(void *priMemory_p)
{
  PriProxyData *priProxy_p = NULL;
  CelloPriResult result;

  cec_packet_t packet;
  char buffer[sizeof(U32)];
  char *ptr;

  ENTER("CelloPiu3_getOwnIdentity");

  result = checkAvailability(priMemory_p, &priProxy_p);
  if (result != CELLO_PRI_OK)
    {
      RETURN result;
    }

  packet.length = sizeof(buffer);
  ptr = packet.data = buffer;

  *(U32*)ptr = PIU3_GET_OWN_IDENTITY;

  cec_send_with_pid(priProxy_p->handle, &packet);

  RETURN CELLO_PRI_OK;
}

CelloPriResult
CelloPri9_getOwnIdentity(void *priMemory_p,
			 U32   clientId)
{
  PriProxyData *priProxy_p = NULL;
  CelloPriResult result;

  cec_packet_t packet;
  char buffer[2*sizeof(U32)];
  char *ptr;

  ENTER("CelloPiu9_getOwnIdentity");

  result = checkAvailability(priMemory_p, &priProxy_p);
  if (result != CELLO_PRI_OK)
    {
      RETURN result;
    }

  packet.length = sizeof(buffer);
  ptr = packet.data = buffer;

  *(U32*)ptr = PRI9_GET_OWN_IDENTITY;
  ptr += sizeof(U32);

  *(U32*)ptr = clientId;

  cec_send_with_pid(priProxy_p->handle, &packet);

  RETURN CELLO_PRI_OK;
}

CelloPriResult
CelloPri8_getOwnIdentity(void *priMemory_p,
			 U32   clientId)
{
  PriProxyData *priProxy_p = NULL;
  CelloPriResult result;

  cec_packet_t packet;
  char buffer[2*sizeof(U32)];
  char *ptr;

  ENTER("CelloPri8_getOwnIdentity");

  result = checkAvailability(priMemory_p, &priProxy_p);
  if (result != CELLO_PRI_OK)
    {
      RETURN result;
    }

  packet.length = sizeof(buffer);
  ptr = packet.data = buffer;

  *(U32*)ptr = PRI8_GET_OWN_IDENTITY;
  ptr += sizeof(U32);

  *(U32*)ptr = clientId;

  cec_send_with_pid(priProxy_p->handle, &packet);

  RETURN CELLO_PRI_OK;
}

CelloPriResult
CelloPri9_getPiuOrDeviceIdentity(void *priMemory_p,
				 U32   piuOrDeviceId,
				 U32   clientId)
{
  PriProxyData *priProxy_p = NULL;
  CelloPriResult result;

  cec_packet_t packet;
  char buffer[3*sizeof(U32)];
  char *ptr;

  ENTER("CelloPri9_getPiuOrDeviceIdentity");

  result = checkAvailability(priMemory_p, &priProxy_p);
  if (result != CELLO_PRI_OK)
    {
      RETURN result;
    }

  packet.length = sizeof(buffer);
  ptr = packet.data = buffer;

  *(U32*)ptr = PRI9_GET_PIU_OR_DEVICE_IDENTITY;
  ptr += sizeof(U32);

  *(U32*)ptr = piuOrDeviceId;
  ptr += sizeof(U32);

  *(U32*)ptr = clientId;

  cec_send_with_pid(priProxy_p->handle, &packet);

  RETURN CELLO_PRI_OK;
}

CelloPriResult
CelloPri8_getPiuOrDeviceIdentity(void *priMemory_p,
				 U32   piuOrDeviceId,
				 U32   clientId)
{
  PriProxyData *priProxy_p = NULL;
  CelloPriResult result;

  cec_packet_t packet;
  char buffer[3*sizeof(U32)];
  char *ptr;

  ENTER("CelloPri8_getPiuOrDeviceIdentity");

  result = checkAvailability(priMemory_p, &priProxy_p);
  if (result != CELLO_PRI_OK)
    {
      RETURN result;
    }

  packet.length = sizeof(buffer);
  ptr = packet.data = buffer;

  *(U32*)ptr = PRI8_GET_PIU_OR_DEVICE_IDENTITY;
  ptr += sizeof(U32);

  *(U32*)ptr = piuOrDeviceId;
  ptr += sizeof(U32);

  *(U32*)ptr = clientId;

  cec_send_with_pid(priProxy_p->handle, &packet);

  RETURN CELLO_PRI_OK;
}

CelloPriResult
CelloPiu4_restartOwnPiu(void   *priMemory_p,
			U32     restartRank,
			Boolean restartEscalation,
			char   *restartCause,
			U32     clientId)
{
  PriProxyData *priProxy_p = NULL;
  CelloPriResult result;

  cec_packet_t packet;
  char buffer[4*sizeof(U32) + PRI_MAX_RESTART_CAUSE_LEN];
  char *ptr;

  ENTER("CelloPiu4_restartOwnPiu");

  result = checkAvailability(priMemory_p, &priProxy_p);
  if (result != CELLO_PRI_OK)
    {
      RETURN result;
    }

  packet.length = sizeof(buffer);
  ptr = packet.data = buffer;

  *(U32*)ptr = PIU4_RESTART_OWN_PIU;
  ptr += sizeof(U32);

  *(U32*)ptr = restartRank;
  ptr += sizeof(U32);

  *(U32*)ptr = restartEscalation;
  ptr += sizeof(U32);

  *(U32*)ptr = clientId;
  ptr += sizeof(U32);

  strncpy(ptr, restartCause, PRI_MAX_RESTART_CAUSE_LEN);
  ptr[PRI_MAX_RESTART_CAUSE_LEN - 1]= '\0';

  cec_send_with_pid(priProxy_p->handle, &packet);

  RETURN CELLO_PRI_OK;
}

CelloPriResult
CelloPiu4_restartOtherPiu(void   *priMemory_p,
			  U32     piuInstanceId,
			  U32     restartRank,
			  Boolean restartEscalation,
			  char   *restartCause,
			  U32     clientId)
{
  PriProxyData *priProxy_p = NULL;
  CelloPriResult result;

  cec_packet_t packet;
  char buffer[5*sizeof(U32) + PRI_MAX_RESTART_CAUSE_LEN];
  char *ptr;

  ENTER(STR("CelloPiu4_restartOtherPiu(piuId = %d)", piuInstanceId));

  result = checkAvailability(priMemory_p, &priProxy_p);
  if (result != CELLO_PRI_OK)
    {
      RETURN result;
    }

  packet.length = sizeof(buffer);
  ptr = packet.data = buffer;

  *(U32*)ptr = PIU4_RESTART_OTHER_PIU;
  ptr += sizeof(U32);

  *(U32*)ptr = piuInstanceId;
  ptr += sizeof(U32);

  *(U32*)ptr = restartRank;
  ptr += sizeof(U32);

  *(U32*)ptr = restartEscalation;
  ptr += sizeof(U32);

  *(U32*)ptr = clientId;
  ptr += sizeof(U32);

  strncpy(ptr, restartCause, PRI_MAX_RESTART_CAUSE_LEN);
  ptr[PRI_MAX_RESTART_CAUSE_LEN - 1]= '\0';

  cec_send_with_pid(priProxy_p->handle, &packet);

  RETURN CELLO_PRI_OK;
}

CelloPriResult
CelloPiu3_getLinkHandlerName(void   *priMemory_p,
			     U32     piuInstanceId,
			     U32     clientId)
{
  PriProxyData *priProxy_p = NULL;
  CelloPriResult result;

  cec_packet_t packet;
  char buffer[3*sizeof(U32)];
  char *ptr;

  ENTER(STR("CelloPiu3_getLinkHandlerName(piuId = %d)", piuInstanceId));

  result = checkAvailability(priMemory_p, &priProxy_p);
  if (result != CELLO_PRI_OK)
    {
      RETURN result;
    }

  packet.length = sizeof(buffer);
  ptr = packet.data = buffer;

  *(U32*)ptr = PIU3_GET_LINK_HANDLER_NAME;
  ptr += sizeof(U32);

  *(U32*)ptr = piuInstanceId;
  ptr += sizeof(U32);

  *(U32*)ptr = clientId;

  cec_send_with_pid(priProxy_p->handle, &packet);

  RETURN CELLO_PRI_OK;
}

static CelloPriResult
findMyProxyData(void *priMemory_p, PriProxyData **myData_p)
{
  *myData_p = (PriProxyData *)get_envp(current_process(), "PRI_PROXY_MEM");

  if (*myData_p == NULL) {
    /* TRACE_OBJ(5, OSA_PRI_IF, */
    /* 		STR("Client has not initiated PRI proxy memory")); */
    return CELLO_PRI_MEMORY_NOT_INITIATED;
  }

  if (*myData_p != priMemory_p) {
    TRACE_OBJ(5, OSA_PRI_IF,
              STR("Provided priMemory_p = %p does not point to allocated"
                  " memory buffer = %p pid = 0x%x",
                  (void*)priMemory_p, (void*)*myData_p,
                  (int)current_process()));
  }
  return CELLO_PRI_OK;
}

static CelloPriResult
checkAvailability(void *priMemory_p, PriProxyData **myData_p)
{
  CelloPriResult result;

  result = findMyProxyData(priMemory_p, myData_p);
  if (result == CELLO_PRI_OK) {
    if((*myData_p)->availabilityState != AVAILABLE) {
      /* The state of the proxy is incorrect. */
      TRACE_OBJ(5, OSA_PRI_IF, STR("The state is not AVAILABLE but = %d",
                                   (*myData_p)->availabilityState));
      result = CELLO_PRI_ERROR_SERVER_NOT_AVAILABLE;
    }
  }
  return result;
}
