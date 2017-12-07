/* ----------------------------------------------------------------------
 * %CCaseFile:	pes_pei.c %
 * %CCaseRev:	/main/R3A/R6A/1 %
 * %CCaseDate:	2016-07-07 %
 * %CCaseDocNo: %
 * Author: <name>, <e-mail address>
 *
 * Short description:
 * PES C interface.
 * ----------------------------------------------------------------------
 * %CCaseTemplateFile:	template.c %
 * %CCaseTemplateId: CCver: /main/1 %
 *
 * %CCaseCopyrightBegin%
 * Copyright (c) Ericsson AB 2014-2016 All rights reserved.
 *
 * The information in this document is the property of Ericsson.
 *
 * Except as specifically authorized in writing by Ericsson, the
 * receiver of this document shall keep the information contained
 * herein confidential and shall protect the same in whole or in
 * part from disclosure and dissemination to third parties.
 *
 * Disclosure and disseminations to the receivers employees shall
 * only be made on a strict need to know basis.
 * %CCaseCopyrightEnd%
 *
 * ----------------------------------------------------------------------
 *
 * Revision history:
 *
 * Rev        Date       Name        What
 * -----      -------    --------    --------------------------
 * R1A/1      2014-11-07 eolaand     Created
 * R6A/1      2016-07-07 etxpeno     Coverity fixes
 * ----------------------------------------------------------------------
 */


#define TRACEPOINT_DEFINE
#include "pes_trace.h"

#define INFO( fmt,...)  TRACE_HELPER(info_trace, fmt, __VA_ARGS__)
#define ERROR( fmt,...) TRACE_HELPER(error_trace, fmt, __VA_ARGS__)
#define DEBUG( fmt,...) TRACE_HELPER(debug_trace, fmt, __VA_ARGS__)

#define TRACE_HELPER(type, fmt,...) do {	\
  char *err_str;\
  asprintf(&err_str,"%s:%d:%s():" fmt, __FILE__,__LINE__, __func__, __VA_ARGS__); \
  tracepoint(com_ericsson_pes, type , err_str);\
  free(err_str);\
  } while(0)






#define TRACE_TO_FILE 0

#define _GNU_SOURCE

#include "pes_pei.h"
#include "pes_hashmap.h"
#include <errno.h>
#include <stdlib.h>
#include <stdio.h>
#include <sys/socket.h>
#include <string.h>
#include <unistd.h>
#include <netdb.h>
#include "cec.h"

#include <stdint.h>
#include <stdbool.h>

#define SIGNATURE {'P', 'E', 'I'}
// #define CEC_RECEIVE_TIMEOUT 100000
#define CEC_RECEIVE_TIMEOUT 3000
#define INITIALIZE_RESPONSE_TIMEOUT 60000


#define PEI_INITIALIZE_MIN_SIZE        (7)

typedef enum {
  INITIALIZE     = 1,
  INITIALIZE_RES = 2,
  ME_ATTR_UPDATE = 3,
  EVENT_JOB      = 4,
  FINALIZE       = 5
} FunctionCodeT;


typedef struct {
  int socketfd;
  cec_handle_t *connectionHandle;
  PeiCallbacksT *callbacks;
} ApplicationDataT;


#if TRACE_TO_FILE
/**
 * Crude trace facility, messages are written to a
 * file under /dev/shm/${LOGNAME} (the directory is
 * trusted to exist).
 *
 * This function must be called before the first
 * invocation of traceLib(), else a silent crash
 * will occur.
 *
 * Requires _GNU_SOURCE and <stdio.h>.
 *
 */
static char *traceFileNameInLib = NULL;


static void traceLibInit(char *prefix) {
  if (traceFileNameInLib == NULL) {
      char *tempnamArg1;
      asprintf(&tempnamArg1, "/tmp/%s/", getenv("LOGNAME"));
      asprintf(&traceFileNameInLib, "%s", tempnam(tempnamArg1, prefix));
      FILE *f = fopen(traceFileNameInLib, "w");
      fclose(f);
  }
}

static void traceLib(char *msg) {
  FILE *f = fopen(traceFileNameInLib, "a");
  fprintf(f, "%s\n", msg);
  fclose(f);
}

static void traceLibD(char *msg, int k) {
  FILE *f = fopen(traceFileNameInLib, "a");
  fprintf(f, "%s%d\n", msg, k);
  fclose(f);
}

static void traceLibS(char *msg, char *s) {
  FILE *f = fopen(traceFileNameInLib, "a");
  fprintf(f, "%s%s\n", msg, s);
  fclose(f);
}

static void traceLibP(char *msg, void *p) {
  FILE *f = fopen(traceFileNameInLib, "a");
  fprintf(f, "%s%p\n", msg, p);
  fclose(f);
}
#endif


PeiResultT peiFinishInitialize(PeiResultT result,
			       cec_packet_t *packet,
			       ApplicationDataT *appData) {
  if (packet->data != NULL) {
    free(packet->data);
  }

  if (result == PEI_OK) {
    return result;
  }
  else {
    if (appData != NULL) {
      free(appData->callbacks->eventJobCallbackSpec);
      free(appData->callbacks->meAttrUpdateCallbackSpec);
      free(appData->callbacks);
      free(appData->connectionHandle);
      free(appData);
    }

    return PEI_INITIALIZE + result;
  }
}


/*
 * Tries to set up another connection to the CS.
 * Stores the connection in static memory and
 * returns a handle.
 *
 * The allocated memory is freed when a subsequent
 * call of peiFinalize() with a matching handle is
 * made.
 */
PeiResultT peiInitialize(PeiHandleT *peiHandle,
			 char *eventMapId,
			 const PeiCallbacksT *peiCallbacks) {

  /* #if TRACE_TO_FILE */
  /*   traceLibInit("lib"); */
  /*   traceLib("I execute, therefore I am"); */
  /* #endif */

  INFO("peiInitialize: eventMap = %s", eventMapId);

  char signature[] = SIGNATURE;
  *peiHandle = NULL;
  (void) eventMapId;

  if (eventMapId == NULL) {
    ERROR("peiInitialize: eventMap = %s", "NULL");
    return PEI_BAD_PARAMETER;
  }

  if (peiCallbacks == NULL) {
    ERROR("peiInitialize: peiCallbacks = %s", "NULL");
    return PEI_BAD_PARAMETER;
  }

  if (PEI_MAX_SESSIONS != PES_HASHMAP_SIZE) {
    return PEI_INTERNAL_LIMITS_INCONSISTENCY;
  }

  ApplicationDataT *appData = malloc(sizeof(ApplicationDataT));
  if (appData == NULL) {
    if (errno == ENOMEM) {
      ERROR("peiInitialize:  %s", "PEI_INTERNAL_OUT_OF_MEMORY");
      return peiFinishInitialize(PEI_LOCATION_1 + PEI_INTERNAL_OUT_OF_MEMORY,
				 NULL, NULL);
    }
    else {
      ERROR("peiInitialize:  %s", "PEI_INTERNAL_ALLOCATION_FAILURE");
      return peiFinishInitialize(PEI_LOCATION_1 +
				 PEI_INTERNAL_ALLOCATION_FAILURE,
				 NULL, NULL);
    }
  }

  appData->callbacks = malloc(sizeof(PeiCallbacksT));

  uint8_t evjob_cb = true;
  if (peiCallbacks->eventJobCallbackSpec == NULL || peiCallbacks->eventJobCallbackSpec->eventJobCallback == NULL) {
    evjob_cb = false;
    appData->callbacks->eventJobCallbackSpec = NULL;
  }
  else {
    appData->callbacks->eventJobCallbackSpec =
      malloc(sizeof(PeiEventJobCallbackSpecT));

    appData->callbacks->eventJobCallbackSpec->eventJobCallback =
      peiCallbacks->eventJobCallbackSpec->eventJobCallback;
    appData->callbacks->eventJobCallbackSpec->userData =
      peiCallbacks->eventJobCallbackSpec->userData;
    INFO("peiInitialize: eventJobCallback %d  userData = %s",
	 peiCallbacks->eventJobCallbackSpec->eventJobCallback,
	 peiCallbacks->eventJobCallbackSpec->userData);
  }

  uint8_t meattr_cb = true;
  if (peiCallbacks->meAttrUpdateCallbackSpec == NULL || peiCallbacks->meAttrUpdateCallbackSpec->meAttrUpdateCallback == NULL) {
    meattr_cb = false;
    appData->callbacks->meAttrUpdateCallbackSpec = NULL;
  }
  else {
    appData->callbacks->meAttrUpdateCallbackSpec =
      malloc(sizeof(PeiMEAttrUpdateCallbackSpecT));

    appData->callbacks->meAttrUpdateCallbackSpec->meAttrUpdateCallback =
      peiCallbacks->meAttrUpdateCallbackSpec->meAttrUpdateCallback;
    appData->callbacks->meAttrUpdateCallbackSpec->userData =
      peiCallbacks->meAttrUpdateCallbackSpec->userData;
    INFO("peiInitialize: meAttrUpdateCallback %d  userData %s",
	 peiCallbacks->meAttrUpdateCallbackSpec->meAttrUpdateCallback,
	 peiCallbacks->meAttrUpdateCallbackSpec->userData);
  }

  appData->connectionHandle = cec_open(signature, sizeof(signature));
  if (appData->connectionHandle == NULL) {
    ERROR("peiInitialize:  %s", "PEI_INTERNAL_CEC_OPEN_FAILURE");
    return peiFinishInitialize(PEI_INTERNAL_CEC_OPEN_FAILURE, NULL, appData);
  }

  int cResult = peiMap_createKeyAndPut("handlePei", appData, peiHandle);
  if (cResult != PES_HASHMAP_OK) {
    if (cResult == PES_HASHMAP_MAX_KEYS_EXCEEDED) {
      ERROR("peiInitialize:  %s", "PEI_INTERNAL_MAX_HANDLES_EXCEEDED");
      return peiFinishInitialize(PEI_INTERNAL_MAX_HANDLES_EXCEEDED, NULL,
				 appData);
    }
    else if (cResult == PES_HASHMAP_OUT_OF_MEMORY) {
      ERROR("peiInitialize:  %s", "PEI_INTERNAL_HASHMAP_OUT_OF_MEMORY");
      return peiFinishInitialize(PEI_INTERNAL_HASHMAP_OUT_OF_MEMORY, NULL,
				 appData);
    }
    else if (cResult == PES_HASHMAP_OVERWRITE) {
      ERROR("peiInitialize:  %s", "PEI_INTERNAL_HASHMAP_OVERWRITE");
      return peiFinishInitialize(PEI_INTERNAL_HASHMAP_OVERWRITE, NULL,
				 appData);
    }
    else if (cResult == PES_HASHMAP_UNSPECIFIC_FAILURE) {
      ERROR("peiInitialize:  %s", "PEI_INTERNAL_HASHMAP_FAILURE");
      return peiFinishInitialize(PEI_INTERNAL_HASHMAP_FAILURE, NULL, appData);
    }
    else {
      // must never happen; would be a coding error
      ERROR("peiInitialize:  %s", "PEI_INTERNAL_HASHMAP_INTERFACE_ERROR");
      return peiFinishInitialize(PEI_INTERNAL_HASHMAP_INTERFACE_ERROR, NULL,
				 appData);
    }
  }

  int idlength = strlen(eventMapId);

  char buffer[PEI_INITIALIZE_MIN_SIZE + idlength];
  cec_packet_t packet;
  char *ptr;

  packet.length = sizeof(buffer);
  ptr = packet.data = buffer;

  *(uint8_t*)ptr = INITIALIZE;
  ptr+=1;

  *(uint32_t*)ptr = idlength;
  ptr+= sizeof(uint32_t);
  memcpy(ptr, eventMapId, idlength);
  ptr+= idlength;

  *(uint8_t*)ptr = evjob_cb;
  ptr+=1;
  *(uint8_t*)ptr = meattr_cb;

  if (cec_send(appData->connectionHandle, &packet) < 0) {
    ERROR("peiInitialize:  %s", "PEI_INTERNAL_CEC_SEND_FAILURE");
    return peiFinishInitialize(PEI_INTERNAL_CEC_SEND_FAILURE, &packet,
			       appData);
  }

  if (cec_receive_w_tmeout(appData->connectionHandle, &packet,
			   INITIALIZE_RESPONSE_TIMEOUT) == -1) {
    ERROR("peiInitialize:  %s", "PEI_INTERNAL_CEC_RECEIVE_FAILURE");
    return peiFinishInitialize(PEI_INTERNAL_CEC_RECEIVE_FAILURE, &packet,
			       appData);
  }

  char *recv_ptr;
  recv_ptr = packet.data;
  int functionCode = *(uint8_t *)recv_ptr;
  PeiResultT result = PEI_OK;

  if ((FunctionCodeT)functionCode == INITIALIZE_RES) {
    recv_ptr += sizeof(uint8_t);
    int erl_result = *(uint8_t *)recv_ptr;
    if (erl_result >= PEI_OK && erl_result <= PEI_INTERNAL_ERROR) {
      result = erl_result;
    } else {
      ERROR("peiInitialize:  %s", "PEI_INTERNAL_DECODE_FAILURE_1");
      result = PEI_INTERNAL_DECODE_FAILURE;
    }
  } else {
    ERROR("peiInitialize:  %s", "PEI_INTERNAL_DECODE_FAILURE_2");
    result = PEI_INTERNAL_DECODE_FAILURE;
  }
  return peiFinishInitialize(result, &packet, appData);
}


PeiResultT peiSelectionObjectGet(
    PeiHandleT peiHandle,
    PeiSelectionObjectT *selectionObject) {

  ApplicationDataT *appData = peiMap_get(peiHandle);
  if (appData == NULL) {
      return PEI_SELECTION_OBJECT_GET + PEI_UNKNOWN_HANDLE;
  }

  *selectionObject = appData->connectionHandle->socket;
  INFO("peiSelectionObjectGet: %s", "PEI_OK");
  return PEI_OK;
}


PeiResultT peiFinishDispatch(PeiResultT result, cec_packet_t *packet) {
  if (packet->data != NULL) {
    free(packet->data);
  }

  return PEI_DISPATCH + result;
}


PeiResultT peiFinishMEAttrUpdate(PeiResultT result, cec_packet_t *packet) {
  if (packet->data != NULL) {
    free(packet->data);
  }
  if (result == PEI_OK) {
    return result;
  }
  else {
    return PEI_ME_ATTR_UPDATE + result;
  }
}


PeiResultT peiFinishEventJob(PeiResultT result, cec_packet_t *packet) {
  if (packet->data != NULL) {
    free(packet->data);
  }
  if (result == PEI_OK) {
    return result;
  }
  else {
    return PEI_EVENT_JOB + result;
  }
}


PeiResultT peiDispatch(PeiHandleT peiHandle) {

  cec_packet_t packet = {.data=NULL};

  ApplicationDataT *appData = peiMap_get(peiHandle);
  if (appData == NULL) {
    ERROR("peiDispatch:  %s", "PEI_UNKNOWN_HANDLE");
    return peiFinishDispatch(PEI_UNKNOWN_HANDLE, &packet);
  }

  if (cec_receive_w_tmeout(appData->connectionHandle, &packet, CEC_RECEIVE_TIMEOUT) == -1) {
    ERROR("peiDispatch:  %s", "PEI_INTERNAL_CEC_RECEIVE_FAILURE");
    return peiFinishDispatch(PEI_INTERNAL_CEC_RECEIVE_FAILURE, &packet);
  }

  char *ptr;
  ptr = packet.data;

  int functionCode = *(uint8_t *)ptr;
  ptr+= sizeof(uint8_t);

  DEBUG("peiDispatch: functionCode = %d", functionCode);
  printf("DISPATCH %d\n", functionCode);



  /*
   * Message = [FunctionCode | Msg]
   * where FunctionCode = uint8
   */

  if ((FunctionCodeT)functionCode == INITIALIZE_RES) {
    INFO("peiDispatch: initializeRes %s", "PEI_OK");
    return PEI_OK;
  }
  else if ((FunctionCodeT)functionCode == EVENT_JOB) {

    /*
     * =============
     * EVENT JOB
     * =============
     *
     * Message = [eventProducerId,eventJobId,requestedJobState,eventTypes,
     *            eventFilterIds,fileControl,streamControl]
     *
     */

    DEBUG("peiDispatch: %s", "eventJobCallback");
    printf("EVENT_JOB\n");
    char *eventProducerId;
    char *eventJobId;
    uint32_t requestedJobState = 0;
    uint32_t strlength = *(uint32_t *)ptr;
    ptr+= sizeof(uint32_t);

    eventProducerId = malloc(strlength + 1);
    memcpy(eventProducerId, ptr, strlength);
    eventProducerId[strlength] = '\0';
    ptr+= strlength;

    printf("eventProducerId %s\n", eventProducerId);

    strlength = *(uint32_t *)ptr;
    ptr+= sizeof(uint32_t);

    eventJobId = malloc(strlength + 1);
    memcpy(eventJobId, ptr, strlength);
    eventJobId[strlength] = '\0';
    ptr+= strlength;

    requestedJobState = *(uint32_t *)ptr;
    ptr+= sizeof(uint32_t);

    uint32_t noofEventTypes = *(uint32_t *)ptr;
    ptr+= sizeof(uint32_t);
    printf("Noof EventTypes %d\n", noofEventTypes);

    PeiEventTypeT *eventTypes;

    if (noofEventTypes == 0)
      eventTypes = NULL;
    else
      {
	printf("loop EventTypes \n");
	eventTypes = calloc(1, sizeof(PeiEventTypeT));
	eventTypes->noofTypeAliases = noofEventTypes;
	printf("Noof EventTypes %d\n", eventTypes->noofTypeAliases);

	uint32_t *eventTypeValues =
	  calloc(noofEventTypes + 1, sizeof(uint32_t));

	eventTypes->typeAliases = eventTypeValues;
	memcpy(eventTypes->typeAliases, ptr, noofEventTypes*sizeof(uint32_t));
	ptr+= noofEventTypes*sizeof(uint32_t);
      }

    for (uint32_t i = 0; i < noofEventTypes; i++) {
      printf("event type %d \n", eventTypes->typeAliases[i]);
    }

    uint32_t noofEventFilters = *(uint32_t *)ptr;
    ptr+= sizeof(uint32_t);
    printf("Noof EventFilters %d\n", noofEventFilters);

    PeiEventFilterT **eventFilterPointers =
      calloc(noofEventFilters + 1, sizeof(PeiEventFilterT *));
    PeiEventFilterT *eventFilters =
      calloc(noofEventFilters, sizeof(PeiEventFilterT));

    if (noofEventFilters == 0)
      eventFilterPointers[0] = NULL;
    else
      for (uint32_t i = 0; i < noofEventFilters; i++) {
	printf("event filter loop %d \n", i);
	eventFilterPointers[i] = &(eventFilters[i]);

	strlength = *(uint32_t *)ptr;
	ptr+= sizeof(uint32_t);
	printf("filter name length %d \n", strlength);
	eventFilters[i].name = malloc(strlength + 1);
	memcpy(eventFilters[i].name, ptr, strlength);
	eventFilters[i].name[strlength] = '\0';
	ptr+= strlength;
	printf("Filter name:  %s\n", eventFilters[i].name);

	strlength = *(uint32_t *)ptr;
	ptr+= sizeof(uint32_t);
	printf("filter value length %d \n", strlength);
	eventFilters[i].value = malloc(strlength + 1);
	memcpy(eventFilters[i].value, ptr, strlength);
	eventFilters[i].value[strlength] = '\0';
	ptr+= strlength;
	printf("Filter value:  %s\n", eventFilters[i].value);
      }

    printf("here \n");

    PeiFileControlT *fileControl = NULL;
    PeiStreamControlT *streamControl = NULL;

    uint8_t validcompr;
    uint8_t valid = *(uint8_t *)ptr;
    ptr+= sizeof(uint8_t);
    printf("file control valid %d \n", valid);
    if (valid) {
      fileControl = calloc(1, sizeof(PeiFileControlT));
      fileControl->reportingPeriod = *(uint32_t *)ptr;
      ptr+= sizeof(uint32_t);
      printf("file reportingPeriod %d \n", fileControl->reportingPeriod);
      validcompr = *(uint8_t *)ptr;
      ptr+= sizeof(uint8_t);
      if (validcompr) {
	PeiCompressionTypeT *fileCompr =
	  calloc(1, sizeof(PeiCompressionTypeT));
	fileCompr->value = *(uint32_t *)ptr;
	ptr+= sizeof(uint32_t);
	fileControl->compressionType = fileCompr;
	printf("file compressionType %d \n", fileCompr->value);
      }
      else {
	fileControl->compressionType = NULL;
      }
    }

    valid = *(uint8_t *)ptr;
    ptr+= sizeof(uint8_t);
    printf("stream control valid %d \n", valid);
    if (valid) {
      streamControl = calloc(1, sizeof(PeiStreamControlT));
      validcompr = *(uint8_t *)ptr;
      ptr+= sizeof(uint8_t);
      if (validcompr) {
	PeiCompressionTypeT *streamCompr =
	  calloc(1, sizeof(PeiCompressionTypeT));
	streamCompr->value = *(uint32_t *)ptr;
	ptr+= sizeof(uint32_t);
	streamControl->compressionType = streamCompr;
	printf("stream compressionType %d \n", streamCompr->value);
      }
      else {
	streamControl->compressionType = NULL;
      }
      strlength = *(uint32_t *)ptr;
      ptr+= sizeof(uint32_t);
      streamControl->destinationIpAddress = malloc(strlength + 1);
      memcpy(streamControl->destinationIpAddress, ptr, strlength);
      streamControl->destinationIpAddress[strlength] = '\0';
      ptr+= strlength;
      printf("Dest IP address  %s\n", streamControl->destinationIpAddress);
      streamControl->destinationPort = *(uint32_t *)ptr;
      ptr+= sizeof(uint32_t);
      printf("stream destinationPort %d \n", streamControl->destinationPort);
    }

    printf("EVENT_JOB callback\n");

	 /* (uint32_t)requestedJobState, */
	 /* (int const * const *)eventTypes, */
	 /* (char const * const *)eventFilterIds, */



    if (appData->callbacks != NULL) {
      DEBUG("peiInitialize: eventJobCallback %d  state: %d", appData->callbacks->eventJobCallbackSpec->eventJobCallback, requestedJobState);
      (appData->callbacks->eventJobCallbackSpec->eventJobCallback)
	(peiHandle,
	 eventProducerId,
	 eventJobId,
	 requestedJobState,
	 (PeiEventTypeT const *)eventTypes,
	 (PeiEventFilterT const * const *)eventFilterPointers,
	 fileControl,
	 streamControl,
	 appData->callbacks->eventJobCallbackSpec->userData);
    }

    if (fileControl) {
      free(fileControl->compressionType);
      free(fileControl);
    }

    if (streamControl) {
      free(streamControl->compressionType);
      free(streamControl->destinationIpAddress);
      free(streamControl);
    }

    if (eventTypes) {
      free(eventTypes->typeAliases);
      free(eventTypes);
    }

    free(eventJobId);
    free(eventProducerId);

    for (uint32_t i = 0; i < noofEventFilters; i++) {
      free(eventFilters[i].name);
      free(eventFilters[i].value);
    }
    free(eventFilters);

    free(eventFilterPointers);

    return peiFinishEventJob(PEI_OK, &packet);
  }
  else if ((FunctionCodeT)functionCode == ME_ATTR_UPDATE) {

    /*
     * =============
     * ME Attr Update
     * =============
     *
     * Message = [userLabel, networkManagedElementId]
     *
     */

    DEBUG("peiDispatch: %s", "meAttrUpdateCallback");
    printf("ME_ATTR_UPDATE\n");
    char *userLabel = NULL;
    char *networkManagedElementId = NULL;

    uint8_t valid = *(uint8_t *)ptr;
    ptr+= sizeof(uint8_t);
    printf("userLabel valid %d \n", valid);
    uint32_t strlength;
    if (valid) {
      strlength = *(uint32_t *)ptr;
      ptr+= sizeof(uint32_t);

      userLabel = malloc(strlength + 1);
      memcpy(userLabel, ptr, strlength);
      userLabel[strlength] = '\0';
      ptr+= strlength;

      printf("userLabel %s\n", userLabel);
    }

    valid = *(uint8_t *)ptr;
    ptr+= sizeof(uint8_t);
    printf("networkManagedElementId valid %d \n", valid);
    if (valid) {
      strlength = *(uint32_t *)ptr;
      ptr+= sizeof(uint32_t);

      networkManagedElementId = malloc(strlength + 1);
      memcpy(networkManagedElementId, ptr, strlength);
      networkManagedElementId[strlength] = '\0';
      ptr+= strlength;

      printf("networkManagedElementId %s\n", networkManagedElementId);
    }

    printf("ME_ATTR_UPDATE callback\n");

    if (appData->callbacks && appData->callbacks->meAttrUpdateCallbackSpec ) {
      (appData->callbacks->meAttrUpdateCallbackSpec->meAttrUpdateCallback)
	(peiHandle,
	 userLabel,
	 networkManagedElementId,
	 appData->callbacks->meAttrUpdateCallbackSpec->userData);
    }

    free(networkManagedElementId);
    free(userLabel);

    printf("ME_ATTR_UPDATE done\n");

    DEBUG("peiDispatch: meAttrUpdateCallback %s", "PEI_OK");
    return peiFinishMEAttrUpdate(PEI_OK, &packet);

  }
  else
    /*
     * ===============
     * UNKNOWN MESSAGE
     * ===============
     */
    INFO("peiDispatch: unknownMessage %s ", "PEI_INTERNAL_UNKNOWN_FUNCTION");
    return peiFinishDispatch(PEI_INTERNAL_UNKNOWN_FUNCTION, &packet);
}



PeiResultT peiFinishFinalize(PeiResultT result, char *buffer) {
  if (buffer != NULL) {
      free(buffer);
  }
  if (result == PEI_OK) {
      return result;
  }
  else {
      return PEI_FINALIZE + result;
  }
}


PeiResultT peiFinalize(PeiHandleT peiHandle) {

  INFO("peiFinalize: %s", " ");
  printf("peiFinalize\n");

  ApplicationDataT *appData = peiMap_get(peiHandle);
  if (appData == NULL) {
      return peiFinishFinalize(PEI_UNKNOWN_HANDLE, NULL);
  }

  /*
   *===========================================
   * encode the Finalize message
   *===========================================
   */

  cec_packet_t packet;
  char *ptr;

  char *buffer = calloc(2, 8);

  packet.length = 2;
  ptr = packet.data = buffer;

  // function code
  *(uint8_t*)ptr = FINALIZE;
  ptr+=1;

  if (cec_send(appData->connectionHandle, &packet) == -1) {
      ERROR("peiFinalize:  %s", "PEI_INTERNAL_CEC_SEND_FAILURE");
      return peiFinishFinalize(PEI_OK, buffer);
  }

  // Finally tear down connection to the CS
  if (cec_close(appData->connectionHandle) == -1) {
      ERROR("peiFinalize:  %s", "PEI_INTERNAL_CEC_CLOSE_FAILURE");
  }

  if (peiMap_clear(peiHandle) == PES_HASHMAP_OK) {
    INFO("peiFinalize:  %s", "PEI_OK");
  }
  else {
    ERROR("peiFinalize:  %s", "PEI_INTERNAL_HASHMAP_FAILURE");
  }

  if (appData && appData->callbacks) {
    free(appData->callbacks->eventJobCallbackSpec);
    free(appData->callbacks);
  }
  free(appData);

  return PEI_OK;
}
