/* ----------------------------------------------------------------------
 * %CCaseFile:	pms_pmi2.c %
 * %CCaseRev:	/main/R2A/R3A/R4A/R5A/R6A/R7A/R9A/3 %
 * %CCaseDate:	2017-04-05 %
 * %CCaseDocNo: %
 * Author:
 * Author: <name>, <e-mail address>
 *
 * Short description:
 * PM C interface.
 * ----------------------------------------------------------------------
 * %CCaseTemplateFile:	template.c %
 * %CCaseTemplateId: CCver: /main/1 %
 *
 * %CCaseCopyrightBegin%
 * Copyright (c) Ericsson AB 2014-2017 All rights reserved.
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
 * R1A/1      2014-08-14 uabesvi     Created
 * R1A/10     2014-10-16 uabesvi     updated for show counters
 * ----------------------------------------------------------------------
 */


#define TRACEPOINT_DEFINE
#include "pms_trace2.h"

#define INFO( fmt,...)  TRACE_HELPER(info_trace, fmt, __VA_ARGS__)
#define ERROR( fmt,...) TRACE_HELPER(error_trace, fmt, __VA_ARGS__)
#define DEBUG( fmt,...) TRACE_HELPER(debug_trace, fmt, __VA_ARGS__)

#define TRACE_HELPER(type, fmt,...) do {	\
  char *err_str;\
  asprintf(&err_str,"%s:%d:%s():" fmt, __FILE__,__LINE__, __func__, __VA_ARGS__); \
  tracepoint(com_ericsson_pmi2, type , err_str);\
  free(err_str);\
  } while(0)




#define TRACE_TO_FILE 0

#define _GNU_SOURCE

#include "pms_pmi2.h"
#include "pms_hashmap.h"
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

#define SIGNATURE {'P', 'M', 'I', '2'}

// #define CEC_RECEIVE_TIMEOUT 100000
#define CEC_RECEIVE_TIMEOUT 3000
#define INITIALIZE_RESPONSE_TIMEOUT 120000

// 1 byte  for message tag
// 3 bytes for CBs
// 1 byte  boolean, true if alias appdata file name is defined
// 4 bytes for length of alias appdata file name
// 1 end byte
#define PMI2_INITIALIZE_SIZE  (32)

typedef enum {
  INITIALIZE     = 1,
  INITIALIZE_RES = 2,
  COUNTER_MAP    = 3,
  SUBSCRIBE_ROP  = 4,
  REPORT_ROP     = 5,
  REPORT_SC      = 6,
  DATA_ROP       = 7,
  DATA_SC        = 8,
  FINALIZE       = 9
} FunctionCodeT;


typedef struct {
  int socketfd;
  cec_handle_t *connectionHandle;
  Pmi2CallbacksT *callbacks;
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


/* static void traceLibInit(char *prefix) { */
/*   if (traceFileNameInLib == NULL) { */
/*       char *tempnamArg1; */
/*       asprintf(&tempnamArg1, "/tmp/%s/", getenv("LOGNAME")); */
/*       asprintf(&traceFileNameInLib, "%s", tempnam(tempnamArg1, prefix)); */
/*       FILE *f = fopen(traceFileNameInLib, "w"); */
/*       fclose(f); */
/*   } */
/* } */

/* static void traceLib(char *msg) { */
/*   FILE *f = fopen(traceFileNameInLib, "a"); */
/*   fprintf(f, "%s\n", msg); */
/*   fclose(f); */
/* } */

/* static void traceLibD(char *msg, int k) { */
/*   FILE *f = fopen(traceFileNameInLib, "a"); */
/*   fprintf(f, "%s%d\n", msg, k); */
/*   fclose(f); */
/* } */

/* static void traceLibS(char *msg, char *s) { */
/*   FILE *f = fopen(traceFileNameInLib, "a"); */
/*   fprintf(f, "%s%s\n", msg, s); */
/*   fclose(f); */
/* } */

/* static void traceLibP(char *msg, void *p) { */
/*   FILE *f = fopen(traceFileNameInLib, "a"); */
/*   fprintf(f, "%s%p\n", msg, p); */
/*   fclose(f); */
/* } */
#endif


 Pmi2ResultT pmi2FinishInitialize(Pmi2ResultT result,
				  cec_packet_t *packet,
				  ApplicationDataT *appData) {

   if (packet != NULL) {
     if (packet->data != NULL) {
       free(packet->data);
     }
   }

   if (result == PMI2_OK) {
     return result;
   }
   else {
     if (appData != NULL) {
       free(appData->callbacks->subscribeRopCallbackSpec);
       free(appData->callbacks->reportRopCallbackSpec);
       free(appData->callbacks->reportShowCountersCallbackSpec);
       free(appData->callbacks);
       free(appData->connectionHandle);
       free(appData);
     }

     return PMI2_INITIALIZE + result;
   }
 }


/*
 * Tries to set up another connection to the CS.
 * Stores the connection in static memory and
 * returns a handle.
 *
 * The allocated memory is freed when a subsequent
 * call of pmi2Finalize() with a matching handle is
 * made.
 */
Pmi2ResultT pmi2Initialize(Pmi2HandleT *pmi2Handle,
			   const char *counterMapId,
			   const Pmi2CallbacksT *pmi2Callbacks) {

  /* #if TRACE_TO_FILE */
  /*   traceLibInit("lib"); */
  /*   traceLib("I execute, therefore I am"); */
  /* #endif */

  INFO("pmi2Initialize: counterMapId = %s", counterMapId);

  int bufferLen;

  if (counterMapId == NULL)
    bufferLen = PMI2_INITIALIZE_SIZE;
  else
    bufferLen = PMI2_INITIALIZE_SIZE + strlen(counterMapId);

  char buffer[bufferLen];
  cec_packet_t packet = {.data=NULL};
  char *ptr;

  INFO("pmi2Initialize: packet = %d", packet);

  char signature[] = SIGNATURE;
  *pmi2Handle = NULL;

  if (PMI2_MAX_SESSIONS != PMS_HASHMAP_SIZE) {
    ERROR("pmi2Initialize: %s", "PMI2_INTERNAL_LIMITS_INCONSISTENCY");
    return PMI2_INTERNAL_LIMITS_INCONSISTENCY;
  }

  ApplicationDataT *appData = malloc(sizeof(ApplicationDataT));
  if (appData == NULL) {
    if (errno == ENOMEM) {
      ERROR("pmi2Initialize: %s", "PMI2_INTERNAL_OUT_OF_MEMORY");
      return pmi2FinishInitialize(PMI2_LOCATION_1 +
				  PMI2_INTERNAL_OUT_OF_MEMORY,
				  NULL,
				  NULL);
    }
    else {
      ERROR("pmi2Initialize: %s", "");
      return pmi2FinishInitialize(PMI2_LOCATION_1 +
				  PMI2_INTERNAL_ALLOCATION_FAILURE,
				  NULL,
				  NULL);
    }
  }

  appData->callbacks = malloc(sizeof(Pmi2CallbacksT));
  appData->callbacks->subscribeRopCallbackSpec = malloc(sizeof(Pmi2SubscribeRopCallbackSpecT));

  if (pmi2Callbacks->subscribeRopCallbackSpec == NULL)
    appData->callbacks->subscribeRopCallbackSpec =
      pmi2Callbacks->subscribeRopCallbackSpec;
  else {
    appData->callbacks->subscribeRopCallbackSpec->subscribeRopCallback =
      pmi2Callbacks->subscribeRopCallbackSpec->subscribeRopCallback;
    appData->callbacks->subscribeRopCallbackSpec->userData =
      pmi2Callbacks->subscribeRopCallbackSpec->userData;
  }

  appData->callbacks->reportRopCallbackSpec = malloc(sizeof(Pmi2ReportRopCallbackSpecT));

  if (pmi2Callbacks->reportRopCallbackSpec == NULL)
  appData->callbacks->reportRopCallbackSpec =
    pmi2Callbacks->reportRopCallbackSpec;
  else {
    appData->callbacks->reportRopCallbackSpec->reportRopCallback =
      pmi2Callbacks->reportRopCallbackSpec->reportRopCallback;
    appData->callbacks->reportRopCallbackSpec->userData =
      pmi2Callbacks->reportRopCallbackSpec->userData;
  }

  appData->callbacks->reportShowCountersCallbackSpec = malloc(sizeof(Pmi2ReportShowCountersCallbackSpecT));

  if (pmi2Callbacks->reportShowCountersCallbackSpec == NULL)
  appData->callbacks->reportShowCountersCallbackSpec = pmi2Callbacks->reportShowCountersCallbackSpec;
  else {
    appData->callbacks->reportShowCountersCallbackSpec->reportShowCountersCallback = pmi2Callbacks->reportShowCountersCallbackSpec->reportShowCountersCallback;
    appData->callbacks->reportShowCountersCallbackSpec->userData = pmi2Callbacks->reportShowCountersCallbackSpec->userData;
  }

  appData->connectionHandle = cec_open(signature, sizeof(signature));
  if (appData->connectionHandle == NULL) {
    ERROR("pmi2Initialize: %s", "PMI2_INTERNAL_CEC_OPEN_FAILURE");
    return pmi2FinishInitialize(PMI2_INTERNAL_CEC_OPEN_FAILURE, NULL, appData);
  }

  int cResult = pmiMap_createKeyAndPut("handlePmi2", appData, pmi2Handle);
  if (cResult != PMS_HASHMAP_OK) {
    if (cResult == PMS_HASHMAP_MAX_KEYS_EXCEEDED) {
      ERROR("pmi2Initialize: %s", "PMI2_INTERNAL_MAX_HANDLES_EXCEEDED");
      return pmi2FinishInitialize(PMI2_INTERNAL_MAX_HANDLES_EXCEEDED, NULL,
				  appData);
    }
    else if (cResult == PMS_HASHMAP_OUT_OF_MEMORY) {
      ERROR("pmi2Initialize: %s", "PMI2_INTERNAL_HASHMAP_OUT_OF_MEMORY");
      return pmi2FinishInitialize(PMI2_INTERNAL_HASHMAP_OUT_OF_MEMORY, NULL,
				  appData);
    }
    else if (cResult == PMS_HASHMAP_OVERWRITE) {
      ERROR("pmi2Initialize: %s", "PMI2_INTERNAL_HASHMAP_OVERWRITE");
      return pmi2FinishInitialize(PMI2_INTERNAL_HASHMAP_OVERWRITE, NULL,
				  appData);
    }
    else if (cResult == PMS_HASHMAP_UNSPECIFIC_FAILURE) {
      ERROR("pmi2Initialize: %s", "PMI2_INTERNAL_HASHMAP_FAILURE");
      return pmi2FinishInitialize(PMI2_INTERNAL_HASHMAP_FAILURE, NULL,
				  appData);
    }
    else {
      // must never happen; would be a coding error
      ERROR("pmi2Initialize: %s", "PMI2_INTERNAL_HASHMAP_INTERFACE_ERROR");
      return pmi2FinishInitialize(PMI2_INTERNAL_HASHMAP_INTERFACE_ERROR, NULL,
				  appData);
    }
  }

  packet.length = sizeof(buffer);
  ptr = packet.data = buffer;

  uint32_t subsc_cb = true;
  if (pmi2Callbacks->subscribeRopCallbackSpec == NULL)
    subsc_cb = false;
  else
    if (pmi2Callbacks->subscribeRopCallbackSpec->subscribeRopCallback == NULL)
      subsc_cb = false;

  uint32_t rop_cb = true;
  if (pmi2Callbacks->reportRopCallbackSpec == NULL)
    rop_cb = false;
  else if (pmi2Callbacks->reportRopCallbackSpec->reportRopCallback == NULL)
    rop_cb = false;

  uint32_t sc_cb = true;
  if (pmi2Callbacks->reportShowCountersCallbackSpec == NULL)
    sc_cb = false;
  else if (pmi2Callbacks->reportShowCountersCallbackSpec->reportShowCountersCallback == NULL)
    sc_cb = false;


  *(uint32_t*)ptr = INITIALIZE;
  ptr+=sizeof(uint32_t);

  // process id
  *(uint32_t*)ptr = getpid();
  ptr+=sizeof(uint32_t);

  // callbacks
  *(uint32_t*)ptr = subsc_cb;
  ptr+=sizeof(uint32_t);
  *(uint32_t*)ptr = rop_cb;
  ptr+=sizeof(uint32_t);
  *(uint32_t*)ptr = sc_cb;
  ptr+=sizeof(uint32_t);

  if (counterMapId == NULL) {
    // false, no counterMapId defined
    *(uint32_t*)ptr = 0;
    ptr+= sizeof(uint32_t);
  }
  else {
    // true, counterMapId defined
    *(uint8_t*)ptr = 1;
    ptr+= sizeof(uint32_t);
    // length of counter map id
    int strlength = strlen(counterMapId);
    *(uint32_t*)ptr = strlength;
    ptr+= sizeof(uint32_t);

    memcpy(ptr, counterMapId, strlength);
    ptr+= strlength;
  }

  // end mark
  *(uint32_t*)ptr = 0;
  ptr+= sizeof(uint32_t);


  if (cec_send(appData->connectionHandle, &packet) < 0) {
    ERROR("pmi2Initialize: %s", "PMI2_INTERNAL_CEC_SEND_FAILURE");
    return pmi2FinishInitialize(PMI2_INTERNAL_CEC_SEND_FAILURE,
				NULL, appData);
  }

  if (cec_receive_w_tmeout(appData->connectionHandle, &packet,
			   INITIALIZE_RESPONSE_TIMEOUT) == -1) {
    ERROR("pmi2Initialize: %s", "PMI2_INTERNAL_CEC_RECEIVE_FAILURE");
    return PMI2_INITIALIZE + PMI2_INTERNAL_CEC_RECEIVE_FAILURE;
  }

  char *recv_ptr;
  recv_ptr = packet.data;
  int functionCode = *(uint32_t *)recv_ptr;
  Pmi2ResultT result = PMI2_OK;

  if ((FunctionCodeT)functionCode == INITIALIZE_RES) {
    recv_ptr += sizeof(uint32_t);
    int erl_result = *(uint32_t *)recv_ptr;
    if (erl_result >= PMI2_OK && erl_result <= PMI2_INTERNAL_ERROR) {
      result = erl_result;
    } else {
      ERROR("pmi2Initialize: %s", "PMI2_INTERNAL_DECODE_FAILURE_1");
      result = PMI2_INTERNAL_DECODE_FAILURE;
    }
  } else {
    ERROR("pmi2Initialize: %s", "PMI2_INTERNAL_DECODE_FAILURE_2");
    result = PMI2_INTERNAL_DECODE_FAILURE;
  }

  if (result == PMI2_OK) {
    INFO("pmi2Initialize: %s", "PMI2_OK");
  }
  else {
/* something went wrong, clean up pmi2Handle */
    if (pmiMap_clear(*pmi2Handle) != PMS_HASHMAP_OK) {
      ERROR("pmi2Initialize: %s", "PMI2_INTERNAL_HASHMAP_FAILURE");
    }
  }

  return pmi2FinishInitialize(result, &packet, appData);
}


Pmi2ResultT pmi2FinishCounterMap(Pmi2ResultT result, char *buffer) {
  if (buffer != NULL) {
    free(buffer);
  }
  
  if (result == PMI2_OK) {
    return result;
  }
  else {
    return PMI2_COUNTER_MAP + result;
  }
}



Pmi2ResultT pmi2CounterMap(Pmi2HandleT pmi2Handle,
			   Pmi2CounterMapT const * const *counterMap) {

  INFO("pmi2CounterMap: %s", "entry");
  if (counterMap == NULL) {
    INFO("pmi2CounterMap: NULL %s", "PMI2_OK");
    return PMI2_OK;
  }

  cec_packet_t packet = {.data=NULL};
  char *ptr;

  ApplicationDataT *appData = pmiMap_get(pmi2Handle);
  if (appData == NULL) {
    ERROR("pmi2CounterMap: %s", "PMI2_UNKNOWN_HANDLE");
    return PMI2_SELECTION_OBJECT_GET + PMI2_UNKNOWN_HANDLE;
  }

  /*
   *===========================================
   * count the total length of the CounterMap
   *===========================================
   */
  int totLen = 0;

  // function code
  totLen = totLen + sizeof(uint32_t);

  int lengthCm;
  for (lengthCm = 0; counterMap[lengthCm]; lengthCm++);

  Pmi2CounterMapT const *cmPtr;

  // number of counter maps
  totLen = totLen + sizeof(uint32_t);


  /**
   * loop PmGroups
   */
  for (int i = 0; i <= lengthCm -1; i++) {
    cmPtr = counterMap[i];

    // length of pmGroupId
    totLen = totLen + sizeof(uint32_t);
    // pmGroupId
    totLen = totLen + strlen(cmPtr->pmGroupId);
    // length of pmGroupIdAlias
    totLen = totLen + sizeof(uint32_t);

    if (cmPtr->measurementTypes != NULL) {
      /**
       * loop Measurement Types belonging to the PM Group
       */
      int lengthMt;

      for (lengthMt = 0; cmPtr->measurementTypes[lengthMt]; lengthMt++);

      Pmi2MeasurementTypeMapT const *mtPtr;

      // number of measurement types
      totLen = totLen + sizeof(uint32_t);

      for (int j = 0; j <= lengthMt -1; j++) {
	mtPtr = cmPtr->measurementTypes[j];
	// length of measurementTypeId
	totLen = totLen + sizeof(uint32_t);
	// measurementTypeId
	totLen = totLen + strlen(mtPtr->measurementTypeId);
	// length of measurementTypeIdAlias
	totLen = totLen + sizeof(uint32_t);
      }
    } else {
      // number of measurement types
      totLen = totLen + sizeof(uint32_t);
    }
  }

  /*
   *===========================================
   * encode the CounterMap message
   *===========================================
   */

  int strlength;

  char *buffer = calloc(totLen, 8);

  packet.length = totLen + sizeof(uint32_t);
  ptr = packet.data = buffer;

  // function code
  *(uint32_t*)ptr = COUNTER_MAP;
  ptr+= sizeof(uint32_t);

  // number of counter maps
  *(uint32_t*)ptr = lengthCm;
  ptr+= sizeof(uint32_t);

  /**
   * loop PmGroups
   */
  for (int i = 0; i <= lengthCm -1; i++) {
    cmPtr = counterMap[i];

    // length of pmGroupId
    strlength = strlen(cmPtr->pmGroupId);
    *(uint32_t*)ptr = strlength;
    ptr+= sizeof(uint32_t);

    // pmGroupId
    memcpy(ptr, cmPtr->pmGroupId, strlength);
    ptr+= strlength;

    // pmGroupIdAlias
    *(uint32_t*)ptr = cmPtr->pmGroupIdAlias;
    ptr+= sizeof(uint32_t);


    if (cmPtr->measurementTypes != NULL) {
      /**
       * loop Measurement Types belonging to the PM Group
       */
      int lengthMt;
      for (lengthMt = 0; cmPtr->measurementTypes[lengthMt]; lengthMt++);

      // number of mMeasurement Types
      *(uint32_t*)ptr = lengthMt;
      ptr+= sizeof(uint32_t);

      Pmi2MeasurementTypeMapT const *mtPtr;

      for (int j = 0; j <= lengthMt -1; j++) {

	mtPtr = cmPtr->measurementTypes[j];

	// length of measurementTypeId
	strlength = strlen(mtPtr->measurementTypeId);
	*(uint32_t*)ptr = strlength;
	ptr+= sizeof(uint32_t);
	// measurementTypeId
	memcpy(ptr, mtPtr->measurementTypeId, strlength);
	ptr+= strlength;
	// measurementTypeIdAlias
	*(uint32_t*)ptr = mtPtr->measurementTypeIdAlias;
	ptr+= sizeof(uint32_t);
      }
    } else {
      // number of mMeasurement Types
      *(uint32_t*)ptr = 0;
      ptr+= sizeof(uint32_t);
    }
  }

  // end mark
  *(uint32_t*)ptr = 0;
  ptr+= sizeof(uint32_t);


  //===========================================
  // Send the CounterMap message
  //===========================================

  Pmi2ResultT result = PMI2_OK;
  if (cec_send(appData->connectionHandle, &packet) < 0) {
    ERROR("pmi2CounterMap: %s", "PMI2_INTERNAL_CEC_SEND_FAILURE");
    result = PMI2_INTERNAL_CEC_SEND_FAILURE;
  } else {
    INFO("pmi2CounterMap: %s", "PMI2_OK");
  }

  return pmi2FinishCounterMap(result, buffer);
}





Pmi2ResultT pmi2SelectionObjectGet(
    Pmi2HandleT pmi2Handle,
    Pmi2SelectionObjectT *selectionObject) {

  ApplicationDataT *appData = pmiMap_get(pmi2Handle);
  if (appData == NULL) {
    ERROR("pmi2SelectionObjectGet: %s", "PMI2_UNKNOWN_HANDLE");
    return PMI2_SELECTION_OBJECT_GET + PMI2_UNKNOWN_HANDLE;
  }

  *selectionObject = appData->connectionHandle->socket;
  INFO("pmi2SelectionObjectGet: %s", "PMI2_OK");
  return PMI2_OK;
}





Pmi2ResultT pmi2FinishDispatch(Pmi2ResultT result, cec_packet_t *packet) {
  if (packet->data != NULL) {
    free(packet->data);
  }

    ERROR("pmi2FinishDispatch: %d", PMI2_DISPATCH + result);
  return PMI2_DISPATCH + result;
}


Pmi2ResultT pmi2FinishSubscribe(Pmi2ResultT result, cec_packet_t *packet) {
  if (packet->data != NULL) {
    free(packet->data);
  }
  if (result == PMI2_OK) {
    DEBUG("pmi2FinishSubscribe: %s", "PMI2_OK");
    return result;
  }
  else {
    ERROR("pmi2FinishSubscribe: %d", PMI2_SUBSCRIBE + result);
    return PMI2_SUBSCRIBE + result;
  }
}


Pmi2ResultT pmi2FinishReportRop(Pmi2ResultT result, cec_packet_t *packet) {
  if (packet->data != NULL) {
    free(packet->data);
  }
  if (result == PMI2_OK) {
    DEBUG("pmi2ReportRop: %s", "PMI2_OK");
    return result;
  }
  else {
    ERROR("pmi2ReportRop: %d", PMI2_REPORT + result);
    return PMI2_REPORT + result;
  }
}


Pmi2ResultT pmi2FinishReportShowCounters(Pmi2ResultT result, cec_packet_t *packet) {
  if (packet->data != NULL) {
    free(packet->data);
  }
  if (result == PMI2_OK) {
    DEBUG("pmi2FinishReportShowCounters: %s", "PMI2_OK");
    return result;
  }
  else {
    ERROR("pmi2FinishReportShowCounters: %d", PMI2_SC_REPORT + result);
    return PMI2_SC_REPORT + result;
  }
}




Pmi2ResultT pmi2Dispatch(Pmi2HandleT pmi2Handle) {


  cec_packet_t packet = {.data=NULL};

  ApplicationDataT *appData = pmiMap_get(pmi2Handle);
  if (appData == NULL) {
    ERROR("pmi2Dispatch: %s", "PMI2_UNKNOWN_HANDLE");
    return pmi2FinishDispatch(PMI2_UNKNOWN_HANDLE, &packet);
  }

  if (cec_receive_w_tmeout(appData->connectionHandle, &packet, CEC_RECEIVE_TIMEOUT) == -1) {
    ERROR("pmi2Dispatch: %s", "PMI2_INTERNAL_CEC_RECEIVE_FAILURE");
    return PMI2_DISPATCH + PMI2_INTERNAL_CEC_RECEIVE_FAILURE;
  }

  char *ptr;
  ptr = packet.data;

  int functionCode = *(uint32_t *)ptr;
  ptr+= sizeof(uint32_t);

  DEBUG("pmi2Dispatch: %d", functionCode);



  /*
   * Message = [FunctionCode | Msg]
   * where FunctionCode = uint8
   */

  if ((FunctionCodeT)functionCode == INITIALIZE_RES) {
    return PMI2_OK;
  }
  else if ((FunctionCodeT)functionCode == SUBSCRIBE_ROP) {

    /*
     * =============
     * SUBSCRIBE_ROP
     * =============
     *
     * Message = [gp, subscrSpecLength, [groupIdAlias, mtLength, [mtIdAlias]]]
     *
     * all parameters are uint32
     */
    DEBUG("pmi2Dispatch: %s", "subscribeRopCallback");
    uint32_t granularityPeriod = *(uint32_t *)ptr;
    ptr+= sizeof(uint32_t);

    uint32_t noofSubscribeSpecs = *(uint32_t *)ptr;
    ptr+= sizeof(uint32_t);

    Pmi2SubscribeRopSpecT **subscribeSpecPointers = calloc(noofSubscribeSpecs + 1, sizeof(Pmi2SubscribeRopSpecT *));
    Pmi2SubscribeRopSpecT *subscribeSpecs = calloc(noofSubscribeSpecs, sizeof(Pmi2SubscribeRopSpecT));

    for (uint32_t i = 0; i < noofSubscribeSpecs; i++) {
      subscribeSpecPointers[i] = &(subscribeSpecs[i]);

      uint32_t groupIdAlias = *(uint32_t *)ptr;
      ptr+= sizeof(uint32_t);
      subscribeSpecs[i].pmGroupIdAlias = groupIdAlias;

      uint32_t mtLength = *(uint32_t *)ptr;
      ptr+= sizeof(uint32_t);
      subscribeSpecs[i].noofMeasurementTypeIdAliases = mtLength;

      uint32_t *mtIdAliases = calloc(mtLength + 1, sizeof(uint32_t));

      subscribeSpecs[i].measurementTypeIdAliases = mtIdAliases;

      for (uint32_t j = 0; j < mtLength; j++) {
	uint32_t mtIdAlias = *(uint32_t *)ptr;
	ptr+= sizeof(uint32_t);

 	*(mtIdAliases + j) = mtIdAlias;
      }
    }


    if (appData->callbacks != NULL) {
      (appData->callbacks->subscribeRopCallbackSpec->subscribeRopCallback)
	(pmi2Handle,
	 (uint32_t)granularityPeriod,
	 (Pmi2SubscribeRopSpecT const * const *)subscribeSpecPointers,
	 appData->callbacks->subscribeRopCallbackSpec->userData);
    }


    for (uint32_t j = 0; j < noofSubscribeSpecs; j++) {
      free((void *)(subscribeSpecPointers[j]->measurementTypeIdAliases));
    }

    free(subscribeSpecs);
    free(subscribeSpecPointers);


    return pmi2FinishSubscribe(PMI2_OK, &packet);
  }
  else if ((FunctionCodeT)functionCode == REPORT_ROP) {
    /*
     * ==========
     * REPORT_ROP
     * ==========
     *
     * Message = [gp, reportId, maxReportingTime]
     *
     * gp and maxReportingTime uint32
     * reportId is uint32
     */


    DEBUG("pmi2Dispatch: %s", "reportRopCallback");
    uint32_t granularityPeriod = *(uint32_t *)ptr;
    ptr+= sizeof(uint32_t);

    uint32_t reportId = *(uint32_t *)ptr;
    ptr+= sizeof(uint32_t);

    uint32_t maxReportingTime = *(uint32_t *)ptr;
    ptr+= sizeof(uint32_t);

    if (appData->callbacks != NULL) {
      (appData->callbacks->reportRopCallbackSpec->reportRopCallback)
	(pmi2Handle,
	 (uint32_t)granularityPeriod,
	 (uint32_t)reportId,
	 (uint32_t)maxReportingTime,
	 appData->callbacks->reportRopCallbackSpec->userData);
    }

    return pmi2FinishReportRop(PMI2_OK, &packet);
  }
  else if ((FunctionCodeT)functionCode == REPORT_SC) {
    /*
     * =========
     * REPORT_SC
     * =========
     *
     * Message = [moInstLdnAlias, reportId, maxReportingTime]
     *
     * gp and maxReportingTime uint32
     * reportId is uint32
     */

    DEBUG("pmi2Dispatch: %s", "reportShowCountersCallback");
    uint32_t reportId = *(uint32_t *)ptr;
    ptr+= sizeof(uint32_t);

    uint32_t moInstLdnAlias = *(uint32_t *)ptr;
    ptr+= sizeof(uint32_t);

    uint32_t noofScSpecs = *(uint32_t *)ptr;
    ptr+= sizeof(uint32_t);

    Pmi2ShowCountersSpecT **scSpecPointers = calloc(noofScSpecs + 1, sizeof(Pmi2SubscribeRopSpecT *));
    Pmi2ShowCountersSpecT *scSpecs = calloc(noofScSpecs, sizeof(Pmi2ShowCountersSpecT));

    for (uint32_t i = 0; i < noofScSpecs; i++) {
      scSpecPointers[i] = &(scSpecs[i]);

      uint32_t groupIdAlias = *(uint32_t *)ptr;
      ptr+= sizeof(uint32_t);
      scSpecs[i].pmGroupIdAlias = groupIdAlias;

      uint32_t mtLength = *(uint32_t *)ptr;
      ptr+= sizeof(uint32_t);
      scSpecs[i].noofMeasurementTypeIdAliases = mtLength;

      uint32_t *mtIdAliases = calloc(mtLength + 1, sizeof(uint32_t));

      scSpecs[i].measurementTypeIdAliases = mtIdAliases;

      for (uint32_t j = 0; j < mtLength; j++) {
	uint32_t mtIdAlias = *(uint32_t *)ptr;
	ptr+= sizeof(uint32_t);

 	*(mtIdAliases + j) = mtIdAlias;
      }
    }

    uint32_t maxReportingTime = *(uint32_t *)ptr;
    ptr+= sizeof(uint32_t);

    if (appData->callbacks != NULL) {
      (appData->callbacks->reportShowCountersCallbackSpec->reportShowCountersCallback)
	(pmi2Handle,
	 (uint32_t)reportId,
	 (uint32_t)moInstLdnAlias,
	 (Pmi2ShowCountersSpecT const * const *)scSpecPointers,
	 (uint32_t)maxReportingTime,
	 appData->callbacks->reportShowCountersCallbackSpec->userData);
    }

    for (uint32_t j = 0; j < noofScSpecs; j++) {
      free((void *)(scSpecPointers[j]->measurementTypeIdAliases));
    }

    free(scSpecs);
    free(scSpecPointers);

    return pmi2FinishReportShowCounters(PMI2_OK, &packet);
  }
  else
    /*
     * ===============
     * UNKNOWN MESSAGE
     * ===============
     *
     * Message = [moInstLdnAlias, reportId, maxReportingTime]
     *
     * gp and maxReportingTime uint32
     * reportId is uint32
     */
    ERROR("pmi2Dispatch: %s", "PMI2_INTERNAL_UNKNOWN_FUNCTION");
    return pmi2FinishDispatch(PMI2_INTERNAL_UNKNOWN_FUNCTION, &packet);
}




Pmi2ResultT pmi2FinishDataRop(Pmi2ResultT result, char *buffer) {
  if (buffer != NULL) {
      free(buffer);
  }
  if (result == PMI2_OK) {
      return result;
  }
  else {
      return PMI2_ROP_DATA + result;
  }
}


Pmi2ResultT pmi2DataRop(
			Pmi2HandleT pmi2Handle,
			uint32_t granularityPeriod,
			uint32_t reportId,
			Pmi2ValueBundleT const * const bundles[],
			const uint32_t finalFragment) {

  DEBUG("pmi2DataRop: %s", "entry");

  switch(granularityPeriod) {
  case TEN_SECONDS :
    break;
  case THIRTY_SECONDS :
    break;
  case ONE_MIN :
    break;
  case FIVE_MIN :
    break;
  case FIFTEEN_MIN :
    break;
  case THIRTY_MIN :
    break;
  case ONE_HOUR :
    break;
  case TWELVE_HOUR :
    break;
  case ONE_DAY :
    break;
  default :
    ERROR("pmi2DataRop: %s", "PMI2_ILLEGAL_GP");
    return pmi2FinishDataRop(PMI2_BAD_PARAMETER, NULL);
  }


  cec_packet_t packet = {.data=NULL};
  char *ptr;


  ApplicationDataT *appData = pmiMap_get(pmi2Handle);
  if (appData == NULL) {
    ERROR("pmi2DataRop: %s", "PMI2_UNKNOWN_HANDLE");
    return pmi2FinishDataRop(PMI2_UNKNOWN_HANDLE, NULL);
  }


  /*
   * [functionCode,
   *  gp,
   *  reportId,
   *  NoOfPmGroups
   *  [pmGroupBundleSize, pmGroupIdAlias,
   *      NoOfinstances,
   *      [InstBundleSize, moInstanceLdnAlias,
   *       measurementLength,
   *       [moInstanceAlias, multiplicity, [values]]]],
   *  boolean]
   *
   * all uint32 expect reportId and values which are uint64 and int64
   * respectively
   */

  /*
   *===========================================
   * count the total length of DataRop
   *===========================================
   */
  int totLen = 0;

  // functionCode
  totLen = totLen + sizeof(uint32_t);

  // gp
  totLen = totLen + sizeof(uint32_t);

  // reportId
  totLen = totLen + sizeof(uint32_t);

  // bundleLength
  int lengthBundles;
  totLen = totLen + sizeof(uint32_t);

  Pmi2ValueBundleT const *bundlePtr;
  for (lengthBundles = 0; bundles[lengthBundles]; lengthBundles++);

  /**
   *-------------------------------------------
   * Pmi2ValueBundleT
   *-------------------------------------------
   */
  for (int v = 0; v <= lengthBundles - 1; v++) {
    bundlePtr = bundles[v];

    // PmGroup bundle lenght
    totLen = totLen + sizeof(uint32_t);

    // pmGroupIdAlias
    totLen = totLen + sizeof(uint32_t);

    /**
     *-------------------------------------------
     * Pmi2MoInstanceBundleT
     *-------------------------------------------
     */

    // number of instance bundles
    totLen = totLen + sizeof(uint32_t);

    int lengthIb;
    Pmi2MoInstanceBundleT const *ibPtr;
    for (lengthIb = 0; bundlePtr->instanceBundles[lengthIb]; lengthIb++);

    for (int i = 0; i <= lengthIb - 1; i++) {

      ibPtr = bundles[v]->instanceBundles[i];

      // Instance bundle lenght
      totLen = totLen + sizeof(uint32_t);

      // moInstanceLdnAlias
      totLen = totLen + sizeof(uint32_t);

      /**
       *-------------------------------------------
       * Pmi2MeasurementValueT
       *-------------------------------------------
       */
      int lengthMv;
      Pmi2MeasurementValueT const *mvPtr;
      for (lengthMv = 0; ibPtr->measurementValues[lengthMv]; lengthMv++);

      // measurement length
      totLen = totLen + sizeof(uint32_t);

      for (int m = 0; m <= lengthMv - 1; m++) {

	mvPtr = ibPtr->measurementValues[m];

	// measurementTypeIdAlias
	totLen = totLen + sizeof(uint32_t);

	/**
	 *-------------------------------------------
	 * elements
	 *-------------------------------------------
	 */

	// number of elements
	totLen = totLen + sizeof(uint32_t);

	// elements
	int lengthElements = mvPtr->multiplicity;
	totLen = totLen + (lengthElements * sizeof(int64_t));

      }
    }
  }

  // finalFragment
  totLen = totLen + sizeof(uint32_t);

  /*
   *===========================================
   * encode the DataRop message
   *===========================================
   */

  char *buffer = calloc(totLen, 8);

  packet.length = totLen + 4;
  ptr = packet.data = buffer;

  // function code
  *(uint32_t*)ptr = DATA_ROP;
  ptr+= sizeof(uint32_t);

  // gp
  *(uint32_t*)ptr = granularityPeriod;
  ptr+= sizeof(uint32_t);

  // reportId
  *(uint32_t*)ptr = reportId;
  ptr+= sizeof(uint32_t);

  // number of bundles
  *(uint32_t*)ptr = lengthBundles;
  ptr+= sizeof(uint32_t);


  /**
   *-------------------------------------------
   * Pmi2ValueBundleT
   *-------------------------------------------
   */

  char *tmpBundlePtr;
  int bundleSize;

  char *tmpInstPtr;
  int instSize;

  for (int v = 0; v <= lengthBundles - 1; v++) {
    bundlePtr = bundles[v];

    tmpBundlePtr = ptr;

    ptr+= sizeof(uint32_t);
    bundleSize = 0;

    // pmGroupIdAlias
    *(uint32_t*)ptr = bundlePtr->pmGroupIdAlias;
    ptr+= sizeof(uint32_t);
    bundleSize+= sizeof(uint32_t);


    /**
     *-------------------------------------------
     * Pmi2MoInstanceBundleT
     *-------------------------------------------
     */


    int lengthIb;
    Pmi2MoInstanceBundleT const *ibPtr;
    for (lengthIb = 0; bundlePtr->instanceBundles[lengthIb]; lengthIb++);

    // number of instance bundles
    *(uint32_t*)ptr = lengthIb;
    ptr+= sizeof(uint32_t);
    bundleSize+= sizeof(uint32_t);

    for (int i = 0; i <= lengthIb - 1; i++) {

      ibPtr = bundles[v]->instanceBundles[i];

      tmpInstPtr = ptr;
      ptr+= sizeof(uint32_t);
      instSize = 0;

      // moInstanceLdnAlias
      *(uint32_t*)ptr = ibPtr->moInstanceLdnAlias;
      ptr+= sizeof(uint32_t);
      instSize+= sizeof(uint32_t);

      /**
       *-------------------------------------------
       * Pmi2MeasurementValueT
       *-------------------------------------------
       */
      int lengthMv;
      Pmi2MeasurementValueT const *mvPtr;
      for (lengthMv = 0; ibPtr->measurementValues[lengthMv]; lengthMv++);

      // number of measurement values
      *(uint32_t*)ptr = lengthMv;
      ptr+= sizeof(uint32_t);
      instSize+= sizeof(uint32_t);

      for (int m = 0; m <= lengthMv - 1; m++) {

	mvPtr = ibPtr->measurementValues[m];

	// measurementTypeIdAlias
	*(uint32_t*)ptr = mvPtr->measurementTypeIdAlias;
	ptr+= sizeof(uint32_t);
	instSize+= sizeof(uint32_t);

	/**
	 *-------------------------------------------
	 * elements
	 *-------------------------------------------
	 */

	// number of elements
	*(uint32_t*)ptr = mvPtr->multiplicity;
	ptr+= sizeof(uint32_t);
	instSize+= sizeof(uint32_t);

	// elements
	memcpy(ptr, mvPtr->elements, (mvPtr->multiplicity)*sizeof(int64_t));
	ptr+= (mvPtr->multiplicity)*sizeof(int64_t);
	instSize+= (mvPtr->multiplicity)*sizeof(int64_t);

      }
      *(uint32_t*)tmpInstPtr = instSize;
      //      *(uint32_t*)tmpInstPtr = ptr - tmpInstPtr - sizeof(uint32_t); 
      bundleSize+= instSize + sizeof(uint32_t);
    }
    *(uint32_t*)tmpBundlePtr = bundleSize;
    //    *(uint32_t*)tmpBundlePtr = ptr - tmpBundlePtr - sizeof(uint32_t);
  }

  // finalFragment
  *(uint32_t*)ptr = (uint32_t) finalFragment;
  ptr+= sizeof(uint32_t);


  //===========================================
  // Send the DataRop message
  //===========================================

  Pmi2ResultT result = PMI2_OK;
  if (cec_send(appData->connectionHandle, &packet) < 0) {
    ERROR("pmi2DataRop: %s", "PMI2_INTERNAL_CEC_SEND_FAILURE");
    result = PMI2_INTERNAL_CEC_SEND_FAILURE;
  }

  return pmi2FinishDataRop(result, buffer);
}





Pmi2ResultT pmi2FinishDataShowCounters(Pmi2ResultT result, char *buffer) {
  if (buffer != NULL) {
      free(buffer);
  }
  if (result == PMI2_OK) {
      return result;
  }
  else {
      return PMI2_SC_DATA + result;
  }
}



Pmi2ResultT pmi2DataShowCounters(
    Pmi2HandleT pmi2Handle,
    uint32_t reportId,
    uint32_t resultCode,
    const char *errorString,
    Pmi2ValueBundleT const * const bundles[]) {

  DEBUG("pmi2DataShowCounters: %s", "entry");

  cec_packet_t packet = {.data=NULL};
  char *ptr;


  ApplicationDataT *appData = pmiMap_get(pmi2Handle);
  if (appData == NULL) {
    ERROR("pmi2DataShowCounters: %s", "PMI2_UNKNOWN_HANDLE");
    return pmi2FinishDataRop(PMI2_UNKNOWN_HANDLE, NULL);
  }


  /*
   * [functionCode,
   *  reportId,
   *  result,
   *  errorStrLen,
   *  errorStr,
   *  measurementLength,
   *  [moInstanceAlias, multiplicity, [values]]
   * ]
   *
   * all uint32 expect reportId and values which are uint64 and int64
   * respectively
   */

  /*
   *===========================================
   * count the total length of DataSc
   *===========================================
   */
  int totLen = 0;

  // functionCode
  totLen = totLen + sizeof(uint32_t);

  // reportId
  totLen = totLen + sizeof(uint32_t);

  // result
  totLen = totLen + sizeof(uint32_t);

  // errorStrLen
  totLen = totLen + sizeof(uint32_t);

  // errorStr
  totLen = totLen + strlen(errorString);

  // bundleLength
  int lengthBundles;
  totLen = totLen + sizeof(uint32_t);

  Pmi2ValueBundleT const *bundlePtr;
  for (lengthBundles = 0; bundles[lengthBundles]; lengthBundles++);

  DEBUG("SHOW COUNTERS: lengthBundles = %d", lengthBundles);
  
  /**
   *-------------------------------------------
   * Pmi2ValueBundleT
   *-------------------------------------------
   */
  for (int v = 0; v <= lengthBundles - 1; v++) {
    bundlePtr = bundles[v];

    // pmGroupIdAlias
    totLen = totLen + sizeof(uint32_t);

    /**
     *-------------------------------------------
     * Pmi2MoInstanceBundleT
     *-------------------------------------------
     */

    // number of instance bundles
    totLen = totLen + sizeof(uint32_t);

    int lengthIb;
    Pmi2MoInstanceBundleT const *ibPtr;
    for (lengthIb = 0; bundlePtr->instanceBundles[lengthIb]; lengthIb++);

    for (int i = 0; i <= lengthIb - 1; i++) {

      ibPtr = bundles[v]->instanceBundles[i];

      // moInstanceLdnAlias
      totLen = totLen + sizeof(uint32_t);

      /**
       *-------------------------------------------
       * Pmi2MeasurementValueT
       *-------------------------------------------
       */
      int lengthMv;
      Pmi2MeasurementValueT const *mvPtr;
      for (lengthMv = 0; ibPtr->measurementValues[lengthMv]; lengthMv++);

      // measurement length
      totLen = totLen + sizeof(uint32_t);

      for (int m = 0; m <= lengthMv - 1; m++) {

	mvPtr = ibPtr->measurementValues[m];

	// measurementTypeIdAlias
	totLen = totLen + sizeof(uint32_t);

	/**
	 *-------------------------------------------
	 * elements
	 *-------------------------------------------
	 */

	// number of elements
	totLen = totLen + sizeof(uint32_t);

	// elements
	int lengthElements = mvPtr->multiplicity;
	totLen = totLen + (lengthElements * sizeof(int64_t));

      }
    }
  }



  /*
   *===========================================
   * encode the DataSc message
   *===========================================
   */

  DEBUG("SC encoded: totLen = %d", totLen);

  char *buffer = calloc(totLen, 8);

  packet.length = totLen + 1;
  ptr = packet.data = buffer;

  // function code
  *(uint32_t*)ptr = DATA_SC;
  ptr+= sizeof(uint32_t);

  DEBUG("SC encoded: FunctionCode = %d", DATA_SC);


  // reportId
  *(uint32_t*)ptr = reportId;
  ptr+= sizeof(uint32_t);

  DEBUG("SC encoded: reportId = %d", reportId);
  // result
  *(uint32_t*)ptr = resultCode;
  ptr+= sizeof(uint32_t);

  DEBUG("SC encoded: resultCode = %d", resultCode);

  // errorStrLen
  int strlength = strlen(errorString);
  *(uint32_t*)ptr = strlength;
  ptr+= sizeof(uint32_t);

  // errorStr
  memcpy(ptr, errorString, strlength);
  ptr+= strlength;


  // number of bundles
  *(uint32_t*)ptr = lengthBundles;
  ptr+= sizeof(uint32_t);

  DEBUG("SC encoded: lengthBundles = %d", lengthBundles);



  /**
   *-------------------------------------------
   * Pmi2ValueBundleT
   *-------------------------------------------
   */
  for (int v = 0; v <= lengthBundles - 1; v++) {
    bundlePtr = bundles[v];

    // pmGroupIdAlias
    *(uint32_t*)ptr = bundlePtr->pmGroupIdAlias;
    ptr+= sizeof(uint32_t);

    DEBUG("SC encoded: bundlePtr->pmGroupIdAlias = %d", bundlePtr->pmGroupIdAlias);

    /**
     *-------------------------------------------
     * Pmi2MoInstanceBundleT
     *-------------------------------------------
     */


    int lengthIb;
    Pmi2MoInstanceBundleT const *ibPtr;
    for (lengthIb = 0; bundlePtr->instanceBundles[lengthIb]; lengthIb++);

    // number of instance bundles
    *(uint32_t*)ptr = lengthIb;
    ptr+= sizeof(uint32_t);

    DEBUG("SC encoded: lengthIb = %d", lengthIb);

    for (int i = 0; i <= lengthIb - 1; i++) {

      ibPtr = bundles[v]->instanceBundles[i];

      // moInstanceLdnAlias
      *(uint32_t*)ptr = ibPtr->moInstanceLdnAlias;
      ptr+= sizeof(uint32_t);
      DEBUG("SC encoded: ibPtr->moInstanceLdnAlias = %d", ibPtr->moInstanceLdnAlias);

      /**
       *-------------------------------------------
       * Pmi2MeasurementValueT
       *-------------------------------------------
       */
      int lengthMv;
      Pmi2MeasurementValueT const *mvPtr;
      for (lengthMv = 0; ibPtr->measurementValues[lengthMv]; lengthMv++);

      // number of measurement values
      *(uint32_t*)ptr = lengthMv;
      ptr+= sizeof(uint32_t);

      DEBUG("SC encoded: lengthMv = %d", lengthMv);
      for (int m = 0; m <= lengthMv - 1; m++) {

	mvPtr = ibPtr->measurementValues[m];

	// measurementTypeIdAlias
	*(uint32_t*)ptr = mvPtr->measurementTypeIdAlias;
	ptr+= sizeof(uint32_t);

	DEBUG("SC encoded: mvPtr->measurementTypeIdAlias = %d", mvPtr->measurementTypeIdAlias);
	/**
	 *-------------------------------------------
	 * elements
	 *-------------------------------------------
	 */

	// number of elements
	*(uint32_t*)ptr = mvPtr->multiplicity;
	ptr+= sizeof(uint32_t);
	
	DEBUG("SC encoded: mvPtr->multiplicity = %d", mvPtr->multiplicity);

	if (mvPtr->multiplicity > 0 && mvPtr->elements == NULL) {
	  return PMI2_BAD_PARAMETER + PMI2_LOCATION_5;
	}

	for (uint32_t i = 0; i < mvPtr->multiplicity; i++) {
	  DEBUG("SC encoded: mvPtr->elements[%d] = %lld", i, (long long) mvPtr->elements[i]);
	}

	// elements
	memcpy(ptr, mvPtr->elements, (mvPtr->multiplicity)*sizeof(int64_t));
	ptr+= (mvPtr->multiplicity)*sizeof(int64_t);
      }
    }
  }

  DEBUG("SC encoded: send the message%s", "!");

  //===========================================
  // Send the CounterMap message
  //===========================================

  Pmi2ResultT result = PMI2_OK;
  if (cec_send(appData->connectionHandle, &packet) < 0) {
    ERROR("pmi2DataShowCounters: %s", "PMI2_UNKNOWN_HANDLE");
    result = PMI2_INTERNAL_CEC_SEND_FAILURE;
  }

  return pmi2FinishDataShowCounters(result, buffer);
}




Pmi2ResultT pmi2FinishFinalize(Pmi2ResultT result, char *buffer) {
  if (buffer != NULL) {
      free(buffer);
  }
  if (result == PMI2_OK) {
      return result;
  }
  else {
      return PMI2_FINALIZE + result;
  }
}


Pmi2ResultT pmi2Finalize(Pmi2HandleT pmi2Handle) {

  DEBUG("pmi2Finalize: %s", "entry");

  ApplicationDataT *appData = pmiMap_get(pmi2Handle);
  if (appData == NULL) {
    ERROR("pmi2Finalize: %s", "PMI2_UNKNOWN_HANDLE");
    return pmi2FinishFinalize(PMI2_UNKNOWN_HANDLE, NULL);
  }

  /*
   *===========================================
   * encode the Finalize message
   *===========================================
   */

  cec_packet_t packet = {.data=NULL};
  char *ptr;

  char *buffer = calloc(2, sizeof(uint32_t));

  packet.length = 2*sizeof(uint32_t);
  ptr = packet.data = buffer;

  // function code
  *(uint32_t*)ptr = FINALIZE;
  ptr+= sizeof(uint32_t);

  if (cec_send(appData->connectionHandle, &packet) == -1) {
    ERROR("pmi2Finalize: %s", "PMI2_INTERNAL_CEC_SEND_FAILURE");
    return pmi2FinishFinalize(PMI2_OK, buffer);
  }

  // Finally tear down connection to the CS
  if (cec_close(appData->connectionHandle) == -1) {
    ERROR("pmi2Finalize: %s", "PMI2_INTERNAL_CEC_CLOSE_FAILURE");
  }

  if (pmiMap_clear(pmi2Handle) == PMS_HASHMAP_OK) {
    INFO("pmi2Finalize: %s", "PMI2_OK");
  }
  else {
    ERROR("pmi2Finalize: %s", "PMI2_INTERNAL_HASHMAP_FAILURE");
  }

  INFO("pmi2Finalize: appdata = %d", appData);
  INFO("pmi2Finalize: appdata->callbacks = %d", appData->callbacks);
  INFO("pmi2Finalize: appdata->callbacks->subscribeRopCallbackSpec = %d", appData->callbacks->subscribeRopCallbackSpec);
  INFO("pmi2Finalize: appdata->callbacks->reportRopCallbackSpec = %d", appData->callbacks->reportRopCallbackSpec);
  INFO("pmi2Finialize: appdata->callbacks->reportShowCountersCallbackSpec = %d", appData->callbacks->reportShowCountersCallbackSpec);
  INFO("pmi2Finialize: appdata->connectionHandle = %d", appData->connectionHandle);

  free(appData->callbacks->subscribeRopCallbackSpec);
  free(appData->callbacks->reportRopCallbackSpec);
  free(appData->callbacks->reportShowCountersCallbackSpec);
  free(appData->callbacks);
  free(appData);

  return PMI2_OK;
}
