/* ----------------------------------------------------------------------
 * %CCaseFile:	pms_pmi.c %
 * %CCaseRev:	/main/R2A/R3A/R6A/R8A/3 %
 * %CCaseDate:	2017-01-13 %
 * %CCaseDocNo: %
 * Author:	erarafo
 * Author: <name>, <e-mail address>
 *
 * Short description:
 * PM C interface.
 * ----------------------------------------------------------------------
 * %CCaseTemplateFile:	template.c %
 * %CCaseTemplateId: CCver: /main/1 %
 *
 * %CCaseCopyrightBegin%
 * Copyright (c) Ericsson AB 2013-2017 All rights reserved.
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
 * R2A/6      2013-02-04 erarafo     Created
 * R2A/16     2013-04-08 erarafo     64-bit signed int measurement values
 * R2A/17     2013-04-10 erarafo     Using 'const' specifiers
 * R2A/18     2013-04-10 erarafo     Removed impractical 'const' specifiers
 * R2A/20     2013-05-03 erarafo     Elaborated failure codes
 * R2A/23     2014-01-20 erarafo     Using 'const char'
 * R2A/32     2014-04-10 uabesvi     Added functions for show counters
 * R2A/33     2014-04-24 eolaand     Fixed a memory leak
 * R2A/34     2014-05-14 uabesvi     Added error string to show counters data
 * R6A/2      2016-07-14 etxpeno     Coverity fixes
 * R8A/1      2017-01-11 erarafo     Unstylish and maybe unsafe code eliminated
 * R8A/2      2017-01-11 erarafo     Unambiguous use of PMI_LOCATION_nn macros
 * R8A/3      2017-01-13 erarafo     Fix compiler warnings
 * ----------------------------------------------------------------------
 */

#define _GNU_SOURCE

#define TRACEPOINT_DEFINE
#include "pms_trace.h"

#define INFO( fmt,...)  TRACE_HELPER(info_trace, fmt, __VA_ARGS__)
#define ERROR( fmt,...) TRACE_HELPER(error_trace, fmt, __VA_ARGS__)
#define DEBUG( fmt,...) TRACE_HELPER(debug_trace, fmt, __VA_ARGS__)

#define TRACE_HELPER(type, fmt,...) do {	\
  char *err_str;\
  asprintf(&err_str,"%s:%d:%s():" fmt, __FILE__, __LINE__, __func__, __VA_ARGS__); \
  tracepoint(com_ericsson_pmi, type , err_str);\
  free(err_str);\
  } while(0)


#define TRACE_TO_FILE 0

#include "pms_pmi.h"
#include "pms_hashmap.h"
#include <errno.h>
#include <stdlib.h>
#include <stdio.h>
#include <sys/socket.h>
#include <string.h>
#include <unistd.h>
#include <netdb.h>
#include "cec.h"
#include "ei.h"

#define SIGNATURE {'P', 'M', 'I'}
// #define CEC_RECEIVE_TIMEOUT 100000
#define CEC_RECEIVE_TIMEOUT 3000


typedef enum {
  SUBSCRIBE = 1,
  REPORT = 2,
  REPORT_SC = 3,
} FunctionCodeT;


typedef struct {
  int socketfd;
  cec_handle_t *connectionHandle;
  PmiCallbacksT *callbacks;
  PmiCallbacksT_2 *callbacks_2;
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


static PmiResultT finishInitialize(PmiResultT result, ei_x_buff *buffer) {
  if (buffer != NULL) {
      ei_x_free(buffer);
  }

  if (result == PMI_OK) {
    INFO("pmiInitialize: %s", "PMI_OK");
    return result;
  }
  else {
    ERROR("pmiInitialize: ERROR %d", PMI_INITIALIZE + result);
    return PMI_INITIALIZE + result;
  }
}


/*
 * Tries to set up another connection to the CS.
 * Stores the connection in static memory and
 * returns a handle.
 *
 * The allocated memory is freed when a subsequent
 * call of pmiFinalize() with a matching handle is
 * made.
 */
PmiResultT pmiInitialize(
    PmiHandleT *pmiHandle,
    const PmiCallbacksT *pmiCallbacks,
    const char **pmGroups) {

  INFO("pmiInitialize %s", "entry");

#if TRACE_TO_FILE
  traceLibInit("lib");
  traceLib("I execute, therefore I am");
#endif


  char signature[] = SIGNATURE;
  *pmiHandle = NULL;

  if (PMI_MAX_SESSIONS != PMS_HASHMAP_SIZE) {
      return PMI_INTERNAL_LIMITS_INCONSISTENCY;
  }

  ApplicationDataT *appData = malloc(sizeof(ApplicationDataT));
  if (appData == NULL) {
      if (errno == ENOMEM) {
          return finishInitialize(
              PMI_LOCATION_1 + PMI_INTERNAL_OUT_OF_MEMORY,
              NULL);
      }
      else {
          return finishInitialize(
              PMI_LOCATION_1 + PMI_INTERNAL_ALLOCATION_FAILURE,
              NULL);
      }
  }

  appData->callbacks_2 = NULL;
  appData->callbacks = malloc(sizeof(PmiCallbacksT));
  if (appData->callbacks == NULL) {
      if (errno == ENOMEM) {
	free(appData);
	return finishInitialize(PMI_LOCATION_1 + PMI_INTERNAL_OUT_OF_MEMORY,
				NULL);
      }
      else {
	free(appData);
	return finishInitialize(PMI_LOCATION_2 + PMI_INTERNAL_ALLOCATION_FAILURE,
				NULL);
      }
  }

  *(appData->callbacks) = *pmiCallbacks;

  appData->connectionHandle = cec_open(signature, sizeof(signature));
  if (appData->connectionHandle == NULL) {
      free(appData);
      return finishInitialize(PMI_INTERNAL_CEC_OPEN_FAILURE, NULL);
  }

  int cResult = pmiMap_createKeyAndPut("handle", appData, pmiHandle);
  if (cResult != PMS_HASHMAP_OK) {
      free(appData->connectionHandle);
      free(appData);
      if (cResult == PMS_HASHMAP_MAX_KEYS_EXCEEDED) {
          return finishInitialize(PMI_INTERNAL_MAX_HANDLES_EXCEEDED, NULL);
      }
      else if (cResult == PMS_HASHMAP_OUT_OF_MEMORY) {
          return finishInitialize(PMI_INTERNAL_HASHMAP_OUT_OF_MEMORY, NULL);
      }
      else if (cResult == PMS_HASHMAP_OVERWRITE) {
          return finishInitialize(PMI_INTERNAL_HASHMAP_OVERWRITE, NULL);
      }
      else if (cResult == PMS_HASHMAP_UNSPECIFIC_FAILURE) {
          return finishInitialize(PMI_INTERNAL_HASHMAP_FAILURE, NULL);
      }
      else {
          // must never happen; would be a coding error
          return finishInitialize(PMI_INTERNAL_HASHMAP_INTERFACE_ERROR, NULL);
      }
  }


  // Send the PmGroups to the PM service. We need to create an
  // ei_x_buff for this.

  ei_x_buff buffer;
  if (ei_x_new_with_version(&buffer) != 0) {
    return finishInitialize(
        PMI_LOCATION_2 + PMI_INTERNAL_ENCODE_FAILURE,
        NULL);
  }

  // Find the number of groups
  int nGroups = 0;
  for (const char **p = pmGroups; *p != NULL; p++) {
      nGroups++;
  }

  // {"initialize", _}
  if (ei_x_encode_tuple_header(&buffer, 2) != 0) {
      return finishInitialize(
          PMI_LOCATION_3 + PMI_INTERNAL_ENCODE_FAILURE,
          &buffer);
  }
  if (ei_x_encode_string(&buffer, "initialize") != 0) {
      return finishInitialize(
          PMI_LOCATION_4 + PMI_INTERNAL_ENCODE_FAILURE,
          &buffer);
  }

  // {"initialize", [_, _, ...]}
  if (ei_x_encode_list_header(&buffer, nGroups) != 0) {
      return finishInitialize(
          PMI_LOCATION_5 + PMI_INTERNAL_ENCODE_FAILURE,
          &buffer);
  }
  for (const char **p = pmGroups; *p != NULL; p++) {
      if (ei_x_encode_string(&buffer, *p) != 0) {
          return finishInitialize(
              PMI_LOCATION_6 + PMI_INTERNAL_ENCODE_FAILURE,
              &buffer);
      }
  }
  if (ei_x_encode_empty_list(&buffer) != 0) {
      return finishInitialize(
          PMI_LOCATION_7 + PMI_INTERNAL_ENCODE_FAILURE,
          &buffer);
  }

  // The buffer is ready, now send it
  // CEC allows the client side to send data immediately
  // cec_packet_t packet = {buffer.buffsz, buffer.buff};
  cec_packet_t packet = {buffer.index, buffer.buff};

  if (cec_send(appData->connectionHandle, &packet) < 0) {
      return finishInitialize(
          PMI_INTERNAL_CEC_SEND_FAILURE,
          &buffer);
  }

  return finishInitialize(PMI_OK, &buffer);
}

/*
 * Tries to set up another connection to the CS.
 * Stores the connection in static memory and
 * returns a handle.
 *
 * The allocated memory is freed when a subsequent
 * call of pmiFinalize() with a matching handle is
 * made.
 */
PmiResultT pmiInitialize_2(
    PmiHandleT *pmiHandle,
    const PmiCallbacksT_2 *pmiCallbacks,
    const char **pmGroups,
    const char *topMoLdn) {

  INFO("pmiInitialize_2 %s", "entry");
#if TRACE_TO_FILE
  traceLibInit("lib");
  traceLib("I execute, therefore I am");
#endif

  char signature[] = SIGNATURE;
  *pmiHandle = NULL;

  if (PMI_MAX_SESSIONS != PMS_HASHMAP_SIZE) {
      return PMI_INTERNAL_LIMITS_INCONSISTENCY;
  }

  ApplicationDataT *appData = malloc(sizeof(ApplicationDataT));
  if (appData == NULL) {
      if (errno == ENOMEM) {
          return finishInitialize(
              PMI_LOCATION_8 + PMI_INTERNAL_OUT_OF_MEMORY,
              NULL);
      }
      else {
          return finishInitialize(
              PMI_LOCATION_9 + PMI_INTERNAL_ALLOCATION_FAILURE,
              NULL);
      }
  }

  appData->callbacks = NULL;
  appData->callbacks_2 = malloc(sizeof(PmiCallbacksT_2));
  if (appData->callbacks_2 == NULL) {
      if (errno == ENOMEM) {
	free(appData);
	return finishInitialize(PMI_LOCATION_10 + PMI_INTERNAL_OUT_OF_MEMORY,
				NULL);
      }
      else {
	free(appData);
	return finishInitialize(PMI_LOCATION_11 + PMI_INTERNAL_ALLOCATION_FAILURE,
				NULL);
      }
  }

  *(appData->callbacks_2) = *pmiCallbacks;

  appData->connectionHandle = cec_open(signature, sizeof(signature));
  if (appData->connectionHandle == NULL) {
      free(appData);
      return finishInitialize(PMI_INTERNAL_CEC_OPEN_FAILURE, NULL);
  }

  int cResult = pmiMap_createKeyAndPut("handle", appData, pmiHandle);
  if (cResult != PMS_HASHMAP_OK) {
      free(appData->connectionHandle);
      free(appData);
      if (cResult == PMS_HASHMAP_MAX_KEYS_EXCEEDED) {
          return finishInitialize(PMI_INTERNAL_MAX_HANDLES_EXCEEDED, NULL);
      }
      else if (cResult == PMS_HASHMAP_OUT_OF_MEMORY) {
          return finishInitialize(PMI_INTERNAL_HASHMAP_OUT_OF_MEMORY, NULL);
      }
      else if (cResult == PMS_HASHMAP_OVERWRITE) {
          return finishInitialize(PMI_INTERNAL_HASHMAP_OVERWRITE, NULL);
      }
      else if (cResult == PMS_HASHMAP_UNSPECIFIC_FAILURE) {
          return finishInitialize(PMI_INTERNAL_HASHMAP_FAILURE, NULL);
      }
      else {
          // must never happen; would be a coding error
          return finishInitialize(PMI_INTERNAL_HASHMAP_INTERFACE_ERROR, NULL);
      }
  }


  // Send the PmGroups to the PM service. We need to create an
  // ei_x_buff for this.

  ei_x_buff buffer;
  if (ei_x_new_with_version(&buffer) != 0) {
    return finishInitialize(
        PMI_LOCATION_12 + PMI_INTERNAL_ENCODE_FAILURE,
        NULL);
  }

  // Find the number of groups
  int nGroups = 0;
  for (const char **p = pmGroups; *p != NULL; p++) {
      nGroups++;
  }

  // {"initialize", _}
  if (ei_x_encode_tuple_header(&buffer, 2) != 0) {
      return finishInitialize(
          PMI_LOCATION_13 + PMI_INTERNAL_ENCODE_FAILURE,
          &buffer);
  }
  if (ei_x_encode_string(&buffer, "initialize") != 0) {
      return finishInitialize(
          PMI_LOCATION_14 + PMI_INTERNAL_ENCODE_FAILURE,
          &buffer);
  }


  // {"initialize", [_, _, ...]}
  if (ei_x_encode_list_header(&buffer, 1) != 0) {
      return finishInitialize(
          PMI_LOCATION_15 + PMI_INTERNAL_ENCODE_FAILURE,
          &buffer);
  }

  // {"initialize", [_, _, ...], _}
  if (ei_x_encode_tuple_header(&buffer, 2) != 0) {
      return finishInitialize(
          PMI_LOCATION_16 + PMI_INTERNAL_ENCODE_FAILURE,
          &buffer);
  }
  if (ei_x_encode_string(&buffer, topMoLdn) != 0) {
      return finishInitialize(
          PMI_LOCATION_17 + PMI_INTERNAL_ENCODE_FAILURE,
          &buffer);
  }


  // {"initialize", [_, _, ...]}
  if (ei_x_encode_list_header(&buffer, nGroups) != 0) {
      return finishInitialize(
          PMI_LOCATION_18 + PMI_INTERNAL_ENCODE_FAILURE,
          &buffer);
  }
  for (const char **p = pmGroups; *p != NULL; p++) {
      if (ei_x_encode_string(&buffer, *p) != 0) {
          return finishInitialize(
              PMI_LOCATION_19 + PMI_INTERNAL_ENCODE_FAILURE,
              &buffer);
      }
  }


  if (ei_x_encode_empty_list(&buffer) != 0) {
      return finishInitialize(
          PMI_LOCATION_20 + PMI_INTERNAL_ENCODE_FAILURE,
          &buffer);
  }

  if (ei_x_encode_empty_list(&buffer) != 0) {
      return finishInitialize(
          PMI_LOCATION_21 + PMI_INTERNAL_ENCODE_FAILURE,
          &buffer);
  }

  // The buffer is ready, now send it
  // CEC allows the client side to send data immediately
  // cec_packet_t packet = {buffer.buffsz, buffer.buff};
  cec_packet_t packet = {buffer.index, buffer.buff};

  if (cec_send(appData->connectionHandle, &packet) < 0) {
      return finishInitialize(
          PMI_INTERNAL_CEC_SEND_FAILURE,
          &buffer);
  }

  return finishInitialize(PMI_OK, &buffer);
}



PmiResultT pmiSelectionObjectGet(
    PmiHandleT pmiHandle,
    PmiSelectionObjectT *selectionObject) {

  ApplicationDataT *appData = pmiMap_get(pmiHandle);
  if (appData == NULL) {
    ERROR("pmiSelectionObjectGet: %s", "PMI_UNKNOWN_HANDLE");
    return PMI_SELECTION_OBJECT_GET + PMI_UNKNOWN_HANDLE;
  }

  *selectionObject = appData->connectionHandle->socket;
  INFO("pmiSelectionObjectGet: %s", "PMI_OK");
  return PMI_OK;
}


static bool verifyTupleHeader(
    const char *buf,
    int* index,
    int expectedArity) {

  int actualArity;
  if (ei_decode_tuple_header(buf, index, &actualArity) == -1) {
      return false;
  }
  else if (actualArity != expectedArity) {
      return false;
  }
  else {
      return true;
  }
}


static int getListLength(const char *buf, int* index) {
  int length;
  if (ei_decode_list_header(buf, index, &length) == -1) {
      return -1;
  }
  else {
      return length;
  }
}


static int getTupleArity(const char *buf, int* index) {
  int arity;
  if (ei_decode_tuple_header(buf, index, &arity) == -1) {
      return -1;
  }
  else {
      return arity;
  }
}


/**
 * Returns string length, or -1 in case of error. The value
 * pointed to by the indexP pointer is not affected.
 */
static int stringLength(const char *buf, int *indexP) {
  int type;
  int result;
  if (ei_get_type(buf, indexP, &type, &result) == -1) {
      return -1;
  }
  else if (type != ERL_STRING_EXT) {
      return -1;
  }
  else {
      return result;
  }
}


static PmiResultT finishDispatch(PmiResultT result, cec_packet_t *packet) {
  if (packet->data != NULL) {
      free(packet->data);
  }

  return PMI_DISPATCH + result;
}


static PmiResultT finishSubscribe(PmiResultT result, cec_packet_t *packet) {
  if (packet->data != NULL) {
      free(packet->data);
  }
  if (result == PMI_OK) {
    DEBUG("pmiSubscribe: %s", "PMI_OK");
    return result;
  }
  else {
    ERROR("pmiSubscribe: %d", PMI_SUBSCRIBE + result);
    return PMI_SUBSCRIBE + result;
  }
}


static PmiResultT finishReport(PmiResultT result, cec_packet_t *packet) {
  if (packet->data != NULL) {
      free(packet->data);
  }
  if (result == PMI_OK) {
    DEBUG("pmiReport: %s", "PMI_OK");
    return result;
  }
  else {
    ERROR("pmiSReport: %d", PMI_REPORT + result);
    return PMI_REPORT + result;
  }
}


static PmiResultT finishReportShowCounters(PmiResultT result, cec_packet_t *packet) {
  if (packet->data != NULL) {
    free(packet->data);
  }
  if (result == PMI_OK) {
    DEBUG("pmiReportShowCounters: %s", "PMI_OK");
    return result;
  }
  else {
    ERROR("pmiReportShowCounters: %d", PMI_SC_REPORT + result);
    return PMI_SC_REPORT + result;
  }
}


PmiResultT pmiDispatch(PmiHandleT pmiHandle) {

  DEBUG("pmiDispatch: %s", "entry");

  cec_packet_t packet = {.data=NULL};

  ApplicationDataT *appData = pmiMap_get(pmiHandle);
  if (appData == NULL) {
    ERROR("pmiDispatch: %s", "PMI_UNKNOWN_HANDLE");
    return finishDispatch(PMI_UNKNOWN_HANDLE, &packet);
  }

  if (cec_receive_w_tmeout(appData->connectionHandle, &packet, CEC_RECEIVE_TIMEOUT) == -1) {
    ERROR("pmiDispatch: %s", "PMI_INTERNAL_CEC_RECEIVE_FAILURE");
    return finishDispatch(PMI_INTERNAL_CEC_RECEIVE_FAILURE, &packet);
  }

  int index = 0;
  int version;
  if (ei_decode_version((char *)(packet.data), &index, &version) == -1) {
    ERROR("pmiDispatch: %s", "PMI_INTERNAL_VERSION_MISMATCH");
    return finishDispatch(PMI_INTERNAL_VERSION_MISMATCH, &packet);
  }

  int topArity = getTupleArity((char *)packet.data, &index);
  if (topArity < 0) {
    ERROR("pmiDispatch: %s", "PMI_INTERNAL_DECODE_FAILURE_1");
    return finishDispatch(PMI_LOCATION_22 + PMI_INTERNAL_DECODE_FAILURE,
			  &packet);
  }

  long functionCode;
  if (ei_decode_long((char *)packet.data, &index, &functionCode) == -1) {
    ERROR("pmiDispatch: %s", "PMI_INTERNAL_DECODE_FAILURE_2");
    return finishDispatch(PMI_LOCATION_23 + PMI_INTERNAL_DECODE_FAILURE,
			  &packet);
  }

  if ((FunctionCodeT)functionCode == SUBSCRIBE) {

    DEBUG("pmiDispatch: %s", "subscribeCallback");
    if (topArity != 3) {
      return finishSubscribe(PMI_INTERNAL_BAD_ARITY, &packet);
    }

    long granularityPeriod;
    if (ei_decode_long((char *)packet.data, &index, &granularityPeriod) == -1) {
      return finishSubscribe(
			     PMI_LOCATION_24 + PMI_INTERNAL_DECODE_FAILURE,
			     &packet);
    }

    int nGroups = getListLength((void *)packet.data, &index);
    if (nGroups < 0) {
      return finishSubscribe(
			     PMI_LOCATION_25 + PMI_INTERNAL_DECODE_FAILURE,
			     &packet);
    }

    CounterSpecT **counterSpecPointers = calloc(nGroups + 1, sizeof(CounterSpecT *));
    CounterSpecT *counterSpecs = calloc(nGroups, sizeof(CounterSpecT));

    for (int j = 0; j < nGroups; j++) {
      counterSpecPointers[j] = &(counterSpecs[j]);

      if (! verifyTupleHeader((char *)packet.data, &index, 2)) {
	free(counterSpecPointers);
	return finishSubscribe(PMI_INTERNAL_BAD_ARITY, &packet);
      }

      int groupNameLength = stringLength((char *)packet.data, &index);
      if (groupNameLength == -1) {
	free(counterSpecPointers);
	return finishSubscribe(
			       PMI_LOCATION_26 + PMI_INTERNAL_DECODE_FAILURE,
			       &packet);
      }

      char *groupName = malloc(groupNameLength + 1);

      if (ei_decode_string((char *)packet.data, &index, groupName) != 0) {
	free(counterSpecPointers);
	return finishSubscribe(
			       PMI_LOCATION_27 + PMI_INTERNAL_DECODE_FAILURE,
			       &packet);
      }

      counterSpecs[j].pmGroup = groupName;

      int nTypes = getListLength((char *)packet.data, &index);
      if (nTypes == -1) {
	free(counterSpecPointers);
	return finishSubscribe(
			       PMI_LOCATION_28 + PMI_INTERNAL_DECODE_FAILURE,
			       &packet);
      }

      const char **mTypes = calloc(nTypes + 1, sizeof(const char *));

      counterSpecs[j].measurementTypes = mTypes;

      for (int k = 0; k < nTypes; k++) {
	int typeLength = stringLength((char *)packet.data, &index);
	if (typeLength == -1) {
	  free(counterSpecPointers);
	  return finishSubscribe(
				 PMI_LOCATION_29 + PMI_INTERNAL_DECODE_FAILURE,
				 &packet);
	}

	char *mType = malloc(typeLength + 1);

	if (ei_decode_string((char *)packet.data, &index, mType) != 0) {
	  free(counterSpecPointers);
	  return finishSubscribe(
				 PMI_LOCATION_30 + PMI_INTERNAL_DECODE_FAILURE,
				 &packet);
	}

	counterSpecs[j].measurementTypes[k] = mType;
      }
      if (nTypes > 0) {
	if (ei_skip_term((char *)packet.data, &index) < 0) {
	  free(counterSpecPointers);
	  return finishSubscribe(
				 PMI_LOCATION_31 + PMI_INTERNAL_DECODE_FAILURE,
				 &packet);
	}
      }
    }

    if (nGroups > 0) {
      if (ei_skip_term((char *)packet.data, &index) < 0) {
	free(counterSpecPointers);
	return finishSubscribe(
			       PMI_LOCATION_32 + PMI_INTERNAL_DECODE_FAILURE,
			       &packet);
      }
    }

    if (appData->callbacks != NULL) {
      (appData->callbacks->pmiSubscribeCallback)(pmiHandle,
						 (unsigned int)granularityPeriod,
						 counterSpecPointers);
    }
    else {
      (appData->callbacks_2->pmiSubscribeCallback)(pmiHandle,
						   (unsigned int)granularityPeriod,
						   counterSpecPointers);
    }

    for (int j = 0; j < nGroups; j++) {
      free((void *)(counterSpecPointers[j]->pmGroup));
      for (const char **p = counterSpecPointers[j]->measurementTypes;
	   *p != NULL;
	   p++) {
	free((void *)*p);
      }
      free(counterSpecPointers[j]->measurementTypes);
    }

    free(counterSpecs);
    free(counterSpecPointers);

    return finishSubscribe(PMI_OK, &packet);
  }
  else if ((FunctionCodeT)functionCode == REPORT) {

    DEBUG("pmiDispatch: %s", "reportCallback");
    if (topArity != 4) {
      return finishReport(PMI_INTERNAL_BAD_ARITY, &packet);
    }

    long granularityPeriod;
    if (ei_decode_long((char *)packet.data, &index, &granularityPeriod) == -1) {
      return finishReport(
			  PMI_LOCATION_33 + PMI_INTERNAL_DECODE_FAILURE,
			  &packet);
    }

    long timeSpec;
    if (ei_decode_long((char *)packet.data, &index, &timeSpec) == -1) {
      return finishReport(
			  PMI_LOCATION_34 + PMI_INTERNAL_DECODE_FAILURE,
			  &packet);
    }

    long deadline;
    if (ei_decode_long((char *)packet.data, &index, &deadline) == -1) {
      return finishReport(
			  PMI_LOCATION_35 + PMI_INTERNAL_DECODE_FAILURE,
			  &packet);
    }

    if (appData->callbacks != NULL) {
      (appData->callbacks->pmiReportCallback)(pmiHandle,
					      (int)granularityPeriod,
					      (int)timeSpec,
					      (int)deadline);
    }
    else {
      (appData->callbacks_2->pmiReportCallback)(pmiHandle,
						(int)granularityPeriod,
						(int)timeSpec,
						(int)deadline);
    }

    return finishReport(PMI_OK, &packet);
  }

  else if ((FunctionCodeT)functionCode == REPORT_SC) {

    DEBUG("pmiDispatch: %s", "reportScCallback");
    if (topArity != 4) {
      return finishReportShowCounters(PMI_INTERNAL_BAD_ARITY, &packet);
    }

    long requestId;
    if (ei_decode_long((char *)packet.data, &index, &requestId) == -1) {
      return finishReportShowCounters(
				      PMI_LOCATION_36 + PMI_INTERNAL_DECODE_FAILURE,
				      &packet);
    }

    int type, size;
    ei_get_type((char *)packet.data, &index, &type, &size);
    char instanceLdn[size+1];

    if (ei_decode_string((char *)packet.data, &index, instanceLdn) == -1) {
      return finishReportShowCounters(
				      PMI_LOCATION_37 + PMI_INTERNAL_DECODE_FAILURE,
				      &packet);
    }

    long maxResponseTime;
    if (ei_decode_long((char *)packet.data, &index, &maxResponseTime) == -1) {
      return finishReportShowCounters(
				      PMI_LOCATION_38 + PMI_INTERNAL_DECODE_FAILURE,
				      &packet);
    }


    (appData->callbacks_2->pmiReportShowCountersCallback)(pmiHandle,
							  (int)requestId,
							  instanceLdn,
							  (int)maxResponseTime);

    return finishReportShowCounters(PMI_OK, &packet);
  }

  else {
    return finishDispatch(PMI_INTERNAL_UNKNOWN_FUNCTION, &packet);
  }
}


static PmiResultT finishData(PmiResultT result, ei_x_buff *buffer) {
  if (buffer != NULL) {
      ei_x_free(buffer);
  }
  if (result == PMI_OK) {
    DEBUG("finishData: %s", "PMI_OK");
    return result;
  }
  else {
    ERROR("finishData: %d", PMI_DATA + result);
    return PMI_DATA + result;
  }
}


PmiResultT pmiData(
    PmiHandleT pmiHandle,
    unsigned int granularityPeriod,
    time_t timeSpec,
    const char *measObjLdn,
    ValueBundleT *bundles[]) {

  DEBUG("pmiData: %s", "entry");
  ApplicationDataT *appData = pmiMap_get(pmiHandle);
  if (appData == NULL) {
      return finishData(PMI_UNKNOWN_HANDLE, NULL);
  }

  // Send data to the PM service. We need to create a
  // binary for this. Format is
  // {"data", GP, timeSpec, LDN, [{A, [{M, 3, {455, 457, 433}}, ...]}, ...]}.

  ei_x_buff buffer;
  if (ei_x_new_with_version(&buffer) != 0) {
    return finishData(
        PMI_LOCATION_39 + PMI_INTERNAL_ENCODE_FAILURE,
        NULL);
  }

  if (ei_x_encode_tuple_header(&buffer, 5) != 0) {
      return finishData(
          PMI_LOCATION_40 + PMI_INTERNAL_ENCODE_FAILURE,
          &buffer);
  }

  if (ei_x_encode_string(&buffer, "data") != 0) {
      return finishData(
          PMI_LOCATION_41 + PMI_INTERNAL_ENCODE_FAILURE,
          &buffer);
  }

  if (ei_x_encode_ulong(&buffer, (unsigned long)granularityPeriod) != 0) {
      return finishData(
          PMI_LOCATION_42 + PMI_INTERNAL_ENCODE_FAILURE,
          &buffer);
  }

  if (ei_x_encode_long(&buffer, timeSpec) != 0) {
      return finishData(
          PMI_LOCATION_43 + PMI_INTERNAL_ENCODE_FAILURE,
          &buffer);
  }

  if (ei_x_encode_string(&buffer, measObjLdn) != 0) {
      return finishData(
          PMI_LOCATION_44 + PMI_INTERNAL_ENCODE_FAILURE,
          &buffer);
  }

  // Count number of groups
  int nGroups = 0;
  for (ValueBundleT **p = bundles; *p != NULL; p++) {
      nGroups++;
  }

  if (ei_x_encode_list_header(&buffer, nGroups) != 0) {
      return finishData(
          PMI_LOCATION_45 + PMI_INTERNAL_ENCODE_FAILURE,
          &buffer);
  }

  for (ValueBundleT **p = bundles; *p != NULL; p++) {

      // {groupName, [...]}
      if (ei_x_encode_tuple_header(&buffer, 2) != 0) {
          return finishData(
              PMI_LOCATION_46 + PMI_INTERNAL_ENCODE_FAILURE,
              &buffer);
      }
      if (ei_x_encode_string(&buffer, (*p)->pmGroup) != 0) {
          return finishData(
              PMI_LOCATION_47 + PMI_INTERNAL_ENCODE_FAILURE,
              &buffer);
      }

      // Count number of measurements in this group

      int nMeas = 0;
      for (MeasurementValueT **q = (*p)->measurementValues;
          *q != NULL;
          q++) {
          nMeas++;
      }

      if (ei_x_encode_list_header(&buffer, nMeas) != 0) {
          return finishData(
              PMI_LOCATION_48 + PMI_INTERNAL_ENCODE_FAILURE,
              &buffer);
      }

      for (MeasurementValueT **q = (*p)->measurementValues;
          *q != NULL;
          q++) {
          // {measType, multiplicity, {...}}
          if (ei_x_encode_tuple_header(&buffer, 3) != 0) {
              return finishData(
                  PMI_LOCATION_49 + PMI_INTERNAL_ENCODE_FAILURE,
                  &buffer);
          }

          if (ei_x_encode_string(
              &buffer,
              (*q)->measurementType) != 0) {
              return finishData(
                  PMI_LOCATION_50 + PMI_INTERNAL_ENCODE_FAILURE,
                  &buffer);
          }

          if (ei_x_encode_ulong(
              &buffer,
              (unsigned long)(*q)->multiplicity) != 0) {
              return finishData(
                  PMI_LOCATION_51 + PMI_INTERNAL_ENCODE_FAILURE,
                  &buffer);
          }

          if (ei_x_encode_tuple_header(
              &buffer,
              (long)(*q)->multiplicity) != 0) {
              return finishData(
                  PMI_LOCATION_52 + PMI_INTERNAL_ENCODE_FAILURE,
                  &buffer);
          }

          const long long *e = (*q)->elements;
          for (unsigned int k = 0; k < (*q)->multiplicity; k++, e++) {
              if (ei_x_encode_longlong(&buffer, *e) != 0) {
                  return finishData(
                      PMI_LOCATION_53 + PMI_INTERNAL_ENCODE_FAILURE,
                      &buffer);
              }
          }
      }
      // end of list of measurements for a group
      if (ei_x_encode_empty_list(&buffer) != 0) {
          return finishData(
              PMI_LOCATION_54 + PMI_INTERNAL_ENCODE_FAILURE,
              &buffer);
      }
  }
  // end of top-level list
  if (ei_x_encode_empty_list(&buffer) != 0) {
      return finishData(
          PMI_LOCATION_55 + PMI_INTERNAL_ENCODE_FAILURE,
          &buffer);
  }

  // The buffer is ready, now send it
  // cec_packet_t packet = {buffer.buffsz, buffer.buff};
  cec_packet_t packet = {buffer.index, buffer.buff};

  if (cec_send(appData->connectionHandle, &packet) == -1) {
      return finishData(PMI_INTERNAL_CEC_SEND_FAILURE, &buffer);
  }
  else {
      return finishData(PMI_OK, &buffer);
  }
}





static PmiResultT finishDataShowCounters(PmiResultT result, ei_x_buff *buffer) {
  if (buffer != NULL) {
      ei_x_free(buffer);
  }
  if (result == PMI_OK) {
    DEBUG("pmiDataShowCounters: %s", "PMI_OK");
    return result;
  }
  else {
    ERROR("pmiDataShowCounters: %d", PMI_DATA + result);
    return PMI_DATA + result;
  }
}



PmiResultT pmiDataShowCounters(
    PmiHandleT pmiHandle,
    unsigned int requestId,
    unsigned int result,
    char *errorString,
    MeasurementValueT *bundles[]) {

  DEBUG("pmiDataShowCounters: %s", "entry");

  ApplicationDataT *appData = pmiMap_get(pmiHandle);
  if (appData == NULL) {
      return finishDataShowCounters(PMI_UNKNOWN_HANDLE, NULL);
  }

  // Send data to the PM service. We need to create a
  // binary for this. Format is
  // {"data_show_counters", RequestId, Result, [{M, 3, {455, 457, 433}}, ...]}.

  ei_x_buff buffer;
  if (ei_x_new_with_version(&buffer) != 0) {
    return finishDataShowCounters(
        PMI_LOCATION_56 + PMI_INTERNAL_ENCODE_FAILURE,
        NULL);
  }

  if (ei_x_encode_tuple_header(&buffer, 5) != 0) {
      return finishDataShowCounters(
          PMI_LOCATION_57 + PMI_INTERNAL_ENCODE_FAILURE,
          &buffer);
  }

  if (ei_x_encode_string(&buffer, "data_show_counters") != 0) {
      return finishDataShowCounters(
          PMI_LOCATION_58 + PMI_INTERNAL_ENCODE_FAILURE,
          &buffer);
  }

  if (ei_x_encode_long(&buffer, (long)requestId) != 0) {
      return finishDataShowCounters(
          PMI_LOCATION_59 + PMI_INTERNAL_ENCODE_FAILURE,
          &buffer);
  }

  if (ei_x_encode_long(&buffer, (long)result) != 0) {
      return finishDataShowCounters(
          PMI_LOCATION_60 + PMI_INTERNAL_ENCODE_FAILURE,
          &buffer);
  }


  if (errorString == NULL) {
    errorString = "";
  }
  if (ei_x_encode_string(&buffer, errorString) != 0) {
      return finishDataShowCounters(
          PMI_LOCATION_61 + PMI_INTERNAL_ENCODE_FAILURE,
          &buffer);
  }

  // Count number of measurements in this group

  int nMeas = 0;
  for (MeasurementValueT **q = bundles;
       *q != NULL;
       q++) {
    nMeas++;
  }

  if (ei_x_encode_list_header(&buffer, nMeas) != 0) {
    return finishDataShowCounters(
		      PMI_LOCATION_62 + PMI_INTERNAL_ENCODE_FAILURE,
		      &buffer);
  }


  for (MeasurementValueT **q = bundles;
       *q != NULL;
       q++) {

    // {measType, multiplicity, {...}}
    if (ei_x_encode_tuple_header(&buffer, 3) != 0) {
      return finishDataShowCounters(
			PMI_LOCATION_63 + PMI_INTERNAL_ENCODE_FAILURE,
			&buffer);
    }

    if (ei_x_encode_string(
			   &buffer,
			   (*q)->measurementType) != 0) {
      return finishDataShowCounters(
			PMI_LOCATION_64 + PMI_INTERNAL_ENCODE_FAILURE,
			&buffer);
    }

    if (ei_x_encode_ulong(
			  &buffer,
			  (unsigned long)(*q)->multiplicity) != 0) {
      return finishDataShowCounters(
			PMI_LOCATION_65 + PMI_INTERNAL_ENCODE_FAILURE,
			&buffer);
    }

    if (ei_x_encode_tuple_header(
				 &buffer,
				 (long)(*q)->multiplicity) != 0) {
      return finishDataShowCounters(
			PMI_LOCATION_66 + PMI_INTERNAL_ENCODE_FAILURE,
			&buffer);
    }

    const long long *e = (*q)->elements;
    for (unsigned int k = 0; k < (*q)->multiplicity; k++, e++) {
      if (ei_x_encode_longlong(&buffer, *e) != 0) {
	return finishDataShowCounters(
			  PMI_LOCATION_67 + PMI_INTERNAL_ENCODE_FAILURE,
			  &buffer);
      }
    }

  }


  // end of list of measurements for a group
  if (ei_x_encode_empty_list(&buffer) != 0) {
    return finishDataShowCounters(
		      PMI_LOCATION_68 + PMI_INTERNAL_ENCODE_FAILURE,
		      &buffer);
  }

  // end of top-level list
  if (ei_x_encode_empty_list(&buffer) != 0) {
      return finishDataShowCounters(
          PMI_LOCATION_69 + PMI_INTERNAL_ENCODE_FAILURE,
          &buffer);
  }

  // The buffer is ready, now send it
  // cec_packet_t packet = {buffer.buffsz, buffer.buff};
  cec_packet_t packet = {buffer.index, buffer.buff};

  if (cec_send(appData->connectionHandle, &packet) == -1) {
      return finishDataShowCounters(PMI_INTERNAL_CEC_SEND_FAILURE, &buffer);
  }
  else {

      return finishDataShowCounters(PMI_OK, &buffer);
  }
}



static PmiResultT finishFinalize(PmiResultT result, ei_x_buff *buffer) {
  if (buffer != NULL) {
      ei_x_free(buffer);
  }
  if (result == PMI_OK) {
    return result;
  }
  else {
    return PMI_FINALIZE + result;
  }
}


PmiResultT pmiFinalize(PmiHandleT pmiHandle) {

  INFO("pmiFinalize: %s", "entry");

  ApplicationDataT *appData = pmiMap_get(pmiHandle);
  if (appData == NULL) {
    ERROR("pmiFinalize: %s", "PMI_UNKNOWN_HANDLE");
    return finishFinalize(PMI_UNKNOWN_HANDLE, NULL);
  }

  // Send a final message: {"finalize"}
  ei_x_buff buffer;
  if (ei_x_new_with_version(&buffer) != 0) {
    ERROR("pmiFinalize: %s", "PMI_LOCATION_70 + PMI_INTERNAL_ENCODE_FAILURE");
    return finishFinalize(PMI_LOCATION_70 + PMI_INTERNAL_ENCODE_FAILURE, NULL);
  }

  if (ei_x_encode_tuple_header(&buffer, 1) != 0) {
    ERROR("pmiFinalize: %s", "PMI_LOCATION_71 + PMI_INTERNAL_ENCODE_FAILURE");
    return finishFinalize(PMI_LOCATION_71 + PMI_INTERNAL_ENCODE_FAILURE, &buffer);
  }
  if (ei_x_encode_string(&buffer, "finalize") != 0) {
    ERROR("pmiFinalize: %s", "PMI_LOCATION_72 + PMI_INTERNAL_ENCODE_FAILURE");
    return finishFinalize(PMI_LOCATION_72 + PMI_INTERNAL_ENCODE_FAILURE, &buffer);
  }

  // cec_packet_t packet = {buffer.buffsz, buffer.buff};
  cec_packet_t packet = {buffer.index, buffer.buff};

  if (cec_send(appData->connectionHandle, &packet) == -1) {
    ERROR("pmiFinalize: %s", "PMI_INTERNAL_CEC_SEND_FAILURE");
    return finishFinalize(PMI_LOCATION_73 + PMI_INTERNAL_CEC_SEND_FAILURE, &buffer);
  }

  ei_x_free(&buffer);

  // Finally tear down connection to the CS
  if (cec_close(appData->connectionHandle) == -1) {
    ERROR("pmiFinalize: %s", "PMI_INTERNAL_CEC_CLOSE_FAILURE");
  }

  if (pmiMap_clear(pmiHandle) != PMS_HASHMAP_OK) {
    ERROR("pmiFinalize: %s", "PMI_INTERNAL_HASHMAP_FAILURE");
    return PMI_INTERNAL_HASHMAP_FAILURE;
  }

  INFO("pmiFinalize: %s", "PMI_OK");
  free(appData->callbacks);
  free(appData->callbacks_2);
  free(appData);
  return PMI_OK;

}
