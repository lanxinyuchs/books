/* ----------------------------------------------------------------------
 * %CCaseFile:	test_pmi2.c %
 * %CCaseRev:	/main/R3A/R4A/R5A/2 %
 * %CCaseDate:	2015-12-02 %
 * %CCaseDocNo: %
 * Author:	eolaand
 * Author: <name>, <e-mail address>
 *
 * Short description:
 * <Some rows here>
 * ----------------------------------------------------------------------
 * %CCaseTemplateFile:	template.c %
 * %CCaseTemplateId: CCver: /main/1 %
 *
 * %CCaseCopyrightBegin%
 * Copyright (c) Ericsson AB 2014-2015 All rights reserved.
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
 * R3A/1      2014-08-29 eolaand     Created
 * R4A/1      2015-11-25 erarafo     Using wrappers for TRI trace
 * R5A/1      2015-12-02 erarafo     Trace of function calls
 * R5A/2      2015-12-02 erarafo     Minor cleanup
 * ----------------------------------------------------------------------
 */

// asprintf requires _GNU_SOURCE
#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif

#include <stdbool.h>
#include <pthread.h>
#include "master.h"
#include "pms_pmi2.h"

#include <stdio.h>
#include <sys/prctl.h>
#include <limits.h>


#define IFT_PMI2_INITIALIZE          10
#define IFT_PMI2_COUNTER_MAP         11
#define IFT_PMI2_SETUP_CALLBACK      12
#define IFT_PMI2_DATA_ROP            13
#define IFT_PMI2_DATA_SHOW_COUNTERS  14
#define IFT_PMI2_FINALIZE            15
#define IFT_PMI2_EMUL_DEATH          16

extern bool TRI_trace_enabled;

/* Returns the space allocation needed for a stringlist,
 * including terminating nulls. Arity is the number of
 * list elements.
 *
 * In case of undecodeable data -1 is returned. Callers
 * must check against this.
 */

/* int stringListSize(ei_x_buff *buffer, int arity) { */
/*   ei_x_buff b = {buffer->buff, buffer->buffsz, buffer->index}; */

/*   int result = 0; */
/*   for (int j = 0; j < arity; j++) { */
/*     int stringLen = stringLength(&b); */
/*     if (stringLen < 0) { */
/*       return -1; */
/*     } */
/*     result += stringLen + 1; */
/*     ei_decode_string(b.buff, &(b.index), NULL); */
/*   } */
/*   return result; */
/* } */

// char *userDataValue;


/**
 * Returns the size of a null-terminated array of
 * pointers.
 */
int pmi2ArraySize(void **x) {
  int result = 0;
  for (void **p = x; *p != NULL; p++) {
    result++;
  }
  return result;
}


void pmi2Subscribe(Pmi2HandleT pmi2Handle,
		   uint32_t granularityPeriod,
		   Pmi2SubscribeRopSpecT const * const *subscribeRopSpecs,
		   void *userData) {

  QTRACE3(3, "callback invoked: pmi2subscribe, handle: %s",
	     (char *)pmi2Handle);

  printf("callback invoked: pmi2subscribe, handle: %s\n", (char *)pmi2Handle);

  ei_x_buff b;
  ei_x_new(&b);
  ei_x_encode_version(&b);

  ei_x_encode_tuple_header(&b, 2);
  ei_x_encode_atom(&b, "signal");

  ei_x_encode_tuple_header(&b, 2);
  ei_x_encode_atom(&b, "pmi2SubscribeRop");

  ei_x_encode_tuple_header(&b, 3);
  ei_x_encode_string(&b, pmi2Handle);
  ei_x_encode_ulong(&b, granularityPeriod);

  if (subscribeRopSpecs != NULL) {
    int nGroups = pmi2ArraySize((void **)subscribeRopSpecs);
    ei_x_encode_list_header(&b, nGroups);

    for (int j = 0; j < nGroups; j++) {
      ei_x_encode_tuple_header(&b, 2);
      ei_x_encode_ulong(&b, subscribeRopSpecs[j]->pmGroupIdAlias);

      int nTypes = subscribeRopSpecs[j]->noofMeasurementTypeIdAliases;
      printf("subscribeRop: nTypes = %d\n", nTypes);
      const unsigned int *types = subscribeRopSpecs[j]->measurementTypeIdAliases;

      ei_x_encode_list_header(&b, nTypes);
      for (int k = 0; k < nTypes; k++) {
	ei_x_encode_ulong(&b, types[k]);
      }
      ei_x_encode_empty_list(&b);
    }
  }

  ei_x_encode_empty_list(&b);

  sendCallback(b);

  QTRACE3(3, "callback invoked: subscribe, handle: %s, delivered message",
	       (char *)pmi2Handle);
}



void pmi2ReportRop(Pmi2HandleT pmi2Handle,
		   const uint32_t granularityPeriod,
		   const uint32_t reportId,
		   const uint32_t maxReportingTime,
		   void *userData) {

  QTRACE3(3, "callback invoked: pmi2ReportRop, handle: %s",
	       (char *)pmi2Handle);
  printf("callback invoked: pmi2ReportRop, handle = %s\n", (char *)pmi2Handle);
  printf("userData = %s\n", (char *)userData);

  ei_x_buff b;
  ei_x_new(&b);
  ei_x_format(&b, "{signal, {pmi2ReportRop, {~s, ~i, ~i, ~i}}}",
	      (char *)pmi2Handle,
	      granularityPeriod,
	      reportId,
	      maxReportingTime);

  sendCallback(b);
}


void pmi2ReportShowCounters(Pmi2HandleT pmi2Handle,
			    const uint32_t reportId,
			    const uint32_t moInstanceLdnAlias,
			    Pmi2ShowCountersSpecT const * const *scSpecs,
			    const uint32_t maxReportingTime,
			    void *userData) {

  printf("callback invoked: pmi2ReportShowCounters, handle = %s\n",
	 (char *)pmi2Handle);
  printf("userData = %s\n", (char *)userData);
  QTRACE3(3, "callback invoked: report, handle: %s", (char *)pmi2Handle);

  ei_x_buff b;
  ei_x_new(&b);
  ei_x_encode_version(&b);

  ei_x_encode_tuple_header(&b, 2);
  ei_x_encode_atom(&b, "signal");

  ei_x_encode_tuple_header(&b, 2);
  ei_x_encode_atom(&b, "pmi2ReportShowCounters");

  ei_x_encode_tuple_header(&b, 5);
  ei_x_encode_string(&b, pmi2Handle);
  ei_x_encode_ulong(&b, reportId);
  ei_x_encode_ulong(&b, moInstanceLdnAlias);

  if (scSpecs != NULL) {
    int nGroups = pmi2ArraySize((void **)scSpecs);
    ei_x_encode_list_header(&b, nGroups);

    for (int j = 0; j < nGroups; j++) {
      ei_x_encode_tuple_header(&b, 2);
      ei_x_encode_ulong(&b, scSpecs[j]->pmGroupIdAlias);

      int nTypes = scSpecs[j]->noofMeasurementTypeIdAliases;
      printf("show counters: nTypes = %d\n", nTypes);
      const unsigned int *types = scSpecs[j]->measurementTypeIdAliases;

      ei_x_encode_list_header(&b, nTypes);
      for (int k = 0; k < nTypes; k++) {
	ei_x_encode_ulong(&b, types[k]);
      }
      ei_x_encode_empty_list(&b);
    }
  }
  ei_x_encode_empty_list(&b);

  ei_x_encode_ulong(&b, maxReportingTime);

  sendCallback(b);

}




ei_x_buff handlePmi2Initialize(int nArgs,
			       ei_x_buff request,
			       ei_x_buff response) {


  int elementType;
  int elementSize;
  ei_get_type(request.buff, &request.index, &elementType,
	      &elementSize);

  printf("Element list type: %d\n", elementType);
  printf("Element list size: %d\n", elementSize);


  char *counterMap;

  if (elementType == ERL_ATOM_EXT) {
    request.index += elementSize + 2; // Length of atom undefined
    counterMap = NULL;
  }
  else {
    int handleLength = stringLength(&request);
    if (handleLength < 0) {
      ei_x_format(
		  &response,
		  "{error, ~s, ~i}",
		  "counterMap string length could not be determined",
		  -handleLength);
      return response;
    }
    counterMap = malloc(handleLength+1);
    ei_decode_string(request.buff, &request.index, counterMap);
    printf("counterMap: %s\n", counterMap);
    QTRACE3(3, "counterMap: %s", counterMap);
  }


  int cbSize;
  ei_decode_tuple_header(request.buff, &request.index, &cbSize);
  if (cbSize != 3) {
    ei_x_format(&response, "{error, ~s}", "invalid_cb_size");
    return response;
  }

  bool pmi2SubscribeCb;
  ei_decode_boolean(request.buff, &request.index, (int *)&pmi2SubscribeCb);
  bool pmi2ReportCb;
  ei_decode_boolean(request.buff, &request.index, (int *)&pmi2ReportCb);
  bool pmi2ReportSCCb;
  ei_decode_boolean(request.buff, &request.index, (int *)&pmi2ReportSCCb);
  int userDataValueLength = stringLength(&request);
  if (userDataValueLength < 0) {
    ei_x_format(
		&response,
		"{error, ~s, ~i}",
		"userData string length could not be determined",
		-userDataValueLength);
    return response;
  }
  char *userDataValue = (char *)malloc(userDataValueLength + 1);
  if (userDataValueLength == 0) {
    ei_skip_term(request.buff, &request.index);
    strcpy(userDataValue, "");
  }
  else {
    ei_decode_string(request.buff, &request.index, userDataValue);
  }

  QTRACE3(3, "got userData: %s\n", userDataValue);
  printf("pmi2Initialize, userData: %s\n",  userDataValue);

  Pmi2SubscribeRopCallbackSpecT pmi2SubscribeCbSpec =
    {.subscribeRopCallback = &pmi2Subscribe,
     .userData = userDataValue};
  Pmi2ReportRopCallbackSpecT pmi2ReportRopCbSpec =
    {.reportRopCallback = &pmi2ReportRop,
     .userData = userDataValue};
  Pmi2ReportShowCountersCallbackSpecT pmi2ReportShowCountersCbSpec =
    {.reportShowCountersCallback =
     &pmi2ReportShowCounters,
     .userData = userDataValue};

  Pmi2CallbacksT callbacks = {
    .subscribeRopCallbackSpec = &pmi2SubscribeCbSpec,
    .reportRopCallbackSpec = &pmi2ReportRopCbSpec,
    .reportShowCountersCallbackSpec = &pmi2ReportShowCountersCbSpec};

  if (!pmi2SubscribeCb)
    callbacks.subscribeRopCallbackSpec = NULL;
  if (!pmi2ReportCb)
    callbacks.reportRopCallbackSpec = NULL;
  if (!pmi2ReportSCCb)
    callbacks.reportShowCountersCallbackSpec = NULL;

  Pmi2HandleT pmi2Handle;

  // Pmi2ResultT r = pmi2Initialize(&pmi2Handle, counterMap, &callbacks);
  DRCALL(Pmi2ResultT, r,
      pmi2Initialize(&pmi2Handle, counterMap, &callbacks),
      "%s", "pmi2Initialize");

  printf("Called pmi2Initialize, got handle: %s\n", (char *)pmi2Handle);

  if (r != PMI2_OK) {
    QTRACE_ERROR2("pmiInitialize failed, status: %d", r);
    ei_x_format(&response, "{error, ~s, ~i}", "pmiInitialize failed", r);
  }
  else {
    ei_x_format(&response, "{ok, ~s}", (char *)pmi2Handle);
  }

  return response;
}


// void pmi2Dispatcher(char *pmi2Handle) {
int pmi2Dispatcher(char *pmi2Handle) {

  //int r = pmi2Dispatch(pmi2Handle);
  DRCALL(int, r, pmi2Dispatch(pmi2Handle), "%s", "pmi2Dispatch");

  if (r != PMI2_OK) {
    printf("pmi2Dispatch failed, handle: %s, status: %d\n", pmi2Handle, r);
    QTRACE4(3, "pmi2Dispatch failed, handle: %s, status: %d", pmi2Handle, r);
    return DISPATCH_FAILED;
  }
  else {
    printf("pmiDispatch succeeded, handle: %s\n", pmi2Handle);
    QTRACE3(3, "pmiDispatch succeeded, handle: %s", pmi2Handle);
    return DISPATCH_OK;
  }
}


ei_x_buff handlePmi2SetupCallback(int nArgs, ei_x_buff request, ei_x_buff response) {

  if (nArgs != 1) {
    ei_x_format(&response, "{error, ~s}", "handlePmi2SetupCallback, wrong number of args");
    return response;
  }

  int handleLength = stringLength(&request);
  if (handleLength < 0) {
    ei_x_format(
		&response,
		"{error, ~s, ~i}",
		"handle length could not be determined",
		-handleLength);
    return response;
  }
  char *handle = malloc(handleLength+1);

  ei_decode_string(request.buff, &request.index, handle);
  QTRACE3(3, "pmiSetupCallback, handle: %s", handle);

  Pmi2SelectionObjectT selectionObject;

  // Pmi2ResultT result = pmi2SelectionObjectGet((Pmi2HandleT)handle, &selectionObject);
  DRCALL(Pmi2ResultT, result,
      pmi2SelectionObjectGet((Pmi2HandleT)handle, &selectionObject),
      "%s", "pmi2SelectionObjectGet");

  if (result != PMI2_OK) {
    QTRACE_ERROR2("pmi2SelectionObjectGet failed, status: %d", result);
    free(handle);
    ei_x_format(&response, "{error, ~s, ~i}", "pmi2SelectionObjectGet failed",
		result);
  }
  else {
    QTRACE4(3, "pmi2SelectionObjectGet, handle: %s, fd: %d", handle,
	       (int)selectionObject);
    addPollScheduleItem(handle, (int)selectionObject, (DispatcherT)&pmi2Dispatcher);
    ei_x_format(&response, "{ok, ~i}", (int)selectionObject);
  }
  return response;
}


ei_x_buff handlePmi2CounterMap(int nArgs, ei_x_buff request, ei_x_buff response)
{


  if (nArgs != 2) {
    ei_x_format(&response, "{error, ~s}",
		"handlePmi2CounterMap, wrong number of args");
    return response;
  }


  int handleLength = stringLength(&request);
  if (handleLength < 0) {
    ei_x_format(
		&response,
		"{error, ~s, ~i}",
		"handle length could not be determined",
		-handleLength);
    return response;
  }

  char pmi2Handle[handleLength+1];
  ei_decode_string(request.buff, &request.index, pmi2Handle);
  printf("pmi2CounterMap, handle: %s\n", pmi2Handle);
  QTRACE3(3, "pmi2CounterMap, handle: %s", pmi2Handle);

  // Get the Counter Map
  int nGroups;
  ei_decode_list_header(request.buff, &request.index, &nGroups);
  printf("Number of Groups: %d\n", nGroups);
  QTRACE3(3, "  n. of groups: %d", nGroups);

  Pmi2CounterMapT **groups = calloc(nGroups+1, sizeof(Pmi2CounterMapT *));
  groups[nGroups] = NULL;
  for (int j = 0; j < nGroups; j++) {
    groups[j] = (Pmi2CounterMapT *)malloc(sizeof(Pmi2CounterMapT));
    int groupArity;
    ei_decode_tuple_header(request.buff, &request.index, &groupArity);
    if (groupArity != 3) {
      ei_x_format(&response, "{error, ~s}", "unexpected group arity");
      return response;
    }

    int groupNameLength = stringLength(&request);
    printf("Group name length: %d\n", groupNameLength);
    if (groupNameLength < 0) {
      ei_x_format(&response,
		  "{error, ~s, ~i}",
		  "group name length could not be determined",
		  -groupNameLength);
      return response;
    }
    char *groupName = (char *)malloc(groupNameLength + 1);
    ei_decode_string(request.buff, &request.index, groupName);
    printf("Group name: %s\n", groupName);
    QTRACE3(3, "  group name: %s", groupName);
    groups[j]->pmGroupId = groupName;

    unsigned long groupIdAlias;
    ei_decode_ulong(request.buff, &request.index, &groupIdAlias);
    printf("pmi2CounterMap, groupIdAlias: %d\n", (int)groupIdAlias);
    QTRACE3(3, "pmi2CounterMap, groupIdAlias: %d", (int)groupIdAlias);
    groups[j]->pmGroupIdAlias = groupIdAlias;

    int nMeasTypes;
    ei_decode_list_header(request.buff, &request.index, &nMeasTypes);
    QTRACE3(3, "  n. of MeasTypes: %d", nMeasTypes);
    printf("Number of MeasTypes: %d\n", nMeasTypes);

    Pmi2MeasurementTypeMapT **measTypes =
      calloc(nMeasTypes+1, sizeof(Pmi2MeasurementTypeMapT *));
    measTypes[nMeasTypes] = NULL;
    for (int l = 0; l < nMeasTypes; l++)
      {
	measTypes[l] =
	  (Pmi2MeasurementTypeMapT *)malloc(sizeof(Pmi2MeasurementTypeMapT));

	int measTypeMapArity;
	ei_decode_tuple_header(request.buff, &request.index, &measTypeMapArity);
	if (measTypeMapArity != 2) {
	  ei_x_format(&response, "{error, ~s}", "unexpected MeasTypeMap arity");
	  return response;
	}

	int typeNameLength = stringLength(&request);
	if (typeNameLength < 0) {
	  ei_x_format(
		      &response,
		      "{error, ~s, ~i}",
		      "type name length could not be determined",
		      -typeNameLength);
	  return response;
	}
	printf("Type name length: %d\n", typeNameLength);
	char *typeName = (char *)malloc(typeNameLength + 1);
	ei_decode_string(request.buff, &request.index, typeName);
	printf("Type name: %s\n", typeName);
	QTRACE3(3, "    type name: %s", typeName);

	measTypes[l]->measurementTypeId = typeName;
	unsigned long measTypeIdAlias;
	ei_decode_ulong(request.buff, &request.index, &measTypeIdAlias);
	printf("pmi2CounterMap, measTypeIdAlias: %d\n", (int)measTypeIdAlias);
	QTRACE3(3, "pmi2CounterMap, measTypeIdAlias: %d",
		     (int) measTypeIdAlias);

	measTypes[l]->measurementTypeIdAlias = measTypeIdAlias;
	printf("for l: %d\n", l);
      }
    printf("for j: %d\n", j);
    if (nMeasTypes > 0) {
      printf("measTypes: %d\n", (int)measTypes);
      groups[j]->measurementTypes =
       	(Pmi2MeasurementTypeMapT const * const *)measTypes;
      printf("measurementTypes, ptr: %d\n", (int)groups[j]->measurementTypes);
      printf("measTypes, Id: %s\n", measTypes[0]->measurementTypeId);
      printf("measurementTypes, Id: %s\n",
	     groups[j]->measurementTypes[0]->measurementTypeId);
      printf("measTypes, Alias: %d\n",
	     (int)measTypes[0]->measurementTypeIdAlias);
      ei_skip_term(request.buff, &request.index);  // end of types in group
    }
    else {
      groups[j]->measurementTypes = NULL;
    }
    printf("pmi2CounterMap, current group name: %s\n", groups[j]->pmGroupId);
  }
  ei_skip_term(request.buff, &request.index);  // end of groups

  Pmi2ResultT result;
  if (nGroups > 0) {
    printf("pmi2CounterMap, first group name: %s\n", groups[0]->pmGroupId);
    printf("Call pmi2CounterMap with handle %s and valid pointer\n",
	   pmi2Handle);
    // result = pmi2CounterMap((Pmi2HandleT)pmi2Handle, (Pmi2CounterMapT const * const *)groups);
    RCALL(result,
        pmi2CounterMap((Pmi2HandleT)pmi2Handle, (Pmi2CounterMapT const * const *)groups),
        "%s", "pmi2CounterMap (1)");
  }
  else {
    printf("Call pmi2CounterMap with handle %s and null pointer\n",
	   pmi2Handle);
    // result = pmi2CounterMap((Pmi2HandleT)pmi2Handle, NULL);
    RCALL(result,
        pmi2CounterMap((Pmi2HandleT)pmi2Handle, NULL),
        "%s", "pmi2CounterMap (2)");
  }

  printf("pmi2CounterMap, result: %d\n", result);
  if (result != PMI2_OK) {
    QTRACE_ERROR2("pmi2CounterMap failed, status: %d", result);
    ei_x_format(&response, "{error, ~s, ~i, ~s}", "pmi2CounterMap failed",
		result, pmi2Handle);
  }
  else {
    QTRACE2(3, "pmi2CounterMap succeeded");
    ei_x_format(&response, "{ok}");
  }

  for (int j = 0; j < nGroups; j++) {
    free((void *)(groups[j]->pmGroupId));
    if (groups[j]->measurementTypes != NULL) {
      Pmi2MeasurementTypeMapT const * const *t = groups[j]->measurementTypes;
      for (Pmi2MeasurementTypeMapT const * const *p = t; *p != NULL; p++) {
	free((void *)(*p)->measurementTypeId);
	free((void *) *p);
      }
    }
    free((void *) groups[j]);
  }

  return response;
}


Pmi2ResultT decode_meas_values(const int elementType,
			       int64_t *elements,
			       ei_x_buff *request,
			       ei_x_buff *response);


void free_meas_values(Pmi2MeasurementValueT const * const *measValues);


void free_value_bundle(Pmi2ValueBundleT **valueBundles);


ei_x_buff handlePmi2DataRop(int nArgs, ei_x_buff request, ei_x_buff response) {

  if (nArgs != 5) {
    ei_x_format(&response, "{error, ~s}",
		"handlePmi2DataRop, wrong number of args");
    return response;
  }

  int handleLength = stringLength(&request);
  if (handleLength < 0) {
    ei_x_format(
		&response,
		"{error, ~s, ~i}",
		"handle length could not be determined",
		-handleLength);
    return response;
  }

  char pmi2Handle[handleLength+1];
  ei_decode_string(request.buff, &request.index, pmi2Handle);
  QTRACE3(3, "pmi2DataRop, handle: %s", pmi2Handle);
  printf("pmi2DataRop, handle: %s\n", pmi2Handle);

  // get GP
  unsigned long gp;
  ei_decode_ulong(request.buff, &request.index, &gp);
  QTRACE3(3, "pmi2DataRop, gp: %d", (int)gp);
  printf("pmi2DataRop, gp: %d\n", (int)gp);

  // get reportId
  unsigned long reportId;
  ei_decode_ulong(request.buff, &request.index, &reportId);
  /* if (reportId >= TWO_TO_31) { */
  /*   ei_x_format( */
  /* 		&response, */
  /* 		"{error, ~s, ~i}", */
  /* 		"unknown type for reportId, code: ", */
  /* 		(long)(reportId - TWO_TO_31)); */
  /*   return response; */
  /* } */

  QTRACE3(3, "pmi2Datarop, reportId: %d", (int)reportId);
  printf("pmi2Datarop, reportId: %d\n", (int)reportId);


  // get the data; number of groups
  int nGroups;
  ei_decode_list_header(request.buff, &request.index, &nGroups);
  QTRACE3(3, "  n. of groups: %d", nGroups);
  printf("pmi2Datarop, number of groups: %d\n", nGroups);

  Pmi2ValueBundleT **groups = calloc(nGroups+1, sizeof(Pmi2ValueBundleT *));
  groups[nGroups] = NULL;
  for (int j = 0; j < nGroups; j++) {
    groups[j] = (Pmi2ValueBundleT *)malloc(sizeof(Pmi2ValueBundleT));

    int groupArity;
    ei_decode_tuple_header(request.buff, &request.index, &groupArity);
    if (groupArity != 2) {
      ei_x_format(&response, "{error, ~s}", "unexpected group arity");
      return response;
    }

    unsigned long groupIdAlias;
    ei_decode_ulong(request.buff, &request.index, &groupIdAlias);
    QTRACE3(3, "pmi2DataRop, groupIdAlias: %d", (int)groupIdAlias);
    printf("pmi2DataRop, groupIdAlias: %d\n", (int)groupIdAlias);

    int nMoInstances;
    ei_decode_list_header(request.buff, &request.index, &nMoInstances);
    QTRACE3(3, "  n. of MO instances: %d", nMoInstances);
    printf("pmi2DataRop, number of MO instances: %d\n", nMoInstances);

    Pmi2MoInstanceBundleT **moInstances =
      calloc(nMoInstances+1, sizeof(Pmi2MoInstanceBundleT *));
    moInstances[nMoInstances] = NULL;
    for (int k = 0; k < nMoInstances; k++) {
      moInstances[k] =
	(Pmi2MoInstanceBundleT *)malloc(sizeof(Pmi2MoInstanceBundleT));

      int moInstArity;
      ei_decode_tuple_header(request.buff, &request.index, &moInstArity);
      if (moInstArity != 2) {
	ei_x_format(&response, "{error, ~s}", "unexpected MO instance arity");
	return response;
      }

      unsigned long moInstAlias;
      ei_decode_ulong(request.buff, &request.index, &moInstAlias);
      QTRACE3(3, "pmi2DataRop, moInstLDNAlias: %d", (int)moInstAlias);
      printf("pmi2DataRop, moInstLDNAlias: %d\n", (int)moInstAlias);
      int nMeasValues;
      ei_decode_list_header(request.buff, &request.index, &nMeasValues);
      QTRACE3(3, "  n. of MeasValues: %d", nMeasValues);
      printf("Number of MeasValues: %d\n", nMeasValues);
      Pmi2MeasurementValueT **measValues =
	calloc(nMeasValues+1, sizeof(Pmi2MeasurementValueT *));
      measValues[nMeasValues] = NULL;
      for (int l = 0; l < nMeasValues; l++)
	{
	  measValues[l] =
	    (Pmi2MeasurementValueT *)malloc(sizeof(Pmi2MeasurementValueT));

	  int measValArity;
	  ei_decode_tuple_header(request.buff, &request.index, &measValArity);
	  if (measValArity != 2) {
	    ei_x_format(&response, "{error, ~s}", "unexpected MeasValue arity");
	    return response;
	  }

	  unsigned long measTypeIdAlias;
	  ei_decode_ulong(request.buff, &request.index, &measTypeIdAlias);
	  QTRACE3(3, "pmi2DataRop, measTypeIdAlias: %d",
		       (int) measTypeIdAlias);
	  printf("pmi2DataRop, measTypeIdAlias: %d\n",(int) measTypeIdAlias);

	  int elementType;
	  int elementSize;
	  ei_get_type(request.buff, &request.index, &elementType,
		      &elementSize);

	  printf("Element list type: %d\n", elementType);
	  printf("Element list size: %d\n", elementSize);

	  int nElements = elementSize;
	  int64_t *elements =
	    (int64_t *)calloc(nElements, sizeof(int64_t));

	  Pmi2ResultT decodeRes;
	  decodeRes =
	    decode_meas_values(elementType, elements, &request, &response);
	  if (decodeRes != PMI2_OK) {
	    return response;
	  }

	  measValues[l]->measurementTypeIdAlias = measTypeIdAlias;
	  measValues[l]->multiplicity = nElements;
	  measValues[l]->elements = elements;
	}
      ei_skip_term(request.buff, &request.index);  // end of types for LDN
      moInstances[k]->moInstanceLdnAlias = moInstAlias;
      moInstances[k]->measurementValues =
	(Pmi2MeasurementValueT const * const *)measValues;
    }
    ei_skip_term(request.buff, &request.index);  // end of LDN for Group
    groups[j]->pmGroupIdAlias = groupIdAlias;
    groups[j]->instanceBundles =
      (Pmi2MoInstanceBundleT const * const *)moInstances;
  }

  ei_skip_term(request.buff, &request.index);  // end of bundles

  bool finalFrag;
  ei_decode_boolean(request.buff, &request.index, (int *)&finalFrag);

  printf("Call pmi2DataRop\n");
  // Pmi2ResultT result = pmi2DataRop((Pmi2HandleT)pmi2Handle, gp, reportId, (Pmi2ValueBundleT const * const *)groups, finalFrag);
  DRCALL(Pmi2ResultT, result,
      pmi2DataRop((Pmi2HandleT)pmi2Handle, gp, reportId, (Pmi2ValueBundleT const * const *)groups, finalFrag),
      "%s", "pmi2DataRop");
  printf("After pmi2DataRop\n");

  if (result != PMI2_OK) {
    QTRACE_ERROR2("pmi2DataRop failed, status: %d", result);
    printf("pmi2DataRop failed, status: %d\n", result);
    ei_x_format(&response, "{error, ~s, ~i, ~s}", "pmi2DataRop failed", result,
		pmi2Handle);
  }
  else {
    QTRACE2(3, "pmi2DataRop succeeded");
    printf("pmi2DataRop succeeded\n");
    ei_x_format(&response, "{ok}");
  }

  free_value_bundle(groups);

  return response;
}


ei_x_buff handlePmi2DataShowCounters(int nArgs, ei_x_buff request,
				     ei_x_buff response) {

  if (nArgs != 5) {
    ei_x_format(&response, "{error, ~s}", "handleDataShowCounters, wrong number of args");
    return response;
  }

  int handleLength = stringLength(&request);
  if (handleLength < 0) {
    ei_x_format(
		&response,
		"{error, ~s, ~i}",
		"handle length could not be determined",
		-handleLength);
    return response;
  }

  char handle[handleLength+1];
  ei_decode_string(request.buff, &request.index, handle);
  QTRACE3(3, "pmiDataShowCounters, handle: %s", handle);

  // get RequestId
  unsigned long reportId;
  ei_decode_ulong(request.buff, &request.index, &reportId);
  QTRACE3(3, "pmiDataShowCounters, requestId: %d", (int)reportId);

  // get Result
  unsigned long result;
  ei_decode_ulong(request.buff, &request.index, &result);
  QTRACE3(3, "pmiDataShowCounters, result: %d", (int)result);


  // get errorString
  int errorStringLength = stringLength(&request);
  if (errorStringLength < 0) {
    ei_x_format(
		&response,
		"{error, ~s, ~i}",
		"errorString length could not be determined",
		-errorStringLength);
    return response;
  }

  char errorString[errorStringLength+1];
  ei_decode_string(request.buff, &request.index, errorString);
  QTRACE3(3, "pmiDataShowCounters, errorString: %s", errorString);
  QINFO2("pmiDataShowCounters, errorString: %s", errorString);
  printf("pmiDataShowCounters, errorString length: %d\n", errorStringLength);
  printf("pmiDataShowCounters, errorString: %s\n", errorString);

  // get the data; number of groups
  int nGroups;
  ei_decode_list_header(request.buff, &request.index, &nGroups);
  QTRACE3(3, "  n. of groups: %d", nGroups);
  printf("pmi2Datarop, number of groups: %d\n", nGroups);

  Pmi2ValueBundleT **groups = calloc(nGroups+1, sizeof(Pmi2ValueBundleT *));
  groups[nGroups] = NULL;
  for (int j = 0; j < nGroups; j++) {
    groups[j] = (Pmi2ValueBundleT *)malloc(sizeof(Pmi2ValueBundleT));

    int groupArity;
    ei_decode_tuple_header(request.buff, &request.index, &groupArity);
    if (groupArity != 2) {
      ei_x_format(&response, "{error, ~s}", "unexpected group arity");
      return response;
    }

    unsigned long groupIdAlias;
    ei_decode_ulong(request.buff, &request.index, &groupIdAlias);
    QTRACE3(3, "pmi2DataRop, groupIdAlias: %d", (int)groupIdAlias);
    printf("pmi2DataRop, groupIdAlias: %d\n", (int)groupIdAlias);

    int nMoInstances;
    ei_decode_list_header(request.buff, &request.index, &nMoInstances);
    QTRACE3(3, "  n. of MO instances: %d", nMoInstances);
    printf("pmi2DataRop, number of MO instances: %d\n", nMoInstances);

    Pmi2MoInstanceBundleT **moInstances =
      calloc(nMoInstances+1, sizeof(Pmi2MoInstanceBundleT *));
    moInstances[nMoInstances] = NULL;
    for (int k = 0; k < nMoInstances; k++) {
      moInstances[k] =
	(Pmi2MoInstanceBundleT *)malloc(sizeof(Pmi2MoInstanceBundleT));

      int moInstArity;
      ei_decode_tuple_header(request.buff, &request.index, &moInstArity);
      if (moInstArity != 2) {
	ei_x_format(&response, "{error, ~s}", "unexpected MO instance arity");
	return response;
      }

      unsigned long moInstAlias;
      ei_decode_ulong(request.buff, &request.index, &moInstAlias);
      QTRACE3(3, "pmi2DataRop, moInstLDNAlias: %d", (int)moInstAlias);
      printf("pmi2DataRop, moInstLDNAlias: %d\n", (int)moInstAlias);
      int nMeasValues;
      ei_decode_list_header(request.buff, &request.index, &nMeasValues);
      QTRACE3(3, "  n. of MeasValues: %d", nMeasValues);
      printf("Number of MeasValues: %d\n", nMeasValues);
      Pmi2MeasurementValueT **measValues =
	calloc(nMeasValues+1, sizeof(Pmi2MeasurementValueT *));
      measValues[nMeasValues] = NULL;
      for (int l = 0; l < nMeasValues; l++)
	{
	  measValues[l] =
	    (Pmi2MeasurementValueT *)malloc(sizeof(Pmi2MeasurementValueT));

	  int measValArity;
	  ei_decode_tuple_header(request.buff, &request.index, &measValArity);
	  if (measValArity != 2) {
	    ei_x_format(&response, "{error, ~s}", "unexpected MeasValue arity");
	    return response;
	  }

	  unsigned long measTypeIdAlias;
	  ei_decode_ulong(request.buff, &request.index, &measTypeIdAlias);
	  QTRACE3(3, "pmi2DataRop, measTypeIdAlias: %d",
		       (int) measTypeIdAlias);
	  printf("pmi2DataRop, measTypeIdAlias: %d\n",(int) measTypeIdAlias);

	  int elementType;
	  int elementSize;
	  ei_get_type(request.buff, &request.index, &elementType,
		      &elementSize);

	  printf("Element list type: %d\n", elementType);
	  printf("Element list size: %d\n", elementSize);

	  int nElements = elementSize;
	  int64_t *elements =
	    (int64_t *)calloc(nElements, sizeof(int64_t));

	  Pmi2ResultT decodeRes;
	  decodeRes =
	    decode_meas_values(elementType, elements, &request, &response);
	  if (decodeRes != PMI2_OK) {
	    return response;
	  }

	  measValues[l]->measurementTypeIdAlias = measTypeIdAlias;
	  measValues[l]->multiplicity = nElements;
	  measValues[l]->elements = elements;
	}
      ei_skip_term(request.buff, &request.index);  // end of types for LDN
      moInstances[k]->moInstanceLdnAlias = moInstAlias;
      moInstances[k]->measurementValues =
	(Pmi2MeasurementValueT const * const *)measValues;
    }
    ei_skip_term(request.buff, &request.index);  // end of LDN for Group
    groups[j]->pmGroupIdAlias = groupIdAlias;
    groups[j]->instanceBundles =
      (Pmi2MoInstanceBundleT const * const *)moInstances;
  }




  ei_skip_term(request.buff, &request.index);  // end of bundles

  Pmi2ResultT pmiResult;
  // pmiResult = pmi2DataShowCounters((Pmi2HandleT)handle, reportId, result, errorString, (Pmi2ValueBundleT const * const *)groups);
  RCALL(pmiResult,
      pmi2DataShowCounters((Pmi2HandleT)handle, reportId, result, errorString, (Pmi2ValueBundleT const * const *)groups),
      "%s", "pmi2DataShowCounters");

  if (pmiResult != PMI2_OK) {
    QTRACE_ERROR2("pmi2DataShowCounters failed, status: %d", pmiResult);
    ei_x_format(&response, "{error, ~s, ~i, ~s}",
		"pmi2DataShowCounters failed", pmiResult, handle);
  }
  else {
    QTRACE2(3, "pmi2DataShowCounters succeeded");
    ei_x_format(&response, "{ok}");
  }

  free_value_bundle(groups);
  /* Pmi2MeasurementValueT **t = measValues;  */
  /* for (Pmi2MeasurementValueT **p = t; *p != NULL; p++) {  */
  /*   free((void *)(*p)->elements);  */
  /*   free(*p);  */
  /* }  */
  /* free(t);  */

  return response;
}


ei_x_buff handlePmi2Finalize(int nArgs, ei_x_buff request, ei_x_buff response) {

  if (nArgs != 1) {
    ei_x_format(&response, "{error, ~s}",
		"handlePmi2Finalize, wrong number of args");
    return response;
  }

  int handleLength = stringLength(&request);
  if (handleLength < 0) {
    ei_x_format(
		&response,
		"{error, ~s, ~i}",
		"handle length could not be determined",
		-handleLength);
    return response;
  }
  char handle[handleLength+1];
  ei_decode_string(request.buff, &request.index, handle);
  QTRACE3(3, "pmiFinalize, handle: %s", handle);

  removePollScheduleItem(handle);

  // Pmi2ResultT r = pmi2Finalize((Pmi2HandleT)handle);
  DRCALL(Pmi2ResultT, r,
      pmi2Finalize((Pmi2HandleT)handle),
      "%s", "pmi2Finalize");

  if (r != PMI2_OK) {
    QTRACE_ERROR2("pmi2Finalize failed, status: %d", r);
    ei_x_format(&response, "{error, ~s, ~i}", "pmi2Finalize failed", r);
  }
  else {
    QTRACE2(3, "pmi2Finalize succeeded");
    ei_x_format(&response, "{ok}");
  }
  return response;
}


ei_x_buff handlePmi2EmulDeath(int nArgs, ei_x_buff request, ei_x_buff response)
{

  if (nArgs != 2) {
    ei_x_format(&response, "{error, ~s}",
		"handlePmi2EmulDeath, wrong number of args");
    return response;
  }

  int handleLength = stringLength(&request);
  if (handleLength < 0) {
    ei_x_format(
		&response,
		"{error, ~s, ~i}",
		"handle length could not be determined",
		-handleLength);
    return response;
  }
  char handle[handleLength+1];
  ei_decode_string(request.buff, &request.index, handle);
  QTRACE3(3, "emulate thread death, handle: %s", handle);

  bool closeSocket;
  ei_decode_boolean(request.buff, &request.index, (int *)&closeSocket);

  setDeathMark(handle, closeSocket);

  ei_x_format(&response, "{ok}");

  return response;
}


char *decodePmi2Function(int code) {
  if (code == IFT_PMI2_INITIALIZE) {
    return "pmi2Initialize";
  }
  else if (code == IFT_PMI2_COUNTER_MAP) {
    return "pmi2CounterMap";
  }
  else if (code == IFT_PMI2_SETUP_CALLBACK) {
    return "pmi2SetupCallback";
  }
  else if (code == IFT_PMI2_DATA_ROP) {
    return "pmi2DataRop";
  }
  else if (code == IFT_PMI2_DATA_SHOW_COUNTERS) {
    return "pmi2DataShowCounters";
  }
  else if (code == IFT_PMI2_FINALIZE) {
    return "pmi2Finalize";
  }
  else if (code == IFT_PMI2_EMUL_DEATH) {
    return "pmi2EmulDeath";
  }
  else {
    QTRACE_ERROR2("test suite error: unknown function code: %d", code);
    return "unknown function";
  }
}

/**
 * Handle a call from the test suite towards the CS.
 */
ei_x_buff send_sig_pmi2(int function, ei_x_buff request) {
  QTRACE3(3, "send_sig_pmi2, function: %s", decodePmi2Function(function));

  QINFO2("TESTAPP pmi function %i\n", function);
  int nArgs;
  ei_decode_tuple_header(request.buff, &request.index, &nArgs);

  ei_x_buff response;
  ei_x_new(&response);

  switch (function) {
  case IFT_PMI2_INITIALIZE:
    return handlePmi2Initialize(nArgs, request, response);
  case IFT_PMI2_SETUP_CALLBACK:
    return handlePmi2SetupCallback(nArgs, request, response);
  case IFT_PMI2_COUNTER_MAP:
    return handlePmi2CounterMap(nArgs, request, response);
  case IFT_PMI2_DATA_ROP:
    return handlePmi2DataRop(nArgs, request, response);
  case IFT_PMI2_DATA_SHOW_COUNTERS:
    return handlePmi2DataShowCounters(nArgs, request, response);
  case IFT_PMI2_FINALIZE:
    return handlePmi2Finalize(nArgs, request, response);
  case IFT_PMI2_EMUL_DEATH:
    QINFO1("TESTAPP pmi2 emul death\n");
    return handlePmi2EmulDeath(nArgs, request, response);
  default:
    QINFO1("TESTAPP pmi2 unknown function\n");
    QTRACE_ERROR2("unknown function: %d", function);
    ei_x_format(&response, "{error, ~s}", "unknown function");
    return response;
  }
}


Pmi2ResultT decode_meas_values(const int elementListType,
			       int64_t *elements,
			       ei_x_buff *request,
			       ei_x_buff *response) {

  int nElements;
  int64_t *q = elements;
  Pmi2ResultT result = 0;

  if (elementListType == ERL_LIST_EXT) {
    ei_decode_list_header(request->buff, &request->index, &nElements);
    QTRACE3(3, "  n. of Elements: %d", nElements);
    printf("Number of Elements: %d\n", nElements);

    for (int m = 0; m < nElements; m++, q++) {

      int elementType;
      int elementSize;
      ei_get_type(request->buff, &request->index, &elementType,
		  &elementSize);

      printf("Integer element type: %d\n", elementType);
      printf("Integer element size: %d\n", elementSize);

      if (elementType == ERL_SMALL_BIG_EXT) {
	// 64-bit signed integer
	ei_decode_longlong(request->buff, &request->index, q);
	QTRACE2(3, "element is 64-bit signed integer, cannot display");
	printf("element is 64-bit signed integer, cannot display");
      }
      else {
	long long elementValueL = decodeInteger(request);
	if (elementValueL >= TWO_TO_31) {
	  ei_x_format(
		      response,
		      "{error, ~s, ~i}",
		      "measurement elementValue of unknown type, code: ",
		      (long)(elementValueL - TWO_TO_31));
	  printf("measurement elementValue of unknown type, code: %d\n",
		 (int)(elementValueL - TWO_TO_31));
	  //	  return response;
	  return result;
	}
	else {
	  printf("Integer element Val: %d\n", (int)elementValueL);
	  *q = elementValueL;
	}
      }
    }
    ei_skip_term(request->buff, &request->index);  // end of values
  }
  else {
    int nElements = stringLength(request);
    if (nElements < 0) {
      ei_x_format(
		  response,
		  "{error, ~s, ~i}",
		  "elements length could not be determined",
		  -nElements);
      return result;
    }
    printf("Elements length: %d\n", nElements);
    char *elementsCh = (char *)malloc(nElements + 1);
    ei_decode_string(request->buff, &request->index, elementsCh);
    for (int n = 0; n < nElements; n++, q++) {
      if (elementsCh[n] < 0) {
	printf("Element value: %d\n", (int)elementsCh[n]);
	*q = (long long)(256 + elementsCh[n]);
      }
      else {
	printf("Element value: %d\n", (int)elementsCh[n]);
	*q = (long long)elementsCh[n];
      }
    }
    free(elementsCh);
  }
  return PMI2_OK;
}


void free_value_bundle(Pmi2ValueBundleT **valueBundles) {

  //  int nGroups = 0;

  if (valueBundles != NULL) {
    for (int i = 0; valueBundles[i] != NULL; i++) {
      if (valueBundles[i]->instanceBundles != NULL) {
	Pmi2MoInstanceBundleT const * const *p =
	  valueBundles[i]->instanceBundles;
	for (int j = 0; p[j] != NULL; j++) {
	  free_meas_values(p[j]->measurementValues);
	  free((void *)p[j]);
	}
	free((void *)valueBundles[i]->instanceBundles);
      }
      free(valueBundles[i]);
    }
  }
  free(valueBundles);
}


void free_meas_values(Pmi2MeasurementValueT const * const *measValues) {

  printf("free_meas_values: measurementValues = %d\n", (int)measValues);
  if (measValues != NULL) {
    for (int i=0; measValues[i] != NULL; i++) {
      free((void *)measValues[i]->elements);
      free((void *)measValues[i]);
    }
  }
  free((void *)measValues);
}
