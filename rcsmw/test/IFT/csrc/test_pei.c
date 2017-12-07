/* ----------------------------------------------------------------------
 * %CCaseFile:	test_pei.c %
 * %CCaseRev:	/main/R3A/R4A/1 %
 * %CCaseDate:	2015-11-26 %
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
 * R3A/1      2014-11-26 eolaand     Created
 * ----------------------------------------------------------------------
 */

// asprintf requires _GNU_SOURCE
#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif

#include <stdbool.h>
#include <pthread.h>
#include "master.h"
#include "pes_pei.h"

#include <stdio.h>
#include <sys/prctl.h>
#include <limits.h>


#define IFT_PEI_INITIALIZE          1
#define IFT_PEI_SETUP_CALLBACK      2
#define IFT_PEI_EVENT_JOB           3
#define IFT_PEI_ME_ATTR_UPDATE      4
#define IFT_PEI_FINALIZE            5
#define IFT_PEI_EMUL_DEATH          6

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
int peiArraySize(void **x) {
  int result = 0;
  for (void **p = x; *p != NULL; p++) {
    result++;
  }
  return result;
}


void peiEventJob(PeiHandleT peiHandle,
		 char *eventProducerId,
		 char *eventJobId,
		 uint32_t requestedJobState,
		 PeiEventTypeT const *eventTypes,
		 PeiEventFilterT const * const *eventFilters,
		 PeiFileControlT *fileControl,
		 PeiStreamControlT *streamControl,
		 void *userData) {

  QTRACE3(3, "callback invoked: peiEventJob, handle: %s",
	       (char *)peiHandle);

  printf("callback invoked: peiEventJob, handle: %s\n", (char *)peiHandle);

  ei_x_buff b;
  ei_x_new(&b);
  ei_x_encode_version(&b);

  ei_x_encode_tuple_header(&b, 2);
  ei_x_encode_atom(&b, "signal");

  ei_x_encode_tuple_header(&b, 2);
  ei_x_encode_atom(&b, "peiEventJob");

  ei_x_encode_tuple_header(&b, 8);
  ei_x_encode_string(&b, peiHandle);
  printf("peiEventJob, eventProducerId: %s\n", eventProducerId);
  ei_x_encode_string(&b, eventProducerId);
  printf("peiEventJob, eventJobId: %s\n", eventJobId);
  ei_x_encode_string(&b, eventJobId);
  printf("peiEventJob, requestedJobState: %d\n", requestedJobState);
  ei_x_encode_ulong(&b, requestedJobState);

  int nTypes = 0;
  if (eventTypes != NULL) {
    nTypes = eventTypes->noofTypeAliases;
    ei_x_encode_list_header(&b, nTypes);
    printf("peiEventJob, nTypes: %d\n", nTypes);
    for (int k = 0; k < nTypes; k++) {
      printf("peiEventJob, eventTypes->typeAliases[%d]: %d\n", k,
	     eventTypes->typeAliases[k]);
      ei_x_encode_ulong(&b, eventTypes->typeAliases[k]);
    }
  }

  ei_x_encode_empty_list(&b);

  int nFilters = 0;
  if (eventFilters[0] != NULL) {
    nFilters = peiArraySize((void **)eventFilters);
    printf("peiEventJob, nFilters: %d\n", nFilters);
    ei_x_encode_list_header(&b, nFilters);
    for (int j = 0; j < nFilters; j++) {
      ei_x_encode_tuple_header(&b, 2);
      ei_x_encode_string(&b, eventFilters[j]->name);
      ei_x_encode_string(&b, eventFilters[j]->value);
    }
  }

  printf("peiEventJob, nFilters after: %d\n", nFilters);
  ei_x_encode_empty_list(&b);

  if (fileControl != NULL) {
    ei_x_encode_tuple_header(&b, 2);
    printf("peiEventJob, fileControl->reportingPeriod: %d\n",
	   fileControl->reportingPeriod);
    ei_x_encode_ulong(&b, fileControl->reportingPeriod);
    if (fileControl->compressionType != NULL) {
      ei_x_encode_ulong(&b, fileControl->compressionType->value);
    }
    else {
      ei_x_encode_atom(&b, "undefined");
    }
  }
  else {
    ei_x_encode_atom(&b, "undefined");
  }

  if (streamControl != NULL) {
    ei_x_encode_tuple_header(&b, 3);
    if (streamControl->compressionType != NULL) {
      ei_x_encode_ulong(&b, streamControl->compressionType->value);
    }
    else {
      ei_x_encode_atom(&b, "undefined");
    }
    printf("peiEventJob, streamControl->destinationPort: %d\n",
	   streamControl->destinationPort);
    ei_x_encode_string(&b, streamControl->destinationIpAddress);
    ei_x_encode_ulong(&b, streamControl->destinationPort);
  }
  else {
    ei_x_encode_atom(&b, "undefined");
  }

  sendCallback(b);

  QTRACE3(3, "callback invoked: eventJob, handle: %s, delivered message",
	       (char *)peiHandle);
}



void peiMEAttrUpdate(PeiHandleT peiHandle,
		     char *userLabel,
		     char *networkManagedElementId,
		     void *userData) {

  QTRACE3(3, "callback invoked: peiMEAttrUpdate, handle: %s",
	       (char *)peiHandle);
  printf("callback invoked: peiMEAttrUpdate, handle = %s\n", (char *)peiHandle);
  printf("userLabel = %s\n", userLabel);
  printf("networkManagedElementId = %s\n", networkManagedElementId);
  printf("userData = %s\n", (char *)userData);

  ei_x_buff b;
  ei_x_new(&b);
  if (userLabel == NULL && networkManagedElementId == NULL) {
    ei_x_format(&b, "{signal,{peiMEAttrUpdate,{~s,undefined,undefined}}}",
		(char *)peiHandle);
  } else if (userLabel == NULL) {
    ei_x_format(&b, "{signal,{peiMEAttrUpdate,{~s,undefined,~s}}}",
		(char *)peiHandle,
		networkManagedElementId);
  } else if (networkManagedElementId == NULL) {
    ei_x_format(&b, "{signal,{peiMEAttrUpdate,{~s,~s,undefined}}}",
		(char *)peiHandle,
		userLabel);
  } else {
    ei_x_format(&b, "{signal,{peiMEAttrUpdate,{~s,~s,~s}}}",
		(char *)peiHandle,
		userLabel,
		networkManagedElementId);
  }
  sendCallback(b);
}


ei_x_buff handlePeiInitialize(int nArgs,
			       ei_x_buff request,
			       ei_x_buff response) {


  int elementType;
  int elementSize;
  ei_get_type(request.buff, &request.index, &elementType,
	      &elementSize);

  printf("Element list type: %d\n", elementType);
  printf("Element list size: %d\n", elementSize);


  char *eventMap;

  if (elementType == ERL_ATOM_EXT) {
    request.index += elementSize + 2;
    eventMap = NULL;
  }
  else {
    int eventMapLength = stringLength(&request);
    if (eventMapLength < 0) {
      ei_x_format(
		  &response,
		  "{error, {~s, ~i}}",
		  "eventMap string length could not be determined",
		  -eventMapLength);
      return response;
    }
    eventMap = malloc(eventMapLength+1);
    ei_decode_string(request.buff, &request.index, eventMap);
    printf("eventMap: %s\n", eventMap);
    QTRACE3(3, "eventMap: %s", eventMap);
  }


  int cbSize;
  ei_decode_tuple_header(request.buff, &request.index, &cbSize);
  if (cbSize != 2) {
    ei_x_format(&response, "{error, ~s}", "invalid_cb_size");
    return response;
  }

  int peiEventJobCb;
  ei_decode_boolean(request.buff, &request.index, &peiEventJobCb);
  QTRACE3(3, "### i peiInitialize:peiEventJobCb  %d", peiEventJobCb);
  int peiMEAttrUpdateCb;
  ei_decode_boolean(request.buff, &request.index, &peiMEAttrUpdateCb);
  QTRACE3(3, "### i peiInitialize:peiMEAttrUpdateCb  %d", peiMEAttrUpdateCb);

  int userDataValueLength = stringLength(&request);
  if (userDataValueLength < 0) {
    ei_x_format(
		&response,
		"{error, {~s, ~i}}",
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
  printf("peiInitialize, userData: %s\n",  userDataValue);

  PeiEventJobCallbackSpecT peiEventJobCbSpec =
    {.eventJobCallback = &peiEventJob,
     .userData = userDataValue};
  PeiMEAttrUpdateCallbackSpecT peiMEAttrUpdateCbSpec =
    {.meAttrUpdateCallback = &peiMEAttrUpdate,
     .userData = userDataValue};

  PeiCallbacksT callbacks = {
    .eventJobCallbackSpec = &peiEventJobCbSpec,
    .meAttrUpdateCallbackSpec = &peiMEAttrUpdateCbSpec};

  printf("peiInitialize, peiEventJobCb: %d\n", peiEventJobCb);
  if (!peiEventJobCb)
    callbacks.eventJobCallbackSpec = NULL;
  printf("peiInitialize, peiMEAttrUpdateCb: %d\n", peiMEAttrUpdateCb);
  if (!peiMEAttrUpdateCb)
    callbacks.meAttrUpdateCallbackSpec = NULL;

  /* printf("peiInitialize, callbacks addr: %d\n",   */
  /* 	 (int)&callbacks);  */

  /* printf("peiInitialize, callbacks eventJob spec addr: %d\n",   */
  /* 	 (int)(callbacks.eventJobCallbackSpec));  */

  /* printf("peiInitialize, callbacks eventJob addr: %d\n",   */
  /* 	 (int)(callbacks.eventJobCallbackSpec->eventJobCallback));  */

  /* printf("peiInitialize, userData in eventJob: %s\n",   */
  /* 	 (char *)callbacks.eventJobCallbackSpec->userData);  */

  /* printf("peiInitialize, callbacks meAttrUpdate addr: %d\n",   */
  /* 	 (int)(callbacks.meAttrUpdateCallbackSpec->meAttrUpdateCallback));  */

  PeiHandleT peiHandle;
  PeiResultT r = peiInitialize(&peiHandle, eventMap, &callbacks);

  printf("Called peiInitialize, result: %d\n", (int) r);
  printf("peiInitialize Handle: %s\n", (char *)peiHandle);

  if (r != PEI_OK) {
    QTRACE_ERROR2("peiInitialize failed, status: %d", r);
    ei_x_format(&response, "{error, {~s, ~i}}", "peiInitialize failed", r);
  }
  else {
    ei_x_format(&response, "{ok, ~s}", (char *)peiHandle);
  }

  return response;
}


// void peiDispatcher(char *peiHandle) {
int peiDispatcher(char *peiHandle) {
  int r = peiDispatch(peiHandle);
  if (r != PEI_OK) {
    printf("peiDispatch failed, handle: %s, status: %d\n", peiHandle, r);
    QTRACE4(3, "peiDispatch failed, handle: %s, status: %d", peiHandle, r);
    return DISPATCH_FAILED;
  }
  else {
    printf("peiDispatch succeeded, handle: %s\n", peiHandle);
    QTRACE3(3, "peiDispatch succeeded, handle: %s", peiHandle);
    return DISPATCH_OK;
  }
}


ei_x_buff handlePeiSetupCallback(int nArgs, ei_x_buff request, ei_x_buff response) {

  if (nArgs != 1) {
    ei_x_format(&response, "{error, ~s}", "handlePeiSetupCallback, wrong number of args");
    return response;
  }

  int handleLength = stringLength(&request);
  if (handleLength < 0) {
    ei_x_format(
		&response,
		"{error, {~s, ~i}}",
		"handle length could not be determined",
		-handleLength);
    return response;
  }
  char *handle = malloc(handleLength+1);

  ei_decode_string(request.buff, &request.index, handle);
  QTRACE3(3, "peiSetupCallback, handle: %s", handle);

  PeiSelectionObjectT selectionObject;
  PeiResultT result = peiSelectionObjectGet((PeiHandleT)handle,
					      &selectionObject);
  if (result != PEI_OK) {
    QTRACE_ERROR2("peiSelectionObjectGet failed, status: %d", result);
    free(handle);
    ei_x_format(&response, "{error, {~s, ~i}}", "peiSelectionObjectGet failed",
		result);
  }
  else {
    QTRACE4(3, "peiSelectionObjectGet, handle: %s, fd: %d", handle,
		 (int)selectionObject);
    addPollScheduleItem(handle, (int)selectionObject, (DispatcherT)&peiDispatcher);
    ei_x_format(&response, "{ok, ~i}", (int)selectionObject);
  }
  return response;
}


ei_x_buff handlePeiFinalize(int nArgs, ei_x_buff request, ei_x_buff response) {

  if (nArgs != 1) {
    ei_x_format(&response, "{error, ~s}",
		"handlePeiFinalize, wrong number of args");
    return response;
  }

  int handleLength = stringLength(&request);
  if (handleLength < 0) {
    ei_x_format(
		&response,
		"{error, {~s, ~i}}",
		"handle length could not be determined",
		-handleLength);
    return response;
  }
  char handle[handleLength+1];
  ei_decode_string(request.buff, &request.index, handle);
  QTRACE3(3, "peiFinalize, handle: %s", handle);

  removePollScheduleItem(handle);

  PeiResultT r = peiFinalize((PeiHandleT)handle);

  if (r != PEI_OK) {
    QTRACE_ERROR2("peiFinalize failed, status: %d", r);
    ei_x_format(&response, "{error, {~s, ~i}}", "peiFinalize failed", r);
  }
  else {
    QTRACE2(3, "peiFinalize succeeded");
    ei_x_format(&response, "{ok}");
  }
  return response;
}


ei_x_buff handlePeiEmulDeath(int nArgs, ei_x_buff request, ei_x_buff response)
{

  if (nArgs != 2) {
    ei_x_format(&response, "{error, ~s}",
		"handlePeiEmulDeath, wrong number of args");
    return response;
  }

  int handleLength = stringLength(&request);
  if (handleLength < 0) {
    ei_x_format(
		&response,
		"{error, {~s, ~i}}",
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


char *decodePeiFunction(int code) {
  if (code == IFT_PEI_INITIALIZE) {
    return "peiInitialize";
  }
  else if (code == IFT_PEI_EVENT_JOB) {
    return "peiEventJob";
  }
  else if (code == IFT_PEI_SETUP_CALLBACK) {
    return "peiSetupCallback";
  }
  else if (code == IFT_PEI_ME_ATTR_UPDATE) {
    return "peiMEAttrUpdate";
  }
  else if (code == IFT_PEI_FINALIZE) {
    return "peiFinalize";
  }
  else if (code == IFT_PEI_EMUL_DEATH) {
    return "peiEmulDeath";
  }
  else {
    QTRACE_ERROR2("test suite error: unknown function code: %d", code);
    return "unknown function";
  }
}

/**
 * Handle a call from the test suite towards the CS.
 */
ei_x_buff send_sig_pei(int function, ei_x_buff request) {
  QTRACE3(3, "send_sig_pei, function: %s", decodePeiFunction(function));

  QINFO2("TESTAPP pei function %i\n", function);
  int nArgs;
  ei_decode_tuple_header(request.buff, &request.index, &nArgs);

  ei_x_buff response;
  ei_x_new(&response); // TODO, where is the char buffer deallocated?

  switch (function) {
  case IFT_PEI_INITIALIZE:
    return handlePeiInitialize(nArgs, request, response);
  case IFT_PEI_SETUP_CALLBACK:
    return handlePeiSetupCallback(nArgs, request, response);
  case IFT_PEI_FINALIZE:
    return handlePeiFinalize(nArgs, request, response);
  case IFT_PEI_EMUL_DEATH:
    QINFO1("TESTAPP pei emul death\n");
    return handlePeiEmulDeath(nArgs, request, response);
  default:
    QINFO1("TESTAPP pei unknown function\n");
    QTRACE_ERROR2("unknown function: %d", function);
    ei_x_format(&response, "{error, ~s}", "unknown function");
    return response;
  }
}

