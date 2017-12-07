/* ----------------------------------------------------------------------
 * %CCaseFile:	test_pmi.c %
 * %CCaseRev:	/main/R2A/R3A/R4A/R5A/2 %
 * %CCaseDate:	2015-12-02 %
 * %CCaseDocNo: %
 * Author:	erarafo
 * Author: <name>, <e-mail address>
 *
 * Short description:
 * <Some rows here>
 * ----------------------------------------------------------------------
 * %CCaseTemplateFile:	template.c %
 * %CCaseTemplateId: CCver: /main/1 %
 *
 * %CCaseCopyrightBegin%
 * Copyright (c) Ericsson AB 2013-2015 All rights reserved.
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
 * R2A/1      2013-02-12 erarafo     Created
 * R2A/6      2013-04-08 erarafo     Support for 64-bit measurement values
 * R2A/7      2013-04-09 erarafo     Fixed TRACE, free memory in handleData()
 * R2A/8      2013-04-24 erarafo     Handle measurement values in range 0..255
 * R2A/9      2013-04-24 erarafo     Fixed compiler warning
 * R2A/10     2013-04-25 erarafo     Duplication of code eliminated
 * R2A/11     2013-05-02 erarafo     Corrected handling of negative pmiData
 * R2A/12     2013-05-02 erarafo     Coding of long long constant (cosmetic)
 * R2A/13     2013-11-07 erarafo     Functions moved to test_common
 * R2A/14     2014-01-20 erarafo     Adapted to 'const char *' style
 * R2A/15     2014-04-17 uabesvi     Added show counters
 * R4A/1      2015-11-25 erarafo     Using wrappers around TRI trace macros
 * R4A/2      2015-11-26 erarafo     Comment only
 * R5A/1      2015-12-02 erarafo     Tracing of function calls
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
#include "pms_pmi.h"

#include <stdio.h>
#include <sys/prctl.h>
#include <limits.h>


#define PMI_INITIALIZE          1
#define PMI_FINALIZE            2
#define PMI_DATA                3
#define PMI_SETUP_CALLBACK      4
#define PMI_EMUL_DEATH          5
#define PMI_INITIALIZE_2        6
#define PMI_DATA_SHOW_COUNTERS  7


extern bool TRI_trace_enabled;


/* static void traceInt(char *msg, int k) { */
/*   FILE *f = fopen("/tmp/test_pmi.txt", "a"); */
/*   fprintf(f, "%s  %d\n", msg, k); */
/*   fclose(f); */
/* } */

/* static void traceNl() { */
/*   FILE *f = fopen("/tmp/test_pmi.txt", "a"); */
/*   fprintf(f, "\n"); */
/*   fclose(f); */
/* } */
/* static void traceStr(char *msg, char *s) { */
/*   FILE *f = fopen("/tmp/test_pmi.txt", "a"); */
/*   fprintf(f, "%s  %s\n", msg, s); */
/*   fclose(f); */
/* } */

/* static void traceC(char s) { */
/*   FILE *f = fopen("/tmp/test_pmi.txt", "a"); */
/*   fprintf(f, "%x ", s); */
/*   fclose(f); */
/* } */

/**
 * Set the given environment variable to a new value. If an old
 * value exists, return a pointer to a buffer containing the
 * old value, or NULL if there is no old value. The returned
 * buffer should be freed when no longer needed.
 *
 * If an error occurred it will be logged and NULL is returned.
 */
char * setEnvironmentVariable(char *key, char *value) {

	char *oldValue = NULL;

	if (getenv(key) != NULL) {
	  if (asprintf(&oldValue, "%s", getenv(key)) < 0) {
	    QTRACE_ERROR1("failed to allocate buffer for old environment variable value");
	    return NULL;
	  }
	}

	if (setenv(key, value, 1) != 0) {
	  QTRACE_ERROR3("failed to set environment variable, key: %s, value: %s",
	      key, value);
	  return NULL;
	}

	QTRACE5(3, "environment variable was set, key: %s, old value: %s, new value: %s",
	    key, (oldValue == NULL? "NULL" : oldValue), value);

	return oldValue;
}

/**
 * Restores the given environment variable to the
 * given old value. It is trusted that the given
 * old value is not NULL. The old value buffer
 * is freed.
 */
void restoreEnvironmentVariable(char *key, char *oldValue) {
	if (setenv(key, oldValue, 1) != 0) {
		QTRACE_ERROR2("failed to restore environment variable, key: %s", key);
	}
	free(oldValue);
}

/**
 * Returns the space allocation needed for a stringlist,
 * including terminating nulls. Arity is the number of
 * list elements.
 *
 * In case of undecodeable data -1 is returned. Callers
 * must check against this.
 */
int stringListSize(ei_x_buff *buffer, int arity) {
	ei_x_buff b = {buffer->buff, buffer->buffsz, buffer->index};

	int result = 0;
	for (int j = 0; j < arity; j++) {
	  int stringLen = stringLength(&b);
	  if (stringLen < 0) {
	    return -1;
	  }
		result += stringLen + 1;
		ei_decode_string(b.buff, &(b.index), NULL);
	}
	return result;
}

/**
 * Returns the size of a null-terminated array of
 * pointers.
 */
int arraySize(void **x) {
	int result = 0;
	for (void **p = x; *p != NULL; p++) {
		result++;
	}
	return result;
}


void subscribe(PmiHandleT handle, unsigned int gp, CounterSpecT **counterSpecs) {

	QTRACE3(3, "callback invoked: subscribe, handle: %s", (char *)handle);

	//ei_x_format(&b, "{signal, {subscribe, {~s, ~i, [{~s, [~s, ~s, ...]}, ...]}}}",
	//		(char *)handle,
	//		GP,
	//		counterSpecs);

	ei_x_buff b;
	ei_x_new(&b);
	ei_x_encode_version(&b);

	ei_x_encode_tuple_header(&b, 2);
	ei_x_encode_atom(&b, "signal");

	ei_x_encode_tuple_header(&b, 2);
	ei_x_encode_atom(&b, "subscribe");

	ei_x_encode_tuple_header(&b, 3);
	ei_x_encode_string(&b, handle);
	ei_x_encode_ulong(&b, gp);

	int nGroups = arraySize((void **)counterSpecs);
	ei_x_encode_list_header(&b, nGroups);

	for (int j = 0; j < nGroups; j++) {
		ei_x_encode_tuple_header(&b, 2);
		ei_x_encode_string(&b, counterSpecs[j]->pmGroup);

		const char **types = counterSpecs[j]->measurementTypes;
		int nTypes = arraySize((void **)types);

		ei_x_encode_list_header(&b, nTypes);
		for (int k = 0; k < nTypes; k++) {
			ei_x_encode_string(&b, types[k]);
		}
		ei_x_encode_empty_list(&b);
	}

	ei_x_encode_empty_list(&b);

	sendCallback(b);

	QTRACE3(3, "callback invoked: subscribe, handle: %s, delivered message", (char *)handle);
}



void report(PmiHandleT handle, unsigned int gp, time_t timeSpec, time_t deadline) {

	QTRACE3(3, "callback invoked: report, handle: %s", (char *)handle);

	ei_x_buff b;
	ei_x_new(&b);
	ei_x_format(&b, "{signal, {report, {~s, ~i, ~i, ~i}}}",
			(char *)handle,
			gp,
			timeSpec,
			deadline);

	sendCallback(b);
}


void showCounters(PmiHandleT handle,
		  const unsigned int requestId,
		  const char *ldn,
		  const unsigned int maxResponseTime) {

  QTRACE3(3, "callback invoked: report, handle: %s", (char *)handle);

  ei_x_buff b;
  ei_x_new(&b);

  ei_x_format(&b, "{signal, {showCounters, {~s, ~i, ~s, ~i}}}",
	      (char *)handle,
	      requestId,
	      ldn,
	      maxResponseTime);


  sendCallback(b);
}


ei_x_buff handleInitialize(int nArgs, ei_x_buff request, ei_x_buff response) {

	if (nArgs != 3) {
		ei_x_format(&response, "{error, ~s}", "handleInitialize, wrong number of args");
		return response;
	}

	char *cecHostKey = "CEC_HOST";

	// decode CEC_HOST value, it is passed as a string
  int cecHostValueLength = stringLength(&request);
  if (cecHostValueLength < 0) {
    ei_x_format(
        &response,
        "{error, ~s, ~i}",
        "cec host string length could not be determined",
        -cecHostValueLength);
    return response;
  }
	char cecHostValue[cecHostValueLength+1];
	if (cecHostValueLength == 0) {
	  ei_skip_term(request.buff, &request.index);
	  strcpy(cecHostValue, "");
	}
	else {
	  ei_decode_string(request.buff, &request.index, cecHostValue);
	}

	QTRACE3(3, "got CEC host: %s\n", cecHostValue);

	char *cecPortKey = "CEC_PORT";

	// decode CEC_PORT value, it is passed as a string
	int cecPortValueLength = stringLength(&request);
	if (cecPortValueLength < 0) {
    ei_x_format(
        &response,
        "{error, ~s, ~i}",
        "cec port string length could not be determined",
        -cecPortValueLength);
    return response;
	}
	char cecPortValue[cecPortValueLength+1];
	ei_decode_string(request.buff, &request.index, cecPortValue);
	QTRACE3(3, "got CEC port: %s\n", cecPortValue);

	int nGroups;
	ei_decode_list_header(request.buff, &request.index, &nGroups);

	const char *groups[nGroups+1];
	groups[nGroups] = NULL;

	int stringSpaceSize = stringListSize(&request, nGroups);
	if (stringSpaceSize == -1) {
    ei_x_format(&response, "{error, ~s}", "PM groups is not a well-formed string list");
    return response;
	}
	char stringSpace[stringSpaceSize];
	char *p = stringSpace;

	for (int j = 0; j < nGroups; j++) {
		ei_decode_string(request.buff, &request.index, p);
		QTRACE3(3, "group: %s", p);
		groups[j] = p;
		p += strlen(p) + 1;
	}

	ei_skip_term(request.buff, &request.index);  // end of list


	// change CEC_HOST and CEC_PORT temporarily to the values
	// passed in the request, then restore the previous values.

	char *oldCecHostValue = NULL;
	if (strlen(cecHostValue) > 0) {
		oldCecHostValue = setEnvironmentVariable(cecHostKey, cecHostValue);
	}
	char *oldCecPortValue = NULL;
	if (strcmp(cecPortValue, "0") != 0) {
		oldCecPortValue = setEnvironmentVariable(cecPortKey, cecPortValue);
	}

	PmiCallbacksT callbacks = {&subscribe, &report};
	PmiHandleT handle;
	//PmiResultT r = pmiInitialize(&handle, &callbacks, groups);
	DRCALL(PmiResultT, r,
	    pmiInitialize(&handle, &callbacks, groups),
	    "%s", "pmiInitialize");

	if (oldCecPortValue != NULL) {
		restoreEnvironmentVariable(cecPortKey, oldCecPortValue);
	}
	if (oldCecHostValue != NULL) {
		restoreEnvironmentVariable(cecHostKey, oldCecHostValue);
	}

	if (r != PMI_OK) {
		QTRACE_ERROR2("pmiInitialize failed, status: %d", r);
		ei_x_format(&response, "{error, ~s, ~i}", "pmiInitialize failed", r);
	}
	else {
		ei_x_format(&response, "{ok, ~s}", (char *)handle);
	}

	return response;
}

ei_x_buff handleInitialize_2(int nArgs, ei_x_buff request, ei_x_buff response) {

  QINFO1("test_pmi handleInitialize_2\n");
  if (nArgs != 4) {
    ei_x_format(&response, "{error, ~s}", "handleInitialize_2, wrong number of args");
    return response;
  }

  char *cecHostKey = "CEC_HOST";

  // decode CEC_HOST value, it is passed as a string
  int cecHostValueLength = stringLength(&request);
  if (cecHostValueLength < 0) {
    ei_x_format(
		&response,
		"{error, ~s, ~i}",
		"cec host string length could not be determined",
		-cecHostValueLength);
    return response;
  }
  char cecHostValue[cecHostValueLength+1];
  if (cecHostValueLength == 0) {
    ei_skip_term(request.buff, &request.index);
    strcpy(cecHostValue, "");
  }
  else {
    ei_decode_string(request.buff, &request.index, cecHostValue);
  }

  QTRACE3(3, "got CEC host: %s\n", cecHostValue);

  char *cecPortKey = "CEC_PORT";

  // decode CEC_PORT value, it is passed as a string
  int cecPortValueLength = stringLength(&request);
  if (cecPortValueLength < 0) {
    ei_x_format(
		&response,
		"{error, ~s, ~i}",
		"cec port string length could not be determined",
		-cecPortValueLength);
    return response;
  }
  char cecPortValue[cecPortValueLength+1];
  ei_decode_string(request.buff, &request.index, cecPortValue);
  QTRACE3(3, "got CEC port: %s\n", cecPortValue);


  int topMoLdnLength = stringLength(&request);
  if (topMoLdnLength < 0) {
    ei_x_format(&response,
		"{error, ~s, ~i}",
		"topMoLdn string length could not be determined",
		-topMoLdnLength);
    return response;
  }
  char topMoLdn[topMoLdnLength+1];
  ei_decode_string(request.buff, &request.index, topMoLdn);
  QTRACE3(3, "got topMoLdn: %s\n", topMoLdn);


  int nGroups;
  ei_decode_list_header(request.buff, &request.index, &nGroups);


  const char *groups[nGroups+1];
  groups[nGroups] = NULL;

  int stringSpaceSize = stringListSize(&request, nGroups);
  if (stringSpaceSize == -1) {
    ei_x_format(&response, "{error, ~s}", "## PM groups is not a well-formed string list");
    return response;
  }
  char stringSpace[stringSpaceSize];
  char *p = stringSpace;

  for (int j = 0; j < nGroups; j++) {
    ei_decode_string(request.buff, &request.index, p);
    QTRACE3(3, "group: %s", p);
    groups[j] = p;
    p += strlen(p) + 1;
  }

  ei_skip_term(request.buff, &request.index);  // end of list


  // change CEC_HOST and CEC_PORT temporarily to the values
  // passed in the request, then restore the previous values.

  char *oldCecHostValue = NULL;
  if (strlen(cecHostValue) > 0) {
    oldCecHostValue = setEnvironmentVariable(cecHostKey, cecHostValue);
  }
  char *oldCecPortValue = NULL;
  if (strcmp(cecPortValue, "0") != 0) {
    oldCecPortValue = setEnvironmentVariable(cecPortKey, cecPortValue);
  }

  PmiCallbacksT_2 callbacks = {&subscribe, &report, &showCounters};
  PmiHandleT handle;
  // PmiResultT r = pmiInitialize_2(&handle, &callbacks, groups, topMoLdn);
  DRCALL(PmiResultT, r,
      pmiInitialize_2(&handle, &callbacks, groups, topMoLdn),
      "%s", "pmiInitialize_2");

  if (oldCecPortValue != NULL) {
    restoreEnvironmentVariable(cecPortKey, oldCecPortValue);
  }
  if (oldCecHostValue != NULL) {
    restoreEnvironmentVariable(cecHostKey, oldCecHostValue);
  }

  if (r != PMI_OK) {
    QTRACE_ERROR2("pmiInitialize_2 failed, status: %d", r);
    ei_x_format(&response, "{error, ~s, ~i}", "pmiInitialize_2 failed", r);
  }
  else {
    ei_x_format(&response, "{ok, ~s}", (char *)handle);
  }

  return response;
}


// void dispatcher(char *handle) {
int dispatcher(char *handle) {
  // int r = pmiDispatch(handle);
  DRCALL(int, r,
      pmiDispatch(handle),
      "%s", "pmiDispatch");
  if (r != PMI_OK) {
    QTRACE4(3, "pmiDispatch failed, handle: %s, status: %d", handle, r);
    return DISPATCH_FAILED;
  }
  else {
    QTRACE3(3, "pmiDispatch succeeded, handle: %s", handle);
    return DISPATCH_OK;
  }
}

ei_x_buff handleSetupCallback(int nArgs, ei_x_buff request, ei_x_buff response) {

  if (nArgs != 1) {
    ei_x_format(&response, "{error, ~s}", "handleSetupCallback, wrong number of args");
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

  PmiSelectionObjectT selectionObject;
  // PmiResultT result = pmiSelectionObjectGet((PmiHandleT)handle, &selectionObject);
  DRCALL(PmiResultT, result,
      pmiSelectionObjectGet((PmiHandleT)handle, &selectionObject),
      "%s", "pmiSelectionObjectGet");
  if (result != PMI_OK) {
    QTRACE_ERROR2("pmiSelectionObjectGet failed, status: %d", result);
    free(handle);
    ei_x_format(&response, "{error, ~s, ~i}", "pmiSelectionObjectGet failed", result);
  }
  else {
    QTRACE4(3, "pmiSelectionObjectGet, handle: %s, fd: %d", handle, (int)selectionObject);
    addPollScheduleItem(handle, (int)selectionObject, (DispatcherT)&dispatcher);
    ei_x_format(&response, "{ok, ~i}", (int)selectionObject);
  }
  return response;
}


ei_x_buff handleData(int nArgs, ei_x_buff request, ei_x_buff response) {


	if (nArgs != 5) {
		ei_x_format(&response, "{error, ~s}", "handleData, wrong number of args");
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
	QTRACE3(3, "pmiData, handle: %s", handle);

	// get GP
	unsigned long gp;
	ei_decode_ulong(request.buff, &request.index, &gp);
	QTRACE3(3, "pmiData, gp: %d", (int)gp);

	// get timeSpec
	long long timeSpecL = decodeInteger(&request);
	if (timeSpecL >= TWO_TO_31) {
	  ei_x_format(
	      &response,
	      "{error, ~s, ~i}",
	      "unknown type for timespec, code: ",
	      (long)(timeSpecL - TWO_TO_31));
	  return response;
	}
	time_t timeSpec = (time_t) timeSpecL;
	QTRACE3(3, "pmiData, timeSpec: %d", (int)timeSpec);

	// get measObjLdn
	int measObjLdnLength = stringLength(&request);
	if (measObjLdnLength < 0) {
	  ei_x_format(
		      &response,
		      "{error, ~s, ~i}",
		      "measObjLdn length could not be determined",
		      -measObjLdnLength);
	  return response;
	}

	char measObjLdn[measObjLdnLength+1];
	ei_decode_string(request.buff, &request.index, measObjLdn);
	QTRACE3(3, "pmiData, measObjLdn: %s", measObjLdn);

	// get the data; number of groups
	int nGroups;
	ei_decode_list_header(request.buff, &request.index, &nGroups);
	QTRACE3(3, "  n. of groups: %d", nGroups);

	ValueBundleT *groups[nGroups+1];
	groups[nGroups] = NULL;
	for (int j = 0; j < nGroups; j++) {
	  groups[j] = (ValueBundleT *)malloc(sizeof(ValueBundleT));

	  int groupArity;
	  ei_decode_tuple_header(request.buff, &request.index, &groupArity);
	  if (groupArity != 2) {
	    ei_x_format(&response, "{error, ~s}", "unexpected group arity");
	    return response;
	  }

	  int groupNameLength = stringLength(&request);
	  if (groupNameLength < 0) {
	    ei_x_format(&response,
			"{error, ~s, ~i}",
			"group name length could not be determined",
			-groupNameLength);
	    return response;
	  }
	  char *groupName = (char *)malloc(groupNameLength + 1);
	  ei_decode_string(request.buff, &request.index, groupName);
	  QTRACE3(3, "  group name: %s", groupName);
	  groups[j]->pmGroup = groupName;

	  int nTypes;
	  ei_decode_list_header(request.buff, &request.index, &nTypes);
	  QTRACE4(3, "  group: %s, n. of types: %d", groupName, nTypes);

	  MeasurementValueT **types = (MeasurementValueT **)calloc(nTypes + 1, sizeof(MeasurementValueT *));
	  // trust that the area is zeroed!

	  groups[j]->measurementValues = types;

	  for (int k = 0; k < nTypes; k++) {
	    types[k] = (MeasurementValueT *)malloc(sizeof(MeasurementValueT));

	    int expect3;
	    ei_decode_tuple_header(request.buff, &request.index, &expect3);
	    if (expect3 != 3) {
	      ei_x_format(&response, "{error, ~s}", "unexpected tuple arity");
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
	    char *typeName = (char *)malloc(typeNameLength + 1);
	    ei_decode_string(request.buff, &request.index, typeName);
	    QTRACE3(3, "    type name: %s", typeName);
	    types[k]->measurementType = typeName;

	    long long multL = decodeInteger(&request);
	    if (multL >= TWO_TO_31) {
	      ei_x_format(
			  &response,
			  "{error, ~s, ~i}",
			  "unknown type for multiplicity, code:",
			  (long)(multL - TWO_TO_31));
	      return response;
	    }
	    long mult = (long)multL;
	    QTRACE4(3, "    type name: %s, multiplicity: %d", typeName, mult);


	    types[k]->multiplicity = mult;

	    int expectM;
	    ei_decode_tuple_header(request.buff, &request.index, &expectM);
	    if (expectM != mult) {
	      ei_x_format(&response, "{error, ~s}", "unexpected arity of measurement value elements");
	      return response;
	    }

	    long long *elements = (long long *)calloc(mult, sizeof(long long));
	    long long *q = elements;
	    /* types[k]->elements = (long long *)calloc(mult, sizeof(long long)); */
	    /* long long *q = types[k]->elements; */
	    for (int m = 0; m < mult; m++, q++) {

	      int elementType;
	      int elementSize;
	      ei_get_type(request.buff, &request.index, &elementType, &elementSize);

	      if (elementType == ERL_SMALL_BIG_EXT) {
		// 64-bit signed integer
		ei_decode_longlong(request.buff, &request.index, q);
		QTRACE2(3, "      element is 64-bit signed integer, cannot display");
	      }
	      else {
		long long elementValueL = decodeInteger(&request);
		if (elementValueL >= TWO_TO_31) {
		  ei_x_format(
			      &response,
			      "{error, ~s, ~i}",
			      "measurement elementValue of unknown type, code: ",
			      (long)(elementValueL - TWO_TO_31));
		  return response;
		}
		else {
		  *q = elementValueL;
		}
		types[k]->elements = elements;
	      }
	    }
	  }
	  ei_skip_term(request.buff, &request.index);  // end of types in group
	}

	ei_skip_term(request.buff, &request.index);  // end of bundles

	// PmiResultT result = pmiData((PmiHandleT)handle, gp, timeSpec, measObjLdn, groups);
	DRCALL(PmiResultT, result,
	    pmiData((PmiHandleT)handle, gp, timeSpec, measObjLdn, groups),
	    "%s", "pmiData");
	if (result != PMI_OK) {
	  QTRACE_ERROR2("pmiData failed, status: %d", result);
	  ei_x_format(&response, "{error, ~s, ~i, ~s}", "pmiData failed", result, handle);
	}
	else {
	  QTRACE2(3, "pmiData succeeded");
	  ei_x_format(&response, "{ok}");
	}

	for (int j = 0; j < nGroups; j++) {
	  free((void *)(groups[j]->pmGroup));
	  MeasurementValueT **t = groups[j]->measurementValues;
	  for (MeasurementValueT **p = t; *p != NULL; p++) {
	    free((void *)(*p)->measurementType);
	    free((void *)(*p)->elements);
	    free(*p);
	  }
	  free(t);
	  free(groups[j]);
	}

	return response;
}





ei_x_buff handleDataShowCounters(int nArgs, ei_x_buff request, ei_x_buff response) {

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
  unsigned long requestId;
  ei_decode_ulong(request.buff, &request.index, &requestId);
  QTRACE3(3, "pmiDataShowCounters, requestId: %d", (int)requestId);

  // get RequestId
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
  QTRACE3(3, "pmiData, errorString: %s", errorString);

  QINFO2("pmiData, errorString: %s", errorString);


  // get the data; number of MTs
  int nTypes;
  ei_decode_list_header(request.buff, &request.index, &nTypes);
  QTRACE3(3, "  n. of groups: %d", nTypes);

  /*  int nTypes; */
  /*   ei_decode_list_header(request.buff, &request.index, &nTypes); */
  /*   QTRACE3(3, "  no. of types: %d", nTypes); */

  MeasurementValueT **types = (MeasurementValueT **)calloc(nTypes + 1, sizeof(MeasurementValueT *));
  // trust that the area is zeroed!


  for (int k = 0; k < nTypes; k++) {
    types[k] = (MeasurementValueT *)malloc(sizeof(MeasurementValueT));

    int expect3;
    ei_decode_tuple_header(request.buff, &request.index, &expect3);

    if (expect3 != 3) {
      ei_x_format(&response, "{error, ~s}", "unexpected tuple arity");
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

    char *typeName = (char *)malloc(typeNameLength + 1);
    ei_decode_string(request.buff, &request.index, typeName);
    QTRACE3(3, "    type name: %s", typeName);
    types[k]->measurementType = typeName;

    long long multL = decodeInteger(&request);
    if (multL >= TWO_TO_31) {
      ei_x_format(
		  &response,
		  "{error, ~s, ~i}",
		  "unknown type for multiplicity, code:",
		  (long)(multL - TWO_TO_31));
      return response;
    }
    long mult = (long)multL;
    QTRACE4(3, "    type name: %s, multiplicity: %d", typeName, mult);

    types[k]->multiplicity = mult;

    int expectM;
    ei_decode_tuple_header(request.buff, &request.index, &expectM);

    if (expectM != mult) {
      ei_x_format(&response, "{error, ~s}", "unexpected arity of measurement value elements");
      return response;
    }

    long long *elements = (long long *)calloc(mult, sizeof(long long));
    long long *q = elements;
    /* types[k]->elements = (long long *)calloc(mult, sizeof(long long)); */
    /* long long *q = types[k]->elements; */
    for (int m = 0; m < mult; m++, q++) {

      int elementType;
      int elementSize;
      ei_get_type(request.buff, &request.index, &elementType, &elementSize);

      if (elementType == ERL_SMALL_BIG_EXT) {
	// 64-bit signed integer
	ei_decode_longlong(request.buff, &request.index, q);
	QTRACE2(3, "      element is 64-bit signed integer, cannot display");
      }
      else {
	long long elementValueL = decodeInteger(&request);
	if (elementValueL >= TWO_TO_31) {
	  ei_x_format(
		      &response,
		      "{error, ~s, ~i}",
		      "measurement elementValue of unknown type, code: ",
		      (long)(elementValueL - TWO_TO_31));
	  return response;
	}
	else {
	  *q = elementValueL;
	}
	types[k]->elements = elements;
      }
    }
  }

  ei_skip_term(request.buff, &request.index);  // end of bundles

  PmiResultT pmiResult;
  if (errorStringLength > 1) {
    // pmiResult = pmiDataShowCounters((PmiHandleT)handle, requestId, result, errorString, types);
    RCALL(pmiResult,
        pmiDataShowCounters((PmiHandleT)handle, requestId, result, errorString, types),
        "%s", "pmiDataShowCounters (1)");
  }
  else {
    // pmiResult = pmiDataShowCounters((PmiHandleT)handle, requestId, result, NULL, types);
    RCALL(pmiResult,
        pmiDataShowCounters((PmiHandleT)handle, requestId, result, NULL, types),
        "%s", "pmiDataShowCounters (2)");
  }

  if (pmiResult != PMI_OK) {
    QTRACE_ERROR2("pmiDataShowCounters failed, status: %d", pmiResult);
    ei_x_format(&response, "{error, ~s, ~i, ~s}", "pmiDataShowCounters failed", pmiResult, handle);
  }
  else {
    QTRACE2(3, "pmiDataShowCounters succeeded");
    ei_x_format(&response, "{ok}");
  }

  MeasurementValueT **t = types;
  for (MeasurementValueT **p = t; *p != NULL; p++) {
    free((void *)(*p)->measurementType);
    free((void *)(*p)->elements);
    free(*p);
  }
  free(t);

  return response;
}


ei_x_buff handleFinalize(int nArgs, ei_x_buff request, ei_x_buff response) {

	if (nArgs != 1) {
		ei_x_format(&response, "{error, ~s}", "handleFinalize, wrong number of args");
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

	// PmiResultT r = pmiFinalize((PmiHandleT)handle);
	DRCALL(PmiResultT, r,
	    pmiFinalize((PmiHandleT)handle),
	    "%s", "pmiFinalize");

	if (r != PMI_OK) {
		QTRACE_ERROR2("pmiFinalize failed, status: %d", r);
		ei_x_format(&response, "{error, ~s, ~i}", "pmiFinalize failed", r);
	}
	else {
		QTRACE2(3, "pmiFinalize succeeded");
		ei_x_format(&response, "{ok}");
	}
	return response;
}


ei_x_buff handleEmulDeath(int nArgs, ei_x_buff request, ei_x_buff response) {

  if (nArgs != 2) {
    ei_x_format(&response, "{error, ~s}", "handleEmulDeath, wrong number of args");
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


char *decodeFunction(int code) {
	if (code == PMI_INITIALIZE) {
		return "pmiInitialize";
	}
	else if (code == PMI_INITIALIZE_2) {
		return "pmiInitialize_2";
	}
	else if (code == PMI_SETUP_CALLBACK) {
		return "setupCallback";
	}
	else if (code == PMI_DATA) {
		return "pmiData";
	}
	else if (code == PMI_DATA_SHOW_COUNTERS) {
		return "pmiDataShowCounters";
	}
	else if (code == PMI_FINALIZE) {
	  return "pmiFinalize";
	}
	else if (code == PMI_EMUL_DEATH) {
	  return "pmiEmulDeath";
	}
	else {
		QTRACE_ERROR2("test suite error: unknown function code: %d", code);
		return "unknown function";
	}
}

/**
 * Handle a call from the test suite towards the CS.
 */
ei_x_buff send_sig_pmi(int function, ei_x_buff request) {
	QTRACE3(3, "send_sig_pmi, function: %s", decodeFunction(function));

	QINFO2("TESTAPP pmi function %i\n", function);
	int nArgs;
	ei_decode_tuple_header(request.buff, &request.index, &nArgs);

	ei_x_buff response;
	ei_x_new(&response); // TODO, where is the char buffer deallocated?

	switch (function) {
	case PMI_INITIALIZE:
		return handleInitialize(nArgs, request, response);
	case PMI_INITIALIZE_2:
		return handleInitialize_2(nArgs, request, response);
	case PMI_SETUP_CALLBACK:
		return handleSetupCallback(nArgs, request, response);
	case PMI_DATA:
		return handleData(nArgs, request, response);
	case PMI_DATA_SHOW_COUNTERS:
		return handleDataShowCounters(nArgs, request, response);
	case PMI_FINALIZE:
		return handleFinalize(nArgs, request, response);
	case PMI_EMUL_DEATH:
	  QINFO1("TESTAPP pmi emul death\n");
	  return handleEmulDeath(nArgs, request, response);
	default:
	  QINFO1("TESTAPP pmi unknown function\n");
	  QTRACE_ERROR2("unknown function: %d", function);
	  ei_x_format(&response, "{error, ~s}", "unknown function");
	  return response;
	}
}

