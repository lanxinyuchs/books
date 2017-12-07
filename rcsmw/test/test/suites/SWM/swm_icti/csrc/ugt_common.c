/* ----------------------------------------------------------------------
 * %CCaseFile:	ugt_common.c %
 * %CCaseRev:	/main/R2A/5 %
 * %CCaseDate:	2013-10-28 %
 * %CCaseDocNo: %
 * Author:	erarafo
 * Author: <name>, <e-mail address>
 *
 * Short description: Common functions.
 *
 * ----------------------------------------------------------------------
 * %CCaseTemplateFile:  template.c %
 * %CCaseTemplateId: CCver: /main/1 %
 *
 * %CCaseCopyrightBegin%
 * Copyright (c) Ericsson AB 2013 All rights reserved.
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
 * R2A/1      2013-08-26 erarafo     First version
 * R2A/2      2013-08-27 erarafo     Functionality added
 * R2A/3      2013-08-29 erarafo     Cleanup of getStringAttribute
 * R2A/4      2013-10-10 erarafo     Removed CPP legacy
 * R2A/5      2013-10-28 erarafo     Uplift to ICTI IWD PB1
 * ----------------------------------------------------------------------
 */

#include "ugt_common.h"


SaUint32T
getVersion(SafcImmCtSchemaVersionT_2 *v) {
  return v->version;
}

SaUint32T
getRelease(SafcImmCtSchemaVersionT_2 *v) {
  return v->release;
}

SaUint32T
getCorrection(SafcImmCtSchemaVersionT_2 *v) {
  return v->correction;
}


char
*saDecode(int r) {
  if (r == 1) {
    return "SA_AIS_OK";
  }
  else if (r == 7) {
    return "SA_AIS_ERR_INVALID_PARAM";
  }
  else if (r == 9) {
    return "SA_AIS_ERR_BAD_HANDLE";
  }
  else if (r == 12) {
    return "SA_AIS_ERR_NOT_EXIST";
  }
  else if (r == 17) {
    // this one is deprecated
    return "SA_AIS_ERR_NAME_NOT_FOUND";
  }
  else if (r == 31) {
    return "SA_AIS_ERR_UNAVAILABLE";
  }
  else {
    // not supposed to happen, don't bother freeing
    char *numeral = NULL;
    asprintf(&numeral, "%d", r);
    return numeral;
  }
}


// Assert that two SaUInt32 entities are equal.

bool
assertEqualsSaUInt32(
    SaUint32T expected,
    SaUint32T actual,
    FILE *log,
    const char *entityDescription) {
  if (expected != actual) {
    fprintf(log, "assertion fails: %s: expected: %u, actual: %u\n",
        entityDescription,
        expected,
        actual);
    fflush(log);
    return false;
  }
  else {
    return true;
  }
}


char *
stringConcat(const char *a, const char *b) {
  char *result;
  asprintf(&result, "%s%s", a, b);
  return result;
}


SaImmClassNameT *
oneClassName(const char *name) {
  SaImmClassNameT *result = (SaImmClassNameT *)calloc(1+1, sizeof(SaImmClassNameT));
  result[0] = (SaImmClassNameT)name;
  return result;
}



SaImmClassNameT *
twoClassNames(const char *name0, const char *name1) {
  SaImmClassNameT *result = (SaImmClassNameT *)calloc(2+1, sizeof(SaImmClassNameT));
  result[0] = (SaImmClassNameT)name0;
  result[1] = (SaImmClassNameT)name1;
  return result;
}

// Returns a pointer to a SaStringT (which is a pointer to pointer
// to char).

SaStringT *
unDeref(SaStringT s) {
  SaStringT *result = (SaStringT *)calloc(1+1, sizeof(SaStringT));
  result[0] = s;
  return result;
}



// Returns a SaInt32T attribute value. If no attribute matches
// the given attribute name then INT_MIN (-2147483648) is returned.

SaInt32T
getAttribute(SafcImmCtInstanceT *inst, const char *attrName) {
  for (SaImmAttrValuesT_2 **p = inst->attrValues; *p != NULL; p++) {
    if (STREQ((char *)(*p)->attrName, attrName) &&
        (*p)->attrValueType == SA_IMM_ATTR_SAINT32T &&
        (*p)->attrValuesNumber == 1) {
      SaImmAttrValueT *values = (*p)->attrValues;
      return *((SaInt32T *)values[0]);
    }
  }
  return INT_MIN;
}


SaStringT
getStringAttribute(SafcImmCtInstanceT *inst, const char *attrName) {
  for (SaImmAttrValuesT_2 **p = inst->attrValues; *p != NULL; p++) {
    if (STREQ((char *)(*p)->attrName, attrName) &&
        (*p)->attrValueType == SA_IMM_ATTR_SASTRINGT &&
        (*p)->attrValuesNumber == 1) {
      SaImmAttrValueT *values = (*p)->attrValues;
      return *((SaStringT *)values[0]);
    }
  }
  return "***AttributeNotFound***";
}



// Fully general constructor for single- or multivalued
// attribute

SaImmAttrValuesT_2 *
createAttrValue(
    SaImmAttrNameT attrName,
    SaImmValueTypeT type,
    SaUint32T mult,
    SaImmAttrValueT *values) {

  SaImmAttrValuesT_2 *result = malloc(sizeof(SaImmAttrValuesT_2));
  result->attrName = attrName;
  result->attrValueType = type;
  result->attrValuesNumber = mult;
  result->attrValues = values;
  return result;
}

// Takes a 'void *' value and puts it in a length-1 array.
// A pointer to the allocated array is returned.

void **
singletArray(void *x) {
  void **result = malloc(sizeof(void *));
  *result = x;
  return result;
}

// Takes two 'void *' values and puts them in a length-2 array.
// A pointer to the allocated array is returned.

void **
doubletArray(void *x, void *y) {
  void **result = calloc(2, sizeof(void *));
  *result = x;
  result++;
  *result = y;
  result --;
  return result;
}


// Returns a location that points to an array of
// char.

void *
wrapString(char *str) {
  char **result = calloc(1, sizeof(char *));
  *result = str;
  return (void *)result;
}


// Returns an RDN attribute. The given rdnName must
// match the class declaration.

SaImmAttrValuesT_2 *
rdn(SaImmAttrNameT rdnName, SaStringT rdnValue) {
  return createAttrValue(
      rdnName,
      SA_IMM_ATTR_SASTRINGT,
      1,
      singletArray(wrapString(rdnValue)));
}


// Returns the length of the given attribute list.

int
nAttrValues(SaImmAttrValuesT_2 **attrValues) {
  int i = 0;
  for (SaImmAttrValuesT_2 **p = attrValues; *p != NULL; p++) {
    i++;
  }
  return i;
}


// Returns an empty attribute list.

SaImmAttrValuesT_2 **
emptyAttrList() {
  SaImmAttrValuesT_2 **result = calloc(1, sizeof(SaImmAttrValuesT_2 *));
  return result;
}


// Returns a list containing one int32-valued attribute.

SaImmAttrValuesT_2 **
oneInt32Attr(SaImmAttrNameT name, SaInt32T value) {
  SaInt32T *z = calloc(1, sizeof(SaInt32T));
  *z = value;
  void *vz = (void *)z;

  SaImmAttrValuesT_2 **result = calloc(1+1, sizeof(SaImmAttrValuesT_2 *));
  SaImmAttrValuesT_2 **p = result;
  *p = createAttrValue(name, SA_IMM_ATTR_SAINT32T, 1, singletArray(vz));
  return result;
}


// Returns a list containing two int32-valued attributes, each
// having multiplicity 1.

SaImmAttrValuesT_2 **
twoInt32Attrs(
    SaImmAttrNameT name0, SaInt32T value0,
    SaImmAttrNameT name1, SaInt32T value1) {

  SaInt32T *u = calloc(2, sizeof(SaInt32T));
  *u = value0;
  void *v0 = (void *)u;

  u++;
  *u = value1;
  void *v1 = (void *)u;

  SaImmAttrValuesT_2 **result = calloc(2+1, sizeof(SaImmAttrValuesT_2 *));
  SaImmAttrValuesT_2 **p = result;
  *p = createAttrValue(name0, SA_IMM_ATTR_SAINT32T, 1, singletArray(v0));
  p++;
  *p = createAttrValue(name1, SA_IMM_ATTR_SAINT32T, 1, singletArray(v1));
  return result;
}

// Returns a list containing two simple string-valued attributes.

SaImmAttrValuesT_2 **
twoStringAttrs(SaImmAttrNameT name0, SaStringT value0, SaImmAttrNameT name1, SaStringT value1) {
  SaImmAttrValuesT_2 **result = calloc(2+1, sizeof(SaImmAttrValuesT_2 *));
  result[0] =
      createAttrValue(
          name0,
          SA_IMM_ATTR_SASTRINGT,
          1,
          singletArray(
              (SaImmAttrValueT)unDeref(value0)));
  result[1] =
      createAttrValue(
          name1,
          SA_IMM_ATTR_SASTRINGT,
          1,
          singletArray(
              (SaImmAttrValueT)unDeref(value1)));
  return result;

}


// Returns a list containing one string-valued attribute of
// multiplicity 2.

SaImmAttrValuesT_2 **
oneStrings2Attr(
    SaImmAttrNameT name, SaStringT value0, SaStringT value1) {

  SaStringT *u = calloc(2, sizeof(SaStringT));
  *u = value0;
  void *v0 = (void *)u;

  u++;
  *u = value1;
  void *v1 = (void *)u;

  SaImmAttrValuesT_2 **result = calloc(1+1, sizeof(SaImmAttrValuesT_2 *));
  SaImmAttrValuesT_2 **p = result;
  *p = createAttrValue(name, SA_IMM_ATTR_SASTRINGT, 2, doubletArray(v0, v1));

  return result;
}

SaImmAttrValuesT_2 **
appendAttr(SaImmAttrValuesT_2 ** attrs, SaImmAttrNameT attrName, SaInt32T attrValue) {

  SaImmAttrValuesT_2 **result = calloc(nAttrValues(attrs)+1+1, sizeof(SaImmAttrValuesT_2 *));

  SaImmAttrValuesT_2 **q = result;
  for (SaImmAttrValuesT_2 **p = attrs; *p != NULL; p++) {
    *q = *p;
    q++;
  }

  void *valueLoc = calloc(1, sizeof(SaInt32T));
  *((SaInt32T *)valueLoc) = attrValue;
  *q = createAttrValue(attrName, SA_IMM_ATTR_SAINT32T, 1, singletArray(valueLoc));

  return result;
}





// Returns the length of the given list.

int
nInstances(SafcImmCtInstanceT **insts) {
  int result = 0;
  for (SafcImmCtInstanceT **p = insts; *p != NULL; p++) {
    result++;
  }
  return result;
}



// Returns the length of the given list.

int
nInstGroups(SafcImmCtInstanceGroupT **groups) {
  int result = 0;
  for (SafcImmCtInstanceGroupT **p = groups; *p != NULL; p++) {
    result++;
  }
  return result;
}


// Returns a SaNameT containing the given text. Truncation
// to 256 characters is enforced if necessary.

SaNameT *
wrapName(char *name) {
  SaNameT *result = malloc(sizeof(SaNameT));
  size_t length = strlen(name);
  if (length > 256) {
    result->length = 256;
  }
  else {
    result->length = length;
  }
  char *p = name;
  for (unsigned int i = 0; i < length; i++) {
    result->value[i] = *p;
    p++;
  }
  return result;
}


// Returns an instance created from the given
// parameters. The parent name must be in leaf-first
// order, or NULL in case of a root instance.

SafcImmCtInstanceT *
inst(char *rdnName,
    char *rdnValue,
    SaNameT *parentName,
    SaImmAttrValuesT_2 **attrValues) {

  const int n = nAttrValues(attrValues);
  SaImmAttrValuesT_2 *rdnAttr = rdn(rdnName, rdnValue);
  SaImmAttrValuesT_2 **moreAttrValues = calloc(1+n+1, sizeof(SaImmAttrValuesT_2 *));
  SaImmAttrValuesT_2 **q = moreAttrValues;
  *q = rdnAttr;
  q++;
  for (SaImmAttrValuesT_2 **p = attrValues; *p != NULL; p++) {
    *q = *p;
    q++;
  }

  SafcImmCtInstanceT *result = calloc(1, sizeof(SafcImmCtInstanceT));
  result->parentName = parentName;
  result->attrValues = moreAttrValues;
  return result;
}


// Returns a null-terminated list of one instance.

SafcImmCtInstanceT **
oneInstance(SafcImmCtInstanceT *inst) {
  SafcImmCtInstanceT **result = calloc(1+1, sizeof(SafcImmCtInstanceT *));
  SafcImmCtInstanceT **p = result;
  *p = inst;
  return result;
}


// Returns a null-terminated list of two instances.

SafcImmCtInstanceT **
twoInstances(SafcImmCtInstanceT *inst0, SafcImmCtInstanceT *inst1) {
  SafcImmCtInstanceT **result = calloc(2+1, sizeof(SafcImmCtInstanceT *));
  SafcImmCtInstanceT **p = result;
  *p = inst0;
  p++;
  *p = inst1;
  return result;
}



// Returns an instance group with no instances.

SafcImmCtInstanceGroupT *
emptyInstGroup(const char *class) {
  SafcImmCtInstanceT **p = calloc(1, sizeof(SafcImmCtInstanceT *));
  SafcImmCtInstanceGroupT *result = calloc(1, sizeof(SafcImmCtInstanceGroupT));
  result->className = (SaImmClassNameT)class;
  result->instances = p;
  return result;
}



// Returns an instance group with the given instances.

SafcImmCtInstanceGroupT *
instGroup(SaImmClassNameT class, SafcImmCtInstanceT **insts) {
  SafcImmCtInstanceGroupT *result = calloc(1, sizeof(SafcImmCtInstanceGroupT));
  result->className = (SaImmClassNameT)class;
  result->instances = insts;
  return result;
}


// Returns a list containing one instance group.

SafcImmCtInstanceGroupT **
oneInstGroup(SafcImmCtInstanceGroupT *instGroup) {
  SafcImmCtInstanceGroupT **result = calloc(1+1, sizeof(SafcImmCtInstanceGroupT *));
  SafcImmCtInstanceGroupT **p = result;
  *p = instGroup;
  return result;
}


// Returns a list containing two instance groups.

SafcImmCtInstanceGroupT **
twoInstGroups(
    SafcImmCtInstanceGroupT *instGroup0,
    SafcImmCtInstanceGroupT *instGroup1) {
  SafcImmCtInstanceGroupT **result = calloc(2+1, sizeof(SafcImmCtInstanceGroupT *));
  SafcImmCtInstanceGroupT **p = result;
  *p = instGroup0;
  p++;
  *p = instGroup1;
  return result;
}


// Returns a list containing three instance groups.

SafcImmCtInstanceGroupT **
threeInstGroups(
    SafcImmCtInstanceGroupT *instGroup0,
    SafcImmCtInstanceGroupT *instGroup1,
    SafcImmCtInstanceGroupT *instGroup2) {
  SafcImmCtInstanceGroupT **result =
      calloc(3+1, sizeof(SafcImmCtInstanceGroupT *));
  SafcImmCtInstanceGroupT **p = result;
  *p = instGroup0;
  p++;
  *p = instGroup1;
  p++;
  *p = instGroup2;
  return result;
}



// Opens a new uniquely named log file.

FILE *openLog(char *name) {
  char *logDir = getenv("LOG_DIR");
  char *logFileName;
  struct timeb tb;
  ftime(&tb);
  asprintf(&logFileName,
      "%s/log_%s_%ld_%d.txt",
      logDir,
      name,
      tb.time,
      tb.millitm);
  return fopen(logFileName, "w");
}


// Returns false on the first invocation and true
// on every subsequent invocation. Uses the file
// system for persistence.

bool hasMark(char *name) {
  char *logDir = getenv("LOG_DIR");
  char *markFileName;
  asprintf(&markFileName, "%s/%s_mark.txt", logDir, name);
  struct stat statBuffer;
  int statResult = stat(markFileName, &statBuffer);
  if (statResult == -1 && errno == ENOENT) {
    FILE *markFile = fopen(markFileName, "w");
    fclose(markFile);
    return false;
  }
  else {
    return true;
  }
}


bool isOk(
    SaAisErrorT r,
    SafcImmCtHandleT handle,
    FILE *log,
    const char *traceMessage) {

  fprintf(log, "%s, result: %s\n", traceMessage, saDecode(r));
  fflush(log);

  if (r == SA_AIS_OK) {
    if (handle != NO_HANDLE) {
      safcImmCtFinalize(handle);
    }
    return true;
  }
  else {
    return false;
  }
}



bool notOk(
    SaAisErrorT r,
    SafcImmCtHandleT handle,
    FILE *log,
    const char *traceMessage) {

  fprintf(log, "%s, result: %s\n", traceMessage, saDecode(r));
  fflush(log);

  if (r != SA_AIS_OK) {
    if (handle != NO_HANDLE) {
      safcImmCtFinalize(handle);
    }
    return true;
  }
  else {
    return false;
  }
}

// Sleeps for approximately the given number of
// seconds. By passing secondsSleep as 0 the sleep
// period will be 100 ms.

void delaySeconds(time_t secondsSleep) {
  int ms = 1000000;    // 1000000 ns is 1 ms
  struct timespec ts = {
      .tv_sec=secondsSleep,
      .tv_nsec=(secondsSleep == 0 ? 100*ms : 0)
  };
  nanosleep(&ts, NULL);
}


// Repeatedly sleeps for approximately the given
// number of seconds. By passing secondsSleep as 0
// the sleep period will be 100 ms. A log message
// is written before each sleep period.

void idle(FILE *log, time_t secondsSleep) {
  while (true) {
    fprintf(log, "idle\n");
    fflush(log);
    delaySeconds(secondsSleep);
  }
}
