/* ----------------------------------------------------------------------
 * %CCaseFile:	ugt_common.c %
 * %CCaseRev:	/main/R2A/R3A/R4A/R8A/1 %
 * %CCaseDate:	2016-11-25 %
 * %CCaseDocNo: %
 * Author:	erarafo
 * Author: <name>, <e-mail address>
 *
 * Short description: Common functions, used by: ugt_s0, ugt_s1v1, ugt_s1v2,
 * ugt_s2 and noble_icti.
 *
 * ----------------------------------------------------------------------
 * %CCaseTemplateFile:  template.c %
 * %CCaseTemplateId: CCver: /main/1 %
 *
 * %CCaseCopyrightBegin%
 * Copyright (c) Ericsson AB 2013-2016 All rights reserved.
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
 * R2A/3      2014-01-18 erarafo     Removed run-once logic
 *                                   "Sleep-less" percentage set to 40
 * R2A/5      2014-03-20 erarafo     Added functions ok() and nok()
 * R2A/6      2014-03-20 erarafo     Removed unused functions
 * R2A/7      2014-03-24 erarafo     Support for incremental AUE model
 * R2A/8      2014-03-31 erarafo     Refactoring
 * R2A/10     2014-06-11 erarafo     Added a print utility
 * R3A/1      2015-02-18 erarafo     Added timestamped logging
 * R4A/1      2015-04-27 erarafo     Function for creating uint32 attribute
 * R4A/2      2015-08-20 erarafo     Added a function
 * R8A/1      2016-11-25 erarafo     printInstanceGroups arity
 * ----------------------------------------------------------------------
 */

#include "ugt_common.h"

void
appLogTimestamp(char *buffer) {
  struct timespec res;
  if (clock_gettime(CLOCK_REALTIME, &res) == 0) {
    unsigned int millis = res.tv_nsec/1000000;
    struct tm lt;
    localtime_r(&(res.tv_sec), &lt);
    snprintf(
        buffer,
        APPLOG_TIMESTAMP_SIZE,
        "%4d-%02d-%02dT%02d:%02d:%02d.%03d",
        lt.tm_year + 1900,
        lt.tm_mon + 1,
        lt.tm_mday,
        lt.tm_hour,
        lt.tm_min,
        lt.tm_sec,
        millis);
    buffer[APPLOG_TIMESTAMP_SIZE - 1] = '\0';
  }
  else {
    strcpy(buffer, "xxxx-xx-xxTxx:xx:xx.xxx");
  }
}

Group zeroGroups[] = {NULL};
Inst zeroInsts[] = {NULL};
Attr zeroAttrs[] = {NULL};
Value zeroValues[] = {NULL};


GroupList
consg(Group group, GroupList groupList) {
  GroupList result = calloc(sizegl(groupList)+1+1, sizeof(Group));
  GroupList q = result;
  *q = group;
  q++;
  for (GroupList p = groupList; *p != NULL; p++) {
    *q = *p;
    q++;
  }
  return result;
}

unsigned int
sizegl(GroupList groupList) {
  unsigned int result = 0;
  for (GroupList p = groupList; *p != NULL; p++) result++;
  return result;
}

Group
createGroup(ClassName className, InstList instList) {
    Group result = (Group)calloc(1, sizeof(SafcImmCtInstanceGroupT));
    result->className = className;
    result->instances = instList;
    return result;
}


InstList
consi(Inst inst, InstList instList) {
  InstList result = calloc(sizeil(instList)+1+1, sizeof(Inst));
  InstList q = result;
  *q = inst;
  q++;
  for (InstList p = instList; *p != NULL; p++) {
    *q = *p;
    q++;
  }
  return result;
}

unsigned int
sizeil(InstList instList) {
  unsigned int result = 0;
  for (InstList p = instList; *p != NULL; p++) result++;
  return result;
}

Inst
createInst(ParentName parentName, AttrList attrList) {
  Inst result = (Inst)calloc(1, sizeof(SafcImmCtInstanceT));
  result->parentName = parentName;
  result->attrValues = attrList;
  return result;
}


AttrList
consa(Attr attr, AttrList attrList) {
  AttrList result = calloc(sizeal(attrList)+1+1, sizeof(Attr));
  AttrList q = result;
  *q = attr;
  q++;
  for (AttrList p = attrList; *p != NULL; p++) {
    *q = *p;
    q++;
  }
  return result;
}

unsigned int
sizeal(AttrList attrList) {
  unsigned int result = 0;
  for (AttrList p = attrList; *p != NULL; p++) result++;
  return result;
}

Attr
createAttr(AttrName attrName, AttrType attrType, Mult mult, ValueList valueList) {
  Attr result = (Attr)calloc(1, sizeof(SaImmAttrValuesT_2));
  result->attrName = attrName;
  result->attrValueType = attrType;
  result->attrValuesNumber = mult;
  result->attrValues = valueList;
  return result;
}


ValueList
consv(Value value, ValueList valueList) {
  ValueList result = calloc(sizevl(valueList)+1+1, sizeof(Value));
  ValueList q = result;
  *q = value;
  q++;
  for (ValueList p = valueList; *p != NULL; p++) {
    *q = *p;
    q++;
  }
  return result;
}

unsigned int
sizevl(ValueList valueList) {
  unsigned int result = 0;
  for (ValueList p = valueList; *p != NULL; p++) result++;
  return result;
}

Value
createValueString(char *string) {
  char **box = (char **)calloc(1, sizeof(char *));
  *box = string;
  return (Value)box;
}



bool
verLessEq(const VersionStruct *x, const VersionStruct *y) {
  if (x->version < y->version) {
    return true;
  }
  else if (x->version > y->version) {
    return false;
  }
  else if (x->release < y->release) {
    return true;
  }
  else if (x->release > y->release) {
    return false;
  }
  else {
    return (x->correction <= y->correction);
  }
}

bool
verEq(const VersionStruct *x, const VersionStruct *y) {
  return x->version == y->version &&
      x->release == y->release &&
      x->correction == y->correction;
}


Handle
initialize(char *log) {
  Handle handle;
  SaAisErrorT result = safcImmCtInitialize(&handle);
  if (result != SA_AIS_OK) {
    APPLOG(log, "failed to get an ICTI handle, code: %d", result)
  }
  return handle;
}

void
finalize(char *log, Handle handle) {
  SaAisErrorT result = safcImmCtFinalize(handle);
  if (result != SA_AIS_OK) {
    APPLOG(log, "failed to finalize, handle: %llu, code: %d", handle, result)
  }
}


void
readSchemaVersions(
    Handle handle,
    String schema,
    Version from,
    Version to) {
  SaAisErrorT result = safcImmCtReadSchemaVersion_2(handle, schema, from, to);
  if (result != SA_AIS_OK) {
    // is this actually needed? TODO
    from->version = 0;
    from->release = 0;
    from->correction = 0;
    to->version = 0;
    to->release = 0;
    to->correction = 0;
  }
}


GroupList
readInsts(Handle handle, ClassName className) {
  ClassName classNames[] = {className, NULL};
  GroupList groupList;
  SaAisErrorT result = safcImmCtReadInstances(handle, classNames, &groupList);
  if (result != SA_AIS_OK) {
    char *message;
    asprintf(&message,
        "failed to read instances, class: %s, code: %d",
        className,
        result);
    safcImmCtFailUpgrade(handle, message);
    sleep(4000000000);
  }
  return groupList;
}

void
writeInsts(Handle handle, GroupList groupList) {
  SaAisErrorT result = safcImmCtWriteInstances(handle, groupList);
  if (result != SA_AIS_OK) {
    char *message;
    asprintf(&message,
        "failed to write instances, class: %s, code: %d",
        groupList[0]->className,
        result);
    safcImmCtFailUpgrade(handle, message);
    sleep(4000000000);
  }
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


// Returns a list containing one moRef-valued attribute

SaImmAttrValuesT_2 **
oneMoRefAttr(SaImmAttrNameT name, SaStringT value) {
  SaNameT *wrappedValue = wrapName(value);
  SaImmAttrValueT *valueArray = calloc(1, sizeof(SaImmAttrValueT));
  valueArray[0] = (SaImmAttrValueT)wrappedValue;
  SaImmAttrValuesT_2 **result = calloc(1 + 1, sizeof(SaImmAttrValuesT_2 *));
  result[0] = createAttrValue(name, SA_IMM_ATTR_SANAMET, 1, valueArray);
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



// Returns a list containing one uint32-valued attribute.

SaImmAttrValuesT_2 **
oneUint32Attr(SaImmAttrNameT name, SaUint32T value) {
  SaUint32T *z = calloc(1, sizeof(SaUint32T));
  *z = value;
  void *vz = (void *)z;

  SaImmAttrValuesT_2 **result = calloc(1+1, sizeof(SaImmAttrValuesT_2 *));
  SaImmAttrValuesT_2 **p = result;
  *p = createAttrValue(name, SA_IMM_ATTR_SAUINT32T, 1, singletArray(vz));
  return result;
}



// Creates a single-valued string-type attribute

SaImmAttrValuesT_2 *
stringAttr(SaImmAttrNameT name, SaStringT value) {
  return createAttrValue(
      name,
      SA_IMM_ATTR_SASTRINGT,
      1,
      singletArray(
          (SaImmAttrValueT)unDeref(value)));
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


// Returns a list containing one moref-valued attribute of
// multiplicity 2.

extern SaImmAttrValuesT_2 **
oneMorefs2Attr(SaImmAttrNameT name, char *ref1, char *ref2) {

  // strings to SanameT
  SaNameT *n1 = wrapName(ref1);
  SaNameT *n2 = wrapName(ref2);

  // two values
  SaImmAttrValueT *valueArray = calloc(2, sizeof(SaImmAttrValueT));
  valueArray[0] = (SaImmAttrValueT)n1;
  valueArray[1] = (SaImmAttrValueT)n2;

  // one attribute, multiplicity 2
  SaImmAttrValuesT_2 *v = calloc(1, sizeof(SaImmAttrValuesT_2));
  v->attrName = name;
  v->attrValueType = SA_IMM_ATTR_SANAMET;
  v->attrValuesNumber = 2;
  v->attrValues = valueArray;

  // one attribute
  SaImmAttrValuesT_2 **result = calloc(1+1, sizeof(SaImmAttrValuesT_2 *));
  result[0] = v;
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



// Functions for printing instance groups

static char *decodeValue(AttrType attrType, Value value) {
  char *result = NULL;
  if (attrType == SA_IMM_ATTR_SASTRINGT) {
    asprintf(&result, "%s", *((char **)value));
    return result;
  }
  else if (attrType == SA_IMM_ATTR_SAINT32T) {
    asprintf(&result, "%d", *((SaInt32T *)value));
    return result;
  }  else if (attrType == SA_IMM_ATTR_SAUINT32T) {
    asprintf(&result, "%u", *((SaUint32T *)value));
    return result;
  }  else if (attrType == SA_IMM_ATTR_SAINT64T) {
    asprintf(&result, "%lld", *((SaInt64T *)value));
    return result;
  }  else if (attrType == SA_IMM_ATTR_SAUINT64T) {
    asprintf(&result, "%llu", *((SaUint64T *)value));
    return result;
  }
  else if (attrType == SA_IMM_ATTR_SATIMET) {
    asprintf(&result, "%lld", *((SaTimeT *)value));
    return result;
  }
  else if (attrType == SA_IMM_ATTR_SANAMET) {
    SaNameT *name = (SaNameT *)value;
    result = (char *)calloc(name->length + 1, sizeof(char));
    for (unsigned int k = 0; k < name->length; k++) {
      SaUint8T byte = name->value[k];
      if (CHAR_MIN < 0 && byte >= 128) {
        result[k] = (int)byte - 256;
      }
      else {
        result[k] = byte;
      }
    }
    return result;
  }
  else if (attrType == SA_IMM_ATTR_SAFLOATT) {
    asprintf(&result, "%e", *((SaFloatT *)value));
    return result;
  }
  else if (attrType == SA_IMM_ATTR_SADOUBLET) {
    asprintf(&result, "%e", *((SaDoubleT *)value));
    return result;
  }
  else if (attrType == SA_IMM_ATTR_SAANYT) {
    asprintf(&result, "<cannot decode: SaAnyT>");
    return result;
  }
  else {
    asprintf(&result, "<unknown type: %d>", attrType);
    return result;
  }
}


static void printAttribute(
    FILE *file,
    AttrName attrName,
    AttrType attrType,
    Mult mult,
    ValueList values) {

  fprintf(file, "    %s: ", attrName);
  if (mult == 0) {
    fprintf(file, "<no value>\n");
  }
  else if (mult == 1) {
    fprintf(file, "%s\n", decodeValue(attrType, values[0]));
  }
  else {
    fprintf(file, "[");
    for (Mult k = 0; k < mult; k++) {
      char *s = decodeValue(attrType, values[k]);
      if (k == 0) {
        fprintf(file, "%s", s);
      }
      else {
        fprintf(file, ", %s", s);
      }
      free((void *)s);
      fprintf(file, "]\n");
    }
  }
}


static void printAttributes(FILE *file, AttrList attrs) {
  for (AttrList p = attrs; (*p) != NULL; p++) {
    AttrName attrName = (*p)->attrName;
    AttrType attrType = (*p)->attrValueType;
    Mult mult = (*p)->attrValuesNumber;
    ValueList values = (*p)->attrValues;
    printAttribute(file, attrName, attrType, mult, values);
  }
}


static void printDn(FILE *file, ParentName parentName) {
  fprintf(file, "  parent: ");
  for (SaUint16T j = 0; j < parentName->length; j++) {
    fprintf(file, "%c", parentName->value[j]);
  }
  fprintf(file, "\n");
}


static void printInstances(FILE *file, InstList instances) {
  for (InstList p = instances; (*p) != NULL; p++) {
    printDn(file, (*p)->parentName);
    printAttributes(file, (*p)->attrValues);
  }
}


void printInstanceGroups(FILE *file, GroupList instanceGroups, bool includeAll) {
  fprintf(file, "\n\n--- show no-value attributes: %s --------\n\n", includeAll ? "yes" : "no");
  for (GroupList p = instanceGroups; *p != NULL; p++) {
    fprintf(file, "class: %s\n", (*p)->className);
    printInstances(file, (*p)->instances);
  }
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


FILE *
appLogAppend(char *name) {
  char *filename;
  asprintf(&filename, "%s/%s.log", getenv("LOG_DIR"), name);
  FILE *stream = fopen(filename, "a");
  free(filename);
  return stream;
}








bool ok(const char *operation, SaAisErrorT r, FILE *log) {
  fprintf(log, "%s -> %s\n", operation, saDecode(r));
  fflush(log);
  return (r == SA_AIS_OK);
}

bool nok(const char *operation, SaAisErrorT r, FILE *log) {
  fprintf(log, "%s -> %s\n", operation, saDecode(r));
  fflush(log);
  return (r != SA_AIS_OK);
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





static bool signalReceived;

static void
usr2Handler() {
  signalReceived = true;
}


/**
 * Runs the specified converter. If log is non-NULL progress
 * messages will be written using APPLOG with the given name.
 * On successful execution RUN_CONVERTER_OK is returned.
 */
RunConverterErrorT
runConverter(const char *name, const char *log) {
  LmhiResultCode r;
  LmhiGetLMsResult lms;
  r = Lmhi_get_lms(NULL, "dynamic", &lms);
  if (r != LMHI_OK) {
    APPLOG(log, "failed to lookup dynamic program: %s, code: %u", name, r);
    return RUN_CONVERTER_LOOKUP_FAILURE;
  }

  unsigned int matchCount = 0;
  unsigned int k = lms.n_lmdata;
  for (unsigned int j = 0; j < lms.n_lmdata; j++) {
    if (STREQ(lms.lmdata[j].name, name)) {
      matchCount++;
      k = j;
    }
  }

  if (matchCount < 1) {
    return RUN_CONVERTER_NOT_FOUND;
  }
  else if (matchCount > 1) {
    return RUN_CONVERTER_AMBIGUOUS;
  }
  else {
    struct sigaction *sigActions = calloc(2, sizeof(struct sigaction));
    sigActions[0].sa_handler = &usr2Handler;
    int sigactionRes;
    sigactionRes = sigaction(SIGUSR2, sigActions, sigActions+1);
    signalReceived = false;
    if (sigactionRes != 0) {
      APPLOG(log, "failed to set up signal handling, code: %d", sigactionRes);
      return RUN_CONVERTER_SIGNAL_HANDLING_SETUP_FAILURE;
    }

    char *pidString;
    asprintf(&pidString, "%d", getpid());
    char *argv[] = {pidString, NULL};
    LmhiStartPgmResult res;
    r = Lmhi_start_pgm(0, 0, lms.lmdata[k].id, lms.lmdata[k].name, argv, &res);
    if (r != LMHI_OK) {
      APPLOG(log, "failed to start dynamic program: %s, code: %u", name, r);
      return RUN_CONVERTER_START_PROGRAM_FAILURE;
    }

    while (!signalReceived) {
        sleep(1);
    }

    sigactionRes = sigaction(SIGUSR2, sigActions+1, NULL);
    if (sigactionRes != 0) {
      APPLOG(log, "failed to restore signal handling, code: %d", sigactionRes);
      return RUN_CONVERTER_SIGNAL_HANDLING_RESTORE_FAILURE;
    }
    free(sigActions);

    r = Lmhi_stop_pgm(0, res.pgmId);
    if (r != LMHI_OK) {
      APPLOG(log, "failed to stop dynamic pgm, code: %u", r);
      return RUN_CONVERTER_STOP_PROGRAM_FAILURE;
    }

    return RUN_CONVERTER_OK;
  }
}
