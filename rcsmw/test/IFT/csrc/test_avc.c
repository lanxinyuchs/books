/* ----------------------------------------------------------------------
 * %CCaseFile:	test_avc.c %
 * %CCaseRev:	/main/R2A/R3A/R5A/R6A/1 %
 * %CCaseDate:	2016-07-11 %
 * %CCaseDocNo: %
 * Author:	erarafo
 * Author: <name>, <e-mail address>
 *
 * Short description:
 * Object implementer functionality needed for tests of AVC.
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
 * R2A/1      2014-01-28 erarafo     Created
 * R2A/2      2014-01-30 erarafo     Passing session handle to test script
 * R2A/4      2014-01-31 erarafo     Create and delete cases added
 * R2A/5      2014-02-10 erarafo     Work in progress
 * R2A/7      2014-03-25 erarafo     Still work in progress
 * R2A/8      2014-03-26 erarafo     Support for tests of multivalue attributes
 * R2A/9      2014-04-01 erarafo     Very embarrassing bug fixed
 * R3A/1      2015-03-07 erarafo     Minor additions
 * R3A/2      2015-03-21 erarafo     Warnings eliminated
 * R6A/1      2016-07-11 etxpeno     Coverity fixes
 * ----------------------------------------------------------------------
 */

#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif

#include <saAis.h>
#include <saImmOi.h>
#include <saImmOm.h>
#include "master.h"
#include "test_avc.h"


#define STREQ(A, B) strcmp((const char *)A, B) == 0

typedef SaImmOiHandleT Handle;
typedef SaNameT *Dn;
typedef SaImmAttrNameT AttrName;
typedef SaImmAttrModificationTypeT ModType;
typedef SaStringT String;
typedef String *StringList;


typedef SaImmAttrValuesT_2 AttrStruct;
typedef AttrStruct *Attr;
typedef AttrStruct **AttrList;

typedef SaImmAttrModificationT_2 ModSpecStruct;
typedef ModSpecStruct *ModSpec;
typedef ModSpecStruct **ModSpecList;


typedef SaImmAttrValueT Value;
typedef SaImmAttrValueT *ValueArray;



static ModSpec emptyModSpecList[] = {NULL};

static String emptyStringList[] = {NULL};


unsigned int sizesl(StringList stringList) {
  unsigned int result = 0;
  for (StringList p = stringList; *p != NULL; p++) {
    result++;
  }
  return result;
}


static StringList
conss(String string, StringList stringList) {
  StringList result = calloc(sizesl(stringList)+1+1, sizeof(String));
  StringList q = result;
  *q = string;
  q++;
  for (StringList p = stringList; *p != NULL; p++) {
    *q = *p;
    q++;
  }
  return result;
}


/**
 * Decodes a non-empty list of strings.
 */
static StringList
decodeStrings(ei_x_buff *buffer) {
  int listSize = -1;
  ei_decode_list_header(buffer->buff, &(buffer->index), &listSize);
  StringList result = calloc(listSize+1, sizeof(String));
  for (int k = 0; k < listSize; k++) {
    result[k] = decodeString(buffer);
  }
  return result;
}



unsigned int sizeml(ModSpecList modSpecList) {
  unsigned int result = 0;
  for (ModSpecList p = modSpecList; *p != NULL; p++) {
    result++;
  }
  return result;
}

static ModSpecList
consm(ModSpec modSpec, ModSpecList modSpecList) {
  ModSpecList result = calloc(sizeml(modSpecList)+1+1, sizeof(ModSpec));
  ModSpecList q = result;
  *q = modSpec;
  q++;
  for (ModSpecList p = modSpecList; *p != NULL; p++) {
    *q = *p;
    q++;
  }
  return result;
}







static SaNameT *saName(const char *name) {
  SaNameT *result = calloc(1, sizeof(SaNameT));
  size_t len = strlen(name);
  const unsigned int n = len > SA_MAX_NAME_LENGTH ? SA_MAX_NAME_LENGTH : len;
  result->length = (SaUint16T)len;
  for (unsigned int j = 0; j < n; j++) {
    result->value[j] = (SaUint8T)name[j];
  }
  return result;
}

static SaImmAttrValuesT_2 *attrValuesEmpty(const char *attrName) {
  SaImmAttrValuesT_2 *result = calloc(1, sizeof(SaImmAttrValuesT_2));
  result->attrName = (char *)attrName;
  result->attrValueType = SA_IMM_ATTR_SAUINT32T;
  result->attrValuesNumber = 0;
  result->attrValues = NULL;
  return result;
}

static SaImmAttrValuesT_2 *attrValuesSingleUint32(const char *attrName, unsigned int k) {
  SaUint32T *boxedInt = calloc(1, sizeof(SaUint32T));
  *boxedInt = k;
  void **values = calloc(1, sizeof(void *));
  *values = (void *)boxedInt;
  SaImmAttrValuesT_2 *result = calloc(1, sizeof(SaImmAttrValuesT_2));
  result->attrName = (char *)attrName;
  result->attrValueType = SA_IMM_ATTR_SAUINT32T;
  result->attrValuesNumber = 1;
  result->attrValues = values;
  return result;
}

static SaImmAttrValuesT_2 *attrValuesSingleInt32(const char *attrName, int k) {
  SaInt32T *boxedInt = calloc(1, sizeof(SaInt32T));
  *boxedInt = k;
  void **values = calloc(1, sizeof(void *));
  *values = (void *)boxedInt;
  SaImmAttrValuesT_2 *result = calloc(1, sizeof(SaImmAttrValuesT_2));
  result->attrName = (char *)attrName;
  result->attrValueType = SA_IMM_ATTR_SAINT32T;
  result->attrValuesNumber = 1;
  result->attrValues = values;
  return result;
}


/**
 * Creates a scalar string-valued Attr.
 */
static Attr
attrValuesSingleString(AttrName attrName, String s) {
  char **boxedString = calloc(1, sizeof(char *));
  asprintf(boxedString, "%s", s);
  void **values = calloc(1, sizeof(void *));
  *values = (void *)boxedString;
  Attr result = calloc(1, sizeof(AttrStruct));
  result->attrName = attrName;
  result->attrValueType = SA_IMM_ATTR_SASTRINGT;
  result->attrValuesNumber = 1;
  result->attrValues = values;
  return result;
}

/**
 * Creates a multi-valued string Attr. The multiplicity is
 * implied by the given StringList.
 */
static Attr
attrValuesMultiString(AttrName attrName, StringList ss) {
  unsigned int mult = sizesl(ss);
  if (mult == 0) {
    Attr result = calloc(1, sizeof(AttrStruct));
    result->attrName = attrName;
    result->attrValueType = SA_IMM_ATTR_SASTRINGT;
    result->attrValuesNumber = 0;
    result->attrValues = NULL;
    return result;
  }
  else {
    ValueArray values = calloc(mult, sizeof(Value));
    StringList z = ss;
    for (unsigned int k = 0; k < mult; k++) {
      String *boxedString = calloc(1, sizeof(String));
      asprintf(boxedString, "%s", *z);
      z++;
      values[k] = (Value)boxedString;
    }
    Attr result = calloc(1, sizeof(AttrStruct));
    result->attrName = attrName;
    result->attrValueType = SA_IMM_ATTR_SASTRINGT;
    result->attrValuesNumber = mult;
    result->attrValues = values;
    return result;
  }
}


//
//  char **boxedString = calloc(1, sizeof(char *));
//  asprintf(boxedString, "%s", s);
//  void **values = calloc(1, sizeof(void *));
//  *values = (void *)boxedString;
//  Attr result = calloc(1, sizeof(AttrStruct));
//  result->attrName = attrName;
//  result->attrValueType = SA_IMM_ATTR_SASTRINGT;
//  result->attrValuesNumber = 1;
//  result->attrValues = values;
//  return result;
//}





/**
 * Create a modification specification from the
 * given Attr and modification type.
 */
static ModSpec
createModSpec(ModType modType, Attr attr) {
  ModSpec result = calloc(1, sizeof(ModSpecStruct));
  result->modType = modType;
  result->modAttr.attrName = attr->attrName;
  result->modAttr.attrValueType = attr->attrValueType;
  result->modAttr.attrValuesNumber = attr->attrValuesNumber;
  result->modAttr.attrValues = attr->attrValues;
  return result;
}



static SaImmAttrValuesT_2 **attrValuesDuo(SaImmAttrValuesT_2 *a1, SaImmAttrValuesT_2 *a2) {
  SaImmAttrValuesT_2 **result = calloc(2+1, sizeof(SaImmAttrValuesT_2 *));
  result[0] = a1;
  result[1] = a2;
  return result;
}


static SaImmAttrModificationT_2 *attrValueSetNone(const char *attrName) {
  SaImmAttrModificationT_2 *result = calloc(1, sizeof(SaImmAttrModificationT_2));
  SaImmAttrValuesT_2 *av = attrValuesEmpty(attrName);
  result->modAttr.attrName = av->attrName;                        // "fat" assignment should work too
  result->modAttr.attrValueType = av->attrValueType;
  result->modAttr.attrValuesNumber = av->attrValuesNumber;
  result->modAttr.attrValues = av->attrValues;
  result->modType = SA_IMM_ATTR_VALUES_REPLACE;
  free(av);
  return result;
}

static SaImmAttrModificationT_2 *attrValueSetOne(const char *attrName, unsigned int k) {
  SaImmAttrModificationT_2 *result = calloc(1, sizeof(SaImmAttrModificationT_2));
  SaImmAttrValuesT_2 *av = attrValuesSingleUint32(attrName, k);
  result->modAttr.attrName = av->attrName;                        // "fat" assignment should work too
  result->modAttr.attrValueType = av->attrValueType;
  result->modAttr.attrValuesNumber = av->attrValuesNumber;
  result->modAttr.attrValues = av->attrValues;
  result->modType = SA_IMM_ATTR_VALUES_REPLACE;
  free(av);
  return result;
}

static SaImmAttrModificationT_2 *attrValueSetOneSigned(const char *attrName, int k) {
  SaImmAttrModificationT_2 *result = calloc(1, sizeof(SaImmAttrModificationT_2));
  SaImmAttrValuesT_2 *av = attrValuesSingleInt32(attrName, k);
  result->modAttr.attrName = av->attrName;                        // "fat" assignment should work too
  result->modAttr.attrValueType = av->attrValueType;
  result->modAttr.attrValuesNumber = av->attrValuesNumber;
  result->modAttr.attrValues = av->attrValues;
  result->modType = SA_IMM_ATTR_VALUES_REPLACE;
  free(av);
  return result;
}










static SaImmAttrModificationT_2 **singleMod() {
  SaImmAttrModificationT_2 **result =
    calloc(1+1, sizeof(SaImmAttrModificationT_2 *));
  return result;
}

/**
 * Changes the specified attribute according to the given modType
 * which is one of SA_IMM_ATTR_VALUES_ADD, SA_IMM_ATTR_VALUES_DELETE
 * and SA_IMM_ATTR_VALUES_REPLACE.
 *
 * Assuming the attribute is not multi-valued, using ADD is perhaps
 * only allowed if the number of values is zero before the call.
 *
 * Assuming the attribute is not multi-valued, using REPLACE is
 * assumed to be OK anytime.
 *
 * Assuming the attribute is not multi-valued, using DELETE may be
 * ok only if there is a value match?
 */
static SaAisErrorT
changeAttrStringScalar(
    Handle handle,
    Dn dn,
    AttrName attrName,
    ModType modType,
    StringList strings) {
  Attr attr = attrValuesMultiString(attrName, strings);
  ModSpec modSpec = createModSpec(modType, attr);
  ModSpecList modSpecList = consm(modSpec, emptyModSpecList);
  SaAisErrorT r = saImmOiRtObjectUpdate_2(handle, dn, (const SaImmAttrModificationT_2 **)modSpecList);
  free(modSpecList);
  free(attr);
  return r;
}



static ei_x_buff
avcInit(ei_x_buff request) {
    // Initialize and return a handle (64-bit unsigned integer
    // cast to a string).
    SaVersionT version = {
        .releaseCode = 'A',
        .majorVersion = 2,
        .minorVersion = 11
    };

    SaImmOiHandleT handle;

      const SaImmOiCallbacksT_2 *callbacks = NULL;

  ei_x_buff response;
  ei_x_new(&response);

    SaAisErrorT r = saImmOiInitialize_2(&handle, callbacks, &version);
    if (r != SA_AIS_OK) {
      APPLOG("initialize, failed: %d", r);
      ei_x_format(&response, "{error, ~i}", r);
      return response;
    } else {
      APPLOG("initialize, success; release code: %d, major: %d, minor: %d",
          version.releaseCode, version.majorVersion, version.minorVersion);
      char *handleS;
      asprintf(&handleS, "%llu", handle);
      ei_x_format(&response, "{ok, ~s}", handleS);
      free((void *)handleS);
      return response;
    }
}

static ei_x_buff
avcFin(ei_x_buff request) {
    const SaImmOiHandleT handle = decodeHandle(&request);

  ei_x_buff response;
  ei_x_new(&response);

    SaAisErrorT r = saImmOiFinalize(handle);
    if (r != SA_AIS_OK) {
      APPLOG("finalize, failed: %d", r);
      ei_x_format(&response, "{error, {initialize, ~i}}", r);
      return response;
    } else {
      APPLOG("finalize, %s", "success");
      ei_x_format(&response, "{ok}");
      return response;
    }
}


static ei_x_buff
avcImplSet(ei_x_buff request) {
    const SaImmOiHandleT handle = decodeHandle(&request);
    char *implName = decodeString(&request);

      ei_x_buff response;
  ei_x_new(&response);

    SaAisErrorT r = saImmOiImplementerSet(handle, (SaImmOiImplementerNameT)implName);
    if (r != SA_AIS_OK) {
      APPLOG("implementerSet, failed: %d", r);
      ei_x_format(&response, "{error, {implementerSet, ~i}}", r);
      return response;
    } else {
      APPLOG("implementerSet, success, name: %s", implName);
      ei_x_format(&response, "{ok}");
      return response;
    }
}

static ei_x_buff
avcImplClear(ei_x_buff request) {
    const SaImmOiHandleT handle = decodeHandle(&request);

  ei_x_buff response;
  ei_x_new(&response);

    SaAisErrorT r = saImmOiImplementerClear(handle);
    if (r != SA_AIS_OK) {
      APPLOG("implementerClear, failed: %d", r);
      ei_x_format(&response, "{error, {implementerClear, ~i}}", r);
      return response;
    } else {
      APPLOG("implementerClear, %s", "success");
      ei_x_format(&response, "{ok}");
      return response;
    }
}

/**
 * DEPRECATED, use avcSet32() instead.
 *
 * Update an uint32 attribute. Parameters are:
 *
 *     handle
 *     DN
 *     attributeName
 *     value
 */
static ei_x_buff
avcSetUint32(ei_x_buff request) {
  const SaImmOiHandleT handle = decodeHandle(&request);
  char *dn = decodeString(&request);
  char *attrName = decodeString(&request);

  ei_x_buff response;
  ei_x_new(&response);

  long long llValue = decodeInteger(&request);
  if (llValue < 0) {
    APPLOG("set attribute failed, dn: %s, attr: %s, value: %lld", dn, attrName, llValue);
    ei_x_format(&response, "{error, {attributeSet, ~s, ~s, ~i}}", dn, attrName, llValue);
    return response;
  }
  const unsigned int v = (unsigned int)llValue;

  SaNameT *objectName = saName(dn);
  const SaImmAttrModificationT_2 **mods = (const SaImmAttrModificationT_2 **)singleMod();
  mods[0] = attrValueSetOne(attrName, v);

  SaAisErrorT r = saImmOiRtObjectUpdate_2(handle, objectName, mods);
  if (r != SA_AIS_OK) {
    APPLOG("set attribute failed, dn: %s, attr: %s, value: %u", dn, attrName, v);
    ei_x_format(&response, "{error, {attributeSet, ~s, ~s, ~i}}", dn, attrName, v);
  }
  else {
    ei_x_format(&response, "{ok}");
  }

  free(mods);
  free(objectName);

  return response;
}

/**
 * Update an 32-bit integer-like attribute. Parameters are:
 *
 *     handle
 *     DN
 *     attributeName
 *     valuetype: 0=signed, 1=unsigned
 *     value
 *
 *     The full unsigned range is NOT handled due to
 *     limitations in decodeInteger().
 */
static ei_x_buff
avcSet32(ei_x_buff request) {
  const SaImmOiHandleT handle = decodeHandle(&request);
  char *dn = decodeString(&request);
  char *attrName = decodeString(&request);

  ei_x_buff response;
  ei_x_new(&response);

  long long llValueType = decodeInteger(&request);

  SaNameT *objectName = saName(dn);
  const SaImmAttrModificationT_2 **mods = (const SaImmAttrModificationT_2 **)singleMod();
  unsigned int v;
  int k;

  if (llValueType == 1) {   // unsigned
    long long llValue = decodeInteger(&request);
    if (llValue < 0) {
      // negative value passed for unsigned item
      APPLOG("set attribute failed, dn: %s, attr: %s, value: %lld", dn, attrName, llValue);
      ei_x_format(&response, "{error, {attributeSet, ~s, ~s, ~i}}", dn, attrName, llValue);
      free(mods);
      free(objectName);
      return response;
    }
    v = (unsigned int)llValue;
    mods[0] = attrValueSetOne(attrName, v);
    SaAisErrorT r = saImmOiRtObjectUpdate_2(handle, objectName, mods);
    if (r != SA_AIS_OK) {
      APPLOG("set attribute failed, dn: %s, attr: %s, value: %u", dn, attrName, v);
      ei_x_format(&response, "{error, {attributeSet, ~s, ~s, ~i}}", dn, attrName, v);
    }
    else {
      ei_x_format(&response, "{ok}");
    }
  }
  else {                    // signed
    long long llValue = decodeInteger(&request);
    k = (int) llValue;
    mods[0] = attrValueSetOneSigned(attrName, k);
    SaAisErrorT r = saImmOiRtObjectUpdate_2(handle, objectName, mods);
    if (r != SA_AIS_OK ) {
      APPLOG("set attribute failed, dn: %s, attr: %s, value: %d", dn, attrName, k);
      ei_x_format(&response, "{error, {attributeSet, ~s, ~s, ~i}}", dn, attrName, k);
    }
    else {
      ei_x_format(&response, "{ok}");
    }
  }

  free(mods);
  free(objectName);

  return response;
}

static ei_x_buff
avcClearUint32(ei_x_buff request) {
  const SaImmOiHandleT handle = decodeHandle(&request);
  char *dn = decodeString(&request);
  char *attrName = decodeString(&request);

  SaNameT *objectName = saName(dn);
  const SaImmAttrModificationT_2 **mods = (const SaImmAttrModificationT_2 **)singleMod();
  mods[0] = attrValueSetNone(attrName);

  ei_x_buff response;
  ei_x_new(&response);

  SaAisErrorT r = saImmOiRtObjectUpdate_2(handle, objectName, mods);
  if (r != SA_AIS_OK) {
    APPLOG("nullify attribute failed, dn: %s, attr: %s", dn, attrName);
    ei_x_format(&response, "{error, {attributeNullify, ~s, ~s}}", dn, attrName);
  }
  else {
    ei_x_format(&response, "{ok}");
  }

  free(mods);
  free(objectName);

  return response;
}

static ei_x_buff
avcChangeAttrStringScalar(ei_x_buff request, int func) {
  const SaImmOiHandleT handle = decodeHandle(&request);
  char *dn = decodeString(&request);
  char *attrName = decodeString(&request);
  SaNameT *objectName = saName(dn);
  ModType modType = SA_IMM_ATTR_VALUES_REPLACE;

  APPLOG("DN: %s, attr: %s, funct: %u", dn, attrName, func);
  APPLOG("modType: %d", modType);

  SaAisErrorT r = SA_AIS_ERR_NO_OP;
  if (func == AVC_ADD_STRING) {
    String attrValue = decodeString(&request);
    StringList stringList = conss(attrValue, emptyStringList);
    r = changeAttrStringScalar(handle, objectName, attrName, modType, stringList);
    free(stringList);
  }
  else if (func == AVC_UPDATE_STRING) {
    String attrValue = decodeString(&request);
    StringList stringList = conss(attrValue, emptyStringList);
    r = changeAttrStringScalar(handle, objectName, attrName, modType, stringList);
    free(stringList);
  }
  else if (func == AVC_REMOVE_STRING) {
    r = changeAttrStringScalar(handle, objectName, attrName, modType, emptyStringList);
  }

  ei_x_buff response;
  ei_x_new(&response);

  if (r != SA_AIS_OK) {
    APPLOG("failure, string op, funct: %d, dn: %s, attr: %s, SAF code: %u", func, dn, attrName, r);
    ei_x_format(&response, "{error, {attributeOp, ~i, ~s, ~s, ~i}}", func, dn, attrName, r);
  }
  else {
    APPLOG("succsessful, string op, funct: %d, dn: %s, attr: %s", func, dn, attrName);
    ei_x_format(&response, "{ok}");
  }

  free(objectName);
  free(dn);

  return response;
}


static ei_x_buff
avcChangeAttrStringMulti(ei_x_buff request, int func) {
  const SaImmOiHandleT handle = decodeHandle(&request);
  char *dn = decodeString(&request);
  char *attrName = decodeString(&request);

  // It is trusted that the function is AVC_*_STRINGS
  ModType modType =
      func == AVC_ADD_STRINGS ? SA_IMM_ATTR_VALUES_ADD :
          func == AVC_DELETE_STRINGS ? SA_IMM_ATTR_VALUES_DELETE :
              SA_IMM_ATTR_VALUES_REPLACE;

  StringList strings =
      func == AVC_CLEAR_STRINGS ? emptyStringList :
          decodeStrings(&request);

  APPLOG("func: %u", func);
  APPLOG("  DN: %s", dn);
  APPLOG("attr: %s", attrName);
  APPLOG("mod type: %u", modType);

  APPLOG("string list size: %u", sizesl(strings))

  if (sizesl(strings) > 0) {
    for (unsigned int k = 0; k < sizesl(strings); k++) {
      APPLOG("    string[%u]: %s", k, strings[k]);
    }
  }

  SaNameT *objectName = saName(dn);
  SaAisErrorT r = changeAttrStringScalar(handle, objectName, attrName, modType, strings);

  ei_x_buff response;
  ei_x_new(&response);

  if (r != SA_AIS_OK) {
    APPLOG("failure, string op, funct: %d, dn: %s, attr: %s, SAF code: %u", func, dn, attrName, r);
    ei_x_format(&response, "{error, {attributeOp, ~i, ~s, ~s, ~i}}", func, dn, attrName, r);
  }
  else {
    APPLOG("succsessful, string op, funct: %d, dn: %s, attr: %s", func, dn, attrName);
    ei_x_format(&response, "{ok}");
  }

  free(objectName);
  free(strings);
  free(dn);

  return response;
}




static ei_x_buff
avcCreateInstanceSingleAttr(ei_x_buff request) {
  const SaImmOiHandleT handle = decodeHandle(&request);
  char *parentDn = decodeString(&request);
  char *className = decodeString(&request);
  char *rdnName = decodeString(&request);
  char *rdnValue = decodeString(&request);

  char *type = decodeString(&request);
  char *attrName = decodeString(&request);
  char *attrValue = decodeString(&request);

  SaImmAttrValuesT_2 **attrValues = NULL;

  if (STREQ(type, "uint32")) {
    attrValues = attrValuesDuo(
        attrValuesSingleString(rdnName, rdnValue),
        attrValuesSingleUint32(attrName, (unsigned int)strtoul(attrValue, NULL, 0)));
  }
  else if (STREQ(type, "int32")) {
    attrValues = attrValuesDuo(
        attrValuesSingleString(rdnName, rdnValue),
        attrValuesSingleInt32(attrName, atoi(attrValue)));
  }

  SaNameT *objectName = saName(parentDn);
  SaAisErrorT r = saImmOiRtObjectCreate_2(
      handle,
      className,
      objectName,
      (const SaImmAttrValuesT_2 **)attrValues);
  ei_x_buff response;
  ei_x_new(&response);
  if (r != SA_AIS_OK) {
    APPLOG("create instance failed, class: %s, parent: %s", className, parentDn);
    ei_x_format(&response, "{error, {createInstance, ~i, ~s, ~s, ~s, ~s, ~s, ~s, ~s}}",
        r,
        parentDn,
        className,
        rdnName,
        rdnValue,
        type,
        attrName,
        attrValue);
  } else {
    ei_x_format(&response, "{ok}");
  }

  free(objectName);
  free(attrValues);
  free(attrValue);
  free(type);
  free(parentDn);

  return response;
}

static ei_x_buff
avcDeleteInstance(ei_x_buff request) {
  const SaImmOiHandleT handle = decodeHandle(&request);
  char *dn = decodeString(&request);

  SaNameT *instance = saName(dn);
  SaAisErrorT r = saImmOiRtObjectDelete(handle, instance);
  free(instance);

  ei_x_buff response;
  ei_x_new(&response);
  if (r != SA_AIS_OK) {
    APPLOG("delete instance failed, code: %d, dn: %s", r, dn);
    ei_x_format(&response, "{error, {deleteInstance, ~i, ~s}}", r, dn);
    return response;
  }
  else {
    ei_x_format(&response, "{ok}");
    return response;
  }
}


/**
 * Display an attribute to the app log. For now it is very hardcoded
 * which instance and attribute to look at.
 */
static ei_x_buff
instDisp() {

  SaVersionT version = {
      .releaseCode = 'A',
      .majorVersion = 2,
      .minorVersion = 11
  };

  ei_x_buff response;
  ei_x_new(&response);

  SaImmHandleT handle;
  SaAisErrorT r;
  r = saImmOmInitialize(
      &handle,
      NULL,
      &version);

  if (r != SA_AIS_OK) {
    APPLOG("OM init failure, code: %u", r);
    ei_x_format(&response, "{error, {omInit, ~i}}", r);
    return response;
  }

  SaImmAccessorHandleT accessorhandle;
  r = saImmOmAccessorInitialize(handle, &accessorhandle);
  if (r != SA_AIS_OK) {
    APPLOG("OM init acc handle failure, code: %u", r);
    ei_x_format(&response, "{error, {omAccInit, ~i}}", r);
    return response;
  }

  SaImmAttrValuesT_2 **avs;

  SaImmAttrNameT attrNames[] = {"c", NULL};
  SaNameT *objectName = saName("argonId=1,neonId=1,NOBLEheliumId=1");

  r = saImmOmAccessorGet_2(
      accessorhandle,
      objectName,
      attrNames,
      &avs);
  free(objectName);

  if (r != SA_AIS_OK) {
    APPLOG("OM get attrs failure, code: %u", r);
    ei_x_format(&response, "{error, {omGet, ~i}}", r);
    return response;
  }

  // Expect just one entry

  APPLOG("got attr name (expect c): %s",
  avs[0]->attrName);

  APPLOG("got mult: %u, type: %u", avs[0]->attrValuesNumber, avs[0]->attrValueType);

  for (unsigned int k = 0; k < avs[0]->attrValuesNumber; k++) {
    char **boxedString = (char **)avs[0]->attrValues[k];
    APPLOG("value[%u]: %s", k, *boxedString);
  }

  r = saImmOmAccessorFinalize(accessorhandle);
  if (r != SA_AIS_OK) {
    APPLOG("OM acc fin failure, code: %u", r);
    ei_x_format(&response, "{error, {omAccFin, ~i}}", r);
    return response;
  }

  r = saImmOmFinalize(handle);
  if (r != SA_AIS_OK) {
    APPLOG("OM fin failure, code: %u", r);
    ei_x_format(&response, "{error, {omFin, ~i}}", r);
    return response;
  }

  ei_x_format(&response, "{ok}");
  return response;
}



/**
 * Handles a call from the test suite towards the CS.
 */
ei_x_buff send_sig_avc(int function, ei_x_buff request) {

  int nArgs;
  ei_decode_tuple_header(request.buff, &request.index, &nArgs);

  APPLOG("send_sig_avc, function: %d, n. of args: %d", function, nArgs);

  if (function == AVC_INIT) {
    return avcInit(request);
  }
  else if (function == AVC_FIN) {
    return avcFin(request);
  }
  else if (function == AVC_IMPL_SET) {
    return avcImplSet(request);
  }
  else if (function == AVC_IMPL_CLEAR) {
    return avcImplClear(request);
  }
  else if (function == AVC_SET_UINT32) {
    return avcSetUint32(request);
  }
  else if (function == AVC_CLEAR_UINT32) {
    return avcClearUint32(request);
  }
  else if (function == AVC_ADD_STRING) {
    return avcChangeAttrStringScalar(request, function);
  }
  else if (function == AVC_UPDATE_STRING) {
    return avcChangeAttrStringScalar(request, function);
  }
  else if (function == AVC_REMOVE_STRING) {
    return avcChangeAttrStringScalar(request, function);
  }
  else if (function == AVC_ADD_STRINGS) {
    return avcChangeAttrStringMulti(request, function);
  }
  else if (function == AVC_DELETE_STRINGS) {
    return avcChangeAttrStringMulti(request, function);
  }
  else if (function == AVC_REPLACE_STRINGS) {
    return avcChangeAttrStringMulti(request, function);
  }
  else if (function == AVC_CLEAR_STRINGS) {
    return avcChangeAttrStringMulti(request, function);
  }
  else if (function == AVC_CREATE_INSTANCE_SINGLE_ATTR) {
    return avcCreateInstanceSingleAttr(request);
  }
  else if (function == AVC_DELETE_INSTANCE) {
    return avcDeleteInstance(request);
  }
  else if (function == AVC_DISP) {
    return instDisp();
  }
  else if (function == AVC_SET_32) {
    return avcSet32(request);
  }
  else
  {
    ei_x_buff response;
    ei_x_new(&response);
    ei_x_format(&response, "{error, {unknown_function, ~i}}", function);
    return response;
  }
}
