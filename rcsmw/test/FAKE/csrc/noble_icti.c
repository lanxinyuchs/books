/* ----------------------------------------------------------------------
 * %CCaseFile:	noble_icti.c %
 * %CCaseRev:	/main/R2A/R3A/R6A/3 %
 * %CCaseDate:	2016-08-16 %
 * %CCaseDocNo: %
 * Author:	erarafo
 *
 * Short description: Handles upgrade for instances belonging to
 * the NOBLE MOM fragment.
 *
 * Another small task of this program is to set a value for the
 * Krypton=1 atomicNumber attribute. This attribute is a cached
 * runtime attribute.
 * ----------------------------------------------------------------------
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
 * R2A/1      2014-01-30 erarafo     First version
 * R2A/2      2014-02-04 erarafo     Upgrade support, preliminary
 * R2A/3      2014-02-05 erarafo     Upgrade support, accurate
 * R2A/4      2014-02-06 erarafo     Upgrade support for class Francium
 * R2A/5      2014-02-06 erarafo     Fixed faulty upgrade case
 * R2A/6      2014-02-06 erarafo     Silence is golden
 * R2A/7      2014-02-06 erarafo     Fixed faulty upgrade case
 * R2A/8      2014-02-06 erarafo     Adjusted to support To-version 1.2.2
 * R2A/9      2014-03-24 erarafo     New AUE model; support for Noble 2.2.0
 * R2A/10     2014-03-24 erarafo     Support for Noble 2.3.0
 * R2A/11     2014-03-29 erarafo     Added noble_icti_dyn (work in progress)
 * R2A/12     2014-03-31 erarafo     Refactoring
 * R2A/13     2014-03-31 erarafo     Simple banner added
 * R6A/3      2016-08-16 erarafo     Hidden class, no XML support, demo
 * ----------------------------------------------------------------------
 */

#define PROGRAM_NAME "noble_icti"
#define LOG "noble_icti"

#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif

#include "ugt_common.h"

#include <saImmOi.h>
#include <saImmOm.h>


static void
silentIdle() {
  while (true) {
    sleep(60);
  }
}


/**
 * TODO, duplicated from IFT test_avc.c 2014-07-01
 */
static SaNameT *saName(const char *name) {
  SaNameT *result = (SaNameT *)calloc(1, sizeof(SaNameT));
  size_t len = strlen(name);
  const unsigned int n = len > SA_MAX_NAME_LENGTH ? SA_MAX_NAME_LENGTH : len;
  result->length = (SaUint16T)len;
  for (unsigned int j = 0; j < n; j++) {
    result->value[j] = (SaUint8T)name[j];
  }
  return result;
}

/**
 * TODO, duplicated from IFT test_avc.c 2014-07-01
 */
static SaImmAttrModificationT_2 **singleMod() {
  SaImmAttrModificationT_2 **result =
      (SaImmAttrModificationT_2 **)calloc(
          1+1,
          sizeof(SaImmAttrModificationT_2 *));
  return result;
}

/**
 * TODO, duplicated from IFT test_avc.c 2014-07-01
 */
static SaImmAttrValuesT_2 *attrValuesSingleUint32(const char *attrName, unsigned int k) {
  SaUint32T *boxedInt = (SaUint32T *)calloc(1, sizeof(SaUint32T));
  *boxedInt = k;
  void **values = (void **)calloc(1, sizeof(void *));
  *values = (void *)boxedInt;
  SaImmAttrValuesT_2 *result = (SaImmAttrValuesT_2 *)calloc(1, sizeof(SaImmAttrValuesT_2));
  result->attrName = (char *)attrName;
  result->attrValueType = SA_IMM_ATTR_SAUINT32T;
  result->attrValuesNumber = 1;
  result->attrValues = values;
  return result;
}

/**
 * TODO, duplicated from IFT test_avc.c 2014-07-01
 */
static SaImmAttrModificationT_2 *attrValueSetOne(const char *attrName, unsigned int k) {
  SaImmAttrModificationT_2 *result = (SaImmAttrModificationT_2 *)calloc(1, sizeof(SaImmAttrModificationT_2));
  SaImmAttrValuesT_2 *av = attrValuesSingleUint32(attrName, k);
  result->modAttr.attrName = av->attrName;                        // "fat" assignment should work too
  result->modAttr.attrValueType = av->attrValueType;
  result->modAttr.attrValuesNumber = av->attrValuesNumber;
  result->modAttr.attrValues = av->attrValues;
  result->modType = SA_IMM_ATTR_VALUES_REPLACE;
  return result;
}



static void setKryptonAtomicNumber(unsigned int number) {

  SaVersionT version = {
      .releaseCode = 'A',
      .majorVersion = 2,
      .minorVersion = 11
  };

  SaImmOiHandleT handle;
  const SaImmOiCallbacksT_2 *callbacks = NULL;
  SaAisErrorT r1 = saImmOiInitialize_2(&handle, callbacks, &version);

  if (r1 != SA_AIS_OK) {
    APPLOG(LOG, "initialize for IMM OI, failed: %d", r1);
    return;
  } else {
    APPLOG(LOG, "initialize for IMM OI, success; release code: %d, major: %d, minor: %d",
        version.releaseCode, version.majorVersion, version.minorVersion);
  }

  const char *dn = "kryptonId=1,argonId=1,neonId=1,NOBLEheliumId=1";
  SaNameT *objectName = saName(dn);

  const SaImmAttrModificationT_2 **mods = (const SaImmAttrModificationT_2 **)singleMod();
  const char *attrName = "atomicNumber";
  mods[0] = attrValueSetOne(attrName, number);

  SaAisErrorT r2 = saImmOiRtObjectUpdate_2(handle, objectName, mods);
  if (r2 != SA_AIS_OK) {
    APPLOG(LOG, "set attribute failed, dn: %s, attr: %s, value: %u, code: %d", dn, attrName, number, r2);
  }
  else {
    APPLOG(LOG, "set attribute successful, dn: %s, attr: %s, value: %u", dn, attrName, number);
  }

  SaAisErrorT r3 = saImmOiFinalize(handle);
  if (r3 != SA_AIS_OK) {
    APPLOG(LOG, "IMM OI finalize, failed: %d", r3);
  } else {
    APPLOG(LOG, "IMM OI finalize, %s", "success");
  }
}


/**
 * Returns the wall clock second, counting from midnight.
 * The returned value is non-negative and wraps back to 0 at
 * midnight.
 *
 * This function is used for demonstrating the hidden-object
 * cache mechanism based in the HiddenNobleCache class.
 */
static SaInt32T
wallClockSecond() {
  struct timespec res;
  if (clock_gettime(CLOCK_REALTIME, &res) == 0) {
    struct tm lt;
    localtime_r(&(res.tv_sec), &lt);
    return (lt.tm_hour*60 + lt.tm_min)*60 + lt.tm_sec;
  }
  else {
    return -1;
  }
}


/**
 * Checks if the hiddenNobleCacheId=1 instance exists. The returned value
 * is 1 if it exists, 0 if it does not exist, or -1 in case of failure.
 */
static int existsInstance() {
  SaImmHandleT immHandle;
  {
    SaVersionT version = {.releaseCode = 'A', .majorVersion = 2, .minorVersion = 0};
    SaAisErrorT r = saImmOmInitialize(&immHandle, NULL, &version);
    if (r != SA_AIS_OK) {
      APPLOG(LOG, "existsInstance: failed to initialize for OM, code: %d", r);
      return -1;
    }
  }

  SaImmAccessorHandleT accessorHandle;
  {
    SaAisErrorT r = saImmOmAccessorInitialize(immHandle, &accessorHandle);
    if (r != SA_AIS_OK) {
      APPLOG(LOG, "existsInstance: failed to initialize OM accessor, code: %d", r);
      return -1;
    }
  }

  int result;
  {
    char *objectNameChar = "hiddenNobleCacheId=1";
    int objectNameLength = strlen(objectNameChar);
    SaNameT objectName = {.length = objectNameLength};
    for (int k = 0; k < objectNameLength; k++) {
      objectName.value[k] = (SaUint8T)objectNameChar[k];
    }
    SaImmAttrNameT attributeNames[] = {NULL};
    SaImmAttrValuesT_2 **attributes = NULL;
    SaAisErrorT r = saImmOmAccessorGet_2(
        accessorHandle,
        &objectName,
        attributeNames,
        &attributes);
    if (r == SA_AIS_ERR_NOT_EXIST) {
      APPLOG(LOG, "existsInstance: %s", "instance does not exist");
      result = 0;
    }
    else if (r != SA_AIS_OK) {
      APPLOG(LOG, "existsInstance: failed to access instance, code: %d", r);
      result = -1;
    }
    else {
      APPLOG(LOG, "existsInstance: %s", "instance exists");
      result = 1;
    }
  }

  {
    SaAisErrorT r = saImmOmAccessorFinalize(accessorHandle);
    if (r != SA_AIS_OK) {
       APPLOG(LOG, "failed to finalize accessor, code: %d", r);
       return -1;
     }
  }

  {
    SaAisErrorT r = saImmOmFinalize(immHandle);
    if (r != SA_AIS_OK) {
      APPLOG(LOG, "failed to finalize, code: %d", r);
      return -1;
    }
  }
  return result;
}


/**
 * Updates hiddenNobleCacheId=1.x with the given value. The
 * returned value is 0, or -1 in case of failure.
 */
static int updateInstance(SaInt32T xValue) {
  SaImmOiHandleT immOiHandle;
  {
    SaVersionT version = {
        .releaseCode = 'A',
        .majorVersion = 2,
        .minorVersion = 0
    };
    SaAisErrorT r = saImmOiInitialize_2(&immOiHandle, NULL, &version);
    if (r != SA_AIS_OK) {
      APPLOG(LOG, "updateInstance: failed to get OI handle for update, code: %d", r);
      return -1;
    }
  }

  {
    SaAisErrorT r = saImmOiImplementerSet(immOiHandle, "HiddenNobleCacheImplementer");
    if (r != SA_AIS_OK) {
      APPLOG(LOG, "updateInstance: failed to set object implementer for update, code: %d", r);
      return -1;
    }
  }

  {
    char *objectNameChar = "hiddenNobleCacheId=1";
    int objectNameLength = strlen(objectNameChar);
    SaNameT objectName = {.length = objectNameLength};
    for (int k = 0; k < objectNameLength; k++) {
      objectName.value[k] = (SaUint8T)objectNameChar[k];
    }
    SaImmAttrValueT valuesX[] = {(SaImmAttrValueT)&xValue};
    SaImmAttrValuesT_2 attrValues = {
        .attrName = "x",
        .attrValueType = SA_IMM_ATTR_SAINT32T,
        .attrValuesNumber = 1,
        .attrValues = valuesX
    };
    SaImmAttrModificationT_2 attrMod = {
        .modType = SA_IMM_ATTR_VALUES_REPLACE,
        .modAttr = attrValues
    };
    SaImmAttrModificationT_2 *attrMods[] = {&attrMod, NULL};
    SaAisErrorT r = saImmOiRtObjectUpdate_2(
        immOiHandle,
        &objectName,
        (const SaImmAttrModificationT_2 **)attrMods);
    if (r != SA_AIS_OK) {
      APPLOG(LOG, "updateInstance: failed to update runtime instance, code: %d", r);
      return -1;
    }
    else {
      APPLOG(LOG, "updateInstance: successfully updated instance: %s, with attribute x: %d",
          "hiddenNobleCacheId=1",
          xValue);
    }
  }

  {
    SaAisErrorT r = saImmOiImplementerClear(immOiHandle);
    if (r != SA_AIS_OK) {
      APPLOG(LOG, "failed to clear object implementer, code: %d", r);
      return -1;
    }
  }

  {
    SaAisErrorT r = saImmOiFinalize(immOiHandle);
    if (r != SA_AIS_OK) {
      APPLOG(LOG, "failed to finalize OI handle, code: %d", r);
      return -1;
    }
  }
  return 0;
}


/**
 * Creates the HiddenNobleCache class. The returned value is 0, or -1 in
 * case of failure.
 */
static int createClass() {
  SaImmHandleT immHandle;
  {
    SaVersionT version = {
        .releaseCode = 'A',
        .majorVersion = 2,
        .minorVersion = 0};
    SaAisErrorT r = saImmOmInitialize(&immHandle, NULL, &version);
    if (r != SA_AIS_OK) {
      APPLOG(LOG, "createClass: failed to initialize for OM, code: %d", r);
      return -1;
    }
  }

  {
    const SaImmClassNameT className = "HiddenNobleCache";
    SaImmAttrDefinitionT_2 attrDefRdn = {
        .attrName = "hiddenNobleCacheId",
        .attrValueType = SA_IMM_ATTR_SASTRINGT,
        .attrFlags = SA_IMM_ATTR_RDN | SA_IMM_ATTR_RUNTIME | SA_IMM_ATTR_CACHED,
        .attrDefaultValue = NULL
    };
    SaImmAttrDefinitionT_2 attrDefX = {
        .attrName = "x",
        .attrValueType = SA_IMM_ATTR_SAINT32T,
        .attrFlags = SA_IMM_ATTR_RUNTIME | SA_IMM_ATTR_PERSISTENT | SA_IMM_ATTR_CACHED,
        .attrDefaultValue = NULL
    };
    SaImmAttrDefinitionT_2 *attrDefs[] = {&attrDefRdn, &attrDefX, NULL};
    SaAisErrorT r = saImmOmClassCreate_2(
        immHandle,
        className,
        SA_IMM_CLASS_RUNTIME,
        (const SaImmAttrDefinitionT_2 **)attrDefs);

    if (r != SA_AIS_OK) {
      APPLOG(LOG, "createClass: failed to create class, code: %d", r);
      return -1;
    }
  }

  {
    SaAisErrorT r = saImmOmFinalize(immHandle);
    if (r != SA_AIS_OK) {
      APPLOG(LOG, "createClass: failed to finalize, code: %d", r);
      return -1;
    }
  }
  APPLOG(LOG, "created class: %s", "HiddenNobleCache");
  return 0;
}


/**
 * Creates the hiddenNobleCache=1 instance and sets the value of
 * the "x" attribute. The returned value is 0, or -1 in case of
 * failure.
 */
static int createInstance(SaInt32T xValue) {
  SaImmOiHandleT immOiHandle;
  {
    SaVersionT version = {.releaseCode = 'A', .majorVersion = 2, .minorVersion = 0};
    SaAisErrorT r = saImmOiInitialize_2(&immOiHandle, NULL, &version);
    if (r != SA_AIS_OK) {
      APPLOG(LOG, "createInstance: failed to get OI handle, code: %d", r);
      return -1;
    }
  }

  {
    SaAisErrorT r = saImmOiImplementerSet(immOiHandle, "HiddenNobleCacheImplementer");
    if (r != SA_AIS_OK) {
      APPLOG(LOG, "createInstance: failed to set object implementer, code: %d", r);
      return -1;
    }
  }

  {
    const SaImmClassNameT className = "HiddenNobleCache";
    char *str = "1";
    char *boxedStr[] = {str};
    SaImmAttrValueT valuesRdn[] = {(SaImmAttrValueT)boxedStr};
    SaImmAttrValuesT_2 rdn = {
        .attrName = "hiddenNobleCacheId",
        .attrValueType = SA_IMM_ATTR_SASTRINGT,
        .attrValuesNumber = 1,
        .attrValues = valuesRdn
    };
    SaImmAttrValueT valuesX[] = {(SaImmAttrValueT)&xValue};
    SaImmAttrValuesT_2 x = {
        .attrName = "x",
        .attrValueType = SA_IMM_ATTR_SAINT32T,
        .attrValuesNumber = 1,
        .attrValues = valuesX
    };
    SaImmAttrValuesT_2 *attrValues[] = {&rdn, &x, NULL};
    SaAisErrorT r = saImmOiRtObjectCreate_2(
        immOiHandle,
        className,
        NULL,
        (const SaImmAttrValuesT_2 **)attrValues);
    if (r != SA_AIS_OK) {
      APPLOG(LOG, "createInstance: failed to create runtime instance, code: %d", r);
      return -1;
    }
    else {
      APPLOG(LOG, "createInstance: successfully created instance: %s with attribute x: %d",
          "hiddenNobleCacheId=1",
          xValue);
    }
  }

  {
    SaAisErrorT r = saImmOiImplementerClear(immOiHandle);
    if (r != SA_AIS_OK) {
      APPLOG(LOG, "createInstance: failed to clear object implementer, code: %d", r);
      return -1;
    }
  }

  {
    SaAisErrorT r = saImmOiFinalize(immOiHandle);
    if (r != SA_AIS_OK) {
      APPLOG(LOG, "createInstance: failed to finalize OI handle, code: %d", r);
      return -1;
    }
  }
  return 0;
}


/**
 * Creates or updates the hiddenNobleCacheId=1.x value as appropriate.
 */
static void initHiddenInstance(SaInt32T xValue) {
  switch (existsInstance()) {
  case -1:
    silentIdle();
    break;

  case 0: {
    // create class and instance
    int r = createClass();
    if (r == -1) {
      silentIdle();
    }
    int s = createInstance(xValue);
    if (s == -1) {
      silentIdle();
    }
  }
  break;

  case 1: {
    // update existing instance
    int r = updateInstance(xValue);
    if (r == -1) {
      silentIdle();
    }
  }
  break;
  }
  APPLOG(LOG, "initialized hiddenNobleCache=1.x with value: %d", xValue);
}


/**
 * Returns the value of hiddenNobleCache=1.x, or -1 in case of failure.
 */
static SaInt32T readAttribute() {
  SaImmHandleT immHandle;
  {
    SaVersionT version = {.releaseCode = 'A', .majorVersion = 2, .minorVersion = 0};
    SaAisErrorT r = saImmOmInitialize(&immHandle, NULL, &version);
    if (r != SA_AIS_OK) {
      APPLOG(LOG, "readAttribute: failed to initialize for OM, code: %d", r);
      return -1;
    }
  }

  SaImmAccessorHandleT accessorHandle;
  {
    SaAisErrorT r = saImmOmAccessorInitialize(immHandle, &accessorHandle);
    if (r != SA_AIS_OK) {
      APPLOG(LOG, "readAttribute: failed to initialize OM accessor, code: %d", r);
      return -1;
    }
  }

  int result = -1;
  {
    char *objectNameChar = "hiddenNobleCacheId=1";
    int objectNameLength = strlen(objectNameChar);
    SaNameT objectName = {.length = objectNameLength};
    for (int k = 0; k < objectNameLength; k++) {
      objectName.value[k] = (SaUint8T)objectNameChar[k];
    }
    SaImmAttrNameT attributeNames[] = {"x", NULL};
    SaImmAttrValuesT_2 **attributes = NULL;
    SaAisErrorT r = saImmOmAccessorGet_2(
        accessorHandle,
        &objectName,
        attributeNames,
        &attributes);
    if (r != SA_AIS_OK) {
      APPLOG(LOG, "readAttribute: failed to get attribute value, code: %d", r);
      return -1;
    }

    for (int j = 0; attributes[j] != NULL; j++) {
      if (STREQ(attributes[j]->attrName, "x")) {
        SaInt32T xValue = *((SaInt32T *)attributes[j]->attrValues[0]);
        result = xValue;
      }
    }
  }

  {
    SaAisErrorT r = saImmOmAccessorFinalize(accessorHandle);
    if (r != SA_AIS_OK) {
       APPLOG(LOG, "readAttribute: failed to finalize accessor, code: %d", r);
       return -1;
     }
  }

  {
    SaAisErrorT r = saImmOmFinalize(immHandle);
    if (r != SA_AIS_OK) {
      APPLOG(LOG, "readAttribute: failed to finalize, code: %d", r);
      return -1;
    }
  }
  return result;
}



/**
 * The value of hiddenNobleCacheId=1.x is fetched and displayed.
 */
static void readHiddenInstance() {
  SaInt32T r = readAttribute();
  if (r == -1) {
    silentIdle();
  }
  APPLOG(LOG, "value of HiddenNobleCacheId=1.x is: %d", r);
  SaInt32T boardUptime = wallClockSecond() - r;
  if (boardUptime < 0) {
    APPLOG(LOG, "%s", "cannot tell board uptime");
  }
  else {
    APPLOG(LOG, "board uptime is (maybe) %d s", boardUptime);
  }
}


int
main(void) {
  APPLOG(LOG, "--- %s starting, version: %s", PROGRAM_NAME, "%CCaseRev:	/main/R2A/R3A/R6A/3 %");
  const char *restartType = getenv("RESTART_TYPE");
  if (restartType == NULL) {
    APPLOG(LOG, "CANNOT HAPPEN: restart type is %s", "undefined");
    silentIdle();
  }
  else {
    APPLOG(LOG, "restart type is: %s", restartType);
  }

  if (STREQ(restartType, "UPGRADE")) {
    APPLOG(LOG, "restart type is: %s, perform data conversion", restartType);
    RunConverterErrorT r = runConverter("noble_icti_dyn", LOG);
    if (r != RUN_CONVERTER_OK) {
      APPLOG(LOG, "failed to run converter, code: %u", r);
    }
    APPLOG(LOG, "%s", "successful conversion");
  }

  if (STREQ(restartType, "COLD") ||
      STREQ(restartType, "COLD_W_TEST") ||
      STREQ(restartType, "UPGRADE")) {
    initHiddenInstance(wallClockSecond());
  }
  else if (STREQ(restartType, "PGM") ||
      STREQ(restartType, "PGM_GRP") ||
      STREQ(restartType, "WARM")) {
    readHiddenInstance();
  }
  else {
    APPLOG(LOG, "CANNOT HAPPEN: restart type: %s", restartType);
    silentIdle();
  }

  // a real application would do useful work here
  setKryptonAtomicNumber(77);
  silentIdle();

  return 0;
}
