/* ----------------------------------------------------------------------
 * %CCaseFile:	noble_upi.c %
 * %CCaseRev:	/main/R3A/R4A/R5A/R6A/R8A/2 %
 * %CCaseDate:	2016-11-25 %
 * %CCaseDocNo: %
 * Author:	erarafo
 * Author: <name>, <e-mail address>
 *
 * Short description: Upgrade Interface program that performs some
 * actions over the IMM_OI interface prior to upgrade. This program
 * was introduced in order to test the handling of persistent
 * runtime attributes at upgrade.
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
 * R3A/1      2014-10-30 erarafo     First version
 * R3A/2      2014-10-30 erarafo     Improved logging; creation of instances
 * R3A/3      2014-10-31 erarafo     Bugfix
 * R3A/4      2014-11-04 erarafo     Extended
 * R3A/5      2014-11-10 erarafo     Fixed typo in modeling
 * R3A/7      2014-11-27 erarafo     Improved logging and robustness
 * R3A/8      2015-02-26 erarafo     Creating some bidirectional references
 * R3A/9      2015-03-03 erarafo     Two more bidirectional references
 * R3A/10     2015-04-20 erarafo     Two distinct bidir relations Tessin->Tamm
 * R3A/11     2015-04-22 erarafo     Setting 'implementer' properly for RT instances
 * R4A/1      2015-04-27 erarafo     Cover all persistent RT cases
 * R4A/2      2015-05-22 erarafo     Creation of Fleming and Ribbing instances
 * R4A/3      2015-05-29 erarafo     Adding a bidir reference to a grandchild
 * R5A/1      2016-03-24 erarafo     Code example using saImmOmAccessorGet_2 added
 * R5A/2      2016-03-30 erarafo     Refactoring
 * R6A/1      2016-07-12 etxpeno     Coverity fixes (a lot of them remains)
 * R8A/1      2016-11-01 erarafo     Cases of instance creation
 * R8A/2      2016-11-25 erarafo     Setup for test of safcImmCtReadInstances_2()
 * ----------------------------------------------------------------------
 */

#define _GNU_SOURCE
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>

#include <saAis.h>
#include <saImmOi.h>
#include <saImmOm.h>

#define STREQ(A, B)  strcmp(A, B) == 0

typedef SaVersionT versionT;
typedef SaImmOiHandleT oiHandleT;
typedef SaNameT *nameT;
typedef const SaNameT *nameCT;
typedef SaImmAttrValuesT_2 *attrBindingT;
typedef attrBindingT *attrBindingsT;
typedef const SaImmAttrValuesT_2 **attrBindingsCT;

typedef const SaImmAttrModificationT_2 **attrModsCT;


static time_t now = 0;

static char *prefixedRdnValue(
    const char *rdnValue) {
  // single-threaded execution is trusted
  if (now == 0) {
    now = time(NULL);
  }
  char *result;
  asprintf(&result, "%ld_%s", now, rdnValue);
  return result;
}



/**
 * Creates a DN where the leaf part will differ for each
 * invocation of this program (to 1 s resolution).
 * A call
 *
 *     createDn("fooId", "ABC", "neonId=1,NOBLEheliumId=1")
 *
 * will return a string such as
 *
 *     "fooId=458765_ABC,neonId=1,NOBLEheliumId=1"
 */
static char *createDn(
    const char *rdnName,
    const char *rdnValue,
    const char *parentName) {
  char *result;
  asprintf(&result,
      "%s=%s,%s",
      rdnName,
      prefixedRdnValue(rdnValue),
      parentName);
  return result;
}


static void logString(char *log, char *string) {
  FILE *f = fopen(log, "a");
  fprintf(f, "%s\n", string);
  fclose(f);
}


static void logString2(char *log, char *string, char *string2) {
  FILE *f = fopen(log, "a");
  fprintf(f, "%s: %s\n", string, string2);
  fclose(f);
}


static void logStringC(char *log, char *string, SaAisErrorT code) {
  FILE *f = fopen(log, "a");
  fprintf(f, "%s: %d\n", string, code);
  fclose(f);
}


static void logString2C(char *log, char *string, char *string2, SaAisErrorT code) {
  FILE *f = fopen(log, "a");
  fprintf(f, "%s: %s: %d\n", string, string2, code);
  fclose(f);
}

static void logStringInt(char *log, char *string, int int1) {
  FILE *f = fopen(log, "a");
  fprintf(f, "%s%d\n", string, int1);
  fclose(f);
}


static char *decodeError(SaAisErrorT code) {
  switch (code) {
  SA_AIS_OK: return "ok";
  SA_AIS_ERR_LIBRARY: return "library";
  SA_AIS_ERR_VERSION: return "version";
  SA_AIS_ERR_INIT: return "init";
  SA_AIS_ERR_TIMEOUT: return "timeout";
  SA_AIS_ERR_TRY_AGAIN: return "try again";
  SA_AIS_ERR_INVALID_PARAM: return "invalid param";
  SA_AIS_ERR_NO_MEMORY: return "no memory";
  SA_AIS_ERR_BAD_HANDLE: return "bad handle";
  SA_AIS_ERR_BUSY: return "busy";
  SA_AIS_ERR_ACCESS: return "access";
  SA_AIS_ERR_NOT_EXIST: return "not exist";
  SA_AIS_ERR_NAME_TOO_LONG: return "name too long";
  SA_AIS_ERR_EXIST: return "exist";
  SA_AIS_ERR_NO_SPACE: return "no space";
  SA_AIS_ERR_INTERRUPT: return "interrupt";
  SA_AIS_ERR_NAME_NOT_FOUND: return "name not found";
  SA_AIS_ERR_NO_RESOURCES: return "no resources";
  SA_AIS_ERR_NOT_SUPPORTED: return "not supported";
  SA_AIS_ERR_BAD_OPERATION: return "bad operation";
  SA_AIS_ERR_FAILED_OPERATION: return "failed operation";
  SA_AIS_ERR_MESSAGE_ERROR: return "message error";
  SA_AIS_ERR_QUEUE_FULL: return "queue full";
  SA_AIS_ERR_QUEUE_NOT_AVAILABLE: return "queue not available";
  SA_AIS_ERR_BAD_FLAGS: return "bad flags";
  SA_AIS_ERR_TOO_BIG: return "too big";
  SA_AIS_ERR_NO_SECTIONS: return "no sections";
  SA_AIS_ERR_NO_OP: return "no op";
  SA_AIS_ERR_REPAIR_PENDING: return "repair pending";
  SA_AIS_ERR_NO_BINDINGS: return "no bindings";
  SA_AIS_ERR_UNAVAILABLE: return "unavailable";
  SA_AIS_ERR_CAMPAIGN_ERROR_DETECTED: return "campaign error detected";
  SA_AIS_ERR_CAMPAIGN_PROC_FAILED: return "campaign proc failed";
  SA_AIS_ERR_CAMPAIGN_CANCELED: return "campaign canceled";
  SA_AIS_ERR_CAMPAIGN_FAILED: return "campaign failed";
  SA_AIS_ERR_CAMPAIGN_SUSPENDED: return "campaign suspended";
  SA_AIS_ERR_CAMPAIGN_SUSPENDING: return "campaign suspending";
  SA_AIS_ERR_ACCESS_DENIED: return "access denied";
  SA_AIS_ERR_NOT_READY: return "not ready";
  SA_AIS_ERR_DEPLOYMENT: return "deployment";
  default: {
      char *result;
      asprintf(&result, "unknown code: %d", code);
      return result;
    }
  }
}

static void doInspect(char *log) {
  char *names[] = {
      "APP_TMP",
      "BT",
      "CXP_PATH",
      "CXP_REV",
      "CXC_NAME",
      "CXC_NO",
      "CXC_REV",
      "LD_LIBRARY_PATH",
      "LOG_DIR",
      "RESTART_TYPE",
      NULL
  };
  FILE *f = fopen(log, "a");
  for (unsigned int j = 0; names[j] != NULL; j++) {
    char *v = getenv(names[j]);
    if (v == NULL) {
      fprintf(f, "%s: %s\n", names[j], "<<null>>");
    }
    else {
      fprintf(f, "%s: %s\n", names[j], v);
    }
  }
  fclose(f);
}


static nameT saName(const char *name) {
  nameT result = (nameT)calloc(1, sizeof(SaNameT));
  size_t len = strlen(name);
  const unsigned int n = len > SA_MAX_NAME_LENGTH ? SA_MAX_NAME_LENGTH : len;
  result->length = (SaUint16T)len;
  for (unsigned int j = 0; j < n; j++) {
    result->value[j] = (SaUint8T)name[j];
  }
  return result;
}


static void oiInitialize(char *log, oiHandleT *handle) {
    versionT version = {
        .releaseCode = 'A',
        .majorVersion = 2,
        .minorVersion = 11
    };
    const SaImmOiCallbacksT_2 *callbacks = NULL;
    SaAisErrorT r = saImmOiInitialize_2(handle, callbacks, &version);
    if (r != SA_AIS_OK) {
      logString(log, "FATAL #############");
      printf("ERROR: failed to initialize OI session");
      exit(0);
    }
}


static void oiFinalize(char *log, oiHandleT handle) {
  SaAisErrorT r = saImmOiFinalize(handle);
  if (r != SA_AIS_OK) {
    logString(log, "FATAL #############");
    printf("ERROR: failed to finalize OI session");
    exit(0);
  }
}





static attrBindingT createAttrBindingStringSingle(char *name, char *value) {
  char *z = strdup(value);
  char **u = (char **)calloc(1, sizeof(char *));
  *u = z;
  attrBindingT result = (attrBindingT)calloc(1, sizeof(SaImmAttrValuesT_2));
  result->attrName = name;
  result->attrValuesNumber = 1;
  result->attrValueType = SA_IMM_ATTR_SASTRINGT;
  result->attrValues = calloc(1, sizeof(SaImmAttrValueT));
  *(result->attrValues) = (SaImmAttrValueT)u;
  return result;
}


static attrBindingT createAttrBindingUint32Single(char *name, SaUint32T value) {
  SaUint32T *u = (SaUint32T *)calloc(1, sizeof(SaUint32T));
  *u = value;
  attrBindingT result = (attrBindingT)calloc(1, sizeof(SaImmAttrValuesT_2));
  result->attrName = name;
  result->attrValuesNumber = 1;
  result->attrValueType = SA_IMM_ATTR_SAUINT32T;
  result->attrValues = calloc(1, sizeof(SaImmAttrValueT));
  *(result->attrValues) = (SaImmAttrValueT)u;
  return result;
}

static attrBindingT createAttrBindingInt32None(char *name) {
  attrBindingT result = (attrBindingT)calloc(1, sizeof(SaImmAttrValuesT_2));
  result->attrName = name;
  result->attrValuesNumber = 0;
  result->attrValueType = SA_IMM_ATTR_SAINT32T;
  result->attrValues = NULL;
  return result;
}

static attrBindingT createAttrBindingInt32Single(char *name, SaInt32T value) {
  SaInt32T *m = (SaInt32T *)calloc(1, sizeof(SaInt32T));
  *m = value;
  attrBindingT result = (attrBindingT)calloc(1, sizeof(SaImmAttrValuesT_2));
  result->attrName = name;
  result->attrValuesNumber = 1;
  result->attrValueType = SA_IMM_ATTR_SAINT32T;
  result->attrValues = calloc(1, sizeof(SaImmAttrValueT));
  *(result->attrValues) = (SaImmAttrValueT)m;
  return result;
}

static unsigned int stringListLength(char **m) {
  unsigned int result = 0;
  for (char **p = m; *p != NULL;  p++) {
    result++;
  }
  return result;
}


static attrBindingT createAttrBindingMultiDn(char *name, char **dns) {
  unsigned int nDns = stringListLength(dns);
  attrBindingT result = (attrBindingT)calloc(1, sizeof(SaImmAttrValuesT_2));
  result->attrName = name;
  result->attrValuesNumber = nDns;
  result->attrValueType = SA_IMM_ATTR_SANAMET;
  nameT *values = (nameT *)calloc(nDns, sizeof(SaImmAttrValueT));
  char **q = dns;
  for (unsigned int j = 0; j < nDns; j++, q++) {
    values[j] = saName(*q);
  }
  result->attrValues = (SaImmAttrValueT *)values;
  return result;
}


static attrBindingsT nilAttrBindings() {
  return calloc(1, sizeof(attrBindingT));
}









static attrBindingsT consAttrBinding(attrBindingT a, attrBindingsT m) {
  unsigned int n = 0;
  for (attrBindingsT p = m; *p != NULL; p++) {
    n++;
  }
  attrBindingsT result = (attrBindingsT)calloc(n+1+1, sizeof(attrBindingT));
  *result = a;
  attrBindingsT r = result;
  for (attrBindingsT q = m; *q != NULL; q++) {
    r++;
    *r = *q;
  }
  return result;
}


static SaImmAttrModificationT_2 **oneAddString(char *log, char *attrName, char *attrValue) {
  attrBindingT b =
      createAttrBindingStringSingle(attrName, attrValue);
  SaImmAttrModificationT_2 *mod = calloc(1, sizeof(SaImmAttrModificationT_2));
  mod->modType = SA_IMM_ATTR_VALUES_REPLACE;
  mod->modAttr.attrName = b->attrName;
  mod->modAttr.attrValueType = b->attrValueType;
  mod->modAttr.attrValuesNumber = b->attrValuesNumber;
  mod->modAttr.attrValues = b->attrValues;
  free(b);
  SaImmAttrModificationT_2 **result =
      (SaImmAttrModificationT_2 **)calloc(1+1, sizeof(SaImmAttrModificationT_2 *));
  *result = mod;

  logString2(log, "attribute name", result[0]->modAttr.attrName);
  logStringC(log, "attribute type", result[0]->modAttr.attrValueType);
  logStringC(log, "attribute mult", result[0]->modAttr.attrValuesNumber);
  logString2(log, "attribute value", *((char **)(result[0]->modAttr.attrValues[0])));



  return result;
}


static SaAisErrorT setAttrRuntimeString(
    char *log,
    oiHandleT handle,
    char *dn,
    char *attrName,
    char *attrValue) {
  SaImmAttrModificationT_2 **mods = oneAddString(log, attrName, attrValue);
  SaNameT *objectName = saName(dn);
  SaAisErrorT r = saImmOiRtObjectUpdate_2(handle, objectName, (const SaImmAttrModificationT_2 **)mods);
  free(mods);
  free(objectName);
  return r;
}







static void createRuntimeInstance(
    char *log,
    oiHandleT handle,
    char *classname,
    char *parentDn,
    attrBindingsT bindings) {
  SaNameT *objectName = saName(parentDn);
  SaAisErrorT r = saImmOiRtObjectCreate_2(
      handle,
      classname,
      objectName,
      (attrBindingsCT)bindings);
  free(objectName);
  if (r == SA_AIS_ERR_EXIST) {
    // this may be OK if the node has been through a previous test case
    logString2(log, "failed to create already existing instance of", classname);
  }
  else if (r != SA_AIS_OK) {
    logString2C(log, "failed to create instance of", classname, r);
    oiFinalize(log, handle);
    logString(log, "FATAL #############");
    printf("ERROR: failed to create instance of: %s, reason: %s\n",
        classname,
        decodeError(r));
    exit(0);
  }
  else {
    logString2(log, "created instance of", classname);
  }
}

#define BAD_HANDLE 32987349734ULL

static SaImmHandleT getOmHandle(char *log) {
  SaVersionT ver = {.releaseCode='A', .majorVersion=2, .minorVersion=11};
  SaImmHandleT immHandle;
  SaAisErrorT r = saImmOmInitialize(&immHandle, NULL, &ver);
  if (r != SA_AIS_OK) {
    logStringC(log, "failed to do OM INIT", r);
    return BAD_HANDLE;
  }
  else {
    logString(log, "successful OM INIT");
    return immHandle;
  }
}


/**
 * Modifies a config instance and returns its DN, or NULL
 * in case of failure.
 */
static char *modifyConfigInstance(
    char *log,
    const char *instanceName,
    attrModsCT mods) {

  SaImmHandleT immHandle = getOmHandle(log);
  if (immHandle == BAD_HANDLE) {
    logString(log, "failed to do OM INIT");
    return NULL;
  }
  else {
    logString(log, "successful OM INIT");
  }

  // ao init
  SaImmAdminOwnerHandleT ownerHandle;
  SaAisErrorT r6 = saImmOmAdminOwnerInitialize(immHandle, "mendelev", SA_TRUE, &ownerHandle);
  if (r6 != SA_AIS_OK) {
    logStringC(log, "failed to do OM ADMIN OWNER INIT", r6);
    saImmOmFinalize(immHandle);
    return NULL;
  }
  else {
    logString(log, "successful OM ADMIN OWNER INIT");
  }

  // ccb init
  SaImmCcbHandleT ccbHandle;
  SaAisErrorT r7 = saImmOmCcbInitialize(ownerHandle, 0, &ccbHandle);
  if (r7 != SA_AIS_OK) {
    logStringC(log, "failed to do CCB INIT", r7);
    saImmOmAdminOwnerFinalize(ownerHandle);
    saImmOmFinalize(immHandle);
    return NULL;
  }
  else {
    logString(log, "successful CCB INIT");
  }

  nameCT ina = saName(instanceName);
  SaNameT **names = calloc(1+1, sizeof(SaNameT *));
  names[0] = (SaNameT *)ina;
  SaAisErrorT r75 = saImmOmAdminOwnerSet(ownerHandle, (const SaNameT **)names, SA_IMM_ONE);
  free(names);
  if (r75 != SA_AIS_OK) {
    logStringC(log, "failed to take ownership of instance to be modified", r75);
    saImmOmAdminOwnerFinalize(ownerHandle);
    saImmOmFinalize(immHandle);
    return NULL;
  }
  else {
    logString(log, "successful ownership of instance to be modified");
  }


  SaAisErrorT r8 = saImmOmCcbObjectModify_2(
      ccbHandle,
      ina,
      mods);

  if (r8 != SA_AIS_OK) {
    // code 20 is SA_AIS_ERR_BAD_OPERATION
    logStringC(log, "failed to do CCB OBJECT MODIFY", r8);
    saImmOmAdminOwnerFinalize(ownerHandle);
    saImmOmFinalize(immHandle);
    return NULL;
  }
  else {
    logString(log, "successful CCB OBJECT MODIFY");
  }

  // ccb apply
  SaAisErrorT r9 = saImmOmCcbApply(ccbHandle);
  if (r9 != SA_AIS_OK) {
    logStringC(log, "failed to do CCB APPLY", r9);
    saImmOmAdminOwnerFinalize(ownerHandle);
    saImmOmFinalize(immHandle);
    return NULL;
  }
  else {
    logString(log, "successful CCB APPLY");
  }


  // This should not be required since we do saImmOmFinalize() below!
  // TODO, might be a bug !!!!!!!!!!!!!

  SaAisErrorT r98 = saImmOmAdminOwnerFinalize(ownerHandle);
  if (r98 != SA_AIS_OK) {
    logStringC(log, "failed to do ADMIN OWNER FINALIZE", r98);
  }
  else {
    logString(log, "successful ADMIN OWNER FINALIZE");
  }


  // ccb fin
  // ao fin
  // om fin
  SaAisErrorT r99 = saImmOmFinalize(immHandle);
  if (r99 != SA_AIS_OK) {
    logStringC(log, "failed to do OM FINALIZE", r99);
    return NULL;
  }
  else {
    logString(log, "successful OM FINALIZE");
    logString2(log, "modified", (char *)instanceName);
    return (char *)instanceName;
  }
}


/**
 * Creates a config instance and returns its DN, or NULL
 * in case of failure.
 */
static char *createConfigInstance(
    char *log,
    char *classname,
    char *parent,
    char *rdnName,
    char *rdnValue,
    attrBindingsT bindings) {

  SaImmHandleT immHandle = getOmHandle(log);
  if (immHandle == BAD_HANDLE) {
    logString(log, "failed to do OM INIT");
    return NULL;
  }
  else {
    logString(log, "successful OM INIT");
  }

  // ao init
  SaImmAdminOwnerHandleT ownerHandle;
  SaAisErrorT r6 = saImmOmAdminOwnerInitialize(immHandle, "mendelev", SA_TRUE, &ownerHandle);
  if (r6 != SA_AIS_OK) {
    logStringC(log, "failed to do OM ADMIN OWNER INIT", r6);
    saImmOmFinalize(immHandle);
    return NULL;
  }
  else {
    logString(log, "successful OM ADMIN OWNER INIT");
  }

  // ccb init
  SaImmCcbHandleT ccbHandle;
  SaAisErrorT r7 = saImmOmCcbInitialize(ownerHandle, 0, &ccbHandle);
  if (r7 != SA_AIS_OK) {
    logStringC(log, "failed to do CCB INIT", r7);
    saImmOmAdminOwnerFinalize(ownerHandle);
    saImmOmFinalize(immHandle);
    return NULL;
  }
  else {
    logString(log, "successful CCB INIT");
  }

  nameT pna = saName(parent);

  SaNameT **names = calloc(1+1, sizeof(SaNameT *));
  *names = pna;


  SaAisErrorT r75 = saImmOmAdminOwnerSet(ownerHandle, (const SaNameT **)names, SA_IMM_ONE);
  free(names);
  if (r75 != SA_AIS_OK) {
    logStringC(log, "failed to take ownership of parent", r75);
    saImmOmAdminOwnerFinalize(ownerHandle);
    saImmOmFinalize(immHandle);
    return NULL;
  }
  else {
    logString(log, "successful ownership of parent");
  }

  attrBindingsT attrs =
      consAttrBinding(createAttrBindingStringSingle(rdnName, rdnValue),
          bindings);
  SaAisErrorT r8 = saImmOmCcbObjectCreate_2(
      ccbHandle,
      classname,
      pna,
      (const SaImmAttrValuesT_2 **)attrs);
  free(attrs);
  if (r8 != SA_AIS_OK) {
    // code 20 is SA_AIS_ERR_BAD_OPERATION
    logStringC(log, "failed to do CCB OBJECT CREATE", r8);
    saImmOmAdminOwnerFinalize(ownerHandle);
    saImmOmFinalize(immHandle);
    return NULL;
  }
  else {
    logString(log, "successful CCB OBJECT CREATE");
  }

  // ccb apply
  SaAisErrorT r9 = saImmOmCcbApply(ccbHandle);
  if (r9 != SA_AIS_OK) {
    logStringC(log, "failed to do CCB APPLY", r9);
    saImmOmAdminOwnerFinalize(ownerHandle);
    saImmOmFinalize(immHandle);
    return NULL;
  }
  else {
    logString(log, "successful CCB APPLY");
  }


  // This should not be required since we do saImmOmFinalize() below!
  // TODO, might be a bug !!!!!!!!!!!!!

  SaAisErrorT r98 = saImmOmAdminOwnerFinalize(ownerHandle);
  if (r98 != SA_AIS_OK) {
    logStringC(log, "failed to do ADMIN OWNER FINALIZE", r98);
  }
  else {
    logString(log, "successful ADMIN OWNER FINALIZE");
  }


  // ccb fin
  // ao fin
  // om fin
  SaAisErrorT r99 = saImmOmFinalize(immHandle);
  if (r99 != SA_AIS_OK) {
    logStringC(log, "failed to do OM FINALIZE", r99);
    return NULL;
  }
  else {
    logString(log, "successful OM FINALIZE");
    char *s2;
    asprintf(&s2, "%s=%s,%s", rdnName, rdnValue, parent);
    logString2(log, "created", s2);
    return s2;
  }
}


static char *doImmAtVerifyPreconditions(char *log) {

  //////////////////////////////////////
  // Create four TESTMOM instances

  {
    // no attributes specified at creation time; no attributes will
    // be presented when fetched with safcImmCtReadInstances()
    char *result =
        createConfigInstance(
            log,
            "TESTMOMTestClassA",
            "TESTMOMtestRootId=1",
            "testClassAId",
            "a",
            nilAttrBindings());
    if (result == NULL) {
      logString(log, "failed to create TestClass4 instance; expected if it already exists");
    }
  }

  {
    // One attribute specified at creation time; one attribute will
    // be presented when fetched with safcImmCtReadInstances(), or
    // all of them if using safcImmCtReadInstances_2()
    attrBindingT tcAbAttr = createAttrBindingInt32Single("i32", 77);
    char *tcAbResult =
        createConfigInstance(
            log,
            "TESTMOMTestClassA",
            "TESTMOMtestRootId=1",
            "testClassAId",
            "b",
            consAttrBinding(tcAbAttr, nilAttrBindings()));
    if (tcAbResult == NULL) {
      logString(log, "failed to create TestClassA=b instance; expected if it already exists");
    }
  }

  {
    // no attributes specified at creation time; however testClass4attr1
    // has a default value so that value will be inserted at creation time
    // and presented when fetched with safcImmCtReadInstances()
    char *tc4Result =
        createConfigInstance(
            log,
            "TESTMOMTestClass4",
            "TESTMOMtestRootId=1",
            "testClass4Id",
            "a",
            nilAttrBindings());
    if (tc4Result == NULL) {
      logString(log, "failed to create TestClass4 instance; expected if it already exists");
    }
  }

  {
    // create the instance with no attributes specified; the default gets
    // used; then modify the attribute, doing a REPLACE with no value for
    // testClass4attr1; the attribute will NOT be presented
    char *tc4Result =
        createConfigInstance(
            log,
            "TESTMOMTestClass4",
            "TESTMOMtestRootId=1",
            "testClass4Id",
            "b",
            nilAttrBindings());
    if (tc4Result == NULL) {
      logString(log, "failed to create TestClass4 instance; expected if it already exists");
    }

    const SaImmAttrModificationT_2 mod = {
        .modType = SA_IMM_ATTR_VALUES_REPLACE,
        .modAttr = {
            .attrName = "testClass4attr1",
            .attrValueType = SA_IMM_ATTR_SAINT32T,
            .attrValuesNumber = 0,
            .attrValues = NULL
        }
    };

    const SaImmAttrModificationT_2 *mods[] = {&mod, NULL};
    modifyConfigInstance(log, "testClass4Id=b,TESTMOMtestRootId=1", mods);

    {
      // create the instance with the testClass4attr1 attribute explicitly
      // specified to have no value; the attribute will now be presented
      // as a "no value" attribute when fetched with safcImmCtReadInstances()
      char *tc4Result =
          createConfigInstance(
              log,
              "TESTMOMTestClass4",
              "TESTMOMtestRootId=1",
              "testClass4Id",
              "c",
              consAttrBinding(
                  createAttrBindingInt32None("testClass4attr1"),
                  nilAttrBindings()));
      if (tc4Result == NULL) {
        logString(log, "failed to create TestClass4 instance; expected if it already exists");
      }
    }
  }


  //////////////////////////////////////
  // Create one Fleming instance under Helium=1

  char *fleming101Client =
      createDn("flemingId", "103",
          createDn("ribbingId", "201",
              createDn("flemingId", "101", "NOBLEheliumId=1")));
  char *fleming101Clients[] = {fleming101Client, NULL};
  char *fleming101 =
      createConfigInstance(log,
          "NOBLEFleming",
          "NOBLEheliumId=1",
          "flemingId",
          prefixedRdnValue("101"),
          consAttrBinding(
              createAttrBindingInt32Single("age", 43),
              consAttrBinding(
                  createAttrBindingMultiDn("reservedBy", fleming101Clients),
                  nilAttrBindings())));

  //////////////////////////////////////
  // Create one Ribbing instance under Fleming 101.

  char *ribbing201Client = createDn("ribbingId", "202", fleming101);
  char *ribbing201Clients[] = {ribbing201Client, NULL};
  char *ribbing201 =
      createConfigInstance(log,
          "NOBLERibbing",
          fleming101,
          "ribbingId",
          prefixedRdnValue("201"),
          consAttrBinding(
              createAttrBindingInt32Single("handicap", 7),
              consAttrBinding(
                  createAttrBindingMultiDn("reservedBy", ribbing201Clients),
                  nilAttrBindings())));

  //////////////////////////////////////
  // Create one Fleming instance under the Ribbing instance.
  // This Fleming instance will be a client of Fleming 101.

  char *fleming103MustHave[] = {fleming101, NULL};
  char *fleming103 =
      createConfigInstance(log,
          "NOBLEFleming",
          ribbing201,
          "flemingId",
          prefixedRdnValue("103"),
          consAttrBinding(
              createAttrBindingInt32Single("age", 44),
              consAttrBinding(
                  createAttrBindingMultiDn("mustHave", fleming103MustHave),
                  nilAttrBindings())));

  //////////////////////////////////////
  // Create a Ribbing sibling under Fleming 101,
  // which is a client of Ribbing 201.

  char *ribbing202Requires[] = {ribbing201, NULL};
  createConfigInstance(log,
      "NOBLERibbing",
      fleming101,
      "ribbingId",
      prefixedRdnValue("202"),
      consAttrBinding(
          createAttrBindingInt32Single("handicap", 22),
          consAttrBinding(
              createAttrBindingMultiDn("requires", ribbing202Requires),
              nilAttrBindings())));

  //////////////////////////////////////
  // Create a Ribbing instance under Fleming 103.
  // This instance will then be referred by its
  // grandparent Ribbing 201.

  //char *ribbing201Client = createDn("ribbingId", "202", fleming101);
  char *ribbing401Clients[] = {ribbing201, NULL};
  char *ribbing401 =
      createConfigInstance(log,
          "NOBLERibbing",
          fleming103,
          "ribbingId",
          prefixedRdnValue("401"),
          consAttrBinding(
              createAttrBindingInt32Single("handicap", 8),
              consAttrBinding(
                  createAttrBindingMultiDn("reservedBy", ribbing401Clients),
                  nilAttrBindings())));

  //////////////////////////////////////
  // Add a 'requires' attribute to the
  // previously created Ribbing 201 instance.
  // Ought to be procedurally encapsulated... TODO

  // FIXME
  // OM init

  SaImmHandleT immHandle = getOmHandle(log);
  if (immHandle == BAD_HANDLE) {
    logString(log, "failed to do OM INIT");
    return strdup("failed to do OM INIT");  // TODO, why strdup????
  }
  else {
    logString(log, "successful OM INIT");
  }

  // admin owner init
  SaAisErrorT r401;
  SaImmAdminOwnerHandleT ownerHandle;
  r401 = saImmOmAdminOwnerInitialize(immHandle, "doImmAtVerifyPreconditions", SA_TRUE, &ownerHandle);
  if (r401 != SA_AIS_OK) {
    char *r401Message = "failed to get IMM handle";
    logStringC(log, r401Message, r401);
    return strdup(r401Message);
  }
  else {
    logString(log, "got IMM handle");
  }

  // CCB init
  SaImmCcbFlagsT ccbFlags = 0;
  SaImmCcbHandleT ccbHandle;
  r401 = saImmOmCcbInitialize(ownerHandle, ccbFlags, &ccbHandle);
  if (r401 != SA_AIS_OK) {
    char *r401Message = "failed to get CCB handle";
    logStringC(log, r401Message, r401);
    return strdup(r401Message);
  }
  else {
    logString(log, "got CCB handle");
  }

  // Admin ownership of instance
  SaNameT * ribbing201Name = saName(ribbing201);
  SaNameT * ribbing201Names[] = {ribbing201Name, NULL};
  r401 = saImmOmAdminOwnerSet(ownerHandle, (const SaNameT **)ribbing201Names, SA_IMM_ONE);
  if (r401 != SA_AIS_OK) {
    char *r401Message = "failed to set admin owner";
    logStringC(log, r401Message, r401);
    return strdup(r401Message);
  }
  else {
    logString(log, "done set admin owner");
  }

  // CCB object modify
  char *ribbing401List[] = {ribbing401, NULL};
  attrBindingT requiresRibbing401 = createAttrBindingMultiDn("requires", ribbing401List);

  SaImmAttrModificationT_2 theMod;
  theMod.modType = SA_IMM_ATTR_VALUES_ADD;
  theMod.modAttr.attrName = requiresRibbing401->attrName;
  theMod.modAttr.attrValueType = requiresRibbing401->attrValueType;
  theMod.modAttr.attrValuesNumber = requiresRibbing401->attrValuesNumber;
  theMod.modAttr.attrValues = requiresRibbing401->attrValues;
  free(requiresRibbing401);

  SaImmAttrModificationT_2 *attrMods[] = {&theMod, NULL};
  r401 = saImmOmCcbObjectModify_2(ccbHandle, ribbing201Name, (const SaImmAttrModificationT_2 **)attrMods);
  if (r401 != SA_AIS_OK) {
    char *r401Message = "failed to CCB object modify";
    logStringC(log, r401Message, r401);
    return strdup(r401Message);
  }
  else {
    logString(log, "done CCB object modify");
  }

  // CCB apply
  r401 = saImmOmCcbApply(ccbHandle);
  if (r401 != SA_AIS_OK) {
    char *r401Message = "failed CCB apply";
    logStringC(log, r401Message, r401);
    return strdup(r401Message);
  }
  else {
    logString(log, "done CCB apply");
  }

  // CCB finalize
  r401 = saImmOmCcbFinalize(ccbHandle);
  if (r401 != SA_AIS_OK) {
    char *r401Message = "failed CCB finalize";
    logStringC(log, r401Message, r401);
    return strdup(r401Message);
  }
  else {
    logString(log, "done CCB finalize");
  }

  // admin owner fin
  r401 = saImmOmAdminOwnerFinalize(ownerHandle);
  if (r401 != SA_AIS_OK) {
    char *r401Message = "failed admin owner finalize";
    logStringC(log, r401Message, r401);
    return strdup(r401Message);
  }
  else {
    logString(log, "done admin owner finalize");
  }

  // OM fin
  r401 = saImmOmFinalize(immHandle);
  if (r401 != SA_AIS_OK) {
    char *r401Message = "failed IMM handle finalize";
    logStringC(log, r401Message, r401);
    return strdup(r401Message);
  }
  else {
    logString(log, "done IMM handle finalize");
  }


  //////////////////////////////////////
  // Create one Xenon instance (config class)
  // No action if the instance exists already

  createConfigInstance(log,
      "NOBLEXenon",
      "argonId=1,neonId=1,NOBLEheliumId=1",
      "xenonId",
      "77",
      nilAttrBindings());

  ///////////////////////////////////////////////
  // Create two Toll instances (config class)

  char *reservers33[] = {
      createDn("tammId", "11", "neonId=1,NOBLEheliumId=1"),
      createDn("tammId", "12", "neonId=1,NOBLEheliumId=1"),
      NULL};

  char *reservers34[] = {
      createDn("tammId", "12", "neonId=1,NOBLEheliumId=1"),
      NULL};

  createConfigInstance(log,
      "NOBLEToll",
      "argonId=1,neonId=1,NOBLEheliumId=1",
      "tollId",
      prefixedRdnValue("33"),
      consAttrBinding(createAttrBindingMultiDn("reservedBy", reservers33),
            nilAttrBindings()));

  createConfigInstance(log,
      "NOBLEToll",
      "argonId=1,neonId=1,NOBLEheliumId=1",
      "tollId",
      prefixedRdnValue("34"),
      consAttrBinding(createAttrBindingMultiDn("reservedBy", reservers34),
            nilAttrBindings()));


  ///////////////////////////////////////////////
  // Create two Tamm instances (config class)
  // Assumes Toll instances exist already

  char *uses11[] = {
      createDn("tollId", "33", "argonId=1,neonId=1,NOBLEheliumId=1"),
      NULL};

  char *uses12[] = {
      createDn("tollId", "33", "argonId=1,neonId=1,NOBLEheliumId=1"),
      createDn("tollId", "34", "argonId=1,neonId=1,NOBLEheliumId=1"),
      NULL};

  char *reservers11[] = {
      createDn("tessinId", "5", "NOBLEheliumId=1"),
      NULL};

  char *reservers12[] = {
      createDn("tessinId", "6", "NOBLEheliumId=1"),
      NULL};

  createConfigInstance(log,
      "NOBLETamm",
      "neonId=1,NOBLEheliumId=1",
      "tammId",
      prefixedRdnValue("11"),
      consAttrBinding(createAttrBindingMultiDn("reservedBy", reservers11),
          consAttrBinding(createAttrBindingMultiDn("uses", uses11),
              nilAttrBindings())));

  createConfigInstance(log,
      "NOBLETamm",
      "neonId=1,NOBLEheliumId=1",
      "tammId",
      prefixedRdnValue("12"),
      consAttrBinding(createAttrBindingMultiDn("reservedBy", reservers12),
          consAttrBinding(createAttrBindingMultiDn("uses", uses12),
              nilAttrBindings())));


  ///////////////////////////////////////////////
  // Create two Tessin instances (config class)
  // Assumes Tamm instances exist already

  char *uses5[] = {
      createDn("tammId", "11", "neonId=1,NOBLEheliumId=1"),
      NULL};

  char *uses6[] = {
      createDn("tammId", "12", "neonId=1,NOBLEheliumId=1"),
      NULL};

  char *uses6extra[] = {
      createDn("tammId", "12", "neonId=1,NOBLEheliumId=1"),
      NULL};

  createConfigInstance(log,
      "NOBLETessin",
      "NOBLEheliumId=1",
      "tessinId",
      prefixedRdnValue("5"),
      consAttrBinding(createAttrBindingMultiDn("uses", uses5),
          nilAttrBindings()));

  createConfigInstance(log,
      "NOBLETessin",
      "NOBLEheliumId=1",
      "tessinId",
      prefixedRdnValue("6"),
      consAttrBinding(createAttrBindingMultiDn("needs", uses6extra),
          consAttrBinding(createAttrBindingMultiDn("uses", uses6),
              nilAttrBindings())));


  ///////////////////////////////////////////////
  // Set the 'persistentNickname' attribute on the Xenon instance.
  // Ought to work fine even if the attribute already exists.

  oiHandleT handle2;
  oiInitialize(log, &handle2);

  char *pauli = "pauli";
  SaAisErrorT r155 = saImmOiImplementerSet(handle2, pauli);
  if (r155 != SA_AIS_OK) {
    logString2C(log, "*** failed to set implementer for OI, code", pauli, r155);
  }

  char *xenon77 = "NOBLEXenon";
  SaAisErrorT r156 = saImmOiClassImplementerSet(handle2, xenon77);
  // code 12 is: SA_AIS_ERR_NOT_EXIST
  if (r156 != SA_AIS_OK) {
    logString2C(log, "*** failed to set class implementership for", xenon77, r156);
  }

  logString2(log, "update", "persistentNickname in xenonId=77,argonId=1,neonId=1,NOBLEheliumId=1");
  char *dn77 = "xenonId=77,argonId=1,neonId=1,NOBLEheliumId=1";
  SaAisErrorT r77 = setAttrRuntimeString(
      log,
      handle2,
      dn77,
      "persistentNickname",
      "this-attribute-name-should-be-fixed");
  if (r77 != SA_AIS_OK) {
    //  7 is SA_AIS_ERR_INVALID_PARAM
    // 12 is SA_AIS_ERR_NOT_EXIST
    logString2C(log, "failed to set attribute on", dn77, r77);
  }
  else {
    logString2(log, "successfully set attribute on", dn77);
  }


  SaAisErrorT r977 = saImmOiClassImplementerRelease(handle2, xenon77);
  if (r977 != SA_AIS_OK) {
    logString2C(log, "failed to release class implementership", xenon77, r977);
  }
  else {
    logString2(log, "successfully released class implementership", xenon77);
  }

  SaAisErrorT r9977 = saImmOiImplementerClear(handle2);
  if (r9977 != SA_AIS_OK) {
    logString2C(log, "failed to clear implementership", xenon77, r9977);
  }
  else {
    logString2(log, "successfully cleared implementership", xenon77);
  }


  oiFinalize(log, handle2);


  ///////////////////////////////////////////////////
  // Create two Astatine and one Radon instance. Also
  // set the 'volatileBird' runtime attribute on the Xenon instance.
  // The instances may exist already; this is OK.
  //
  // Also create instances of Ovett, Cram and Nurmi (persistent runtime
  // classes). The purpose is to test the safcImmCtCopyInstances(),
  // safcImmCtCopyRtInstances() and safcImmCtWriteRtInstances() requests.
  // The implementer here is "fermi".

  oiHandleT handle;
  oiInitialize(log, &handle);

  // presumably required when creating runtime instances
  SaAisErrorT r55 = saImmOiImplementerSet(handle, "fermi");
  if (r55 != SA_AIS_OK) {
    logStringC(log, "*** failed to set implementer for OI, code", r55);
  }

  createRuntimeInstance(
      log,
      handle,
      "NOBLEAstatine",
      "argonId=1,neonId=1,NOBLEheliumId=1",
      consAttrBinding(createAttrBindingStringSingle("volatileBird", "hummingbird"),
          consAttrBinding(createAttrBindingStringSingle("astatineId", "1"),
              nilAttrBindings())));
  logString(log, "created: astatineId=1,argonId=1,neonId=1,NOBLEheliumId=1");

  createRuntimeInstance(
      log,
      handle,
      "NOBLEAstatine",
      "argonId=1,neonId=1,NOBLEheliumId=1",
      consAttrBinding(createAttrBindingStringSingle("astatineId", "2"),
          nilAttrBindings()));
  logString(log, "created: astatineId=2,argonId=1,neonId=1,NOBLEheliumId=1");

  createRuntimeInstance(
      log,
      handle,
      "NOBLERadon",
      "argonId=1,neonId=1,NOBLEheliumId=1",
      consAttrBinding(createAttrBindingStringSingle("radonId", "91"),
          consAttrBinding(createAttrBindingStringSingle("persistentNickname", "raddy"),
              consAttrBinding(createAttrBindingStringSingle("volatileBird", "sparrow"),
                  consAttrBinding(createAttrBindingUint32Single("massNumber", 344),
                      nilAttrBindings())))));

  logString(log, "created: radonId=91,argonId=1,neonId=1,NOBLEheliumId=1");

  createRuntimeInstance(
      log,
      handle,
      "S1V2Ovett",
      "S1V2rootS1V2Id=1",
      consAttrBinding(createAttrBindingStringSingle("ovettId", prefixedRdnValue("177")),
          consAttrBinding(createAttrBindingUint32Single("shoeSize", 177),
              nilAttrBindings())));
  logString(log, "created: an Ovett instance under the S1V2 root instance");

  createRuntimeInstance(
      log,
      handle,
      "S1V2Cram",
      "S1V2rootS1V2Id=1",
      consAttrBinding(createAttrBindingStringSingle("cramId", prefixedRdnValue("188")),
          consAttrBinding(createAttrBindingUint32Single("shoeSize", 188),
              nilAttrBindings())));
  logString(log, "created: a Cram instance under the S1V2 root instance");

  createRuntimeInstance(
      log,
      handle,
      "S1V2Nurmi",
      "S1V2rootS1V2Id=1",
      consAttrBinding(createAttrBindingStringSingle("nurmiId", prefixedRdnValue("199")),
          consAttrBinding(createAttrBindingUint32Single("shoeSize", 199),
              nilAttrBindings())));
  logString(log, "created: a Nurmi instance under the S1V2 root instance");


  SaAisErrorT r56 = saImmOiClassImplementerSet(handle, xenon77);
  // code 12 is: SA_AIS_ERR_NOT_EXIST
  if (r56 != SA_AIS_OK) {
    logString2C(log, "*** failed to set class implementership for", xenon77, r56);
  }

  char *x77 = "xenonId=77,argonId=1,neonId=1,NOBLEheliumId=1";
  logString2(log, "update", "volatileBird in xenonId=77,argonId=1,neonId=1,NOBLEheliumId=1");
  SaAisErrorT rr77 = setAttrRuntimeString(
      log,
      handle,
      x77,
      "volatileBird",
      "blackbird");
  if (rr77 != SA_AIS_OK) {
    //  7 is SA_AIS_ERR_INVALID_PARAM
    // 12 is SA_AIS_ERR_NOT_EXIST
    logString2C(log, "failed to set attribute on", x77, rr77);
  }
  else {
    logString2(log, "successfully set attribute on", x77);
  }

  oiFinalize(log, handle);

  ///////////// read a runtime instance, code example
  ///////////// this code was handed over to Rolf Perbrink for troubleshooting HU68841

  SaVersionT version_A_2_11 = {
      .releaseCode = 'A',
      .majorVersion = 2,
      .minorVersion = 11
  };

  SaImmHandleT myImmHandle;
  SaAisErrorT resultFromInit = saImmOmInitialize(&myImmHandle, NULL, &version_A_2_11);
  if (resultFromInit != SA_AIS_OK) {
    logString(log, "failed to saImmOmInitialize");
  }
  else {
    SaImmAccessorHandleT myAccessorHandle;
    SaAisErrorT resultFromAI = saImmOmAccessorInitialize(myImmHandle, &myAccessorHandle);
    if (resultFromAI != SA_AIS_OK) {
      logString(log, "failed to saImmOmAccessorInitialize");
    }
    else {
      char *myObjectNameString = "radonId=91,argonId=1,neonId=1,NOBLEheliumId=1";
      SaNameT myObjectName = {
          .length = strlen(myObjectNameString)
      };
      for (int j = 0; j < myObjectName.length; j++) {
        myObjectName.value[j] = (SaUint8T)myObjectNameString[j];
      }
      SaImmAttrValuesT_2 **myAttributes;
      SaAisErrorT resultFromGet = saImmOmAccessorGet_2(
          myAccessorHandle,
          &myObjectName,
          NULL,
          &myAttributes);
      if (resultFromGet != SA_AIS_OK) {
        logStringC(log, "failed to saImmOmAccessorGet_2, code: ", resultFromGet);
      }
      else {
        for (int k = 0; myAttributes[k] != NULL; k++) {
          SaImmAttrValuesT_2 *attr = myAttributes[k];
          logString2(log, "attr name", attr->attrName);
          logStringInt(log, "attr type", attr->attrValueType);
          logStringInt(log, "attr mult", attr->attrValuesNumber);

          if (attr->attrValueType != SA_IMM_ATTR_SASTRINGT) {
            logString(log, "will not display non-string data");
          }
          else {
            for (SaUint32T m = 0; m < attr->attrValuesNumber; m++) {
              char **stringValueP = (char **)attr->attrValues[m];
              char *stringValue = *stringValueP;
              logString2(log, "string value", stringValue);
            }
          }
        }
        saImmOmFinalize(myImmHandle);
      }
    }
  }
  //////////// end of code example

  return strdup("OK");
}


int main(int argc, char * const argv[]) {
  if (argc < 2) {
    // must not happen
    printf("ERROR: no argument\n");
    return 0;
  }
  else {
    char *log;
    if (getenv("LOG_DIR") == NULL) {
      printf("ERROR: Cannot write to LOG_DIR\n");
      return 0;
    }
    else {
      asprintf(&log, "%s/%s", getenv("LOG_DIR"), "noble_upi.txt");
    }

    if (STREQ(argv[1], "verifyPreconditions")) {
      FILE *f = fopen(log, "a");
      fprintf(f, "%s\n", "%CCaseRev:	/main/R3A/R4A/R5A/R6A/R8A/2 %");
      fclose(f);
      doInspect(log);
      char *result = doImmAtVerifyPreconditions(log);

      printf("%s: noble_upi, %s\n", result, argv[1]);
    }
    else {
      printf("OK: noble_upi, %s\n", argv[1]);
    }
  }
}
