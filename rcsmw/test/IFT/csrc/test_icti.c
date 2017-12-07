/* ----------------------------------------------------------------------
 * %CCaseFile:	test_icti.c %
 * %CCaseRev:	/main/R2A/3 %
 * %CCaseDate:	2013-10-25 %
 * %CCaseDocNo: %
 * Author:	erarafo
 * Author: <name>, <e-mail address>
 *
 * Short description: Simple tests of the ICTI interface. The tests
 * are run when the master process starts. No Erlang test suite is
 * involved. Results are logged to LOG_DIR/test_icti.log.
 *
 * This C module depends on the object model and instances that come
 * with the FAKE2 CXC.
 *
 * ----------------------------------------------------------------------
 * %CCaseTemplateFile:	template.c %
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
 * R2A/1      2013-06-18 erarafo     First version
 * R2A/2      2013-06-19 erarafo     Handle root instances correctly
 * ----------------------------------------------------------------------
 */

#define _GNU_SOURCE

#include <stdbool.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#include "safcImmCt.h"



// Derives the filename, opens the file in append mode
// and returns the stream.

static FILE *getFile() {
  char *logFileName = NULL;
  asprintf(&logFileName, "%s/test_icti.log", getenv("LOG_DIR"));
  FILE *result =  fopen(logFileName, "a");
  free(logFileName);
  return result;
}

static void log_f(char *format) {
  FILE *f = getFile();
  fprintf(f, format);
  fclose(f);
}

static void log_f_c(char *format, char c) {
  FILE *f = getFile();
  fprintf(f, format, c);
  fclose(f);
}

static void log_f_s(char *format, char *string) {
  FILE *f = getFile();
  fprintf(f, format, string);
  fclose(f);
}

static void log_f_s_s_s_d(char *format, char *s1, char *s2, char *s3, int d) {
  FILE *f = getFile();
  fprintf(f, format, s1, s2, s3, d);
  fclose(f);
}

static void log_f_d(char *format, SaInt32T d) {
  FILE *f = getFile();
  fprintf(f, format, d);
  fclose(f);
}

static void log_f_u(char *format, SaUint32T u) {
  FILE *f = getFile();
  fprintf(f, format, u);
  fclose(f);
}
static void log_f_lld(char *format, SaInt64T d) {
  FILE *f = getFile();
  fprintf(f, format, d);
  fclose(f);
}

static void log_f_llu(char *format, SaUint64T u) {
  FILE *f = getFile();
  fprintf(f, format, u);
  fclose(f);
}





static bool streq(char *a, char *b) {
  return strcmp(a, b) == 0;
}


static char *saDecode(int r) {
  if (r == 1) {
    return "SA_AIS_OK";
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


static void printSaNameT(SaNameT *name) {
  for (int i = 0; i < name->length; i++) {
    log_f_c("%c", name->value[i]);
  }
}


static char *decodeAttrType(int n) {
  if (n == 1) {
    return "int32";
  }
  else if (n == 2) {
    return "uint32";
  }
  else if (n == 3) {
    return "int64";
  }
  else if (n == 4) {
    return "uint64";
  }
  else if (n == 5) {
    return "time";
  }
  else if (n == 6) {
    return "name";
  }
  else if (n == 7) {
    return "float";
  }
  else if (n == 8) {
    return "double";
  }
  else if (n == 9) {
    return "string";
  }
  else if (n == 10) {
    return "any";
  }
  else {
    // not supposed to happen, don't bother freeing
    char *numeral = NULL;
    asprintf(&numeral, "%d", n);
    return numeral;
  }
}


void exerciseIcti()
{
  char *restartType = getenv("RESTART_TYPE");
  if (restartType == NULL) {
    // not supposed to happen
    return;
  }

  if (! streq(restartType, "UPGRADE")) {
    // not an upgrade restart
    return;
  }

  char *safcImmCtPort = getenv("SAFC_IMM_CT_PORT");
  if (safcImmCtPort == NULL) {
    // not supposed to happen
    return;
  }

  if (streq(safcImmCtPort, "10005")) {
    // skip these tests on target, for now at least
    return;
  }

  char *logDir = getenv("LOG_DIR");
  if (logDir == NULL) {
    // not supposed to happen
    return;
  }

  log_f("IFT application, this is an upgrade restart\n");

  // safcTraceLevel(2);

  SaAisErrorT r;
  SafcImmCtHandleT handle;
  r = safcImmCtInitialize(&handle);
  log_f_s("initialize, returned: %s\n", saDecode(r));
  if (r != SA_AIS_OK) {
    log_f("!!! terminating unexpectedly\n");
    return;
  }
  log_f_llu("initialize: got handle: %llu\n", handle);


//  int badHandle = 34598;
//  r = safcImmCtFailUpgrade(badHandle, "using a bad handle");
//  fprintf(log, "fail upgrade with bad handle, returned: %s\n", saDecode(r));

//  NEEDS MORE TESTING -- should cause a rollback!
//  r = safcImmCtFailUpgrade(handle, "get me out of here!");
//  fprintf(log, "fail upgrade, returned: %s\n", saDecode(r));
//  fflush(log);
//  if (r != SA_AIS_OK) {
//    fclose(log);
//    return;
//  }
//  else {
//    fprintf(log, "fail upgrade: success");
//    fflush(log);
//  }


//  // Testing with bad handle, should be rejected
//  SaStringT schemaName="BadHandleAndNoSuchSchema";
//  SafcImmCtSchemaVersionT oldSchemaVersion;
//  SafcImmCtSchemaVersionT newSchemaVersion;
//  r = safcImmCtReadSchemaVersion(
//      badHandle,
//      schemaName,
//      &oldSchemaVersion,
//      &newSchemaVersion);
//  fprintf(log, "readSchemaVersion, bad handle, returned: %s\n", saDecode(r));


//  // Good handle, nonexistent schema
//  SaStringT schemaName2="NoSuchSchema";
//  SafcImmCtSchemaVersionT oldSchemaVersion2;
//  SafcImmCtSchemaVersionT newSchemaVersion2;
//  r = safcImmCtReadSchemaVersion(
//      handle,
//      schemaName2,
//      &oldSchemaVersion2,
//      &newSchemaVersion2);
//  fprintf(log, "readSchemaVersion, returned: %s\n", saDecode(r));
//  if (r != SA_AIS_OK) {
//    fclose(log);
//    return;
//  }
//  else {
//    fprintf(log, "  old: %u.%u\n", oldSchemaVersion2.version, oldSchemaVersion2.release);
//    fprintf(log, "  new: %u.%u\n", newSchemaVersion2.version, newSchemaVersion2.release);
//  }


  SaImmClassNameT classNames[] = {
      "Transport",
      "TESTMOMTestRoot",
      "TESTMOMTestClass1",
      "TESTMOMStruct1",
      "TESTMOMTestClass2",
      "TESTMOMStruct2",
      "TESTMOMTestClass3",
      "TESTMOMStruct3",
      "TESTMOMTestClass4",
      NULL
  };

  SafcImmCtInstanceGroupT **instanceGroups;
  r = safcImmCtReadInstances(handle, classNames, &instanceGroups);
  log_f_s("readInstances, returned: %s\n", saDecode(r));
  if (r != SA_AIS_OK) {
    log_f("!!! terminating unexpectedly\n");
    return;
  }

  for (SafcImmCtInstanceGroupT **p = instanceGroups; *p != NULL; p++) {
    log_f_s("  class: %s\n", (*p)->className);

    for (SafcImmCtInstanceT **instances = (*p)->instances; *instances != NULL; instances++) {

      SafcImmCtInstanceT *q = *instances;
      log_f("    IMM-style parent name: ");
      if (q->parentName == NULL) {
        log_f("N/A for root instance");
      }
      else {
        printSaNameT(q->parentName);
      }
      log_f("\n");

      bool firstLap = true;
      for (SaImmAttrValuesT_2 **vv = q->attrValues; *vv != NULL; vv++) {
        SaImmAttrNameT attrName = (*vv)->attrName;
        SaImmValueTypeT attrType = (*vv)->attrValueType;
        SaUint32T mult = (*vv)->attrValuesNumber;
        log_f_s_s_s_d("      %s: %s, type: %s, mult: %d\n",
            (firstLap ? "rdnName" : "attribute"),
            attrName,
            decodeAttrType(attrType),
            mult);
        firstLap = false;

        SaImmAttrValueT *x =  (*vv)->attrValues;
        for (int k = 0; k < mult; k++) {
          SaImmAttrValueT y = x[k];
          if (attrType == SA_IMM_ATTR_SAINT32T) {
            log_f_d("        value: %d\n", *(SaInt32T *)y);
          }
          else if (attrType == SA_IMM_ATTR_SAUINT32T) {
            log_f_u("        value: %u\n", *(SaUint32T *)y);
          }
          else if (attrType == SA_IMM_ATTR_SAINT64T) {
            log_f_lld("        value: %lld\n", *(SaInt64T *)y);
          }
          else if (attrType == SA_IMM_ATTR_SAUINT64T) {
            log_f_llu("        value: %llu\n", *(SaUint64T *)y);
          }
          else if (attrType == SA_IMM_ATTR_SASTRINGT) {
            log_f_s("        value: %s\n", *((SaStringT *)y));
          }
          else if (attrType == SA_IMM_ATTR_SANAMET) {
            log_f("        value: ");
            printSaNameT((SaNameT *)y);
            log_f("\n");
          }
          else {
            log_f("        value: cannot decode (yet)\n");
          }
        }
      }
    }
  }


  r = safcImmCtInstanceGroupsMemoryFree(handle, instanceGroups);
  log_f_s("instanceGroupsMemoryFree, returned: %s\n", saDecode(r));
  if (r != SA_AIS_OK) {
    log_f("!!! terminating unexpectedly\n");
    return;
  }


//  SaImmClassNameT waitForClasses[3];
//  waitForClasses[0] = "TESTMOMTestRoot";
//  waitForClasses[1] = "TESTMOMTestClass1";
//  waitForClasses[2] = NULL;
//  r = safcImmCtWaitForClasses(handle, waitForClasses);
//  fprintf(log, "waitForClasses, returned: %s\n", saDecode(r));
//  if (r != SA_AIS_OK) {
//    fprintf(log, "!!! terminating unexpectedly\n");
//    fclose(log);
//    return;
//  }


  r = safcImmCtFinalize(handle);
  log_f_s("finalize, returned: %s\n", saDecode(r));
  if (r != SA_AIS_OK) {
    log_f("!!! terminating unexpectedly\n");
    return;
  }
}

