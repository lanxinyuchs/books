/* ----------------------------------------------------------------------
 * %CCaseFile:	ugt_s0.c %
 * %CCaseRev:	/main/R2A/R8A/1 %
 * %CCaseDate:	2017-01-02 %
 * %CCaseDocNo: %
 * Author:	erarafo
 * Author: <name>, <e-mail address>
 *
 * Short description: "Upgrade engine" that uses the ICTI interface
 * to gather information about the S0 schema, which is a named schema
 * that exists only in the From-version.
 *
 * ----------------------------------------------------------------------
 * %CCaseTemplateFile:  template.c %
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
 * R2A/2      2013-10-10 erarafo     Removed CPP legacy
 * R2A/3      2013-10-10 erarafo     Handle case where To-schema == From-schema
 * R2A/4      2014-01-18 erarafo     Removed run-once logic
 * R2A/5      2014-03-20 erarafo     Simplifications
 * R8A/1      2017-01-02 ecaiyan     Conditional call safcImmCtInitialize_2
 * ----------------------------------------------------------------------
 */

#include "ugt_common.h"

#define APPNAME "ugt_s0"

static void delayedFinalize(FILE *log, SafcImmCtHandleT handle, int delay) {
  if (delay > 0) {
    fprintf(log, "%s: start sleep before finalize, seconds: %d\n", APPNAME, delay);
    fflush(log);
    delaySeconds(delay);
    fprintf(log, "%s: done sleep before finalize, seconds: %d\n", APPNAME, delay);
    fflush(log);
  }
  SaAisErrorT r = safcImmCtFinalize(handle);
  ok("finalize", r, log);
}

static void doIcti(FILE *log, int delayBeforeFinalize) {

  SaAisErrorT r;
  SafcImmCtHandleT handle;
  r = delayBeforeFinalize > 0 ?
      safcImmCtInitialize_2(&handle, (SaBoolT)1) :
      safcImmCtInitialize(&handle);
  if (nok("initialize", r, log)) {
    return;
  }

  fprintf(log, "handle: %llu\n", handle);
  fflush(log);

  // Test referring to the From-version schema

  SaStringT schemaName = "s0";
  SafcImmCtSchemaVersionT_2 oldSchemaVersion;
  SafcImmCtSchemaVersionT_2 newSchemaVersion;

  oldSchemaVersion.version = UNDEFINED;
  oldSchemaVersion.release = UNDEFINED;
  oldSchemaVersion.correction = UNDEFINED;
  newSchemaVersion.version = UNDEFINED;
  newSchemaVersion.release = UNDEFINED;
  newSchemaVersion.correction = UNDEFINED;

  r = safcImmCtReadSchemaVersion_2(handle, schemaName, &oldSchemaVersion,
      &newSchemaVersion);
  if (nok("readSchemaVersion", r, log)) {
    safcImmCtFinalize(handle);
    return;
  }

  fprintf(log, "old schema version: %d.%d.%d\n",
      oldSchemaVersion.version,
      oldSchemaVersion.release,
      oldSchemaVersion.correction);
  fprintf(log, "new schema version: %d.%d.%d\n",
      newSchemaVersion.version,
      newSchemaVersion.release,
      newSchemaVersion.correction);
  fflush(log);

  if (oldSchemaVersion.version == 2 &&
      oldSchemaVersion.release == 7 &&
      newSchemaVersion.version == 0 &&
      newSchemaVersion.release == 0) {
    // The case where the s0 schema is dropped because it only exists
    // in the From-version.
    fprintf(log, "old s0 schema is 2.7, new does not exist\n");
    fflush(log);
  }
  else if (
      oldSchemaVersion.version == 2 &&
      oldSchemaVersion.release == 7 &&
      newSchemaVersion.version == 2 &&
      newSchemaVersion.release == 7) {
    // The case where there is no schema change.
    fprintf(log, "old s0 schema is 2.7, new schema also 2.7; classification will be s1v3\n");
    fflush(log);
  }
  else if (
      oldSchemaVersion.version == 0 &&
      oldSchemaVersion.release == 0 &&
      newSchemaVersion.version == 2 &&
      newSchemaVersion.release == 7) {
    // The case where no From-version schema exists. Do not try
    // to read old instances. Here it is necessary to mark the
    // classes as handled at least.
    // It is not planned to run this case in CI.
    fprintf(log, "new s0 schema is 2.7, no old schema, classification will be s2\n");
    fflush(log);

    delaySeconds(1);

    r = safcImmCtWriteInstances(
        handle,
        twoInstGroups(emptyInstGroup("S0RootS0"), emptyInstGroup("S0AlphaS0")));
    if (nok("writeInstances", r, log)) {
      fprintf(log, "failed to write instances: S0RootS0, S0AlphaS0\n");
      fflush(log);
      // error case, do not bother with delay
      safcImmCtFinalize(handle);
      return;
    }
    else {
      fprintf(log, "marked classes as handled: S0RootS0, S0AlphaS0\n");
      fflush(log);
    }

    delayedFinalize(log, handle, delayBeforeFinalize);
    return;
  }
  else {
    fprintf(log, "FATAL: unexpected schema versions obtained\n");
    fflush(log);
    return;
  }

  // Check if reading instances from the old schema is still
  // possible

  SaImmClassNameT *s0ClassNames = (SaImmClassNameT *)calloc(2+1, sizeof(SaImmClassNameT));
  s0ClassNames[0] = "S0RootS0";
  s0ClassNames[1] = "S0AlphaS0";
  SafcImmCtInstanceGroupT **s0InstGroups;

  r = safcImmCtReadInstances(handle, s0ClassNames, &s0InstGroups);
  if (nok("readInstances", r, log)) {
    return;
  }

  // verify that we get precisely two instance groups
  if (nInstGroups(s0InstGroups) != 2) {
    fprintf(log, "unexpected number of instance groups: %d\n", nInstGroups(s0InstGroups));
    fflush(log);
    return;
  }

  // verify that we have precisely one Alpha instance
  SafcImmCtInstanceT **s0Insts = s0InstGroups[1]->instances;
  if (nInstances(s0Insts) != 1) {
    fprintf(log, "unexpected number of instances: %d\n", nInstances(s0Insts));
    fflush(log);
    return;
  }

  SafcImmCtInstanceT *headInst = s0Insts[0];
  SaInt32T s0OldAlphaA = getAttribute(headInst, "alphaA");

  fprintf(log, "old value of alphaA seems to be: %d\n", s0OldAlphaA);
  fflush(log);


  // at this point the structure returned by readInstances can be reclaimed

  r = safcImmCtInstanceGroupsMemoryFree(handle, s0InstGroups);
  if (nok("instanceGroupsMemoryFree", r, log)) {
    return;
  }

  delaySeconds(2);

  delayedFinalize(log, handle, delayBeforeFinalize);
}



int
main(void) {
  FILE *log = openLog(APPNAME);

  char *restartType = getenv("RESTART_TYPE");

  if (restartType == NULL) {
    fprintf(log, "RESTART_TYPE is NULL, internal error!!\n");
    fflush(log);
  }
  else if (! STREQ(getenv("RESTART_TYPE"), "UPGRADE")) {
    fprintf(log, "not upgrade\n");
    fflush(log);
  }
  else {
    fprintf(log, "upgrade\n");
    fflush(log);

    const char *delayS = getenv("DELAY_BEFORE_FINALIZE");

    fprintf(log, "delay before finalize (string): %s\n", delayS == NULL ? "<null>" : delayS);
    fflush(log);

    int delayBeforeFinalize = delayS == NULL ? 0 : atoi(delayS);

    fprintf(log, "delay before finalize: %d\n", delayBeforeFinalize);
    fflush(log);

    doIcti(log, delayBeforeFinalize);
  }

  idle(log, 60);
}

