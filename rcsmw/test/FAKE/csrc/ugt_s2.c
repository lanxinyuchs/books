/* ----------------------------------------------------------------------
 * %CCaseFile:	ugt_s2.c %
 * %CCaseRev:	/main/R2A/R3A/R8A/1 %
 * %CCaseDate:	2017-01-02 %
 * %CCaseDocNo: %
 * Author:	erarafo
 * Author: <name>, <e-mail address>
 *
 * Short description: "Upgrade engine" that uses the ICTI interface
 * for conversion of IMM instances belonging to the S2 MOM.
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
 * R2A/3      2013-10-11 erarafo     Elaborated version logic
 * R2A/4      2014-01-18 erarafo     Removed run-once logic
 * R2A/5      2014-02-25 etxivri     Update to be used in fail upgrade TC A18.
 *                                   When trying to write attribute type.
 * R2A/6      2014-03-10 erarafo     TC A18, adjustments
 * R2A/7      2014-03-20 erarafo     Corrections and robustness
 * R2A/8      2014-03-20 erarafo     Corrections
 * R2A/9      2014-03-25 erarafo     A18 behavior adjusted
 * R2A/10     2014-04-29 erarafo     A18 log messages flipped
 * R3A/1      2014-11-15 erarafo     Comments only
 * R8A/1      2017-01-02 ecaiyan     Conditional call for safcImmCtInitialize_2
 * ----------------------------------------------------------------------
 */

#include "ugt_common.h"

#define APPNAME "ugt_s2"

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

  delaySeconds(1);

  // Test referring to a non-existent schema
  SaStringT badSchemaName = "NoSuchSchema";
  SafcImmCtSchemaVersionT_2 oldSchemaVersion;
  SafcImmCtSchemaVersionT_2 newSchemaVersion;
  r = safcImmCtReadSchemaVersion_2(handle, badSchemaName, &oldSchemaVersion,
      &newSchemaVersion);
  if (ok("readSchemaVersion", r, log)) {
    // we should never end up here
    fprintf(log, "unexpected: successful read of non-existent schema: %s\n", badSchemaName);
    delayedFinalize(log, handle, delayBeforeFinalize);
    //safcImmCtFinalize(handle);
    return;
  }

  // Check that a good schema name yields correct results.
  oldSchemaVersion.version = UNDEFINED;
  oldSchemaVersion.release = UNDEFINED;
  oldSchemaVersion.correction = UNDEFINED;
  newSchemaVersion.version = UNDEFINED;
  newSchemaVersion.release = UNDEFINED;
  newSchemaVersion.correction = UNDEFINED;

  SaStringT goodSchemaName = "s2";
  r = safcImmCtReadSchemaVersion_2(handle, goodSchemaName, &oldSchemaVersion,
      &newSchemaVersion);
  if (nok("readSchemaVersion", r, log)) {
    delayedFinalize(log, handle, delayBeforeFinalize);
    //safcImmCtFinalize(handle);
    return;
  }
  else {
    fprintf(log, "old schema: %d.%d.%d\n",
        oldSchemaVersion.version,
        oldSchemaVersion.release,
        oldSchemaVersion.correction);
    fprintf(log, "new schema: %d.%d.%d\n",
        newSchemaVersion.version,
        newSchemaVersion.release,
        newSchemaVersion.correction);
    fflush(log);

    if (oldSchemaVersion.version == newSchemaVersion.version &&
        oldSchemaVersion.release == newSchemaVersion.release &&
        oldSchemaVersion.correction == newSchemaVersion.correction) {
      // s1v3 case -- schemas unchanged
      fprintf(log, "s1v3 case; no writing allowed\n");
      //safcImmCtFinalize(handle);
      delayedFinalize(log, handle, delayBeforeFinalize);
      return;
    }
    else if (
        oldSchemaVersion.version == 0 &&
        oldSchemaVersion.release == 0 &&
        newSchemaVersion.version == 1 &&
        newSchemaVersion.release == 0) {
      // Old schema nonexistent
      fprintf(log, "s2 case\n");
      fflush(log);
    }
    else {
      fprintf(log, "upgrade case not supported (yet); doing nothing\n");
      //safcImmCtFinalize(handle);
      delayedFinalize(log, handle, delayBeforeFinalize);
      return;
    }
  }

  // Write instances for the S2 schema. This is a new schema so all
  // instances must be created right here. Write instance groups in
  // leaf-first order to verify that the Erlang side can cope with
  // write operations in the "wrong" order.

  // Write BetaS2 instances; a single instance with a multi-valued
  // attribute.

  delaySeconds(1);

  if (STREQ(getenv("FAIL_UPGRADE"), "A18")) {
    // Negative test: Try to write an S2BetaS2 instance where the type of
    // the betaM attribute is not correct (int32 instead of string).
    r = safcImmCtWriteInstances(
        handle,
        oneInstGroup(
            instGroup(
                "S2BetaS2",
                oneInstance(
                    inst("betaS2Id", "1", wrapName("alphaS2Id=2,S2rootS2Id=1"),
                        oneInt32Attr("betaM", 17))))));

    // The message texts below are matched by test suites--do not edit!
    if (nok("writeInstances", r, log)) {
      fprintf(log, "UNEXPECTED, non-OK return code from safcImmCtWriteInstances()\n");
    }
    else {
      fprintf(log, "expected, fault_a18 case, return code from ICTI expected OK\n");
    }
  }
  else {
    // Positive scenario
    r = safcImmCtWriteInstances(
        handle,
        oneInstGroup(
            instGroup(
                "S2BetaS2",
                oneInstance(
                    inst("betaS2Id", "1", wrapName("alphaS2Id=2,S2rootS2Id=1"),
                        oneStrings2Attr("betaM", "krypton", "xenon"))))));

    if (nok("writeInstances", r, log)) {
      fprintf(log, "failed to write S2BetaS2 instances\n");
      return;
    }
  }

  // Write AlphaS2 instances; two of them.

  delaySeconds(1);

  r = safcImmCtWriteInstances(
      handle,
      oneInstGroup(
          instGroup(
              "S2AlphaS2",
              twoInstances(
                  inst("alphaS2Id", "1", wrapName("S2rootS2Id=1"), oneInt32Attr("alphaA", 17)),
                  inst("alphaS2Id", "2", wrapName("S2rootS2Id=1"), oneInt32Attr("alphaA", 18))))));

  if (nok("writeInstances", r, log)) {
    fprintf(log, "failed to write S2AlphaS2 instances\n");
    return;
  }

  // Write RootS2 instances; just a single one of course.

  delaySeconds(1);

  // If there is ever a need to cause a condition where the upgrade
  // hangs waiting for classes, then comment the code below.
  // /* ---------------------------------------------------------------
  r = safcImmCtWriteInstances(
      handle,
      oneInstGroup(
          instGroup(
              "S2RootS2",
              oneInstance(
                  inst("S2rootS2Id", "1", NULL, emptyAttrList())))));

  if (nok("writeInstances", r, log)) {
    fprintf(log, "failed to write an S2RootS2 instance\n");
    return;
  }
  // -------------------------------------------------------------- */

  r = safcImmCtWaitForClasses(handle, twoClassNames("S1V1DeltaS1V1", "S1V2AlphaS1V2"));
  if (nok("waitForClasses", r, log)) {
    fprintf(log, "failed to wait for classes: S1V1DeltaS1V1, S1V2AlphaS1V2\n");
    return;
  }

  // The delays in this UE are 1 second, which is less than what
  // the other UEs have. Expect to see this 'finalize' call hang for
  // a while until the other UEs have finished.

  delayedFinalize(log, handle, delayBeforeFinalize);
}



int main() {

  FILE *log = openLog(APPNAME);

  char *restartType = getenv("RESTART_TYPE");

  if (restartType == NULL) {
    fprintf(log, "RESTART_TYPE is NULL, internal error!!\n");
    fflush(log);
  }
  else if (! STREQ(restartType, "UPGRADE")) {
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
