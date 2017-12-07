/* ----------------------------------------------------------------------
 * %CCaseFile:	ugt_s2.c %
 * %CCaseRev:	/main/R2A/14 %
 * %CCaseDate:	2013-10-28 %
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
 * R2A/2      2013-08-19 erarafo     Minimal changes to make upgrade succeed
 * R2A/3      2013-08-20 erarafo     Creating a tree of S2 instances
 * R2A/6      2013-08-21 erarafo     First complete set of upgrade tests
 * R2A/7      2013-08-21 erarafo     Replaced my_proc with a chosen name
 * R2A/10     2013-08-26 erarafo     Minor refactoring
 * R2A/11     2013-08-27 erarafo     Test reading of schema versions
 * R2A/12     2013-08-28 erarafo     Added a "waitForClasses" call
 * R2A/13     2013-10-10 erarafo     Removed CPP legacy
 * R2A/14     2013-10-28 erarafo     Uplift to ICTI IWD PB1
 * ----------------------------------------------------------------------
 */

#include "ugt_common.h"

#define APPNAME "ugt_s2"

static void doIcti(FILE *log) {

  SaAisErrorT r;
  SafcImmCtHandleT handle;
  r = safcImmCtInitialize(&handle);
  if (notOk(r, handle, log, "initialize")) {
    return;
  }

  fprintf(log, "handle: %llu\n", handle);
  fflush(log);

  // Test referring to a non-existent schema

  SaStringT badSchemaName = "NoSuchSchema";
  SafcImmCtSchemaVersionT_2 oldSchemaVersion;
  SafcImmCtSchemaVersionT_2 newSchemaVersion;

  delaySeconds(1);


  // Check that a bad schema name is handled well

  r = safcImmCtReadSchemaVersion_2(handle, badSchemaName, &oldSchemaVersion,
      &newSchemaVersion);
  if (isOk(r, handle, log, "bad schema name")) {
    return;
  }


  // Check that a good schema name yields correct results
  // Hard-coded version.release for the new schema matches
  // values given in swm_icti_extensive_test.sh.

  oldSchemaVersion.version = UNDEFINED;
  oldSchemaVersion.release = UNDEFINED;
  oldSchemaVersion.correction = UNDEFINED;

  newSchemaVersion.version = UNDEFINED;
  newSchemaVersion.release = UNDEFINED;
  newSchemaVersion.correction = UNDEFINED;

  SaStringT goodSchemaName = "s2";
  r = safcImmCtReadSchemaVersion_2(handle, goodSchemaName, &oldSchemaVersion,
      &newSchemaVersion);
  if (notOk(r, handle, log, "readSchemaVersion")) {
    return;
  }

  if (!(
	assertEqualsSaUInt32(0, getVersion(&oldSchemaVersion), log, "old version") &&
	assertEqualsSaUInt32(0, getRelease(&oldSchemaVersion), log, "old release") &&
	assertEqualsSaUInt32(0, getCorrection(&oldSchemaVersion), log, "old correction") &&
	assertEqualsSaUInt32(1, getVersion(&newSchemaVersion), log, "new version") &&
	assertEqualsSaUInt32(0, getRelease(&newSchemaVersion), log, "new release") &&
	assertEqualsSaUInt32(0, getCorrection(&newSchemaVersion), log, "new correction"))) {
    return;
  }



  // Write instances for the S2 schema. This is a new schema so all
  // instances must be created right here. Write instance groups in
  // leaf-first order to verify that the Erlang side can cope with
  // write operations in the "wrong" order.

  // Write BetaS2 instances; a single instance with a multi-valued
  // attribute.

  delaySeconds(1);

  r = safcImmCtWriteInstances(
      handle,
      oneInstGroup(
          instGroup(
              "S2BetaS2",
              oneInstance(
                  inst("betaS2Id", "1", wrapName("alphaS2Id=2,S2rootS2Id=1"),
                      oneStrings2Attr("betaM", "krypton", "xenon"))))));

  if (notOk(r, handle, log, "writeInstances (S2BetaS2)")) {
    return;
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

  if (notOk(r, handle, log, "writeInstances (S2AlphaS2)")) {
    return;
  }


  // Write RootS2 instances; just a single one of course.

  delaySeconds(1);

  r = safcImmCtWriteInstances(
      handle,
      oneInstGroup(
          instGroup(
              "S2RootS2",
              oneInstance(
                  inst("S2rootS2Id", "1", NULL, emptyAttrList())))));

  if (notOk(r, handle, log, "writeInstances (S2RootS2)")) {
    return;
  }

  r = safcImmCtWaitForClasses(handle, twoClassNames("S1V1DeltaS1V1", "S1V2AlphaS1V2"));
  if (notOk(r, handle, log, "waitForInstances(S1V1DeltaS1V1, S1V2AlphaS1V2)")) {
    return;
  }


  // The delays in this UE are 1 second, which is less than what
  // the other UEs have. Expect to see this 'finalize' call hang for
  // a while until the other UEs have finished.

  r = safcImmCtFinalize(handle);
  notOk(r, NO_HANDLE, log, "finalize");
}



int main(void)
{
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
  else if (hasMark(APPNAME)) {
    fprintf(log, "upgrade, refusing to retry\n");
    fflush(log);
  }
  else {
    fprintf(log, "upgrade\n");
    fflush(log);
    doIcti(log);
  }

  idle(log, 5);
}
