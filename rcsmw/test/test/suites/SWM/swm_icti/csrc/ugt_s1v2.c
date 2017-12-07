/* ----------------------------------------------------------------------
 * %CCaseFile:	ugt_s1v2.c %
 * %CCaseRev:	/main/R2A/6 %
 * %CCaseDate:	2013-10-28 %
 * %CCaseDocNo: %
 * Author:	erarafo
 * Author: <name>, <e-mail address>
 *
 * Short description: "Upgrade engine" that uses the ICTI interface
 * for conversion of IMM instances.
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
 * R2A/1      2013-08-22 erarafo     Cloned from ugt_s2.c
 * R2A/2      2013-08-26 erarafo     Minor refactoring
 * R2A/3      2013-08-27 erarafo     Added memory reclaim
 * R2A/4      2013-08-27 erarafo     Test reading of schema versions
 * R2A/5      2013-10-10 erarafo     Removed CPP legacy
 * R2A/6      2013-10-28 erarafo     Uplift to ICTI IWD PB1
 * ----------------------------------------------------------------------
 */

#include "ugt_common.h"

#define APPNAME "ugt_s1v2"

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

  SaStringT schemaName = "NoSuchSchema";
  SafcImmCtSchemaVersionT_2 oldSchemaVersion;
  SafcImmCtSchemaVersionT_2 newSchemaVersion;

  delaySeconds(2);

  r = safcImmCtReadSchemaVersion_2(handle, schemaName, &oldSchemaVersion,
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

  SaStringT goodSchemaName = "s1v2";
  r = safcImmCtReadSchemaVersion_2(handle, goodSchemaName, &oldSchemaVersion,
      &newSchemaVersion);
  if (notOk(r, handle, log, "readSchemaVersion")) {
    return;
  }

  if (!(
	assertEqualsSaUInt32(2, getVersion(&oldSchemaVersion), log, "old version") &&
	assertEqualsSaUInt32(7, getRelease(&oldSchemaVersion), log, "old release") &&
	assertEqualsSaUInt32(17, getCorrection(&oldSchemaVersion), log, "old correction") &&
	assertEqualsSaUInt32(4, getVersion(&newSchemaVersion), log, "new version") &&
	assertEqualsSaUInt32(0, getRelease(&newSchemaVersion), log, "new release") &&
	assertEqualsSaUInt32(0, getCorrection(&newSchemaVersion), log, "new correction"))) {
    return;
  }



  // S1V2 schema

  // Copy the root instance

  delaySeconds(2);

  r = safcImmCtCopyInstances(handle, oneClassName("S1V2RootS1V2"));
  if (notOk(r, handle, log, "copyInstances")) {
    return;
  }

  // Read the AlphaS1V2 instance, get hold of the alphaS attribute value V

  SafcImmCtInstanceGroupT **s1v2InstGroups;

  delaySeconds(2);

  r = safcImmCtReadInstances(handle, oneClassName("S1V2AlphaS1V2"), &s1v2InstGroups);
  if (notOk(r, handle, log, "readInstances")) {
    return;
  }

  SafcImmCtInstanceGroupT *s1v2Alphas = s1v2InstGroups[0];
  SafcImmCtInstanceT **s1v2AlphaInsts = s1v2Alphas->instances;

  SafcImmCtInstanceT *s1v2i = s1v2AlphaInsts[0];


  SaStringT s1v2AV = getStringAttribute(s1v2i, "alphaS");

  fprintf(log, "old alphaS string value: %s\n", s1v2AV);
  fflush(log);

  // Create an extended instance that includes alphaT. Set the value to black++V.

  // write the AlphaS1V2 instances (just one)

  delaySeconds(2);

  r = safcImmCtWriteInstances(
      handle,
      oneInstGroup(
          instGroup(
              "S1V2AlphaS1V2",
              oneInstance(
                  inst("alphaS1V2Id",
                      "1",
                      wrapName("S1V2rootS1V2Id=1"),
                      twoStringAttrs(
                          "alphaS", s1v2AV,
                          "alphaT", stringConcat("black", s1v2AV)))))));

  if (notOk(r, handle, log, "writeInstances (S1V2AlphaS1V2)")) {
    return;
  }


  // at this point the structure returned by readInstances can be reclaimed

  r = safcImmCtInstanceGroupsMemoryFree(handle, s1v2InstGroups);
  if (notOk(r, handle, log, "instanceGroupsMemoryFree")) {
    return;
  }

  delaySeconds(2);

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
