/* ----------------------------------------------------------------------
 * %CCaseFile:	ugt_s1v1.c %
 * %CCaseRev:	/main/R2A/6 %
 * %CCaseDate:	2013-10-28 %
 * %CCaseDocNo: %
 * Author:	erarafo
 * Author: <name>, <e-mail address>
 *
 * Short description: "Upgrade engine" that uses the ICTI interface
 * for conversion of IMM instances belonging to the S1V1 MOM.
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

#define APPNAME "ugt_s1v1"

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

  delaySeconds(2);


  // Check that a good schema name yields correct results
  // Hard-coded version.release for the new schema matches
  // values given in swm_icti_extensive_test.sh.

  oldSchemaVersion.version = UNDEFINED;
  oldSchemaVersion.release = UNDEFINED;
  oldSchemaVersion.correction = UNDEFINED;

  newSchemaVersion.version = UNDEFINED;
  newSchemaVersion.release = UNDEFINED;
  newSchemaVersion.correction = UNDEFINED;

  SaStringT goodSchemaName = "s1v1";
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



  // S1V1 schema

  SaImmClassNameT *s1v1ClassNames = (SaImmClassNameT *)calloc(1+1, sizeof(SaImmClassNameT));
  s1v1ClassNames[0] = "S1V1AlphaS1V1";
  SafcImmCtInstanceGroupT **s1v1InstGroups;

  delaySeconds(2);

  r = safcImmCtReadInstances(handle, s1v1ClassNames, &s1v1InstGroups);
  if (notOk(r, handle, log, "readInstances")) {
    return;
  }

  // verify that we get precisely one instance group
  if (nInstGroups(s1v1InstGroups) != 1) {
    fprintf(log, "unexpected number of instance groups: %d\n", nInstGroups(s1v1InstGroups));
    fflush(log);
    return;
  }

  SafcImmCtInstanceT **s1v1Insts = s1v1InstGroups[0]->instances;



  // verify that we have precisely one instance
  if (nInstances(s1v1Insts) != 1) {
    fprintf(log, "unexpected number of instances: %d\n", nInstances(s1v1Insts));
    fflush(log);
    return;
  }

  SafcImmCtInstanceT *headInst = s1v1Insts[0];
  SaInt32T s1v1OldAlphaA = getAttribute(headInst, "alphaA");

  fprintf(log, "old value of alphaA seems to be: %d\n", s1v1OldAlphaA);
  fflush(log);


  // at this point the structure returned by readInstances can be reclaimed

  r = safcImmCtInstanceGroupsMemoryFree(handle, s1v1InstGroups);
  if (notOk(r, handle, log, "instanceGroupsMemoryFree")) {
    return;
  }

  delaySeconds(2);

  r = safcImmCtWriteInstances(
      handle,
      twoInstGroups(
          instGroup(
              "S1V1DeltaS1V1",
              oneInstance(
                  inst(
                      "deltaS1V1Id",
                      "A",
                      wrapName("gammaS1V1Id=U,S1V1rootS1V1Id=1"),
                      oneInt32Attr("deltaA", s1v1OldAlphaA)))),
          instGroup(
              "S1V1GammaS1V1",
              oneInstance(
                  inst(
                      "gammaS1V1Id",
                      "U",
                      wrapName("S1V1rootS1V1Id=1"),
                      emptyAttrList())))
          ));
  if (notOk(r, handle, log, "writeInstances (S1V1GammaS1V1, S1V1DeltaS1V1)")) {
    return;
  }

  // Finally copy the root instance

  delaySeconds(2);

  r = safcImmCtCopyInstances(handle, oneClassName("S1V1RootS1V1"));
  if (notOk(r, handle, log, "copyInstances")) {
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
