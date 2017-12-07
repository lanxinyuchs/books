/* ----------------------------------------------------------------------
 * %CCaseFile:	ugt_s1v1.c %
 * %CCaseRev:	/main/R2A/5 %
 * %CCaseDate:	2014-03-21 %
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
 * Copyright (c) Ericsson AB 2013-2014 All rights reserved.
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
 * R2A/5      2014-03-20 erarafo     Simplifications
 * ----------------------------------------------------------------------
 */

#include "ugt_common.h"

#define APPNAME "ugt_s1v1"

static void doIcti(FILE *log) {

  SaAisErrorT r;
  SafcImmCtHandleT handle;
  r = safcImmCtInitialize(&handle);
  if (nok("initialize", r, log)) {
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
  if (ok("readSchemaVersion", r, log)) {
    fprintf(log, "unexpected: result ok when reading non-existent schema\n");
    fflush(log);
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
  if (nok("readSchemaVersion", r, log)) {
    if (r == SA_AIS_ERR_NOT_EXIST) {
      fprintf(log, "No schema named s1v1 exists (neither From nor To)\n");
      fflush(log);
    }
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

  if (oldSchemaVersion.version == newSchemaVersion.version &&
      oldSchemaVersion.release == newSchemaVersion.release &&
      oldSchemaVersion.correction == newSchemaVersion.correction) {
    // s1v3 case -- schemas unchanged
    fprintf(log, "s1v3 case; no writing allowed\n");
    fflush(log);
    r = safcImmCtFinalize(handle);
    nok("finalize", r, log);
    return;
  }

  if (oldSchemaVersion.version == 0 &&
      oldSchemaVersion.release == 0 &&
      newSchemaVersion.version == 1 &&
      newSchemaVersion.release == 0) {
    fprintf(log, "No old s1v1 schema, new schema exists and is named, version 1.0\n");
    fflush(log);
  }

  // S1V1 schema

  SaImmClassNameT *s1v1ClassNames = (SaImmClassNameT *)calloc(1+1, sizeof(SaImmClassNameT));
  s1v1ClassNames[0] = "S1V1AlphaS1V1";
  SafcImmCtInstanceGroupT **s1v1InstGroups;

  delaySeconds(2);

  r = safcImmCtReadInstances(handle, s1v1ClassNames, &s1v1InstGroups);
  if (nok("readInstances", r, log)) {
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
  if (nok("instanceGroupsMemoryFree", r, log)) {
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
  if (nok("writeInstances", r, log)) {
    return;
  }

  // Finally copy the root instance

  delaySeconds(2);

  r = safcImmCtCopyInstances(handle, oneClassName("S1V1RootS1V1"));
  if (nok("copyInstances", r, log)) {
    return;
  }

  delaySeconds(2);

  r = safcImmCtFinalize(handle);
  ok("finalize", r, log);
}



int main(void) {

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
    doIcti(log);
  }

  idle(log, 60);
}
