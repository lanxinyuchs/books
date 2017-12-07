/* ----------------------------------------------------------------------
 * %CCaseFile:	ugt_s0.c %
 * %CCaseRev:	/main/R2A/3 %
 * %CCaseDate:	2013-10-28 %
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
 * R2A/2      2013-10-10 erarafo     Removed CPP legacy
 * R2A/3      2013-10-28 erarafo     Uplift to ICTI IWD PB1
 * ----------------------------------------------------------------------
 */

#include "ugt_common.h"

#define APPNAME "ugt_s0"






static void doIcti(FILE *log) {

  SaAisErrorT r;
  SafcImmCtHandleT handle;
  r = safcImmCtInitialize(&handle);
  if (notOk(r, handle, log, "initialize")) {
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
  if (notOk(r, handle, log, "read schema version")) {
    return;
  }

  if (!(
      assertEqualsSaUInt32(2, getVersion(&oldSchemaVersion), log, "old version") &&
      assertEqualsSaUInt32(7, getRelease(&oldSchemaVersion), log, "old release") &&
      assertEqualsSaUInt32(0, getCorrection(&oldSchemaVersion), log, "old corection") &&
      assertEqualsSaUInt32(0, getVersion(&newSchemaVersion), log, "new version") &&
      assertEqualsSaUInt32(0, getRelease(&newSchemaVersion), log, "new release") &&
      assertEqualsSaUInt32(0, getCorrection(&newSchemaVersion), log, "new correction"))) {
    fprintf(log, "unexpected schema versions obtained");
    fflush(log);
    return;
  }
  else {
    fprintf(log, "expected schema versions obtained");
    fflush(log);
  }



  // Check if reading instances from the old schema is still
  // possible

  SaImmClassNameT *s0ClassNames = (SaImmClassNameT *)calloc(2+1, sizeof(SaImmClassNameT));
  s0ClassNames[0] = "S0RootS0";
  s0ClassNames[1] = "S0AlphaS0";
  SafcImmCtInstanceGroupT **s0InstGroups;

  r = safcImmCtReadInstances(handle, s0ClassNames, &s0InstGroups);
  if (notOk(r, handle, log, "readInstances")) {
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
