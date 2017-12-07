/* ----------------------------------------------------------------------
 * %CCaseFile:	ugt_s1v2.c %
 * %CCaseRev:	/main/R2A/R3A/R4A/R8A/1 %
 * %CCaseDate:	2016-11-25 %
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
 * Copyright (c) Ericsson AB 2013-2016 All rights reserved.
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
 * R2A/5      2014-01-18 erarafo     Removed run-once logic
 * R2A/6      2014-02-11 erarafo     Update to handle different causes of upgrade failure.
 * R2A/8      2014-02-21 etxivri     Update to upgrade failure from application.
 * R2A/9      2014-02-21 etxivri     Update to trying to write an attr not supported in xml structure.
 * R2A/10     2014-03-20 erarafo     Simplifications
 * R2A/11     2014-04-23 erarafo     Adaptations for the A17 suite
 * R2A/12     2014-06-11 erarafo     Using newly added print utility
 * R3A/2      2015-02-12 erarafo     Optional long delay, trigger data conversion timeout
 * R3A/3      2015-04-23 erarafo     Handle schema update 2.7.0 -> 2.7.1 in s1v2 AND s1v3
 * R3A/6      2015-04-23 erarafo     Deadlock fixed
 * R3A/7      2015-04-23 erarafo     Fix for 2.7.1 -> 4.0.* case
 * R4A/1      2015-04-27 erarafo     Handle Ovett, Cram, Nurmi for 2.* -> 4.*
 * R8A/1      2016-11-25 erarafo     Adapted to change of printInstanceGroups
 * ----------------------------------------------------------------------
 */

#include "ugt_common.h"

#define APPNAME "ugt_s1v2"

#define FAIL_UPGRADE "FAIL_UPGRADE"


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


static long ver(const SafcImmCtSchemaVersionT_2 *version) {
  return 1000000*(long)version->version +
      1000*(long)version->release +
      (long)version->correction;
}


/**
 * Returns true if the copying was NOT ok.
 */
static bool copyOneClass(FILE *log, SafcImmCtHandleT handle, const char *name) {
  fprintf(log, "copy one class: %s\n", name);
  fflush(log);
  SaAisErrorT r = safcImmCtCopyInstances(handle, oneClassName(name));
  return nok("copyInstances", r, log);
}

/**
 * Returns true if the copying was NOT ok.
 */
static bool copyOneClassRt(FILE *log, SafcImmCtHandleT handle, const char *name, char *implementer) {
  fprintf(log, "copy one class: %s, implementer: %s\n", name, implementer);
  fflush(log);
  SaAisErrorT r = safcImmCtCopyRtInstances(handle, implementer, oneClassName(name));
  return nok("copyInstances", r, log);
}


static bool writeOneClassEmpty(FILE *log, SafcImmCtHandleT handle, char *name) {
  fprintf(log, "write one class: %s, zero instances\n", name);
  fflush(log);
  SaAisErrorT r = safcImmCtWriteInstances(
    handle,
    oneInstGroup(
        instGroup(
            name,
            zeroInsts)));
  return nok("writeInstances", r, log);
}


static void doIcti(FILE *log, SafcImmCtHandleT handle) {
  fprintf(log, "doIcti\n");

  SaAisErrorT r;

  // Test referring to a non-existent schema

  SaStringT schemaName = "NoSuchSchema";
  SafcImmCtSchemaVersionT_2 oldSchemaVersion;
  SafcImmCtSchemaVersionT_2 newSchemaVersion;

  delaySeconds(2);

  r = safcImmCtReadSchemaVersion_2(handle, schemaName, &oldSchemaVersion,
      &newSchemaVersion);
  if (ok("readSchemaVersion", r, log)) {
    fprintf(log, "unexpected: ok when reading non-existent schema\n");
    fflush(log);
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
  if (nok("readSchemaVersion", r, log)) {
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

  if (ver(&newSchemaVersion) == ver(&oldSchemaVersion)) {
    fprintf(log, "the 's1v2' schema has not changed, no writing or copying allowed\n");
    fflush(log);
    return;
  }

  if (ver(&oldSchemaVersion) == 2007000 && ver(&newSchemaVersion) == 2007002) {
    fprintf(log, "schema 's1v2', 2.7.0 -> 2.7.2, trivial handling\n");
    fflush(log);

    if (copyOneClass(log, handle, "S1V2RootS1V2") == false &&
        copyOneClass(log, handle, "S1V2AlphaS1V2") == false &&
        writeOneClassEmpty(log, handle, "S1V2Ovett") == false &&
        writeOneClassEmpty(log, handle, "S1V2Cram") == false &&
        writeOneClassEmpty(log, handle, "S1V2Nurmi") == false) {
      fprintf(log, "all copying and writing successful for the 's1v2' schema\n");
      fflush(log);
    }
    return;
  }

  if (ver(&oldSchemaVersion) == 2007001 && ver(&newSchemaVersion) == 2007002) {
    fprintf(log, "schema 's1v2', 2.7.1 -> 2.7.2, trivial handling\n");
    fflush(log);

    if (copyOneClass(log, handle, "S1V2RootS1V2") == false &&
        copyOneClass(log, handle, "S1V2AlphaS1V2") == false &&
        copyOneClass(log, handle, "S1V2Ovett") == false &&
        writeOneClassEmpty(log, handle, "S1V2Cram") == false &&
        writeOneClassEmpty(log, handle, "S1V2Nurmi") == false) {
      fprintf(log, "all copying and writing successful for the 's1v2' schema\n");
      fflush(log);
    }
    return;
  }

  if (oldSchemaVersion.version == 2 &&
      oldSchemaVersion.release == 7 &&
      newSchemaVersion.version == 4 &&
      newSchemaVersion.release == 0) {
    fprintf(log, "schema 's1v2', 2.7.* -> 4.0.*\n");
    fflush(log);

    // This is the case where a modified To-UP has been prepared
    // in order to force the "update" case for the s1v2 schema.

    // The FAIL_UPGRADE value is set in ugt_s1v2_cfg.xml, which
    // may be modified by some test cases.

    if (STREQ(getenv(FAIL_UPGRADE), "true")) {
      // Intentionally fail the upgrade: Used in swm_activate_fail_a16_SUITE.erl

      r = safcImmCtFailUpgrade(handle,
          "intentionally failing this upgrade, application: ugt_s1v2");
      if (nok("failUpgrade", r, log)) {
        return;
      }
      safcImmCtFinalize(handle);
      idle(log, 60);
    }

    delaySeconds(2);

    if (STREQ(getenv("FAIL_UPGRADE"), "A15")) {
      // Wrong classname in safcImmCtCopyInstances()

      r = safcImmCtCopyInstances(handle, oneClassName("S1VRootS1V2"));
      if (nok("copyInstances", r, log)) {
        fprintf(log, "intentional fault_a15 when copy not existing instance\n");
        fflush(log);
        // proceed in test case; it is up to the CS to stop the upgrade
      }
      else {
        fprintf(log, "unexpected: ok when copying non-existing instance\n");
        fflush(log);
      }
    }

    // Copy the root instance
    r = safcImmCtCopyInstances(handle, oneClassName("S1V2RootS1V2"));
    if (nok("copyInstances", r, log)) {
      return;
    }

    delaySeconds(2);

    SafcImmCtInstanceGroupT **s1v2InstGroups;
    if (STREQ(getenv("FAIL_UPGRADE"), "A14")) {
      // changed classname (S1VAlphaS1V2) to a class that does not exist in the From-version
      // First read class that not exist
      r = safcImmCtReadInstances(handle, oneClassName("S1VAlphaS1V2"), &s1v2InstGroups);
      if (nok("readInstances", r, log)) {
        fprintf(log, "intentional fault_a14 when read not existing class\n");
        fflush(log);
      }
      else {
        fprintf(log, "unexpected: ok when reading non-existing class\n");
        fflush(log);
      }
    }

    // Read the AlphaS1V2 instance, get hold of the alphaS attribute value V
    r = safcImmCtReadInstances(handle, oneClassName("S1V2AlphaS1V2"), &s1v2InstGroups);
    if (nok("readInstances", r, log)) {
      return;
    }

    printInstanceGroups(log, s1v2InstGroups, false);

    SafcImmCtInstanceGroupT *s1v2Alphas = s1v2InstGroups[0];
    SafcImmCtInstanceT **s1v2AlphaInsts = s1v2Alphas->instances;

    SafcImmCtInstanceT *s1v2i = s1v2AlphaInsts[0];

    SaStringT s1v2AV = getStringAttribute(s1v2i, "alphaS");

    fprintf(log, "old alphaS string value: %s\n", s1v2AV);
    fflush(log);

    // Create an extended instance that includes alphaT. Set the value to black++V.

    // write the AlphaS1V2 instances (just one)

    if (STREQ(getenv("FAIL_UPGRADE"), "TOO_SLOW")) {
      // This delay will cause a rollback due to the
      // 150 s data conversion timeout. No testcase uses
      // this feature yet.
      delaySeconds(1000);
    }
    else {
      delaySeconds(2);
    }

    if (STREQ(getenv("FAIL_UPGRADE"), "A17")) {
      // invalid attribute name in write operation
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
                              // "alphaT", stringConcat("black", s1v2AV)))))));
                              // Changed alphaT to alphaX i.e,
                              // attribute not supported in the XML structure
                              "alphaX", stringConcat("black", s1v2AV)))))));

      if (nok("writeInstances", r, log)) {
        fprintf(log, "unexpected: code: %d\n", r);
        fflush(log);
      }
      else {
        fprintf(log, "expected: code SA_AIS_OK even though an invalid attribute name (alphaX) was used,\n");
        fprintf(log, "fallback expected later on due to post-reboot 'activate' timer (approximately 150 s)\n");
        fflush(log);
      }
    }
    else {
      r = safcImmCtWriteInstances(handle,
          oneInstGroup(
              instGroup("S1V2AlphaS1V2",
                  oneInstance(
                      inst("alphaS1V2Id", "1",
                          wrapName("S1V2rootS1V2Id=1"),
                          twoStringAttrs("alphaS", s1v2AV,
                              "alphaT",
                              stringConcat("black",
                                  s1v2AV)))))));

      if (nok("writeInstances", r, log)) {
        return;
      }
    }

    // at this point the structure returned by readInstances can be reclaimed
    r = safcImmCtInstanceGroupsMemoryFree(handle, s1v2InstGroups);
    if (nok("instanceGroupsMemoryFree", r, log)) {
      return;
    }

    delaySeconds(2);

    // take care of any S1V2Ovett instances (possibly none)

    char *implementer = "ugt_s1v2_implementer";
    if (copyOneClassRt(log, handle, "S1V2Ovett", implementer) == false) {
      fprintf(
          log,
          "copied instances (possibly zero) of the S1V2Ovett class; implementer set to: %s\n",
          implementer);
      fflush(log);
    }

    // write one S1V2Cram instance, ignore any existing ones
    r = safcImmCtWriteRtInstances(
        handle,
        implementer,
        oneInstGroup(
            instGroup("S1V2Cram",
                oneInstance(
                    inst("cramId", prefixedRdnValue("166"),
                        wrapName("S1V2rootS1V2Id=1"),
                        oneUint32Attr("shoeSize", 166))))));
    if (nok("writeRtInstances", r, log)) {
      fprintf(log, "unexpected: code: %d\n", r);
      fflush(log);
    }

    // copy any Nurmi instances, without altering the implementer
    if (copyOneClass(log, handle, "S1V2Nurmi") == false) {
      fprintf(
          log,
          "copied instances (possibly zero) of the S1V2Nurmi class\n");
      fflush(log);
    }
  }
}

/**
 * Special handling needed because s1v3 schema was extended.
 * Note the awkward naming; this program is ugt_s1v2 but it
 * handles the s1v3 classes too now.
 */
static void doIcti_s1v3(FILE *log, SafcImmCtHandleT handle) {
  fprintf(log, "doIcti_s1v3, using handle: %llu\n", handle);
  fflush(log);

  SaStringT schemaName = "s1v3";
  SafcImmCtSchemaVersionT_2 oldSchemaVersion;
  SafcImmCtSchemaVersionT_2 newSchemaVersion;

  SaAisErrorT r;
  r = safcImmCtReadSchemaVersion_2(
      handle,
      schemaName,
      &oldSchemaVersion,
      &newSchemaVersion);
  if (nok("readSchemaVersion", r, log)) {
    fprintf(log, "unexpected: nok when reading schema\n");
    fflush(log);
    return;
  }

  fprintf(log, "old s1v3 schema version: %d.%d.%d\n",
      oldSchemaVersion.version,
      oldSchemaVersion.release,
      oldSchemaVersion.correction);
  fprintf(log, "new s1v3 schema version: %d.%d.%d\n",
      newSchemaVersion.version,
      newSchemaVersion.release,
      newSchemaVersion.correction);
  fflush(log);

  if (ver(&newSchemaVersion) == ver(&oldSchemaVersion)) {
    fprintf(log, "schema 's1v3' unchanged; no writing allowed\n");
    fflush(log);
    return;
  }

  if (ver(&newSchemaVersion) == 2007001 && ver(&oldSchemaVersion) < 2007001) {
    fprintf(log, "schema 's1v3', 2.7.0 -> 2.7.1, trivial handling\n");
    fflush(log);

    // trust there are no S1V3Coe instances before 2.7.1
    if (copyOneClass(log, handle, "S1V3RootS1V3") == false &&
        copyOneClass(log, handle, "S1V3AlphaS1V3") == false &&
        writeOneClassEmpty(log, handle, "S1V3Coe") == false) {
      fprintf(log, "all copying and writing successful for schema 's1v3'\n");
      fflush(log);
    }
  }
}


int main() {

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

    SafcImmCtHandleT handle;
    SaAisErrorT r = safcImmCtInitialize(&handle);
    if (nok("initialize", r, log)) {
      return 1;
    }

    fprintf(log, "got handle: %llu\n", handle);
    fflush(log);

    doIcti(log, handle);
    doIcti_s1v3(log, handle);

    r = safcImmCtFinalize(handle);
    ok("finalize", r, log);
  }
  fprintf(log, "all done, going idle\n");
  idle(log, 60);
}
