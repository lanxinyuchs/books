/* ----------------------------------------------------------------------
 * %CCaseFile:	noble_icti_dyn.c %
 * %CCaseRev:	/main/R2A/R3A/R4A/R5A/R6A/R8A/R11A/3 %
 * %CCaseDate:	2017-10-06 %
 * %CCaseDocNo: %
 * Author:	erarafo
 * Author: <name>, <e-mail address>
 *
 * Short description:
 * Data conversion during upgrade. Supports the NOBLE, ALKALI, TESTMOM and BUG MOMs.
 * ----------------------------------------------------------------------
 * %CCaseTemplateFile:	template.c %
 * %CCaseTemplateId: CCver: /main/1 %
 *
 * %CCaseCopyrightBegin%
 * Copyright (c) Ericsson AB 2014-2017 All rights reserved.
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
 * R2A/1      2014-03-29 erarafo     Created
 * R2A/2      2014-03-31 erarafo     Refactored
 * R2A/3      2014-03-31 erarafo     Simple banner added
 * R2A/4      2014-04-17 erarafo     Support NOBLE 3.1.0
 * R2A/5      2014-04-18 erarafo     Type fault corrected
 * R3A/1      2014-10-30 erarafo     Groundwork for persistent runtime attributes
 * R3A/2      2014-11-11 erarafo     Fix attribute name problem by upgrade
 * R3A/3      2014-11-11 erarafo     Fixed ANOTHER bloody typo in THIS module
 * R3A/4      2014-11-11 erarafo     And One More
 * R3A/5      2014-11-15 erarafo     Removed all handling of Astatine
 * R3A/6      2015-03-03 erarafo     Added class Tessin
 * R3A/7      2015-03-06 erarafo     Added two attributes
 * R3A/8      2015-03-06 erarafo     Reverted to R3A/6
 * R3A/9      2015-03-07 erarafo     Added one attribute and one ENUM for 3.3.1
 * R3A/10     2015-03-11 erarafo     Added a boolean attribute for 3.3.2
 * R3A/11     2015-04-20 erarafo     No data conversion for 3.3.3
 * R3A/12     2015-04-21 erarafo     Timestamped logfile name
 * R4A/1      2015-05-22 erarafo     Support for schema version 3.4.0
 * R4A/2      2015-06-08 erarafo     Demo of how to read parentName, for Goran Hansson
 * R4A/3      2015-06-09 erarafo     Upgrade engine code corrected
 * R4A/6      2015-06-15 erarafo     Added MOM RcsHwIM
 * R4A/7      2015-06-17 erarafo     Removed MOM named BIRD
 * R4A/8      2015-08-20 erarafo     Handling upgrade for TESTMOM too
 * R4A/9      2015-08-21 erarafo     Handling upgrade for TESTMOM too
 * R4A/11     2015-08-28 erarafo     Added TestClass[ABCD] and Struct4
 * R4A/13     2015-08-30 erarafo     Added attributes of all types to TestClassA
 * R4A/14     2015-12-02 etxpeno     Add doTransport()
 * R5A/2      2016-01-26 etxpeno     Remove support for MOM RcsHwIM
 * R5A/3      2016-04-08 erarafo     TESTMOM 1.2.0 -> 1.3.0
 * R5A/4      2016-04-09 erarafo     TESTMOM 1.2.0 -> 1.3.0, bug fixed
 * R5A/5      2016-04-11 erarafo     TESTMOM 1.3.0 -> 1.4.0, added TestClassF
 * R5A/6      2016-04-11 erarafo     TESTMOM 1.4.0 -> 1.5.0, extended TestClassE, buggy
 * R5A/7      2016-04-12 erarafo     TESTMOM 1.4.0 -> 1.5.0, extended TestClassE, bug fixed
 * R6A/3      2016-05-18 erarafo     BUG 1.1.0, new MOM introduced
 * R6A/4      2016-05-24 erarafo     BUG 1.2.0, classes and structs added, corrections
 * R6A/6      2016-05-25 erarafo     TESTMOM 1.5.0 -> 1.6.0, added TestClassG, StructV
 * R6A/7      2016-06-08 erarafo     BUG 1.2.0 -> 1.3.0, added Cricket[0-5]
 * R8A/1      2016-11-01 erarafo     Investigating cases of instance creation
 * R8A/2      2016-11-25 erarafo     Test of safcImmCtReadInstances_2
 * R11A/2     2017-09-14 etxpeno     TESTMOM 1.6.0 -> 1.7.0
 * R11A/3     2017-10-06 etxpeno     correction of SP549 conversions
 * ----------------------------------------------------------------------
 */

#include "ugt_common.h"

#include <sys/types.h>
#include <signal.h>
// #include <time.h>  .. in ugt_common.h
// #include <stdio.h> .. in ugt_common.h

#define DYN logFileName

static char *logFileName;


#define VERSION(V, R, C)  (1000000*V + 1000*R + C)

static int version(VersionStruct *vs) {
  return VERSION(vs->version, vs->release, vs->correction);
}


struct GroupListMapStruct {
  char *key;
  GroupList value;
  struct GroupListMapStruct *next;
};

typedef struct GroupListMapStruct *GroupListMap;



static GroupList fetch(GroupListMap map, char *key) {
  for (GroupListMap p = map; p != NULL; p = p->next) {
    if (strcmp(key, p->key) == 0) {
      return p->value;
    }
  }
  APPLOG(DYN, "failed to fetch: %s", key);
  return NULL;
}


static void store(GroupListMap *mapP, char *key, GroupList value) {
  if (*mapP == NULL) {
    // map is empty
    GroupListMap new = (GroupListMap)calloc(1, sizeof(struct GroupListMapStruct));
    new->key = key;
    new->value = value;
    new->next = NULL;
    *mapP = new;
  }
  else {
    for (GroupListMap p = *mapP; p != NULL; p = p->next) {
      if (strcmp(key, p->key) == 0) {
        // update existing entry
        p->value = value;
        return;
      }
    }
    // no entry with given key exists
    GroupListMap new = (GroupListMap)calloc(1, sizeof(struct GroupListMapStruct));
    new->key = key;
    new->value = value;
    new->next = *mapP;
    *mapP = new;
  }
}





static void
chattyIdle() {
  int period = 60;
  for (; true; ) {
    APPLOG(DYN, "sleep %d s", period);
    sleep(period);
  }
}



static GroupList
createSingleEmptyInstGroup(char *className) {
  SafcImmCtInstanceGroupT *r =
      (SafcImmCtInstanceGroupT *)calloc(1, sizeof(SafcImmCtInstanceGroupT));
  r->className = className;
  r->instances = zeroInsts;
  return consg(r, zeroGroups);
}


static AttrList fixXenonAttrs(AttrList attrList){
  unsigned int nAttrs = 0;
  for (AttrList p = attrList; *p != NULL; p++) {
    nAttrs++;
  }
  AttrList result = calloc(nAttrs+1, sizeof(Attr));
  for (unsigned int k = 0; k < nAttrs; k++) {
    if (strcmp(attrList[k]->attrName, "peristentNickname") == 0) {
      char **boxedString = calloc(1, sizeof(char *));
      *boxedString = "xenia";
      SaImmAttrValueT *newValuesArray = calloc(1, sizeof(SaImmAttrValueT));
      newValuesArray[0] = (SaImmAttrValueT)boxedString;
      result[k] = calloc(1, sizeof(SaImmAttrValuesT_2));
      result[k]->attrName = "persistentNickname";
      result[k]->attrValueType = attrList[k]->attrValueType;
      result[k]->attrValuesNumber =attrList[k]->attrValuesNumber;
      result[k]->attrValues = newValuesArray;
    }
    else {
      result[k] = attrList[k];
    }
  }
  return result;
}


static Inst fixXenonInst(Inst i) {
  Inst result = calloc(1, sizeof(SafcImmCtInstanceT));
  result->parentName = i->parentName;
  result->attrValues = fixXenonAttrs(i->attrValues);
  return result;
}


/**
 * Trust that the number of instances in the given list
 * is exactly 1. It is known that this is true for existing
 * test data.
 */
static InstList fixXenonInsts(InstList m) {
  InstList result = calloc(1+1, sizeof(Inst));
  result[0] = fixXenonInst(m[0]);
  return result;
}


static Group fixXenonGroup(Group g) {
  Group result = calloc(1, sizeof(GroupStruct));
  result->className = g->className;
  result->instances = fixXenonInsts(g->instances);
  return result;
}


/**
 * Trust that the given list has just one instance group.
 */
static GroupList fixXenonGroupList(GroupList m) {
  GroupList result = calloc(1+1, sizeof(Group));
  result[0] = fixXenonGroup(m[0]);
  return result;
}


/**
 * Print the parent name of the first instance in the
 * first group (not a very generally useful function).
 */
/* static void printFirstInstParentName(char *text, GroupList gg) { */
/*   Group g = gg[0]; */
/*   InstList gii = g->instances; */
/*   Inst gi = gii[0]; */
/*   if (gi == NULL) { */
/*     APPLOG(DYN, "%s: %s\n", text, "no instances"); */
/*   } */
/*   else { */
/*     ParentName gip = gi->parentName; */
/*     char gips[1 + gip->length]; */
/*     for (int k = 0; k < gip->length; k++) { */
/*       gips[k] = (char)gip->value[k]; */
/*     } */
/*     gips[gip->length] = '\0'; */
/*     APPLOG(DYN, "%s: %s\n", text, gips); */
/*   } */
/* } */


static void doReadSysM(Handle handle) {
  APPLOG(DYN, "%s", "begin read SysM");
  GroupList groups = readInsts(handle, "SysM");
  Group group = groups[0];
  if (group == NULL) {
    APPLOG(DYN, "%s", "FAILURE, no SysM instance group");
  }
  else {
    APPLOG(DYN, "%s %s", "got instance group, class:", group->className);
    InstList insts = group->instances;
    Inst inst = insts[0];
    if (inst == NULL) {
      APPLOG(DYN, "%s", "FAILURE, no SysM instance");
    }
    else {
      ParentName parentName = inst->parentName;
      if (parentName == NULL) {
        APPLOG(DYN, "%s", "FAILURE, parentName is NULL");
      }
      else {
        char pn[1+parentName->length];
        for (int k = 0; k < parentName->length; k++) {
          pn[k] = (char)parentName->value[k];
        }
        pn[parentName->length] = '\0';
        APPLOG(DYN, "%s %s", "parentName is: ", pn);
        APPLOG(DYN, "%s", "done read SysM");
      }
    }
  }
}




static GroupList correctLadyBugs(GroupList x) {
  Group g = x[0];
  InstList instances = g->instances;
  // shortcut: we trust that when this function is invoked
  // there is exactly one item in the list.
  Inst theInst = instances[0];
  // we wish to change one attribute value
  AttrList attrList = theInst->attrValues;

  // corrected value
  AttrList attrListCopy = oneMoRefAttr("legs", "id=legs,ladyBugId=1");
  for (int i = 0; attrList[i] != NULL; i++) {
    if (strcmp(attrList[i]->attrName, "legs") == 0) {
      ; // do not copy faulty value from old instance
    }
    else if (strcmp(attrList[i]->attrName, "ladyBugId") == 0) {
      ; // do not copy old RDN since we provide RDN below
    }
    else {
      attrListCopy = consa(attrList[i], attrListCopy);
    }
  }
  return
      oneInstGroup(
          instGroup("LadyBug",
              oneInstance(
                  inst("ladyBugId", "1", NULL,
                      attrListCopy))));
}


static GroupList correctLegs(GroupList x) {
  APPLOG(DYN, "%s: get leading instance group", "BUG");
  Group g = x[0];

  APPLOG(DYN, "%s: get list of instances", "BUG");
  InstList instances = g->instances;


  // shortcut: we trust that when this function is invoked
  // there is exactly one item in the list.

  APPLOG(DYN, "%s: get leading instance", "BUG");
  Inst theInst = instances[0];

  APPLOG(DYN, "%s: get attributes", "BUG");
  AttrList attrList = theInst->attrValues;

  APPLOG(DYN, "%s: copy attributes except RDN attribute", "BUG");
  AttrList attrListCopy = zeroAttrs;
  // copy all attributes except the RDN attribute
  for (int i = 0; attrList[i] != NULL; i++) {
    if (strcmp(attrList[i]->attrName, "id") != 0) {
      attrListCopy = consa(attrList[i], attrListCopy);
    }
  }
  // create the instance with a correct RDN
  APPLOG(DYN, "%s: return one inst group with one corrected inst", "BUG");
  return
      oneInstGroup(
          instGroup("Legs",
              oneInstance(
                  inst("id", "legs", wrapName("ladyBugId=1"),
                      attrListCopy))));
}



static void doBug(Handle handle) {
  APPLOG(DYN, "%s", "begin BUG conversions");
  char *schema = "bug";
  VersionStruct schemaFrom;
  VersionStruct schemaTo;
  readSchemaVersions(handle, schema, &schemaFrom, &schemaTo);
  APPLOG(DYN, "%s schema versions: %u.%u.%u -> %u.%u.%u",
      schema,
      schemaFrom.version,
      schemaFrom.release,
      schemaFrom.correction,
      schemaTo.version,
      schemaTo.release,
      schemaTo.correction);

  int from = version(&schemaFrom);

  GroupListMap m = NULL;

  if (from <= VERSION(0, 0, 0)) {
    if (from == VERSION(0, 0, 0)) {
      ; // nothing can be read, everything must be created
    }

    GroupList groupListLegs =
        oneInstGroup(
            instGroup("Legs",
                oneInstance(
                    inst("id", "legs_1", wrapName("ladyBugId=1"),
                        twoInt32Attrs(
                            "leftFrontLeg", 311,
                            "rightFrontLeg", 411)))));

    GroupList groupListLadyBugs =
        oneInstGroup(
            instGroup("LadyBug",
                oneInstance(
                    inst("ladyBugId", "1", NULL,
                        oneMoRefAttr(
                            "legs", "id=legs_1,ladyBugId=1")))));

    store(&m, "ladyBug_1_1_0", groupListLadyBugs);
    store(&m, "legs_1_1_0", groupListLegs);
  }

  // ======= convert from 1_1_0 to 1_2_0

  if (from <= VERSION(1, 1, 0)) {
    if (from == VERSION(1, 1, 0)) {
      APPLOG(DYN, "%s: read 1_1_0 LadyBug instances", schema);
      store(&m, "ladyBug_1_1_0", readInsts(handle, "LadyBug"));
      APPLOG(DYN, "%s: read 1_1_0 Legs instances", schema);
      store(&m, "legs_1_1_0", readInsts(handle, "Legs"));
      APPLOG(DYN, "%s: read 1_1_0 instances - DONE", schema);
    }
    APPLOG(DYN, "%s: copy LadyBug 1_1_0 instances to 1_2_0", schema);
    store(&m, "ladyBug_1_2_0", correctLadyBugs(fetch(m, "ladyBug_1_1_0")));
    APPLOG(DYN, "%s: copy Legs 1_1_0 instances to 1_2_0", schema);
    store(&m, "legs_1_2_0", correctLegs(fetch(m, "legs_1_1_0")));
    APPLOG(DYN, "%s: create MealyBug 1_2_0 instances", schema);
    store(&m, "mealyBug_1_2_0",
        oneInstGroup(
                    instGroup("MealyBug",
                        oneInstance(
                            inst("mealyBugId", "1", wrapName("ladyBugId=1"),
                                emptyAttrList())))));
    APPLOG(DYN, "%s: create Antennas 1_2_0 instances", schema);
    store(&m, "antennas_1_2_0",
        oneInstGroup(
            instGroup("Antennas",
                oneInstance(
                    inst("id", "antennas", wrapName("stinkBugId=1,ladyBugId=1"),
                        appendAttr(
                            appendAttr(
                                oneMoRefAttr("middleAntenna", "mealyBugId=1,ladyBugId=1"),
                                "leftAntenna", 444),
                                "rightAntenna", 555))))));
    APPLOG(DYN, "%s: create StinkBug 1_2_0 instances", schema);
    store(&m, "stinkBug_1_2_0",
        oneInstGroup(
            instGroup("StinkBug",
                oneInstance(
                    inst("stinkBugId", "1", wrapName("ladyBugId=1"),
                        oneMoRefAttr("antennas", "id=antennas,stinkBugId=1,ladyBugId=1"))))));
  }

  // ======= convert from 1_2_0 to 1_3_0

  if (from <= VERSION(1, 2, 0)) {
    if (from == VERSION(1, 2, 0)) {
      store(&m, "ladyBug_1_2_0",  readInsts(handle, "LadyBug"));
      store(&m, "stinkBug_1_2_0", readInsts(handle, "StinkBug"));
      store(&m, "mealyBug_1_2_0", readInsts(handle, "MealyBug"));
      store(&m, "legs_1_2_0",     readInsts(handle, "Legs"));
      store(&m, "antennas_1_2_0", readInsts(handle, "Antennas"));
    }
    store(&m, "ladyBug_1_3_0",  fetch(m, "ladyBug_1_2_0"));
    store(&m, "stinkBug_1_3_0", fetch(m, "stinkBug_1_2_0"));
    store(&m, "mealyBug_1_3_0", fetch(m, "mealyBug_1_2_0"));
    store(&m, "legs_1_3_0",     fetch(m, "legs_1_2_0"));
    store(&m, "antennas_1_3_0", fetch(m, "antennas_1_2_0"));
    store(&m, "cricket0_1_3_0", createSingleEmptyInstGroup("Cricket0"));
    store(&m, "cricket1_1_3_0", createSingleEmptyInstGroup("Cricket1"));
    store(&m, "cricket2_1_3_0", createSingleEmptyInstGroup("Cricket2"));
    store(&m, "cricket3_1_3_0", createSingleEmptyInstGroup("Cricket3"));
    store(&m, "cricket4_1_3_0", createSingleEmptyInstGroup("Cricket4"));
    store(&m, "cricket5_1_3_0", createSingleEmptyInstGroup("Cricket5"));
  }

  // =================== always this final block, specifying the 'to' version
  if (from <= VERSION(1, 3, 0)) {
    if (from == VERSION(1, 3, 0)) {
      ;
    }
    else {
      APPLOG(DYN, "%s: write 1_3_0 instances", schema);
      writeInsts(handle, fetch(m, "ladyBug_1_3_0"));
      writeInsts(handle, fetch(m, "mealyBug_1_3_0"));
      writeInsts(handle, fetch(m, "stinkBug_1_3_0"));
      writeInsts(handle, fetch(m, "legs_1_3_0"));
      writeInsts(handle, fetch(m, "antennas_1_3_0"));
      writeInsts(handle, fetch(m, "cricket0_1_3_0"));
      writeInsts(handle, fetch(m, "cricket1_1_3_0"));
      writeInsts(handle, fetch(m, "cricket2_1_3_0"));
      writeInsts(handle, fetch(m, "cricket3_1_3_0"));
      writeInsts(handle, fetch(m, "cricket4_1_3_0"));
      writeInsts(handle, fetch(m, "cricket5_1_3_0"));
    }
  }

  APPLOG(DYN, "done %s conversions", schema);
}

static void
doTestmom(Handle handle, bool includeAll, FILE *stream) {
  APPLOG(DYN, "%s", "begin TESTMOM conversions");
  char *schema = "testmom";
  VersionStruct schemaFrom;
  VersionStruct schemaTo;

  {
    // examine instances created by noble_upi
    char *classNames[] = {
        "TESTMOMTestClass4",
        "TESTMOMTestClassA",
        NULL};
    SafcImmCtInstanceGroupT **instGroups;
    SaAisErrorT result =
        includeAll ? safcImmCtReadInstances_2(handle, classNames, &instGroups) :
            safcImmCtReadInstances(handle, classNames, &instGroups);
    if (result != SA_AIS_OK) {
      APPLOG(DYN, "failed to read selected TESTMOM instances, code: %d", result);
    }
    else {
      APPLOG(DYN, "successfully read selected TESTMOM instances, include all: %s",
          includeAll ? "yes" : "no");
      APPLOG(DYN, "displaying selected TESTMOM instances, stream: %d", (stream != NULL));
      printInstanceGroups(stream, instGroups, includeAll);
      safcImmCtInstanceGroupsMemoryFree(handle, instGroups);
      APPLOG(DYN, "%s", "display done");
    }
  }

  readSchemaVersions(handle, schema, &schemaFrom, &schemaTo);
  APPLOG(DYN, "testmom schema versions: %u.%u.%u -> %u.%u.%u",
      schemaFrom.version,
      schemaFrom.release,
      schemaFrom.correction,
      schemaTo.version,
      schemaTo.release,
      schemaTo.correction);

  GroupListMap m = NULL;

  // 0_0_0 -> 1_0_0 conversions

  if (version(&schemaFrom) <= VERSION(0, 0, 0)) {
    if (version(&schemaFrom) == VERSION(0, 0, 0)) {
      store(&m, "testRoot_0_0_0", readInsts(handle, "TESTMOMTestRoot"));
      store(&m, "testClass1_0_0_0", readInsts(handle, "TESTMOMTestClass1"));
      store(&m, "testClass2_0_0_0", readInsts(handle, "TESTMOMTestClass2"));
      //testClass3_0_0_0 = readInsts(handle, "TESTMOMTestClass3");
      store(&m, "testClass4_0_0_0", readInsts(handle, "TESTMOMTestClass4"));
      store(&m, "testClass6_0_0_0", readInsts(handle, "TESTMOMTestClass6"));
      store(&m, "testClass7_0_0_0", readInsts(handle, "TESTMOMTestClass7"));
      store(&m, "testClass8_0_0_0", readInsts(handle, "TESTMOMTestClass8"));
      store(&m, "testClass9_0_0_0", readInsts(handle, "TESTMOMTestClass9"));
      store(&m, "struct1_0_0_0", readInsts(handle, "TESTMOMStruct1"));
      store(&m, "struct2_0_0_0", readInsts(handle, "TESTMOMStruct2"));
      //struct3_0_0_0 = readInsts(handle, "TESTMOMStruct3");

    }
    store(&m, "testRoot_1_0_0", fetch(m, "testRoot_0_0_0"));
    store(&m, "testClass1_1_0_0", fetch(m, "testClass1_0_0_0"));
    store(&m, "testClass2_1_0_0", fetch(m, "testClass2_0_0_0"));

    Inst c3i1 = inst("testClass3Id", "1",
        wrapName("testClass2Id=1,TESTMOMtestRoot=1"),
        oneMorefs2Attr("struct3seq",
            "id=struct3seq_1,testClass3Id=1,testClass2Id=1,TESTMOMtestRoot=1",
            "id=struct3seq_2,testClass3Id=1,testClass2Id=1,TESTMOMtestRoot=1"));

    InstList class3insts = consi(c3i1, zeroInsts);
    Group class3ig = createGroup("TESTMOMTestClass3", class3insts);
    store(&m, "testClass3_1_0_0", oneInstGroup(class3ig));

    store(&m, "testClass4_1_0_0", fetch(m, "testClass4_0_0_0"));
    store(&m, "testClass6_1_0_0", fetch(m, "testClass6_0_0_0"));
    store(&m, "testClass7_1_0_0", fetch(m, "testClass7_0_0_0"));
    store(&m, "testClass8_1_0_0", fetch(m, "testClass8_0_0_0"));
    store(&m, "testClass9_1_0_0", fetch(m, "testClass9_0_0_0"));
    store(&m, "struct1_1_0_0", fetch(m, "struct1_0_0_0"));
    store(&m, "struct2_1_0_0", fetch(m, "struct2_0_0_0"));

    // we could actually retrieve the '1' and '2' values
    // by reading from struct3_0_0_0 ...
    Inst si1 = inst("id", "struct3seq_1",
        wrapName("testClass3Id=1,testClass2Id=1,TESTMOMtestRoot=1"),
        oneInt32Attr("int32", 1));

    Inst si2 = inst("id", "struct3seq_2",
        wrapName("testClass3Id=1,testClass2Id=1,TESTMOMtestRoot=1"),
        oneInt32Attr("int32", 2));

    InstList struct3insts = consi(si1, consi(si2, zeroInsts));
    Group struct3ig = createGroup("TESTMOMStruct3", struct3insts);
    store(&m, "struct3_1_0_0", oneInstGroup(struct3ig));
  }

  // 1_0_0 -> 1_1_0 conversions

  if (version(&schemaFrom) <= VERSION(1, 0, 0)) {
    if (version(&schemaFrom) == VERSION(1, 0, 0)) {
      store(&m, "testRoot_1_0_0", readInsts(handle, "TESTMOMTestRoot"));
      store(&m, "testClass1_1_0_0", readInsts(handle, "TESTMOMTestClass1"));
      store(&m, "testClass2_1_0_0", readInsts(handle, "TESTMOMTestClass2"));
      store(&m, "testClass3_1_0_0", readInsts(handle, "TESTMOMTestClass3"));
      store(&m, "testClass4_1_0_0", readInsts(handle, "TESTMOMTestClass4"));
      store(&m, "testClass6_1_0_0", readInsts(handle, "TESTMOMTestClass6"));
      store(&m, "testClass7_1_0_0", readInsts(handle, "TESTMOMTestClass7"));
      store(&m, "testClass8_1_0_0", readInsts(handle, "TESTMOMTestClass8"));
      store(&m, "testClass9_1_0_0", readInsts(handle, "TESTMOMTestClass9"));
      store(&m, "struct1_1_0_0", readInsts(handle, "TESTMOMStruct1"));
      store(&m, "struct2_1_0_0", readInsts(handle, "TESTMOMStruct2"));
      store(&m, "struct3_1_0_0", readInsts(handle, "TESTMOMStruct3"));
    }
    store(&m, "testRoot_1_1_0", fetch(m, "testRoot_1_0_0"));
    store(&m, "testClass1_1_1_0", fetch(m, "testClass1_1_0_0"));
    store(&m, "testClass2_1_1_0", fetch(m, "testClass2_1_0_0"));
    store(&m, "testClass3_1_1_0", fetch(m, "testClass3_1_0_0"));
    store(&m, "testClass4_1_1_0", fetch(m, "testClass4_1_0_0"));
    store(&m, "testClass6_1_1_0", fetch(m, "testClass6_1_0_0"));
    store(&m, "testClass7_1_1_0", fetch(m, "testClass7_1_0_0"));
    store(&m, "testClass8_1_1_0", fetch(m, "testClass8_1_0_0"));
    store(&m, "testClass9_1_1_0", fetch(m, "testClass9_1_0_0"));
    store(&m, "testClassA_1_1_0", oneInstGroup(createGroup("TESTMOMTestClassA", zeroInsts)));
    store(&m, "testClassB_1_1_0", oneInstGroup(createGroup("TESTMOMTestClassB", zeroInsts)));
    store(&m, "testClassC_1_1_0", oneInstGroup(createGroup("TESTMOMTestClassC", zeroInsts)));
    store(&m, "testClassD_1_1_0", oneInstGroup(createGroup("TESTMOMTestClassD", zeroInsts)));
    store(&m, "struct1_1_1_0", fetch(m, "struct1_1_0_0"));
    store(&m, "struct2_1_1_0", fetch(m, "struct2_1_0_0"));
    store(&m, "struct3_1_1_0", fetch(m, "struct3_1_0_0"));
    store(&m, "struct4_1_1_0", oneInstGroup(createGroup("TESTMOMStruct4", zeroInsts)));
  }

  // 1_1_0 -> 1_2_0 conversions
  // Entirely trivial upgrade this time

  if (version(&schemaFrom) <= VERSION(1, 1, 0)) {
    if (version(&schemaFrom) == VERSION(1, 1, 0)) {
      store(&m, "testRoot_1_1_0", readInsts(handle, "TESTMOMTestRoot"));
      store(&m, "testClass1_1_1_0", readInsts(handle, "TESTMOMTestClass1"));
      store(&m, "testClass2_1_1_0", readInsts(handle, "TESTMOMTestClass2"));
      store(&m, "testClass3_1_1_0", readInsts(handle, "TESTMOMTestClass3"));
      store(&m, "testClass4_1_1_0", readInsts(handle, "TESTMOMTestClass4"));
      store(&m, "testClass6_1_1_0", readInsts(handle, "TESTMOMTestClass6"));
      store(&m, "testClass7_1_1_0", readInsts(handle, "TESTMOMTestClass7"));
      store(&m, "testClass8_1_1_0", readInsts(handle, "TESTMOMTestClass8"));
      store(&m, "testClass9_1_1_0", readInsts(handle, "TESTMOMTestClass9"));
      store(&m, "testClassA_1_1_0", readInsts(handle, "TESTMOMTestClassA"));
      store(&m, "testClassB_1_1_0", readInsts(handle, "TESTMOMTestClassB"));
      store(&m, "testClassC_1_1_0", readInsts(handle, "TESTMOMTestClassC"));
      store(&m, "testClassD_1_1_0", readInsts(handle, "TESTMOMTestClassD"));
      store(&m, "struct1_1_1_0", readInsts(handle, "TESTMOMStruct1"));
      store(&m, "struct2_1_1_0", readInsts(handle, "TESTMOMStruct2"));
      store(&m, "struct3_1_1_0", readInsts(handle, "TESTMOMStruct3"));
      store(&m, "struct4_1_1_0", readInsts(handle, "TESTMOMStruct4"));
    }
    store(&m, "testRoot_1_2_0", fetch(m, "testRoot_1_1_0"));
    store(&m, "testClass1_1_2_0", fetch(m, "testClass1_1_1_0"));
    store(&m, "testClass2_1_2_0", fetch(m, "testClass2_1_1_0"));
    store(&m, "testClass3_1_2_0", fetch(m, "testClass3_1_1_0"));
    store(&m, "testClass4_1_2_0", fetch(m, "testClass4_1_1_0"));
    store(&m, "testClass6_1_2_0", fetch(m, "testClass6_1_1_0"));
    store(&m, "testClass7_1_2_0", fetch(m, "testClass7_1_1_0"));
    store(&m, "testClass8_1_2_0", fetch(m, "testClass8_1_1_0"));
    store(&m, "testClass9_1_2_0", fetch(m, "testClass9_1_1_0"));
    store(&m, "testClassA_1_2_0", fetch(m, "testClassA_1_1_0"));
    store(&m, "testClassB_1_2_0", fetch(m, "testClassB_1_1_0"));
    store(&m, "testClassC_1_2_0", fetch(m, "testClassC_1_1_0"));
    store(&m, "testClassD_1_2_0", fetch(m, "testClassD_1_1_0"));
    store(&m, "struct1_1_2_0", fetch(m, "struct1_1_1_0"));
    store(&m, "struct2_1_2_0", fetch(m, "struct2_1_1_0"));
    store(&m, "struct3_1_2_0", fetch(m, "struct3_1_1_0"));
    store(&m, "struct4_1_2_0", fetch(m, "struct4_1_1_0"));
  }

  // 1_2_0 -> 1_3_0 conversions
  // Adding TestClassE, StructP, StructS

  if (version(&schemaFrom) <= VERSION(1, 2, 0)) {
    if (version(&schemaFrom) == VERSION(1, 2, 0)) {
      store(&m, "testRoot_1_2_0", readInsts(handle, "TESTMOMTestRoot"));
      store(&m, "testClass1_1_2_0", readInsts(handle, "TESTMOMTestClass1"));
      store(&m, "testClass2_1_2_0", readInsts(handle, "TESTMOMTestClass2"));
      store(&m, "testClass3_1_2_0", readInsts(handle, "TESTMOMTestClass3"));
      store(&m, "testClass4_1_2_0", readInsts(handle, "TESTMOMTestClass4"));
      store(&m, "testClass6_1_2_0", readInsts(handle, "TESTMOMTestClass6"));
      store(&m, "testClass7_1_2_0", readInsts(handle, "TESTMOMTestClass7"));
      store(&m, "testClass8_1_2_0", readInsts(handle, "TESTMOMTestClass8"));
      store(&m, "testClass9_1_2_0", readInsts(handle, "TESTMOMTestClass9"));
      store(&m, "testClassA_1_2_0", readInsts(handle, "TESTMOMTestClassA"));
      store(&m, "testClassB_1_2_0", readInsts(handle, "TESTMOMTestClassB"));
      store(&m, "testClassC_1_2_0", readInsts(handle, "TESTMOMTestClassC"));
      store(&m, "testClassD_1_2_0", readInsts(handle, "TESTMOMTestClassD"));
      store(&m, "struct1_1_2_0", readInsts(handle, "TESTMOMStruct1"));
      store(&m, "struct2_1_2_0", readInsts(handle, "TESTMOMStruct2"));
      store(&m, "struct3_1_2_0", readInsts(handle, "TESTMOMStruct3"));
      store(&m, "struct4_1_2_0", readInsts(handle, "TESTMOMStruct4"));
    }
    store(&m, "testRoot_1_3_0", fetch(m, "testRoot_1_2_0"));
    store(&m, "testClass1_1_3_0", fetch(m, "testClass1_1_2_0"));
    store(&m, "testClass2_1_3_0", fetch(m, "testClass2_1_2_0"));
    store(&m, "testClass3_1_3_0", fetch(m, "testClass3_1_2_0"));
    store(&m, "testClass4_1_3_0", fetch(m, "testClass4_1_2_0"));
    store(&m, "testClass6_1_3_0", fetch(m, "testClass6_1_2_0"));
    store(&m, "testClass7_1_3_0", fetch(m, "testClass7_1_2_0"));
    store(&m, "testClass8_1_3_0", fetch(m, "testClass8_1_2_0"));
    store(&m, "testClass9_1_3_0", fetch(m, "testClass9_1_2_0"));
    store(&m, "testClassA_1_3_0", fetch(m, "testClassA_1_2_0"));
    store(&m, "testClassB_1_3_0", fetch(m, "testClassB_1_2_0"));
    store(&m, "testClassC_1_3_0", fetch(m, "testClassC_1_2_0"));
    store(&m, "testClassD_1_3_0", fetch(m, "testClassD_1_2_0"));
    store(&m, "testClassE_1_3_0", oneInstGroup(createGroup("TESTMOMTestClassE", zeroInsts)));
    store(&m, "struct1_1_3_0", fetch(m, "struct1_1_2_0"));
    store(&m, "struct2_1_3_0", fetch(m, "struct2_1_2_0"));
    store(&m, "struct3_1_3_0", fetch(m, "struct3_1_2_0"));
    store(&m, "struct4_1_3_0", fetch(m, "struct4_1_2_0"));
    store(&m, "structP_1_3_0", oneInstGroup(createGroup("TESTMOMStructP", zeroInsts)));
    store(&m, "structS_1_3_0", oneInstGroup(createGroup("TESTMOMStructS", zeroInsts)));
  }

  // 1_3_0 -> 1_4_0 conversions
  // Adding TestClassF

  if (version(&schemaFrom) <= VERSION(1, 3, 0)) {
    if (version(&schemaFrom) == VERSION(1, 3, 0)) {
      store(&m, "testRoot_1_3_0", readInsts(handle, "TESTMOMTestRoot"));
      store(&m, "testClass1_1_3_0", readInsts(handle, "TESTMOMTestClass1"));
      store(&m, "testClass2_1_3_0", readInsts(handle, "TESTMOMTestClass2"));
      store(&m, "testClass3_1_3_0", readInsts(handle, "TESTMOMTestClass3"));
      store(&m, "testClass4_1_3_0", readInsts(handle, "TESTMOMTestClass4"));
      store(&m, "testClass6_1_3_0", readInsts(handle, "TESTMOMTestClass6"));
      store(&m, "testClass7_1_3_0", readInsts(handle, "TESTMOMTestClass7"));
      store(&m, "testClass8_1_3_0", readInsts(handle, "TESTMOMTestClass8"));
      store(&m, "testClass9_1_3_0", readInsts(handle, "TESTMOMTestClass9"));
      store(&m, "testClassA_1_3_0", readInsts(handle, "TESTMOMTestClassA"));
      store(&m, "testClassB_1_3_0", readInsts(handle, "TESTMOMTestClassB"));
      store(&m, "testClassC_1_3_0", readInsts(handle, "TESTMOMTestClassC"));
      store(&m, "testClassD_1_3_0", readInsts(handle, "TESTMOMTestClassD"));
      store(&m, "testClassE_1_3_0", readInsts(handle, "TESTMOMTestClassE"));
      store(&m, "struct1_1_3_0", readInsts(handle, "TESTMOMStruct1"));
      store(&m, "struct2_1_3_0", readInsts(handle, "TESTMOMStruct2"));
      store(&m, "struct3_1_3_0", readInsts(handle, "TESTMOMStruct3"));
      store(&m, "struct4_1_3_0", readInsts(handle, "TESTMOMStruct4"));
      store(&m, "structP_1_3_0", readInsts(handle, "TESTMOMStructP"));
      store(&m, "structS_1_3_0", readInsts(handle, "TESTMOMStructS"));
    }
    store(&m, "testRoot_1_4_0", fetch(m, "testRoot_1_3_0"));
    store(&m, "testClass1_1_4_0", fetch(m, "testClass1_1_3_0"));
    store(&m, "testClass2_1_4_0", fetch(m, "testClass2_1_3_0"));
    store(&m, "testClass3_1_4_0", fetch(m, "testClass3_1_3_0"));
    store(&m, "testClass4_1_4_0", fetch(m, "testClass4_1_3_0"));
    store(&m, "testClass6_1_4_0", fetch(m, "testClass6_1_3_0"));
    store(&m, "testClass7_1_4_0", fetch(m, "testClass7_1_3_0"));
    store(&m, "testClass8_1_4_0", fetch(m, "testClass8_1_3_0"));
    store(&m, "testClass9_1_4_0", fetch(m, "testClass9_1_3_0"));
    store(&m, "testClassA_1_4_0", fetch(m, "testClassA_1_3_0"));
    store(&m, "testClassB_1_4_0", fetch(m, "testClassB_1_3_0"));
    store(&m, "testClassC_1_4_0", fetch(m, "testClassC_1_3_0"));
    store(&m, "testClassD_1_4_0", fetch(m, "testClassD_1_3_0"));
    store(&m, "testClassE_1_4_0", fetch(m, "testClassE_1_3_0"));
    store(&m, "testClassF_1_4_0", oneInstGroup(createGroup("TESTMOMTestClassF", zeroInsts)));
    store(&m, "struct1_1_4_0", fetch(m, "struct1_1_3_0"));
    store(&m, "struct2_1_4_0", fetch(m, "struct2_1_3_0"));
    store(&m, "struct3_1_4_0", fetch(m, "struct3_1_3_0"));
    store(&m, "struct4_1_4_0", fetch(m, "struct4_1_3_0"));
    store(&m, "structP_1_4_0", fetch(m, "structP_1_3_0"));
    store(&m, "structS_1_4_0", fetch(m, "structS_1_3_0"));
  }

  // 1_4_0 -> 1_5_0 conversions
  // Extending TestClassE, StructP and StructS; no need to covert any instances

  if (version(&schemaFrom) <= VERSION(1, 4, 0)) {
    if (version(&schemaFrom) == VERSION(1, 4, 0)) {
      store(&m, "testRoot_1_4_0", readInsts(handle, "TESTMOMTestRoot"));
      store(&m, "testClass1_1_4_0", readInsts(handle, "TESTMOMTestClass1"));
      store(&m, "testClass2_1_4_0", readInsts(handle, "TESTMOMTestClass2"));
      store(&m, "testClass3_1_4_0", readInsts(handle, "TESTMOMTestClass3"));
      store(&m, "testClass4_1_4_0", readInsts(handle, "TESTMOMTestClass4"));
      store(&m, "testClass6_1_4_0", readInsts(handle, "TESTMOMTestClass6"));
      store(&m, "testClass7_1_4_0", readInsts(handle, "TESTMOMTestClass7"));
      store(&m, "testClass8_1_4_0", readInsts(handle, "TESTMOMTestClass8"));
      store(&m, "testClass9_1_4_0", readInsts(handle, "TESTMOMTestClass9"));
      store(&m, "testClassA_1_4_0", readInsts(handle, "TESTMOMTestClassA"));
      store(&m, "testClassB_1_4_0", readInsts(handle, "TESTMOMTestClassB"));
      store(&m, "testClassC_1_4_0", readInsts(handle, "TESTMOMTestClassC"));
      store(&m, "testClassD_1_4_0", readInsts(handle, "TESTMOMTestClassD"));
      store(&m, "testClassE_1_4_0", readInsts(handle, "TESTMOMTestClassE"));
      store(&m, "testClassF_1_4_0", readInsts(handle, "TESTMOMTestClassF"));
      store(&m, "struct1_1_4_0", readInsts(handle, "TESTMOMStruct1"));
      store(&m, "struct2_1_4_0", readInsts(handle, "TESTMOMStruct2"));
      store(&m, "struct3_1_4_0", readInsts(handle, "TESTMOMStruct3"));
      store(&m, "struct4_1_4_0", readInsts(handle, "TESTMOMStruct4"));
      store(&m, "structP_1_4_0", readInsts(handle, "TESTMOMStructP"));
      store(&m, "structS_1_4_0", readInsts(handle, "TESTMOMStructS"));
    }
    store(&m, "testRoot_1_5_0", fetch(m, "testRoot_1_4_0"));
    store(&m, "testClass1_1_5_0", fetch(m, "testClass1_1_4_0"));
    store(&m, "testClass2_1_5_0", fetch(m, "testClass2_1_4_0"));
    store(&m, "testClass3_1_5_0", fetch(m, "testClass3_1_4_0"));
    store(&m, "testClass4_1_5_0", fetch(m, "testClass4_1_4_0"));
    store(&m, "testClass6_1_5_0", fetch(m, "testClass6_1_4_0"));
    store(&m, "testClass7_1_5_0", fetch(m, "testClass7_1_4_0"));
    store(&m, "testClass8_1_5_0", fetch(m, "testClass8_1_4_0"));
    store(&m, "testClass9_1_5_0", fetch(m, "testClass9_1_4_0"));
    store(&m, "testClassA_1_5_0", fetch(m, "testClassA_1_4_0"));
    store(&m, "testClassB_1_5_0", fetch(m, "testClassB_1_4_0"));
    store(&m, "testClassC_1_5_0", fetch(m, "testClassC_1_4_0"));
    store(&m, "testClassD_1_5_0", fetch(m, "testClassD_1_4_0"));
    store(&m, "testClassE_1_5_0", fetch(m, "testClassE_1_4_0"));
    store(&m, "testClassF_1_5_0", fetch(m, "testClassF_1_4_0"));
    store(&m, "struct1_1_5_0", fetch(m, "struct1_1_4_0"));
    store(&m, "struct2_1_5_0", fetch(m, "struct2_1_4_0"));
    store(&m, "struct3_1_5_0", fetch(m, "struct3_1_4_0"));
    store(&m, "struct4_1_5_0", fetch(m, "struct4_1_4_0"));
    store(&m, "structP_1_5_0", fetch(m, "structP_1_4_0"));
    store(&m, "structS_1_5_0", fetch(m, "structS_1_4_0"));
  }

  // 1_5_0 -> 1_6_0 conversions
  // Adding TestClassG and StructV; no instances

  if (version(&schemaFrom) <= VERSION(1, 5, 0)) {
    if (version(&schemaFrom) == VERSION(1, 5, 0)) {
      store(&m, "testRoot_1_5_0", readInsts(handle, "TESTMOMTestRoot"));
      store(&m, "testClass1_1_5_0", readInsts(handle, "TESTMOMTestClass1"));
      store(&m, "testClass2_1_5_0", readInsts(handle, "TESTMOMTestClass2"));
      store(&m, "testClass3_1_5_0", readInsts(handle, "TESTMOMTestClass3"));
      store(&m, "testClass4_1_5_0", readInsts(handle, "TESTMOMTestClass4"));
      store(&m, "testClass6_1_5_0", readInsts(handle, "TESTMOMTestClass6"));
      store(&m, "testClass7_1_5_0", readInsts(handle, "TESTMOMTestClass7"));
      store(&m, "testClass8_1_5_0", readInsts(handle, "TESTMOMTestClass8"));
      store(&m, "testClass9_1_5_0", readInsts(handle, "TESTMOMTestClass9"));
      store(&m, "testClassA_1_5_0", readInsts(handle, "TESTMOMTestClassA"));
      store(&m, "testClassB_1_5_0", readInsts(handle, "TESTMOMTestClassB"));
      store(&m, "testClassC_1_5_0", readInsts(handle, "TESTMOMTestClassC"));
      store(&m, "testClassD_1_5_0", readInsts(handle, "TESTMOMTestClassD"));
      store(&m, "testClassE_1_5_0", readInsts(handle, "TESTMOMTestClassE"));
      store(&m, "testClassF_1_5_0", readInsts(handle, "TESTMOMTestClassF"));
      store(&m, "struct1_1_5_0", readInsts(handle, "TESTMOMStruct1"));
      store(&m, "struct2_1_5_0", readInsts(handle, "TESTMOMStruct2"));
      store(&m, "struct3_1_5_0", readInsts(handle, "TESTMOMStruct3"));
      store(&m, "struct4_1_5_0", readInsts(handle, "TESTMOMStruct4"));
      store(&m, "structP_1_5_0", readInsts(handle, "TESTMOMStructP"));
      store(&m, "structS_1_5_0", readInsts(handle, "TESTMOMStructS"));
    }
    store(&m, "testRoot_1_6_0", fetch(m, "testRoot_1_5_0"));
    store(&m, "testClass1_1_6_0", fetch(m, "testClass1_1_5_0"));
    store(&m, "testClass2_1_6_0", fetch(m, "testClass2_1_5_0"));
    store(&m, "testClass3_1_6_0", fetch(m, "testClass3_1_5_0"));
    store(&m, "testClass4_1_6_0", fetch(m, "testClass4_1_5_0"));
    store(&m, "testClass6_1_6_0", fetch(m, "testClass6_1_5_0"));
    store(&m, "testClass7_1_6_0", fetch(m, "testClass7_1_5_0"));
    store(&m, "testClass8_1_6_0", fetch(m, "testClass8_1_5_0"));
    store(&m, "testClass9_1_6_0", fetch(m, "testClass9_1_5_0"));
    store(&m, "testClassA_1_6_0", fetch(m, "testClassA_1_5_0"));
    store(&m, "testClassB_1_6_0", fetch(m, "testClassB_1_5_0"));
    store(&m, "testClassC_1_6_0", fetch(m, "testClassC_1_5_0"));
    store(&m, "testClassD_1_6_0", fetch(m, "testClassD_1_5_0"));
    store(&m, "testClassE_1_6_0", fetch(m, "testClassE_1_5_0"));
    store(&m, "testClassF_1_6_0", fetch(m, "testClassF_1_5_0"));
    store(&m, "testClassG_1_6_0", oneInstGroup(createGroup("TESTMOMTestClassG", zeroInsts)));
    store(&m, "struct1_1_6_0", fetch(m, "struct1_1_5_0"));
    store(&m, "struct2_1_6_0", fetch(m, "struct2_1_5_0"));
    store(&m, "struct3_1_6_0", fetch(m, "struct3_1_5_0"));
    store(&m, "struct4_1_6_0", fetch(m, "struct4_1_5_0"));
    store(&m, "structP_1_6_0", fetch(m, "structP_1_5_0"));
    store(&m, "structS_1_6_0", fetch(m, "structS_1_5_0"));
    store(&m, "structV_1_6_0", oneInstGroup(createGroup("TESTMOMStructV", zeroInsts)));
  }

  // 1_6_0 -> 1_7_0 conversions
  // Adding LeafConfigReservedMo and TESTMOMLeafConfigReserver; no instances

  if (version(&schemaFrom) <= VERSION(1, 6, 0)) {
    if (version(&schemaFrom) == VERSION(1, 6, 0)) {
      store(&m, "testRoot_1_6_0", readInsts(handle, "TESTMOMTestRoot"));
      store(&m, "testClass1_1_6_0", readInsts(handle, "TESTMOMTestClass1"));
      store(&m, "testClass2_1_6_0", readInsts(handle, "TESTMOMTestClass2"));
      store(&m, "testClass3_1_6_0", readInsts(handle, "TESTMOMTestClass3"));
      store(&m, "testClass4_1_6_0", readInsts(handle, "TESTMOMTestClass4"));
      store(&m, "testClass6_1_6_0", readInsts(handle, "TESTMOMTestClass6"));
      store(&m, "testClass7_1_6_0", readInsts(handle, "TESTMOMTestClass7"));
      store(&m, "testClass8_1_6_0", readInsts(handle, "TESTMOMTestClass8"));
      store(&m, "testClass9_1_6_0", readInsts(handle, "TESTMOMTestClass9"));
      store(&m, "testClassA_1_6_0", readInsts(handle, "TESTMOMTestClassA"));
      store(&m, "testClassB_1_6_0", readInsts(handle, "TESTMOMTestClassB"));
      store(&m, "testClassC_1_6_0", readInsts(handle, "TESTMOMTestClassC"));
      store(&m, "testClassD_1_6_0", readInsts(handle, "TESTMOMTestClassD"));
      store(&m, "testClassE_1_6_0", readInsts(handle, "TESTMOMTestClassE"));
      store(&m, "testClassF_1_6_0", readInsts(handle, "TESTMOMTestClassF"));
      store(&m, "testClassG_1_6_0", readInsts(handle, "TESTMOMTestClassG"));
      store(&m, "struct1_1_6_0", readInsts(handle, "TESTMOMStruct1"));
      store(&m, "struct2_1_6_0", readInsts(handle, "TESTMOMStruct2"));
      store(&m, "struct3_1_6_0", readInsts(handle, "TESTMOMStruct3"));
      store(&m, "struct4_1_6_0", readInsts(handle, "TESTMOMStruct4"));
      store(&m, "structP_1_6_0", readInsts(handle, "TESTMOMStructP"));
      store(&m, "structS_1_6_0", readInsts(handle, "TESTMOMStructS"));
      store(&m, "structV_1_6_0", readInsts(handle, "TESTMOMStructV"));
    }
    store(&m, "testRoot_1_7_0", fetch(m, "testRoot_1_6_0"));
    store(&m, "testClass1_1_7_0", fetch(m, "testClass1_1_6_0"));
    store(&m, "testClass2_1_7_0", fetch(m, "testClass2_1_6_0"));
    store(&m, "testClass3_1_7_0", fetch(m, "testClass3_1_6_0"));
    store(&m, "testClass4_1_7_0", fetch(m, "testClass4_1_6_0"));
    store(&m, "testClass6_1_7_0", fetch(m, "testClass6_1_6_0"));
    store(&m, "testClass7_1_7_0", fetch(m, "testClass7_1_6_0"));
    store(&m, "testClass8_1_7_0", fetch(m, "testClass8_1_6_0"));
    store(&m, "testClass9_1_7_0", fetch(m, "testClass9_1_6_0"));
    store(&m, "testClassA_1_7_0", fetch(m, "testClassA_1_6_0"));
    store(&m, "testClassB_1_7_0", fetch(m, "testClassB_1_6_0"));
    store(&m, "testClassC_1_7_0", fetch(m, "testClassC_1_6_0"));
    store(&m, "testClassD_1_7_0", fetch(m, "testClassD_1_6_0"));
    store(&m, "testClassE_1_7_0", fetch(m, "testClassE_1_6_0"));
    store(&m, "testClassF_1_7_0", fetch(m, "testClassF_1_6_0"));
    store(&m, "testClassG_1_7_0", fetch(m, "testClassG_1_6_0"));
    store(&m, "leafConfigReservedMo_1_7_0", oneInstGroup(createGroup("TESTMOMLeafConfigReservedMo", zeroInsts)));
    store(&m, "leafConfigReserver_1_7_0", oneInstGroup(createGroup("TESTMOMLeafConfigReserver", zeroInsts)));
    store(&m, "struct1_1_7_0", fetch(m, "struct1_1_6_0"));
    store(&m, "struct2_1_7_0", fetch(m, "struct2_1_6_0"));
    store(&m, "struct3_1_7_0", fetch(m, "struct3_1_6_0"));
    store(&m, "struct4_1_7_0", fetch(m, "struct4_1_6_0"));
    store(&m, "structP_1_7_0", fetch(m, "structP_1_6_0"));
    store(&m, "structS_1_7_0", fetch(m, "structS_1_6_0"));
    store(&m, "structV_1_7_0", fetch(m, "structV_1_6_0"));
  }

  // ================ always this last block, specifying the to-version everywhere

  if (version(&schemaFrom) <= VERSION(1, 7, 0)) {
    if (version(&schemaFrom) == VERSION(1, 7, 0)) {
      ;
    }
    else {
      writeInsts(handle, fetch(m, "testRoot_1_7_0"));
      writeInsts(handle, fetch(m, "testClass1_1_7_0"));
      writeInsts(handle, fetch(m, "testClass2_1_7_0"));
      writeInsts(handle, fetch(m, "testClass3_1_7_0"));
      writeInsts(handle, fetch(m, "testClass4_1_7_0"));
      writeInsts(handle, fetch(m, "testClass6_1_7_0"));
      writeInsts(handle, fetch(m, "testClass7_1_7_0"));
      writeInsts(handle, fetch(m, "testClass8_1_7_0"));
      writeInsts(handle, fetch(m, "testClass9_1_7_0"));
      writeInsts(handle, fetch(m, "testClassA_1_7_0"));
      writeInsts(handle, fetch(m, "testClassB_1_7_0"));
      writeInsts(handle, fetch(m, "testClassC_1_7_0"));
      writeInsts(handle, fetch(m, "testClassD_1_7_0"));
      writeInsts(handle, fetch(m, "testClassE_1_7_0"));
      writeInsts(handle, fetch(m, "testClassF_1_7_0"));
      writeInsts(handle, fetch(m, "testClassG_1_7_0"));
      writeInsts(handle, fetch(m, "leafConfigReservedMo_1_7_0"));
      writeInsts(handle, fetch(m, "leafConfigReserver_1_7_0"));
      writeInsts(handle, fetch(m, "struct1_1_7_0"));
      writeInsts(handle, fetch(m, "struct2_1_7_0"));
      writeInsts(handle, fetch(m, "struct3_1_7_0"));
      writeInsts(handle, fetch(m, "struct4_1_7_0"));
      writeInsts(handle, fetch(m, "structP_1_7_0"));
      writeInsts(handle, fetch(m, "structS_1_7_0"));
      writeInsts(handle, fetch(m, "structV_1_7_0"));
    }
  }

  APPLOG(DYN, "%s", "done TESTMOM conversions");
}



static void
doAlkali(Handle handle) {

  APPLOG(DYN, "%s", "begin alkali conversions");

  char *schema = "alkali";

  VersionStruct alkaliFrom;
  VersionStruct alkaliTo;

  const VersionStruct v_1_1_0 = {.version=1, .release=1, .correction=0};
  const VersionStruct v_1_2_0 = {.version=1, .release=2, .correction=0};
  const VersionStruct v_1_2_1 = {.version=1, .release=2, .correction=1};
  const VersionStruct v_1_2_2 = {.version=1, .release=2, .correction=2};

  readSchemaVersions(handle, schema, &alkaliFrom, &alkaliTo);

  GroupListMap m = NULL;

  APPLOG(DYN, "done alkali read schema versions: %u.%u.%u -> %u.%u.%u",
      alkaliFrom.version,
      alkaliFrom.release,
      alkaliFrom.correction,
      alkaliTo.version,
      alkaliTo.release,
      alkaliTo.correction);

  if (verLessEq(&alkaliFrom, &v_1_1_0)) {
    if (verEq(&alkaliFrom, &v_1_1_0)) {
      store(&m, "lithium_1_1_0", readInsts(handle, "ALKALILithium"));
      store(&m, "caesium_1_1_0", readInsts(handle, "ALKALICaesium"));
    }
    store(&m, "lithium_1_2_0", fetch(m, "lithium_1_1_0"));
    store(&m, "caesium_1_2_0", fetch(m, "caesium_1_1_0"));
    store(&m, "francium_1_2_0", createSingleEmptyInstGroup("ALKALIFrancium"));
  }

  APPLOG(DYN, "%s", "done 1.1.0 handling");

  if (verLessEq(&alkaliFrom, &v_1_2_0)) {
    if (verEq(&alkaliFrom, &v_1_2_0)) {
      store(&m, "lithium_1_2_0", readInsts(handle, "ALKALILithium"));
      store(&m, "caesium_1_2_0", readInsts(handle, "ALKALICaesium"));
      store(&m, "francium_1_2_0", readInsts(handle, "ALKALIFrancium"));
    }
    store(&m, "lithium_1_2_1", fetch(m, "lithium_1_2_0"));
    store(&m, "caesium_1_2_1", fetch(m, "caesium_1_2_0"));
    store(&m, "francium_1_2_1", fetch(m, "francium_1_2_0"));
  }

  APPLOG(DYN, "%s", "done 1.2.0 handling");

  if (verLessEq(&alkaliFrom, &v_1_2_1)) {
    if (verEq(&alkaliFrom, &v_1_2_1)) {
      store(&m, "lithium_1_2_1", readInsts(handle, "ALKALILithium"));
      store(&m, "caesium_1_2_1", readInsts(handle, "ALKALICaesium"));
      store(&m, "francium_1_2_1", readInsts(handle, "ALKALIFrancium"));
    }
    store(&m, "lithium_1_2_2", fetch(m, "lithium_1_2_1"));
    store(&m, "caesium_1_2_2", fetch(m, "caesium_1_2_1"));
    store(&m, "francium_1_2_2", fetch(m, "francium_1_2_1"));
  }

  APPLOG(DYN, "%s", "done 1.2.1 handling");

  if (verLessEq(&alkaliFrom, &v_1_2_2)) {
    if (verEq(&alkaliFrom, &v_1_2_2)) {
      ;
    }
    else {
      writeInsts(handle, fetch(m, "lithium_1_2_2"));
      writeInsts(handle, fetch(m, "caesium_1_2_2"));
      writeInsts(handle, fetch(m, "francium_1_2_2"));
    }
  }

  APPLOG(DYN, "%s", "done 1.2.2 handling");

  APPLOG(DYN, "%s", "done alkali conversions");
}


static void
doNoble(Handle handle) {

  APPLOG(DYN, "%s", "begin noble conversions");

  VersionStruct nobleFrom;
  VersionStruct nobleTo;

  const VersionStruct v_1_1_0 = {.version=1, .release=1, .correction=0};
  const VersionStruct v_1_1_1 = {.version=1, .release=1, .correction=1};
  const VersionStruct v_2_1_0 = {.version=2, .release=1, .correction=0};
  const VersionStruct v_2_1_1 = {.version=2, .release=1, .correction=1};
  const VersionStruct v_2_2_0 = {.version=2, .release=2, .correction=0};
  const VersionStruct v_2_3_0 = {.version=2, .release=3, .correction=0};
  const VersionStruct v_3_1_0 = {.version=3, .release=1, .correction=0};
  const VersionStruct v_3_2_0 = {.version=3, .release=2, .correction=0};
  const VersionStruct v_3_2_1 = {.version=3, .release=2, .correction=1};
  const VersionStruct v_3_3_0 = {.version=3, .release=3, .correction=0};
  const VersionStruct v_3_3_1 = {.version=3, .release=3, .correction=1};
  const VersionStruct v_3_3_2 = {.version=3, .release=3, .correction=2};
  const VersionStruct v_3_3_3 = {.version=3, .release=3, .correction=3};
  const VersionStruct v_3_4_0 = {.version=3, .release=4, .correction=0};

  readSchemaVersions(handle, "noble", &nobleFrom, &nobleTo);

  GroupListMap m = NULL;

  // 1.1.0 -> 1.1.1 ==============================

  if (verLessEq(&nobleFrom, &v_1_1_0)) {
    if (verEq(&nobleFrom, &v_1_1_0)) {
      store(&m, "helium_1_1_0", readInsts(handle, "NOBLEHelium"));
      store(&m, "neon_1_1_0", readInsts(handle, "NOBLENeon"));
      store(&m, "argon_1_1_0", readInsts(handle, "NOBLEArgon"));
      store(&m, "krypton_1_1_0", readInsts(handle, "NOBLEKrypton"));
      store(&m, "xenon_1_1_0", readInsts(handle, "NOBLEXenon"));
      store(&m, "radon_1_1_1", createSingleEmptyInstGroup("NOBLERadon"));
    }
    store(&m, "helium_1_1_1", fetch(m, "helium_1_1_0"));
    store(&m, "neon_1_1_1", fetch(m, "neon_1_1_0"));
    store(&m, "argon_1_1_1", fetch(m, "argon_1_1_0"));
    store(&m, "krypton_1_1_1", fetch(m, "krypton_1_1_0"));
    store(&m, "xenon_1_1_1", fetch(m, "xenon_1_1_0"));
  }


  // 1.1.1 -> 2.1.0 ==============================

  if (verLessEq(&nobleFrom, &v_1_1_1)) {
    if (verEq(&nobleFrom, &v_1_1_1)) {
      store(&m, "helium_1_1_1", readInsts(handle, "NOBLEHelium"));
      store(&m, "neon_1_1_1", readInsts(handle, "NOBLENeon"));
      store(&m, "argon_1_1_1", readInsts(handle, "NOBLEArgon"));
      store(&m, "krypton_1_1_1", readInsts(handle, "NOBLEKrypton"));
      store(&m, "xenon_1_1_1", readInsts(handle, "NOBLEXenon"));
      store(&m, "radon_1_1_1", readInsts(handle, "NOBLERadon"));
    }
    store(&m, "helium_2_1_0", fetch(m, "helium_1_1_1"));
    store(&m, "neon_2_1_0", fetch(m, "neon_1_1_1"));
    store(&m, "argon_2_1_0", fetch(m, "argon_1_1_1"));
    store(&m, "krypton_2_1_0", fetch(m, "krypton_1_1_1"));
    store(&m, "xenon_2_1_0", fetch(m, "xenon_1_1_1"));
    store(&m, "radon_2_1_0", fetch(m, "radon_1_1_1"));
  }

  // 2.1.0 -> 2.1.1 ==============================

  if (verLessEq(&nobleFrom, &v_2_1_0)) {
    if (verEq(&nobleFrom, &v_2_1_0)) {
      store(&m, "helium_2_1_0", readInsts(handle, "NOBLEHelium"));
      store(&m, "neon_2_1_0", readInsts(handle, "NOBLENeon"));
      store(&m, "argon_2_1_0", readInsts(handle, "NOBLEArgon"));
      store(&m, "krypton_2_1_0", readInsts(handle, "NOBLEKrypton"));
      store(&m, "xenon_2_1_0", readInsts(handle, "NOBLEXenon"));
      store(&m, "radon_2_1_0", readInsts(handle, "NOBLERadon"));
    }
    store(&m, "helium_2_1_1", fetch(m, "helium_2_1_0"));
    store(&m, "neon_2_1_1", fetch(m, "neon_2_1_0"));
    store(&m, "argon_2_1_1", fetch(m, "argon_2_1_0"));
    store(&m, "krypton_2_1_1", fetch(m, "krypton_2_1_0"));
    store(&m, "xenon_2_1_1", fetch(m, "xenon_2_1_0"));
    store(&m, "radon_2_1_1", fetch(m, "radon_2_1_0"));
  }

  // 2.1.1 -> 2.2.0 ==============================

  if (verLessEq(&nobleFrom, &v_2_1_1)) {
    if (verEq(&nobleFrom, &v_2_1_1)) {
      APPLOG(DYN, "%s", "noble, reading, 2.1.1");
      store(&m, "helium_2_1_1", readInsts(handle, "NOBLEHelium"));
      store(&m, "neon_2_1_1", readInsts(handle, "NOBLENeon"));
      store(&m, "argon_2_1_1", readInsts(handle, "NOBLEArgon"));
      store(&m, "krypton_2_1_1", readInsts(handle, "NOBLEKrypton"));
      store(&m, "xenon_2_1_1", readInsts(handle, "NOBLEXenon"));
      store(&m, "radon_2_1_1", readInsts(handle, "NOBLERadon"));
    }
    APPLOG(DYN, "%s", "noble, transforming, 2.1.1 -> 2.2.0");
    store(&m, "helium_2_2_0", fetch(m, "helium_2_1_1"));
    store(&m, "neon_2_2_0", fetch(m, "neon_2_1_1"));
    store(&m, "argon_2_2_0", fetch(m, "argon_2_1_1"));
    store(&m, "krypton_2_2_0", fetch(m, "krypton_2_1_1"));
    store(&m, "xenon_2_2_0", fetch(m, "xenon_2_1_1"));
    store(&m, "radon_2_2_0", fetch(m, "radon_2_1_1"));
  }

  // 2.2.0 -> 2.3.0 ==============================

  if (verLessEq(&nobleFrom, &v_2_2_0)) {
    if (verEq(&nobleFrom, &v_2_2_0)) {
      APPLOG(DYN, "%s", "noble, reading, 2.2.0");
      store(&m, "helium_2_2_0", readInsts(handle, "NOBLEHelium"));
      store(&m, "neon_2_2_0", readInsts(handle, "NOBLENeon"));
      store(&m, "argon_2_2_0", readInsts(handle, "NOBLEArgon"));
      store(&m, "krypton_2_2_0", readInsts(handle, "NOBLEKrypton"));
      store(&m, "xenon_2_2_0", readInsts(handle, "NOBLEXenon"));
      store(&m, "radon_2_2_0", readInsts(handle, "NOBLERadon"));
    }
    APPLOG(DYN, "%s", "noble, transforming, 2.2.0 -> 2.3.0");
    store(&m, "helium_2_3_0", fetch(m, "helium_2_2_0"));
    store(&m, "neon_2_3_0", fetch(m, "neon_2_2_0"));
    store(&m, "argon_2_3_0", fetch(m, "argon_2_2_0"));
    store(&m, "krypton_2_3_0", fetch(m, "krypton_2_2_0"));
    store(&m, "xenon_2_3_0", fetch(m, "xenon_2_2_0"));
    store(&m, "radon_2_3_0", fetch(m, "radon_2_2_0"));
  }

  // 2.3.0 -> 3.1.0 ==============================

  if (verLessEq(&nobleFrom, &v_2_3_0)) {
    if (verEq(&nobleFrom, &v_2_3_0)) {
      APPLOG(DYN, "%s", "noble, reading, 2.3.0");
      store(&m, "helium_2_3_0", readInsts(handle, "NOBLEHelium"));
      store(&m, "neon_2_3_0", readInsts(handle, "NOBLENeon"));
      store(&m, "argon_2_3_0", readInsts(handle, "NOBLEArgon"));
      store(&m, "krypton_2_3_0", readInsts(handle, "NOBLEKrypton"));
      store(&m, "xenon_2_3_0", readInsts(handle, "NOBLEXenon"));
      store(&m, "radon_2_3_0", readInsts(handle, "NOBLERadon"));
    }
    APPLOG(DYN, "%s", "noble, transforming, 2.3.0 -> 3.1.0");
    store(&m, "helium_3_1_0", fetch(m, "helium_2_3_0"));
    store(&m, "neon_3_1_0", fetch(m, "neon_2_3_0"));
    store(&m, "argon_3_1_0", fetch(m, "argon_2_3_0"));
    store(&m, "krypton_3_1_0", fetch(m, "krypton_2_3_0"));
    store(&m, "xenon_3_1_0", fetch(m, "xenon_2_3_0"));
    store(&m, "radon_3_1_0", fetch(m, "radon_2_3_0"));
    store(&m, "tamm_3_1_0",  createSingleEmptyInstGroup("NOBLETamm"));
    store(&m, "toll_3_1_0",  createSingleEmptyInstGroup("NOBLEToll"));
  }

  // 3.1.0 -> 3.2.0 ==============================

  if (verLessEq(&nobleFrom, &v_3_1_0)) {
    if (verEq(&nobleFrom, &v_3_1_0)) {
      APPLOG(DYN, "%s", "noble, reading, 3.1.0");
      store(&m, "helium_3_1_0", readInsts(handle, "NOBLEHelium"));
      store(&m, "neon_3_1_0", readInsts(handle, "NOBLENeon"));
      store(&m, "argon_3_1_0", readInsts(handle, "NOBLEArgon"));
      store(&m, "krypton_3_1_0", readInsts(handle, "NOBLEKrypton"));
      store(&m, "xenon_3_1_0", readInsts(handle, "NOBLEXenon"));
      store(&m, "radon_3_1_0", readInsts(handle, "NOBLERadon"));
      store(&m, "tamm_3_1_0", readInsts(handle, "NOBLETamm"));
      store(&m, "toll_3_1_0", readInsts(handle, "NOBLEToll"));
    }
    APPLOG(DYN, "%s", "noble, transforming, 3.1.0 -> 3.2.0");
    store(&m, "helium_3_2_0", fetch(m, "helium_3_1_0"));
    store(&m, "neon_3_2_0", fetch(m, "neon_3_1_0"));
    store(&m, "argon_3_2_0", fetch(m, "argon_3_1_0"));
    store(&m, "krypton_3_2_0", fetch(m, "krypton_3_1_0"));
    store(&m, "xenon_3_2_0", fetch(m, "xenon_3_1_0"));
    store(&m, "radon_3_2_0", fetch(m, "radon_3_1_0"));
    store(&m, "tamm_3_2_0", fetch(m, "tamm_3_1_0"));
    store(&m, "toll_3_2_0", fetch(m, "toll_3_1_0"));
  }

  // 3.2.0 -> 3.2.1 ==============================

  if (verLessEq(&nobleFrom, &v_3_2_0)) {
    if (verEq(&nobleFrom, &v_3_2_0)) {
      APPLOG(DYN, "%s", "noble, reading, 3.2.0");
      store(&m, "helium_3_2_0", readInsts(handle, "NOBLEHelium"));
      store(&m, "neon_3_2_0", readInsts(handle, "NOBLENeon"));
      store(&m, "argon_3_2_0", readInsts(handle, "NOBLEArgon"));
      store(&m, "krypton_3_2_0", readInsts(handle, "NOBLEKrypton"));
      store(&m, "xenon_3_2_0", readInsts(handle, "NOBLEXenon"));
      store(&m, "radon_3_2_0", readInsts(handle, "NOBLERadon"));
      store(&m, "tamm_3_2_0", readInsts(handle, "NOBLETamm"));
      store(&m, "toll_3_2_0", readInsts(handle, "NOBLEToll"));
    }
    APPLOG(DYN, "%s", "noble, transforming, 3.2.0 -> 3.2.1");
    store(&m, "helium_3_2_1", fetch(m, "helium_3_2_0"));
    store(&m, "neon_3_2_1", fetch(m, "neon_3_2_0"));
    store(&m, "argon_3_2_1", fetch(m, "argon_3_2_0"));
    store(&m, "krypton_3_2_1", fetch(m, "krypton_3_2_0"));
    store(&m, "xenon_3_2_1", fixXenonGroupList(fetch(m, "xenon_3_2_0")));
    store(&m, "radon_3_2_1", fetch(m, "radon_3_2_0"));
    store(&m, "tamm_3_2_1", fetch(m, "tamm_3_2_0"));
    store(&m, "toll_3_2_1", fetch(m, "toll_3_2_0"));
  }

  // 3_2_1 -> 3_3_0 ==============================

  if (verLessEq(&nobleFrom, &v_3_2_1)) {
    if (verEq(&nobleFrom, &v_3_2_1)) {
      APPLOG(DYN, "%s", "noble, reading, 3_2_1");
      store(&m, "helium_3_2_1", readInsts(handle, "NOBLEHelium"));
      store(&m, "neon_3_2_1", readInsts(handle, "NOBLENeon"));
      store(&m, "argon_3_2_1", readInsts(handle, "NOBLEArgon"));
      store(&m, "krypton_3_2_1", readInsts(handle, "NOBLEKrypton"));
      store(&m, "xenon_3_2_1", readInsts(handle, "NOBLEXenon"));
      store(&m, "radon_3_2_1", readInsts(handle, "NOBLERadon"));
      store(&m, "tamm_3_2_1", readInsts(handle, "NOBLETamm"));
      store(&m, "toll_3_2_1", readInsts(handle, "NOBLEToll"));
    }

    // Refer to the immediately preceding AND the To-version below
    APPLOG(DYN, "%s", "noble, transforming, 3_2_1 -> 3_3_0");
    store(&m, "helium_3_3_0", fetch(m, "helium_3_2_1"));
    store(&m, "neon_3_3_0", fetch(m, "neon_3_2_1"));
    store(&m, "argon_3_3_0", fetch(m, "argon_3_2_1"));
    store(&m, "krypton_3_3_0", fetch(m, "krypton_3_2_1"));
    store(&m, "xenon_3_3_0", fetch(m, "xenon_3_2_1"));
    store(&m, "radon_3_3_0", fetch(m, "radon_3_2_1"));
    store(&m, "tamm_3_3_0", fetch(m, "tamm_3_2_1"));
    store(&m, "toll_3_3_0", fetch(m, "toll_3_2_1"));
    store(&m, "tessin_3_3_0", createSingleEmptyInstGroup("NOBLETessin"));
  }

  // 3_3_0 -> 3_3_1 ==============================

  if (verLessEq(&nobleFrom, &v_3_3_0)) {
    if (verEq(&nobleFrom, &v_3_3_0)) {
      APPLOG(DYN, "%s", "noble, reading, 3_3_0");
      store(&m, "helium_3_3_0", readInsts(handle, "NOBLEHelium"));
      store(&m, "neon_3_3_0", readInsts(handle, "NOBLENeon"));
      store(&m, "argon_3_3_0", readInsts(handle, "NOBLEArgon"));
      store(&m, "krypton_3_3_0", readInsts(handle, "NOBLEKrypton"));
      store(&m, "xenon_3_3_0", readInsts(handle, "NOBLEXenon"));
      store(&m, "radon_3_3_0", readInsts(handle, "NOBLERadon"));
      store(&m, "tamm_3_3_0", readInsts(handle, "NOBLETamm"));
      store(&m, "toll_3_3_0", readInsts(handle, "NOBLEToll"));
      store(&m, "tessin_3_3_0", readInsts(handle, "NOBLETessin"));
    }

    // Refer to the immediately preceding AND the To-version below
    APPLOG(DYN, "%s", "noble, transforming, 3_3_0 -> 3_3_1");
    store(&m, "helium_3_3_1", fetch(m, "helium_3_3_0"));
    store(&m, "neon_3_3_1", fetch(m, "neon_3_3_0"));
    store(&m, "argon_3_3_1", fetch(m, "argon_3_3_0"));
    store(&m, "krypton_3_3_1", fetch(m, "krypton_3_3_0"));
    store(&m, "xenon_3_3_1", fetch(m, "xenon_3_3_0"));
    store(&m, "radon_3_3_1", fetch(m, "radon_3_3_0"));
    store(&m, "tamm_3_3_1", fetch(m, "tamm_3_3_0"));
    store(&m, "toll_3_3_1", fetch(m, "toll_3_3_0"));
    store(&m, "tessin_3_3_1", fetch(m, "tessin_3_3_0"));
  }

  // 3_3_1 -> 3_3_2 ==============================

  if (verLessEq(&nobleFrom, &v_3_3_1)) {
    if (verEq(&nobleFrom, &v_3_3_1)) {
      APPLOG(DYN, "%s", "noble, reading, 3_3_1");
      store(&m, "helium_3_3_1", readInsts(handle, "NOBLEHelium"));
      store(&m, "neon_3_3_1", readInsts(handle, "NOBLENeon"));
      store(&m, "argon_3_3_1", readInsts(handle, "NOBLEArgon"));
      store(&m, "krypton_3_3_1", readInsts(handle, "NOBLEKrypton"));
      store(&m, "xenon_3_3_1", readInsts(handle, "NOBLEXenon"));
      store(&m, "radon_3_3_1", readInsts(handle, "NOBLERadon"));
      store(&m, "tamm_3_3_1", readInsts(handle, "NOBLETamm"));
      store(&m, "toll_3_3_1", readInsts(handle, "NOBLEToll"));
      store(&m, "tessin_3_3_1", readInsts(handle, "NOBLETessin"));
    }

    // Refer to the immediately preceding AND the To-version below
    APPLOG(DYN, "%s", "noble, transforming, 3_3_1 -> 3_3_2");
    store(&m, "helium_3_3_2", fetch(m, "helium_3_3_1"));
    store(&m, "neon_3_3_2", fetch(m, "neon_3_3_1"));
    store(&m, "argon_3_3_2", fetch(m, "argon_3_3_1"));
    store(&m, "krypton_3_3_2", fetch(m, "krypton_3_3_1"));
    store(&m, "xenon_3_3_2", fetch(m, "xenon_3_3_1"));
    store(&m, "radon_3_3_2", fetch(m, "radon_3_3_1"));
    store(&m, "tamm_3_3_2", fetch(m, "tamm_3_3_1"));
    store(&m, "toll_3_3_2", fetch(m, "toll_3_3_1"));
    store(&m, "tessin_3_3_2", fetch(m, "tessin_3_3_1"));
  }

  // 3_3_2 -> 3_3_3 ==============================

  if (verLessEq(&nobleFrom, &v_3_3_2)) {
    if (verEq(&nobleFrom, &v_3_3_2)) {
      APPLOG(DYN, "%s", "noble, reading, 3_3_2");
      store(&m, "helium_3_3_2", readInsts(handle, "NOBLEHelium"));
      store(&m, "neon_3_3_2", readInsts(handle, "NOBLENeon"));
      store(&m, "argon_3_3_2", readInsts(handle, "NOBLEArgon"));
      store(&m, "krypton_3_3_2", readInsts(handle, "NOBLEKrypton"));
      store(&m, "xenon_3_3_2", readInsts(handle, "NOBLEXenon"));
      store(&m, "radon_3_3_2", readInsts(handle, "NOBLERadon"));
      store(&m, "tamm_3_3_2", readInsts(handle, "NOBLETamm"));
      store(&m, "toll_3_3_2", readInsts(handle, "NOBLEToll"));
      store(&m, "tessin_3_3_2", readInsts(handle, "NOBLETessin"));
    }

    // Refer to the immediately preceding AND the To-version below
    APPLOG(DYN, "%s", "noble, transforming, 3_3_2 -> 3_3_3");
    store(&m, "helium_3_3_3", fetch(m, "helium_3_3_2"));
    store(&m, "neon_3_3_3", fetch(m, "neon_3_3_2"));
    store(&m, "argon_3_3_3", fetch(m, "argon_3_3_2"));
    store(&m, "krypton_3_3_3", fetch(m, "krypton_3_3_2"));
    store(&m, "xenon_3_3_3", fetch(m, "xenon_3_3_2"));
    store(&m, "radon_3_3_3", fetch(m, "radon_3_3_2"));
    store(&m, "tamm_3_3_3", fetch(m, "tamm_3_3_2"));
    store(&m, "toll_3_3_3", fetch(m, "toll_3_3_2"));
    store(&m, "tessin_3_3_3", fetch(m, "tessin_3_3_2"));
  }

  // 3_3_3 -> 3_4_0, new classes: Fleming, Ribbing ========

  if (verLessEq(&nobleFrom, &v_3_3_3)) {
    if (verEq(&nobleFrom, &v_3_3_3)) {
      APPLOG(DYN, "%s", "noble, reading, 3_3_3");
      store(&m, "helium_3_3_3", readInsts(handle, "NOBLEHelium"));
      store(&m, "neon_3_3_3", readInsts(handle, "NOBLENeon"));
      store(&m, "argon_3_3_3", readInsts(handle, "NOBLEArgon"));
      store(&m, "krypton_3_3_3", readInsts(handle, "NOBLEKrypton"));
      store(&m, "xenon_3_3_3", readInsts(handle, "NOBLEXenon"));
      store(&m, "radon_3_3_3", readInsts(handle, "NOBLERadon"));
      store(&m, "tamm_3_3_3", readInsts(handle, "NOBLETamm"));
      store(&m, "toll_3_3_3", readInsts(handle, "NOBLEToll"));
      store(&m, "tessin_3_3_3", readInsts(handle, "NOBLETessin"));
    }

    // Refer to the immediately preceding AND the To-version below
    APPLOG(DYN, "%s", "noble, transforming, 3_3_3 -> 3_4_0");
    store(&m, "helium_3_4_0", fetch(m, "helium_3_3_3"));
    store(&m, "neon_3_4_0", fetch(m, "neon_3_3_3"));
    store(&m, "argon_3_4_0", fetch(m, "argon_3_3_3"));
    store(&m, "krypton_3_4_0", fetch(m, "krypton_3_3_3"));
    store(&m, "xenon_3_4_0", fetch(m, "xenon_3_3_3"));
    store(&m, "radon_3_4_0", fetch(m, "radon_3_3_3"));
    store(&m, "tamm_3_4_0", fetch(m, "tamm_3_3_3"));
    store(&m, "toll_3_4_0", fetch(m, "toll_3_3_3"));
    store(&m, "tessin_3_4_0", fetch(m, "tessin_3_3_3"));
    store(&m, "fleming_3_4_0", createSingleEmptyInstGroup("NOBLEFleming"));
    store(&m, "ribbing_3_4_0", createSingleEmptyInstGroup("NOBLERibbing"));
  }

  // ==========================================
  // ALWAYS INCLUDE A FINAL SECTION LIKE THIS
  // Refer to the To-version everywhere below
  // Also make sure a new v_X_Y_Z is declared

  if (verLessEq(&nobleFrom, &v_3_4_0)) {
    if (verEq(&nobleFrom, &v_3_4_0)) {
      ;
    }
    else {
      writeInsts(handle, fetch(m, "helium_3_4_0"));
      writeInsts(handle, fetch(m, "neon_3_4_0"));
      writeInsts(handle, fetch(m, "argon_3_4_0"));
      writeInsts(handle, fetch(m, "krypton_3_4_0"));
      writeInsts(handle, fetch(m, "xenon_3_4_0"));
      writeInsts(handle, fetch(m, "radon_3_4_0"));
      writeInsts(handle, fetch(m, "tamm_3_4_0"));
      writeInsts(handle, fetch(m, "toll_3_4_0"));
      writeInsts(handle, fetch(m, "tessin_3_4_0"));
      writeInsts(handle, fetch(m, "fleming_3_4_0"));
      writeInsts(handle, fetch(m, "ribbing_3_4_0"));
    }
  }

  APPLOG(DYN, "%s", "done noble conversions");
}

static void
doTransport(Handle handle) {
  APPLOG(DYN, "%s", "begin transport conversions");
  char *schema = "transport";
  VersionStruct schemaFrom;
  VersionStruct schemaTo;
  SaImmClassNameT classNames[] = {"Router", "InterfaceIPv4", "AddressIPv4",
				  "InterfaceIPv6", "AddressIPv6", NULL};

  readSchemaVersions(handle, schema, &schemaFrom, &schemaTo);

  if (!verEq(&schemaFrom, &schemaTo))
    safcImmCtCopyInstances(handle, classNames);

  APPLOG(DYN, "%s", "done transport conversions");
}

static void
doSP549_1(Handle handle) {
  APPLOG(DYN, "%s", "begin SP549_1 conversions");
  char *schema = "sp549_1";
  VersionStruct schemaFrom;
  VersionStruct schemaTo;
  SaImmClassNameT classNames[] = {"SP549_1Parent1", "SP549_1Child", NULL};

  readSchemaVersions(handle, schema, &schemaFrom, &schemaTo);
  if (!verEq(&schemaFrom, &schemaTo))
    safcImmCtCopyInstances(handle, classNames);

  APPLOG(DYN, "%s", "done SP549_1 conversions");
}

static void
doSP549_2(Handle handle) {
  APPLOG(DYN, "%s", "begin SP549_2 conversions");
  char *schema = "sp549_2";
  VersionStruct schemaFrom;
  VersionStruct schemaTo;
  SaImmClassNameT classNames[] = {"SP549_2Parent2", NULL};

  readSchemaVersions(handle, schema, &schemaFrom, &schemaTo);
  if (!verEq(&schemaFrom, &schemaTo))
    safcImmCtCopyInstances(handle, classNames);

  APPLOG(DYN, "%s", "done SP549_2 conversions");
}

int
main(int argc, char *argv[]) {

  struct timespec res;
  if (clock_gettime(CLOCK_REALTIME, &res) == 0) {
    struct tm lt;
    localtime_r(&(res.tv_sec), &lt);
    asprintf(
        &logFileName,
        "noble_icti_dyn_%4d-%02d-%02dT%02d_%02d_%02d",
        lt.tm_year + 1900,
        lt.tm_mon + 1,
        lt.tm_mday,
        lt.tm_hour,
        lt.tm_min,
        lt.tm_sec);
  }
  else {
    logFileName = "noble_icti_dyn";
  }

  APPLOG(DYN,
      "--- %s starting, version: %s",
      DYN,
      "%CCaseRev:	/main/R2A/R3A/R4A/R5A/R6A/R8A/R11A/3 %");

  Handle handle = initialize(DYN);
  doBug(handle);

  char *filename;
  asprintf(&filename, "%s/%s", getenv("LOG_DIR"), "TESTMOM_instances.txt");
  APPLOG(DYN, "displaying selected TESTMOM instances to: %s", filename);
  FILE *stream = fopen(filename, "w");
  free(filename);
  doTestmom(handle, false, stream);
  /* doTestmom(handle, true, stream); */
  fclose(stream);

  doReadSysM(handle);
  doAlkali(handle);
  doNoble(handle);
  doTransport(handle);
  doSP549_1(handle);
  doSP549_2(handle);
  finalize(DYN, handle);

  if (argc < 2) {
    APPLOG(DYN, "too few arguments, argc: %d", argc);
    chattyIdle();
  }
  else {
    int pid = atoi(argv[1]);
    int r = kill(pid, SIGUSR2);
    if (r != 0) {
      APPLOG(DYN, "failed to send USR2, code: %d", r);
      chattyIdle();
    }
    else {
      APPLOG(DYN, "%s", "successfully sent USR2");
    }
    chattyIdle();
  }
}
