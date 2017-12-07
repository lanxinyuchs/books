/* ----------------------------------------------------------------------
 * %CCaseFile:	upgrade_demo.c %
 * %CCaseRev:	/main/R2A/R3A/1 %
 * %CCaseDate:	2014-10-22 %
 * %CCaseDocNo: %
 * Author:	erarafo
 * Author: <name>, <e-mail address>
 *
 * Short description: A small self-contained upgrade engine, for inclusion
 * in the ICTI IWD.
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
 * R2A/3      2013-10-25 erarafo     Exercising 3-level versioning
 * R2A/5      2014-03-19 erarafo     Reverted to R2A/3
 * R3A/1      2014-10-22 erarafo     Added waiting for classes
 * ----------------------------------------------------------------------
 */

#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif

#include <stdbool.h>
#include <sys/timeb.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "safcImmCt.h"

static void
idle(FILE *log) {
  fclose(log);
  while (true) {
    sleep(60);
  }
}

static void
finalize(SafcImmCtHandleT handle, FILE *log) {
  SaAisErrorT r = safcImmCtFinalize(handle);
  if (r != SA_AIS_OK) {
    fprintf(log, "failed to close ICTI session\n");
  }
  else {
    fprintf(log, "closed ICTI session\n");
  }
  idle(log);
}

int
main(void) {
  // Open a log file with a timestamp embedded in the filename.
  char *logFileName;
  struct timeb timeBuffer;
  ftime(&timeBuffer);
  asprintf(&logFileName,
      "%s/upgrade_demo_%ld_%d.txt",
      getenv("LOG_DIR"),
      timeBuffer.time,
      timeBuffer.millitm);
  FILE *log = fopen(logFileName, "w");
  free(logFileName);
  if (log == NULL) {
    // not supposed to happen
    exit(91);
  }

  // Hold here if not upgrade
  if (strcmp(getenv("RESTART_TYPE"), "UPGRADE") != 0) {
    fprintf(log, "context is: not upgrade\n");
    idle(log);
  }

  // Open an ICTI session
  SafcImmCtHandleT handle;
  SaAisErrorT r1 = safcImmCtInitialize(&handle);
  if (r1 != SA_AIS_OK) {
    fprintf(log, "failed to open ICTI session, code: %d\n", r1);
    idle(log);
  }
  else {
    fprintf(log, "ICTI session opened\n");
  }

  SafcImmCtSchemaVersionT_2 old;
  SafcImmCtSchemaVersionT_2 new;
  SaAisErrorT r2 = safcImmCtReadSchemaVersion_2(handle, "demo", &old, &new);
  if (r2 != SA_AIS_OK) {
    fprintf(log, "failed to read schema version, code: %d\n", r2);
    idle(log);
  }
  else {
    fprintf(log, "old schema version: %u, release: %u, correction: %u\n",
        old.version, old.release,old.correction);
    fprintf(log, "new schema version: %u, release: %u, correction: %u\n",
        new.version, new.release, new.correction);
  }

  if (old.version == new.version &&
      old.release == new.release &&
      old.correction == new.correction) {
    // version unchanged, all MO instances have been
    // restored automatically.

    // wait for our classes
    SaImmClassNameT classNames21[] = {"DEMORoot", "DEMOLeaf", NULL};
    SaAisErrorT r21 = safcImmCtWaitForClasses(handle, classNames21);
    if (r21 != SA_AIS_OK) {
      fprintf(log, "failed to wait for classes, code: %d\n", r21);
    }
    else {
      fprintf(log, "request made to wait for classes\n");
    }

    finalize(handle, log);
  }
  else {
    fprintf(log, "handle all MO instances");
  }

  // Copy the DEMORoot instances (there is just one).
  SaImmClassNameT classNames3[] = {"DEMORoot", NULL};
  SaAisErrorT r3 = safcImmCtCopyInstances(handle, classNames3);
  if (r3 != SA_AIS_OK) {
    fprintf(log, "failed to copy the DEMORoot instances, code: %d\n", r3);
    idle(log);
  }
  else {
    fprintf(log, "copied DEMORoot instances\n");
  }

  // Read the DEMOLeaf instances
  SaImmClassNameT classNames4[] = {"DEMOLeaf", NULL};
  SafcImmCtInstanceGroupT **instanceGroups;
  SaAisErrorT r4 = safcImmCtReadInstances(handle, classNames4, &instanceGroups);
  if (r4 != SA_AIS_OK) {
    fprintf(log, "failed to read the DEMOLeaf instances, code: %d\n", r4);
    idle(log);
  }
  else {
    fprintf(log, "read DEMOLeaf instances\n");
  }

  fprintf(log, "class name of read instances: %s\n", instanceGroups[0]->className);

  SafcImmCtInstanceT **instances = instanceGroups[0]->instances;

  // Inspect the first instance
  SaNameT *parentName = instances[0]->parentName;
  char parentNameString[(parentName->length) + 1];
  for (unsigned int k = 0; k < parentName->length; k++) {
    parentNameString[k] = (char)parentName->value[k];
  }
  parentNameString[parentName->length] = '\0';
  fprintf(log, "parent name of the first instance: %s\n", parentNameString);

  SaImmAttrValuesT_2 **attributes = instances[0]->attrValues;

  // Search for and inspect the first attribute of type SA_IMM_ATTR_SAINT32T
  for (unsigned int m = 0; attributes[m] != NULL; m++) {
    if (attributes[m]->attrValueType == SA_IMM_ATTR_SAINT32T) {
      fprintf(log, "attribute: %s, type: %d, multiplicity: %d\n",
          attributes[m]->attrName,
          attributes[m]->attrValueType,
          attributes[m]->attrValuesNumber);
      SaImmAttrValueT *values = attributes[m]->attrValues;
      fprintf(log, "attribute value: %d\n", *((SaInt32T *)values[0]));
      break;
    }
  }

  // Create a modified copy of the DEMOLeaf instance: Add a new attribute
  // in accordance with the To-version schema.

  SaStringT newString = "oval";

  SaImmAttrValueT newValues[] = {(SaImmAttrValueT *)&newString};

  SaImmAttrValuesT_2 newAttr = {
      .attrName = "shape",
      .attrValueType = SA_IMM_ATTR_SASTRINGT,
      .attrValuesNumber = 1,
      .attrValues = newValues
  };

  // Count the attributes in the old instance
  unsigned int nAttributes = 0;
  for (unsigned int k = 0; attributes[k] != NULL; k++) {
    nAttributes++;
  }

  SaImmAttrValuesT_2 *newAttrs[nAttributes+1+1];
  for (unsigned int k = 0; k < nAttributes; k++) {
    newAttrs[k] = attributes[k];
  }
  newAttrs[nAttributes] = &newAttr;
  newAttrs[nAttributes+1] = NULL;

  SafcImmCtInstanceT newInstance = {
      .parentName = parentName,
      .attrValues = newAttrs
  };

  SafcImmCtInstanceT *newInstances[] = {&newInstance, NULL};

  SafcImmCtInstanceGroupT newInstanceGroup = {
      .className="DEMOLeaf",
      .instances=newInstances
  };

  SafcImmCtInstanceGroupT *newInstanceGroups[] = {&newInstanceGroup, NULL};

  SaAisErrorT r5 = safcImmCtWriteInstances(handle, newInstanceGroups);
  if (r5 != SA_AIS_OK) {
    fprintf(log, "failed to write new DEMOLeaf instances, code: %d\n", r5);
    idle(log);
  }
  else {
    fprintf(log, "wrote DEMOLeaf instances\n");
  }

  // At this point it is safe to reclaim memory
  SaAisErrorT r6 = safcImmCtInstanceGroupsMemoryFree(handle, instanceGroups);
  if (r6 != SA_AIS_OK) {
    fprintf(log, "failed to free memory, code: %d\n", r6);
    idle(log);
  }
  else {
    fprintf(log, "freed some memory\n");
  }

  finalize(handle, log);
}

