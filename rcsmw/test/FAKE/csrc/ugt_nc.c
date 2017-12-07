/* ----------------------------------------------------------------------
 * %CCaseFile:	ugt_nc.c %
 * %CCaseRev:	/main/R3A/R6A/1 %
 * %CCaseDate:	2016-07-11 %
 * %CCaseDocNo: %
 * Author:      etxpeno
 * Author: <name>, <e-mail address>
 *
 * Short description: "Upgrade engine" that uses the ICTI interface
 * to upgrade the IMM classes "Equipment", "FieldReplaceableUnit" and
 * "MpClusterHandling"
 *
 * ----------------------------------------------------------------------
 * %CCaseTemplateFile:  template.c %
 * %CCaseTemplateId: CCver: /main/1 %
 *
 * %CCaseCopyrightBegin%
 * Copyright (c) Ericsson AB 2014-2016 All rights reserved.
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
 * R3A/1      2014-11-10 etxpeno     First version
 * R6A/1      2016-07-11 etxpeno     Coverity fixes
 * ----------------------------------------------------------------------
 */

#include "ugt_common.h"

static void do_icti();
static SaAisErrorT do_Equipment(const SafcImmCtHandleT *handle);
static SaAisErrorT do_FieldReplaceableUnit(const SafcImmCtHandleT *handle);
static SaAisErrorT do_MpClusterHandling(const SafcImmCtHandleT *handle);
static SaImmAttrValuesT_2 **oneStringAttr(SaImmAttrNameT name, SaStringT value);

int
main(void)
{
  char *restartType;

  restartType = getenv("RESTART_TYPE");

  if (restartType != NULL && strcmp(restartType, "UPGRADE") == 0)
    do_icti();

  return 0;
}

static void
do_icti()
{
  SaAisErrorT result;
  SafcImmCtHandleT handle;
  SaImmClassNameT classes[4] = {"Equipment",
				"FieldReplaceableUnit",
				"MpClusterHandling",
				NULL};

  result = safcImmCtInitialize(&handle);
  if (result != SA_AIS_OK)
    return;
  result = do_Equipment(&handle);
  if (result != SA_AIS_OK)
    goto ERROR;
  result = do_FieldReplaceableUnit(&handle);
  if (result != SA_AIS_OK)
    goto ERROR;
  result = do_MpClusterHandling(&handle);
  if (result != SA_AIS_OK)
    goto ERROR;
  result = safcImmCtWaitForClasses(handle, classes);
 ERROR:
  safcImmCtFinalize(handle);
}

static SaAisErrorT
do_Equipment(const SafcImmCtHandleT *handle)
{
  SaAisErrorT result = SA_AIS_OK;
  SafcImmCtSchemaVersionT_2 oldSchemaVersion;
  SafcImmCtSchemaVersionT_2 newSchemaVersion;

  result = safcImmCtReadSchemaVersion_2(*handle, "ReqEquipment",
					&oldSchemaVersion,
					&newSchemaVersion);
  if (result != SA_AIS_OK)
    return result;

  if (oldSchemaVersion.version == newSchemaVersion.version &&
      oldSchemaVersion.release == newSchemaVersion.release &&
      oldSchemaVersion.correction == newSchemaVersion.correction) {
    return result;
  }

  if (oldSchemaVersion.version == 0 &&
      oldSchemaVersion.release == 0 &&
      newSchemaVersion.version == 1 &&
      newSchemaVersion.release == 0) {
    SaImmAttrValuesT_2 **attrValues;
    char *rdnName;
    char *rdnValue;
    SaNameT *parentName;
    SafcImmCtInstanceT *instance;
    SaImmClassNameT class;
    SafcImmCtInstanceT **instances;
    SafcImmCtInstanceGroupT *instanceGroup;
    SafcImmCtInstanceGroupT **instanceGroups;

    attrValues = oneStringAttr("userLabel", "Equip_1");
    rdnName = "equipmentId";
    rdnValue = "1";
    parentName = NULL;
    instance = inst(rdnName, rdnValue, parentName, attrValues);

    instances = oneInstance(instance);

    class = "Equipment";
    instanceGroup = instGroup(class, instances);

    instanceGroups = oneInstGroup(instanceGroup);

    result = safcImmCtWriteInstances(*handle, instanceGroups);

    free(instanceGroups);
    free(attrValues);
  }

  return result;
}

static SaAisErrorT
do_FieldReplaceableUnit(const SafcImmCtHandleT *handle)
{
  SaAisErrorT result = SA_AIS_OK;
  SafcImmCtSchemaVersionT_2 oldSchemaVersion;
  SafcImmCtSchemaVersionT_2 newSchemaVersion;

  result = safcImmCtReadSchemaVersion_2(*handle, "ReqFieldReplaceableUnit",
					&oldSchemaVersion,
					&newSchemaVersion);
  if (result != SA_AIS_OK)
    return result;

  if (oldSchemaVersion.version == newSchemaVersion.version &&
      oldSchemaVersion.release == newSchemaVersion.release &&
      oldSchemaVersion.correction == newSchemaVersion.correction) {
    return result;
  }

  if (oldSchemaVersion.version == 0 &&
      oldSchemaVersion.release == 0 &&
      newSchemaVersion.version == 0 &&
      newSchemaVersion.release == 3) {
    result = safcImmCtWriteInstances(*handle, zeroGroups);
  }

  return result;
}

static SaAisErrorT
do_MpClusterHandling(const SafcImmCtHandleT *handle)
{
  SaAisErrorT result = SA_AIS_OK;
  SafcImmCtSchemaVersionT_2 oldSchemaVersion;
  SafcImmCtSchemaVersionT_2 newSchemaVersion;

  result = safcImmCtReadSchemaVersion_2(*handle, "ReqEquipment",
					&oldSchemaVersion,
					&newSchemaVersion);
  if (result != SA_AIS_OK)
    return result;

  if (oldSchemaVersion.version == newSchemaVersion.version &&
      oldSchemaVersion.release == newSchemaVersion.release &&
      oldSchemaVersion.correction == newSchemaVersion.correction) {
    return result;
  }

  if (oldSchemaVersion.version == 0 &&
      oldSchemaVersion.release == 0 &&
      newSchemaVersion.version == 1 &&
      newSchemaVersion.release == 2) {
    char *rdnName;
    char *rdnValue;
    SaNameT *parentName;
    SafcImmCtInstanceT *instance;
    SaImmClassNameT class;
    SafcImmCtInstanceT **instances;
    SafcImmCtInstanceGroupT *instanceGroup;
    SafcImmCtInstanceGroupT **instanceGroups;

    rdnName = "mpClusterHandlingId";
    rdnValue = "1";
    parentName = wrapName("nodeSupportId=1");
    instance = inst(rdnName, rdnValue, parentName, zeroAttrs);

    instances = oneInstance(instance);

    class = "MpClusterHandling";
    instanceGroup = instGroup(class, instances);

    instanceGroups = oneInstGroup(instanceGroup);

    result = safcImmCtWriteInstances(*handle, instanceGroups);

    free(instanceGroups);
  }

  return result;
}

static SaImmAttrValuesT_2 **
oneStringAttr(SaImmAttrNameT name, SaStringT value) {
  SaImmAttrValuesT_2 **result = malloc(2*sizeof(SaImmAttrValuesT_2 *));
  result[0] = stringAttr(name, value);
  result[1] = NULL;
  return result;
}
