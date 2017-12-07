/*
 * %EricssonCopyright%
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2012-2017. All Rights Reserved.
 *
 * The program may be used and/or copied only with the written permission from
 * Ericsson AB, or in accordance with the terms and conditions stipulated in
 * the agreement/contract under which the program has been supplied.
 *
 * %CopyrightEnd%
 *
 * ----------------------------------------------------------------------
 *  Purpose : SAF NTF Agent memory handling
 * ----------------------------------------------------------------------
 *
 */

#include <stdlib.h>
#include <string.h>

#include "safc_ntf_mem.h"
#include "safc_ais_common.h"

void
freeSaNtfNotificationHeader(const SaNtfNotificationHeaderT *notificationHeader);

SaAisErrorT
checkSourceIndicator(SaNtfSourceIndicatorT sourceIndicator);

SaAisErrorT
checkValueType(SaNtfValueTypeT valueType);

SaAisErrorT
checkProbableCause(SaNtfProbableCauseT probableCause);

SaAisErrorT
checkSeverity(SaNtfSeverityT severity);

SaAisErrorT
checkSeverityTrend(SaNtfSeverityTrendT trend);

SaAisErrorT
checkEventType(SaNtfEventTypeT eventType);

SaAisErrorT
checkInfoType(SaNtfValueTypeT infoType);

static SaAisErrorT
allocSaNtfNotificationHeader(SaNtfNotificationHeaderT *notificationHeader,
			     SaUint16T numCorrelatedNotifications,
			     SaUint16T lengthAdditionalText,
			     SaUint16T numAdditionalInfo)
{
  if (notificationHeader == NULL)
    return SA_AIS_ERR_INVALID_PARAM;

  notificationHeader->eventType = NULL;
  notificationHeader->notificationObject = NULL;
  notificationHeader->notifyingObject = NULL;
  notificationHeader->notificationClassId = NULL;
  notificationHeader->notificationId = NULL;
  notificationHeader->correlatedNotifications = NULL;
  notificationHeader->additionalText = NULL;
  notificationHeader->additionalInfo = NULL;

  notificationHeader->eventType =
    malloc(sizeof(*notificationHeader->eventType));
  if (notificationHeader->eventType == NULL) {
    freeSaNtfNotificationHeader(notificationHeader);
    return SA_AIS_ERR_NO_MEMORY;
  }

  notificationHeader->notificationObject =
    malloc(sizeof(*notificationHeader->notificationObject));
  if (notificationHeader->notificationObject == NULL) {
    freeSaNtfNotificationHeader(notificationHeader);
    return SA_AIS_ERR_NO_MEMORY;
  }

  notificationHeader->notifyingObject =
    calloc(1,sizeof(*notificationHeader->notifyingObject));
  if (notificationHeader->notifyingObject == NULL) {
    freeSaNtfNotificationHeader(notificationHeader);
    return SA_AIS_ERR_NO_MEMORY;
  }

  notificationHeader->notificationClassId = malloc(sizeof(*notificationHeader->notificationClassId));
  if (notificationHeader->notificationClassId == NULL) {
    freeSaNtfNotificationHeader(notificationHeader);
    return SA_AIS_ERR_NO_MEMORY;
  }

  notificationHeader->eventTime = malloc(sizeof(*notificationHeader->eventTime));
  if (notificationHeader->eventTime == NULL) {
    freeSaNtfNotificationHeader(notificationHeader);
    return SA_AIS_ERR_NO_MEMORY;
  }

  notificationHeader->numCorrelatedNotifications = numCorrelatedNotifications;
  notificationHeader->lengthAdditionalText = lengthAdditionalText;
  notificationHeader->numAdditionalInfo = numAdditionalInfo;

  notificationHeader->notificationId = malloc(sizeof(*notificationHeader->notificationId));
  if (notificationHeader->notificationId == NULL) {
    freeSaNtfNotificationHeader(notificationHeader);
    return SA_AIS_ERR_NO_MEMORY;
  }

  if (numCorrelatedNotifications != 0) {
    notificationHeader->correlatedNotifications =
      malloc(numCorrelatedNotifications * sizeof(SaNtfIdentifierT));
    if (notificationHeader->correlatedNotifications == NULL) {
      freeSaNtfNotificationHeader(notificationHeader);
      return SA_AIS_ERR_NO_MEMORY;
    }
  }

  if (lengthAdditionalText != 0) {
    notificationHeader->additionalText = malloc(lengthAdditionalText);
    if (notificationHeader->additionalText == NULL) {
      freeSaNtfNotificationHeader(notificationHeader);
      return SA_AIS_ERR_NO_MEMORY;
    }
  }

  if (numAdditionalInfo != 0) {
    notificationHeader->additionalInfo = calloc(numAdditionalInfo,
						sizeof(SaNtfAdditionalInfoT));
    if (notificationHeader->additionalInfo == NULL) {
      freeSaNtfNotificationHeader(notificationHeader);
      return SA_AIS_ERR_NO_MEMORY;
    }
  }

  return SA_AIS_OK;
}

void
freeSaNtfNotificationHeader(const SaNtfNotificationHeaderT *notificationHeader)
{
  if (notificationHeader == NULL)
    return;

  free(notificationHeader->eventType);
  free(notificationHeader->notificationObject);
  free(notificationHeader->notifyingObject);
  free(notificationHeader->notificationClassId);
  free(notificationHeader->eventTime);
  free(notificationHeader->notificationId);
  free(notificationHeader->correlatedNotifications);
  free(notificationHeader->additionalText);
  free(notificationHeader->additionalInfo);
}

SaAisErrorT
allocSaNtfObjectCreateDeleteNotification(SaNtfObjectCreateDeleteNotificationT *objectCreateDeleteNotification,
					 SaUint16T numCorrelatedNotifications,
					 SaUint16T lengthAdditionalText,
					 SaUint16T numAdditionalInfo,
					 SaUint16T numAttributes)
{
  SaAisErrorT rc;

  rc = allocSaNtfNotificationHeader(&(objectCreateDeleteNotification->notificationHeader),
				    numCorrelatedNotifications,
				    lengthAdditionalText,
				    numAdditionalInfo);
  if (rc != SA_AIS_OK)
    return rc;

  objectCreateDeleteNotification->sourceIndicator = NULL;
  objectCreateDeleteNotification->objectAttributes = NULL;

  objectCreateDeleteNotification->numAttributes = numAttributes;
  objectCreateDeleteNotification->sourceIndicator =
    malloc(sizeof(*objectCreateDeleteNotification->sourceIndicator));
  if (objectCreateDeleteNotification->sourceIndicator == NULL) {
    freeSaNtfObjectCreateDeleteNotification(objectCreateDeleteNotification);
    return SA_AIS_ERR_NO_MEMORY;
  }

  *(objectCreateDeleteNotification->sourceIndicator) = SA_NTF_UNKNOWN_OPERATION;
  if (numAttributes != 0) {
    objectCreateDeleteNotification->objectAttributes =
      calloc(numAttributes,
	     sizeof(*objectCreateDeleteNotification->objectAttributes));
    if (objectCreateDeleteNotification->objectAttributes == NULL) {
      freeSaNtfObjectCreateDeleteNotification(objectCreateDeleteNotification);
      return SA_AIS_ERR_NO_MEMORY;
    }
  }

  return SA_AIS_OK;
}

void
freeSaNtfObjectCreateDeleteNotification(SaNtfObjectCreateDeleteNotificationT *objectCreateDeleteNotification)
{
  if (objectCreateDeleteNotification == NULL)
    return;

  freeSaNtfNotificationHeader(&objectCreateDeleteNotification->notificationHeader);
  free(objectCreateDeleteNotification->sourceIndicator);
  free(objectCreateDeleteNotification->objectAttributes);

}

SaAisErrorT
allocSaNtfAttributeChangeNotification(SaNtfAttributeChangeNotificationT *attributeChangeNotification,
				      SaUint16T numCorrelatedNotifications,
				      SaUint16T lengthAdditionalText,
				      SaUint16T numAdditionalInfo,
				      SaUint16T numAttributes)
{
  SaAisErrorT rc;

  rc = allocSaNtfNotificationHeader(&(attributeChangeNotification->notificationHeader),
				    numCorrelatedNotifications,
				    lengthAdditionalText,
				    numAdditionalInfo);
  if (rc != SA_AIS_OK)
    return rc;

  attributeChangeNotification->sourceIndicator = NULL;
  attributeChangeNotification->changedAttributes = NULL;

  attributeChangeNotification->numAttributes = numAttributes;
  attributeChangeNotification->sourceIndicator =
    malloc(sizeof(*attributeChangeNotification->sourceIndicator));
  if (attributeChangeNotification->sourceIndicator == NULL) {
    freeSaNtfAttributeChangeNotification(attributeChangeNotification);
    return SA_AIS_ERR_NO_MEMORY;
  }
  *(attributeChangeNotification->sourceIndicator) = SA_NTF_UNKNOWN_OPERATION;

  if (numAttributes != 0) {
    attributeChangeNotification->changedAttributes =
      calloc(numAttributes,
	     sizeof(*attributeChangeNotification->changedAttributes));
    if (attributeChangeNotification->changedAttributes == NULL) {
      freeSaNtfAttributeChangeNotification(attributeChangeNotification);
      return SA_AIS_ERR_NO_MEMORY;
    }
  }

  return SA_AIS_OK;
}

void
freeSaNtfAttributeChangeNotification(SaNtfAttributeChangeNotificationT *attributeChangeNotification)
{
  if (attributeChangeNotification == NULL)
    return;

  freeSaNtfNotificationHeader(&attributeChangeNotification->notificationHeader);
  free(attributeChangeNotification->sourceIndicator);
  free(attributeChangeNotification->changedAttributes);
}

SaAisErrorT
allocSaNtfStateChangeNotification(SaNtfStateChangeNotificationT *stateChangeNotification,
				  SaUint16T numCorrelatedNotifications,
				  SaUint16T lengthAdditionalText,
				  SaUint16T numAdditionalInfo,
				  SaUint16T numStateChanges)
{
  SaAisErrorT rc;

  rc = allocSaNtfNotificationHeader(&(stateChangeNotification->notificationHeader),
				    numCorrelatedNotifications,
				    lengthAdditionalText,
				    numAdditionalInfo);
  if (rc != SA_AIS_OK)
    return rc;

  stateChangeNotification->sourceIndicator = NULL;
  stateChangeNotification->changedStates = NULL;

  stateChangeNotification->numStateChanges = numStateChanges;
  stateChangeNotification->sourceIndicator =
    malloc(sizeof(*stateChangeNotification->sourceIndicator));
  if (stateChangeNotification->sourceIndicator == NULL) {
    freeSaNtfStateChangeNotification(stateChangeNotification);
    return SA_AIS_ERR_NO_MEMORY;
  }

  *(stateChangeNotification->sourceIndicator) = SA_NTF_UNKNOWN_OPERATION;
  if (numStateChanges != 0) {
    stateChangeNotification->changedStates =
      calloc(numStateChanges,
	     sizeof(*stateChangeNotification->changedStates));
    if (stateChangeNotification->changedStates == NULL) {
      freeSaNtfStateChangeNotification(stateChangeNotification);
      return SA_AIS_ERR_NO_MEMORY;
    }
  }

  return SA_AIS_OK;
}

void
freeSaNtfStateChangeNotification(SaNtfStateChangeNotificationT *stateChangeNotification)
{
  if (stateChangeNotification == NULL)
    return;

  freeSaNtfNotificationHeader(&stateChangeNotification->notificationHeader);
  free(stateChangeNotification->sourceIndicator);
  free(stateChangeNotification->changedStates);
}

SaAisErrorT
allocSaNtfAlarmNotification(SaNtfAlarmNotificationT *alarmNotification,
			    SaUint16T numCorrelatedNotifications,
			    SaUint16T lengthAdditionalText,
			    SaUint16T numAdditionalInfo,
			    SaUint16T numSpecificProblems,
			    SaUint16T numMonitoredAttributes,
			    SaUint16T numProposedRepairActions)
{
  SaAisErrorT rc;

  rc = allocSaNtfNotificationHeader(&(alarmNotification->notificationHeader),
				    numCorrelatedNotifications,
				    lengthAdditionalText,
				    numAdditionalInfo);
  if (rc != SA_AIS_OK)
    return rc;

  alarmNotification->numSpecificProblems = numSpecificProblems;
  alarmNotification->numMonitoredAttributes = numMonitoredAttributes;
  alarmNotification->numProposedRepairActions = numProposedRepairActions;

  alarmNotification->probableCause = NULL;
  alarmNotification->specificProblems = NULL;
  alarmNotification->perceivedSeverity = NULL;
  alarmNotification->trend = NULL;
  alarmNotification->thresholdInformation = NULL;
  alarmNotification->monitoredAttributes = NULL;
  alarmNotification->proposedRepairActions = NULL;

  alarmNotification->perceivedSeverity = malloc(sizeof(*alarmNotification->perceivedSeverity));
  if (alarmNotification->perceivedSeverity == NULL) {
    freeSaNtfAlarmNotification(alarmNotification);
    return SA_AIS_ERR_NO_MEMORY;
  }

  alarmNotification->trend = malloc(sizeof(*alarmNotification->trend));
  if (alarmNotification->trend == NULL) {
    freeSaNtfAlarmNotification(alarmNotification);
    return SA_AIS_ERR_NO_MEMORY;
  }

  *(alarmNotification->trend) = SA_NTF_TREND_NO_CHANGE;

  alarmNotification->thresholdInformation =
    calloc(1, sizeof(*alarmNotification->thresholdInformation));
  if (alarmNotification->thresholdInformation == NULL) {
    freeSaNtfAlarmNotification(alarmNotification);
    return SA_AIS_ERR_NO_MEMORY;
  }

  alarmNotification->thresholdInformation->thresholdId = 0xffff;

  alarmNotification->probableCause = malloc(sizeof(*alarmNotification->probableCause));
  if (alarmNotification->probableCause == NULL) {
    freeSaNtfAlarmNotification(alarmNotification);
    return SA_AIS_ERR_NO_MEMORY;
  }

  if (numSpecificProblems != 0) {
    alarmNotification->specificProblems = calloc(numSpecificProblems,
						 sizeof(*alarmNotification->specificProblems));
    if (alarmNotification->specificProblems == NULL) {
      freeSaNtfAlarmNotification(alarmNotification);
      return SA_AIS_ERR_NO_MEMORY;
    }
  }

  if (numMonitoredAttributes != 0) {
    alarmNotification->monitoredAttributes = calloc(numMonitoredAttributes,
						    sizeof(SaNtfAttributeT));
    if (alarmNotification->monitoredAttributes == NULL) {
      freeSaNtfAlarmNotification(alarmNotification);
      return SA_AIS_ERR_NO_MEMORY;
    }
  }

  if (numProposedRepairActions != 0) {
    alarmNotification->proposedRepairActions = calloc(numProposedRepairActions,
						      sizeof(SaNtfProposedRepairActionT));
    if (alarmNotification->proposedRepairActions == NULL) {
      freeSaNtfAlarmNotification(alarmNotification);
      return SA_AIS_ERR_NO_MEMORY;
    }
  }

  return SA_AIS_OK;
}

void
freeSaNtfAlarmNotification(SaNtfAlarmNotificationT *alarmNotification)
{
  if (alarmNotification == NULL)
    return;

  freeSaNtfNotificationHeader(&alarmNotification->notificationHeader);
  free(alarmNotification->probableCause);
  free(alarmNotification->specificProblems);
  free(alarmNotification->perceivedSeverity);
  free(alarmNotification->trend);
  free(alarmNotification->thresholdInformation);
  free(alarmNotification->monitoredAttributes);
  free(alarmNotification->proposedRepairActions);
}

SaAisErrorT
allocSaNtfSecurityAlarmNotification(SaNtfSecurityAlarmNotificationT *securityAlarmNotification,
				    SaUint16T numCorrelatedNotifications,
				    SaUint16T lengthAdditionalText,
				    SaUint16T numAdditionalInfo)
{
  SaAisErrorT rc;

  rc = allocSaNtfNotificationHeader(&(securityAlarmNotification->notificationHeader),
				    numCorrelatedNotifications,
				    lengthAdditionalText,
				    numAdditionalInfo);
  if (rc != SA_AIS_OK)
    return rc;

  securityAlarmNotification->probableCause = NULL;
  securityAlarmNotification->severity = NULL;
  securityAlarmNotification->securityAlarmDetector = NULL;
  securityAlarmNotification->serviceUser = NULL;
  securityAlarmNotification->serviceProvider = NULL;

  securityAlarmNotification->probableCause =
    malloc(sizeof(*securityAlarmNotification->probableCause));
  if (securityAlarmNotification->probableCause == NULL) {
    freeSaNtfSecurityAlarmNotification(securityAlarmNotification);
    return SA_AIS_ERR_NO_MEMORY;
  }

  securityAlarmNotification->severity =
    malloc(sizeof(*securityAlarmNotification->severity));
  if (securityAlarmNotification->severity == NULL) {
    freeSaNtfSecurityAlarmNotification(securityAlarmNotification);
    return SA_AIS_ERR_NO_MEMORY;
  }

  securityAlarmNotification->securityAlarmDetector =
    malloc(sizeof(*securityAlarmNotification->securityAlarmDetector));
  if (securityAlarmNotification->securityAlarmDetector == NULL) {
    freeSaNtfSecurityAlarmNotification(securityAlarmNotification);
    return SA_AIS_ERR_NO_MEMORY;
  }

  securityAlarmNotification->serviceUser =
    malloc(sizeof(*securityAlarmNotification->serviceUser));
  if (securityAlarmNotification->serviceUser == NULL) {
    freeSaNtfSecurityAlarmNotification(securityAlarmNotification);
    return SA_AIS_ERR_NO_MEMORY;
  }

  securityAlarmNotification->serviceProvider =
    malloc(sizeof(*securityAlarmNotification->serviceProvider));
  if (securityAlarmNotification->serviceProvider == NULL) {
    freeSaNtfSecurityAlarmNotification(securityAlarmNotification);
    return SA_AIS_ERR_NO_MEMORY;
  }

  return SA_AIS_OK;
}

void
freeSaNtfSecurityAlarmNotification(SaNtfSecurityAlarmNotificationT *securityAlarmNotification)
{
  if (securityAlarmNotification == NULL)
    return;

  freeSaNtfNotificationHeader(&securityAlarmNotification->notificationHeader);
  free(securityAlarmNotification->probableCause);
  free(securityAlarmNotification->severity);
  free(securityAlarmNotification->securityAlarmDetector);
  free(securityAlarmNotification->serviceUser);
  free(securityAlarmNotification->serviceProvider);
}

SaAisErrorT
allocSafsNtfObjectCreateDeleteNotification(SafsNtfObjectCreateDeleteNotification **pptr,
					   SaNtfObjectCreateDeleteNotificationT *objectCreateDeleteNotification,
					   v_data * variableData)
{
  SaAisErrorT rc;
  int i;
  SafsNtfObjectCreateDeleteNotification *ptr = NULL;
  SaNtfSourceIndicatorT sourceIndicator;

  *pptr = ptr;

  sourceIndicator = *(objectCreateDeleteNotification->sourceIndicator);

  rc = checkSourceIndicator(sourceIndicator);
  if (rc != SA_AIS_OK)
    return rc;

  ptr = malloc(sizeof(*ptr));
  if (ptr == NULL)
    return SA_AIS_ERR_NO_MEMORY;

  safs_ntf_object_create_delete_notification__init(ptr);

  rc = allocSafsNtfNotificationHeader(&ptr->notificationheader,
				      objectCreateDeleteNotification->notificationHeader,
				      variableData);
  if (rc != SA_AIS_OK)
    goto ERROR;

  ptr->sourceindicator = sourceIndicator;

  ptr->n_objectattributes = objectCreateDeleteNotification->numAttributes;
  if (ptr->n_objectattributes != 0) {
    ptr->objectattributes =
      malloc(ptr->n_objectattributes * sizeof(*ptr->objectattributes));
    if (ptr->objectattributes == NULL) {
      rc = SA_AIS_ERR_NO_MEMORY;
      goto ERROR;
    }
    for (i = 0; i < ptr->n_objectattributes; i++) {
      rc = allocSafsNtfAttribute(&ptr->objectattributes[i],
				 objectCreateDeleteNotification->objectAttributes + i,
				 variableData);
      if (rc != SA_AIS_OK)
	goto ERROR;
    }
  }

  *pptr = ptr;
  return SA_AIS_OK;

 ERROR:
  freeSafsNtfObjectCreateDeleteNotification(ptr);

  return rc;
}

void
freeSafsNtfObjectCreateDeleteNotification(SafsNtfObjectCreateDeleteNotification *ptr)
{
  int i;

  if (ptr == NULL)
    return;

  for (i = 0; i < ptr->n_objectattributes; i++)
    freeSafsNtfAttribute(ptr->objectattributes[i]);
  free(ptr->objectattributes);
  freeSafsNtfNotificationHeader(ptr->notificationheader);
  free(ptr);
}

SaAisErrorT
allocSafsNtfAttributeChangeNotification(SafsNtfAttributeChangeNotification **pptr,
					SaNtfAttributeChangeNotificationT *attributeChangeNotification,
					v_data * variableData)
{
  SaAisErrorT rc;
  SafsNtfAttributeChangeNotification *ptr = NULL;
  int i;
  SaNtfSourceIndicatorT sourceIndicator;

  *pptr = ptr;

  sourceIndicator = *(attributeChangeNotification->sourceIndicator);

  rc = checkSourceIndicator(sourceIndicator);
  if (rc != SA_AIS_OK)
    return rc;

  ptr = malloc(sizeof(*ptr));
  if (ptr == NULL)
    return SA_AIS_ERR_NO_MEMORY;

  safs_ntf_attribute_change_notification__init(ptr);

  rc = allocSafsNtfNotificationHeader(&ptr->notificationheader,
				      attributeChangeNotification->notificationHeader,
				      variableData);

  if (rc != SA_AIS_OK)
    goto ERROR;

  ptr->sourceindicator = sourceIndicator;

  ptr->n_changedattributes = attributeChangeNotification->numAttributes;
  if (ptr->n_changedattributes != 0) {
    ptr->changedattributes =
      malloc(ptr->n_changedattributes * sizeof(*ptr->changedattributes));
    if (ptr->changedattributes == NULL) {
      rc = SA_AIS_ERR_NO_MEMORY;
      goto ERROR;
    }

    for (i = 0; i < ptr->n_changedattributes; i++) {
      rc = allocSafsNtfAttributeChange(&ptr->changedattributes[i],
				       attributeChangeNotification->changedAttributes + i,
				       variableData);
      if (rc != SA_AIS_OK)
	goto ERROR;
    }
  }

  *pptr = ptr;
  return SA_AIS_OK;

 ERROR:
  freeSafsNtfAttributeChangeNotification(ptr);

  return rc;
}

void
freeSafsNtfAttributeChangeNotification(SafsNtfAttributeChangeNotification *ptr)
{
  int i;

  if (ptr == NULL)
    return;

  for (i = 0; i < ptr->n_changedattributes; i++)
    freeSafsNtfAttributeChange(ptr->changedattributes[i]);
  free(ptr->changedattributes);
  freeSafsNtfNotificationHeader(ptr->notificationheader);
  free(ptr);
}

SaAisErrorT
allocSafsNtfStateChangeNotification(SafsNtfStateChangeNotification **pptr,
				    SaNtfStateChangeNotificationT *stateChangeNotification,
				    v_data * variableData)
{
  SaAisErrorT rc;
  SafsNtfStateChangeNotification *ptr = NULL;
  int i;
  SaNtfSourceIndicatorT sourceIndicator;

  *pptr = ptr;

  sourceIndicator = *(stateChangeNotification->sourceIndicator);

  rc = checkSourceIndicator(sourceIndicator);
  if (rc != SA_AIS_OK)
    return rc;

  ptr = malloc(sizeof(*ptr));
  if (ptr == NULL)
    return SA_AIS_ERR_NO_MEMORY;

  safs_ntf_state_change_notification__init(ptr);

  rc = allocSafsNtfNotificationHeader(&ptr->notificationheader,
				      stateChangeNotification->notificationHeader,
				      variableData);

  if (rc != SA_AIS_OK)
    goto ERROR;

  ptr->sourceindicator = sourceIndicator;

  ptr->n_changedstates = stateChangeNotification->numStateChanges;
  if (ptr->n_changedstates != 0) {
    ptr->changedstates =
      malloc(ptr->n_changedstates * sizeof(*ptr->changedstates));
    if (ptr->changedstates == NULL) {
      rc = SA_AIS_ERR_NO_MEMORY;
      goto ERROR;
    }

    for (i = 0; i < ptr->n_changedstates; i++) {
      rc = allocSafsNtfStateChange(&ptr->changedstates[i],
				   stateChangeNotification->changedStates + i,
				   variableData);
      if (rc != SA_AIS_OK)
	goto ERROR;
    }
  }

  *pptr = ptr;
  return SA_AIS_OK;

 ERROR:
  freeSafsNtfStateChangeNotification(ptr);

  return rc;
}

void
freeSafsNtfStateChangeNotification(SafsNtfStateChangeNotification *ptr)
{
  int i;

  if (ptr == NULL)
    return;

  for (i = 0; i < ptr->n_changedstates; i++)
    freeSafsNtfStateChange(ptr->changedstates[i]);
  free(ptr->changedstates);
  freeSafsNtfNotificationHeader(ptr->notificationheader);
  free(ptr);
}

SaAisErrorT
allocSafsNtfAlarmNotification(SafsNtfAlarmNotification** pptr,
			      SaNtfAlarmNotificationT *alarmNotification,
			      v_data *variableData)
{
  SaAisErrorT rc;
  SafsNtfAlarmNotification *ptr = NULL;
  int i;
  SaNtfProbableCauseT probableCause;
  SaNtfSeverityT perceivedSeverity;
  SaNtfSeverityTrendT trend;

  *pptr = ptr;

  probableCause = *(alarmNotification->probableCause);
  rc = checkProbableCause(probableCause);
  if (rc != SA_AIS_OK)
    return rc;

  perceivedSeverity = *(alarmNotification->perceivedSeverity);
  rc = checkSeverity(perceivedSeverity);
  if (rc != SA_AIS_OK)
    return rc;

  trend = *(alarmNotification->trend);
  rc = checkSeverityTrend(trend);
  if (rc != SA_AIS_OK)
    return rc;

  ptr = malloc(sizeof(*ptr));
  if (ptr == NULL)
    return SA_AIS_ERR_NO_MEMORY;

  safs_ntf_alarm_notification__init(ptr);

  rc = allocSafsNtfNotificationHeader(&ptr->notificationheader,
				      alarmNotification->notificationHeader,
				      variableData);
  if (rc != SA_AIS_OK)
    goto ERROR;

  ptr->probablecause = probableCause;

  ptr->n_specificproblems = alarmNotification->numSpecificProblems;
  if (ptr->n_specificproblems != 0) {
    ptr->specificproblems =
      malloc(ptr->n_specificproblems * sizeof(*ptr->specificproblems));
    if (ptr->specificproblems == NULL) {
      rc = SA_AIS_ERR_NO_MEMORY;
      goto ERROR;
    }

    for (i = 0; i < ptr->n_specificproblems; i++) {
      rc = allocSafsNtfSpecificProblem(&ptr->specificproblems[i],
				       alarmNotification->specificProblems + i,
				       variableData);
      if (rc != SA_AIS_OK)
	goto ERROR;
    }
  }

  ptr->perceivedseverity = perceivedSeverity;
  ptr->trend = trend;

  rc = allocSafsNtfThresholdInformation(&ptr->thresholdinformation,
					alarmNotification->thresholdInformation,
					variableData);
  if (rc != SA_AIS_OK)
    goto ERROR;

  ptr->n_monitoredattributes = alarmNotification->numMonitoredAttributes;
  if (ptr->n_monitoredattributes != 0) {
    ptr->monitoredattributes =
      malloc(ptr->n_monitoredattributes * sizeof(*ptr->monitoredattributes));
    if (ptr->monitoredattributes == NULL) {
      rc = SA_AIS_ERR_NO_MEMORY;
      goto ERROR;
    }
    for (i = 0; i < ptr->n_monitoredattributes; i++) {
      rc = allocSafsNtfAttribute(&ptr->monitoredattributes[i],
				 alarmNotification->monitoredAttributes + i,
				 variableData);
      if (rc != SA_AIS_OK)
	goto ERROR;
    }
  }

  ptr->n_proposedrepairactions = alarmNotification->numProposedRepairActions;
  if (ptr->n_proposedrepairactions != 0) {
    ptr->proposedrepairactions =
      malloc(ptr->n_proposedrepairactions * sizeof(SafsNtfProposedRepairAction *));
    if (ptr->proposedrepairactions == NULL) {
      rc = SA_AIS_ERR_NO_MEMORY;
      goto ERROR;
    }
    for (i = 0; i < ptr->n_proposedrepairactions; i++) {
      rc = allocSafsNtfProposedRepairAction(&ptr->proposedrepairactions[i],
					    alarmNotification->proposedRepairActions + i,
					    variableData);
      if (rc != SA_AIS_OK)
	goto ERROR;
    }
  }

  *pptr = ptr;
  return SA_AIS_OK;

 ERROR:
  freeSafsNtfAlarmNotification(ptr);

  return rc;
}

void
freeSafsNtfAlarmNotification(SafsNtfAlarmNotification *ptr)
{
  int i;

  if (ptr == NULL)
    return;

  for (i = 0; i < ptr->n_proposedrepairactions; i++)
    freeSafsNtfProposedRepairAction(ptr->proposedrepairactions[i]);
  free(ptr->proposedrepairactions);
  for (i = 0; i < ptr->n_monitoredattributes; i++)
    freeSafsNtfAttribute(ptr->monitoredattributes[i]);
  free(ptr->monitoredattributes);
  freeSafsNtfThresholdInformation(ptr->thresholdinformation);
  for (i = 0; i < ptr->n_specificproblems; i++)
    freeSafsNtfSpecificProblem(ptr->specificproblems[i]);
  free(ptr->specificproblems);
  freeSafsNtfNotificationHeader(ptr->notificationheader);
  free(ptr);
}

SaAisErrorT
allocSafsNtfSecurityAlarmNotification(SafsNtfSecurityAlarmNotification**pptr,
				      SaNtfSecurityAlarmNotificationT *securityAlarmNotification,
				      v_data *variableData)
{
  SaAisErrorT rc;
  SafsNtfSecurityAlarmNotification *ptr = NULL;
  SaNtfProbableCauseT probableCause;
  SaNtfSeverityT severity;

  *pptr = ptr;

  probableCause = *(securityAlarmNotification->probableCause);
  rc = checkProbableCause(probableCause);
  if (rc != SA_AIS_OK)
    return rc;

  severity = *(securityAlarmNotification->severity);
  rc = checkSeverity(severity);
  if (rc != SA_AIS_OK)
    return rc;

  ptr = malloc(sizeof(*ptr));
  if (ptr == NULL)
    return SA_AIS_ERR_NO_MEMORY;

  safs_ntf_security_alarm_notification__init(ptr);

  rc = allocSafsNtfNotificationHeader(&ptr->notificationheader,
				      securityAlarmNotification->notificationHeader,
				      variableData);
  if (rc != SA_AIS_OK)
    goto ERROR;

  ptr->probablecause = probableCause;
  ptr->severity = severity;

  rc = allocSafsNtfSecurityAlarmDetector(&ptr->securityalarmdetector,
					 securityAlarmNotification->securityAlarmDetector,
					 variableData);
  if (rc != SA_AIS_OK)
    goto ERROR;

  rc = allocSafsNtfServiceUser(&ptr->serviceuser,
			       securityAlarmNotification->serviceUser,
			       variableData);
  if (rc != SA_AIS_OK)
    goto ERROR;

  rc = allocSafsNtfServiceUser(&ptr->serviceprovider,
			       securityAlarmNotification->serviceProvider,
			       variableData);
  if (rc != SA_AIS_OK)
    goto ERROR;

  *pptr = ptr;
  return SA_AIS_OK;

 ERROR:
  freeSafsNtfSecurityAlarmNotification(ptr);

  return rc;
}

void
freeSafsNtfSecurityAlarmNotification(SafsNtfSecurityAlarmNotification *ptr)
{
  if (ptr == NULL)
    return;

  freeSafsNtfServiceUser(ptr->serviceprovider);
  freeSafsNtfServiceUser(ptr->serviceuser);
  freeSafsNtfSecurityAlarmDetector(ptr->securityalarmdetector);
  freeSafsNtfNotificationHeader(ptr->notificationheader);
  free(ptr);
}

SaAisErrorT
allocSafsNtfNotificationHeader(SafsNtfNotificationHeader** pptr,
			       SaNtfNotificationHeaderT notificationHeader,
			       v_data * variableData)
{
  SaAisErrorT rc;
  SafsNtfNotificationHeader *ptr = NULL;
  int i;
  SaNtfEventTypeT eventType;

  *pptr = ptr;

  eventType = *(notificationHeader.eventType);
  rc = checkEventType(eventType);
  if (rc != SA_AIS_OK)
    return rc;

  ptr = malloc(sizeof(*ptr));
  if (ptr == NULL)
    return SA_AIS_ERR_NO_MEMORY;

  safs_ntf_notification_header__init(ptr);

  ptr->eventtype = eventType;

  rc = allocBufferFromSaNameT(&ptr->notificationobject,
			      notificationHeader.notificationObject);
  if (rc != SA_AIS_OK)
    goto ERROR;

  if (notificationHeader.notifyingObject->length == 0)
    rc = allocBufferFromSaNameT(&ptr->notifyingobject,
				notificationHeader.notificationObject);
  else
    rc = allocBufferFromSaNameT(&ptr->notifyingobject,
				notificationHeader.notifyingObject);
  if (rc != SA_AIS_OK)
    goto ERROR;

  rc = allocSafsNtfClassid(&ptr->notificationclassid,
			   notificationHeader.notificationClassId);
  if (rc != SA_AIS_OK)
    goto ERROR;

  if (*(notificationHeader.eventTime) == SA_TIME_UNKNOWN)
    ptr->eventtime = safc_current_time();
  else
    ptr->eventtime = *(notificationHeader.eventTime);

  ptr->n_correlatednotifications = notificationHeader.numCorrelatedNotifications;

  if (ptr->n_correlatednotifications != 0) {
    ptr->correlatednotifications =
      malloc(ptr->n_correlatednotifications * sizeof(uint64_t));
    if (ptr->correlatednotifications == NULL) {
      rc = SA_AIS_ERR_NO_MEMORY;
      goto ERROR;
    }
    memcpy(ptr->correlatednotifications,
	   notificationHeader.correlatedNotifications,
	   notificationHeader.numCorrelatedNotifications * sizeof(uint64_t));
  }

  rc = allocBufferFromSaStringT(&ptr->additionaltext,
				notificationHeader.additionalText,
				notificationHeader.lengthAdditionalText);

  if (rc != SA_AIS_OK)
    goto ERROR;

  ptr->n_additionalinfo = notificationHeader.numAdditionalInfo;
  if (ptr->n_additionalinfo != 0) {
    ptr->additionalinfo =
      malloc(ptr->n_additionalinfo * sizeof(SafsNtfAdditionalInfo*));
    if (ptr->additionalinfo == NULL) {
      rc = SA_AIS_ERR_NO_MEMORY;
      goto ERROR;
    }

    for (i = 0; i < ptr->n_additionalinfo; i++) {
      rc  = allocSafsNtfAdditionalInfo(&ptr->additionalinfo[i],
				       notificationHeader.additionalInfo + i,
				       variableData);
      if (rc != SA_AIS_OK)
	goto ERROR;
    }
  }

  *pptr = ptr;
  return SA_AIS_OK;

 ERROR:
  freeSafsNtfNotificationHeader(ptr);

  return rc;
}

void
freeSafsNtfNotificationHeader(SafsNtfNotificationHeader *ptr)
{
  int i;

  if (ptr == NULL)
    return;

  free(ptr->notificationobject);
  free(ptr->notifyingobject);
  freeSafsNtfClassid(ptr->notificationclassid);
  free(ptr->correlatednotifications);
  free(ptr->additionaltext);
  for (i = 0; i < ptr->n_additionalinfo; i++) {
    freeSafsNtfAdditionalInfo(ptr->additionalinfo[i]);
  }
  free(ptr->additionalinfo);
  free(ptr);
}

SaAisErrorT
allocSafsNtfClassid(SafsNtfClassId**pptr,
		    SaNtfClassIdT* classId)
{
  SafsNtfClassId *ptr = NULL;

  *pptr = ptr;

  ptr = malloc(sizeof(*ptr));
  if (ptr == NULL)
    return SA_AIS_ERR_NO_MEMORY;

  safs_ntf_class_id__init(ptr);

  ptr->vendorid = classId->vendorId;
  ptr->majorid  = classId->majorId;
  ptr->minorid  = classId->minorId;

  *pptr = ptr;
  return SA_AIS_OK;
}

void
freeSafsNtfClassid(SafsNtfClassId *ptr)
{
  free(ptr);
}

SaAisErrorT
allocSafsNtfAdditionalInfo(SafsNtfAdditionalInfo** pptr,
			   SaNtfAdditionalInfoT* additionalInfo,
			   v_data *variableData)
{
  SaAisErrorT rc;
  SafsNtfAdditionalInfo *ptr = NULL;
  SaNtfValueTypeT infoType = additionalInfo->infoType;

  *pptr = ptr;

  rc = checkInfoType(infoType);
  if (rc != SA_AIS_OK)
    return rc;

  ptr = malloc(sizeof(*ptr));
  if (ptr == NULL)
    return SA_AIS_ERR_NO_MEMORY;

  safs_ntf_additional_info__init(ptr);

  ptr->infoid = additionalInfo->infoId;
  ptr->infotype = infoType;
  rc = allocSafsNtfValue(&ptr->infovalue,
			 infoType,
			 additionalInfo->infoValue,
			 variableData);

  if (rc != SA_AIS_OK)
    goto ERROR;

  *pptr = ptr;
  return SA_AIS_OK;

 ERROR:
  freeSafsNtfAdditionalInfo(ptr);

  return rc;
}

void
freeSafsNtfAdditionalInfo(SafsNtfAdditionalInfo *ptr)
{
  if (ptr == NULL)
    return;

  freeSafsNtfValue(ptr->infovalue);
  free(ptr);
}

SaAisErrorT
allocSafsNtfValue(SafsNtfValue **pptr,
		  SaNtfValueTypeT infoType,
		  SaNtfValueT infoValue,
		  v_data *variableData)
{
  SaAisErrorT rc;
  SafsNtfValue *ptr = NULL;

  *pptr = ptr;

  ptr = calloc(1, sizeof(*ptr));
  if (ptr == NULL)
    return SA_AIS_ERR_NO_MEMORY;

  safs_ntf_value__init(ptr);

  switch (infoType) {
  case SA_NTF_VALUE_UINT8:
    ptr->has_uint8val = 1;
    ptr->uint8val = infoValue.uint8Val;
    break;
  case SA_NTF_VALUE_INT8:
    ptr->has_int8val = 1;
    ptr->int8val = infoValue.int8Val;
    break;
  case SA_NTF_VALUE_UINT16:
    ptr->has_uint16val = 1;
    ptr->uint16val = infoValue.uint16Val;
    break;
  case SA_NTF_VALUE_INT16:
    ptr->has_int16val = 1;
    ptr->int16val = infoValue.int16Val;
    break;
  case SA_NTF_VALUE_UINT32:
    ptr->has_uint32val = 1;
    ptr->uint32val = infoValue.uint32Val;
    break;
  case SA_NTF_VALUE_INT32:
    ptr->has_int32val = 1;
    ptr->int32val = infoValue.int32Val;
    break;
  case SA_NTF_VALUE_UINT64:
    ptr->has_uint64val = 1;
    ptr->uint64val = infoValue.uint64Val;
    break;
  case SA_NTF_VALUE_INT64:
    ptr->has_int64val = 1;
    ptr->int64val = infoValue.int64Val;
    break;
  case SA_NTF_VALUE_FLOAT:
    ptr->has_floatval = 1;
    ptr->floatval = infoValue.floatVal;
    break;
  case SA_NTF_VALUE_DOUBLE:
    ptr->has_doubleval = 1;
    ptr->doubleval = infoValue.doubleVal;
    break;
  case SA_NTF_VALUE_LDAP_NAME:
    {
      SaUint16T dataSize = 0;
      void *dataPtr = NULL;
      void *data;
      SaNameT *namePtr;

      get_ntf_ptr_val(variableData, &infoValue, &dataSize, &dataPtr);
      namePtr = dataPtr;

      data = malloc(namePtr->length);
      if (data == NULL) {
	rc = SA_AIS_ERR_NO_MEMORY;
	goto ERROR;
      }

      memcpy(data, namePtr->value, namePtr->length);

      ptr->has_variable = 1;
      ptr->variable.data = data;
      ptr->variable.len = namePtr->length;
      break;
    }
  case SA_NTF_VALUE_STRING:
  case SA_NTF_VALUE_IPADDRESS:
    {
      SaUint16T dataSize = 0;
      void *dataPtr = NULL;
      void *data;

      get_ntf_ptr_val(variableData, &infoValue, &dataSize, &dataPtr);

      /* Remove the terminating '\0' character. */
      dataSize--;

      data = malloc(dataSize);
      if (data == NULL) {
	rc = SA_AIS_ERR_NO_MEMORY;
	goto ERROR;
      }

      memcpy(data, dataPtr, dataSize);

      ptr->has_variable = 1;
      ptr->variable.data = data;
      ptr->variable.len = dataSize;
      break;
    }
  case SA_NTF_VALUE_BINARY:
    {
      SaUint16T dataSize = 0;
      void *dataPtr = NULL;
      void *data;

      get_ntf_ptr_val(variableData, &infoValue, &dataSize, &dataPtr);

      data = malloc(dataSize);
      if (data == NULL) {
	rc = SA_AIS_ERR_NO_MEMORY;
	goto ERROR;
      }

      memcpy(data, dataPtr, dataSize);

      ptr->has_variable = 1;
      ptr->variable.data = data;
      ptr->variable.len = dataSize;
      break;
    }
  case SA_NTF_VALUE_ARRAY:
    {
      SaUint16T numElements = 0;
      SaUint16T elementSize = 0;
      SaUint32T dataSize;
      void *arrayPtr = NULL;
      void *data;

      get_ntf_array_val(variableData, &infoValue, &numElements, &elementSize,
			&arrayPtr);

      dataSize = numElements*elementSize;
      data = malloc(dataSize);
      if (data == NULL) {
	rc = SA_AIS_ERR_NO_MEMORY;
	goto ERROR;
      }

      memcpy(data, arrayPtr, dataSize);
      ptr->has_variable = 1;
      ptr->variable.data = data;
      ptr->variable.len = dataSize;

      break;
    }
  default:
    rc = SA_AIS_ERR_INVALID_PARAM;
    goto ERROR;
  }

  *pptr = ptr;
  return SA_AIS_OK;

 ERROR:
  freeSafsNtfValue(ptr);

  return rc;
}

void
freeSafsNtfValue(SafsNtfValue *ptr)
{
  if (ptr == NULL)
    return;

  free(ptr->variable.data);
  free(ptr);
}

SaAisErrorT
allocSafsNtfSpecificProblem(SafsNtfSpecificProblem **pptr,
			    SaNtfSpecificProblemT *specificProblem,
			    v_data * variableData)
{
  SaAisErrorT rc;
  SafsNtfSpecificProblem *ptr = NULL;
  SaNtfValueTypeT problemType = specificProblem->problemType;

  *pptr = ptr;

  rc = checkInfoType(problemType);
  if (rc != SA_AIS_OK)
    return rc;

  ptr = malloc(sizeof(*ptr));
  if (ptr == NULL)
    return SA_AIS_ERR_NO_MEMORY;

  safs_ntf_specific_problem__init(ptr);

  ptr->problemid = specificProblem->problemId;
  rc = allocSafsNtfClassid(&ptr->problemclassid,
			   &specificProblem->problemClassId);
  if (rc != SA_AIS_OK)
    goto ERROR;

  ptr->problemtype = problemType;

  rc = allocSafsNtfValue(&ptr->problemvalue,
			 problemType,
			 specificProblem->problemValue,
			 variableData);
  if (rc != SA_AIS_OK)
    goto ERROR;

  *pptr = ptr;
  return SA_AIS_OK;

 ERROR:
  freeSafsNtfSpecificProblem(ptr);

  return rc;
}

void
freeSafsNtfSpecificProblem(SafsNtfSpecificProblem *ptr)
{
  if (ptr == NULL)
    return;

  freeSafsNtfValue(ptr->problemvalue);
  freeSafsNtfClassid(ptr->problemclassid);
  free(ptr);
}

SaAisErrorT
allocSafsNtfThresholdInformation(SafsNtfThresholdInformation ** pptr,
				 SaNtfThresholdInformationT* thresholdInformation,
				 v_data *variableData)
{
  SaAisErrorT rc;
  SafsNtfThresholdInformation *ptr = NULL;
  SaNtfValueTypeT thresholdValueType;

  *pptr = ptr;

  // Check if thresholdInformation has been set
  if (thresholdInformation->thresholdId == 0xffff)
    return SA_AIS_OK;

  thresholdValueType = thresholdInformation->thresholdValueType;
  rc = checkInfoType(thresholdValueType);
  if (rc != SA_AIS_OK)
    return rc;

  ptr = malloc(sizeof(*ptr));
  if (ptr == NULL)
    return SA_AIS_ERR_NO_MEMORY;

  safs_ntf_threshold_information__init(ptr);

  ptr->thresholdid = thresholdInformation->thresholdId;
  ptr->thresholdvaluetype = thresholdValueType;

  rc = allocSafsNtfValue(&ptr->thresholdvalue,
			 thresholdValueType,
			 thresholdInformation->thresholdValue,
			 variableData);
  if (rc != SA_AIS_OK)
    goto ERROR;

  rc = allocSafsNtfValue(&ptr->thresholdhysteresis,
			 thresholdValueType,
			 thresholdInformation->thresholdHysteresis,
			 variableData);
  if (rc != SA_AIS_OK)
    goto ERROR;

  rc = allocSafsNtfValue(&ptr->observedvalue,
			 thresholdValueType,
			 thresholdInformation->observedValue,
			 variableData);
  if (rc != SA_AIS_OK)
    goto ERROR;

  ptr->armtime = thresholdInformation->armTime;

  *pptr = ptr;
  return SA_AIS_OK;

 ERROR:
  freeSafsNtfThresholdInformation(ptr);

  return rc;
}

void
freeSafsNtfThresholdInformation(SafsNtfThresholdInformation *ptr)
{
  if (ptr == NULL)
    return;

  freeSafsNtfValue(ptr->observedvalue);
  freeSafsNtfValue(ptr->thresholdhysteresis);
  freeSafsNtfValue(ptr->thresholdvalue);
  free(ptr);
}

SaAisErrorT
allocSafsNtfAttribute(SafsNtfAttribute **pptr,
		      SaNtfAttributeT* attribute,
		      v_data *variableData)
{
  SaAisErrorT rc;
  SafsNtfAttribute *ptr = NULL;
  SaNtfValueTypeT attributeType = attribute->attributeType;

  *pptr = ptr;

  rc = checkInfoType(attributeType);
  if (rc != SA_AIS_OK)
    return rc;

  ptr = malloc(sizeof(*ptr));
  if (ptr == NULL)
    return SA_AIS_ERR_NO_MEMORY;

  safs_ntf_attribute__init(ptr);

  ptr->attributeid = attribute->attributeId;
  ptr->attributetype = attributeType;
  rc = allocSafsNtfValue(&ptr->attributevalue,
			 attributeType,
			 attribute->attributeValue,
			 variableData);
  if (rc != SA_AIS_OK)
    goto ERROR;

  *pptr = ptr;
  return SA_AIS_OK;

 ERROR:
  freeSafsNtfAttribute(ptr);

  return rc;
}

void
freeSafsNtfAttribute(SafsNtfAttribute *ptr)
{
  if (ptr == NULL)
    return;

  freeSafsNtfValue(ptr->attributevalue);
  free(ptr);
}

SaAisErrorT
allocSafsNtfProposedRepairAction(SafsNtfProposedRepairAction ** pptr,
				 SaNtfProposedRepairActionT *proposedRepairActions,
				 v_data *variableData)
{
  SaAisErrorT rc;
  SafsNtfProposedRepairAction *ptr = NULL;
  SaNtfValueTypeT actionValueType = proposedRepairActions->actionValueType;

  *pptr = ptr;

  rc = checkInfoType(actionValueType);
  if (rc != SA_AIS_OK)
    return rc;

  ptr = malloc(sizeof(*ptr));
  if (ptr == NULL)
    return SA_AIS_ERR_NO_MEMORY;

  safs_ntf_proposed_repair_action__init(ptr);

  ptr->actionid = proposedRepairActions->actionId;
  ptr->actionvaluetype = actionValueType;
  rc = allocSafsNtfValue(&ptr->actionvalue,
			 actionValueType,
			 proposedRepairActions->actionValue,
			 variableData);
  if (rc != SA_AIS_OK)
    goto ERROR;

  *pptr = ptr;
  return SA_AIS_OK;

 ERROR:
  freeSafsNtfProposedRepairAction(ptr);

  return rc;
}

void
freeSafsNtfProposedRepairAction(SafsNtfProposedRepairAction *ptr)
{
  if (ptr == NULL)
    return;

  freeSafsNtfValue(ptr->actionvalue);
  free(ptr);
}

SaAisErrorT
allocSafsNtfAttributeChange(SafsNtfAttributeChange **pptr,
			    SaNtfAttributeChangeT* attributeChange,
			    v_data *variableData)
{
  SaAisErrorT rc;
  SafsNtfAttributeChange *ptr = NULL;
  SaNtfValueTypeT attributeType = attributeChange->attributeType;

  *pptr = ptr;

  rc = checkInfoType(attributeType);
  if (rc != SA_AIS_OK)
    return rc;

  ptr = malloc(sizeof(*ptr));
  if (ptr == NULL)
    return SA_AIS_ERR_NO_MEMORY;

  safs_ntf_attribute_change__init(ptr);

  ptr->attributeid = attributeChange->attributeId;
  ptr->attributetype = attributeType;
  rc = allocSafsNtfValue(&ptr->oldattributevalue,
			 attributeType,
			 attributeChange->oldAttributeValue,
			 variableData);
  if (rc != SA_AIS_OK)
    goto ERROR;

  rc = allocSafsNtfValue(&ptr->newattributevalue,
			 attributeType,
			 attributeChange->newAttributeValue,
			 variableData);
  if (rc != SA_AIS_OK)
    goto ERROR;

  *pptr = ptr;
  return SA_AIS_OK;

 ERROR:
  freeSafsNtfAttributeChange(ptr);

  return rc;
}

void
freeSafsNtfAttributeChange(SafsNtfAttributeChange *ptr)
{
  if (ptr == NULL)
    return;

  freeSafsNtfValue(ptr->newattributevalue);
  freeSafsNtfValue(ptr->oldattributevalue);
  free(ptr);
}

SaAisErrorT
allocSafsNtfStateChange(SafsNtfStateChange ** pptr,
			SaNtfStateChangeT* stateChange,
			v_data *variableData)
{
  SafsNtfStateChange *ptr = NULL;

  *pptr = ptr;

  ptr = malloc(sizeof(*ptr));
  if (ptr == NULL)
    return SA_AIS_ERR_NO_MEMORY;

  safs_ntf_state_change__init(ptr);

  ptr->stateid = stateChange->stateId;

  if (stateChange->oldStatePresent == SA_TRUE) {
    ptr->has_oldstate = 1;
    ptr->oldstate = stateChange->oldState;
  }
  ptr->newstate = stateChange->newState;

  *pptr = ptr;
  return SA_AIS_OK;
}

void
freeSafsNtfStateChange(SafsNtfStateChange *ptr)
{
  free(ptr);
}

SaAisErrorT
allocSafsNtfSecurityAlarmDetector(SafsNtfSecurityAlarmDetector** pptr,
				  SaNtfSecurityAlarmDetectorT* securityAlarmDetector,
				  v_data *variableData)
{
  SaAisErrorT rc;
  SafsNtfSecurityAlarmDetector *ptr = NULL;
  SaNtfValueTypeT valueType = securityAlarmDetector->valueType;

  *pptr = ptr;

  rc = checkInfoType(valueType);
  if (rc != SA_AIS_OK)
    return rc;

  ptr = malloc(sizeof(*ptr));
  if (ptr == NULL)
    return SA_AIS_ERR_NO_MEMORY;

  safs_ntf_security_alarm_detector__init(ptr);

  ptr->valuetype = valueType;
  rc = allocSafsNtfValue(&ptr->value,
			 valueType,
			 securityAlarmDetector->value,
			 variableData);
  if (rc != SA_AIS_OK)
    goto ERROR;

  *pptr = ptr;
  return SA_AIS_OK;

 ERROR:
  freeSafsNtfSecurityAlarmDetector(ptr);

  return rc;
}

void
freeSafsNtfSecurityAlarmDetector(SafsNtfSecurityAlarmDetector *ptr)
{
  if (ptr == NULL)
    return;

  freeSafsNtfValue(ptr->value);
  free(ptr);
}

SaAisErrorT
allocSafsNtfServiceUser(SafsNtfServiceUser** pptr,
			SaNtfServiceUserT* serviceUser,
			v_data *variableData)
{
  SaAisErrorT rc;
  SafsNtfServiceUser *ptr = NULL;
  SaNtfValueTypeT valueType = serviceUser->valueType;

  *pptr = ptr;

  rc = checkInfoType(valueType);
  if (rc != SA_AIS_OK)
    return rc;

  ptr = malloc(sizeof(*ptr));
  if (ptr == NULL)
    return SA_AIS_ERR_NO_MEMORY;

  safs_ntf_service_user__init(ptr);

  ptr->valuetype = valueType;
  rc = allocSafsNtfValue(&ptr->value,
			 valueType,
			 serviceUser->value,
			 variableData);
  if (rc != SA_AIS_OK)
    goto ERROR;

  *pptr = ptr;
  return SA_AIS_OK;

 ERROR:
  freeSafsNtfServiceUser(ptr);

  return rc;
}

void
freeSafsNtfServiceUser(SafsNtfServiceUser *ptr)
{
  if (ptr == NULL)
    return;

  freeSafsNtfValue(ptr->value);
  free(ptr);
}

SaAisErrorT
allocBufferFromSaNameT(char** pptr, SaNameT *saName)
{
  char* ptr = NULL;

  *pptr = ptr;
  ptr = malloc(saName->length+1);
  if (ptr == NULL)
    return SA_AIS_ERR_NO_MEMORY;

  memcpy(ptr, saName->value, saName->length);
  ptr[saName->length] = 0;

  *pptr = ptr;
  return SA_AIS_OK;
}

SaAisErrorT
allocBufferFromSaStringT(char** pptr, SaStringT saString, size_t len)
{
  char* ptr = NULL;

  *pptr = ptr;

  ptr = malloc(len+1);
  if (ptr == NULL)
    return SA_AIS_ERR_NO_MEMORY;

  memcpy(ptr, saString, len);
  ptr[len] = 0;

  *pptr = ptr;
  return SA_AIS_OK;
}

void
allocNtfVariableData(v_data *variableData,
		     SaUint16T max_data_size)
{
  variableData->ptr = NULL;
  variableData->size = 0;
  variableData->max_data_size = max_data_size;
}

void
freeNtfVariableData(v_data *variableData)
{
  free(variableData->ptr);
  variableData->ptr = NULL;
  variableData->size = 0;

}

SaAisErrorT
alloc_ntf_ptr_val(v_data *variableData,
		  SaNtfValueT *value,
		  SaUint16T dataSize,
		  void **dataPtr)
{
  if ((variableData->size + dataSize) <= variableData->max_data_size) {
    void *ptr;

    ptr = realloc(variableData->ptr, variableData->size + dataSize);
    if (ptr == NULL)
      return SA_AIS_ERR_NO_MEMORY;

    variableData->ptr = ptr;
    value->ptrVal.dataOffset = variableData->size;
    value->ptrVal.dataSize = dataSize;
    *dataPtr = (void*)((char*)variableData->ptr + variableData->size);
    variableData->size += dataSize;
  } else {
    return SA_AIS_ERR_NO_SPACE;
  }

  return SA_AIS_OK;
}

SaAisErrorT
get_ntf_ptr_val(v_data *variableData,
		const SaNtfValueT *value,
		SaUint16T *dataSize,
		void **dataPtr)
{
  if (variableData->size < value->ptrVal.dataOffset ||
      variableData->ptr == NULL)
    return SA_AIS_ERR_INVALID_PARAM; // What return value???

  *dataPtr = (void*)((char*)variableData->ptr + value->ptrVal.dataOffset);
  *dataSize = value->ptrVal.dataSize;

  return SA_AIS_OK;
}

SaAisErrorT
alloc_ntf_array_val(v_data *variableData,
		    SaNtfValueT *value,
		    SaUint16T numElements,
		    SaUint16T elementSize,
		    void **arrayPtr)
{
  SaUint32T addedSize = numElements * elementSize;

  if ((variableData->size + addedSize) <= variableData->max_data_size) {
    void *ptr;

    ptr = realloc(variableData->ptr, variableData->size + addedSize);
    if (ptr == NULL)
      return SA_AIS_ERR_NO_MEMORY;

    variableData->ptr = ptr;
    value->arrayVal.arrayOffset = variableData->size;
    value->arrayVal.numElements = numElements;
    value->arrayVal.elementSize = elementSize;
    *arrayPtr = (void*)((char*)variableData->ptr + variableData->size);
    variableData->size += addedSize;
  } else {
    return SA_AIS_ERR_NO_SPACE;
  }

  return SA_AIS_OK;
}

SaAisErrorT
get_ntf_array_val(v_data *variableData,
		  const SaNtfValueT *value,
		  SaUint16T *numElements,
		  SaUint16T *elementSize,
		  void **arrayPtr)
{
  SaUint32T maxOffset = value->arrayVal.arrayOffset +
    value->arrayVal.numElements * value->arrayVal.elementSize;
  SaUint32T size = variableData->size;

  if (size < maxOffset || variableData->ptr == NULL)
    return SA_AIS_ERR_INVALID_PARAM; // What return value???

  *numElements = value->arrayVal.numElements;
  *elementSize = value->arrayVal.elementSize;
  *arrayPtr = (void*)((char*)variableData->ptr + value->arrayVal.arrayOffset);

  return SA_AIS_OK;
}

SaAisErrorT
checkSourceIndicator(SaNtfSourceIndicatorT sourceIndicator)
{
  if (sourceIndicator < SA_NTF_OBJECT_OPERATION ||
      sourceIndicator > SA_NTF_UNKNOWN_OPERATION)
    return SA_AIS_ERR_INVALID_PARAM;

  return SA_AIS_OK;
}

SaAisErrorT
checkValueType(SaNtfValueTypeT valueType)
{
  if (valueType < SA_NTF_VALUE_UINT8 ||
      valueType > SA_NTF_VALUE_ARRAY)
    return SA_AIS_ERR_INVALID_PARAM;

  return SA_AIS_OK;
}

SaAisErrorT
checkProbableCause(SaNtfProbableCauseT probableCause)
{
  if (probableCause < SA_NTF_ADAPTER_ERROR ||
      probableCause > SA_NTF_UNSPECIFIED_REASON)
    return SA_AIS_ERR_INVALID_PARAM;

  return SA_AIS_OK;
}

SaAisErrorT
checkSeverity(SaNtfSeverityT severity)
{
  /* SA_NTF_SEVERITY_CLEARED only valid in alarm*/
  if (severity < SA_NTF_SEVERITY_CLEARED ||
      severity > SA_NTF_SEVERITY_CRITICAL)
    return SA_AIS_ERR_INVALID_PARAM;

  return SA_AIS_OK;
}

SaAisErrorT
checkSeverityTrend(SaNtfSeverityTrendT trend)
{
  if (trend < SA_NTF_TREND_MORE_SEVERE ||
      trend > SA_NTF_TREND_LESS_SEVERE)
    return SA_AIS_ERR_INVALID_PARAM;

  return SA_AIS_OK;
}

SaAisErrorT
checkEventType(SaNtfEventTypeT eventType)
{
  if (eventType < SA_NTF_OBJECT_NOTIFICATIONS_START ||
      eventType > SA_NTF_TIME_VIOLATION)
    return SA_AIS_ERR_INVALID_PARAM;

  return SA_AIS_OK;
}

SaAisErrorT
checkInfoType(SaNtfValueTypeT valueType)
{
  if (valueType < SA_NTF_VALUE_UINT8 ||
      valueType > SA_NTF_VALUE_ARRAY)
    return SA_AIS_ERR_INVALID_PARAM;

  return SA_AIS_OK;
}
