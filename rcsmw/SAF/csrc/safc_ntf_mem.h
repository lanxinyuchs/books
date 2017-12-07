/*
 * %EricssonCopyright%
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2012-2012. All Rights Reserved.
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

#include <saNtf.h>
#include "ntf.pb-c.h"

typedef struct variable_data {
  void *ptr;
  size_t size;
  SaUint32T max_data_size;
} v_data;

extern SaAisErrorT
allocSaNtfObjectCreateDeleteNotification(SaNtfObjectCreateDeleteNotificationT *objectCreateDeleteNotification,
					 SaUint16T numCorrelatedNotifications,
					 SaUint16T lengthAdditionalText,
					 SaUint16T numAdditionalInfo,
					 SaUint16T numAttributes);
extern void
freeSaNtfObjectCreateDeleteNotification(SaNtfObjectCreateDeleteNotificationT *objectCreateDeleteNotification);

extern SaAisErrorT
allocSaNtfAttributeChangeNotification(SaNtfAttributeChangeNotificationT *attributeChangeNotification,
				      SaUint16T numCorrelatedNotifications,
				      SaUint16T lengthAdditionalText,
				      SaUint16T numAdditionalInfo,
				      SaUint16T numAttributes);
extern void
freeSaNtfAttributeChangeNotification(SaNtfAttributeChangeNotificationT *attributeChangeNotification);

extern SaAisErrorT
allocSaNtfStateChangeNotification(SaNtfStateChangeNotificationT *stateChangeNotification,
				  SaUint16T numCorrelatedNotifications,
				  SaUint16T lengthAdditionalText,
				  SaUint16T numAdditionalInfo,
				  SaUint16T numStateChanges);
extern void
freeSaNtfStateChangeNotification(SaNtfStateChangeNotificationT *stateChangeNotification);

extern SaAisErrorT
allocSaNtfAlarmNotification(SaNtfAlarmNotificationT *alarmNotification,
			    SaUint16T numCorrelatedNotifications,
			    SaUint16T lengthAdditionalText,
			    SaUint16T numAdditionalInfo,
			    SaUint16T numSpecificProblems,
			    SaUint16T numMonitoredAttributes,
			    SaUint16T numProposedRepairActions);
extern void
freeSaNtfAlarmNotification(SaNtfAlarmNotificationT *alarmNotification);

extern SaAisErrorT
allocSaNtfSecurityAlarmNotification(SaNtfSecurityAlarmNotificationT *securityAlarmNotification,
				    SaUint16T numCorrelatedNotifications,
				    SaUint16T lengthAdditionalText,
				    SaUint16T numAdditionalInfo);
extern void
freeSaNtfSecurityAlarmNotification(SaNtfSecurityAlarmNotificationT *securityAlarmNotification);

extern SaAisErrorT
allocSafsNtfObjectCreateDeleteNotification(SafsNtfObjectCreateDeleteNotification **pptr,
					   SaNtfObjectCreateDeleteNotificationT *objectCreateDeleteNotification,
					   v_data *variableData);
extern void
freeSafsNtfObjectCreateDeleteNotification(SafsNtfObjectCreateDeleteNotification *ptr);

extern SaAisErrorT
allocSafsNtfAttributeChangeNotification(SafsNtfAttributeChangeNotification **pptr,
					SaNtfAttributeChangeNotificationT *attributeChangeNotification,
					v_data * variableData);
extern void
freeSafsNtfAttributeChangeNotification(SafsNtfAttributeChangeNotification *ptr);

extern SaAisErrorT
allocSafsNtfStateChangeNotification(SafsNtfStateChangeNotification **pptr,
				    SaNtfStateChangeNotificationT *stateChangeNotification,
				    v_data * variableData);
extern void
freeSafsNtfStateChangeNotification(SafsNtfStateChangeNotification *ptr);

extern SaAisErrorT
allocSafsNtfAlarmNotification(SafsNtfAlarmNotification **pptr,
			      SaNtfAlarmNotificationT *alarmNotification,
			      v_data * variableData);
extern void
freeSafsNtfAlarmNotification(SafsNtfAlarmNotification *ptr);

extern SaAisErrorT
allocSafsNtfSecurityAlarmNotification(SafsNtfSecurityAlarmNotification **pptr,
				      SaNtfSecurityAlarmNotificationT *securityAlarmNotification,
				      v_data * variableData);
extern void
freeSafsNtfSecurityAlarmNotification(SafsNtfSecurityAlarmNotification *ptr);

extern SaAisErrorT
allocSafsNtfNotificationHeader(SafsNtfNotificationHeader **pptr,
			       SaNtfNotificationHeaderT notificationHeader,
			       v_data * variableData);
extern void
freeSafsNtfNotificationHeader(SafsNtfNotificationHeader *ptr);

extern SaAisErrorT
allocSafsNtfClassid(SafsNtfClassId **pptr,
		    SaNtfClassIdT *classId);
extern void
freeSafsNtfClassid(SafsNtfClassId *ptr);

extern SaAisErrorT
allocSafsNtfAdditionalInfo(SafsNtfAdditionalInfo **pptr,
			   SaNtfAdditionalInfoT *additionalInfo,
			   v_data *variableData);
extern void
freeSafsNtfAdditionalInfo(SafsNtfAdditionalInfo *ptr);

extern SaAisErrorT
allocSafsNtfValue(SafsNtfValue **pptr,
		  SaNtfValueTypeT infoType,
		  SaNtfValueT infoValue,
		  v_data *variableData);
extern void
freeSafsNtfValue(SafsNtfValue *ptr);

extern SaAisErrorT
allocSafsNtfSpecificProblem(SafsNtfSpecificProblem **pptr,
			    SaNtfSpecificProblemT *SpecificProblem,
			    v_data * variableData);
extern void
freeSafsNtfSpecificProblem(SafsNtfSpecificProblem *ptr);

extern SaAisErrorT
allocSafsNtfThresholdInformation(SafsNtfThresholdInformation **pptr,
				 SaNtfThresholdInformationT *thresholdInformation,
				 v_data *variableData);
extern void
freeSafsNtfThresholdInformation(SafsNtfThresholdInformation *ptr);

extern SaAisErrorT
allocSafsNtfAttribute(SafsNtfAttribute **pptr,
		      SaNtfAttributeT *attribute,
		      v_data *variableData);
extern void
freeSafsNtfAttribute(SafsNtfAttribute *ptr);

extern SaAisErrorT
allocSafsNtfProposedRepairAction(SafsNtfProposedRepairAction** pptr,
				 SaNtfProposedRepairActionT *proposedRepairActions,
				 v_data *variableData);
extern void
freeSafsNtfProposedRepairAction(SafsNtfProposedRepairAction *ptr);

extern SaAisErrorT
allocSafsNtfAttributeChange(SafsNtfAttributeChange** pptr,
			    SaNtfAttributeChangeT* attributeChange,
			    v_data *variableData);
extern void
freeSafsNtfAttributeChange(SafsNtfAttributeChange *ptr);

extern SaAisErrorT
allocSafsNtfStateChange(SafsNtfStateChange** pptr,
			SaNtfStateChangeT* stateChange,
			v_data *variableData);
extern void
freeSafsNtfStateChange(SafsNtfStateChange *ptr);

extern SaAisErrorT
allocSafsNtfSecurityAlarmDetector(SafsNtfSecurityAlarmDetector** pptr,
				  SaNtfSecurityAlarmDetectorT* securityAlarmDetector,
				  v_data *variableData);
extern void
freeSafsNtfSecurityAlarmDetector(SafsNtfSecurityAlarmDetector *ptr);

extern SaAisErrorT
allocSafsNtfServiceUser(SafsNtfServiceUser**pptr,
			SaNtfServiceUserT* serviceUser,
			v_data *variableData);
extern void
freeSafsNtfServiceUser(SafsNtfServiceUser *ptr);

extern SaAisErrorT
allocBufferFromSaStringT(char** pptr, SaStringT saString, size_t len);
extern SaAisErrorT
allocBufferFromSaNameT(char** pptr, SaNameT *saName);

extern void
allocNtfVariableData(v_data *variableData,
		     SaUint16T max_data_size);
extern void
freeNtfVariableData(v_data *variableData);

extern SaAisErrorT
alloc_ntf_ptr_val(v_data *variableData,
		  SaNtfValueT *value,
		  SaUint16T dataSize,
		  void **dataPtr);

extern SaAisErrorT
alloc_ntf_array_val(v_data *variableData,
		    SaNtfValueT *value,
		    SaUint16T numElements,
		    SaUint16T elementSize,
		    void **arrayPtr);

extern SaAisErrorT
get_ntf_ptr_val(v_data *variableData,
		const SaNtfValueT *value,
		SaUint16T *dataSize,
		void **dataPtr);

extern SaAisErrorT
get_ntf_array_val(v_data *variableData,
		  const SaNtfValueT *value,
		  SaUint16T *numElements,
		  SaUint16T *elementSize,
		  void **arrayPtr);
