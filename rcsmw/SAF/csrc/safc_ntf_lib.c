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
 *  Purpose : SAF NTF Agent Library
 * ----------------------------------------------------------------------
 *
 */

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <pthread.h>
#include <netinet/in.h>

#include <saAis.h>
#include <saNtf.h>

#include "safc_trace.h"
#include "safc_sock_lib.h"
#include "safc_ntf_mem.h"
#include "safc_array.h"
#include "safc_list.h"
#include "sa_ais.pb-c.h"
#include "ntf.pb-c.h"

/*
 * Macros for lock handling.
 */
#define ZERO(expr) assert(0 == (expr))
#define LOCK_INIT(lock) ZERO(pthread_rwlock_init(lock, NULL))
#define LOCK_RDLOCK(lock) ZERO(pthread_rwlock_rdlock(lock))
#define LOCK_WRLOCK(lock) ZERO(pthread_rwlock_wrlock(lock))
#define LOCK_UNLOCK(lock) ZERO(pthread_rwlock_unlock(lock))

#define MUTEX_INIT(mutex) ZERO(pthread_mutex_init(mutex, NULL))
#define MUTEX_LOCK(mutex) ZERO(pthread_mutex_lock(mutex))
#define MUTEX_UNLOCK(mutex) ZERO(pthread_mutex_unlock(mutex))

#define NTF_RELEASE_CODE 'A'
#define NTF_MAJOR_VERSION 1
#define NTF_MINOR_VERSION 1

typedef struct {
  SaNtfHandleT internalHandle;
  int hasCallbacks;
  SaNtfCallbacksT callbacks;
  SafcSListT notifications;
  pthread_rwlock_t notifications_lock;
  int sockfd;
  int cb_sockfd;
  pthread_mutex_t mutex;
} SafcNtfClientT;

typedef struct {
  SafcNtfClientT* client;
  SaNtfNotificationTypeT notificationType;
  SaNtfNotificationHandleT notificationHandle;
  union
  {
    SaNtfObjectCreateDeleteNotificationT* objectCreateDeleteNotification;
    SaNtfAttributeChangeNotificationT*    attributeChangeNotification;
    SaNtfStateChangeNotificationT*        stateChangeNotification;
    SaNtfAlarmNotificationT*              alarmNotification;
    SaNtfSecurityAlarmNotificationT*      securityAlarmNotification;
  } notification;
  v_data variableData;
} SafcNtfNotificationT;

pthread_rwlock_t ntfClients_lock = PTHREAD_RWLOCK_INITIALIZER;
pthread_rwlock_t ntfNotifications_lock = PTHREAD_RWLOCK_INITIALIZER;
safc_array ntfClients = SAFC_ARRAY_INITIALIZER;
safc_array ntfNotifications = SAFC_ARRAY_INITIALIZER;

/* safc_ntf_version_validate */
static SaAisErrorT safc_ntf_version_validate(SaVersionT *version)
{
   SaAisErrorT rc = SA_AIS_OK;

   if ((version->releaseCode != NTF_RELEASE_CODE) ||
       (version->majorVersion > NTF_MAJOR_VERSION) ||
       (version->majorVersion <= 0)) {
      TRACE1(("ERR_VERSION: Version %c %u not supported\n",
	      version->releaseCode,
	      version->majorVersion));
      rc = SA_AIS_ERR_VERSION;
   }

   version->releaseCode = NTF_RELEASE_CODE;
   version->majorVersion = NTF_MAJOR_VERSION;
   version->minorVersion = NTF_MINOR_VERSION;
   return rc;
}

/* 3.13.1 saNtfInitialize */
SaAisErrorT
saNtfInitialize(SaNtfHandleT *ntfHandle,
		const SaNtfCallbacksT *ntfCallbacks,
		SaVersionT *version)
{
  SaAisErrorT rc = SA_AIS_OK;
  SaAisErrorT recv_ret = SA_AIS_OK;
  SafcNtfClientT *client = NULL;
  int port;
  char* hostname = "localhost";
  safc_buf_t msgbuf;
  SafsNtfInitialize init_msg =  SAFS_NTF_INITIALIZE__INIT;
  SafsNtfCallbacks cb_msg =  SAFS_NTF_CALLBACKS__INIT;
  SafsVersion version_msg =  SAFS_VERSION__INIT;
  SafsNtfMessage msg = SAFS_NTF_MESSAGE__INIT;
  SafsNtfInitializeRet *ret_msg;
  SafsVersion *returned_version;
  
  TRACE_ENTER();

  if (ntfHandle == NULL || version == NULL) {
    rc = SA_AIS_ERR_INVALID_PARAM;
    goto ERROR;
  }

   /* Validations : Version */
   rc = safc_ntf_version_validate(version);
   if (rc != SA_AIS_OK) {
      TRACE1(("ERR_VERSION: Version validation failed\n"));
      goto ERROR;
   }

  /* Alloc the client info data structure, handle for now */
  client = calloc(1, sizeof(SafcNtfClientT));
  if (client == NULL) {
    rc = SA_AIS_ERR_NO_MEMORY;
    goto ERROR;
  }

  /* Store the callback functions, if set */
  if (ntfCallbacks) {
    client->hasCallbacks = 1;
    client->callbacks = *ntfCallbacks;
  } else {
    client->hasCallbacks = 0;
  }

  client->notifications = NULL;
  LOCK_INIT(&client->notifications_lock);

  port = safc_protobuf_port("SAFC_NTF_PORT", 10003);
  client->sockfd = safc_connect(hostname, port);
  if (client->sockfd < 0) {
    rc = SA_AIS_ERR_UNAVAILABLE;
    goto ERROR;
  }

  MUTEX_INIT(&client->mutex);

  version_msg.releasecode = version->releaseCode;
  version_msg.majorversion = version->majorVersion;
  version_msg.minorversion = version->minorVersion;

  init_msg.callbacks = &cb_msg;
  init_msg.version = &version_msg;

  msg.initialize = &init_msg;

  msgbuf.size = safs_ntf_message__get_packed_size(&msg);
  msgbuf.buf = malloc(msgbuf.size);
  safs_ntf_message__pack(&msg, (uint8_t *) msgbuf.buf);

  MUTEX_LOCK(&(client->mutex));
  recv_ret = safc_send_recv(client->sockfd, &msgbuf);
  MUTEX_UNLOCK(&(client->mutex));

  if (recv_ret == SA_AIS_ERR_MESSAGE_ERROR)
    {
      TRACE1(("SA_AIS_ERR_UNAVAILABLE: Couldn't send/read from socket\n"));
      free(msgbuf.buf);
      rc = SA_AIS_ERR_UNAVAILABLE; // Perhaps another error code;
      goto ERROR;
   }

  /* Decoding of answer */
  ret_msg = safs_ntf_initialize_ret__unpack(NULL,
					    msgbuf.size,
					    (uint8_t *) msgbuf.buf);
  free(msgbuf.buf);

  if (ret_msg == NULL) {
    TRACE1(("error unpacking incoming message\n"));
    rc = SA_AIS_ERR_UNAVAILABLE; // Perhaps another error code;
    goto ERROR;
  }

  returned_version = ret_msg->version;

  version->releaseCode = returned_version->releasecode;
  version->majorVersion = returned_version->majorversion;
  version->minorVersion = returned_version->minorversion;

  rc = ret_msg->returnval;

  if (rc != SA_AIS_OK) {
    safc_disconnect(client->sockfd);
    goto ERROR;
  } else {
    client->internalHandle = ret_msg->handle;
    LOCK_WRLOCK(&ntfClients_lock);
    *ntfHandle = safc_array_insert(&ntfClients, (void*)client);
    LOCK_UNLOCK(&ntfClients_lock);
  }

  safs_ntf_initialize_ret__free_unpacked(ret_msg, NULL);
  TRACE_LEAVE();
  return rc;
 ERROR:
  free(client);
  TRACE_LEAVE();
  return rc;
}

/* 3.13.2 saNtfSelectionObjectGet */
SaAisErrorT
saNtfSelectionObjectGet(SaNtfHandleT ntfHandle,
			SaSelectionObjectT *selectionObject)
{
  SaAisErrorT rc = SA_AIS_OK;
  SafcNtfClientT *client;

  TRACE_ENTER();

  if (selectionObject == NULL) {
    rc = SA_AIS_ERR_INVALID_PARAM;
    goto EXIT;
  }

  LOCK_RDLOCK(&ntfClients_lock);
  client = safc_array_get(&ntfClients, (int)ntfHandle);
  LOCK_UNLOCK(&ntfClients_lock);
  if (client == NULL) {
    rc = SA_AIS_ERR_BAD_HANDLE;
    goto EXIT;
  }

  *selectionObject = (SaSelectionObjectT)client->cb_sockfd;

 EXIT:
  TRACE_LEAVE();
  return rc;
}

/* 3.13.3 saNtfDispatch */
SaAisErrorT
saNtfDispatch(SaNtfHandleT ntfHandle,
	      SaDispatchFlagsT dispatchFlags)
{
  SaAisErrorT rc = SA_AIS_OK;
  SafcNtfClientT *client;

  TRACE_ENTER();

  if (dispatchFlags != SA_DISPATCH_ONE &&
      dispatchFlags != SA_DISPATCH_ALL &&
      dispatchFlags != SA_DISPATCH_BLOCKING) {
    rc = SA_AIS_ERR_INVALID_PARAM;
    goto EXIT;
  }

  LOCK_RDLOCK(&ntfClients_lock);
  client = safc_array_get(&ntfClients, (int)ntfHandle);
  LOCK_UNLOCK(&ntfClients_lock);
  if (client == NULL) {
    rc = SA_AIS_ERR_BAD_HANDLE;
    goto EXIT;
  }

 EXIT:
  TRACE_LEAVE();
  return rc;
}

/* 3.13.4 saNtfFinalize */
SaAisErrorT
saNtfFinalize(SaNtfHandleT ntfHandle)
{
  SaAisErrorT rc = SA_AIS_OK;
  SaAisErrorT recv_ret = SA_AIS_OK;
  SafcNtfClientT *client;
  SafcNtfNotificationT *notification;

  safc_buf_t msgbuf;
  SafsNtfFinalize fin_msg =  SAFS_NTF_FINALIZE__INIT;
  SafsNtfMessage msg = SAFS_NTF_MESSAGE__INIT;
  SafsNtfFinalizeRet *ret_msg;

  TRACE_ENTER();

  LOCK_RDLOCK(&ntfClients_lock);
  client = safc_array_get(&ntfClients, (int)ntfHandle);
  LOCK_UNLOCK(&ntfClients_lock);
  if (client == NULL) {
    rc = SA_AIS_ERR_BAD_HANDLE;
    goto EXIT;
  }

  /* Coding of message */
  fin_msg.handle = client->internalHandle;

  msg.finalize = &fin_msg;

  msgbuf.size = safs_ntf_message__get_packed_size(&msg);
  msgbuf.buf = malloc(msgbuf.size);
  safs_ntf_message__pack(&msg, (uint8_t *) msgbuf.buf);

  /* Send message & wait for answer*/
  MUTEX_LOCK(&(client->mutex));
  recv_ret = safc_send_recv(client->sockfd, &msgbuf);
  MUTEX_UNLOCK(&(client->mutex));

  if(recv_ret == SA_AIS_ERR_MESSAGE_ERROR)
  {
     TRACE1(("SA_AIS_ERR_UNAVAILABLE: Couldn't send/read from socket\n"));
     rc = SA_AIS_ERR_UNAVAILABLE; // Perhaps another error code;
     goto EXIT;
  }

  /* Decoding of answer */
  ret_msg = safs_ntf_finalize_ret__unpack(NULL,
					  msgbuf.size,
					  (uint8_t *) msgbuf.buf);
  free(msgbuf.buf);

  if (ret_msg == NULL) {
    TRACE1(("error unpacking incoming message\n"));
    rc = SA_AIS_ERR_UNAVAILABLE; // Perhaps another error code;
    goto EXIT;
  }

  rc = ret_msg->returnval;
  safs_ntf_finalize_ret__free_unpacked(ret_msg, NULL);

  safc_disconnect(client->sockfd);

  LOCK_WRLOCK(&client->notifications_lock);
  while(client->notifications != NULL) {
    client->notifications = safc_slist_remove_first(client->notifications,
						    (void**)&notification);
    LOCK_WRLOCK(&ntfNotifications_lock);
    safc_array_remove(&ntfNotifications, (int)notification->notificationHandle);
    LOCK_UNLOCK(&ntfNotifications_lock);
    free(notification);
  }
  LOCK_UNLOCK(&client->notifications_lock);

  LOCK_WRLOCK(&ntfClients_lock);
  safc_array_remove(&ntfClients, (int)ntfHandle);
  LOCK_UNLOCK(&ntfClients_lock);

  free(client);

 EXIT:
  TRACE_LEAVE();
  return rc;
}

/* 3.14.1 saNtfObjectCreateDeleteNotificationAllocate */
SaAisErrorT
saNtfObjectCreateDeleteNotificationAllocate(SaNtfHandleT ntfHandle,
					    SaNtfObjectCreateDeleteNotificationT *objectCreateDeleteNotification,
					    SaUint16T numCorrelatedNotifications,
					    SaUint16T lengthAdditionalText,
					    SaUint16T numAdditionalInfo,
					    SaUint16T numAttributes,
					    SaInt16T variableDataSize)
{
  SaAisErrorT rc = SA_AIS_OK;
  SafcNtfClientT *client;
  SafcNtfNotificationT *notification;

  TRACE_ENTER();

  if (objectCreateDeleteNotification == NULL) {
    rc = SA_AIS_ERR_INVALID_PARAM;
    goto EXIT;
  }

  LOCK_RDLOCK(&ntfClients_lock);
  client = safc_array_get(&ntfClients, (int)ntfHandle);
  LOCK_UNLOCK(&ntfClients_lock);
  if (client == NULL) {
    rc = SA_AIS_ERR_BAD_HANDLE;
    goto EXIT;
  }

  notification = calloc(1, sizeof(*notification));
  if (notification == NULL) {
    rc = SA_AIS_ERR_NO_MEMORY;
    goto EXIT;
  }

  notification->client = client;
  notification->notificationType = SA_NTF_TYPE_OBJECT_CREATE_DELETE;

  allocNtfVariableData(&notification->variableData, variableDataSize);

  rc = allocSaNtfObjectCreateDeleteNotification(objectCreateDeleteNotification,
						numCorrelatedNotifications,
						lengthAdditionalText,
						numAdditionalInfo,
						numAttributes);
  if (rc != SA_AIS_OK) {
    free(notification);
    goto EXIT;
  }

  LOCK_WRLOCK(&client->notifications_lock);
  client->notifications = safc_slist_prepend(client->notifications,
					     notification);
  LOCK_UNLOCK(&client->notifications_lock);

  LOCK_WRLOCK(&ntfNotifications_lock);
  objectCreateDeleteNotification->notificationHandle =
    safc_array_insert(&ntfNotifications, (void*)notification);
  LOCK_UNLOCK(&ntfNotifications_lock);

  notification->notificationHandle =
    objectCreateDeleteNotification->notificationHandle;
  notification->notification.objectCreateDeleteNotification =
    objectCreateDeleteNotification;

 EXIT:
  TRACE_LEAVE();
  return rc;
}

/* 3.14.2 saNtfAttributeChangeNotificationAllocate */
SaAisErrorT
saNtfAttributeChangeNotificationAllocate(SaNtfHandleT ntfHandle,
					 SaNtfAttributeChangeNotificationT *attributeChangeNotification,
					 SaUint16T numCorrelatedNotifications,
					 SaUint16T lengthAdditionalText,
					 SaUint16T numAdditionalInfo,
					 SaUint16T numAttributes,
					 SaInt16T variableDataSize)
{
  SaAisErrorT rc = SA_AIS_OK;
  SafcNtfClientT *client;
  SafcNtfNotificationT *notification;

  TRACE_ENTER();

  if (attributeChangeNotification == NULL) {
    rc = SA_AIS_ERR_INVALID_PARAM;
    goto EXIT;
  }

  LOCK_RDLOCK(&ntfClients_lock);
  client = safc_array_get(&ntfClients, (int)ntfHandle);
  LOCK_UNLOCK(&ntfClients_lock);
  if (client == NULL) {
    rc = SA_AIS_ERR_BAD_HANDLE;
    goto EXIT;
  }

  notification = calloc(1, sizeof(*notification));
  if (notification == NULL) {
    rc = SA_AIS_ERR_NO_MEMORY;
    goto EXIT;
  }

  notification->client = client;
  notification->notificationType = SA_NTF_TYPE_ATTRIBUTE_CHANGE;

  allocNtfVariableData(&notification->variableData, variableDataSize);

  rc = allocSaNtfAttributeChangeNotification(attributeChangeNotification,
					     numCorrelatedNotifications,
					     lengthAdditionalText,
					     numAdditionalInfo,
					     numAttributes);
  if (rc != SA_AIS_OK) {
    free(notification);
    goto EXIT;
  }

  LOCK_WRLOCK(&client->notifications_lock);
  client->notifications = safc_slist_prepend(client->notifications,
					     notification);
  LOCK_UNLOCK(&client->notifications_lock);

  LOCK_WRLOCK(&ntfNotifications_lock);
  attributeChangeNotification->notificationHandle =
    safc_array_insert(&ntfNotifications, (void*)notification);
  LOCK_UNLOCK(&ntfNotifications_lock);

  notification->notificationHandle =
    attributeChangeNotification->notificationHandle;
  notification->notification.attributeChangeNotification =
    attributeChangeNotification;

 EXIT:
  TRACE_LEAVE();
  return rc;
}

/* 3.14.3 saNtfStateChangeNotificationAllocate */
SaAisErrorT
saNtfStateChangeNotificationAllocate(SaNtfHandleT ntfHandle,
				     SaNtfStateChangeNotificationT *stateChangeNotification,
				     SaUint16T numCorrelatedNotifications,
				     SaUint16T lengthAdditionalText,
				     SaUint16T numAdditionalInfo,
				     SaUint16T numStateChanges,
				     SaInt16T variableDataSize)
{
  SaAisErrorT rc = SA_AIS_OK;
  SafcNtfClientT *client;
  SafcNtfNotificationT *notification;

  TRACE_ENTER();

  if (stateChangeNotification == NULL) {
    rc = SA_AIS_ERR_INVALID_PARAM;
    goto EXIT;
  }

  LOCK_RDLOCK(&ntfClients_lock);
  client = safc_array_get(&ntfClients, (int)ntfHandle);
  LOCK_UNLOCK(&ntfClients_lock);
  if (client == NULL) {
    rc = SA_AIS_ERR_BAD_HANDLE;
    goto EXIT;
  }

  notification = calloc(1, sizeof(*notification));
  if (notification == NULL) {
    rc = SA_AIS_ERR_NO_MEMORY;
    goto EXIT;
  }

  notification->client = client;
  notification->notificationType = SA_NTF_TYPE_STATE_CHANGE;

  allocNtfVariableData(&notification->variableData, variableDataSize);

  rc = allocSaNtfStateChangeNotification(stateChangeNotification,
					 numCorrelatedNotifications,
					 lengthAdditionalText,
					 numAdditionalInfo,
					 numStateChanges);
  if (rc != SA_AIS_OK) {
    free(notification);
    goto EXIT;
  }

  LOCK_WRLOCK(&client->notifications_lock);
  client->notifications = safc_slist_prepend(client->notifications,
					     notification);
  LOCK_UNLOCK(&client->notifications_lock);

  LOCK_WRLOCK(&ntfNotifications_lock);
  stateChangeNotification->notificationHandle =
    safc_array_insert(&ntfNotifications, (void*)notification);
  LOCK_UNLOCK(&ntfNotifications_lock);

  notification->notificationHandle =
    stateChangeNotification->notificationHandle;
  notification->notification.stateChangeNotification =
    stateChangeNotification;

 EXIT:
  TRACE_LEAVE();
  return rc;
}

/* 3.14.4 saNtfAlarmNotificationAllocate */
SaAisErrorT
saNtfAlarmNotificationAllocate(SaNtfHandleT ntfHandle,
			       SaNtfAlarmNotificationT *alarmNotification,
			       SaUint16T numCorrelatedNotifications,
			       SaUint16T lengthAdditionalText,
			       SaUint16T numAdditionalInfo,
			       SaUint16T numSpecificProblems,
			       SaUint16T numMonitoredAttributes,
			       SaUint16T numProposedRepairActions,
			       SaInt16T variableDataSize)
{
  SaAisErrorT rc = SA_AIS_OK;
  SafcNtfClientT *client;
  SafcNtfNotificationT *notification;

  TRACE_ENTER();

  if (alarmNotification == NULL) {
    rc = SA_AIS_ERR_INVALID_PARAM;
    goto EXIT;
  }

  LOCK_RDLOCK(&ntfClients_lock);
  client = safc_array_get(&ntfClients, (int)ntfHandle);
  LOCK_UNLOCK(&ntfClients_lock);
  if (client == NULL) {
    rc = SA_AIS_ERR_BAD_HANDLE;
    goto EXIT;
  }

  notification = calloc(1, sizeof(*notification));
  if (notification == NULL) {
    rc = SA_AIS_ERR_NO_MEMORY;
    goto EXIT;
  }

  notification->client = client;
  notification->notificationType = SA_NTF_TYPE_ALARM;

  allocNtfVariableData(&notification->variableData, variableDataSize);

  rc = allocSaNtfAlarmNotification(alarmNotification,
				   numCorrelatedNotifications,
				   lengthAdditionalText,
				   numAdditionalInfo,
				   numSpecificProblems,
				   numMonitoredAttributes,
				   numProposedRepairActions);
  if (rc != SA_AIS_OK) {
    free(notification);
    goto EXIT;
  }

  LOCK_WRLOCK(&client->notifications_lock);
  client->notifications = safc_slist_prepend(client->notifications,
					     notification);
  LOCK_UNLOCK(&client->notifications_lock);

  LOCK_WRLOCK(&ntfNotifications_lock);
  alarmNotification->notificationHandle =
    safc_array_insert(&ntfNotifications, (void*)notification);
  LOCK_UNLOCK(&ntfNotifications_lock);

  notification->notificationHandle = alarmNotification->notificationHandle;
  notification->notification.alarmNotification = alarmNotification;

 EXIT:
  TRACE_LEAVE();
  return rc;
}

/* 3.14.5 saNtfSecurityAlarmNotificationAllocate */
SaAisErrorT
saNtfSecurityAlarmNotificationAllocate(SaNtfHandleT ntfHandle,
				       SaNtfSecurityAlarmNotificationT *securityAlarmNotification,
				       SaUint16T numCorrelatedNotifications,
				       SaUint16T lengthAdditionalText,
				       SaUint16T numAdditionalInfo,
				       SaInt16T variableDataSize)
{
  SaAisErrorT rc = SA_AIS_OK;
  SafcNtfClientT *client;
  SafcNtfNotificationT *notification;

  TRACE_ENTER();

  if (securityAlarmNotification == NULL) {
    rc = SA_AIS_ERR_INVALID_PARAM;
    goto EXIT;
  }

  LOCK_RDLOCK(&ntfClients_lock);
  client = safc_array_get(&ntfClients, (int)ntfHandle);
  LOCK_UNLOCK(&ntfClients_lock);
  if (client == NULL) {
    rc = SA_AIS_ERR_BAD_HANDLE;
    goto EXIT;
  }

  notification = calloc(1, sizeof(*notification));
  if (notification == NULL) {
    rc = SA_AIS_ERR_NO_MEMORY;
    goto EXIT;
  }

  notification->client = client;
  notification->notificationType = SA_NTF_TYPE_SECURITY_ALARM;

  allocNtfVariableData(&notification->variableData, variableDataSize);

  rc = allocSaNtfSecurityAlarmNotification(securityAlarmNotification,
					   numCorrelatedNotifications,
					   lengthAdditionalText,
					   numAdditionalInfo);
  if (rc != SA_AIS_OK) {
    free(notification);
    goto EXIT;
  }

  LOCK_WRLOCK(&client->notifications_lock);
  client->notifications = safc_slist_prepend(client->notifications,
					     notification);
  LOCK_UNLOCK(&client->notifications_lock);

  LOCK_WRLOCK(&ntfNotifications_lock);
  securityAlarmNotification->notificationHandle =
    safc_array_insert(&ntfNotifications, (void*)notification);
  LOCK_UNLOCK(&ntfNotifications_lock);

  notification->notificationHandle = securityAlarmNotification->notificationHandle;
  notification->notification.securityAlarmNotification = securityAlarmNotification;

 EXIT:
  TRACE_LEAVE();
  return rc;
}

/* 3.14.6 saNtfPtrValAllocate */
SaAisErrorT
saNtfPtrValAllocate(SaNtfNotificationHandleT notificationHandle,
		    SaUint16T dataSize,
		    void **dataPtr,
		    SaNtfValueT *value)
{
  SaAisErrorT rc = SA_AIS_OK;
  SafcNtfNotificationT *notification;

  TRACE_ENTER();

  if (dataPtr == NULL || value == NULL) {
    rc = SA_AIS_ERR_INVALID_PARAM;
    goto EXIT;
  }

  LOCK_RDLOCK(&ntfNotifications_lock);
  notification = safc_array_get(&ntfNotifications, (int)notificationHandle);
  LOCK_UNLOCK(&ntfNotifications_lock);
  if (notification == NULL) {
    rc = SA_AIS_ERR_BAD_HANDLE;
    goto EXIT;
  }

  rc = alloc_ntf_ptr_val(&notification->variableData, value, dataSize, dataPtr);

 EXIT:
  TRACE_LEAVE();
  return rc;
}

/* 3.14.7 saNtfArrayValAllocate */
SaAisErrorT
saNtfArrayValAllocate(SaNtfNotificationHandleT notificationHandle,
		      SaUint16T numElements,
		      SaUint16T elementSize,
		      void **arrayPtr,
		      SaNtfValueT *value)
{
  SaAisErrorT rc = SA_AIS_OK;
  SafcNtfNotificationT *notification;

  TRACE_ENTER();

  if (arrayPtr == NULL || value == NULL) {
    rc = SA_AIS_ERR_INVALID_PARAM;
    goto EXIT;
  }

  LOCK_RDLOCK(&ntfNotifications_lock);
  notification = safc_array_get(&ntfNotifications, (int)notificationHandle);
  LOCK_UNLOCK(&ntfNotifications_lock);
  if (notification == NULL) {
    rc = SA_AIS_ERR_BAD_HANDLE;
    goto EXIT;
  }

  rc = alloc_ntf_array_val(&notification->variableData, value, numElements,
			   elementSize, arrayPtr);

 EXIT:
  TRACE_LEAVE();
  return rc;
}

/* 3.14.8 saNtfNotificationSend */
SaAisErrorT
saNtfNotificationSend(SaNtfNotificationHandleT notificationHandle)
{
  SaAisErrorT rc = SA_AIS_OK;
  SaAisErrorT recv_ret = SA_AIS_OK;
  SafcNtfNotificationT *notification;
  SafcNtfClientT *client;
  safc_buf_t msgbuf;
  SafsNtfNotificationSend not_send_msg = SAFS_NTF_NOTIFICATION_SEND__INIT;
  SafsNtfMessage msg = SAFS_NTF_MESSAGE__INIT;
  SafsNtfNotificationSendRet *ret_msg;
  SafsNtfNotification not_msg = SAFS_NTF_NOTIFICATION__INIT;

  TRACE_ENTER();

  LOCK_RDLOCK(&ntfNotifications_lock);
  notification = safc_array_get(&ntfNotifications, (int)notificationHandle);
  LOCK_UNLOCK(&ntfNotifications_lock);
  if (notification == NULL) {
    rc = SA_AIS_ERR_BAD_HANDLE;
    goto EXIT;
  }

  client = notification->client;

  /* Coding of message */
  msg.notificationsend = &not_send_msg;
  not_send_msg.handle = client->internalHandle;
  not_send_msg.notification = &not_msg;

  switch (notification->notificationType) {
  case SA_NTF_TYPE_OBJECT_CREATE_DELETE:
    rc = allocSafsNtfObjectCreateDeleteNotification(&not_msg.objectcreatedeletenotification,
						    notification->notification.objectCreateDeleteNotification,
						    &notification->variableData);
    break;
  case SA_NTF_TYPE_ATTRIBUTE_CHANGE:
    rc = allocSafsNtfAttributeChangeNotification(&not_msg.attributechangenotification,
						 notification->notification.attributeChangeNotification,
						 &notification->variableData);
    break;
  case SA_NTF_TYPE_STATE_CHANGE:
    rc = allocSafsNtfStateChangeNotification(&not_msg.statechangenotification,
					     notification->notification.stateChangeNotification,
					     &notification->variableData);
    break;
  case SA_NTF_TYPE_ALARM:
    rc = allocSafsNtfAlarmNotification(&not_msg.alarmnotification,
    				       notification->notification.alarmNotification,
    				       &notification->variableData);
    break;
  case SA_NTF_TYPE_SECURITY_ALARM:
    rc = allocSafsNtfSecurityAlarmNotification(&not_msg.securityalarmnotification,
					       notification->notification.securityAlarmNotification,
					       &notification->variableData);
    break;
  case SA_NTF_TYPE_MISCELLANEOUS:
    break;
  }

  if (rc != SA_AIS_OK)
    goto EXIT;

  msgbuf.size = safs_ntf_message__get_packed_size(&msg);
  msgbuf.buf = malloc(msgbuf.size);
  safs_ntf_message__pack(&msg, (uint8_t *) msgbuf.buf);

  switch (notification->notificationType) {
  case SA_NTF_TYPE_OBJECT_CREATE_DELETE:
    freeSafsNtfObjectCreateDeleteNotification(not_msg.objectcreatedeletenotification);
    break;
  case SA_NTF_TYPE_ATTRIBUTE_CHANGE:
    freeSafsNtfAttributeChangeNotification(not_msg.attributechangenotification);
    break;
  case SA_NTF_TYPE_STATE_CHANGE:
    freeSafsNtfStateChangeNotification(not_msg.statechangenotification);
    break;
  case SA_NTF_TYPE_ALARM:
    freeSafsNtfAlarmNotification(not_msg.alarmnotification);
    break;
  case SA_NTF_TYPE_SECURITY_ALARM:
    freeSafsNtfSecurityAlarmNotification(not_msg.securityalarmnotification);
    break;
  case SA_NTF_TYPE_MISCELLANEOUS:
    break;
  }

  /* Send message & wait for answer*/
  MUTEX_LOCK(&(client->mutex));
  recv_ret = safc_send_recv(client->sockfd, &msgbuf);
  MUTEX_UNLOCK(&(client->mutex));

  if(recv_ret == SA_AIS_ERR_MESSAGE_ERROR)
  {
     TRACE1(("SA_AIS_ERR_UNAVAILABLE: Couldn't send/read from socket\n"));
     rc = SA_AIS_ERR_UNAVAILABLE; // Perhaps another error code;
     goto EXIT;
  }

  /* Decoding of answer */
  ret_msg = safs_ntf_notification_send_ret__unpack(NULL,
						   msgbuf.size,
						   (uint8_t *) msgbuf.buf);
  free(msgbuf.buf);

  if (ret_msg == NULL) {
    TRACE1(("error unpacking incoming message\n"));
    rc = SA_AIS_ERR_UNAVAILABLE; // Perhaps another error code;
    goto EXIT;
  }

  rc = ret_msg->returnval;

  if (ret_msg->has_identifier)
    {
      switch (notification->notificationType) {
      case SA_NTF_TYPE_OBJECT_CREATE_DELETE:
	*(notification->notification.objectCreateDeleteNotification->notificationHeader.notificationId) = ret_msg->identifier;
	break;
      case SA_NTF_TYPE_ATTRIBUTE_CHANGE:
	*(notification->notification.attributeChangeNotification->notificationHeader.notificationId) = ret_msg->identifier;
	break;
      case SA_NTF_TYPE_STATE_CHANGE:
	*(notification->notification.stateChangeNotification->notificationHeader.notificationId) = ret_msg->identifier;
	break;
      case SA_NTF_TYPE_ALARM:
	*(notification->notification.alarmNotification->notificationHeader.notificationId) = ret_msg->identifier;
	break;
      case SA_NTF_TYPE_SECURITY_ALARM:
	*(notification->notification.securityAlarmNotification->notificationHeader.notificationId) = ret_msg->identifier;
	break;
      case SA_NTF_TYPE_MISCELLANEOUS:
	break;
      }
    }

  safs_ntf_notification_send_ret__free_unpacked(ret_msg, NULL);

 EXIT:
  TRACE_LEAVE();
  return rc;
}

/* 3.14.9 saNtfNotificationFree */
SaAisErrorT
saNtfNotificationFree(SaNtfNotificationHandleT notificationHandle)
{
  SaAisErrorT rc = SA_AIS_OK;
  SafcNtfNotificationT *notification;
  SafcNtfClientT *client;

  TRACE_ENTER();

  LOCK_RDLOCK(&ntfNotifications_lock);
  notification = safc_array_get(&ntfNotifications, (int)notificationHandle);
  LOCK_UNLOCK(&ntfNotifications_lock);
  if (notification == NULL) {
    rc = SA_AIS_ERR_BAD_HANDLE;
    goto EXIT;
  }

  client = notification->client;

  LOCK_WRLOCK(&client->notifications_lock);
  client->notifications = safc_slist_remove(client->notifications,
					    notification);
  LOCK_UNLOCK(&client->notifications_lock);

  LOCK_WRLOCK(&ntfNotifications_lock);
  safc_array_remove(&ntfNotifications, (int)notificationHandle);
  LOCK_UNLOCK(&ntfNotifications_lock);

  switch (notification->notificationType) {
  case SA_NTF_TYPE_OBJECT_CREATE_DELETE:
    freeSaNtfObjectCreateDeleteNotification(notification->notification.objectCreateDeleteNotification);
    break;
  case SA_NTF_TYPE_ATTRIBUTE_CHANGE:
    freeSaNtfAttributeChangeNotification(notification->notification.attributeChangeNotification);
    break;
  case SA_NTF_TYPE_STATE_CHANGE:
    freeSaNtfStateChangeNotification(notification->notification.stateChangeNotification);
    break;
  case SA_NTF_TYPE_ALARM:
    freeSaNtfAlarmNotification(notification->notification.alarmNotification);
    break;
  case SA_NTF_TYPE_SECURITY_ALARM:
    freeSaNtfSecurityAlarmNotification(notification->notification.securityAlarmNotification);
    break;
  case SA_NTF_TYPE_MISCELLANEOUS:
    break;
  }

  freeNtfVariableData(&notification->variableData);
  free(notification);

 EXIT:
  TRACE_LEAVE();
  return rc;
}

/* 3.15.2.1 saNtfLocalizedMessageGet */
SaAisErrorT
saNtfLocalizedMessageGet(SaNtfNotificationHandleT notificationHandle,
			 SaStringT *message)
{
  SaAisErrorT rc = SA_AIS_ERR_NOT_SUPPORTED;
  /* Silence the compiler */
  (void) notificationHandle;
  (void) message;

  TRACE_ENTER();
  TRACE_LEAVE();
  return rc;
}

/* 3.15.2.2 saNtfLocalizedMessageFree */
SaAisErrorT
saNtfLocalizedMessageFree(SaStringT message)
{
  SaAisErrorT rc = SA_AIS_ERR_NOT_SUPPORTED;
  /* Silence the compiler */
  (void) message;

  TRACE_ENTER();
  TRACE_LEAVE();
  return rc;
}

/* 3.15.2.3 saNtfPtrValGet */
SaAisErrorT
saNtfPtrValGet(SaNtfNotificationHandleT notificationHandle,
	       const SaNtfValueT *value,
	       void **dataPtr,
	       SaUint16T *dataSize)
{
  SaAisErrorT rc = SA_AIS_OK;
  SafcNtfNotificationT *notification;

  TRACE_ENTER();

  if (dataPtr == NULL || value == NULL || dataSize == NULL ||
      value->ptrVal.dataSize == 0) {
    rc = SA_AIS_ERR_INVALID_PARAM;
    goto EXIT;
  }

  LOCK_RDLOCK(&ntfNotifications_lock);
  notification = safc_array_get(&ntfNotifications, (int)notificationHandle);
  LOCK_UNLOCK(&ntfNotifications_lock);
  if (notification == NULL) {
    rc = SA_AIS_ERR_BAD_HANDLE;
    goto EXIT;
  }

  rc = get_ntf_ptr_val(&notification->variableData, value, dataSize, dataPtr);

 EXIT:
  TRACE_LEAVE();
  return rc;
}

/* 3.15.2.4 saNtfArrayValGet */
SaAisErrorT
saNtfArrayValGet(SaNtfNotificationHandleT notificationHandle,
		 const SaNtfValueT *value,
		 void **arrayPtr,
		 SaUint16T *numElements,
		 SaUint16T *elementSize)
{
  SaAisErrorT rc = SA_AIS_OK;
  SafcNtfNotificationT *notification;

  TRACE_ENTER();

  if (arrayPtr == NULL || value == NULL || numElements == NULL ||
      elementSize == NULL || elementSize == NULL ||
      value->arrayVal.elementSize == 0 || value->arrayVal.numElements == 0) {
    rc = SA_AIS_ERR_INVALID_PARAM;
    goto EXIT;
  }

  LOCK_RDLOCK(&ntfNotifications_lock);
  notification = safc_array_get(&ntfNotifications, (int)notificationHandle);
  LOCK_UNLOCK(&ntfNotifications_lock);
  if (notification == NULL) {
    rc = SA_AIS_ERR_BAD_HANDLE;
    goto EXIT;
  }

  rc = get_ntf_array_val(&notification->variableData, value,numElements,
			 elementSize, arrayPtr);

 EXIT:
  TRACE_LEAVE();
  return rc;
}

/* 3.15.2.5 saNtfObjectCreateDeleteNotificationFilterAllocate */
SaAisErrorT
saNtfObjectCreateDeleteNotificationFilterAllocate(SaNtfHandleT ntfHandle,
						  SaNtfObjectCreateDeleteNotificationFilterT *notificationFilter,
						  SaUint16T numEventTypes,
						  SaUint16T numNotificationObjects,
						  SaUint16T numNotifyingObjects,
						  SaUint16T numNotificationClassIds,
						  SaUint16T numSourceIndicators)
{
  SaAisErrorT rc = SA_AIS_OK;
  SafcNtfClientT *client;

  TRACE_ENTER();

  if (notificationFilter == NULL) {
    rc = SA_AIS_ERR_INVALID_PARAM;
    goto EXIT;
  }

  LOCK_RDLOCK(&ntfClients_lock);
  client = safc_array_get(&ntfClients, (int)ntfHandle);
  LOCK_UNLOCK(&ntfClients_lock);
  if (client == NULL) {
    rc = SA_AIS_ERR_BAD_HANDLE;
    goto EXIT;
  }

 EXIT:
  TRACE_LEAVE();
  return rc;
}

/* 3.15.2.6 saNtfAttributeChangeNotificationFilterAllocate */
SaAisErrorT
saNtfAttributeChangeNotificationFilterAllocate(SaNtfHandleT ntfHandle,
					       SaNtfAttributeChangeNotificationFilterT *notificationFilter,
					       SaUint16T numEventTypes,
					       SaUint16T numNotificationObjects,
					       SaUint16T numNotifyingObjects,
					       SaUint16T numNotificationClassIds,
					       SaUint16T numSourceIndicators)
{
  SaAisErrorT rc = SA_AIS_OK;
  SafcNtfClientT *client;

  TRACE_ENTER();

  if (notificationFilter == NULL) {
    rc = SA_AIS_ERR_INVALID_PARAM;
    goto EXIT;
  }

  LOCK_RDLOCK(&ntfClients_lock);
  client = safc_array_get(&ntfClients, (int)ntfHandle);
  LOCK_UNLOCK(&ntfClients_lock);
  if (client == NULL) {
    rc = SA_AIS_ERR_BAD_HANDLE;
    goto EXIT;
  }

 EXIT:
  TRACE_LEAVE();
  return rc;
}

/* 3.15.2.7 saNtfStateChangeNotificationFilterAllocate */
SaAisErrorT
saNtfStateChangeNotificationFilterAllocate(SaNtfHandleT ntfHandle,
					   SaNtfStateChangeNotificationFilterT *notificationFilter,
					   SaUint16T numEventTypes,
					   SaUint16T numNotificationObjects,
					   SaUint16T numNotifyingObjects,
					   SaUint16T numNotificationClassIds,
					   SaUint16T numSourceIndicators,
					   SaUint16T numChangedStates)
{
  SaAisErrorT rc = SA_AIS_OK;
  SafcNtfClientT *client;

  TRACE_ENTER();

  if (notificationFilter == NULL) {
    rc = SA_AIS_ERR_INVALID_PARAM;
    goto EXIT;
  }

  LOCK_RDLOCK(&ntfClients_lock);
  client = safc_array_get(&ntfClients, (int)ntfHandle);
  LOCK_UNLOCK(&ntfClients_lock);
  if (client == NULL) {
    rc = SA_AIS_ERR_BAD_HANDLE;
    goto EXIT;
  }

 EXIT:
  TRACE_LEAVE();
  return rc;
}

/* 3.15.2.8 saNtfAlarmNotificationFilterAllocate */
SaAisErrorT
saNtfAlarmNotificationFilterAllocate(SaNtfHandleT ntfHandle,
				     SaNtfAlarmNotificationFilterT *notificationFilter,
				     SaUint16T numEventTypes,
				     SaUint16T numNotificationObjects,
				     SaUint16T numNotifyingObjects,
				     SaUint16T numNotificationClassIds,
				     SaUint16T numProbableCauses,
				     SaUint16T numPerceivedSeverities,
				     SaUint16T numTrends)
{
  SaAisErrorT rc = SA_AIS_OK;
  SafcNtfClientT *client;

  TRACE_ENTER();

  if (notificationFilter == NULL) {
    rc = SA_AIS_ERR_INVALID_PARAM;
    goto EXIT;
  }

  LOCK_RDLOCK(&ntfClients_lock);
  client = safc_array_get(&ntfClients, (int)ntfHandle);
  LOCK_UNLOCK(&ntfClients_lock);
  if (client == NULL){
    rc = SA_AIS_ERR_BAD_HANDLE;
    goto EXIT;
  }

 EXIT:
  TRACE_LEAVE();
  return rc;
}

/* 3.15.2.9 saNtfSecurityAlarmNotificationFilterAllocate */
SaAisErrorT
saNtfSecurityAlarmNotificationFilterAllocate(SaNtfHandleT ntfHandle,
					     SaNtfSecurityAlarmNotificationFilterT *notificationFilter,
					     SaUint16T numEventTypes,
					     SaUint16T numNotificationObjects,
					     SaUint16T numNotifyingObjects,
					     SaUint16T numNotificationClassIds,
					     SaUint16T numProbableCauses,
					     SaUint16T numSeverities,
					     SaUint16T numSecurityAlarmDetectors,
					     SaUint16T numServiceUsers,
					     SaUint16T numServiceProviders)
{
  SaAisErrorT rc = SA_AIS_OK;
  SafcNtfClientT *client;

  TRACE_ENTER();

  if (notificationFilter == NULL) {
    rc = SA_AIS_ERR_INVALID_PARAM;
    goto EXIT;
  }

  LOCK_RDLOCK(&ntfClients_lock);
  client = safc_array_get(&ntfClients, (int)ntfHandle);
  LOCK_UNLOCK(&ntfClients_lock);
  if (client == NULL) {
    rc = SA_AIS_ERR_BAD_HANDLE;
    goto EXIT;
  }

 EXIT:
  TRACE_LEAVE();
  return rc;
}

/* 3.15.2.10 saNtfNotificationFilterFree */
SaAisErrorT
saNtfNotificationFilterFree(SaNtfNotificationFilterHandleT notificationFilterHandle)
{
  SaAisErrorT rc = SA_AIS_OK;

  TRACE_ENTER();
  TRACE_LEAVE();
  return rc;
}

/* 3.15.3.1 saNtfNotificationSubscribe */
SaAisErrorT
saNtfNotificationSubscribe(const SaNtfNotificationTypeFilterHandlesT *notificationFilterHandles,
			   SaNtfSubscriptionIdT subscriptionId)
{
  SaAisErrorT rc = SA_AIS_OK;

  TRACE_ENTER();
  TRACE_LEAVE();
  return rc;
}

/* 3.15.3.2 saNtfNotificationUnsubscribe */
SaAisErrorT
saNtfNotificationUnsubscribe(SaNtfSubscriptionIdT subscriptionId)
{
  SaAisErrorT rc = SA_AIS_OK;

  TRACE_ENTER();
  TRACE_LEAVE();
  return rc;
}

/* 3.15.4.1 saNtfNotificationReadInitialize */
SaAisErrorT
saNtfNotificationReadInitialize(SaNtfSearchCriteriaT searchCriteria,
				const SaNtfNotificationTypeFilterHandlesT *notificationFilterHandles,
				SaNtfReadHandleT *readHandle)
{
  SaAisErrorT rc = SA_AIS_OK;

  TRACE_ENTER();

  if (notificationFilterHandles == NULL || readHandle == NULL ||
      searchCriteria.searchMode < SA_NTF_SEARCH_BEFORE_OR_AT_TIME ||
      searchCriteria.searchMode > SA_NTF_SEARCH_ONLY_FILTER) {
    rc = SA_AIS_ERR_INVALID_PARAM;
    goto EXIT;
  }

 EXIT:
  TRACE_LEAVE();
  return rc;
}

/* 3.15.4.2 saNtfNotificationReadNext */
SaAisErrorT
saNtfNotificationReadNext(SaNtfReadHandleT readHandle,
			  SaNtfSearchDirectionT searchDirection,
			  SaNtfNotificationsT *notification)
{
  SaAisErrorT rc = SA_AIS_OK;

  TRACE_ENTER();

  if (notification == NULL || searchDirection < SA_NTF_SEARCH_OLDER ||
      searchDirection > SA_NTF_SEARCH_YOUNGER) {
    rc = SA_AIS_ERR_INVALID_PARAM;
    goto EXIT;
  }

 EXIT:
  TRACE_LEAVE();
  return rc;
}

/* 3.15.4.3 saNtfNotificationReadFinalize */
SaAisErrorT
saNtfNotificationReadFinalize(SaNtfReadHandleT readHandle)
{
  SaAisErrorT rc = SA_AIS_OK;

  TRACE_ENTER();
  TRACE_LEAVE();
  return rc;
}
