/*
 * %EricssonCopyright%
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2013-2017. All Rights Reserved.
 *
 * The program may be used and/or copied only with the written permission from
 * Ericsson AB, or in accordance with the terms and conditions stipulated in
 * the agreement/contract under which the program has been supplied.
 *
 * %CopyrightEnd%
 *
 * ----------------------------------------------------------------------
 *  Purpose : SAF LOG Agent Library
 * ----------------------------------------------------------------------
 *
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <pthread.h>

#include <saAis.h>
#include <saLog.h>

#include "safc_trace.h"
#include "safc_sock_lib.h"
#include "safc_array.h"
#include "safc_list.h"
#include "safc_ais_common.h"
#include "sa_ais.pb-c.h"
#include "log.pb-c.h"

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

#define LOG_RELEASE_CODE 'A'
#define LOG_MAJOR_VERSION 2
#define LOG_MINOR_VERSION 1

typedef struct {
  SaLogHandleT internalHandle;
  int hasCallbacks;
  SaLogCallbacksT callbacks;
  SafcSListT streams;
  pthread_rwlock_t streams_lock;
  int sockfd;
  int cb_sockfd;
  pthread_mutex_t mutex;
} SafcLogClientT;

typedef struct {
  SafcLogClientT* client;
  SaLogStreamHandleT logStreamHandle;
  SaLogStreamHandleT internalStreamHandle;
  SaLogHeaderTypeT logHdrType;
} SafcLogStreamT;

pthread_rwlock_t logClients_lock = PTHREAD_RWLOCK_INITIALIZER;
pthread_rwlock_t logStreams_lock = PTHREAD_RWLOCK_INITIALIZER;
safc_array logClients = SAFC_ARRAY_INITIALIZER;
safc_array logStreams = SAFC_ARRAY_INITIALIZER;

static void dispatchCallback(SaLogCallbacks callbacks_msg,
			     SaLogCallbacksT callbacks);

static void logStreamOpenCb(SaLogStreamOpenCallback data,
			    SaLogStreamOpenCallbackT *cb);

static void logWriteLogCb(SaLogWriteLogCallback data,
			  SaLogWriteLogCallbackT *cb);

static void logFilterSetCb(SaLogFilterSetCallback data,
			   SaLogFilterSetCallbackT *cb);

static SaLogSeverityFlagsT severityFlags(SafsLogSeverityFlags *logseverity);

static SaAisErrorT
allocSafsLogStreamOpen2(SafsLogStreamOpen2 **pptr,
			SaLogHandleT logHandle,
			const SaNameT *logStreamName,
			const SaLogFileCreateAttributesT_2 *logFileCreateAttributes,
			SaLogStreamOpenFlagsT logStreamOpenFlags,
			SaTimeT timeout,
			SaLogHeaderTypeT *logHdrType);
static void freeSafsLogStreamOpen2(SafsLogStreamOpen2* ptr);

static SaAisErrorT allocSafsLogFileCreateAttributes2(SafsLogFileCreateAttributes2 **pptr,
						     const SaLogFileCreateAttributesT_2 *logFileCreateAttributes,
						     const SaNameT *logStreamName,
						     SaLogStreamOpenFlagsT logStreamOpenFlags,
						     SaLogHeaderTypeT *logHdrType);
static void freeSafsLogFileCreateAttributes2(SafsLogFileCreateAttributes2* ptr);

static SaAisErrorT allocSafsLogWriteLogAsync(SafsLogWriteLogAsync **pptr,
					     SaLogStreamHandleT logstreamhandle,
					     SaInvocationT invocation,
					     SaLogAckFlagsT ackFlags,
					     const SaLogRecordT *logRecord,
					     SaLogHeaderTypeT logHdrType,
					     SaLogWriteLogCallbackT saLogWriteLogCallback);


static SaAisErrorT allocSafsLogDeleteFiles(SafsLogDeleteFiles **pptr,
					   SaLogHandleT handle,
					   SaStringT logFileName,
					   SaStringT logFilePathName);


static void freeSafsLogDeleteFiles(SafsLogDeleteFiles *ptr);

static void freeSafsLogWriteLogAsync(SafsLogWriteLogAsync *ptr);

static SaAisErrorT allocSafsLogRecord(SafsLogRecord **pptr,
				      const SaLogRecordT *logRecord,
				      SaLogHeaderTypeT logHdrType);
static void freeSafsLogRecord(SafsLogRecord *ptr);

static SaAisErrorT allocSafsLogHeader(SafsLogHeader **pptr,
				      SafsLogHeaderType loghdrtype,
				      const SaLogHeaderT *logHeader);
static void freeSafsLogHeader(SafsLogHeader *ptr);

static SaAisErrorT allocSafsLogNtfLogHeader(SafsLogNtfLogHeader **pptr,
					    const SaLogNtfLogHeaderT *ntfHdr);
static void freeSafsLogNtfLogHeader(SafsLogNtfLogHeader *ptr);

static SaAisErrorT allocSafsLogGenericLogHeader(SafsLogGenericLogHeader **pptrr,
						const SaLogGenericLogHeaderT *genericHdr);
static void freeSafsLogGenericLogHeader(SafsLogGenericLogHeader *ptr);

static SaAisErrorT allocSafsNtfClassid(SafsNtfClassId **pptr,
				       SaNtfClassIdT* classId);
static void freeSafsNtfClassid(SafsNtfClassId *ptr);

static SaAisErrorT allocBufferFromSaNameT(char** pptr, const SaNameT *saName);
static SaAisErrorT allocBufferFromSaLogBufferT(char** pptr,
					       const SaLogBufferT *logBuffer);

static SaAisErrorT allocSaLogLimitGet(SafsLogLimitGet **pptr,
				      SaLogHandleT handle,
				      SaLogLimitIdT limitId);

static void freeSaLogLimitGet(SafsLogLimitGet *ptr);

/* safc_log_version_validate */
static SaAisErrorT safc_log_version_validate(SaVersionT *version)
{
   SaAisErrorT rc = SA_AIS_OK;

   if ((version->releaseCode != LOG_RELEASE_CODE) || 
       (version->majorVersion != LOG_MAJOR_VERSION)) {
      TRACE1(("ERR_VERSION: Version %c %u not supported\n",
	      version->releaseCode,
	      version->majorVersion));
      rc = SA_AIS_ERR_VERSION;
   }

   version->releaseCode = LOG_RELEASE_CODE;
   version->majorVersion = LOG_MAJOR_VERSION;
   version->minorVersion = LOG_MINOR_VERSION;
   return rc;
}

/* 3.5.1 saLogInitialize */
SaAisErrorT
saLogInitialize(SaLogHandleT *logHandle,
		const SaLogCallbacksT *callbacks,
		SaVersionT *version)
{
  SaAisErrorT rc = SA_AIS_OK;
  SaAisErrorT recv_ret = SA_AIS_OK;
  SafcLogClientT *client;
  int port;
  char* hostname = "localhost";
  safc_buf_t msgbuf;

  SafsLogInitialize init_msg =  SAFS_LOG_INITIALIZE__INIT;
  SafsLogCallbacks cb_msg =  SAFS_LOG_CALLBACKS__INIT;
  SafsVersion version_msg =  SAFS_VERSION__INIT;
  SafsLogMessage msg = SAFS_LOG_MESSAGE__INIT;
  SafsLogInitializeRet *ret_msg;
  SafsLogCallbacksInitialize cb_init_msg = SAFS_LOG_CALLBACKS_INITIALIZE__INIT;
  SafsLogCallbacksInitializeRet *cb_init_ret_msg;
  SafsVersion *returned_version;
  
  TRACE_ENTER();

  if ((logHandle == NULL) || (version == NULL)) {
    rc = SA_AIS_ERR_INVALID_PARAM;
    goto EXIT;
  }

   /* Validations : Version */
   rc = safc_log_version_validate(version);
   if (rc != SA_AIS_OK) {
      TRACE1(("ERR_VERSION: Version validation failed\n"));
      goto EXIT;
      //      return rc;
   }

  /* Alloc the client info data structure, handle for now */
  client = calloc(1, sizeof(SafcLogClientT));
  if (client == NULL) {
    rc = SA_AIS_ERR_NO_MEMORY;
    goto EXIT;
  }

  /* Store the callback functions, if set */
  if (callbacks) {
    client->hasCallbacks = 1;
    client->callbacks = *callbacks;
  } else {
    client->hasCallbacks = 0;
  }

  client->streams = NULL;
  LOCK_INIT(&client->streams_lock);

  port = safc_protobuf_port("SAFC_LOG_PORT", 10004);
  client->sockfd = safc_connect(hostname, port);
  if (client->sockfd < 0) {
    logHandle = NULL;
    free(client);
    rc = SA_AIS_ERR_UNAVAILABLE;
    goto EXIT;
  }

  MUTEX_INIT(&client->mutex);

  version_msg.releasecode = version->releaseCode;
  version_msg.majorversion = version->majorVersion;
  version_msg.minorversion = version->minorVersion;

  if (callbacks) {
    if (callbacks->saLogFilterSetCallback)
      cb_msg.salogfiltersetcallback = 1;

    if (callbacks->saLogStreamOpenCallback)
      cb_msg.salogstreamopencallback = 1;

    if (callbacks->saLogWriteLogCallback)
      cb_msg.salogwritelogcallback = 1;
  }

  init_msg.callbacks = &cb_msg;
  init_msg.version = &version_msg;

  msg.initialize = &init_msg;

  msgbuf.size = safs_log_message__get_packed_size(&msg);
  msgbuf.buf = malloc(msgbuf.size);
  safs_log_message__pack(&msg, (uint8_t *)msgbuf.buf);

  MUTEX_LOCK(&(client->mutex));
  recv_ret = safc_send_recv(client->sockfd, &msgbuf);
  MUTEX_UNLOCK(&(client->mutex));

  if (recv_ret == SA_AIS_ERR_MESSAGE_ERROR) {
    TRACE1(("SA_AIS_ERR_UNAVAILABLE: Couldn't send/read from socket\n"));
    free(msgbuf.buf);
    rc = SA_AIS_ERR_UNAVAILABLE; // Perhaps another error code;
    goto ERROR;
  }

  /* Decoding of answer */
  ret_msg = safs_log_initialize_ret__unpack(NULL,
                                            msgbuf.size,
                                            (uint8_t *)msgbuf.buf);
  free(msgbuf.buf);

  if (ret_msg == NULL) {
    TRACE1(("error unpacking incoming message\n"));
    rc = SA_AIS_ERR_UNAVAILABLE; // Perhaps another error code;
    goto ERROR;
  }

  client->internalHandle = ret_msg->handle;
  returned_version = ret_msg->version;

  version->releaseCode = returned_version->releasecode;
  version->majorVersion = returned_version->majorversion;
  version->minorVersion = returned_version->minorversion;

  rc = ret_msg->returnval;
  safs_log_initialize_ret__free_unpacked(ret_msg, NULL);

  if (rc != SA_AIS_OK)
    goto ERROR;

  if (client->hasCallbacks) {
    client->cb_sockfd = safc_connect(hostname, port);
    if (client->cb_sockfd < 0) {
      TRACE1(("safc_connect failed for cb socket\n"));
      rc = SA_AIS_ERR_UNAVAILABLE;
      goto ERROR;
    }

    /* Coding of callback init message */

    cb_init_msg.handle = client->internalHandle;

    msg.initialize = NULL;
    msg.callbacksinitialize = &cb_init_msg;

    msgbuf.size = safs_log_message__get_packed_size(&msg);
    msgbuf.buf = malloc(msgbuf.size);
    safs_log_message__pack(&msg, (uint8_t *)msgbuf.buf);

    /* Send message & wait for answer*/
    MUTEX_LOCK(&(client->mutex));
    recv_ret = safc_send_recv(client->cb_sockfd, &msgbuf);
    MUTEX_UNLOCK(&(client->mutex));

    if (recv_ret == SA_AIS_ERR_MESSAGE_ERROR)
      {
	rc = SA_AIS_ERR_UNAVAILABLE; // Perhaps another error code;
	free(msgbuf.buf);
	goto CBERROR;
      }

    /* Decoding of answer */
    cb_init_ret_msg =
      safs_log_callbacks_initialize_ret__unpack(NULL,
						msgbuf.size,
						(uint8_t *)msgbuf.buf);
    free(msgbuf.buf);

    if (cb_init_ret_msg == NULL)
      {
	TRACE1(("error unpacking incoming message\n"));
	rc = SA_AIS_ERR_UNAVAILABLE; // Perhaps another error code;
	goto CBERROR;
      }

    rc = cb_init_ret_msg->returnval;
    safs_log_callbacks_initialize_ret__free_unpacked(cb_init_ret_msg, NULL);

    if (rc != SA_AIS_OK)
      goto CBERROR;
  }

  LOCK_WRLOCK(&logClients_lock);
  *logHandle = safc_array_insert(&logClients, (void*)client);
  LOCK_UNLOCK(&logClients_lock);

  goto EXIT;

 CBERROR:
  safc_disconnect(client->cb_sockfd);

 ERROR:
  safc_disconnect(client->sockfd);
  logHandle = NULL;
  free(client);

 EXIT:
  TRACE_LEAVE();
  return rc;
}

/* 3.5.2 saLogSelectionObjectGet */
SaAisErrorT
saLogSelectionObjectGet(SaLogHandleT logHandle,
			SaSelectionObjectT *selectionObject)
{
  SaAisErrorT rc = SA_AIS_OK;
  SafcLogClientT *client;

  TRACE_ENTER();

  if (selectionObject == NULL) {
    rc = SA_AIS_ERR_INVALID_PARAM;
    goto EXIT;
  }

  LOCK_RDLOCK(&logClients_lock);
  client = safc_array_get(&logClients, (int)logHandle);
  LOCK_UNLOCK(&logClients_lock);
  if (client == NULL) {
    rc = SA_AIS_ERR_BAD_HANDLE;
    goto EXIT;
  }

  *selectionObject = (SaSelectionObjectT)client->cb_sockfd;

 EXIT:
  TRACE_LEAVE();
  return rc;
}

/* 3.5.3 saLogDispatch */
SaAisErrorT
saLogDispatch(SaLogHandleT logHandle, SaDispatchFlagsT dispatchFlags)
{
  SaAisErrorT rc = SA_AIS_OK;
  safc_buf_t msgbuf;
  SaLogCallbacks *callbacks_msg;
  SafcLogClientT *client;

  TRACE_ENTER();

  if (dispatchFlags != SA_DISPATCH_ONE &&
      dispatchFlags != SA_DISPATCH_ALL &&
      dispatchFlags != SA_DISPATCH_BLOCKING) {
    rc = SA_AIS_ERR_INVALID_PARAM;
    goto EXIT;
  }

  LOCK_RDLOCK(&logClients_lock);
  client = safc_array_get(&logClients, (int)logHandle);
  LOCK_UNLOCK(&logClients_lock);
  if (client == NULL) {
    rc = SA_AIS_ERR_BAD_HANDLE;
    goto EXIT;
  }

  if (client->hasCallbacks == 0)
    return SA_AIS_ERR_BAD_HANDLE;

  msgbuf.buf = NULL;
  MUTEX_LOCK(&(client->mutex));
  rc = safc_recv(client->cb_sockfd, &msgbuf);
  MUTEX_UNLOCK(&(client->mutex));
  if (rc != SA_AIS_OK) {
    TRACE1(("saImmDispatch: failed socket receive\n"));
    return SA_AIS_ERR_UNAVAILABLE;
  }

  /* Decoding of message */
  callbacks_msg = sa_log_callbacks__unpack(NULL,
					   msgbuf.size,
					   (uint8_t *)msgbuf.buf);
  free(msgbuf.buf);

  if (callbacks_msg == NULL) {
    TRACE1(("error unpacking incoming message\n"));
    rc = SA_AIS_ERR_UNAVAILABLE; // Perhaps another error code;
    return rc;
  }

  dispatchCallback(*callbacks_msg, client->callbacks);
  sa_log_callbacks__free_unpacked(callbacks_msg, NULL);

 EXIT:
  TRACE_LEAVE();
  return rc;
}

/* 3.5.4 saLogFinalize */
SaAisErrorT
saLogFinalize(SaLogHandleT logHandle)
{
  SaAisErrorT rc = SA_AIS_OK;
  SaAisErrorT recv_ret = SA_AIS_OK;
  SafcLogClientT *client;
  SafcLogStreamT *stream;

  safc_buf_t msgbuf;
  SafsLogFinalize fin_msg =  SAFS_LOG_FINALIZE__INIT;
  SafsLogMessage msg = SAFS_LOG_MESSAGE__INIT;
  SafsLogFinalizeRet *ret_msg;

  TRACE_ENTER();

  LOCK_RDLOCK(&logClients_lock);
  client = safc_array_get(&logClients, (int)logHandle);
  LOCK_UNLOCK(&logClients_lock);
  if (client == NULL) {
    rc = SA_AIS_ERR_BAD_HANDLE;
    goto EXIT;
  }

  /* Coding of message */
  fin_msg.handle = client->internalHandle;

  msg.finalize = &fin_msg;

  msgbuf.size = safs_log_message__get_packed_size(&msg);
  msgbuf.buf = malloc(msgbuf.size);
  safs_log_message__pack(&msg, (uint8_t *)msgbuf.buf);

  /* Send message & wait for answer*/
  MUTEX_LOCK(&(client->mutex));
  recv_ret = safc_send_recv(client->sockfd, &msgbuf);
  MUTEX_UNLOCK(&(client->mutex));

  if (recv_ret == SA_AIS_ERR_MESSAGE_ERROR) {
    TRACE1(("SA_AIS_ERR_UNAVAILABLE: Couldn't send/read from socket\n"));
    rc = SA_AIS_ERR_UNAVAILABLE; // Perhaps another error code;
    goto EXIT;
  }

  /* Decoding of answer */
  ret_msg = safs_log_finalize_ret__unpack(NULL,
                                          msgbuf.size,
                                          (uint8_t *)msgbuf.buf);
  free(msgbuf.buf);

  if (ret_msg == NULL) {
    TRACE1(("error unpacking incoming message\n"));
    rc = SA_AIS_ERR_UNAVAILABLE; // Perhaps another error code;
    goto EXIT;
  }

  rc = ret_msg->returnval;
  safs_log_finalize_ret__free_unpacked(ret_msg, NULL);

  safc_disconnect(client->sockfd);
  if (client->hasCallbacks)
    safc_disconnect(client->cb_sockfd);

  LOCK_WRLOCK(&client->streams_lock);
  while (client->streams) {
    client->streams = safc_slist_remove_first(client->streams,
					      (void**)&stream);
    LOCK_WRLOCK(&logStreams_lock);
    safc_array_remove(&logStreams, (int)stream->logStreamHandle);
    LOCK_UNLOCK(&logStreams_lock);
    free(stream);
  }
  LOCK_UNLOCK(&client->streams_lock);

  LOCK_WRLOCK(&logClients_lock);
  safc_array_remove(&logClients, (int)logHandle);
  LOCK_UNLOCK(&logClients_lock);

  free(client);

 EXIT:
  TRACE_LEAVE();
  return rc;
}

/* 3.6.1 saLogStreamOpen_2 */
SaAisErrorT
saLogStreamOpen_2(SaLogHandleT logHandle,
		  const SaNameT *logStreamName,
		  const SaLogFileCreateAttributesT_2 *logFileCreateAttributes,
		  SaLogStreamOpenFlagsT logStreamOpenFlags,
		  SaTimeT timeout,
		  SaLogStreamHandleT *logStreamHandle)
{
  SaAisErrorT rc = SA_AIS_OK;
  SaAisErrorT recv_ret;
  SafcLogClientT *client;
  SafcLogStreamT *stream;
  safc_buf_t msgbuf;
  SafsLogStreamOpen2 *stream_open2_msg;
  SafsLogMessage msg = SAFS_LOG_MESSAGE__INIT;
  SafsLogStreamOpen2Ret *ret_msg;
  SaLogStreamHandleT internalStreamHandle;
  SaLogHeaderTypeT logHdrType = SA_LOG_GENERIC_HEADER;

  TRACE_ENTER();

  if (logStreamHandle == NULL) {
    rc = SA_AIS_ERR_INVALID_PARAM;
    goto EXIT;
  }

  LOCK_RDLOCK(&logClients_lock);
  client = safc_array_get(&logClients, (int)logHandle);
  LOCK_UNLOCK(&logClients_lock);
  if (client == NULL) {
    rc = SA_AIS_ERR_BAD_HANDLE;
    goto EXIT;
  }

  /* Coding of message */
  rc = allocSafsLogStreamOpen2(&stream_open2_msg,
			       client->internalHandle,
			       logStreamName,
			       logFileCreateAttributes,
			       logStreamOpenFlags,
			       timeout,
			       &logHdrType);
  if (rc != SA_AIS_OK)
    goto EXIT;

  msg.logstreamopen = stream_open2_msg;

  msgbuf.size = safs_log_message__get_packed_size(&msg);
  msgbuf.buf = malloc(msgbuf.size);
  safs_log_message__pack(&msg, (uint8_t *)msgbuf.buf);

  freeSafsLogStreamOpen2(stream_open2_msg);

  MUTEX_LOCK(&(client->mutex));
  recv_ret = safc_send_recv(client->sockfd, &msgbuf);
  MUTEX_UNLOCK(&(client->mutex));

  if (recv_ret == SA_AIS_ERR_MESSAGE_ERROR) {
    TRACE1(("SA_AIS_ERR_UNAVAILABLE: Couldn't send/read from socket\n"));
    free(msgbuf.buf);
    rc = SA_AIS_ERR_UNAVAILABLE; // Perhaps another error code;
    goto EXIT;
  }

  /* Decoding of answer */
  ret_msg = safs_log_stream_open_2_ret__unpack(NULL,
					       msgbuf.size,
					       (uint8_t *)msgbuf.buf);
  free(msgbuf.buf);

  if (ret_msg == NULL) {
    TRACE1(("SA_AIS_ERR_UNAVAILABLE: Couldn't unpack incoming message\n"));
    rc = SA_AIS_ERR_UNAVAILABLE;
    goto EXIT;
  }

  rc = ret_msg->returnval;
  internalStreamHandle = ret_msg->logstreamhandle;
  safs_log_stream_open_2_ret__free_unpacked(ret_msg, NULL);

  if (rc != SA_AIS_OK)
    goto EXIT;

  stream = calloc(1, sizeof(*stream));
  if (stream == NULL) {
    rc = SA_AIS_ERR_NO_MEMORY;
    goto EXIT;
  }

  stream->client = client;
  stream->internalStreamHandle = internalStreamHandle;
  stream->logHdrType = logHdrType;

  LOCK_WRLOCK(&client->streams_lock);
  client->streams = safc_slist_prepend(client->streams, stream);
  LOCK_UNLOCK(&client->streams_lock);

  LOCK_WRLOCK(&logStreams_lock);
  *logStreamHandle = safc_array_insert(&logStreams, (void*)stream);
  LOCK_UNLOCK(&logStreams_lock);
  stream->logStreamHandle = *logStreamHandle;

 EXIT:
  TRACE_LEAVE();
  return rc;
}

/* 3.6.1 saLogStreamOpenAsync_2 */
SaAisErrorT
saLogStreamOpenAsync_2(SaLogHandleT logHandle,
		       const SaNameT *logStreamName,
		       const SaLogFileCreateAttributesT_2 *logFileCreateAttributes,
		       SaLogStreamOpenFlagsT logStreamOpenFlags,
		       SaInvocationT invocation)
{
  TRACE_ENTER();

  (void)logHandle;
  (void)logStreamName;
  (void)logFileCreateAttributes;
  (void)logStreamOpenFlags;
  (void)invocation;

  TRACE_LEAVE();

  return SA_AIS_ERR_NOT_SUPPORTED;
}

/* 3.6.3 saLogWriteLog */
SaAisErrorT
saLogWriteLog(SaLogStreamHandleT logStreamHandle,
	      SaTimeT timeout,
	      const SaLogRecordT *logRecord)
{
  TRACE_ENTER();

  (void)logStreamHandle;
  (void)timeout;
  (void)logRecord;

  TRACE_LEAVE();

  return SA_AIS_ERR_NOT_SUPPORTED;
}

/* 3.6.3 saLogWriteLogAsync */
SaAisErrorT
saLogWriteLogAsync(SaLogStreamHandleT logStreamHandle,
		   SaInvocationT invocation,
		   SaLogAckFlagsT ackFlags,
		   const SaLogRecordT *logRecord)
{
  SaAisErrorT rc = SA_AIS_OK;
  SaAisErrorT recv_ret;
  SafcLogStreamT *stream;
  SafcLogClientT *client;
  safc_buf_t msgbuf;
  SafsLogWriteLogAsync *write_log_async_msg;
  SafsLogMessage msg = SAFS_LOG_MESSAGE__INIT;

  TRACE_ENTER();

  LOCK_RDLOCK(&logStreams_lock);
  stream = safc_array_get(&logStreams, (int)logStreamHandle);
  LOCK_UNLOCK(&logStreams_lock);
  if (stream == NULL) {
    rc = SA_AIS_ERR_BAD_HANDLE;
    goto EXIT;
  }
  client = stream->client;

  /* Coding of message */
  rc = allocSafsLogWriteLogAsync(&write_log_async_msg,
				 stream->internalStreamHandle,
				 invocation,
				 ackFlags,
				 logRecord,
				 stream->logHdrType,
				 client->callbacks.saLogWriteLogCallback);
  if (rc != SA_AIS_OK)
    goto EXIT;

  msg.logwritelogasync = write_log_async_msg;

  msgbuf.size = safs_log_message__get_packed_size(&msg);
  msgbuf.buf = malloc(msgbuf.size);
  safs_log_message__pack(&msg, (uint8_t *)msgbuf.buf);

  freeSafsLogWriteLogAsync(write_log_async_msg);

  MUTEX_LOCK(&(client->mutex));
  recv_ret = safc_send(client->sockfd, &msgbuf);
  MUTEX_UNLOCK(&(client->mutex));

  if (recv_ret == SA_AIS_ERR_MESSAGE_ERROR) {
    TRACE1(("SA_AIS_ERR_UNAVAILABLE: Couldn't send/read from socket\n"));
    free(msgbuf.buf);
    rc = SA_AIS_ERR_UNAVAILABLE; // Perhaps another error code;
    goto EXIT;
  }

  free(msgbuf.buf);
  goto EXIT;
  

/*    Decoding of answer  */
/*   ret_msg = safs_log_write_log_async_ret__unpack(NULL, */
/* 						 msgbuf.size, */
/* 						 (uint8_t *)msgbuf.buf); */
/*   free(msgbuf.buf); */

/*   if (ret_msg == NULL) { */
/*     TRACE1(("SA_AIS_ERR_UNAVAILABLE: Couldn't unpack incoming message\n")); */
/*     rc = SA_AIS_ERR_UNAVAILABLE; */
/*     goto EXIT; */
/*   } */

/*   rc = ret_msg->returnval; */
/*   safs_log_write_log_async_ret__free_unpacked(ret_msg, NULL); */

 EXIT:
  TRACE_LEAVE();
  return rc;
}

/* 3.6.6 saLogStreamClose */
SaAisErrorT
saLogStreamClose(SaLogStreamHandleT logStreamHandle)
{
  SaAisErrorT rc = SA_AIS_OK;
  SaAisErrorT recv_ret;
  SafcLogStreamT *stream;
  SafcLogClientT *client;
  safc_buf_t msgbuf;
  SafsLogStreamClose stream_close_msg = SAFS_LOG_STREAM_CLOSE__INIT;
  SafsLogMessage msg = SAFS_LOG_MESSAGE__INIT;
  SafsLogStreamCloseRet *ret_msg;

  TRACE_ENTER();

  LOCK_RDLOCK(&logStreams_lock);
  stream = safc_array_get(&logStreams, (int)logStreamHandle);
  LOCK_UNLOCK(&logStreams_lock);
  if (stream == NULL) {
    rc = SA_AIS_ERR_BAD_HANDLE;
    goto EXIT;
  }

  client = stream->client;

  /* Coding of message */
  stream_close_msg.logstreamhandle = stream->internalStreamHandle;

  msg.logstreamclose = &stream_close_msg;

  msgbuf.size = safs_log_message__get_packed_size(&msg);
  msgbuf.buf = malloc(msgbuf.size);
  safs_log_message__pack(&msg, (uint8_t *)msgbuf.buf);

  MUTEX_LOCK(&(client->mutex));
  recv_ret = safc_send_recv(client->sockfd, &msgbuf);
  MUTEX_UNLOCK(&(client->mutex));

  if (recv_ret == SA_AIS_ERR_MESSAGE_ERROR) {
    TRACE1(("SA_AIS_ERR_UNAVAILABLE: Couldn't send/read from socket\n"));
    free(msgbuf.buf);
    rc = SA_AIS_ERR_UNAVAILABLE; // Perhaps another error code;
    goto EXIT;
  }

  /* Decoding of answer */
  ret_msg = safs_log_stream_close_ret__unpack(NULL,
					      msgbuf.size,
					      (uint8_t *)msgbuf.buf);
  free(msgbuf.buf);

  if (ret_msg == NULL) {
    TRACE1(("SA_AIS_ERR_UNAVAILABLE: Couldn't unpack incoming message\n"));
    rc = SA_AIS_ERR_UNAVAILABLE;
    goto EXIT;
  }

  rc = ret_msg->returnval;
  safs_log_stream_close_ret__free_unpacked(ret_msg, NULL);

  LOCK_WRLOCK(&client->streams_lock);
  client->streams = safc_slist_remove(client->streams, stream);
  LOCK_UNLOCK(&client->streams_lock);

  LOCK_WRLOCK(&logStreams_lock);
  safc_array_remove(&logStreams, (int)logStreamHandle);
  LOCK_UNLOCK(&logStreams_lock);

  free(stream);

 EXIT:
  TRACE_LEAVE();
  return rc;
}

/* 3.7.1 saLogLimitGet */
SaAisErrorT
saLogLimitGet(SaLogHandleT logHandle,
	      SaLogLimitIdT limitId,
	      SaLimitValueT *limitValue)
{

  SaAisErrorT rc = SA_AIS_OK;
  SaAisErrorT recv_ret;
  SafcLogClientT *client;
  safc_buf_t msgbuf;
  SafsLogLimitGet *log_limit_get_msg;
  SafsLogMessage msg = SAFS_LOG_MESSAGE__INIT;
  SafsLogLimitGetRet *ret_msg;
  SaLimitValueT internalLimitValue;

  TRACE_ENTER();
  fprintf(stderr, "saLogLimitGet LIB limitid: %d\n", (int) limitId);

   if (limitValue == NULL) {
    rc = SA_AIS_ERR_INVALID_PARAM;
    goto EXIT;
  }

   if (limitId != SA_LOG_MAX_NUM_CLUSTER_APP_LOG_STREAMS_ID) {
    rc = SA_AIS_ERR_INVALID_PARAM;
    goto EXIT;
  }

  LOCK_RDLOCK(&logClients_lock);
  client = safc_array_get(&logClients, (int)logHandle);
  LOCK_UNLOCK(&logClients_lock);

  if (client == NULL) {
    rc = SA_AIS_ERR_BAD_HANDLE;
    goto EXIT;
  }

  /* Coding of message */
  rc = allocSaLogLimitGet(&log_limit_get_msg,
			  client->internalHandle,
			  limitId);
  if (rc != SA_AIS_OK)
    goto EXIT;

  msg.loglimitget = log_limit_get_msg;

  msgbuf.size = safs_log_message__get_packed_size(&msg);
  msgbuf.buf = malloc(msgbuf.size);
  safs_log_message__pack(&msg, (uint8_t *)msgbuf.buf);

  freeSaLogLimitGet(log_limit_get_msg);

  MUTEX_LOCK(&(client->mutex));
  recv_ret = safc_send_recv(client->sockfd, &msgbuf);
  MUTEX_UNLOCK(&(client->mutex));

  if (recv_ret == SA_AIS_ERR_MESSAGE_ERROR) {
    TRACE1(("SA_AIS_ERR_UNAVAILABLE: Couldn't send/read from socket\n"));
    free(msgbuf.buf);
    rc = SA_AIS_ERR_UNAVAILABLE; // Perhaps another error code;
    goto EXIT;
  }

  /* Decoding of answer */
  ret_msg = safs_log_limit_get_ret__unpack(NULL,
					   msgbuf.size,
					   (uint8_t *)msgbuf.buf);
  free(msgbuf.buf);

  if (ret_msg == NULL) {
    TRACE1(("SA_AIS_ERR_UNAVAILABLE: Couldn't unpack incoming message\n"));
    rc = SA_AIS_ERR_UNAVAILABLE;
    goto EXIT;
  }

  rc = ret_msg->returnval;
  internalLimitValue.uint64Value = ret_msg->limitvalue->uint64value;
  safs_log_limit_get_ret__free_unpacked(ret_msg, NULL);

  if (rc != SA_AIS_OK)
    goto EXIT;

  *limitValue = internalLimitValue;

 EXIT:
  TRACE_LEAVE();
  return rc;
}



static void
dispatchCallback(SaLogCallbacks callbacks_msg,
		 SaLogCallbacksT callbacks)
{
  if (callbacks_msg.logstreamopencallback)
    logStreamOpenCb(*callbacks_msg.logstreamopencallback,
		    &callbacks.saLogStreamOpenCallback);
  else if (callbacks_msg.logwritelogcallback)
    logWriteLogCb(*callbacks_msg.logwritelogcallback,
		  &callbacks.saLogWriteLogCallback);
  else if (callbacks_msg.logfiltersetcallback)
    logFilterSetCb(*callbacks_msg.logfiltersetcallback,
		   &callbacks.saLogFilterSetCallback);
}

static void
logStreamOpenCb(SaLogStreamOpenCallback data,
		SaLogStreamOpenCallbackT *cb)
{
  SaInvocationT invocation = data.invocation;
  SaLogStreamHandleT logStreamHandle = data.logstreamhandle;
  SaAisErrorT error = data.error;

  TRACE_ENTER();

  (*cb)(invocation, logStreamHandle, error);

  TRACE_LEAVE();
}

static void
logWriteLogCb(SaLogWriteLogCallback data,
	      SaLogWriteLogCallbackT *cb)
{
  SaInvocationT invocation = data.invocation;
  SaAisErrorT error = data.error;

  TRACE_ENTER();

  (*cb)(invocation, error);

  TRACE_LEAVE();
}

static void
logFilterSetCb(SaLogFilterSetCallback data,
	       SaLogFilterSetCallbackT *cb)
{
  SaLogStreamHandleT logStreamHandle = data.logstreamhandle;
  SaLogSeverityFlagsT logSeverity;

  TRACE_ENTER();

  logSeverity = severityFlags(data.logseverity);
  (*cb)(logStreamHandle, logSeverity);

  TRACE_LEAVE();
}

static SaLogSeverityFlagsT
severityFlags(SafsLogSeverityFlags *logseverity)
{
  SaLogSeverityFlagsT rc = 0;

  if (logseverity->salogsevflagemergency)
    rc |= SA_LOG_SEV_FLAG_EMERGENCY;

  if (logseverity->salogsevflagalert)
    rc |= SA_LOG_SEV_FLAG_ALERT;

  if (logseverity->salogsevflagcritical)
    rc |= SA_LOG_SEV_FLAG_CRITICAL;

  if (logseverity->salogsevflagerror)
    rc |= SA_LOG_SEV_FLAG_ERROR;

  if (logseverity->salogsevflagwarning)
    rc |= SA_LOG_SEV_FLAG_WARNING;

  if (logseverity->salogsevflagnotice)
    rc |= SA_LOG_SEV_FLAG_NOTICE;

  if (logseverity->salogsevflaginfo)
    rc |= SA_LOG_SEV_FLAG_INFO;

  return rc;
}

static SaAisErrorT
allocSafsLogStreamOpen2(SafsLogStreamOpen2 **pptr,
			SaLogHandleT logHandle,
			const SaNameT *logStreamName,
			const SaLogFileCreateAttributesT_2 *logFileCreateAttributes,
			SaLogStreamOpenFlagsT logStreamOpenFlags,
			SaTimeT timeout,
			SaLogHeaderTypeT *logHdrType)
{
  SaAisErrorT rc;
  SafsLogStreamOpen2* ptr = NULL;

  *pptr = ptr;

  if (logStreamName == NULL)
    return SA_AIS_ERR_INVALID_PARAM;

  ptr = malloc(sizeof(*ptr));
  if (ptr == NULL)
    return SA_AIS_ERR_NO_MEMORY;

  safs_log_stream_open_2__init(ptr);

  rc = allocSafsLogFileCreateAttributes2(&ptr->logfilecreateattributes,
					 logFileCreateAttributes,
					 logStreamName,
					 logStreamOpenFlags,
					 logHdrType);
  if (rc != SA_AIS_OK)
    goto ERROR;

  ptr->handle = logHandle;
  rc = allocBufferFromSaNameT(&ptr->logstreamname, logStreamName);
  if (rc != SA_AIS_OK)
    goto ERROR;

  if (logStreamOpenFlags == SA_LOG_STREAM_CREATE) {
    ptr->has_logstreamopenflags = 1;
    ptr->logstreamopenflags = SAFS_LOG_STREAM_OPEN_FLAGS__sa_log_stream_create;
  }

  ptr->has_timeout = 1;
  ptr->timeout = timeout;

  *pptr = ptr;
  return SA_AIS_OK;

 ERROR:
  freeSafsLogStreamOpen2(ptr);

  return rc;
}

static void
freeSafsLogStreamOpen2(SafsLogStreamOpen2* ptr)
{
  if (ptr == NULL)
    return;

  free(ptr->logstreamname);
  freeSafsLogFileCreateAttributes2(ptr->logfilecreateattributes);
  free(ptr);
}

static SaAisErrorT
allocSafsLogFileCreateAttributes2(SafsLogFileCreateAttributes2 **pptr,
				  const SaLogFileCreateAttributesT_2 *logFileCreateAttributes,
				  const SaNameT *logStreamName,
				  SaLogStreamOpenFlagsT logStreamOpenFlags,
				  SaLogHeaderTypeT *logHdrType)
{
  SafsLogFileCreateAttributes2* ptr = NULL;

  *pptr = ptr;

  switch(logStreamOpenFlags) {
  case 0:
  case SA_LOG_STREAM_CREATE:
    break;
  default:
    return SA_AIS_ERR_BAD_FLAGS;
  }

  if (strcmp((char *)logStreamName->value, SA_LOG_STREAM_ALARM) == 0 ||
      strcmp((char *)logStreamName->value, SA_LOG_STREAM_NOTIFICATION) == 0 ||
      strcmp((char *)logStreamName->value, SA_LOG_STREAM_SYSTEM) == 0) {
    if (logFileCreateAttributes || logStreamOpenFlags == SA_LOG_STREAM_CREATE)
      return SA_AIS_ERR_INVALID_PARAM;
    if (strcmp((char *)logStreamName->value, SA_LOG_STREAM_SYSTEM) != 0)
      *logHdrType = SA_LOG_NTF_HEADER;
  } else {
    if (logStreamOpenFlags == SA_LOG_STREAM_CREATE &&
	logFileCreateAttributes == NULL)
      return SA_AIS_ERR_INVALID_PARAM;

    if (logStreamOpenFlags != SA_LOG_STREAM_CREATE && logFileCreateAttributes)
      return SA_AIS_ERR_INVALID_PARAM;

    if (strncmp((const char *)logStreamName->value, "safLgStr=", 9) != 0)
      return SA_AIS_ERR_INVALID_PARAM;

    if (logFileCreateAttributes) {
      if (logFileCreateAttributes->logFileName == NULL)
	return SA_AIS_ERR_INVALID_PARAM;

      if (logFileCreateAttributes->maxLogFileSize == 0)
	return SA_AIS_ERR_INVALID_PARAM;

      if (logFileCreateAttributes->maxLogRecordSize == 0)
	return SA_AIS_ERR_INVALID_PARAM;

      switch (logFileCreateAttributes->logFileFullAction) {
      case SA_LOG_FILE_FULL_ACTION_WRAP:
      case SA_LOG_FILE_FULL_ACTION_HALT:
      case SA_LOG_FILE_FULL_ACTION_ROTATE:
	break;
      default:
	return SA_AIS_ERR_INVALID_PARAM;
      }

      switch (logFileCreateAttributes->haProperty) {
      case SA_FALSE:
      case SA_TRUE :
	break;
      default:
	return SA_AIS_ERR_INVALID_PARAM;
      }

      if (logFileCreateAttributes->maxLogRecordSize >
	  logFileCreateAttributes->maxLogFileSize)
	return SA_AIS_ERR_INVALID_PARAM;
    }
  }

  if (logFileCreateAttributes == NULL)
    return SA_AIS_OK;

  ptr = malloc(sizeof(*ptr));
  if (ptr == NULL)
    return SA_AIS_ERR_NO_MEMORY;

  safs_log_file_create_attributes_2__init(ptr);

  ptr->logfilename = logFileCreateAttributes->logFileName;
  ptr->logfilepathname = logFileCreateAttributes->logFilePathName;
  ptr->maxlogfilesize = logFileCreateAttributes->maxLogFileSize;
  ptr->maxlogrecordsize = logFileCreateAttributes->maxLogRecordSize;
  ptr->haproperty = logFileCreateAttributes->haProperty;
  switch (logFileCreateAttributes->logFileFullAction) {
  case SA_LOG_FILE_FULL_ACTION_WRAP:
    ptr->logfilefullaction =
      SAFS_LOG_FILE_FULL_ACTION__sa_log_file_full_action_wrap;
    break;
  case SA_LOG_FILE_FULL_ACTION_HALT:
    ptr->logfilefullaction =
      SAFS_LOG_FILE_FULL_ACTION__sa_log_file_full_action_halt;
    break;
  case SA_LOG_FILE_FULL_ACTION_ROTATE:
    ptr->logfilefullaction =
      SAFS_LOG_FILE_FULL_ACTION__sa_log_file_full_action_rotate;
    ptr->has_maxfilesrotated = 1;
    ptr->maxfilesrotated = logFileCreateAttributes->maxFilesRotated;
    break;
  }
  ptr->logfilefmt = logFileCreateAttributes->logFileFmt;

  *pptr = ptr;
  return SA_AIS_OK;
}

static void
freeSafsLogFileCreateAttributes2(SafsLogFileCreateAttributes2* ptr)
{
  free(ptr);
}

static SaAisErrorT
allocSafsLogWriteLogAsync(SafsLogWriteLogAsync **pptr,
			  SaLogStreamHandleT logstreamhandle,
			  SaInvocationT invocation,
			  SaLogAckFlagsT ackFlags,
			  const SaLogRecordT *logRecord,
			  SaLogHeaderTypeT logHdrType,
			  SaLogWriteLogCallbackT saLogWriteLogCallback)
{
  SaAisErrorT rc;
  SafsLogWriteLogAsync *ptr = NULL;

  *pptr = ptr;

  if ((ackFlags != 0) && (ackFlags != SA_LOG_RECORD_WRITE_ACK))
    return SA_AIS_ERR_BAD_FLAGS;

  if (ackFlags == SA_LOG_RECORD_WRITE_ACK && saLogWriteLogCallback == NULL)
    return SA_AIS_ERR_INIT;

  ptr = malloc(sizeof(*ptr));
  if (ptr == NULL)
    return SA_AIS_ERR_NO_MEMORY;

  safs_log_write_log_async__init(ptr);

  ptr->logstreamhandle = logstreamhandle;
  ptr->invocation = invocation;
  if (ackFlags == SA_LOG_RECORD_WRITE_ACK) {
    ptr->has_ackflags = 1;
    ptr->ackflags = SAFS_LOG_ACK_FLAGS__sa_log_record_write_ack;
  }
  rc = allocSafsLogRecord(&ptr->logrecord, logRecord, logHdrType);
  if (rc != SA_AIS_OK)
    goto ERROR;

  *pptr = ptr;
  return SA_AIS_OK;

 ERROR:
  freeSafsLogWriteLogAsync(ptr);

  return rc;
}

static void
freeSafsLogWriteLogAsync(SafsLogWriteLogAsync *ptr)
{
  if (ptr == NULL)
    return;

  freeSafsLogRecord(ptr->logrecord);
  free(ptr);
}

static SaAisErrorT
allocSafsLogRecord(SafsLogRecord **pptr,
		   const SaLogRecordT *logRecord,
		   SaLogHeaderTypeT logHdrType)
{
  SaAisErrorT rc;
  SafsLogRecord *ptr = NULL;

  *pptr = ptr;

  if (logRecord == NULL)
    return SA_AIS_ERR_INVALID_PARAM;

  if (logRecord->logHdrType != logHdrType)
    return SA_AIS_ERR_INVALID_PARAM;

  ptr = malloc(sizeof(*ptr));
  if (ptr == NULL)
    return SA_AIS_ERR_NO_MEMORY;

  safs_log_record__init(ptr);

  if (logRecord->logTimeStamp == (SaTimeT)SA_TIME_UNKNOWN)
    ptr->logtimestamp = safc_current_time();
  else
    ptr->logtimestamp = logRecord->logTimeStamp;

  if (logRecord->logHdrType == SA_LOG_NTF_HEADER)
    ptr->loghdrtype = SAFS_LOG_HEADER_TYPE__sa_log_ntf_header;
  else
    ptr->loghdrtype = SAFS_LOG_HEADER_TYPE__sa_log_generic_header;

  rc = allocSafsLogHeader(&ptr->logheader, ptr->loghdrtype,
			  &logRecord->logHeader);
  if (rc != SA_AIS_OK)
    goto ERROR;

  rc = allocBufferFromSaLogBufferT(&ptr->logbuffer, logRecord->logBuffer);
  if (rc != SA_AIS_OK)
    goto ERROR;

  *pptr = ptr;
  return SA_AIS_OK;

 ERROR:
  freeSafsLogRecord(ptr);

  return rc;
}

static void
freeSafsLogRecord(SafsLogRecord *ptr)
{
  if (ptr == NULL)
    return;

  free(ptr->logbuffer);
  freeSafsLogHeader(ptr->logheader);
  free(ptr);
}

static SaAisErrorT
allocSafsLogHeader(SafsLogHeader **pptr,
		   SafsLogHeaderType loghdrtype,
		   const SaLogHeaderT *logHeader)
{
  SaAisErrorT rc;
  SafsLogHeader *ptr = NULL;

  *pptr = ptr;

  ptr = malloc(sizeof(*ptr));
  if (ptr == NULL)
    return SA_AIS_ERR_NO_MEMORY;

  safs_log_header__init(ptr);

  if (loghdrtype == SAFS_LOG_HEADER_TYPE__sa_log_ntf_header)
    rc = allocSafsLogNtfLogHeader(&ptr->ntfhdr, &logHeader->ntfHdr);
  else
    rc = allocSafsLogGenericLogHeader(&ptr->generichdr, &logHeader->genericHdr);

  if (rc != SA_AIS_OK)
    goto ERROR;

  *pptr = ptr;
  return SA_AIS_OK;

 ERROR:
  freeSafsLogHeader(ptr);

  return rc;
}

static void
freeSafsLogHeader(SafsLogHeader *ptr)
{
  if (ptr == NULL)
    return;

  freeSafsLogNtfLogHeader(ptr->ntfhdr);
  freeSafsLogGenericLogHeader(ptr->generichdr);
  free(ptr);
}

static SaAisErrorT
allocSafsLogNtfLogHeader(SafsLogNtfLogHeader **pptr,
			 const SaLogNtfLogHeaderT *ntfHdr)
{
  SaAisErrorT rc;
  SafsLogNtfLogHeader *ptr = NULL;

  *pptr = ptr;

  if (ntfHdr->notificationObject == NULL || ntfHdr->notifyingObject == NULL)
    return SA_AIS_ERR_INVALID_PARAM;

  ptr = malloc(sizeof(*ptr));
  if (ptr == NULL)
    return SA_AIS_ERR_NO_MEMORY;

  safs_log_ntf_log_header__init(ptr);

  ptr->notificationid = ntfHdr->notificationId;
  ptr->eventtype = ntfHdr->eventType;

  rc = allocBufferFromSaNameT(&ptr->notificationobject,
			      ntfHdr->notificationObject);
  if (rc != SA_AIS_OK)
    goto ERROR;

  rc = allocBufferFromSaNameT(&ptr->notifyingobject, ntfHdr->notifyingObject);
  if (rc != SA_AIS_OK)
    goto ERROR;

  rc = allocSafsNtfClassid(&ptr->notificationclassid,
			   ntfHdr->notificationClassId);
  if (rc != SA_AIS_OK)
    goto ERROR;

  ptr->eventtime = ntfHdr->eventTime;

  *pptr = ptr;
  return SA_AIS_OK;

 ERROR:
  freeSafsLogNtfLogHeader(ptr);

  return rc;
}

static void
freeSafsLogNtfLogHeader(SafsLogNtfLogHeader *ptr)
{
  if (ptr == NULL)
    return;

  freeSafsNtfClassid(ptr->notificationclassid);
  free(ptr->notifyingobject);
  free(ptr->notificationobject);
  free(ptr);
}

static SaAisErrorT
allocSafsLogGenericLogHeader(SafsLogGenericLogHeader **pptr,
			     const SaLogGenericLogHeaderT *genericHdr)
{
  SaAisErrorT rc;
  SafsLogGenericLogHeader *ptr = NULL;

  *pptr = ptr;

  switch (genericHdr->logSeverity) {
  case SA_LOG_SEV_EMERGENCY:
  case SA_LOG_SEV_ALERT:
  case SA_LOG_SEV_CRITICAL:
  case SA_LOG_SEV_ERROR:
  case SA_LOG_SEV_WARNING:
  case SA_LOG_SEV_NOTICE:
  case SA_LOG_SEV_INFO:
    break;
  default:
    return SA_AIS_ERR_INVALID_PARAM;
  }

  ptr = malloc(sizeof(*ptr));
  if (ptr == NULL)
    return SA_AIS_ERR_NO_MEMORY;

  safs_log_generic_log_header__init(ptr);

  rc = allocSafsNtfClassid(&ptr->notificationclassid,
			   genericHdr->notificationClassId);
  if (rc != SA_AIS_OK)
    goto ERROR;

  if (genericHdr->logSvcUsrName == NULL) {
    char *logSvcUsrChars;
    SaNameT logSvcUsrName;

    logSvcUsrChars = getenv("SA_AMF_COMPONENT_NAME");
    if (logSvcUsrChars == NULL)
      return SA_AIS_ERR_INVALID_PARAM;

    logSvcUsrName.length = strlen(logSvcUsrChars);
    strncpy((char *)logSvcUsrName.value, logSvcUsrChars, SA_MAX_NAME_LENGTH);
    rc = allocBufferFromSaNameT(&ptr->logsvcusrname, &logSvcUsrName);
  } else {
    rc = allocBufferFromSaNameT(&ptr->logsvcusrname, genericHdr->logSvcUsrName);
  }
  if (rc != SA_AIS_OK)
    goto ERROR;

  switch (genericHdr->logSeverity) {
  case SA_LOG_SEV_EMERGENCY:
    ptr->logseverity = SAFS_LOG_SEVERITY__sa_log_sev_emergency;
    break;
  case SA_LOG_SEV_ALERT:
    ptr->logseverity = SAFS_LOG_SEVERITY__sa_log_sev_alert;
    break;
  case SA_LOG_SEV_CRITICAL:
    ptr->logseverity = SAFS_LOG_SEVERITY__sa_log_sev_critical;
    break;
  case SA_LOG_SEV_ERROR:
    ptr->logseverity = SAFS_LOG_SEVERITY__sa_log_sev_error;
    break;
  case SA_LOG_SEV_WARNING:
    ptr->logseverity = SAFS_LOG_SEVERITY__sa_log_sev_warning;
    break;
  case SA_LOG_SEV_NOTICE:
    ptr->logseverity = SAFS_LOG_SEVERITY__sa_log_sev_notice;
    break;
  case SA_LOG_SEV_INFO:
    ptr->logseverity = SAFS_LOG_SEVERITY__sa_log_sev_info;
    break;
  }

  *pptr = ptr;
  return SA_AIS_OK;

 ERROR:
  freeSafsLogGenericLogHeader(ptr);

  return rc;
}

static void
freeSafsLogGenericLogHeader(SafsLogGenericLogHeader *ptr)
{
  if (ptr == NULL)
    return;

  free(ptr->logsvcusrname);
  freeSafsNtfClassid(ptr->notificationclassid);
  free(ptr);
}

static SaAisErrorT
allocSafsNtfClassid(SafsNtfClassId **pptr,
                    SaNtfClassIdT* classId)
{
  SafsNtfClassId *ptr = NULL;

  *pptr = ptr;

  if (classId == NULL)
    return SA_AIS_OK;

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

static void
freeSafsNtfClassid(SafsNtfClassId *ptr)
{
  free(ptr);
}

static SaAisErrorT
allocBufferFromSaNameT(char** pptr, const SaNameT *saName)
{
  char* ptr = NULL;

  *pptr = ptr;

  if (saName == NULL)
    return SA_AIS_OK;

  if (saName->length > SA_MAX_NAME_LENGTH)
    return SA_AIS_ERR_INVALID_PARAM;

  ptr = malloc(saName->length+1);
  if (ptr == NULL)
    return SA_AIS_ERR_NO_MEMORY;

  memcpy(ptr, saName->value, saName->length);
  ptr[saName->length] = 0;

  *pptr = ptr;
  return SA_AIS_OK;
}

static SaAisErrorT
allocBufferFromSaLogBufferT(char** pptr, const SaLogBufferT *logBuffer)
{
  char* ptr = NULL;

  *pptr = ptr;

  if (logBuffer == NULL)
    return SA_AIS_OK;

  if (logBuffer->logBuf == NULL && logBuffer->logBufSize != 0)
    return SA_AIS_ERR_INVALID_PARAM;

  if (logBuffer->logBuf == NULL)
    return SA_AIS_OK;

  ptr = malloc(logBuffer->logBufSize+1);
  if (ptr == NULL)
    return SA_AIS_ERR_NO_MEMORY;

  memcpy(ptr, logBuffer->logBuf, logBuffer->logBufSize);
  ptr[logBuffer->logBufSize] = 0;

  *pptr = ptr;
  return SA_AIS_OK;
}


/* Extended Interface saLogDeleteFiles */
SaAisErrorT
saLogDeleteFiles(SaLogHandleT logHandle,
		 SaStringT logFileName,
		 SaStringT logFilePathName)
{
  SaAisErrorT rc = SA_AIS_OK;
  SaAisErrorT recv_ret;
  SafcLogClientT *client;
  safc_buf_t msgbuf;
  SafsLogDeleteFiles *delete_files_msg;
  SafsLogMessage msg = SAFS_LOG_MESSAGE__INIT;
  SafsLogDeleteFilesRet *ret_msg;

  TRACE_ENTER();


  LOCK_RDLOCK(&logClients_lock);
  client = safc_array_get(&logClients, (int)logHandle);
  LOCK_UNLOCK(&logClients_lock);
  if (client == NULL) {
    rc = SA_AIS_ERR_BAD_HANDLE;
    goto EXIT;
  }


  /* Coding of message */
  rc = allocSafsLogDeleteFiles(&delete_files_msg,
			       client->internalHandle,
			       logFileName,
			       logFilePathName);
  if (rc != SA_AIS_OK)
    goto EXIT;

  msg.logdeletefiles = delete_files_msg;

  msgbuf.size = safs_log_message__get_packed_size(&msg);
  msgbuf.buf = malloc(msgbuf.size);
  safs_log_message__pack(&msg, (uint8_t *)msgbuf.buf);

  freeSafsLogDeleteFiles(delete_files_msg);

  MUTEX_LOCK(&(client->mutex));
  recv_ret = safc_send_recv(client->sockfd, &msgbuf);
  MUTEX_UNLOCK(&(client->mutex));

  if (recv_ret == SA_AIS_ERR_MESSAGE_ERROR) {
    TRACE1(("SA_AIS_ERR_UNAVAILABLE: Couldn't send/read from socket\n"));
    free(msgbuf.buf);
    rc = SA_AIS_ERR_UNAVAILABLE; // Perhaps another error code;
    goto EXIT;
  }

  /* Decoding of answer */
  ret_msg = safs_log_delete_files_ret__unpack(NULL,
					      msgbuf.size,
					      (uint8_t *)msgbuf.buf);
  free(msgbuf.buf);

  if (ret_msg == NULL) {
    TRACE1(("SA_AIS_ERR_UNAVAILABLE: Couldn't unpack incoming message\n"));
    rc = SA_AIS_ERR_UNAVAILABLE;
    goto EXIT;
  }

  rc = ret_msg->returnval;
  safs_log_delete_files_ret__free_unpacked(ret_msg, NULL);

 EXIT:
  TRACE_LEAVE();
  return rc;
}





static SaAisErrorT
allocSafsLogDeleteFiles(SafsLogDeleteFiles **pptr,
			SaLogHandleT loghandle,
			SaStringT logfilename,
			SaStringT logfilepathname)
{
  SafsLogDeleteFiles* ptr = NULL;

  *pptr = ptr;

  if (logfilename == NULL)
    return SA_AIS_ERR_INVALID_PARAM;

  ptr = malloc(sizeof(*ptr));
  if (ptr == NULL)
    return SA_AIS_ERR_NO_MEMORY;

  safs_log_delete_files__init(ptr);

  ptr->handle = loghandle;
  ptr->logfilename = logfilename;
  ptr->logfilepathname = logfilepathname;

  *pptr = ptr;
  return SA_AIS_OK;

}

static void
freeSafsLogDeleteFiles(SafsLogDeleteFiles* ptr)
{
  free(ptr);
}




static SaAisErrorT
allocSaLogLimitGet(SafsLogLimitGet **pptr,
		   SaLogHandleT loghandle,
		   SaLogLimitIdT limitId)
{
  SafsLogLimitGet* ptr = NULL;

  *pptr = ptr;

  ptr = malloc(sizeof(*ptr));
  if (ptr == NULL)
    return SA_AIS_ERR_NO_MEMORY;

  safs_log_limit_get__init(ptr);

  ptr->handle = loghandle;
  ptr->limitid = limitId;

  *pptr = ptr;
  return SA_AIS_OK;

}

static void
freeSaLogLimitGet(SafsLogLimitGet* ptr)
{
  free(ptr);
}
