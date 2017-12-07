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
 *  Purpose : SAF IMM OI Agent Library
 * ----------------------------------------------------------------------
 *
 */

#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <netinet/in.h>
#include <pthread.h>
#include <fcntl.h>
#include <stdio.h>
#include <saAis.h>
#include <saImm.h>
#include <saImmOm.h>
#include <saImmOi.h>
#include "safc_sock_lib.h"
#include "safc_trace.h"
#include "safc_array.h"
/* #include "sa_ais.pb-c.h" */
#include "oi.pb-c.h"
#include "safc_imm_lib.h"
#include "safc_imm_om_lib.h"
#include "saImmOi_A_2_safc.h"

/*
 * Client structures
 */
typedef struct imm_oi_client {
  int hasCallbacks;
  SaImmOiCallbacksT_2 callbacks;
  int isImmA2b;
  int sockfd;
  int cb_sockfd;
  long long srv_handle;
  pthread_mutex_t mutex;
} SafcImmOiClientT;

typedef struct {
   int ccbId;
   SaImmHandleT immHandle;
} SafcImmOiAugmentedCcbStructT;

/*
 * Variables for the client structure storage array
 */
pthread_rwlock_t immOiClients_lock = PTHREAD_RWLOCK_INITIALIZER;
safc_array immOiClients = SAFC_ARRAY_INITIALIZER;

/*
 * Function declarations
 */
SaAisErrorT dispatchOiCallback(SaImmOiHandleT immOiHandle,
			     SaImmOiCallbacks callbacks_msg,
			     SaImmOiCallbacksT_2 callbacks);

int isVoidOiCallback(SaImmOiCallbacks callbacks_msg);

void safs_to_sa_copy_attr_values(SaImmAttrValueT* attrVals,
				 const SaImmValueTypeT attrValueType,
				 const SafsImmAttrValues2* avals);

void safs_to_sa_copy_attr_value(SaImmAttrValueT* attrValue,
				const SaImmValueTypeT attrValueType,
				SafsImmAttrValue* aval);

void free_sa_attr_values(SaImmAttrValueT* attrVals,
			 const SaUint32T attrValuesNumber,
			 const SaImmValueTypeT attrValueType);

void free_sa_attr_value(SaImmAttrValueT attrValue,
			const SaImmValueTypeT attrValueType);
void safc_free_rt_oc2_msg(SafsImmOiRtObjectCreate2 rt_oc2_msg);
void safc_free_rt_ou2_msg(SafsImmOiRtObjectUpdate2 rt_ou2_msg);
void safc_copy_from_imm_attr_value(SafsImmAttrValue* aval,
				   const SaImmValueTypeT attrValueType,
				   const SaImmAttrValueT attrValue);
int safc_find_augmented_ccb(void* elem, void* sval);
void safc_print_augmented_ccb(void* elem);


/*
 * Variables for the augmentation handle
 */
pthread_rwlock_t immAugmentCcbs_lock = PTHREAD_RWLOCK_INITIALIZER;
safc_array immAugmentCcbs = {0, 0, NULL, &safc_print_augmented_ccb, 
			     NULL, &safc_find_augmented_ccb};

extern pthread_rwlock_t immOmClients_lock;
extern safc_array immOmClients;

/*
 * ======================================================================
 * 5.3 Library Life Cycle
 * ======================================================================
 */
SaAisErrorT saImmOiInitialize_2(SaImmOiHandleT *immOiHandle,
				const SaImmOiCallbacksT_2 *immOiCallbacks,
				SaVersionT *version)
{
   SafcImmOiClientT *client=NULL;
   SaAisErrorT ret = SA_AIS_OK;
   SaAisErrorT recv_ret = SA_AIS_OK;
   int port;
   char* hostname = "localhost";
   safc_buf_t msgbuf;
   SafsImmOiCallbacks cb_msg =  SAFS_IMM_OI_CALLBACKS__INIT;
   SafsVersion version_msg =  SAFS_VERSION__INIT;
   SafsImmOiInitialize init_msg =  SAFS_IMM_OI_INITIALIZE__INIT;
   SafsImmOiMessage msg = SAFS_IMM_OI_MESSAGE__INIT;
   SafsImmOiInitializeRet *ret_msg;
   SafsImmOiCallbacksInitialize cb_init_msg =
      SAFS_IMM_OI_CALLBACKS_INITIALIZE__INIT;
   SafsImmOiCallbacksInitializeRet *cb_init_ret_msg;
   SafsVersion *returned_version;
   
   TRACE_ENTER();

   if ((!immOiHandle) || (!version)) {
      TRACE1(("ERR_INVALID_PARAM: immOiHandle is NULL or version is NULL\n"));
      return SA_AIS_ERR_INVALID_PARAM;
   }

   *immOiHandle = 0;
   
   /* Validations : Version */
   ret = safc_imm_version_validate(version);
   if (ret != SA_AIS_OK) {
      TRACE1(("ERR_VERSION: Version validation failed\n"));
      return ret;
   }

   /* Alloc the client info data structure, handle for now */
   client = (SafcImmOiClientT *) calloc(1, sizeof(SafcImmOiClientT));
   if (client == NULL) {
      TRACE1(("ERR_NO_MEMORY: IMM client structure alloc failed\n"));
      return SA_AIS_ERR_NO_MEMORY;
   }

   /* Store the callback functions, if set */
   if (immOiCallbacks) {
      client->hasCallbacks = 1;
      client->callbacks = *immOiCallbacks;
   } else
      client->hasCallbacks = 0;

   port = safc_protobuf_port("SAFC_IMM_OI_PORT", 10002);
   client->sockfd = safc_connect(hostname, port);
   if (client->sockfd < 0) {
      TRACE1(("safc_connect failed\n"));
      return SA_AIS_ERR_UNAVAILABLE;
   }

   MUTEX_INIT(&client->mutex);

   LOCK_WRLOCK(&immOiClients_lock);
   *immOiHandle =
      (SaImmOiHandleT) safc_array_insert(&immOiClients, (void*) client);
   LOCK_UNLOCK(&immOiClients_lock);

   /* Coding of message */

   version_msg.releasecode = version->releaseCode;
   version_msg.majorversion = version->majorVersion;
   version_msg.minorversion = version->minorVersion;

   if(client->hasCallbacks) {
      if (immOiCallbacks->saImmOiCcbObjectCreateCallback != NULL)
	 cb_msg.saimmoiccbobjectcreatecallback_2 = 1;
      if (immOiCallbacks->saImmOiCcbObjectDeleteCallback != NULL)
	 cb_msg.saimmoiccbobjectdeletecallback = 1;
      if (immOiCallbacks->saImmOiCcbObjectModifyCallback != NULL)
	 cb_msg.saimmoiccbobjectmodifycallback_2 = 1;
      if (immOiCallbacks->saImmOiCcbCompletedCallback != NULL)
	 cb_msg.saimmoiccbcompletedcallback = 1;
      if (immOiCallbacks->saImmOiCcbApplyCallback != NULL)
	 cb_msg.saimmoiccbapplycallback = 1;
      if (immOiCallbacks->saImmOiCcbAbortCallback != NULL)
	 cb_msg.saimmoiccbabortcallback = 1;
      if (immOiCallbacks->saImmOiAdminOperationCallback != NULL)
	 cb_msg.saimmoiadminoperationcallback_2 = 1;
      if (immOiCallbacks->saImmOiRtAttrUpdateCallback != NULL)
	 cb_msg.saimmoirtattrupdatecallback = 1;
   }
   init_msg.callbacks = &cb_msg;
   init_msg.version = &version_msg;

   /* msg.type = SAFS_IMM_OI_MESSAGE__TYPE__OPER_OI_INITIALIZE_2; */
   msg.initialize = &init_msg;

   msgbuf.size = safs_imm_oi_message__get_packed_size(&msg);
   msgbuf.buf = malloc(msgbuf.size);
   safs_imm_oi_message__pack(&msg, (uint8_t *) msgbuf.buf);

   /* Send message & wait for answer*/
   /* TRACE1(("Sent message length %d\n", msgbuf.size)); */

   MUTEX_LOCK(&(client->mutex));
   recv_ret = safc_send_recv(client->sockfd, &msgbuf);
   MUTEX_UNLOCK(&(client->mutex));

  if(recv_ret == SA_AIS_ERR_MESSAGE_ERROR)
   {
      TRACE1(("SA_AIS_ERR_UNAVAILABLE: Couldn't send/read from socket\n"));
      free(msgbuf.buf);
      ret = SA_AIS_ERR_UNAVAILABLE; // Perhaps another error code;
      goto error;
   }
    /* Decoding of answer */
   ret_msg = safs_imm_oi_initialize_ret__unpack(NULL,
						msgbuf.size,
						(uint8_t *) msgbuf.buf);
   free(msgbuf.buf);

   if (ret_msg == NULL)
   {
      TRACE1(("error unpacking incoming message\n"));
      ret = SA_AIS_ERR_UNAVAILABLE; // Perhaps another error code;
      goto error;
   }

   client->srv_handle = ret_msg->handle;
   returned_version = ret_msg->version;

   version->releaseCode = returned_version->releasecode;
   version->majorVersion = returned_version->majorversion;
   version->minorVersion = returned_version->minorversion;

   ret = ret_msg->returnval;
   safs_imm_oi_initialize_ret__free_unpacked(ret_msg, NULL);

   if (ret != SA_AIS_OK)
      goto error;

   if(client->hasCallbacks) {
      client->cb_sockfd = safc_connect(hostname, port);
      if (client->cb_sockfd < 0) {
	 TRACE1(("safc_connect failed for cb socket\n"));
	 ret = SA_AIS_ERR_UNAVAILABLE;
	 goto error;
      }

      /* Coding of callback init message */

      cb_init_msg.handle = client->srv_handle;

      msg.initialize = NULL;
      msg.callbacksinitialize = &cb_init_msg;

      msgbuf.size = safs_imm_oi_message__get_packed_size(&msg);
      msgbuf.buf = malloc(msgbuf.size);
      safs_imm_oi_message__pack(&msg, (uint8_t *) msgbuf.buf);

      /* Send message & wait for answer*/
      MUTEX_LOCK(&(client->mutex));
      recv_ret = safc_send_recv(client->cb_sockfd, &msgbuf);
      MUTEX_UNLOCK(&(client->mutex));

      if(recv_ret == SA_AIS_ERR_MESSAGE_ERROR)
      {
	 ret = SA_AIS_ERR_UNAVAILABLE; // Perhaps another error code;
	 free(msgbuf.buf);
	 goto cberror;
      }

      /* Decoding of answer */
      cb_init_ret_msg =
	 safs_imm_oi_callbacks_initialize_ret__unpack(NULL,
						      msgbuf.size,
						      (uint8_t *) msgbuf.buf);
      free(msgbuf.buf);

      if (cb_init_ret_msg == NULL)
      {
	 TRACE1(("error unpacking incoming message\n"));
	 ret = SA_AIS_ERR_UNAVAILABLE; // Perhaps another error code;
	 goto cberror;
      }

      ret = cb_init_ret_msg->returnval;
      safs_imm_oi_callbacks_initialize_ret__free_unpacked(cb_init_ret_msg, NULL);


      if(ret != SA_AIS_OK)
	 goto cberror;

      TRACE1(("CB Socket %llu: UP & RUNNING\n", (long long unsigned) client->srv_handle));

   }
   TRACE_LEAVE();

   return ret;

 cberror:
   safc_disconnect(client->cb_sockfd);

 error:
   safc_disconnect(client->sockfd);

   LOCK_WRLOCK(&immOiClients_lock);
   safc_array_remove(&immOiClients, (int) *immOiHandle);
   LOCK_UNLOCK(&immOiClients_lock);
   *immOiHandle = 0;

   free(client);

   TRACE_LEAVE();
   return ret;
}

SaAisErrorT saImmOiSelectionObjectGet(SaImmOiHandleT immOiHandle,
				      SaSelectionObjectT *selectionObject)
{
  SafcImmOiClientT *client;

  TRACE_ENTER();

  if (!selectionObject)
    return SA_AIS_ERR_INVALID_PARAM;

  LOCK_RDLOCK(&immOiClients_lock);
  client =
    (SafcImmOiClientT *) safc_array_get(&immOiClients, (int) immOiHandle);
  LOCK_UNLOCK(&immOiClients_lock);
  if (!client)
    return SA_AIS_ERR_BAD_HANDLE;

  *selectionObject = (SaSelectionObjectT) client->cb_sockfd;

  TRACE_LEAVE();

  return SA_AIS_OK;

}


SaAisErrorT saImmOiDispatch(SaImmOiHandleT immOiHandle,
			    SaDispatchFlagsT dispatchFlags)
{

  SaAisErrorT ret = SA_AIS_OK;
  SaAisErrorT cb_ret = SA_AIS_OK;
  safc_buf_t msgbuf;
  SaImmOiCallbacks *callbacks_msg;
  SafsImmOiCallbacksRet callbacks_ret_msg = SAFS_IMM_OI_CALLBACKS_RET__INIT;
  SafcImmOiClientT *client;

  TRACE_ENTER();

  /* Just support DISPATCH_ONE in first step */
  if (dispatchFlags != SA_DISPATCH_ONE)
    return SA_AIS_ERR_INVALID_PARAM;

  LOCK_RDLOCK(&immOiClients_lock);
  client =
    (SafcImmOiClientT *) safc_array_get(&immOiClients, (int) immOiHandle);
  LOCK_UNLOCK(&immOiClients_lock);
  if (!client)
    return SA_AIS_ERR_BAD_HANDLE;

  if(client->hasCallbacks == 0)
    return SA_AIS_ERR_BAD_HANDLE;

  TRACE1(("saImmDispatch: read message from socket and unpack \n")); 
  msgbuf.buf = NULL;
  MUTEX_LOCK(&(client->mutex));
  ret = safc_recv(client->cb_sockfd, &msgbuf);
  MUTEX_UNLOCK(&(client->mutex));
  if(ret != SA_AIS_OK) {
     TRACE1(("saImmDispatch: failed socket receive\n"));
     return SA_AIS_ERR_UNAVAILABLE;
  }

  /* LATH */
  TRACE1(("saImmOiDispatch: message size = %u\n", msgbuf.size));

  /* Decoding of message */
  callbacks_msg = sa_imm_oi_callbacks__unpack(NULL,
					      msgbuf.size,
					      (uint8_t *) msgbuf.buf);
  free(msgbuf.buf);

  if (callbacks_msg == NULL)
    {
      fprintf(stderr, "error unpacking incoming message\n");
      exit(1);
    }
  
  TRACE1(("saImmOiDispatch: call correct cb\n")); 
  cb_ret = dispatchOiCallback(immOiHandle, *callbacks_msg, client->callbacks);

  if (!isVoidOiCallback(*callbacks_msg))
    {
      TRACE1(("saImmOiDispatch: pack answer \n"));
      callbacks_ret_msg.returnval = cb_ret;
      callbacks_ret_msg.callbackmessagenumber = callbacks_msg->callbackmessagenumber;
      msgbuf.size = safs_imm_oi_callbacks_ret__get_packed_size(&callbacks_ret_msg);
      msgbuf.buf = malloc(msgbuf.size);
      safs_imm_oi_callbacks_ret__pack(&callbacks_ret_msg, (uint8_t *) msgbuf.buf);

      TRACE1(("saImmOiDispatch: send answer on socket\n"));
      TRACE1(("Send message length %d on socket %d\n", msgbuf.size,
	 client->sockfd)); 
      MUTEX_LOCK(&(client->mutex));
      safc_send(client->cb_sockfd, &msgbuf);
      MUTEX_UNLOCK(&(client->mutex));
      TRACE1(("Message sent\n"));

      free(msgbuf.buf);
    }

  sa_imm_oi_callbacks__free_unpacked(callbacks_msg, NULL);

  TRACE_LEAVE();

  return ret;
}

SaAisErrorT saImmOiFinalize(SaImmOiHandleT immOiHandle)
{
  SaAisErrorT ret = SA_AIS_OK;
  SaAisErrorT recv_ret = SA_AIS_OK;
  safc_buf_t msgbuf;
  SafsImmOiFinalize fin_msg =  SAFS_IMM_OI_FINALIZE__INIT;
  SafsImmOiMessage msg = SAFS_IMM_OI_MESSAGE__INIT;
  SafsImmOiFinalizeRet *ret_msg;
  SafcImmOiClientT *client;

  TRACE_ENTER();

  LOCK_RDLOCK(&immOiClients_lock);
  client =
    (SafcImmOiClientT *) safc_array_get(&immOiClients, (int) immOiHandle);
  LOCK_UNLOCK(&immOiClients_lock);

  if (!client) {
     TRACE_LEAVE();
    return SA_AIS_ERR_BAD_HANDLE;
  }
  
  /* Coding of message */
  fin_msg.handle = client->srv_handle;
  /* msg.type = SAFS_IMM_OI_MESSAGE__TYPE__OPER_OI_FINALIZE; */
  msg.finalize = &fin_msg;

  msgbuf.size = safs_imm_oi_message__get_packed_size(&msg);
  msgbuf.buf = malloc(msgbuf.size);
  safs_imm_oi_message__pack(&msg, (uint8_t *) msgbuf.buf);

  /* Send message & wait for answer*/
  /* TRACE1(("Sent message length %d on socket %d\n", msgbuf.size,
     client->sockfd)); */
  MUTEX_LOCK(&(client->mutex));
  recv_ret = safc_send_recv(client->sockfd, &msgbuf);
  MUTEX_UNLOCK(&(client->mutex));

  if(recv_ret == SA_AIS_ERR_MESSAGE_ERROR)
  {
     TRACE1(("SA_AIS_ERR_UNAVAILABLE: Couldn't send/read from socket\n"));
     free(msgbuf.buf);
     TRACE_LEAVE();
     return SA_AIS_ERR_UNAVAILABLE; // Perhaps another error code;
  }

  /* Decoding of answer */
  ret_msg = safs_imm_oi_finalize_ret__unpack(NULL,
					     msgbuf.size,
					     (uint8_t *) msgbuf.buf);
  free(msgbuf.buf);

  if (ret_msg == NULL)
    {
      TRACE1(("SA_AIS_ERR_UNAVAILABLE: Couldn't unpack incoming message\n"));
      TRACE_LEAVE();
      return SA_AIS_ERR_UNAVAILABLE;
    }

  ret = ret_msg->returnval;
  safs_imm_oi_finalize_ret__free_unpacked(ret_msg, NULL);

  safc_disconnect(client->sockfd);
  if(client->hasCallbacks)
     safc_disconnect(client->cb_sockfd);

  LOCK_WRLOCK(&immOiClients_lock);
  safc_array_remove(&immOiClients, (int) immOiHandle);
  LOCK_UNLOCK(&immOiClients_lock);

  free(client);

  TRACE_LEAVE();
  return ret;

}

/*
 * ======================================================================
 * 5.4 Object Implementer
 * ======================================================================
 */
SaAisErrorT saImmOiImplementerSet(SaImmOiHandleT immOiHandle,
				  const SaImmOiImplementerNameT implementerName)
{
  SaAisErrorT ret = SA_AIS_OK;
  SaAisErrorT recv_ret = SA_AIS_OK;
  safc_buf_t msgbuf;
  SafsImmOiImplementerSet impl_set_msg = SAFS_IMM_OI_IMPLEMENTER_SET__INIT;
  SafsImmOiMessage msg = SAFS_IMM_OI_MESSAGE__INIT;
  SafsImmOiImplementerSetRet *ret_msg;
  SafcImmOiClientT *client;

  TRACE_ENTER();

  LOCK_RDLOCK(&immOiClients_lock);
  client =
    (SafcImmOiClientT *) safc_array_get(&immOiClients, (int) immOiHandle);
  LOCK_UNLOCK(&immOiClients_lock);
  if (!client)
    return SA_AIS_ERR_BAD_HANDLE;

  /* Coding of message */
  impl_set_msg.handle = client->srv_handle;
  impl_set_msg.implementername = implementerName;
  /* msg.type = SAFS_IMM_OI_MESSAGE__TYPE__OPER_OI_IMPLEMENTER_SET; */
  msg.implementerset = &impl_set_msg;

  msgbuf.size = safs_imm_oi_message__get_packed_size(&msg);
  msgbuf.buf = malloc(msgbuf.size);
  safs_imm_oi_message__pack(&msg, (uint8_t *) msgbuf.buf);

  /* Send message & wait for answer*/
  /* TRACE1(("Sent message length %d on socket %d\n", msgbuf.size,
     client->sockfd)); */
  MUTEX_LOCK(&(client->mutex));
  recv_ret = safc_send_recv(client->sockfd, &msgbuf);
  MUTEX_UNLOCK(&(client->mutex));

  if(recv_ret == SA_AIS_ERR_MESSAGE_ERROR)
  {
     TRACE1(("SA_AIS_ERR_UNAVAILABLE: Couldn't send/read from socket\n"));
     free(msgbuf.buf);
     return SA_AIS_ERR_UNAVAILABLE; // Perhaps another error code;
  }

  /* Decoding of answer */
  ret_msg = safs_imm_oi_implementer_set_ret__unpack(NULL,
						    msgbuf.size,
						    (uint8_t *) msgbuf.buf);
  free(msgbuf.buf);

  if (ret_msg == NULL)
    {
      TRACE1(("SA_AIS_ERR_UNAVAILABLE: Couldn't unpack incoming message\n"));
      return SA_AIS_ERR_UNAVAILABLE;
    }

  ret = ret_msg->returnval;
  safs_imm_oi_implementer_set_ret__free_unpacked(ret_msg, NULL);

  TRACE_LEAVE();

  return ret;

}

SaAisErrorT saImmOiImplementerClear(SaImmOiHandleT immOiHandle)
{
  SaAisErrorT ret = SA_AIS_OK;
  SaAisErrorT recv_ret = SA_AIS_OK;
  safc_buf_t msgbuf;
  SafsImmOiImplementerClear impl_clear_msg =
    SAFS_IMM_OI_IMPLEMENTER_CLEAR__INIT;
  SafsImmOiMessage msg = SAFS_IMM_OI_MESSAGE__INIT;
  SafsImmOiImplementerClearRet *ret_msg;
  SafcImmOiClientT *client;

  TRACE_ENTER();

  LOCK_RDLOCK(&immOiClients_lock);
  client =
    (SafcImmOiClientT *) safc_array_get(&immOiClients, (int) immOiHandle);
  LOCK_UNLOCK(&immOiClients_lock);
  if (!client)
    return SA_AIS_ERR_BAD_HANDLE;

  /* Coding of message */
  impl_clear_msg.handle = client->srv_handle;
  /* msg.type = SAFS_IMM_OI_MESSAGE__TYPE__OPER_OI_IMPLEMENTER_CLEAR; */
  msg.implementerclear = &impl_clear_msg;

  msgbuf.size = safs_imm_oi_message__get_packed_size(&msg);
  msgbuf.buf = malloc(msgbuf.size);
  safs_imm_oi_message__pack(&msg, (uint8_t *) msgbuf.buf);

  /* Send message & wait for answer*/
  /* TRACE1(("Sent message length %d on socket %d\n", msgbuf.size,
     client->sockfd)); */
  MUTEX_LOCK(&(client->mutex));
  recv_ret = safc_send_recv(client->sockfd, &msgbuf);
  MUTEX_UNLOCK(&(client->mutex));

  if(recv_ret == SA_AIS_ERR_MESSAGE_ERROR)
  {
     TRACE1(("SA_AIS_ERR_UNAVAILABLE: Couldn't send/read from socket\n"));
     free(msgbuf.buf);
     return SA_AIS_ERR_UNAVAILABLE; // Perhaps another error code;
  }

  /* Decoding of answer */
  ret_msg = safs_imm_oi_implementer_clear_ret__unpack(NULL,
						      msgbuf.size,
						      (uint8_t *) msgbuf.buf);
  free(msgbuf.buf);

  if (ret_msg == NULL)
  {
     TRACE1(("SA_AIS_ERR_UNAVAILABLE: Couldn't unpack incoming message\n"));
     return SA_AIS_ERR_UNAVAILABLE;
  }

  ret = ret_msg->returnval;
  safs_imm_oi_implementer_clear_ret__free_unpacked(ret_msg, NULL);

  TRACE_LEAVE();

  return ret;

}

SaAisErrorT saImmOiClassImplementerSet(SaImmOiHandleT immOiHandle,
				       const SaImmClassNameT className)
{
  SaAisErrorT ret = SA_AIS_OK;
  SaAisErrorT recv_ret = SA_AIS_OK;
  safc_buf_t msgbuf;
  SafsImmOiClassImplementerSet class_impl_set_msg =
    SAFS_IMM_OI_CLASS_IMPLEMENTER_SET__INIT;
  SafsImmOiMessage msg = SAFS_IMM_OI_MESSAGE__INIT;
  SafsImmOiClassImplementerSetRet *ret_msg;
  SafcImmOiClientT *client;

  TRACE_ENTER();

  LOCK_RDLOCK(&immOiClients_lock);
  client =
    (SafcImmOiClientT *) safc_array_get(&immOiClients, (int) immOiHandle);
  LOCK_UNLOCK(&immOiClients_lock);
  if (!client)
    return SA_AIS_ERR_BAD_HANDLE;

  /* Coding of message */
  class_impl_set_msg.handle = client->srv_handle;
  class_impl_set_msg.classname = className;

  msg.classimplementerset = &class_impl_set_msg;

  msgbuf.size = safs_imm_oi_message__get_packed_size(&msg);
  msgbuf.buf = malloc(msgbuf.size);
  safs_imm_oi_message__pack(&msg, (uint8_t *) msgbuf.buf);

  /* Send message & wait for answer*/
  /* TRACE1(("Sent message length %d on socket %d\n", msgbuf.size,
     client->sockfd)); */
  MUTEX_LOCK(&(client->mutex));
  recv_ret =safc_send_recv(client->sockfd, &msgbuf);
  MUTEX_UNLOCK(&(client->mutex));

  if(recv_ret == SA_AIS_ERR_MESSAGE_ERROR)
  {
     TRACE1(("SA_AIS_ERR_UNAVAILABLE: Couldn't send/read from socket\n"));
     free(msgbuf.buf);
     return SA_AIS_ERR_UNAVAILABLE; // Perhaps another error code;
  }

  /* Decoding of answer */
  ret_msg =
    safs_imm_oi_class_implementer_set_ret__unpack(NULL,
						  msgbuf.size,
						  (uint8_t *) msgbuf.buf);
  free(msgbuf.buf);

  if (ret_msg == NULL)
    {
      TRACE1(("SA_AIS_ERR_UNAVAILABLE: Couldn't unpack incoming message\n"));
      return SA_AIS_ERR_UNAVAILABLE;
    }

  ret = ret_msg->returnval;
  safs_imm_oi_class_implementer_set_ret__free_unpacked(ret_msg, NULL);

  TRACE_LEAVE();

  return ret;

}

SaAisErrorT saImmOiClassImplementerRelease(SaImmOiHandleT immOiHandle,
					   const SaImmClassNameT className)
{
  SaAisErrorT ret = SA_AIS_OK;
  SaAisErrorT recv_ret = SA_AIS_OK;
  safc_buf_t msgbuf;
  SafsImmOiClassImplementerRelease class_impl_rel_msg =
    SAFS_IMM_OI_CLASS_IMPLEMENTER_RELEASE__INIT;
  SafsImmOiMessage msg = SAFS_IMM_OI_MESSAGE__INIT;
  SafsImmOiClassImplementerReleaseRet *ret_msg;
  SafcImmOiClientT *client;

  TRACE_ENTER();

  LOCK_RDLOCK(&immOiClients_lock);
  client =
    (SafcImmOiClientT *) safc_array_get(&immOiClients, (int) immOiHandle);
  LOCK_UNLOCK(&immOiClients_lock);
  if (!client)
    return SA_AIS_ERR_BAD_HANDLE;

  /* Coding of message */
  class_impl_rel_msg.handle = client->srv_handle;
  class_impl_rel_msg.classname = className;

  msg.classimplementerrelease = &class_impl_rel_msg;

  msgbuf.size = safs_imm_oi_message__get_packed_size(&msg);
  msgbuf.buf = malloc(msgbuf.size);
  safs_imm_oi_message__pack(&msg, (uint8_t *) msgbuf.buf);

  /* Send message & wait for answer*/
  /* TRACE1(("Sent message length %d on socket %d\n", msgbuf.size,
     client->sockfd)); */
  MUTEX_LOCK(&(client->mutex));
  recv_ret = safc_send_recv(client->sockfd, &msgbuf);
  MUTEX_UNLOCK(&(client->mutex));

   if(recv_ret == SA_AIS_ERR_MESSAGE_ERROR)
   {
      TRACE1(("SA_AIS_ERR_UNAVAILABLE: Couldn't send/read from socket\n"));
      free(msgbuf.buf);
      return SA_AIS_ERR_UNAVAILABLE; // Perhaps another error code;
   }

  /* Decoding of answer */
  ret_msg =
    safs_imm_oi_class_implementer_release_ret__unpack(NULL,
						      msgbuf.size,
						      (uint8_t *) msgbuf.buf);
  free(msgbuf.buf);

  if (ret_msg == NULL)
  {
     TRACE1(("SA_AIS_ERR_UNAVAILABLE: Couldn't unpack incoming message\n"));
     return SA_AIS_ERR_UNAVAILABLE;
  }

  ret = ret_msg->returnval;
  safs_imm_oi_class_implementer_release_ret__free_unpacked(ret_msg, NULL);

  TRACE_LEAVE();

  return ret;

}

SaAisErrorT saImmOiObjectImplementerSet(SaImmOiHandleT immOiHandle,
					const SaNameT *objectName,
					SaImmScopeT scope)
{
  SaAisErrorT ret = SA_AIS_OK;
  SaAisErrorT recv_ret = SA_AIS_OK;
  safc_buf_t msgbuf;
  SafsImmOiObjectImplementerSet obj_impl_set_msg =
    SAFS_IMM_OI_OBJECT_IMPLEMENTER_SET__INIT;
  SafsImmOiMessage msg = SAFS_IMM_OI_MESSAGE__INIT;
  SafsImmOiObjectImplementerSetRet *ret_msg;
  SafcImmOiClientT *client;

  TRACE_ENTER();

  switch (scope) {
  case SA_IMM_ONE:
  case SA_IMM_SUBLEVEL:
  case SA_IMM_SUBTREE:
    break;
  default:
    return SA_AIS_ERR_INVALID_PARAM;
  }

  if (objectName == NULL || (objectName->length == 0) || (objectName->length >= SA_MAX_NAME_LENGTH)) {
     TRACE1(("ERR_INVALID_PARAM: objectname"));
     return SA_AIS_ERR_INVALID_PARAM;
  }

  LOCK_RDLOCK(&immOiClients_lock);
  client =
    (SafcImmOiClientT *) safc_array_get(&immOiClients, (int) immOiHandle);
  LOCK_UNLOCK(&immOiClients_lock);
  if (!client)
    return SA_AIS_ERR_BAD_HANDLE;

  /* Coding of message */
  obj_impl_set_msg.handle = client->srv_handle;
  obj_impl_set_msg.objectname = calloc(1, sizeof(SaUint8T) * (objectName->length + 1));      
  (void)memcpy(obj_impl_set_msg.objectname, objectName->value, objectName->length);
  obj_impl_set_msg.objectname[objectName->length] = '\0';
  obj_impl_set_msg.scope = scope;

  msg.objectimplementerset = &obj_impl_set_msg;

  msgbuf.size = safs_imm_oi_message__get_packed_size(&msg);
  msgbuf.buf = malloc(msgbuf.size);
  safs_imm_oi_message__pack(&msg, (uint8_t *) msgbuf.buf);

  free(obj_impl_set_msg.objectname);

  /* Send message & wait for answer*/
  /* TRACE1(("Sent message length %d on socket %d\n", msgbuf.size,
     client->sockfd)); */
  MUTEX_LOCK(&(client->mutex));
  recv_ret = safc_send_recv(client->sockfd, &msgbuf);
  MUTEX_UNLOCK(&(client->mutex));

  if(recv_ret == SA_AIS_ERR_MESSAGE_ERROR)
  {
     TRACE1(("SA_AIS_ERR_UNAVAILABLE: Couldn't send/read from socket\n"));
     free(msgbuf.buf);
     return SA_AIS_ERR_UNAVAILABLE; // Perhaps another error code;
  }

  /* Decoding of answer */
  ret_msg =
    safs_imm_oi_object_implementer_set_ret__unpack(NULL,
						   msgbuf.size,
						   (uint8_t *) msgbuf.buf);
  free(msgbuf.buf);

  if (ret_msg == NULL)
    {
      TRACE1(("SA_AIS_ERR_UNAVAILABLE: Couldn't unpack incoming message\n"));
      return SA_AIS_ERR_UNAVAILABLE;
    }

  ret = ret_msg->returnval;
  safs_imm_oi_object_implementer_set_ret__free_unpacked(ret_msg, NULL);

  TRACE_LEAVE();

  return ret;

}

SaAisErrorT saImmOiObjectImplementerRelease(SaImmOiHandleT immOiHandle,
					    const SaNameT *objectName,
					    SaImmScopeT scope)
{
  SaAisErrorT ret = SA_AIS_OK;
  SaAisErrorT recv_ret = SA_AIS_OK;
  safc_buf_t msgbuf;
  SafsImmOiObjectImplementerRelease obj_impl_rel_msg =
    SAFS_IMM_OI_OBJECT_IMPLEMENTER_RELEASE__INIT;
  SafsImmOiMessage msg = SAFS_IMM_OI_MESSAGE__INIT;
  SafsImmOiObjectImplementerReleaseRet *ret_msg;
  SafcImmOiClientT *client;

  TRACE_ENTER();

  switch (scope) {
  case SA_IMM_ONE:
  case SA_IMM_SUBLEVEL:
  case SA_IMM_SUBTREE:
    break;
  default:
    return SA_AIS_ERR_INVALID_PARAM;
  }

  if (objectName == NULL || (objectName->length == 0) || (objectName->length >= SA_MAX_NAME_LENGTH)) {
     TRACE1(("ERR_INVALID_PARAM: objectname"));
     return SA_AIS_ERR_INVALID_PARAM;
  }

  LOCK_RDLOCK(&immOiClients_lock);
  client =
    (SafcImmOiClientT *) safc_array_get(&immOiClients, (int) immOiHandle);
  LOCK_UNLOCK(&immOiClients_lock);
  if (!client)
    return SA_AIS_ERR_BAD_HANDLE;

  /* Coding of message */
  obj_impl_rel_msg.handle = client->srv_handle;
  obj_impl_rel_msg.objectname = calloc(1, sizeof(SaUint8T) * (objectName->length + 1));      
  (void)memcpy(obj_impl_rel_msg.objectname, objectName->value, objectName->length);
  obj_impl_rel_msg.objectname[objectName->length] = '\0';
  obj_impl_rel_msg.scope = scope;

  msg.objectimplementerrelease = &obj_impl_rel_msg;

  msgbuf.size = safs_imm_oi_message__get_packed_size(&msg);
  msgbuf.buf = malloc(msgbuf.size);
  safs_imm_oi_message__pack(&msg, (uint8_t *) msgbuf.buf);

  free(obj_impl_rel_msg.objectname);

  /* Send message & wait for answer*/
  /* TRACE1(("Sent message length %d on socket %d\n", msgbuf.size,
     client->sockfd)); */
  MUTEX_LOCK(&(client->mutex));
  recv_ret = safc_send_recv(client->sockfd, &msgbuf);
  MUTEX_UNLOCK(&(client->mutex));

  if(recv_ret == SA_AIS_ERR_MESSAGE_ERROR)
  {
     TRACE1(("SA_AIS_ERR_UNAVAILABLE: Couldn't send/read from socket\n"));
     free(msgbuf.buf);
     return SA_AIS_ERR_UNAVAILABLE; // Perhaps another error code;
  }

  /* Decoding of answer */
  ret_msg =
    safs_imm_oi_object_implementer_release_ret__unpack(NULL,
						       msgbuf.size,
						       (uint8_t *) msgbuf.buf);
  free(msgbuf.buf);

  if (ret_msg == NULL)
    {
      TRACE1(("SA_AIS_ERR_UNAVAILABLE: Couldn't unpack incoming message\n"));
      return SA_AIS_ERR_UNAVAILABLE;
    }

  ret = ret_msg->returnval;
  safs_imm_oi_object_implementer_release_ret__free_unpacked(ret_msg, NULL);

  TRACE_LEAVE();

  return ret;

}


/*
 * ======================================================================
 * 5.5 Runtime Objects Management
 * ======================================================================
 */

SaAisErrorT saImmOiRtObjectCreate_2(SaImmOiHandleT immOiHandle,
				    const SaImmClassNameT className,
				    const SaNameT *parentName,
				    const SaImmAttrValuesT_2 **attrValues)
{
  SaAisErrorT ret = SA_AIS_OK;
  SaAisErrorT recv_ret = SA_AIS_OK;
  SafcImmOiClientT *client=NULL;
  safc_buf_t msgbuf;
  SafsImmOiRtObjectCreate2 rt_obj_create_msg =
     SAFS_IMM_OI_RT_OBJECT_CREATE_2__INIT;
  SafsImmOiMessage msg = SAFS_IMM_OI_MESSAGE__INIT;
  SafsImmOiRtObjectCreate2Ret *ret_msg;
  const SaImmAttrValuesT_2* attribute;
  /* SafsImmAttrValue** avals; */
  int i;
  /* const SaImmAttrValueT *attributeValues; */

  TRACE_ENTER();

  if (className == NULL) {
     TRACE1(("ERR_INVALID_PARAM: classname is NULL"));
     return SA_AIS_ERR_INVALID_PARAM;
  }

  if (attrValues == NULL) {
     TRACE1(("ERR_INVALID_PARAM: attrValues is NULL"));
     return SA_AIS_ERR_INVALID_PARAM;
  }

  if (parentName && parentName->length >= SA_MAX_NAME_LENGTH) {
     TRACE1(("ERROR: parentName->length >= SA_MAX_NAME_LENGTH\n"));
     return SA_AIS_ERR_NAME_TOO_LONG;
  }

  LOCK_RDLOCK(&immOiClients_lock);
  client =
    (SafcImmOiClientT *) safc_array_get(&immOiClients, (int) immOiHandle);
  LOCK_UNLOCK(&immOiClients_lock);
  if (!client)
    return SA_AIS_ERR_BAD_HANDLE;

  /* Coding of message */
  rt_obj_create_msg.handle = client->srv_handle;
  rt_obj_create_msg.classname = (char*) className;

  if(parentName != NULL) {
      assert(parentName->length < SA_MAX_NAME_LENGTH);
      rt_obj_create_msg.parentname = calloc(1, sizeof(SaUint8T) * (parentName->length + 1));
      (void)memcpy(rt_obj_create_msg.parentname,  parentName->value, parentName->length);
      rt_obj_create_msg.parentname[parentName->length] = '\0';
  }

   /* Calculate number of attributes */
   for (i = 0; attrValues[i]; i++);

   rt_obj_create_msg.n_attrvalues = i;
   rt_obj_create_msg.attrvalues =
      calloc (1, sizeof (SafsImmAttrValues2*) * rt_obj_create_msg.n_attrvalues);

   for (i = 0; i < rt_obj_create_msg.n_attrvalues; i++) {
      attribute = attrValues[i];
      safc_copy_from_imm_attributes(attribute, &(rt_obj_create_msg.attrvalues[i]));


      /* attributeValues = attribute->attrValues; */

      /* rt_obj_create_msg.attrvalues[i] = calloc (1, sizeof (SafsImmAttrValues2)); */
      /* safs_imm_attr_values_2__init(rt_obj_create_msg.attrvalues[i]); */

      /* rt_obj_create_msg.attrvalues[i]->attrname = (char*) attribute->attrName; */
      /* rt_obj_create_msg.attrvalues[i]->attrvaluetype = attribute->attrValueType; */
      /* rt_obj_create_msg.attrvalues[i]->attrvaluesnumber = attribute->attrValuesNumber; */

      /* rt_obj_create_msg.attrvalues[i]->attrvalues = */
      /* 	 calloc (1, sizeof (SafsImmAttrValue*) * attribute->attrValuesNumber); */
      /* rt_obj_create_msg.attrvalues[i]->n_attrvalues = attribute->attrValuesNumber; */

      /* avals = rt_obj_create_msg.attrvalues[i]->attrvalues; */

      /* for (j = 0; j < attribute->attrValuesNumber; j++) { */
      /* 	 avals[j] = calloc (1, sizeof (SafsImmAttrValue)); */
      /* 	 safs_imm_attr_value__init(avals[j]); */

      /* 	 safc_copy_from_imm_attr_value(avals[j], attribute->attrValueType, attributeValues[j]); */

   }

  msg.rtobjectcreate_2 = &rt_obj_create_msg;

  msgbuf.size = safs_imm_oi_message__get_packed_size(&msg);
  msgbuf.buf = malloc(msgbuf.size);
  safs_imm_oi_message__pack(&msg, (uint8_t *) msgbuf.buf);

  safc_free_rt_oc2_msg(rt_obj_create_msg);

  /* Send message & wait for answer*/
  MUTEX_LOCK(&(client->mutex));
  recv_ret = safc_send_recv(client->sockfd, &msgbuf);
  MUTEX_UNLOCK(&(client->mutex));

  if(recv_ret == SA_AIS_ERR_MESSAGE_ERROR)
  {
     TRACE1(("SA_AIS_ERR_UNAVAILABLE: Couldn't send/read from socket\n"));
     free(msgbuf.buf);
     return SA_AIS_ERR_UNAVAILABLE; // Perhaps another error code;
  }

  /* Decoding of answer */
  ret_msg =
    safs_imm_oi_rt_object_create2_ret__unpack(NULL,
					       msgbuf.size,
					       (uint8_t *) msgbuf.buf);
  free(msgbuf.buf);

  if (ret_msg == NULL)
    {
      TRACE1(("SA_AIS_ERR_UNAVAILABLE: Couldn't unpack incoming message\n"));
      return SA_AIS_ERR_UNAVAILABLE;
    }

  ret = ret_msg->returnval;
  safs_imm_oi_rt_object_create2_ret__free_unpacked(ret_msg, NULL);

  TRACE_LEAVE();

  return ret;
}

SaAisErrorT saImmOiRtObjectDelete(SaImmOiHandleT immOiHandle,
				  const SaNameT *objectName)
{
  SaAisErrorT ret = SA_AIS_OK;
  SaAisErrorT recv_ret = SA_AIS_OK;
  SafcImmOiClientT *client=NULL;
  safc_buf_t msgbuf;
  SafsImmOiRtObjectDelete rt_obj_delete_msg =
     SAFS_IMM_OI_RT_OBJECT_DELETE__INIT;
  SafsImmOiMessage msg = SAFS_IMM_OI_MESSAGE__INIT;
  SafsImmOiRtObjectDeleteRet *ret_msg;

  TRACE_ENTER();

  if (objectName == NULL || (objectName->length == 0) || (objectName->length >= SA_MAX_NAME_LENGTH)) {
     TRACE1(("ERR_INVALID_PARAM: objectname"));
     return SA_AIS_ERR_INVALID_PARAM;
  }

  LOCK_RDLOCK(&immOiClients_lock);
  client =
    (SafcImmOiClientT *) safc_array_get(&immOiClients, (int) immOiHandle);
  LOCK_UNLOCK(&immOiClients_lock);
  if (!client)
    return SA_AIS_ERR_BAD_HANDLE;

  /* Coding of message */
  rt_obj_delete_msg.handle = client->srv_handle;
  assert(objectName->length < SA_MAX_NAME_LENGTH);
  rt_obj_delete_msg.objectname = calloc(1, sizeof(SaUint8T) * (objectName->length + 1));      
  (void)memcpy(rt_obj_delete_msg.objectname, objectName->value, objectName->length);
  rt_obj_delete_msg.objectname[objectName->length] = '\0';

  msg.rtobjectdelete = &rt_obj_delete_msg;

  msgbuf.size = safs_imm_oi_message__get_packed_size(&msg);
  msgbuf.buf = malloc(msgbuf.size);
  safs_imm_oi_message__pack(&msg, (uint8_t *) msgbuf.buf);

  free(rt_obj_delete_msg.objectname);

  /* Send message & wait for answer*/
  MUTEX_LOCK(&(client->mutex));
  recv_ret = safc_send_recv(client->sockfd, &msgbuf);
  MUTEX_UNLOCK(&(client->mutex));

  if(recv_ret == SA_AIS_ERR_MESSAGE_ERROR)
  {
     TRACE1(("SA_AIS_ERR_UNAVAILABLE: Couldn't send/read from socket\n"));
     free(msgbuf.buf);
     return SA_AIS_ERR_UNAVAILABLE; // Perhaps another error code;
  }

  /* Decoding of answer */
 ret_msg =
    safs_imm_oi_rt_object_delete_ret__unpack(NULL,
					       msgbuf.size,
					       (uint8_t *) msgbuf.buf);
  free(msgbuf.buf);

  if (ret_msg == NULL)
    {
      TRACE1(("SA_AIS_ERR_UNAVAILABLE: Couldn't unpack incoming message\n"));
      return SA_AIS_ERR_UNAVAILABLE;
    }

  ret = ret_msg->returnval;
  safs_imm_oi_rt_object_delete_ret__free_unpacked(ret_msg, NULL);

  TRACE_LEAVE();

  return ret;
;
}

SaAisErrorT saImmOiRtObjectUpdate_2(SaImmOiHandleT immOiHandle,
				    const SaNameT *objectName,
				    const SaImmAttrModificationT_2 **attrMods)
{
  SaAisErrorT ret = SA_AIS_OK;
  SaAisErrorT recv_ret = SA_AIS_OK;
  SafcImmOiClientT *client=NULL;
  safc_buf_t msgbuf;
  SafsImmOiRtObjectUpdate2 rt_obj_update_msg =
     SAFS_IMM_OI_RT_OBJECT_UPDATE_2__INIT;
  SafsImmOiMessage msg = SAFS_IMM_OI_MESSAGE__INIT;
  SafsImmOiRtObjectUpdate2Ret *ret_msg;

  /* SafsImmAttrValue** avals; */
  int i;
  /* const SaImmAttrValueT *attributeValues; */

  TRACE_ENTER();

  if (objectName == NULL || (objectName->length == 0) || (objectName->length >= SA_MAX_NAME_LENGTH)) {
     TRACE1(("ERR_INVALID_PARAM: objectname"));
     return SA_AIS_ERR_INVALID_PARAM;
  }

  if (attrMods == NULL) {
     TRACE1(("ERR_INVALID_PARAM: attrMods is NULL"));
     return SA_AIS_ERR_INVALID_PARAM;
  }

  /* if (objectName && objectName->length >= SA_MAX_NAME_LENGTH) { */
  /*    TRACE1(("ERROR: objectName->length > SA_MAX_NAME_LENGTH\n")); */
  /*    return SA_AIS_ERR_NAME_TOO_LONG; */
  /* } */

  LOCK_RDLOCK(&immOiClients_lock);
  client =
    (SafcImmOiClientT *) safc_array_get(&immOiClients, (int) immOiHandle);
  LOCK_UNLOCK(&immOiClients_lock);
  if (!client)
    return SA_AIS_ERR_BAD_HANDLE;

  /* Coding of message */
  rt_obj_update_msg.handle = client->srv_handle;

  rt_obj_update_msg.objectname = calloc(1, sizeof(SaUint8T) * (objectName->length + 1));
  (void)memcpy(rt_obj_update_msg.objectname,  objectName->value, objectName->length);
  rt_obj_update_msg.objectname[objectName->length] = '\0';

   /* Calculate number of attributes */
   for (i = 0; attrMods[i]; i++);

   rt_obj_update_msg.n_attrmods = i;
   rt_obj_update_msg.attrmods =
      calloc (1, sizeof (SafsImmAttrModification2*) * rt_obj_update_msg.n_attrmods);

   for (i = 0; i < rt_obj_update_msg.n_attrmods; i++) {
      /* attributeValues = attrMods[i]->modAttr.attrValues; */
      rt_obj_update_msg.attrmods[i] = calloc (1, sizeof (SafsImmAttrModification2));
      safs_imm_attr_modification_2__init(rt_obj_update_msg.attrmods[i]);

      rt_obj_update_msg.attrmods[i]->modtype = attrMods[i]->modType;

      safc_copy_from_imm_attributes(&(attrMods[i]->modAttr), &(rt_obj_update_msg.attrmods[i]->modattr));

      /* rt_obj_update_msg.attrmods[i]->modattr = calloc (1, sizeof (SafsImmAttrValues2)); */
      /* safs_imm_attr_values_2__init(rt_obj_update_msg.attrmods[i]->modattr); */

      /* rt_obj_update_msg.attrmods[i]->modattr->attrname = attrMods[i]->modAttr.attrName; */
      /* rt_obj_update_msg.attrmods[i]->modattr->attrvaluetype = attrMods[i]->modAttr.attrValueType; */
      /* rt_obj_update_msg.attrmods[i]->modattr->attrvaluesnumber = attrMods[i]->modAttr.attrValuesNumber; */

      /* rt_obj_update_msg.attrmods[i]->modattr->attrvalues = */
      /* 	 calloc (1, sizeof (SafsImmAttrValue*) * attrMods[i]->modAttr.attrValuesNumber); */
      /* rt_obj_update_msg.attrmods[i]->modattr->n_attrvalues = attrMods[i]->modAttr.attrValuesNumber; */

      /* avals = rt_obj_update_msg.attrmods[i]->modattr->attrvalues; */

      /* for (j = 0; j < attrMods[i]->modAttr.attrValuesNumber; j++) { */
      /* 	 avals[j] = calloc (1, sizeof (SafsImmAttrValue)); */
      /* 	 safs_imm_attr_value__init(avals[j]); */

      /* 	 safc_copy_from_imm_attr_value(avals[j], attrMods[i]->modAttr.attrValueType, */
      /* 				       attributeValues[j]); */

      /* } */
   }

  msg.rtobjectupdate_2 = &rt_obj_update_msg;

  msgbuf.size = safs_imm_oi_message__get_packed_size(&msg);
  msgbuf.buf = malloc(msgbuf.size);
  safs_imm_oi_message__pack(&msg, (uint8_t *) msgbuf.buf);

  safc_free_rt_ou2_msg(rt_obj_update_msg);

  /* Send message & wait for answer*/
  MUTEX_LOCK(&(client->mutex));
  recv_ret = safc_send_recv(client->sockfd, &msgbuf);
  MUTEX_UNLOCK(&(client->mutex));

  if(recv_ret == SA_AIS_ERR_MESSAGE_ERROR)
  {
     TRACE1(("SA_AIS_ERR_UNAVAILABLE: Couldn't send/read from socket\n"));
     free(msgbuf.buf);
     return SA_AIS_ERR_UNAVAILABLE; // Perhaps another error code;
   }

  /* Decoding of answer */
 ret_msg =
    safs_imm_oi_rt_object_update2_ret__unpack(NULL,
					       msgbuf.size,
					       (uint8_t *) msgbuf.buf);
  free(msgbuf.buf);

  if (ret_msg == NULL)
    {
      TRACE1(("SA_AIS_ERR_UNAVAILABLE: Couldn't unpack incoming message\n"));
      return SA_AIS_ERR_UNAVAILABLE;
    }

  ret = ret_msg->returnval;
  safs_imm_oi_rt_object_update2_ret__free_unpacked(ret_msg, NULL);

  TRACE_LEAVE();

  return ret;
}

/* SaImmOiRtAttrUpdateCallback */

/*
 * ======================================================================
 * 5.6 Configuration Objects Implementer
 * ======================================================================
 */

/* SaImmOiCcbObjectCreateCallback */
static SaAisErrorT ccbObjCreateCb(SaImmOiHandleT immOiHandle,
				  SaImmOiCcbObjectCreateCallback2 objCreateData,
				  SaImmOiCcbObjectCreateCallbackT_2 *objCreateCb)
{
  SaAisErrorT ret = SA_AIS_OK;
  SaImmAttrValuesT_2 **attributes;
  /* const SafsImmAttrValues2* avals = NULL; */
  SaNameT *parentname;
  int i, augmentHandle;
  SafcImmOiAugmentedCcbStructT* augment_ptr=NULL;

  TRACE_ENTER();

  attributes =
    calloc (1, sizeof (SaImmAttrValuesT_2*) * (objCreateData.n_attr+1));

  for (i = 0; i < objCreateData.n_attr; i++)
    {
      /* avals = objCreateData.attr[i]; */

      attributes[i] = calloc (1, sizeof (SaImmAttrValuesT_2));
      safc_copy_to_imm_attributes(objCreateData.attr[i], attributes[i]);
      
      /* attributes[i]->attrName = avals->attrname; */
      /* attributes[i]->attrValueType = avals->attrvaluetype; */
      /* attributes[i]->attrValuesNumber = avals->attrvaluesnumber; */
      /* attributes[i]->attrValues = */
      /* 	calloc (1, sizeof (SaImmAttrValueT*) * avals->attrvaluesnumber); */

      /* safs_to_sa_copy_attr_values(attributes[i]->attrValues, */
      /* 				  avals->attrvaluetype, avals); */
    }

  attributes[i] = NULL;

  parentname = calloc(1, sizeof(SaNameT));
  parentname->length = strlen(objCreateData.parentname);
  (void)memcpy(parentname->value, objCreateData.parentname,
	       parentname->length);
  TRACE2(("Call Create Object Callback\n"));
  ret = (*objCreateCb)(immOiHandle,
		       objCreateData.ccbid,
		       objCreateData.classname,
		       parentname,
		       (const SaImmAttrValuesT_2 **)attributes);
  TRACE2(("Returned from Create Object Callbac: %ul\n", ret));

  free(parentname);

  safc_free_imm_attributes(attributes);

  TRACE2(("Cb parameter memeory deallocated\n"));

  /* Check if there is an augmentation to finalize */  
  LOCK_RDLOCK(&immAugmentCcbs_lock);
  augmentHandle = safc_array_find(&immAugmentCcbs, &(objCreateData.ccbid));
  LOCK_UNLOCK(&immAugmentCcbs_lock);

  TRACE2(("Check Augmentation\n"));

  if (augmentHandle != 0 ) {    
     TRACE2(("Found Augmentation handle\n"));
     LOCK_RDLOCK(&immAugmentCcbs_lock);
     augment_ptr = (SafcImmOiAugmentedCcbStructT *) safc_array_get(&immAugmentCcbs, augmentHandle);
     LOCK_UNLOCK(&immAugmentCcbs_lock);

     (void) saImmOmFinalize(augment_ptr->immHandle);
     TRACE2(("Augmentation finalized\n"));

     LOCK_WRLOCK(&immAugmentCcbs_lock);
     safc_array_remove(&immAugmentCcbs, augmentHandle);
     LOCK_UNLOCK(&immAugmentCcbs_lock);
     free(augment_ptr);
  }

  TRACE_LEAVE();

  return ret;
}

/* SaImmOiCcbObjectDeleteCallback */
static SaAisErrorT ccbObjDeleteCb(SaImmOiHandleT immOiHandle,
				  SaImmOiCcbObjectDeleteCallback objDeleteData,
				  SaImmOiCcbObjectDeleteCallbackT *objDeleteCb)
{
  SaNameT *objname;
  SaAisErrorT ret = SA_AIS_OK;
  int augmentHandle;
  SafcImmOiAugmentedCcbStructT* augment_ptr=NULL;

  TRACE_ENTER();

  objname = calloc(1, sizeof(SaNameT));
  objname->length = strlen(objDeleteData.objectname);
  (void)memcpy(objname->value, objDeleteData.objectname,
	       objname->length);

  ret = (*objDeleteCb)(immOiHandle, objDeleteData.ccbid, objname);

  free(objname),

  /* Check if there is an augmentation to finalize */  
  LOCK_RDLOCK(&immAugmentCcbs_lock);
  augmentHandle = safc_array_find(&immAugmentCcbs, &(objDeleteData.ccbid));
  LOCK_UNLOCK(&immAugmentCcbs_lock);

  if (augmentHandle != 0 ) {    
     LOCK_RDLOCK(&immAugmentCcbs_lock);
     augment_ptr = (SafcImmOiAugmentedCcbStructT *) safc_array_get(&immAugmentCcbs, augmentHandle);
     LOCK_UNLOCK(&immAugmentCcbs_lock);

     (void) saImmOmFinalize(augment_ptr->immHandle);
     TRACE2(("Augmentation finalized\n"));

     LOCK_WRLOCK(&immAugmentCcbs_lock);
     safc_array_remove(&immAugmentCcbs, augmentHandle);
     LOCK_UNLOCK(&immAugmentCcbs_lock);
     free(augment_ptr);
  }

  TRACE_LEAVE();

  return ret;
}

/* SaImmOiCcbObjectModifyCallback */
static SaAisErrorT ccbObjModifyCb(SaImmOiHandleT immOiHandle,
				  SaImmOiCcbObjectModifyCallback2 objModifyData,
				  SaImmOiCcbObjectModifyCallbackT_2 *objModifyCb)
{

   SaAisErrorT ret = SA_AIS_OK;
  SaNameT *objName;
  const SafsImmAttrModification2* attrmods;
  const SafsImmAttrValues2* modattr = NULL;
  SaImmAttrModificationT_2 **attrMods;
  int i, j, augmentHandle;
  SafcImmOiAugmentedCcbStructT* augment_ptr=NULL;

  TRACE_ENTER();

  attrMods =
    calloc (1, sizeof (SaImmAttrModificationT_2*) * (objModifyData.n_attrmods+1));

  for (i = 0; i < objModifyData.n_attrmods; i++)
    {
      attrmods = objModifyData.attrmods[i];
      modattr = attrmods->modattr;

      attrMods[i] = calloc (1, sizeof (SaImmAttrModificationT_2));
      attrMods[i]->modType = attrmods->modtype;

      safc_copy_to_imm_attributes(modattr, &(attrMods[i]->modAttr));

    }

  attrMods[i] = NULL;

  objName = calloc(1, sizeof(SaNameT));
  objName->length = strlen(objModifyData.objectname);
  (void)memcpy(objName->value, objModifyData.objectname,
	       objName->length);

  ret = (*objModifyCb)(immOiHandle,
		       objModifyData.ccbid,
		       objName,
		       (const SaImmAttrModificationT_2 **)attrMods);
 
 free(objName);
  for (i = 0; i < objModifyData.n_attrmods; i++)
    {
       for (j = 0; j < attrMods[i]->modAttr.attrValuesNumber; j++)
	  safc_free_imm_attr_value(attrMods[i]->modAttr.attrValueType, 
				   attrMods[i]->modAttr.attrValues[j]);
      free(attrMods[i]->modAttr.attrValues);
      free(attrMods[i]->modAttr.attrName);
      free(attrMods[i]);
    }
  free(attrMods);

  /* Check if there is an augmentation to finalize */  
  LOCK_RDLOCK(&immAugmentCcbs_lock);
  augmentHandle = safc_array_find(&immAugmentCcbs, &(objModifyData.ccbid));
  LOCK_UNLOCK(&immAugmentCcbs_lock);

  if (augmentHandle != 0 ) {    
     LOCK_RDLOCK(&immAugmentCcbs_lock);
     augment_ptr = (SafcImmOiAugmentedCcbStructT *) safc_array_get(&immAugmentCcbs, augmentHandle);
     LOCK_UNLOCK(&immAugmentCcbs_lock);

     (void) saImmOmFinalize(augment_ptr->immHandle);
     TRACE2(("Augmentation finalized\n"));

     LOCK_WRLOCK(&immAugmentCcbs_lock);
     safc_array_remove(&immAugmentCcbs, augmentHandle);
     LOCK_UNLOCK(&immAugmentCcbs_lock);
     free(augment_ptr);
  }

  TRACE_LEAVE();

  return ret;
}

/* SaImmOiCcbCompletedCallback */
static SaAisErrorT ccbCompletedCb(SaImmOiHandleT immOiHandle,
				  SaImmOiCcbCompletedCallback objComplData,
				  SaImmOiCcbCompletedCallbackT *objComplCb)
{
  SaAisErrorT ret = SA_AIS_OK;

  TRACE_ENTER();

  ret = (*objComplCb)(immOiHandle, objComplData.ccbid);

  TRACE_LEAVE();

  return ret;
}

/* SaImmOiCcbApplyCallback */
static SaAisErrorT ccbApplyCb(SaImmOiHandleT immOiHandle,
			      SaImmOiCcbApplyCallback applyData,
			      SaImmOiCcbApplyCallbackT *applyCb)
{
  SaAisErrorT ret = SA_AIS_OK;

  TRACE_ENTER();

  (*applyCb)(immOiHandle, applyData.ccbid);

  TRACE_LEAVE();

  return ret;
}

/* SaImmOiCcbAbortCallback */
static SaAisErrorT ccbAbortCb(SaImmOiHandleT immOiHandle,
			      SaImmOiCcbAbortCallback abortData,
			      SaImmOiCcbAbortCallbackT *abortCb)
{
  SaAisErrorT ret = SA_AIS_OK;

  TRACE_ENTER();

  /* (*abortCb)(abortData.handle, abortData.ccbid); */
  (*abortCb)(immOiHandle, abortData.ccbid);

  TRACE_LEAVE();

  return ret;
}

/* SaImmOiAdminOperationCallback */
static SaAisErrorT adminOpCb(SaImmOiHandleT immOiHandle,
			     SaImmOiAdminOperationCallback2 adminOpData,
			     SaImmOiAdminOperationCallbackT_2 *adminOpCb)
{
  SaNameT *objectname;
  SaImmAdminOperationParamsT_2 **params;
  int i;
  SaAisErrorT ret = SA_AIS_OK;

  TRACE_ENTER();

  TRACE1(("adminOpCb: objectname=%s\n", adminOpData.objectname));
  TRACE1(("adminOpCb: invocation=%llu\n", (long long unsigned int) adminOpData.invocation));
  TRACE1(("adminOpCb: operationId=%llu\n", (long long unsigned int) adminOpData.operationid));
  objectname = calloc(1, sizeof(SaNameT));
  objectname->length = strlen(adminOpData.objectname);
  (void)memcpy(objectname->value, adminOpData.objectname,
	       objectname->length);


  params = calloc(1, sizeof(SaImmAdminOperationParamsT_2*) * (adminOpData.n_params+1));

  for (i = 0; i < adminOpData.n_params; i++) {
     params[i] = calloc(1, sizeof(SaImmAdminOperationParamsT_2));
     params[i]->paramName = adminOpData.params[i]->paramname;
     params[i]->paramType = adminOpData.params[i]->paramtype;
     safs_to_sa_copy_attr_value(&(params[i]->paramBuffer),
      				params[i]->paramType,
     				adminOpData.params[i]->parambuffer);


  }
  params[i] = NULL;

  TRACE1(("adminOpCb: call to user callback\n"));
  (*adminOpCb)(immOiHandle,
	       adminOpData.invocation,
	       objectname,
	       adminOpData.operationid,
	       (const SaImmAdminOperationParamsT_2 **) params);

  for (i = 0; i < adminOpData.n_params; i++) {
     free_sa_attr_value(params[i]->paramBuffer, params[i]->paramType);
     free(params[i]);
  }

  free(params);
  free(objectname);
  TRACE_LEAVE();

  return ret;
}


/* SaImmOiRtAttrUpdateCallback */
static SaAisErrorT rtAttrUpdateCb(SaImmOiHandleT immOiHandle,
				  SaImmOiRtAttrUpdateCallback rtAttrUpdateData,
				  SaImmOiRtAttrUpdateCallbackT *rtAttrUpdateCb)
{
  SaNameT *objname;
  SaImmAttrNameT *attrNames;
  SaAisErrorT ret = SA_AIS_OK;
  int i;

  TRACE_ENTER();

  objname = calloc(1, sizeof(SaNameT));
  objname->length = strlen(rtAttrUpdateData.objectname);
  (void)memcpy(objname->value, rtAttrUpdateData.objectname,
	       objname->length);

  attrNames =  calloc(1, sizeof(SaImmAttrNameT *) * (rtAttrUpdateData.n_attributenames+1));

  for (i = 0; i < rtAttrUpdateData.n_attributenames; i++)
     attrNames[i] = rtAttrUpdateData.attributenames[i];

  attrNames[i] = NULL;

  /* (*rtAttrUpdateCb)(rtAttrUpdateData.handle, rtAttrUpdateData.ccbid); */
  ret = (*rtAttrUpdateCb)(immOiHandle, objname, attrNames);

  free(attrNames);
  free(objname),

  TRACE_LEAVE();

  return ret;
}


SaAisErrorT dispatchOiCallback(SaImmOiHandleT immOiHandle,
			       SaImmOiCallbacks callbacks_msg,
			       SaImmOiCallbacksT_2 callbacks)
{
  SaAisErrorT ret = SA_AIS_OK;

  if (callbacks_msg.ccbobjectcreatecallback_2 != NULL) {
     ret = ccbObjCreateCb(immOiHandle,
			  *callbacks_msg.ccbobjectcreatecallback_2,
			  &callbacks.saImmOiCcbObjectCreateCallback);
  }
  else if (callbacks_msg.ccbobjectdeletecallback != NULL) {
    ret = ccbObjDeleteCb(immOiHandle,
			 *callbacks_msg.ccbobjectdeletecallback,
			 &callbacks.saImmOiCcbObjectDeleteCallback);
  }
  else if (callbacks_msg.ccbobjectmodifycallback_2 != NULL) {
    ret = ccbObjModifyCb(immOiHandle,
			 *callbacks_msg.ccbobjectmodifycallback_2,
			 &callbacks.saImmOiCcbObjectModifyCallback);
  }
  else if (callbacks_msg.ccbcompletedcallback != NULL) {
    ret = ccbCompletedCb(immOiHandle,
			 *callbacks_msg.ccbcompletedcallback,
			 &callbacks.saImmOiCcbCompletedCallback);
  }
  else if (callbacks_msg.ccbapplycallback != NULL) {
    ret = ccbApplyCb(immOiHandle,
		     *callbacks_msg.ccbapplycallback,
		     &callbacks.saImmOiCcbApplyCallback);
  }
  else if (callbacks_msg.ccbabortcallback != NULL) {
    ret = ccbAbortCb(immOiHandle,
		     *callbacks_msg.ccbabortcallback,
		     &callbacks.saImmOiCcbAbortCallback);
  }
  else if (callbacks_msg.adminoperationcallback_2 != NULL) {
    ret = adminOpCb(immOiHandle,
		     *callbacks_msg.adminoperationcallback_2,
		     &callbacks.saImmOiAdminOperationCallback);
  }
  else if (callbacks_msg.rtattrupdatecallback != NULL) {
    ret = rtAttrUpdateCb(immOiHandle,
			 *callbacks_msg.rtattrupdatecallback,
			 &callbacks.saImmOiRtAttrUpdateCallback);
  }


  return ret;

}

int isVoidOiCallback(SaImmOiCallbacks callbacks_msg)
{
   if (callbacks_msg.adminoperationcallback_2 != NULL)
      return 1;
   else
      return 0;
}

/*
 * ======================================================================
 * 5.7 Administrative Operations
 * ======================================================================
 */
/* SaImmOiCcbAdminOperationCallback */

SaAisErrorT saImmOiAdminOperationResult(SaImmOiHandleT immOiHandle,
					SaInvocationT invocation,
					SaAisErrorT result)
{

  SaAisErrorT ret = SA_AIS_OK;
  SaAisErrorT recv_ret = SA_AIS_OK;
  SafcImmOiClientT *client=NULL;
  safc_buf_t msgbuf;
  SafsImmOiAdminOperationResult adm_op_result_msg =
     SAFS_IMM_OI_ADMIN_OPERATION_RESULT__INIT;
  SafsImmOiMessage msg = SAFS_IMM_OI_MESSAGE__INIT;
  SafsImmOiAdminOperationResultRet *ret_msg;

  TRACE_ENTER();

  LOCK_RDLOCK(&immOiClients_lock);
  client =
    (SafcImmOiClientT *) safc_array_get(&immOiClients, (int) immOiHandle);
  LOCK_UNLOCK(&immOiClients_lock);
  if (!client)
    return SA_AIS_ERR_BAD_HANDLE;

  /* Coding of message */
  adm_op_result_msg.handle = client->srv_handle;
  adm_op_result_msg.invocation = invocation;
  adm_op_result_msg.result = result;

  msg.adminoperationresult = &adm_op_result_msg;

  msgbuf.size = safs_imm_oi_message__get_packed_size(&msg);
  msgbuf.buf = malloc(msgbuf.size);
  safs_imm_oi_message__pack(&msg, (uint8_t *) msgbuf.buf);

  /* Send message & wait for answer*/
  MUTEX_LOCK(&(client->mutex));
  recv_ret = safc_send_recv(client->sockfd, &msgbuf);
  MUTEX_UNLOCK(&(client->mutex));

   if(recv_ret == SA_AIS_ERR_MESSAGE_ERROR)
   {
      TRACE1(("SA_AIS_ERR_UNAVAILABLE: Couldn't send/read from socket\n"));
      free(msgbuf.buf);
      return SA_AIS_ERR_UNAVAILABLE; // Perhaps another error code;
   }

  /* Decoding of answer */
  ret_msg =
    safs_imm_oi_admin_operation_result_ret__unpack(NULL,
						   msgbuf.size,
						   (uint8_t *) msgbuf.buf);
  free(msgbuf.buf);

  if (ret_msg == NULL)
    {
      TRACE1(("SA_AIS_ERR_UNAVAILABLE: Couldn't unpack incoming message\n"));
      return SA_AIS_ERR_UNAVAILABLE;
    }

  ret = ret_msg->returnval;
  safs_imm_oi_admin_operation_result_ret__free_unpacked(ret_msg, NULL);

  TRACE_LEAVE();

  return ret;
}


SaAisErrorT saImmOiAdminOperationResult_o2(SaImmOiHandleT immOiHandle,
					   SaInvocationT invocation,
					   SaAisErrorT result,
					   const SaImmAdminOperationParamsT_2 **returnParams)
{
  SaAisErrorT ret = SA_AIS_OK;
  SaAisErrorT recv_ret = SA_AIS_OK;
  SafcImmOiClientT *client=NULL;
  safc_buf_t msgbuf;
  SafsImmOiAdminOperationResultO2 adm_op_result_msg =
     SAFS_IMM_OI_ADMIN_OPERATION_RESULT_O2__INIT;
  SafsImmOiMessage msg = SAFS_IMM_OI_MESSAGE__INIT;
  SafsImmOiAdminOperationResultO2Ret *ret_msg;
  int i;

  TRACE_ENTER();

  LOCK_RDLOCK(&immOiClients_lock);
  client =
    (SafcImmOiClientT *) safc_array_get(&immOiClients, (int) immOiHandle);
  LOCK_UNLOCK(&immOiClients_lock);
  if (!client)
    return SA_AIS_ERR_BAD_HANDLE;

  /* Coding of message */
  adm_op_result_msg.handle = client->srv_handle;
  adm_op_result_msg.invocation = invocation;
  adm_op_result_msg.result = result;

  if (returnParams != NULL) {
     /* Calculate number of parameters*/
     for (i = 0; returnParams[i]; i++);
     adm_op_result_msg.n_returnparams = i;

     adm_op_result_msg.returnparams =
	calloc (1, sizeof (SafsImmAdminOperationParams2*) * adm_op_result_msg.n_returnparams);
     TRACE1(("Number of params: %d\n", i));

     for (i = 0; i < adm_op_result_msg.n_returnparams; i++) {
	adm_op_result_msg.returnparams[i] = calloc (1, sizeof (SafsImmAdminOperationParams2));
	safs_imm_admin_operation_params_2__init(adm_op_result_msg.returnparams[i]);
	adm_op_result_msg.returnparams[i]->paramname = returnParams[i]->paramName;
	adm_op_result_msg.returnparams[i]->paramtype = returnParams[i]->paramType;
	if(returnParams[i]->paramBuffer) {
	   adm_op_result_msg.returnparams[i]->parambuffer = calloc (1, sizeof (SafsImmAttrValue));
	   safs_imm_attr_value__init(adm_op_result_msg.returnparams[i]->parambuffer);
	   safc_copy_from_imm_attr_value(adm_op_result_msg.returnparams[i]->parambuffer,
					 returnParams[i]->paramType,
					 returnParams[i]->paramBuffer);
	} else
	   adm_op_result_msg.returnparams[i]->parambuffer = NULL;
     }

  }


  msg.adminoperationresult_o2 = &adm_op_result_msg;

  msgbuf.size = safs_imm_oi_message__get_packed_size(&msg);
  msgbuf.buf = malloc(msgbuf.size);
  safs_imm_oi_message__pack(&msg, (uint8_t *) msgbuf.buf);

  safc_free_admop_params(adm_op_result_msg.n_returnparams, adm_op_result_msg.returnparams);

  /* Send message & wait for answer*/
  MUTEX_LOCK(&(client->mutex));
  recv_ret = safc_send_recv(client->sockfd, &msgbuf);
  MUTEX_UNLOCK(&(client->mutex));

  if(recv_ret == SA_AIS_ERR_MESSAGE_ERROR)
  {
     TRACE1(("SA_AIS_ERR_UNAVAILABLE: Couldn't send/read from socket\n"));
     free(msgbuf.buf);
     return SA_AIS_ERR_UNAVAILABLE; // Perhaps another error code;
  }

  /* Decoding of answer */
  ret_msg =
    safs_imm_oi_admin_operation_result_o2_ret__unpack(NULL,
						   msgbuf.size,
						   (uint8_t *) msgbuf.buf);
  free(msgbuf.buf);

  if (ret_msg == NULL)
    {

      TRACE1(("SA_AIS_ERR_UNAVAILABLE: Couldn't unpack incoming message\n"));
      return SA_AIS_ERR_UNAVAILABLE;
    }

  ret = ret_msg->returnval;

  safs_imm_oi_admin_operation_result_o2_ret__free_unpacked(ret_msg, NULL);

  TRACE_LEAVE();

  return ret;
}

/*
 * ======================================================================
 * Augmented Ccb's
 * ======================================================================
 */
SaAisErrorT saImmOiAugmentCcbInitialize(SaImmOiHandleT immOiHandle,
					SaImmOiCcbIdT ccbId,
					SaImmCcbHandleT *ccbHandle,
					SaImmAdminOwnerHandleT *ownerHandle)
{
   SaAisErrorT ret = SA_AIS_OK,
      initRet = SA_AIS_OK;
   SaAisErrorT recv_ret = SA_AIS_OK;
   SafcImmOiClientT *client = NULL;
   safc_buf_t msgbuf;
   SafsImmOiAugmentCcbInitialize augment_ccb_initialize_msg =
      SAFS_IMM_OI_AUGMENT_CCB_INITIALIZE__INIT;
   SafsImmOiMessage msg = SAFS_IMM_OI_MESSAGE__INIT;
   SafsImmOiAugmentCcbInitializeRet *ret_msg;

   SaVersionT version = {'A',  /* LATH: get version from oi instead */
			 0x02,
			 0x0b};
   SaImmHandleT immHandle; 
   SafcImmAdminOwnerClientT *ao_ptr;
   SafcImmCcbClientT *ccb_ptr;
   SafcImmOiAugmentedCcbStructT *augment_ptr;


   TRACE_ENTER();

   if (ccbHandle == NULL) {
      TRACE1(("ERR_INVALID_PARAM: 'ccbHandle is NULL"));
      TRACE_LEAVE();
      return SA_AIS_ERR_INVALID_PARAM;
   }

   if (ownerHandle == NULL) {
      TRACE1(("ERR_INVALID_PARAM: 'ownerHandle is NULL"));
      TRACE_LEAVE();
      return SA_AIS_ERR_INVALID_PARAM;
   }

   if(ccbId == 0) {
      TRACE1(("Invalid ccbId, must not be zero"));
      TRACE_LEAVE();
      return SA_AIS_ERR_INVALID_PARAM;
   }

   *ccbHandle = 0;
   *ownerHandle = 0;

   LOCK_RDLOCK(&immOiClients_lock);
   client =
      (SafcImmOiClientT *) safc_array_get(&immOiClients, (int) immOiHandle);
   LOCK_UNLOCK(&immOiClients_lock);
   if (!client)
      return SA_AIS_ERR_BAD_HANDLE;
   
   /* Coding of message */
   augment_ccb_initialize_msg.handle = client->srv_handle;
   augment_ccb_initialize_msg.ccbid = ccbId;
   
   msg.augmentccbinitialize = &augment_ccb_initialize_msg;
   
   msgbuf.size = safs_imm_oi_message__get_packed_size(&msg);
   msgbuf.buf = malloc(msgbuf.size);
   safs_imm_oi_message__pack(&msg, (uint8_t *) msgbuf.buf);
   
   /* Send message & wait for answer*/
   MUTEX_LOCK(&(client->mutex));
   recv_ret = safc_send_recv(client->sockfd, &msgbuf);
   MUTEX_UNLOCK(&(client->mutex));
   
   if(recv_ret == SA_AIS_ERR_MESSAGE_ERROR)
   {
      TRACE1(("SA_AIS_ERR_UNAVAILABLE: Couldn't send/read from socket\n"));
      free(msgbuf.buf);
      return SA_AIS_ERR_UNAVAILABLE; // Perhaps another error code;
   }
   
   /* Decoding of answer */
   ret_msg =
      safs_imm_oi_augment_ccb_initialize_ret__unpack(NULL,
						     msgbuf.size,
						     (uint8_t *) msgbuf.buf);
   free(msgbuf.buf);
   
   if (ret_msg == NULL)
   {
      TRACE1(("SA_AIS_ERR_UNAVAILABLE: Couldn't unpack incoming message\n"));
      ret = SA_AIS_ERR_UNAVAILABLE; // Perhaps another error code;
       goto error;
  }
   
   ret = ret_msg->returnval;

   if(ret != SA_AIS_OK)
      goto error;

   initRet = saImmOmInitialize_o2(&immHandle,
				  NULL,
				  &version);
   if(initRet != SA_AIS_OK)
   {
      TRACE1(("Couldn't not initialize OM Handle, ErrorCode=%ul\n", initRet));
      ret = initRet;
      goto error;
  }

   ao_ptr = (SafcImmAdminOwnerClientT *)calloc(1, sizeof(SafcImmAdminOwnerClientT));
   if (ao_ptr == NULL) {
      TRACE1(("ERR_NO_MEMORY: IMM Admin Owner client structure alloc failed\n"));
      ret = SA_AIS_ERR_NO_MEMORY;
      goto error;
   }

   LOCK_WRLOCK(&immOmClients_lock);
   *ownerHandle = (SaImmAdminOwnerHandleT) safc_array_insert(&immOmClients, (void*) ao_ptr);
   LOCK_UNLOCK(&immOmClients_lock);

   ao_ptr->immHandle = immHandle;
   ao_ptr->releaseOnFinalize = SA_TRUE;
   ao_ptr->adminOwnerName = NULL;
   /* ao_ptr->adminOwnerName = calloc(1, nameLength+1); */
   /* strncpy(ao_ptr->adminOwnerName, adminOwnerName, nameLength); */
   ao_ptr->srv_handle = ret_msg->ownerhandle;

   ccb_ptr = (SafcImmCcbClientT *)calloc(1, sizeof(SafcImmCcbClientT));
   if (ccb_ptr == NULL) {
      TRACE1(("ERR_NO_MEMORY: IMM CCB client structure alloc failed\n"));
      ret = SA_AIS_ERR_NO_MEMORY;
      goto error;
   }
	 
   LOCK_WRLOCK(&immOmClients_lock);
   *ccbHandle = safc_array_insert(&immOmClients, (void*) ccb_ptr);
   LOCK_UNLOCK(&immOmClients_lock);

   ccb_ptr->erlCcbHandle = ret_msg->ccbhandle;
   ccb_ptr->immHandle = immHandle;
   ccb_ptr->aoHandle = *ownerHandle;

   augment_ptr = (SafcImmOiAugmentedCcbStructT *)calloc(1, sizeof(SafcImmOiAugmentedCcbStructT));
   augment_ptr->ccbId = ccbId;
   augment_ptr->immHandle = immHandle;

   LOCK_WRLOCK(&immAugmentCcbs_lock);
   safc_array_insert(&immAugmentCcbs, (void*) augment_ptr);
   LOCK_UNLOCK(&immAugmentCcbs_lock);  

error:
   safs_imm_oi_augment_ccb_initialize_ret__free_unpacked(ret_msg, NULL);

   TRACE_LEAVE();
   return ret;
}

/*
 * ======================================================================
 * Support For Ccb Error Strings
 * ======================================================================
 */
SaAisErrorT saImmOiCcbSetErrorString(SaImmOiHandleT immOiHandle,
				     SaImmOiCcbIdT ccbId,
				     const SaStringT errorString)
{
   SaAisErrorT ret = SA_AIS_OK;
   SaAisErrorT recv_ret = SA_AIS_OK;
   SafcImmOiClientT *client = NULL;
   safc_buf_t msgbuf;
   SafsImmOiCcbSetErrorString ccb_set_error_string_msg =
      SAFS_IMM_OI_CCB_SET_ERROR_STRING__INIT;
   SafsImmOiMessage msg = SAFS_IMM_OI_MESSAGE__INIT;
   SafsImmOiCcbSetErrorStringRet *ret_msg;
   int length = 0;

   TRACE_ENTER();

   LOCK_RDLOCK(&immOiClients_lock);
   client =
      (SafcImmOiClientT *) safc_array_get(&immOiClients, (int) immOiHandle);
   LOCK_UNLOCK(&immOiClients_lock);
   if (!client)
      return SA_AIS_ERR_BAD_HANDLE;
   
  /* Coding of message */
  ccb_set_error_string_msg.handle = client->srv_handle;
  ccb_set_error_string_msg.ccbid = ccbId;
  length = strlen(errorString);
  TRACE1(("Strlen: %d\n", length));
  ccb_set_error_string_msg.errorstring = calloc(1, length+1);
  (void) strcpy(ccb_set_error_string_msg.errorstring, errorString);
  //ccb_set_error_string_msg.errorstring[errorString->length] = '\0';

  msg.ccbseterrorstring = &ccb_set_error_string_msg;

  msgbuf.size = safs_imm_oi_message__get_packed_size(&msg);
  msgbuf.buf = malloc(msgbuf.size);
  safs_imm_oi_message__pack(&msg, (uint8_t *) msgbuf.buf);

  /* Send message & wait for answer*/
  MUTEX_LOCK(&(client->mutex));
  recv_ret = safc_send_recv(client->sockfd, &msgbuf);
  MUTEX_UNLOCK(&(client->mutex));

   if(recv_ret == SA_AIS_ERR_MESSAGE_ERROR)
   {
      TRACE1(("SA_AIS_ERR_UNAVAILABLE: Couldn't send/read from socket\n"));
      free(ccb_set_error_string_msg.errorstring);
      free(msgbuf.buf);
      return SA_AIS_ERR_UNAVAILABLE; // Perhaps another error code;
   }

   /* Decoding of answer */
   ret_msg =
      safs_imm_oi_ccb_set_error_string_ret__unpack(NULL,
						   msgbuf.size,
						   (uint8_t *) msgbuf.buf);
   free(ccb_set_error_string_msg.errorstring);
   free(msgbuf.buf);

   if (ret_msg == NULL)
   {
      TRACE1(("SA_AIS_ERR_UNAVAILABLE: Couldn't unpack incoming message\n"));
      return SA_AIS_ERR_UNAVAILABLE;
   }

   ret = ret_msg->returnval;
   safs_imm_oi_ccb_set_error_string_ret__free_unpacked(ret_msg, NULL);

   TRACE_LEAVE();

   return ret;
}


/*
 * ======================================================================
 * Special SAFC function for delaying NC updates
 * ======================================================================
 */
SaAisErrorT saImmOiRtObjectUpdateDelayedNcValues_s2(SaImmOiHandleT immOiHandle,
						    const SaNameT *objectName,
						    const SaImmAttrNameT *attributeNames)
{
  SaAisErrorT ret = SA_AIS_OK;
  SaAisErrorT recv_ret = SA_AIS_OK;
  SafcImmOiClientT *client=NULL;
  safc_buf_t msgbuf;
  SafsImmOiRtObjectUpdateDelayedNcValuesS2 rt_obj_update_delayed_nc_values_msg =
     SAFS_IMM_OI_RT_OBJECT_UPDATE_DELAYED_NC_VALUES_S2__INIT;
  SafsImmOiMessage msg = SAFS_IMM_OI_MESSAGE__INIT;
  SafsImmOiRtObjectUpdateDelayedNcValuesS2Ret *ret_msg;

  SaImmAttrNameT attributeName;

  TRACE_ENTER();

  if (objectName == NULL || (objectName->length == 0) || (objectName->length >= SA_MAX_NAME_LENGTH)) {
     TRACE1(("ERR_INVALID_PARAM: objectname"));
     return SA_AIS_ERR_INVALID_PARAM;
  }

  LOCK_RDLOCK(&immOiClients_lock);
  client =
    (SafcImmOiClientT *) safc_array_get(&immOiClients, (int) immOiHandle);
  LOCK_UNLOCK(&immOiClients_lock);
  if (!client)
    return SA_AIS_ERR_BAD_HANDLE;

  /* Coding of message */
  rt_obj_update_delayed_nc_values_msg.handle = client->srv_handle;

  rt_obj_update_delayed_nc_values_msg.objectname = calloc(1, sizeof(SaUint8T) * (objectName->length + 1));
  (void)memcpy(rt_obj_update_delayed_nc_values_msg.objectname,  objectName->value, objectName->length);
  rt_obj_update_delayed_nc_values_msg.objectname[objectName->length] = '\0';

  if (attributeNames) {
     int i;
     /* Calculate number of attribute names */
     for (i = 0; attributeNames[i]; i++);
     
     rt_obj_update_delayed_nc_values_msg.n_attributenames = i;
     rt_obj_update_delayed_nc_values_msg.attributenames = 
	calloc (1, sizeof (char*) * rt_obj_update_delayed_nc_values_msg.n_attributenames);
     
     for (i = 0; i < rt_obj_update_delayed_nc_values_msg.n_attributenames; i++) {
	attributeName = attributeNames[i];
	rt_obj_update_delayed_nc_values_msg.attributenames[i] = (char*) attributeName;
     }
  } else {
     TRACE1(("ERR_INVALID_PARAM: attributeNames is NULL"));
     return SA_AIS_ERR_INVALID_PARAM;
  }
  
  msg.rtobjectdelayedncupdate_s2 = &rt_obj_update_delayed_nc_values_msg;
  
  msgbuf.size = safs_imm_oi_message__get_packed_size(&msg);
  msgbuf.buf = malloc(msgbuf.size);
  safs_imm_oi_message__pack(&msg, (uint8_t *) msgbuf.buf);
  
  free(rt_obj_update_delayed_nc_values_msg.attributenames);
  free(rt_obj_update_delayed_nc_values_msg.objectname);
   
  /* Send message & wait for answer*/
  MUTEX_LOCK(&(client->mutex));
  recv_ret = safc_send_recv(client->sockfd, &msgbuf);
  MUTEX_UNLOCK(&(client->mutex));

  if(recv_ret == SA_AIS_ERR_MESSAGE_ERROR)
  {
     TRACE1(("SA_AIS_ERR_UNAVAILABLE: Couldn't send/read from socket\n"));
     free(msgbuf.buf);
     return SA_AIS_ERR_UNAVAILABLE; // Perhaps another error code;
   }

  /* Decoding of answer */
 ret_msg =
    safs_imm_oi_rt_object_update_delayed_nc_values_s2_ret__unpack(NULL,
							      msgbuf.size,
							      (uint8_t *) msgbuf.buf);
  free(msgbuf.buf);

  if (ret_msg == NULL)
    {
      TRACE1(("SA_AIS_ERR_UNAVAILABLE: Couldn't unpack incoming message\n"));
      return SA_AIS_ERR_UNAVAILABLE;
    }

  ret = ret_msg->returnval;
  safs_imm_oi_rt_object_update_delayed_nc_values_s2_ret__free_unpacked(ret_msg, NULL);

  TRACE_LEAVE();

  return ret;
}



/*
 * ======================================================================
 * Internal Functions
 * ======================================================================
 */
/* void safs_to_sa_copy_attr_values(SaImmAttrValueT* attrVals, */
/* 				 const SaImmValueTypeT attrValueType, */
/* 				 const SafsImmAttrValues2* avals) */
/* { */
/*   int i; */

/*   for (i = 0; i < avals->attrvaluesnumber; i++) */
/*     { */
/*       safs_to_sa_copy_attr_value(&attrVals[i], avals->attrvaluetype, */
/* 				 avals->attrvalues[i]); */
/*     } */
/* } */


void safs_to_sa_copy_attr_value(SaImmAttrValueT* attrValue,
				const SaImmValueTypeT attrValueType,
				SafsImmAttrValue* aval)
{
	/*
	  Copies ONE attribute value.
	  Multivalued attributes need to copy each value.
	*/

	SaAnyT *saAnyTp;
	SaNameT *saNameTp;

	switch (attrValueType) {
	case SA_IMM_ATTR_SAINT32T:
	  *attrValue = &(aval->saint32);
	  break;
	case SA_IMM_ATTR_SAUINT32T:
	  *attrValue = &(aval->sauint32);
	  break;
	case SA_IMM_ATTR_SAINT64T:
	  *attrValue = &(aval->saint64);
	  break;
	case SA_IMM_ATTR_SAUINT64T:
	  *attrValue = &(aval->sauint64);
	  break;
	case SA_IMM_ATTR_SATIMET:
	  *attrValue = &(aval->satime);
	  break;
	case SA_IMM_ATTR_SAFLOATT:
	  *attrValue = &(aval->safloat);
	  break;
	case SA_IMM_ATTR_SADOUBLET:
	  *attrValue = &(aval->sadouble);
	  break;
	case SA_IMM_ATTR_SANAMET:
	  saNameTp = calloc(1, sizeof(SaNameT));
	  saNameTp->length = strlen(aval->saname);
	  (void)memcpy(saNameTp->value, aval->saname, saNameTp->length);
	  *attrValue = saNameTp;
	  break;
	case SA_IMM_ATTR_SASTRINGT:
	  *attrValue = &(aval->sastring);
	  break;

	case SA_IMM_ATTR_SAANYT:
	  saAnyTp = calloc(1, sizeof(SaAnyT));
	  saAnyTp->bufferSize = aval->saany.len;
	  saAnyTp->bufferAddr =
	    calloc(1, sizeof(SaUint8T) * (saAnyTp->bufferSize));
	  (void)memcpy(saAnyTp->bufferAddr, aval->saany.data,
		       aval->saany.len);
	  *attrValue = saAnyTp;
	  break;

	default:
	  break;
	}
}

void free_sa_attr_values(SaImmAttrValueT* attrVals,
			 const SaUint32T attrValuesNumber,
			 const SaImmValueTypeT attrValueType)
{
  int i;

  for (i = 0; i < attrValuesNumber; i++)
    {
      free_sa_attr_value(attrVals[i], attrValueType);
    }

}

void free_sa_attr_value(SaImmAttrValueT attrValue,
			const SaImmValueTypeT attrValueType)
{
	/*
	  Frees one attribute value.
	  Multivalued attributes need to free each value.
	*/

	SaAnyT *saAnyTp;

	switch (attrValueType) {
	case SA_IMM_ATTR_SANAMET:
	  free(attrValue);
	  break;
	case SA_IMM_ATTR_SAANYT:
	  saAnyTp = (SaAnyT *) attrValue;
	  free(saAnyTp->bufferAddr);
	  free(saAnyTp);
	  break;
	default:
	  break;
	}
}

void safc_free_rt_oc2_msg(SafsImmOiRtObjectCreate2 rt_oc2_msg)
{
   int i;
   /* SafsImmAttrValues2 *attr; */

   for (i = 0; i < rt_oc2_msg.n_attrvalues; i++) /* { */
      safc_free_safs_attr_values_2(rt_oc2_msg.attrvalues[i]);

      /* attr = rt_oc2_msg.attrvalues[i]; */
      /* for (j = 0; j < attr->attrvaluesnumber; j++) { */
      /* 	 switch(attr->attrvaluetype) { */
      /* 	 case SA_IMM_ATTR_SANAMET: */
      /* 	    free(attr->attrvalues[j]->saname); */
      /* 	    break; */
      /* 	 case SA_IMM_ATTR_SAANYT: */
      /* 	    free(attr->attrvalues[j]->saany.data); */
      /* 	    break; */
      /* 	 default: */
      /* 	    break; */
      /* 	 } */
      /* 	 free(attr->attrvalues[j]); */
      /* } */
   /*    free(attr->attrvalues); */
   /*    free(rt_oc2_msg.attrvalues[i]); */
   /* } */
   free(rt_oc2_msg.attrvalues);
   free(rt_oc2_msg.parentname);
}

void safc_free_rt_ou2_msg(SafsImmOiRtObjectUpdate2 rt_ou2_msg)
{
   int i;
   /* SafsImmAttrValues2 *attr; */

   for (i = 0; i < rt_ou2_msg.n_attrmods; i++) {
      safc_free_safs_attr_values_2(rt_ou2_msg.attrmods[i]->modattr);
      free(rt_ou2_msg.attrmods[i]);

      /* attr = rt_ou2_msg.attrmods[i]->modattr; */
      /* for (j = 0; j < attr->attrvaluesnumber; j++) { */
      /* 	 switch(attr->attrvaluetype) { */
      /* 	 case SA_IMM_ATTR_SANAMET: */
      /* 	    free(attr->attrvalues[j]->saname); */
      /* 	    break; */
      /* 	 case SA_IMM_ATTR_SAANYT: */
      /* 	    free(attr->attrvalues[j]->saany.data); */
      /* 	    break; */
      /* 	 default: */
      /* 	    break; */
      /* 	 } */
      /* 	 free(attr->attrvalues[j]); */
      /* } */
      /* free(attr->attrvalues); */
      /* free(rt_ou2_msg.attrmods[i]->modattr); */
      /* free(rt_ou2_msg.attrmods[i]); */
   }
   free(rt_ou2_msg.attrmods);
   free(rt_ou2_msg.objectname);
}

int safc_find_augmented_ccb(void* elem, void* sval)
{

   SafcImmOiAugmentedCcbStructT *m = (SafcImmOiAugmentedCcbStructT*) elem;

   if(m->ccbId == *((int*) sval))
      return 1;
   else
      return 0;
}

void safc_print_augmented_ccb(void* elem)
{

   /*SafcImmOiAugmentedCcbStructT *m = (SafcImmOiAugmentedCcbStructT*) elem;*/

   /* printf("Ccb: %llu, ImmHandle: %llu\n",  */
   /* 	  (unsigned long long) m->ccbId,  */
   /* 	  (unsigned long long) m->immHandle); */
   /* fflush(stdout); */
   return;
}
