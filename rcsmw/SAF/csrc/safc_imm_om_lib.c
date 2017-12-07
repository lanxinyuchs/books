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
 *  Purpose : SAF IMM OM Agent Library
 * ----------------------------------------------------------------------
 *
 */

#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <netinet/in.h>
#include <unistd.h>
#include <fcntl.h>
#include <stdio.h>
#include "safc_sock_lib.h"
#include "safc_trace.h"
#include "safc_array.h"
#include "safc_imm_om_lib.h"

/*
 * Function declarations
 */
SaAisErrorT dispatchOmCallback(SaImmHandleT immHandle,
			       SaImmCallbacks callbacks_msg,
			       SafcImmOmClientT* client);
int isVoidCallback(SaImmCallbacks callbacks_msg);
void safc_free_class_create_msg(SafsImmOmClassCreate2 cc_msg);
void safc_free_ccb_oc2_msg(SafsImmOmCcbObjectCreate2 ccb_oc2_msg);
void safc_free_ccb_om2_msg(SafsImmOmCcbObjectModify2 ccb_om2_msg);
int safc_find_allocated_memory(void* elem, void* sval);
int safc_find_ao_from_immhandle(void* elem, void* sval);
int safc_find_ccb_from_immhandle(void* elem, void* sval);
void safc_free_attr_definitions(SaImmAttrDefinitionT_2 **attrDefinitions);
void safc_free_admop_return_params(SaImmAdminOperationParamsT_2 **returnParams);
void safc_delete_allocated_memory(void* elem);
void safc_free_error_strings(SaStringT *strings);

/*
 * Trace variable
 */
int safc_imm_trace_level;

/*
 * Variables for the client structure storage array
 */
pthread_rwlock_t immOmClients_lock = PTHREAD_RWLOCK_INITIALIZER;
safc_array immOmClients = SAFC_ARRAY_INITIALIZER;


/*
 * ======================================================================
 * 4.3 Library Life Cycle
 * ======================================================================
 */


static SaAisErrorT initialize_common(SaImmHandleT *immHandle, SafcImmOmClientT *client,
				     SaVersionT *version);


SaAisErrorT saImmOmInitialize_o2(SaImmHandleT *immHandle,
			      const SaImmCallbacksT_o2 *immCallbacks,
			      SaVersionT *version)
{
   SafcImmOmClientT *cl=NULL;
   SaAisErrorT rc = SA_AIS_OK;

   TRACE_ENTER();

   if ((!immHandle) || (!version)) {
      TRACE1(("ERR_INVALID_PARAM: immHandle is NULL or version is NULL\n"));
      return SA_AIS_ERR_INVALID_PARAM;
   }

   *immHandle = 0;

   if ((version->releaseCode != 'A') || (version->majorVersion != 0x02) ||
       ((version->minorVersion < 11) && (version->minorVersion != 0x00))) {
      TRACE1(("ERR_VERSION: THIS SHOULD BE A VERSION >= A.2.11 initialize but claims to be %c %u %u\n",
	      version->releaseCode, version->majorVersion, version->minorVersion));
      safc_imm_version_validate(version);
      TRACE_LEAVE();
      return SA_AIS_ERR_VERSION;
   }

   /* Validations : Version */
   rc = safc_imm_version_validate(version);
   if (rc != SA_AIS_OK) {
      TRACE1(("ERR_VERSION: Version validation failed\n"));
      TRACE_LEAVE();
      return rc;
   }

   /* Alloc the client info data structure, handle for now */
   cl = (SafcImmOmClientT *) calloc(1, sizeof(SafcImmOmClientT));
   if (cl == NULL) {
      TRACE1(("ERR_NO_MEMORY: IMM client structure alloc failed\n"));
      TRACE_LEAVE();
      return SA_AIS_ERR_NO_MEMORY;
   }

   cl->isImmA2b = 1;

   /* Store the callback functions, if set */
   if (immCallbacks) {
      cl->hasCallbacks = 1;
      cl->cb.callbacksA2b = *immCallbacks;
   } else
      cl->hasCallbacks = 0;


   return initialize_common(immHandle, cl, version);
}

SaAisErrorT saImmOmInitialize(SaImmHandleT *immHandle,
			      const SaImmCallbacksT *immCallbacks,
			      SaVersionT *version)
{
   SafcImmOmClientT *cl = NULL;
   SaAisErrorT rc = SA_AIS_OK;

   TRACE_ENTER();

   if ((!immHandle) || (!version)) {
      TRACE1(("ERR_INVALID_PARAM: immHandle is NULL or version is NULL\n"));
      return SA_AIS_ERR_INVALID_PARAM;
   }

   *immHandle = 0;

   /* Validations : Version */
   rc = safc_imm_version_validate(version);
   if (rc != SA_AIS_OK) {
      TRACE1(("ERR_VERSION: Version validation failed\n"));
      TRACE_LEAVE();
      return rc;
   }

   /* Alloc the client info data structure, handle for now */
   cl = (SafcImmOmClientT *) calloc(1, sizeof(SafcImmOmClientT));
   if (cl == NULL) {
      TRACE1(("ERR_NO_MEMORY: IMM client structure alloc failed\n"));
      TRACE_LEAVE();
      return SA_AIS_ERR_NO_MEMORY;
   }

   cl->isImmA2b = 0;

   /* Store the callback functions, if set */
   if (immCallbacks) {
      TRACE1(("IMM OM has callbacks\n"));
      cl->hasCallbacks = 1;
      cl->cb.callbacks = *immCallbacks;
   } else {
      TRACE1(("IMM OM has no callbacks\n"));
      cl->hasCallbacks = 0;
   }


   return initialize_common(immHandle, cl, version);
}

static SaAisErrorT initialize_common(SaImmHandleT *immHandle, SafcImmOmClientT *client,
				     SaVersionT *version)
{
   SaAisErrorT ret = SA_AIS_OK;
   SaAisErrorT recv_ret = SA_AIS_OK;
   int port;
   char* hostname = "localhost";
   safc_buf_t msgbuf;
   SafsVersion version_msg =  SAFS_VERSION__INIT;
   SafsImmOmMessage msg = SAFS_IMM_OM_MESSAGE__INIT;
   SafsImmOmInitializeRet *ret_msg;
   SafsImmCallbacksO2 cb2_msg =  SAFS_IMM_CALLBACKS_O2__INIT;
   SafsImmOmInitializeO2 init2_msg =  SAFS_IMM_OM_INITIALIZE_O2__INIT;
   SafsImmCallbacks cb_msg =  SAFS_IMM_CALLBACKS__INIT;
   SafsImmOmInitialize init_msg =  SAFS_IMM_OM_INITIALIZE__INIT;
   SafsVersion *returned_version;
   
   port = safc_protobuf_port("SAFC_IMM_OM_PORT", 10001);
   client->sockfd = safc_connect(hostname, port);
   if (client->sockfd < 0) {
      TRACE1(("safc_connect failed\n"));
       return SA_AIS_ERR_UNAVAILABLE;
   }

   safc_array_init(&(client->allocated_memory),
		   NULL,
		   &safc_delete_allocated_memory,
		   &safc_find_allocated_memory);

   MUTEX_INIT(&client->mutex);

   LOCK_WRLOCK(&immOmClients_lock);
   *immHandle = (SaImmHandleT) safc_array_insert(&immOmClients, (void*) client);
   LOCK_UNLOCK(&immOmClients_lock);

   version_msg.releasecode = version->releaseCode;
   version_msg.majorversion = version->majorVersion;
   version_msg.minorversion = version->minorVersion;

   /* Coding of message */
   if(client->isImmA2b == 1){
      if(client->hasCallbacks) {
	 if (client->cb.callbacks.saImmOmAdminOperationInvokeCallback != NULL)
	    cb2_msg.saimmomadminoperationinvokecallback = 1;
      }
      init2_msg.callerpid = getpid();
      init2_msg.callbacks = &cb2_msg;
      init2_msg.version = &version_msg;

      msg.initialize_o2 = &init2_msg;

   } else {
      if(client->hasCallbacks) {
	 if (client->cb.callbacks.saImmOmAdminOperationInvokeCallback != NULL)
	    cb_msg.saimmomadminoperationinvokecallback = 1;
      }

      init_msg.callerpid = getpid();
      init_msg.callbacks = &cb_msg;
      init_msg.version = &version_msg;

      msg.initialize = &init_msg;
   }

   msgbuf.size = safs_imm_om_message__get_packed_size(&msg);
   msgbuf.buf = malloc(msgbuf.size);
   safs_imm_om_message__pack(&msg, (uint8_t *) msgbuf.buf);

   /* Send message & wait for answer*/
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
   ret_msg = safs_imm_om_initialize_ret__unpack(NULL,
						msgbuf.size,
						(uint8_t *) msgbuf.buf);
   free(msgbuf.buf);

   if (ret_msg == NULL)
   {
      TRACE1(("SA_AIS_ERR_UNAVAILABLE: error unpacking incoming message\n"));
      ret = SA_AIS_ERR_UNAVAILABLE; // Perhaps another error code;
      goto error;
   }

   client->srv_handle = ret_msg->handle;
   returned_version = ret_msg->version;
   version->releaseCode = returned_version->releasecode;
   version->majorVersion = returned_version->majorversion;
   version->minorVersion = returned_version->minorversion;

   ret = ret_msg->returnval;
   safs_imm_om_initialize_ret__free_unpacked(ret_msg, NULL);

   if(ret != SA_AIS_OK)
      goto error;

   if(client->hasCallbacks) {
      SafsImmOmCallbacksInitialize cbinit_msg = SAFS_IMM_OM_CALLBACKS_INITIALIZE__INIT;
      SafsImmOmCallbacksInitializeRet *cbret_msg;

      client->cb_sockfd = safc_connect(hostname, port);
      if (client->cb_sockfd < 0) {
	 TRACE1(("safc_connect failed for cb socket\n"));
	 ret = SA_AIS_ERR_UNAVAILABLE;
	 goto error;
      }
      TRACE1(("C\n"));

      /* Coding of callback_connect message */
      cbinit_msg.handle = client->srv_handle;

      msg.initialize = NULL;
      msg.callbacksinitialize = &cbinit_msg;

      msgbuf.size = safs_imm_om_message__get_packed_size(&msg);
      msgbuf.buf = malloc(msgbuf.size);
      safs_imm_om_message__pack(&msg, (uint8_t *) msgbuf.buf);

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
      cbret_msg = safs_imm_om_callbacks_initialize_ret__unpack(NULL,
							       msgbuf.size,
							       (uint8_t *) msgbuf.buf);
      free(msgbuf.buf);

      if (cbret_msg == NULL)
      {
	 TRACE1(("error unpacking incoming message\n"));
	 ret = SA_AIS_ERR_UNAVAILABLE; // Perhaps another error code;
	 goto cberror;
      }

      ret = cbret_msg->returnval;
      safs_imm_om_callbacks_initialize_ret__free_unpacked(cbret_msg, NULL);

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

   LOCK_WRLOCK(&immOmClients_lock);
   safc_array_remove(&immOmClients, (int) *immHandle);
   LOCK_UNLOCK(&immOmClients_lock);
   *immHandle = 0;

   free(client);

   TRACE_LEAVE();
   return ret;
}

SaAisErrorT saImmOmSelectionObjectGet(SaImmHandleT immHandle,
				      SaSelectionObjectT *selectionObject)
{
   SafcImmOmClientT *client;

   TRACE_ENTER();

   if (!selectionObject)
      return SA_AIS_ERR_INVALID_PARAM;

   /* *selectionObject = (-1); */
   LOCK_RDLOCK(&immOmClients_lock);
   client =
     (SafcImmOmClientT *) safc_array_get(&immOmClients, (int) immHandle);
   LOCK_UNLOCK(&immOmClients_lock);
   if (!client)
      return SA_AIS_ERR_BAD_HANDLE;

   *selectionObject = (SaSelectionObjectT) client->cb_sockfd;

   TRACE_LEAVE();

   return SA_AIS_OK;
}

SaAisErrorT saImmOmDispatch(SaImmHandleT immHandle,
			    SaDispatchFlagsT dispatchFlags)
{
  SaAisErrorT ret = SA_AIS_OK;
  SaAisErrorT cb_ret = SA_AIS_OK;
  SaAisErrorT recv_ret = SA_AIS_OK;
  safc_buf_t msgbuf;
  SaImmCallbacks *callbacks_msg;
  SafsImmOmCallbacksRet callbacks_ret_msg = SAFS_IMM_OM_CALLBACKS_RET__INIT;
  SafcImmOmClientT *client;

   TRACE_ENTER();

  /* Just support DISPATCH_ONE in first step */
  if (dispatchFlags != SA_DISPATCH_ONE)
    return SA_AIS_ERR_INVALID_PARAM;

  LOCK_RDLOCK(&immOmClients_lock);
  client =
    (SafcImmOmClientT *) safc_array_get(&immOmClients, (int) immHandle);
  LOCK_UNLOCK(&immOmClients_lock);
  if (!client)
    return SA_AIS_ERR_BAD_HANDLE;

  if(client->hasCallbacks == 0)
    return SA_AIS_ERR_BAD_HANDLE;

  /*  TRACE1(("saImmOmDispatch: read message from socket and unpack \n")); */
  msgbuf.buf = NULL;
  MUTEX_LOCK(&(client->mutex));
  recv_ret = safc_recv(client->cb_sockfd, &msgbuf);
  MUTEX_UNLOCK(&(client->mutex));

  if(recv_ret == SA_AIS_ERR_MESSAGE_ERROR)
  {
     TRACE1(("SA_AIS_ERR_UNAVAILABLE: Couldn't read from socket!"));
     free(msgbuf.buf);
     return SA_AIS_ERR_UNAVAILABLE;
  }

  /* Decoding of message */
  callbacks_msg = sa_imm_callbacks__unpack(NULL,
					   msgbuf.size,
					   (uint8_t *) msgbuf.buf);
  free(msgbuf.buf);

  if (callbacks_msg == NULL)
    {
      TRACE1(("SA_AIS_ERR_UNAVAILABLE: Couldn't unpack incoming message\n"));
      return SA_AIS_ERR_UNAVAILABLE;
    }

  /* TRACE1(("saImmOmDispatch: call correct cb \n")); */
  cb_ret = dispatchOmCallback(immHandle,
			      *callbacks_msg,
			      client);
  sa_imm_callbacks__free_unpacked(callbacks_msg, NULL);

  if (!isVoidCallback(*callbacks_msg))
    {
      /* TRACE1(("saImmOmDispatch: pack answer \n")); */
      callbacks_ret_msg.returnval = cb_ret;
      msgbuf.size = safs_imm_om_callbacks_ret__get_packed_size(&callbacks_ret_msg);
      msgbuf.buf = malloc(msgbuf.size);
      safs_imm_om_callbacks_ret__pack(&callbacks_ret_msg, (uint8_t *) msgbuf.buf);

      /* TRACE1(("saImmOmDispatch: send answer on socket\n"));
	 TRACE1(("Sent message length %d on socket %d\n", msgbuf.size,
	 client->sockfd)); */
      MUTEX_LOCK(&(client->mutex));
      safc_send(client->cb_sockfd, &msgbuf);
      MUTEX_UNLOCK(&(client->mutex));
      free(msgbuf.buf);
    }

   TRACE_LEAVE();

   return ret;
}

SaAisErrorT saImmOmFinalize(SaImmHandleT immHandle)
{
   SaAisErrorT ret = SA_AIS_OK;
   SaAisErrorT recv_ret = SA_AIS_OK;
   safc_buf_t msgbuf;
   SafsImmOmFinalize fin_msg =  SAFS_IMM_OM_FINALIZE__INIT;
   SafsImmOmMessage msg = SAFS_IMM_OM_MESSAGE__INIT;
   SafsImmOmFinalizeRet *ret_msg;
   SafcImmOmClientT *client;

   TRACE_ENTER();

   LOCK_RDLOCK(&immOmClients_lock);
   client = (SafcImmOmClientT *) safc_array_get(&immOmClients, (int) immHandle);
   LOCK_UNLOCK(&immOmClients_lock);
   if (!client) {
      TRACE_LEAVE();
      return SA_AIS_ERR_BAD_HANDLE;
   }

   /* Coding of message */
   fin_msg.handle = client->srv_handle;
   msg.finalize = &fin_msg;

   msgbuf.size = safs_imm_om_message__get_packed_size(&msg);
   msgbuf.buf = malloc(msgbuf.size);
   safs_imm_om_message__pack(&msg, (uint8_t *) msgbuf.buf);

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
   ret_msg = safs_imm_om_finalize_ret__unpack(NULL,
					    msgbuf.size,
					    (uint8_t *) msgbuf.buf);
   free(msgbuf.buf);

   if (ret_msg == NULL)
   {
      TRACE1(("SA_AIS_ERR_UNAVAILABLE: Couldn't unpack incoming message\n"));
      return SA_AIS_ERR_UNAVAILABLE;
   }

   ret = ret_msg->returnval;
   safs_imm_om_finalize_ret__free_unpacked(ret_msg, NULL);

   safc_disconnect(client->sockfd);
   if(client->hasCallbacks)
      safc_disconnect(client->cb_sockfd);


   safc_array_finalize(&(client->allocated_memory));

   LOCK_WRLOCK(&immOmClients_lock);
   safc_array_remove(&immOmClients, (int) immHandle);
   LOCK_UNLOCK(&immOmClients_lock);

   /* LATH: Remove sub handles */

   free(client);

   TRACE_LEAVE();

   return ret;
}

/*
 * ======================================================================
 * 4.4 Object Class Management
 * ======================================================================
 */
SaAisErrorT saImmOmClassCreate_2(SaImmHandleT immHandle,
				 const SaImmClassNameT className,
				 SaImmClassCategoryT classCategory,
				 const SaImmAttrDefinitionT_2 **attrDefinitions)
{
   SaAisErrorT ret = SA_AIS_OK;
   SaAisErrorT recv_ret = SA_AIS_OK;
   SafcImmOmClientT *client=NULL;
   safc_buf_t msgbuf;
   SafsImmOmClassCreate2 class_create_msg = SAFS_IMM_OM_CLASS_CREATE_2__INIT;
   SafsImmOmMessage msg = SAFS_IMM_OM_MESSAGE__INIT;
   SafsImmOmClassCreate2Ret *ret_msg;
   int i;
   const SaImmAttrDefinitionT_2 *attrDef;

   TRACE_ENTER();

   if (className == NULL) {
      TRACE1(("ERR_INVALID_PARAM: classname is NULL\n"));
      TRACE_LEAVE();
      return SA_AIS_ERR_INVALID_PARAM;
   }

   if (attrDefinitions == NULL) {
      TRACE1(("ERR_INVALID_PARAM: attrDefinitions is NULL\n"));
      TRACE_LEAVE();
      return SA_AIS_ERR_INVALID_PARAM;
   }

   LOCK_RDLOCK(&immOmClients_lock);
   client = (SafcImmOmClientT *) safc_array_get(&immOmClients, (int) immHandle);
   LOCK_UNLOCK(&immOmClients_lock);
   if (!client) {
      TRACE1(("ERR_BAD_HANDLE: No initialized om handle exists!"));
      TRACE_LEAVE();
      return SA_AIS_ERR_BAD_HANDLE;
   }

   /* Coding of message */
   class_create_msg.handle = client->srv_handle;
   class_create_msg.classname = className;
   class_create_msg.classcategory = classCategory;

   for (i = 0; attrDefinitions[i]; i++);

   class_create_msg.n_attrdefinitions = i;
   class_create_msg.attrdefinitions =
      calloc (1, sizeof (SafsImmAttrDefinition2*) * class_create_msg.n_attrdefinitions);

   for (i = 0; i < class_create_msg.n_attrdefinitions; i++) {
      attrDef = attrDefinitions[i];

      class_create_msg.attrdefinitions[i] = calloc(1, sizeof (SafsImmAttrDefinition2));
      safs_imm_attr_definition_2__init(class_create_msg.attrdefinitions[i]);
      class_create_msg.attrdefinitions[i]->attrname = (char*) attrDef->attrName;
      class_create_msg.attrdefinitions[i]->attrvaluetype = attrDef->attrValueType;
      class_create_msg.attrdefinitions[i]->attrflags = attrDef->attrFlags;

      if (!attrDef->attrDefaultValue)
	 class_create_msg.attrdefinitions[i]->attrdefaultvalue = NULL;
      else {
	 if(attrDef->attrDefaultValue) {
	    class_create_msg.attrdefinitions[i]->attrdefaultvalue =
	       calloc (1, sizeof (SafsImmAttrValue));
	    safs_imm_attr_value__init(class_create_msg.attrdefinitions[i]->attrdefaultvalue);
	    safc_copy_from_imm_attr_value(class_create_msg.attrdefinitions[i]->attrdefaultvalue,
					  attrDef->attrValueType,
					  attrDef->attrDefaultValue);
	 } else 
	    class_create_msg.attrdefinitions[i]->attrdefaultvalue = NULL;	 	    
      }
   }

   msg.classcreate_2 = &class_create_msg;

   msgbuf.size = safs_imm_om_message__get_packed_size(&msg);
   msgbuf.buf = malloc(msgbuf.size);
   safs_imm_om_message__pack(&msg, (uint8_t *) msgbuf.buf);

   safc_free_class_create_msg(class_create_msg);

   /* Send message & wait for answer */
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
   ret_msg = safs_imm_om_class_create2_ret__unpack(NULL,
						   msgbuf.size,
						   (uint8_t *) msgbuf.buf);
   free(msgbuf.buf);

   if (ret_msg == NULL)
   {
      TRACE1(("SA_AIS_ERR_UNAVAILABLE: Couldn't unpack incoming message\n"));
      return SA_AIS_ERR_UNAVAILABLE;
   }

   ret = ret_msg->returnval;

   safs_imm_om_class_create2_ret__free_unpacked(ret_msg, NULL);

   TRACE_LEAVE();

   return ret;
}


SaAisErrorT saImmOmClassDescriptionGet_2(SaImmHandleT immHandle,
					 const SaImmClassNameT className,
					 SaImmClassCategoryT *classCategory,
					 SaImmAttrDefinitionT_2 ***attrDefinitions)
{
   SaAisErrorT ret = SA_AIS_OK;
   SaAisErrorT recv_ret = SA_AIS_OK;
   SafcImmOmClientT *client=NULL;
   safc_buf_t msgbuf;
   SafsImmOmClassDescriptionGet2 get_msg =  SAFS_IMM_OM_CLASS_DESCRIPTION_GET_2__INIT;
   SafsImmOmMessage msg = SAFS_IMM_OM_MESSAGE__INIT;
   SafsImmOmClassDescriptionGet2Ret *ret_msg;
   SafcImmOmMemoryStructT *memory;

   TRACE_ENTER();

   if (className == NULL) {
      TRACE1(("ERR_INVALID_PARAM: classname is NULL\n"));
      TRACE_LEAVE();
      return SA_AIS_ERR_INVALID_PARAM;
   }

   LOCK_RDLOCK(&immOmClients_lock);
   client = (SafcImmOmClientT *) safc_array_get(&immOmClients, (int) immHandle);
   LOCK_UNLOCK(&immOmClients_lock);
   if (!client) {
      TRACE1(("ERR_BAD_HANDLE: No initialized om handle exists!"));
      TRACE_LEAVE();
      return SA_AIS_ERR_BAD_HANDLE;
   }

   /* Coding of message */
   get_msg.handle = client->srv_handle;
   get_msg.classname = className;

   msg.classdescriptionget_2 = &get_msg;

   msgbuf.size = safs_imm_om_message__get_packed_size(&msg);
   msgbuf.buf = malloc(msgbuf.size);
   safs_imm_om_message__pack(&msg, (uint8_t *) msgbuf.buf);

   /* Send message & wait for answer */
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
   ret_msg = safs_imm_om_class_description_get2_ret__unpack(NULL,
						   msgbuf.size,
						   (uint8_t *) msgbuf.buf);
   free(msgbuf.buf);

   if (ret_msg == NULL)
   {
      TRACE1(("SA_AIS_ERR_UNAVAILABLE: Couldn't unpack incoming message\n"));
      return SA_AIS_ERR_UNAVAILABLE;
   }

   ret = ret_msg->returnval;
   if(ret == SA_AIS_OK) {
      int i, name_len;
      SaImmAttrDefinitionT_2 **attrDefs = NULL;

      if(ret_msg->has_classcategory)
	 *classCategory = ret_msg->classcategory;
      else
	 TRACE1(("Error: No class category in message!"));

      attrDefs = calloc(1, sizeof(SaImmAttrDefinitionT_2 *) * (ret_msg->n_attrdefinitions + 1));

      for (i=0; i < ret_msg->n_attrdefinitions; i++) {
	 attrDefs[i] = calloc(1, sizeof(SaImmAttrDefinitionT_2));

	 name_len = strlen(ret_msg->attrdefinitions[i]->attrname);

	 attrDefs[i]->attrName = malloc(name_len + 1);
	 strncpy(attrDefs[i]->attrName, ret_msg->attrdefinitions[i]->attrname, name_len + 1);
	 attrDefs[i]->attrValueType = (SaImmValueTypeT) ret_msg->attrdefinitions[i]->attrvaluetype;
	 attrDefs[i]->attrFlags = ret_msg->attrdefinitions[i]->attrflags;
	 if(!(ret_msg->attrdefinitions[i]->attrdefaultvalue))
	    attrDefs[i]->attrDefaultValue = NULL;
	 else
	    attrDefs[i]->attrDefaultValue = safc_copy_to_imm_attr_value(ret_msg->attrdefinitions[i]->attrvaluetype,
									ret_msg->attrdefinitions[i]->attrdefaultvalue);
      }

      attrDefs[ret_msg->n_attrdefinitions] = NULL;
      memory = (SafcImmOmMemoryStructT *) calloc(1, sizeof(SafcImmOmMemoryStructT));
      memory->type = class_definition_attrs;
      memory->ptr = (void*) attrDefs;
      (void) safc_array_insert(&(client->allocated_memory), (void*) memory);
      *attrDefinitions = attrDefs;
   }
   /* Store the allocated memory block in handle */
   safs_imm_om_class_description_get2_ret__free_unpacked(ret_msg, NULL);

   TRACE_LEAVE();

   return ret;
}


SaAisErrorT saImmOmClassDescriptionMemoryFree_2(SaImmHandleT immHandle,
						SaImmAttrDefinitionT_2 **attrDefinitions)
{
   SaAisErrorT ret = SA_AIS_OK;
   SafcImmOmClientT *client=NULL;
   int ref;

   TRACE_ENTER();

   LOCK_RDLOCK(&immOmClients_lock);
   client = (SafcImmOmClientT *) safc_array_get(&immOmClients, (int) immHandle);
   LOCK_UNLOCK(&immOmClients_lock);
   if (!client) {
      TRACE1(("ERR_BAD_HANDLE: No initialized om handle exists!"));
      TRACE_LEAVE();
      return SA_AIS_ERR_BAD_HANDLE;
   }

   /* for(i=0; attrDefinitions[i] != NULL; i++) { */
   /*    free(attrDefinitions[i]->attrName); */
   /*    if(attrDefinitions[i]->attrDefaultValue) */
   /* 	 safc_free_imm_attr_value(attrDefinitions[i]->attrValueType, */
   /* 				  attrDefinitions[i]->attrDefaultValue); */
   /*    free(attrDefinitions[i]); */
   /* } */

   ref = safc_array_find(&(client->allocated_memory), (void*) attrDefinitions);
   if(ref) {
      free(safc_array_get(&(client->allocated_memory), ref)) ;
      safc_array_remove(&(client->allocated_memory), ref) ;
   } else
      TRACE1(("Couldn't find memory block in allocated_memory!"));

   safc_free_attr_definitions(attrDefinitions);

   TRACE_LEAVE();

   return ret;
}


SaAisErrorT saImmOmClassDelete(SaImmHandleT immHandle,
			       const SaImmClassNameT className)
{
   SaAisErrorT ret = SA_AIS_OK;
   SaAisErrorT recv_ret = SA_AIS_OK;
   SafcImmOmClientT *client=NULL;
   safc_buf_t msgbuf;
   SafsImmOmClassDelete class_delete_msg = SAFS_IMM_OM_CLASS_DELETE__INIT;
   SafsImmOmMessage msg = SAFS_IMM_OM_MESSAGE__INIT;
   SafsImmOmClassDeleteRet *ret_msg;

   TRACE_ENTER();

   if (className == NULL) {
      TRACE1(("ERR_INVALID_PARAM: classname is NULL\n"));
      TRACE_LEAVE();
      return SA_AIS_ERR_INVALID_PARAM;
   }

   LOCK_RDLOCK(&immOmClients_lock);
   client = (SafcImmOmClientT *) safc_array_get(&immOmClients, (int) immHandle);
   LOCK_UNLOCK(&immOmClients_lock);

   if (!client) {
      TRACE1(("bad handle\n"));
      TRACE_LEAVE();
      return SA_AIS_ERR_BAD_HANDLE;
   }

   class_delete_msg.handle = client->srv_handle;
   class_delete_msg.classname = className;

   msg.classdelete = &class_delete_msg;

   msgbuf.size = safs_imm_om_message__get_packed_size(&msg);
   msgbuf.buf = malloc(msgbuf.size);
   safs_imm_om_message__pack(&msg, (uint8_t *) msgbuf.buf);

   /* Send message & wait for answer */
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
   ret_msg = safs_imm_om_class_delete_ret__unpack(NULL,
						   msgbuf.size,
						   (uint8_t *) msgbuf.buf);
   free(msgbuf.buf);

   if (ret_msg == NULL)
   {
      TRACE1(("SA_AIS_ERR_UNAVAILABLE: Couldn't unpack incoming message\n"));
      return SA_AIS_ERR_UNAVAILABLE;
   }

   ret = ret_msg->returnval;

   safs_imm_om_class_delete_ret__free_unpacked(ret_msg, NULL);

   TRACE_LEAVE();

   return ret;
}


/*
 * ======================================================================
 * 4.5 Object Class Management
 * ======================================================================
 */
SaAisErrorT saImmOmSearchInitialize_2(SaImmHandleT immHandle,
				      const SaNameT *rootName,
				      SaImmScopeT scope,
				      SaImmSearchOptionsT searchOptions,
				      const SaImmSearchParametersT_2 *searchParam,
				      const SaImmAttrNameT *attributeNames,
				      SaImmSearchHandleT *searchHandle)

{
   SaAisErrorT ret = SA_AIS_OK;
   SaAisErrorT recv_ret = SA_AIS_OK;
   SafcImmOmClientT *client=NULL;
   SafcImmSearchClientT *search_ptr = NULL;
   safc_buf_t msgbuf;
   SafsImmOmSearchInitialize2 search_init_msg =  SAFS_IMM_OM_SEARCH_INITIALIZE_2__INIT;
   SafsImmOmMessage msg = SAFS_IMM_OM_MESSAGE__INIT;
   SafsImmOmSearchInitialize2Ret *ret_msg;
   SafsImmSearchParameters2 searchParams = SAFS_IMM_SEARCH_PARAMETERS_2__INIT;
   SafsImmSearchOneAttr2 searchOneAttr = SAFS_IMM_SEARCH_ONE_ATTR_2__INIT;
   SafsImmAttrValue attrVal = SAFS_IMM_ATTR_VALUE__INIT;

   SaImmAttrNameT attributeName;

   TRACE_ENTER();

   if (searchHandle == NULL) {
      TRACE1(("ERR_INVALID_PARAM: 'searchHandle is NULL"));
      TRACE_LEAVE();
      return SA_AIS_ERR_INVALID_PARAM;
   }
   
   *searchHandle = 0;

   if ((searchOptions & SA_IMM_SEARCH_GET_SOME_ATTR) && (attributeNames == NULL)) {
      TRACE1(("ERR_INVALID_PARAM: SA_IMM_SEARCH_GET_SOME_ATTR is set & AttributeName is NULL"));
      TRACE_LEAVE();
      return SA_AIS_ERR_INVALID_PARAM;
   }

   if ((searchParam) && ((searchParam->searchOneAttr.attrName == NULL) &&
			 (searchParam->searchOneAttr.attrValue != NULL))) {
      TRACE1(("ERR_INVALID_PARAM: attrName is NULL but attrValue is not NULL"));
      TRACE_LEAVE();
      return SA_AIS_ERR_INVALID_PARAM;
   }

   if(attributeNames && (!(searchOptions & SA_IMM_SEARCH_GET_SOME_ATTR))) {
      TRACE1(("ERR_IVALID_PARAM: attributeNames != NULL but searchOptions is not set to IMM_SEARCH_GET_SOME_ATTR"));
      TRACE_LEAVE();
      return SA_AIS_ERR_INVALID_PARAM;
   }

   LOCK_RDLOCK(&immOmClients_lock);
   client = (SafcImmOmClientT *) safc_array_get(&immOmClients, (int) immHandle);
   LOCK_UNLOCK(&immOmClients_lock);

   if (!client) {
      TRACE_LEAVE();
      return SA_AIS_ERR_BAD_HANDLE;
   }

   search_ptr = (SafcImmSearchClientT *)calloc(1, sizeof(SafcImmSearchClientT));
   if (search_ptr == NULL) {
      TRACE1(("ERR_NO_MEMORY: IMM Search client structure alloc failed\n"));
      TRACE_LEAVE();
      return SA_AIS_ERR_NO_MEMORY;
   }

   /* Coding of message */
   search_init_msg.handle = client->srv_handle;
   if(rootName != NULL){
      assert(rootName->length < SA_MAX_NAME_LENGTH);
      search_init_msg.rootname = calloc(1, sizeof(SaUint8T) * (rootName->length + 1));
      (void)memcpy(search_init_msg.rootname,  rootName->value, rootName->length);
      search_init_msg.rootname[rootName->length] = '\0';
   }

   search_init_msg.scope = scope;
   search_init_msg.searchoptions = searchOptions;

   if (!searchParam || (!searchParam->searchOneAttr.attrName)) {
      search_init_msg.searchparam = NULL;
   } else {
      searchOneAttr.attrname = searchParam->searchOneAttr.attrName;
      searchOneAttr.attrvaluetype = searchParam->searchOneAttr.attrValueType;
      if(searchParam->searchOneAttr.attrValue) {
	 safc_copy_from_imm_attr_value(&attrVal,
				       searchOneAttr.attrvaluetype,
				       searchParam->searchOneAttr.attrValue);
	 searchOneAttr.attrvalue = &attrVal;
      } else
	 searchOneAttr.attrvalue = NULL;

      searchParams.searchoneattr = &searchOneAttr;
      search_init_msg.searchparam = &searchParams;
   }

   if (attributeNames) {
      int i;

      /* Calculate number of attribute names */
      for (i = 0; attributeNames[i]; i++);

      search_init_msg.n_attributenames = i;
      search_init_msg.attributenames = calloc (1, sizeof (char*) * search_init_msg.n_attributenames);

      for (i = 0; i < search_init_msg.n_attributenames; i++) {
	 attributeName = attributeNames[i];
	 search_init_msg.attributenames[i] = (char*) attributeName;
      }
   } else {
      search_init_msg.n_attributenames = 0;
      search_init_msg.attributenames = NULL;
   }

   msg.searchinitialize_2 = &search_init_msg;

   msgbuf.size = safs_imm_om_message__get_packed_size(&msg);
   msgbuf.buf = malloc(msgbuf.size);
   safs_imm_om_message__pack(&msg, (uint8_t *) msgbuf.buf);

   free(search_init_msg.attributenames);
   free(search_init_msg.rootname);

   /* Send message & wait for answer*/
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
   ret_msg = safs_imm_om_search_initialize2_ret__unpack(NULL,
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
   search_ptr->immHandle = immHandle;
   search_ptr->srv_handle = ret_msg->handle;
   search_ptr->objectsOrAttributes = 0;
   search_ptr->objects = NULL;
   search_ptr->attributes = NULL;

   safs_imm_om_search_initialize2_ret__free_unpacked(ret_msg, NULL);
   
   if(ret != SA_AIS_OK)
      goto error;

   LOCK_WRLOCK(&immOmClients_lock);
   *searchHandle = (SaImmSearchHandleT) safc_array_insert(&immOmClients, (void*) search_ptr);
   LOCK_UNLOCK(&immOmClients_lock);
   
   TRACE_LEAVE();
   return ret;

 error:
   free(search_ptr);

   TRACE_LEAVE();
   return ret;
}

SaAisErrorT saImmOmSearchClassInitialize_s2(SaImmHandleT immHandle,
					    const SaNameT *rootName,
					    SaImmScopeT scope,
					    SaImmSearchOptionsT searchOptions,
					    const SaStringT *classNames,
					    const SaImmAttrNameT *attributeNames,
					    SaImmSearchHandleT *searchHandle)

{
   SaAisErrorT ret = SA_AIS_OK;
   SaAisErrorT recv_ret = SA_AIS_OK;
   SafcImmOmClientT *client=NULL;
   SafcImmSearchClientT *search_ptr = NULL;
   safc_buf_t msgbuf;
   SafsImmOmSearchClassInitializeS2 search_init_msg =  SAFS_IMM_OM_SEARCH_CLASS_INITIALIZE_S2__INIT;
   SafsImmOmMessage msg = SAFS_IMM_OM_MESSAGE__INIT;
   SafsImmOmSearchClassInitializeS2Ret *ret_msg;
   SaStringT className;
   SaImmAttrNameT attributeName;

   TRACE_ENTER();

   if (searchHandle == NULL) {
      TRACE1(("ERR_INVALID_PARAM: 'searchHandle is NULL"));
      TRACE_LEAVE();
      return SA_AIS_ERR_INVALID_PARAM;
   }

   *searchHandle = 0;
   
   if ((searchOptions & SA_IMM_SEARCH_GET_SOME_ATTR) && (attributeNames == NULL)) {
      TRACE1(("ERR_INVALID_PARAM: SA_IMM_SEARCH_GET_SOME_ATTR is set & AttributeName is NULL"));
      TRACE_LEAVE();
      return SA_AIS_ERR_INVALID_PARAM;
   }

   if(classNames == NULL) {
      TRACE1(("ERR_IVALID_PARAM: classNames is NULL"));
      TRACE_LEAVE();
      return SA_AIS_ERR_INVALID_PARAM;
   }

   if(attributeNames && (!(searchOptions & SA_IMM_SEARCH_GET_SOME_ATTR))) {
      TRACE1(("ERR_IVALID_PARAM: attributeNames != NULL but searchOptions is not set to IMM_SEARCH_GET_SOME_ATTR"));
      TRACE_LEAVE();
      return SA_AIS_ERR_INVALID_PARAM;
   }

   LOCK_RDLOCK(&immOmClients_lock);
   client = (SafcImmOmClientT *) safc_array_get(&immOmClients, (int) immHandle);
   LOCK_UNLOCK(&immOmClients_lock);

   if (!client) {
      TRACE_LEAVE();
      return SA_AIS_ERR_BAD_HANDLE;
   }

   search_ptr = (SafcImmSearchClientT *)calloc(1, sizeof(SafcImmSearchClientT));

   if (search_ptr == NULL) {
      TRACE1(("ERR_NO_MEMORY: IMM Search client structure alloc failed\n"));
      TRACE_LEAVE();
      return SA_AIS_ERR_NO_MEMORY;
   }

   LOCK_WRLOCK(&immOmClients_lock);
   *searchHandle = (SaImmSearchHandleT) safc_array_insert(&immOmClients, (void*) search_ptr);
   LOCK_UNLOCK(&immOmClients_lock);

   /* Coding of message */
   search_init_msg.handle = client->srv_handle;
   if(rootName != NULL){
      assert(rootName->length < SA_MAX_NAME_LENGTH);
      search_init_msg.rootname = calloc(1, sizeof(SaUint8T) * (rootName->length + 1));
      (void)memcpy(search_init_msg.rootname,  rootName->value, rootName->length);
      search_init_msg.rootname[rootName->length] = '\0';
   }

   search_init_msg.scope = scope;
   search_init_msg.searchoptions = searchOptions;

   if (classNames) {
      int i;

      /* Calculate number of attribute names */
      for (i = 0; classNames[i]; i++);

      search_init_msg.n_classnames = i;
      search_init_msg.classnames = calloc (1, sizeof (char*) * search_init_msg.n_classnames);

      for (i = 0; i < search_init_msg.n_classnames; i++) {
	 className = classNames[i];
	 search_init_msg.classnames[i] = (char*) className;
      }
   } else {
      search_init_msg.n_classnames = 0;
      search_init_msg.classnames = NULL;
   }

   if (attributeNames) {
      int i;

      /* Calculate number of attribute names */
      for (i = 0; attributeNames[i]; i++);

      search_init_msg.n_attributenames = i;
      search_init_msg.attributenames = calloc (1, sizeof (char*) * search_init_msg.n_attributenames);

      for (i = 0; i < search_init_msg.n_attributenames; i++) {
	 attributeName = attributeNames[i];
	 search_init_msg.attributenames[i] = (char*) attributeName;
      }
   } else {
      search_init_msg.n_attributenames = 0;
      search_init_msg.attributenames = NULL;
   }

   msg.searchclassinitialize_s2 = &search_init_msg;

   msgbuf.size = safs_imm_om_message__get_packed_size(&msg);
   msgbuf.buf = malloc(msgbuf.size);
   safs_imm_om_message__pack(&msg, (uint8_t *) msgbuf.buf);

   free(search_init_msg.classnames);
   free(search_init_msg.attributenames);
   free(search_init_msg.rootname);

   /* Send message & wait for answer*/
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
   ret_msg = safs_imm_om_search_class_initialize_s2_ret__unpack(NULL,
						       msgbuf.size,
						       (uint8_t *) msgbuf.buf);
   free(msgbuf.buf);

   if (ret_msg == NULL)
   {
      TRACE1(("SA_AIS_ERR_UNAVAILABLE: Couldn't unpack incoming message\n"));
      ret = SA_AIS_ERR_UNAVAILABLE;
      goto error;
   }

   ret = ret_msg->returnval;
   search_ptr->immHandle = immHandle;
   search_ptr->srv_handle = ret_msg->handle;
   search_ptr->objectsOrAttributes = 0;
   search_ptr->objects = NULL;
   search_ptr->attributes = NULL;

   safs_imm_om_search_class_initialize_s2_ret__free_unpacked(ret_msg, NULL);

   if(ret != SA_AIS_OK)
      goto error;

   LOCK_WRLOCK(&immOmClients_lock);
   *searchHandle = (SaImmSearchHandleT) safc_array_insert(&immOmClients, (void*) search_ptr);
   LOCK_UNLOCK(&immOmClients_lock);

   TRACE_LEAVE();
   return ret;

error:
   
   free(search_ptr);
   
   TRACE_LEAVE();
   return ret;
}

SaAisErrorT saImmOmSearchNext_2(SaImmSearchHandleT searchHandle,
				SaNameT *objectName,
				SaImmAttrValuesT_2 ***attributes)
{
   SaAisErrorT ret = SA_AIS_OK;
   SaAisErrorT recv_ret = SA_AIS_OK;
   SafcImmSearchClientT *search_ptr = NULL;
   SafcImmOmClientT* client;
   safc_buf_t msgbuf;
   SafsImmOmSearchNext2 search_next_msg =  SAFS_IMM_OM_SEARCH_NEXT_2__INIT;
   SafsImmOmMessage msg = SAFS_IMM_OM_MESSAGE__INIT;
   SafsImmOmSearchNext2Ret *ret_msg;

   TRACE_ENTER();

   if (!objectName) {
      TRACE1(("ERR_INVALID_PARAM: Invalid parameter: objectName"));
      return SA_AIS_ERR_INVALID_PARAM;
   }

   if (!attributes) {
      TRACE1(("ERR_INVALID_PARAM: Invalid parameter: attributes"));
      return SA_AIS_ERR_INVALID_PARAM;
   }

   LOCK_RDLOCK(&immOmClients_lock);
   search_ptr = (SafcImmSearchClientT *) safc_array_get(&immOmClients, (int) searchHandle);
   LOCK_UNLOCK(&immOmClients_lock);

   if (!search_ptr) {
      TRACE1(("ERR_BAD_HANDLE: No initialized search handle exists!"));
      return SA_AIS_ERR_BAD_HANDLE;
   }

   /* Free memory from previous search */
   switch (search_ptr->objectsOrAttributes) {
   case 1:
      safc_free_imm_attributes(search_ptr->attributes);
      search_ptr->objectsOrAttributes = 0;
      search_ptr->attributes = NULL;
      break;
   case 2:
      safc_free_imm_objects(search_ptr->objects);
      search_ptr->objectsOrAttributes = 0;
      search_ptr->objects = NULL;
      break;
   default:
      break;
   }

   LOCK_RDLOCK(&immOmClients_lock);
   client = (SafcImmOmClientT *) safc_array_get(&immOmClients, (int) search_ptr->immHandle);
   LOCK_UNLOCK(&immOmClients_lock);

   if (!client) {
      TRACE1(("ERR_BAD_HANDLE: No initialized imm om handle exists for the search handle!"));
      return SA_AIS_ERR_BAD_HANDLE;
   }

   /* Coding of message */

   search_next_msg.searchhandle = search_ptr->srv_handle;
   msg.searchnext_2 = &search_next_msg;

   msgbuf.size = safs_imm_om_message__get_packed_size(&msg);
   msgbuf.buf = malloc(msgbuf.size);
   safs_imm_om_message__pack(&msg, (uint8_t *) msgbuf.buf);

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
   ret_msg = safs_imm_om_search_next2_ret__unpack(NULL,
						  msgbuf.size,
						  (uint8_t *) msgbuf.buf);
   free(msgbuf.buf);

   if (ret_msg == NULL)
   {
      TRACE1(("SA_AIS_ERR_UNAVAILABLE: Couldn't unpack incoming message\n"));
      return SA_AIS_ERR_UNAVAILABLE;
   }

   ret = ret_msg->returnval;
   if(ret == SA_AIS_OK) {
      int i;
      SaImmAttrValuesT_2 **attr = NULL;

      objectName->length = strlen(ret_msg->objectname);
      memcpy(objectName->value, ret_msg->objectname, objectName->length);

      attr = calloc(1, sizeof(SaImmAttrValuesT_2 *) * (ret_msg->n_attributes + 1));

      for (i=0; i < ret_msg->n_attributes; i++) {
	 attr[i] = calloc(1, sizeof(SaImmAttrValuesT_2));
	 safc_copy_to_imm_attributes(ret_msg->attributes[i], attr[i]);
      }

      attr[ret_msg->n_attributes] = NULL;
      *attributes = attr;
      search_ptr->objectsOrAttributes = 1;
      search_ptr->attributes = attr;
   }

   safs_imm_om_search_next2_ret__free_unpacked(ret_msg, NULL);

   TRACE_LEAVE();

   return ret;
}

SaAisErrorT saImmOmSearchNextN_s2(SaImmSearchHandleT searchHandle,
				  SaUint32T requestedNumberOfObjects,
				  SaImmSearchObjectsT_s2 ***objects)
{
   SaAisErrorT ret = SA_AIS_OK;
   SaAisErrorT recv_ret = SA_AIS_OK;
   SafcImmSearchClientT *search_ptr = NULL;
   SafcImmOmClientT* client;
   safc_buf_t msgbuf;
   SafsImmOmSearchNextNS2 search_next_n_msg =  SAFS_IMM_OM_SEARCH_NEXT_N_S2__INIT;
   SafsImmOmMessage msg = SAFS_IMM_OM_MESSAGE__INIT;
   SafsImmOmSearchNextNS2Ret *ret_msg;

   TRACE_ENTER();

   if (!objects) {
      TRACE1(("ERR_INVALID_PARAM: Invalid parameter: objects"));
      return SA_AIS_ERR_INVALID_PARAM;
   }

   LOCK_RDLOCK(&immOmClients_lock);
   search_ptr = (SafcImmSearchClientT *) safc_array_get(&immOmClients, (int) searchHandle);
   LOCK_UNLOCK(&immOmClients_lock);

   if (!search_ptr) {
      TRACE1(("ERR_BAD_HANDLE: No initialized search handle exists!"));
      return SA_AIS_ERR_BAD_HANDLE;
   }

   /* Free memory from previous search */
   switch (search_ptr->objectsOrAttributes) {
   case 1:
      safc_free_imm_attributes(search_ptr->attributes);
      search_ptr->objectsOrAttributes = 0;
      search_ptr->attributes = NULL;
      break;
   case 2:
      safc_free_imm_objects(search_ptr->objects);
      search_ptr->objectsOrAttributes = 0;
      search_ptr->objects = NULL;
      break;
   default:
      break;
   }

   LOCK_RDLOCK(&immOmClients_lock);
   client = (SafcImmOmClientT *) safc_array_get(&immOmClients, (int) search_ptr->immHandle);
   LOCK_UNLOCK(&immOmClients_lock);

   if (!client) {
      TRACE1(("ERR_BAD_HANDLE: No initialized imm om handle exists for the search handle!"));
      return SA_AIS_ERR_BAD_HANDLE;
   }

   /* Coding of message */

   search_next_n_msg.searchhandle = search_ptr->srv_handle;
   search_next_n_msg.numberofobjects = requestedNumberOfObjects;
   msg.searchnextn_s2 = &search_next_n_msg;

   msgbuf.size = safs_imm_om_message__get_packed_size(&msg);
   msgbuf.buf = malloc(msgbuf.size);
   safs_imm_om_message__pack(&msg, (uint8_t *) msgbuf.buf);

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
   ret_msg = safs_imm_om_search_next_ns2_ret__unpack(NULL,
						     msgbuf.size,
						     (uint8_t *) msgbuf.buf);
   free(msgbuf.buf);

   if (ret_msg == NULL)
   {
      TRACE1(("SA_AIS_ERR_UNAVAILABLE: Couldn't unpack incoming message\n"));
      return SA_AIS_ERR_UNAVAILABLE;
   }

   ret = ret_msg->returnval;
   if(ret == SA_AIS_OK) {
      int i, j;
      SaImmSearchObjectsT_s2 **objs = NULL;
      SaImmAttrValuesT_2 **attr = NULL;
      SafsImmSearchObjectsS2 *received_object;
      SaImmSearchObjectsT_s2 *object;

      objs = calloc(1, sizeof(SaImmSearchObjectsT_s2 *) * (ret_msg->n_searchobjects + 1));

      for (i=0; i < ret_msg->n_searchobjects; i++) {
	 received_object = ret_msg->searchobjects[i];
	 
	 object = calloc(1, sizeof(SaImmSearchObjectsT_s2));
	 object->objectName.length = strlen(received_object->objectname);
	 memcpy(object->objectName.value, received_object->objectname, object->objectName.length);

	 attr = calloc(1, sizeof(SaImmAttrValuesT_2 *) * (received_object->n_attributes + 1));

	 for (j=0; j < received_object->n_attributes; j++) {
	    attr[j] = calloc(1, sizeof(SaImmAttrValuesT_2));
	    safc_copy_to_imm_attributes(received_object->attributes[j], attr[j]);
	 }

	 attr[received_object->n_attributes] = NULL;
	 object->attributes = attr;
	 objs[i] = object;
      }

      objs[ret_msg->n_searchobjects] = NULL;
      *objects = objs;
      search_ptr->objectsOrAttributes = 2;
      search_ptr->objects = objs;
   }

   safs_imm_om_search_next_ns2_ret__free_unpacked(ret_msg, NULL);

   TRACE_LEAVE();

   return ret;
}



SaAisErrorT saImmOmSearchFinalize(SaImmSearchHandleT searchHandle)
{
   SaAisErrorT ret = SA_AIS_OK;
   SaAisErrorT recv_ret = SA_AIS_OK;
   SafcImmOmClientT *client=NULL;
   SafcImmSearchClientT *search_ptr = NULL;
   safc_buf_t msgbuf;
   SafsImmOmSearchFinalize search_finalize_msg =  SAFS_IMM_OM_SEARCH_FINALIZE__INIT;
   SafsImmOmMessage msg = SAFS_IMM_OM_MESSAGE__INIT;
   SafsImmOmSearchFinalizeRet *ret_msg;

   TRACE_ENTER();

   LOCK_RDLOCK(&immOmClients_lock);
   search_ptr = (SafcImmSearchClientT *) safc_array_get(&immOmClients, (int) searchHandle);
   LOCK_UNLOCK(&immOmClients_lock);

   if (!search_ptr) {
      TRACE_LEAVE();
      return SA_AIS_ERR_BAD_HANDLE;
   }

   /* Free memory from previous search */
   switch (search_ptr->objectsOrAttributes) {
   case 1:
      safc_free_imm_attributes(search_ptr->attributes);
      search_ptr->objectsOrAttributes = 0;
      search_ptr->attributes = NULL;
      break;
   case 2:
      safc_free_imm_objects(search_ptr->objects);
      search_ptr->objectsOrAttributes = 0;
      search_ptr->objects = NULL;
      break;
   default:
      break;
   }

   LOCK_RDLOCK(&immOmClients_lock);
   client = (SafcImmOmClientT *) safc_array_get(&immOmClients, (int) search_ptr->immHandle);
   LOCK_UNLOCK(&immOmClients_lock);

   if (!client) {
      TRACE_LEAVE();
      return SA_AIS_ERR_BAD_HANDLE;
   }

   /* Coding of message */

   search_finalize_msg.searchhandle = search_ptr->srv_handle;

   msg.searchfinalize = &search_finalize_msg;

   msgbuf.size = safs_imm_om_message__get_packed_size(&msg);
   msgbuf.buf = malloc(msgbuf.size);
   safs_imm_om_message__pack(&msg, (uint8_t *) msgbuf.buf);

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
   ret_msg = safs_imm_om_search_finalize_ret__unpack(NULL,
						  msgbuf.size,
						  (uint8_t *) msgbuf.buf);
   free(msgbuf.buf);

   if (ret_msg == NULL)
   {
      TRACE1(("SA_AIS_ERR_UNAVAILABLE: Couldn't unpack incoming message\n"));
      return SA_AIS_ERR_UNAVAILABLE;
   }

   ret = ret_msg->returnval;

   safs_imm_om_search_finalize_ret__free_unpacked(ret_msg, NULL);

   LOCK_WRLOCK(&immOmClients_lock);
   safc_array_remove(&immOmClients, (int) searchHandle);
   LOCK_UNLOCK(&immOmClients_lock);

   free(search_ptr);

   TRACE_LEAVE();

   return ret;
}


/*
 * ======================================================================
 * 4.6 Object Access
 * ======================================================================
 */
SaAisErrorT saImmOmAccessorInitialize(SaImmHandleT immHandle,
				      SaImmAccessorHandleT *accessorHandle)
{
   SaAisErrorT ret = SA_AIS_OK;
   SaAisErrorT recv_ret = SA_AIS_OK;
   SafcImmOmClientT *client=NULL;
   SafcImmAccessorClientT *acc_ptr = NULL;
   safc_buf_t msgbuf;
   SafsImmOmAccessorInitialize initialize_msg =  SAFS_IMM_OM_ACCESSOR_INITIALIZE__INIT;
   SafsImmOmMessage msg = SAFS_IMM_OM_MESSAGE__INIT;
   SafsImmOmAccessorInitializeRet *ret_msg;

   TRACE_ENTER();

   if (accessorHandle == NULL) {
      TRACE1(("ERR_INVALID_PARAM: 'accessorHandle is NULL"));
      TRACE_LEAVE();
      return SA_AIS_ERR_INVALID_PARAM;
   }
      
   *accessorHandle = 0;

   LOCK_RDLOCK(&immOmClients_lock);
   client = (SafcImmOmClientT *) safc_array_get(&immOmClients, (int) immHandle);
   LOCK_UNLOCK(&immOmClients_lock);

   if (!client) {
      TRACE_LEAVE();
      return SA_AIS_ERR_BAD_HANDLE;
   }

   acc_ptr = (SafcImmAccessorClientT *)calloc(1, sizeof(SafcImmAccessorClientT));

   if (acc_ptr == NULL) {
      TRACE1(("ERR_NO_MEMORY: IMM Accessor client structure alloc failed\n"));
      TRACE_LEAVE();
      return SA_AIS_ERR_NO_MEMORY;
   }

   /* Coding of message */

   initialize_msg.handle = immHandle;

   msg.accessorinitialize = &initialize_msg;

   msgbuf.size = safs_imm_om_message__get_packed_size(&msg);
   msgbuf.buf = malloc(msgbuf.size);
   safs_imm_om_message__pack(&msg, (uint8_t *) msgbuf.buf);

   /* Send message & wait for answer*/

   MUTEX_LOCK(&(client->mutex));
   recv_ret = safc_send_recv(client->sockfd, &msgbuf);
   MUTEX_UNLOCK(&(client->mutex));

   if(recv_ret == SA_AIS_ERR_MESSAGE_ERROR)
   {
      TRACE1(("SA_AIS_ERR_UNAVAILABLE: Couldn't send/read from socket\n"));
      free(msgbuf.buf);
      ret =  SA_AIS_ERR_UNAVAILABLE; // Perhaps another error code;
      goto error;
   }

   /* Decoding of answer */
   ret_msg = safs_imm_om_accessor_initialize_ret__unpack(NULL,
							 msgbuf.size,
							 (uint8_t *) msgbuf.buf);
   free(msgbuf.buf);

   if (ret_msg == NULL)
   {
      TRACE1(("SA_AIS_ERR_UNAVAILABLE: Couldn't unpack incoming message\n"));
      ret = SA_AIS_ERR_UNAVAILABLE;
      goto error;
   }

   ret = ret_msg->returnval;
   acc_ptr->immHandle = immHandle;
   acc_ptr->srv_handle = ret_msg->handle;

   safs_imm_om_accessor_initialize_ret__free_unpacked(ret_msg, NULL);

   if(ret != SA_AIS_OK) 
      goto error;

   LOCK_WRLOCK(&immOmClients_lock);
   *accessorHandle = safc_array_insert(&immOmClients, (void*) acc_ptr);
   LOCK_UNLOCK(&immOmClients_lock);

   TRACE_LEAVE();
   return ret;

 error:
   free(acc_ptr);
   
   TRACE_LEAVE();
   return ret;
}

SaAisErrorT saImmOmAccessorGet_2(SaImmAccessorHandleT accessorHandle,
				 const SaNameT *objectName,
				 const SaImmAttrNameT *attributeNames,
				 SaImmAttrValuesT_2 ***attributes)
{
   SaAisErrorT ret = SA_AIS_OK;
   SaAisErrorT recv_ret = SA_AIS_OK;
   SafcImmOmClientT *client=NULL;
   SafcImmAccessorClientT *acc_ptr = NULL;
   safc_buf_t msgbuf;
   SafsImmOmAccessorGet2 get_msg =  SAFS_IMM_OM_ACCESSOR_GET_2__INIT;
   SafsImmOmMessage msg = SAFS_IMM_OM_MESSAGE__INIT;
   SafsImmOmAccessorGet2Ret *ret_msg;

   SaImmAttrNameT attributeName;

   TRACE_ENTER();

   if (objectName == NULL || (objectName->length == 0) || (objectName->length >= SA_MAX_NAME_LENGTH)) {
      TRACE1(("ERR_INVALID_PARAM: objectName"));
      TRACE_LEAVE();
      return SA_AIS_ERR_INVALID_PARAM;
   }

   if (!attributes) {
      TRACE1(("ERR_INVALID_PARAM: Invalid parameter: attributes"));
      TRACE_LEAVE();
      return SA_AIS_ERR_INVALID_PARAM;
   }

   LOCK_RDLOCK(&immOmClients_lock);
   acc_ptr = (SafcImmAccessorClientT *) safc_array_get(&immOmClients, (int) accessorHandle);
   LOCK_UNLOCK(&immOmClients_lock);

   if (!acc_ptr) {
      TRACE1(("ERR_BAD_HANDLE: No initialized accessor handle exists!"));
      TRACE_LEAVE();
      return SA_AIS_ERR_BAD_HANDLE;
   }

   /* Free memory from previous search */
   if (acc_ptr->attributes) {
       safc_free_imm_attributes(acc_ptr->attributes);
       acc_ptr->attributes = NULL;
   }

   LOCK_RDLOCK(&immOmClients_lock);
   client = (SafcImmOmClientT *) safc_array_get(&immOmClients, (int) acc_ptr->immHandle);
   LOCK_UNLOCK(&immOmClients_lock);

   if (!client) {
      TRACE1(("ERR_BAD_HANDLE: No initialized imm om handle exists for the accessor handle!"));
      TRACE_LEAVE();
      return SA_AIS_ERR_BAD_HANDLE;
   }

   /* Coding of message */

   get_msg.accessorhandle = acc_ptr->srv_handle;
   if(objectName != NULL){
      assert(objectName->length < SA_MAX_NAME_LENGTH);
      get_msg.objectname = calloc(1, sizeof(SaUint8T) * (objectName->length + 1));
      (void)memcpy(get_msg.objectname,  objectName->value, objectName->length);
      get_msg.objectname[objectName->length] = '\0';
   }

   if (attributeNames) {
      int i;
      /* Calculate number of attribute names */
      for (i = 0; attributeNames[i]; i++);

      get_msg.n_attributenames = i;
      get_msg.attributenames = calloc (1, sizeof (char*) * get_msg.n_attributenames);

      for (i = 0; i < get_msg.n_attributenames; i++) {
	 attributeName = attributeNames[i];
	 get_msg.attributenames[i] = (char*) attributeName;
      }
   } else {
      get_msg.n_attributenames = 0;
      get_msg.attributenames = NULL;
   }

   msg.accessorget_2 = &get_msg;

   msgbuf.size = safs_imm_om_message__get_packed_size(&msg);
   msgbuf.buf = malloc(msgbuf.size);
   safs_imm_om_message__pack(&msg, (uint8_t *) msgbuf.buf);

   free(get_msg.attributenames);
   free(get_msg.objectname);

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
   ret_msg = safs_imm_om_accessor_get2_ret__unpack(NULL,
						   msgbuf.size,
						   (uint8_t *) msgbuf.buf);
   free(msgbuf.buf);

   if (ret_msg == NULL)
   {
      TRACE1(("SA_AIS_ERR_UNAVAILABLE: Couldn't unpack incoming message\n"));
      return SA_AIS_ERR_UNAVAILABLE;
   }

   ret = ret_msg->returnval;
   if(ret == SA_AIS_OK) {
      int i;
      SaImmAttrValuesT_2 **attr = NULL;

      attr = calloc(1, sizeof(SaImmAttrValuesT_2 *) * (ret_msg->n_attributes + 1));

      for (i=0; i < ret_msg->n_attributes; i++) {
	 attr[i] = calloc(1, sizeof(SaImmAttrValuesT_2));
	 safc_copy_to_imm_attributes(ret_msg->attributes[i], attr[i]);
      }

      attr[ret_msg->n_attributes] = NULL;
      *attributes = attr;
      acc_ptr->attributes = attr;

   }

   safs_imm_om_accessor_get2_ret__free_unpacked(ret_msg, NULL);

   TRACE_LEAVE();

   return ret;
}

SaAisErrorT saImmOmAccessorFinalize(SaImmAccessorHandleT accessorHandle)
{
   SaAisErrorT ret = SA_AIS_OK;
   SaAisErrorT recv_ret = SA_AIS_OK;
   SafcImmOmClientT *client=NULL;
   SafcImmAccessorClientT *acc_ptr = NULL;
   safc_buf_t msgbuf;
   SafsImmOmAccessorFinalize finalize_msg =  SAFS_IMM_OM_ACCESSOR_FINALIZE__INIT;
   SafsImmOmMessage msg = SAFS_IMM_OM_MESSAGE__INIT;
   SafsImmOmAccessorFinalizeRet *ret_msg;

   TRACE_ENTER();

   LOCK_RDLOCK(&immOmClients_lock);
   acc_ptr = (SafcImmAccessorClientT *) safc_array_get(&immOmClients, (int) accessorHandle);
   LOCK_UNLOCK(&immOmClients_lock);

   if (!acc_ptr) {
      TRACE_LEAVE();
      return SA_AIS_ERR_BAD_HANDLE;
   }

   /* Free memory from previous search */
   if (acc_ptr->attributes) {
       safc_free_imm_attributes(acc_ptr->attributes);
       acc_ptr->attributes = NULL;
   }

   LOCK_RDLOCK(&immOmClients_lock);
   client = (SafcImmOmClientT *) safc_array_get(&immOmClients, (int) acc_ptr->immHandle);
   LOCK_UNLOCK(&immOmClients_lock);

   if (!client) {
      TRACE_LEAVE();
      return SA_AIS_ERR_BAD_HANDLE;
   }

  /* Coding of message */

   finalize_msg.accessorhandle = acc_ptr->srv_handle;

   msg.accessorfinalize = &finalize_msg;

   msgbuf.size = safs_imm_om_message__get_packed_size(&msg);
   msgbuf.buf = malloc(msgbuf.size);
   safs_imm_om_message__pack(&msg, (uint8_t *) msgbuf.buf);

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
   ret_msg = safs_imm_om_accessor_finalize_ret__unpack(NULL,
						       msgbuf.size,
						       (uint8_t *) msgbuf.buf);
   free(msgbuf.buf);

   if (ret_msg == NULL)
   {
      TRACE1(("SA_AIS_ERR_UNAVAILABLE: Couldn't unpack incoming message\n"));
      return SA_AIS_ERR_UNAVAILABLE;
   }

   ret = ret_msg->returnval;

   safs_imm_om_accessor_finalize_ret__free_unpacked(ret_msg, NULL);

   LOCK_WRLOCK(&immOmClients_lock);
   safc_array_remove(&immOmClients, (int) accessorHandle);
   LOCK_UNLOCK(&immOmClients_lock);

   free(acc_ptr);

   TRACE_LEAVE();
   return ret;
}


/*
 * ======================================================================
 * 4.7 Object Administration Ownership
 * ======================================================================
 */
SaAisErrorT saImmOmAdminOwnerInitialize(SaImmHandleT immHandle,
					const SaImmAdminOwnerNameT adminOwnerName,
					SaBoolT releaseOwnershipOnFinalize,
					SaImmAdminOwnerHandleT *ownerHandle)
{
   SaAisErrorT ret = SA_AIS_OK;
   SaAisErrorT recv_ret = SA_AIS_OK;
   SafcImmAdminOwnerClientT *ao_ptr = NULL;
   SafcImmOmClientT *client;
   SaUint32T nameLength = 0;
   safc_buf_t msgbuf;
   SafsImmOmAdminOwnerInitialize ao_init_msg =  SAFS_IMM_OM_ADMIN_OWNER_INITIALIZE__INIT;
   SafsImmOmMessage msg = SAFS_IMM_OM_MESSAGE__INIT;
   SafsImmOmAdminOwnerInitializeRet *ret_msg;

   TRACE_ENTER();

   if ((ownerHandle == NULL) || (adminOwnerName == NULL) ||
       (nameLength = strlen(adminOwnerName)) == 0) {
      TRACE1(("ERR_INVALID_PARAM: 'adminOwnerHandle is NULL, or adminOwnerName is NULL,"
	      "or adminOwnerName has zero length"));
      TRACE_LEAVE();
      return SA_AIS_ERR_INVALID_PARAM;
   }

   *ownerHandle = 0;

   if (releaseOwnershipOnFinalize && (releaseOwnershipOnFinalize != 1)) {
      TRACE1(("ERR_INVALID_PARAM: releaseOwnershipOnFinalize must be 1 or 0"));
      TRACE_LEAVE();
      return SA_AIS_ERR_INVALID_PARAM;
   }

   if (nameLength >= SA_MAX_NAME_LENGTH) {
      TRACE1(("ERR_INVALID_PARAM: Admin owner name too long, size: %u max:%u",
	      nameLength, SA_MAX_NAME_LENGTH - 1));
      TRACE_LEAVE();
      return SA_AIS_ERR_INVALID_PARAM;
   }

   LOCK_RDLOCK(&immOmClients_lock);
   client = (SafcImmOmClientT *) safc_array_get(&immOmClients, (int) immHandle);
   LOCK_UNLOCK(&immOmClients_lock);

   if (!client) {
      TRACE_LEAVE();
      return SA_AIS_ERR_BAD_HANDLE;
   }

   ao_ptr = (SafcImmAdminOwnerClientT *)calloc(1, sizeof(SafcImmAdminOwnerClientT));
   if (ao_ptr == NULL) {
      TRACE1(("ERR_NO_MEMORY: IMM Admin Owner client structure alloc failed\n"));
      TRACE_LEAVE();
      return SA_AIS_ERR_NO_MEMORY;
   }

   ao_ptr->immHandle = immHandle;
   ao_ptr->releaseOnFinalize = releaseOwnershipOnFinalize;

   /* Coding of message */

   ao_init_msg.handle = client->srv_handle;
   ao_init_msg.adminownername = adminOwnerName;
   ao_init_msg.releaseownershiponfinalize = releaseOwnershipOnFinalize;

   msg.adminownerinitialize = &ao_init_msg;

   msgbuf.size = safs_imm_om_message__get_packed_size(&msg);
   msgbuf.buf = malloc(msgbuf.size);
   safs_imm_om_message__pack(&msg, (uint8_t *) msgbuf.buf);

   /* Send message & wait for answer */
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
   ret_msg = safs_imm_om_admin_owner_initialize_ret__unpack(NULL,
							    msgbuf.size,
							    (uint8_t *) msgbuf.buf);
   free(msgbuf.buf);

   if (ret_msg == NULL)
   {
      TRACE1(("SA_AIS_ERR_UNAVAILABLE: Couldn't unpack incoming message\n"));
      ret = SA_AIS_ERR_UNAVAILABLE;
      goto error;
   }

   ret = ret_msg->returnval;
   ao_ptr->adminOwnerName = calloc(1, nameLength+1);
   strncpy(ao_ptr->adminOwnerName, adminOwnerName, nameLength);
   ao_ptr->srv_handle = ret_msg->handle;

   safs_imm_om_admin_owner_initialize_ret__free_unpacked(ret_msg, NULL);

   if(ret != SA_AIS_OK) 
      goto error;
      
   LOCK_WRLOCK(&immOmClients_lock);
   *ownerHandle = (SaImmAdminOwnerHandleT) safc_array_insert(&immOmClients, (void*) ao_ptr);
   LOCK_UNLOCK(&immOmClients_lock);

   TRACE_LEAVE();
   return ret;

error:

   free(ao_ptr->adminOwnerName);  
   free(ao_ptr);

   TRACE_LEAVE();
   return ret;

}

SaAisErrorT saImmOmAdminOwnerSet(SaImmAdminOwnerHandleT ownerHandle,
				 const SaNameT **objectNames,
				 SaImmScopeT scope)
{
   SaAisErrorT ret = SA_AIS_OK;
   SaAisErrorT recv_ret = SA_AIS_OK;
   SafcImmAdminOwnerClientT *ao_ptr = NULL;
   SafcImmOmClientT *client;
   safc_buf_t msgbuf;
   SafsImmOmAdminOwnerSet set_msg =  SAFS_IMM_OM_ADMIN_OWNER_SET__INIT;
   SafsImmOmMessage msg = SAFS_IMM_OM_MESSAGE__INIT;
   SafsImmOmAdminOwnerSetRet *ret_msg;
   int i;
   const SaNameT *objectName;

   TRACE_ENTER();

   if (!objectNames || (objectNames[0] == 0)) {
      return SA_AIS_ERR_INVALID_PARAM;
   }

   switch (scope) {
   case SA_IMM_ONE:
   case SA_IMM_SUBLEVEL:
   case SA_IMM_SUBTREE:
      break;
   default:
      return SA_AIS_ERR_INVALID_PARAM;
   }

   LOCK_RDLOCK(&immOmClients_lock);
   ao_ptr = (SafcImmAdminOwnerClientT *) safc_array_get(&immOmClients, (int) ownerHandle);
   LOCK_UNLOCK(&immOmClients_lock);

   if (!ao_ptr)
      return SA_AIS_ERR_BAD_HANDLE;

   /* Coding of message */
   set_msg.ownerhandle = ao_ptr->srv_handle;

   for (i = 0; objectNames[i]; i++);
   set_msg.n_objectnames = i;

   set_msg.objectnames = calloc (1, sizeof (char*) * set_msg.n_objectnames);

   for (i = 0; i < set_msg.n_objectnames; i++) {
      objectName = objectNames[i];
      assert(objectName->length < SA_MAX_NAME_LENGTH);
      set_msg.objectnames[i] = calloc(1, sizeof(SaUint8T) * (objectName->length + 1));      
      (void)memcpy(set_msg.objectnames[i], objectName->value, objectName->length);
      set_msg.objectnames[i][objectName->length] = '\0';
   }
   set_msg.scope = scope;

   msg.adminownerset = &set_msg;

   msgbuf.size = safs_imm_om_message__get_packed_size(&msg);
   msgbuf.buf = malloc(msgbuf.size);
   safs_imm_om_message__pack(&msg, (uint8_t *) msgbuf.buf);

   for (i = 0; i < set_msg.n_objectnames; i++) {
      free(set_msg.objectnames[i]);
   }
   free(set_msg.objectnames);

   LOCK_RDLOCK(&immOmClients_lock);
   client = (SafcImmOmClientT *) safc_array_get(&immOmClients, (int) ao_ptr->immHandle);
   LOCK_UNLOCK(&immOmClients_lock);
   if (!client)
      return SA_AIS_ERR_BAD_HANDLE;

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
   ret_msg = safs_imm_om_admin_owner_set_ret__unpack(NULL,
						     msgbuf.size,
						     (uint8_t *) msgbuf.buf);
   free(msgbuf.buf);

   if (ret_msg == NULL)
   {
      TRACE1(("SA_AIS_ERR_UNAVAILABLE: Couldn't unpack incoming message\n"));
      return SA_AIS_ERR_UNAVAILABLE;
   }

   ret = ret_msg->returnval;
   safs_imm_om_admin_owner_set_ret__free_unpacked(ret_msg, NULL);

   TRACE_LEAVE();

   return ret;
}

SaAisErrorT saImmOmAdminOwnerRelease(SaImmAdminOwnerHandleT ownerHandle,
				     const SaNameT **objectNames,
				     SaImmScopeT scope)
{
   SaAisErrorT ret = SA_AIS_OK;
   SaAisErrorT recv_ret = SA_AIS_OK;
   SafcImmAdminOwnerClientT *ao_ptr = NULL;
   SafcImmOmClientT *client;
   safc_buf_t msgbuf;
   SafsImmOmAdminOwnerRelease release_msg =  SAFS_IMM_OM_ADMIN_OWNER_RELEASE__INIT;
   SafsImmOmMessage msg = SAFS_IMM_OM_MESSAGE__INIT;
   SafsImmOmAdminOwnerReleaseRet *ret_msg;
   int i;
   const SaNameT *objectName;

   TRACE_ENTER();

   if (!objectNames || (objectNames[0] == 0)) {
      return SA_AIS_ERR_INVALID_PARAM;
   }

   switch (scope) {
   case SA_IMM_ONE:
   case SA_IMM_SUBLEVEL:
   case SA_IMM_SUBTREE:
      break;
   default:
      return SA_AIS_ERR_INVALID_PARAM;
   }

   LOCK_RDLOCK(&immOmClients_lock);
   ao_ptr = (SafcImmAdminOwnerClientT *) safc_array_get(&immOmClients, (int) ownerHandle);
   LOCK_UNLOCK(&immOmClients_lock);

   if (!ao_ptr)
      return SA_AIS_ERR_BAD_HANDLE;

   /* Coding of message */
   release_msg.ownerhandle =  ao_ptr->srv_handle;

   for (i = 0; objectNames[i]; i++);
   release_msg.n_objectnames = i;

   release_msg.objectnames = calloc (1, sizeof (char*) * release_msg.n_objectnames);

   for (i = 0; i < release_msg.n_objectnames; i++) {
      objectName = objectNames[i];
      assert(objectName->length < SA_MAX_NAME_LENGTH);
      release_msg.objectnames[i] =  calloc(1, sizeof(SaUint8T) * (objectName->length + 1));      
      (void)memcpy(release_msg.objectnames[i], objectName->value, objectName->length);
      release_msg.objectnames[i][objectName->length] = '\0';
   }
   release_msg.scope = scope;

   msg.adminownerrelease = &release_msg;

   msgbuf.size = safs_imm_om_message__get_packed_size(&msg);
   msgbuf.buf = malloc(msgbuf.size);
   safs_imm_om_message__pack(&msg, (uint8_t *) msgbuf.buf);

   for (i = 0; i < release_msg.n_objectnames; i++) {
      free(release_msg.objectnames[i]);
   }
   free(release_msg.objectnames);

   LOCK_RDLOCK(&immOmClients_lock);
   client = (SafcImmOmClientT *) safc_array_get(&immOmClients, (int) ao_ptr->immHandle);
   LOCK_UNLOCK(&immOmClients_lock);
   if (!client)
      return SA_AIS_ERR_BAD_HANDLE;

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
   ret_msg = safs_imm_om_admin_owner_release_ret__unpack(NULL,
						     msgbuf.size,
						     (uint8_t *) msgbuf.buf);
   free(msgbuf.buf);

   if (ret_msg == NULL)
   {
      TRACE1(("SA_AIS_ERR_UNAVAILABLE: Couldn't unpack incoming message\n"));
      return SA_AIS_ERR_UNAVAILABLE;
   }

   ret = ret_msg->returnval;
   safs_imm_om_admin_owner_release_ret__free_unpacked(ret_msg, NULL);

   TRACE_LEAVE();
   return ret;
}

SaAisErrorT saImmOmAdminOwnerFinalize(SaImmAdminOwnerHandleT ownerHandle)
{
   SaAisErrorT ret = SA_AIS_OK;
   SaAisErrorT recv_ret = SA_AIS_OK;
   SafcImmAdminOwnerClientT *ao_ptr = NULL;
   SafcImmOmClientT *client;
   safc_buf_t msgbuf;
   SafsImmOmAdminOwnerFinalize fin_msg =  SAFS_IMM_OM_ADMIN_OWNER_FINALIZE__INIT;
   SafsImmOmMessage msg = SAFS_IMM_OM_MESSAGE__INIT;
   SafsImmOmAdminOwnerFinalizeRet *ret_msg;

   TRACE_ENTER();

   LOCK_RDLOCK(&immOmClients_lock);
   ao_ptr = (SafcImmAdminOwnerClientT *) safc_array_get(&immOmClients, (int) ownerHandle);
   LOCK_UNLOCK(&immOmClients_lock);

   if (!ao_ptr) {
      TRACE_LEAVE();     
      return SA_AIS_ERR_BAD_HANDLE;
   }

   /* Coding of message */
   fin_msg.ownerhandle =  ao_ptr->srv_handle;

   msg.adminownerfinalize = &fin_msg;

   msgbuf.size = safs_imm_om_message__get_packed_size(&msg);
   msgbuf.buf = malloc(msgbuf.size);
   safs_imm_om_message__pack(&msg, (uint8_t *) msgbuf.buf);

   LOCK_RDLOCK(&immOmClients_lock);
   client = (SafcImmOmClientT *) safc_array_get(&immOmClients, (int) ao_ptr->immHandle);
   LOCK_UNLOCK(&immOmClients_lock);

   /* Send message & wait for answer*/
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
   ret_msg = safs_imm_om_admin_owner_finalize_ret__unpack(NULL,
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
   safs_imm_om_admin_owner_finalize_ret__free_unpacked(ret_msg, NULL);

   LOCK_WRLOCK(&immOmClients_lock);
   safc_array_remove(&immOmClients, (int) ownerHandle);
   LOCK_UNLOCK(&immOmClients_lock);

   /* LATH: Remove ccb handles */

   free(ao_ptr->adminOwnerName);
   free(ao_ptr);
   TRACE_LEAVE();
   return ret;
}

SaAisErrorT saImmOmAdminOwnerClear(SaImmHandleT immHandle,
				   const SaNameT **objectNames,
				   SaImmScopeT scope)
{
   SaAisErrorT ret = SA_AIS_OK;
   SaAisErrorT recv_ret = SA_AIS_OK;
   SafcImmOmClientT *client;
   safc_buf_t msgbuf;
   SafsImmOmAdminOwnerClear clear_msg =  SAFS_IMM_OM_ADMIN_OWNER_CLEAR__INIT;
   SafsImmOmMessage msg = SAFS_IMM_OM_MESSAGE__INIT;
   SafsImmOmAdminOwnerClearRet *ret_msg;
   int i;
   const SaNameT *objectName;

   TRACE_ENTER();

   if (!objectNames || (objectNames[0] == 0)) {
      return SA_AIS_ERR_INVALID_PARAM;
   }

   switch (scope) {
   case SA_IMM_ONE:
   case SA_IMM_SUBLEVEL:
   case SA_IMM_SUBTREE:
      break;
   default:
      return SA_AIS_ERR_INVALID_PARAM;
   }

   LOCK_RDLOCK(&immOmClients_lock);
   client = (SafcImmOmClientT *) safc_array_get(&immOmClients, (int) immHandle);
   LOCK_UNLOCK(&immOmClients_lock);
   if (!client) {
      TRACE_LEAVE();
      return SA_AIS_ERR_BAD_HANDLE;
   }
   /* Coding of message */
   clear_msg.handle = client->srv_handle;

   for (i = 0; objectNames[i]; i++);
   clear_msg.n_objectnames = i;

   clear_msg.objectnames = calloc(1, sizeof (char*) * clear_msg.n_objectnames);

   for (i = 0; i < clear_msg.n_objectnames; i++) {
      objectName = objectNames[i];
      assert(objectName->length < SA_MAX_NAME_LENGTH);
      clear_msg.objectnames[i] = calloc(1, sizeof(SaUint8T) * (objectName->length + 1));      
      (void)memcpy(clear_msg.objectnames[i], objectName->value, objectName->length);
      clear_msg.objectnames[i][objectName->length] = '\0';
   }
   clear_msg.scope = scope;

   msg.adminownerclear = &clear_msg;

   msgbuf.size = safs_imm_om_message__get_packed_size(&msg);
   msgbuf.buf = malloc(msgbuf.size);
   safs_imm_om_message__pack(&msg, (uint8_t *) msgbuf.buf);

   for (i = 0; i < clear_msg.n_objectnames; i++) {
      free(clear_msg.objectnames[i]);
   }   free(clear_msg.objectnames);

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
   ret_msg = safs_imm_om_admin_owner_clear_ret__unpack(NULL,
						     msgbuf.size,
						     (uint8_t *) msgbuf.buf);
   free(msgbuf.buf);

   if (ret_msg == NULL)
   {
      fprintf(stderr, "error unpacking incoming message\n");
      exit(1);
   }

   ret = ret_msg->returnval;
   safs_imm_om_admin_owner_clear_ret__free_unpacked(ret_msg, NULL);

   TRACE_LEAVE();
   return ret;
}


/*
 * ======================================================================
 * 4.8 Configuration Changes
 * ======================================================================
 */
SaAisErrorT saImmOmCcbInitialize(SaImmAdminOwnerHandleT ownerHandle,
				 SaImmCcbFlagsT ccbFlags,
				 SaImmCcbHandleT *ccbHandle)
{
   SaAisErrorT ret = SA_AIS_OK;
   SaAisErrorT recv_ret = SA_AIS_OK;
   SafcImmAdminOwnerClientT *ao_ptr = NULL;
   SafcImmOmClientT *client=NULL;
   SafcImmCcbClientT *ccb_ptr = NULL;
   safc_buf_t msgbuf;
   SafsImmOmCcbInitialize ccb_init_msg =  SAFS_IMM_OM_CCB_INITIALIZE__INIT;
   SafsImmOmMessage msg = SAFS_IMM_OM_MESSAGE__INIT;
   SafsImmOmCcbInitializeRet *ret_msg;

   TRACE_ENTER();

   if (ccbHandle == NULL) {
      TRACE1(("ERR_INVALID_PARAM: 'ccbHandle is NULL"));
      TRACE_LEAVE();
      return SA_AIS_ERR_INVALID_PARAM;
   }
   
   *ccbHandle = 0;
   
   LOCK_RDLOCK(&immOmClients_lock);
   ao_ptr = (SafcImmAdminOwnerClientT *) safc_array_get(&immOmClients, (int) ownerHandle);
   LOCK_UNLOCK(&immOmClients_lock);
   
   if (!ao_ptr) {
      TRACE_LEAVE();      
      return SA_AIS_ERR_BAD_HANDLE;
   }
   LOCK_RDLOCK(&immOmClients_lock);
   client = (SafcImmOmClientT *) safc_array_get(&immOmClients, (int) ao_ptr->immHandle);
   LOCK_UNLOCK(&immOmClients_lock);
   
   if (!client) {
      TRACE_LEAVE();
      return SA_AIS_ERR_BAD_HANDLE;
   }

   if (ccbFlags) {
      SaImmCcbFlagsT tmpFlags = ccbFlags;
      if(tmpFlags & SA_IMM_CCB_REGISTERED_OI) {
	 TRACE1(("SA_IMM_CCB_REGISTERED_OI is set"));
	 tmpFlags &= ~SA_IMM_CCB_REGISTERED_OI;
	 if(tmpFlags & SA_IMM_CCB_ALLOW_NULL_OI) {
	    TRACE1(("SA_IMM_CCB_ALLOW_NULL_OI is set"));
	    if(!(client->isImmA2b)) {
	       TRACE1(("ERR_VERSION: SA_IMM_CCB_ALLOW_NULL_OI"
		       "requires IMM version A.02.11"));
	       return SA_AIS_ERR_VERSION;
	    }
	    tmpFlags &= ~SA_IMM_CCB_ALLOW_NULL_OI;
	 }
      }
      if(tmpFlags) {
	 TRACE1(("ERR_INVALID_PARAM: Unknown flags in ccbFlags: 0x%llx", ccbFlags));
	 return SA_AIS_ERR_INVALID_PARAM;
      }
   }

   ccb_ptr = (SafcImmCcbClientT *)calloc(1, sizeof(SafcImmCcbClientT));
   if (ccb_ptr == NULL) {
      TRACE1(("ERR_NO_MEMORY: IMM CCB client structure alloc failed\n"));
      TRACE_LEAVE();
      return SA_AIS_ERR_NO_MEMORY;
   }

   /* Coding of message */

   ccb_init_msg.ownerhandle = ao_ptr->srv_handle;
   ccb_init_msg.ccbflags = ccbFlags;

   msg.ccbinitialize = &ccb_init_msg;

   msgbuf.size = safs_imm_om_message__get_packed_size(&msg);
   msgbuf.buf = malloc(msgbuf.size);
   safs_imm_om_message__pack(&msg, (uint8_t *) msgbuf.buf);

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
   ret_msg = safs_imm_om_ccb_initialize_ret__unpack(NULL,
						    msgbuf.size,
						    (uint8_t *) msgbuf.buf);
   free(msgbuf.buf);

   if (ret_msg == NULL)
   {
      TRACE1(("SA_AIS_ERR_UNAVAILABLE: Couldn't unpack incoming message\n"));
      ret = SA_AIS_ERR_UNAVAILABLE;
      goto error;
   }

   ret = ret_msg->returnval;
   ccb_ptr->erlCcbHandle = ret_msg->handle;
   ccb_ptr->immHandle = ao_ptr->immHandle;
   ccb_ptr->aoHandle = ownerHandle;
   ccb_ptr->ccbFlags = ccbFlags;
   ccb_ptr->attributes = NULL;
   ccb_ptr->errorStrings = NULL;

   safs_imm_om_ccb_initialize_ret__free_unpacked(ret_msg, NULL);

   if(ret != SA_AIS_OK) 
      goto error;

   LOCK_WRLOCK(&immOmClients_lock);
   *ccbHandle = safc_array_insert(&immOmClients, (void*) ccb_ptr);
   LOCK_UNLOCK(&immOmClients_lock);

   TRACE_LEAVE();
   return ret;

 error:
   free(ccb_ptr);
   TRACE_LEAVE();
   return ret;  
}

SaAisErrorT saImmOmCcbObjectCreate_2(SaImmCcbHandleT ccbHandle,
				     const SaImmClassNameT className,
				     const SaNameT *parentName,
				     const SaImmAttrValuesT_2 **attrValues)
{

   SaAisErrorT ret = SA_AIS_OK;
   SaAisErrorT recv_ret = SA_AIS_OK;
   SafcImmOmClientT *client=NULL;
   SafcImmCcbClientT *ccb_ptr = NULL;
   safc_buf_t msgbuf;
   SafsImmOmCcbObjectCreate2 ccb_oc2_msg =  SAFS_IMM_OM_CCB_OBJECT_CREATE_2__INIT;
   SafsImmOmMessage msg = SAFS_IMM_OM_MESSAGE__INIT;
   SafsImmOmCcbObjectCreate2Ret *ret_msg;
   const SaImmAttrValuesT_2* attribute;
   int i;

   TRACE_ENTER();

   if (className == NULL) {
      TRACE1(("ERR_INVALID_PARAM: classname is NULL\n"));
      TRACE_LEAVE();
      return SA_AIS_ERR_INVALID_PARAM;
   }

   if (attrValues == NULL) {
      TRACE1(("ERR_INVALID_PARAM: attrValues is NULL\n"));
      TRACE_LEAVE();
      return SA_AIS_ERR_INVALID_PARAM;
   }

   LOCK_RDLOCK(&immOmClients_lock);
   ccb_ptr = (SafcImmCcbClientT *) safc_array_get(&immOmClients, (int) ccbHandle);
   LOCK_UNLOCK(&immOmClients_lock);

   if (!ccb_ptr) {
      TRACE_LEAVE();
      return SA_AIS_ERR_BAD_HANDLE;
   }

   LOCK_RDLOCK(&immOmClients_lock);
   client = (SafcImmOmClientT *) safc_array_get(&immOmClients, (int) ccb_ptr->immHandle);
   LOCK_UNLOCK(&immOmClients_lock);

   if (!client) {
      TRACE_LEAVE();
      return SA_AIS_ERR_BAD_HANDLE;
   }

   /* Free memory from previous saImmOmCcbGetErrorStrings */
   if (ccb_ptr->errorStrings) {
      safc_free_error_strings(ccb_ptr->errorStrings);
      ccb_ptr->errorStrings = NULL;
   }

   /* Coding of message */
   ccb_oc2_msg.handle = ccb_ptr->erlCcbHandle;
   ccb_oc2_msg.classname = className;
   if(parentName != NULL) {
      assert(parentName->length < SA_MAX_NAME_LENGTH);
      ccb_oc2_msg.parentname = calloc(1, sizeof(SaUint8T) * (parentName->length + 1));
      (void)memcpy(ccb_oc2_msg.parentname,  parentName->value, parentName->length);
      ccb_oc2_msg.parentname[parentName->length] = '\0';
   }

   /* Calculate number of attributes */
   for (i = 0; attrValues[i]; i++);

   ccb_oc2_msg.n_attrvalues = i;
   ccb_oc2_msg.attrvalues =
      calloc (1, sizeof (SafsImmAttrValues2*) * ccb_oc2_msg.n_attrvalues);

   for (i = 0; i < ccb_oc2_msg.n_attrvalues; i++) {
      attribute = attrValues[i];
      safc_copy_from_imm_attributes(attribute, &(ccb_oc2_msg.attrvalues[i]));
   }

   msg.ccbobjectcreate_2 = &ccb_oc2_msg;

   msgbuf.size = safs_imm_om_message__get_packed_size(&msg);
   msgbuf.buf = malloc(msgbuf.size);
   safs_imm_om_message__pack(&msg, (uint8_t *) msgbuf.buf);

   safc_free_ccb_oc2_msg(ccb_oc2_msg);

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
   ret_msg = safs_imm_om_ccb_object_create2_ret__unpack(NULL,
							msgbuf.size,
							(uint8_t *) msgbuf.buf);
   free(msgbuf.buf);

   if (ret_msg == NULL)
   {
      TRACE1(("SA_AIS_ERR_UNAVAILABLE: Couldn't unpack incoming message\n"));
      return SA_AIS_ERR_UNAVAILABLE;
   }

   ret = ret_msg->returnval;

   safs_imm_om_ccb_object_create2_ret__free_unpacked(ret_msg, NULL);


   TRACE_LEAVE();

   return ret;
}


SaAisErrorT saImmOmCcbObjectDelete(SaImmCcbHandleT ccbHandle,
				   const SaNameT *objectName)
{
   SaAisErrorT ret = SA_AIS_OK;
   SaAisErrorT recv_ret = SA_AIS_OK;
   SafcImmOmClientT *client=NULL;
   SafcImmCcbClientT *ccb_ptr = NULL;
   safc_buf_t msgbuf;
   SafsImmOmCcbObjectDelete ccb_od_msg =  SAFS_IMM_OM_CCB_OBJECT_DELETE__INIT;
   SafsImmOmMessage msg = SAFS_IMM_OM_MESSAGE__INIT;
   SafsImmOmCcbObjectDeleteRet *ret_msg;

   TRACE_ENTER();

   if (objectName == NULL || (objectName->length == 0) || (objectName->length >= SA_MAX_NAME_LENGTH)) {
      TRACE1(("ERR_INVALID_PARAM: objectName"));
      return SA_AIS_ERR_INVALID_PARAM;
   }

   LOCK_RDLOCK(&immOmClients_lock);
   ccb_ptr = (SafcImmCcbClientT *) safc_array_get(&immOmClients, (int) ccbHandle);
   LOCK_UNLOCK(&immOmClients_lock);

   if (!ccb_ptr)
      return SA_AIS_ERR_BAD_HANDLE;

   LOCK_RDLOCK(&immOmClients_lock);
   client = (SafcImmOmClientT *) safc_array_get(&immOmClients, (int) ccb_ptr->immHandle);
   LOCK_UNLOCK(&immOmClients_lock);

   if (!client)
      return SA_AIS_ERR_BAD_HANDLE;

   /* Free memory from previous saImmOmCcbGetErrorStrings */
   if (ccb_ptr->errorStrings) {
      safc_free_error_strings(ccb_ptr->errorStrings);
      ccb_ptr->errorStrings = NULL;
   }

   /* Coding of message */
   ccb_od_msg.handle = ccb_ptr->erlCcbHandle;
   assert(objectName->length < SA_MAX_NAME_LENGTH);
   ccb_od_msg.objectname = calloc(1, sizeof(SaUint8T) * (objectName->length + 1));      
   (void)memcpy(ccb_od_msg.objectname, objectName->value, objectName->length);
   ccb_od_msg.objectname[objectName->length] = '\0';

   msg.ccbobjectdelete = &ccb_od_msg;

   msgbuf.size = safs_imm_om_message__get_packed_size(&msg);
   msgbuf.buf = malloc(msgbuf.size);
   safs_imm_om_message__pack(&msg, (uint8_t *) msgbuf.buf);

   free(ccb_od_msg.objectname);

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
   ret_msg = safs_imm_om_ccb_object_delete_ret__unpack(NULL,
						       msgbuf.size,
						       (uint8_t *) msgbuf.buf);
   free(msgbuf.buf);

   if (ret_msg == NULL)
   {
      TRACE1(("SA_AIS_ERR_UNAVAILABLE: Couldn't unpack incoming message\n"));
      return SA_AIS_ERR_UNAVAILABLE;
   }

   ret = ret_msg->returnval;

   safs_imm_om_ccb_object_delete_ret__free_unpacked(ret_msg, NULL);

   TRACE_LEAVE();
   return ret;
}


SaAisErrorT saImmOmCcbObjectModify_2(SaImmCcbHandleT ccbHandle,
				     const SaNameT *objectName,
				     const SaImmAttrModificationT_2 **attrMods)
{
   SaAisErrorT ret = SA_AIS_OK;
   SaAisErrorT recv_ret = SA_AIS_OK;
   SafcImmOmClientT *client=NULL;
   SafcImmCcbClientT *ccb_ptr = NULL;
   safc_buf_t msgbuf;
   SafsImmOmCcbObjectModify2 ccb_om2_msg =  SAFS_IMM_OM_CCB_OBJECT_MODIFY_2__INIT;
   SafsImmOmMessage msg = SAFS_IMM_OM_MESSAGE__INIT;
   SafsImmOmCcbObjectModify2Ret *ret_msg;
   int i; 

   TRACE_ENTER();

   if (objectName == NULL || (objectName->length == 0) || (objectName->length >= SA_MAX_NAME_LENGTH)) {
      TRACE1(("ERR_INVALID_PARAM: objectName"));
      return SA_AIS_ERR_INVALID_PARAM;
   }

   if (attrMods == NULL) {
      TRACE1(("ERR_INVALID_PARAM: attrMods is NULL"));
      return SA_AIS_ERR_INVALID_PARAM;
   }

   LOCK_RDLOCK(&immOmClients_lock);
   ccb_ptr = (SafcImmCcbClientT *) safc_array_get(&immOmClients, (int) ccbHandle);
   LOCK_UNLOCK(&immOmClients_lock);

   if (!ccb_ptr)
      return SA_AIS_ERR_BAD_HANDLE;

   LOCK_RDLOCK(&immOmClients_lock);
   client = (SafcImmOmClientT *) safc_array_get(&immOmClients, (int) ccb_ptr->immHandle);
   LOCK_UNLOCK(&immOmClients_lock);

   if (!client)
      return SA_AIS_ERR_BAD_HANDLE;

   /* Free memory from previous saImmOmCcbGetErrorStrings */
   if (ccb_ptr->errorStrings) {
      safc_free_error_strings(ccb_ptr->errorStrings);
      ccb_ptr->errorStrings = NULL;
   }

   /* Coding of message */
   ccb_om2_msg.handle = ccb_ptr->erlCcbHandle;
   assert(objectName->length < SA_MAX_NAME_LENGTH);
   ccb_om2_msg.objectname = calloc(1, sizeof(SaUint8T) * (objectName->length + 1));      
   (void)memcpy(ccb_om2_msg.objectname,  objectName->value, objectName->length);
   ccb_om2_msg.objectname[objectName->length] = '\0';

   /* Calculate number of attribute modifiers*/
   for (i = 0; attrMods[i]; i++);

   ccb_om2_msg.n_attrmods = i;
   ccb_om2_msg.attrmods =
      calloc (1, sizeof (SafsImmAttrModification2*) * ccb_om2_msg.n_attrmods);

   for (i = 0; i < ccb_om2_msg.n_attrmods; i++) {
      ccb_om2_msg.attrmods[i] = calloc (1, sizeof (SafsImmAttrModification2));
      safs_imm_attr_modification_2__init(ccb_om2_msg.attrmods[i]);

      ccb_om2_msg.attrmods[i]->modtype = attrMods[i]->modType;

      safc_copy_from_imm_attributes(&(attrMods[i]->modAttr), &(ccb_om2_msg.attrmods[i]->modattr));
   }

   msg.ccbobjectmodify_2 = &ccb_om2_msg;

   msgbuf.size = safs_imm_om_message__get_packed_size(&msg);
   msgbuf.buf = malloc(msgbuf.size);
   safs_imm_om_message__pack(&msg, (uint8_t *) msgbuf.buf);

   safc_free_ccb_om2_msg(ccb_om2_msg);

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
   ret_msg = safs_imm_om_ccb_object_modify2_ret__unpack(NULL,
   							msgbuf.size,
   							(uint8_t *) msgbuf.buf);
   free(msgbuf.buf);

   if (ret_msg == NULL)
   {
      TRACE1(("SA_AIS_ERR_UNAVAILABLE: Couldn't unpack incoming message\n"));
      return SA_AIS_ERR_UNAVAILABLE;
   }

   ret = ret_msg->returnval;

   safs_imm_om_ccb_object_modify2_ret__free_unpacked(ret_msg, NULL);

   TRACE_LEAVE();
   return ret;
}

SaAisErrorT saImmOmCcbApply(SaImmCcbHandleT ccbHandle)
{
   SaAisErrorT ret = SA_AIS_OK;
   SaAisErrorT recv_ret = SA_AIS_OK;
   SafcImmOmClientT *client=NULL;
   SafcImmCcbClientT *ccb_ptr = NULL;
   safc_buf_t msgbuf;
   SafsImmOmCcbApply ccb_apply_msg =  SAFS_IMM_OM_CCB_APPLY__INIT;
   SafsImmOmMessage msg = SAFS_IMM_OM_MESSAGE__INIT;
   SafsImmOmCcbApplyRet *ret_msg;


   TRACE_ENTER();

   LOCK_RDLOCK(&immOmClients_lock);
   ccb_ptr = (SafcImmCcbClientT *) safc_array_get(&immOmClients, (int) ccbHandle);
   LOCK_UNLOCK(&immOmClients_lock);

   if (!ccb_ptr)
      return SA_AIS_ERR_BAD_HANDLE;

   LOCK_RDLOCK(&immOmClients_lock);
   client = (SafcImmOmClientT *) safc_array_get(&immOmClients, (int) ccb_ptr->immHandle);
   LOCK_UNLOCK(&immOmClients_lock);

   if (!client)
      return SA_AIS_ERR_BAD_HANDLE;

   /* Free memory from previous saImmOmCcbGetErrorStrings */
   if (ccb_ptr->errorStrings) {
      safc_free_error_strings(ccb_ptr->errorStrings);
      ccb_ptr->errorStrings = NULL;
   }

   /* Coding of message */
   ccb_apply_msg.handle = ccb_ptr->erlCcbHandle;

   msg.ccbapply = &ccb_apply_msg;

   msgbuf.size = safs_imm_om_message__get_packed_size(&msg);
   msgbuf.buf = malloc(msgbuf.size);
   safs_imm_om_message__pack(&msg, (uint8_t *) msgbuf.buf);

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
   ret_msg = safs_imm_om_ccb_apply_ret__unpack(NULL,
					       msgbuf.size,
					       (uint8_t *) msgbuf.buf);
   free(msgbuf.buf);

   if (ret_msg == NULL)
   {
      TRACE1(("SA_AIS_ERR_UNAVAILABLE: Couldn't unpack incoming message\n"));
      return SA_AIS_ERR_UNAVAILABLE;
   }

   ret = ret_msg->returnval;

   safs_imm_om_ccb_apply_ret__free_unpacked(ret_msg, NULL);

   TRACE_LEAVE();
   return ret;
}


SaAisErrorT saImmOmCcbValidate(SaImmCcbHandleT ccbHandle)
{
   SaAisErrorT ret = SA_AIS_OK;
   SaAisErrorT recv_ret = SA_AIS_OK;
   SafcImmOmClientT *client=NULL;
   SafcImmCcbClientT *ccb_ptr = NULL;
   safc_buf_t msgbuf;
   SafsImmOmCcbValidate ccb_validate_msg =  SAFS_IMM_OM_CCB_VALIDATE__INIT;
   SafsImmOmMessage msg = SAFS_IMM_OM_MESSAGE__INIT;
   SafsImmOmCcbValidateRet *ret_msg;


   TRACE_ENTER();

   LOCK_RDLOCK(&immOmClients_lock);
   ccb_ptr = (SafcImmCcbClientT *) safc_array_get(&immOmClients, (int) ccbHandle);
   LOCK_UNLOCK(&immOmClients_lock);

   if (!ccb_ptr)
      return SA_AIS_ERR_BAD_HANDLE;

   LOCK_RDLOCK(&immOmClients_lock);
   client = (SafcImmOmClientT *) safc_array_get(&immOmClients, (int) ccb_ptr->immHandle);
   LOCK_UNLOCK(&immOmClients_lock);

   if (!client)
      return SA_AIS_ERR_BAD_HANDLE;

   /* Free memory from previous saImmOmCcbGetErrorStrings */
   if (ccb_ptr->errorStrings) {
      safc_free_error_strings(ccb_ptr->errorStrings);
      ccb_ptr->errorStrings = NULL;
   }

   /* Coding of message */
   ccb_validate_msg.handle = ccb_ptr->erlCcbHandle;

   msg.ccbvalidate = &ccb_validate_msg;

   msgbuf.size = safs_imm_om_message__get_packed_size(&msg);
   msgbuf.buf = malloc(msgbuf.size);
   safs_imm_om_message__pack(&msg, (uint8_t *) msgbuf.buf);

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
   ret_msg = safs_imm_om_ccb_validate_ret__unpack(NULL,
						  msgbuf.size,
						  (uint8_t *) msgbuf.buf);
   free(msgbuf.buf);

   if (ret_msg == NULL)
   {
      TRACE1(("SA_AIS_ERR_UNAVAILABLE: Couldn't unpack incoming message\n"));
      return SA_AIS_ERR_UNAVAILABLE;
   }

   ret = ret_msg->returnval;

   safs_imm_om_ccb_validate_ret__free_unpacked(ret_msg, NULL);

   TRACE_LEAVE();
   return ret;
}


SaAisErrorT saImmOmCcbAbort(SaImmCcbHandleT ccbHandle)
{
   SaAisErrorT ret = SA_AIS_OK;
   SaAisErrorT recv_ret = SA_AIS_OK;
   SafcImmOmClientT *client=NULL;
   SafcImmCcbClientT *ccb_ptr = NULL;
   safc_buf_t msgbuf;
   SafsImmOmCcbAbort ccb_abort_msg =  SAFS_IMM_OM_CCB_ABORT__INIT;
   SafsImmOmMessage msg = SAFS_IMM_OM_MESSAGE__INIT;
   SafsImmOmCcbAbortRet *ret_msg;


   TRACE_ENTER();

   LOCK_RDLOCK(&immOmClients_lock);
   ccb_ptr = (SafcImmCcbClientT *) safc_array_get(&immOmClients, (int) ccbHandle);
   LOCK_UNLOCK(&immOmClients_lock);

   if (!ccb_ptr)
      return SA_AIS_ERR_BAD_HANDLE;

   LOCK_RDLOCK(&immOmClients_lock);
   client = (SafcImmOmClientT *) safc_array_get(&immOmClients, (int) ccb_ptr->immHandle);
   LOCK_UNLOCK(&immOmClients_lock);

   if (!client)
      return SA_AIS_ERR_BAD_HANDLE;

   /* Free memory from previous saImmOmCcbGetErrorStrings */
   if (ccb_ptr->errorStrings) {
      safc_free_error_strings(ccb_ptr->errorStrings);
      ccb_ptr->errorStrings = NULL;
   }

   /* Coding of message */
   ccb_abort_msg.handle = ccb_ptr->erlCcbHandle;

   msg.ccbabort = &ccb_abort_msg;

   msgbuf.size = safs_imm_om_message__get_packed_size(&msg);
   msgbuf.buf = malloc(msgbuf.size);
   safs_imm_om_message__pack(&msg, (uint8_t *) msgbuf.buf);

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
   ret_msg = safs_imm_om_ccb_abort_ret__unpack(NULL,
					       msgbuf.size,
					       (uint8_t *) msgbuf.buf);
   free(msgbuf.buf);

   if (ret_msg == NULL)
   {
      TRACE1(("SA_AIS_ERR_UNAVAILABLE: Couldn't unpack incoming message\n"));
      return SA_AIS_ERR_UNAVAILABLE;
   }

   ret = ret_msg->returnval;

   safs_imm_om_ccb_abort_ret__free_unpacked(ret_msg, NULL);

   TRACE_LEAVE();
   return ret;
}

SaAisErrorT saImmOmCcbFinalize(SaImmCcbHandleT ccbHandle)
{
   SaAisErrorT ret = SA_AIS_OK;
   SaAisErrorT recv_ret = SA_AIS_OK;
   SafcImmOmClientT *client=NULL;
   SafcImmCcbClientT *ccb_ptr = NULL;
   safc_buf_t msgbuf;
   SafsImmOmCcbFinalize ccb_finalize_msg =  SAFS_IMM_OM_CCB_FINALIZE__INIT;
   SafsImmOmMessage msg = SAFS_IMM_OM_MESSAGE__INIT;
   SafsImmOmCcbFinalizeRet *ret_msg;

   TRACE_ENTER();

   LOCK_RDLOCK(&immOmClients_lock);
   ccb_ptr = (SafcImmCcbClientT *) safc_array_get(&immOmClients, (int) ccbHandle);
   LOCK_UNLOCK(&immOmClients_lock);

   if (!ccb_ptr) {
      TRACE_LEAVE();      
      return SA_AIS_ERR_BAD_HANDLE;
   }

   /* Free memory from previous object read */
   if (ccb_ptr->attributes) {
       safc_free_imm_attributes(ccb_ptr->attributes);
       ccb_ptr->attributes = NULL;
   }

   LOCK_RDLOCK(&immOmClients_lock);
   client = (SafcImmOmClientT *) safc_array_get(&immOmClients, (int) ccb_ptr->immHandle);
   LOCK_UNLOCK(&immOmClients_lock);

   if (!client) {
      TRACE_LEAVE();     
      return SA_AIS_ERR_BAD_HANDLE;
   }

   /* Free memory from previous saImmOmCcbGetErrorStrings */
   if (ccb_ptr->errorStrings) {
      safc_free_error_strings(ccb_ptr->errorStrings);
      ccb_ptr->errorStrings = NULL;
   }

   /* Coding of message */
   ccb_finalize_msg.handle = ccb_ptr->erlCcbHandle;

   msg.ccbfinalize = &ccb_finalize_msg;

   msgbuf.size = safs_imm_om_message__get_packed_size(&msg);
   msgbuf.buf = malloc(msgbuf.size);
   safs_imm_om_message__pack(&msg, (uint8_t *) msgbuf.buf);

   /* Send message & wait for answer*/

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
   ret_msg = safs_imm_om_ccb_finalize_ret__unpack(NULL,
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

   safs_imm_om_ccb_finalize_ret__free_unpacked(ret_msg, NULL);

   LOCK_WRLOCK(&immOmClients_lock);
   safc_array_remove(&immOmClients, (int) ccbHandle);
   LOCK_UNLOCK(&immOmClients_lock);

   free(ccb_ptr);

   TRACE_LEAVE();
   return ret;
}

/*
 * ======================================================================
 * 4.9 Administrative Operations Invocation
 * ======================================================================
 */
SaAisErrorT saImmOmAdminOperationInvoke_2(SaImmAdminOwnerHandleT ownerHandle,
					  const SaNameT *objectName,
					  SaImmContinuationIdT continuationId,
					  SaImmAdminOperationIdT operationId,
					  const SaImmAdminOperationParamsT_2 **params,
					  SaAisErrorT *operationReturnValue, SaTimeT timeout)
{
   SaAisErrorT ret = SA_AIS_OK;
   SaAisErrorT recv_ret = SA_AIS_OK;
   SafcImmOmClientT *client=NULL;
   SafcImmAdminOwnerClientT *ao_ptr = NULL;
   safc_buf_t msgbuf;
   SafsImmOmAdminOperationInvoke2 adm_op_msg =  SAFS_IMM_OM_ADMIN_OPERATION_INVOKE_2__INIT;
   SafsImmOmMessage msg = SAFS_IMM_OM_MESSAGE__INIT;
   SafsImmOmAdminOperationInvoke2Ret *ret_msg;
   int i;

   TRACE_ENTER();

   if ((objectName == NULL) || (objectName->length == 0) || (objectName->length >= SA_MAX_NAME_LENGTH) ||
       (operationReturnValue == NULL) || (params == NULL)) {
      TRACE1(("ERR_INVALID_PARAM: objectName, operationReturnValue or params"));
      return SA_AIS_ERR_INVALID_PARAM;
   }

   LOCK_RDLOCK(&immOmClients_lock);
   ao_ptr = (SafcImmAdminOwnerClientT *) safc_array_get(&immOmClients, (int) ownerHandle);
   LOCK_UNLOCK(&immOmClients_lock);

   if (!ao_ptr) {
      TRACE1(("ERR_BAD_HANDLE: No initialized admin owner handle exists!"));
      return SA_AIS_ERR_BAD_HANDLE;
   }

   LOCK_RDLOCK(&immOmClients_lock);
   client = (SafcImmOmClientT *) safc_array_get(&immOmClients, (int) ao_ptr->immHandle);
   LOCK_UNLOCK(&immOmClients_lock);

   if (!client) {
      TRACE1(("ERR_BAD_HANDLE: No initialized imm om handle exists for the search handle!"));
      return SA_AIS_ERR_BAD_HANDLE;
   }

   /*Overwrite any old/uninitialized value. */
   *operationReturnValue = SA_AIS_ERR_NO_SECTIONS;	/* Set to bad value to prevent user mistakes. */

   /* Coding of message */
   adm_op_msg.ownerhandle = ao_ptr->srv_handle;
   adm_op_msg.objectname = calloc(1, sizeof(SaUint8T) * (objectName->length + 1));      
   (void)memcpy(adm_op_msg.objectname, objectName->value, objectName->length);
   adm_op_msg.objectname[objectName->length] = '\0';
   adm_op_msg.continuationid = continuationId;
   adm_op_msg.operationid = operationId;

   /* Calculate number of parameters*/
   for (i = 0; params[i]; i++);
   adm_op_msg.n_params = i;

   adm_op_msg.params =
      calloc (1, sizeof (SafsImmAdminOperationParams2*) * adm_op_msg.n_params);

   for (i = 0; i < adm_op_msg.n_params; i++) {
      adm_op_msg.params[i] = calloc (1, sizeof (SafsImmAdminOperationParams2));
      safs_imm_admin_operation_params_2__init(adm_op_msg.params[i]);
      adm_op_msg.params[i]->paramname = params[i]->paramName;
      adm_op_msg.params[i]->paramtype = params[i]->paramType;
      if(params[i]->paramBuffer) {
	 adm_op_msg.params[i]->parambuffer = calloc (1, sizeof (SafsImmAttrValue));
	 safs_imm_attr_value__init(adm_op_msg.params[i]->parambuffer);
	 safc_copy_from_imm_attr_value(adm_op_msg.params[i]->parambuffer,
				       params[i]->paramType,
				       params[i]->paramBuffer);
      } else
	 adm_op_msg.params[i]->parambuffer = NULL;
   }

   adm_op_msg.timeout = timeout;

   msg.adminoperationinvoke_2 = &adm_op_msg;

   msgbuf.size = safs_imm_om_message__get_packed_size(&msg);
   msgbuf.buf = malloc(msgbuf.size);
   safs_imm_om_message__pack(&msg, (uint8_t *) msgbuf.buf);

   free(adm_op_msg.objectname);
   safc_free_admop_params(adm_op_msg.n_params, adm_op_msg.params);

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
   ret_msg = safs_imm_om_admin_operation_invoke2_ret__unpack(NULL,
							     msgbuf.size,
							     (uint8_t *) msgbuf.buf);
   free(msgbuf.buf);

   if (ret_msg == NULL)
   {
      TRACE1(("SA_AIS_ERR_UNAVAILABLE: Couldn't unpack incoming message\n"));
      return SA_AIS_ERR_UNAVAILABLE;
   }

   ret = ret_msg->returnval;
   *operationReturnValue = ret_msg->operationreturnvalue;

   safs_imm_om_admin_operation_invoke2_ret__free_unpacked(ret_msg, NULL);

   TRACE_LEAVE();

   return ret;
}


SaAisErrorT saImmOmAdminOperationInvoke_o2(SaImmAdminOwnerHandleT ownerHandle,
					  const SaNameT *objectName,
					  SaImmContinuationIdT continuationId,
					  SaImmAdminOperationIdT operationId,
					  const SaImmAdminOperationParamsT_2 **params,
					  SaAisErrorT *operationReturnValue, SaTimeT timeout,
					  SaImmAdminOperationParamsT_2 ***returnParams)
{
   SaAisErrorT ret = SA_AIS_OK;
   SaAisErrorT recv_ret = SA_AIS_OK;
   SafcImmOmClientT *client=NULL;
   SafcImmAdminOwnerClientT *ao_ptr = NULL;
   safc_buf_t msgbuf;
   SafsImmOmAdminOperationInvokeO2 adm_op_msg =  SAFS_IMM_OM_ADMIN_OPERATION_INVOKE_O2__INIT;
   SafsImmOmMessage msg = SAFS_IMM_OM_MESSAGE__INIT;
   SafsImmOmAdminOperationInvokeO2Ret *ret_msg;
   int i;
   SafcImmOmMemoryStructT *memory;

   TRACE_ENTER();

   if ((objectName == NULL) || (objectName->length == 0) || (objectName->length >= SA_MAX_NAME_LENGTH) ||
       (operationReturnValue == NULL) || (params == NULL)) {
      TRACE1(("ERR_INVALID_PARAM: objectName, operationReturnValue or params"));
      return SA_AIS_ERR_INVALID_PARAM;
   }

   LOCK_RDLOCK(&immOmClients_lock);
   ao_ptr = (SafcImmAdminOwnerClientT *) safc_array_get(&immOmClients, (int) ownerHandle);
   LOCK_UNLOCK(&immOmClients_lock);

   if (!ao_ptr) {
      TRACE1(("ERR_BAD_HANDLE: No initialized admin owner handle exists!"));
      return SA_AIS_ERR_BAD_HANDLE;
   }

   LOCK_RDLOCK(&immOmClients_lock);
   client = (SafcImmOmClientT *) safc_array_get(&immOmClients, (int) ao_ptr->immHandle);
   LOCK_UNLOCK(&immOmClients_lock);

   if (!client) {
      TRACE1(("ERR_BAD_HANDLE: No initialized imm om handle exists for the search handle!"));
      return SA_AIS_ERR_BAD_HANDLE;
   }

   /*Overwrite any old/uninitialized value. */
   *operationReturnValue = SA_AIS_ERR_NO_SECTIONS;	/* Set to bad value to prevent user mistakes. */

   /* Coding of message */
   adm_op_msg.ownerhandle = ao_ptr->srv_handle;
   adm_op_msg.objectname = calloc(1, sizeof(SaUint8T) * (objectName->length + 1));      
   (void)memcpy(adm_op_msg.objectname, objectName->value, objectName->length);
   adm_op_msg.objectname[objectName->length] = '\0';
   adm_op_msg.continuationid = continuationId;
   adm_op_msg.operationid = operationId;

   /* Calculate number of parameters*/
   for (i = 0; params[i]; i++);
   adm_op_msg.n_params = i;

   adm_op_msg.params =
      calloc (1, sizeof (SafsImmAdminOperationParams2*) * adm_op_msg.n_params);

   for (i = 0; i < adm_op_msg.n_params; i++) {
      adm_op_msg.params[i] = calloc (1, sizeof (SafsImmAdminOperationParams2));
      safs_imm_admin_operation_params_2__init(adm_op_msg.params[i]);
      adm_op_msg.params[i]->paramname = params[i]->paramName;
      adm_op_msg.params[i]->paramtype = params[i]->paramType;
      if(params[i]->paramBuffer) {
	 adm_op_msg.params[i]->parambuffer = calloc (1, sizeof (SafsImmAttrValue));
	 safs_imm_attr_value__init(adm_op_msg.params[i]->parambuffer);
	 safc_copy_from_imm_attr_value(adm_op_msg.params[i]->parambuffer,
				       params[i]->paramType,
				       params[i]->paramBuffer);
      } else
	 adm_op_msg.params[i]->parambuffer = NULL;
   }

   adm_op_msg.timeout = timeout;

   msg.adminoperationinvoke_o2 = &adm_op_msg;

   msgbuf.size = safs_imm_om_message__get_packed_size(&msg);
   msgbuf.buf = malloc(msgbuf.size);
   safs_imm_om_message__pack(&msg, (uint8_t *) msgbuf.buf);

   free(adm_op_msg.objectname);
   safc_free_admop_params(adm_op_msg.n_params, adm_op_msg.params);

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
   ret_msg = safs_imm_om_admin_operation_invoke_o2_ret__unpack(NULL,
							     msgbuf.size,
							     (uint8_t *) msgbuf.buf);
   free(msgbuf.buf);

   if (ret_msg == NULL)
   {
      TRACE1(("SA_AIS_ERR_UNAVAILABLE: Couldn't unpack incoming message\n"));
      return SA_AIS_ERR_UNAVAILABLE;
   }

   ret = ret_msg->returnval;

   if(ret == SA_AIS_OK) {

      *operationReturnValue = ret_msg->operationreturnvalue;

      *returnParams = calloc (1, sizeof (SaImmAdminOperationParamsT_2*) * (ret_msg->n_returnparams+1));

      for (i = 0; i < ret_msg->n_returnparams; i++) {
	 (*returnParams)[i] = calloc (1, sizeof (SaImmAdminOperationParamsT_2));
	 (*returnParams)[i]->paramName = strdup(ret_msg->returnparams[i]->paramname);
	 (*returnParams)[i]->paramType = ret_msg->returnparams[i]->paramtype;
	 (*returnParams)[i]->paramBuffer = 
	    safc_copy_to_imm_attr_value(ret_msg->returnparams[i]->paramtype,
					ret_msg->returnparams[i]->parambuffer);
      }
      (*returnParams)[i] = NULL;

      memory = (SafcImmOmMemoryStructT *) calloc(1, sizeof(SafcImmOmMemoryStructT));
      memory->type = admin_op_params;
      memory->ptr = (void*) *returnParams;
      (void) safc_array_insert(&(client->allocated_memory), (void*) memory);

   }

   safs_imm_om_admin_operation_invoke_o2_ret__free_unpacked(ret_msg, NULL);

   TRACE_LEAVE();

   return ret;
}


SaAisErrorT saImmOmAdminOperationInvokeAsync_2(SaImmAdminOwnerHandleT ownerHandle,
					       SaInvocationT invocation,
					       const SaNameT *objectName,
					       SaImmContinuationIdT continuationId,
					       SaImmAdminOperationIdT operationId,
					       const SaImmAdminOperationParamsT_2 **params)
{

   SaAisErrorT ret = SA_AIS_OK;
   SaAisErrorT recv_ret = SA_AIS_OK;
   SafcImmOmClientT *client=NULL;
   SafcImmAdminOwnerClientT *ao_ptr = NULL;
   safc_buf_t msgbuf;
   SafsImmOmAdminOperationInvokeAsync2 adm_op_msg =  SAFS_IMM_OM_ADMIN_OPERATION_INVOKE_ASYNC_2__INIT;
   SafsImmOmMessage msg = SAFS_IMM_OM_MESSAGE__INIT;
   SafsImmOmAdminOperationInvokeAsync2Ret *ret_msg;
   int i;

   TRACE_ENTER();

   if ((objectName == NULL) || (objectName->length == 0) || (objectName->length >= SA_MAX_NAME_LENGTH) ||
       (params == NULL)) {
      TRACE1(("ERR_INVALID_PARAM: objectName, operationReturnValue or params"));
      return SA_AIS_ERR_INVALID_PARAM;
   }

   LOCK_RDLOCK(&immOmClients_lock);
   ao_ptr = (SafcImmAdminOwnerClientT *) safc_array_get(&immOmClients, (int) ownerHandle);
   LOCK_UNLOCK(&immOmClients_lock);

   if (!ao_ptr)
      return SA_AIS_ERR_BAD_HANDLE;

   LOCK_RDLOCK(&immOmClients_lock);
   client = (SafcImmOmClientT *) safc_array_get(&immOmClients, (int) ao_ptr->immHandle);
   LOCK_UNLOCK(&immOmClients_lock);

   if (!client)
      return SA_AIS_ERR_BAD_HANDLE;

   adm_op_msg.ownerhandle = ao_ptr->srv_handle;
   adm_op_msg.invocation = invocation;
   adm_op_msg.objectname = calloc(1, sizeof(SaUint8T) * (objectName->length + 1));      
   (void)memcpy(adm_op_msg.objectname, objectName->value, objectName->length);
   adm_op_msg.objectname[objectName->length] = '\0';
   adm_op_msg.continuationid = continuationId;
   adm_op_msg.operationid = operationId;

   for (i = 0; params[i]; i++);
   adm_op_msg.n_params = i;

   adm_op_msg.params =
      calloc (1, sizeof (SafsImmAdminOperationParams2*) * adm_op_msg.n_params);

   for (i = 0; i < adm_op_msg.n_params; i++) {
      adm_op_msg.params[i] = calloc (1, sizeof (SafsImmAdminOperationParams2));
      safs_imm_admin_operation_params_2__init(adm_op_msg.params[i]);
      adm_op_msg.params[i]->paramname = params[i]->paramName;
      adm_op_msg.params[i]->paramtype = params[i]->paramType;
      if(params[i]->paramBuffer) {
	 adm_op_msg.params[i]->parambuffer = calloc (1, sizeof (SafsImmAttrValue));
	 safs_imm_attr_value__init(adm_op_msg.params[i]->parambuffer);
	 safc_copy_from_imm_attr_value(adm_op_msg.params[i]->parambuffer,
				       params[i]->paramType,
				       params[i]->paramBuffer);
      } else 
	 adm_op_msg.params[i]->parambuffer = NULL;
   }

   free(adm_op_msg.objectname);
   msg.adminoperationinvokeasync_2 = &adm_op_msg;

   msgbuf.size = safs_imm_om_message__get_packed_size(&msg);
   msgbuf.buf = malloc(msgbuf.size);
   safs_imm_om_message__pack(&msg, (uint8_t *) msgbuf.buf);

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
   ret_msg = safs_imm_om_admin_operation_invoke_async2_ret__unpack(NULL,
								   msgbuf.size,
								   (uint8_t *) msgbuf.buf);
   free(msgbuf.buf);

   if (ret_msg == NULL)
   {
      TRACE1(("SA_AIS_ERR_UNAVAILABLE: Couldn't unpack incoming message\n"));
      return SA_AIS_ERR_UNAVAILABLE;
   }

   ret = ret_msg->returnval;

   safs_imm_om_admin_operation_invoke_async2_ret__free_unpacked(ret_msg, NULL);

   TRACE_LEAVE();

   return ret;
}

SaAisErrorT saImmOmAdminOperationContinue(SaImmAdminOwnerHandleT ownerHandle,
					  const SaNameT *objectName,
					  SaImmContinuationIdT continuationId,
					  SaAisErrorT *operationReturnValue)
{
   SaAisErrorT ret = SA_AIS_ERR_BAD_OPERATION;

   TRACE_ENTER();

   *operationReturnValue = SA_AIS_ERR_BAD_OPERATION;
   TRACE_LEAVE();

   return ret;
}

SaAisErrorT saImmOmAdminOperationContinueAsync(SaImmAdminOwnerHandleT ownerHandle,
					       SaInvocationT invocation,
					       const SaNameT *objectName,
					       SaImmContinuationIdT continuationId)
{

   SaAisErrorT ret = SA_AIS_ERR_BAD_OPERATION;
   TRACE_ENTER();

   TRACE_LEAVE();
   return ret;
}

SaAisErrorT saImmOmAdminOperationContinuationClear(SaImmAdminOwnerHandleT ownerHandle,
						   const SaNameT *objectName,
						   SaImmContinuationIdT continuationId)
{
   SaAisErrorT ret = SA_AIS_ERR_BAD_OPERATION;

   TRACE_ENTER();
   TRACE_LEAVE();
   return ret;
}

SaAisErrorT saImmOmAdminOperationMemoryFree(SaImmAdminOwnerHandleT ownerHandle,
					    SaImmAdminOperationParamsT_2 **returnParams)
{
   SaAisErrorT ret = SA_AIS_OK;
   SafcImmOmClientT *client=NULL;
   SafcImmAdminOwnerClientT *ao_ptr = NULL;
   int ref;
   SafcImmOmMemoryStructT *memory;

   TRACE_ENTER();

   LOCK_RDLOCK(&immOmClients_lock);
   ao_ptr = (SafcImmAdminOwnerClientT *) safc_array_get(&immOmClients, (int) ownerHandle);
   LOCK_UNLOCK(&immOmClients_lock);
   if (!ao_ptr) {
      TRACE1(("ERR_BAD_HANDLE: No initialized admin owner handle exists!"));
      return SA_AIS_ERR_BAD_HANDLE;
   }

   LOCK_RDLOCK(&immOmClients_lock);
   client = (SafcImmOmClientT *) safc_array_get(&immOmClients, (int) ao_ptr->immHandle);
   LOCK_UNLOCK(&immOmClients_lock);

   if (!client)
      return SA_AIS_ERR_BAD_HANDLE;

   /* for (i = 0; returnParams[i] != NULL; i++) { */
   /*    safc_free_imm_attr_value(returnParams[i]->paramType, returnParams[i]->paramBuffer); */
   /*    free(returnParams[i]); */
   /* } */

   /* free(returnParams); */

   ref = safc_array_find(&(client->allocated_memory), (void*) returnParams);
   if(ref) {
      memory = (SafcImmOmMemoryStructT *) safc_array_get(&(client->allocated_memory), ref);
      safc_array_remove(&(client->allocated_memory), ref) ;
      free(memory);
   }
   else
      TRACE1(("Couldn't find memory block in allocated_memory!"));

   safc_free_admop_return_params(returnParams);

   TRACE_LEAVE();

   return ret;
}

/* SaImmOmAdminOperationInvokeCallback */
static SaAisErrorT adminOpInvokeCb(SaImmHandleT immHandle,
			    SaImmOmAdminOperationInvokeCallback adminOpData,
			    SaImmOmAdminOperationInvokeCallbackT *adminOpCb)
{
  SaAisErrorT ret = SA_AIS_OK;

  TRACE_ENTER();

  (*adminOpCb)(adminOpData.invocation,
	       adminOpData.operationreturnvalue,
	       adminOpData.error);

  TRACE_LEAVE();

  return ret;

}

static SaAisErrorT adminOpInvokeCb_o2(SaImmHandleT immHandle,
			       SaImmOmAdminOperationInvokeCallbackO2 adminOpData,
			       SaImmOmAdminOperationInvokeCallbackT_o2 *adminOpCb)
{
  SaImmAdminOperationParamsT_2 **returnParams;
  SafcImmOmClientT *client=NULL;
  SaAisErrorT ret = SA_AIS_OK;
  int i;
  SafcImmOmMemoryStructT *memory;

  TRACE_ENTER();


   LOCK_RDLOCK(&immOmClients_lock);
   client = (SafcImmOmClientT *) safc_array_get(&immOmClients, (int) immHandle);
   LOCK_UNLOCK(&immOmClients_lock);
   if (!client) {
      TRACE1(("ERR_BAD_HANDLE: No initialized om handle exists!"));
      TRACE_LEAVE();
      return SA_AIS_ERR_BAD_HANDLE;
   }


   returnParams = calloc (1, sizeof (SaImmAdminOperationParamsT_2*) * (adminOpData.n_returnparams+1));

  for (i = 0; i < adminOpData.n_returnparams; i++) {
     returnParams[i] = calloc (1, sizeof (SaImmAdminOperationParamsT_2));
     returnParams[i]->paramName = adminOpData.returnparams[i]->paramname;
     returnParams[i]->paramType = adminOpData.returnparams[i]->paramtype;
     returnParams[i]->paramBuffer = safc_copy_to_imm_attr_value(returnParams[i]->paramType,
								   adminOpData.returnparams[i]->parambuffer);

  }
  returnParams[i] = NULL;
  memory = (SafcImmOmMemoryStructT *) calloc(1, sizeof(SafcImmOmMemoryStructT));
  memory->type = admin_op_params;
  memory->ptr = (void*) returnParams;
  (void) safc_array_insert(&(client->allocated_memory), (void*) memory);

  (*adminOpCb)(adminOpData.invocation,
	       adminOpData.operationreturnvalue,
	       adminOpData.error,
	       (const SaImmAdminOperationParamsT_2 **) returnParams);

  TRACE_LEAVE();

  return ret;

}

SaAisErrorT dispatchOmCallback(SaImmHandleT immHandle,
			       SaImmCallbacks callbacks_msg,
			       SafcImmOmClientT* client)
{
  SaAisErrorT ret = SA_AIS_OK;

  if (callbacks_msg.adminoperationinvokecallback != NULL && !client->isImmA2b)
    ret = adminOpInvokeCb(immHandle,
			 *callbacks_msg.adminoperationinvokecallback,
			  &(client->cb.callbacks.saImmOmAdminOperationInvokeCallback));
  else if (callbacks_msg.adminoperationinvokecallback_o2 != NULL&& client->isImmA2b)
    ret = adminOpInvokeCb_o2(immHandle,
			 *callbacks_msg.adminoperationinvokecallback_o2,
			     &(client->cb.callbacksA2b.saImmOmAdminOperationInvokeCallback));

  return ret;

}


int isVoidCallback(SaImmCallbacks callbacks_msg)
{

  if (callbacks_msg.adminoperationinvokecallback != NULL)
    return 1;
  else if (callbacks_msg.adminoperationinvokecallback_o2 != NULL)
    return 1;
  else
    return 0;

}

/*
 * ======================================================================
 * Support For Ccb Error Strings
 * ======================================================================
 */
SaAisErrorT saImmOmCcbGetErrorStrings(SaImmCcbHandleT ccbHandle,
				      const SaStringT **errorStrings)
{
   SaAisErrorT ret = SA_AIS_OK;
   SaAisErrorT recv_ret = SA_AIS_OK;
   SafcImmOmClientT *client = NULL;
   SafcImmCcbClientT *ccb_ptr = NULL;
   safc_buf_t msgbuf;
   SafsImmOmCcbGetErrorStrings ccb_get_error_strings_msg =
      SAFS_IMM_OM_CCB_GET_ERROR_STRINGS__INIT;
   SafsImmOmMessage msg = SAFS_IMM_OM_MESSAGE__INIT;
   SafsImmOmCcbGetErrorStringsRet *ret_msg;

   TRACE_ENTER();

   LOCK_RDLOCK(&immOmClients_lock);
   ccb_ptr = (SafcImmCcbClientT *) safc_array_get(&immOmClients, (int) ccbHandle);
   LOCK_UNLOCK(&immOmClients_lock);

   if (!ccb_ptr)
      return SA_AIS_ERR_BAD_HANDLE;

   LOCK_RDLOCK(&immOmClients_lock);
   client = (SafcImmOmClientT *) safc_array_get(&immOmClients, (int) ccb_ptr->immHandle);
   LOCK_UNLOCK(&immOmClients_lock);

   if (!client)
      return SA_AIS_ERR_BAD_HANDLE;
   
   /* Free memory from previous saImmOmCcbGetErrorStrings */
   if (ccb_ptr->errorStrings) {
      safc_free_error_strings(ccb_ptr->errorStrings);
      ccb_ptr->errorStrings = NULL;
   }

   /* Coding of message */
   ccb_get_error_strings_msg.handle = ccb_ptr->erlCcbHandle;

   msg.ccbgeterrorstrings = &ccb_get_error_strings_msg;

   msgbuf.size = safs_imm_om_message__get_packed_size(&msg);
   msgbuf.buf = malloc(msgbuf.size);
   safs_imm_om_message__pack(&msg, (uint8_t *) msgbuf.buf);
   
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
   ret_msg = safs_imm_om_ccb_get_error_strings_ret__unpack(NULL,
							   msgbuf.size,
							   (uint8_t *) msgbuf.buf);
   free(msgbuf.buf);

   if (ret_msg == NULL)
   {
      TRACE1(("SA_AIS_ERR_UNAVAILABLE: Couldn't unpack incoming message\n"));
      return SA_AIS_ERR_UNAVAILABLE;
   }

   ret = ret_msg->returnval;
   if(ret == SA_AIS_OK) {
      int i, length;
      SaStringT *strings;

      strings = calloc(1, sizeof(SaStringT *) * (ret_msg->n_errorstrings + 1));
      for (i=0; i < ret_msg->n_errorstrings; i++) {
	 length = strlen(ret_msg->errorstrings[i]);
	 strings[i] = calloc(1, length+1);
	 (void) strcpy(strings[i], ret_msg->errorstrings[i]);
      }
      strings[ret_msg->n_errorstrings] = NULL; 
      ccb_ptr->errorStrings = strings;

   }
   *errorStrings = ccb_ptr->errorStrings;

   safs_imm_om_ccb_get_error_strings_ret__free_unpacked(ret_msg, NULL);

   TRACE_LEAVE();

   return ret;
}



/*
 * ======================================================================
 * Support For Transactional Read
 * ======================================================================
 */
SaAisErrorT saImmOmCcbObjectRead(SaImmCcbHandleT ccbHandle,
					const SaNameT *objectName,
					const SaImmAttrNameT *attributeNames,
					SaImmAttrValuesT_2 ***attributes)
{
   SaAisErrorT ret = SA_AIS_OK;
   SaAisErrorT recv_ret = SA_AIS_OK;
   SafcImmOmClientT *client=NULL;
   SafcImmCcbClientT *ccb_ptr = NULL;
   safc_buf_t msgbuf;
   SafsImmOmCcbObjectRead ccb_or_msg =  SAFS_IMM_OM_CCB_OBJECT_READ__INIT;
   SafsImmOmMessage msg = SAFS_IMM_OM_MESSAGE__INIT;
   SafsImmOmCcbObjectReadRet *ret_msg;
   SaImmAttrNameT attributeName;

   TRACE_ENTER();

   if (objectName == NULL || (objectName->length == 0) || (objectName->length >= SA_MAX_NAME_LENGTH)) {
      TRACE1(("ERR_INVALID_PARAM: objectName"));
      TRACE_LEAVE();
      return SA_AIS_ERR_INVALID_PARAM;
   }

   if (!attributes) {
      TRACE1(("ERR_INVALID_PARAM: Invalid parameter: attributes"));
      TRACE_LEAVE();
      return SA_AIS_ERR_INVALID_PARAM;
   }

   LOCK_RDLOCK(&immOmClients_lock);
   ccb_ptr = (SafcImmCcbClientT *) safc_array_get(&immOmClients, (int) ccbHandle);
   LOCK_UNLOCK(&immOmClients_lock);

   if (!ccb_ptr) {
      TRACE_LEAVE();
      return SA_AIS_ERR_BAD_HANDLE;
   }

   /* Free memory from previous read */
   if (ccb_ptr->attributes) {
       safc_free_imm_attributes(ccb_ptr->attributes);
       ccb_ptr->attributes = NULL;
   }

   LOCK_RDLOCK(&immOmClients_lock);
   client = (SafcImmOmClientT *) safc_array_get(&immOmClients, (int) ccb_ptr->immHandle);
   LOCK_UNLOCK(&immOmClients_lock);

   if (!client)
      return SA_AIS_ERR_BAD_HANDLE;

   /* Free memory from previous saImmOmCcbGetErrorStrings */
   if (ccb_ptr->errorStrings) {
      safc_free_error_strings(ccb_ptr->errorStrings);
      ccb_ptr->errorStrings = NULL;
   }

   /* Coding of message */
   ccb_or_msg.handle = ccb_ptr->erlCcbHandle;
   assert(objectName->length < SA_MAX_NAME_LENGTH);
   ccb_or_msg.objectname = calloc(1, sizeof(SaUint8T) * (objectName->length + 1));      
   (void)memcpy(ccb_or_msg.objectname,  objectName->value, objectName->length);
   ccb_or_msg.objectname[objectName->length] = '\0';

   if (attributeNames) {
      int i;
      /* Calculate number of attribute names */
      for (i = 0; attributeNames[i]; i++);

      ccb_or_msg.n_attributenames = i;
      ccb_or_msg.attributenames = calloc (1, sizeof (char*) * ccb_or_msg.n_attributenames);

      for (i = 0; i < ccb_or_msg.n_attributenames; i++) {
	 attributeName = attributeNames[i];
	 ccb_or_msg.attributenames[i] = (char*) attributeName;
      }
   } else {
      ccb_or_msg.n_attributenames = 0;
      ccb_or_msg.attributenames = NULL;
   }

   msg.ccbobjectread = &ccb_or_msg;

   msgbuf.size = safs_imm_om_message__get_packed_size(&msg);
   msgbuf.buf = malloc(msgbuf.size);
   safs_imm_om_message__pack(&msg, (uint8_t *) msgbuf.buf);

   free(ccb_or_msg.attributenames);
   free(ccb_or_msg.objectname);

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
   ret_msg = safs_imm_om_ccb_object_read_ret__unpack(NULL,
						     msgbuf.size,
						     (uint8_t *) msgbuf.buf);
   free(msgbuf.buf);
   if (ret_msg == NULL)
   {
      TRACE1(("SA_AIS_ERR_UNAVAILABLE: Couldn't unpack incoming message\n"));
      return SA_AIS_ERR_UNAVAILABLE;
   }

   ret = ret_msg->returnval;
   if(ret == SA_AIS_OK) {
      int i;
      SaImmAttrValuesT_2 **attr = NULL;

      attr = calloc(1, sizeof(SaImmAttrValuesT_2 *) * (ret_msg->n_attributes + 1));

      for (i=0; i < ret_msg->n_attributes; i++) {
	 attr[i] = calloc(1, sizeof(SaImmAttrValuesT_2));
	 safc_copy_to_imm_attributes(ret_msg->attributes[i], attr[i]);
      }

      attr[ret_msg->n_attributes] = NULL;
      *attributes = attr;
      ccb_ptr->attributes = attr;
   }

   safs_imm_om_ccb_object_read_ret__free_unpacked(ret_msg, NULL);

   TRACE_LEAVE();

   return ret;
}



/*
 * ======================================================================
 * Internal Functions
 * ======================================================================
 */
void safc_free_class_create_msg(SafsImmOmClassCreate2 cc_msg)
{
   int i;
   SafsImmAttrDefinition2 *attr_def;

   for (i = 0; i < cc_msg.n_attrdefinitions; i++) {
      attr_def = cc_msg.attrdefinitions[i];

      if (attr_def->attrdefaultvalue)
	 switch(attr_def->attrvaluetype) {
	 case SA_IMM_ATTR_SANAMET:
	    free(attr_def->attrdefaultvalue->saname);
	    break;
	 case SA_IMM_ATTR_SAANYT:
	    free(attr_def->attrdefaultvalue->saany.data);
	    break;
	 default:
	    break;
	 }

      free(attr_def->attrdefaultvalue);
      free(cc_msg.attrdefinitions[i]);
   }
   free(cc_msg.attrdefinitions);
}

void safc_free_ccb_oc2_msg(SafsImmOmCcbObjectCreate2 ccb_oc2_msg)
{
   int i;

   for (i = 0; i < ccb_oc2_msg.n_attrvalues; i++)
      safc_free_safs_attr_values_2(ccb_oc2_msg.attrvalues[i]);
   free(ccb_oc2_msg.attrvalues);
   free(ccb_oc2_msg.parentname);
}

void safc_free_ccb_om2_msg(SafsImmOmCcbObjectModify2 ccb_om2_msg)
{
   int i;

   for (i = 0; i < ccb_om2_msg.n_attrmods; i++) {
      safc_free_safs_attr_values_2(ccb_om2_msg.attrmods[i]->modattr);
      free(ccb_om2_msg.attrmods[i]);
   }
   free(ccb_om2_msg.attrmods);
   free(ccb_om2_msg.objectname);
}

void safc_free_attr_definitions(SaImmAttrDefinitionT_2 **attrDefinitions)
{
   int i;

   for(i=0; attrDefinitions[i] != NULL; i++) {
      free(attrDefinitions[i]->attrName);
      if(attrDefinitions[i]->attrDefaultValue)
	 safc_free_imm_attr_value(attrDefinitions[i]->attrValueType,
				  attrDefinitions[i]->attrDefaultValue);
      free(attrDefinitions[i]);
   }

   free(attrDefinitions);

   return;
}

void safc_free_admop_return_params(SaImmAdminOperationParamsT_2 **returnParams)
{
   int i;

   for (i = 0; returnParams[i] != NULL; i++) {
      free(returnParams[i]->paramName);
      safc_free_imm_attr_value(returnParams[i]->paramType, returnParams[i]->paramBuffer);
      free(returnParams[i]);
   }

   free(returnParams);

   return;
}

void safc_delete_allocated_memory(void* elem)
{
   SafcImmOmMemoryStructT *m;

   if(!elem)
      return;
   else
      m = (SafcImmOmMemoryStructT*) elem;

   switch(m->type) {
   case class_definition_attrs:
      safc_free_attr_definitions((SaImmAttrDefinitionT_2 **) m->ptr);
      break;
   case admin_op_params:
      safc_free_admop_return_params((SaImmAdminOperationParamsT_2 **) m->ptr);
      break;
   default:
      TRACE1(("Wrong type of memory block, can't deallocate\n"));
      break;
   }

   free(m);

   return;
}

int safc_find_allocated_memory(void* elem, void* sval)
{

   SafcImmOmMemoryStructT *m = (SafcImmOmMemoryStructT*) elem;

   if(m->ptr == sval)
      return 1;
   else
      return 0;
}

int safc_find_ao_from_immhandle(void* elem, void* sval)
{

   SafcImmAdminOwnerClientT *m = (SafcImmAdminOwnerClientT*) elem;

   if(m->immHandle == *((SaImmHandleT*) sval))
      return 1;
   else
      return 0;
}

int safc_find_ccb_from_immhandle(void* elem, void* sval)
{

   SafcImmCcbClientT *m = (SafcImmCcbClientT*) elem;

   if(m->immHandle == *((SaImmHandleT*) sval))
      return 1;
   else
      return 0;
}

void safc_free_error_strings(SaStringT *strings) 
{
   int i;

   for (i = 0; strings[i] != NULL; i++) {
      free(strings[i]);
   }
   free(strings);
   return;
}
