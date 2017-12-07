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
 *  Purpose : SAFC IMM Class Transfer library
 * ----------------------------------------------------------------------
 *
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <netinet/in.h>
#include <pthread.h>
#include <unistd.h>
#include "safc_sock_lib.h"
#include "safc_array.h"
#include "safc_imm_lib.h"
#include "safc_trace.h"
#include "safcImmCt.h"
#include "safs_imm_ct_p.pb-c.h"

#define WRITE_INSTANCES 1
#define WRITE_RT_INSTANCES 2


/*
 * Client structures
 */
typedef struct safc_imm_ct_client {
   int sockfd;
   long long srv_handle;
   pthread_mutex_t mutex;
   safc_array allocated_memory;
} SafcImmCtClientT;


/*
 * Function declarations
 */

void safc_imm_ct_free_instance_groups(SafcImmCtInstanceGroupT **instanceGroups);
int safc_imm_ct_find_allocated_memory(void* elem, void* sval);
void free_write_msg(SafsImmCtWriteInstances write_msg);
void free_write_rt_msg(SafsImmCtWriteRtInstances write_msg);
static SaAisErrorT validate_arguments(
    const int function,
    const SaStringT implementerName,
    SafcImmCtInstanceGroupT **instanceGroups);

/*
 * Trace variable
 */
int safc_trace_level;

/*
 * Variables for the client structure storage array
 */
pthread_rwlock_t safcImmCtClients_lock = PTHREAD_RWLOCK_INITIALIZER;
safc_array safcImmCtClients = SAFC_ARRAY_INITIALIZER;

/*
 * ======================================================================
 * Library Functions
 * ======================================================================
 */
SaAisErrorT safcImmCtInitialize(SafcImmCtHandleT *handle)
{
   SafcImmCtClientT *client = NULL;
   SaAisErrorT ret = SA_AIS_OK;
   SaAisErrorT recv_ret = SA_AIS_OK;
   int port;
   char* hostname = "localhost";
   safc_buf_t msgbuf;
   SafsImmCtMessage msg = SAFS_IMM_CT_MESSAGE__INIT;
   SafsImmCtInitialize init_msg = SAFS_IMM_CT_INITIALIZE__INIT;
   SafsImmCtInitializeRet *ret_msg;

   TRACE_ENTER();

   if (!handle) {
      TRACE1(("ERR_INVALID_PARAM: handle is NULL\n"));
      return SA_AIS_ERR_INVALID_PARAM;
   }

   /* Alloc the client info data structure, handle for now */
   client = (SafcImmCtClientT *) calloc(1, sizeof(SafcImmCtClientT));
   if (client == NULL) {
      TRACE1(("ERR_NO_MEMORY: SAFC IMM CT client structure alloc failed\n"));
      return SA_AIS_ERR_NO_MEMORY;
   }

   port = safc_protobuf_port("SAFC_IMM_CT_PORT", 10005);

   client->sockfd = safc_connect(hostname, port);
   if (client->sockfd < 0) {
      TRACE1(("Connect to ICTI Port failed\n"));
      return SA_AIS_ERR_UNAVAILABLE;
   }

   safc_array_init(&(client->allocated_memory),
		       NULL, NULL, &safc_imm_ct_find_allocated_memory);
		       /* &safc_imm_ct_delete_allocated_memory,  */

   MUTEX_INIT(&client->mutex);

   LOCK_WRLOCK(&safcImmCtClients_lock);
   *handle = (SafcImmCtHandleT) safc_array_insert(&safcImmCtClients, (void*) client);
   LOCK_UNLOCK(&safcImmCtClients_lock);

   /* Coding of message */
   init_msg.callerpid = getpid();
   msg.initialize = &init_msg;

   msgbuf.size = safs_imm_ct_message__get_packed_size(&msg);
   msgbuf.buf = malloc(msgbuf.size);
   safs_imm_ct_message__pack(&msg, (uint8_t *) msgbuf.buf);

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
   ret_msg = safs_imm_ct_initialize_ret__unpack(NULL,
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
   ret = ret_msg->returnval;
   safs_imm_ct_initialize_ret__free_unpacked(ret_msg, NULL);

   if(ret != SA_AIS_OK)
      goto error;

   TRACE_LEAVE();

   return ret;

 error:
   safc_disconnect(client->sockfd);

   LOCK_WRLOCK(&safcImmCtClients_lock);
   safc_array_remove(&safcImmCtClients, (int) *handle);
   LOCK_UNLOCK(&safcImmCtClients_lock);
   handle = NULL;

   free(client);

   TRACE_LEAVE();

   return ret;
}

SaAisErrorT safcImmCtInitialize_2(SafcImmCtHandleT *handle, SaBoolT delayedStart)
{
   SafcImmCtClientT *client = NULL;
   SaAisErrorT ret = SA_AIS_OK;
   SaAisErrorT recv_ret = SA_AIS_OK;
   int port;
   char* hostname = "localhost";
   safc_buf_t msgbuf;
   SafsImmCtMessage msg = SAFS_IMM_CT_MESSAGE__INIT;
   SafsImmCtInitialize2 init_msg = SAFS_IMM_CT_INITIALIZE_2__INIT;
   SafsImmCtInitializeRet *ret_msg;

   TRACE_ENTER();

   if (!handle) {
      TRACE1(("ERR_INVALID_PARAM: handle is NULL\n"));
      return SA_AIS_ERR_INVALID_PARAM;
   }

   /* Alloc the client info data structure, handle for now */
   client = (SafcImmCtClientT *) calloc(1, sizeof(SafcImmCtClientT));
   if (client == NULL) {
      TRACE1(("ERR_NO_MEMORY: SAFC IMM CT client structure alloc failed\n"));
      return SA_AIS_ERR_NO_MEMORY;
   }

   port = safc_protobuf_port("SAFC_IMM_CT_PORT", 10005);

   client->sockfd = safc_connect(hostname, port);
   if (client->sockfd < 0) {
      TRACE1(("Connect to ICTI Port failed\n"));
      return SA_AIS_ERR_UNAVAILABLE;
   }

   safc_array_init(&(client->allocated_memory),
		       NULL, NULL, &safc_imm_ct_find_allocated_memory);
		       /* &safc_imm_ct_delete_allocated_memory,  */

   MUTEX_INIT(&client->mutex);

   LOCK_WRLOCK(&safcImmCtClients_lock);
   *handle = (SafcImmCtHandleT) safc_array_insert(&safcImmCtClients, (void*) client);
   LOCK_UNLOCK(&safcImmCtClients_lock);

   /* Coding of message */
   init_msg.callerpid = getpid();
   init_msg.delayedstart = delayedStart;
   msg.initialize_2 = &init_msg;

   msgbuf.size = safs_imm_ct_message__get_packed_size(&msg);
   msgbuf.buf = malloc(msgbuf.size);
   safs_imm_ct_message__pack(&msg, (uint8_t *) msgbuf.buf);

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
   ret_msg = safs_imm_ct_initialize_ret__unpack(NULL,
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
   ret = ret_msg->returnval;
   safs_imm_ct_initialize_ret__free_unpacked(ret_msg, NULL);

   if(ret != SA_AIS_OK)
      goto error;

   TRACE_LEAVE();

   return ret;

 error:
   safc_disconnect(client->sockfd);

   LOCK_WRLOCK(&safcImmCtClients_lock);
   safc_array_remove(&safcImmCtClients, (int) *handle);
   LOCK_UNLOCK(&safcImmCtClients_lock);
   handle = NULL;

   free(client);

   TRACE_LEAVE();

   return ret;
}

SaAisErrorT safcImmCtFinalize(SafcImmCtHandleT handle)
{
   SafcImmCtClientT *client = NULL;
   SaAisErrorT ret = SA_AIS_OK;
   SaAisErrorT recv_ret = SA_AIS_OK;
   safc_buf_t msgbuf;
   SafsImmCtMessage msg = SAFS_IMM_CT_MESSAGE__INIT;
   SafsImmCtFinalize fin_msg = SAFS_IMM_CT_FINALIZE__INIT;
   SafsImmCtFinalizeRet *ret_msg;


   TRACE_ENTER();

   LOCK_RDLOCK(&safcImmCtClients_lock);
   client = (SafcImmCtClientT *) safc_array_get(&safcImmCtClients, (int) handle);
   LOCK_UNLOCK(&safcImmCtClients_lock);
   if (!client)
      return SA_AIS_ERR_BAD_HANDLE;

   /* Coding of message */
   fin_msg.handle = client->srv_handle;
   msg.finalize = &fin_msg;

   msgbuf.size = safs_imm_ct_message__get_packed_size(&msg);
   msgbuf.buf = malloc(msgbuf.size);
   safs_imm_ct_message__pack(&msg, (uint8_t *) msgbuf.buf);

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
   ret_msg = safs_imm_ct_finalize_ret__unpack(NULL,
					     msgbuf.size,
					     (uint8_t *) msgbuf.buf);
   free(msgbuf.buf);

   if (ret_msg == NULL)
   {
      TRACE1(("SA_AIS_ERR_UNAVAILABLE: error unpacking incoming message\n"));
      return SA_AIS_ERR_UNAVAILABLE; // Perhaps another error code;
   }

   ret = ret_msg->returnval;
   safs_imm_ct_finalize_ret__free_unpacked(ret_msg, NULL);

   safc_disconnect(client->sockfd);

   safc_array_finalize(&(client->allocated_memory));

   LOCK_WRLOCK(&safcImmCtClients_lock);
   safc_array_remove(&safcImmCtClients, (int) handle);
   LOCK_UNLOCK(&safcImmCtClients_lock);

   free(client);

   TRACE_LEAVE();

   return ret;
}

SaAisErrorT safcImmCtReadSchemaVersion(SafcImmCtHandleT handle,
				       SaStringT schemaName,
				       SafcImmCtSchemaVersionT *oldSchemaVersion,
				       SafcImmCtSchemaVersionT *newSchemaVersion)
{
   SafcImmCtClientT *client = NULL;
   SaAisErrorT ret = SA_AIS_OK;
   SaAisErrorT recv_ret = SA_AIS_OK;
   safc_buf_t msgbuf;
   SafsImmCtMessage msg = SAFS_IMM_CT_MESSAGE__INIT;
   SafsImmCtReadSchemaVersion read_msg = SAFS_IMM_CT_READ_SCHEMA_VERSION__INIT;
   SafsImmCtReadSchemaVersionRet *ret_msg;

   TRACE_ENTER();

   LOCK_RDLOCK(&safcImmCtClients_lock);
   client = (SafcImmCtClientT *) safc_array_get(&safcImmCtClients, (int) handle);
   LOCK_UNLOCK(&safcImmCtClients_lock);
   if (!client)
      return SA_AIS_ERR_BAD_HANDLE;

   /* Coding of message */
   read_msg.handle = client->srv_handle;
   read_msg.schemaname = (char*) schemaName;

   msg.readschemaversion = &read_msg;

   msgbuf.size = safs_imm_ct_message__get_packed_size(&msg);
   msgbuf.buf = malloc(msgbuf.size);
   safs_imm_ct_message__pack(&msg, (uint8_t *) msgbuf.buf);

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
   ret_msg = safs_imm_ct_read_schema_version_ret__unpack(NULL,
							 msgbuf.size,
							 (uint8_t *) msgbuf.buf);
   free(msgbuf.buf);

   if (ret_msg == NULL)
   {
      TRACE1(("SA_AIS_ERR_UNAVAILABLE: error unpacking incoming message\n"));
      return SA_AIS_ERR_UNAVAILABLE; // Perhaps another error code;
   }

   ret = ret_msg->returnval;

   if(ret == SA_AIS_OK)
   {
      SafsImmCtSchemaVersion *old_version = ret_msg->oldversion,
	 *new_version = ret_msg->newversion;
      oldSchemaVersion->version = old_version->version;
      oldSchemaVersion->release = old_version->release;
      newSchemaVersion->version = new_version->version;
      newSchemaVersion->release = new_version->release;
   }

   safs_imm_ct_read_schema_version_ret__free_unpacked(ret_msg, NULL);

   TRACE_LEAVE();

   return ret;
}

SaAisErrorT safcImmCtReadSchemaVersion_2(SafcImmCtHandleT handle,
					 SaStringT schemaName,
					 SafcImmCtSchemaVersionT_2 *oldSchemaVersion,
					 SafcImmCtSchemaVersionT_2 *newSchemaVersion)
{
   SafcImmCtClientT *client = NULL;
   SaAisErrorT ret = SA_AIS_OK;
   SaAisErrorT recv_ret = SA_AIS_OK;
   safc_buf_t msgbuf;
   SafsImmCtMessage msg = SAFS_IMM_CT_MESSAGE__INIT;
   SafsImmCtReadSchemaVersion read_msg = SAFS_IMM_CT_READ_SCHEMA_VERSION__INIT;
   SafsImmCtReadSchemaVersionRet *ret_msg;

   TRACE_ENTER();

   LOCK_RDLOCK(&safcImmCtClients_lock);
   client = (SafcImmCtClientT *) safc_array_get(&safcImmCtClients, (int) handle);
   LOCK_UNLOCK(&safcImmCtClients_lock);
   if (!client)
      return SA_AIS_ERR_BAD_HANDLE;

   /* Coding of message */
   read_msg.handle = client->srv_handle;
   read_msg.schemaname = (char*) schemaName;

   msg.readschemaversion = &read_msg;

   msgbuf.size = safs_imm_ct_message__get_packed_size(&msg);
   msgbuf.buf = malloc(msgbuf.size);
   safs_imm_ct_message__pack(&msg, (uint8_t *) msgbuf.buf);

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
   ret_msg = safs_imm_ct_read_schema_version_ret__unpack(NULL,
							 msgbuf.size,
							 (uint8_t *) msgbuf.buf);
   free(msgbuf.buf);

   if (ret_msg == NULL)
   {
      TRACE1(("SA_AIS_ERR_UNAVAILABLE: error unpacking incoming message\n"));
      return SA_AIS_ERR_UNAVAILABLE; // Perhaps another error code;
   }

   ret = ret_msg->returnval;

   if(ret == SA_AIS_OK)
   {
      SafsImmCtSchemaVersion *old_version = ret_msg->oldversion,
	 *new_version = ret_msg->newversion;
      oldSchemaVersion->version = old_version->version;
      oldSchemaVersion->release = old_version->release;
      oldSchemaVersion->correction = old_version->correction;
      newSchemaVersion->version = new_version->version;
      newSchemaVersion->release = new_version->release;
      newSchemaVersion->correction = new_version->correction;
   }

   safs_imm_ct_read_schema_version_ret__free_unpacked(ret_msg, NULL);

   TRACE_LEAVE();

   return ret;
}

SaAisErrorT safcImmCtFailUpgrade(SafcImmCtHandleT handle, SaStringT message)
{
   SafcImmCtClientT *client = NULL;
   SaAisErrorT ret = SA_AIS_OK;
   SaAisErrorT recv_ret = SA_AIS_OK;
   safc_buf_t msgbuf;
   SafsImmCtMessage msg = SAFS_IMM_CT_MESSAGE__INIT;
   SafsImmCtFailUpgrade fail_msg = SAFS_IMM_CT_FAIL_UPGRADE__INIT;
   SafsImmCtFailUpgradeRet *ret_msg;

   TRACE_ENTER();

   LOCK_RDLOCK(&safcImmCtClients_lock);
   client = (SafcImmCtClientT *) safc_array_get(&safcImmCtClients, (int) handle);
   LOCK_UNLOCK(&safcImmCtClients_lock);
   if (!client)
      return SA_AIS_ERR_BAD_HANDLE;

   /* Coding of message */
   fail_msg.handle = client->srv_handle;
   fail_msg.message = (char*) message;

   msg.failupgrade = &fail_msg;

   msgbuf.size = safs_imm_ct_message__get_packed_size(&msg);
   msgbuf.buf = malloc(msgbuf.size);
   safs_imm_ct_message__pack(&msg, (uint8_t *) msgbuf.buf);

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
   ret_msg = safs_imm_ct_fail_upgrade_ret__unpack(NULL,
					       msgbuf.size,
					       (uint8_t *) msgbuf.buf);
   free(msgbuf.buf);

   if (ret_msg == NULL)
   {
      TRACE1(("SA_AIS_ERR_UNAVAILABLE: error unpacking incoming message\n"));
      return SA_AIS_ERR_UNAVAILABLE; // Perhaps another error code;
   }

   ret = ret_msg->returnval;


   safs_imm_ct_fail_upgrade_ret__free_unpacked(ret_msg, NULL);


   TRACE_LEAVE();

   return ret;
}


SaAisErrorT safcImmCtReadInstances(SafcImmCtHandleT handle,
				   SaImmClassNameT *classNames,
				   SafcImmCtInstanceGroupT ***instanceGroups)
{
   SafcImmCtClientT *client = NULL;
   SaAisErrorT ret = SA_AIS_OK;
   SaAisErrorT recv_ret = SA_AIS_OK;
   safc_buf_t msgbuf;
   SafsImmCtMessage msg = SAFS_IMM_CT_MESSAGE__INIT;
   SafsImmCtReadInstances read_msg = SAFS_IMM_CT_READ_INSTANCES__INIT;
   SafsImmCtReadInstancesRet *ret_msg;
   int i;

   TRACE_ENTER();

   if (!classNames || (classNames[0] == 0)) {
      return SA_AIS_ERR_INVALID_PARAM;
   }

   LOCK_RDLOCK(&safcImmCtClients_lock);
   client = (SafcImmCtClientT *) safc_array_get(&safcImmCtClients, (int) handle);
   LOCK_UNLOCK(&safcImmCtClients_lock);
   if (!client)
      return SA_AIS_ERR_BAD_HANDLE;

   /* Coding of message */
   read_msg.handle = client->srv_handle;

   for (i = 0; classNames[i]; i++);
   read_msg.n_classnames = i;

   read_msg.classnames = calloc(1, sizeof (char*) * read_msg.n_classnames);

   for (i = 0; i < read_msg.n_classnames; i++) {
      read_msg.classnames[i] = (char*) classNames[i];
   }

   msg.readinstances = &read_msg;

   msgbuf.size = safs_imm_ct_message__get_packed_size(&msg);
   msgbuf.buf = malloc(msgbuf.size);
   safs_imm_ct_message__pack(&msg, (uint8_t *) msgbuf.buf);

   free(read_msg.classnames);

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
   ret_msg = safs_imm_ct_read_instances_ret__unpack(NULL,
						 msgbuf.size,
						 (uint8_t *) msgbuf.buf);
   free(msgbuf.buf);

   if (ret_msg == NULL)
   {
      TRACE1(("SA_AIS_ERR_UNAVAILABLE: error unpacking incoming message\n"));
      return SA_AIS_ERR_UNAVAILABLE; // Perhaps another error code;
   }

   ret = ret_msg->returnval;

   if(ret == SA_AIS_OK) {
      int i, j, k, len;
      SaImmAttrValuesT_2 **attrs = NULL;
      SafcImmCtInstanceGroupT **instGrps;
      SafcImmCtInstanceT **instances;
      SafsImmCtInstance *instance;

      instGrps = calloc(1, sizeof(SafcImmCtInstanceGroupT *) * (ret_msg->n_instancegroups + 1));
      for (i=0; i < ret_msg->n_instancegroups; i++) {

	 instGrps[i] = calloc(1, sizeof(SafcImmCtInstanceGroupT));
	 len = strlen(ret_msg->instancegroups[i]->classname);
	 instGrps[i]->className = calloc(1, len+1);
	 (void) memcpy(instGrps[i]->className, ret_msg->instancegroups[i]->classname, len+1);

	 instGrps[i]->instances = calloc(1, sizeof(SafcImmCtInstanceT *) * (ret_msg->instancegroups[i]->n_instances + 1));
	 instances = instGrps[i]->instances;
	 for (j=0; j < ret_msg->instancegroups[i]->n_instances; j++) {
	    instances[j] = calloc(1, sizeof(SafcImmCtInstanceT));
	    instance = ret_msg->instancegroups[i]->instances[j];

	    if(instance->parentname != NULL) {
	       len = strlen(instance->parentname);
	       instances[j]->parentName = calloc(1, sizeof(SaNameT));
	       instances[j]->parentName->length = len;
	       (void) memcpy(instances[j]->parentName->value, instance->parentname, len);
	    } else
	       instances[j]->parentName = NULL;

	    instances[j]->attrValues = calloc(1, sizeof(SaImmAttrValuesT_2 *) * (instance->n_attrvalues + 1));
	    attrs = instances[j]->attrValues;

	    for (k=0; k < instance->n_attrvalues; k++) {
	       attrs[k] = calloc(1, sizeof(SaImmAttrValuesT_2));
	       safc_copy_to_imm_attributes(instance->attrvalues[k], attrs[k]);
	    }
	    attrs[instance->n_attrvalues] = NULL;
	 }
	 instances[ret_msg->instancegroups[i]->n_instances] = NULL;
      }
      instGrps[ret_msg->n_instancegroups] = NULL;

      (void) safc_array_insert(&(client->allocated_memory), (void*) instGrps);

      *instanceGroups = instGrps;
   }

   safs_imm_ct_read_instances_ret__free_unpacked(ret_msg, NULL);

   TRACE_LEAVE();

   return ret;
}


SaAisErrorT safcImmCtReadInstances_2(SafcImmCtHandleT handle,
				     SaImmClassNameT *classNames,
				     SafcImmCtInstanceGroupT ***instanceGroups)
{
   SafcImmCtClientT *client = NULL;
   SaAisErrorT ret = SA_AIS_OK;
   SaAisErrorT recv_ret = SA_AIS_OK;
   safc_buf_t msgbuf;
   SafsImmCtMessage msg = SAFS_IMM_CT_MESSAGE__INIT;
   SafsImmCtReadInstances2 read_msg = SAFS_IMM_CT_READ_INSTANCES_2__INIT;
   SafsImmCtReadInstancesRet *ret_msg;
   int i;

   TRACE_ENTER();

   if (!classNames || (classNames[0] == 0)) {
      return SA_AIS_ERR_INVALID_PARAM;
   }

   LOCK_RDLOCK(&safcImmCtClients_lock);
   client = (SafcImmCtClientT *) safc_array_get(&safcImmCtClients, (int) handle);
   LOCK_UNLOCK(&safcImmCtClients_lock);
   if (!client)
      return SA_AIS_ERR_BAD_HANDLE;

   /* Coding of message */
   read_msg.handle = client->srv_handle;

   for (i = 0; classNames[i]; i++);
   read_msg.n_classnames = i;

   read_msg.classnames = calloc(1, sizeof (char*) * read_msg.n_classnames);

   for (i = 0; i < read_msg.n_classnames; i++) {
      read_msg.classnames[i] = (char*) classNames[i];
   }

   msg.readinstances_2 = &read_msg;

   msgbuf.size = safs_imm_ct_message__get_packed_size(&msg);
   msgbuf.buf = malloc(msgbuf.size);
   safs_imm_ct_message__pack(&msg, (uint8_t *) msgbuf.buf);

   free(read_msg.classnames);

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
   ret_msg = safs_imm_ct_read_instances_ret__unpack(NULL,
						    msgbuf.size,
						    (uint8_t *) msgbuf.buf);
   free(msgbuf.buf);

   if (ret_msg == NULL)
   {
      TRACE1(("SA_AIS_ERR_UNAVAILABLE: error unpacking incoming message\n"));
      return SA_AIS_ERR_UNAVAILABLE; // Perhaps another error code;
   }

   ret = ret_msg->returnval;

   if(ret == SA_AIS_OK) {
      int i, j, k, len;
      SaImmAttrValuesT_2 **attrs = NULL;
      SafcImmCtInstanceGroupT **instGrps;
      SafcImmCtInstanceT **instances;
      SafsImmCtInstance *instance;

      instGrps = calloc(1, sizeof(SafcImmCtInstanceGroupT *) * (ret_msg->n_instancegroups + 1));
      for (i=0; i < ret_msg->n_instancegroups; i++) {

	 instGrps[i] = calloc(1, sizeof(SafcImmCtInstanceGroupT));
	 len = strlen(ret_msg->instancegroups[i]->classname);
	 instGrps[i]->className = calloc(1, len+1);
	 (void) memcpy(instGrps[i]->className, ret_msg->instancegroups[i]->classname, len+1);

	 instGrps[i]->instances = calloc(1, sizeof(SafcImmCtInstanceT *) * (ret_msg->instancegroups[i]->n_instances + 1));
	 instances = instGrps[i]->instances;
	 for (j=0; j < ret_msg->instancegroups[i]->n_instances; j++) {
	    instances[j] = calloc(1, sizeof(SafcImmCtInstanceT));
	    instance = ret_msg->instancegroups[i]->instances[j];

	    if(instance->parentname != NULL) {
	       len = strlen(instance->parentname);
	       instances[j]->parentName = calloc(1, sizeof(SaNameT));
	       instances[j]->parentName->length = len;
	       (void) memcpy(instances[j]->parentName->value, instance->parentname, len);
	    } else
	       instances[j]->parentName = NULL;

	    instances[j]->attrValues = calloc(1, sizeof(SaImmAttrValuesT_2 *) * (instance->n_attrvalues + 1));
	    attrs = instances[j]->attrValues;

	    for (k=0; k < instance->n_attrvalues; k++) {
	       attrs[k] = calloc(1, sizeof(SaImmAttrValuesT_2));
	       safc_copy_to_imm_attributes(instance->attrvalues[k], attrs[k]);
	    }
	    attrs[instance->n_attrvalues] = NULL;
	 }
	 instances[ret_msg->instancegroups[i]->n_instances] = NULL;
      }
      instGrps[ret_msg->n_instancegroups] = NULL;

      (void) safc_array_insert(&(client->allocated_memory), (void*) instGrps);

      *instanceGroups = instGrps;
   }

   safs_imm_ct_read_instances_ret__free_unpacked(ret_msg, NULL);

   TRACE_LEAVE();

   return ret;
}


SaAisErrorT safcImmCtWriteInstances(SafcImmCtHandleT handle,
				    SafcImmCtInstanceGroupT **instanceGroups)
{
   SafcImmCtClientT *client = NULL;
   SaAisErrorT ret = SA_AIS_OK;
   SaAisErrorT recv_ret = SA_AIS_OK;
   safc_buf_t msgbuf;
   SafsImmCtMessage msg = SAFS_IMM_CT_MESSAGE__INIT;
   SafsImmCtWriteInstances write_msg = SAFS_IMM_CT_WRITE_INSTANCES__INIT;
   SafsImmCtWriteInstancesRet *ret_msg;
   int i, j, k;
   SafsImmCtInstance **instances;
   SafcImmCtInstanceT **insts;
   SaAisErrorT validation_ret;

   TRACE_ENTER();

   validation_ret = validate_arguments(WRITE_INSTANCES, NULL, instanceGroups);
   if (validation_ret != SA_AIS_OK) {
     return validation_ret;
   }

   LOCK_RDLOCK(&safcImmCtClients_lock);
   client = (SafcImmCtClientT *) safc_array_get(&safcImmCtClients, (int) handle);
   LOCK_UNLOCK(&safcImmCtClients_lock);
   if (!client)
      return SA_AIS_ERR_BAD_HANDLE;

   /* Coding of message */
   write_msg.handle = client->srv_handle;

   for (i = 0; instanceGroups[i]; i++);
   write_msg.n_instancegroups = i;
   write_msg.instancegroups = calloc(1, sizeof(SafsImmCtInstanceGroup*) * i);/* Should be freed*/

   for (i = 0; i < write_msg.n_instancegroups; i++) {
      write_msg.instancegroups[i] = calloc(1, sizeof (SafsImmCtInstanceGroup));/* Should be freed*/
      safs_imm_ct_instance_group__init(write_msg.instancegroups[i]);
      write_msg.instancegroups[i]->classname = (char*) instanceGroups[i]->className;

      insts = instanceGroups[i]->instances;

      for (j = 0; insts[j]; j++);
      write_msg.instancegroups[i]->n_instances = j;
      write_msg.instancegroups[i]->instances = calloc(1, sizeof(SafsImmCtInstance*) * j);/* Should be freed*/
      instances = write_msg.instancegroups[i]->instances;

      for (j = 0; j < write_msg.instancegroups[i]->n_instances; j++) {
	 instances[j] = calloc(1, sizeof (SafsImmCtInstance));/* Should be freed*/
	 safs_imm_ct_instance__init(instances[j]);
	 if(insts[j]->parentName != NULL) {
	    instances[j]->parentname = calloc(1, insts[j]->parentName->length+1);
	    (void) memcpy(instances[j]->parentname, insts[j]->parentName->value, insts[j]->parentName->length); /* Should be freed*/
	    instances[j]->parentname[insts[j]->parentName->length] = '\0';
	 } else
	    instances[j]->parentname = NULL;

	 for(k = 0; insts[j]->attrValues[k]; k++);
	 instances[j]->n_attrvalues = k;
	 instances[j]->attrvalues = calloc (1, sizeof(SafsImmAttrValues2*) * k);

	 for (k = 0; k < instances[j]->n_attrvalues; k++) {
	    safc_copy_from_imm_attributes(insts[j]->attrValues[k], &(instances[j]->attrvalues[k]));
	 }
      }
   }

   msg.writeinstances = &write_msg;

   msgbuf.size = safs_imm_ct_message__get_packed_size(&msg);
   msgbuf.buf = malloc(msgbuf.size);
   safs_imm_ct_message__pack(&msg, (uint8_t *) msgbuf.buf);

   free_write_msg(write_msg);

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
   ret_msg = safs_imm_ct_write_instances_ret__unpack(NULL,
						  msgbuf.size,
						 (uint8_t *) msgbuf.buf);
   free(msgbuf.buf);

   if (ret_msg == NULL)
   {
      TRACE1(("SA_AIS_ERR_UNAVAILABLE: error unpacking incoming message\n"));
      return SA_AIS_ERR_UNAVAILABLE; // Perhaps another error code;
   }

   ret = ret_msg->returnval;

   safs_imm_ct_write_instances_ret__free_unpacked(ret_msg, NULL);

   TRACE_LEAVE();

   return ret;
}


SaAisErrorT safcImmCtCopyInstances(SafcImmCtHandleT handle,
				   SaImmClassNameT *classNames)
{
   SafcImmCtClientT *client = NULL;
   SaAisErrorT ret = SA_AIS_OK;
   SaAisErrorT recv_ret = SA_AIS_OK;
   safc_buf_t msgbuf;
   SafsImmCtMessage msg = SAFS_IMM_CT_MESSAGE__INIT;
   SafsImmCtCopyInstances copy_msg = SAFS_IMM_CT_COPY_INSTANCES__INIT;
   SafsImmCtCopyInstancesRet *ret_msg;
   int i;

   TRACE_ENTER();

   if (!classNames || (classNames[0] == 0)) {
      return SA_AIS_ERR_INVALID_PARAM;
   }

   LOCK_RDLOCK(&safcImmCtClients_lock);
   client = (SafcImmCtClientT *) safc_array_get(&safcImmCtClients, (int) handle);
   LOCK_UNLOCK(&safcImmCtClients_lock);
   if (!client)
      return SA_AIS_ERR_BAD_HANDLE;

   /* Coding of message */
   copy_msg.handle = client->srv_handle;

   for (i = 0; classNames[i]; i++);
   copy_msg.n_classnames = i;

   copy_msg.classnames = calloc(1, sizeof(char*) * copy_msg.n_classnames);

   for (i = 0; i < copy_msg.n_classnames; i++) {
      copy_msg.classnames[i] = (char*) classNames[i];
   }

   msg.copyinstances = &copy_msg;

   msgbuf.size = safs_imm_ct_message__get_packed_size(&msg);
   msgbuf.buf = malloc(msgbuf.size);
   safs_imm_ct_message__pack(&msg, (uint8_t *) msgbuf.buf);

   free(copy_msg.classnames);

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
   ret_msg = safs_imm_ct_copy_instances_ret__unpack(NULL,
						 msgbuf.size,
						 (uint8_t *) msgbuf.buf);
   free(msgbuf.buf);

   if (ret_msg == NULL)
   {
      TRACE1(("SA_AIS_ERR_UNAVAILABLE: error unpacking incoming message\n"));
      return SA_AIS_ERR_UNAVAILABLE; // Perhaps another error code;
   }

   ret = ret_msg->returnval;

   safs_imm_ct_copy_instances_ret__free_unpacked(ret_msg, NULL);

   TRACE_LEAVE();

   return ret;
}

SaAisErrorT safcImmCtWaitForClasses(SafcImmCtHandleT handle,
				    SaImmClassNameT *classNames)
{
   SafcImmCtClientT *client = NULL;
   SaAisErrorT ret = SA_AIS_OK;
   SaAisErrorT recv_ret = SA_AIS_OK;
   safc_buf_t msgbuf;
   SafsImmCtMessage msg = SAFS_IMM_CT_MESSAGE__INIT;
   SafsImmCtWaitForClasses wait_msg = SAFS_IMM_CT_WAIT_FOR_CLASSES__INIT;
   SafsImmCtWaitForClassesRet *ret_msg;
   int i;

   TRACE_ENTER();

   if (!classNames || (classNames[0] == 0)) {
      return SA_AIS_ERR_INVALID_PARAM;
   }

   LOCK_RDLOCK(&safcImmCtClients_lock);
   client = (SafcImmCtClientT *) safc_array_get(&safcImmCtClients, (int) handle);
   LOCK_UNLOCK(&safcImmCtClients_lock);
   if (!client)
      return SA_AIS_ERR_BAD_HANDLE;

   /* Coding of message */
   wait_msg.handle = client->srv_handle;

   for (i = 0; classNames[i]; i++);
   wait_msg.n_classnames = i;

   wait_msg.classnames = calloc(1, sizeof(char*) * wait_msg.n_classnames);

   for (i = 0; i < wait_msg.n_classnames; i++) {
      /* className = classNames[i]; */
      /* assert(objectName->length < SA_MAX_NAME_LENGTH); */
      wait_msg.classnames[i] = (char*) classNames[i];
   }

   msg.waitforclasses = &wait_msg;

   msgbuf.size = safs_imm_ct_message__get_packed_size(&msg);
   msgbuf.buf = malloc(msgbuf.size);
   safs_imm_ct_message__pack(&msg, (uint8_t *) msgbuf.buf);

   free(wait_msg.classnames);

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
   ret_msg = safs_imm_ct_wait_for_classes_ret__unpack(NULL,
						 msgbuf.size,
						 (uint8_t *) msgbuf.buf);
   free(msgbuf.buf);

   if (ret_msg == NULL)
   {
      TRACE1(("SA_AIS_ERR_UNAVAILABLE: error unpacking incoming message\n"));
      return SA_AIS_ERR_UNAVAILABLE; // Perhaps another error code;
   }

   ret = ret_msg->returnval;

   safs_imm_ct_wait_for_classes_ret__free_unpacked(ret_msg, NULL);

   TRACE_LEAVE();

   return ret;
}


SaAisErrorT safcImmCtWriteRtInstances(SafcImmCtHandleT handle,
				      SaStringT implementerName,
				      SafcImmCtInstanceGroupT **instanceGroups)
{
   SafcImmCtClientT *client = NULL;
   SaAisErrorT ret = SA_AIS_OK;
   SaAisErrorT recv_ret = SA_AIS_OK;
   safc_buf_t msgbuf;
   SafsImmCtMessage msg = SAFS_IMM_CT_MESSAGE__INIT;
   SafsImmCtWriteRtInstances write_msg = SAFS_IMM_CT_WRITE_RT_INSTANCES__INIT;
   SafsImmCtWriteRtInstancesRet *ret_msg;
   int i, j, k;
   SafsImmCtInstance **instances;
   SafcImmCtInstanceT **insts;
   SaAisErrorT validation_ret;

   TRACE_ENTER();

   validation_ret = validate_arguments(WRITE_INSTANCES, implementerName, instanceGroups);
   if (validation_ret != SA_AIS_OK) {
     return validation_ret;
   }

   LOCK_RDLOCK(&safcImmCtClients_lock);
   client = (SafcImmCtClientT *) safc_array_get(&safcImmCtClients, (int) handle);
   LOCK_UNLOCK(&safcImmCtClients_lock);
   if (!client)
      return SA_AIS_ERR_BAD_HANDLE;

   /* Coding of message */
   write_msg.handle = client->srv_handle;
   write_msg.implementername = (char*) implementerName;

   for (i = 0; instanceGroups[i]; i++);
   write_msg.n_instancegroups = i;
   write_msg.instancegroups = calloc(1, sizeof(SafsImmCtInstanceGroup*) * i);/* Should be freed*/

   for (i = 0; i < write_msg.n_instancegroups; i++) {
      write_msg.instancegroups[i] = calloc(1, sizeof (SafsImmCtInstanceGroup));/* Should be freed*/
      safs_imm_ct_instance_group__init(write_msg.instancegroups[i]);
      write_msg.instancegroups[i]->classname = (char*) instanceGroups[i]->className;

      insts = instanceGroups[i]->instances;

      for (j = 0; insts[j]; j++);
      write_msg.instancegroups[i]->n_instances = j;
      write_msg.instancegroups[i]->instances = calloc(1, sizeof(SafsImmCtInstance*) * j);/* Should be freed*/
      instances = write_msg.instancegroups[i]->instances;

      for (j = 0; j < write_msg.instancegroups[i]->n_instances; j++) {
	 instances[j] = calloc(1, sizeof (SafsImmCtInstance));/* Should be freed*/
	 safs_imm_ct_instance__init(instances[j]);
	 if(insts[j]->parentName != NULL) {
	    instances[j]->parentname = calloc(1, insts[j]->parentName->length+1);
	    (void) memcpy(instances[j]->parentname, insts[j]->parentName->value, insts[j]->parentName->length); /* Should be freed*/
	    instances[j]->parentname[insts[j]->parentName->length] = '\0';
	 } else
	    instances[j]->parentname = NULL;

	 for(k = 0; insts[j]->attrValues[k]; k++);
	 instances[j]->n_attrvalues = k;
	 instances[j]->attrvalues = calloc (1, sizeof(SafsImmAttrValues2*) * k);

	 for (k = 0; k < instances[j]->n_attrvalues; k++) {
	    safc_copy_from_imm_attributes(insts[j]->attrValues[k], &(instances[j]->attrvalues[k]));
	 }
      }
   }

   msg.writertinstances = &write_msg;

   msgbuf.size = safs_imm_ct_message__get_packed_size(&msg);
   msgbuf.buf = malloc(msgbuf.size);
   safs_imm_ct_message__pack(&msg, (uint8_t *) msgbuf.buf);

   free_write_rt_msg(write_msg);

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
   ret_msg = safs_imm_ct_write_rt_instances_ret__unpack(NULL,
						  msgbuf.size,
						 (uint8_t *) msgbuf.buf);
   free(msgbuf.buf);

   if (ret_msg == NULL)
   {
      TRACE1(("SA_AIS_ERR_UNAVAILABLE: error unpacking incoming message\n"));
      return SA_AIS_ERR_UNAVAILABLE; // Perhaps another error code;
   }

   ret = ret_msg->returnval;

   safs_imm_ct_write_rt_instances_ret__free_unpacked(ret_msg, NULL);

   TRACE_LEAVE();

   return ret;
}


SaAisErrorT safcImmCtCopyRtInstances(SafcImmCtHandleT handle,
				     SaStringT implementerName,
				     SaImmClassNameT *classNames)
{
   SafcImmCtClientT *client = NULL;
   SaAisErrorT ret = SA_AIS_OK;
   SaAisErrorT recv_ret = SA_AIS_OK;
   safc_buf_t msgbuf;
   SafsImmCtMessage msg = SAFS_IMM_CT_MESSAGE__INIT;
   SafsImmCtCopyRtInstances copy_msg = SAFS_IMM_CT_COPY_RT_INSTANCES__INIT;
   SafsImmCtCopyRtInstancesRet *ret_msg;
   int i;

   TRACE_ENTER();

   if (!classNames || (classNames[0] == 0)) {
      return SA_AIS_ERR_INVALID_PARAM;
   }

   LOCK_RDLOCK(&safcImmCtClients_lock);
   client = (SafcImmCtClientT *) safc_array_get(&safcImmCtClients, (int) handle);
   LOCK_UNLOCK(&safcImmCtClients_lock);
   if (!client)
      return SA_AIS_ERR_BAD_HANDLE;

   /* Coding of message */
   copy_msg.handle = client->srv_handle;
   copy_msg.implementername = (char*) implementerName;

   for (i = 0; classNames[i]; i++);
   copy_msg.n_classnames = i;

   copy_msg.classnames = calloc(1, sizeof(char*) * copy_msg.n_classnames);

   for (i = 0; i < copy_msg.n_classnames; i++) {
      /* className = classNames[i]; */
      /* assert(objectName->length < SA_MAX_NAME_LENGTH); */
      copy_msg.classnames[i] = (char*) classNames[i];
   }

   msg.copyrtinstances = &copy_msg;

   msgbuf.size = safs_imm_ct_message__get_packed_size(&msg);
   msgbuf.buf = malloc(msgbuf.size);
   safs_imm_ct_message__pack(&msg, (uint8_t *) msgbuf.buf);

   free(copy_msg.classnames);

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
   ret_msg = safs_imm_ct_copy_rt_instances_ret__unpack(NULL,
						 msgbuf.size,
						 (uint8_t *) msgbuf.buf);
   free(msgbuf.buf);

   if (ret_msg == NULL)
   {
      TRACE1(("SA_AIS_ERR_UNAVAILABLE: error unpacking incoming message\n"));
      return SA_AIS_ERR_UNAVAILABLE; // Perhaps another error code;
   }

   ret = ret_msg->returnval;

   safs_imm_ct_copy_rt_instances_ret__free_unpacked(ret_msg, NULL);

   TRACE_LEAVE();

   return ret;
}


SaAisErrorT safcImmCtInstanceGroupsMemoryFree(SafcImmCtHandleT handle,
				       SafcImmCtInstanceGroupT **instanceGroups)
{
   SaAisErrorT ret = SA_AIS_OK;
   SafcImmCtClientT *client=NULL;
   int ref;

   TRACE_ENTER();

   LOCK_RDLOCK(&safcImmCtClients_lock);
   client = (SafcImmCtClientT *) safc_array_get(&safcImmCtClients, (int) handle);
   LOCK_UNLOCK(&safcImmCtClients_lock);
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

   ref = safc_array_find(&(client->allocated_memory), (void*) instanceGroups);
   if(ref) {
      /* free(safc_array_get(&(client->allocated_memory), ref)) ; */
      safc_array_remove(&(client->allocated_memory), ref) ;
   } else
      TRACE1(("Couldn't find memory block in allocated_memory!"));

   safc_imm_ct_free_instance_groups(instanceGroups);

   TRACE_LEAVE();

   return ret;
}

/*
 * ======================================================================
 * Internal Functions
 * ======================================================================
 */

static void safc_imm_ct_free_instances(SafcImmCtInstanceT **instances)
{
   int i;

   for (i = 0; instances[i] != NULL; i++) {
      free(instances[i]->parentName);
      safc_free_imm_attributes(instances[i]->attrValues);
      free(instances[i]);
   }

   free(instances);

   return;
}

void safc_imm_ct_free_instance_groups(SafcImmCtInstanceGroupT **instanceGroups)
{
   int i;

   for (i = 0; instanceGroups[i] != NULL; i++) {
      free(instanceGroups[i]->className);
      safc_imm_ct_free_instances(instanceGroups[i]->instances);
      free(instanceGroups[i]);
   }

   free(instanceGroups);

   return;
}

int safc_imm_ct_find_allocated_memory(void* elem, void* sval)
{
   if(elem == sval)
      return 1;
   else
      return 0;
}

/* void safc_imm_ct_delete_allocated_memory(void* elem) */
/* { */
/*    SafcImmCtMemoryStructT *m; */

/*    if(!elem) */
/*       return; */
/*    else */
/*       m = (SafcImmCtMemoryStructT*) elem; */

/*    switch(m->type) { */
/*    case class_definition_attrs: */
/*       free_attr_definitions((SaImmAttrDefinitionT_2 **) m->ptr); */
/*       break; */
/*    case admin_op_params: */
/*       free_admop_return_params((SaImmAdminOperationParamsT_2 **) m->ptr); */
/*       break; */
/*    default: */
/*       TRACE1(("Wrong type of memory block, can't deallocate\n")); */
/*       break; */
/*    } */

/*    free(m); */

/*    return; */
/* } */

void free_write_msg(SafsImmCtWriteInstances write_msg)
{
   int i, j, k , l;
   SafsImmAttrValues2 *attr;
   SafsImmCtInstance *instance;
   SafsImmCtInstanceGroup *instance_group;

      for (i = 0; i < write_msg.n_instancegroups; i++) {
	 instance_group = write_msg.instancegroups[i];
	 // instance_group->classname : used original value and not allocated.
	 for (j = 0; j < instance_group->n_instances; j++) {
	    instance = instance_group->instances[j];
	    free(instance->parentname);
	    for (k = 0; k < instance->n_attrvalues; k++) {
	       attr = instance->attrvalues[k];
	       for (l = 0; l < attr->attrvaluesnumber; l++) {
		  switch(attr->attrvaluetype) {
		  case SA_IMM_ATTR_SANAMET:
		     free(attr->attrvalues[l]->saname);
		     break;
		  case SA_IMM_ATTR_SAANYT:
		     free(attr->attrvalues[l]->saany.data);
		     break;
		  default:
		     break;
		  }
		  free(attr->attrvalues[l]);
	       }
	       free(attr->attrvalues);
	       free(instance->attrvalues[k]);
	    }
	    free(instance->attrvalues);
	    free(instance_group->instances[j]);
	 }
	 free(instance_group->instances);
	 free(write_msg.instancegroups[i]);
      }
      free(write_msg.instancegroups);

      return;
}

void free_write_rt_msg(SafsImmCtWriteRtInstances write_msg)
{
   int i, j, k , l;
   SafsImmAttrValues2 *attr;
   SafsImmCtInstance *instance;
   SafsImmCtInstanceGroup *instance_group;

      for (i = 0; i < write_msg.n_instancegroups; i++) {
	 instance_group = write_msg.instancegroups[i];
	 // instance_group->classname : used original value and not allocated.
	 for (j = 0; j < instance_group->n_instances; j++) {
	    instance = instance_group->instances[j];
	    free(instance->parentname);
	    for (k = 0; k < instance->n_attrvalues; k++) {
	       attr = instance->attrvalues[k];
	       for (l = 0; l < attr->attrvaluesnumber; l++) {
		  switch(attr->attrvaluetype) {
		  case SA_IMM_ATTR_SANAMET:
		     free(attr->attrvalues[l]->saname);
		     break;
		  case SA_IMM_ATTR_SAANYT:
		     free(attr->attrvalues[l]->saany.data);
		     break;
		  default:
		     break;
		  }
		  free(attr->attrvalues[l]);
	       }
	       free(attr->attrvalues);
	       free(instance->attrvalues[k]);
	    }
	    free(instance->attrvalues);
	    free(instance_group->instances[j]);
	 }
	 free(instance_group->instances);
	 free(write_msg.instancegroups[i]);
      }
      free(write_msg.instancegroups);

      return;
}

/**
 * Sanity checks of the arguments passed to the writeInstances
 * functions.
 */
static SaAisErrorT validate_arguments(
    const int function,
    const SaStringT implementerName,
    SafcImmCtInstanceGroupT **instanceGroups) {

  if (function == WRITE_RT_INSTANCES) {
    if (implementerName == NULL) {
      TRACE1(("ImplementerName == NULL!"));
      return SA_AIS_ERR_INVALID_PARAM;
    }
  }

  if (function == WRITE_INSTANCES || function == WRITE_RT_INSTANCES) {
    if (instanceGroups == NULL) {
       TRACE1(("InstanceGroups == NULL!"));
       return SA_AIS_ERR_INVALID_PARAM;
    }
    else {
       for (int i = 0; instanceGroups[i] != NULL; i++) {
	  SafcImmCtInstanceT **insts;
	  SafcImmCtInstanceGroupT *ig = instanceGroups[i];
	  if (ig->className == NULL) {
	     TRACE1(("InstanceGroup classname == NULL!"));
	     return SA_AIS_ERR_INVALID_PARAM;
	  }
	  insts = ig->instances;
	  if (insts == NULL) {
	     TRACE1(("InstanceGroup instances == NULL!"));
	     return SA_AIS_ERR_INVALID_PARAM;
	  }
	  for (int j = 0; insts[j] != NULL; j++) {
	     SaImmAttrValuesT_2 **attrs;
	     SafcImmCtInstanceT *inst = insts[j];
	     SaNameT *pn = inst->parentName;
	     // the parent name MAY be NULL; otherwise check it
	     if (pn != NULL) {
		if (pn->length > SA_MAX_NAME_LENGTH) {
		   TRACE1(("InstanceGroup instamnce parentname too long!"));
		   return SA_AIS_ERR_INVALID_PARAM;
		}
	     }
	     attrs = inst->attrValues;
	     if (attrs == NULL) {
		TRACE1(("InstanceGroup instances attrs == NULL!"));
		return SA_AIS_ERR_INVALID_PARAM;
	     }
	     else {
		for (int k = 0; attrs[k] != NULL; k++) {
		   SaImmValueTypeT type;
		   SaImmAttrValuesT_2 *attr = attrs[k];
		   if (attr->attrName == NULL) {
		      TRACE1(("InstanceGroup instances attrs attrName == NULL!"));
		      return SA_AIS_ERR_INVALID_PARAM;
		   }
		   type = attr->attrValueType;
		   if (type != SA_IMM_ATTR_SAANYT
		       && type != SA_IMM_ATTR_CSSTRUCTT
		       && type != SA_IMM_ATTR_SADOUBLET
		       && type != SA_IMM_ATTR_SAFLOATT
		       && type != SA_IMM_ATTR_SAINT32T
		       && type != SA_IMM_ATTR_SAINT64T
		       && type != SA_IMM_ATTR_SANAMET
		       && type != SA_IMM_ATTR_SASTRINGT
		       && type != SA_IMM_ATTR_SATIMET
		       && type != SA_IMM_ATTR_SAUINT32T
		       && type != SA_IMM_ATTR_SAUINT64T) {
		      TRACE1(("InstanceGroup instances attrs attrValueType not allowed!"));
		      return SA_AIS_ERR_INVALID_PARAM;
		   }
		   for (int m = 0; m < attr->attrValuesNumber; m++) {
		      SaImmAttrValueT value = attr->attrValues[m];
		      if (value == NULL) {
			 TRACE1(("InstanceGroup instances attrs value == NULL !"));
			 return SA_AIS_ERR_INVALID_PARAM;
		      }
		      if (type == SA_IMM_ATTR_SASTRINGT) {
			 char **stringValue = (char **)value;
			 if (*stringValue == NULL) {
			    TRACE1(("InstanceGroup instances attrs string value == NULL !"));
			    return SA_AIS_ERR_INVALID_PARAM;
			 }
		      }
		      else if (type == SA_IMM_ATTR_SANAMET) {
			 SaNameT *name = (SaNameT *)value;
			 if (name->length > SA_MAX_NAME_LENGTH) {
			    TRACE1(("InstanceGroup instances attrs NameT value too long!"));
			    return SA_AIS_ERR_INVALID_PARAM;
			 }
		      }
		   }
		}
	     }
	  }
       }
    }
  }

  return SA_AIS_OK;
}
