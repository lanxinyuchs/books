/*
 *
 * Copyright (c) Ericsson AB  2013-2017 All rights reserved.
 *
 * The information in this document is the property of Ericsson. Except
 * as specifically authorized in writing by Ericsson,the receiver of this
 * document shall keep the information contained herein confidential
 * and shall protect the same in whole or in part from disclosure and
 * dissemination to third parties. Disclosure and disseminations to the
 * receiver's employees shall only be made on a strict need to know basis.
 */

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <fcntl.h>

#include <cec.h>

#include "cmsi.pb-c.h"
#include "cmsi.h"


#define CMSI_INIT_SERVICE         0
#define CMSI_TERM_SERVICE         1
#define CMSI_MIM2IMM              2
#define CMSI_IMM2MIM              3
#define CMSI_IS_MOCLASS_STRUCT    4
#define CMSI_IS_ATTRREF2STRUCT    5
#define CMSI_GET_STRUCTREFFORATTR 6
#define CMSI_GET_INTEGER          7
#define CMSI_GET_STRING           8
#define CMSI_GET_INTEGERS         9
#define CMSI_GET_STRINGS         10
#define CMSI_GET_MEID            11
#define CMSI_GET_ADMOPID         12
#define CMSI_GET_ACTIONNAME      13
#define CMSI_GET_OAPDATA         14
#define CMSI_GET_ME_USER_LABEL   15
#define CMSI_GET_IMM_CLASS       16
#define CMSI_GET_BIDIR_ASSOC     17
#define CMSI_GET_OAPDATA2        18
#define CMSI_GET_DN_PREFIX       19
#define CMSI_SUBSCR_OAP_DATA     20
#define CMSI_ILLEGAL_TYPE        99

typedef struct
{
  cec_handle_t *handle;
} CmsiProxyData;

static char signature[] = {'D', 'N', 'T', 'I'};

static CmsiProxyData* init_conn(void *);
static CmsiResultT send_recv(CmsiProxyData *, cec_packet_t *, cec_packet_t *);
static void term_conn(void *, CmsiProxyData *);
static size_t cmsi_strlcpy(char *, const char *, size_t);
static void populateCmsiAssociationEnd(struct CmsiAssociationEnd *,
				       ProtoAssociationEnd *);
static void populateCmsiHasClass(struct CmsiHasClass *, ProtoHasClass *);
static struct CmsiCardinality *createCmsiCardinality(ProtoCardinality *);
static void freeCmsiAssociationEnd(struct CmsiAssociationEnd *associationEnd);
static void freeCmsiCardinality(struct CmsiCardinality* cardinality);
static CmsiResultT parse_oapdata2(cec_packet_t *recv_packet,
				  struct CmsiOapData2 *oapData);

void*
cmsiInitiateService(void)
{
  CmsiProxyData *cmsiHandleP = NULL;

  cec_packet_t packet;
  char buffer[4];
  char *ptr;

  cmsiHandleP = malloc(sizeof *cmsiHandleP);

  cmsiHandleP->handle = cec_open(signature, sizeof(signature));

  packet.length = sizeof(buffer);
  ptr = packet.data = buffer;

  *(uint32_t*)ptr = CMSI_INIT_SERVICE;

  cec_send_with_pid(cmsiHandleP->handle, &packet);

  return cmsiHandleP;
}

void
cmsiTerminateService(void* cmsiHandleP)
{
  CmsiProxyData *cmsiProxyP = NULL;

  cec_packet_t packet;
  char buffer[4];
  char *ptr;

  packet.length = sizeof(buffer);
  ptr = packet.data = buffer;
  cmsiProxyP = (CmsiProxyData *)cmsiHandleP;

  *(uint32_t*)ptr = CMSI_TERM_SERVICE;
  cec_send_with_pid(cmsiProxyP->handle, &packet);
  cec_close(cmsiProxyP->handle);

  free(cmsiHandleP);
}

CmsiResultT
cmsiTransform(void *cmsiHandleP,
	      CmsiTransformDirectionT direction,
	      char *fromDnP,
	      char **toDnP)
{
  CmsiProxyData *cmsiProxyP = NULL;
  cec_packet_t send_packet, recv_packet;
  int fromDnSize = strlen(fromDnP) + 1;
  int send_length;
  char *ptr;
  int requestType  = CMSI_ILLEGAL_TYPE;
  int result       = CMSI_OK;

  /* toDnP should be NULL */
  if (toDnP == NULL || *toDnP != NULL) {
    return CMSI_INVALID_PARAMETER_TODNP;
  }

  /* Check if valid direction */
  if (direction == MIM_TO_IMM)
    requestType = CMSI_MIM2IMM;
  else if (direction == IMM_TO_MIM)
    requestType = CMSI_IMM2MIM;
  else {
    return CMSI_INVALID_PARAMETER_DIRECTION;
  }

  /* Valid in parameters */

  /* Initialize connection if not already initialized */
  cmsiProxyP = init_conn(cmsiHandleP);

  /* Build the send signal */
  send_length = fromDnSize + 2*sizeof(uint32_t);
  ptr = send_packet.data = malloc(send_length);
  send_packet.length = send_length;

  *(uint32_t*)ptr = requestType;
  ptr+=4;
  *(uint32_t*)ptr = fromDnSize;
  ptr+=4;
  strcpy(ptr, fromDnP);

  /* Send the request and wait for the translated reply */
  result = send_recv(cmsiProxyP, &send_packet, &recv_packet);
  if (result == CMSI_OK) {
    /* OK. Copy the result to result parameter */
    *toDnP = malloc(recv_packet.length);
    result = *((char *)recv_packet.data);
    strcpy(*toDnP, ((char *)recv_packet.data)+1);
    free(recv_packet.data);
  }

  free(send_packet.data);

  /* Terminate the connection if there was no connection before */
  term_conn(cmsiHandleP, cmsiProxyP);

  return result;
}

CmsiResultT
cmsiIsMoClassStruct(void *cmsiHandleP,
		    const SaImmClassNameT className,
		    int *isMoClassStruct)
{
  CmsiProxyData *cmsiProxyP = NULL;
  cec_packet_t send_packet, recv_packet;
  int classSize = strlen(className);
  int send_length;
  char *ptr;
  CmsiResultT result = CMSI_OK;

  /* Initialize connection if not already initialized */
  cmsiProxyP = init_conn(cmsiHandleP);

  /* Build the send signal */
  send_length = classSize + sizeof(uint32_t);
  ptr = send_packet.data = malloc(send_length);
  send_packet.length = send_length;

  *(uint32_t*)ptr = CMSI_IS_MOCLASS_STRUCT;
  ptr += sizeof(uint32_t);
  memcpy(ptr, className, classSize);

  /* Send the request and wait for the reply */
  result = send_recv(cmsiProxyP, &send_packet, &recv_packet);
  if (result == CMSI_OK) {
    ptr = recv_packet.data;
    result = *(uint32_t *)ptr;
    ptr += sizeof(uint32_t);
    *isMoClassStruct = *(int *)ptr;
    free(recv_packet.data);
  }

  free(send_packet.data);

  /* Terminate the connection if there was no connection before */
  term_conn(cmsiHandleP, cmsiProxyP);

  return result;
}

CmsiResultT
cmsiIsAttrRef2Struct(void *cmsiHandleP,
		     const SaImmClassNameT className,
		     SaImmAttrNameT attrName,
		     int *isAttrRef2Struct)
{
  CmsiProxyData *cmsiProxyP = NULL;
  cec_packet_t send_packet, recv_packet;
  int classSize = strlen(className);
  int attrSize = strlen(attrName);
  int send_length;
  char *ptr;
  CmsiResultT result = CMSI_OK;

  /* Initialize connection if not already initialized */
  cmsiProxyP = init_conn(cmsiHandleP);

  /* Build the send signal */
  send_length = sizeof(uint32_t) + sizeof(uint32_t) + classSize + attrSize;
  ptr = send_packet.data = malloc(send_length);
  send_packet.length = send_length;

  *(uint32_t*)ptr = CMSI_IS_ATTRREF2STRUCT;
  ptr += sizeof(uint32_t);
  *(uint32_t*)ptr = classSize;
  ptr += sizeof(uint32_t);
  memcpy(ptr, className, classSize);
  ptr += classSize;
  memcpy(ptr, attrName, attrSize);

  /* Send the request and wait for the reply */
  result = send_recv(cmsiProxyP, &send_packet, &recv_packet);
  if (result == CMSI_OK) {
    ptr = recv_packet.data;
    result = *(uint32_t *)ptr;
    ptr += sizeof(uint32_t);
    *isAttrRef2Struct = *(int *)ptr;
    free(recv_packet.data);
  }

  free(send_packet.data);

  /* Terminate the connection if there was no connection before */
  term_conn(cmsiHandleP, cmsiProxyP);

  return result;
}

CmsiResultT
cmsiGetStructRefForAttr(void *cmsiHandleP,
			const SaImmClassNameT className,
			const SaImmAttrNameT attrName,
			SaImmClassNameT* structRefName)
{
  CmsiProxyData *cmsiProxyP = NULL;
  cec_packet_t send_packet, recv_packet;
  uint32_t classSize = strlen(className);
  uint32_t attrSize = strlen(attrName);
  int send_length;
  char *ptr;
  CmsiResultT result = CMSI_OK;

  *structRefName = NULL;

  /* Initialize connection if not already initialized */
  cmsiProxyP = init_conn(cmsiHandleP);

  /* Build the send signal */
  send_length = sizeof(uint32_t) + sizeof(uint32_t) + classSize + attrSize;
  ptr = send_packet.data = malloc(send_length);
  send_packet.length = send_length;

  *(uint32_t*)ptr = CMSI_GET_STRUCTREFFORATTR;
  ptr += sizeof(uint32_t);
  *(uint32_t*)ptr = classSize;
  ptr += sizeof(uint32_t);
  memcpy(ptr, className, classSize);
  ptr += classSize;
  memcpy(ptr, attrName, attrSize);

  /* Send the request and wait for the reply */
  result = send_recv(cmsiProxyP, &send_packet, &recv_packet);
  if (result == CMSI_OK) {
    ptr = recv_packet.data;
    result = *(uint32_t *)ptr;

    if (result == CMSI_OK) {
      ptr += sizeof(uint32_t);
      *structRefName = malloc(recv_packet.length-sizeof(uint32_t));
      strcpy(*structRefName, ptr);
    }

    free(recv_packet.data);
  }

  free(send_packet.data);

  /* Terminate the connection if there was no connection before */
  term_conn(cmsiHandleP, cmsiProxyP);

  return result;
}

CmsiResultT
cmsiGetObjectId(void *cmsiHandleP,
		char *Ldn,
		int  *objectId)
{
  CmsiProxyData *cmsiProxyP = NULL;
  cec_packet_t send_packet, recv_packet;
  int dnSize = strlen(Ldn);
  int send_length;
  char *ptr;
  CmsiResultT result = CMSI_OK;

  /* Initialize connection if not already initialized */
  cmsiProxyP = init_conn(cmsiHandleP);

  /* Build the send signal */
  send_length = dnSize + sizeof(uint32_t);
  ptr = send_packet.data = malloc(send_length);
  send_packet.length = send_length;

  *(uint32_t*)ptr = CMSI_GET_INTEGER;
  ptr += sizeof(uint32_t);
  memcpy(ptr, Ldn, dnSize);

  /* Send the request and wait for the reply */
  result = send_recv(cmsiProxyP, &send_packet, &recv_packet);
  if (result == CMSI_OK) {
    ptr = recv_packet.data;
    result = *(uint32_t *)ptr;
    ptr += sizeof(uint32_t);
    *objectId = *(int *)ptr;
    free(recv_packet.data);
  }

  free(send_packet.data);

  /* Terminate the connection if there was no connection before */
  term_conn(cmsiHandleP, cmsiProxyP);

  return result;
}

CmsiResultT
cmsiGetObjectIds(void  *cmsiHandleP,
		 char **Ldns,
		 int    members,
		 int   *objectIds)
{
  CmsiProxyData *cmsiProxyP = NULL;
  cec_packet_t send_packet, recv_packet;
  size_t send_length;
  int i;
  char *ptr;
  CmsiResultT result = CMSI_OK;

  /* Initialize connection if not already initialized */
  cmsiProxyP = init_conn(cmsiHandleP);

  /* Build the send signal */
  send_length = (members + 1) * sizeof(uint32_t);
  for (i = 0; i < members; i++)
    send_length += strlen(Ldns[i]);

  ptr = send_packet.data = malloc(send_length);
  send_packet.length = send_length;

  *(uint32_t*)ptr = CMSI_GET_INTEGERS;
  ptr += sizeof(uint32_t);

  for (i = 0; i < members; i++) {
    uint32_t size = strlen(Ldns[i]);
    *(uint32_t*)ptr = size;
    ptr += sizeof(uint32_t);
    memcpy(ptr, Ldns[i], size);
    ptr += size;
  }

  /* Send the request and wait for the reply */
  result = send_recv(cmsiProxyP, &send_packet, &recv_packet);
  if (result == CMSI_OK) {
    ptr = recv_packet.data;
    result = *(uint32_t *)ptr;
    ptr += sizeof(uint32_t);

    for (i = 0; i < members; i++, objectIds++) {
      *objectIds = *(uint32_t *)ptr;
      ptr += sizeof(uint32_t);
    }

    free(recv_packet.data);
  }

  free(send_packet.data);

  /* Terminate the connection if there was no connection before */
  term_conn(cmsiHandleP, cmsiProxyP);

  return result;
}

CmsiResultT
cmsiGetLdn(void *cmsiHandleP,
           int   objectId,
           char *Ldn)
{
  CmsiProxyData *cmsiProxyP = NULL;
  cec_packet_t send_packet, recv_packet;
  int send_length;
  char *ptr;
  CmsiResultT result = CMSI_OK;

  /* Initialize connection if not already initialized */
  cmsiProxyP = init_conn(cmsiHandleP);

  /* Build the send signal */
  send_length = sizeof(uint32_t) + sizeof(int);
  ptr = send_packet.data = malloc(send_length);
  send_packet.length = send_length;

  *(uint32_t*)ptr = CMSI_GET_STRING;
  ptr += sizeof(uint32_t);
  *(int*)ptr = objectId;

  /* Send the request and wait for the reply */
  result = send_recv(cmsiProxyP, &send_packet, &recv_packet);
  if (result == CMSI_OK) {
    ptr = recv_packet.data;
    result = *(uint32_t *)ptr;

    if (result == CMSI_OK) {
      ptr += sizeof(uint32_t);
      strcpy(Ldn, ptr);
    }

    free(recv_packet.data);
  }

  free(send_packet.data);

  /* Terminate the connection if there was no connection before */
  term_conn(cmsiHandleP, cmsiProxyP);

  return result;
}

CmsiResultT
cmsiGetLdns(void  *cmsiHandleP,
	    int   *objectIds,
	    int    members,
	    char **Ldns)
{
  CmsiProxyData *cmsiProxyP = NULL;
  cec_packet_t send_packet, recv_packet;
  size_t send_length;
  int i;
  char *ptr;
  CmsiResultT result = CMSI_OK;

  /* Initialize connection if not already initialized */
  cmsiProxyP = init_conn(cmsiHandleP);

  /* Build the send signal */
  send_length = members*sizeof(int) + sizeof(uint32_t);
  ptr = send_packet.data = malloc(send_length);
  send_packet.length = send_length;

  *(uint32_t*)ptr = CMSI_GET_STRINGS;
  ptr += sizeof(uint32_t);

  for (i = 0; i < members; i++) {
    *(uint32_t*)ptr = objectIds[i];
    ptr += sizeof(uint32_t);
  }

  /* Send the request and wait for the reply */
  result = send_recv(cmsiProxyP, &send_packet, &recv_packet);
  if (result == CMSI_OK) {
    ptr = recv_packet.data;
    result = *(uint32_t *)ptr;
    ptr += sizeof(uint32_t);

    for (i = 0; i < members; i++, Ldns++) {
      uint32_t size = *(uint32_t *)ptr;

      ptr += sizeof(uint32_t);
      if (size == 0)
	**Ldns = 0;
      else {
	strcpy(*Ldns, ptr);
	ptr += size;
      }
    }

    free(recv_packet.data);
  }

  free(send_packet.data);

  /* Terminate the connection if there was no connection before */
  term_conn(cmsiHandleP, cmsiProxyP);

  return result;
}

CmsiResultT
cmsiGetManagedElementId(void *cmsiHandleP,
			char *managedElementId,
			size_t *size)
{
  CmsiProxyData *cmsiProxyP = NULL;
  cec_packet_t send_packet, recv_packet;
  size_t send_length;
  char *ptr;
  CmsiResultT result = CMSI_OK;

  /* Initialize connection if not already initialized */
  cmsiProxyP = init_conn(cmsiHandleP);

  /* Build the send signal */
  send_length = sizeof(uint32_t);
  ptr = send_packet.data = malloc(send_length);
  send_packet.length = send_length;

  *(uint32_t*)ptr = CMSI_GET_MEID;

  /* Send the request and wait for the reply */
  result = send_recv(cmsiProxyP, &send_packet, &recv_packet);
  if (result == CMSI_OK) {
    size_t len = *size;

    ptr = recv_packet.data;
    result = *(uint32_t *)ptr;
    ptr += sizeof(uint32_t);

    *size = cmsi_strlcpy(managedElementId, ptr, len);

    free(recv_packet.data);
  }

  free(send_packet.data);

  /* Terminate the connection if there was no connection before */
  term_conn(cmsiHandleP, cmsiProxyP);

  return result;
}

CmsiResultT
cmsiGetAdmOpId(void                   *cmsiHandleP,
	       const SaImmClassNameT   className,
	       const char*             actionName,
	       SaImmAdminOperationIdT *admOpId)
{
  CmsiProxyData *cmsiProxyP = NULL;
  cec_packet_t send_packet, recv_packet;
  uint32_t classSize = strlen(className);
  uint32_t actionSize = strlen(actionName);
  int send_length;
  char *ptr;
  CmsiResultT result = CMSI_OK;

  /* Initialize connection if not already initialized */
  cmsiProxyP = init_conn(cmsiHandleP);

  /* Build the send signal */
  send_length = sizeof(uint32_t) + sizeof(uint32_t) + classSize + actionSize;
  ptr = send_packet.data = malloc(send_length);
  send_packet.length = send_length;

  *(uint32_t*)ptr = CMSI_GET_ADMOPID;
  ptr += sizeof(uint32_t);
  *(uint32_t*)ptr = classSize;
  ptr += sizeof(uint32_t);
  memcpy(ptr, className, classSize);
  ptr += classSize;
  memcpy(ptr, actionName, actionSize);

  /* Send the request and wait for the reply */
  result = send_recv(cmsiProxyP, &send_packet, &recv_packet);
  if (result == CMSI_OK) {
    ptr = recv_packet.data;
    result = *(uint32_t *)ptr;
    ptr += sizeof(uint32_t);
    *admOpId = *(SaImmAdminOperationIdT *)ptr;
    free(recv_packet.data);
  }

  free(send_packet.data);

  /* Terminate the connection if there was no connection before */
  term_conn(cmsiHandleP, cmsiProxyP);

  return result;
}

CmsiResultT
cmsiGetActionName(void                  *cmsiHandleP,
		  const SaImmClassNameT  className,
		  SaImmAdminOperationIdT admOpId,
		  char*                  actionName)
{
  CmsiProxyData *cmsiProxyP = NULL;
  cec_packet_t send_packet, recv_packet;
  uint32_t classSize = strlen(className);
  size_t send_length;
  char *ptr;
  CmsiResultT result = CMSI_OK;

  /* Initialize connection if not already initialized */
  cmsiProxyP = init_conn(cmsiHandleP);

  /* Build the send signal */
  send_length = sizeof(uint32_t) + sizeof(SaImmAdminOperationIdT) + classSize;
  ptr = send_packet.data = malloc(send_length);
  send_packet.length = send_length;

  *(uint32_t*)ptr = CMSI_GET_ACTIONNAME;
  ptr += sizeof(uint32_t);

  *(SaImmAdminOperationIdT*)ptr = admOpId;
  ptr += sizeof admOpId;

  memcpy(ptr, className, classSize);

  /* Send the request and wait for the reply */
  result = send_recv(cmsiProxyP, &send_packet, &recv_packet);
  if (result == CMSI_OK) {
    ptr = recv_packet.data;
    result = *(uint32_t *)ptr;
    ptr += sizeof(uint32_t);

    cmsi_strlcpy(actionName, ptr, CMSI_ACTION_MAX_SIZE);

    free(recv_packet.data);
  }

  free(send_packet.data);

  /* Terminate the connection if there was no connection before */
  term_conn(cmsiHandleP, cmsiProxyP);

  return result;
}

CmsiResultT
cmsiGetOapData(void               *cmsiHandleP,
               struct CmsiOapData *oapData)
{
  CmsiProxyData *cmsiProxyP = NULL;
  cec_packet_t send_packet, recv_packet;
  size_t send_length;
  char *ptr;
  CmsiResultT result = CMSI_OK;

  /* Initialize connection if not already initialized */
  cmsiProxyP = init_conn(cmsiHandleP);

  /* Build the send signal */
  send_length = sizeof(uint32_t);
  ptr = send_packet.data = malloc(send_length);
  send_packet.length = send_length;

  *(uint32_t*)ptr = CMSI_GET_OAPDATA;

  /* Send the request and wait for the reply */
  result = send_recv(cmsiProxyP, &send_packet, &recv_packet);
  if (result == CMSI_OK) {
    char ns[100];
    int fd;

    ptr = recv_packet.data;
    result = *(uint32_t *)ptr;
    ptr += sizeof(uint32_t);

    oapData->addr = *(uint32_t *)ptr;
    ptr += sizeof(uint32_t);

    oapData->dscp = *(uint32_t *)ptr;
    ptr += sizeof(uint32_t);

    cmsi_strlcpy(ns, ptr, 100);
    fd = open(ns, O_RDONLY);
    oapData->fd = fd;

    free(recv_packet.data);
  }

  free(send_packet.data);

  /* Terminate the connection if there was no connection before */
  term_conn(cmsiHandleP, cmsiProxyP);

  return result;
}

CmsiResultT
cmsiGetOapData2(void                *cmsiHandleP,
		struct CmsiOapData2 *oapData)
{
  CmsiProxyData *cmsiProxyP = NULL;
  cec_packet_t send_packet, recv_packet;
  size_t send_length;
  char *ptr;
  CmsiResultT result = CMSI_OK;

  /* Initialize connection if not already initialized */
  cmsiProxyP = init_conn(cmsiHandleP);

  /* Build the send signal */
  send_length = sizeof(uint32_t);
  ptr = send_packet.data = malloc(send_length);
  send_packet.length = send_length;

  *(uint32_t*)ptr = CMSI_GET_OAPDATA2;

  /* Send the request and wait for the reply */
  result = send_recv(cmsiProxyP, &send_packet, &recv_packet);
  if (result == CMSI_OK) {
    result = parse_oapdata2(&recv_packet, oapData);
    free(recv_packet.data);
  }

  free(send_packet.data);

  /* Terminate the connection if there was no connection before */
  term_conn(cmsiHandleP, cmsiProxyP);

  return result;
}

CmsiResultT
cmsiGetManagedElementUserLabel(void *cmsiHandleP,
			       char *userLabel,
			       size_t *size)
{
  CmsiProxyData *cmsiProxyP = NULL;
  cec_packet_t send_packet, recv_packet;
  size_t send_length;
  char *ptr;
  CmsiResultT result = CMSI_OK;

  /* Initialize connection if not already initialized */
  cmsiProxyP = init_conn(cmsiHandleP);

  /* Build the send signal */
  send_length = sizeof(uint32_t);
  ptr = send_packet.data = malloc(send_length);
  send_packet.length = send_length;

  *(uint32_t*)ptr = CMSI_GET_ME_USER_LABEL;

  /* Send the request and wait for the reply */
  result = send_recv(cmsiProxyP, &send_packet, &recv_packet);
  if (result == CMSI_OK) {
    size_t len = *size;

    ptr = recv_packet.data;
    result = *(uint32_t *)ptr;
    ptr += sizeof(uint32_t);

    *size = cmsi_strlcpy(userLabel, ptr, len);

    free(recv_packet.data);
  }

  free(send_packet.data);

  /* Terminate the connection if there was no connection before */
  term_conn(cmsiHandleP, cmsiProxyP);

  return result;
}

CmsiResultT
cmsiGetImmClassName(void            *cmsiHandleP,
		    const char      *immDnP,
		    SaImmClassNameT *immClassName)
{
  CmsiProxyData *cmsiProxyP = NULL;
  cec_packet_t send_packet, recv_packet;
  size_t send_length;
  char *ptr;
  int immDnSize = strlen(immDnP) + 1;

  CmsiResultT result = CMSI_OK;

  *immClassName = NULL;

  /* Initialize connection if not already initialized */
  cmsiProxyP = init_conn(cmsiHandleP);

  /* Build the send signal */
  send_length = sizeof(uint32_t) + immDnSize;
  ptr = send_packet.data = malloc(send_length);
  send_packet.length = send_length;

  *(uint32_t*)ptr = CMSI_GET_IMM_CLASS;
  ptr += sizeof(uint32_t);
  strcpy(ptr, immDnP);

  /* Send the request and wait for the reply */
  result = send_recv(cmsiProxyP, &send_packet, &recv_packet);
  if (result == CMSI_OK) {
    ptr = recv_packet.data;
    result = *(uint32_t *)ptr;

    if (result == CMSI_OK) {
      ptr += sizeof(uint32_t);
      *immClassName = malloc(recv_packet.length-sizeof(uint32_t));
      strcpy(*immClassName, ptr);
    }

    free(recv_packet.data);
  }

  free(send_packet.data);

  /* Terminate the connection if there was no connection before */
  term_conn(cmsiHandleP, cmsiProxyP);

  return result;
}

void
cmsiFree(void* p)
{
  free(p);
}

CmsiResultT
cmsiGetBiDirAssociationForClient(void *cmsiHandleP,
				 const SaImmClassNameT reservingImmMoClass,
				 struct CmsiBiDirectionalAssociation*** pToppNullTermArray)
{
  CmsiProxyData *cmsiProxyP = NULL;
  cec_packet_t send_packet, recv_packet;
  uint32_t classSize = strlen(reservingImmMoClass);
  size_t send_length;
  char *ptr;
  CmsiResultT result = CMSI_OK;
  ProtoBidirRet *ret_msg;
  size_t n_bidirectionalassoc, i;
  struct CmsiBiDirectionalAssociation** ppNullTermArray;

  if (pToppNullTermArray == NULL)
    return result;

  *pToppNullTermArray = NULL;

  /* Initialize connection if not already initialized */
  cmsiProxyP = init_conn(cmsiHandleP);

  /* Build the send signal */
  send_length = sizeof(uint32_t) + classSize;
  ptr = send_packet.data = malloc(send_length);
  send_packet.length = send_length;

  *(uint32_t*)ptr = CMSI_GET_BIDIR_ASSOC;
  ptr += sizeof(uint32_t);

  memcpy(ptr, reservingImmMoClass, classSize);

  /* Send the request and wait for the reply */
  result = send_recv(cmsiProxyP, &send_packet, &recv_packet);

  if (result != CMSI_OK)
    goto EXIT;

  ret_msg = proto_bidir_ret__unpack(NULL, recv_packet.length,
				    (uint8_t *)recv_packet.data);

  free(recv_packet.data);

  if (ret_msg == NULL) {
    result = CMSI_RECEIVE_ERROR;
    goto EXIT;
  }

  result = ret_msg->result;

  if (result == CMSI_OK) {
    n_bidirectionalassoc = ret_msg->n_bidirectionalassoc;
    *pToppNullTermArray = ppNullTermArray =
      malloc((1+n_bidirectionalassoc) * sizeof(struct CmsiBiDirectionalAssociation*));
    ppNullTermArray[n_bidirectionalassoc] = NULL;

    for (i = 0; i < n_bidirectionalassoc; i++) {
      ProtoBiDirectionalAssociation *bidirectionalassoc;

      ppNullTermArray[i] = malloc(sizeof (struct CmsiBiDirectionalAssociation));
      bidirectionalassoc = ret_msg->bidirectionalassoc[i];

      populateCmsiAssociationEnd(&(ppNullTermArray[i]->reservingAssociationEnd),
				 bidirectionalassoc->reservingassociationend);
      populateCmsiAssociationEnd(&(ppNullTermArray[i]->reservedAssociationEnd),
				 bidirectionalassoc->reservedassociationend);
    }
  }

  proto_bidir_ret__free_unpacked(ret_msg, NULL);

 EXIT:
  free(send_packet.data);

  /* Terminate the connection if there was no connection before */
  term_conn(cmsiHandleP, cmsiProxyP);

  return result;
}

CmsiResultT
cmsiFreeBiDir(void *cmsiHandleP,
	      struct CmsiBiDirectionalAssociation** ppNullTermArray)
{

  (void)cmsiHandleP;
  struct CmsiBiDirectionalAssociation** pptr;

  if (ppNullTermArray == NULL)
    return CMSI_OK;

  pptr = ppNullTermArray;

  while (*pptr) {
    struct CmsiBiDirectionalAssociation* ptr = *pptr;

    freeCmsiAssociationEnd(&(ptr->reservingAssociationEnd));
    freeCmsiAssociationEnd(&(ptr->reservedAssociationEnd));

    free(ptr);
    pptr++;
  }

  free(ppNullTermArray);

  return CMSI_OK;
}

CmsiResultT
cmsiGetDnPrefix(void *cmsiHandleP,
		char *dnPrefix,
		size_t *size)
{
  CmsiProxyData *cmsiProxyP = NULL;
  cec_packet_t send_packet, recv_packet;
  size_t send_length;
  char *ptr;
  CmsiResultT result = CMSI_OK;

  /* Initialize connection if not already initialized */
  cmsiProxyP = init_conn(cmsiHandleP);

  /* Build the send signal */
  send_length = sizeof(uint32_t);
  ptr = send_packet.data = malloc(send_length);
  send_packet.length = send_length;

  *(uint32_t*)ptr = CMSI_GET_DN_PREFIX;

  /* Send the request and wait for the reply */
  result = send_recv(cmsiProxyP, &send_packet, &recv_packet);
  if (result == CMSI_OK) {
    size_t len = *size;

    ptr = recv_packet.data;
    result = *(uint32_t *)ptr;
    ptr += sizeof(uint32_t);

    *size = cmsi_strlcpy(dnPrefix, ptr, len);

    free(recv_packet.data);
  }

  free(send_packet.data);

  /* Terminate the connection if there was no connection before */
  term_conn(cmsiHandleP, cmsiProxyP);

  return result;
}

/* connection information, only ONE connection per thread is supported */
static __thread cec_handle_t *oapDataHandle = NULL;
static __thread cmsiOapDataSubscribeCallbackT theOapDataCallback = NULL;

CmsiResultT
cmsiOapDataSubscribe(cmsiOapDataSubscribeCallbackT oapDataCb,
		     int* oapDataFd)
{
  cec_packet_t packet;
  char buffer[sizeof(uint32_t)];
  char *ptr;

  if (oapDataCb == NULL) {
    if (oapDataHandle)
      cec_close(oapDataHandle);
    oapDataHandle = NULL;
    theOapDataCallback = NULL;
    *oapDataFd = -1;
    return CMSI_OK;
  }

  if (theOapDataCallback) {
    theOapDataCallback = oapDataCb;
    *oapDataFd = oapDataHandle->socket;
    return CMSI_OK;
  }

  oapDataHandle = cec_open(signature, sizeof(signature));

  packet.length = sizeof(buffer);
  ptr = packet.data = buffer;

  *(uint32_t*)ptr = CMSI_SUBSCR_OAP_DATA;

  if (cec_send_with_pid(oapDataHandle, &packet) == -1) {
    if (oapDataHandle)
      cec_close(oapDataHandle);
    oapDataHandle = NULL;
    theOapDataCallback = NULL;
    *oapDataFd = -1;
    return CMSI_SEND_ERROR;
  };

  theOapDataCallback = oapDataCb;
  *oapDataFd = oapDataHandle->socket;

  return CMSI_OK;
}

CmsiResultT
cmsiOapDataDispatch(void)
{
  CmsiResultT result;
  cec_packet_t recv_packet;
  struct CmsiOapData2 oapData;
  int timeout = 43000; // milliseconds

  if (oapDataHandle == NULL)
    return CMSI_NO_SUBSCRIBER;

  memset(&recv_packet, 0, sizeof recv_packet);
  if (cec_receive_w_tmeout(oapDataHandle, &recv_packet, timeout) == -1)
    return CMSI_RECEIVE_ERROR;

  result = parse_oapdata2(&recv_packet, &oapData);

  if (result == CMSI_OK)
    theOapDataCallback(&oapData);

  free(recv_packet.data);

  return result;
}

static CmsiProxyData*
init_conn(void *cmsiHandleP)
{
  CmsiProxyData *cmsiProxyP;

  if (cmsiHandleP == NULL) {
    cmsiProxyP = malloc(sizeof *cmsiProxyP);
    cmsiProxyP->handle = cec_open(signature, sizeof(signature));
  }
  else {
    cmsiProxyP = (CmsiProxyData *)cmsiHandleP;
  }

  return cmsiProxyP;
}

static CmsiResultT
send_recv(CmsiProxyData *cmsiProxyP,
	  cec_packet_t *send_packet,
	  cec_packet_t *recv_packet)
{
  int timeout = 43000; // milliseconds

  if (cec_send_with_pid(cmsiProxyP->handle, send_packet) == -1) {
    return CMSI_SEND_ERROR;
  }

  if (cec_receive_w_tmeout(cmsiProxyP->handle, recv_packet, timeout) == -1) {
    return CMSI_RECEIVE_ERROR;
  }

  return CMSI_OK;
}

static void
term_conn(void *cmsiHandleP, CmsiProxyData *cmsiProxyP)
{
  if (cmsiHandleP == NULL) {
    cec_close(cmsiProxyP->handle);
    free(cmsiProxyP);
  }
}

static size_t
cmsi_strlcpy(char *dst, const char *src, size_t size)
{
  /* Not the most efficient implementation :-)
   * Should be replaced by strlcpy() when/if it is included in the c library
   */

  size_t src_len, len;

  src_len = strlen(src);

  if (size != 0) {
    len = (src_len > size - 1) ? size - 1 : src_len;
    memcpy(dst, src, len);
    dst[len] = '\0';
  }

  return src_len;
}

static void
populateCmsiAssociationEnd(struct CmsiAssociationEnd *associationEnd,
			   ProtoAssociationEnd *protoAssociationEnd)
{
  associationEnd->name = strdup(protoAssociationEnd->name);
  populateCmsiHasClass(&(associationEnd->hasClass),
		       protoAssociationEnd->hasclass);
  associationEnd->cardinality =
    createCmsiCardinality(protoAssociationEnd->cardinality);
  associationEnd->isReserving = protoAssociationEnd->isreserving;
}

static void
populateCmsiHasClass(struct CmsiHasClass *hasClass,
		     ProtoHasClass *protoHasClass)
{
  hasClass->name     = strdup(protoHasClass->name);
  hasClass->mimName  = strdup(protoHasClass->mimname);
  hasClass->isScoped = protoHasClass->isscoped;
}

static struct CmsiCardinality*
createCmsiCardinality(ProtoCardinality *protoCardinality)
{
  struct CmsiCardinality *cardinality;

  if (protoCardinality == NULL)
    return NULL;

  cardinality = malloc(sizeof *cardinality);

  if (protoCardinality->has_min) {
    cardinality->min = malloc(sizeof (struct CmsiCardinalityValue));
    cardinality->min->value = protoCardinality->min;
  } else
    cardinality->min = NULL;

  if (protoCardinality->has_max) {
    cardinality->max = malloc(sizeof (struct CmsiCardinalityValue));
    cardinality->max->value = protoCardinality->max;
  } else
    cardinality->max = NULL;

  return cardinality;
}

static void
freeCmsiAssociationEnd(struct CmsiAssociationEnd *associationEnd)
{
  free(associationEnd->name);
  freeCmsiCardinality(associationEnd->cardinality);
  free(associationEnd->hasClass.name);
  free(associationEnd->hasClass.mimName);
}

static void
freeCmsiCardinality(struct CmsiCardinality* cardinality)
{
  if (cardinality == NULL)
    return;
  free(cardinality->min);
  free(cardinality->max);
  free(cardinality);
}

static CmsiResultT
parse_oapdata2(cec_packet_t *recv_packet,
	       struct CmsiOapData2 *oapData)
{
  CmsiResultT result = CMSI_OK;
  char *ptr;

  char ns[100];
  int fd;

  ptr = recv_packet->data;
  result = *(uint32_t *)ptr;
  ptr += sizeof(uint32_t);

  uint32_t len = *(uint32_t *)ptr;
  ptr += sizeof(uint32_t);

  if (len == 4) {
    struct sockaddr_in *ip4;

    ip4 = calloc(1, sizeof *ip4);
    ip4->sin_family = AF_INET;
    memcpy(&(ip4->sin_addr.s_addr), ptr, len);

    oapData->domain  = AF_INET;
    oapData->addr    = (struct sockaddr *)ip4;
    oapData->addrlen = sizeof *ip4;
  } else if (len == 16) {
    struct sockaddr_in6 *ip6;

    ip6 = calloc(1, sizeof *ip6);
    ip6->sin6_family = AF_INET6;
    memcpy(&(ip6->sin6_addr.s6_addr), ptr, len);

    oapData->domain  = AF_INET6;
    oapData->addr    = (struct sockaddr *)ip6;
    oapData->addrlen = sizeof *ip6;
  }

  ptr += len;

  oapData->dscp = *(uint32_t *)ptr;
  ptr += sizeof(uint32_t);

  cmsi_strlcpy(ns, ptr, 100);
  fd = open(ns, O_RDONLY);
  oapData->fd = fd;

  return result;
}
