/* ----------------------------------------------------------------------
 * %CCaseFile:	test_cmsi.c %
 * %CCaseRev:	/main/R2A/R3A/R4A/R5A/R6A/R8A/2 %
 * %CCaseDate:	2017-01-16 %
 * %CCaseDocNo: %
 * Author:	erarafo
 * Author: <name>, <e-mail address>
 *
 * Short description:
 * <Some rows here>
 * ----------------------------------------------------------------------
 * %CCaseTemplateFile:	template.c %
 * %CCaseTemplateId: CCver: /main/1 %
 *
 * %CCaseCopyrightBegin%
 * Copyright (c) Ericsson AB 2013-2017 All rights reserved.
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
 * R2A/3      2013-02-18 erarafo     Created
 * R3A/6      2014-12-03 etxpeno     Add support for cmsiGetAdmOpId() and
 *                                   cmsiGetActionName()
 * R4A/1      2015-11-26 erarafo     Wrappers around TRI trace macros
 * R5A/1      2016-04-19 etxpeno     Support for
 *                                   cmsiGetBiDirAssociationForClient() and
 *                                   cmsiFreeBiDir()
 * R6A/1      2016-05-25 etxpeno     Support for cmsiGetDnPrefix()
 * R8A/1      2017-01-16 erarafo     Adjustments for VRCS64
 * R8A/2      2017-01-16 erarafo     ClearCase version embedded in executable
 * ----------------------------------------------------------------------
 */

#include <cmsi.h>

#include "master.h"

#include <arpa/inet.h>

/* must match $RCS_TOP/GMF/GMF_CNX9012719/test/suites/test_cmsi.hrl */
#define CMSI_TRANSFORM                      1
#define CMSI_PASS_INVALID_TODNP_A           2
#define CMSI_CODE_EXAMPLE                   3
#define CMSI_INITIATE_SERVICE               4
#define CMSI_TERMINATE_SERVICE              5
#define CMSI_CLEANUP                        6
#define CMSI_TERMINATE_SERVICE_KEEP_HANDLE  7
#define CMSI_PASS_INVALID_TODNP_B           8
#define CMSI_IS_MO_CLASS_STRUCT             9
#define CMSI_IS_ATTR_REF_2_STRUCT           10
#define CMSI_GET_STRUCTREF_FOR_ATTR         11
#define CMSI_GET_OBJECT_ID                  12
#define CMSI_GET_LDN                        13
#define CMSI_GET_OBJECT_IDS                 14
#define CMSI_GET_LDNS                       15
#define CMSI_GET_ME_ID                      16
#define CMSI_GET_ADM_OP_ID                  17
#define CMSI_GET_ACTION_NAME                18
#define CMSI_GET_OAP_DATA                   19
#define CMSI_GET_ME_USER_LABEL              20
#define CMSI_GET_IMM_CLASS_NAME             21
#define CMSI_GET_OAP_DATA2                  22
#define CMSI_GET_BIDIR_ASS_FOR_CLIENT       23
#define CMSI_GET_DN_PREFIX                  24

extern bool TRI_trace_enabled;

static ei_x_buff send_sig_cmsi2(int func, ei_x_buff args);
static ei_x_buff send_sig_cmsi3(int func, ei_x_buff args);
static ei_x_buff send_sig_cmsi4(int func, ei_x_buff args);
static ei_x_buff send_sig_cmsi5(int func, ei_x_buff args);
static void encode_CmsiAssociationEnd(ei_x_buff*, const char*,
				      struct CmsiAssociationEnd*);

char *test_cmsi_version = "test_cmsi.c %CCaseRev:	/main/R2A/R3A/R4A/R5A/R6A/R8A/2 %";

/*
 * Translates function number to name.
 *
 */
static char*
getName(int func) {
  switch(func) {
  case CMSI_TRANSFORM:
    return "CMSI_TRANSFORM";
  case CMSI_PASS_INVALID_TODNP_A:
    return "CMSI_PASS_INVALID_TODNP_A";
  case CMSI_CODE_EXAMPLE:
    return "CMSI_CODE_EXAMPLE";
  case CMSI_INITIATE_SERVICE:
    return "CMSI_INITIATE_SERVICE";
  case CMSI_TERMINATE_SERVICE:
    return "CMSI_TERMINATE_SERVICE";
  case CMSI_CLEANUP:
    return "CMSI_CLEANUP";
  case CMSI_TERMINATE_SERVICE_KEEP_HANDLE:
    return "CMSI_TERMINATE_SERVICE_KEEP_HANDLE";
  case CMSI_PASS_INVALID_TODNP_B:
    return "CMSI_PASS_INVALID_TODNP_B";
  case CMSI_IS_MO_CLASS_STRUCT:
    return "CMSI_IS_MO_CLASS_STRUCT";
  case CMSI_IS_ATTR_REF_2_STRUCT:
    return "CMSI_IS_ATTR_REF_2_STRUCT";
  case CMSI_GET_STRUCTREF_FOR_ATTR:
    return "CMSI_GET_STRUCTREF_FOR_ATTR";
  case CMSI_GET_OBJECT_ID:
    return "CMSI_GET_OBJECT_ID";
  case CMSI_GET_LDN:
    return "CMSI_GET_LDN";
  case CMSI_GET_OBJECT_IDS:
    return "CMSI_GET_OBJECT_IDS";
  case CMSI_GET_LDNS:
    return "CMSI_GET_LDNS";
  case CMSI_GET_ME_ID:
    return "CMSI_GET_ME_ID";
  case CMSI_GET_ADM_OP_ID:
    return "CMSI_GET_ADM_OP_ID";
  case CMSI_GET_ACTION_NAME:
    return "CMSI_GET_ACTION_NAME";
  case  CMSI_GET_OAP_DATA:
    return "CMSI_GET_OAP_DATA";
  case CMSI_GET_ME_USER_LABEL:
    return "CMSI_GET_ME_USER_LABEL";
  case CMSI_GET_IMM_CLASS_NAME:
    return "CMSI_GET_IMM_CLASS_NAME";
  case  CMSI_GET_OAP_DATA2:
    return "CMSI_GET_OAP_DATA2";
  case  CMSI_GET_BIDIR_ASS_FOR_CLIENT:
    return "CMSI_GET_BIDIR_ASS_FOR_CLIENT";
  case CMSI_GET_DN_PREFIX:
    return "CMSI_GET_DN_PREFIX";
  default:
    return "functionNameUnknown";
  }
}

/*
 * Translates direction number to name.
 *
 */
static char*
getDirection(CmsiTransformDirectionT direction) {
  switch (direction) {
  case MIM_TO_IMM:
    return "MIM_TO_IMM";
  case IMM_TO_MIM:
    return "IMM_TO_MIM";
  default:
    return "directionNameUnknown";
  }
}



/*
 * Translates numeric result code to name.
 *
 */
static char*
getResult(CmsiResultT result) {
  switch (result) {
  case CMSI_OK:
    return "CMSI_OK";
  case CMSI_INVALID_PARAMETER_DIRECTION:
    return "CMSI_INVALID_PARAMETER_DIRECTION";
  case CMSI_INVALID_PARAMETER_TODNP:
    return "CMSI_INVALID_PARAMETER_TODNP";
  case CMSI_SEND_ERROR:
    return "CMSI_SEND_ERROR";
  case CMSI_RECEIVE_ERROR:
    return "CMSI_RECEIVE_ERROR";
  case CMSI_OBJECT_CLASS_NOT_FOUND:
    return "CMSI_OBJECT_CLASS_NOT_FOUND";
  case CMSI_CLASS_NOT_FOUND:
    return "CMSI_CLASS_NOT_FOUND";
  case CMSI_ATTRIBUTE_NOT_FOUND:
    return "CMSI_ATTRIBUTE_NOT_FOUND";
  case CMSI_ATTRIBUTE_NO_STRUCT_REF:
    return "CMSI_ATTRIBUTE_NO_STRUCT_REF";
  default:
    return "unknown result code";

  }
}

static char*
getBool(int boolean) {
  if (boolean == 0)
    return "false";
  return "true";
}

/*
 * The code example from the IWD, with minor
 * adjustments.
 *
 */
static int
codeExample(int argc, const char* argv[])
{
  QENTER1("codeExample");

  CmsiResultT res;

  if (argc != 3) {
    QTRACE3(3, "usage: %s direction distinguished_name", argv[0]);
    res = 1;
  }
  else {
    int dir = atoi(argv[1]);
    char* fromDnP = (char *) argv[2];
    char* toDnP = NULL;

    QTRACE4(3,
	    "call cmsiTransform(), direction: %d, input DN: %s",
	    dir, fromDnP);

    res = cmsiTransform(NULL, dir, fromDnP, &toDnP);

    if (res != CMSI_OK) {
      QTRACE_ERROR2("CMSI error, result: %d", res);
    }
    else {
      QTRACE3(3, "transformed DN: %s", toDnP);
    }

    free(toDnP);
  }

  QRETURN1(res);
}

static char*
getString(char* string, char* defaultString) {
  char* result;
  if (string == NULL) {
    result = defaultString;
  }
  else {
    result = string;
  }
  return result;
}

typedef struct {
  unsigned long clientId;
  void* handle;
  struct handles* next;
} handles;

static void*
getHandle(unsigned long id, handles* h) {
  QENTER1("getHandle");
  handles* p;
  void* result = NULL;
  for (p = h; p != NULL; p = (handles*)p->next) {
    if (p->clientId == id) {
      result = (void*)p->handle;
      break;
    }
  }
  QRETURN1(result);
}

static handles*
addHandle(unsigned long id, void* handle, handles* h) {
  QENTER1("addHandle");
  handles* result = malloc(sizeof(handles));
  result->clientId = id;
  result->handle = handle;
  result->next = (struct handles*)h;
  QRETURN1(result);
}

static handles*
connectionHandles = NULL;

static void
deleteHandles(handles* h) {
  QENTER1("deleteHandles");
  handles* q;
  for (handles* p = h; p != NULL; p = q) {
    q = (handles*)p->next;
    free(p);
  }
  QRETURN;
}

/*
 * Calls cmsiTransform once per list item. Optionally calls the
 * cmsiInitiateService and cmsiTerminateService functions too.
 * Optionally calls the codeExample function.
 *
 */
ei_x_buff
send_sig_cmsi(int func, ei_x_buff args) {
  int arity;

  QENTER3("send_sig_cmsi, function: %d (%s)", func, getName(func));

  QTRACE3(3, "CEC_PORT: %s", getString(getenv("CEC_PORT"), "undefined"));

  ei_decode_tuple_header(args.buff, &args.index, &arity);

  switch (arity) {
  case 2:
    return send_sig_cmsi2(func, args);
  case 3:
    return send_sig_cmsi3(func, args);
  case 4:
    return send_sig_cmsi4(func, args);
  case 5:
    return send_sig_cmsi5(func, args);
  default:
    {
      ei_x_buff resp;
      ei_x_format(&resp, "{ok, ~s, ~i}", "arity not supported", arity);
      QRETURN1(resp);
    }
  }
}

static ei_x_buff
send_sig_cmsi2(int func, ei_x_buff args){
  ei_x_buff resp;
  unsigned long clientId;
  int longLivingTransport;

  ei_x_new(&resp);
  ei_decode_ulong(args.buff, &args.index, &clientId);
  ei_decode_boolean(args.buff, &args.index, &longLivingTransport);

  switch (func) {
  case CMSI_GET_OAP_DATA:
    {
      CmsiResultT result;
      void *cmsiHandleP;
      struct CmsiOapData oapData;

      cmsiHandleP = clientId ? getHandle(clientId, connectionHandles) : NULL;

      result = cmsiGetOapData(cmsiHandleP, &oapData);
      if (result == CMSI_OK) {
	if (oapData.fd >= 0)
	  close(oapData.fd);

	ei_x_format(&resp, "{ok, ~i, ~i, ~u, ~i}", result, oapData.fd,
		    oapData.addr, oapData.dscp);
      }
      else
	ei_x_format(&resp, "{ok, ~i}", result);

      break;
    }
  case CMSI_GET_OAP_DATA2:
    {
      CmsiResultT result;
      void *cmsiHandleP;
      struct CmsiOapData2 oapData;

      cmsiHandleP = clientId ? getHandle(clientId, connectionHandles) : NULL;

      result = cmsiGetOapData2(cmsiHandleP, &oapData);
      if (result == CMSI_OK) {
	char str[INET6_ADDRSTRLEN];

	switch(oapData.domain) {
	case AF_INET:
	  // compiler warning for inet_ntop, cannot be fixed by including
	  // <arpa/inet.h> since this causes a conflict for the 'send' function
	  inet_ntop(AF_INET, &(((struct sockaddr_in *)oapData.addr)->sin_addr),
		    str, INET_ADDRSTRLEN);

	  ei_x_format(&resp, "{ok, ~i, ~i, ~a, ~s, ~i}", result, oapData.fd,
		      "AF_INET", str, oapData.dscp);
	  break;
	case AF_INET6:
	  // compiler warning for inet_ntop, cannot be fixed by including
	  // <arpa/inet.h> since this causes a conflict for the 'send' function
	  inet_ntop(AF_INET6,
		    &(((struct sockaddr_in6 *)oapData.addr)->sin6_addr),
		    str, INET6_ADDRSTRLEN);
	  ei_x_format(&resp, "{ok, ~i, ~i, ~a, ~s, ~i}", result, oapData.fd,
		      "AF_INET6", str, oapData.dscp);
	  break;
	default:
	  ei_x_format(&resp, "{ok, ~s, ~i}", "incorrect_domain",
		      oapData.domain);
	}

 	if (oapData.fd >= 0)
	  close(oapData.fd);

	free(oapData.addr);
      }
      else
	ei_x_format(&resp, "{ok, ~i}", result);

      break;
    }
  default:
    ei_x_format(&resp, "{ok, ~s, ~i}", "function does not exist", func);
  }

  return resp;
}

static ei_x_buff
send_sig_cmsi3(int func, ei_x_buff args){
  ei_x_buff resp;
  unsigned long clientId;
  int longLivingTransport;

  ei_x_new(&resp);
  ei_decode_ulong(args.buff, &args.index, &clientId);
  ei_decode_boolean(args.buff, &args.index, &longLivingTransport);

  switch (func) {
  case CMSI_IS_MO_CLASS_STRUCT:
    {
      CmsiResultT result;
      int isMoClassStruct;
      int type, size;
      char *className;
      void *cmsiHandleP;

      cmsiHandleP = clientId ? getHandle(clientId, connectionHandles) : NULL;

      ei_get_type(args.buff, &args.index, &type, &size);
      className = malloc(size+1);
      ei_decode_string(args.buff, &args.index, className);
      result = cmsiIsMoClassStruct(cmsiHandleP, className, &isMoClassStruct);
      free(className);
      ei_x_format(&resp, "{ok, ~i, ~a}", result, getBool(isMoClassStruct));
      break;
    }
  case CMSI_GET_OBJECT_ID:
    {
      CmsiResultT result;
      int objectId;
      int type, size;
      char *Ldn;
      void *cmsiHandleP;

      cmsiHandleP = clientId ? getHandle(clientId, connectionHandles) : NULL;

      ei_get_type(args.buff, &args.index, &type, &size);
      Ldn = malloc(size+1);
      ei_decode_string(args.buff, &args.index, Ldn);
      result = cmsiGetObjectId(cmsiHandleP, Ldn, &objectId);
      free(Ldn);
      if (result == CMSI_OK)
	ei_x_format(&resp, "{ok, ~i, ~i}", result, objectId);
      else
	ei_x_format(&resp, "{ok, ~i}", result);

      break;
    }
  case CMSI_GET_LDN:
    {
      CmsiResultT result;
      U32 objectId;
      char Ldn[CMSI_DN_MAX_SIZE];
      void *cmsiHandleP;

      cmsiHandleP = clientId ? getHandle(clientId, connectionHandles) : NULL;

      objectId = decodeToU32(&args);
      result = cmsiGetLdn(cmsiHandleP, (int)objectId, Ldn);
      if (result == CMSI_OK)
	ei_x_format(&resp, "{ok, ~i, ~s}", result, Ldn);
      else
	ei_x_format(&resp, "{ok, ~i}", result);

      break;
    }
  case CMSI_GET_OBJECT_IDS:
    {
      int type, size, i;
      CmsiResultT result;
      char **Ldns;
      int members;
      int *objects;
      void *cmsiHandleP;

      cmsiHandleP = clientId ? getHandle(clientId, connectionHandles) : NULL;

      ei_decode_list_header(args.buff, &args.index, &size);
      members = size;
      Ldns = malloc(members*sizeof(*Ldns));
      objects = malloc(members*sizeof(*objects));

      for (i = 0; i < members; i++) {
	ei_get_type(args.buff, &args.index, &type, &size);
	Ldns[i] = malloc(size+1);
	ei_decode_string(args.buff, &args.index, Ldns[i]);
      }

      result = cmsiGetObjectIds(cmsiHandleP, Ldns, members, objects);
      if (result == CMSI_OK) {
	ei_x_encode_version(&resp);
	ei_x_encode_tuple_header(&resp, 3);
	ei_x_encode_atom(&resp, "ok");
	ei_x_encode_long(&resp, result);
	ei_x_encode_list_header(&resp, members);
	for (i = 0; i < members; i++)
	  ei_x_encode_long(&resp, objects[i]);

	ei_x_encode_empty_list(&resp);
      }
      else
	ei_x_format(&resp, "{ok, ~i}", result);

      for (i = 0; i < members; i++)
	free(Ldns[i]);

      free(objects);
      free(Ldns);

      break;
    }
  case CMSI_GET_LDNS:
    {
      int type, size, i;
      CmsiResultT result;
      int *objects;
      int members;
      char **Ldns;
      void *cmsiHandleP;

      cmsiHandleP = clientId ? getHandle(clientId, connectionHandles) : NULL;

      ei_get_type(args.buff, &args.index, &type, &size);

      members = size;
      objects = malloc(members*sizeof(*objects));
      Ldns = malloc(members*sizeof(*Ldns));
      for (i = 0; i < members; i++)
	Ldns[i] = malloc(CMSI_DN_MAX_SIZE+1);

      switch(type) {
      case ERL_STRING_EXT:
	{
	  char *string = malloc(members+1);
	  ei_decode_string(args.buff, &args.index, string);
	  for (i = 0; i < members; i++)
	    objects[i] = string[i];

	  free(string);
	  break;
	}
      case ERL_LIST_EXT:
	ei_decode_list_header(args.buff, &args.index, &size);

	for (i = 0; i < members; i++) {
	  long iLong;
	  ei_decode_long(args.buff, &args.index, &iLong);
	  objects[i] = (int)iLong;
	}

	break;
      }

      result = cmsiGetLdns(cmsiHandleP, objects, members, Ldns);
      if (result == CMSI_OK) {
	ei_x_encode_version(&resp);
	ei_x_encode_tuple_header(&resp, 3);
	ei_x_encode_atom(&resp, "ok");
	ei_x_encode_long(&resp, result);
	ei_x_encode_list_header(&resp, members);
	for (i = 0; i < members; i++)
	  ei_x_encode_string(&resp, Ldns[i]);

	ei_x_encode_empty_list(&resp);
      }
      else
	ei_x_format(&resp, "{ok, ~i}", result);

      for (i = 0; i < members; i++)
	free(Ldns[i]);

      free(Ldns);
      free(objects);

      break;
    }
  case CMSI_GET_ME_ID:
    {
      CmsiResultT result;
      char *managedElementId;
      size_t size;
      void *cmsiHandleP;

      cmsiHandleP = clientId ? getHandle(clientId, connectionHandles) : NULL;

      size = (size_t)decodeToU32(&args);

      managedElementId = malloc(size);

      result = cmsiGetManagedElementId(cmsiHandleP,
				       managedElementId,
				       &size);
      if (result == CMSI_OK)
	ei_x_format(&resp, "{ok, ~i, ~s, ~i}", result, managedElementId,
		    size);
      else
	ei_x_format(&resp, "{ok, ~i}", result);

      free(managedElementId);
      break;
    }
  case CMSI_GET_ADM_OP_ID:
    {
      CmsiResultT result;
      int arity;
      int type, size;
      SaImmClassNameT className;
      char *actionName;
      SaImmAdminOperationIdT admOpId;
      void *cmsiHandleP;

      cmsiHandleP = clientId ? getHandle(clientId, connectionHandles) : NULL;

      ei_decode_tuple_header(args.buff, &args.index, &arity);

      ei_get_type(args.buff, &args.index, &type, &size);
      className = malloc(size+1);
      ei_decode_string(args.buff, &args.index, className);

      ei_get_type(args.buff, &args.index, &type, &size);
      actionName = malloc(size+1);
      ei_decode_string(args.buff, &args.index, actionName);

      result = cmsiGetAdmOpId(cmsiHandleP, className, actionName, &admOpId);
      if (result == CMSI_OK) {
	ei_x_encode_version(&resp);
	ei_x_encode_tuple_header(&resp, 3);
	ei_x_encode_atom(&resp, "ok");
	ei_x_encode_long(&resp, result);
	ei_x_encode_ulonglong(&resp, admOpId);
      }
      else
	ei_x_format(&resp, "{ok, ~i}", result);

      free(className);
      free(actionName);
      break;
    }
  case CMSI_GET_ACTION_NAME:
    {
      CmsiResultT result;
      int arity;
      int type, size;
      SaImmClassNameT className;
      SaImmAdminOperationIdT admOpId;
      char *actionName;
      void *cmsiHandleP;

      cmsiHandleP = clientId ? getHandle(clientId, connectionHandles) : NULL;

      ei_decode_tuple_header(args.buff, &args.index, &arity);

      ei_get_type(args.buff, &args.index, &type, &size);
      className = malloc(size+1);
      ei_decode_string(args.buff, &args.index, className);

      ei_decode_ulonglong(args.buff, &args.index,
			  (unsigned long long*)&admOpId);
      actionName = malloc(CMSI_ACTION_MAX_SIZE);

      result = cmsiGetActionName(cmsiHandleP, className, admOpId, actionName);
      if (result == CMSI_OK)
	ei_x_format(&resp, "{ok, ~i, ~s}", result, actionName);
      else
	ei_x_format(&resp, "{ok, ~i}", result);

      free(className);
      free(actionName);
      break;
    }
  case CMSI_GET_ME_USER_LABEL:
    {
      CmsiResultT result;
      char *userLabel;
      size_t size;
      void *cmsiHandleP;

      cmsiHandleP = clientId ? getHandle(clientId, connectionHandles) : NULL;

      size = (size_t)decodeToU32(&args);

      userLabel = malloc(size);

      result = cmsiGetManagedElementUserLabel(cmsiHandleP, userLabel, &size);
      if (result == CMSI_OK)
	ei_x_format(&resp, "{ok, ~i, ~s, ~i}", result, userLabel, size);
      else
	ei_x_format(&resp, "{ok, ~i}", result);

      free(userLabel);
      break;
    }
  case CMSI_GET_IMM_CLASS_NAME:
    {
      CmsiResultT result;
      char *immDnP;
      int type, size;
      SaImmClassNameT immClassName = NULL;
      void *cmsiHandleP;

      cmsiHandleP = clientId ? getHandle(clientId, connectionHandles) : NULL;
      ei_get_type(args.buff, &args.index, &type, &size);
      immDnP = malloc(size+1);
      ei_decode_string(args.buff, &args.index, immDnP);

      result = cmsiGetImmClassName(cmsiHandleP, immDnP, &immClassName);
      if (result == CMSI_OK)
	ei_x_format(&resp, "{ok, ~i, ~s}", result, immClassName);
      else
	ei_x_format(&resp, "{ok, ~i}", result);

      free(immClassName);
      free(immDnP);
      break;
    }
  case CMSI_GET_BIDIR_ASS_FOR_CLIENT:
    {
      CmsiResultT result;
      struct CmsiBiDirectionalAssociation** ppNullTermArray;
      int type, size;
      SaImmClassNameT immClass = NULL;
      void *cmsiHandleP;

      cmsiHandleP = clientId ? getHandle(clientId, connectionHandles) : NULL;
      ei_get_type(args.buff, &args.index, &type, &size);
      immClass = malloc(size+1);
      ei_decode_string(args.buff, &args.index, immClass);

      result = cmsiGetBiDirAssociationForClient(cmsiHandleP, immClass,
						&ppNullTermArray);
      if (result == CMSI_OK) {
	struct CmsiBiDirectionalAssociation** ptr;

	ei_x_encode_version(&resp);
	ei_x_encode_tuple_header(&resp, 3);
	ei_x_encode_atom(&resp, "ok");
	ei_x_encode_long(&resp, result);
	for (ptr = ppNullTermArray; *ptr; ptr++) {
	  ei_x_encode_list_header(&resp, 1);
	  ei_x_encode_tuple_header(&resp, 2);
	  encode_CmsiAssociationEnd(&resp, "reserving",
				    &(*ptr)->reservingAssociationEnd);
	  encode_CmsiAssociationEnd(&resp, "reserved",
				    &(*ptr)->reservedAssociationEnd);
	}

	ei_x_encode_empty_list(&resp);
	cmsiFreeBiDir(cmsiHandleP, ppNullTermArray);
      } else
	ei_x_format(&resp, "{ok, ~i}", result);

      free(immClass);
      break;
    }
  case CMSI_GET_DN_PREFIX:
    {
      CmsiResultT result;
      char *dnPrefix;
      size_t size;
      void *cmsiHandleP;

      cmsiHandleP = clientId ? getHandle(clientId, connectionHandles) : NULL;

      size = (size_t)decodeToU32(&args);

      dnPrefix = malloc(size);

      result = cmsiGetDnPrefix(cmsiHandleP, dnPrefix, &size);
      if (result == CMSI_OK)
	ei_x_format(&resp, "{ok, ~i, ~s, ~i}", result, dnPrefix, size);
      else
	ei_x_format(&resp, "{ok, ~i}", result);

      free(dnPrefix);
      break;
    }
  default:
    ei_x_format(&resp, "{ok, ~s, ~i}", "function does not exist", func);
  }

  return resp;
}

static ei_x_buff
send_sig_cmsi4(int func, ei_x_buff args){
  ei_x_buff resp;
  unsigned long clientId;
  int longLivingTransport;

  ei_x_new(&resp);
  ei_decode_ulong(args.buff, &args.index, &clientId);
  ei_decode_boolean(args.buff, &args.index, &longLivingTransport);

  switch (func) {
  case CMSI_IS_ATTR_REF_2_STRUCT:
    {
      CmsiResultT result;
      int isAttrRef2Struct;
      int type, size;
      char *className;
      char *attrName;
      void *cmsiHandleP;

      cmsiHandleP = clientId ? getHandle(clientId, connectionHandles) : NULL;
      ei_get_type(args.buff, &args.index, &type, &size);
      className = malloc(size+1);
      ei_decode_string(args.buff, &args.index, className);
      ei_get_type(args.buff, &args.index, &type, &size);
      attrName = malloc(size+1);
      ei_decode_string(args.buff, &args.index, attrName);
      result = cmsiIsAttrRef2Struct(cmsiHandleP, className, attrName,
				    &isAttrRef2Struct);
      free(className);
      free(attrName);
      ei_x_format(&resp, "{ok, ~i, ~a}", result, getBool(isAttrRef2Struct));
      break;
    }
  case CMSI_GET_STRUCTREF_FOR_ATTR:
    {
      CmsiResultT result;
      int type, size;
      char *className;
      char *attrName;
      SaImmClassNameT structRefName = NULL;
      void *cmsiHandleP;

      cmsiHandleP = clientId ? getHandle(clientId, connectionHandles) : NULL;
      ei_get_type(args.buff, &args.index, &type, &size);
      className = malloc(size+1);
      ei_decode_string(args.buff, &args.index, className);
      ei_get_type(args.buff, &args.index, &type, &size);
      attrName = malloc(size+1);
      ei_decode_string(args.buff, &args.index, attrName);
      result = cmsiGetStructRefForAttr(cmsiHandleP, className, attrName,
				       &structRefName);
      if (structRefName)
	ei_x_format(&resp, "{ok, ~i, ~s}", result, structRefName);
      else
	ei_x_format(&resp, "{ok, ~i, undefined}", result);

      free(className);
      free(attrName);
      free(structRefName);
      break;
    }
  default:
    ei_x_format(&resp, "{ok, ~s, ~i}", "function does not exist", func);
  }

  return resp;
}

static ei_x_buff
send_sig_cmsi5(int func, ei_x_buff args) {
  /* We have a 5-element tuple
   * {ClientId, LongLivingTn, Direction, InDnLength, InDn}
   */

  ei_x_buff resp;
  unsigned long clientId;

  ei_x_new(&resp);
  ei_decode_ulong(args.buff, &args.index, &clientId);

  if (func == CMSI_CLEANUP) {
    deleteHandles(connectionHandles);
    connectionHandles = NULL;
    ei_x_format(&resp, "{ok}");
  }
  else if (func == CMSI_INITIATE_SERVICE) {
    void* handle = cmsiInitiateService();
    if (handle == NULL) {
      ei_x_format(&resp, "{error, ~s, ~i}",
		  "could not initiate service for client", clientId);
    }
    else {
      connectionHandles = addHandle(clientId, handle, connectionHandles);
      ei_x_format(&resp, "{ok}");
    }
  }
  else if (func == CMSI_TERMINATE_SERVICE) {
    void* handle = getHandle(clientId, connectionHandles);
    if (handle == NULL) {
      ei_x_format(&resp, "{error, ~s, ~i}", "no active connection for client",
		  clientId);
    }
    else {
      cmsiTerminateService(handle);
      connectionHandles = addHandle(clientId, NULL, connectionHandles);
      ei_x_format(&resp, "{ok}");
    }
  }
  else if (func == CMSI_TERMINATE_SERVICE_KEEP_HANDLE) {
    void* handle = getHandle(clientId, connectionHandles);
    if (handle == NULL) {
      ei_x_format(&resp, "{error, ~s, ~i}", "no active connection for client"
		  , clientId);
    }
    else {
      cmsiTerminateService(handle);
      ei_x_format(&resp, "{ok}");
    }
  }
  else {
    int longLivingTransport;
    long direction;
    long dnLength;

    ei_decode_boolean(args.buff, &args.index, &longLivingTransport);
    ei_decode_long(args.buff, &args.index, &direction);
    ei_decode_long(args.buff, &args.index, &dnLength);

    char* inDn = malloc(dnLength + 1);
    ei_decode_string(args.buff, &args.index, inDn);

    if (func == CMSI_CODE_EXAMPLE) {
      /* invoke the code example; transformed DN can be seen in trace output */
      char* argv[3];
      argv[0] = "codeExample";
      argv[1] = malloc(2);
      snprintf(argv[1], 2, "%ld", direction);
      argv[2] = inDn;
      CmsiResultT cmsiResult = codeExample(3, (const char**)&argv);
      free(argv[1]);
      free(inDn);

      ei_x_format(&resp, "{ok, ~i}", cmsiResult);
    }
    else if (func == CMSI_TRANSFORM) {

      void *handle = getHandle(clientId, connectionHandles);

      if (clientId != 0 && handle == NULL) {
        ei_x_format(&resp, "{error, ~s, ~i}",
		    "no connection handle for client", clientId);
      }
      else {
        char* outDn = NULL;
        QTRACE4(3,
		"call cmsiTransform(), direction: %s, inDn: %s",
		getDirection(direction), inDn);
        CmsiResultT cmsiResult =
	  cmsiTransform((clientId != 0 ? handle : NULL),
			(CmsiTransformDirectionT)direction,
			inDn,
			&outDn);
        QTRACE4(3,
		"cmsiTransform result: %d, outDn: %s", cmsiResult,
		getString(outDn, "NULL"));

        free(inDn);

        if (cmsiResult == CMSI_OK ||
	    cmsiResult == CMSI_OBJECT_CLASS_NOT_FOUND) {
          ei_x_format(&resp, "{ok, ~i, ~s}", cmsiResult, outDn);
        }
        else {
          ei_x_format(&resp, "{ok, ~i, ~s}", cmsiResult, getResult(cmsiResult));
        }

        if (outDn != NULL) {
          free(outDn);
        }
      }
    }

    else if (func == CMSI_PASS_INVALID_TODNP_A) {
      // Used in negative test where the location pointed to by
      // the 4th argument to cmsiTransform does not contain NULL.

      if (clientId != 0 && getHandle(clientId, connectionHandles) == NULL) {
	ei_x_format(&resp, "{error, ~s, ~i}",
		    "no connection handle for client", clientId);
      }
      else {
	char* outDn = "somethingOtherThanNull";
	char* outDnBeforeCall = outDn;
	QTRACE4(3,
		"call cmsiTransform(), direction: %s, inDn: %s",
		getDirection(direction), inDn);
	CmsiResultT cmsiResult =
	  cmsiTransform((clientId != 0 ?
			 getHandle(clientId, connectionHandles) : NULL),
			(CmsiTransformDirectionT)direction,
			inDn,
			&outDn);
	QTRACE4(3,
		"cmsiTransform result: %d, outDn: %s", cmsiResult,
		getString(outDn, "NULL"));

	free(inDn);

	if (cmsiResult == CMSI_INVALID_PARAMETER_TODNP &&
	    outDn != outDnBeforeCall) {
	  ei_x_format(&resp, "{ok, ~i, ~s}", cmsiResult,
		      "unexpected: outDn was changed");
	}
	else {
	  ei_x_format(&resp, "{ok, ~i, ~s}", cmsiResult, getResult(cmsiResult));
	}
      }
    }

    else if (func == CMSI_PASS_INVALID_TODNP_B) {
      // Used in negative test where the 4th argument to cmsiTransform
      // is NULL.

      if (clientId != 0 && getHandle(clientId, connectionHandles) == NULL) {
	ei_x_format(&resp, "{error, ~s, ~i}",
		    "no connection handle for client", clientId);
      }
      else {

	QTRACE4(3,
		"call cmsiTransform(), direction: %s, inDn: %s, "
		"outDn is NULL (incorrect by intention)",
		getDirection(direction), inDn);
	CmsiResultT cmsiResult =
	  cmsiTransform((clientId != 0 ?
			 getHandle(clientId, connectionHandles) : NULL),
			(CmsiTransformDirectionT)direction,
			inDn,
			NULL);
	QTRACE3(3, "cmsiTransform result: %d", cmsiResult);

	free(inDn);

	ei_x_format(&resp, "{ok, ~i, ~s}", cmsiResult, getResult(cmsiResult));
      }
    }

    else {
      ei_x_format(&resp, "{ok, ~s, ~i}", "function does not exist", func);
    }

  }

  QRETURN1(resp);
}

static void
encode_CmsiAssociationEnd(ei_x_buff* x, const char *atom,
			  struct CmsiAssociationEnd* assEnd)
{
  ei_x_encode_tuple_header(x, 2);
  ei_x_encode_atom(x, atom);
  ei_x_encode_list_header(x, 7);

  ei_x_encode_tuple_header(x, 2);
  ei_x_encode_atom(x, "name");
  ei_x_encode_string(x, assEnd->name);

  ei_x_encode_tuple_header(x, 2);
  ei_x_encode_atom(x, "className");
  ei_x_encode_string(x, assEnd->hasClass.name);

  ei_x_encode_tuple_header(x, 2);
  ei_x_encode_atom(x, "mimName");
  ei_x_encode_string(x, assEnd->hasClass.mimName);

  ei_x_encode_tuple_header(x, 2);
  ei_x_encode_atom(x, "isScoped");
  ei_x_encode_boolean(x, assEnd->hasClass.isScoped);

  ei_x_encode_tuple_header(x, 2);
  ei_x_encode_atom(x, "isReserving");
  ei_x_encode_boolean(x, assEnd->isReserving);

  ei_x_encode_tuple_header(x, 2);
  ei_x_encode_atom(x, "minCardinality");
  if (assEnd->cardinality == NULL || assEnd->cardinality->min == NULL)
    ei_x_encode_atom(x, "undefined");
  else
    ei_x_encode_long(x, assEnd->cardinality->min->value);

  ei_x_encode_tuple_header(x, 2);
  ei_x_encode_atom(x, "maxCardinality");
  if (assEnd->cardinality == NULL || assEnd->cardinality->max == NULL)
    ei_x_encode_atom(x, "undefined");
  else
    ei_x_encode_long(x, assEnd->cardinality->max->value);

  ei_x_encode_empty_list(x);

}
