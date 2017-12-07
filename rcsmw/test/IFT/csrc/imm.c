/* ----------------------------------------------------------------------
 * %CCaseFile:	imm.c %
 * %CCaseRev:	/main/R3A/R5A/R6A/8 %
 * %CCaseDate:	2016-06-20 %
 * %CCaseDocNo: %
 * Author:	erarafo
 * Author: <name>, <e-mail address>
 *
 * Short description: This module is a general purpose bridge between
 * test cases that wish to use IMM OM and IMM OI. The intention is to
 * reduce C coding for test cases to an absolute minimum.
 *
 * Dependencies: IMM itself, and test_common.
 *
 * Test suites known to use this module:
 *   comsa_avc_SUITE
 *
 * ----------------------------------------------------------------------
 * %CCaseTemplateFile:	template.c %
 * %CCaseTemplateId: CCver: /main/1 %
 *
 * %CCaseCopyrightBegin%
 * Copyright (c) Ericsson AB 2015-2016 All rights reserved.
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
 * R3A/1      2015-03-11 erarafo     Work in progress
 * R3A/2      2015-03-23 erarafo     Corrections and refactoring
 * R5A/1      2016-04-17 erarafo     Some OI functions added
 * R6A/1      2016-06-01 erarafo     Corrected dependency info
 * R6A/3      2016-06-10 erarafo     Added function omCcbObjsCreate
 * R6A/4      2016-06-10 erarafo     Corrected error handling
 * R6A/5      2016-06-12 erarafo     Write-out-of-bounds error fixed
 * R6A/6      2016-06-17 erarafo     Added support for CCB object modification
 * R6A/7      2016-06-20 erarafo     Limited support for struct attributes
 * R6A/8      2016-06-20 erarafo     Correction in oiFinalize
 * ----------------------------------------------------------------------
 */

#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif

#include "master.h"

#include <saAis.h>
#include <saImmOi.h>
#include <saImmOm.h>

#include <stdio.h>

#define IMM_OM_RELEASE_CODE 'A'
#define IMM_OM_MAJOR_VERSION 2
#define IMM_OM_MINOR_VERSION 13

#define IMM_OI_RELEASE_CODE 'A'
#define IMM_OI_MAJOR_VERSION 2
#define IMM_OI_MINOR_VERSION 13


#define STREQ(A, B)  (strcmp(A, B) == 0)

#define RETURN0(F)   ei_x_buff(__response); \
                     ei_x_new(&__response); \
                     ei_x_format(&__response, F); \
                     return __response

#define RETURN1(F, X)   ei_x_buff(__response); \
                     ei_x_new(&__response); \
                     ei_x_format(&__response, F, X); \
                     return __response

#define RETURN2(F, X, Y)   ei_x_buff(__response); \
                     ei_x_new(&__response); \
                     ei_x_format(&__response, F, X, Y); \
                     return __response

#define RETURN3(F, X, Y, Z)   ei_x_buff(__response); \
                     ei_x_new(&__response); \
                     ei_x_format(&__response, F, X, Y, Z); \
                     return __response

typedef struct PairS {
  void *voidP;
  struct PairS *next;
} Pair;


static void reclaim(Pair *p) {
  Pair *r;
  for (Pair *q = p; q != NULL; q = r) {
    r = q->next;
    free(q->voidP);
    free((void *)q);
  }
}

static Pair *cons(void *p, Pair *u) {
  Pair *result = (Pair *)calloc(1, sizeof(Pair));
  result->voidP = p;
  result->next = u;
  return result;
}

static void *zalloc(Pair **u, size_t n, size_t size) {
  void *result = calloc(n, size);
  Pair *pair = *u;
  *u = cons(result, pair);
  return result;
}


/**
 * Create a SaNameT for a trusted-length string.
 */
static SaNameT *zname(Pair **u, char *string) {
  SaNameT *result = zalloc(u, 1, sizeof(SaNameT));
  result->length = strlen(string);
  for (int k = 0; k < (result->length); k++) {
    result->value[k] = (SaUint8T)string[k];
  }
  return result;
}


/* static */ char *zNameToString(Pair **u, SaNameT *name) {
  char *result = (char *)zalloc(u, (name->length)+1, sizeof(char));
  for (int j = 0; j < name->length; j++) {
    result[j] = name->value[j];
  }
  return result;
}



static char *zdecodeString(Pair **u, ei_x_buff *req) {

  int type;
  int size;
  ei_get_type(req->buff, &(req->index), &type, &size);

  if (type == ERL_NIL_EXT) {
    ei_skip_term(req->buff, &(req->index));
    return (char *)zalloc(u, 1, sizeof(char));
  }
  else {
    // trust that the type is string
    char *result = (char *)zalloc(u, size+1, sizeof(char));
    ei_decode_string(req->buff, &(req->index), result);
    return result;
  }
}

static char *zdecodePrefixedString(Pair **u, ei_x_buff *req) {
  int type;
  int size;
  ei_get_type(req->buff, &(req->index), &type, &size);
  // trust that the type is string of length >= 1.
  char buffer[size+1];
  ei_decode_string(req->buff, &(req->index), buffer);
  char *result = (char *)zalloc(u, size, sizeof(char));
  memcpy(result, buffer+1, size);
  return result;
}



// Unused and untested ...
//static char *zdecodeAtom(Pair **u, ei_x_buff *req) {
//
//  int ignoreType;
//  int size;
//  ei_get_type(req->buff, &(req->index), &ignoreType, &size);
//
//  // trust that the type is atom
//  char *result = (char *)zalloc(u, size+1, sizeof(char));
//  ei_decode_atom(req->buff, &(req->index), result);
//  return result;
//}

static int getType(ei_x_buff *req) {
  int type;
  int ignoredSize;
  int r = ei_get_type((const char *)req->buff, &(req->index), &type, &ignoredSize);
  if (r < 0) {
    return r;
  }
  else {
    return type;
  }
}

static int getSize(ei_x_buff *req) {
  int ignoredType;
  int size;
  const int r = ei_get_type((const char *)req->buff, &(req->index), &ignoredType, &size);
  return r < 0 ? r : size;
}

static int zdecodeInt(Pair **u, ei_x_buff *request) {
  int binarySize = getSize(request);
  char *decimal = (char *)zalloc(u, binarySize+1, sizeof(char));
  long ignoreLong;
  ei_decode_binary(request->buff, &(request->index), decimal, &ignoreLong);
  int result = (int)strtol(decimal, NULL, 10);
  return result;
}


/**
 * Encode an int as a decimal string.
 */
static char *zencodeInt32(Pair **u, int k) {
  char *r = NULL;
  asprintf(&r, "%d", k);
  Pair *pair = *u;
  *u = cons(r, pair);
  return r;
}


static int decodeTupleHeader(ei_x_buff *req) {
  int arity;
  const int r = ei_decode_tuple_header((const char *)req->buff, &(req->index), &arity);
  return r < 0 ? r : arity;
}


static long decodeLong(ei_x_buff *req) {
  long result;
  ei_decode_long(req->buff, &(req->index), &result);
  return result;
}


static SaImmAttrValuesT_2 **zdecodeAttrBindings(Pair **, ei_x_buff *);

/**
 * Decode a null-terminated array of struct members.
 */
static SaImmAttrValuesT_2 **zdecodeStructMembers(Pair **u, ei_x_buff *request) {
  return zdecodeAttrBindings(u, request);
}


/**
 * Decode a struct value.
 */
static SaImmCsStructT *zdecodeStruct(Pair **u, ei_x_buff *request) {
  decodeTupleHeader(request);    // trust arity 2
  SaImmCsStructT *result = (SaImmCsStructT *)zalloc(u, 1, sizeof(SaImmCsStructT));
  result->structName = zdecodeString(u, request);
  result->structMembers = zdecodeStructMembers(u, request);
  return result;
}


/**
 * Decodes a single attribute-bindings structure.
 */
static SaImmAttrValuesT_2 *zdecodeAttrBinding(Pair **u, ei_x_buff *request) {
  decodeTupleHeader(request); // trust 3 always

  SaImmAttrValuesT_2 *result = zalloc(u, 1, sizeof(SaImmAttrValuesT_2));
  result->attrName = zdecodeString(u, request);
  result->attrValueType = (int)decodeLong(request);

  result->attrValuesNumber = decodeTupleHeader(request);
  if (result->attrValuesNumber == 0) {
    result->attrValues = NULL;
  }
  else {
    void **voidPointers = zalloc(u, result->attrValuesNumber, sizeof(void *));
    result->attrValues = voidPointers;
    for (int j = 0; j < result->attrValuesNumber; j++) {
      if (result->attrValueType == SA_IMM_ATTR_SAINT32T) {
        SaInt32T *m = zalloc(u, 1, sizeof(SaInt32T));
        *m = zdecodeInt(u, request);
        voidPointers[j] = (void *)m;
      }
      else if (result->attrValueType == SA_IMM_ATTR_SASTRINGT) {
        char **sp = zalloc(u, 1, sizeof(char *));
        *sp = zdecodePrefixedString(u, request);
        voidPointers[j] = (void *)sp;
      }
      else if (result->attrValueType == SA_IMM_ATTR_SANAMET) {
        char *nameS = zdecodePrefixedString(u, request);
        SaNameT *nameP = zname(u, nameS);
        voidPointers[j] = (void *)nameP;
      }
      else if (result->attrValueType  == SA_IMM_ATTR_CSSTRUCTT) {
        voidPointers[j] = (void *)zdecodeStruct(u, request);
      }
      else {
        // must not happen; it is trusted that only value types
        // encoded in imm.erl can appear
        return NULL;
      }
    }
  }
  return result;
}


/**
 * Decodes a tuple representing a number of attribute bindings.
 */
static SaImmAttrValuesT_2 **zdecodeAttrBindings(Pair **u, ei_x_buff *request) {
  const int nAttrs = decodeTupleHeader(request);
  SaImmAttrValuesT_2 **result = zalloc(u, nAttrs+1, sizeof(SaImmAttrValuesT_2 *));
  for (int k = 0; k < nAttrs; k++) {
    result[k] = zdecodeAttrBinding(u, request);
  }
  return result;
}


static SaImmAttrModificationT_2 *zdecodeAttrMod(Pair **u, ei_x_buff *request) {
  decodeTupleHeader(request);  // trust size 2
  SaImmAttrModificationT_2 *result = zalloc(u, 1, sizeof(SaImmAttrModificationT_2));
  result->modType = (int)decodeLong(request);
  SaImmAttrValuesT_2 *attrBinding = zdecodeAttrBinding(u, request);
  // consider struct assignment here
  (result->modAttr).attrName = attrBinding->attrName;
  (result->modAttr).attrValueType = attrBinding->attrValueType;
  (result->modAttr).attrValuesNumber = attrBinding->attrValuesNumber;
  (result->modAttr).attrValues = attrBinding->attrValues;
  return result;
}


static SaImmAttrModificationT_2 **zdecodeAttrMods(Pair **u, ei_x_buff *request) {
  const int nMods = decodeTupleHeader(request);
  SaImmAttrModificationT_2 **result = zalloc(u, nMods+1, sizeof(SaImmAttrModificationT_2 *));
  for (int k = 0; k < nMods; k++) {
    result[k] = zdecodeAttrMod(u, request);
  }
  return result;
}


//        /* 4.2.3 SaImmValueTypeT */
//        typedef enum {
//                SA_IMM_ATTR_SAINT32T = 1,       /* SaInt32T */
//                SA_IMM_ATTR_SAUINT32T = 2,      /* SaUint32T */
//                SA_IMM_ATTR_SAINT64T = 3,       /* SaInt64T */
//                SA_IMM_ATTR_SAUINT64T = 4,      /* SaUint64T */
//                SA_IMM_ATTR_SATIMET = 5,        /* SaTimeT */
//                SA_IMM_ATTR_SANAMET = 6,        /* SaNameT */
//                SA_IMM_ATTR_SAFLOATT = 7,       /* SaFloatT */
//                SA_IMM_ATTR_SADOUBLET = 8,      /* SaDoubleT */
//                SA_IMM_ATTR_SASTRINGT = 9,      /* SaStringT */
//                SA_IMM_ATTR_SAANYT = 10 /* SaAnyT */
//        } SaImmValueTypeT;
//
// The integer types are self-explanatory.
//
// SATIMET is time in nanoseconds, represented as a SaInt64T
// SANAMET is a struct of fixed size:
//
// typedef struct {
//    SaUint16T length;
//    SaUint8T value[SA_MAX_NAME_LENGTH];
// } SaNameT;
//
// SAANYT is a struct that wraps a size and a buffer pointer,
// where SaSizeT is a SaUint64T.
// typedef struct {
//   SaSizeT   bufferSize;
//   SaUint8T  *bufferAddr;
// } SaAnyT;

//static int printedSize(SaUint64T handle) {
//  if (handle < 10) {
//    return 1;
//  }
//  else {
//    return 1 + printedSize(handle/10);
//  }
//}

static char *errorAtom(SaAisErrorT r) {

  switch (r) {
  case SA_AIS_ERR_LIBRARY:
    return "SA_AIS_ERR_LIBRARY";
  case SA_AIS_ERR_VERSION:
    return "SA_AIS_ERR_VERSION";
  case SA_AIS_ERR_INIT:
    return "SA_AIS_ERR_INIT";
  case SA_AIS_ERR_TIMEOUT:
    return "SA_AIS_ERR_TIMEOUT";
  case SA_AIS_ERR_TRY_AGAIN:
    return "SA_AIS_ERR_TRY_AGAIN";
  case SA_AIS_ERR_INVALID_PARAM:
    return "SA_AIS_ERR_INVALID_PARAM";
  case SA_AIS_ERR_NO_MEMORY:
    return "SA_AIS_ERR_NO_MEMORY";
  case SA_AIS_ERR_BAD_HANDLE:
    return "SA_AIS_ERR_BAD_HANDLE";
  case SA_AIS_ERR_BUSY:
    return "SA_AIS_ERR_BUSY";
  case SA_AIS_ERR_ACCESS:
    return "SA_AIS_ERR_ACCESS";
  case SA_AIS_ERR_NOT_EXIST:
    return "SA_AIS_ERR_NOT_EXIST";
  case SA_AIS_ERR_NAME_TOO_LONG:
    return "SA_AIS_ERR_NAME_TOO_LONG";
  case SA_AIS_ERR_EXIST:
    return "SA_AIS_ERR_EXIST";
  case SA_AIS_ERR_NO_SPACE:
    return "SA_AIS_ERR_NO_SPACE";
  case SA_AIS_ERR_INTERRUPT:
    return "SA_AIS_ERR_INTERRUPT";
  case SA_AIS_ERR_NAME_NOT_FOUND:
    return "SA_AIS_ERR_NAME_NOT_FOUND";
  case SA_AIS_ERR_NO_RESOURCES:
    return "SA_AIS_ERR_NO_RESOURCES";
  case SA_AIS_ERR_NOT_SUPPORTED:
    return "SA_AIS_ERR_NOT_SUPPORTED";
  case SA_AIS_ERR_BAD_OPERATION:
    return "SA_AIS_ERR_BAD_OPERATION";
  case SA_AIS_ERR_FAILED_OPERATION:
    return "SA_AIS_ERR_FAILED_OPERATION";
  case SA_AIS_ERR_MESSAGE_ERROR:
    return "SA_AIS_ERR_MESSAGE_ERROR";
  case SA_AIS_ERR_QUEUE_FULL:
    return "SA_AIS_ERR_QUEUE_FULL";
  case SA_AIS_ERR_QUEUE_NOT_AVAILABLE:
    return "SA_AIS_ERR_QUEUE_NOT_AVAILABLE";
  case SA_AIS_ERR_BAD_FLAGS:
    return "SA_AIS_ERR_BAD_FLAGS";
  case SA_AIS_ERR_TOO_BIG:
    return "SA_AIS_ERR_TOO_BIG";
  case SA_AIS_ERR_NO_SECTIONS:
    return "SA_AIS_ERR_NO_SECTIONS";
  case SA_AIS_ERR_NO_OP:
    return "SA_AIS_ERR_NO_OP";
  case SA_AIS_ERR_REPAIR_PENDING:
    return "SA_AIS_ERR_REPAIR_PENDING";
  case SA_AIS_ERR_NO_BINDINGS:
    return "SA_AIS_ERR_NO_BINDINGS";
  case SA_AIS_ERR_UNAVAILABLE:
    return "SA_AIS_ERR_UNAVAILABLE";
  case SA_AIS_ERR_CAMPAIGN_ERROR_DETECTED:
    return "SA_AIS_ERR_CAMPAIGN_ERROR_DETECTED";
  case SA_AIS_ERR_CAMPAIGN_PROC_FAILED:
    return "SA_AIS_ERR_CAMPAIGN_PROC_FAILED";
  case SA_AIS_ERR_CAMPAIGN_CANCELED:
    return "SA_AIS_ERR_CAMPAIGN_CANCELED";
  case SA_AIS_ERR_CAMPAIGN_FAILED:
    return "SA_AIS_ERR_CAMPAIGN_FAILED";
  case SA_AIS_ERR_CAMPAIGN_SUSPENDED:
    return "SA_AIS_ERR_CAMPAIGN_SUSPENDED";
  case SA_AIS_ERR_CAMPAIGN_SUSPENDING:
    return "SA_AIS_ERR_CAMPAIGN_SUSPENDING";
  case SA_AIS_ERR_ACCESS_DENIED:
    return "SA_AIS_ERR_ACCESS_DENIED";
  case SA_AIS_ERR_NOT_READY:
    return "SA_AIS_ERR_NOT_READY";
  case SA_AIS_ERR_DEPLOYMENT:
    return "SA_AIS_ERR_DEPLOYMENT";
  case SA_AIS_OK:
    return "SA_AIS_OK";                    // not expected to happen
  default:
    return "errorAtom: parameter out of range";   // not expected to happen
  }
}


/**
 * Encode a handle as a string.
 */
static char *encodeHandle(Pair **u, SaUint64T handle) {
  char *result;
  asprintf(&result, "%llu", handle);
  Pair *z = cons((void *)result, *u);
  *u = z;
  return result;
}


/**
 * Decode a handle. It is trusted that a handle is represented
 * as a binarized decimal string.
 */
static SaUint64T decodeU64Handle(ei_x_buff *req) {
  int ignoreInt;
  long ignoreLong;
  int size;
  ei_get_type(req->buff, &(req->index), &ignoreInt, &size);
  char handleB[size + 1];
  ei_decode_binary(req->buff, &(req->index), handleB, &ignoreLong);
  handleB[size] = 0;
  return (SaUint64T)strtoull(handleB, NULL, 10);
}

static ei_x_buff omInitialize(int arity, ei_x_buff request) {
  ei_x_buff response;
  ei_x_new(&response);
  if (arity != 0) {
    ei_x_format(&response, "{error, {bad_arity, ~i}}", arity);
    return response;
  }
  SaVersionT version = {
      .releaseCode = IMM_OM_RELEASE_CODE,
      .majorVersion = IMM_OM_MAJOR_VERSION,
      .minorVersion = IMM_OM_MINOR_VERSION
  };
  SaImmHandleT immHandle;
  const SaImmCallbacksT *callbacks = NULL;

  SaAisErrorT r = saImmOmInitialize(&immHandle, callbacks, &version);
  if (r != SA_AIS_OK) {
    ei_x_format(&response, "{error, ~a}", errorAtom(r));
    return response;
  }
  else {

    FILE *f = fopen("/tmp/imm-h.txt", "a"); fprintf(f, "handle: %llu\n", immHandle); fclose(f);

    char *immHandleS;
    asprintf(&immHandleS, "%llu", immHandle);
    ei_x_format(&response, "{ok, {~s, {~i, ~i, ~i}}}",
        immHandleS,
        (int)version.releaseCode,
        (int)version.majorVersion,
        (int)version.minorVersion);
    free(immHandleS);
    return response;
  }
}





static ei_x_buff omFinalize(int arity, ei_x_buff request) {
  ei_x_buff response;
  ei_x_new(&response);
  if (arity != 1) {
    ei_x_format(&response, "{error, {bad_arity, ~i}}", arity);
    return response;
  }
  SaImmHandleT handle = decodeU64Handle(&request);
  SaAisErrorT r = saImmOmFinalize(handle);
  if (r != SA_AIS_OK) {
    ei_x_format(&response, "{error, ~a}", errorAtom(r));
    return response;
  }
  else {
    ei_x_format(&response, "{ok}");
    return response;
  }
}


/**
 * Verify that the request has the atom 'end'. NOTE: The caller
 * index is not affected. Used by the testInteger() function.
 */
static bool verifyEnd(ei_x_buff request) {
  int type;
  int size;
  ei_get_type((const char *)request.buff, &(request.index), &type, &size);
  if (type != ERL_ATOM_EXT || size != 3) {
    return false;
  }
  else {
    char atom[3 + 1];
    ei_decode_atom(request.buff, &(request.index), atom);
    return strcmp(atom, "end") == 0;
  }
}

/**
 * Test passing of integers downstream. This is not for use by testcases.
 */
static ei_x_buff testInteger(int nofArgs, ei_x_buff request) {
  ei_x_buff response;
  ei_x_new(&response);
  int type;
  int size;

  if (nofArgs != 2) {
    ei_x_format(&response, "{error, {arg_count, ~i}}", nofArgs);
    return response;
  }

  ei_get_type((const char *)request.buff, &(request.index), &type, &size);
  if (type == ERL_SMALL_INTEGER_EXT) {
    long value;
    if (ei_decode_long(request.buff, &(request.index), &value) == -1) {
      ei_x_format(&response, "{error, ei_decode_long_1}");
      return response;
    }
    else if (!verifyEnd(request)) {
      ei_x_format(&response, "{error, alignment_1}");
      return response;
    }
    else {
      ei_x_format(&response, "{ok, {~i, ~i}}", type, value);
      return response;
    }

  }
  else if (type == ERL_INTEGER_EXT) {
    long value;
    if (ei_decode_long(request.buff, &(request.index), &value) == -1) {
      ei_x_format(&response, "{error, ei_decode_long_2}");
      return response;
    }
    else if (!verifyEnd(request)) {
      ei_x_format(&response, "{error, alignment_2}");
      return response;
    }
    else {
      ei_x_format(&response, "{ok, {~i, ~i}}", type, value);
      return response;
    }
  }
  else if (type == ERL_SMALL_BIG_EXT) {
    long long value;
    if (ei_decode_longlong(request.buff, &(request.index), &value) == -1) {
      ei_x_format(&response, "{error, ei_decode_longlong}");
      return response;
    }
    else if (!verifyEnd(request)) {
      ei_x_format(&response, "{error, alignment_3}");
      return response;
    }
    else {
      char decimal[50];
      for (int k = 0; k < 50; k++)
        decimal[k] = 0;
      //sprintf(decimal, "%lld", value);
      strcpy(decimal, "349853459875");

      long long z = 349853459875LL;
      ei_x_format(&response, "{ok, {~i, ~s, ~l}}", type,
      //value,
          decimal, z);
      return response;
    }
  }
  else {
    ei_x_format(&response, "{error, {unknown_type, ~i}}", type);
    return response;
  }
}


static ei_x_buff omAoInitialize(int arity, ei_x_buff request) {

  Pair *alloc = NULL;

  ei_x_buff response;
  ei_x_new(&response);

  if (arity != 3) {
    ei_x_format(&response, "{error, {bad_arity, ~i}}", arity);
    return response;
  }

  SaImmHandleT immHandle = (SaImmHandleT)decodeU64Handle(&request);
  const SaImmAdminOwnerNameT aoName = zdecodeString(&alloc, &request);
  SaBoolT releaseOwnershipOnFinalize = (SaBoolT)decodeLong(&request);
  SaImmAdminOwnerHandleT aoHandle;
  SaAisErrorT r = saImmOmAdminOwnerInitialize(immHandle, aoName, releaseOwnershipOnFinalize, &aoHandle);
  if (r != SA_AIS_OK) {
    ei_x_format(&response, "{error, ~a}", errorAtom(r));
    reclaim(alloc);
    return response;
  }
  else {
    ei_x_format(&response, "{ok, ~s}",
        encodeHandle(&alloc, (SaUint64T)aoHandle));
    reclaim(alloc);
    return response;
  }
}

static ei_x_buff omAoFinalize(int arity, ei_x_buff request) {
  ei_x_buff response;
  ei_x_new(&response);
  if (arity != 1) {
    ei_x_format(&response, "{error, {bad_arity, ~i}}", arity);
    return response;
  }
  SaImmHandleT aoHandle = decodeU64Handle(&request);
  SaAisErrorT r = saImmOmAdminOwnerFinalize(aoHandle);
  if (r != SA_AIS_OK) {
    ei_x_format(&response, "{error, ~a}", errorAtom(r));
    return response;
  }
  else {
    ei_x_format(&response, "{ok}");
    return response;
  }
}

/**
 * Set administrative ownership on the given instances.
 * Arguments are:
 *   binarized decimal   aoHandle
 *   tuple of string     objectNames
 *   long                scope
 */
static ei_x_buff omAoSet(int arity, ei_x_buff request) {
  Pair *alloc = NULL;
  ei_x_buff response;
  ei_x_new(&response);

  if (arity != 3) {
    ei_x_format(&response, "{error, {bad_arity, ~i}}", arity);
    return response;
  }

  SaImmAdminOwnerHandleT aoHandle = (SaImmAdminOwnerHandleT)decodeU64Handle(&request);

  int length;
  ei_decode_tuple_header(request.buff, &(request.index), &length);
  SaNameT **objectNames =
      (SaNameT **)zalloc(&alloc, length+1, sizeof(SaNameT *));
  for (int k = 0; k < length; k++) {
    char *name = zdecodeString(&alloc, &request);
    objectNames[k] = zname(&alloc, name);
  }

  SaImmScopeT scope = (SaImmScopeT)decodeLong(&request);
  if (scope != SA_IMM_ONE && scope != SA_IMM_SUBLEVEL && scope != SA_IMM_SUBTREE) {
    ei_x_format(&response, "{error, {bad_scope, ~i}}", scope);
    reclaim(alloc);
    return response;
  }

  SaAisErrorT r = saImmOmAdminOwnerSet(aoHandle, (const SaNameT **)objectNames, scope);
  if (r != SA_AIS_OK) {
    ei_x_format(&response, "{error, ~a}", errorAtom(r));
    reclaim(alloc);
    return response;
  }
  else {
    ei_x_format(&response, "{ok}");
    reclaim(alloc);
    return response;
  }
}


static ei_x_buff omAoRelease(int arity, ei_x_buff request) {
  Pair *alloc = NULL;
  ei_x_buff response;
  ei_x_new(&response);

  if (arity != 3) {
    ei_x_format(&response, "{error, {bad_arity, ~i}}", arity);
    return response;
  }

  SaImmAdminOwnerHandleT aoHandle = (SaImmAdminOwnerHandleT)decodeU64Handle(&request);

  int length;
  ei_decode_tuple_header(request.buff, &(request.index), &length);

  SaNameT **objectNames =
      (SaNameT **)zalloc(&alloc, length+1, sizeof(SaNameT *));

  for (int k = 0; k < length; k++) {
    char *name = zdecodeString(&alloc, &request);
    objectNames[k] = zname(&alloc, name);
  }

  SaImmScopeT scope = (SaImmScopeT)decodeLong(&request);
  if (scope != SA_IMM_ONE && scope != SA_IMM_SUBLEVEL && scope != SA_IMM_SUBTREE) {
    ei_x_format(&response, "{error, {bad_scope, ~i}}", scope);
    reclaim(alloc);
    return response;
  }

  SaAisErrorT r = saImmOmAdminOwnerRelease(aoHandle, (const SaNameT **)objectNames, scope);
  if (r != SA_AIS_OK) {
    ei_x_format(&response, "{error, ~a}", errorAtom(r));
    reclaim(alloc);
    return response;
  }
  else {
    ei_x_format(&response, "{ok}");
    reclaim(alloc);
    return response;
  }
}


static ei_x_buff omCcbInitialize(int arity, ei_x_buff request) {

  Pair *alloc = NULL;
  ei_x_buff response;
  ei_x_new(&response);

  if (arity != 2) {
    ei_x_format(&response, "{error, {bad_arity, ~i}}", arity);
    return response;
  }
  SaImmAdminOwnerHandleT aoHandle = (SaImmAdminOwnerHandleT)decodeU64Handle(&request);
  const SaImmCcbFlagsT ccbFlags = decodeLong(&request);
  SaImmCcbHandleT ccbHandle;
  SaAisErrorT r = saImmOmCcbInitialize(aoHandle, ccbFlags, &ccbHandle);
  if (r != SA_AIS_OK) {
    ei_x_format(&response, "{error, ~a}", errorAtom(r));
    reclaim(alloc);
    return response;
  }
  else {
    char *result = encodeHandle(&alloc, ccbHandle);
    ei_x_format(&response, "{ok, ~s}", result);
    reclaim(alloc);
    return response;
  }
}


//static ei_x_buff omCcbFinalize(int arity, ei_x_buff request) {
//  ei_x_buff response;
//  ei_x_new(&response);
//  if (arity != 1) {
//    ei_x_format(&response, "{error, {bad_arity, ~i}}", arity);
//    return response;
//  }
//
//  int type;
//  int size;
//  long ignoreLong;
//  ei_get_type((const char *)request.buff, &(request.index), &type, &size);
//  if (type != ERL_BINARY_EXT) {
//    ei_x_format(&response, "{error, ~a}", "bad_handle");
//    return response;
//  }
//  else {
//    char string[size + 1];
//    ei_decode_binary(request.buff, &(request.index), string, &ignoreLong);
//    string[size] = '\0';
//    SaImmHandleT handle = strtoull(string, NULL, 10);
//    SaAisErrorT r = saImmOmCcbFinalize(handle);
//    if (r != SA_AIS_OK) {
//      ei_x_format(&response, "{error, ~a}", errorAtom(r));
//      return response;
//    }
//    else {
//      ei_x_format(&response, "{ok}");
//      return response;
//    }
//  }
//}

static ei_x_buff omCcbFinalize(int arity, ei_x_buff request) {
  ei_x_buff response;
  ei_x_new(&response);
  if (arity != 1) {
    ei_x_format(&response, "{error, {bad_arity, ~i}}", arity);
    return response;
  }
  SaImmHandleT handle = decodeU64Handle(&request);
  SaAisErrorT r = saImmOmCcbFinalize(handle);
  if (r != SA_AIS_OK) {
    ei_x_format(&response, "{error, ~a}", errorAtom(r));
    return response;
  }
  else {
    ei_x_format(&response, "{ok}");
    return response;
  }
}


static ei_x_buff omCcbObjDelete(int arity, ei_x_buff request) {
  Pair *alloc = NULL;

  if (arity != 2) {
    RETURN1("{error, {bad_arity, ~i}}", arity);
  }

  SaImmCcbHandleT ccbHandle = (SaImmCcbHandleT)decodeU64Handle(&request);

  char *nameS = zdecodeString(&alloc, &request);
  SaNameT *name = zname(&alloc, nameS);

  SaAisErrorT r = saImmOmCcbObjectDelete(ccbHandle, name);
  if (r != SA_AIS_OK) {
    reclaim(alloc);
    RETURN1("{error, ~a}", errorAtom(r));
  }
  else {
    reclaim(alloc);
    RETURN0("{ok}");
  }
}


/**
 * Create an instance in the given CCB.
 * Arguments are:
 *   binarized decimal  ccbHandle
 *   string             className
 *   string             parentNameS
 *   tuple              attrBindingsList
 */
static ei_x_buff omCcbObjCreate(int arity, ei_x_buff request) {
  Pair *alloc = NULL;
  if (arity != 4) {
    RETURN1("{error, {bad_arity, ~i}}", arity);
  }
  const SaImmHandleT ccbHandle = decodeU64Handle(&request);
  char *className = zdecodeString(&alloc, &request);
  char *parentNameS = zdecodeString(&alloc, &request);
  SaNameT *parentName = zname(&alloc, parentNameS);
  SaImmAttrValuesT_2 **attrBindingsList = zdecodeAttrBindings(&alloc, &request);
  SaAisErrorT r =
      saImmOmCcbObjectCreate_2(
          ccbHandle,
          className,
          parentName,
          (const SaImmAttrValuesT_2 **)attrBindingsList);
  if (r != SA_AIS_OK) {
    reclaim(alloc);
    RETURN1("{error, ~a}", errorAtom(r));
  }
  else {
    reclaim(alloc);
    RETURN0("{ok}");
  }
}


/**
 * Create instances in the given CCB.
 * Arguments are:
 *   binarized decimal  ccbHandle
 *   tuple              instanceDescriptors
 *
 * Each instance descriptor is a classname+parentname+attrbindingslist
 * triple; see the omCcbObjCreate function for details.
 */
static ei_x_buff omCcbObjsCreate(int arity, ei_x_buff request) {
  Pair *alloc = NULL;
  if (arity != 2) {
    RETURN1("{error, {bad_arity, ~i}}", arity);
  }

  const SaImmHandleT ccbHandle = decodeU64Handle(&request);

  const int nInsts = decodeTupleHeader(&request);

  for (int k = 0; k < nInsts; k++) {
    const int tupleSize3 = decodeTupleHeader(&request);
    if (tupleSize3 != 3) {
      RETURN1("{error, {bad_tuple_size, ~i}}", tupleSize3);
    }
    char *className = zdecodeString(&alloc, &request);
    char *parentNameS = zdecodeString(&alloc, &request);
    SaNameT *parentName = zname(&alloc, parentNameS);
    SaImmAttrValuesT_2 **attrBindingsList = zdecodeAttrBindings(&alloc, &request);
    SaAisErrorT r =
        saImmOmCcbObjectCreate_2(
            ccbHandle,
            className,
            parentName,
            (const SaImmAttrValuesT_2 **)attrBindingsList);
    if (r != SA_AIS_OK) {
      char parentNameCopy[strlen(parentNameS)+1];
      char classNameCopy[strlen(className)+1];
      strcpy(parentNameCopy, parentNameS);
      strcpy(classNameCopy, className);
      reclaim(alloc);
      RETURN3("{error, {~a, ~a, ~a}}", parentNameCopy, classNameCopy, errorAtom(r));
    }
  }
  reclaim(alloc);
  RETURN0("{ok}");
}


static ei_x_buff omCcbObjModify(int arity, ei_x_buff request) {
  if (arity != 3) {
    RETURN1("{error, {bad_arity, ~i}}", arity);
  }
  SaImmHandleT ccbHandle = (SaImmHandleT)decodeU64Handle(&request);
  Pair *alloc = NULL;
  char *nameS = zdecodeString(&alloc, &request);
  SaNameT *name = zname(&alloc, nameS);
  SaImmAttrModificationT_2 **mods = zdecodeAttrMods(&alloc, &request);
  SaAisErrorT r =
      saImmOmCcbObjectModify_2(
          ccbHandle,
          name,
          (const SaImmAttrModificationT_2 **)mods);
  if (r != SA_AIS_OK) {
    reclaim(alloc);
    RETURN1("{error, ~a}", errorAtom(r));
  }
  else {
    reclaim(alloc);
    RETURN0("{ok}");
  }
}



static ei_x_buff omCcbApply(int arity, ei_x_buff request) {
  if (arity != 1) {
    RETURN1("{error, {bad_arity, ~i}}", arity);
  }
  SaImmHandleT ccbHandle = (SaImmHandleT)decodeU64Handle(&request);
  SaAisErrorT r = saImmOmCcbApply(ccbHandle);
  if (r != SA_AIS_OK) {
    RETURN1("{error, ~a}", errorAtom(r));
  }
  else {
    RETURN0("{ok}");
  }
}


static ei_x_buff omAccessorInitialize(int arity, ei_x_buff request) {
  if (arity != 1) {
    RETURN1("{error, {bad_arity, ~i}}", arity);
  }
  SaImmHandleT immHandle =
      (SaImmHandleT)decodeU64Handle(&request);
  SaImmAccessorHandleT accHandle;
  SaAisErrorT r = saImmOmAccessorInitialize(immHandle, &accHandle);
  ei_x_buff response;
  ei_x_new(&response);
  if (r != SA_AIS_OK) {
    ei_x_format(&response, "{error, ~a}", errorAtom(r));
    return response;
  }
  else {
    Pair *alloc = NULL;
    ei_x_format(&response, "{ok, ~s}",
        encodeHandle(&alloc, (SaUint64T)accHandle));
    reclaim(alloc);
    return response;
  }
}


static ei_x_buff omAccessorFinalize(int arity, ei_x_buff request) {
  if (arity != 1) {
    RETURN1("{error, {bad_arity, ~i}}", arity);
  }
  SaImmAccessorHandleT accHandle =
      (SaImmAccessorHandleT)decodeU64Handle(&request);
  SaAisErrorT r = saImmOmAccessorFinalize(accHandle);
  ei_x_buff response;
  ei_x_new(&response);
  if (r != SA_AIS_OK) {
    ei_x_format(&response, "{error, ~a}", errorAtom(r));
    return response;
  }
  else {
    ei_x_format(&response, "{ok}");
    return response;
  }
}


static ei_x_buff omAccessorGet(int arity, ei_x_buff request) {
  if (arity != 3) {
    RETURN1("{error, {bad_arity, ~i}}", arity);
  }
  SaImmAccessorHandleT accHandle =
      (SaImmAccessorHandleT)decodeU64Handle(&request);
  Pair *alloc = NULL;
  char *nameS = zdecodeString(&alloc, &request);
  SaNameT *name = zname(&alloc, nameS);

  const int nAttrNames = decodeTupleHeader(&request);
  SaImmAttrNameT *attrNames =
      (SaImmAttrNameT *)zalloc(&alloc, nAttrNames+1, sizeof(SaImmAttrNameT));
  for (int k = 0; k < nAttrNames; k++) {
    attrNames[k] = zdecodeString(&alloc, &request);
  }

  SaImmAttrValuesT_2 **result;
  SaAisErrorT r = saImmOmAccessorGet_2(accHandle, name, attrNames, &result);
  if (r != SA_AIS_OK) {
    reclaim(alloc);
    RETURN1("{error, ~a}", errorAtom(r));
  }
  else {
    int nResultElements = 0;
    for (int k = 0; result[k] != NULL; k++) {
      nResultElements++;
    }
    if (nResultElements != nAttrNames) {
      reclaim(alloc);
      RETURN2("{error, {unexpected_number_of_results, ~i, ~i}}",
          nAttrNames,
          nResultElements);
    }
    ei_x_buff response;
    ei_x_new_with_version(&response);

    ei_x_encode_tuple_header(&response, 2);
    ei_x_encode_atom(&response, "ok");            // {ok,

    ei_x_encode_tuple_header(&response, nResultElements);

    for (int k = 0; k < nResultElements; k++) {
      ei_x_encode_tuple_header(&response, 4);
      ei_x_encode_atom(&response, "attrValues");    // #attrValues{
      ei_x_encode_string(&response, result[k]->attrName);

      // TODO, consider a function to map numeric type to atom
      if (result[k]->attrValueType == SA_IMM_ATTR_SAINT32T) {
        // TODO, replace hardcoded string by macro
        ei_x_encode_atom(&response, "int32");
      }
      else {
        // TODO, handle all data types, including struct
        // TODO, replace hardcoded string by macro
        ei_x_encode_atom(&response, "string");
      }

      ei_x_encode_tuple_header(&response, result[k]->attrValuesNumber);
      for (int m = 0; m < result[k]->attrValuesNumber; m++) {
        if (result[k]->attrValueType == SA_IMM_ATTR_SAINT32T) {
          char *encodedInt =
              zencodeInt32(&alloc, *((SaInt32T *)result[k]->attrValues[m]));
          ei_x_encode_binary(&response, (void *)encodedInt, strlen(encodedInt));
        }
        else {
          // TODO, handle all types here
          ei_x_encode_string(&response, "unknownType");
        }
      }
    }
    reclaim(alloc);
    return response;
  }
}


static ei_x_buff oiInitialize(int arity, ei_x_buff request) {
  ei_x_buff response;
  ei_x_new(&response);
  if (arity != 0) {
    ei_x_format(&response, "{error, {bad_arity, ~i}}", arity);
    return response;
  }
  SaVersionT version = {
      .releaseCode = IMM_OI_RELEASE_CODE,
      .majorVersion = IMM_OI_MAJOR_VERSION,
      .minorVersion = IMM_OI_MINOR_VERSION
  };
  SaImmOiHandleT oiHandle;
  const SaImmOiCallbacksT_2 *callbacks = NULL;

  SaAisErrorT r = saImmOiInitialize_2(&oiHandle, callbacks, &version);
  if (r != SA_AIS_OK) {
    ei_x_format(&response, "{error, ~a}", errorAtom(r));
    return response;
  }
  else {
    char *oiHandleS;
    asprintf(&oiHandleS, "%llu", oiHandle);
    ei_x_format(&response, "{ok, {~s, {~i, ~i, ~i}}}",
        oiHandleS,
        (int)version.releaseCode,
        (int)version.majorVersion,
        (int)version.minorVersion);
    free(oiHandleS);
    return response;
  }
}



static ei_x_buff oiFinalize(int arity, ei_x_buff request) {
  ei_x_buff response;
  ei_x_new(&response);
  if (arity != 1) {
    ei_x_format(&response, "{error, {bad_arity, ~i}}", arity);
    return response;
  }
  SaImmOiHandleT handle = (SaImmOiHandleT)decodeU64Handle(&request);
  SaAisErrorT r = saImmOiFinalize(handle);
  if (r != SA_AIS_OK) {
    ei_x_format(&response, "{error, ~a}", errorAtom(r));
    return response;
  }
  else {
    ei_x_format(&response, "{ok}");
    return response;
  }
}


static ei_x_buff oiImplementerSet(int arity, ei_x_buff request) {
  ei_x_buff response;
  ei_x_new(&response);
  if (arity != 2) {
    ei_x_format(&response, "{error, {bad_arity, ~i}}", arity);
    return response;
  }
  SaImmHandleT handle = decodeU64Handle(&request);
  Pair *alloc = NULL;
  SaImmOiImplementerNameT iname = (SaImmOiImplementerNameT)zdecodeString(&alloc, &request);

  SaAisErrorT r = saImmOiImplementerSet(handle, iname);
  reclaim(alloc);
  if (r != SA_AIS_OK) {
    ei_x_format(&response, "{error, ~a}", errorAtom(r));
    return response;
  }
  else {
    ei_x_format(&response, "{ok}");
    return response;
  }
}


static ei_x_buff oiImplementerClear(int arity, ei_x_buff request) {
  ei_x_buff response;
  ei_x_new(&response);
  if (arity != 1) {
    ei_x_format(&response, "{error, {bad_arity, ~i}}", arity);
    return response;
  }
  SaImmHandleT handle = decodeU64Handle(&request);
  SaAisErrorT r = saImmOiImplementerClear(handle);
  if (r != SA_AIS_OK) {
    ei_x_format(&response, "{error, ~a}", errorAtom(r));
    return response;
  }
  else {
    ei_x_format(&response, "{ok}");
    return response;
  }
}


static ei_x_buff oiRtObjectCreate(int arity, ei_x_buff request) {
  Pair *alloc = NULL;
  if (arity != 4) {
    RETURN1("{error, {bad_arity, ~i}}", arity);
  }
  const SaImmHandleT handle = decodeU64Handle(&request);

  SaImmClassNameT className = zdecodeString(&alloc, &request);

  char *parentNameS = zdecodeString(&alloc, &request);
  SaNameT *parentName = zname(&alloc, parentNameS);

  SaImmAttrValuesT_2 **attrBindingsList = zdecodeAttrBindings(&alloc, &request);
  SaAisErrorT r =
      saImmOiRtObjectCreate_2(
          handle,
          className,
          parentName,
          (const SaImmAttrValuesT_2 **)attrBindingsList);
  if (r != SA_AIS_OK) {
    reclaim(alloc);
    RETURN1("{error, ~a}", errorAtom(r));
  }
  else {
    reclaim(alloc);
    RETURN0("{ok}");
  }
}


static ei_x_buff oiRtObjectDelete(int arity, ei_x_buff request) {
  Pair *alloc = NULL;
  if (arity != 2) {
    RETURN1("{error, {bad_arity, ~i}}", arity);
  }
  SaImmCcbHandleT handle = (SaImmCcbHandleT)decodeU64Handle(&request);
  char *nameS = zdecodeString(&alloc, &request);
  SaNameT *name = zname(&alloc, nameS);
  SaAisErrorT r = saImmOiRtObjectDelete(handle, name);
  if (r != SA_AIS_OK) {
    reclaim(alloc);
    RETURN1("{error, ~a}", errorAtom(r));
  }
  else {
    reclaim(alloc);
    RETURN0("{ok}");
  }
}


static ei_x_buff oiRtObjectUpdate(int arity, ei_x_buff request) {
  if (arity != 4) {
    RETURN1("{error, {bad_arity, ~i}}", arity);
  }

  SaImmCcbHandleT handle = (SaImmCcbHandleT)decodeU64Handle(&request);

  Pair *alloc = NULL;
  char *nameS = zdecodeString(&alloc, &request);
  SaNameT *name = zname(&alloc, nameS);

  SaImmAttrValuesT_2 **attrBindingsList = zdecodeAttrBindings(&alloc, &request);

  int nModTypes = decodeTupleHeader(&request);
  SaImmAttrModificationT_2 *attrMods[nModTypes+1];
  attrMods[nModTypes] = NULL;
  for (int k = 0; k < nModTypes; k++) {
    attrMods[k] = zalloc(&alloc, 1, sizeof(SaImmAttrModificationT_2));
    attrMods[k]->modType = decodeLong(&request);
    attrMods[k]->modAttr = *(attrBindingsList[k]);
  }

  SaAisErrorT r = saImmOiRtObjectUpdate_2(handle, name, (const SaImmAttrModificationT_2 **)attrMods);
  if (r != SA_AIS_OK) {
    reclaim(alloc);
    RETURN1("{error, ~a}", errorAtom(r));
  }
  else {
    reclaim(alloc);
    RETURN0("{ok}");
  }
}

/**
 * Was used for debugging; generalize or remove
 */
void verifyArgs(SaImmCcbHandleT ccbHandle,
    const SaImmClassNameT className,
    const SaNameT *parentName,
    const SaImmAttrValuesT_2 **attrValues) {

  FILE *f = fopen("/tmp/imm-v.txt", "w");

  fprintf(f, "CCB handle: %llu\n", ccbHandle);
  fprintf(f, "class name: %s\n", className);

  char parentNameS[(parentName->length)+1];
  for (int j = 0; j < (parentName->length); j++) {
    parentNameS[j] = (char)parentName->value[j];
  }
  parentNameS[parentName->length] = '\0';

  fprintf(f, "parent name: %s\n", parentNameS);

  if (attrValues[1] != NULL) {
    fprintf(f, "unexpected n of attrs!!!!!!!!!!!!!!\n");
    fclose(f);
    return;
  }

  fprintf(f, "attr name: %s\n", attrValues[0]->attrName);
  fprintf(f, "attr value type: %d\n", attrValues[0]->attrValueType);
  fprintf(f, "attr value mult: %u\n", attrValues[0]->attrValuesNumber);

  const SaUint32T one = 1;
  if (attrValues[0]->attrValuesNumber != one) {
    fprintf(f, "unexpected multiplicity!!!!!!!!!!!!!!\n");
    fclose(f);
    return;
  }
  SaImmAttrValueT u = attrValues[0]->attrValues[0];
  char **z = (char **)u;
  fprintf(f, "attr value: %s\n", *z);
  fclose(f);
}

/**
 * Handle an rct_proxy request.
 */
ei_x_buff send_sig_imm(ei_x_buff request) {
  if (getType(&request) != ERL_SMALL_TUPLE_EXT) {
    RETURN0("{error, tuple_expected}");
  }
  else if (getSize(&request) != 2) {
    RETURN0("{error, bad_tuple_size}");
  }
  else {
    decodeTupleHeader(&request);
    if (getType(&request) != ERL_ATOM_EXT) {
      RETURN0("{error, function_name_not_atom}");
    }
    else {
      char functionName[getSize(&request) + 1];
      ei_decode_atom((const char *)request.buff, &(request.index), functionName);

      if (getType(&request) != ERL_SMALL_TUPLE_EXT) {
        RETURN0("{error, bad_arglist}");
      }
      else {
        const int funArity = decodeTupleHeader(&request);
        if (STREQ(functionName, "omInitialize")) {
          return omInitialize(funArity, request);
        }
        else if (STREQ(functionName, "omFinalize")) {
          return omFinalize(funArity, request);
        }
        else if (STREQ(functionName, "omAoInitialize")) {
          return omAoInitialize(funArity, request);
        }
        else if (STREQ(functionName, "omAoFinalize")) {
          return omAoFinalize(funArity, request);
        }
        else if (STREQ(functionName, "omAoSet")) {
          return omAoSet(funArity, request);
        }
        else if (STREQ(functionName, "omAoRelease")) {
          return omAoRelease(funArity, request);
        }
        else if (STREQ(functionName, "omCcbInitialize")) {
          return omCcbInitialize(funArity, request);
        }
        else if (STREQ(functionName, "omCcbFinalize")) {
          return omCcbFinalize(funArity, request);
        }
        else if (STREQ(functionName, "omCcbObjDelete")) {
          return omCcbObjDelete(funArity, request);
        }
        else if (STREQ(functionName, "omCcbObjCreate")) {
          return omCcbObjCreate(funArity, request);
        }
        else if (STREQ(functionName, "omCcbObjsCreate")) {
          return omCcbObjsCreate(funArity, request);
        }
        else if (STREQ(functionName, "omCcbObjModify")) {
          return omCcbObjModify(funArity, request);
        }
        else if (STREQ(functionName, "omCcbApply")) {
          return omCcbApply(funArity, request);
        }
        else if (STREQ(functionName, "omAccessorInitialize")) {
          return omAccessorInitialize(funArity, request);
        }
        else if (STREQ(functionName, "omAccessorFinalize")) {
          return omAccessorFinalize(funArity, request);
        }
        else if (STREQ(functionName, "omAccessorGet")) {
          return omAccessorGet(funArity, request);
        }
        else if (STREQ(functionName, "oiInitialize")) {
          return oiInitialize(funArity, request);
        }
        else if (STREQ(functionName, "oiFinalize")) {
          return oiFinalize(funArity, request);
        }
        else if (STREQ(functionName, "oiImplementerSet")) {
          return oiImplementerSet(funArity, request);
        }
        else if (STREQ(functionName, "oiImplementerClear")) {
          return oiImplementerClear(funArity, request);
        }
        else if (STREQ(functionName, "oiRtObjectCreate")) {
          return oiRtObjectCreate(funArity, request);
        }
        else if (STREQ(functionName, "oiRtObjectDelete")) {
          return oiRtObjectDelete(funArity, request);
        }
        else if (STREQ(functionName, "oiRtObjectUpdate")) {
          return oiRtObjectUpdate(funArity, request);
        }
        else if (STREQ(functionName, "testInteger")) {
          return testInteger(funArity, request);
        }
        else {
          RETURN1("{error, {undefined_function, ~a}}", functionName);
        }
      }
    }
  }
}
