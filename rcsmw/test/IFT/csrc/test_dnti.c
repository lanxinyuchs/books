/* ----------------------------------------------------------------------
 * %CCaseFile:	test_dnti.c %
 * %CCaseRev:	/main/R2A/R4A/1 %
 * %CCaseDate:	2015-11-26 %
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
 * Copyright (c) Ericsson AB 2012-2015 All rights reserved.
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
 * R4A/1      2015-11-26 erarafo     Wrappers around TRI trace macros
 * ----------------------------------------------------------------------
 */


#include "master.h"
#include "dnti.h"

/* must match test_dnti.hrl */
#define DNTI_TRANSFORM                      1
#define DNTI_PASS_INVALID_TODNP_A           2
#define DNTI_CODE_EXAMPLE                   3
#define DNTI_INITIATE_SERVICE               4
#define DNTI_TERMINATE_SERVICE              5
#define DNTI_CLEANUP                        6
#define DNTI_TERMINATE_SERVICE_KEEP_HANDLE  7
#define DNTI_PASS_INVALID_TODNP_B           8

extern bool TRI_trace_enabled;

/*
 * Translates function number to name.
 *
 */
char*
getName(int func) {
  switch(func) {
  case DNTI_TRANSFORM:
    return "DNTI_TRANSFORM";
  case DNTI_PASS_INVALID_TODNP_A:
    return "DNTI_PASS_INVALID_TODNP_A";
  case DNTI_CODE_EXAMPLE:
    return "DNTI_CODE_EXAMPLE";
  case DNTI_INITIATE_SERVICE:
    return "DNTI_INITIATE_SERVICE";
  case DNTI_TERMINATE_SERVICE:
    return "DNTI_TERMINATE_SERVICE";
  case DNTI_CLEANUP:
    return "DNTI_CLEANUP";
  case DNTI_TERMINATE_SERVICE_KEEP_HANDLE:
    return "DNTI_TERMINATE_SERVICE_KEEP_HANDLE";
  case DNTI_PASS_INVALID_TODNP_B:
    return "DNTI_PASS_INVALID_TODNP_B";
  default:
    return "functionNameUnknown";
  }
}

/*
 * Translates direction number to name.
 *
 */
char*
getDirection(DntiTransformDirectionT direction) {
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
char*
getResult(DntiResultT result) {
  switch (result) {
  case DNTI_OK:
    return "DNTI_OK";
  case DNTI_INVALID_PARAMETER_DIRECTION:
    return "DNTI_INVALID_PARAMETER_DIRECTION";
  case DNTI_INVALID_PARAMETER_TODNP:
    return "DNTI_INVALID_PARAMETER_TODNP";
  case DNTI_SEND_ERROR:
    return "DNTI_SEND_ERROR";
  case DNTI_RECEIVE_ERROR:
    return "DNTI_RECEIVE_ERROR";
  case DNTI_OBJECT_CLASS_NOT_FOUND:
    return "DNTI_OBJECT_CLASS_NOT_FOUND";
  default:
    return "unknown result code";

  }
}





/*
 * The code example from the IWD, with minor
 * adjustments.
 *
 */
int
codeExample(int argc, const char* argv[])
{
  QENTER1("codeExample");

  DntiResultT res;

  if (argc != 3) {
    QTRACE3(3, "usage: %s direction distinguished_name", argv[0]);
    res = 1;
  }
  else {
    int dir = atoi(argv[1]);
    char* fromDnP = (char *) argv[2];
    char* toDnP = NULL;

    QTRACE4(3, "call dntiTransform(), direction: %d, input DN: %s", dir, fromDnP);

    res = dntiTransform(NULL, dir, fromDnP, &toDnP);

    if (res != DNTI_OK) {
      QTRACE_ERROR2("DNTI error, result: %d", res);
    }
    else {
      QTRACE3(3, "transformed DN: %s", toDnP);
    }

    if (toDnP != NULL) {
      free(toDnP);
    }
  }

  QRETURN1(res);
}


char*
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



void*
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



handles*
addHandle(unsigned long id, void* handle, handles* h) {
  QENTER1("addHandle");
  handles* result = malloc(sizeof(handles));
  result->clientId = id;
  result->handle = handle;
  result->next = (struct handles*)h;
  QRETURN1(result);
}


handles*
connectionHandles = NULL;




void
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
 * Calls dntiTransform once per list item. Optionally calls the
 * dntiInitiateService and dntiTerminateService functions too.
 * Optionally calls the codeExample function.
 *
 */
ei_x_buff
send_sig_dnti(int func, ei_x_buff args) {
  QENTER3("send_sig_dnti, function: %d (%s)", func, getName(func));

  QTRACE3(3, "CEC_PORT: %s", getString(getenv("CEC_PORT"), "undefined"));

  ei_x_buff resp;
  ei_x_new(&resp);

  /* trust that we have a 5-element tuple {ClientId, LongLivingTn, Direction, InDnLength, InDn} */
  ei_decode_tuple_header(args.buff, &args.index, NULL);

  unsigned long clientId;
  ei_decode_ulong(args.buff, &args.index, &clientId);

  if (func == DNTI_CLEANUP) {
    deleteHandles(connectionHandles);
    connectionHandles = NULL;
    ei_x_format(&resp, "{ok}");
  }
  else if (func == DNTI_INITIATE_SERVICE) {
    void* handle = dntiInitiateService();
    if (handle == NULL) {
      ei_x_format(&resp, "{error, ~s, ~i}", "could not initiate service for client", clientId);
    }
    else {
      connectionHandles = addHandle(clientId, handle, connectionHandles);
      ei_x_format(&resp, "{ok}");
    }
  }
  else if (func == DNTI_TERMINATE_SERVICE) {
    void* handle = getHandle(clientId, connectionHandles);
    if (handle == NULL) {
      ei_x_format(&resp, "{error, ~s, ~i}", "no active connection for client", clientId);
    }
    else {
      dntiTerminateService(handle);
      connectionHandles = addHandle(clientId, NULL, connectionHandles);
      ei_x_format(&resp, "{ok}");
    }
  }
  else if (func == DNTI_TERMINATE_SERVICE_KEEP_HANDLE) {
    void* handle = getHandle(clientId, connectionHandles);
    if (handle == NULL) {
      ei_x_format(&resp, "{error, ~s, ~i}", "no active connection for client", clientId);
    }
    else {
      dntiTerminateService(handle);
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

    if (func == DNTI_CODE_EXAMPLE) {
      /* invoke the code example; transformed DN can be seen in trace output */
      char* argv[3];
      argv[0] = "codeExample";
      argv[1] = malloc(2);
      snprintf(argv[1], 2, "%ld", direction);
      argv[2] = inDn;
      DntiResultT dntiResult = codeExample(3, (const char**)&argv);
      free(argv[1]);
      free(inDn);

      ei_x_format(&resp, "{ok, ~i}", dntiResult);
    }
    else if (func == DNTI_TRANSFORM) {

      void *handle = getHandle(clientId, connectionHandles);

      if (clientId != 0 && handle == NULL) {
        ei_x_format(&resp, "{error, ~s, ~i}", "no connection handle for client", clientId);
      }
      else {
        char* outDn = NULL;
        QTRACE4(3, "call dntiTransform(), direction: %s, inDn: %s", getDirection(direction), inDn);
        DntiResultT dntiResult =
            dntiTransform((clientId != 0 ? handle : NULL),
                (DntiTransformDirectionT)direction,
                inDn,
                &outDn);
        QTRACE4(3, "dntiTransform result: %d, outDn: %s", dntiResult, getString(outDn, "NULL"));

        free(inDn);

        if (dntiResult == DNTI_OK || dntiResult == DNTI_OBJECT_CLASS_NOT_FOUND) {
          ei_x_format(&resp, "{ok, ~i, ~s}", dntiResult, outDn);
        }
        else {
          ei_x_format(&resp, "{ok, ~i, ~s}", dntiResult, getResult(dntiResult));
        }

        if (outDn != NULL) {
          free(outDn);
        }
      }
    }

    else if (func == DNTI_PASS_INVALID_TODNP_A) {
      // Used in negative test where the location pointed to by
      // the 4th argument to dntiTransform does not contain NULL.

      if (clientId != 0 && getHandle(clientId, connectionHandles) == NULL) {
	ei_x_format(&resp, "{error, ~s, ~i}", "no connection handle for client", clientId);
      }
      else {
	char* outDn = "somethingOtherThanNull";
	char* outDnBeforeCall = outDn;
	QTRACE4(3, "call dntiTransform(), direction: %s, inDn: %s", getDirection(direction), inDn);
	DntiResultT dntiResult =
	  dntiTransform((clientId != 0 ? getHandle(clientId, connectionHandles) : NULL),
			(DntiTransformDirectionT)direction,
			inDn,
			&outDn);
	QTRACE4(3, "dntiTransform result: %d, outDn: %s", dntiResult, getString(outDn, "NULL"));

	free(inDn);

	if (dntiResult == DNTI_INVALID_PARAMETER_TODNP && outDn != outDnBeforeCall) {
	  ei_x_format(&resp, "{ok, ~i, ~s}", dntiResult, "unexpected: outDn was changed");
	}
	else {
	  ei_x_format(&resp, "{ok, ~i, ~s}", dntiResult, getResult(dntiResult));
	}
      }
    }

    else if (func == DNTI_PASS_INVALID_TODNP_B) {
      // Used in negative test where the 4th argument to dntiTransform
      // is NULL.

      if (clientId != 0 && getHandle(clientId, connectionHandles) == NULL) {
	ei_x_format(&resp, "{error, ~s, ~i}", "no connection handle for client", clientId);
      }
      else {

	QTRACE4(3, "call dntiTransform(), direction: %s, inDn: %s, outDn is NULL (incorrect by intention)", getDirection(direction), inDn);
	DntiResultT dntiResult =
	  dntiTransform((clientId != 0 ? getHandle(clientId, connectionHandles) : NULL),
			(DntiTransformDirectionT)direction,
			inDn,
			NULL);
	QTRACE3(3, "dntiTransform result: %d", dntiResult);

	free(inDn);

	ei_x_format(&resp, "{ok, ~i, ~s}", dntiResult, getResult(dntiResult));
      }
    }

    else {
      ei_x_format(&resp, "{ok, ~s}", "function does not exist", func);
    }

  }

  QRETURN1(resp);
}
