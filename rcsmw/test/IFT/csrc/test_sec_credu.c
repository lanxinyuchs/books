/* ----------------------------------------------------------------------
 * %CCaseFile:	test_sec_credu.c %
 * %CCaseRev:	/main/R11A/R12A/1 %
 * %CCaseDate:	2017-12-06 %
 * %CCaseDocNo: %
 * Author: enekdav
 *
 * Short description:
 * Tests of SEC_CREDU use functions defined here.
 * ----------------------------------------------------------------------
 * %CCaseTemplateFile:	template.c %
 * %CCaseTemplateId: CCver: /main/1 %
 *
 * %CCaseCopyrightBegin%
 * Copyright (c) Ericsson AB 2017 All rights reserved.
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
 * -----      -------    --------    ------------------------------------
 * R11A/2      2017-09-13 enekdav     Created
 * R11A/3      2017-09-18 eivmiha     Added INIT & FINAL
 * R11A/4      2017-09-18 eivmiha     Fixed traces
 * R11A/6      2017-09-18 eivmiha     Added NC/TCat subscribe/unsubscribe
 * R11A/8      2017-09-22 enekdav     Fixed a lot of issues
 * R11A/9      2017-09-22 eivmiha     Fixed NC/TCat subscribe/unsubscribe
 * R11A/11     2017-09-27 evadumb     Added get_selectionobject, dispatch
 * R11A/13     2017-10-02 emirbos     Fixed TCat subscribe
 * R11A/14     2017-10-06 evadumb     Fixed GET_TCAT, GET_TCAT_CERT
 * R11A/15     2017-10-10 enekdav     Minor change in initialize
 * R11A/16     2017-10-19 enekdav     Added STATUS_STRING test
 * R12A/1      2017-12-06 emirbos     Fixed GET_NC_CERT, GET_NC_KEY and GET_TCAT_DIR
 * ----------------------------------------------------------------------
 */

#include "master.h"
#include "sec_credu_api.h"
#include "shell.h"
#include <unistd.h>
#include <string.h>
#include <stdio.h>
#include <errno.h>
#include <sys/select.h>
#include <sys/utsname.h>
#include <sys/types.h>

typedef enum {
	STATUS_STRING,
	INIT,
	FINAL,
	GET_SELECTIONOBJECT,
	DISPATCH,
	NC_SUB,
	NC_UNSUB,
	GET_NC_CERT,
	GET_NC_KEY,
	TCAT_SUB,
	TCAT_UNSUB,
	GET_TCAT,
	FREE_TCAT,
	GET_TCAT_DIR,
	GET_TCAT_COUNT,
	GET_TCAT_CERT
} SecCreduTestType;

typedef enum {
	SUCCESS = 0,
	ERROR_HANDLE_NULL = 1,
	ERROR_VERSION_NULL = 2,
	ERROR_PARAMETERS_NULL = 3,
	ERROR_INVALID_ID = 4,
	ERROR_INVALID_NODE_CREDENTIAL_ID = 5,
	ERROR_INVALID_TRUST_CATEGORY_ID = 6,
	ERROR_INVALID_SUBSCRIBE_ID = 7
}TestSuccess;


extern bool TRI_trace_enabled;

bool nc_callback_called = false;
bool tcat_callback_called = false;

static long decodelong(const char* buffer, int* index) {
	long result;
	ei_decode_long(buffer, index, &result);
	return result;
}

static void verifyTuple(const char* buffer, int* index, int expectedArity,
		char* tag) {
	int arity;
	ei_decode_tuple_header(buffer, index, &arity);
	if (arity != expectedArity) {
		QTRACE_ERROR4("tuple arity mismatch, expected: %d, actual: %d, tag: %s",
				expectedArity, arity, tag);
	}
}

static char *
decodeWrappedString(const char* buffer, int* index) {
  verifyTuple(buffer, index, 2, "ill-formed wrapped string");
  unsigned long length;
  ei_decode_ulong(buffer, index, &length);
  char* result = malloc(length+1);
  ei_decode_string(buffer, index, result);
  return result;
}

void secCreduNodeCredentialChangeCallback(SecCreduHandle handle,
		SecCreduSubscription nodecredential_subscription,
		const char *nodecredential_id) {
	QTRACE2(3, "NodeCredential callback");
	nc_callback_called = true;
}

void secCreduTrustCategoryChangeCallback(SecCreduHandle handle,
		SecCreduSubscription trustcategory_subscription,
		const char *trustcategory_id) {
	QTRACE2(3, "TrustCategory callback");
	tcat_callback_called = true;
}

ei_x_buff send_sig_sec_credu(int func, ei_x_buff args) {
	ei_x_buff resp;
	ei_x_new_with_version(&resp);

	SecCreduHandle handle;
	SecCreduParameters *secCreduParameters = NULL;
	SecCreduStatus result;
	SecCreduSubscription nodecredential_subscription;
	SecCreduSubscription trustcategory_subscription;
	SecCreduFormat format;
	char *nodecredential_data;
	const char *status_string;
	SecCreduTrustCategory *category;
	int successful;
	int secStatusString;
	int fd;

	QENTER2("send_sig_sec_credu(), func: %d", func);

	switch (func) {
	case STATUS_STRING:
		QTRACE2(3, "call: Sec_credu_status_string()");
		verifyTuple(args.buff, &args.index, 1, "STATUS_STRING");

		secStatusString = (int) decodelong(args.buff, &args.index);

		status_string = sec_credu_status_string(secStatusString);
		if (strcmp(status_string,"NULL") == 0) {
			ei_x_format_wo_ver(&resp, "{error, ~s}", status_string);
		} else {
			ei_x_format_wo_ver(&resp, "{ok, ~s}", status_string);
		}

		break;

	case INIT:
		QTRACE2(3, "call: Sec_credu_initialize()");
		SecCreduVersion *secCreduVersion = NULL;

		verifyTuple(args.buff, &args.index, 4, "INIT");
		successful = (int) decodelong(args.buff, &args.index);
		if (successful != ERROR_VERSION_NULL) {
			secCreduVersion = malloc(sizeof(SecCreduVersion));
			secCreduVersion->release_code = (char) decodelong(args.buff,
					&args.index);
			secCreduVersion->major_version = (uint8_t) decodelong(args.buff,
					&args.index);
			secCreduVersion->minor_version = (uint8_t) decodelong(args.buff,
					&args.index);
		}
		if (successful != ERROR_PARAMETERS_NULL) {
			secCreduParameters = malloc(sizeof(SecCreduParameters));
			secCreduParameters->nodecredential_change_callback =
					&secCreduNodeCredentialChangeCallback;
			secCreduParameters->trustcategory_change_callback =
					&secCreduTrustCategoryChangeCallback;
		}

		result = sec_credu_initialize(&handle, secCreduParameters,
				secCreduVersion);
		if (result == SEC_CREDU_OK) {
			ei_x_format_wo_ver(&resp, "{ok, ~i, ~i, ~i, {~c,~i,~i}}", result, handle, *(int *)handle, secCreduVersion->release_code, secCreduVersion->major_version, secCreduVersion->minor_version);
		} else {
			ei_x_format_wo_ver(&resp, "{error, ~i}", result);
		}

		break;

	case FINAL:
		QTRACE2(3, "call: Sec_credu_finalize()");
		verifyTuple(args.buff, &args.index, 2, "FINAL");

		successful = (int) decodelong(args.buff, &args.index);
		handle = (SecCreduHandle) decodelong(args.buff,
				&args.index);

		if (successful == ERROR_HANDLE_NULL) {
			handle = NULL;
		} else if (successful == ERROR_INVALID_ID) {
			*((int *) handle) += 1;
		}

		result = sec_credu_finalize(handle);
		if (successful == ERROR_INVALID_ID) {
			*((int *) handle) -= 1;
		}
		if (result == SEC_CREDU_OK) {
			ei_x_format_wo_ver(&resp, "{ok, ~i}", result);
		} else {
			ei_x_format_wo_ver(&resp, "{error, ~i}", result);
		}
		break;

	case GET_SELECTIONOBJECT:
		QTRACE2(3, "call: Sec_credu_selection_object_get()");
		verifyTuple(args.buff, &args.index, 2, "GET_SELECTIONOBJECT");
		successful = (int) decodelong(args.buff, &args.index);
		handle = (SecCreduHandle)decodelong(args.buff, &args.index);

		if (successful == ERROR_HANDLE_NULL) {
			handle = NULL;
		} else if (successful == ERROR_INVALID_ID) {
			*((int *) handle) += 1;
		}

		fd = -1;

		result = sec_credu_selectionobject_get(handle, &fd);
		if (successful == ERROR_INVALID_ID) {
			*((int *) handle) -= 1;
		}

		if (result == SEC_CREDU_OK) {
			ei_x_format_wo_ver(&resp, "{ok, ~i, ~i}", result, fd);
		}
		else {
			ei_x_format_wo_ver(&resp, "{error, ~i}", result);
		}

		break;

	case DISPATCH:
		nc_callback_called = false;
		tcat_callback_called = false;

		QTRACE2(3, "call: Sec_credu_dispatch()");
		verifyTuple(args.buff, &args.index, 4, "DISPATCH");
		successful = (int) decodelong(args.buff, &args.index);
		handle = (SecCreduHandle)decodelong(args.buff, &args.index);
		fd = (int) decodelong(args.buff, &args.index);
		SecCreduDispatchFlags flags = (SecCreduDispatchFlags) decodelong(args.buff, &args.index);

		if (successful == ERROR_HANDLE_NULL) {
			handle = NULL;
		} else if (successful == ERROR_INVALID_ID) {
			*((int *) handle) += 1;
		}

		result = sec_credu_dispatch(handle, fd, flags);

		if (result == SEC_CREDU_OK) {
			ei_x_format_wo_ver(&resp, "{ok, ~i, ~i, ~i}", result, nc_callback_called, tcat_callback_called);
		}
		else {
			ei_x_format_wo_ver(&resp, "{error, ~i}", result);
		}

		break;


	case NC_SUB:
		QTRACE2(3, "call: Sec_credu_nodecredential_subscribe()");

		verifyTuple(args.buff, &args.index, 3, "NC_SUB");

		successful = (int) decodelong(args.buff, &args.index);
		handle = (SecCreduHandle) decodelong(args.buff,
				&args.index);
		const char* nodecredential_id = decodeWrappedString(args.buff,
				&args.index);

		if (successful == ERROR_HANDLE_NULL)
			handle = NULL;
		else if (successful == ERROR_INVALID_ID)
			*((int *) handle) = *((int *) handle) + 1;

		result = sec_credu_nodecredential_subscribe(handle,
							nodecredential_id, &nodecredential_subscription);

		if (successful == ERROR_INVALID_ID) {
			*((int *) handle) -= 1;
		}

		if (result == SEC_CREDU_OK) {
			ei_x_format_wo_ver(&resp, "{ok, ~i, {~i}}", result, nodecredential_subscription);
		} else {
			ei_x_format_wo_ver(&resp, "{error, ~i}", result);
		}
		break;

	case NC_UNSUB:
		QTRACE2(3, "call: Sec_credu_nodecredential_unsubscribe()");

		verifyTuple(args.buff, &args.index, 3, "NC_UNSUB");

		successful = (int) decodelong(args.buff, &args.index);
		handle = (SecCreduHandle) decodelong(args.buff,
				&args.index);
		nodecredential_subscription =
				(SecCreduSubscription) decodelong(args.buff, &args.index);

		if (successful == ERROR_HANDLE_NULL)
			handle = NULL;
		else if (successful == ERROR_INVALID_ID)
			*((int *) handle) = *((int *) handle) + 1;

		result = sec_credu_nodecredential_unsubscribe(handle,
					nodecredential_subscription);

		if (successful == ERROR_INVALID_ID) {
			*((int *) handle) -= 1;
		}

		if (result == SEC_CREDU_OK) {
			ei_x_format_wo_ver(&resp, "{ok, ~i}", result);
		} else {
			ei_x_format_wo_ver(&resp, "{error, ~i}", result);
		}
		break;

	case GET_NC_CERT:
		QTRACE2(3, "call: Sec_credu_nodecredential_cert_get()");

		verifyTuple(args.buff, &args.index, 4, "GET_NC_CERT");

		successful = (int) decodelong(args.buff, &args.index);
		handle = (SecCreduHandle) decodelong(args.buff,
				&args.index);
		nodecredential_subscription =
				(SecCreduSubscription) decodelong(args.buff, &args.index);
		format = (SecCreduFormat) decodelong(args.buff, &args.index);

		if (successful == ERROR_HANDLE_NULL)
			handle = NULL;
		else if (successful == ERROR_INVALID_ID)
			*((int *) handle) = *((int *) handle) + 1;

		result = sec_credu_nodecredential_cert_get(handle,
				nodecredential_subscription, format, &nodecredential_data);

		if (successful == ERROR_INVALID_ID) {
			*((int *) handle) -= 1;
		}

		if (result == SEC_CREDU_OK) {
			if (nodecredential_data != NULL) {
				ei_x_format_wo_ver(&resp, "{ok, ~i, {~s}}", result, nodecredential_data);
			} else {
			    ei_x_format_wo_ver(&resp, "{ok, ~i, {~s}}", result, "NULL");
			}
		} else {
			ei_x_format_wo_ver(&resp, "{error, ~i}", result);
		}

		break;

	case GET_NC_KEY:
		QTRACE2(3, "call: Sec_credu_nodecredential_key_get()");

		verifyTuple(args.buff, &args.index, 4, "GET_NC_KEY");

		successful = (int) decodelong(args.buff, &args.index);
		handle = (SecCreduHandle) decodelong(args.buff,
				&args.index);
		nodecredential_subscription =
				(SecCreduSubscription) decodelong(args.buff, &args.index);
		format = (SecCreduFormat) decodelong(args.buff, &args.index);

		if (successful == ERROR_HANDLE_NULL)
			handle = NULL;
		else if (successful == ERROR_INVALID_ID)
			*((int *) handle) = *((int *) handle) + 1;

		result = sec_credu_nodecredential_key_get(handle,
				nodecredential_subscription, format, &nodecredential_data);

		if (successful == ERROR_INVALID_ID) {
			*((int *) handle) -= 1;
		}

		if (result == SEC_CREDU_OK) {
			if (nodecredential_data != NULL) {
				ei_x_format_wo_ver(&resp, "{ok, ~i, {~s}}", result, nodecredential_data);
			} else {
			    ei_x_format_wo_ver(&resp, "{ok, ~i, {~s}}", result, "NULL");
			}
		} else {
			ei_x_format_wo_ver(&resp, "{error, ~i}", result);
		}
		break;

	case TCAT_SUB:
		QTRACE2(3, "call: Sec_credu_trustcategory_subscribe()");

		verifyTuple(args.buff, &args.index, 3, "TCAT_SUB");

		successful = (int) decodelong(args.buff, &args.index);
		handle = (SecCreduHandle) decodelong(args.buff,
				&args.index);
		const char* trustcategory_id = decodeWrappedString(args.buff,
				&args.index);

		if (successful == ERROR_HANDLE_NULL)
			handle = NULL;
		else if (successful == ERROR_INVALID_ID)
			*((int *) handle) = *((int *) handle) + 1;

		result = sec_credu_trustcategory_subscribe(handle,
							trustcategory_id, &trustcategory_subscription);

		if (successful == ERROR_INVALID_ID) {
			*((int *) handle) -= 1;
		}

		if (result == SEC_CREDU_OK) {
			ei_x_format_wo_ver(&resp, "{ok, ~i, {~i}}", result, trustcategory_subscription);
		} else {
			ei_x_format_wo_ver(&resp, "{error, ~i}", result);
		}
		break;

	case TCAT_UNSUB:
		QTRACE2(3, "call: Sec_credu_trustcategory_unsubscribe()");
		verifyTuple(args.buff, &args.index, 3, "TCAT_UNSUB");

		successful = (int) decodelong(args.buff, &args.index);
		handle = (SecCreduHandle) decodelong(args.buff,
				&args.index);
		trustcategory_subscription =
				(SecCreduSubscription) decodelong(args.buff, &args.index);

		if (successful == ERROR_HANDLE_NULL)
			handle = NULL;
		else if (successful == ERROR_INVALID_ID)
			*((int *) handle) = *((int *) handle) + 1;

		result = sec_credu_trustcategory_unsubscribe(handle,
							trustcategory_subscription);

		if (successful == ERROR_INVALID_ID) {
			*((int *) handle) -= 1;
		}

		if (result == SEC_CREDU_OK) {
			ei_x_format_wo_ver(&resp, "{ok, ~i}", result);
		} else {
			ei_x_format_wo_ver(&resp, "{error, ~i}", result);
		}
		break;

	case GET_TCAT:
		QTRACE2(3, "call: Sec_credu_get_tcat()");
		verifyTuple(args.buff, &args.index, 3, "GET_TCAT");
		successful = (int) decodelong(args.buff, &args.index);
		handle = (SecCreduHandle) decodelong(args.buff, &args.index);
		trustcategory_subscription = (SecCreduSubscription) decodelong(args.buff, &args.index);

		if (successful == ERROR_HANDLE_NULL)
			handle = NULL;
		else if (successful == ERROR_INVALID_ID)
			*((int *) handle) = *((int *) handle) + 1;

		result = sec_credu_trustcategory_get(handle, trustcategory_subscription, &category);

		if (successful == ERROR_INVALID_ID) {
			*((int *) handle) -= 1;
		}

		if (result == SEC_CREDU_OK) {
			ei_x_format_wo_ver(&resp, "{ok, ~i, {~i}}", result, category);
		} else {
			ei_x_format_wo_ver(&resp, "{error, ~i}", result);
		}

		break;

	case FREE_TCAT:
		QTRACE2(3, "call: Sec_credu_trustcategory_free()");

		verifyTuple(args.buff, &args.index, 3, "FREE_TCAT");

		successful = (int) decodelong(args.buff, &args.index);
		handle = (SecCreduHandle) decodelong(args.buff,
				&args.index);
		category = (SecCreduTrustCategory *) decodelong(args.buff, &args.index);

		if (successful == ERROR_HANDLE_NULL)
			handle = NULL;

		result = sec_credu_trustcategory_free(handle, &category);

		if (result == SEC_CREDU_OK) {
			ei_x_format_wo_ver(&resp, "{ok, ~i}", result);
		} else {
			ei_x_format_wo_ver(&resp, "{error, ~i}", result);
		}
		break;

	case GET_TCAT_DIR:
		QTRACE2(3, "call: Sec_credu_trustcategory_dirname_get()");
		verifyTuple(args.buff, &args.index, 2, "GET_TCAT_DIR");

		successful = (int) decodelong(args.buff, &args.index);
		category = (SecCreduTrustCategory *) decodelong(args.buff, &args.index);
		char *dirname;

		if (successful == ERROR_HANDLE_NULL)
			category = NULL;

		result = sec_credu_trustcategory_dirname_get(category, &dirname);

		if (result == SEC_CREDU_OK) {
			if (dirname != NULL) {
				ei_x_format_wo_ver(&resp, "{ok, ~i, {~s}}", result, dirname);
			} else {
			    ei_x_format_wo_ver(&resp, "{ok, ~i, {~s}}", result, "NULL");
			}
		} else {
			ei_x_format_wo_ver(&resp, "{error, ~i}", result);
		}
		break;

	case GET_TCAT_COUNT:
		QTRACE2(3, "call: Sec_credu_trustcategory_cert_count_get()");
		verifyTuple(args.buff, &args.index, 2, "GET_TCAT_COUNT");

		successful = (int) decodelong(args.buff, &args.index);
		category = (SecCreduTrustCategory *) decodelong(args.buff, &args.index);
		size_t trustedcertificate_count;

		if (successful == ERROR_HANDLE_NULL)
			category = NULL;

		result = sec_credu_trustcategory_cert_count_get(category, &trustedcertificate_count);

		if (result == SEC_CREDU_OK) {
			ei_x_format_wo_ver(&resp, "{ok, ~i, {~i}}", result, trustedcertificate_count);
		} else {
			ei_x_format_wo_ver(&resp, "{error, ~i}", result);
		}

		break;

	case GET_TCAT_CERT:
		QTRACE2(3, "call: Sec_credu_trustcategory_cert_get()");

		verifyTuple(args.buff, &args.index, 4, "GET_TCAT_CERT");
		successful = (int) decodelong(args.buff, &args.index);
		category = (SecCreduTrustCategory *) decodelong(args.buff, &args.index);
		format = (SecCreduFormat) decodelong(args.buff, &args.index);
		size_t index = (size_t) decodelong(args.buff, &args.index) -1;

		char* trustedcertificate_id;
		char *trustedcertificate_data;

		if (successful == ERROR_HANDLE_NULL)
			category = NULL;

		result = sec_credu_trustcategory_cert_get(category, format, index,
				&trustedcertificate_data, &trustedcertificate_id);

		if (result == SEC_CREDU_OK) {
			ei_x_format_wo_ver(&resp, "{ok, ~i, {~s, ~s}}", result, trustedcertificate_id, trustedcertificate_data);
		} else {
			ei_x_format_wo_ver(&resp, "{error, ~i}", result);
		}
		break;

	default:
		ei_x_format_wo_ver(&resp, "{error, ~s. ~i}", "unknown function code",
				func);
	}

	QRETURN1(resp);
}
