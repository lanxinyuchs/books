/* ----------------------------------------------------------------------
 * %CCaseFile:	test_self.c %
 * %CCaseRev:	/main/R2A/R3A/R5A/R6A/2 %
 * %CCaseDate:	2016-07-11 %
 * %CCaseDocNo: %
 * Author:	erarafo
 * Author: <name>, <e-mail address>
 *
 * Short description: Very simple self-test of the IFT application, for
 * use as a block-local test. Extend as needed.
 * ----------------------------------------------------------------------
 * %CCaseTemplateFile:	template.c %
 * %CCaseTemplateId: CCver: /main/1 %
 *
 * %CCaseCopyrightBegin%
 * Copyright (c) Ericsson AB 2014-2016 All rights reserved.
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
 * R2A/1      2014-08-26 erarafo     First version
 * R2A/2      2014-09-18 erarafo     Extended functionality
 * R2A/3      2014-09-23 erarafo     Increased code reuse
 * R3A/1      2015-01-08 erarafo     Fetch entire environment
 * R5A/1      2015-12-13 erarafo     APPLOG log levels
 * R6A/1      2016-06-20 erarafo     Query struct version
 * R6A/2      2016-07-11 etxpeno     Coverity fix
 */

#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif

#include "master.h"
#include "appm_lmhi.h"

#include <unistd.h>
#include <stdlib.h>

#define HELLO_WORLD 1
#define GET_SELECTED_ENV 2
#define GET_PATH 3
#define GET_WORKING_DIR 4
#define GET_ENTIRE_ENV 5
#define UDP_DUMP_START 6
#define UDP_DUMP_STOP 7

#define GET_APPLOG_LEVEL 11
#define SET_APPLOG_LEVEL 12

#define GET_STRUCT_VERSION 21

#define STREQ(A, B) (strcmp(A, B) == 0)

/**
 * Handles a call from the test suite.
 *
 * Do not bother deallocating memory that the envExpand()
 * calls bind.
 */
ei_x_buff send_sig_self(int function, ei_x_buff request) {

  int nArgs;
  ei_decode_tuple_header(request.buff, &request.index, &nArgs);

  APPLOG("send_sig_self, function: %d, n. of args: %d", function, nArgs);

  if (function == HELLO_WORLD) {

    int type;
    int size;
    ei_get_type(request.buff, &request.index, &type, &size);
    APPLOG("type info; type: %d, size: %d", type, size);
    if (type != ERL_ATOM_EXT) {
      APPLOG("unexpected type: %d", type);
    }
    char atomName[size+1];
    ei_decode_atom(request.buff, &request.index, atomName);
    APPLOG("argument: %s", atomName);

    ei_x_buff response;
    ei_x_new(&response);
    ei_x_format(&response, "{ok, {hello, world}}");
    return response;
  }
  else if (function == GET_SELECTED_ENV) {
    ei_x_buff response;
    ei_x_new(&response);
    ei_x_format(
        &response,
        "{ok, [{~s, ~s}, {~s, ~s}, {~s, ~s}, {~s, ~s}, {~s, ~s}, {~s, ~s}, {~s, ~s}, {~s, ~s}, {~s, ~s}, {~s, ~s}]}",
        "APP_TMP", envExpand("$APP_TMP"),
        "BT", envExpand("$BT"),
        "CXP_REV", envExpand("$CXP_REV"),
        "CXP_PATH", envExpand("$CXP_PATH"),
        "CXC_NAME", envExpand("$CXC_NAME"),
        "CXC_NO", envExpand("$CXC_NO"),
        "CXC_REV", envExpand("$CXC_REV"),
        "LD_LIBRARY_PATH", envExpand("$LD_LIBRARY_PATH"),
        "LOG_DIR", envExpand("$LOG_DIR"),
        "RESTART_TYPE", envExpand("$RESTART_TYPE")
        );
    return response;
  }
  else if (function == GET_PATH) {
    ei_x_buff response;
    ei_x_new(&response);
    ei_x_format(&response, "{ok, {~s, ~s}}", "PATH", envExpand("$PATH"));
    return response;
  }
  else if (function == GET_WORKING_DIR) {
    char *cwd = getcwd(NULL, 0);
    ei_x_buff response;
    ei_x_new(&response);
    ei_x_format(&response, "{ok, {~s, ~s}}", "cwd", cwd);
    free(cwd);
    return response;
  }
  else if (function == GET_ENTIRE_ENV) {
    long int envSize = 0;
    for (char **p = environ; *p != NULL; p++) {
      envSize++;
    }

    ei_x_buff response;
    ei_x_new(&response);
    ei_x_encode_version(&response);
    ei_x_encode_tuple_header(&response, 2);
    ei_x_encode_atom(&response, "ok");
    ei_x_encode_list_header(&response, envSize);
    for (char **p = environ; *p != NULL; p++) {
      ei_x_encode_string(&response, *p);
    }
    ei_x_encode_empty_list(&response);
    return response;
  }
  else if (function == UDP_DUMP_START) {
    // parameters: port :: string()
    char *port = decodeString(&request);

    ei_x_buff response;
    ei_x_new(&response);
    uint32_t duId = 0;
    uint32_t cpuSet = 0;
    char *lmId = getenv("FAKE_CXC_NO");
    if (lmId == NULL) {
      free(port);
      ei_x_format(&response, "{error, missing_cxc_no}");
      return response;
    }
    char *pgmName = "udpdump_dyn";
    char *const lmhi_argv[] = {pgmName, port, NULL};
    LmhiStartPgmResult startPgmRes;
    LmhiResultCode r1 = Lmhi_start_pgm(duId, cpuSet, lmId, pgmName, lmhi_argv, &startPgmRes);
    free(port);
    if (r1 != LMHI_OK) {
      ei_x_format(&response, "{error, ~i}", r1);
      return response;
    }
    else {
      ei_x_format(&response, "{ok, ~i}", startPgmRes.pgmId);
      return response;
    }
  }
  else if (function == UDP_DUMP_STOP) {
    // parameters: pgmId :: integer()

    ei_x_buff response;
    ei_x_new(&response);
    uint32_t duId = 0;
    uint32_t pgmId = (uint32_t) decodeInteger(&request);
    LmhiResultCode r2 = Lmhi_stop_pgm(duId, pgmId);
    if (r2 != LMHI_OK) {
      ei_x_format(&response, "{error, ~i}", r2);
      return response;
    }
    else {
      ei_x_format(&response, "{ok}");
      return response;
    }
  }
  else if (function == GET_APPLOG_LEVEL) {
    ei_x_buff response;
    ei_x_new(&response);
    ei_x_format(&response, "{ok, ~i}", applogLevel);
    return response;
  }
  else if (function == SET_APPLOG_LEVEL) {
    ei_x_buff response;
    ei_x_new(&response);
    if (nArgs != 1) {
      ei_x_format(&response, "{error, wrong_number_of_arguments}");
      return response;
    }
    else {
      int newLevel = (int)decodeInteger(&request);
      if (newLevel < A_ERROR || newLevel > A_DEBUG) {
        ei_x_format(&response, "{error, value_out_of_range}");
        return response;
      }
      else {
        applogLevel = newLevel;
        ei_x_format(&response, "{ok}");
        return response;
      }
    }
  }
  else if (function == GET_STRUCT_VERSION) {
    char *structIsEncodedAs = envExpand("$STRUCT_IS_ENCODED_AS");
    char *result =
        STREQ(structIsEncodedAs, "OBJECT") ? "object" :
            STREQ(structIsEncodedAs, "ATTRIBUTE") ? "attribute" : "undefined";
    free(structIsEncodedAs);
    ei_x_buff response;
    ei_x_new(&response);
    ei_x_format(&response, "{ok, ~a}", result);
    return response;
  }
  else
  {
    ei_x_buff response;
    ei_x_new(&response);
    ei_x_format(&response, "{error, {unknown_function, ~i}}", function);
    return response;
  }
}
