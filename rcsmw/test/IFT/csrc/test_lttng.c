/* ----------------------------------------------------------------------
 * %CCaseFile:	test_lttng.c %
 * %CCaseRev:	/main/R2A/3 %
 * %CCaseDate:	2014-02-27 %
 * %CCaseDocNo: %
 * Author:	erarafo
 *
 * Short description:
 * Start and stop of the dynamic test_trace program.
 *
 * ----------------------------------------------------------------------
 * %CCaseTemplateFile:	template.c %
 * %CCaseTemplateId: CCver: /main/1 %
 *
 * %CCaseCopyrightBegin%
 * Copyright (c) Ericsson AB 2014 All rights reserved.
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
 * R2A/1      2014-02-18 erarafo     Created
 * R2A/2      2014-02-19 erarafo     STOP function takes PID as parameter
 * R2A/3      2014-02-27 erarafo     Comments only
 * ----------------------------------------------------------------------
 */

#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif

#include "master.h"
#include "appm_lmhi.h"

// #include "test_avc.h" TODO, reuse this pattern maybe
#define LTTNG_TEST_TRACE_START 1
#define LTTNG_TEST_TRACE_STOP 2

//#define STREQ(A, B) strcmp((const char *)A, B) == 0


static LmhiStartPgmResult startPgmRes;


/**
 * Handles a call from the test suite towards the CS.
 */
ei_x_buff send_sig_lttng(int function, ei_x_buff request) {

  APPLOG("send_sig_lttng, function: %d", function);

  int nArgs;
  ei_decode_tuple_header(request.buff, &request.index, &nArgs);

  ei_x_buff response;
  ei_x_new(&response);

  switch (function) {
  case LTTNG_TEST_TRACE_START:
  {
    uint32_t duId = 0;
    uint32_t cpuSet = 0;
    char *lmId = "LM008";
    char *pgmName = "TESTRA";
    char *const lmhi_argv[] = {"test_trace", NULL};
    LmhiResultCode r1 = Lmhi_start_pgm(duId, cpuSet, lmId, pgmName, lmhi_argv, &startPgmRes);
    if (r1 != LMHI_OK) {
      ei_x_format(&response, "{error, ~i}", r1);
      return response;
    }
    else {
      ei_x_format(&response, "{ok, ~i}", startPgmRes.pgmId);
      return response;
    }
  }

  case LTTNG_TEST_TRACE_STOP:
  {
    unsigned long long pid = decodeHandle(&request);
    LmhiStartPgmResult res = {.pgmId=(uint32_t)pid};
    uint32_t duId = 0;
    LmhiResultCode r2 = Lmhi_stop_pgm(duId, res.pgmId);
    if (r2 != LMHI_OK) {
      ei_x_format(&response, "{error, ~i}", r2);
      return response;
    }
    else {
      ei_x_format(&response, "{ok}");
      return response;
    }
  }

  default:
    ei_x_format(&response, "{error, {unknown_function, ~i}}", function);
    return response;
  }
}
