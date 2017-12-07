/* ----------------------------------------------------------------------
 * %CCaseFile:	test_vii.c %
 * %CCaseRev:	/main/R2A/R4A/R8A/2 %
 * %CCaseDate:	2017-01-16 %
 * %CCaseDocNo: %
 * Author:	erarafo
 * Author: <name>, <e-mail address>
 *
 * Short description:
 * Tests of the VII functionality use this module.
 * ----------------------------------------------------------------------
 * %CCaseTemplateFile:	template.c %
 * %CCaseTemplateId: CCver: /main/1 %
 *
 * %CCaseCopyrightBegin%
 * Copyright (c) Ericsson AB 2012-2017 All rights reserved.
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
 * R4A/1      2015-11-25 erarafo     Wrappers for TRI trace macros
 * R8A/1      2017-01-15 erarafo     Fix stack corruption (VRCS64 issue)
 * R8A/2      2017-01-16 erarafo     ClearCase version embedded in executable
 * ----------------------------------------------------------------------
 */

#include "master.h"
#include "cello_vii.h"

#include "cello_te_trace.h"

#define CelloVii_visualIndRequest_no 1
#define CelloVii_visualIndGet_no 2

char *test_vii_version = "test_vii.c %CCaseRev:	/main/R2A/R4A/R8A/2 %";

extern bool TRI_trace_enabled;

ei_x_buff
send_sig_vii(int func, ei_x_buff args) {
  CelloViiResult_e celloViiResult;
  ei_x_buff resp;

  QTRACE3(1, "send_sig_vii func = %d\n", func);

  ei_x_new(&resp);

  switch (func) {
  case CelloVii_visualIndRequest_no:
    {
      QTRACE2(1, "visualIndRequest");

      ei_decode_tuple_header(args.buff, &args.index, NULL);
      long indicationL;
      ei_decode_long(args.buff, &args.index, &indicationL);
      CelloViiCommand_e indication = (CelloViiCommand_e)indicationL;
      celloViiResult = CelloVii_visualIndRequest(indication);

      ei_x_format(&resp, "{ok, ~i}", celloViiResult);
      return resp;
    }
  case CelloVii_visualIndGet_no:
    {
      QTRACE2(1, "visualIndGet");

      ei_decode_tuple_header(args.buff, &args.index, NULL);
      long led_typeL;
      ei_decode_long(args.buff, &args.index, &led_typeL);
      CelloViiLed_e led_type = (CelloViiLed_e)led_typeL;
      CelloViiLedState_e led_state;
      celloViiResult = CelloVii_visualIndGet(led_type, &led_state);

      if (celloViiResult == CELLO_VII_SUCCESS) {
	ei_x_format(&resp, "{ok, ~i}", led_state);
      } else {
	ei_x_format(&resp, "{error, ~i}", celloViiResult);
      }

      return resp;
    }
  }

  ei_x_format(&resp, "{ok, function_does_not_exist}");
  return resp;
}
