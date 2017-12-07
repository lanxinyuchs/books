/* ----------------------------------------------------------------------
 * %CCaseFile:	test_oei.c %
 * %CCaseRev:	/main/R2A/R4A/R11A/10 %
 * %CCaseDate:	2017-10-11 %
 * %CCaseDocNo: %
 * Author:	erarafo
 * Author: <name>, <e-mail address>
 *
 * Short description:
 * TBD
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
 * R4A/1      2015-11-26 erarafo     Wrappers around TRI trace macros
 * R11A/1     2017-08-25 elarrun     Added signals for OEIv2
 * ----------------------------------------------------------------------
 */
#include "master.h"
#include "cello_oei.h"
#include "rcs_oei_2.h"

#include "cello_te_trace.h"

#define CelloOei_initiateMemory_no   1
#define CelloOei_getEventIdentity_no 2
#define RcsOei_getEventIdentity_no   3
#define RcsOei_getDerivedCorrelationUUID_no 4
#define RcsOei_getDerivedCorrelationUUID2_no 5
#define RcsOei_getRcsOeiUUID_no 6

static union SIGNAL* oeiService_p; // assume just one active client for now

extern bool TRI_trace_enabled;

ei_x_buff
send_sig_oei(int func, ei_x_buff args) {
  ei_x_buff resp;

  QTRACE3(1, "send_sig_oei func = %d\n", func);

  ei_x_new(&resp);

  switch (func) {
  case CelloOei_initiateMemory_no:
    oeiService_p = CelloOei_initiateMemory();
    ei_x_format(&resp, "{ok, memory_initiated}");
    return resp;
  case CelloOei_getEventIdentity_no:
    {
      CelloOeiResult celloOeiResult;
      U32 timeoutValue, eventId;

      ei_decode_tuple_header(args.buff, &args.index, NULL);
      ei_decode_ulong(args.buff, &args.index,
                      (long unsigned int *)&timeoutValue);

      celloOeiResult = CelloOei_getEventIdentity(oeiService_p,
						 timeoutValue,
						 &eventId);
      ei_x_format(&resp, "{ok, ~i, ~i}", (unsigned int)celloOeiResult,
		  (unsigned int)eventId);
      return resp;
    }
  case RcsOei_getEventIdentity_no:
    {
      RcsOeiResult_t rcsOeiResult;
      U32 timeoutValue, eventId;

      ei_decode_tuple_header(args.buff, &args.index, NULL);
      ei_decode_ulong(args.buff, &args.index,
                      (long unsigned int *)&timeoutValue);

      rcsOeiResult = rcsOeiGetEventIdentity(timeoutValue,
					      &eventId);
      ei_x_format(&resp, "{ok, ~i, ~i}", (unsigned int)rcsOeiResult,
		  (unsigned int)eventId);
      return resp;
    }
  case RcsOei_getDerivedCorrelationUUID_no:
    {
      RcsOeiResult_t rcsOeiResult;
      U32 eventId, timeoutValue;
      RcsOeiDerivedCorrelationUuid_t derivedCorrelationUUID;

      ei_decode_tuple_header(args.buff, &args.index, NULL);
      ei_decode_ulong(args.buff, &args.index,
                      (long unsigned int *)&eventId);
      ei_decode_ulong(args.buff, &args.index,
                      (long unsigned int *)&timeoutValue);

      rcsOeiResult = rcsOeiGetDerivedCorrelationUUID(eventId,
                                                     timeoutValue,
                                                     &derivedCorrelationUUID);

      ei_x_format(&resp, "{ok, ~i, ~s}", (unsigned int)rcsOeiResult,
		  derivedCorrelationUUID.value);
      return resp;
    }
  case RcsOei_getDerivedCorrelationUUID2_no:
    {
      RcsOeiResult_t rcsOeiResult;
      U32 eventId, timeoutValue;
      RcsOeiSystemUuid_t systemUUID;
      RcsOeiDerivedCorrelationUuid_t derivedCorrelationUUID;

      ei_decode_tuple_header(args.buff, &args.index, NULL);
      ei_decode_ulong(args.buff, &args.index,
                      (long unsigned int *)&eventId);
      ei_decode_ulong(args.buff, &args.index,
                      (long unsigned int *)&timeoutValue);
      ei_decode_string(args.buff, &args.index, systemUUID.value);

      rcsOeiResult = rcsOeiGetDerivedCorrelationUUID2(eventId,
                                                      timeoutValue,
                                                      &systemUUID,
                                                      &derivedCorrelationUUID);

      ei_x_format(&resp, "{ok, ~i, ~s}", (unsigned int)rcsOeiResult,
		  derivedCorrelationUUID.value);
      return resp;
    }
  case RcsOei_getRcsOeiUUID_no:
    {
      RcsOeiResult_t rcsOeiResult;
      RcsOeiUuid_t rcsOeiUUID;

      rcsOeiResult = rcsOeiGetUUID(&rcsOeiUUID);
      ei_x_format(&resp, "{ok, ~i, ~s}", (unsigned int)rcsOeiResult,
		  rcsOeiUUID.value);
      return resp;
    }
  }

  ei_x_format(&resp, "{ok, function_does_not_exist}");
  return resp;
}
