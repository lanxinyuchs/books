/* ----------------------------------------------------------------------
 * %CCaseFile:	test_tzii.c %
 * %CCaseRev:	/main/R4A/R5A/R7A/2 %
 * %CCaseDate:	2016-09-20 %
 * %CCaseDocNo: %
 * Author:	erarafo
 * Author: <name>, <e-mail address>
 *
 * Short description: Tests of the TIM service.
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
 * R4A/1      2015-08-31 erarafo     First version
 * R4A/2      2015-09-02 erarafo     Name change, more functionality
 * R4A/3      2015-09-04 erarafo     Receiving signals
 * R4A/5      2015-09-08 erarafo     Comments added
 * R4A/6      2015-09-08 erarafo     Handling all signals properly
 * R4A/7      2015-09-08 erarafo     Fault corrected
 * R4A/8      2015-09-11 erarafo     Delivering child process id upstream
 * R4A/9      2015-10-13 erarafo     Negative tests support
 * R4A/10     2015-10-15 erarafo     Version request added
 * R4A/12     2015-11-06 erarafo     Support for setMailbox operation added
 * R4A/13     2015-11-13 erarafo     Support for start/stop of code example
 * R4A/14     2015-11-16 erarafo     Support for setMailbox tests
 * R5A/1      2015-12-13 erarafo     Memory leak fixed
 * R5A/2      2016-02-12 erarafo     Evil fall-through in switch fixed
 * R7A/1      2016-09-19 erarafo     Support for fetching board time
 * R7A/2      2016-09-20 erarafo     Support for reporting uptime too
 * ----------------------------------------------------------------------
 */

#define TEST_TZII_VERSION "%CCaseRev:	/main/R4A/R5A/R7A/2 %"

#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif

#include "master.h"
#include "appm_lmhi.h"
#include "cello_tzii.h"
#include "cello_tzii.sig"
#include "test_tzii_requests.h"

#include <utmp.h>
#include <time.h>

union SIGNAL {
  SIGSELECT sigNo;
  CelloTziiServerUpInd servUpInd;
  CelloTziiServerDownInd servDownInd;
  CelloTziiInitiateServiceCfm initServCfm;
  CelloTziiInitiateServiceRej initServRej;
  CelloTziiInitiateServiceSus initServSus;
  CelloTziiTerminateServiceCfm termServCfm;
  CelloTziiServerUnpublishInd servUnpubInd;
  CelloTziiSubscribeDaylightSavingTimeCfm subscrDstCfm;
  CelloTziiSubscribeDaylightSavingTimeRej subscrDstRej;
  CelloTziiSubscribeDaylightSavingTimeInd subscrDstInd;
  CelloTziiSubscribeLeapSecondsCfm subscrLeapSecsCfm;
  CelloTziiSubscribeLeapSecondsRej subscrLeapSecsRej;
  CelloTziiSubscribeLeapSecondsInd subscrLeapSecsInd;
};


static LmhiStartPgmResult startPgmRes;

/**
 * Handle a downstream request (from application towards server).
 */
ei_x_buff send_sig_tzii(
    void **tziiMemory_pp,
    union SIGNAL *sig_p,
    int function,
    ei_x_buff args) {

  APPLOG("send_sig_tzii, function: %d", function);
  ei_x_buff resp;
  ei_x_new(&resp);

  switch (function) {

  case iftRequest_boardTime: {
    struct utmp buffer;

    long uptimeSeconds = -1;

    FILE *utmp = fopen("/var/run/utmp", "r");
    if (utmp != NULL) {
      uptimeSeconds = -2;
      while (true) {
        int nItems = fread(&buffer, sizeof(struct utmp), 1, utmp);
        if (nItems != 1) {
          break;
        }
        else if (buffer.ut_type == BOOT_TIME) {
          long bootTimeSeconds = buffer.ut_tv.tv_sec;
          long nowSeconds = time(NULL);
          uptimeSeconds = (nowSeconds == -1) ? -3 : nowSeconds - bootTimeSeconds;
          break;
        }
      }
      fclose(utmp);
    }

    struct timespec ts;
    if (clock_gettime(CLOCK_REALTIME, &ts) != 0) {
      ei_x_format(&resp, "{error, {gettime, ~i}}", errno);
    }
    else {
      ei_x_format(&resp, "{ok, ~l, ~l, ~l}", ts.tv_sec, ts.tv_nsec, uptimeSeconds);
      return resp;
    }
  }

  case iftRequest_version: {
    ei_x_format(&resp, "{ok, ~s}", TEST_TZII_VERSION);
    return resp;
  }

  case iftRequest_initiateMemory: {
    *tziiMemory_pp = CelloTzii_initiateMemory();
    ei_x_format(&resp, "{ok, memory_initiated}");
    //APPLOG(">>>> %u initiate: %p contains: %p", myProcess, (void *)tziiMemory_pp, *tziiMemory_pp);
    return resp;
  }

  case iftRequest_freeMemory: {
    CelloTziiResult r = CelloTzii_freeMemory(tziiMemory_pp);
    ei_x_format(&resp, "{ok, ~i}", r);
    return resp;
  }

  case iftRequest_initiateService:
  {
    int arity;
    ei_decode_tuple_header(args.buff, &(args.index), &arity);
    if (arity != 3) {
      ei_x_format(&resp, "{error, {bad_arity, ~i}}", arity);
      return resp;
    }
    else {
      unsigned long wantedPv1;
      ei_decode_ulong(args.buff, &(args.index), &wantedPv1);
      unsigned long wantedPv2;
      ei_decode_ulong(args.buff, &(args.index), &wantedPv2);
      unsigned long wantedPv3;
      ei_decode_ulong(args.buff, &(args.index), &wantedPv3);
      CelloTziiResult r =
          CelloTzii_initiateService(*tziiMemory_pp, wantedPv1, wantedPv2, wantedPv3);
      ei_x_format(&resp, "{ok, ~u}", r);
      return resp;
    }
  }

  case iftRequest_terminateService: {
    // APPLOG("send_sig_tzii, function: %d, about to call CelloTzii_terminateService", function);
    CelloTziiResult r =
        CelloTzii_terminateService(*tziiMemory_pp);
    APPLOG("send_sig_tzii, function: %d, CelloTzii_terminateService() -> %u", function, r);
    ei_x_format(&resp, "{ok, ~u}", r);
    return resp;
  }

  case iftRequest_internal: {
    CelloTziiResult r =
        CelloTzii_internal(*tziiMemory_pp, sig_p);
    free_buf(&sig_p);
    ei_x_format(&resp, "{ok, ~u}", r);
    return resp;
  }

  case iftRequest_internalForPeer: {
    int arity;
    ei_decode_tuple_header(args.buff, &(args.index), &arity);
    if (arity != 1) {
      ei_x_format(&resp, "{error, {bad_arity, ~i}}", arity);
      return resp;
    }
    else {
      unsigned long peerSpid;
      ei_decode_ulong(args.buff, &(args.index), &peerSpid);
      void *peerProxy_p = getPeerProxyPointer((PROCESS)peerSpid);
      if (peerProxy_p == NULL) {
        ei_x_format(&resp, "{error, {no_peer_proxy, ~u}}", peerSpid);
        return resp;
      }
      else {
        CelloTziiResult r =
            CelloTzii_internal(peerProxy_p, sig_p);
        free_buf(&sig_p);
        ei_x_format(&resp, "{ok, ~u}", r);
        return resp;
      }
    }
  }

  case iftRequest_subscribeDaylightSavingTime: {
    int arity;
    ei_decode_tuple_header(args.buff, &(args.index), &arity);
    if (arity != 2) {
      ei_x_format(&resp, "{error, {bad_arity, ~i}}", arity);
      return resp;
    }
    else {
      unsigned long clientInfo;
      ei_decode_ulong(args.buff, &(args.index), &clientInfo);
      unsigned long preWarningTime;
      ei_decode_ulong(args.buff, &(args.index), &preWarningTime);
      CelloTziiResult r =
          CelloTzii_subscribeDaylightSavingTime(
              *tziiMemory_pp,
              clientInfo,
              preWarningTime);
      ei_x_format(&resp, "{ok, ~u}", r);
      return resp;
    }
  }

  case iftRequest_subscribeLeapSeconds: {
    int arity;
    ei_decode_tuple_header(args.buff, &(args.index), &arity);
    if (arity != 2) {
      ei_x_format(&resp, "{error, {bad_arity, ~i}}", arity);
      return resp;
    }
    else {
      unsigned long clientInfo;
      ei_decode_ulong(args.buff, &(args.index), &clientInfo);
      unsigned long preWarningTime;
      ei_decode_ulong(args.buff, &(args.index), &preWarningTime);
      CelloTziiResult r = CelloTzii_subscribeLeapSeconds(
          *tziiMemory_pp,
          clientInfo,
          preWarningTime);
      ei_x_format(&resp, "{ok, ~u}", r);
      return resp;
    }
  }

  case iftRequest_startExample: {
    uint32_t duId = 0;
    uint32_t cpuSet = 0;
    char *lmId = getenv("FAKE_CXC_NO");
    if (lmId == NULL) {
      ei_x_format(&resp, "{error, missing_cxc_no}");
      return resp;
    }
    char *pgmName = "TZIEXA";
    char *const lmhi_argv[] = {"tziiExample", NULL};
    LmhiResultCode r1 = Lmhi_start_pgm(duId, cpuSet, lmId, pgmName, lmhi_argv, &startPgmRes);
    if (r1 != LMHI_OK) {
      ei_x_format(&resp, "{error, cannot_start, ~i}", r1);
      return resp;
    }
    else {
      ei_x_format(&resp, "{ok, ~i}", startPgmRes.pgmId);
      return resp;
    }
  }

  case iftRequest_stopExample: {
    uint32_t duId = 0;
    LmhiResultCode r2 = Lmhi_stop_pgm(duId, startPgmRes.pgmId);
    if (r2 != LMHI_OK) {
      ei_x_format(&resp, "{error, cannot_stop, ~i}", r2);
      return resp;
    }
    else {
      ei_x_format(&resp, "{ok}");
      return resp;
    }
  }

  case iftRequest_getPid: {
    ei_x_format(&resp, "{ok, ~u}", current_process());
    return resp;
  }

  case iftRequest_internalBad: {
    if (sig_p->termServCfm.sigNo == CELLO_TZII_TERMINATE_SERVICE_CFM) {
      union SIGNAL *mod_p =
          (union SIGNAL *)calloc(1, sizeof(CelloTziiTerminateServiceCfm));

      // make this an unknown signal
      mod_p->termServCfm.sigNo = CELLO_TZII_TERMINATE_SERVICE_CFM + 777;

      CelloTziiResult r =
          CelloTzii_internal(*tziiMemory_pp, mod_p);
      ei_x_format(&resp, "{ok, ~u}", r);
      free((void *)mod_p);
      return resp;
    }
    else {
      // default action, just like iftRequest_internal
      CelloTziiResult r =
          CelloTzii_internal(*tziiMemory_pp, sig_p);
      free_buf(&sig_p);
      ei_x_format(&resp, "{ok, ~u}", r);
      return resp;
    }
  }

  case iftRequest_setMailbox: {
    int arity;
    ei_decode_tuple_header(args.buff, &(args.index), &arity);
    if (arity != 1) {
      ei_x_format(&resp, "{error, {bad_arity, ~i}}", arity);
      return resp;
    }
    else {
      unsigned long otherMbox;
      ei_decode_ulong(args.buff, &(args.index), &otherMbox);
      CelloTzii_setMailbox(*tziiMemory_pp, otherMbox);
      ei_x_format(&resp, "{ok, ~u}", CELLO_TZII_OK);
      return resp;
    }
  }

  default: {
    ei_x_format(&resp, "{error, undefined_function, ~i}");
    return resp;
  }
  }
}


/**
 * Handle a signal (from server towards application).
 */
ei_x_buff
recv_sig_tzii(void ***proxyMemory_ppp, union SIGNAL *sig_p) {
  PROCESS mypid = current_process();
  ei_x_buff resp;
  ei_x_new(&resp);

  switch(sig_p->sigNo) {

  case CELLO_TZII_SERVER_UP_IND:
  case CELLO_TZII_TERMINATE_SERVICE_CFM:
  //case CELLO_TZII_SERVER_DOWN_IND:            ... not in G2
  //case CELLO_TZII_SERVER_UNPUBLISH_IND:       ... not in G2
    ei_x_format(&resp, "{signal, {~u, ~u}}",
        mypid,
        sig_p->sigNo);
    return resp;

  case CELLO_TZII_INITIATE_SERVICE_CFM:
    ei_x_format(&resp, "{signal, {~u, ~u, ~u, ~u}}",
        mypid,
        sig_p->initServCfm.sigNo,
        sig_p->initServCfm.signalRevision,
        sig_p->initServCfm.selectedPV);
    return resp;

//  case CELLO_TZII_INITIATE_SERVICE_REJ:       ... not in G2
//    ei_x_format(&resp, "{signal, {~u, ~u, ~u, ~u, ~u}}",
//        mypid,
//        sig_p->initServRej.sigNo,
//        sig_p->initServRej.signalRevision,
//        sig_p->initServRej.result,
//        sig_p->initServRej.highestPV);
//    return resp;

  case CELLO_TZII_INITIATE_SERVICE_SUS:
    ei_x_format(&resp, "{signal, {~u, ~u, ~u, ~u}}",
        mypid,
        sig_p->initServSus.sigNo,
        sig_p->initServSus.signalRevision,
        sig_p->initServSus.highestPV);
    return resp;

  case CELLO_TZII_SUBSCRIBE_DAYLIGHT_SAVING_TIME_CFM:
    ei_x_format(&resp, "{signal, {~u, ~u, ~u, ~i, ~i, ~u}}",
        mypid,
        sig_p->subscrDstCfm.sigNo,
        sig_p->subscrDstCfm.clientInfo,
        sig_p->subscrDstCfm.timeOffsetToUtc,
        sig_p->subscrDstCfm.daylightSavingTimeOffset,
        sig_p->subscrDstCfm.daylightSavingTimeOn);
    free_buf(&sig_p);
    return resp;

  case CELLO_TZII_SUBSCRIBE_DAYLIGHT_SAVING_TIME_REJ:
    ei_x_format(&resp, "{signal, {~u, ~u, ~u, ~u}}",
        mypid,
        sig_p->subscrDstRej.sigNo,
        sig_p->subscrDstRej.clientInfo,
        sig_p->subscrDstRej.rejectCause);
    free_buf(&sig_p);
    return resp;

  case CELLO_TZII_SUBSCRIBE_DAYLIGHT_SAVING_TIME_IND:
    ei_x_format(&resp, "{signal, {~u, ~u, ~u, ~i, ~i, ~u, ~u, ~u}}",
        mypid,
        sig_p->subscrDstInd.sigNo,
        sig_p->subscrDstInd.clientInfo,
        sig_p->subscrDstInd.timeOffsetToUtc,
        sig_p->subscrDstInd.daylightSavingTimeOffset,
        sig_p->subscrDstInd.daylightSavingTimeOn,
        sig_p->subscrDstInd.timeOfChange,
        sig_p->subscrDstInd.causeForChange);
    free_buf(&sig_p);
    return resp;

  case CELLO_TZII_SUBSCRIBE_LEAP_SECONDS_CFM:
    ei_x_format(&resp, "{signal, {~u, ~u, ~u, ~i}}",
        mypid,
        sig_p->subscrLeapSecsCfm.sigNo,
        sig_p->subscrLeapSecsCfm.clientInfo,
        sig_p->subscrLeapSecsCfm.leapSeconds);
    free_buf(&sig_p);
    return resp;

  case CELLO_TZII_SUBSCRIBE_LEAP_SECONDS_REJ:
    ei_x_format(&resp, "{signal, {~u, ~u, ~u, ~u}}",
        mypid,
        sig_p->subscrLeapSecsRej.sigNo,
        sig_p->subscrLeapSecsRej.clientInfo,
        sig_p->subscrLeapSecsRej.rejectCause);
    free_buf(&sig_p);
    return resp;

  case CELLO_TZII_SUBSCRIBE_LEAP_SECONDS_IND:
    ei_x_format(&resp, "{signal, {~u, ~u, ~u, ~i, ~u}}",
        mypid,
        sig_p->subscrLeapSecsInd.sigNo,
        sig_p->subscrLeapSecsInd.clientInfo,
        sig_p->subscrLeapSecsInd.leapSeconds,
        sig_p->subscrLeapSecsInd.causeForChange);
    free_buf(&sig_p);
    return resp;

  default:
    APPLOG("unknown signal seen, child: %u, signal number: %u", mypid, sig_p->sigNo);
    ei_x_format(&resp, "{signal, {~u, ~u}}", mypid, sig_p->sigNo);
    free_buf(&sig_p);
    return resp;
  }
}
