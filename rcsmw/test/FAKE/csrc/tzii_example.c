/* ----------------------------------------------------------------------
 * %CCaseFile:	tzii_example.c %
 * %CCaseRev:	/main/R4A/3 %
 * %CCaseDate:	2015-11-15 %
 * %CCaseDocNo: %
 * Author:	erarafo
 * Author: <name>, <e-mail address>
 *
 * Short description: Example program using the TZI interface.
 * <Some rows here>
 * ----------------------------------------------------------------------
 * %CCaseTemplateFile:	template.c %
 * %CCaseTemplateId: CCver: /main/1 %
 *
 * %CCaseCopyrightBegin%
 * Copyright (c) Ericsson AB 2015 All rights reserved.
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
 * R4A/1      2015-11-11 erarafo     First version
 * R4A/3      2015-11-15 erarafo     IWD version
 * ----------------------------------------------------------------------
 */

/**
 * This program demonstrates use of the TZI service on RBS CS. It can
 * be run as a local, central or dynamic program on a target node or
 * the simulator.
 *
 * Before running the program the TimeSettings MO must be configured.
 *
 * To see trace messages, do `te config +all tzii_user'.
 *
 * The program runs forever, listening for TZI signals. By sending a
 * USR1 signal to the Unix process the "terminate service" sequence
 * will be invoked, after which the program goes idle.
 */

#include <osetypes.h>
#include <cello_te_ose.h>
#include <cello_te_trace.h>

#include <unistd.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <signal.h>

#include "cello_tzii.h"
#include "cello_tzii.sig"

union SIGNAL {
  SIGSELECT                                sigNo;
  CelloTziiServerUpInd                     tziiServerUpInd;
  CelloTziiInitiateServiceCfm              tziiInitServCfm;
  CelloTziiInitiateServiceSus              tziiInitServSus;
  CelloTziiSubscribeDaylightSavingTimeCfm  tziiSubsDstCfm;
  CelloTziiSubscribeDaylightSavingTimeRej  tziiSubsDstRej;
  CelloTziiSubscribeDaylightSavingTimeInd  tziiSubsDstInd;
  CelloTziiSubscribeLeapSecondsCfm         tziiSubsLeapSecCfm;
  CelloTziiSubscribeLeapSecondsRej         tziiSubsLeapSecRej;
  CelloTziiSubscribeLeapSecondsInd         tziiSubsLeapSecInd;
  CelloTziiTerminateServiceCfm             tziiTermServCfm;
};

static char *signalName(SIGSELECT sigNo) {
  switch (sigNo) {
  case CELLO_TZII_SERVER_DOWN_IND:
    return "CELLO_TZII_SERVER_DOWN_IND";
  case CELLO_TZII_SERVER_UP_IND:
    return "CELLO_TZII_SERVER_UP_IND";
  case CELLO_TZII_INITIATE_SERVICE_CFM:
    return "CELLO_TZII_INITIATE_SERVICE_CFM";
  case CELLO_TZII_INITIATE_SERVICE_SUS:
    return "CELLO_TZII_INITIATE_SERVICE_SUS";
  case CELLO_TZII_INITIATE_SERVICE_REJ:
    return "CELLO_TZII_INITIATE_SERVICE_REJ";
  case CELLO_TZII_SERVER_UNPUBLISH_IND:
    return "CELLO_TZII_SERVER_UNPUBLISH_IND";
  case CELLO_TZII_TERMINATE_SERVICE_CFM:
    return "CELLO_TZII_TERMINATE_SERVICE_CFM";
  case CELLO_TZII_SUBSCRIBE_DAYLIGHT_SAVING_TIME_CFM:
    return "CELLO_TZII_SUBSCRIBE_DAYLIGHT_SAVING_TIME_CFM";
  case CELLO_TZII_SUBSCRIBE_DAYLIGHT_SAVING_TIME_REJ:
    return "CELLO_TZII_SUBSCRIBE_DAYLIGHT_SAVING_TIME_REJ";
  case CELLO_TZII_SUBSCRIBE_DAYLIGHT_SAVING_TIME_IND:
    return "CELLO_TZII_SUBSCRIBE_DAYLIGHT_SAVING_TIME_IND";
  case CELLO_TZII_SUBSCRIBE_LEAP_SECONDS_CFM:
    return "CELLO_TZII_SUBSCRIBE_LEAP_SECONDS_CFM";
  case CELLO_TZII_SUBSCRIBE_LEAP_SECONDS_REJ:
    return "CELLO_TZII_SUBSCRIBE_LEAP_SECONDS_REJ";
  case CELLO_TZII_SUBSCRIBE_LEAP_SECONDS_IND:
    return "CELLO_TZII_SUBSCRIBE_LEAP_SECONDS_IND";
  default:
    return "UNKNOWN_SIGNAL";
  }
}

static enum {RUNNING, TERMINATE, DONE} terminate = RUNNING;

static void handleUsr1(__attribute__((unused)) int unixSigNo) {
  terminate = TERMINATE;
}

OS_PROCESS(tzii_user) {
  uint32_t myDstPrewarn = 2000;
  uint32_t myLeapPrewarn = 3000;
  uint32_t myDstInfo = 4711;
  uint32_t myLeapInfo = 4712;
  void *proxy_pp[] = {NULL};
  CelloTziiResult result;

  proxy_pp[0] = CelloTzii_initiateMemory();
  INFO("initiateMemory done");

  result = CelloTzii_initiateService(
      proxy_pp[0],
      CELLO_TZII_PV1,
      CELLO_TZII_NO_PV,
      CELLO_TZII_NO_PV);
  if (result != CELLO_TZII_OK) {
    TRACE_ERROR(STR("initiateService failed, reason: %u", result));
  }
  else {
    INFO("initiateService successful");
  }

  for(; true;) {
    if (terminate == TERMINATE) {
      result = CelloTzii_terminateService(proxy_pp[0]);
      if (result != CELLO_TZII_OK) {
        TRACE_ERROR(STR("terminateService failed, reason: %u", result));
      }
      else {
        INFO("terminateService successful");
        terminate = DONE;
      }
    }

    SIGSELECT anySig[] = {0};
    uint32_t timeoutMillis = 1000;
    union SIGNAL *sig_p = receive_w_tmo(timeoutMillis, anySig);
    if (sig_p != NULL) {
      switch(sig_p->sigNo) {
      case CELLO_TZII_SERVER_UP_IND:
        TRACE_REC_SIG(sig_p,
            STR("signal: 0x%x, %s, "
                "server is up",
                sig_p->sigNo,
                signalName(sig_p->sigNo)));
        result = CelloTzii_internal(proxy_pp[0], sig_p);
        if (result != CELLO_TZII_OK) {
          TRACE_ERROR(STR("internal failed, reason: %u", result));
        }
        break;

      case CELLO_TZII_INITIATE_SERVICE_CFM:
        TRACE_REC_SIG(sig_p,
            STR("signal: 0x%x, %s, "
                "service confirmed, "
                "protocol version: %u",
                sig_p->sigNo,
                signalName(sig_p->sigNo),
                sig_p-> tziiInitServCfm.selectedPV));
        result = CelloTzii_internal(proxy_pp[0], sig_p);
        if (result != CELLO_TZII_OK) {
          TRACE_ERROR(STR("internal failed, reason: %u", result));
        }

        struct sigaction usr1Action;
        usr1Action.sa_handler = &handleUsr1;
        usr1Action.sa_flags = 0;
        sigemptyset(&(usr1Action.sa_mask));
        int r = sigaction(SIGUSR1, &usr1Action, NULL);
        if (r != 0) {
          TRACE_ERROR("sigaction failed");
        }
        else {
          INFO(STR("to terminate service, do 'kill -s USR1 %d'", getpid()));
        }
        result = CelloTzii_subscribeDaylightSavingTime(proxy_pp[0], myDstInfo,
            myDstPrewarn);
        if (result != CELLO_TZII_OK) {
          TRACE_ERROR(
              STR("subscribeDaylightSavingTime failed, reason: %u", result));
        }
        result = CelloTzii_subscribeLeapSeconds(proxy_pp[0], myLeapInfo,
            myLeapPrewarn);
        if (result != CELLO_TZII_OK) {
          TRACE_ERROR(STR("subscribeLeapSeconds failed, reason: %u", result));
        }
        break;

      case CELLO_TZII_SUBSCRIBE_DAYLIGHT_SAVING_TIME_CFM:
        TRACE_REC_SIG(sig_p,
            STR("signal: 0x%x, %s, "
                "subscribe to DST confirmed, "
                "time offset to UTC: %d, "
                "DST offset: %d, "
                "DST: %s",
                sig_p->sigNo,
                signalName(sig_p->sigNo),
                sig_p-> tziiSubsDstCfm.timeOffsetToUtc,
                sig_p-> tziiSubsDstCfm.daylightSavingTimeOffset,
                sig_p-> tziiSubsDstCfm.daylightSavingTimeOn == TRUE ?
                    "on" :
                    "off"));
        break;


      case CELLO_TZII_SUBSCRIBE_DAYLIGHT_SAVING_TIME_REJ:
        TRACE_REC_SIG(sig_p,
            STR("signal: 0x%x, %s, "
                "subscribe to DST rejected, reason: %u",
                sig_p->sigNo,
                signalName(sig_p->sigNo),
                sig_p-> tziiSubsDstRej.rejectCause));
        break;

      case CELLO_TZII_SUBSCRIBE_DAYLIGHT_SAVING_TIME_IND:
        TRACE_REC_SIG(sig_p,
            STR("signal: 0x%x, %s, "
                "DST changes in %u ms, DST will be: %s",
                sig_p->sigNo,
                signalName(sig_p->sigNo),
                myDstPrewarn,
                sig_p->tziiSubsDstInd.daylightSavingTimeOn == TRUE ?
                    "on" :
                    "off"));
        break;

      case CELLO_TZII_SUBSCRIBE_LEAP_SECONDS_CFM:
        TRACE_REC_SIG(sig_p,
            STR("signal: 0x%x, %s, "
                "subscribe to Leap Seconds confirmed, "
                "current GPS to UTC leap seconds: %d",
                sig_p->sigNo,
                signalName(sig_p->sigNo),
                sig_p-> tziiSubsLeapSecCfm.leapSeconds));
        break;

      case CELLO_TZII_SUBSCRIBE_LEAP_SECONDS_REJ:
        TRACE_REC_SIG(sig_p,
            STR("signal: 0x%x, %s, "
                "subscribe to leap seconds rejected, reason: %u",
                sig_p->sigNo,
                signalName(sig_p->sigNo),
                sig_p-> tziiSubsLeapSecRej.rejectCause));
        break;

      case CELLO_TZII_SUBSCRIBE_LEAP_SECONDS_IND:
        TRACE_REC_SIG(sig_p,
            STR("signal: 0x%x, %s, "
                "leap seconds value changes in %u ms, new value will be: %d",
                sig_p->sigNo,
                signalName(sig_p->sigNo),
                myLeapPrewarn,
                sig_p->tziiSubsLeapSecInd.leapSeconds));
        break;

      case CELLO_TZII_TERMINATE_SERVICE_CFM:
        TRACE_REC_SIG(sig_p,
            STR("signal: 0x%x, %s, "
                "terminate service confirmed",
                sig_p->sigNo,
                signalName(sig_p->sigNo)));
        result = CelloTzii_internal(proxy_pp[0], sig_p);
        if (result != CELLO_TZII_OK) {
          TRACE_ERROR(STR("internal failed, reason: %u", result));
        }
        result = CelloTzii_freeMemory(proxy_pp);
        if (result != CELLO_TZII_OK) {
          TRACE_ERROR(STR("freeMemory failed, reason: %u", result));
        }
        else {
          INFO("freeMemory done");
        }
        for (; true;) {
          INFO("idle");
          sleep(60);
        }
        break;

      default:
        TRACE_ERROR(STR("unknown signal received, number: 0x%x", sig_p->sigNo));
      }
      free_buf(&sig_p);
    }
  }
}
