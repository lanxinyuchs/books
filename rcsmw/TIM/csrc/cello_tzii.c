/* ----------------------------------------------------------------------
 * %CCaseFile:	cello_tzii.c %
 * %CCaseRev:	/main/R4A/R6A/1 %
 * %CCaseDate:	2016-07-07 %
 * %CCaseDocNo: %
 * Author:	erarafo
 * Author: <name>, <e-mail address>
 *
 * Short description: Implementation of the TZII interface.
 *
 * To obtain trace, type the following in a shell on the node:
 *
 *   te config -all *
 *   te log clear
 *   te config +all *
 *
 *   < execute some test >
 *
 *   te log read | grep cello_tzii.c
 *
 * ----------------------------------------------------------------------
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
 * R4A/1      2015-08-27 erarafo     First version
 * R4A/2      2015-09-03 erarafo     Work in progress
 * R4A/3      2015-09-04 erarafo     Trivial adjustment
 * R4A/4      2015-09-04 erarafo     Corrected behavior
 * R4A/5      2015-10-20 erarafo     Using new return codes
 * R4A/6      2015-10-20 erarafo     Transient TCP connections
 * R4A/8      2015-10-23 erarafo     State machine simplified; using TRI
 * R4A/9      2015-10-29 erarafo     Responses from timServer checked
 * R4A/10     2015-11-02 erarafo     Definition of SIGNAL cannot be in header file
 * R4A/11     2015-11-05 erarafo     setMailbox support
 * R4A/12     2015-11-26 erarafo     Wrappers for TRI trace calls
 * R6A/1      2016-07-07 etxpeno     Coverity fixes
 * ----------------------------------------------------------------------
 */

#define VERSION "%CCaseRev:	/main/R4A/R6A/1 %"

#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif

#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <sys/utsname.h>

#include "ose.h"
#include "osetypes.h"
#include "cello_te_ose.h"
#include "cello_te_trace.h"
#include "cello_te_trace_obj.h"

#include "cello_tzii.h"
#include "cello_tzii.sig"

#include "cec.h"
#include "cello_tzii_internal.h"

#define STREQ(A, B) (strcmp(A, B) == 0)

#define QENTER1(A)                 if (!isSimulator()) {ENTER(A); }
#define QENTER2(F, A)              if (!isSimulator()) {ENTER(STR(F, A)); }
#define QRETURN                    if (!isSimulator()) {RETURN; } else {return; }
#define QRETURN1(R)                if (!isSimulator()) {RETURN R; } else {return R; }
#define QTRACE_ERROR1(A)           if (!isSimulator()) {TRACE_ERROR(A); }
#define QTRACE_ERROR2(F, A)        if (!isSimulator()) {TRACE_ERROR(STR(F, A)); }
#define QTRACE_STATE3(F, A, B)     if (!isSimulator()) {TRACE_STATE(STR(F, A, B)); }

/**
 * SIGNAL is the union of all signal structs in
 * cello_tzii.sig. In this module only the signal
 * number is being referred to.
 */
union SIGNAL {
   SIGSELECT sigNo;
};


/**
 * An instance of this struct gets allocated for each
 * client using the TZI service.
 */
typedef struct {
  PROCESS spidInitial;
  PROCESS spid;
  uint32_t availabilityState;
} CelloTziiProxyData;


/**
 * Returns true if running on the simulator. This function is used for disabling
 * TRI trace when running on the simulator (workaround until TRI trace is safe
 * to use everywhere).
 */
static bool isSimulator() {
  struct utsname buffer;
  int result = uname(&buffer);
  return result == 0 && STREQ(buffer.machine, "x86_64");
}


static void stateChange(CelloTziiProxyData *proxyData_p, uint32_t newState) {
  QENTER1("stateChange");
  uint32_t oldState = proxyData_p->availabilityState;
  proxyData_p->availabilityState = newState;
  QTRACE_STATE3(
      "proxy state: %s -> %s",
      stateNames[oldState],
      stateNames[newState]);
  QRETURN;
}


/**
 * Send an array of uint32_t values.
 */
static int
cecOpenSendClose(uint32_t *uint32Array, unsigned int arraySize) {
  QENTER1("cecOpenSendClose");
  char *signature = CEC_SIGNATURE;
  cec_handle_t *handle = cec_open(CEC_SIGNATURE, strlen(signature));
  if (handle == NULL) {
    QRETURN1(CEC_ERROR_OPEN);
  }
  else {
    cec_packet_t packet = {
        .length = arraySize*sizeof(uint32_t),
        .data = (void *)uint32Array
    };
    int r1 = cec_send(handle, &packet);
    if (r1 < 0) {
      cec_close(handle);
      QRETURN1(CEC_ERROR_SEND);
    }
    else {
      cec_packet_t packet;
      int r2 = cec_receive_w_tmeout(handle, &packet, 5000);
      if (r2 < 0 ) {
	free(packet.data);
	cec_close(handle);
        QRETURN1(CEC_ERROR_RECEIVE);
      }
      else if (STREQ((char *)packet.data, CEC_RESPONSE_OK)) {
        free(packet.data);
        if (cec_close(handle) < 0) {
          QRETURN1(CEC_ERROR_CLOSE_A);
        }
        else {
          QRETURN1(CEC_OK);
        }
      }
      else if (STREQ((char *)packet.data, CEC_RESPONSE_UNKNOWN)) {
        free(packet.data);
        if (cec_close(handle) < 0) {
          QRETURN1(CEC_ERROR_CLOSE_B);
        }
        else {
          QRETURN1(CEC_UNKNOWN);
        }
      }
      else {
        // unexpected response from timServer, should not happen
        free(packet.data);
        if (cec_close(handle) < 0) {
          QRETURN1(CEC_ERROR_CLOSE_C);
        }
        else {
          QRETURN1(CEC_UNEXPECTED);
        }
      }
    }
  }
}


/******************************************************************************
 *
 * Allocates the TZII data.
 * This function should be called once in each process.
 *
 * @return an TZII interface handle
 *
 *****************************************************************************/
void *
CelloTzii_initiateMemory(void) {
  QENTER2("CelloTzii_initiateMemory, cello_tzii version: %s", VERSION);
  CelloTziiProxyData *proxyData =
      (CelloTziiProxyData *)malloc(sizeof(CelloTziiProxyData));
  PROCESS self = current_process();
  proxyData->spidInitial = self;
  proxyData->spid = self;
  proxyData->availabilityState = SERVER_UNAVAILABLE;
  QRETURN1((void *)proxyData);
}

/*
  // when a higher protocol version is introduced, prepend it to this list
  static uint32_t protocolVersions[] = {CELLO_TZII_PV1, CELLO_TZII_NO_PV};
*/



CelloTziiResult
CelloTzii_initiateService(void *tziiMemory_p,
                          uint32_t pvFirstWanted,
                          uint32_t pvSecondWanted,
                          uint32_t pvThirdWanted) {
  QENTER1("CelloTzii_initiateService");
  if (tziiMemory_p == NULL) {
    // TC: test_memory_not_initiated
    QRETURN1(CELLO_TZII_MEMORY_NOT_INITIATED);
  }
  else {
    CelloTziiProxyData *proxyData_p = (CelloTziiProxyData *)tziiMemory_p;
    uint32_t proxyState = proxyData_p->availabilityState;
    if (proxyState == SERVER_INITIATING) {
      QRETURN1(CELLO_TZII_ILLEGAL_WHEN_INITIATING);
    }
    else if (proxyState == SERVER_SUSPENDED) {
      QRETURN1(CELLO_TZII_ILLEGAL_WHEN_SUSPENDED);
    }
    else if (proxyState == SERVER_TERMINATING) {
      QRETURN1(CELLO_TZII_ILLEGAL_WHEN_TERMINATING);
    }
    else if (proxyState == SERVER_AVAILABLE) {
      QRETURN1(CELLO_TZII_ALREADY_INITIATED);
    }
    else {
      // the proxy state must be UNAVAILABLE
      uint32_t buffer[] = {
          CEC_REQUEST_INIT_SERVICE,
          proxyData_p->spidInitial,
          proxyData_p->spid,
          pvFirstWanted,
          pvSecondWanted,
          pvThirdWanted};

      int r = cecOpenSendClose(buffer, 6);
      if (r == CEC_UNKNOWN) {
        QRETURN1(CELLO_TZII_UNKNOWN_CLIENT);
      }
      else if (r != CEC_OK) {
        QRETURN1(CELLO_TZII_INTERNAL_ERROR_INIT_SVC + r);
      }
      else {
        stateChange(proxyData_p, SERVER_INITIATING);
        QRETURN1(CELLO_TZII_OK);
      }
    }
  }
}


/******************************************************************************
 *
 * This function terminates the service and requests disconnection
 * from the server
 *
 * @param TZII interface handle
 *
 * @return CELLO_TZII_OK,
 *         CELLO_TZII_MEMORY_NOT_INITIATED
 *
 *****************************************************************************/
CelloTziiResult
CelloTzii_terminateService(void *tziiMemory_p) {
  QENTER1("CelloTzii_terminateService");
  if (tziiMemory_p == NULL) {
    QRETURN1(CELLO_TZII_MEMORY_NOT_INITIATED);
  }
  else {
    CelloTziiProxyData *proxyData_p = (CelloTziiProxyData *)tziiMemory_p;
    uint32_t proxyState = proxyData_p->availabilityState;
    if (proxyState == SERVER_UNAVAILABLE) {
      QRETURN1(CELLO_TZII_SERVER_UNAVAILABLE);
    }
    else if (proxyState == SERVER_INITIATING) {
      QRETURN1(CELLO_TZII_ILLEGAL_WHEN_INITIATING);
    }
    else if (proxyState == SERVER_TERMINATING) {
      QRETURN1(CELLO_TZII_ILLEGAL_WHEN_TERMINATING);
    }
    else {
      uint32_t buffer[] = {
          CEC_REQUEST_TERM_SERVICE,
          proxyData_p->spidInitial};

      int r = cecOpenSendClose(buffer, 2);

      if (r == CEC_UNKNOWN) {
        QRETURN1(CELLO_TZII_UNKNOWN_CLIENT);
      }
      else if (r != CEC_OK) {
        QRETURN1(CELLO_TZII_INTERNAL_ERROR_TERM_SVC + r);
      }
      else {
        stateChange(proxyData_p, SERVER_TERMINATING);
        QRETURN1(CELLO_TZII_OK);
      }
    }
  }
}


/******************************************************************************
 *
 * This function is a wrapper to be called by the client when any of
 * the signals
 *   CELLO_TZII_SERVER_UP_IND
 *   CELLO_TZII_INITIATE_SERVICE_CFM
 *   CELLO_TZII_INITIATE_SERVICE_SUS
 *   CELLO_TZII_TERMINATE_SERVICE_CFM
 * is received by the client.
 *
 * @param tziiMemory_p - TZII handle
 * @param sig_p - signal received by the client
 *
 * @return CELLO_TZII_OK,
 *         CELLO_TZII_ILLEGAL_SIGNAL,
 *         CELLO_TZII_MEMORY_NOT_INITIATED
 *
 *****************************************************************************/
CelloTziiResult
CelloTzii_internal(void *tziiMemory_p, union SIGNAL *sig_p) {
  QENTER1("CelloTzii_internal");
  if (tziiMemory_p == NULL) {
    QRETURN1(CELLO_TZII_MEMORY_NOT_INITIATED);
  }
  else {
    CelloTziiProxyData *proxyData_p = (CelloTziiProxyData *)tziiMemory_p;
    SIGSELECT sigNo = sig_p->sigNo;
    if (sigNo == CELLO_TZII_INITIATE_SERVICE_REJ) {
      QRETURN1(CELLO_TZII_ILLEGAL_SIGNAL);
    }
    else {
      switch (proxyData_p->availabilityState) {
      case SERVER_UNAVAILABLE:
        QRETURN1(CELLO_TZII_SERVER_UNAVAILABLE);

      case SERVER_INITIATING:
        if (sigNo == CELLO_TZII_SERVER_UP_IND) {
          uint32_t buffer[] = {
              CEC_REQUEST_PROCEED,
              proxyData_p->spidInitial
          };
          int r = cecOpenSendClose(buffer, 2);
          if (r == CEC_UNKNOWN) {
            QRETURN1(CELLO_TZII_UNKNOWN_CLIENT);
          }
          else if (r != CEC_OK) {
            QRETURN1(CELLO_TZII_INTERNAL_ERROR_INTERNAL_1 + r);
          }
        }
        else if (sigNo == CELLO_TZII_INITIATE_SERVICE_SUS) {
          stateChange(proxyData_p, SERVER_SUSPENDED);
        }
        else if (sigNo == CELLO_TZII_INITIATE_SERVICE_CFM) {
          stateChange(proxyData_p, SERVER_AVAILABLE);
        }
        else {
          QRETURN1(CELLO_TZII_ILLEGAL_SIGNAL);
        }
        QRETURN1(CELLO_TZII_OK);

      case SERVER_SUSPENDED:
        QRETURN1(CELLO_TZII_ILLEGAL_WHEN_SUSPENDED);

      case SERVER_AVAILABLE:
        QRETURN1(CELLO_TZII_ILLEGAL_WHEN_AVAILABLE);

      case SERVER_TERMINATING:
        if (sigNo == CELLO_TZII_TERMINATE_SERVICE_CFM) {
          stateChange(proxyData_p, SERVER_UNAVAILABLE);
          QRETURN1(CELLO_TZII_OK);
        }
        else {
          QRETURN1(CELLO_TZII_ILLEGAL_SIGNAL);
        }

      default:
        // cannot happen since all states are covered above
        QRETURN1(CELLO_TZII_OK);
      }
    }
  }
}



CelloTziiResult
CelloTzii_freeMemory(void **tziiMemory_p) {
  QENTER1("CelloTzii_freeMemory");
  if (*tziiMemory_p == NULL) {
    QRETURN1(CELLO_TZII_MEMORY_NOT_INITIATED);
  }
  else {
    CelloTziiProxyData *proxyData = (CelloTziiProxyData *)*tziiMemory_p;
    if (proxyData->availabilityState != SERVER_UNAVAILABLE) {
      QRETURN1(CELLO_TZII_ACTIVE_SERVICE);
    }
    else {
      free((void *)proxyData);
      *tziiMemory_p = NULL;
      QRETURN1(CELLO_TZII_OK);
    }
  }
}


/*
 ****************************  PROTOCOL VERSION 1 *****************************
 */

/******************************************************************************
 *
 * Start subscription on Daylight Saving Time (DST) information.
 *
 * If client is already subscribing on DST information, the old subscription
 * is discarded and the client will receive a new confirm signal.
 *
 * @param tziiMemory_p   - TZII handle
 * @param clientInfo     - client data returned in indication signal
 * @param preWarningTime - advance notification in milliseconds (<= 23 hours)
 *
 * @return CELLO_TZII_OK
 *         CELLO_TZII_SERVER_UNAVAILABLE
 *         CELLO_TZII_MEMORY_NOT_INITIATED
 *
 * The client will receive an indication signal with cause
 * CELLO_TZII_PREWARNING_START when:
 * - the subscription is issued during the pre-warning interval
 * - time passes into the pre-warning interval
 * - the subscription enters the pre-warning interval due to:
 *   - the DST on/off date is updated
 *   - the time offset to UTC is updated
 *   - the node UTC time is updated
 *   - the client updates the pre-warning interal
 *
 * The client will receive an indication signal with cause
 * CELLO_TZII_PREWARNING_CANCEL when:
 * - the subscription leaves the pre-warning interval due to:
 *   - the DST on/off date is updated
 *   - the time offset to UTC is updated
 *   - the node UTC time is updated
 *
 * The client will receive an indication signal with cause
 * CELLO_TZII_ATTRIBUTE_OR_TIME_UPDATE when:
 * - the time offset to UTC is updated
 * - the DST on/off date is updated causing the DST on/off value to change
 *
 *****************************************************************************/
CelloTziiResult
CelloTzii_subscribeDaylightSavingTime(void *tziiMemory_p,
                                      uint32_t clientInfo,
                                      uint32_t preWarningTime) {
  QENTER1("CelloTzii_subscribeDaylightSavingTime");
  if (tziiMemory_p == NULL) {
    QRETURN1(CELLO_TZII_MEMORY_NOT_INITIATED);
  }
  else {
    CelloTziiProxyData *proxyData_p = (CelloTziiProxyData *)tziiMemory_p;
    if (proxyData_p->availabilityState != SERVER_AVAILABLE) {
      QRETURN1(CELLO_TZII_SERVER_UNAVAILABLE);
    }
    else {
      uint32_t buffer[] = {
          CEC_REQUEST_SUBSCRIBE_DST,
          proxyData_p->spidInitial,
          clientInfo,
          preWarningTime
      };

      int r = cecOpenSendClose(buffer, 4);
      if (r == CEC_UNKNOWN) {
        QRETURN1(CELLO_TZII_UNKNOWN_CLIENT);
      }
      else if (r != CEC_OK) {
        QRETURN1(CELLO_TZII_INTERNAL_ERROR_SUBSCR_DST + r);
      }
      else {
        QRETURN1(CELLO_TZII_OK);
      }
    }
  }
}


/******************************************************************************
 *
 * Start subscription on leap seconds information.
 *
 * If client is already subscribing on DST information, the old subscription
 * is discarded and the client will receive a new confirm signal.
 *
 * @param tziiMemory_p   - TZII handle
 * @param clientInfo     - client data returned in indication signal
 * @param preWarningTime - advance notification in milliseconds (<= 23 hours)
 *
 * Return:  CELLO_TZII_OK
 *          CELLO_TZII_SERVER_UNAVAILABLE
 *          CELLO_TZII_MEMORY_NOT_INITIATED
 *
 * The client will receive an indication signal with cause
 * CELLO_TZII_PREWARNING_START when:
 * - the subscription is issued during the pre-warning interval
 * - time passes into the pre-warning interval
 * - the subscription enters the pre-warning interval due to:
 *   - the node UTC time is updated
 *   - the client updates the pre-warning interal
 *
 * The client will receive an indication signal with cause
 * CELLO_TZII_PREWARNING_CANCEL when:
 * - the subscription leaves the pre-warning interval due to:
 *   - the node UTC time is updated
 *
 * The client will receive an indication signal with cause
 * CELLO_TZII_ATTRIBUTE_OR_TIME_UPDATE when:
 * - the time offset to UTC is updated
 * - the leap seconds MO attribute is updated manually
 *
 *****************************************************************************/
CelloTziiResult
CelloTzii_subscribeLeapSeconds(void *tziiMemory_p,
                               uint32_t clientInfo,
                               uint32_t preWarningTime) {
  QENTER1("CelloTzii_subscribeLeapSeconds");
  if (tziiMemory_p == NULL) {
    QRETURN1(CELLO_TZII_MEMORY_NOT_INITIATED);
  }
  else {
    CelloTziiProxyData *proxyData_p = (CelloTziiProxyData *)tziiMemory_p;
    if (proxyData_p->availabilityState != SERVER_AVAILABLE) {
      QRETURN1(CELLO_TZII_SERVER_UNAVAILABLE);
    }
    else {
      uint32_t buffer[] = {
          CEC_REQUEST_SUBSCRIBE_LEAP_SEC,
          proxyData_p->spidInitial,
          clientInfo,
          preWarningTime
      };

      int r = cecOpenSendClose(buffer, 4);
      if (r == CELLO_TZII_UNKNOWN_CLIENT) {
        QRETURN1(CELLO_TZII_UNKNOWN_CLIENT);
      }
      else if (r > 0) {
        QRETURN1(CELLO_TZII_INTERNAL_ERROR_SUBSCR_LEAP + r);
      }
      else {
        QRETURN1(CELLO_TZII_OK);
      }
    }
  }
}

void
CelloTzii_setMailbox(void *tziiMemory_p, PROCESS mbox) {
  QENTER1("CelloTzii_setMailbox");
  if (tziiMemory_p == NULL) {
    QTRACE_ERROR1("no proxy");
  }
  else {
    CelloTziiProxyData *proxyData_p = (CelloTziiProxyData *)tziiMemory_p;
    PROCESS newMbox =
        mbox == 0 ? proxyData_p->spidInitial : mbox;
    uint32_t buffer[] = {
        CEC_REQUEST_SET_MAILBOX,
        proxyData_p->spidInitial,
        newMbox};
    int r = cecOpenSendClose(buffer, 3);
    if (r != CEC_OK) {
      QTRACE_ERROR2("failed to set mailbox, code: %u", r);
    }
    else {
      proxyData_p->spid = newMbox;
    }
  }
  QRETURN;
}
