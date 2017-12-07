/* ----------------------------------------------------------------------
 * %CCaseFile:	child.c %
 * %CCaseRev:	/main/R1A/R2A/R3A/R4A/R5A/R8A/R10A/R11A/4 %
 * %CCaseDate:	2017-09-13 %
 * %CCaseDocNo: %
 * Author:	etxivri
 * Author: Ivan Ribrant, <ivan.ribrant@ericsson.com>
 *
 * Short description:
 * Child.
 * ----------------------------------------------------------------------
 * %CCaseTemplateFile:  template.c %
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
 * R1A/2      2012-08-31 etxivri     Created
 * R1A/3      2012-08-31 etxivri     More genar recv_sig. Comment out some printf
 * R2A/3      2012-11-02 etxarnu     Added missing signal for PRI
 * R2A/4      2012-11-16 erarafo     Added support for CLH (the CSI interface)
 * R2A/5      2012-11-21 etxpeno     Added support for VII
 * R2A/14     2013-04-09 erarafo     Freeing heap space for PMI
 * R2A/18     2013-11-07 erarafo     Added handling of NTFI tests
 * R2A/20     2013-12-06 erarafo     Added a log message when child starts
 * R2A/21     2014-01-28 erarafo     Added support for AVC tests
 * R2A/22     2014-02-18 etxpejn     Added support for LIHI tests
 * R2A/25     2014-03-11 erarafo     Polling for PMI callbacks elaborated
 * R2A/27     2014-08-26 erarafo     Support for simple self-test
 * R3A/1      2014-09-05 etxpejn     Added FI
 * R3A/2      2014-09-05 etxpejn     Added break to "case SELF:"
 * R3A/3      2014-09-15 eolaand     Added PMI2
 * R3A/5      2014-11-24 etxpejn     Added license capacity signals
 * R3A/6      2014-11-26 eolaand     Added PEI
 * R3A/8      2014-12-15 etxpeno     Added CHI
 * R3A/11     2015-03-21 erarafo     Added IMM
 * R3A/12     2015-03-30 eolaand     Exit if dispatch fails
 * R4A/1      2015-05-19 etxberb     Updated CSI & CHI signals.
 * R4A/2      2015-08-18 etxkols     Removed all references to lici
 * R4A/3      2015-08-31 erarafo     Added TIM, removed some noise
 * R4A/4      2015-09-02 erarafo     TZII, work in progress
 * R4A/5      2015-09-03 erarafo     TZII, signals handled
 * R4A/6      2015-09-08 uabesvi     Cluster restart added
 * R4A/7      2015-11-20 erarafo     Trace messages wrapped as macros
 * R4A/8      2015-11-25 erarafo     Small delay inserted in spinning loop
 * R4A/9      2015-11-25 erarafo     TRI calls bypassed on simulator
 * R4A/10     2015-11-26 erarafo     Visibility of functions
 * R5A/1      2015-12-02 erarafo     Tracing of function calls
 * R5A/2      2015-12-02 erarafo     Cleanup
 * R5A/3      2015-12-11 etomist     AVLI ITC support
 * R5A/4      2015-12-13 erarafo     Separate queues for down/up messages
 * R5A/5      2015-12-17 erarafo     Minor adjustments
 * R5A/6      2015-12-21 erarafo     Minor adjustments
 * R5A/7      2015-12-29 etomist     AVLI ITC removed
 * R5A/8      2016-01-27 etxpejn     Added LFCI signals
 * R5A/9      2016-02-19 ekurnik     Added CCI
 * R8A/1      2017-01-15 etxpejn     Added new LCCI signals
 * R10A/1     2017-05-08 uabhten     Added CSTN
 * R10A/3     2017-06-29 evadumb     Added CCI2_NTP_STATE_IND signal
 * R11A/4     2017-09-13 enekdav     Added SEC_CREDU
* ----------------------------------------------------------------------
 */

#define CCREV "%CCaseRev:	/main/R1A/R2A/R3A/R4A/R5A/R8A/R10A/R11A/4 %"

#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif

#include <poll.h>
#include <unistd.h>
#include <time.h>

#include "master.h"
#include "cello_piu.h"
#include "cello_piu.sig"
#include "clh_csi.h"
#include "clh_csi.sig"
#include "cello_avli.h"
#include "cello_avli.sig"
#include "licenseFeatureControlI.sig"
#include "licenseCapacityControlI.sig"
#include "cello_buti.sig"
#include "clh_chi.h"
#include "clh_chi.sig"
#include "cello_tzii.h"
#include "cello_tzii.sig"
#include "cci.sig"

#define EI_X_FREE(A)   ei_x_free(A);
#define FREE(A)        free(A);

// per-client poll period
#define CLIENT_TIMEOUT_MILLIS 50


/* test_pri.c functions */
extern ei_x_buff send_sig_pri(void **, union SIGNAL *, int, ei_x_buff);
extern ei_x_buff recv_sig_pri(void *, union SIGNAL *);

/* test_csi.c functions */
extern ei_x_buff send_sig_csi(int, ei_x_buff);
extern ei_x_buff recv_sig_csi(union SIGNAL *);

/* test_vii.c functions */
extern ei_x_buff send_sig_vii(int, ei_x_buff);

/* test_avli.c functions */
extern ei_x_buff send_sig_avli(int, ei_x_buff);
extern ei_x_buff recv_sig_avli(union SIGNAL *);

/* test_dnti.c functions */
extern ei_x_buff send_sig_dnti(int, ei_x_buff);

/* test_pmi.c functions*/
extern ei_x_buff send_sig_pmi(int, ei_x_buff);

/* test_pmi2.c functions*/
extern ei_x_buff send_sig_pmi2(int, ei_x_buff);

/* test_pei.c functions*/
extern ei_x_buff send_sig_pei(int, ei_x_buff);

/* test_oei.c functions*/
extern ei_x_buff send_sig_oei(int, ei_x_buff);

/* test_rcs_oei_2.c functions*/
extern ei_x_buff send_sig_rcs_oei_2(int, ei_x_buff);

/* test_lmhi.c functions*/
extern ei_x_buff send_sig_lmhi(void **, int, ei_x_buff);

/* test_cmsi.c functions */
extern ei_x_buff send_sig_cmsi(int, ei_x_buff);

/* test_ntfi.c functions */
extern ei_x_buff send_sig_ntfi(int, ei_x_buff);

/* test_avc.c functions */
extern ei_x_buff send_sig_avc(int, ei_x_buff);

/* test_lihi.c functions */
extern ei_x_buff send_sig_lihi(void **, union SIGNAL *, int, ei_x_buff);
extern ei_x_buff recv_sig_lihi(union SIGNAL *);

/* test_lttng.c functions */
extern ei_x_buff send_sig_lttng(int, ei_x_buff);

/* test_buti.c functions */
extern ei_x_buff send_sig_buti(void **, union SIGNAL *, int, ei_x_buff);
extern ei_x_buff recv_sig_buti(void *, union SIGNAL *);

/* for self-test of IFT */
extern ei_x_buff send_sig_self(int, ei_x_buff);
/* for self-test of IFT */

/* test_fi.c functions */
extern ei_x_buff send_sig_fi(int, ei_x_buff);

/* test_seci.c functions */
extern ei_x_buff send_sig_seci(int, ei_x_buff);

/* test_chi.c functions */
extern ei_x_buff send_sig_chi(int, ei_x_buff);
extern ei_x_buff recv_sig_chi(union SIGNAL *);

/* imm.c functions */
extern ei_x_buff send_sig_imm(ei_x_buff);

/* test_tzii.c functions */
extern ei_x_buff send_sig_tzii(void **, union SIGNAL *, int, ei_x_buff);
extern ei_x_buff recv_sig_tzii(void ***, union SIGNAL *);

/* test_cci.c functions */
extern ei_x_buff send_sig_cci(int, ei_x_buff);
extern ei_x_buff recv_sig_cci(union SIGNAL *);

/* test_cstn.c functions*/
extern ei_x_buff send_sig_cstn(int, ei_x_buff);

/* test_sec_credu.c functions*/
extern ei_x_buff send_sig_sec_credu(int, ei_x_buff);


extern bool TRI_trace_enabled;
extern void sleepMillis(int millis);
extern PROCESS getMasterPid();
extern ClientDataT *getClientByPid(PROCESS pid);
extern size_t removeClient(PROCESS pid);
extern size_t nofClients();
extern void enqueueUpstreamMessage(PROCESS cSrc, const erlang_pid *dest, ei_x_buff msg, char *context);
extern bool hasMsg(PROCESS pid);
extern MsgDataT *unlinkMsg(PROCESS pid);
extern void freeMsgData(MsgDataT *msg);

union SIGNAL {
  SIGSELECT sigNo;
};


/**
 * See master.h for a description.
 */
void
addPollScheduleItem(char *handle, int socketFd, DispatcherT dispatcher) {
  PROCESS pid = current_process();
  ClientDataT *clientData = getClientByPid(pid);

  PollScheduleItemT *newItem = calloc(1, sizeof(PollScheduleItemT));
  newItem->pid = pid;
  newItem->handle = handle;
  newItem->socketFd = socketFd;
  newItem->dispatcher = dispatcher;
  newItem->deathMark = false;
  newItem->closeSocket = false;

  LOCK_WRLOCK(&(clientData->schedLock));
  newItem->next = clientData->sched;
  clientData->sched = newItem;
  LOCK_UNLOCK(&(clientData->schedLock));
}


/**
 * See master.h for a description.
 */
int
setDeathMark(char *handle, bool closeSocket) {
  PROCESS pid = current_process();
  ClientDataT *clientData = getClientByPid(pid);

  LOCK_WRLOCK(&(clientData->schedLock));
  for (PollScheduleItemT *p = clientData->sched;
        p != NULL;
        p = p->next) {
    if (strcmp(p->handle, handle) == 0) {
      p->deathMark = true;
      p->closeSocket = closeSocket;
      LOCK_UNLOCK(&(clientData->schedLock));
      return 0;
    }
  }
  LOCK_UNLOCK(&(clientData->schedLock));
  return -1;
}


/**
 * See master.h for a description.
 */
void
removePollScheduleItem(char *handle) {
  PROCESS pid = current_process();
  ClientDataT *clientData = getClientByPid(pid);

  PollScheduleItemT *previousP = NULL;
  LOCK_WRLOCK(&(clientData->schedLock));
  for (PollScheduleItemT *p = clientData->sched;
      p != NULL;
      previousP = p, p = p->next) {
    if (strcmp(p->handle, handle) == 0) {
      if (previousP == NULL) {
        clientData->sched = p->next;
      }
      else {
        previousP->next = p->next;
      }
      LOCK_UNLOCK(&(clientData->schedLock));
      FREE(p->handle);
      FREE(p);
      return;
    }
  }
  LOCK_UNLOCK(&(clientData->schedLock));
  QTRACE_ERROR2("no poll schedule item removed for handle: %s", handle);
  return;
}


/**
 * Enqueue a callback message for sending it upstream.
 */
void
sendCallback(ei_x_buff message) {
  QTRACE2(3, "send callback message to RCT proxy");
  PROCESS pid = current_process();
  ClientDataT *clientData = getClientByPid(pid);
  enqueueUpstreamMessage(pid, &(clientData->epid), message, UPSTREAM_CALLBACK);
  APPLOG_D("<-- /%u/ %s: callback", pid, clientData->id);
}

/**
 * Kill this thread if the flag says so. PMI
 * tests use this feature.
 */
static void
deathCheck(PollScheduleItemT *p) {
  if (p->deathMark) {
    if (p->closeSocket) {
      close(p->socketFd);
    }
    PROCESS self = current_process();
    size_t nClients = removeClient(self);
    QINFO2("killing process intentionally: %u", self);
    APPLOG_I("intentionally terminated client: %u, remaining n. of clients: %u", self, nClients);
    kill_proc(self);
  }
}


static ei_x_buff encode(const char *fmt) {
  ei_x_buff result;
  ei_x_new(&result);
  ei_x_format(&result, fmt);
  return result;
}

static ei_x_buff encodeUnsigned(const char *fmt, unsigned int k) {
  ei_x_buff result;
  ei_x_new(&result);
  ei_x_format(&result, fmt, k);
  return result;
}


/**
 * Polls the given connection. Invokes the callback repeatedly as long as there is
 * data on the connection.
 */
static void
pollForCallback(PollScheduleItemT *item) {
  int socketFd = item->socketFd;
  char *handle = item->handle;
  DispatcherT dispatcher = item->dispatcher;
  int dispatchRes = 0;

  for (bool retry = true; retry;) {
    struct pollfd pfd = {.fd=socketFd, .events=POLLIN, .revents=0};
    int r = poll(&pfd, 1, 0);
    if (r < 0) {
      QTRACE_ERROR3("poll failure, %s (%d)\n", strerror(errno), errno);
      retry = false;
    }
    else if (r == 0) {
      // lots of trace here if enabled
      // QTRACE4(3, "no data on socket: %d, handle: %s", selectionObject, handle));
      retry = false;
    }
    else if (pfd.revents & (POLLERR|POLLHUP|POLLNVAL)) {
      QTRACE_ERROR2("poll failure, flag: %d\n", pfd.revents);
      retry = false;
    }
    else if (!(pfd.revents & POLLIN)) {
      QTRACE_ERROR1("unexpected: POLLIN flag is not set\n");
      retry = false;
    }
    else {
      QTRACE4(3,
          "callback is pending, invoking dispatcher; fd: %d, handle: %s",
          socketFd,
          handle);
      dispatchRes = (*dispatcher)(handle);
      QINFO2("dispatchRes: %d", dispatchRes);
      if (dispatchRes != DISPATCH_OK) {
        close(socketFd);
        //removeStaticData();
        PROCESS self = current_process();
        QINFO2("Failed to dispatch, kill process: %u", self);
        size_t nClients = removeClient(self);
        APPLOG_E("failed to dispatch, killing self: %u, remaining n. of clients: %u", self, nClients);
        kill_proc(self);
      }
      else
      {
        QTRACE2(3, "invoked dispatcher, will retry poll");
        retry = true;
      }
    }
  }
}


/**
 * Handle a signal according to its signal number.
 */
static ei_x_buff recv_sig(void **proxyMemory_pp, union SIGNAL *sig_p) {
  const SIGSELECT sigNo = sig_p->sigNo;
  switch(sigNo) {
  case CELLO_PRI_SERVER_UP_IND:
  case CELLO_PRI_INITIATE_SERVICE_CFM:
  case CELLO_PRI_INITIATE_SERVICE_SUS:
  case CELLO_PRI_INITIATE_SERVICE_REJ:
  case CELLO_PRI_SERVER_DOWN_IND:
  case CELLO_PRI_SERVER_UNPUBLISH_IND:
  case CELLO_PRI_TERMINATE_SERVICE_CFM:
  case CELLO_PIU3_GET_HUNT_PATH_CFM:
  case CELLO_PIU3_GET_OWN_ID_CFM:
  case CELLO_PIU4_RESTART_PIU_CFM:
  case CELLO_PIU4_RESTART_PIU_REJ:
  case CELLO_PRI8_GET_IDENTITY_CFM:
  case CELLO_PRI9_GET_IDENTITY_CFM:
  case CELLO_PIU10_OPERATIONAL_PID_CFM:
    QPRINTF2("### CLIENT PRI receive sigNo: %x\n", sigNo);
    return recv_sig_pri(&proxyMemory_pp, sig_p);

  case CSI_CORE_STATE_CHANGE_IND:
  case CSI_CLUSTER_RESTART_IND:
    QPRINTF2("### CLIENT CSI receive sigNo: %x\n", sigNo);
    return recv_sig_csi(sig_p);

  case CELLO_AVLI_WRITE_CFM:
  case CELLO_AVLI_HUNT_NS_IND:
  case CELLO_AVLI_SERVER_UP_IND:
  case CELLO_AVLI_SERVER_DOWN_IND:
  case CELLO_AVLI_ATTACH_NS_IND:
  case CELLO_AVLI_UNPUBLISH_IND:
  case CELLO_AVLI2_INITIATE_SERVICE_CFM:
  case CELLO_AVLI2_INITIATE_SERVICE_REJ:
  case CELLO_AVLI2_INITIATE_SERVICE_SUS:
    QPRINTF2("### CLIENT AVLI receive sigNo: %x\n", sigNo);
    return recv_sig_avli(sig_p);

  case LFCI_CONN_TO_SERVER_CFM:
  case LFCI_CONN_TO_SERVER_REJ:
  case LFCI_FEATURE_LICENSE_SUBSCRIBE_CFM:
  case LFCI_FEATURE_LICENSE_SUBSCRIBE2_CFM:
  case LFCI_FEATURE_LICENSE_UNSUBSCRIBE_CFM:
  case LFCI_FEATURE_LICENSE_CHANGE_IND:
  case LFCI_FEATURE_LICENSE_CHANGE2_IND:
  case LFCI_FEATURE_LICENSE_DISCONNECT_IND:
  case LCCI_CONN_TO_SERVER_CFM:
  case LCCI_CONN_TO_SERVER_REJ:
  case LCCI_CAPACITY_LICENSE_SUBSCRIBE_CFM:
  case LCCI_CAPACITY_LICENSE_SUBSCRIBE2_CFM:
  case LCCI_CAPACITY_LICENSE_UNSUBSCRIBE_CFM:
  case LCCI_CAPACITY_LICENSE_CHANGE_IND:
  case LCCI_CAPACITY_LICENSE_CHANGE2_IND:
  case LCCI_CAPACITY_LICENSE_DISCONNECT_IND:
  case LCCI_CAPACITY_LICENSE_GP_ACTIVATED_FWD:
    return recv_sig_lihi(sig_p);

  case CELLO_BUTI_INITIATE_SERVICE_CFM:
  case CELLO_BUTI_INITIATE_SERVICE_REJ:
  case CELLO_BUTI_SUBSCRIBE_BUTTON_EVENT_CFM:
  case CELLO_BUTI_UNSUBSCRIBE_BUTTON_EVENT_CFM:
  case CELLO_BUTI_CHANGE_FEEDBACK_MODE_CFM:
  case CELLO_BUTI_CHANGE_FEEDBACK_MODE_REJ:
  case CELLO_BUTI_TERMINATE_SERVICE_CFM:
  case CELLO_BUTI_EVENT_IND:
    return recv_sig_buti(&proxyMemory_pp, sig_p);

  case CHI_OP_STATE_CHANGE_IND:
    return recv_sig_chi(sig_p);

  case CELLO_TZII_SERVER_DOWN_IND:
  case CELLO_TZII_SERVER_UP_IND:
  case CELLO_TZII_INITIATE_SERVICE_CFM:
  case CELLO_TZII_INITIATE_SERVICE_SUS:
  case CELLO_TZII_INITIATE_SERVICE_REJ:
  case CELLO_TZII_SERVER_UNPUBLISH_IND:
  case CELLO_TZII_TERMINATE_SERVICE_CFM:
  case CELLO_TZII_SUBSCRIBE_DAYLIGHT_SAVING_TIME_CFM:
  case CELLO_TZII_SUBSCRIBE_DAYLIGHT_SAVING_TIME_REJ:
  case CELLO_TZII_SUBSCRIBE_DAYLIGHT_SAVING_TIME_IND:
  case CELLO_TZII_SUBSCRIBE_LEAP_SECONDS_CFM:
  case CELLO_TZII_SUBSCRIBE_LEAP_SECONDS_REJ:
  case CELLO_TZII_SUBSCRIBE_LEAP_SECONDS_IND:
    return recv_sig_tzii(&proxyMemory_pp, sig_p);

  case CCI_TIME_UPDATE_IND:
  case CCI_SUBSCRIBE_CFM:
  case CCI_SUBSCRIBE_REJ:
  case CCI_UNSUBSCRIBE_CFM:
  case CCI_UNSUBSCRIBE_REJ:
  case CCI_SERVER_DOWN_IND:
  case CCI2_NTP_STATE_IND:
	return recv_sig_cci(sig_p);

  default:
    QPRINTF2("###UNKNOWN receive sigNo: %x\n", sigNo);
    APPLOG_W("unknown signal received, sigNo: %u", sigNo);
    return encodeUnsigned("{signal, ~u}", sigNo);
  }
}


/**
 * Client process, started by the master and runs until
 * a stop request occurs.
 */
OS_PROCESS(client) {
  PROCESS const mypid = current_process();

  QINFO2("CHILD starting, pid: %u", mypid);
  APPLOG("client starts, pid: %u, child.c version: %s", mypid, CCREV);

  // an initial message is expected very soon; wait for it
  for (int attempts = 1; true; attempts++) {
    MsgDataT *msgDataP = unlinkMsg(mypid);
    if (msgDataP == NULL) {
      sleepMillis(1);
    }
    else {
      freeMsgData(msgDataP);
      APPLOG_D("client: %u, initial message received, attempts: %d", mypid, attempts);
      break;
    }
  }

  ClientDataT *clientData = getClientByPid(mypid);
  char id[strlen(clientData->id)+1];
  strcpy(id, clientData->id);

  ei_x_buff initialResp = encode("{ok, client_started}");
  enqueueUpstreamMessage(mypid, &(clientData->epid), initialResp, UPSTREAM_RESPONSE);

  APPLOG_D("<-- /%u/ %s: client started", mypid, id);
  APPLOG_I("%s started, n. of clients: %u", id, nofClients());

  union SIGNAL *last_sig_p = NULL;
  erlang_pid ePid;
  bool ePidIsSet = false;

  while (true) {

    // handle OSE signal, if any
    SIGSELECT any[] = {0};
    union SIGNAL *sig_p = receive_w_tmo(CLIENT_TIMEOUT_MILLIS, any);
    if (sig_p != NULL) {
      APPLOG_D("client: %s, received signal, sigNo: %u", id, sig_p->sigNo);
      QPRINTFF2("CLIENT receive_w_tmo %p\n", (void*)sig_p);

      DRCALL(ei_x_buff, signalData,
          recv_sig(&(clientData->mem_p), sig_p),
          "/%u/ %s, handle signal", mypid, id);

      erlang_pid *ePidBestEffort = ePidIsSet ? &ePid : &(clientData->epid);

      enqueueUpstreamMessage(mypid, ePidBestEffort, signalData, UPSTREAM_SIGNAL);
      APPLOG_D("<-- /%u/ %s, signal", mypid, id);
      last_sig_p = sig_p;
    }

    // handle callback, if any
    LOCK_RDLOCK(&(clientData->schedLock));
    for (PollScheduleItemT *p = clientData->sched; p != NULL; p = p->next) {
      deathCheck(p);
      pollForCallback(p);
    }
    LOCK_UNLOCK(&(clientData->schedLock));

    // handle downstream message, if any
    if (hasMsg(mypid)) {

      MsgDataT *msgDataP = unlinkMsg(mypid);

      APPLOG_D("%s: request received, protocol: %u", id, clientData->protocol);
      QPUTSF1("CLIENT received msg");
      ei_x_buff resp;
      uint32_t protocol = msgDataP->prot;
      if (protocol == DIE) {
        QINFO1("CHILD  DIE killing client\n");
        QPUTSF1("Prot: DIE kill client");

        ei_x_buff finalMessage;
        ei_x_new(&finalMessage);
        ei_x_format(&finalMessage, "{ok, client_stopped}");
        enqueueUpstreamMessage(mypid, &(msgDataP->ePid), finalMessage, UPSTREAM_RESPONSE);

        size_t nClients = removeClient(mypid);
        APPLOG_I("%s stopped, n. of clients: %u", id, nClients);

        freeMsgData(msgDataP);
        kill_proc(mypid);
      }
      else {

        ePid = msgDataP->ePid;  // for use in responses and by signals
        ePidIsSet = true;

        switch (protocol) {
        // use the RCALL(R, FUNCALL, FMT, ...) macro around send_sig_*()
        // if suspecting crashes in particular test modules or C interfaces
        case PRI:
          QPRINTFF2("CLIENT PRI Func: %u\n", msgDataP->func);
          resp = send_sig_pri(&(clientData->mem_p), last_sig_p, msgDataP->func, msgDataP->buf);
          break;

        case CSI:
          QPRINTFF2("CLIENT CSI Func: %u\n", msgDataP->func);
          resp = send_sig_csi(msgDataP->func, msgDataP->buf);
          break;

        case VII:
          QPRINTFF2("CLIENT VII Func: %u\n", msgDataP->func);
          resp = send_sig_vii(msgDataP->func, msgDataP->buf);
          break;

        case AVLI:
          QPRINTFF2("CLIENT AVLI Func: %u\n", msgDataP->func);
          resp = send_sig_avli(msgDataP->func, msgDataP->buf);
          break;

        case DNTI:
          QPRINTFF2("CLIENT DNTI Func: %u\n", msgDataP->func);
          resp = send_sig_dnti(msgDataP->func, msgDataP->buf);

          break;

        case PMI:
          QINFO2("CHILD PMI, func: %u", msgDataP->func);
          QPRINTFF2("CLIENT PMI Func: %i\n", (int) msgDataP->func);
          resp = send_sig_pmi(msgDataP->func, msgDataP->buf);
          break;

        case PMI2:
          QINFO2("CHILD PMI2, func: %u", msgDataP->func);
          QPRINTFF2("CLIENT PMI2 Func: %u\n", msgDataP->func);
          resp = send_sig_pmi2(msgDataP->func, msgDataP->buf);
          break;

        case PEI:
          QINFO2("CHILD PEI, func: %u", msgDataP->func);
          QPRINTFF2("CLIENT PEI Func: %u\n", msgDataP->func);
          resp = send_sig_pei(msgDataP->func, msgDataP->buf);
          break;

        case OEI:
          QPRINTFF2("CLIENT OEI Func: %u\n", msgDataP->func);
          resp = send_sig_oei(msgDataP->func, msgDataP->buf);
          break;

        case LMHI:
          QPRINTFF2("CLIENT LMHI Func: %u\n", msgDataP->func);
          resp = send_sig_lmhi(&(clientData->mem_p), msgDataP->func, msgDataP->buf);
          break;

        case CMSI:
          QPRINTFF2("CLIENT CMSI Func: %u\n", msgDataP->func);
          resp = send_sig_cmsi(msgDataP->func, msgDataP->buf);
          break;

        case NTFI:
          QPRINTFF2("CLIENT NTFI Func: %u\n", msgDataP->func);
          resp = send_sig_ntfi(msgDataP->func, msgDataP->buf);
          break;

        case AVC:
          QPRINTFF2("CLIENT AVC Func: %u\n", msgDataP->func);
          resp = send_sig_avc(msgDataP->func, msgDataP->buf);

          break;

        case LIHI:
          resp = send_sig_lihi(&(clientData->mem_p), last_sig_p, msgDataP->func,
              msgDataP->buf);
          break;

        case LTTNG:
          resp = send_sig_lttng(msgDataP->func, msgDataP->buf);
          break;

        case BUTI:
          resp = send_sig_buti(&(clientData->mem_p), last_sig_p, msgDataP->func, msgDataP->buf);
          break;

        case SELF:
          QPRINTFF2("CLIENT SELF Func: %u\n", msgDataP->func);
          resp = send_sig_self(msgDataP->func, msgDataP->buf);
          break;

        case FI:
          resp = send_sig_fi(msgDataP->func, msgDataP->buf);
          break;

        case SECI:
          QPRINTFF2("CLIENT SECI Func: %u\n", msgDataP->func);
          resp = send_sig_seci(msgDataP->func, msgDataP->buf);
          break;

        case CHI:
          QPRINTFF2("CLIENT CHI Func: %u\n", msgDataP->func);
          resp = send_sig_chi(msgDataP->func, msgDataP->buf);
          break;

        case IMM:
          resp = send_sig_imm(msgDataP->buf);
          break;

        case TZII:
          resp = send_sig_tzii(&(clientData->mem_p), last_sig_p, msgDataP->func, msgDataP->buf);
          break;

        case CCI:
		  QPRINTFF2("CLIENT CCI Func: %u\n", msgDataP->func);
		  resp = send_sig_cci(msgDataP->func, msgDataP->buf);
		  break;

        case CSTN:         
          QPRINTFF2("CLIENT CSTN Func: %u\n", msgDataP->func);
          resp = send_sig_cstn(msgDataP->func, msgDataP->buf);
          break;

        case SEC_CREDU:
          QPRINTFF2("CLIENT SEC_CREDU Func: %u\n", msgDataP->func);
          resp = send_sig_sec_credu(msgDataP->func, msgDataP->buf);
          break;

        default:
          QINFO1("CHILD  switch default \n");
          APPLOG_E("unknown protocol: %u", clientData->protocol);
          resp = encodeUnsigned("{error, {unknown_protocol, ~u}}", clientData->protocol);
          break;
        }

        freeMsgData(msgDataP);

        enqueueUpstreamMessage(mypid, &ePid, resp, UPSTREAM_RESPONSE);
        APPLOG_D("<-- /%u/ %s, response", mypid, id);
      }
    }
  }
}
