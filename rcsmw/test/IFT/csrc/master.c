/* ----------------------------------------------------------------------
 * %CCaseFile:	master.c %
 * %CCaseRev:	/main/R1A/R2A/R3A/R4A/R5A/R6A/R11A/1 %
 * %CCaseDate:	2017-08-16 %
 * %CCaseDocNo: %
 * Author:	etxivri
 * Author: Ivan Ribrant, <ivan.ribrant@ericsson.com>
 *
 * Short description:
 * Master.
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
 * R1A/2      2012-08-31  etxivri    Created
 * R1A/3      2012-09-04  etxivri    Removed som printf
 * R1A/4      2012-09-10  etxivri    Increased MAX_CLIENTS
 * R2A/12     2013-06-18  erarafo    Added tests of ICTI at upgrade restart
 * R2A/16     2013-12-06  erarafo    Fixed buffer overrun: create_child_process
 * R3A/1      2014-12-05  etxpeno    Handle disconnect_node() from erlang
 * R3A/2      2014-12-08  etxpeno    Handle net_kernel:connect_node() from erlang
 * R3A/3      2015-01-29  erarafo    Timestamps in APPLOG entries
 * R3A/4      2015-01-29  erarafo    String null termination fixed
 * R3A/5      2015-01-29  erarafo    Extra guard against buffer overflow
 * R3A/6      2015-01-30  erarafo    Revert to R3A/2
 * R3A/7      2015-01-30  erarafo    Rsurrect R3A/5
 * R3A/8      2015-03-21  erarafo    Eliminated one warning, more to go
 * R4A/1      2015-11-16  erarafo    Support for sharing proxy pointers
 * R4A/2      2015-11-18  erarafo    Support for sharing proxy pointers
 * R4A/4      2015-11-25  erarafo    Function sendUpstream added
 * R4A/5      2015-11-25  erarafo    Faulty ...
 * R4A/6      2015-11-28  erarafo    Bypass TRI trace on simulator
 * R5A/1      2015-12-02  erarafo    Trace of function calls
 * R5A/2      2015-12-02  erarafo    Cleanup
 * R5A/3      2015-12-13  erarafo    APPLOG log levels, separate queues
 * R5A/4      2015-12-17  erarafo    Messages displayed in APPLOG
 * R5A/5      2015-12-20  erarafo    Minor adjustment
 * R5A/6      2015-12-21  erarafo    Minor adjustment
 * R6A/1      2016-07-07  etxpeno    Coverity fixes
 * ----------------------------------------------------------------------
 */

#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif


#include "master.h"
#include "shell.h"
#include <unistd.h>
#include <time.h>
#include <string.h>
#include <stdio.h>
#include <errno.h>
#include <sys/select.h>
#include <sys/utsname.h>


// Deallocation of memory uses these macros
#define EI_X_FREE(A)   ei_x_free(A);
#define FREE(A)        free(A);
//#define EI_X_FREE(A)   ;
//#define FREE(A)        ;


// Delay when retrying the "publish" operation towards
// epmd. A value in the 0..999 range can be defined.
#define PUBLISH_RETRY_MILLIS 300

// Maximum number of publish attempts
#define PUBLISH_MAX_COUNT 10


// Poll period of the master process
#define POLL_PERIOD_MILLIS 20




/**
 * Logging level for APPLOG. The default applies unless a
 * value for APPLOG_LEVEL has been set in the environment.
 * Changing the value dynamically can be done using a SELF
 * client (see test_self.c).
 */
volatile int applogLevel = APPLOG_LEVEL_DEFAULT;


/**
 * TRI trace is enabled when true. Currently this
 * variable is set to true on target nodes and false
 * on the simulator.
 */
volatile bool TRI_trace_enabled = true;


/**
 * The pid of the master process. The value is assigned once
 * and never changed.
 */
static volatile PROCESS masterPid = 0;


/**
 * Consider removing.
 */
extern void exerciseIcti();


/**
 * Function that implements client processes.
 */
extern void client(void);


/**
 * Collection of client data instances.
 */
static ClientDataT *clients = NULL;

static size_t numberOfClients = 0;

static pthread_rwlock_t clients_lock = PTHREAD_RWLOCK_INITIALIZER;


void sleepMillis(int millis) {
  struct timespec delay = {
      .tv_nsec = (millis%1000)*1000000,
      .tv_sec = millis/1000
  };
  nanosleep(&delay, NULL);
}


/**
 * Returns the master pid.
 */
PROCESS
getMasterPid() {
  return masterPid;
}


/**
 * Returns a pointer to the indicated client or NULL
 * if no such client exists.
 */
static ClientDataT *
getClientByName(const char *id) {
  LOCK_RDLOCK(&clients_lock);
  for (ClientDataT *p = clients; p != NULL; p = p->next) {
    if (strcmp(p->id, id) == 0) {
      LOCK_UNLOCK(&clients_lock);
      return p;
    }
  }
  LOCK_UNLOCK(&clients_lock);
  return NULL;
}


/**
 * Returns a pointer to the indicated client or NULL
 * if no such client exists.
 */
ClientDataT *
getClientByPid(PROCESS pid) {
  LOCK_RDLOCK(&clients_lock);
  for (ClientDataT *p = clients; p != NULL; p = p->next) {
    if (p->pid == pid) {
      LOCK_UNLOCK(&clients_lock);
      return p;
    }
  }
  LOCK_UNLOCK(&clients_lock);
  return NULL;
}


/**
 * Returns the proxy pointer of the client indicated
 * by the given pid. One client can get hold of another
 * client's proxy pointer using this function; use
 * with care.
 */
void *
getPeerProxyPointer(PROCESS pid) {
  ClientDataT *p = getClientByPid(pid);
  return p == NULL? NULL : p->mem_p;
}


/**
 * Returns the number of client data instances.
 */
size_t
nofClients() {
  size_t result;
  LOCK_RDLOCK(&clients_lock);
  result = numberOfClients;
  LOCK_UNLOCK(&clients_lock);
  return result;
}


/**
 * Adds the given client data instance to the existing
 * collection. The resulting number of clients is returned.
 */
static size_t
addClient(ClientDataT *c) {
  LOCK_WRLOCK(&clients_lock);
  numberOfClients++;
  size_t result = numberOfClients;
  if (clients == NULL) {
    clients = c;
    c->next = NULL;
    LOCK_UNLOCK(&clients_lock);
    return result;
  }
  else {
    for (ClientDataT *p = clients; p != NULL; p = p->next) {
      if (p->next == NULL) {
        p->next = c;
        c->next = NULL;
        LOCK_UNLOCK(&clients_lock);
        return result;
      }
    }
    // cannot happen but compiler thinks it can
    return 999999;
  }
}


/**
 * Deallocates a client data instance.
 */
static void
freeClient(ClientDataT *x) {
  PollScheduleItemT *p = x->sched;

  while(p) {
    PollScheduleItemT *p1 = p;
    p = p->next;
    FREE(p1);
  }

  FREE(x->id);
  FREE(x);
}


/**
 * Removes a client data instance if found. The number of
 * remaining instances is returned.
 */
size_t
removeClient(PROCESS pid) {
  LOCK_WRLOCK(&clients_lock);
  size_t result = numberOfClients - 1;
  if (clients == NULL) {
    APPLOG_E("cannot remove client data for: %u", pid);
    LOCK_UNLOCK(&clients_lock);
    return 0;
  }
  else if (clients->pid == pid) {
    ClientDataT *x = clients;
    clients = x->next;
    freeClient(x);
    numberOfClients--;
    LOCK_UNLOCK(&clients_lock);
    return result;
  }
  else {
    for (ClientDataT *p = clients; p->next != NULL; p = p->next) {
      if (p->next->pid == pid) {
        ClientDataT *x = p->next;
        p->next = x->next;
        freeClient(x);
        numberOfClients--;
        LOCK_UNLOCK(&clients_lock);
        return result;
      }
    }
    // suspicious, client not found
    result = numberOfClients;
    LOCK_UNLOCK(&clients_lock);
    APPLOG_W("not removed: %u", pid);
    return result;
  }
}


/**
 * Shared collection of MsgDataT instances representing
 * downstream messages.
 */
static MsgDataT *msgs = NULL;

static pthread_rwlock_t msg_lock = PTHREAD_RWLOCK_INITIALIZER;


/**
 * Returns true if the downstream queue has a message
 * for the given pid.
 */
bool
hasMsg(PROCESS pid) {
  if (msgs == NULL) {
    return NULL;
  }
  else {
    LOCK_RDLOCK(&msg_lock);
    for (MsgDataT *q = msgs; q != NULL; q = q->next) {
      if (q->cDest == pid) {
        LOCK_UNLOCK(&msg_lock);
        return true;
      }
    }
    LOCK_UNLOCK(&msg_lock);
    return false;
  }
}


/**
 * Adds a downstream message to the back of the queue.
 */
static void
enqueueDownstreamMessage(
    PROCESS dest,
    const erlang_pid *eSrc,
    uint32_t protocol,
    uint32_t function,
    const ei_x_buff *args) {
  MsgDataT *msgData = calloc(1, sizeof(MsgDataT));
  msgData->cDest = dest;
  if (eSrc != NULL) {
    msgData->ePid = *eSrc;
  }
  msgData->prot = protocol;
  msgData->func = function;

  if (args != NULL) {
    msgData->buf = *args;
  }
  msgData->next = NULL;
  LOCK_WRLOCK(&msg_lock);
  if (msgs == NULL) {
    msgs = msgData;
  }
  else {
    for (MsgDataT *p = msgs; p != NULL; p = p->next) {
      if (p->next == NULL) {
        p->next = msgData;
        break;
      }
    }
  }
  LOCK_UNLOCK(&msg_lock);
  return;
}


/**
 * Deallocates a downstream message.
 */
void freeMsgData(MsgDataT *msg) {
  if (msg->buf.buff != NULL) {
    FREE(msg->buf.buff);
  }
  FREE(msg);
}


/**
 * Unlinks the indicated downstream message.
 */
MsgDataT *
unlinkMsg(PROCESS pid) {
  if (msgs == NULL) {
    return NULL;
  }
  else {
    LOCK_WRLOCK(&msg_lock);
    if (msgs->cDest == pid) {
      MsgDataT *result = msgs;
      msgs = msgs->next;
      LOCK_UNLOCK(&msg_lock);
      result->next = NULL;
      return result;
    }
    else {
      MsgDataT *p = msgs;
      for (MsgDataT *q = p->next; q != NULL; q = q->next) {
        if (q->cDest != pid) {
          p = q;
        }
        else {
          p->next = q->next;
          LOCK_UNLOCK(&msg_lock);
          q->next = NULL;
          return q;
        }
      }
      LOCK_UNLOCK(&msg_lock);
      return NULL;
    }
  }
}


/**
 * Shared collection of MsgDataT instances representing
 * upstream messages.
 */
static MsgUpT *msgsUp = NULL;

static pthread_rwlock_t msgsUpLock = PTHREAD_RWLOCK_INITIALIZER;


/**
 * Adds an upstream message to the back of the queue.
 */
void
enqueueUpstreamMessage(
    const PROCESS cSrc,
    const erlang_pid *dest,
    ei_x_buff msg,
    const char *context) {
  MsgUpT *msgUp = calloc(1, sizeof(MsgUpT));
  msgUp->cSrc = cSrc;
  ClientDataT *clientData = getClientByPid(cSrc);
  msgUp->clientId =
      strdup(clientData == NULL ? "<unknown>" : clientData->id);
  msgUp->eDest = *dest;
  msgUp->buf = msg;
  msgUp->context = context;
  msgUp->next = NULL;

  LOCK_WRLOCK(&msgsUpLock);
  if (msgsUp == NULL) {
    msgsUp = msgUp;
  }
  else {
    for (MsgUpT *p = msgsUp; p != NULL; p = p->next) {
      if (p->next == NULL) {
        p->next = msgUp;
        break;
      }
    }
  }
  LOCK_UNLOCK(&msgsUpLock);
  return;
}


/**
 * Deallocates an upstream message.
 */
void freeMsgUp(MsgUpT *msg) {
  if (msg->buf.buff != NULL) {
    FREE(msg->buf.buff);
  }
  FREE((void *)msg->clientId);
  FREE(msg);
}


/**
 * Unlinks the front-end upstream message, if one
 * exists.
 */
MsgUpT *
unlinkMsgUp() {
  LOCK_WRLOCK(&msgsUpLock);
  if (msgsUp == NULL) {
    LOCK_UNLOCK(&msgsUpLock);
    return NULL;
  }
  else {
    MsgUpT *result = msgsUp;
    msgsUp = msgsUp->next;
    LOCK_UNLOCK(&msgsUpLock);
    return result;
  }
}


/**
 * Places a null-terminated node name in the given buffer
 * of the given size n. The name is formed by concatenating
 * "ift_app_" and the value of the SNAME environment variable.
 * If SNAME is not set then "ift_app" is used for the node
 * name. The resulting string is truncated to length n-1
 * if its length would exceed n-1; null termination is
 * provided in all cases.
 */
static void
getNodename(char *nodename, size_t n) {
  const char *nodenameDefault = "ift_app";
  char *sname = getenv("SNAME");
  if (sname == NULL) {
    strncpy(nodename, nodenameDefault, n);
  }
  else {
    snprintf(nodename, n, "ift_app_%s", sname);
  }
  nodename[n - 1] = '\0';
}


/**
 * Initializes a socket and returns a file descriptor. The given
 * port is set. In case of failure a negative integer is returned.
 */
static int
getListenFd(int *port) {
  int listen_fd = socket(AF_INET, SOCK_STREAM, 0);
  if (listen_fd < 0) {
    APPLOG_E("failed to create socket: %s", strerror(errno));
    return -1;
  }

  int on = 1;
  if (setsockopt(listen_fd, SOL_SOCKET, SO_REUSEADDR, &on, sizeof(on)) < 0) {
    close(listen_fd);
    APPLOG_E("failed to set socket options: %s", strerror(errno));
    return -2;
  }

  struct sockaddr_in addr;
  memset(&addr, 0, sizeof(addr));
  addr.sin_family = AF_INET;
  addr.sin_port = htons(*port);              // always zero by static analysis!!
  addr.sin_addr.s_addr = htonl(INADDR_ANY);

  if (bind(listen_fd, (struct sockaddr*)&addr, sizeof addr) < 0) {
    close(listen_fd);
    APPLOG_E("failed to bind: %s", strerror(errno));
    return -3;
  }

  int backlog = 5;
  if (listen(listen_fd, backlog) < 0) {
    close(listen_fd);
    APPLOG_E("failed to listen: %s", strerror(errno));
    return -4;
  }

  socklen_t len = sizeof(struct sockaddr *);
  if (getsockname(listen_fd, (struct sockaddr *)&addr, &len) < 0) {
    close(listen_fd);
    APPLOG_E("failed to set socket name: %s", strerror(errno));
    return -5;
  }

  APPLOG_D("got socket name length: %d", len);

  *port = (int)ntohs(addr.sin_port);
  APPLOG_D("port was set to: %d", *port);

  return listen_fd;
}


/**
 * Creates a child process and returns its pid. The
 * process name is formed by prepending "ift_client_"
 * to the given child name.
 */
static PROCESS
createChildProcess(char *id) {
  QENTER2("create_child_process, name: %s", id);

  QINFO1("MASTER create_block\n");
  char *blocknamePrefix = "ift_block_";
  int blocknameLength = strlen(blocknamePrefix) + strlen(id) + 1;
  char blockname[blocknameLength];
  strcpy(blockname, blocknamePrefix);
  strcat(blockname, id);
  PROCESS Block = create_block(blockname, 0, 0, 0, 0);
  QINFO1("MASTER create_block created\n");

  char *clientnamePrefix = "ift_client_";
  int clientnameLength = strlen(clientnamePrefix) + strlen(id) + 1;
  char clientname[clientnameLength];
  strcpy(clientname, clientnamePrefix);
  strcat(clientname, id);

  PROCESS pid = create_process(OS_PRI_PROC,
                       clientname,
                       client,
                       32768, 4, 0, Block,
                       NULL, 0, 0);

  QINFO2("MASTER create_process, pid: %u", pid);

  QINFO1("MASTER start pid\n");
  start(pid);

  QRETURN1(pid);
}


/**
 * Sends an Erlang term, represented as a string, to the
 * Erlang side.
 */
static int
sendUpstream(int fd, erlang_pid *to, const char *term) {
  ei_x_buff buf;
  ei_x_new(&buf);
  ei_x_format(&buf, term);

  int result = ei_send(fd, to, buf.buff, buf.index);

  // consider eliminating duplicated code here
  ei_x_buff response = buf;
  response.index = 0;
  int ignoredVersion;
  ei_decode_version(response.buff, &(response.index), &ignoredVersion);
  char *respStr = NULL;
  if (ei_s_print_term(&respStr, response.buff, &(response.index)) == -1) {
    APPLOG_E("<--   master, %s; failed to parse term", UPSTREAM_RESPONSE_MASTER);
  }
  else {
    APPLOG_I("<--   master, %s: %s", UPSTREAM_RESPONSE_MASTER, respStr);
    FREE(respStr);
  }

  EI_X_FREE(&buf);
  return result;
}


OS_PROCESS(master) {
  char *applogLevelName = getenv("APPLOG_LEVEL");
  if (applogLevelName != NULL) {
    if (strcmp(applogLevelName, "ERROR") == 0) {
      applogLevel = A_ERROR;
    }
    else if (strcmp(applogLevelName, "WARNING") == 0) {
      applogLevel = A_WARNING;
    }
    else if (strcmp(applogLevelName, "INFO") == 0) {
      applogLevel = A_INFO;
    }
    else if (strcmp(applogLevelName, "DEBUG") == 0) {
      applogLevel = A_DEBUG;
    }
  }

  // applogLevel = A_DEBUG;

  struct utsname unameBuffer;
  int unameResult = uname(&unameBuffer);
  if (unameResult == -1) {
    APPLOG_E("failed to get uname data, %s", strerror(errno));
  }
  else if (strcmp(unameBuffer.machine, "x86_64") == 0) {
    // assuming simulator, TRI trace believed to be flaky
    TRI_trace_enabled = false;
  }

  QENTER1("create_master_process");
  masterPid = current_process();
  APPLOG_I(
      "%s\n"
      "ift_app starting, pid: %u, master.c version: %s\n"
      "system: %s, release: %s, version: %s, hardware: %s",
      "============================================================",
      masterPid,
      "%CCaseRev:	/main/R1A/R2A/R3A/R4A/R5A/R6A/R11A/1 %",
      unameBuffer.sysname,
      unameBuffer.release,
      unameBuffer.version,
      unameBuffer.machine);

  exerciseIcti();

  ei_cnode cnode;
  char nodename[MAXNODELEN+1];
  getNodename(nodename, MAXNODELEN+1);
  if (ei_connect_init(&cnode, nodename, "ift_app", 0) == -1) {
    APPLOG_E("C node failed to initialize, reason: %d", erl_errno);
    erl_err_quit("erl_connect_init");
  }

  // Make a listen socket
  // The port variable will be updated with the TCP port number
  int port = 0;
  int listenfd = getListenFd(&port);
  if (listenfd <= 0) {
    APPLOG_E("Socket creation failed, reason: %d", listenfd);
    erl_err_quit("my_listen");
  }
  else {
    APPLOG_D("have file descriptor: %d", listenfd);
  }

  for (int attempts = 0; true; attempts++) {
    int epmdPort = ei_publish(&cnode, port);
    if (epmdPort == -1 && attempts == PUBLISH_MAX_COUNT) {
      APPLOG_E("Register with epmd failed, reason: %d, %s, terminating",
          erl_errno,
          strerror(erl_errno));
      erl_err_quit("ei_publish");
    }
    else if (epmdPort == -1) {
      APPLOG_W("Register with epmd failed, reason: %d, %s, attempts: %d",
          erl_errno,
          strerror(erl_errno),
          attempts);
      sleepMillis(PUBLISH_RETRY_MILLIS);
    }
    else {
      APPLOG_D("Registered with epmd, port: %d", epmdPort);
      break;
    }
  }

  while (true) {

    int fd;
    for (int laps = 0; true; laps++) {
      ErlConnect conn;
      fd = ei_accept(&cnode, listenfd, &conn);
      if (fd != ERL_ERROR) {
        APPLOG_D("Accepted connection, laps: %d, file descriptor: %d", laps, fd);
        break;
      }
      else if (laps < 10) {
        APPLOG_W("no connection yet, lap: %d, reason: %d", laps, erl_errno);
      }
      else if (laps == 10) {
        APPLOG_E("%s", "no connection yet, further messages suppressed");
      }
    }

    while (true) {
      //struct Monitor_flag monFlag;

      fd_set readmask;
      struct timeval tv = {.tv_sec = 0, .tv_usec = POLL_PERIOD_MILLIS*1000};
      FD_ZERO(&readmask);
      FD_SET(fd,&readmask);
      int r = select(fd+1, &readmask, NULL, NULL, &tv);

      // APPLOG_D("select -> %d", r); // very verbose

      // if r is zero, do nothing; this happens all the time
      // when a test suite is running and the ift_app is idle
      if (r < 0) {
        APPLOG_E("error in 'select', reason: %s", strerror(errno));
      }
      else if (r > 1) {
        // should not happen
        APPLOG_E("unexpected result from select: %d", r);
      }
      else if (r == 1) {
        APPLOG_D("%s", "data on socket");

        // check if there is REALLY data; if not then probably the
        // Erlang side has disconnected
        char xbuf[1];
        size_t xlen = 1;
        ssize_t x = recv(fd, (void *)xbuf, xlen, MSG_PEEK|MSG_DONTWAIT);
        if (x == -1) {
          APPLOG_E("peek failed: reason: %d", x);
        }
        else if (x == 0) {
          APPLOG_I("%s", "assuming other side disconnected");
          close(fd);
          break;
        }
        else if (x == 1) {
          APPLOG_D("peek: have data, leading byte is: %d", xbuf[0]);
        }
        else {
          APPLOG_E("peek: unexpected result: %d", x);
        }

        ei_x_buff buf;
        ei_x_new(&buf);
        erlang_msg emsg;
        int got = ei_xreceive_msg(fd, &emsg, &buf);
        if (got == ERL_TICK) {
          APPLOG_D("received: %s", "ERL_TICK");
        }
        else if (got == ERL_ERROR) {
          // since the peek check was added we don't expect to
          // end up here at all
          EI_X_FREE(&buf);
          APPLOG_E("ERL_ERROR in received message, cause: %d", erl_errno);
          QINFO1("MASTER  ERL_ERROR in received message. Close the file descriptor\n");
          close(fd);
          break;
        }
        else if (emsg.msgtype != ERL_REG_SEND) {
          EI_X_FREE(&buf);
          APPLOG_W("unexpected message type: %ld", emsg.msgtype);
        }
        else {
          APPLOG_D("regular message: %ld, from: <%u,%u,%u>, to: <%u,%u,%u>, to: %s",
              emsg.msgtype,
              emsg.from.creation, emsg.from.num, emsg.from.serial,
              emsg.to.creation, emsg.to.num, emsg.to.serial,
              emsg.toname
          );
          APPLOG_D("buffer: size: %d, index: %d", buf.buffsz, buf.index);

          buf.index = 0;
          int version;
          ei_decode_version(buf.buff, &buf.index, &version);
          APPLOG_D("decoded version: %d", version);

          int tupleSize;
          ei_decode_tuple_header(buf.buff, &buf.index, &tupleSize);
          APPLOG_D("decoded tuple header, size: %d", tupleSize);

          // Assume the tuple size is at least 1; the leading
          // element is the Erlang side PID.
          erlang_pid from;
          ei_decode_pid(buf.buff, &buf.index, &from);
          APPLOG_D("decoded from-pid: <%u,%u,%u>", from.creation, from.num, from.serial);

          if (strcmp(emsg.toname, "master_ping") == 0) {
            EI_X_FREE(&buf);
            APPLOG_I("received: %s", "master_ping");
            sendUpstream(fd, &from, "{ok, master_pong}");
          }
          else if (strcmp(emsg.toname, "tri_trace_on") == 0) {
            EI_X_FREE(&buf);
            TRI_trace_enabled = true;
            APPLOG_I("TRI trace: %s", "on");
            sendUpstream(fd, &from, "{ok, true}");
          }
          else if (strcmp(emsg.toname, "tri_trace_off") == 0) {
            EI_X_FREE(&buf);
            TRI_trace_enabled = false;
            APPLOG_I("TRI trace: %s", "off");
            sendUpstream(fd, &from, "{ok, false}");
          }
          else if (strcmp(emsg.toname, "exit") == 0) {
            // This function is not invoked by rct_proxy but
            // other test libraries may do so
            EI_X_FREE(&buf);
            APPLOG_I("received: %s", "exit");
            QINFO1("MASTER Exiting\n");
            QPUTSF1("MASTER exit");
            sendUpstream(fd, &from, "{ok, exited}");
            exit(EXIT_SUCCESS);
          }
          else {
            // tuple size at least 2, so it is safe to decode the
            // next element which is the child name
            int clientIdType;
            int clientIdSize;
            ei_get_type(buf.buff, &buf.index, &clientIdType, &clientIdSize);
            char clientId[clientIdSize + 1];
            if (clientIdType != ERL_ATOM_EXT) {
              APPLOG_E("unexpected type: %d, size: %d", clientIdType, clientIdSize);
            }
            ei_decode_atom(buf.buff, &buf.index, clientId);
            if (strcmp(emsg.toname, "start") == 0) {
              APPLOG_D("received: %s", "start");
              if (getClientByName(clientId) != NULL) {
                EI_X_FREE(&buf);
                APPLOG_W("already started, cannot start: %s", clientId);
                QINFO1("MASTER clientId_already_in_use\n");
                QPUTSF1("MASTER clientId_already_in_use");
                sendUpstream(fd, &from, "{ok, clientId_already_in_use}");
              }
              else {
                APPLOG_I("start client: %s", clientId);
                QINFO1("MASTER start child\n");
                QPUTSF1("MASTER start child");
                PROCESS const pid = createChildProcess(clientId);
                APPLOG_D("client process created, pid: %u", pid);
                QINFO2("MASTER child pid %i\n", pid);

                int protocolType;
                int protocolSize;
                ei_get_type(buf.buff, &buf.index, &protocolType, &protocolSize);

                APPLOG_D("protocol size: %d", protocolSize);
                if (protocolType != ERL_SMALL_INTEGER_EXT) {
                  APPLOG_E("unexpected protocol type: %d", protocolType);
                }

                long unsigned int protocol;
                ei_decode_ulong(buf.buff, &buf.index, &protocol);
                EI_X_FREE(&buf);

                ClientDataT *newClient = calloc(1, sizeof(ClientDataT));
                newClient->id = strdup(clientId);
                newClient->pid = pid;
                newClient->epid = from;
                newClient->protocol = protocol;
                newClient->sched = NULL;
                addClient(newClient);

                enqueueDownstreamMessage(pid, &from, 0, 0, NULL);
                APPLOG_D("/%u/   --> create child: %u, %s", masterPid, pid, clientId);
                QINFO1("MASTER start child send msg\n");

                APPLOG_D("protocol set for client: %s, protocol: %u",
                    clientId,
                    (unsigned int)protocol);

                QINFO1("MASTER start child end\n");
              }
            }

            else if (strcmp(emsg.toname, "stop") == 0) {
              EI_X_FREE(&buf);
              ClientDataT *c = getClientByName(clientId);
              if (c == NULL) {
                APPLOG_W("unknown client, cannot stop: %s", clientId);
                QINFO1("MASTER stop request, already stopped \n");
                sendUpstream(fd, &from, "{ok, client_already_stopped}");
              }
              else {
                QINFO1("MASTER stop request -> DIE \n");
                PROCESS pid = c->pid;
                enqueueDownstreamMessage(pid, &from, DIE, 0, NULL);
                APPLOG_D("/%u/   --> stop child: %s", masterPid, clientId);

                APPLOG_I("  --> %s, %s/%d",
                    clientId, PROTOCOL_NAME(DIE), 0);

              }

            }
            else if (strcmp(emsg.toname, "send") == 0) {
              APPLOG_D("received: %s", "send");
              ClientDataT *c = getClientByName(clientId);
              if (c == NULL) {
                EI_X_FREE(&buf);
                APPLOG_W("cannot send, client not started: %s", clientId);
                QINFO1("MASTER {ok, client_not_started}\n");
                sendUpstream(fd, &from, "{ok, client_not_started}");
              }
              else {
                APPLOG_D("send to client: %s", clientId);

                // trusting that the type is right here
                long unsigned int func;
                ei_decode_ulong(buf.buff, &buf.index, &func);

                QINFO1("MASTER send msg\n");
                QPUTSF1("MASTER send msg");

                enqueueDownstreamMessage(c->pid, &from, c->protocol, func, &buf);

                ei_x_buff bufCopy = buf;
                char *requestStr = NULL;
                if (ei_s_print_term(&requestStr, bufCopy.buff, &(bufCopy.index)) == -1) {
                  APPLOG_E("  --> %s, %s/%lu, failed to parse arguments",
                      clientId, PROTOCOL_NAME(c->protocol), func);
                }
                else {
                  APPLOG_I("  --> %s, %s/%lu, args: %s",
                      clientId, PROTOCOL_NAME(c->protocol), func, requestStr);
                  FREE(requestStr);
                }
              }
            }

            else {
              APPLOG_W("unknown message, name: %s", emsg.toname);
              EI_X_FREE(&buf);
            }
          }
        } // normal message: ERL_REG_SEND
      } // handle data on socket

      MsgUpT *msgUp = unlinkMsgUp();
      if (msgUp != NULL) {
        // handle one upstream message
        int size = msgUp->buf.index;
        DRCALL(int, sendResult,
            ei_send(fd, &(msgUp->eDest), msgUp->buf.buff, size),
            "<--   /%u/ forward upstream, size: %d", masterPid, size);

        ei_x_buff response = msgUp->buf;
        response.index = 0;
        int ignoredVersion;
        ei_decode_version(response.buff, &(response.index), &ignoredVersion);
        char *upStr = NULL;
        if (ei_s_print_term(&upStr, response.buff, &response.index) == -1) {
          APPLOG_E("<--   %s, %s: failed to parse term", msgUp->clientId, msgUp->context);
        }
        else {
          APPLOG_I("<--   %s, %s: %s", msgUp->clientId, msgUp->context, upStr);
          FREE(upStr);
        }

        if (sendResult != 0) {
          // erl_errno codes are listed in errno-base.h
          APPLOG_E("failed to send upstream: %d, erl_errno: %u", sendResult, erl_errno);
        }
        freeMsgUp(msgUp);
      }
    } // while (true) -- inner
  } // while (true) -- outer
} // master


static pthread_rwlock_t LogFileLock = PTHREAD_RWLOCK_INITIALIZER;

FILE *
appLogAppend() {
  const char *logDirname = getenv("LOG_DIR");
  const int pathnameLength = strlen(logDirname) + 1 + strlen(APPLOG_BASENAME) + 1;
  char pathname[pathnameLength];
  strcpy(pathname, logDirname);
  strcat(pathname, "/");
  strcat(pathname, APPLOG_BASENAME);
  LOCK_WRLOCK(&LogFileLock);
  FILE *stream = fopen(pathname, "a");
  return stream;
}


void
appLogClose(FILE *stream) {
  fclose(stream);
  LOCK_UNLOCK(&LogFileLock);
}


void
appLogTimestamp(FILE *stream) {
  struct timespec res;
  if (clock_gettime(CLOCK_REALTIME, &res) == 0) {
    unsigned int millis = res.tv_nsec/1000000;
    struct tm lt;
    localtime_r(&(res.tv_sec), &lt);
    fprintf(stream, "%4d-%02d-%02dT%02d:%02d:%02d.%03d ",
        lt.tm_year + 1900,
        lt.tm_mon + 1,
        lt.tm_mday,
        lt.tm_hour,
        lt.tm_min,
        lt.tm_sec,
        millis);
  }
  else {
    fprintf(stream, "xxxx-xx-xxTxx:xx:xx.xxx ");
  }
}


long long int makeTag() {
  static struct timespec refTime = {.tv_nsec=-1};
  struct timespec now;
  clock_gettime(CLOCK_REALTIME, &now);
  if (refTime.tv_nsec < 0) {
    refTime = now;
    return makeTag();
  }
  else {
    long long int secsPart = 1000000000LL * (now.tv_sec - refTime.tv_sec);
    return secsPart + (now.tv_nsec - refTime.tv_nsec);
  }
}


/**
 * Consider removing if nowhere used.
 */
static int
handleProtocolsCmd()
{
  printf("Supported protocols:\n"
         "LICI\n"
         "PRI\n");

  return RET_SUCCESS;
}


/**
 * Consider removing if nowhere used.
 */
static const char longTraceAndErrorUsage[] =
  "ift <cmd>\n"
  "where <cmd> is one of:\n"
  "protocols Display supported protocols in ift_app\n";


/**
 * Consider removing if nowhere used.
 */
static int
IFT_traceAndErrorCommand(int argc, char **argv) {
  // Skip command name
  argv++;
  argc--;
  if (argc < 1) {
    printf("Usage: %s", longTraceAndErrorUsage);
    return RET_ERROR;
  }
  if (strcmp(argv[0], "help") == 0) {
    printf("Usage: %s", longTraceAndErrorUsage);
    return RET_SUCCESS;
  }
  if (strcmp(argv[0], "protocols") == 0) {
    return handleProtocolsCmd();
  }
  printf("Usage: %s", longTraceAndErrorUsage);
  return RET_ERROR;
}


/**
 * Consider removing if nowhere used.
 */
void
IFT_addTraceAndErrorCmd() {
  shell_add_cmd("ift", "ift - Handling of ift test application",
                "ift help | <cmd>", IFT_traceAndErrorCommand);
}
