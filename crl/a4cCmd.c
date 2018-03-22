/* -*- c -*- ******************************************************************
 *
 *      COPYRIGHT (C)                 Ericsson Radio Systems AB, Sweden
 *
 *      The copyright to the computer program(s) herein is the property
 *      of Ericsson Radio Systems AB.
 *
 *      The program(s) may be used and/or copied only with the written
 *      permission from Ericsson Radio Systems AB or in accordance with
 *      the terms and conditions stipulated in the agreement/contract
 *      under which the program(s) have been supplied.
 *
 *****************************************************************************/

/******************************************************************************
 *
 * Product name:
 *      A4C Test
 *
 * File:
 *      a4cCmd.c
 *
 * Author:
 *      Peter Marchall
 *
 * Description:
 *      .
 *
 * Reviewed:
 *
 * Revision history:
 *      2004-03-22, Peter Marchall (QPETMAR)
 *          File created.
 *      2005-12-10, Sven Löfgren (qlofsve)
 *          Added alternative commands (and processes).
 *      2015-04-29, Peter Marchall (QPETMAR)
 *          Made server selectable.
 *
 *****************************************************************************/

/*----------------------------  Include files  ------------------------------*/

#include <string.h>
#include <stdlib.h>
#include <stdio.h>

#include "ose.h"
#include "osetypes.h"

#include "efs.h"

#include "cello_cri.h"
#include "cello_te_ose.h"
#include "cello_te_trace.h"

#include "bcp_a4ci.h"
#include "bcp_a4ci.sig"


/*----------------------------  CONSTANTS  ----------------------------------*/

static const char *a4cHelp =
  "A4C commands:\r\n"
  "a4c -start <instance>\r\n"
  "a4c -kill <instance>\r\n"
  "a4c -ce <instance> <server> <protocol revision>\r\n"
  "a4c -df <instance> <server> <port> <address> <length> <1:st byte> <2:nd byte>...\r\n"
  "a4c -dr <instance> <server> <port> <address> <length> <1:st byte> <2:nd byte>...\r\n";

static const char *a4cLazy =
  "A4C command examples:\r\n"
  "a4c -start 0\r\n"
  "a4c -kill  0\r\n"
  "a4c -ce 0 A4ciServer 1\r\n"
  "a4c -df 0 A4ciServer 1 1 5 1 2 3 4 5\r\n"
  "a4c -dr 0 A4ciServer 1 1 2 0x6c 0x00\r\n"
  "# CC HW PID (address FCU=0x2e, BFU=0x2d)\r\n"
  "a4c -dr 0 BXP_0/A4ciServer 1 0x2e 2 0x8 1\r\n"
  "a4c -dr 0 BXP_0/A4ciServer 1 0x2e 0\r\n";

/*----------------------------  MACROS  -------------------------------------*/
/*----------------------------  Structs and typedefs  -----------------------*/

union SIGNAL {
  SIGSELECT                     sigNo;
  struct a4ci_connEstablishReqS a4ciConnEstablishReq;
  struct a4ci_connEstablishCfmS a4ciConnEstablishCfm;
  struct a4ci_connEstablishRejS a4ciConnEstablishRej;
  struct a4ci_dataFwdS          a4ciDataFwd;
  struct a4ci_dataReqS          a4ciDataReq;
  struct a4ci_dataCfmS          a4ciDataCfm;
  struct a4ci_dataRejS          a4ciDataRej;
};


/*----------------------------  Definition of Global Variables  -------------*/

extern OSENTRYPOINT a4cClient;

/*----------------------------  Definition of Local Variables  --------------*/
/*----------------------------  Declaration of Local Functions  -------------*/
/*----------------------------  Function Definitions  -----------------------*/

static PROCESS
a4cHuntClient(char *name, const char *instance)
{
  PROCESS pid = 0;

  (void) sprintf(name, "A4C_Client_%s", instance);
  (void) hunt(name, 0, &pid, 0);

  return pid;
}

static PROCESS
a4cHuntServer(char *name)
{
  SIGSELECT     sel[] = { 1, 0xcafebabe };
  PROCESS       pid;
  union SIGNAL *sig;

  sig = alloc(sizeof(SIGSELECT), 0xcafebabe);
  (void) hunt(name, 0, 0, &sig);

  sig = receive_w_tmo((OSTIME) 5000, sel);
  if (sig) {
    pid = sender(&sig);
    free_buf(&sig);
  } else {
    pid = 0;
  }

  return pid;
}

static int
a4cStart(int argc, char **argv)
{
  PROCESS cPid;
  char    name[64];

  if (argc != 3) {
    printf("The start command takes one parameter\r\n");
    return 1;
  }

  cPid = a4cHuntClient(name, argv[2]);
  if (cPid == 0) {
    cPid = create_process(OS_PRI_PROC, name, a4cClient,
                          1024, 16, 0, 0, 0, 0, 0);
    start(cPid);
    printf("Client \"%s\" (0x%x) successfully started\r\n", name, cPid);
  } else {
    printf("Client \"%s\" (0x%x) already started\r\n", name, cPid);
  }

  return 0;
}

static int
a4cKill(int argc, char **argv)
{
  PROCESS cPid;
  char    name[64];

  if (argc != 3) {
    printf("The kill command takes one parameter\r\n");
    return 1;
  }

  cPid = a4cHuntClient(name, argv[2]);
  if (cPid != 0) {
    kill_proc(cPid);
    printf("Client \"%s\" (0x%x) successfully killed\r\n", name, cPid);
  } else {
    printf("Client \"%s\" does not exist\r\n", name);
  }

  return 0;
}

static int
a4cConnectionEstablish(int argc, char **argv)
{
  union SIGNAL *sig;
  PROCESS       cPid, sPid;
  char          name[64];
  int           status;

  if (argc != 5) {
    printf("The connection establish command takes three parameters\r\n");
    return 1;
  }

  cPid = a4cHuntClient(name, argv[2]);
  if (cPid == 0) {
    printf("Client \"%s\" does not exist\r\n", name);
    return 1;
  }

  sPid = a4cHuntServer(argv[3]);
  if (sPid == 0) {
    printf("Server \"%s\" does not exist\r\n", argv[3]);
    return 1;
  }

  sig = alloc(sizeof(struct a4ci_connEstablishReqS), A4CI_CONN_ESTABLISH_REQ);

  status = sscanf(argv[4], "%hu", &sig->a4ciConnEstablishReq.protocolRev);
  if (status != 1) {
    printf("Invalid value \"%s\" for protocol revision\r\n", argv[4]);
    return 1;
  }

  send_w_s(&sig, sPid, cPid);

  return 0;
}

static int
a4cDataFwd(int argc, char **argv)
{
  union SIGNAL *sig;
  PROCESS       cPid, sPid;
  int           status;
  U32           tmp;
  U16           idx;
  char          name[64];

  if (argc < 7) {
    printf("Too few parameters\r\n");
    return 1;
  }

  cPid = a4cHuntClient(name, argv[2]);
  if (cPid == 0) {
    printf("Client \"%s\" does not exist\r\n", name);
    return 1;
  }

  sPid = a4cHuntServer(argv[3]);
  if (sPid == 0) {
    printf("Server \"%s\" does not exist\r\n", argv[3]);
    return 1;
  }

  sig = alloc(sizeof(struct a4ci_dataFwdS), A4CI_DATA_FWD);

  status = sscanf(argv[4], "%hu", &sig->a4ciDataFwd.port);
  if (status != 1) {
    printf("Invalid value \"%s\" for port\r\n", argv[4]);
    return 1;
  }

  status = sscanf(argv[5], "0x%lx", &tmp);
  if (status != 1) {
    status = sscanf(argv[5], "%lu", &tmp);
    if (status != 1) {
      printf("Invalid value \"%s\" for HDLC address\r\n", argv[5]);
      return 1;
    }
  }
  sig->a4ciDataFwd.hdlcAddr = (U8) tmp;

  status = sscanf(argv[6], "%hu", &sig->a4ciDataFwd.length);
  if (status != 1) {
    printf("Invalid value \"%s\" for length\r\n", argv[6]);
    return 1;
  }

  if (argc < (7 + sig->a4ciDataFwd.length)) {
    printf("Too few data parameters\r\n");
    return 1;
  }

  for (idx = 0; idx < sig->a4ciDataFwd.length; idx++) {
    status = sscanf(argv[7 + idx], "0x%lx", &tmp);
    if (status != 1) {
      status = sscanf(argv[7 + idx], "%lu", &tmp);
      if (status != 1) {
        printf("Invalid value \"%s\" for data\r\n", argv[7 + idx]);
        return 1;
      }
    }
    sig->a4ciDataFwd.data[idx] = (U8) tmp;
  }

  send_w_s(&sig, sPid, cPid);

  return 0;
}

static int
a4cDataReq(int argc, char **argv)
{
  union SIGNAL *sig;
  PROCESS       cPid, sPid;
  int           status;
  U32           tmp;
  U16           idx;
  char          name[64];

  if (argc < 7) {
    printf("Too few parameters\r\n");
    return 1;
  }

  cPid = a4cHuntClient(name, argv[2]);
  if (cPid == 0) {
    printf("Client \"%s\" does not exist\r\n", name);
    return 1;
  }

  sPid = a4cHuntServer(argv[3]);
  if (sPid == 0) {
    printf("Server \"%s\" does not exist\r\n", argv[3]);
    return 1;
  }

  sig = alloc(sizeof(struct a4ci_dataReqS), A4CI_DATA_REQ);

  status = sscanf(argv[4], "%hu", &sig->a4ciDataReq.port);
  if (status != 1) {
    printf("Invalid value \"%s\" for port\r\n", argv[4]);
    return 1;
  }

  status = sscanf(argv[5], "0x%lx", &tmp);
  if (status != 1) {
    status = sscanf(argv[5], "%lu", &tmp);
    if (status != 1) {
      printf("Invalid value \"%s\" for HDLC address\r\n", argv[5]);
      return 1;
    }
  }
  sig->a4ciDataReq.hdlcAddr = (U8) tmp;

  status = sscanf(argv[6], "%hu", &sig->a4ciDataReq.length);
  if (status != 1) {
    printf("Invalid value \"%s\" for length\r\n", argv[6]);
    return 1;
  }

  if (argc < (7 + sig->a4ciDataReq.length)) {
    printf("Too few data parameters\r\n");
    return 1;
  }

  for (idx = 0; idx < sig->a4ciDataReq.length; idx++) {
    status = sscanf(argv[7 + idx], "0x%lx", &tmp);
    if (status != 1) {
      status = sscanf(argv[7 + idx], "%lu", &tmp);
      if (status != 1) {
        printf("Invalid value \"%s\" for data\r\n", argv[7 + idx]);
        return 1;
      }
    }
    sig->a4ciDataReq.data[idx] = (U8) tmp;
  }

  send_w_s(&sig, sPid, cPid);

  return 0;
}


int
a4cCmd(int argc, char **argv)
{
  char cmd[16];
  int  status;

  if (argc < 2)
    goto a4c_cmd_fail;

  status = sscanf(argv[1], "%s", cmd);
  if (status != 1)
    goto a4c_cmd_fail;

  if (strcmp(cmd, "-start") == 0) {
    return a4cStart(argc, argv);
  } else if (strcmp(cmd, "-kill") == 0) {
    return a4cKill(argc, argv);
  } else if (strcmp(cmd, "-ce") == 0) {
    return a4cConnectionEstablish(argc, argv);
  } else if (strcmp(cmd, "-df") == 0) {
    return a4cDataFwd(argc, argv);
  } else if (strcmp(cmd, "-dr") == 0) {
    return a4cDataReq(argc, argv);
  } else if (strcmp(cmd, "-help") == 0) {
    printf(a4cHelp);
    return 0;
  } else if (strcmp(cmd, "-lazy") == 0) {
    printf(a4cLazy);
    return 0;
  }

a4c_cmd_fail:
  printf("Commands are; -start, -kill, -ce, -df, -dr, -help, -lazy\r\n");
  return 1;
}

void
addCmd_a4c(void)
{
  CelloCri_addShellCommand("a4c",
                           "a4c [-help]|[-lazy]|[<cmd> <...> <...> ...]",
                           "Execute A4C commands.",
                           a4cCmd);
}


/******************************************************************************
 *
 * Process name:
 *      a4cClient
 *
 * Process type:
 *      Prioritized process
 *
 * Description:
 *      .
 *
 * Used global variables:
 *      None.
 *
 *****************************************************************************/

OS_PROCESS(a4cClient)
{
  static const SIGSELECT  all[] = { 0 };
  union SIGNAL           *sig;
  struct OS_pcb          *pcb;
  char                   *name;
  PROCESS                 pid;

  for (;;) {

    sig = receive((SIGSELECT*) all);
    pid = sender(&sig);

    pcb = get_pcb(pid);
    if (pcb == NULL) {
      TRACE_ERROR("No PCB");
      free_buf(&sig);
      continue;
    }
    name = &pcb->strings[pcb->name];

    switch (sig->sigNo) {

      /*
      ** CLIENT TO SERVER SIGNALS.
      */

      case A4CI_CONN_ESTABLISH_REQ:
        INFO(STR("Sent A4CI_CONN_ESTABLISH_REQ(p:%u) to %s",
                 sig->a4ciConnEstablishReq.protocolRev,
                 name));
        send(&sig, pid);
        break;

      case A4CI_DATA_FWD:
        INFO(STR("Sent A4CI_DATA_FWD(p:%u,a:0x%x,l:%u) to %s",
                 sig->a4ciDataFwd.port,
                 sig->a4ciDataFwd.hdlcAddr,
                 sig->a4ciDataFwd.length,
                 name));
        TRACE_BUS_SEND("A4CI_DATA_FWD.data:",
                       sig->a4ciDataFwd.data, sig->a4ciDataFwd.length);
        send(&sig, pid);
        break;

      case A4CI_DATA_REQ:
        INFO(STR("Sent A4CI_DATA_REQ(p:%u,a:0x%x,l:%u) to %s",
                 sig->a4ciDataReq.port,
                 sig->a4ciDataReq.hdlcAddr,
                 sig->a4ciDataReq.length,
                 name));
        TRACE_BUS_SEND("A4CI_DATA_REQ.data:",
                       sig->a4ciDataReq.data, sig->a4ciDataReq.length);
        send(&sig, pid);
        break;


      /*
      ** SERVER TO CLIENT SIGNALS.
      */

      case A4CI_CONN_ESTABLISH_CFM:
        INFO(STR("Received A4CI_CONN_ESTABLISH_CFM from %s",
                 name));
        break;

      case A4CI_CONN_ESTABLISH_REJ:
        INFO(STR("Received A4CI_CONN_ESTABLISH_REJ(e:0x%x,p:%u) from %s",
                 sig->a4ciConnEstablishRej.errorCode,
                 sig->a4ciConnEstablishRej.protocolRev,
                 name));
        break;

      case A4CI_DATA_CFM:
        INFO(STR("Received A4CI_DATA_CFM(p:%u,a:0x%x,l:%u) from %s",
                 sig->a4ciDataCfm.port,
                 sig->a4ciDataCfm.hdlcAddr,
                 sig->a4ciDataCfm.length,
                 name));
        TRACE_BUS_RECEIVE("A4CI_DATA_CFM.data:",
                          sig->a4ciDataCfm.data, sig->a4ciDataCfm.length);
        break;

      case A4CI_DATA_REJ:
        INFO(STR("Received A4CI_DATA_REJ(e:0x%x,p:%u,a:0x%x) from %s",
                 sig->a4ciDataRej.errorCode,
                 sig->a4ciDataRej.port,
                 sig->a4ciDataRej.hdlcAddr,
                 name));
        break;

      default:
        TRACE_ERROR(STR("Received unexpected signal 0x%08x from %s",
                        sig->sigNo, name));
        break;
    }

    if (sig) {
      free_buf(&sig);
    }

    free_buf((union SIGNAL**)(void*) &pcb);

  }
}
