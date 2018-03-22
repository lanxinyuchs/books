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
 *      A2C Test
 *
 * File:
 *      a2cCmd.c
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
 *      2015-08-28, Peter Marchall (QPETMAR)
 *          Created
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

#include "bcp_a2ci.h"
#include "bcp_a2ci.sig"


/*----------------------------  CONSTANTS  ----------------------------------*/

static const char *a2cHelp =
  "A2C commands:\r\n"
  "a2c -start <instance>\r\n"
  "a2c -kill <instance>\r\n"
  "a2c -ce <instance> <server> <protocol revision>\r\n"
  "a2c -df <instance> <server> <length> <1:st byte> <2:nd byte>...\r\n"
  "a2c -reset <instance> <server>\r\n";

static const char *a2cLazy =
  "A2C command examples:\r\n"
  "a2c -start 0\r\n"
  "a2c -kill  0\r\n"
  "a2c -ce 0 BXP_0/EPP_AISG_42_2 1\r\n"
  "a2c -df 0 BXP_0/EPP_AISG_42_2 5 1 2 3 4 5\r\n"
  "a2c -reset 0 BXP_0/EPP_AISG_42_2\r\n";

/*----------------------------  MACROS  -------------------------------------*/
/*----------------------------  Structs and typedefs  -----------------------*/

union SIGNAL {
  SIGSELECT                     sigNo;
  struct a2ci_connEstablishReqS a2ciConnEstablishReq;
  struct a2ci_connEstablishCfmS a2ciConnEstablishCfm;
  struct a2ci_connEstablishRejS a2ciConnEstablishRej;
  struct a2ci_dataFwdS          a2ciDataFwd;
  struct a2ci_dataIndS          a2ciDataInd;
};


/*----------------------------  Definition of Global Variables  -------------*/

extern OSENTRYPOINT a2cClient;

/*----------------------------  Definition of Local Variables  --------------*/
/*----------------------------  Declaration of Local Functions  -------------*/
/*----------------------------  Function Definitions  -----------------------*/

static PROCESS
a2cHuntClient(char *name, const char *instance)
{
  PROCESS pid = 0;

  (void) sprintf(name, "A2C_Client_%s", instance);
  (void) hunt(name, 0, &pid, 0);

  return pid;
}

static PROCESS
a2cHuntServer(char *name)
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
a2cStart(int argc, char **argv)
{
  PROCESS cPid;
  char    name[64];

  if (argc != 3) {
    printf("The start command takes one parameter\r\n");
    return 1;
  }

  cPid = a2cHuntClient(name, argv[2]);
  if (cPid == 0) {
    cPid = create_process(OS_PRI_PROC, name, a2cClient,
                          1024, 16, 0, 0, 0, 0, 0);
    start(cPid);
    printf("Client \"%s\" (0x%x) successfully started\r\n", name, cPid);
  } else {
    printf("Client \"%s\" (0x%x) already started\r\n", name, cPid);
  }

  return 0;
}

static int
a2cKill(int argc, char **argv)
{
  PROCESS cPid;
  char    name[64];

  if (argc != 3) {
    printf("The kill command takes one parameter\r\n");
    return 1;
  }

  cPid = a2cHuntClient(name, argv[2]);
  if (cPid != 0) {
    kill_proc(cPid);
    printf("Client \"%s\" (0x%x) successfully killed\r\n", name, cPid);
  } else {
    printf("Client \"%s\" does not exist\r\n", name);
  }

  return 0;
}

static int
a2cConnectionEstablish(int argc, char **argv)
{
  union SIGNAL *sig;
  PROCESS       cPid, sPid;
  char          name[64];
  int           status;

  if (argc != 5) {
    printf("The connection establish command takes three parameters\r\n");
    return 1;
  }

  cPid = a2cHuntClient(name, argv[2]);
  if (cPid == 0) {
    printf("Client \"%s\" does not exist\r\n", name);
    return 1;
  }

  sPid = a2cHuntServer(argv[3]);
  if (sPid == 0) {
    printf("Server \"%s\" does not exist\r\n", argv[3]);
    return 1;
  }

  sig = alloc(sizeof(struct a2ci_connEstablishReqS), A2CI_CONN_ESTABLISH_REQ);

  status = sscanf(argv[4], "%hu", &sig->a2ciConnEstablishReq.protocolRev);
  if (status != 1) {
    printf("Invalid value \"%s\" for protocol revision\r\n", argv[4]);
    return 1;
  }

  send_w_s(&sig, sPid, cPid);

  return 0;
}

static int
a2cDataFwd(int argc, char **argv)
{
  union SIGNAL *sig;
  PROCESS       cPid, sPid;
  int           status;
  U32           tmp;
  U16           idx;
  char          name[64];

  if (argc < 6) {
    printf("Too few parameters\r\n");
    return 1;
  }

  cPid = a2cHuntClient(name, argv[2]);
  if (cPid == 0) {
    printf("Client \"%s\" does not exist\r\n", name);
    return 1;
  }

  sPid = a2cHuntServer(argv[3]);
  if (sPid == 0) {
    printf("Server \"%s\" does not exist\r\n", argv[3]);
    return 1;
  }

  sig = alloc(sizeof(struct a2ci_dataFwdS), A2CI_DATA_FWD);

  status = sscanf(argv[4], "%hu", &sig->a2ciDataFwd.length);
  if (status != 1) {
    printf("Invalid value \"%s\" for length\r\n", argv[4]);
    return 1;
  }

  if (argc < (5 + sig->a2ciDataFwd.length)) {
    printf("Too few data parameters\r\n");
    return 1;
  }

  for (idx = 0; idx < sig->a2ciDataFwd.length; idx++) {
    status = sscanf(argv[5 + idx], "0x%lx", &tmp);
    if (status != 1) {
      status = sscanf(argv[5 + idx], "%lu", &tmp);
      if (status != 1) {
        printf("Invalid value \"%s\" for data\r\n", argv[5 + idx]);
        return 1;
      }
    }
    sig->a2ciDataFwd.data[idx] = (U8) tmp;
  }

  send_w_s(&sig, sPid, cPid);

  return 0;
}

static int
a2cResetFwd(int argc, char **argv)
{
  union SIGNAL *sig;
  PROCESS       cPid, sPid;
  char          name[64];

  if (argc != 4) {
    printf("The connection establish command takes two parameters\r\n");
    return 1;
  }

  cPid = a2cHuntClient(name, argv[2]);
  if (cPid == 0) {
    printf("Client \"%s\" does not exist\r\n", name);
    return 1;
  }

  sPid = a2cHuntServer(argv[3]);
  if (sPid == 0) {
    printf("Server \"%s\" does not exist\r\n", argv[3]);
    return 1;
  }

  sig = alloc(sizeof(struct a2ci_resetFwdS), A2CI_RESET_FWD);

  send_w_s(&sig, sPid, cPid);

  return 0;
}

int
a2cCmd(int argc, char **argv)
{
  char cmd[16];
  int  status;

  if (argc < 2)
    goto a2c_cmd_fail;

  status = sscanf(argv[1], "%s", cmd);
  if (status != 1)
    goto a2c_cmd_fail;

  if (strcmp(cmd, "-start") == 0) {
    return a2cStart(argc, argv);
  } else if (strcmp(cmd, "-kill") == 0) {
    return a2cKill(argc, argv);
  } else if (strcmp(cmd, "-ce") == 0) {
    return a2cConnectionEstablish(argc, argv);
  } else if (strcmp(cmd, "-df") == 0) {
    return a2cDataFwd(argc, argv);
  } else if (strcmp(cmd, "-reset") == 0) {
    return a2cResetFwd(argc, argv);
  } else if (strcmp(cmd, "-help") == 0) {
    printf(a2cHelp);
    return 0;
  } else if (strcmp(cmd, "-lazy") == 0) {
    printf(a2cLazy);
    return 0;
  }

a2c_cmd_fail:
  printf("Commands are; -start, -kill, -ce, -df, -reset, -help, -lazy\r\n");
  return 1;
}

void
addCmd_a2c(void)
{
  CelloCri_addShellCommand("a2c",
                           "a2c [-help]|[-lazy]|[<cmd> <...> <...> ...]",
                           "Execute A2C commands.",
                           a2cCmd);
}


/******************************************************************************
 *
 * Process name:
 *      a2cClient
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

OS_PROCESS(a2cClient)
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

      case A2CI_CONN_ESTABLISH_REQ:
        INFO(STR("Sent A2CI_CONN_ESTABLISH_REQ(p:%u) to %s",
                 sig->a2ciConnEstablishReq.protocolRev,
                 name));
        send(&sig, pid);
        break;

      case A2CI_DATA_FWD:
        INFO(STR("Sent A2CI_DATA_FWD(l:%u) to %s",
                 sig->a2ciDataFwd.length,
                 name));
        TRACE_BUS_SEND("A2CI_DATA_FWD.data:",
                       sig->a2ciDataFwd.data, sig->a2ciDataFwd.length);
        send(&sig, pid);
        break;

      case A2CI_RESET_FWD:
        INFO(STR("Sent A2CI_RESET_FWD to %s",
                 name));
        send(&sig, pid);
        break;


      /*
      ** SERVER TO CLIENT SIGNALS.
      */

      case A2CI_CONN_ESTABLISH_CFM:
        INFO(STR("Received A2CI_CONN_ESTABLISH_CFM from %s",
                 name));
        break;

      case A2CI_CONN_ESTABLISH_REJ:
        INFO(STR("Received A2CI_CONN_ESTABLISH_REJ(e:0x%x,p:%u) from %s",
                 sig->a2ciConnEstablishRej.errorCode,
                 sig->a2ciConnEstablishRej.protocolRev,
                 name));
        break;

      case A2CI_DATA_IND:
        INFO(STR("Received A2CI_DATA_IND(l:%u) from %s",
                 sig->a2ciDataInd.length,
                 name));
        TRACE_BUS_RECEIVE("A2CI_DATA_IND.data:",
                          sig->a2ciDataInd.data, sig->a2ciDataInd.length);
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
