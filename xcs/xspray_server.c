/******************************************************************************
 *
 *      COPYRIGHT (C) Ericsson Radio Systems AB, Sweden
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
 *      XSPRAY,  CAH 109 2191
 *
 * File:
 *      xspray_server.c
 *
 * Author:
 *      Arturo Salinas (esalart)
 *
 * Description:
 *      This file implements the xsprayServer process.
 *
 * Revision history:
 *      2011-12-22 Arturo Salinas (esalart)
 *                 Initial implementation according to CR: BSDB0006283
 *      2012-04-25 Arturo Salinas (esalart)
 *                 Changes for BP
 *      2012-05-09 ewenyao change for echo
 *      2012-08-23 Göran Nordin (egornor)
 *                 HQ34850, XSPRAY often hangs shell and can,t verify high prio
 *                 message echoed to RU.
 *                 Major refactoring to fix blocking issues.
 *      2014-04-02 Göran Nordin (egornor)
 *                 MR 105 65-0163/00699, "RBS 6000 with Radio Dot System".
 *                 - Added statistics.
 *                 - Changed connection establishment to initialize receiver
 *                   that do not have shell.
 *                 - Echo operation is now not terminated until all sent
 *                   messages are received.
 *      2014-10-01 Peter Ulfheden (epeulfh)
 *                 HS93909 Fix for OSE5.7 and GCC -wFormat warnings. Added type
 *                 casting to print/sprintf/scanf functions for %l arguments.
 *      2014-12-01 Peter Ulfheden (epeulfh)
 *                 HS93909 Fix warnings related to OSE5.7
 *      2015-05-07 Vivian XIONG (edanxio)
 *                 Lint corrections for Lint warning level 2. Step 2.
 *      2015-06-25 Göran Nordin (egornor)
 *                 Changed to only use funtions that are supported in lits
 *                 emulation.
 */

/*----------------------------  Include files  ------------------------------*/

#include <limits.h>
#include <string.h>
#include <stdio.h>
#include "xspray.h"
#include "xspray_server.h"
#include "xspray_trace.h"
#include "stdarg.h"

/*----------------------------  CONSTANTS  ----------------------------------*/

/* The current protocol version. */
#define XSPRAY_PROTOCOL_VER 1

/*
 * The name of the environment pointer to use for communicaion towards server.
 */
#define XSPRAY_SERVER_STATUS_ENVPTR "XSPRAY_SERVER_STATUS"
/*
 * Name of the environment variable that defines the sender to use by Tx
 * server.
 */
#define XSPRAY_TX_SRV_SENDER_ENVPTR "XSPRAY_TX_SRV_SENDER"

#define XSPRAY_TX_SRV_KILL "xsprayTxSrvKill"
#define XSPRAY_TX_SRV_RX_SUPERV "xsprayTxSrvRxSuperv"

/*----------------------------  MACROS  -------------------------------------*/

/*----------------------------  Structs and typedefs  -----------------------*/

/*
 * Processes managing an xspray server.
 */
struct XSPRAY_ServerS {
	U32 numOfProcesses;
  PROCESS pid[2];
};

/*
 * Initialise connection with the receiver.
 */
struct XSPRAY_ConnEstablishReqS {
  SIGSELECT sigNo;
  U32 protocolRev;
  /* Mode, used to set mode in tagets that does not have shell. */
  U32 mode; /* 0 = Normal, 1 = Ack, 2 = echo */
};

struct XSPRAY_ConnEstablishCfmS {
  SIGSELECT sigNo;
};

struct XSPRAY_ConnEstablishRejS {
  SIGSELECT sigNo;
  U32 supportedProtocolRev;
  U32 errorCode; /* One of errorcodes defined in XSPRAY_ErrorCode */
};

/*
 * Stores time stamp returned from get_systime.
 */
struct XSPRAY_SysTimeS {
  OSTICK tick;
  OSTICK micro;
};

/*
 * Contents of generated message.
 */
struct XSPRAY_GenMsgS {
  SIGSELECT sigNo;
  union {
    struct {
      U8 value[1];
    } data;
    struct {
      struct XSPRAY_SysTimeS sysTime;
      U8 value[1];
    } dataRTD;
  } u;
};

/*
 * The union below defines signals received and sent by xspray monitor.
 */
union SIGNAL
{
  SIGSELECT                       sigNo;
  struct XSPRAY_ConnEstablishReqS connEstablishReq;
  struct XSPRAY_ConnEstablishCfmS connEstablishCfm;
  struct XSPRAY_ConnEstablishRejS connEstablishRej;
  struct XSPRAY_AttachKillReqS    attachKillReq;
  struct XSPRAY_GenMsgS           genMsg;
};

/*----------------------------  Declaration of External Functions  ----------*/

extern OSENTRYPOINT xsprayTxSrvRxSuperv;

/*----------------------------  Declaration of Local Functions  -------------*/

OSENTRYPOINT xsprayTxSrv;
OSENTRYPOINT xsprayRxSrv;

OSENTRYPOINT xsprayTxSrvKill;
/*
 * Cannot include arpa/inet.h, due to conflicting types for 'send' in
 * sys/socket.h and lits version of ose.h
 */
uint32_t htonl(uint32_t hostlong);
uint32_t ntohl(uint32_t netlong);
static void xspraySrvFwdStateAndDie(struct XSPRAY_SysTimeS *pStartTime,
                                    struct XSPRAY_StatCommonS *pStatCommon,
                                    enum XSPRAY_State state, char *format,
                                    ...);

static void xspraySrvGetNextMsg(struct XSPRAY_CfgTxAndRxAckS *pCfgTxAndRxAck,
                                U32 *pMsgIndex, U32 *pSize);

static void xspraySrvForwardConnect(XSPRAY_serverStatusArray pStatus);
static PROCESS xspraySrvConnect(XSPRAY_serverStatusArray pStatus);

static PROCESS xspraySrvHuntReceiver(XSPRAY_serverStatusArray pStatus);

static PROCESS xspraySrvRequestConnect(XSPRAY_serverStatusArray pStatus,
                                       PROCESS pid);

static U32 xsprayTxSrvSetMsg(U32 modeFlags, union SIGNAL *sig,
                             OSBUFSIZE size, U8 pattern);

static PROCESS xspraySrvWaitForConnect(XSPRAY_serverStatusArray pStatus,
                                       union SIGNAL **pSigPtr);

static int xsprayRxSrvVerifyMsg(U32 modeFlags, union SIGNAL *sig,
                                OSBUFSIZE size, struct XSPRAY_MsgCfgS *pMsgCfg,
                                U8 pattern, char *pErrorStr, U32 errorStrSize);

static U32 xsprayRxSrvGetOffset(const struct XSPRAY_SysTimeS *pStart,
                                const struct XSPRAY_SysTimeS *pStop,
                                const OSTIME systemTick);

/*----------------------------  Definition of Global Variables  -------------*/

/*----------------------------  Definition of Local Variables  --------------*/

/* System tick in us (returned from system_tick()). */
static OSTIME xspraySrvSystemTick;

/*----------------------------  Function Definitions  -----------------------*/

/******************************************************************************
 *
 * Function:
 *      xsprayStartServer
 *
 * Parameters:
 *      pStatusArray  Pointer to status array.
 *
 * Return value:
 *      Handle to started server.
 *
 * Description:
 *      Creates a xspray server.
 *
 *****************************************************************************/
void *xsprayStartServer(XSPRAY_serverStatusArray *pStatusArray)
{

  U32 i;
  PROCESS killSrvPid, rxSupervPid;
  struct XSPRAY_ServerS *hSrv = NULL;

  /*
   * Always initalize static variable. It does not matter if it is overwritten.
   */
  xspraySrvSystemTick = system_tick();

  if(!hunt(XSPRAY_TX_SRV_KILL, 0, &killSrvPid, 0))
  {
    killSrvPid = create_process(OS_PRI_PROC, XSPRAY_TX_SRV_KILL,
                                xsprayTxSrvKill, 256, 0, 0, 0, 0, 0, 0);
    start(killSrvPid);
  }

  if(!hunt(XSPRAY_TX_SRV_RX_SUPERV, 0, &rxSupervPid, 0))
  {
#if !defined(_LITS_OSE_H) && defined(OSE_VERSION) && (OSE_VERSION >= 500)
    rxSupervPid = create_process(OS_INT_PROC, XSPRAY_TX_SRV_RX_SUPERV,
                                 xsprayTxSrvRxSuperv, 256, 31, 0, 0, 0,
                                 (OSVECTOR) -1, 0);
#else
    /*
     * OSE4 does not allow attach from interrupt process therefore a
     * prioritized process need to be created.
     */
    /*
     * lits does currently not support interrupt process without HW vector,
     * ((OSVECTOR) -1),  process therefore a prioritized process need to be
     * created.
     */
    rxSupervPid = create_process(OS_PRI_PROC, XSPRAY_TX_SRV_RX_SUPERV,
                                 xsprayTxSrvRxSuperv, 256, 0, 0, 0, 0, 0, 0);
#endif
    start(rxSupervPid);
  }

  hSrv = (struct XSPRAY_ServerS *) alloc(sizeof(struct XSPRAY_ServerS), 0);
  hSrv->numOfProcesses = 0;

  if ((*pStatusArray)[XSPRAY_STATUS_TAG_CFG_COMMON]->cfgCommon.role ==
        XSPRAY_ROLE_TX &&
      (*pStatusArray)[XSPRAY_STATUS_TAG_CFG_COMMON]->cfgCommon.mode ==
      XSPRAY_MODE_ECHO)
  {
    /* Tx server in echo mode, create echo receiver.*/
    char *pName;

    pName = (char *)
      alloc(strlen((*pStatusArray)[XSPRAY_STATUS_TAG_CFG_COMMON]->
                   cfgCommon.name) + sizeof("Echo"),
            0);
    strcpy(pName,
           (*pStatusArray)[XSPRAY_STATUS_TAG_CFG_COMMON]->cfgCommon.name);
    strcat(pName, "Echo");

    if (!(*pStatusArray)[XSPRAY_STATUS_TAG_CFG_COMMON]->cfgCommon.priority)
    {
      /*
       * The rx echo friend must be higher priotitized than the tx server so
       * if tx server prio is zero then it has to be lowered.
       */
      (*pStatusArray)[XSPRAY_STATUS_TAG_CFG_COMMON]->cfgCommon.priority++;
    }

    hSrv->pid[hSrv->numOfProcesses] =
      create_process(OS_PRI_PROC, pName, xsprayRxSrv, 1024,
                     (*pStatusArray)[XSPRAY_STATUS_TAG_CFG_COMMON]->
                       cfgCommon.priority - 1,
                     0, 0, 0, 0, 0);
    hSrv->numOfProcesses++;
    free_buf((union SIGNAL **) &pName);

    if (!set_envp(hSrv->pid[hSrv->numOfProcesses - 1],
                  XSPRAY_SERVER_STATUS_ENVPTR, (OSADDRESS) pStatusArray))
    {
      goto xsprayStartServerError;
    }
  }

  /* Create Rx or Tx server. */
  hSrv->pid[hSrv->numOfProcesses] =
    create_process(OS_PRI_PROC,
                   (*pStatusArray)[XSPRAY_STATUS_TAG_CFG_COMMON]->
                     cfgCommon.name,
                   ((*pStatusArray)[XSPRAY_STATUS_TAG_CFG_COMMON]->
                     cfgCommon.role == XSPRAY_ROLE_TX) ?
                   xsprayTxSrv : xsprayRxSrv,
                   1024,
                   (*pStatusArray)[XSPRAY_STATUS_TAG_CFG_COMMON]->
                     cfgCommon.priority,
                   0, 0, 0, 0, 0);

  hSrv->numOfProcesses++;

  if (!set_envp(hSrv->pid[hSrv->numOfProcesses - 1],
                XSPRAY_SERVER_STATUS_ENVPTR, (OSADDRESS) pStatusArray))
  {
    goto xsprayStartServerError;
  }

  if ((*pStatusArray)[XSPRAY_STATUS_TAG_CFG_COMMON]->cfgCommon.role ==
        XSPRAY_ROLE_TX)
  {
    /*
     * Set sender of connection establish and all signals.
     * It will either be the tx server itself or the echo receiver.
     */
	  if (!set_envp(hSrv->pid[hSrv->numOfProcesses - 1],
                  XSPRAY_TX_SRV_SENDER_ENVPTR, (OSADDRESS) hSrv->pid[0]))
    {
      goto xsprayStartServerError;
    }
  }

  /*
   * Dynamic blocks are not supported in lits so instead we let all precesses
   * in group attach to each other.
   */
  for (i = 0; i < hSrv->numOfProcesses; i++)
  {
    U32 j;

    for (j = 0; j < hSrv->numOfProcesses; j++)
    {
      if (i != j)
      {
        union SIGNAL *sig = alloc(sizeof(struct XSPRAY_AttachKillReqS),
                                  XSPRAY_ATTACH_REQ);
        sig->attachKillReq.pidToAttachToOrKill = hSrv->pid[j];
        sig->attachKillReq.killSrvPid = killSrvPid;
        send_w_s(&sig, hSrv->pid[i], rxSupervPid);
      }
    }
  }

  for (i = 0; i < hSrv->numOfProcesses; i++)
  {
    start(hSrv->pid[i]);
  }

  return hSrv;

xsprayStartServerError:

    for (i = 0; i < hSrv->numOfProcesses; i++)
    {
      kill_proc(hSrv->pid[i]);
    }

    free_buf((union SIGNAL **) &hSrv);

  return NULL;
}

/******************************************************************************
 *
 * Function:
 *      xsprayStopServer
 *
 * Parameters:
 *      hServer  Pointer to handle to started server. Pointer is set to NULL.
 *
 * Return value:
 *      Non-zero on success zero on failure.
 *
 * Description:
 *      Kills a xspray server.
 *
 *****************************************************************************/
int xsprayStopServer(void **hServer)
{
  U32 i;
  struct XSPRAY_ServerS *hSrv = (struct XSPRAY_ServerS *) *hServer;

   for (i = 0; i < hSrv->numOfProcesses; i++)
  {
    /* get_ptype needed because Lits does not support stopping zoombie. */
    if (get_ptype(hSrv->pid[i]) !=  OS_ZOOMBIE)
      stop(hSrv->pid[i]);
  }

  for (i = 0; i < hSrv->numOfProcesses; i++)
  {
    /* get_ptype needed because Lits does not support killing zoombie. */
    if (get_ptype(hSrv->pid[i]) !=  OS_ZOOMBIE)
      kill_proc(hSrv->pid[i]);
  }

  *hServer = NULL;

  return 1;
}

/******************************************************************************
 *
 * Process name:
 *      xsprayTxSrvKill
 *
 * Process type:
 *      Interrupt process without HW vector, i.e. created with OSVECTOR -1.
 *
 * Description:
 *      xspray process that supervise connection to receiving process.
 *      It is an interrupt process without HW vector because we want to be
 *      able to stop the tx server regardless of which priority it is
 *      executing on.
 *      If connection is lost then the current block is ordered to be killed.
 *      Because this interrupt process cannot use get_envp to get pointer to
 *      status area it relies on that redirection tables are setup on the tx
 *      server that handles transmission. The OS_ATTACH_SIG should have tx
 *      server as addressee. The XSPRAY_KILL_REQ is sent to the tx server but
 *      but is redirected to xsprayTxSrvKill.
 *
 *****************************************************************************/
OS_PROCESS(xsprayTxSrvKill)
{
  union SIGNAL *sig;
  XSPRAY_serverStatusArray *pStatusArray;

  for (;;)
  {
    sig = receive(xsprayAllSig);

    if (sig->sigNo == XSPRAY_KILL_REQ)
    {
      /* get_ptype needed because Lits does not support killing zoombie. */
      if(get_ptype(sig->attachKillReq.pidToAttachToOrKill) !=  OS_ZOOMBIE) {
	if ((pStatusArray = (XSPRAY_serverStatusArray *)
	     get_envp(sig->attachKillReq.pidToAttachToOrKill,
		      XSPRAY_SERVER_STATUS_ENVPTR)) != NULL)
	  {
	    snprintf((*pStatusArray)[XSPRAY_STATUS_TAG_STAT_COMMON]->
		     statCommon.infoText,
		     sizeof((*pStatusArray)[XSPRAY_STATUS_TAG_STAT_COMMON]->
			    statCommon.infoText),
		     "Connection to receiver lost");
	    (*pStatusArray)[XSPRAY_STATUS_TAG_STAT_COMMON]->statCommon.state =
	      XSPRAY_STATE_DONE;
	    kill_proc(sig->attachKillReq.pidToAttachToOrKill);
	  }
      }
    }
    free_buf(&sig);
  }
}

/******************************************************************************
 *
 * Process name:
 *      xsprayTxSrv
 *
 * Process type:
 *      Prioritized process
 *
 * Description:
 *      xspray processes for transmitting messages.
 *
 *****************************************************************************/
OS_PROCESS(xsprayTxSrv)
{
  PROCESS sender_proc, pid, killSrvPid, rxSupervPid;
  U32 i, j, msgIndex, size, numOfMsgSinceAck;
  U32 *pAbortValue[XSPRAY_NUM_OF_OPTS];
  U8 pattern;
  XSPRAY_serverStatusArray *pStatusArray;
  int firstLap = 1;

  struct XSPRAY_SysTimeS startTime, curTime;
  struct XSPRAY_SysTimeS prevRecTime = {0,0};
                     /* Initialized to avoid compiler warning. */

  struct XSPRAY_CfgCommonS *pCfgCommon;
  struct XSPRAY_CfgAbortS *pCfgAbort;
  struct XSPRAY_CfgTxS *pCfgTx;
  struct XSPRAY_CfgTxAndRxAckS *pCfgTxAndRxAck;
  struct XSPRAY_StatCommonS *pStatCommon;
  struct XSPRAY_StatTxS *pStatTx;
  struct XSPRAY_StatTxAckEchoAndRxS *pStatTxAckEchoAndRx;

  if ((pStatusArray  = (XSPRAY_serverStatusArray *)
       get_envp(current_process(), XSPRAY_SERVER_STATUS_ENVPTR)) == NULL)
  {
    /* Should never happen. */
    kill_proc(current_process());
  }

  if ((sender_proc  = (PROCESS)
       get_envp(current_process(),
                XSPRAY_TX_SRV_SENDER_ENVPTR)) == (PROCESS) NULL)
  {
    /* Should never happen. */
    kill_proc(current_process());
  }

  pCfgCommon = &(*pStatusArray)[XSPRAY_STATUS_TAG_CFG_COMMON]->cfgCommon;
  pCfgAbort = &(*pStatusArray)[XSPRAY_STATUS_TAG_CFG_ABORT]->cfgAbort;
  pCfgTx = &(*pStatusArray)[XSPRAY_STATUS_TAG_CFG_TX]->cfgTx;
  pCfgTxAndRxAck =
    &(*pStatusArray)[XSPRAY_STATUS_TAG_CFG_TX_AND_RX_ACK]->cfgTxAndRxAck;
  pStatCommon = &(*pStatusArray)[XSPRAY_STATUS_TAG_STAT_COMMON]->statCommon;
  pStatTx = &(*pStatusArray)[XSPRAY_STATUS_TAG_STAT_TX]->statTx;
  pStatTxAckEchoAndRx =
    &(*pStatusArray)[XSPRAY_STATUS_TAG_STAT_TX_ACK_ECHO_AND_RX]->
      statTxAckEchoAndRx;

  if (sender_proc != current_process())
  {
    pid = xspraySrvRequestConnect(*pStatusArray, sender_proc);
  }
  else
  {
    pid = xspraySrvConnect(*pStatusArray);
  }

  if(!hunt(XSPRAY_TX_SRV_KILL, 0, &killSrvPid, 0))
  {
    xspraySrvFwdStateAndDie(
      NULL, &(*pStatusArray)[XSPRAY_STATUS_TAG_STAT_COMMON]->statCommon,
      XSPRAY_STATE_ERROR,
      "Failed to find process %s", XSPRAY_TX_SRV_KILL);
  }

  if(hunt(XSPRAY_TX_SRV_RX_SUPERV, 0, &rxSupervPid, 0))
  {

    union SIGNAL *sig = alloc(sizeof(struct XSPRAY_AttachKillReqS),
                              XSPRAY_ATTACH_REQ);
    sig->attachKillReq.pidToAttachToOrKill = pid;
    sig->attachKillReq.killSrvPid = killSrvPid;
    send(&sig, rxSupervPid);

  }
  else
  {
    xspraySrvFwdStateAndDie(
      NULL, &(*pStatusArray)[XSPRAY_STATUS_TAG_STAT_COMMON]->statCommon,
      XSPRAY_STATE_ERROR,
      "Failed to find process %s", XSPRAY_TX_SRV_RX_SUPERV);
  }

  if (pCfgCommon->modeFlags & XSPRAY_MODE_FLAG_ECHO_RTD)
  {
    for (i = 0; i < pCfgTxAndRxAck->numOfMsg; i++)
    {
      if (pCfgTxAndRxAck->msg[i].adjustedSize.min <
          offsetof(struct XSPRAY_GenMsgS, u.dataRTD.sysTime) +
            sizeof(struct XSPRAY_SysTimeS))
      {
        pCfgTxAndRxAck->msg[i].adjustedSize.min =
          offsetof(struct XSPRAY_GenMsgS, u.dataRTD.sysTime) +
            sizeof(struct XSPRAY_SysTimeS);
      }

      if (pCfgTxAndRxAck->msg[i].adjustedSize.max <
          offsetof(struct XSPRAY_GenMsgS, u.dataRTD.sysTime) +
            sizeof(struct XSPRAY_SysTimeS))
      {
        pCfgTxAndRxAck->msg[i].adjustedSize.max =
          offsetof(struct XSPRAY_GenMsgS, u.dataRTD.sysTime) +
            sizeof(struct XSPRAY_SysTimeS);
      }
    }
    snprintf(pStatCommon->infoText,
             sizeof(pStatCommon->infoText),
             "Round trip delay measurements enabled which forces minimum"
             " message size to %d and excludes %d bytes from echo check.",
             offsetof(struct XSPRAY_GenMsgS, u.dataRTD.sysTime) +
               sizeof(struct XSPRAY_SysTimeS), sizeof(struct XSPRAY_SysTimeS));
  }

  /*
   * Initialize abort options.
   */
  for (i = 0;
       i < sizeof(pAbortValue) / sizeof(pAbortValue[XSPRAY_NUM_OF_OPTS]);
       i++)
  {
    pAbortValue[i] = NULL;
  }

  for (i = 0; i < pCfgAbort->numOfConditions; i++)
  {
    pAbortValue[pCfgAbort->condition[i].option] =
      &pCfgAbort->condition[i].value;
  }

  pStatCommon->state = XSPRAY_STATE_BUSY;
  startTime.tick = get_systime(&startTime.micro);

  for (i = 0, msgIndex = 0, size = pCfgTxAndRxAck->msg[0].adjustedSize.min,
         numOfMsgSinceAck = 0, pattern = 0;
       pCfgTx->burstCount == XSPRAY_COUNT_INFINITE || i < pCfgTx->burstCount;
       i++)
  {
    for (j = 0;
         pCfgTx->msgCount == XSPRAY_COUNT_INFINITE || j < pCfgTx->msgCount;
         j++)
    {
      union SIGNAL *sig;
      struct XSPRAY_SysTimeS sendTime;

      sig = alloc(size, pCfgTxAndRxAck->msg[msgIndex].id);

      if (pCfgCommon->mode == XSPRAY_MODE_ECHO ||
          (pCfgCommon->modeFlags & XSPRAY_MODE_FLAG_VERIFY))
      {
        if (xsprayTxSrvSetMsg(pCfgCommon->modeFlags, sig, size, pattern))
        {
          pattern++;
        }
      }

      send_w_s(&sig, sender_proc, pid);

      if (pCfgCommon->mode == XSPRAY_MODE_ACK)
      {
        sendTime.tick = get_systime(&sendTime.micro);
      }

      if (pStatTx->bytes < ULONG_MAX - size)
      {
        pStatTx->bytes += size;
      }

      if (pStatTx->msgs < ULONG_MAX)
      {
        pStatTx->msgs++;
      }

      xspraySrvGetNextMsg(pCfgTxAndRxAck, &msgIndex, &size);
      numOfMsgSinceAck++;

      if (pCfgCommon->mode == XSPRAY_MODE_ACK &&
          numOfMsgSinceAck == pCfgTxAndRxAck->ackCount)
      {
        U32 timeOffset;

        if (pAbortValue[XSPRAY_OPT_ACK] != NULL ||
            (pAbortValue[XSPRAY_OPT_TMDI] != NULL && !firstLap))
        {
          U32 timeout = ULONG_MAX;

          if (pAbortValue[XSPRAY_OPT_ACK] != NULL)
          {
            timeout = *(pAbortValue[XSPRAY_OPT_ACK]);
          }

          if (pAbortValue[XSPRAY_OPT_TMDI] != NULL)
          {
            curTime.tick = get_systime(&curTime.micro);
            timeOffset = xsprayRxSrvGetOffset(&prevRecTime, &curTime,
                                              xspraySrvSystemTick);
            if (timeOffset <  *(pAbortValue[XSPRAY_OPT_TMDI]))
            {
              timeOffset = *(pAbortValue[XSPRAY_OPT_TMDI]) - timeOffset;
            }
            else
            {
              timeOffset = 0;
            }

            if (timeOffset < timeout)
            {
              timeout = timeOffset;
            }
          }

          if ((sig = receive_w_tmo(timeout, xsprayAllSig)) == NIL)
          {
            xspraySrvFwdStateAndDie(
              &startTime, pStatCommon, XSPRAY_STATE_DONE,
              (timeout == *(pAbortValue[XSPRAY_OPT_ACK])) ?
               "Delay between messages exceeds configured a: abort timeout,"
               " aborting ..." :
               "Delay between messages exceeds configured t: abort timeout,"
               " aborting ...");
          }
        }
        else
        {
          sig = receive(xsprayAllSig);
        }

        prevRecTime.tick = get_systime(&prevRecTime.micro);

        if (sender(&sig) != pid)
        {
          xspraySrvFwdStateAndDie(
            &startTime, pStatCommon, XSPRAY_STATE_ERROR,
            "Unexpected signal received, sigNo=0x%x, sender=0x%x",
            sig->sigNo, pid);
        }

        free_buf(&sig);
        numOfMsgSinceAck = 0;

        timeOffset = xsprayRxSrvGetOffset(&sendTime, &prevRecTime,
                                          xspraySrvSystemTick); /*lint !e645 sendTime initialized by get_systime above */

        if (timeOffset < pStatTxAckEchoAndRx->echoAndAck.roundTripDelay.min)
        {
          pStatTxAckEchoAndRx->echoAndAck.roundTripDelay.min = timeOffset;
        }
        if (timeOffset > pStatTxAckEchoAndRx->echoAndAck.roundTripDelay.max)
        {
          pStatTxAckEchoAndRx->echoAndAck.roundTripDelay.max = timeOffset;
        }
      }

      if (pCfgTx->msgDelay)
      {
        delay(pCfgTx->msgDelay);
      }
    } /* for (j = 0; ... )*/

    if (pCfgTx->burstDelay)
    {
      delay(pCfgTx->burstDelay);
    }
  } /* for (i = 0, ... )*/

  if ((*pStatusArray)[XSPRAY_STATUS_TAG_CFG_COMMON]->cfgCommon.mode ==
      XSPRAY_MODE_ECHO)
  {
    /* If echo mode then it is the RX echo server that kills block. */
    stop(current_process());
  }

  xspraySrvFwdStateAndDie(&startTime, pStatCommon, XSPRAY_STATE_DONE, NULL);
}

/******************************************************************************
 *
 * Process name:
 *      xsprayRxSrv
 *
 * Process type:
 *      Prioritized process
 *
 * Description:
 *      xspray processes for receiving messages.
 *
 *****************************************************************************/
OS_PROCESS(xsprayRxSrv)
{
  U32 i;
  U32 *pAbortValue[XSPRAY_NUM_OF_OPTS];
  XSPRAY_serverStatusArray *pStatusArray;
  int bEchoSrv = 0;
  union SIGNAL *sig1 = NULL;

  struct XSPRAY_CfgCommonS *pCfgCommon;
  struct XSPRAY_CfgAbortS *pCfgAbort;
  struct XSPRAY_CfgTxS *pCfgTx;
  struct XSPRAY_CfgTxAndRxAckS *pCfgTxAndRxAck;
  struct XSPRAY_CfgTxAckEchoAndRxS *pCfgTxAckEchoAndRx;
  struct XSPRAY_StatCommonS *pStatCommon;
  struct XSPRAY_StatTxAckEchoAndRxS *pStatTxAckEchoAndRx;


  if ((pStatusArray  = (XSPRAY_serverStatusArray *)
       get_envp(current_process(), XSPRAY_SERVER_STATUS_ENVPTR)) == NULL)
  {
    /* Should never happen. */
    kill_proc(current_process());
  }

  pCfgCommon = &(*pStatusArray)[XSPRAY_STATUS_TAG_CFG_COMMON]->cfgCommon;
  pCfgAbort = &(*pStatusArray)[XSPRAY_STATUS_TAG_CFG_ABORT]->cfgAbort;
  pCfgTx = &(*pStatusArray)[XSPRAY_STATUS_TAG_CFG_TX]->cfgTx;
  pCfgTxAndRxAck =
    &(*pStatusArray)[XSPRAY_STATUS_TAG_CFG_TX_AND_RX_ACK]->cfgTxAndRxAck;
  pCfgTxAckEchoAndRx =
    &(*pStatusArray)[XSPRAY_STATUS_TAG_CFG_TX_ACK_ECHO_AND_RX]->
      cfgTxAckEchoAndRx;
  pStatCommon = &(*pStatusArray)[XSPRAY_STATUS_TAG_STAT_COMMON]->statCommon;
  pStatTxAckEchoAndRx =
    &(*pStatusArray)[XSPRAY_STATUS_TAG_STAT_TX_ACK_ECHO_AND_RX]->
      statTxAckEchoAndRx;

  /*
   * Initialize abort options.
   */
  for (i = 0;
       i < sizeof(pAbortValue) / sizeof(pAbortValue[XSPRAY_NUM_OF_OPTS]);
       i++)
  {
    pAbortValue[i] = NULL;
  }

  for (i = 0; i < pCfgAbort->numOfConditions; i++)
  {
    pAbortValue[pCfgAbort->condition[i].option] =
      &pCfgAbort->condition[i].value;
  }

  if ((*pStatusArray)[XSPRAY_STATUS_TAG_CFG_COMMON]->cfgCommon.role ==
        XSPRAY_ROLE_TX &&
      (*pStatusArray)[XSPRAY_STATUS_TAG_CFG_COMMON]->cfgCommon.mode ==
      XSPRAY_MODE_ECHO)
  {
    /* Tx server in echo mode, i.e. this process is the echo receiver.*/
    bEchoSrv = 1;
  }

  if (bEchoSrv)
  {
    xspraySrvForwardConnect(*pStatusArray);
  }

  for (;;)
  {
    struct XSPRAY_SysTimeS startTime;
    PROCESS pid = 0; /* Initialized to avoid compiler warning. */
    struct XSPRAY_SysTimeS prevRecTime = {0,0};
                     /* Initialized to avoid compiler warning. */

    int firstLap = 1;
    U8 pattern = 0;
    U32 numOfMsgSinceAck = 0;
    U32 msgIndex = 0;
    U32 size = 0;
    unsigned long long numOfMsgToReceive = 0; /* Initialize to avoid compiler
                                                 warning (uninitialized) */

    if (!bEchoSrv)
    {
      pid = xspraySrvWaitForConnect(*pStatusArray, &sig1);
      pStatCommon->state = XSPRAY_STATE_BUSY;
    }

    startTime.tick = get_systime(&startTime.micro);

    for (;;)
    {
      struct XSPRAY_SysTimeS recTime;
      U32 timeOffset;

      if (pAbortValue[XSPRAY_OPT_TMDI] == NULL)
      {
        sig1 = receive(xsprayAllSig);
      }
      else
      {
        if ((sig1 = receive_w_tmo(*(pAbortValue[XSPRAY_OPT_TMDI]),
                                  xsprayAllSig)) == NIL)
        {
          xspraySrvFwdStateAndDie(
            &startTime, pStatCommon, XSPRAY_STATE_DONE,
            "Delay between messages exceeds configured t: abort timeout,"
              " aborting ...");
        }
      }

      recTime.tick = get_systime(&recTime.micro);

      if (firstLap)
      {
        firstLap = 0;
        prevRecTime = recTime;

        if (pCfgTxAndRxAck->numOfMsg)
        {
          size = pCfgTxAndRxAck->msg[0].adjustedSize.min;
        }

        if (bEchoSrv)
        {
          /*
           * If echo server and first signal then store sender as expected pid
           * and calculate number of messages to receive.
           */
          pid = sender(&sig1);

          if (pCfgTx->burstCount == XSPRAY_COUNT_INFINITE ||
              pCfgTx->msgCount == XSPRAY_COUNT_INFINITE)
          {
            numOfMsgToReceive = (unsigned long long) -1;
          }
          else
          {
            numOfMsgToReceive = (unsigned long long)pCfgTx->burstCount *
                                (unsigned long long)pCfgTx->msgCount;
          }
        }
      }

      if (sender(&sig1) != pid)
      {
        if (!bEchoSrv)
        {
          /* If not echo server and connection lost to transmitter that made
           * connection establishment then wait for new connection
           * establishment. Otwrwise ignore signal.
           */
          if (get_ptype(pid) == OS_ZOOMBIE)
          {
            pStatCommon->state = XSPRAY_STATE_IDLE;
            break;
          }

          snprintf((*pStatusArray)[XSPRAY_STATUS_TAG_STAT_COMMON]->statCommon.
                   infoText,
                   sizeof((*pStatusArray)[XSPRAY_STATUS_TAG_STAT_COMMON]->
                          statCommon.infoText),
                   "Ignoring unexpected signal, sigNo=0x%x, sender=0x%x",
                   (unsigned int) sig1->sigNo, (unsigned int) sender(&sig1));
        }
        else
        {
          SIGSELECT sigNo = sig1->sigNo;

          free_buf(&sig1);
          /* If echo server then unexpected sender is treated as error */
          xspraySrvFwdStateAndDie(
            &startTime, pStatCommon, XSPRAY_STATE_ERROR,
            "Unexpected signal received, sigNo=0x%x, sender=0x%x",
            sigNo, pid);
        }
      }
      else
      {
        pStatTxAckEchoAndRx->lastMsg.id = sig1->sigNo;
        pStatTxAckEchoAndRx->lastMsg.size = sigsize(&sig1);

        if (pStatTxAckEchoAndRx->bytes <
            ULONG_MAX - pStatTxAckEchoAndRx->lastMsg.size)
        {
          pStatTxAckEchoAndRx->bytes += pStatTxAckEchoAndRx->lastMsg.size;
        }

        if (pStatTxAckEchoAndRx->msgs < ULONG_MAX)
        {
          pStatTxAckEchoAndRx->msgs++;
        }

        timeOffset = xsprayRxSrvGetOffset(&prevRecTime, &recTime,
                                          xspraySrvSystemTick);
        prevRecTime = recTime;

        for (i = 0; i < pCfgTxAckEchoAndRx->numOfTimeSlots; i++)
        {
          if (timeOffset <= pCfgTxAckEchoAndRx->timeSlot[i])
          {
            if (pStatTxAckEchoAndRx->timeSlotMsgCounter[i] < ULONG_MAX)
            {
              pStatTxAckEchoAndRx->timeSlotMsgCounter[i]++;
            }
            break;
          }
        }

        if (pCfgCommon->modeFlags & XSPRAY_MODE_FLAG_VERIFY || bEchoSrv)
        {
          int msgContainData;
          /* Use max size of info text for error string */
          char errorStr[sizeof(pStatCommon->infoText)];
          U32 *pAbortCount = (bEchoSrv) ?
            pAbortValue[XSPRAY_OPT_ECHO] : pAbortValue[XSPRAY_OPT_VERIFY];

          /*
           * Check echoed message
           */
          msgContainData =
            xsprayRxSrvVerifyMsg(pCfgCommon->modeFlags, sig1,size,
                                 &pCfgTxAndRxAck->msg[msgIndex], pattern,
                                 errorStr,sizeof(errorStr));
          if (msgContainData < 0)
          {
            pStatTxAckEchoAndRx->echoAndVerify.errors++;

            if (pAbortCount != NULL)
            {
              if (pStatTxAckEchoAndRx->echoAndVerify.errors >= *pAbortCount)
              {
                xspraySrvFwdStateAndDie(
                  &startTime, pStatCommon, XSPRAY_STATE_DONE,
                  "%s Number of %s erros have reached configured abort limit,"
                  " aborting ...", errorStr, (bEchoSrv) ? "echo" : "verify");
              }
            }
          }
          else if (msgContainData)
          {
            pattern++;
          }

          xspraySrvGetNextMsg(pCfgTxAndRxAck, &msgIndex, &size);
        }

        if (!bEchoSrv)
        {
          if (pCfgCommon->mode == XSPRAY_MODE_ECHO)
          {
            send(&sig1, sender(&sig1));
            sig1 = NULL;
          }
          else if (pCfgCommon->mode == XSPRAY_MODE_ACK)
          {
            numOfMsgSinceAck++;

            if (numOfMsgSinceAck == pCfgTxAndRxAck->ackCount)
            {
              union SIGNAL *sig2 =
                alloc(size, pCfgTxAndRxAck->msg[msgIndex].id);

              send(&sig2, sender(&sig1));
              xspraySrvGetNextMsg(pCfgTxAndRxAck, &msgIndex, &size);
              numOfMsgSinceAck = 0;
            }
          }
        }
        else
        {
          if (pCfgCommon->modeFlags & XSPRAY_MODE_FLAG_ECHO_RTD)
          {
            timeOffset = xsprayRxSrvGetOffset(&sig1->genMsg.u.dataRTD.sysTime,
                                              &recTime, xspraySrvSystemTick);

            if (timeOffset <
                pStatTxAckEchoAndRx->echoAndAck.roundTripDelay.min)
            {
              pStatTxAckEchoAndRx->echoAndAck.roundTripDelay.min = timeOffset;
            }
            if (timeOffset > pStatTxAckEchoAndRx->echoAndAck.roundTripDelay.max)
            {
              pStatTxAckEchoAndRx->echoAndAck.roundTripDelay.max = timeOffset;
            }

            if (pAbortValue[XSPRAY_OPT_ECHO_RTD] != NULL)
            {

              if (timeOffset >= *(pAbortValue[XSPRAY_OPT_ECHO_RTD]))
              {
                xspraySrvFwdStateAndDie(
                  &startTime, pStatCommon, XSPRAY_STATE_DONE,
                  "Delay between messages exceeds configured E: abort timeout,"
                  " aborting ...");
              }
            }
          }
          if (numOfMsgToReceive != (unsigned long long) -1)
          {
            numOfMsgToReceive--;
            if (!numOfMsgToReceive)
            {
              xspraySrvFwdStateAndDie(&startTime, pStatCommon,
                                      XSPRAY_STATE_DONE, NULL);
            }
          }
        }
      }

      if (sig1 != NULL)
      {
        free_buf(&sig1);
      }
    }
  }
}

/******************************************************************************
 *
 * Function:
 *      xspraySrvFwdStateAndDie
 *
 * Parameters:
 *      pStartTime   Pointer to start time or NULL.
 *      pStatCommon  Pointer to common statistics.
 *      state        state to forward.
 *      format       Format string or NULL.
 *      ...          Variable number of arguments.
 *
 * Return value:
 *      None
 *
 * Description:
 *      Stores state, info and kills current block.
 *
 *****************************************************************************/
static void xspraySrvFwdStateAndDie(struct XSPRAY_SysTimeS *pStartTime,
                                    struct XSPRAY_StatCommonS *pStatCommon,
                                    enum XSPRAY_State state, char *format,
                                    ...)
{
  va_list arg;

  if (pStartTime != NULL)
  {
    struct XSPRAY_SysTimeS curTime;
    curTime.tick = get_systime(&curTime.micro);
    pStatCommon->totalTime =
      (xsprayRxSrvGetOffset(pStartTime, &curTime, xspraySrvSystemTick) + 500) /
        1000;
  }

  if (format != NULL)
  {
    va_start(arg, format);
    INFO("XSPRAY operation aborted");
    vsnprintf(pStatCommon->infoText, sizeof(pStatCommon->infoText), format,
              arg);
    va_end(arg);
  }

  pStatCommon->state = state;
  kill_proc(current_process());
}

/******************************************************************************
 *
 * Function:
 *      xspraySrvGetNextMsg
 *
 * Parameters:
 *      pCfgTxAndRxAck  Pointer to Tx and rx with acknowledge configuration.
 *      pMsgIndex       Pointer to message index.
 *      pSsize          Pointer to message size.
 *
 * Return value:
 *      None.
 *
 * Description:
 *      Get values for next message.
 *
 *****************************************************************************/
static void xspraySrvGetNextMsg(struct XSPRAY_CfgTxAndRxAckS *pCfgTxAndRxAck,
                                U32 *pMsgIndex, U32 *pSize)
{
  if (*pSize == pCfgTxAndRxAck->msg[*pMsgIndex].adjustedSize.max)
  {
	  if(pCfgTxAndRxAck->numOfMsg)
		  *pMsgIndex = ((*pMsgIndex) + 1) % pCfgTxAndRxAck->numOfMsg;
	  *pSize = pCfgTxAndRxAck->msg[*pMsgIndex].adjustedSize.min;
  }
  else
  {
    (*pSize)++;
  }
}

/******************************************************************************
 *
 * Function:
 *      xspraySrvConnect
 *
 * Parameters:
 *      pStatus  Pointer to status area.
 *
 * Return value:
 *      Pid of receiver
 *
 * Description:
 *      Connects to receiver.
 *
 *****************************************************************************/
static PROCESS xspraySrvConnect(XSPRAY_serverStatusArray pStatus)
{
  PROCESS pid = 0;

  while (!pid)
  {
    pid = xspraySrvRequestConnect(pStatus, xspraySrvHuntReceiver(pStatus));
  }

  return pid;
}

/******************************************************************************
 *
 * Function:
 *      xspraySrvHuntReceiver
 *
 * Parameters:
 *      pStatus  Pointer to status area.
 *
 * Return value:
 *      Pid of receiver
 *
 * Description:
 *      Hunts for a receiver.
 *
 *****************************************************************************/
static PROCESS xspraySrvHuntReceiver(XSPRAY_serverStatusArray pStatus)
{
  union SIGNAL *sig;
  SIGSELECT sigNo;
  PROCESS pid;

  sig = alloc(sizeof(SIGSELECT), XSPRAY_HUNT_IND);
  (void) hunt(pStatus[XSPRAY_STATUS_TAG_CFG_TX]->cfgTx.rxName, 0, NULL, &sig);
  sig = receive(xsprayAllSig);
  sigNo = sig->sigNo;
  pid = sender(&sig);
  free_buf(&sig);

  if (sigNo != XSPRAY_HUNT_IND)
  {
    goto xspraySrvHuntReceiverUnexpSig;
  }

  return pid;

xspraySrvHuntReceiverUnexpSig:
  xspraySrvFwdStateAndDie(
    NULL, &pStatus[XSPRAY_STATUS_TAG_STAT_COMMON]->statCommon,
    XSPRAY_STATE_ERROR,
    "Unexpected signal received, sigNo=0x%x, sender=0x%x", sigNo, pid);

  return 0; /* Never executed, added to avoid compiler warning*/
}

/******************************************************************************
 *
 * Function:
 *      xspraySrvRequestConnect
 *
 * Parameters:
 *      pStatus  Pointer to status area.
 *      pid      receiver of connection request.
 *
 * Return value:
 *      Pid of connected receiver
 *
 * Description:
 *      Sends connection request to receiver.
 *
 *****************************************************************************/
static PROCESS xspraySrvRequestConnect(XSPRAY_serverStatusArray pStatus,
                                       PROCESS pid)
{
  union SIGNAL *sig;
  SIGSELECT sigNo;
  PROCESS sender_proc;
  OSATTREF attRef;

  attRef = attach(NULL, pid);
  sig = alloc(sizeof(struct XSPRAY_ConnEstablishReqS),
              XSPRAY_CONN_ESTABLISH_REQ);
  sig->connEstablishReq.protocolRev = htonl(XSPRAY_PROTOCOL_VER);
  sig->connEstablishReq.mode = htonl(
    pStatus[XSPRAY_STATUS_TAG_CFG_COMMON]->cfgCommon.mode);
  send(&sig, pid);
  sig = receive(xsprayAllSig);
  sigNo = sig->sigNo;
  sender_proc = sender(&sig);

  if (sigNo == XSPRAY_CONN_ESTABLISH_REJ)
  {
    U32 supportedProtocolRev = ntohl(sig->connEstablishRej.supportedProtocolRev);
    U32 errorCode = ntohl(sig->connEstablishRej.errorCode);

    free_buf(&sig);
    xspraySrvFwdStateAndDie(
      NULL, &pStatus[XSPRAY_STATUS_TAG_STAT_COMMON]->statCommon,
      XSPRAY_STATE_ERROR,
      "Receiver rejected connection with error:0x%x."
        " Expected protVer:0x%x, receiver supports protVer:0x%x",
      errorCode, XSPRAY_PROTOCOL_VER, supportedProtocolRev);
  }

  free_buf(&sig);

  if (sigNo == OS_ATTACH_SIG)
  {
    return 0;
  }

  detach(&attRef);

  if (sigNo == XSPRAY_CONN_ESTABLISH_CFM)
  {
    return sender_proc;
  }

  xspraySrvFwdStateAndDie(
    NULL, &pStatus[XSPRAY_STATUS_TAG_STAT_COMMON]->statCommon,
    XSPRAY_STATE_ERROR,
    "Unexpected signal received, sigNo=0x%x, sender=0x%x", sigNo, sender_proc);

  return 0; /* Never executed, added to avoid compiler warning*/
}

/******************************************************************************
 *
 * Function:
 *      xsprayTxSrvSetMsg
 *
 * Parameters:
 *      modeFlags  Mode flags.
 *      sig        Pointer to signal buffer to set
 *      size       Size of signal buffer.
 *      pattern    Pattern to set.
 *
 * Return value:
 *      The number of initilized bytes.
 *
 * Description:
 *      Sets the contents of a message
 *
 *****************************************************************************/
static U32 xsprayTxSrvSetMsg(U32 modeFlags, union SIGNAL *sig,
                             OSBUFSIZE size, U8 pattern)
{
  U32 k = 0;
  U32 offset = (modeFlags & XSPRAY_MODE_FLAG_ECHO_RTD) ?
    offsetof(struct XSPRAY_GenMsgS, u.dataRTD.value) :
    offsetof(struct XSPRAY_GenMsgS, u.data.value);

  if (size > offset)
  {
    U8 *pData = ((U8 *) sig) + offset;

    size -= offset;

    while (k < size)
    {
      pData[k] = pattern + k;
      k++;
    }
  }

  if (modeFlags & XSPRAY_MODE_FLAG_ECHO_RTD)
  {
    sig->genMsg.u.dataRTD.sysTime.tick =
      get_systime(&sig->genMsg.u.dataRTD.sysTime.micro);
  }

  return k;
}

/******************************************************************************
 *
 * Function:
 *      xspraySrvWaitForConnect
 *
 * Parameters:
 *      pStatus     Pointer to status area.
 *      pSigPtr     Address of pointer that points to to received signal or
 *                  NULL.
 *
 * Return value:
 *      Pid of receiver
 *
 * Description:
 *      Waits on connection from an transmitter.
 *
 *****************************************************************************/
static void xspraySrvForwardConnect(XSPRAY_serverStatusArray pStatus)
{
  union SIGNAL *sig;
  SIGSELECT sigNo;
  PROCESS sender_proc, pid = 0;

  sig = receive(xsprayAllSig);
  sigNo = sig->sigNo;
  sender_proc = sender(&sig);
  free_buf(&sig);

  if (sigNo != XSPRAY_CONN_ESTABLISH_REQ)
  {
    goto xspraySrvForwardConnectUnexpSig;
  }

  pid = xspraySrvConnect(pStatus);
  sig = alloc(sizeof(struct XSPRAY_ConnEstablishCfmS),
              XSPRAY_CONN_ESTABLISH_CFM);
  send_w_s(&sig, pid, sender_proc);
  return;

xspraySrvForwardConnectUnexpSig:
  xspraySrvFwdStateAndDie(
    NULL, &pStatus[XSPRAY_STATUS_TAG_STAT_COMMON]->statCommon,
    XSPRAY_STATE_ERROR,
    "Unexpected signal received, sigNo=0x%x, sender=0x%x", sigNo, sender_proc);
}

/******************************************************************************
 *
 * Function:
 *      xspraySrvWaitForConnect
 *
 * Parameters:
 *      pStatus     Pointer to status area.
 *      pSigPtr     Address of pointer that points to to received signal or
 *                  NULL.
 *
 * Return value:
 *      Pid of receiver
 *
 * Description:
 *      Waits on connection from an transmitter.
 *
 *****************************************************************************/
static PROCESS xspraySrvWaitForConnect(XSPRAY_serverStatusArray pStatus,
                                       union SIGNAL **pSigPtr)
{
  PROCESS pid = 0;

  while(!pid)
  {
    if (*pSigPtr == NULL)
    {
      *pSigPtr = receive(xsprayAllSig);
    }

    if ((*pSigPtr)->sigNo == XSPRAY_CONN_ESTABLISH_REQ)
    {
      union SIGNAL *sig2;

      if ((*pSigPtr)->connEstablishReq.protocolRev == htonl(XSPRAY_PROTOCOL_VER))
      {
        sig2 = alloc(sizeof(struct XSPRAY_ConnEstablishCfmS),
                    XSPRAY_CONN_ESTABLISH_CFM);
        pid = sender(pSigPtr);
      }
      else
      {
        snprintf(pStatus[XSPRAY_STATUS_TAG_STAT_COMMON]->statCommon.
                 infoText,
                 sizeof(pStatus[XSPRAY_STATUS_TAG_STAT_COMMON]->statCommon.
                        infoText),
                 "Rejecting invalid connection attempt from transmitter."
                 " Expected protVer:0x%x, got protVer:0x%x",
                 XSPRAY_PROTOCOL_VER, ntohl((*pSigPtr)->connEstablishReq.protocolRev));
        sig2 = alloc(sizeof(struct XSPRAY_ConnEstablishRejS),
                    XSPRAY_CONN_ESTABLISH_REJ);
        sig2->connEstablishRej.supportedProtocolRev = htonl(XSPRAY_PROTOCOL_VER);
        sig2->connEstablishRej.errorCode = htonl(XSPRAY_ERR_WRONG_PARAM);
      }
      send(&sig2, sender(pSigPtr));
    }
    else
    {
      snprintf(pStatus[XSPRAY_STATUS_TAG_STAT_COMMON]->statCommon.
               infoText,
               sizeof(pStatus[XSPRAY_STATUS_TAG_STAT_COMMON]->statCommon.
                      infoText),
               "Ignoring unexpected signal, sigNo=0x%x, sender=0x%x",
               (unsigned int) (*pSigPtr)->sigNo,
               (unsigned int) sender(pSigPtr));
    }
    free_buf(pSigPtr);
    *pSigPtr = NULL;
  }
  return pid;
}

/******************************************************************************
 *
 * Function:
 *      xsprayRxSrvVerifyMsg
 *
 * Parameters:
 *      modeFlags     Mode flags.
 *      sig           Pointer to signal buffer to verify.
 *      size          expected size.
 *      pMsgCfg       Pointer to expected message id and mask.
 *      pattern       Expected pattern.
 *      pErrorStr     Pointer to where error description should be returned.
 *      errorStrSize  Size of pErrorStr.
 *
 * Return value:
 *      < 0 On failure
 *      = 0 When verification succeded and msg did not contain data.
 *      > 0 When verification succeded and msg did contain data.
 *
 * Description:
 *      Verifies the contents of a message
 *
 *****************************************************************************/
static int xsprayRxSrvVerifyMsg(U32 modeFlags, union SIGNAL *sig,
                                OSBUFSIZE size, struct XSPRAY_MsgCfgS *pMsgCfg,
                                U8 pattern, char *pErrorStr, U32 errorStrSize)
{
  int msgContainData = 0;
  int numOfChars;
  U32 i = 0;
  U32 recSize = sigsize(&sig);
  U32 offset = (modeFlags & XSPRAY_MODE_FLAG_ECHO_RTD) ?
    offsetof(struct XSPRAY_GenMsgS, u.dataRTD.value) :
    offsetof(struct XSPRAY_GenMsgS, u.data.value);


  if (size != recSize)
  {
    msgContainData = -1;

    numOfChars =
      snprintf(pErrorStr, errorStrSize,
               "Invalid msg size: expected %u got %u. ", size,
               sigsize(&sig));

    pErrorStr += numOfChars;
    errorStrSize -= numOfChars;

    if (!errorStrSize)
    {
      goto xsprayRxSrvVerifyMsgExit;
    }
  }

  if (((sig->sigNo ^ pMsgCfg->id) & pMsgCfg->echoAndVerify.mask))
  {
    msgContainData = -1;

    numOfChars =
      snprintf(pErrorStr, errorStrSize,
               "Invalid msg id: expected %#010x & %#010x got %#010x. ",
               (unsigned int) pMsgCfg->id,
               (unsigned int) pMsgCfg->echoAndVerify.mask,
               (unsigned int) sig->sigNo);

    pErrorStr += numOfChars;
    errorStrSize -= numOfChars;

    if (!errorStrSize)
    {
      goto xsprayRxSrvVerifyMsgExit;
    }
  }

  if (size > offset && recSize > offset)
  {
    U8 *pData = ((U8 *) sig) + offset;

    if (msgContainData != -1)
    {
      msgContainData = 1;
    }

    size -= offset;
    recSize -= offset;

    while (i < size)
    {
      if ((pData[i] ^ (pattern + i)) & 0xff)
      {
        U32 start = (i > 5) ? (i - 6) : 0;
        U32 end = ((i + 6) > size) ? size :  (i + 6);
        U32 recEnd = ((i + 6) > recSize) ? recSize :  (i + 6);

        msgContainData = -1;

        numOfChars =
          snprintf(pErrorStr, errorStrSize,
                   "Invalid msg data at offset %u. Expected from offset %u:",
                   offset + i, offset + start);

        pErrorStr += numOfChars;
        errorStrSize -= numOfChars;

        if (!errorStrSize)
        {
          goto xsprayRxSrvVerifyMsgExit;
        }

        for (i = start; i < end; i++ )
        {
          numOfChars = snprintf(pErrorStr, errorStrSize, " %#.2x",
                                (unsigned int) (pattern + i));
          pErrorStr += numOfChars;
          errorStrSize -= numOfChars;

          if (!errorStrSize)
          {
            goto xsprayRxSrvVerifyMsgExit;
          }
        }

        numOfChars = snprintf(pErrorStr, errorStrSize, " got:");

        pErrorStr += numOfChars;
        errorStrSize -= numOfChars;

        if (!errorStrSize)
        {
          goto xsprayRxSrvVerifyMsgExit;
        }

        for (i = start; i < recEnd; i++ )
        {
          numOfChars = snprintf(pErrorStr, errorStrSize, " %#.2x", pData[i]);
          pErrorStr += numOfChars;
          errorStrSize -= numOfChars;

          if (!errorStrSize)
          {
            goto xsprayRxSrvVerifyMsgExit;
          }
        }

        snprintf(pErrorStr, errorStrSize, ".");
        goto xsprayRxSrvVerifyMsgExit;

      } /* if (pData[i] != (pattern + i)) */
      i++;
    }
  }

xsprayRxSrvVerifyMsgExit:
  if (msgContainData == -1)
  {
    TRACE_BUS_RECEIVE("Invalid msg:", (U8 *) sig, sigsize(&sig));
  }

  return msgContainData;
}

/******************************************************************************
 *
 * Function:
 *      xsprayRxSrvGetOffset
 *
 * Parameters:
 *      pStart       Pointer to start time.
 *      pStop        Pointer to stop time.
 *      systemTick  System tick in us.
 *
 * Return value:
 *      The offset in us between start and stop time.
 *
 * Description:
 *      Returns the offset in us between start and stop time.
 *
 *****************************************************************************/
static U32 xsprayRxSrvGetOffset(const struct XSPRAY_SysTimeS *pStart,
                                const struct XSPRAY_SysTimeS *pStop,
                                const OSTIME systemTick)
{
  U32 offset;

  if (pStop->tick >= pStart->tick)
  {
    offset = (pStop->tick - pStart->tick) * systemTick + pStop->micro -
      pStart->micro;
  }
  else /* Wrap around */
  {
    offset = (pStop->tick + (0xffffffff - pStart->tick + 1)) * systemTick +
      pStop->micro - pStart->micro;
  }
  return offset;
}
