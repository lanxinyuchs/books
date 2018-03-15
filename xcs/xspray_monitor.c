/****************************************************************************
 *
 *  COPYRIGHT (C) Ericsson Radio Systems AB, Sweden
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
 *      xspray_monitor.c
 *
 * Author:
 *      Arturo Salinas (esalart)
 *
 * Description:
 *      This file implements the xsprayMonitor process.
 *
 * Revision history:
 *      2012-01-26, Arturo Salinas (esalart)
 *              Initial implementation according to CR: BSDB0006283
 *
 *      2012-04-25 Arturo Salinas (esalart)
 *                 Changes for BP
 *      2012-05-09 ewenyao change for echo
 *      2012-08-23 Göran Nordin (egornor)
 *                 HQ34850, XSPRAY often hangs shell and can,t verify high prio
 *                 message echoed to RU.
 *                 Major refactoring to fix blocking issues.
 *      2013-08-13 Peter Ulfheden (epeulfh)
 *                 Minor fixes for Lint.
 *      2014-04-02 Göran Nordin (egornor)
 *                 MR 105 65-0163/00699, "RBS 6000 with Radio Dot System".
 *                 Added statistics.
 *      2015-04-27 Peter Ulfheden (epeulfh)
 *                 Lint corrections for Lint warning level 2. Step 1.
 *      2015-05-11 Vivian XIONG (edanxio)
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
#include "xspray_monitor.h"
#include "xspray_trace.h"
#include "stdarg.h"
#include "ose.h"
/*----------------------------  CONSTANTS  ----------------------------------*/

#define XSPRAY_DEFAULT_PRIORITY          31
#define XSPRAY_DEFAULT_ACK_COUNT         1
#define XSPRAY_DEFAULT_ECHO_VERIFY_MASK  0xffffffff
#define XSPRAY_DEFAULT_ID                1
#define XSPRAY_MIN_SIZE                  sizeof(SIGSELECT)
#define XSPRAY_DEFAULT_SIZE              XSPRAY_MIN_SIZE
#define XSPRAY_DEFAULT_MSG_COUNT         10
#define XSPRAY_DEFAULT_MSG_DELAY         0
#define XSPRAY_DEFAULT_BURST_COUNT       XSPRAY_COUNT_INFINITE
#define XSPRAY_DEFAULT_BURST_DELAY       100

/*----------------------------  MACROS  -------------------------------------*/

/*----------------------------  Structs and typedefs  -----------------------*/

struct XSPRAY_ProcSearchTagS {
  enum {
    XSPRAY_SEARCH_NAME,
    XSPRAY_SEARCH_ENTRY
  } type;

  union {
    char *pName;
    void *hEntry;
  } u;
};

struct XSPRAY_ProcS {
  struct XSPRAY_ProcS  *pNext;
  void *hServer;
  XSPRAY_serverStatusArray pStatus;
};

struct XSPRAY_ProcRootS {
  struct XSPRAY_ProcS *pHead;
  struct XSPRAY_ProcS *pTail;
};

/*
 * The union below defines signals received and sent by xspray monitor.
 */
union SIGNAL
{
  SIGSELECT                    sigNo;
  struct XSPRAY_AddReqS        addReq;
  struct XSPRAY_AddCfmS        addCfm;
  struct XSPRAY_RemoveReqS     removeReq;
  struct XSPRAY_RemoveCfmS     removeCfm;
  struct XSPRAY_GetReqS        getReq;
  struct XSPRAY_GetCfmS        getCfm;
  struct XSPRAY_GetNextReqS    getNextReq;
  struct XSPRAY_GetNextCfmS    getNextCfm;
  struct XSPRAY_SetOptionReqS  setOptionReq;
  struct XSPRAY_SetOptionCfmS  setOptionCfm;
  struct XSPRAY_StartReqS      startReq;
  struct XSPRAY_StartCfmS      startCfm;
  struct XSPRAY_StopReqS       stopReq;
  struct XSPRAY_StopCfmS       stopCfm;
  struct XSPRAY_GetStatusReqS  getStatusReq;
  struct XSPRAY_GetStatusCfmS  getStatusCfm;
  struct XSPRAY_RejS           rej;
};

/*----------------------------  Declaration of External Functions  ----------*/

/*----------------------------  Declaration of Local Functions  -------------*/

static void xsprayHandleAdd(struct XSPRAY_ProcRootS *pRoot,
                            struct XSPRAY_AddReqS *pAddReq, PROCESS sender);

static void xsprayHandleRemove(struct XSPRAY_ProcRootS *pRoot,
                               struct XSPRAY_RemoveReqS *pRemoveReq,
                               PROCESS sender);

static void xsprayHandleGet(struct XSPRAY_ProcRootS *pRoot,
                            struct XSPRAY_GetReqS *pGetReq, PROCESS sender);

static void xsprayHandleGetNext(struct XSPRAY_ProcRootS *pRoot,
                                struct XSPRAY_GetNextReqS *pGetNextReq,
                                PROCESS sender);

static void xsprayHandleSetOption(struct XSPRAY_ProcRootS *pRoot,
                                  struct XSPRAY_SetOptionReqS *pSetOptionReq,
                                  PROCESS sender);

static void xsprayHandleStart(struct XSPRAY_ProcRootS *pRoot,
                              struct XSPRAY_StartReqS *pStartReq,
                              PROCESS sender);

static void xsprayHandleStop(struct XSPRAY_ProcRootS *pRoot,
                             struct XSPRAY_StopReqS *pStopReq,
                             PROCESS sender);

static void xsprayHandleGetStatus(struct XSPRAY_ProcRootS *pRoot,
                                  struct XSPRAY_GetStatusReqS *pGetStatusReq,
                                  PROCESS sender);

static void xspraySendReject(PROCESS pid, SIGSELECT sigNo,
                             enum XSPRAY_ErrorCode error, char *format,
                             ...);

static void xsprayEnqueueProc(struct XSPRAY_ProcRootS *pRoot,
                              struct XSPRAY_ProcS *pProc);

static void xsprayDequeueProc(struct XSPRAY_ProcRootS *pRoot,
                              struct XSPRAY_ProcS *pPred,
                              struct XSPRAY_ProcS *pProc);

static struct XSPRAY_ProcS *xsprayFindProc(
  const struct XSPRAY_ProcRootS *pRoot,
  const struct XSPRAY_ProcSearchTagS *pTag,
  struct XSPRAY_ProcS **pPredPtr);

static void *xsprayRealloc(void *pBuf, size_t size, size_t preserveSize);

void xspraySetDefaultMsgCfg(struct XSPRAY_MsgCfgS *pMsgCfg, SIGSELECT id);

static int xspraySetOptionForMsgId(
  PROCESS sender,
  struct XSPRAY_SetOptionReqS *pSetOptionReq,
  struct XSPRAY_CfgTxAndRxAckS *pCfgTxAndRxAck);

static struct XSPRAY_OptionInstanceS *xsprayInvOptInstForMsgId(
  struct XSPRAY_SetOptionReqS *pSetOptionReq,
  struct XSPRAY_CfgTxAndRxAckS *pCfgTxAndRxAck);

static int xsprayGetOptValueForMsgId(
  struct XSPRAY_SetOptionReqS *pSetOptionReq, U32 id, U32 orderOfId,
  U32 *pValue);

static void xsprayResetStat(struct XSPRAY_ProcS *pProc);

/*----------------------------  Definition of Global Variables  -------------*/

/*----------------------------  Definition of Local Variables  --------------*/

/*----------------------------  Function Definitions  -----------------------*/

/******************************************************************************
 *
 * Process name:
 *      xsprayMonitor
 *
 * Process type:
 *      Prioritized process
 *
 * Description:
 *      Manages xspray processes for receiving and transmitting messages.
 *
 *****************************************************************************/
OS_PROCESS(xsprayMonitor)
{
  union SIGNAL *sig;
  PROCESS senderProc;
  struct XSPRAY_ProcRootS procRoot;

  procRoot.pHead = procRoot.pTail = 0;

  for (;;)
  {
    sig = receive(xsprayAllSig);
    senderProc = sender(&sig);

    switch (sig->sigNo)
    {
      case XSPRAY_ADD_REQ:
        TRACE_REC_SIG(sig, STR("XSPRAY_ADD_REQ(name:\"%s\")",
                               sig->addReq.name));

        xsprayHandleAdd(&procRoot, &sig->addReq, senderProc);
        break;
      case XSPRAY_REMOVE_REQ:
        TRACE_REC_SIG(sig, STR("XSPRAY_REMOVE_REQ(hEntry:0x%x)",
                                (unsigned int)sig->removeReq.hEntry));

        xsprayHandleRemove(&procRoot, &sig->removeReq, senderProc);
        break;
      case XSPRAY_GET_REQ:
        TRACE_REC_SIG(sig, STR("XSPRAY_GET_REQ(name:\"%s\")",
                               sig->getReq.name));

        xsprayHandleGet(&procRoot, &sig->getReq, senderProc);
        break;
      case XSPRAY_GET_NEXT_REQ:
        TRACE_REC_SIG(sig, STR("XSPRAY_GET_NEXT_REQ(hEntry:0x%x)",
                               (unsigned int)sig->getNextReq.hEntry));

        xsprayHandleGetNext(&procRoot, &sig->getNextReq, senderProc);
        break;
      case XSPRAY_SET_OPTION_REQ:
        TRACE_REC_SIG(sig, STR("XSPRAY_SET_OPTION_REQ(hEntry:0x%x,"
                               "option:%u,numOfInstances:%u)",
                               (unsigned int)sig->setOptionReq.hEntry,
                               sig->setOptionReq.option,
                               sig->setOptionReq.numOfInstances));

        xsprayHandleSetOption(&procRoot, &sig->setOptionReq, senderProc);
        break;
      case XSPRAY_START_REQ:
        TRACE_REC_SIG(sig, STR("XSPRAY_START_REQ(hEntry:0x%x,rxName:\"%s\")",
                               (unsigned int)sig->startReq.hEntry,
                               sig->startReq.rxName));

        xsprayHandleStart(&procRoot, &sig->startReq, senderProc);
        break;
      case XSPRAY_STOP_REQ:
        TRACE_REC_SIG(sig, STR("XSPRAY_STOP_REQ(hEntry:0x%x)",
                               (unsigned int)sig->stopReq.hEntry));

        xsprayHandleStop(&procRoot, &sig->stopReq, senderProc);
        break;
      case XSPRAY_GET_STATUS_REQ:
        TRACE_REC_SIG(sig, STR("XSPRAY_GET_STATUS_REQ(hEntry:0x%x,tag:%d)",
                               (unsigned int)sig->getStatusReq.hEntry,
                               sig->getStatusReq.tag));

        xsprayHandleGetStatus(&procRoot, &sig->getStatusReq, senderProc);
        break;
      default:
        TRACE_ERROR(STR("Unexpected signal received, sigNo=0x%x, sender=0x%x",
                      sig->sigNo, senderProc));
        break;
    }

    free_buf(&sig);
  }
}

/******************************************************************************
 *
 * Function:
 *      xsprayHandleAdd
 *
 * Parameters:
 *      pRoot    Pointer to root of process list
 *      pAddReq  Pointer to request.
 *      sender   sender of request.
 *
 * Return value:
 *      None
 *
 * Description:
 *      Handles add request.
 *
 *****************************************************************************/
static void xsprayHandleAdd(struct XSPRAY_ProcRootS *pRoot,
                            struct XSPRAY_AddReqS *pAddReq, PROCESS sender)
{
  struct XSPRAY_ProcSearchTagS tag;
  union SIGNAL *sig;
  struct XSPRAY_ProcS *pProc;

  tag.type = XSPRAY_SEARCH_NAME;
  tag.u.pName = pAddReq->name;

  if (xsprayFindProc(pRoot, &tag, NULL) != NULL)
  {
    xspraySendReject(sender, XSPRAY_ADD_REJ, XSPRAY_ERR_WRONG_PARAM,
                     "%s already exist", pAddReq->name);
    return;
  }

  pProc = (struct XSPRAY_ProcS *) alloc(sizeof(struct XSPRAY_ProcS), 0);
  memset(pProc, 0, sizeof(*pProc));
  pProc->pStatus[XSPRAY_STATUS_TAG_CFG_COMMON] = (union XSPRAY_Status *)
    alloc(offsetof(struct XSPRAY_CfgCommonS, name) + strlen(pAddReq->name) +
          sizeof('\0'), 0);
  pProc->pStatus[XSPRAY_STATUS_TAG_CFG_COMMON]->cfgCommon.priority =
    XSPRAY_DEFAULT_PRIORITY;
  pProc->pStatus[XSPRAY_STATUS_TAG_CFG_COMMON]->cfgCommon.mode =
    XSPRAY_MODE_NORMAL;
  pProc->pStatus[XSPRAY_STATUS_TAG_CFG_COMMON]->cfgCommon.modeFlags = 0;
  strcpy(pProc->pStatus[XSPRAY_STATUS_TAG_CFG_COMMON]->cfgCommon.name,
         pAddReq->name);

  pProc->pStatus[XSPRAY_STATUS_TAG_CFG_ABORT] = (union XSPRAY_Status *)
    alloc(offsetof(struct XSPRAY_CfgAbortS, condition), 0);
  pProc->pStatus[XSPRAY_STATUS_TAG_CFG_ABORT]->cfgAbort.numOfConditions = 0;

  pProc->pStatus[XSPRAY_STATUS_TAG_CFG_TX] = (union XSPRAY_Status *)
    alloc(sizeof(struct XSPRAY_CfgTxS), 0);
  pProc->pStatus[XSPRAY_STATUS_TAG_CFG_TX]->cfgTx.msgCount =
    XSPRAY_DEFAULT_MSG_COUNT;
  pProc->pStatus[XSPRAY_STATUS_TAG_CFG_TX]->cfgTx.msgDelay =
    XSPRAY_DEFAULT_MSG_DELAY;
  pProc->pStatus[XSPRAY_STATUS_TAG_CFG_TX]->cfgTx.burstCount =
    XSPRAY_DEFAULT_BURST_COUNT;
  pProc->pStatus[XSPRAY_STATUS_TAG_CFG_TX]->cfgTx.burstDelay =
    XSPRAY_DEFAULT_BURST_DELAY;
  pProc->pStatus[XSPRAY_STATUS_TAG_CFG_TX]->cfgTx.rxName[0] = '\0';

  pProc->pStatus[XSPRAY_STATUS_TAG_CFG_TX_AND_RX_ACK] = (union XSPRAY_Status *)
    alloc(sizeof(struct XSPRAY_CfgTxAndRxAckS), 0);
  pProc->pStatus[XSPRAY_STATUS_TAG_CFG_TX_AND_RX_ACK]->cfgTxAndRxAck.ackCount =
    XSPRAY_DEFAULT_ACK_COUNT;
  pProc->pStatus[XSPRAY_STATUS_TAG_CFG_TX_AND_RX_ACK]->cfgTxAndRxAck.
    numOfMsg = 1;
  xspraySetDefaultMsgCfg(
     &pProc->pStatus[XSPRAY_STATUS_TAG_CFG_TX_AND_RX_ACK]->cfgTxAndRxAck.
     msg[0],
     XSPRAY_DEFAULT_ID);

  pProc->pStatus[XSPRAY_STATUS_TAG_CFG_TX_ACK_ECHO_AND_RX] = (union XSPRAY_Status *)
    alloc(offsetof(struct XSPRAY_CfgTxAckEchoAndRxS, timeSlot), 0);
  pProc->pStatus[XSPRAY_STATUS_TAG_CFG_TX_ACK_ECHO_AND_RX]->cfgTxAckEchoAndRx.
    numOfTimeSlots = 0;

  pProc->pStatus[XSPRAY_STATUS_TAG_STAT_COMMON] = (union XSPRAY_Status *)
    alloc(sizeof(struct XSPRAY_StatCommonS), 0);
  pProc->pStatus[XSPRAY_STATUS_TAG_STAT_TX] = (union XSPRAY_Status *)
    alloc(sizeof(struct XSPRAY_StatTxS), 0);
  pProc->pStatus[XSPRAY_STATUS_TAG_STAT_TX_ACK_ECHO_AND_RX] =
    (union XSPRAY_Status *)
      alloc(offsetof(struct XSPRAY_StatTxAckEchoAndRxS, timeSlotMsgCounter),
            0);
  xsprayResetStat(pProc);
  xsprayEnqueueProc(pRoot, pProc);

  sig =  alloc(sizeof(struct XSPRAY_AddCfmS), XSPRAY_ADD_CFM);
  sig->addCfm.hEntry = xsprayFindProc(pRoot, &tag, NULL);
  TRACE_SEND_SIG(sig, sender, STR("XSPRAY_ADD_CFM(hEntry:0x%x)",
                                  (unsigned int)sig->addCfm.hEntry));
  send(&sig, sender);
}

/******************************************************************************
 *
 * Function:
 *      xsprayHandleRemove
 *
 * Parameters:
 *      pRoot       Pointer to root of process list
 *      pRemoveReq  Pointer to request.
 *      sender      sender of request.
 *
 * Return value:
 *      None
 *
 * Description:
 *      Handles remove request.
 *
 *****************************************************************************/
static void xsprayHandleRemove(struct XSPRAY_ProcRootS *pRoot,
                               struct XSPRAY_RemoveReqS *pRemoveReq,
                               PROCESS sender)
{
  struct XSPRAY_ProcSearchTagS tag;
  struct XSPRAY_ProcS *pPred;
  struct XSPRAY_ProcS *pProc;
  union SIGNAL *sig;
  U32 i;

  tag.type = XSPRAY_SEARCH_ENTRY;
  tag.u.hEntry = pRemoveReq->hEntry;

  pProc = xsprayFindProc(pRoot, &tag, &pPred);

  if (pProc == NULL)
  {
    xspraySendReject(sender, XSPRAY_REMOVE_REJ, XSPRAY_ERR_WRONG_PARAM,
                     "process Not found");
    return;
  }

  if (pProc->hServer != NULL)
  {
    if (!xsprayStopServer(&pProc->hServer))
    {
      xspraySendReject(sender, XSPRAY_REMOVE_REJ, XSPRAY_ERR_OTHER,
                       "stop failed");
      return;
    }
  }

  xsprayDequeueProc( pRoot, pPred, pProc);

  for (i = 0; i  < sizeof(pProc->pStatus)/sizeof(pProc->pStatus[0]); i++)
  {
    if (pProc->pStatus[i] != NULL)
    {
      free_buf((union SIGNAL **) &pProc->pStatus[i]);
    }
  }

  free_buf((union SIGNAL **) &pProc);
  sig = alloc(sizeof(struct XSPRAY_RemoveCfmS), XSPRAY_REMOVE_CFM);
  TRACE_SEND_SIG(sig, sender, "XSPRAY_REMOVE_CFM");
  send(&sig, sender);
}

/******************************************************************************
 *
 * Function:
 *      xsprayHandleGet
 *
 * Parameters:
 *      pRoot    Pointer to root of process list
 *      pGetReq  Pointer to request.
 *      sender   sender of request.
 *
 * Return value:
 *      None
 *
 * Description:
 *      Handles get request.
 *
 *****************************************************************************/
static void xsprayHandleGet(struct XSPRAY_ProcRootS *pRoot,
                            struct XSPRAY_GetReqS *pGetReq, PROCESS sender)
{
	struct XSPRAY_ProcSearchTagS tag;
  union SIGNAL *sig;

  tag.type = XSPRAY_SEARCH_NAME;
  tag.u.pName = pGetReq->name;

  sig = alloc(sizeof(struct XSPRAY_GetCfmS), XSPRAY_GET_CFM);
  sig->getCfm.hEntry = xsprayFindProc(pRoot, &tag, NULL);
  TRACE_SEND_SIG(sig, sender, STR("XSPRAY_GET_CFM(hEntry:0x%x)",
                                  (unsigned int)sig->getCfm.hEntry));
  send(&sig, sender);
}

/******************************************************************************
 *
 * Function:
 *      xsprayHandleGetNext
 *
 * Parameters:
 *      pRoot        Pointer to root of process list
 *      pGetNextReq  Pointer to request.
 *      sender       sender of request.
 *
 * Return value:
 *      None
 *
 * Description:
 *      Handles get next request.
 *
 *****************************************************************************/
static void xsprayHandleGetNext(struct XSPRAY_ProcRootS *pRoot,
                                struct XSPRAY_GetNextReqS *pGetNextReq,
                                PROCESS sender)
{
  struct XSPRAY_ProcSearchTagS tag;
  struct XSPRAY_ProcS *pProc;
  union SIGNAL *sig;

  tag.type = XSPRAY_SEARCH_ENTRY;
  tag.u.hEntry = pGetNextReq->hEntry;

  sig = alloc(sizeof(struct XSPRAY_GetNextCfmS), XSPRAY_GET_NEXT_CFM);
  sig->getNextCfm.hNext = NULL;

  pProc = xsprayFindProc(pRoot, &tag, NULL);

  if (pProc != NULL)
  {
    sig->getNextCfm.hNext = pProc->pNext;
  }

  TRACE_SEND_SIG(sig, sender, STR("XSPRAY_GET_NEXT_CFM(hNext:0x%x)",
                                  (unsigned int)sig->getNextCfm.hNext));
  send(&sig, sender);
}

/******************************************************************************
 *
 * Function:
 *      xsprayHandleSetOption
 *
 * Parameters:
 *      pRoot          Pointer to root of process list
 *      pSetOptionReq  Pointer to request.
 *      sender         sender of request.
 *
 * Return value:
 *      None
 *
 * Description:
 *      Handles set option request.
 *
 *****************************************************************************/
static void xsprayHandleSetOption(struct XSPRAY_ProcRootS *pRoot,
                                  struct XSPRAY_SetOptionReqS *pSetOptionReq,
                                  PROCESS sender)
{
  struct XSPRAY_ProcSearchTagS tag;
  struct XSPRAY_ProcS *pProc;
  union SIGNAL *sig;
  U32 i;
  int rejected = 0;

  tag.type = XSPRAY_SEARCH_ENTRY;
  tag.u.hEntry = pSetOptionReq->hEntry;

  pProc = xsprayFindProc(pRoot, &tag, NULL);

  if (pProc == NULL)
  {
    xspraySendReject(sender, XSPRAY_SET_OPTION_REJ, XSPRAY_ERR_WRONG_PARAM,
                     "process not found");
    return;
  }

  if (pProc->hServer != NULL)
  {
    xspraySendReject(sender, XSPRAY_SET_OPTION_REJ, XSPRAY_ERR_WRONG_PARAM,
                     "%s has to be stopped before configuration can be "
                       "changed",
                     pProc->pStatus[XSPRAY_STATUS_TAG_CFG_COMMON]->cfgCommon.
                       name);
    return;
  }

  switch(pSetOptionReq->option)
  {
    /* -p <prio> */
    case XSPRAY_OPT_PRIO:
      if (pSetOptionReq->instance[0].value[0] > 31)
      {
        xspraySendReject(sender, XSPRAY_SET_OPTION_REJ,
                         XSPRAY_ERR_WRONG_PARAM,
                         "invalid %d%s argument, must be less or egual to 31",
                         pSetOptionReq->instance[0].argIndex,
                         xsprayGetArgNumSuffix(pSetOptionReq->instance[0].
                                               argIndex));
        rejected = 1;
        break;
      }
      pProc->pStatus[XSPRAY_STATUS_TAG_CFG_COMMON]->cfgCommon.priority =
        pSetOptionReq->instance[0].value[0];
      break;

    /* -a [<count>] */
    case XSPRAY_OPT_ACK:
      pProc->pStatus[XSPRAY_STATUS_TAG_CFG_COMMON]->cfgCommon.mode =
        XSPRAY_MODE_ACK;
      pProc->pStatus[XSPRAY_STATUS_TAG_CFG_TX_AND_RX_ACK]->cfgTxAndRxAck.
        ackCount =
          (pSetOptionReq->instance[0].numOfValues) ?
            pSetOptionReq->instance[0].value[0] : XSPRAY_DEFAULT_ACK_COUNT;
      break;

    /* -e [[<signo>:]<mask>] */
    case XSPRAY_OPT_ECHO:
      /* -i should always precede this option, i.e. default mask is set. */
      if (!xspraySetOptionForMsgId(
            sender,
            pSetOptionReq,
            &pProc->pStatus[XSPRAY_STATUS_TAG_CFG_TX_AND_RX_ACK]->
              cfgTxAndRxAck))
      {
        rejected = 1;
        break;
      }
      else
      {
        pProc->pStatus[XSPRAY_STATUS_TAG_CFG_COMMON]->cfgCommon.mode =
          XSPRAY_MODE_ECHO;
        pProc->pStatus[XSPRAY_STATUS_TAG_CFG_COMMON]->cfgCommon.modeFlags &=
          ~ XSPRAY_MODE_FLAG_ECHO_RTD;
      }
      break;

    /* -E */
    case XSPRAY_OPT_ECHO_RTD:
      if (pProc->pStatus[XSPRAY_STATUS_TAG_CFG_COMMON]->cfgCommon.mode !=
          XSPRAY_MODE_ECHO)
      {
        xspraySendReject(sender, XSPRAY_SET_OPTION_REJ,
                         XSPRAY_ERR_WRONG_STATE,
                         "invalid %d%s argument, must be defined in "
                           "combination with %s",
                         pSetOptionReq->instance[0].argIndex,
                         xsprayGetArgNumSuffix(pSetOptionReq->instance[0].
                                               argIndex),
                         xsprayOptArg[XSPRAY_OPT_ECHO]);
        rejected = 1;
        break;
      }

      pProc->pStatus[XSPRAY_STATUS_TAG_CFG_COMMON]->cfgCommon.modeFlags |=
        XSPRAY_MODE_FLAG_ECHO_RTD;
      break;

    /* -v */
    case XSPRAY_OPT_VERIFY:
      /* -i should always precede this option, i.e. default mask is set. */
      if (!xspraySetOptionForMsgId(
            sender,
            pSetOptionReq,
            &pProc->pStatus[XSPRAY_STATUS_TAG_CFG_TX_AND_RX_ACK]->
              cfgTxAndRxAck))
      {
        rejected = 1;
        break;
      }
      else
      {
        pProc->pStatus[XSPRAY_STATUS_TAG_CFG_COMMON]->cfgCommon.modeFlags |=
          XSPRAY_MODE_FLAG_VERIFY;
      }
      break;

    /* -i <signo> */
    case XSPRAY_OPT_ID:
      pProc->pStatus[XSPRAY_STATUS_TAG_CFG_TX_AND_RX_ACK] =
        xsprayRealloc(
          pProc->pStatus[XSPRAY_STATUS_TAG_CFG_TX_AND_RX_ACK],
          offsetof(struct XSPRAY_CfgTxAndRxAckS, msg) +
            sizeof(struct XSPRAY_MsgCfgS) * pSetOptionReq->numOfInstances,
          offsetof(struct XSPRAY_CfgTxAndRxAckS, msg));

      pProc->pStatus[XSPRAY_STATUS_TAG_CFG_TX_AND_RX_ACK]->cfgTxAndRxAck.
        numOfMsg = pSetOptionReq->numOfInstances;

      for (i = 0; i < pSetOptionReq->numOfInstances; i++)
      {
        xspraySetDefaultMsgCfg(
          &pProc->pStatus[XSPRAY_STATUS_TAG_CFG_TX_AND_RX_ACK]->cfgTxAndRxAck.
            msg[i],
          pSetOptionReq->instance[i].value[0]);
      }
      break;

    /* -m [<signo>:]<size> */
    case XSPRAY_OPT_MINSZ:
      /* -i should always precede this option, i.e. default min size is set. */
      if (!xspraySetOptionForMsgId(
            sender,
            pSetOptionReq,
            &pProc->pStatus[XSPRAY_STATUS_TAG_CFG_TX_AND_RX_ACK]->
            cfgTxAndRxAck))
      {
        rejected = 1;
        break;
      }
      break;

    /* -M [<signo>:]<size> */
    case XSPRAY_OPT_MAXSZ:
      /* -i should always precede this option, i.e. default max size is set. */
      if (!xspraySetOptionForMsgId(
            sender,
            pSetOptionReq,
            &pProc->pStatus[XSPRAY_STATUS_TAG_CFG_TX_AND_RX_ACK]->
            cfgTxAndRxAck))
      {
        rejected = 1;
        break;
      }
      break;

    /* -q ... */
    case XSPRAY_OPT_ABORT:
      pProc->pStatus[XSPRAY_STATUS_TAG_CFG_ABORT] =
        xsprayRealloc(
          pProc->pStatus[XSPRAY_STATUS_TAG_CFG_ABORT],
          offsetof(struct XSPRAY_CfgAbortS, condition) +
            sizeof(struct XSPRAY_AbortConditionS) *
              pSetOptionReq->numOfInstances,
          offsetof(struct XSPRAY_CfgAbortS, condition));
      pProc->pStatus[XSPRAY_STATUS_TAG_CFG_ABORT]->cfgAbort.numOfConditions =
        pSetOptionReq->numOfInstances;

      for (i = 0; i < pSetOptionReq->numOfInstances; i++)
      {
        pProc->pStatus[XSPRAY_STATUS_TAG_CFG_ABORT]->cfgAbort.condition[i].
          option = (enum XSPRAY_Option) pSetOptionReq->instance[i].value[0];
        pProc->pStatus[XSPRAY_STATUS_TAG_CFG_ABORT]->cfgAbort.condition[i].
          value = pSetOptionReq->instance[i].value[1];
      }
      break;

    /* -t <time>s | <time>ms | <time>us*/
    case XSPRAY_OPT_TMDI:
      pProc->pStatus[XSPRAY_STATUS_TAG_CFG_TX_ACK_ECHO_AND_RX] =
        xsprayRealloc(
          pProc->pStatus[XSPRAY_STATUS_TAG_CFG_TX_ACK_ECHO_AND_RX],
          offsetof(struct XSPRAY_CfgTxAckEchoAndRxS, timeSlot) +
            sizeof(U32) * pSetOptionReq->numOfInstances,
          offsetof(struct XSPRAY_CfgTxAckEchoAndRxS, timeSlot));
      pProc->pStatus[XSPRAY_STATUS_TAG_STAT_TX_ACK_ECHO_AND_RX] =
        xsprayRealloc(
          pProc->pStatus[XSPRAY_STATUS_TAG_STAT_TX_ACK_ECHO_AND_RX],
          offsetof(struct XSPRAY_StatTxAckEchoAndRxS,
                   timeSlotMsgCounter) +
            sizeof(U32) * pSetOptionReq->numOfInstances,
          offsetof(struct XSPRAY_StatTxAckEchoAndRxS,
                   timeSlotMsgCounter));

      pProc->pStatus[XSPRAY_STATUS_TAG_CFG_TX_ACK_ECHO_AND_RX]->
        cfgTxAckEchoAndRx.numOfTimeSlots =
        pProc->pStatus[XSPRAY_STATUS_TAG_STAT_TX_ACK_ECHO_AND_RX]->
          statTxAckEchoAndRx.numOfTimeSlotMsgCounters =
            pSetOptionReq->numOfInstances;

      for (i = 0; i < pSetOptionReq->numOfInstances; i++)
      {
        pProc->pStatus[XSPRAY_STATUS_TAG_CFG_TX_ACK_ECHO_AND_RX]->
          cfgTxAckEchoAndRx.timeSlot[i] = pSetOptionReq->instance[i].value[0];
      }
      break;

    /* -c <count> */
    case XSPRAY_OPT_MSGC:
      pProc->pStatus[XSPRAY_STATUS_TAG_CFG_TX]->cfgTx.msgCount =
        pSetOptionReq->instance[0].value[0];
      break;

    /* -d <delay> */
    case XSPRAY_OPT_MSGD:
      pProc->pStatus[XSPRAY_STATUS_TAG_CFG_TX]->cfgTx.msgDelay =
        pSetOptionReq->instance[0].value[0];
      break;

    /* -C <count> */
    case XSPRAY_OPT_BURSTC:
      pProc->pStatus[XSPRAY_STATUS_TAG_CFG_TX]->cfgTx.burstCount =
        pSetOptionReq->instance[0].value[0];
      break;

    /* -D <delay> */
    case XSPRAY_OPT_BURSTD:
      pProc->pStatus[XSPRAY_STATUS_TAG_CFG_TX]->cfgTx.burstDelay =
        pSetOptionReq->instance[0].value[0];
      break;

    default:
      xspraySendReject(sender, XSPRAY_SET_OPTION_REJ, XSPRAY_ERR_WRONG_PARAM,
                       "unknown option %d", pSetOptionReq->option);
      rejected = 1;
      break;
  }

  if (!rejected)
  {
    xsprayResetStat(pProc);
    sig = alloc(sizeof(struct XSPRAY_SetOptionCfmS), XSPRAY_SET_OPTION_CFM);
    TRACE_SEND_SIG(sig, sender, "XSPRAY_SET_OPTION_CFM");
    send(&sig, sender);
  }

}

/******************************************************************************
 *
 * Function:
 *      xsprayHandleStart
 *
 * Parameters:
 *      pRoot      Pointer to root of process list
 *      pStartReq  Pointer to request.
 *      sender     sender of request.
 *
 * Return value:
 *      None
 *
 * Description:
 *      Handles start request.
 *
 *****************************************************************************/
static void xsprayHandleStart(struct XSPRAY_ProcRootS *pRoot,
                              struct XSPRAY_StartReqS *pStartReq,
                              PROCESS sender)
{
  struct XSPRAY_ProcSearchTagS tag;
  struct XSPRAY_ProcS *pProc;
  union SIGNAL *sig;

  tag.type = XSPRAY_SEARCH_ENTRY;
  tag.u.hEntry = pStartReq->hEntry;

  pProc = xsprayFindProc(pRoot, &tag, NULL);

  if (pProc == NULL)
  {
    xspraySendReject(sender, XSPRAY_START_REJ, XSPRAY_ERR_WRONG_PARAM,
                     "process not found");
    return;
  }

  if (pProc->hServer != NULL)
  {
    xspraySendReject(sender, XSPRAY_START_REJ, XSPRAY_ERR_WRONG_PARAM,
                     "%s is already started",
                     pProc->pStatus[XSPRAY_STATUS_TAG_CFG_COMMON]->cfgCommon.
                       name);
    return;
  }

  /* Reset statistics. */
  xsprayResetStat(pProc);

  /* Set Mode. */
  if (pStartReq->rxName[0] != '\0')
  {
    pProc->pStatus[XSPRAY_STATUS_TAG_CFG_COMMON]->cfgCommon.role =
      XSPRAY_ROLE_TX;
  }
  else
  {
    pProc->pStatus[XSPRAY_STATUS_TAG_CFG_COMMON]->cfgCommon.role =
      XSPRAY_ROLE_RX;
  }

  /* Copy or reset name of receiver. */
  pProc->pStatus[XSPRAY_STATUS_TAG_CFG_TX] =
    xsprayRealloc(
      pProc->pStatus[XSPRAY_STATUS_TAG_CFG_TX],
      offsetof(struct XSPRAY_CfgTxS, rxName) + strlen(pStartReq->rxName) +
        sizeof('\0'),
      offsetof(struct XSPRAY_CfgTxS, rxName));

  strcpy(pProc->pStatus[XSPRAY_STATUS_TAG_CFG_TX]->cfgTx.rxName,
         pStartReq->rxName);

  /* Start xsprayServer */
  pProc->hServer = xsprayStartServer(&pProc->pStatus);

  if (pProc->hServer == NULL)
  {
    xspraySendReject(sender, XSPRAY_START_REJ, XSPRAY_ERR_OTHER,
                     "Failed to start %s",
                     &pProc->pStatus[XSPRAY_STATUS_TAG_CFG_COMMON]->cfgCommon.
                       name);
    return;
  }

  sig = alloc(sizeof(struct XSPRAY_StartCfmS), XSPRAY_START_CFM);
  TRACE_SEND_SIG(sig, sender, "XSPRAY_START_CFM");
  send(&sig, sender);
}

/******************************************************************************
 *
 * Function:
 *      xsprayHandleStop
 *
 * Parameters:
 *      pRoot     Pointer to root of process list
 *      pStopReq  Pointer to request.
 *      sender    sender of request.
 *
 * Return value:
 *      None
 *
 * Description:
 *      Handles stop request.
 *
 *****************************************************************************/
static void xsprayHandleStop(struct XSPRAY_ProcRootS *pRoot,
                             struct XSPRAY_StopReqS *pStopReq,
                             PROCESS sender)
{
  struct XSPRAY_ProcSearchTagS tag;
  struct XSPRAY_ProcS *pProc;
  union SIGNAL *sig;

  tag.type = XSPRAY_SEARCH_ENTRY;
  tag.u.hEntry = pStopReq->hEntry;

  pProc = xsprayFindProc(pRoot, &tag, NULL);

  if (pProc == NULL)
  {
    xspraySendReject(sender, XSPRAY_STOP_REJ, XSPRAY_ERR_WRONG_PARAM,
                     "process not found");
    return;
  }

  if (pProc->hServer != NULL)
  {
    if (!xsprayStopServer(&pProc->hServer))
    {
      xspraySendReject(sender, XSPRAY_REMOVE_REJ, XSPRAY_ERR_OTHER,
                       "stop failed");
      return;
    }

    pProc->pStatus[XSPRAY_STATUS_TAG_STAT_COMMON]->statCommon.state =
      XSPRAY_STATE_STOPPED;
  }

  sig = alloc(sizeof(struct XSPRAY_StopCfmS), XSPRAY_STOP_CFM);
  TRACE_SEND_SIG(sig, sender, "XSPRAY_STOP_CFM");
  send(&sig, sender);
}

/******************************************************************************
 *
 * Function:
 *      xsprayHandleGetStatus
 *
 * Parameters:
 *      pRoot          Pointer to root of process list
 *      pGetStatusReq  Pointer to request.
 *      sender         sender of request.
 *
 * Return value:
 *      None
 *
 * Description:
 *      Handles get status request.
 *
 *****************************************************************************/
static void xsprayHandleGetStatus(struct XSPRAY_ProcRootS *pRoot,
                                  struct XSPRAY_GetStatusReqS *pGetStatusReq,
                                  PROCESS sender)
{
  struct XSPRAY_ProcSearchTagS tag;
  struct XSPRAY_ProcS *pProc;
  union SIGNAL *sig;

  tag.type = XSPRAY_SEARCH_ENTRY;
  tag.u.hEntry = pGetStatusReq->hEntry;

  pProc = xsprayFindProc(pRoot, &tag, NULL);

  if (pProc == NULL)
  {
    xspraySendReject(sender, XSPRAY_GET_STATUS_REJ, XSPRAY_ERR_WRONG_PARAM,
                     "process not found");
    return;
  }

  sig  = alloc(offsetof(struct XSPRAY_GetStatusCfmS, u) +
               sigsize((union SIGNAL **) &pProc->pStatus[pGetStatusReq->tag]),
               XSPRAY_GET_STATUS_CFM);
  sig->getStatusCfm.tag = pGetStatusReq->tag;
  memcpy(&sig->getStatusCfm.u, pProc->pStatus[pGetStatusReq->tag],
         sigsize((union SIGNAL **) &pProc->pStatus[pGetStatusReq->tag]));
  if (sig->getStatusCfm.tag == XSPRAY_STATUS_TAG_STAT_TX_ACK_ECHO_AND_RX &&
      !sig->getStatusCfm.u.statTxAckEchoAndRx.echoAndAck.roundTripDelay.max)
  {
    /* Hide ULONG_MAX init value for echo.roundTripDelay.min. */
    sig->getStatusCfm.u.statTxAckEchoAndRx.echoAndAck.roundTripDelay.min = 0;
  }

  TRACE_SEND_SIG(sig, sender, STR("XSPRAY_GET_STATUS_CFM(tag:%d)",
                                  sig->getStatusCfm.tag));
  send(&sig, sender);
}

/******************************************************************************
 *
 * Function:
 *      xspraySendReject
 *
 * Parameters:
 *      addressee  Addressee
 *      sigNo      Signal number.
 *      error      Error code.
 *      format     Format string.
 *      ...        Variable number of arguments.
 *
 * Return value:
 *      None
 *
 * Description:
 *      Sends an reject.
 *
 *****************************************************************************/
static void xspraySendReject(PROCESS adressee, SIGSELECT sigNo,
                             enum XSPRAY_ErrorCode error, char *format,
                             ...)
{
  va_list arg;

  union SIGNAL *sig =  alloc(sizeof(struct XSPRAY_RejS), sigNo);

  sig->rej.errorCode = (U16) error;
  va_start(arg, format);
  vsnprintf(sig->rej.errorDescription, sizeof(sig->rej.errorDescription),
            format, arg);
  va_end(arg);

  TRACE_SEND_SIG(sig, adressee, STR("XSPRAY_XXX_REJ(e:0x%x, \"%s\")",
                                    sig->rej.errorCode,
                                    sig->rej.errorDescription));
  send(&sig, adressee);
}

/******************************************************************************
 *
 * Function:
 *      xsprayEnqueueProc
 *
 * Parameters:
 *      pRoot  Pointer to root of process list
 *      pProc  Pointer to process element to enqueue.
 *
 * Return value:
 *      None
 *
 * Description:
 *      Enqueues a process element
 *
 *****************************************************************************/
static void xsprayEnqueueProc(struct XSPRAY_ProcRootS *pRoot,
                              struct XSPRAY_ProcS *pProc)
{
  pProc->pNext = 0;

  if (pRoot->pHead == NULL)
  {
    pRoot->pHead = pProc;
  }
  else
  {
    pRoot->pTail->pNext = pProc;
  }

  pRoot->pTail = pProc;
}

/******************************************************************************
 *
 * Function:
 *      xsprayDequeueProc
 *
 * Parameters:
 *      pRoot  Pointer to root of process list
 *      pPred  Pointer predecessor of process element.
 *      pProc  Pointer to process element to enqueue.
 *
 * Return value:
 *      None
 *
 * Description:
 *      Dequeues a process element
 *
 *****************************************************************************/
static void xsprayDequeueProc(struct XSPRAY_ProcRootS *pRoot,
                              struct XSPRAY_ProcS *pPred,
                              struct XSPRAY_ProcS *pProc)
{
  if (pPred == NULL)
  {
    pRoot->pHead = pProc->pNext;
  }
  else
  {
    pPred->pNext = pProc->pNext;

    if (pPred->pNext == NULL)
    {
      pRoot->pTail = pPred;
    }
  }

  pProc->pNext = 0;
}

/******************************************************************************
 *
 * Function:
 *      xsprayFindProc
 *
 * Parameters:
 *      pRoot         Pointer to root of process list
 *      pPag          Pointer to Search tag.
 *      pPredPointer  Address where to return pointer to predecessor of process
 *                    or NULL.
 *
 * Return value:
 *      Pointer to process element or NULL if no element found,
 *
 * Description:
 *      Searches for process element in list.
 *
 *****************************************************************************/
static struct XSPRAY_ProcS *xsprayFindProc(
  const struct XSPRAY_ProcRootS *pRoot,
  const struct XSPRAY_ProcSearchTagS *pTag,
  struct XSPRAY_ProcS **pPredPtr)
{
  struct XSPRAY_ProcS *pProc = pRoot->pHead;

  if (pPredPtr != NULL)
  {
    *pPredPtr = NULL;
  }

  while (pProc != NULL)
  {
    if (pTag->type == XSPRAY_SEARCH_NAME)
    {
      if (pTag->u.pName[0] == '\0' ||
          !strcmp(pTag->u.pName,
                  pProc->pStatus[XSPRAY_STATUS_TAG_CFG_COMMON]->
                    cfgCommon.name))
      {
        break;
      }
    }

    if (pTag->type == XSPRAY_SEARCH_ENTRY && pTag->u.hEntry == pProc)
    {
      break;
    }

    if (pPredPtr != NULL)
    {
      *pPredPtr = pProc;
    }

    pProc = pProc->pNext;
  }

  return pProc;
}

/******************************************************************************
 *
 * Function:
 *      xsprayRealloc
 *
 * Parameters:
 *      pBuf         Pointer to original buffer.
 *      size         Size of new buffer to allocate.
 *      preserveSize Size to copy from original to new buffer.
 *
 * Return value:
 *      Pointer to reallocated buffer.
 *
 * Description:
 *      Reallocates a buffer.
 *
 *****************************************************************************/
static void *xsprayRealloc(void *pBuf, size_t size, size_t preserveSize)
{
  void *pNewBuf = alloc(size, 0);
  memcpy(pNewBuf, pBuf, preserveSize);
  free_buf((union SIGNAL **) &pBuf);
  return pNewBuf;
}

/******************************************************************************
 *
 * Function:
 *      xspraySetDefaultMsgCfg
 *
 * Parameters:
 *      pMsgCfg  Pointer to message config.
 *      id       Message id.
 *
 * Return value:
 *      None
 *
 * Description:
 *      Sets default values except id for message config.
 *
 *****************************************************************************/
void xspraySetDefaultMsgCfg(struct XSPRAY_MsgCfgS *pMsgCfg, SIGSELECT id)
{
  pMsgCfg->id = id;
  pMsgCfg->size.min = XSPRAY_DEFAULT_SIZE;
  pMsgCfg->size.max = XSPRAY_DEFAULT_SIZE;
  pMsgCfg->echoAndVerify.mask = XSPRAY_DEFAULT_ECHO_VERIFY_MASK;
}

/******************************************************************************
 *
 * Function:
 *      xspraySetOptionForMsgId
 *
 * Parameters:
 *      sender          Sender of set option request.
 *      pSetOptionReq   Pointer to set option request.
 *      pCfgTxAndRxAck  Pointer to Tx and rx with acknowledge configuration.
 *
 * Return value:
 *      Non-zero if option values were valid otherwise zero.
 *
 * Description:
 *      Set option values for per message id configuration.
 *      XSPRAY_SET_OPTION_REJ is sent if option values are invalid,
 *
 *****************************************************************************/
static int xspraySetOptionForMsgId(
  PROCESS sender,
  struct XSPRAY_SetOptionReqS *pSetOptionReq,
  struct XSPRAY_CfgTxAndRxAckS *pCfgTxAndRxAck)
{
  U32 i;

  struct XSPRAY_OptionInstanceS *pInvalidInstance =
    xsprayInvOptInstForMsgId(
      pSetOptionReq,
      pCfgTxAndRxAck);

  if (pInvalidInstance != NULL)
  {
    xspraySendReject(sender, XSPRAY_SET_OPTION_REJ,
                     XSPRAY_ERR_WRONG_PARAM,
                     "invalid %d%s argument, <signo> need to be set via %s",
                     pInvalidInstance->argIndex,
                     xsprayGetArgNumSuffix(pInvalidInstance->argIndex),
                     xsprayOptArg[XSPRAY_OPT_ID]);
    return 0;
  }

  for (i = 0; i < pCfgTxAndRxAck->numOfMsg;  i++)
  {
     U32 value = 0; /* Initialized to avoid compiler warning. */
     U32 orderOfId;
     U32 j;

     for (j = 0, orderOfId = 1; j < i;  j++)
     {
       if (pCfgTxAndRxAck->msg[j].id == pCfgTxAndRxAck->msg[i].id) /*lint !e661 length of msg == numOfMsg */
       {
         orderOfId++;
       }
     }

     if (xsprayGetOptValueForMsgId(pSetOptionReq, pCfgTxAndRxAck->msg[i].id,
                                   orderOfId, &value)) /*lint !e661 */
     {
       if (pSetOptionReq->option == XSPRAY_OPT_ECHO ||
           pSetOptionReq->option == XSPRAY_OPT_VERIFY)
       {
         pCfgTxAndRxAck->msg[i].echoAndVerify.mask = value; /*lint !e661 */
       }
       else if (pSetOptionReq->option == XSPRAY_OPT_MINSZ)
       {
         pCfgTxAndRxAck->msg[i].size.min = MAX(value, XSPRAY_MIN_SIZE); /*lint !e661 */
         pCfgTxAndRxAck->msg[i].size.max =
           MAX(pCfgTxAndRxAck->msg[i].size.min,
               pCfgTxAndRxAck->msg[i].size.max); /*lint !e661 */
       }
       else if (pSetOptionReq->option == XSPRAY_OPT_MAXSZ)
       {
         pCfgTxAndRxAck->msg[i].size.max = MAX(value, XSPRAY_MIN_SIZE); /*lint !e661 */
         pCfgTxAndRxAck->msg[i].size.min =
           MIN(pCfgTxAndRxAck->msg[i].size.min,
               pCfgTxAndRxAck->msg[i].size.max); /*lint !e661 */
       }
     }
  }
  return 1;
}

/******************************************************************************
 *
 * Function:
 *      xsprayInvOptInstForMsgId
 *
 * Parameters:
 *      pSetOptionReq   Pointer to set option request.
 *      pCfgTxAndRxAck  Pointer to Tx and rx with acknowledge configuration.
 *
 * Return value:
 *      Pointer to invalid option intance in or NULL.
 *
 * Description:
 *      Checks if option values are valid for per message id configuration.
 *
 *****************************************************************************/
static struct XSPRAY_OptionInstanceS *xsprayInvOptInstForMsgId(
  struct XSPRAY_SetOptionReqS *pSetOptionReq,
  struct XSPRAY_CfgTxAndRxAckS *pCfgTxAndRxAck)
{
  U32 i;

  for (i = 0; i < pSetOptionReq->numOfInstances; i++)
  {
    U32 j;
    int found = 0;

    if (pSetOptionReq->instance[i].numOfValues <= 1)
    {
      continue;
    }

    if (pSetOptionReq->instance[i].numOfValues != 2)
    {
      return &pSetOptionReq->instance[i];
    }

    for (j = 0; j < pCfgTxAndRxAck->numOfMsg; j++)
    {
      if (pCfgTxAndRxAck->msg[j].id ==
          pSetOptionReq->instance[i].value[0])
      {
        found = 1;
        break;
      }
    }

    if (!found)
    {
      return &pSetOptionReq->instance[i];
    }
  }

  return NULL;
}

/******************************************************************************
 *
 * Function:
 *      xsprayGetOptValueForMsgId
 *
 * Parameters:
 *      pSetOptionReq   Pointer to set option request.
 *      id              Message id.
 *      orderOfId       The order of message id among messages with same id.
 *      pValue          Pointer to returned value.
 *
 * Return value:
 *      Non-zero if value found otherwise zero.
 *
 * Description:
 *      Return if option value for per message id configuration.
 *
 *****************************************************************************/
static int xsprayGetOptValueForMsgId(
  struct XSPRAY_SetOptionReqS *pSetOptionReq, U32 id, U32 orderOfId,
  U32 *pValue)
{
  U32 i;
  enum {XSPRAY_NOT_FOUND, XSPRAY_DEFAULT_FOUND, XSPRAY_ID_FOUND} found =
    XSPRAY_NOT_FOUND;

  for (i = 0; i < pSetOptionReq->numOfInstances; i++)
  {
    if (!pSetOptionReq->instance[i].numOfValues)
    {
      continue;
    }
    else if (pSetOptionReq->instance[i].numOfValues == 1 &&
             found == XSPRAY_NOT_FOUND )
    {
      *pValue = pSetOptionReq->instance[i].value[0];
      found = XSPRAY_DEFAULT_FOUND;
    }
    else if( pSetOptionReq->instance[i].value[0] == id)
    {
      if (orderOfId == 1 || found != XSPRAY_ID_FOUND)
      {
        *pValue = pSetOptionReq->instance[i].value[1];
      }
      orderOfId--;
      found = XSPRAY_ID_FOUND;
    }
  }
  return (found != XSPRAY_NOT_FOUND);
}

/******************************************************************************
 *
 * Function:
 *      xsprayResetStat
 *
 * Parameters:
 *      pProc  Pointer to process element to enqueue.
 *
 * Return value:
 *      None
 *
 * Description:
 *      Resets statistics.
 *
 *****************************************************************************/
static void xsprayResetStat(struct XSPRAY_ProcS *pProc)
{
  U32 i;

  pProc->pStatus[XSPRAY_STATUS_TAG_CFG_COMMON]->cfgCommon.role =
    XSPRAY_ROLE_IDLE;
  pProc->pStatus[XSPRAY_STATUS_TAG_STAT_COMMON]->statCommon.state =
    XSPRAY_STATE_IDLE;
  pProc->pStatus[XSPRAY_STATUS_TAG_STAT_COMMON]->statCommon.totalTime = 0;
  pProc->pStatus[XSPRAY_STATUS_TAG_STAT_COMMON]->statCommon.infoText[0] = '\0';
  memset(pProc->pStatus[XSPRAY_STATUS_TAG_STAT_TX], 0,
         sigsize((union SIGNAL **) &pProc->
                 pStatus[XSPRAY_STATUS_TAG_STAT_TX]));
  for (i = 0;
       i < pProc->pStatus[XSPRAY_STATUS_TAG_CFG_TX_AND_RX_ACK]->cfgTxAndRxAck.
         numOfMsg;
       i++)
  {
    pProc->pStatus[XSPRAY_STATUS_TAG_CFG_TX_AND_RX_ACK]->cfgTxAndRxAck.
      msg[i].adjustedSize =
        pProc->pStatus[XSPRAY_STATUS_TAG_CFG_TX_AND_RX_ACK]->cfgTxAndRxAck.
          msg[i].size;
  }

  memset(pProc->pStatus[XSPRAY_STATUS_TAG_STAT_TX_ACK_ECHO_AND_RX], 0,
         sigsize((union SIGNAL **) &pProc->
                 pStatus[XSPRAY_STATUS_TAG_STAT_TX_ACK_ECHO_AND_RX]));
  pProc->pStatus[XSPRAY_STATUS_TAG_STAT_TX_ACK_ECHO_AND_RX]->
    statTxAckEchoAndRx.echoAndAck.roundTripDelay.min = ULONG_MAX;
}
