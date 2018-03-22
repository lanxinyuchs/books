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
 *      xspray_server.h
 *
 * Author:
 *      Arturo Salinas (esalart)
 *
 * Description:
 *      This header file exposes the interface of the xsprayServer process.
 *
 * Revision history:
 *      2011-12-18, Arturo Salinas (esalart)
 *                 Initial implementation according to CR: BSDB0006283
 *      2012-05-08 ewenyao  change for echo
 *      2012-08-23 Göran Nordin (egornor)
 *                 HQ34850, XSPRAY often hangs shell and can,t verify high prio
 *                 message echoed to RU.
 *                 Major refactoring to fix blocking issues.
 *      2014-04-02 Göran Nordin (egornor)
 *                 MR 105 65-0163/00699, "RBS 6000 with Radio Dot System".
 *                 Added statistics.
 *      2015-06-25 Göran Nordin (egornor)
 *                 Changed to only use funtions that are supported in lits
 *                 emulation.
 */

#ifndef _XSPRAY_SERVER_H_
#define _XSPRAY_SERVER_H_

/*----------------------------  Include files  ------------------------------*/

#include <ose.h>
#include <osetypes.h>

/*----------------------------  CONSTANTS  ----------------------------------*/

/* Configuration flag defining round trip delay measurements in echo mode. */
#define  XSPRAY_MODE_FLAG_ECHO_RTD 0x00000001
/* Configuration flag defining verify mode. */
#define  XSPRAY_MODE_FLAG_VERIFY   0x00000002

#define XSPRAY_COUNT_INFINITE    (U32) -1
#define XSPRAY_ABORT_TMO_DISABLED (U32) -1

/*----------------------------  MACROS  -------------------------------------*/

/*----------------------------  Structs and typedefs  -----------------------*/

/*
 * Tags defining status contents.
 */
enum XSPRAY_StatusTag
{
  /* Common configuration, type is XSPRAY_CfgCommonS. */
  XSPRAY_STATUS_TAG_CFG_COMMON,
  /* Abort configuration, type is XSPRAY_CfgAbortS */
  XSPRAY_STATUS_TAG_CFG_ABORT,
  /* Tx configuration, type is XSPRAY_CfgTxS. */
  XSPRAY_STATUS_TAG_CFG_TX,
  /*Tx and rx with ack configuration, type is XSPRAY_CfgTxAndRxAckS. */
  XSPRAY_STATUS_TAG_CFG_TX_AND_RX_ACK,
  /* Rx configuration, type is XSPRAY_CfgTxAckEchoAndRxS. */
  XSPRAY_STATUS_TAG_CFG_TX_ACK_ECHO_AND_RX,
  /* Common statistics, type is XSPRAY_StatCommonS. */
  XSPRAY_STATUS_TAG_STAT_COMMON,
  /* Tx statistics, type is XSPRAY_StatTxS. */
  XSPRAY_STATUS_TAG_STAT_TX,
  /* Rx statistics, type is XSPRAY_StatTxAckEchoAndRxS. */
  XSPRAY_STATUS_TAG_STAT_TX_ACK_ECHO_AND_RX,
  /* Total number of tags. */
  XSPRAY_NUM_OF_STATUS_TAGS
};

/*
 * The type defines the communication towards server which is performad via
 * memory. The client (monitor) allocates the communication area and sets an
 * environment pointer on the block of the server to publish the area.
 */
typedef union XSPRAY_Status
  *XSPRAY_serverStatusArray[XSPRAY_NUM_OF_STATUS_TAGS];

/*
 * Command line options.
 */
enum XSPRAY_Option
{
   /* No option found */
  XSPRAY_OPT_NONE = -1,
  /* -p <prio> */
  XSPRAY_OPT_PRIO,
  /* -a [<count>] */
  XSPRAY_OPT_ACK,
  /* -e [[<signo>:]<mask>] */
  XSPRAY_OPT_ECHO,
  /* -E */
  XSPRAY_OPT_ECHO_RTD,
  /* -v */
  XSPRAY_OPT_VERIFY,
  /* -i <signo> */
  XSPRAY_OPT_ID,
  /* -m [<signo>:]<size> */
  XSPRAY_OPT_MINSZ,
  /* -M [<signo>:]<size> */
  XSPRAY_OPT_MAXSZ,
  /* -q t:<timeout>|e:<count>|v:<count> */
  XSPRAY_OPT_ABORT,
  /* -t <time>s | <time>ms | <time>us*/
  XSPRAY_OPT_TMDI,
  /* -c <count> */
  XSPRAY_OPT_MSGC,
  /* -d <delay> */
  XSPRAY_OPT_MSGD,
  /* -C <count> */
  XSPRAY_OPT_BURSTC,
  /* -D <delay> */
  XSPRAY_OPT_BURSTD,
  /* Total number of options. */
  XSPRAY_NUM_OF_OPTS
};

/*
 * Role of a xspray process
 */
enum XSPRAY_Role
{
  XSPRAY_ROLE_IDLE,
  XSPRAY_ROLE_RX,
  XSPRAY_ROLE_TX,
  /* Total number of roles. */
  XSPRAY_NUM_OF_ROLES
};

/*
 * State of a xspray process
 */
enum XSPRAY_State
{
  XSPRAY_STATE_IDLE,
  XSPRAY_STATE_BUSY,
  XSPRAY_STATE_STOPPED,
  XSPRAY_STATE_DONE,
  XSPRAY_STATE_ERROR,
  /* Total number of states. */
  XSPRAY_NUM_OF_STATES
};

/*
 * Mode of a xspray process
 */
enum XSPRAY_Mode
{
  XSPRAY_MODE_NORMAL = 0,
  XSPRAY_MODE_ACK,
  XSPRAY_MODE_ECHO,
  /* Total number of modes. */
  XSPRAY_NUM_OF_MODES
};

/*
 * Limits.
 */
struct XSPRAY_MinMaxS {
  U32 min;
  U32 max;
};

/*
 * Message config.
 */
struct XSPRAY_MsgCfgS {
  SIGSELECT id;
  struct XSPRAY_MinMaxS size;
  /*
   * Message sizes that may be bigger than configured when mode is
   * XSPRAY_MODE_ECHO and flag is XSPRAY_MODE_FLAG_ECHO_RTD.
   */
  struct XSPRAY_MinMaxS adjustedSize;
  struct {
    /*
     * Mask that defines which bits in message is to validate. Only valid for
     * Tx when mode is XSPRAY_MODE_ECHO and Rx when modeFlags is
     * XSPRAY_MODE_FLAG_VERIFY.
     */
    U32 mask;
  } echoAndVerify;
};

/* Common configuration */
struct XSPRAY_CfgCommonS {
  /* Process priority. */
  OSPRIORITY priority;
  /* Configured role. */
  enum XSPRAY_Role role;
  /* Cofigured Mode */
  enum XSPRAY_Mode mode;
  /* Additional mode configuration */
  U32 modeFlags;
  /* Process name. */
  char name[1];
};

/* Abort conditions. */
struct XSPRAY_AbortConditionS {
  enum XSPRAY_Option option;
  U32 value;
};

/* Abort configuration. */
struct XSPRAY_CfgAbortS {
  /* The number of abort conditions */
  U32 numOfConditions;
  /* Abort conditions. */
  struct XSPRAY_AbortConditionS condition[1];
};

/* Tx configuration. */
struct XSPRAY_CfgTxS {
    /* the number of messages within a burst. -1 define infinite. */
    U32 msgCount;
    /* Delay in ms between messages. */
    U32 msgDelay;
    /* the number of bursts. -1 define infinite. */
    U32 burstCount;
    /* Delay in ms between burts. */
    U32 burstDelay;
    /* Receiving process name. */
    char rxName[1];
};

/* Tx and rx with acknowledge configuration. */
struct XSPRAY_CfgTxAndRxAckS {
  /*
   * Defines how many messages to send before waiting for acknowedge or how
   * many messages to receive before sending acknowledge.
   */
  U32 ackCount;
  /* The number of message configurations. */
  U32 numOfMsg;
  /* Message configurations. */
  struct XSPRAY_MsgCfgS msg[1];
};

/* Rx configuration. */
struct XSPRAY_CfgTxAckEchoAndRxS {
  /* The number of time distribution slots. */
  U32 numOfTimeSlots;
  /* Time distribution slots. */
  U32 timeSlot[1];
};

/* Common statistics. */
struct XSPRAY_StatCommonS {
    /* State */
    enum XSPRAY_State state;
    /* Time to complete operation  */
    U32 totalTime;
    /* Info string  */
    char infoText[256];
};

/* Tx statistics. */
struct XSPRAY_StatTxS {
  /* The number of sent messages. */
  U32 msgs;
  /* The number of sent bytes. */
  U32 bytes;
};

/* Rx statistics */
struct XSPRAY_StatTxAckEchoAndRxS {
  /* The number of received messages. */
  U32 msgs;
  /* The number of received  bytes. */
  U32 bytes;
  struct {
    /*
     * The number of invalid messages. Only valid for Tx when mode is
     * XSPRAY_MODE_ECHO and Rx when modeFlags is XSPRAY_MODE_FLAG_VERIFY.
     */
    U32 errors;
  } echoAndVerify;
  struct {
    /*
     * The minimum and maximum round trip delay in us.
     * Only valid for Tx when mode is XSPRAY_MODE_ACK or mode is
     * XSPRAY_MODE_ECHO and Flags is XSPRAY_MODE_FLAG_ECHO_RTD.
     */
    struct XSPRAY_MinMaxS roundTripDelay;
  } echoAndAck;
  /* Last received message. */
  struct {
    SIGSELECT id;
    U32 size;
  } lastMsg;
  /* The number of time slots counters. */
  U32 numOfTimeSlotMsgCounters;
  /* Message counters for the slots */
  U32 timeSlotMsgCounter[1];
};

union XSPRAY_Status {
  struct XSPRAY_CfgCommonS cfgCommon;
  struct XSPRAY_CfgAbortS cfgAbort;
  struct XSPRAY_CfgTxS cfgTx;
  struct XSPRAY_CfgTxAndRxAckS cfgTxAndRxAck;
  struct XSPRAY_CfgTxAckEchoAndRxS cfgTxAckEchoAndRx;
  struct XSPRAY_StatCommonS statCommon;
  struct XSPRAY_StatTxS statTx;
  struct XSPRAY_StatTxAckEchoAndRxS statTxAckEchoAndRx;
};

/*
 * Signal that tx server use to get killed when connection to receiver is lost.
 */
struct XSPRAY_AttachKillReqS {
  SIGSELECT sigNo;
  PROCESS pidToAttachToOrKill;
  PROCESS killSrvPid;
};

/*----------------------------  Declaration of Global Variables  ------------*/

/*----------------------------  Declaration of Global Functions  ------------*/

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
void *xsprayStartServer(XSPRAY_serverStatusArray *pStatusArray);

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
int xsprayStopServer(void **hServer);

#endif  /* _XSPRAY_SERVER_H_ */
