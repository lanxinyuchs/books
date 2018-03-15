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
 *     XSPRAY,  CAH 109 2191
 *
 * File:
 *      xspray.c
 *
 * Author:
 *      Arturo Salinas (esalart)
 *
 * Description:
 *      This file implements the xspray command.
 *
 * Revision history:
 *      2011-12-18 Arturo Salinas (esalart)
 *                 Initial implementation according to CR: BSDB0006283
 *      2012-04-18 Arturo Salinas (esalart)
 *                 Changes for BP
 *      2012-05-09 ewenyao  change for echo
 *      2012-08-23 Göran Nordin (egornor)
 *                 HQ34850, XSPRAY often hangs shell and can,t verify high prio
 *                 message echoed to RU.
 *                 Major refactoring to fix blocking issues.
 *      2013-08-13 Peter Ulfheden (epeulfh)
 *                 Minor fixes for Lint.
 *      2014-04-02 Göran Nordin (egornor)
 *                 MR 105 65-0163/00699, "RBS 6000 with Radio Dot System".
 *                 Added statistics.
 *
 *      2014-10-01 Peter Ulfheden (epeulfh)
 *              HS93909 Fix for OSE5.7 and GCC -wFormat warnings. Added type
 *              casting to print/sprintf/scanf functions for %l arguments.
 *
 *      2014-12-01 Peter Ulfheden (epeulfh)
 *              HS93909 Fix warnings related to OSE5.7
 *
 *      2015-04-27 Peter Ulfheden (epeulfh)
 *              Lint corrections for Lint warning level 2. Step 1.
 *
 *      2015-05-11 Vivian XIONG (edanxio)
 *              Lint corrections for Lint warning level 2. Step 2.
 *
 *      2015-07-06 Freda Ruan (efanrua)
 *              Fix coverity warnings.
 *
 *
 */

/*----------------------------  Include files  ------------------------------*/

#include <limits.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include "shell.h"
#include <ose.h>
#include <osetypes.h>
#include "xspray.h"
#include "xspray_monitor.h"

/*----------------------------  CONSTANTS  ----------------------------------*/

#define XSPRAY_MONITOR_NAME       "xsprayMonitor"

#define XSPRAY_NAME_COLUMN_WIDTH       20
#define XSPRAY_STATE_COLUMN_WIDTH      10
#define XSPRAY_PRIO_COLUMN_WIDTH        4
#define XSPRAY_MODE_COLUMN_WIDTH        9
#define XSPRAY_ULONG_COLUMN_WIDTH      10
#define XSPRAY_MASK_COUNT_COLUMN_WIDTH 11
#define XSPRAY_ABORT_COLUMN_WIDTH                       \
  (sizeof("x:") - 1 +  XSPRAY_ULONG_COLUMN_WIDTH)
#define XSPRAY_COLUMN_MARGIN_WIDTH 3
#define XSPRAY_LINE_CONT_WIDTH (sizeof(" \\")-1)
#define XSPRAY_ROW_LENGTH                                               \
  (XSPRAY_NAME_COLUMN_WIDTH + XSPRAY_COLUMN_MARGIN_WIDTH +              \
   XSPRAY_STATE_COLUMN_WIDTH + XSPRAY_COLUMN_MARGIN_WIDTH +             \
   XSPRAY_MODE_COLUMN_WIDTH + XSPRAY_COLUMN_MARGIN_WIDTH +              \
   XSPRAY_PRIO_COLUMN_WIDTH + XSPRAY_COLUMN_MARGIN_WIDTH +              \
   XSPRAY_ULONG_COLUMN_WIDTH + XSPRAY_COLUMN_MARGIN_WIDTH +             \
   XSPRAY_ULONG_COLUMN_WIDTH + XSPRAY_COLUMN_MARGIN_WIDTH +             \
   XSPRAY_ULONG_COLUMN_WIDTH + XSPRAY_COLUMN_MARGIN_WIDTH +             \
   XSPRAY_MASK_COUNT_COLUMN_WIDTH + XSPRAY_COLUMN_MARGIN_WIDTH +        \
   XSPRAY_ABORT_COLUMN_WIDTH)

#define XSPRAY_EXTRA_COLUMN_LENGTH                              \
  (XSPRAY_ROW_LENGTH -                                          \
   (XSPRAY_NAME_COLUMN_WIDTH + XSPRAY_COLUMN_MARGIN_WIDTH))

#define XSPRAY_CMD_NAME            "xspray"

#define XSPRAY_CMD_SHORT_DESCR                                          \
  "Message transmission, check of received messages, display of statistics"

/*----------------------------  MACROS  -------------------------------------*/

/*----------------------------  Structs and typedefs  -----------------------*/

/*
 * The union below defines signals received and sent by xspray
 * shell command process.
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


/*
 * Commands
 */
enum XSPRAY_Command
{
   /* All available commands */
  XSPRAY_CMD_ALL=-1,
  /* xspray -h*/
  XSPRAY_CMD_HELP,
  /* xspray [options] -s <process> */
  XSPRAY_CMD_SETUP,
  /* xspray -r  <process> */
  XSPRAY_CMD_REMOVE,
  /* xspray -br <process> */
  XSPRAY_CMD_START_RX,
  /* xspray -bt <process> <receiver> */
  XSPRAY_CMD_START_TX,
  /* xspray -k  <process> */
  XSPRAY_CMD_STOP,
  /*xspray -l [<process>] */
  XSPRAY_CMD_LIST,
  /* Total number commands. */
  XSPRAY_NUM_OF_CMDS
};

/*----------------------------  Declaration of External Functions  ----------*/

/*----------------------------  Declaration of Local Functions  -------------*/

static int xsprayCmd(int argc, char **argv);

static int xsprayCmdHelp(int argc, char **argv, int argIndex);

static int xsprayCmdSetup(int argc, char **argv, int argIndex);

static int xsprayCmdRemove(int argc, char **argv, int argIndex);

static int xsprayCmdStartRx(int argc, char **argv, int argIndex);

static int xsprayCmdStartTx(int argc, char **argv, int argIndex);

static int xsprayCmdStop(int argc, char **argv, int argIndex);

static int xsprayCmdList(int argc, char **argv, int argIndex);

static void xsprayPrintHelp(void);

static void xsprayPrintSynopsis(enum XSPRAY_Command cmd);

static void xsprayPrintInvalidArg(enum XSPRAY_Command cmd, char *prefix,
                                  char *arg, int argIndex);

static int xspraySetOption(PROCESS pid, void *hEntry,
                           enum XSPRAY_Option option, int argc, char **argv,
                           OSATTREF *pAttRef);

static enum XSPRAY_Option xsprayGetOption(char *arg);

static int xsprayGetNumOfOptionArgs(enum XSPRAY_Command cmd,
                                    enum XSPRAY_Option option, int argc,
                                    char **argv, int argIndex);

static int xsprayGetOptionValue(enum XSPRAY_Option option, char *arg,
                                int argIndex,
                                struct XSPRAY_OptionInstanceS *pInstance);

static int xsprayCompareOptionValue(const void *a, const void *b);

static int stringToUl(const char *pString, const char *pStop,
                      unsigned long *pValue);

static PROCESS xsprayGetMonitor(void);

static void *xsprayGetProcess(PROCESS pid, const char *name,
                              OSATTREF *pAttRef);

static int xsprayRemove(const char *name);

static int xsprayStart(const char *name, const char *rxName);

static int xsprayList(const char *name);

static union SIGNAL *xsprayRec(SIGSELECT sigNoCfm, SIGSELECT sigNoRej,
                               OSATTREF *pAttRef);

static int xsprayPrintStatus(PROCESS pid, void *hEntry, OSATTREF *pAttRef);

static union SIGNAL *xsprayReqStatus(PROCESS pid, void *hEntry,
                                     enum XSPRAY_StatusTag tag,
                                     OSATTREF *pAttRef);

static void xsprayPrintHdr(enum XSPRAY_Role role, enum XSPRAY_Mode mode,
                           U32 modeFlags, U32 numOfAbortconditions);

static void xsprayPrintCommon(struct XSPRAY_CfgCommonS *pCfgCommon,
                              struct XSPRAY_CfgAbortS * pCfgAbort,
                              struct XSPRAY_CfgTxS *pCfgTx,
                              struct XSPRAY_CfgTxAndRxAckS *pCfgTxAndRxAck,
                              struct XSPRAY_StatCommonS *pStatCommon);

static U32 xsprayPrintName(int leftAlign, const char *pName, const char *pEnd);

static void xsprayPrintState(enum XSPRAY_Role role, enum XSPRAY_State state);

static void xsprayPrintInfo(const char *info);

static void xsprayPrintTxCfgStat(struct XSPRAY_CfgCommonS *pCfgCommon,
                                 struct XSPRAY_CfgTxS *pCfgTx,
                                 struct XSPRAY_StatCommonS *pStatCommon,
                                 struct XSPRAY_StatTxS *pStatTx);

static void xsprayPrintTxAckEchoAndRxCfgStat(
  struct XSPRAY_CfgCommonS *pCfgCommon,
  struct XSPRAY_CfgTxAckEchoAndRxS *pCfgTxAckEchoAndRx,
  struct XSPRAY_StatCommonS *pStatCommon,
  struct XSPRAY_StatTxAckEchoAndRxS *pStatTxAckEchoAndRx);

/*----------------------------  Definition of Global Variables  -------------*/

/* Receive mask to select all signals. */
const SIGSELECT xsprayAllSig[] = {0};

/* Command line options. */
const char *xsprayOptArg[XSPRAY_NUM_OF_OPTS] =
{
  /* XSPRAY_OPT_PRIO */
  "-p",
  /* XSPRAY_OPT_ACK */
  "-a",
  /* XSPRAY_OPT_ECHO */
  "-e",
  /* XSPRAY_OPT_ECHO_RTD */
  "-E",
  /* XSPRAY_OPT_VERIFY */
  "-v",
  /* XSPRAY_OPT_ID */
  "-i",
  /* XSPRAY_OPT_MINSZ */
  "-m",
  /* XSPRAY_OPT_MAXSZ */
  "-M",
  /* XSPRAY_OPT_ABORT */
  "-q",
  /* XSPRAY_OPT_TMDI */
  "-t",
  /* XSPRAY_OPT_MSGC */
  "-c",
  /* XSPRAY_OPT_MSGD */
  "-d",
  /* XSPRAY_OPT_BURSTC */
  "-C",
  /* XSPRAY_OPT_BURSTD */
  "-D"
};

/*----------------------------  Definition of Local Variables  --------------*/

/* Used to printout suffix of argument number */
static const char *xsprayArgNumSuffix[] = {"st", "nd", "rd", "th"};

static int (*xsprayCmdFunc[XSPRAY_NUM_OF_CMDS])
  (int argc, char **argv, int argIndex)  =
{
  /* XSPRAY_CMD_HELP */
  xsprayCmdHelp,
  /* XSPRAY_CMD_SETUP */
  xsprayCmdSetup,
  /* XSPRAY_CMD_REMOVE */
  xsprayCmdRemove,
  /* XSPRAY_CMD_START_RX */
  xsprayCmdStartRx,
  /* XSPRAY_CMD_START_TX */
  xsprayCmdStartTx,
  /* XSPRAY_CMD_STOP */
  xsprayCmdStop,
  /* XSPRAY_CMD_LIST */
  xsprayCmdList
};

static const char *xsprayCmdArg[XSPRAY_NUM_OF_CMDS] =
{
  /* XSPRAY_CMD_HELP */
  "-h",
  /* XSPRAY_CMD_SETUP */
  "-s",
  /* XSPRAY_CMD_REMOVE */
  "-r",
  /* XSPRAY_CMD_START_RX */
  "-br",
  /* XSPRAY_CMD_START_TX */
  "-bt",
  /* XSPRAY_CMD_STOP */
  "-k",
  /* XSPRAY_CMD_LIST */
  "-l"
};
static const char *xsprayCmdSyntax[XSPRAY_NUM_OF_CMDS] =
{
  /* XSPRAY_CMD_HELP */
  "    xspray -h",
  /* XSPRAY_CMD_SETUP */
  "    xspray [-p <prio>] [-a [<count>]] [-e [[<signo>:]<mask>]]... [-E]\n"
  "        [-v [[<signo>:]<mask>]]... [-i <signo>]... [-m [<signo>:]<size>]...\n"
  "        [-M [<signo>:]<size>]...\n"
  "        [-q t:<timeout>|a:<timeout>|e:<count>|E:<timeout>|v:<count>]\n"
  "        [-t <time>s|<time>ms|<time>us]... [-c <count>] [-d <delay>]\n"
  "        [-C <count>] [-D <delay>] -s <process>",
  /* XSPRAY_CMD_REMOVE */
  "    xspray -r <process>",
  /* XSPRAY_CMD_START_RX */
  "    xspray -br <process>",
  /* XSPRAY_CMD_START_TX */
  "    xspray -bt <process> <receiver>",
  /* XSPRAY_CMD_STOP */
  "    xspray -k <process>",
  /* XSPRAY_CMD_LIST */
  "    xspray -l [<process>]"
};

static const char *xsprayCmdDescr[XSPRAY_NUM_OF_CMDS] =
{
  /* XSPRAY_CMD_HELP */
  "    xspray -h                          Prints help text.",
  /* XSPRAY_CMD_SETUP */
  "    xspray [options] -s <process>      Configures a xspray process.\n"
  "                                       If process exist then it must be stopped \n"
  "                                       before configuration can be changed.\n"
  "                                       This form accepts a number of options.\n"
  "                                       <process> is the name of the process.",
  /* XSPRAY_CMD_REMOVE */
  "    xspray -r <process>                Removes a xspray process.\n"
  "                                       <process> is the name of the process.",
  /* XSPRAY_CMD_START_RX */
  "    xspray -br <process>               Starts reception.\n"
  "                                       <process> is the name of the process.",
  /* XSPRAY_CMD_START_TX */
  "    xspray -bt <process> <receiver>    Starts transmission.\n"
  "                                       <process> is the name of the process.\n"
  "                                       <receiver> is the name of the receiver.",
  /* XSPRAY_CMD_STOP */
  "    xspray -k <process>                Stops reception or transmission.\n"
  "                                       <process> is the name of the process.",
  /* XSPRAY_CMD_LIST */
  "    xspray -l [<process>]              Lists configuration and statistics.\n"
  "                                       <process> is the name of the process.\n"
  "                                       All processes are listed if <process>\n"
  "                                       argument is omitted."
};

static const char *xsprayOptDescr[XSPRAY_NUM_OF_OPTS] =
{
  /* XSPRAY_OPT_PRIO */
  "    [-p <prio>]\n"
  "    Sets the process priority to execute transmission or reception on.\n"
  "    Default value for <prio> is 31.",
  /* XSPRAY_OPT_ACK */
  "    [-a [<count>]]\n"
  "    Triggers a receiver to acknowledge messages and a transmitter to wait\n"
  "    for acknowledgements. When this option is used then -i, -m and -M options\n"
  "    also apply for the receiver to define acknowledgement signal.\n"
  "    If <count> is set to 1 then every message is acknowledged.\n"
  "    If <count> is set to 2 then every 2nd message is acknowledged and so on.\n"
  "    Default value for <count> is 1.\n"
  "    Note that options -e and -a are mutually exclusive.",
  /* XSPRAY_OPT_ECHO */
  "    [-e [[<signo>:]<mask>]]\n"
  "    Triggers the receiver to echo the message and the transmitter to check\n"
  "    echoed message. The <mask> parameter defines which bits in the echoed\n"
  "    signal number that the transmitter should check. The <signo> parameter\n"
  "    makes it possible to define individual masks for different signal numbers.\n"
  "    Default value for <mask> is 0xffffffff, i.e. all bits will be checked.\n"
  "    E.g. \"-e 1:0xffffffff -e 0x80000003:0x7fffffff -e 0xfffffffe\" makes all\n"
  "    bits to be validated for signal number 1, all bits except bit31 to be\n"
  "    validated for signal number 0x80000003 and all bits except bit0 to be\n"
  "    validated for all other signal numbers.\n"
  "    Note that options -e and -a are mutually exclusive.",
  /* XSPRAY_OPT_ECHO_RTD */
  "    [-E]\n"
  "    Enables round trip delay measurements. This option is valid for message\n"
  "    transmission when -e is used. The message sizes are increased from\n"
  "    configured values to be able to hold the timestamp.",
  /* XSPRAY_OPT_VERIFY */
  "    [-v [[<signo>:]<mask>]]\n"
  "    Triggers transmitter to initialize message data with a known pattern and\n"
  "    the receiver to check received messages against that pattern.\n"
  "    The configuration via -i, -m and -M options must be equal in the \n"
  "    transmitter and receiver. Configuration values are the same as for -e.\n"
  "    Note that this option is redundant for a transmitter with -e configured.",
  /* XSPRAY_OPT_ID */
  "    [-i <signo>]\n"
  "    Sets signal numbers for messages to transmit. This option is valid for\n"
  "    message transmission and message reception when -a is used.\n"
  "    Default value for <signo> is 1",
  /* XSPRAY_OPT_MINSZ */
  "    [-m [<signo>:]<size>]\n"
  "    Sets minimum size for messages to transmit. This option is valid for\n"
  "    message transmission and message reception when -a is used. If minimum and\n"
  "    maximum size differs then the size is incremented with 1 byte for every\n"
  "    transmission starting at minimum size until it reaches maximum size. When\n"
  "    it reaches maximum size it wraps around to use it uses minimum again.\n"
  "    The <size> parameter defines minimum size for message to transmit.\n"
  "    The <signo> parameter makes it possible to define individual sizes for\n"
  "    different signal numbers.\n"
  "    Default value for <size> is sizeof(<signo>), i.e. 4 bytes.\n"
  "    E.g. \"-m 1:8 -m 12\" sets minimum size to 8 for signal number 1 and\n"
  "    minimum size to 12 for all other signal numbers.",
  /* XSPRAY_OPT_MAXSZ */
  "    [-M [<signo>:]<size>]\n"
  "    Sets maximum size for messages to transmit. Configuration values are the\n"
  "    same as for -m.",
  /* XSPRAY_OPT_ABORT */
  "    [-q t:<timeout>|a:<timeout>|e:<count>|E:<timeout>|v:<count>]\n"
  "    Sets the abort condition.\n"
  "    t:<timeout> sets the abort timeout. If the delay in ms between arrival of\n"
  "    messages is greater or equal to the timeout then the operation should be\n"
  "    aborted. t:<timeout> is valid for message reception and message\n"
  "    transmission when -a or -e is used.\n"
  "    a:<timeout> sets the acknowledgement abort timeout. If the time between the\n"
  "    sent message and the acknowledgement is greater or equal to the timeout then\n"
  "    the operation should be aborted. a:<timeout> is valid for transmission when\n"
  "    -a is used.\n"
  "    e:<count> sets the number of echo errors before aborting. e:<count> is\n"
  "    valid for message transmission when -e is used.\n"
  "    E:<timeout> sets the echo round trip delay abort timeout. If the round trip\n"
  "    delay is greater or equal to the timeout then the operation should be\n"
  "    aborted. E:<timeout> is valid for transmission when -e is used.\n"
  "    v:<count> sets the number of verify errors before aborting. v:<count> is\n"
  "    valid for message reception when -v is used.",
  /* XSPRAY_OPT_TMDI */
  "    [-t <time>s|<time>ms|<time>us]\n"
  "    Sets the time interval borders for which the message reception\n"
  "    distribution should be displayed This option is valid for message\n"
  "    reception and message transmission when -a or -e is used.\n"
  "    E.g. \"-t5ms -t10ms -t200ms\" results in intervals 0-5ms, 5ms-10ms,\n"
  "    10ms-200ms and > 200ms.",
  /* XSPRAY_OPT_MSGC */
  "    [-c <count>]\n"
  "    Sets the number of messages to transmit in a burst. This option is valid\n"
  "    for message transmission. If <count> is set to -1 then infinite number of\n"
  "    messages are transmitted.\n"
  "    Default value for <count> is 10.",
  /* XSPRAY_OPT_MSGD */
  "    [-d <delay>]\n"
  "    Sets the delay in ms between messages to transmit within a burst. This\n"
  "    option is valid for message transmission.\n"
  "    Default value for <delay> is 0.",
  /* XSPRAY_OPT_BURSTC */
  "    [-C <count>]\n"
  "    Sets the number of bursts to transmit. This option is valid for message\n"
  "    transmission. If <count> is set to -1 then infinite number of bursts are\n"
  "    transmitted.\n"
  "    Default value for <count> is -1, i.e. infinite number of bursts.",
  /* XSPRAY_OPT_BURSTD */
  "    [-D <delay>]\n"
  "    Sets the delay in ms between burst to transmit. This option is valid for\n"
  "    message transmission. Default value for <delay> is 100."
};

static const struct {
  int multArg;
  int abortArg;
} xsprayOptAllowedAs[XSPRAY_NUM_OF_OPTS] =
{
  /* XSPRAY_OPT_PRIO */
  {0, 0},
  /* XSPRAY_OPT_ACK */
  {0, 1},
  /* XSPRAY_OPT_ECHO */
  {1, 1},
  /* XSPRAY_OPT_ECHO_RTD */
  {0, 1},
  /* XSPRAY_OPT_VERIFY */
  {1, 1},
  /* XSPRAY_OPT_ID */
  {1, 0},
  /* XSPRAY_OPT_MINSZ */
  {1, 0},
  /* XSPRAY_OPT_MAXSZ */
  {1, 0},
  /* XSPRAY_OPT_ABORT */
  {1, 0},
  /* XSPRAY_OPT_TMDI */
  {1, 1},
  /* XSPRAY_OPT_MSGC */
  {0, 0},
  /* XSPRAY_OPT_MSGD */
  {0, 0},
  /* XSPRAY_OPT_BURSTC */
  {0, 0},
  /* XSPRAY_OPT_BURSTD */
  {0, 0}
};

/*----------------------------  Function Definitions  -----------------------*/

/******************************************************************************
 *
 * Function:
 *      xsprayGetArgNumSuffix
 *
 * Parameters:
 *      index  Index number of argument
 *
 * Return value:
 *      the argument suffix string
 *
 * Description:
 *      Returns the argument suffix string.
 *
 *****************************************************************************/
const char *xsprayGetArgNumSuffix(int index)
{
  if ((size_t) index >
      sizeof(xsprayArgNumSuffix)/ sizeof(xsprayArgNumSuffix[0]))
  {
    index = sizeof(xsprayArgNumSuffix)/sizeof(xsprayArgNumSuffix[0]);
  }

  return xsprayArgNumSuffix[index-1];
}

/******************************************************************************
 *
 * Function:
 *      xsprayInit
 *
 * Parameters:
 *      None.
 *
 * Return value:
 *      None.
 *
 * Description:
 *      Adds the shell command "xspray".
 *
 *****************************************************************************/
void xsprayInit(void)
{
  shell_add_cmd(XSPRAY_CMD_NAME, "-h for help",  XSPRAY_CMD_SHORT_DESCR,
                xsprayCmd);
}

/******************************************************************************
 *
 * Function:
 *      xsprayCmd
 *
 * Parameters:
 *      argc  Number of arguments.
 *      argv  Argument list.
 *
 * Return value:
 *      RET_SUCCESS on success or RET_ERROR on failure.
 *
 * Description:
 *      This function executes the shell command "xspray".
 *
 *****************************************************************************/
static int xsprayCmd(int argc, char **argv)
{

  int i, cmd;

  for (i = 1; i < argc; i++)
  {
    for (cmd = 0;
         (size_t) cmd < sizeof(xsprayCmdArg) / sizeof(xsprayCmdArg[0]);
         cmd++)
    {
      if (!strcmp(argv[i], xsprayCmdArg[cmd]))
      {
        return xsprayCmdFunc[cmd](argc, argv, i);
      }
    }
  }

  printf(XSPRAY_CMD_NAME ": no valid command argument given\n");
  xsprayPrintSynopsis(XSPRAY_CMD_ALL);
  return RET_ERROR;
}

/******************************************************************************
 *
 * Function:
 *      xsprayCmdHelp
 *
 * Parameters:
 *      argc  Number of arguments.
 *      argv  Argument list.
 *
 * Return value:
 *      RET_SUCCESS on success or RET_ERROR on failure.
 *
 * Description:
 *      Handles the help command.
 *
 *****************************************************************************/
static int xsprayCmdHelp(int argc, char **argv, int argIndex)
{
  (void)argv; /* Avoid compiler warning */
  (void)argIndex; /* Avoid compiler warning */

  if (argc != 2)
  {
    printf(XSPRAY_CMD_NAME ": invalid number of arguments\n");
    xsprayPrintSynopsis(XSPRAY_CMD_HELP);
    return RET_ERROR;
  }

  xsprayPrintHelp();
  return RET_SUCCESS;
}

/******************************************************************************
 *
 * Function:
 *      xsprayCmdSetup
 *
 * Parameters:
 *      argc      Number of arguments.
 *      argv      Argument list.
 *      argIndex  Index in argv where command switch were found.
 *
 * Return value:
 *      RET_SUCCESS on success or RET_ERROR on failure.
 *
 * Description:
 *      Handles the setup command.
 *
 *****************************************************************************/
static int xsprayCmdSetup(int argc, char **argv, int argIndex)
{
  enum XSPRAY_Option option;
  int i;
  union SIGNAL *sig;
  PROCESS pid;
  OSATTREF attRef = (OSATTREF) NULL;
  int alreadyExist = 1;
  int result = RET_ERROR;
  void *hEntry = NULL;

  if (argc < 3 || argIndex > argc - 2)
  {
    printf(XSPRAY_CMD_NAME ": invalid number of arguments\n");
    xsprayPrintSynopsis(XSPRAY_CMD_SETUP);
    goto xsprayCmdSetupExit;
  }

  /* Validate arguments. */
  i = 1;
  while (i < argc)
  {
    if (!strcmp(argv[i], xsprayCmdArg[XSPRAY_CMD_SETUP]))
    {
      /* Setup command, continue with next option*/
      i+= 2;
      continue;
    }

    option = xsprayGetOption(argv[i]);

    if (option == XSPRAY_OPT_NONE)
    {
      xsprayPrintInvalidArg(XSPRAY_CMD_SETUP, "invalid", argv[i], i);
      goto xsprayCmdSetupExit;
    }

    i += xsprayGetNumOfOptionArgs(XSPRAY_CMD_SETUP, option, argc, argv, i);
    i++;
  }

  pid = xsprayGetMonitor();
  attRef = attach(NULL, pid);
  hEntry = xsprayGetProcess(pid, argv[argIndex+1], &attRef);

  if (hEntry == NULL)
  {
    alreadyExist = 0;
    sig = alloc(offsetof(struct XSPRAY_AddReqS, name) +
                  strlen(argv[argIndex+1]) + sizeof('\0'), XSPRAY_ADD_REQ);
    strcpy(sig->addReq.name, argv[argIndex+1]);
    send(&sig, pid);

    sig = xsprayRec(XSPRAY_ADD_CFM, XSPRAY_ADD_REJ, &attRef);
    if (sig == NULL)
    {
      goto xsprayCmdSetupExit;
    }

    hEntry = sig->addCfm.hEntry;
    free_buf(&sig);
  }
  /*
   * First send option XSPRAY_OPT_ID followed by option XSPRAY_OPT_ECHO.
   * This is needed for error checking in xspray monitor.
   */
  if ((result =
         xspraySetOption(pid, hEntry, XSPRAY_OPT_ID, argc, argv,
                         &attRef)) != RET_SUCCESS)
  {
    goto xsprayCmdSetupExit;
  }

  if ((result =
         xspraySetOption(pid, hEntry, XSPRAY_OPT_ECHO, argc, argv,
                         &attRef)) != RET_SUCCESS)
  {
    goto xsprayCmdSetupExit;
  }

  /*
   * Send options other than XSPRAY_OPT_ID and XSPRAY_OPT_ECHO.
   */
  for (option = XSPRAY_OPT_PRIO; /* First option */
       (unsigned char) option < sizeof(xsprayOptArg) / sizeof(xsprayOptArg[0]);
       option++)
  {
    if (option != XSPRAY_OPT_ID && option != XSPRAY_OPT_ECHO)
    {
      if ((result =
           xspraySetOption(pid, hEntry, option, argc, argv,
                           &attRef)) != RET_SUCCESS)
      {
        goto xsprayCmdSetupExit;
      }
    }
  }

xsprayCmdSetupExit:
  if (result != RET_SUCCESS && !alreadyExist)
  {
    (void) xsprayRemove(argv[2]);
  }

  if (attRef != (OSATTREF) NULL)
  {
    detach(&attRef);
  }

  return result;
}

/******************************************************************************
 *
 * Function:
 *      xsprayCmdRemove
 *
 * Parameters:
 *      argc      Number of arguments.
 *      argv      Argument list.
 *      argIndex  Index in argv where command switch were found.
 *
 * Return value:
 *      RET_SUCCESS on success or RET_ERROR on failure.
 *
 * Description:
 *      Handles the remove command.
 *
 *****************************************************************************/
static int xsprayCmdRemove(int argc, char **argv, int argIndex)
{
  (void)argIndex; /* Avoid compiler warning */

  if (argc != 3)
  {
    printf(XSPRAY_CMD_NAME ": invalid number of arguments\n");
    xsprayPrintSynopsis(XSPRAY_CMD_REMOVE);
    return RET_ERROR;
  }

  return xsprayRemove(argv[2]);
}

/******************************************************************************
 *
 * Function:
 *      xsprayCmdStartRx
 *
 * Parameters:
 *      argc   Number of arguments.
 *      argv   Argument list.
 *      Index  Index in argv where command switch were found.
 *
 * Return value:
 *      RET_SUCCESS on success or RET_ERROR on failure.
 *
 * Description:
 *      Handles the start rx command.
 *
 *****************************************************************************/
static int xsprayCmdStartRx(int argc, char **argv, int argIndex)
{
  (void)argIndex; /* Avoid compiler warning */

  if (argc != 3)
  {
    printf(XSPRAY_CMD_NAME ": invalid number of arguments\n");
    xsprayPrintSynopsis(XSPRAY_CMD_START_TX);
    return RET_ERROR;
  }

  return xsprayStart(argv[2], "");
}

/******************************************************************************
 *
 * Function:
 *      xsprayCmdStartTx
 *
 * Parameters:
 *      argc      Number of arguments.
 *      argv      Argument list.
 *      argIndex  Index in argv where command switch were found.
 *
 * Return value:
 *      RET_SUCCESS on success or RET_ERROR on failure.
 *
 * Description:
 *      Handles the start tx command.
 *
 *****************************************************************************/
static int xsprayCmdStartTx(int argc, char **argv, int argIndex)
{
  (void)argIndex; /* Avoid compiler warning */

  if (argc != 4)
  {
    printf(XSPRAY_CMD_NAME ": invalid number of arguments\n");
    xsprayPrintSynopsis(XSPRAY_CMD_START_TX);
    return RET_ERROR;
  }

  return xsprayStart(argv[2], argv[3]);
}

/******************************************************************************
 *
 * Function:
 *      xsprayCmdStop
 *
 * Parameters:
 *      argc      Number of arguments.
 *      argv      Argument list.
 *      argIndex  Index in argv where command switch were found.
 *
 * Return value:
 *      RET_SUCCESS on success or RET_ERROR on failure.
 *
 * Description:
 *      Handles the stop command.
 *
 *****************************************************************************/
static int xsprayCmdStop(int argc, char **argv, int argIndex)
{
  union SIGNAL *sig;
  OSATTREF attRef;
  PROCESS pid;
  int result = RET_ERROR;
  void *hEntry = NULL;

  (void)argIndex; /* Avoid compiler warning */

  if (argc != 3)
  {
    printf(XSPRAY_CMD_NAME ": invalid number of arguments\n");
    xsprayPrintSynopsis(XSPRAY_CMD_STOP);
    return RET_ERROR;
  }

  pid = xsprayGetMonitor();
  attRef = attach(NULL, pid);
  hEntry = xsprayGetProcess(pid, argv[2], &attRef);

  if (hEntry == NULL)
  {
    printf(XSPRAY_CMD_NAME ": %s not found.\n", argv[2]);
  }
  else
  {
    sig = alloc(sizeof(struct XSPRAY_StopReqS), XSPRAY_STOP_REQ);
    sig->stopReq.hEntry = hEntry;
    send(&sig, pid);

    sig = xsprayRec(XSPRAY_STOP_CFM, XSPRAY_STOP_REJ, &attRef);
    if (sig != NULL)
    {
      free_buf(&sig);
      result = RET_SUCCESS;
    }
  }

  if (attRef != (OSATTREF) NULL)
  {
    detach(&attRef);
  }

  return result;
}

/******************************************************************************
 *
 * Function:
 *      xsprayCmdList
 *
 * Parameters:
 *      argc      Number of arguments.
 *      argv      Argument list.
 *      argIndex  Index in argv where command switch were found.
 *
 * Return value:
 *      RET_SUCCESS on success or RET_ERROR on failure.
 *
 * Description:
 *      Handles the list command.
 *
 *****************************************************************************/
static int xsprayCmdList(int argc, char **argv, int argIndex)
{
  (void)argIndex; /* Avoid compiler warning */

  if (argc > 3)
  {
    printf(XSPRAY_CMD_NAME ": invalid number of arguments\n");
    printf("\nSYNOPSIS:%s\n", xsprayCmdSyntax[XSPRAY_CMD_LIST]);
    return RET_ERROR;
  }

  return xsprayList((argc == 3) ? argv[2] : "");
}

/******************************************************************************
 *
 * Function:
 *      xsprayPrintHelp
 *
 * Parameters:
 *      None
 *
 * Return value:
 *      None
 *
 * Description:
 *      Prints help text.
 *
 *****************************************************************************/
static void xsprayPrintHelp(void)
{
  U32 i;

  printf("NAME:\n    %s - %s\n", XSPRAY_CMD_NAME, XSPRAY_CMD_SHORT_DESCR);

  xsprayPrintSynopsis(XSPRAY_CMD_ALL);

  printf("DESCRIPTION:\n");

  for (i = 0; i < XSPRAY_NUM_OF_CMDS; i++)
  {
    printf("%s\n", xsprayCmdDescr[i]);
  }

  printf("\nOPTIONS:");

  for (i = 0; i < XSPRAY_NUM_OF_OPTS; i++)
  {
    printf("\n%s\n\n", xsprayOptDescr[i]);
  }
}

/******************************************************************************
 *
 * Function:
 *      xsprayPrintSynopsis
 *
 * Parameters:
 *      cmd  comand to print synopsis for or XSPRAY_CMD_ALL for all commands.
 *
 * Return value:
 *      None
 *
 * Description:
 *      Prints synopsis.
 *
 *****************************************************************************/
static void xsprayPrintSynopsis(enum XSPRAY_Command cmd)
{
  U32 i;

  printf("\nSYNOPSIS:\n");

  if (cmd == XSPRAY_CMD_ALL)
  {
    for (i = 0; i < XSPRAY_NUM_OF_CMDS; i++)
    {
      printf("%s\n", xsprayCmdSyntax[i]);
    }
  }
  else
  {
    printf("%s\n", xsprayCmdSyntax[cmd]);
  }

  printf("\n");
}

/******************************************************************************
 *
 * Function:
 *      xsprayPrintInvalidArg
 *
 * Parameters:
 *      cmd       Command.
 *      prefix    Prefix in printout.
 *      arg       Argument.
 *      argIndex  Index in argv where argument were found.
 *
 * Return value:
 *      None
 *
 * Description:
 *      Prints an invalid argumanet.
 *
 *****************************************************************************/
static void xsprayPrintInvalidArg(enum XSPRAY_Command cmd, char *prefix,
                                  char *arg, int argIndex)
{
  printf(XSPRAY_CMD_NAME ": %s %d%s argument \"%s\"\n", prefix, argIndex,
         xsprayGetArgNumSuffix(argIndex), arg);
  xsprayPrintSynopsis(cmd);
}

/******************************************************************************
 *
 * Function:
 *      xspraySetOption
 *
 * Parameters:
 *      pid       Pid of monitor process.
 *      hEntry    Handle to entry for which option should be set.
 *      option    Option.
 *      argc      Number of arguments.
 *      argv      Argument list.
 *      pAttRef   Pointer to attach reference.
 *
 * Return value:
 *      RET_SUCCESS on success or RET_ERROR on failure.
 *
 * Description:
 *      Sets an option.
 *
 *****************************************************************************/
static int xspraySetOption(PROCESS pid, void *hEntry,
                           enum XSPRAY_Option option, int argc, char **argv,
                           OSATTREF *pAttRef)
{
  U32 i;
  int result = RET_SUCCESS;
  union SIGNAL *sig = NULL;
  U32 numOfInstances = 0;

  /* First get number of instances of option*/
  i = 1;
  while (i < (U32) argc)
  {
    enum XSPRAY_Option currentOption;

    if (!strcmp(argv[i], xsprayCmdArg[XSPRAY_CMD_SETUP]))
    {
      /* Setup command, continue with next option*/
      i+= 2;
      continue;
    }

    currentOption = xsprayGetOption(argv[i]);

    if (currentOption == option)
    {
      numOfInstances++;
    }

    if (numOfInstances > 1 && !xsprayOptAllowedAs[option].multArg)
    {
      xsprayPrintInvalidArg(XSPRAY_CMD_SETUP, "invalid multiple", argv[i], i);
      result = RET_ERROR;
      goto xspraySetOptionExit;
    }

    i += xsprayGetNumOfOptionArgs(XSPRAY_CMD_SETUP, currentOption, argc, argv,
                                  i);
    i++;
  }

  if (numOfInstances)
  {
    U32 j = 0;

    if (option == XSPRAY_OPT_TMDI)
    {
      /* Add one for last slot containing ULONG_MAX. */
      numOfInstances++;
    }

    /* Get values. */
    sig = alloc(offsetof(struct XSPRAY_SetOptionReqS, instance) +
                  sizeof(struct XSPRAY_OptionInstanceS) * numOfInstances,
                XSPRAY_SET_OPTION_REQ);
    sig->setOptionReq.hEntry = hEntry;
    sig->setOptionReq.option = option;
    sig->setOptionReq.numOfInstances = numOfInstances;

    i = 1;

    while (i < (U32) argc)
    {
      enum XSPRAY_Option currentOption;
      int numOfArgs;

      if (!strcmp(argv[i], xsprayCmdArg[XSPRAY_CMD_SETUP]))
      {
        /* Setup command, continue with next option*/
        i+= 2;
        continue;
      }

      currentOption = xsprayGetOption(argv[i]);

      numOfArgs = xsprayGetNumOfOptionArgs(XSPRAY_CMD_SETUP, currentOption,
                                           argc, argv, i);
      if (currentOption == option)
      {
        sig->setOptionReq.instance[j].argIndex = i;
        sig->setOptionReq.instance[j].numOfValues = 0;

        if (numOfArgs)
        {
          if ((result =
               xsprayGetOptionValue(option, argv[i+1], i+1,
                                    &sig->setOptionReq.instance[j] )) !=
                                      RET_SUCCESS)
          {
            goto xspraySetOptionExit;
          }
        }
        j++;
      }
      i += numOfArgs;
      i++;
    }

    if (option == XSPRAY_OPT_TMDI)
    {
      /* Set last slot to ULONG_MAX. */
      sig->setOptionReq.instance[j].numOfValues = 1; /*lint !e661 */
      sig->setOptionReq.instance[j].value[0] = ULONG_MAX; /*lint !e662 */

      /* sort vales in ascending order */
      qsort(sig->setOptionReq.instance, sig->setOptionReq.numOfInstances,
            sizeof(sig->setOptionReq.instance[0]), xsprayCompareOptionValue);
    }
    else if (option == XSPRAY_OPT_ABORT)
    {
      /* Check for multiple abort conditions */
      for (i = 0; i < sig->setOptionReq.numOfInstances-1; i++)
      {
        for (j = i+1; j < sig->setOptionReq.numOfInstances; j++)
        {
          if (sig->setOptionReq.instance[j].value[0] ==
              sig->setOptionReq.instance[i].value[0]) /*lint !e662 */
          {
            xsprayPrintInvalidArg(XSPRAY_CMD_SETUP, "invalid multiple",
                                  argv[sig->setOptionReq.instance[j].argIndex], /*lint !e661 */
                                  sig->setOptionReq.instance[j].argIndex); /*lint !e661 */
            result = RET_ERROR;
            goto xspraySetOptionExit;
          }
        }
      }
    }

    send(&sig, pid);
    sig = xsprayRec(XSPRAY_SET_OPTION_CFM, XSPRAY_SET_OPTION_REJ, pAttRef);

    if (sig == NULL)
    {
      result = RET_ERROR;
      goto xspraySetOptionExit;
    }

  }
xspraySetOptionExit:
  if (sig != NULL)
  {
    free_buf(&sig);
  }
  return result;
}

/******************************************************************************
 *
 * Function:
 *      xsprayGetOption
 *
 * Parameters:
 *      arg   Argument.
 *
 * Return value:
 *      option or XSPRAY_OPT_NONE if argument dees not correspond to option
 *
 * Description:
 *      Converts and comandline argument to option.
 *
 *****************************************************************************/
static enum XSPRAY_Option xsprayGetOption(char *arg)
{
  enum XSPRAY_Option option;

  for (option = XSPRAY_OPT_PRIO; /* First option */
       (unsigned char) option < sizeof(xsprayOptArg) / sizeof(xsprayOptArg[0]);
       option++)
  {
    if (!strcmp(arg, xsprayOptArg[option]))
    {
      break;
    }
  }

  if (option == sizeof(xsprayOptArg) / sizeof(xsprayOptArg[0]))
  {
    option = XSPRAY_OPT_NONE;
  }

  return option;
}

/******************************************************************************
 *
 * Function:
 *      xsprayGetNumOfOptionArgs
 *
 * Parameters:
 *      cmd       Command.
 *      option    Option.
 *      argc      Number of arguments.
 *      argv      Argument list.
 *      argIndex  Index in argv where argument were found.
 *
 * Return value:
 *      The number of arguments for option.
 *
 * Description:
 *      Returns The number of arguments for option.
 *
 *****************************************************************************/
static int xsprayGetNumOfOptionArgs(enum XSPRAY_Command cmd,
                                    enum XSPRAY_Option option, int argc,
                                    char **argv, int argIndex)
{
  int numOfArgs = 1;

  if (option == XSPRAY_OPT_ECHO_RTD)
  {
    /* No arguments for XSPRAY_OPT_ECHO_RTD */

    numOfArgs = 0;
  }
  else if (option == XSPRAY_OPT_ACK || option == XSPRAY_OPT_ECHO ||
           option == XSPRAY_OPT_VERIFY)
  {
    /* Argument is optional*/
    if (argIndex > argc - 2 ||
        xsprayGetOption(argv[argIndex+1]) != XSPRAY_OPT_NONE ||
        !strcmp(argv[argIndex+1], xsprayCmdArg[cmd]))
    {
      /*
       * No more arguments or Next argument is an option.
       */
      numOfArgs = 0;
    }
  }
  return numOfArgs;
}

/******************************************************************************
 *
 * Function:
 *      xsprayGetOptionValue
 *
 * Parameters:
 *      option     Option.
 *      arg        Argument.
 *      argIndex   Index in argv where argument were found.
 *      pInstance  Pointer to returned value(s)
 *
 * Return value:
 *      RET_SUCCESS on success or RET_ERROR on failure.
 *
 * Description:
 *      Converts and comandline argument to option value(s).
 *
 *****************************************************************************/
static int xsprayGetOptionValue(enum XSPRAY_Option option, char *arg,
                                int argIndex,
                                struct XSPRAY_OptionInstanceS *pInstance)
{
  char *pStop = NULL;
  unsigned long int tmpUL;

  pInstance->argIndex = argIndex;
  pInstance->numOfValues = 1;

  if (option == XSPRAY_OPT_TMDI)
  {
    U32 unit;
    if ((pStop = strstr (arg,"us")) != NULL)
    {
      unit = 1;
    }
    else if ((pStop = strstr (arg,"ms")) != NULL)
    {
      unit = 1000;
    }
    else if ((pStop = strstr (arg,"s")) != NULL)
    {
      unit = 1000000;
    }
    else
    {
      xsprayPrintInvalidArg(XSPRAY_CMD_SETUP, "invalid", arg, argIndex);
      return RET_ERROR;
    }

    if (!stringToUl(arg, pStop, &tmpUL))
    {
      xsprayPrintInvalidArg(XSPRAY_CMD_SETUP, "invalid", arg, argIndex);
      return RET_ERROR;
    }
    pInstance->value[0] = (U32)tmpUL;

    if (pInstance->value[0] > (ULONG_MAX / unit))
    {
      xsprayPrintInvalidArg(XSPRAY_CMD_SETUP, "to large", arg, argIndex);
      return RET_ERROR;
    }

    pInstance->value[0] *= unit;
  }
  else if (option == XSPRAY_OPT_ABORT)
  {
    enum XSPRAY_Option abortOption;

    pInstance->numOfValues = 2;

    for (abortOption = XSPRAY_OPT_PRIO; /* First option */
         (unsigned char) abortOption < sizeof(xsprayOptArg) / sizeof(xsprayOptArg[0]);
         abortOption++)
    {
      if (arg[0] == xsprayOptArg[abortOption][1])
      {
        if (!xsprayOptAllowedAs[abortOption].abortArg)
        {
          xsprayPrintInvalidArg(XSPRAY_CMD_SETUP, "invalid", arg, argIndex);
          return RET_ERROR;
        }

        break;
      }
    }

    if (abortOption == sizeof(xsprayOptArg) / sizeof(xsprayOptArg[0]) ||
        arg[1] != ':' )
    {
      xsprayPrintInvalidArg(XSPRAY_CMD_SETUP, "invalid", arg, argIndex);
      return RET_ERROR;
    }

    pInstance->value[0] = (U32)(int)abortOption;

    if (!stringToUl(&arg[2], pStop, &tmpUL))
    {
      xsprayPrintInvalidArg(XSPRAY_CMD_SETUP, "invalid", arg, argIndex);
      return RET_ERROR;
    }
    pInstance->value[1] = (U32)tmpUL;
  }
  else
  {
    if (((option == XSPRAY_OPT_MSGC || option == XSPRAY_OPT_BURSTC)) &&
        atoi(arg) == -1)
    {
      pInstance->value[0] = (U32) -1;
    }
    else
    {
      if (option == XSPRAY_OPT_ECHO || option == XSPRAY_OPT_VERIFY ||
          option == XSPRAY_OPT_MINSZ || option == XSPRAY_OPT_MAXSZ)
      {
        pStop = strstr (arg,":");
      }

      if (!stringToUl(arg, pStop, &tmpUL))
      {
        xsprayPrintInvalidArg(XSPRAY_CMD_SETUP, "invalid", arg, argIndex);
        return RET_ERROR;
      }
      pInstance->value[0] = (U32)tmpUL;

      if (pStop != NULL)
      {
        pInstance->numOfValues++;

        if (!stringToUl(pStop+1, NULL, &tmpUL))
        {
          xsprayPrintInvalidArg(XSPRAY_CMD_SETUP, "invalid", arg, argIndex);
          return RET_ERROR;
        }
        pInstance->value[1] = (U32)tmpUL;
      }
    }
  }
  return RET_SUCCESS;
}

/******************************************************************************
 *
 * Function:
 *      xsprayCompareOptionValue
 *
 * Parameters:
 *      a  See qsort documentation.
 *      b  See qsort documentation.
 *
 * Return value:
 *      See qsort documentation.
 *
 * Description:
 *      qsort compare function.
 *
 *****************************************************************************/
static int xsprayCompareOptionValue(const void *a, const void *b)
{
  if (((struct XSPRAY_OptionInstanceS*) a)->numOfValues != 1 ||
      ((struct XSPRAY_OptionInstanceS*) b)->numOfValues != 1)
  {
    return 0;
  }

  if (((struct XSPRAY_OptionInstanceS*) a)->value[0] <
      ((struct XSPRAY_OptionInstanceS*) b)->value[0])
  {
    return -1;
  }
  else if (((struct XSPRAY_OptionInstanceS*) a)->value[0] ==
      ((struct XSPRAY_OptionInstanceS*) b)->value[0])
  {
    return 0;
  }
  else
  {
    return 1;
  }
}

/******************************************************************************
 *
 * Function:
 *      stringToUl
 *
 * Parameters:
 *      pString  String to convert.
 *      pStop    Pointer to position where to stop conversion or NULL if
 *               conversion should continue to end of string.
 *      pValue   Pointer to converted value.
 *
 * Return value:
 *      Non-zero if success otherwise zero.
 *
 * Description:
 *      Converts a string to unsigned long.
 *
 *****************************************************************************/
static int stringToUl(const char *pString, const char *pStop,
                      unsigned long *pValue)
{
  char *pEnd;

  *pValue = strtoul( pString, &pEnd, 0 );

  if (pEnd != ((pStop != NULL) ? pStop : pString + strlen(pString)))
  {
    return 0;
  }

  return 1;
}

/******************************************************************************
 *
 * Function:
 *      xsprayGetMonitor
 *
 * Parameters:
 *      None
 *
 * Return value:
 *      Pid of monitor process.
 *
 * Description:
 *      Creates monitor process or returns pid to existng monitor process.
 *
 *****************************************************************************/
static PROCESS xsprayGetMonitor(void)
{
  static PROCESS pid;

  if(!hunt(XSPRAY_MONITOR_NAME, 0, &pid, 0))
  {
    pid = create_process(OS_PRI_PROC, XSPRAY_MONITOR_NAME, xsprayMonitor,
                         1024, 0 /* Highest possible prio. */, 0, 0, 0, 0, 0);
    start(pid);
  }

  return pid;
}

/******************************************************************************
 *
 * Function:
 *      xsprayGetProcess
 *
 * Parameters:
 *      pid      Pid of monitor process.
 *      name     Pointer to name of process.
 *      pAttRef  Pointer to attach reference.
 *
 * Return value:
 *      Handle to proccess.
 *
 * Description:
 *      Returns handle to proccess to use in subsequent minitor requests.
 *
 *****************************************************************************/
static void *xsprayGetProcess(PROCESS pid, const char *name,
                              OSATTREF *pAttRef)
{
  union SIGNAL *sig;
  void *hEntry = NULL;

  sig = alloc(offsetof(struct XSPRAY_GetReqS, name) + strlen(name) +
              sizeof('\0'), XSPRAY_GET_REQ);
  strcpy(sig->getReq.name, name);
  send(&sig, pid);

  sig = xsprayRec(XSPRAY_GET_CFM, XSPRAY_GET_REJ, pAttRef);
  if (sig != NULL)
  {
    hEntry = sig->getCfm.hEntry;
    free_buf(&sig);
  }

  return hEntry;
}

/******************************************************************************
 *
 * Function:
 *      xsprayRemove
 *
 * Parameters:
 *      name    Name of process to remove.
 *
 * Return value:
 *      RET_SUCCESS on success or RET_ERROR on failure.
 *
 * Description:
 *      Removes process.
 *
 *****************************************************************************/
static int xsprayRemove(const char *name)
{
  union SIGNAL *sig;
  PROCESS pid;
  OSATTREF attRef = (OSATTREF) NULL;
  int result = RET_ERROR;
  void *hEntry = NULL;

  pid = xsprayGetMonitor();
  attRef = attach(NULL, pid);
  hEntry = xsprayGetProcess(pid, name, &attRef);

  if (hEntry == NULL)
  {
    printf(XSPRAY_CMD_NAME ": %s not found.\n", name);
  }
  else
  {
    sig = alloc(sizeof(struct XSPRAY_RemoveReqS), XSPRAY_REMOVE_REQ);
    sig->removeReq.hEntry = hEntry;
    send(&sig, pid);
    sig = xsprayRec(XSPRAY_REMOVE_CFM, XSPRAY_REMOVE_REJ, &attRef);

    if (sig != NULL)
    {
      free_buf(&sig);
      result = RET_SUCCESS;
    }
  }

  if (attRef != (OSATTREF) NULL)
  {
    detach(&attRef);
  }

  return result;
}

/******************************************************************************
 *
 * Function:
 *      xsprayStart
 *
 * Parameters:
 *      name    Name of process to start.
 *      rxName  Name of receiver if transmission should be started, empty
 *              string reception should be started.
 *
 * Return value:
 *      RET_SUCCESS on success or RET_ERROR on failure.
 *
 * Description:
 *      Starts reception or transmission.
 *
 *****************************************************************************/
static int xsprayStart(const char *name, const char *rxName)
{
  union SIGNAL *sig;
  PROCESS pid;
  OSATTREF attRef = (OSATTREF) NULL;
  int result = RET_ERROR;
  void *hEntry = NULL;

  pid = xsprayGetMonitor();
  attRef = attach(NULL, pid);
  hEntry = xsprayGetProcess(pid, name, &attRef);

  if (hEntry == NULL)
  {
    printf(XSPRAY_CMD_NAME ": %s not found.\n", name);
  }
  else
  {
    sig = alloc(offsetof(struct XSPRAY_StartReqS, rxName) + strlen(rxName) +
                sizeof('\0'), XSPRAY_START_REQ);
    sig->startReq.hEntry = hEntry;
    strcpy(sig->startReq.rxName, rxName);
    send(&sig, pid);
    sig = xsprayRec(XSPRAY_START_CFM, XSPRAY_START_REJ, &attRef);

    if (sig != NULL)
    {
      free_buf(&sig);
      result = RET_SUCCESS;
    }
  }

  if (attRef != (OSATTREF) NULL)
  {
    detach(&attRef);
  }

  return result;
}

/******************************************************************************
 *
 * Function:
 *      xsprayList
 *
 * Parameters:
 *      name  Name of process to list or empty string list all processes.
 *
 * Return value:
 *      RET_SUCCESS on success or RET_ERROR on failure.
 *
 * Description:
 *      Lists statistics.
 *
 *****************************************************************************/
static int xsprayList(const char *name)
{
  PROCESS pid;
  union SIGNAL *sig;
  OSATTREF attRef = (OSATTREF) NULL;
  int result = RET_SUCCESS;
  void *hEntry = NULL;

  pid = xsprayGetMonitor();
  attRef = attach(NULL, pid);
  hEntry = xsprayGetProcess(pid, name, &attRef);

  if (hEntry == NULL)
  {
    if (name[0] != '\0')
    {
      printf(XSPRAY_CMD_NAME ": %s not found.\n", name);
      result = RET_ERROR;
    }
  }

  while (hEntry != NULL && result == RET_SUCCESS)
  {
    result = xsprayPrintStatus(pid, hEntry, &attRef);

    if (result == RET_SUCCESS && name[0] == '\0' )
    {
      sig = alloc(sizeof(struct XSPRAY_GetNextReqS), XSPRAY_GET_NEXT_REQ);
      sig->getNextReq.hEntry = hEntry;
      send(&sig, pid);
      sig = xsprayRec(XSPRAY_GET_NEXT_CFM, XSPRAY_GET_NEXT_REJ, &attRef);

      if (sig != NULL)
      {
        hEntry = sig->getNextCfm.hNext;
        free_buf(&sig);
      }
      else
      {
        result = RET_ERROR;
      }
    }
    else
    {
      hEntry = NULL;
    }
  }

  if (attRef != (OSATTREF) NULL)
  {
    detach(&attRef);
  }

  return result;
}

/******************************************************************************
 *
 * Function:
 *      xsprayRec
 *
 * Parameters:
 *      sigNoCfm  Signal number of confirm.
 *      sigNoRej  Signal number of reject.
 *      pAttRef   Pointer to attach reference.
 *
 * Return value:
 *      Pointer to received signal or NULL if failure.
 *
 * Description:
 *      Waits for confirm or error.
 *
 *****************************************************************************/
static union SIGNAL *xsprayRec(SIGSELECT sigNoCfm, SIGSELECT sigNoRej,
                               OSATTREF *pAttRef)
{
  union SIGNAL *sig;
  SIGSELECT sel[4];

  sel[0] = 3;
  sel[1] = OS_ATTACH_SIG;
  sel[2] = sigNoCfm;
  sel[3] = sigNoRej;

  sig = receive(sel);

  if (sig->sigNo == OS_ATTACH_SIG)
  {
    *pAttRef = (OSATTREF) NULL;
    printf(XSPRAY_CMD_NAME ": \"%s\" unexpectedly died\n",
           XSPRAY_MONITOR_NAME);
  }
  else if (sig->sigNo == sigNoRej)
  {
    printf(XSPRAY_CMD_NAME ": %s\n", sig->rej.errorDescription);
  }

  if (sig->sigNo != sigNoCfm)
  {
    free_buf(&sig);
    sig = NULL;
  }

  return sig;
}

/******************************************************************************
 *
 * Function:
 *      xsprayPrintStatus
 *
 * Parameters:
 *      pid             Pid of Xspray monitor process.
 *      hEntry          Handle to entry for which process status is requested.
 *      pAttRef         Pointer to attach reference.
 *
 * Return value:
 *      RET_SUCCESS on success or RET_ERROR on failure.
 *
 * Description:
 *      Prints status.
 *
 *****************************************************************************/
static int xsprayPrintStatus(PROCESS pid, void *hEntry, OSATTREF *pAttRef)
{
  union SIGNAL *sig[XSPRAY_NUM_OF_STATUS_TAGS];
  U32 i;

  int result = RET_SUCCESS;

  memset(sig, 0, sizeof(sig));

  /*
   * Read Common config and status
   */
  sig[XSPRAY_STATUS_TAG_CFG_COMMON] =
    xsprayReqStatus(pid, hEntry, XSPRAY_STATUS_TAG_CFG_COMMON, pAttRef);

  if (sig[XSPRAY_STATUS_TAG_CFG_COMMON] == NULL)
  {
    result = RET_ERROR;
    goto xsprayPrintStatusExit;
  }

  sig[XSPRAY_STATUS_TAG_STAT_COMMON] =
    xsprayReqStatus(pid, hEntry, XSPRAY_STATUS_TAG_STAT_COMMON, pAttRef);

  if (sig[XSPRAY_STATUS_TAG_STAT_COMMON] == NULL)
  {
    result = RET_ERROR;
    goto xsprayPrintStatusExit;
  }

  /*
   * Read abort config.
   */
  sig[XSPRAY_STATUS_TAG_CFG_ABORT] =
    xsprayReqStatus(pid, hEntry, XSPRAY_STATUS_TAG_CFG_ABORT, pAttRef);

  if (sig[XSPRAY_STATUS_TAG_CFG_ABORT] == NULL)
  {
    result = RET_ERROR;
    goto xsprayPrintStatusExit;
  }

  /*
   * Read idle/tx config and status
   */
  sig[XSPRAY_STATUS_TAG_CFG_TX] =
    xsprayReqStatus(pid, hEntry, XSPRAY_STATUS_TAG_CFG_TX, pAttRef);

  if (sig[XSPRAY_STATUS_TAG_CFG_TX] == NULL)
  {
    result = RET_ERROR;
    goto xsprayPrintStatusExit;
  }

  sig[XSPRAY_STATUS_TAG_STAT_TX] =
    xsprayReqStatus(pid, hEntry, XSPRAY_STATUS_TAG_STAT_TX, pAttRef);

  if (sig[XSPRAY_STATUS_TAG_STAT_TX] == NULL)
  {
    result = RET_ERROR;
    goto xsprayPrintStatusExit;
  }

  /*
   * Read idle/tx/rx ack config.
   */
  sig[XSPRAY_STATUS_TAG_CFG_TX_AND_RX_ACK] =
    xsprayReqStatus(pid, hEntry, XSPRAY_STATUS_TAG_CFG_TX_AND_RX_ACK, pAttRef);

  if (sig[XSPRAY_STATUS_TAG_CFG_TX_AND_RX_ACK] == NULL)
  {
    result = RET_ERROR;
    goto xsprayPrintStatusExit;
  }

  /*
   * Read idle ack echo/tx ack echo/rx config and status
   */
  sig[XSPRAY_STATUS_TAG_CFG_TX_ACK_ECHO_AND_RX] =
    xsprayReqStatus(pid, hEntry, XSPRAY_STATUS_TAG_CFG_TX_ACK_ECHO_AND_RX,
                    pAttRef);

  if (sig[XSPRAY_STATUS_TAG_CFG_TX_ACK_ECHO_AND_RX] == NULL)
  {
    result = RET_ERROR;
    goto xsprayPrintStatusExit;
  }

  sig[XSPRAY_STATUS_TAG_STAT_TX_ACK_ECHO_AND_RX] =
    xsprayReqStatus(pid, hEntry, XSPRAY_STATUS_TAG_STAT_TX_ACK_ECHO_AND_RX,
                    pAttRef);

  if (sig[XSPRAY_STATUS_TAG_STAT_TX_ACK_ECHO_AND_RX] == NULL)
  {
    result = RET_ERROR;
    goto xsprayPrintStatusExit;
  }

  xsprayPrintHdr(
    sig[XSPRAY_STATUS_TAG_CFG_COMMON]->getStatusCfm.u.cfgCommon.role,
    sig[XSPRAY_STATUS_TAG_CFG_COMMON]->getStatusCfm.u.cfgCommon.mode,
    sig[XSPRAY_STATUS_TAG_CFG_COMMON]->getStatusCfm.u.cfgCommon.modeFlags,
    sig[XSPRAY_STATUS_TAG_CFG_ABORT]->getStatusCfm.u.cfgAbort.numOfConditions);

  xsprayPrintCommon(
    &sig[XSPRAY_STATUS_TAG_CFG_COMMON]->getStatusCfm.u.cfgCommon,
    &sig[XSPRAY_STATUS_TAG_CFG_ABORT]->getStatusCfm.u.cfgAbort,
    &sig[XSPRAY_STATUS_TAG_CFG_TX]->getStatusCfm.u.cfgTx,
    &sig[XSPRAY_STATUS_TAG_CFG_TX_AND_RX_ACK]->getStatusCfm.u.cfgTxAndRxAck,
    &sig[XSPRAY_STATUS_TAG_STAT_COMMON]->getStatusCfm.u.statCommon);

  printf("%*s |\n",  XSPRAY_NAME_COLUMN_WIDTH, "");

  xsprayPrintInfo(sig[XSPRAY_STATUS_TAG_STAT_COMMON]->getStatusCfm.u.
                  statCommon.infoText);

  /*
   * Print idle/tx config and statistics.
   */
  xsprayPrintTxCfgStat(
    &sig[XSPRAY_STATUS_TAG_CFG_COMMON]->getStatusCfm.u.cfgCommon,
    &sig[XSPRAY_STATUS_TAG_CFG_TX]->getStatusCfm.u.cfgTx,
    &sig[XSPRAY_STATUS_TAG_STAT_COMMON]->getStatusCfm.u.statCommon,
    &sig[XSPRAY_STATUS_TAG_STAT_TX]->getStatusCfm.u.statTx);

  /*
   * Print idle ack echo/tx ack echo/rx config and statistics.
   */
  xsprayPrintTxAckEchoAndRxCfgStat(
    &sig[XSPRAY_STATUS_TAG_CFG_COMMON]->getStatusCfm.u.cfgCommon,
    &sig[XSPRAY_STATUS_TAG_CFG_TX_ACK_ECHO_AND_RX]->getStatusCfm.u.
      cfgTxAckEchoAndRx,
    &sig[XSPRAY_STATUS_TAG_STAT_COMMON]->getStatusCfm.u.statCommon,
    &sig[XSPRAY_STATUS_TAG_STAT_TX_ACK_ECHO_AND_RX]->getStatusCfm.u.
      statTxAckEchoAndRx);

  printf("\n");

xsprayPrintStatusExit:
  for (i = 0; i  < sizeof(sig)/sizeof(sig[0]); i++)
  {
    if (sig[i] != NULL)
    {
      free_buf(&sig[i]);
    }
  }
  return result;
}

/******************************************************************************
 *
 * Function:
 *      xsprayReqStatus
 *
 * Parameters:
 *      pid      Pid of receiver of request.
 *      hEntry   Handle to entry for which process status is requested.
 *      tag      Defines which status that is requested.
 *      pAttRef  Pointer to attach reference.
 *
 * Return value:
 *      Pointer to received signal or NULL if failure.
 *
 * Description:
 *      Requests status.
 *
 *****************************************************************************/
static union SIGNAL *xsprayReqStatus(PROCESS pid, void *hEntry,
                                     enum XSPRAY_StatusTag tag,
                                     OSATTREF *pAttRef)
{
  union SIGNAL *sig;

  sig = alloc(sizeof(struct XSPRAY_GetStatusReqS), XSPRAY_GET_STATUS_REQ);
  sig->getStatusReq.hEntry = hEntry;
  sig->getStatusReq.tag = tag;
  send(&sig, pid);
  return xsprayRec(XSPRAY_GET_STATUS_CFM, XSPRAY_GET_STATUS_REJ, pAttRef);
}

/******************************************************************************
 *
 * Function:
 *      xsprayPrintHdr
 *
 * Parameters:
 *      role                  Configured role.
 *      mode                  Configured mode.
 *      modeFlags             Additional mode configuration
 *      numOfAbortconditions  Number of configured abort options.
 *
 * Return value:
 *      None
 *
 * Description:
 *      Prints column header.
 *
 *****************************************************************************/
static void xsprayPrintHdr(enum XSPRAY_Role role, enum XSPRAY_Mode mode,
                           U32 modeFlags, U32 numOfAbortconditions)
{
  U32 i;

  static const char *nameHdrStr[XSPRAY_NUM_OF_ROLES] =
  {
    /* XSPRAY_ROLE_IDLE */
    "Process",
    /* XSPRAY_ROLE_RX */
    "Receiver",
    /* XSPRAY_ROLE_TX */
    "Transmitter"
  };

  printf("%*s | %*s | %*s | %*s | %*s | %*s | %*s | ",
         XSPRAY_NAME_COLUMN_WIDTH,
         nameHdrStr[role],
         XSPRAY_STATE_COLUMN_WIDTH, "State",
         XSPRAY_MODE_COLUMN_WIDTH, "Mode",
         XSPRAY_PRIO_COLUMN_WIDTH, "Prio",
         XSPRAY_ULONG_COLUMN_WIDTH, "Id",
         XSPRAY_ULONG_COLUMN_WIDTH, "Min size",
         XSPRAY_ULONG_COLUMN_WIDTH, "Max size");

  if ((role != XSPRAY_ROLE_RX && mode == XSPRAY_MODE_ECHO) ||
      (role != XSPRAY_ROLE_TX &&
       (modeFlags & XSPRAY_MODE_FLAG_VERIFY)) ||
      (mode == XSPRAY_MODE_ACK) || numOfAbortconditions)
  {
    if (role == XSPRAY_ROLE_IDLE && mode == XSPRAY_MODE_ECHO &&
        (modeFlags & XSPRAY_MODE_FLAG_VERIFY))
    {
      printf("%*s | ", XSPRAY_MASK_COUNT_COLUMN_WIDTH, "Mask");
    }
    else if (role != XSPRAY_ROLE_RX && mode == XSPRAY_MODE_ECHO)
    {
      printf("%*s | ", XSPRAY_MASK_COUNT_COLUMN_WIDTH, "Echo mask");
    }
    else if (role != XSPRAY_ROLE_TX &&
             (modeFlags & XSPRAY_MODE_FLAG_VERIFY))
    {
      printf("%*s | ", XSPRAY_MASK_COUNT_COLUMN_WIDTH, "Verify mask");
    }
    else if (mode == XSPRAY_MODE_ACK )
    {
      printf("%*s | ", XSPRAY_MASK_COUNT_COLUMN_WIDTH, "Ack count");
    }

    if (numOfAbortconditions)
    {
      printf("%*s", XSPRAY_ABORT_COLUMN_WIDTH, "Abort option");
    }

    printf("\n");
  }
  else
  {
    printf("\n");
  }

  for (i = 0; i < XSPRAY_ROW_LENGTH+1; i++)
  {
    printf("%s", "-");
  }

  printf("\n");
}

/******************************************************************************
 *
 * Function:
 *      xsprayPrintCommon
 *
 * Parameters:
 *      pCfgCommon      Pointer to common configuration.
 *      pCfgAbort       Pointer to abort configuration.
 *      pCfgTx          Pointer to tx configuration or NULL.
 *      pCfgTxAndRxAck  Pointer to Tx and rx with ack configuration or NULL.
 *      pStatCommon     Pointer to common statistics.
 *
 * Return value:
 *      None
 *
 * Description:
 *      Prints common config and statistics.
 *
 *****************************************************************************/
static void xsprayPrintCommon(struct XSPRAY_CfgCommonS *pCfgCommon,
                              struct XSPRAY_CfgAbortS * pCfgAbort,
                              struct XSPRAY_CfgTxS *pCfgTx,
                              struct XSPRAY_CfgTxAndRxAckS *pCfgTxAndRxAck,
                              struct XSPRAY_StatCommonS *pStatCommon)
{
  char *pName, *pNameEnd, *pRxName, *pRxNameEnd,
    modeStr[XSPRAY_MODE_COLUMN_WIDTH+1];
  const char *pModeFlagStr;
  U32 numOfPrintedChars;
  U32 numOfAbortconditions;
  U32 numOfMsg = 0;
  U32 abortIndex = 0;
  U32 msgIndex = 0;
  int firstLine = 1;
  U32 rowsSinceEndofName = 0;
  int leftAlignName = 0;

  static const char *modeConstStr[XSPRAY_NUM_OF_MODES] =
  {
    /* XSPRAY_MODE_NORMAL */
    "Normal",
    /* XSPRAY_MODE_ACK */
    "Ack",
     /* XSPRAY_MODE_ECHO */
    "Echo"
  };

  pName = pCfgCommon->name;
  pNameEnd = pName + strlen(pName);

  numOfAbortconditions = pCfgAbort->numOfConditions;

  pRxName = pCfgTx->rxName;
  pRxNameEnd = pRxName + strlen(pRxName);

  if (pCfgCommon->role != XSPRAY_ROLE_RX ||
      pCfgCommon->mode == XSPRAY_MODE_ACK ||
      (pCfgCommon->modeFlags & XSPRAY_MODE_FLAG_VERIFY))
  {
    numOfMsg = pCfgTxAndRxAck->numOfMsg;
  }

  pModeFlagStr = "";

  if (pCfgCommon->mode == XSPRAY_MODE_NORMAL &&
      (pCfgCommon->modeFlags & XSPRAY_MODE_FLAG_VERIFY))
  {
    strncpy(modeStr,"Verify", sizeof(modeStr));
  }
  else
  {
    snprintf(modeStr,sizeof(modeStr),"%s%s", modeConstStr[pCfgCommon->mode],
             (pCfgCommon->modeFlags & XSPRAY_MODE_FLAG_ECHO_RTD) ?
               "(RTD)" : "");

    if (pCfgCommon->modeFlags & XSPRAY_MODE_FLAG_VERIFY)
    {
      pModeFlagStr = "Verify";
    }
  }

  do
  {
    if (*pName != '\0' || (*pName == '\0' && !rowsSinceEndofName))
    {
      numOfPrintedChars = xsprayPrintName(leftAlignName, pName, pNameEnd);
      leftAlignName |= (pNameEnd - pName > XSPRAY_NAME_COLUMN_WIDTH);   /*lint !e514 */
      pName += numOfPrintedChars;

      if (*pName == '\0')
      {
        rowsSinceEndofName++;
      }
    }
    else
    {
      if (*pRxName != '\0' && rowsSinceEndofName == 1)
      {
        printf("%*s | ", XSPRAY_NAME_COLUMN_WIDTH, "");
      }
      else if (*pRxName != '\0' && rowsSinceEndofName == 2)
      {
        printf("%*s | ", XSPRAY_NAME_COLUMN_WIDTH, "Receiver:");
        leftAlignName = 0;
      }
      else
      {
        numOfPrintedChars = xsprayPrintName(leftAlignName, pRxName, pRxNameEnd);
        leftAlignName |= (pRxNameEnd - pRxName > XSPRAY_NAME_COLUMN_WIDTH);  /*lint !e514 */
        pRxName += numOfPrintedChars;
      }
      rowsSinceEndofName++;
    }

    if (firstLine || *pModeFlagStr != '\0' || msgIndex < numOfMsg ||
        abortIndex < numOfAbortconditions)
    {
      if (firstLine)
      {
        xsprayPrintState(pCfgCommon->role, pStatCommon->state);
        printf("%*s | %*u | ",
               XSPRAY_MODE_COLUMN_WIDTH,
               modeStr,
               XSPRAY_PRIO_COLUMN_WIDTH,
               pCfgCommon->priority);
      }
      else
      {
        printf("%*s | %*s | %*s | ",
               XSPRAY_STATE_COLUMN_WIDTH, "",
               XSPRAY_MODE_COLUMN_WIDTH, pModeFlagStr,
               XSPRAY_PRIO_COLUMN_WIDTH, "");
        pModeFlagStr = "";
      }

      if (msgIndex < numOfMsg)
      {
        printf("%#0*x | %*lu | %*lu | ",
               XSPRAY_ULONG_COLUMN_WIDTH,
               (unsigned int) pCfgTxAndRxAck->msg[msgIndex].id,
               XSPRAY_ULONG_COLUMN_WIDTH,
               (unsigned long int)pCfgTxAndRxAck->
               msg[msgIndex].adjustedSize.min,
               XSPRAY_ULONG_COLUMN_WIDTH,
               (unsigned long int)pCfgTxAndRxAck->
               msg[msgIndex].adjustedSize.max);

        if ((pCfgCommon->role != XSPRAY_ROLE_RX &&
             pCfgCommon->mode == XSPRAY_MODE_ECHO) ||
            (pCfgCommon->role != XSPRAY_ROLE_TX &&
             (pCfgCommon->modeFlags & XSPRAY_MODE_FLAG_VERIFY)))
        {
          U32 i;

          for (i = XSPRAY_ULONG_COLUMN_WIDTH;
               i < XSPRAY_MASK_COUNT_COLUMN_WIDTH;
               i++)
          {
            printf(" ");
          }

          printf("%#0*x | ", XSPRAY_ULONG_COLUMN_WIDTH,
                 (unsigned int) pCfgTxAndRxAck->msg[msgIndex].echoAndVerify.
                   mask);
        }
        msgIndex++;
      }
      else
      {
        printf("%*s | %*s | %*s | ",
               XSPRAY_ULONG_COLUMN_WIDTH, "",
               XSPRAY_ULONG_COLUMN_WIDTH, "",
               XSPRAY_ULONG_COLUMN_WIDTH, "");

        if ((pCfgCommon->role != XSPRAY_ROLE_RX &&
             pCfgCommon->mode == XSPRAY_MODE_ECHO) ||
            (pCfgCommon->role != XSPRAY_ROLE_TX &&
             (pCfgCommon->modeFlags & XSPRAY_MODE_FLAG_VERIFY)))
        {
          printf("%*s | ", XSPRAY_MASK_COUNT_COLUMN_WIDTH, "");
        }
      }

      if (pCfgCommon->mode == XSPRAY_MODE_ACK )
      {
        if (firstLine)
        {
          printf("%*lu | ", XSPRAY_MASK_COUNT_COLUMN_WIDTH,
                 (unsigned long int)pCfgTxAndRxAck->ackCount);
        }
        else
        {
          printf("%*s | ", XSPRAY_MASK_COUNT_COLUMN_WIDTH, "");
        }
      }

      if (numOfAbortconditions)
      {
        if (abortIndex < numOfAbortconditions)
        {
          printf("%c:%0*lu",
                 xsprayOptArg[pCfgAbort->condition[abortIndex].option][1],
                 XSPRAY_ABORT_COLUMN_WIDTH-2,
                 (unsigned long int)pCfgAbort->condition[abortIndex].value);
          abortIndex++;
        }
      }
    }

    printf("\n");
    firstLine = 0;

  } while (*pName != '\0' || *pRxName != '\0' || *pModeFlagStr != '\0' ||
           msgIndex < numOfMsg || abortIndex < numOfAbortconditions);
}

/******************************************************************************
 *
 * Function:
 *      xsprayPrintName
 *
 * Parameters:
 *      leftAlign Non zero if name should be left aligned.
 *      pName     Name.
 *      pEnd      Pointer to end of name.
 *
 * Return value:
 *      Number of printed characters.
 *
 * Description:
 *      Prints a name.
 *
 *****************************************************************************/
static U32 xsprayPrintName(int leftAlign, const char *pName, const char *pEnd)
{
  if (*pName != '\0')
  {
    if (pEnd - pName > XSPRAY_NAME_COLUMN_WIDTH)
    {
      printf("%*.*s \\ | ",
             XSPRAY_NAME_COLUMN_WIDTH - XSPRAY_LINE_CONT_WIDTH,
             XSPRAY_NAME_COLUMN_WIDTH - XSPRAY_LINE_CONT_WIDTH,
             pName);
      return (XSPRAY_NAME_COLUMN_WIDTH - XSPRAY_LINE_CONT_WIDTH);
    }
    else
    {
      if (leftAlign)
      {
        printf("%-*s | ", XSPRAY_NAME_COLUMN_WIDTH, pName);
      }
      else
      {
        printf("%*s | ", XSPRAY_NAME_COLUMN_WIDTH, pName);
      }
      return pEnd-pName;
    }
  }
  else
  {
    printf("%*s | ", XSPRAY_NAME_COLUMN_WIDTH, "");
    return 0;
  }
}

/******************************************************************************
 *
 * Function:
 *      xsprayPrintState
 *
 * Parameters:
 *      role   Configured role.
 *      state  Current state.
 *
 * Return value:
 *      None
 *
 * Description:
 *      Prints the state.
 *
 *****************************************************************************/
static void xsprayPrintState(enum XSPRAY_Role role, enum XSPRAY_State state)
{
  static const char *roleStr[XSPRAY_NUM_OF_ROLES] =
  {
    /* XSPRAY_ROLE_IDLE */
    "Idle",
    /* XSPRAY_ROLE_RX */
    "Rx",
    /* XSPRAY_ROLE_TX */
    "Tx"
  };

  static const char *stateStr[XSPRAY_NUM_OF_STATES] =
  {
    /* XSPRAY_STATE_IDLE */
    ":Idle",
    /* XSPRAY_STATE_BUSY */
    ":Busy",
    /* XSPRAY_STATE_STOPPED */
    ":Stopped",
    /* XSPRAY_STATE_DONE */
    ":Done",
    /* XSPRAY_STATE_ERROR */
    ":Error"
  };

  char roleStateStr[XSPRAY_STATE_COLUMN_WIDTH+1];

  snprintf(roleStateStr,sizeof(roleStateStr),"%s%s", roleStr[role],
           (role != XSPRAY_ROLE_IDLE) ? stateStr[state] : "");
  printf("%*s | ",XSPRAY_STATE_COLUMN_WIDTH, roleStateStr);
}

/******************************************************************************
 *
 * Function:
 *      xsprayPrintInfo
 *
 * Parameters:
 *      info  Info string.
 *
 * Return value:
 *      None
 *
 * Description:
 *      Prints info string.
 *
 *****************************************************************************/
static void xsprayPrintInfo(const char *info)
{
  size_t remainingLength = strlen(info);
  int firstLine = 1;

  while (remainingLength)
  {
    size_t i,length;

    /* Skip leading spaces */
    while (remainingLength && *info  == ' ')
    {
      remainingLength--;
      info++;
    }

    if (!remainingLength)
    {
      break;
    }

    length = (remainingLength > XSPRAY_EXTRA_COLUMN_LENGTH) ?
      XSPRAY_EXTRA_COLUMN_LENGTH - XSPRAY_LINE_CONT_WIDTH :
        remainingLength;

    /* Break line at space. */
    for (i = length; length < remainingLength && i > 0; i--)
    {
      if (info[i] == ' ')
      {
        break;
      }

    }

    if (i)
    {
      length = i;
    }

    remainingLength -= length;

    printf("%*s | %-.*s%s\n",
           XSPRAY_NAME_COLUMN_WIDTH, (firstLine) ? "Info:" : "",
           length, info, (remainingLength) ? " \\" : "");

    info += length;
    firstLine = 0;
  }
}

/******************************************************************************
 *
 * Function:
 *      xsprayPrintTxCfgStat
 *
 * Parameters:
 *      pCfgCommon   Pointer to common configuration.
 *      pCfgTx       Pointer to tx configuration.
 *      pStatCommon  Pointer to common statistics.
 *      pStatTx      Pointer to tx statistics.
 *
 * Return value:
 *      None
 *
 * Description:
 *      Prints tx config.
 *
 *****************************************************************************/
static void xsprayPrintTxCfgStat(struct XSPRAY_CfgCommonS *pCfgCommon,
                                 struct XSPRAY_CfgTxS *pCfgTx,
                                 struct XSPRAY_StatCommonS *pStatCommon,
                                 struct XSPRAY_StatTxS *pStatTx)
{
  if (pCfgCommon->role == XSPRAY_ROLE_RX)
  {
    return;
  }

  printf("%*s | ", XSPRAY_NAME_COLUMN_WIDTH, "Burst Cfg Tx:");

  if (pCfgTx->msgCount == (U32) -1)
  {
    printf("msg count=%s, ", "Infinite (-1)");
  }
  else
  {
    printf("msg count=%lu, ", (unsigned long int)pCfgTx->msgCount);
  }

  printf("msg delay=%lums, ", (unsigned long int)pCfgTx->msgDelay);

  if (pCfgTx->burstCount == (U32) -1)
  {
    printf("burst count=%s, ", "Infinite (-1)");
  }
  else
  {
    printf("burst count=%lu, ", (unsigned long int)pCfgTx->burstCount);
  }

  printf("burst delay=%lums\n", (unsigned long int)pCfgTx->burstDelay);

  if (pCfgCommon->role == XSPRAY_ROLE_TX)
  {
    printf("%*s | sent: msgs=%lu, bytes=%lu",
           XSPRAY_NAME_COLUMN_WIDTH, "Stat Tx:", (unsigned long int)pStatTx->
           msgs,
           (unsigned long int)pStatTx->bytes);

    if (pStatCommon->totalTime)
    {
      printf(", total time=%lums\n", (unsigned long int)pStatCommon->totalTime);
    }
    else
    {
      printf("\n");
    }
  }
}

/******************************************************************************
 *
 * Function:
 *      xsprayPrintTxAckEchoAndRxCfgStat
 *
 * Parameters:
 *      pCfgCommon           Pointer to common configuration.
 *      pCfgTxAckEchoAndRx   Pointer to tx ack echo/rx configuration.
 *      pStatCommon          Pointer to common statistics.
 *      pStatTxAckEchoAndRx  Pointer to tx ack echo/rx statistics.
 *
 * Return value:
 *      None
 *
 * Description:
 *      Prints tx ack echo/rx configuration and statistics.
 *
 *****************************************************************************/
static void xsprayPrintTxAckEchoAndRxCfgStat(
  struct XSPRAY_CfgCommonS *pCfgCommon,
  struct XSPRAY_CfgTxAckEchoAndRxS *pCfgTxAckEchoAndRx,
  struct XSPRAY_StatCommonS *pStatCommon,
  struct XSPRAY_StatTxAckEchoAndRxS *pStatTxAckEchoAndRx)
{
  U32 i, prevTimeSlot;
  char *prevTimeSlotUnitStr;

  if ((pCfgCommon->role == XSPRAY_ROLE_IDLE &&
      !pCfgTxAckEchoAndRx->numOfTimeSlots) ||
      (pCfgCommon->role == XSPRAY_ROLE_TX &&
      pCfgCommon->mode == XSPRAY_MODE_NORMAL))
  {
    return;
  }

  printf("%*s | ", XSPRAY_NAME_COLUMN_WIDTH, "Cfg,Stat Rx:");

  if (pCfgCommon->role != XSPRAY_ROLE_IDLE)
  {
    printf("rec: msgs=%lu, bytes=%lu",
           (unsigned long int)pStatTxAckEchoAndRx->msgs,
           (unsigned long int)pStatTxAckEchoAndRx->bytes);

    if (pCfgCommon->role == XSPRAY_ROLE_RX && pStatCommon->totalTime)
    {
      printf(", total time=%lums\n", (unsigned long int)pStatCommon->totalTime);
    }
    else
    {
      printf("\n");
    }

    if (pCfgCommon->role == XSPRAY_ROLE_TX)
    {
      if (pCfgCommon->mode == XSPRAY_MODE_ECHO)
      {
        printf("%*s | echo: errors=%lu",
               XSPRAY_NAME_COLUMN_WIDTH, "",
               (unsigned long int)pStatTxAckEchoAndRx->echoAndVerify.errors);

        if (pCfgCommon->modeFlags & XSPRAY_MODE_FLAG_ECHO_RTD)
        {
          printf(", min RTT=%luus, max RTT=%luus",
                 (unsigned long int)pStatTxAckEchoAndRx->
                 echoAndAck.roundTripDelay.min,
                 (unsigned long int)pStatTxAckEchoAndRx->
                 echoAndAck.roundTripDelay.max);
        }
        printf("\n");
      }
      else if (pCfgCommon->mode == XSPRAY_MODE_ACK)
      {
        printf("%*s | ack: min ack time=%luus, max ack time=%luus\n",
               XSPRAY_NAME_COLUMN_WIDTH, "",
               (unsigned long int)pStatTxAckEchoAndRx->
               echoAndAck.roundTripDelay.min,
               (unsigned long int)pStatTxAckEchoAndRx->
               echoAndAck.roundTripDelay.max);
      }
    }
    else if (pCfgCommon->role == XSPRAY_ROLE_RX &&
             (pCfgCommon->modeFlags & XSPRAY_MODE_FLAG_VERIFY))
    {
      printf("%*s | verify: errors=%lu\n",
             XSPRAY_NAME_COLUMN_WIDTH, "",
             (unsigned long int)pStatTxAckEchoAndRx->echoAndVerify.errors);
    }

    if (pStatTxAckEchoAndRx->msgs)
    {
      printf("%*s | last rec msg: id=0x%x, size=%lu\n",
             XSPRAY_NAME_COLUMN_WIDTH, "",
             (unsigned int) pStatTxAckEchoAndRx->lastMsg.id,
             (unsigned long int)pStatTxAckEchoAndRx->lastMsg.size);
    }
  }

  for (i = prevTimeSlot = 0, prevTimeSlotUnitStr = "";
       i < pCfgTxAckEchoAndRx->numOfTimeSlots;
       i++)
  {
    char *timeSlotUnitStr;
    U32 timeSlot = pCfgTxAckEchoAndRx->timeSlot[i];

    timeSlotUnitStr = "us";

    if(timeSlot / 1000)
    {
      timeSlot = timeSlot / 1000;
      timeSlotUnitStr = "ms";
    }

    if(timeSlot / 1000)
    {
      timeSlot = timeSlot / 1000;
      timeSlotUnitStr = "s";
    }

    if (i || pCfgCommon->role != XSPRAY_ROLE_IDLE)
    {
      printf("%*s | ", XSPRAY_NAME_COLUMN_WIDTH, "");
    }

    if (i != pCfgTxAckEchoAndRx->numOfTimeSlots - 1)
    {
      printf("%lu%s < t <= %lu%s = %lu msgs\n",
             (unsigned long int)prevTimeSlot, prevTimeSlotUnitStr,
             (unsigned long int)timeSlot, timeSlotUnitStr,
             (unsigned long int)pStatTxAckEchoAndRx->timeSlotMsgCounter[i]);
    }
    else
    {
      printf("> %lu%s = %lu msgs\n",
             (unsigned long int)prevTimeSlot, prevTimeSlotUnitStr,
             (unsigned long int)pStatTxAckEchoAndRx->timeSlotMsgCounter[i]);
    }

    prevTimeSlot = timeSlot;
    prevTimeSlotUnitStr = timeSlotUnitStr;
  }
}
