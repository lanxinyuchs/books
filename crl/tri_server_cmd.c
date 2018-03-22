/**
 *   Implements the command interpreter for the TRI server.
 *
 *   The contents of this file implements the command interpreter
 *   for the TRI server.
 *
 *   Copyright (C) 2013 by Ericsson AB. All rights reserved. The
 *   information in this document is the property of Ericsson. Except
 *   as specifically authorized in writing by Ericsson, the receiver
 *   of this document shall keep the information contained herein
 *   confidential and shall protect the same in whole or in part from
 *   disclosure and dissemination to third parties. Disclosure and
 *   disseminations to the receiver's employees shall only be made on
 *   a strict need to know basis.
 */

/* ========================================================================
 *   History of development:
 *   -----------------------
 *   Revised : 2013-04-30 Anette Schött
 *   Change  : First version.
 *
 *   Revised : 2014-02-04 Stanislav Vovk
 *   Change  : Changed 'mbox' field to 'pid' in status print out
 *
 *   Revised : 2014-02-18 Stanislav Vovk
 *   Change  : Using shell_run_cmd() instead of system()
 *
 *   Revised : 2015-06-02 Fredrik Skog
 *   Change  : TR HT78355: Added checks for return value of all malloc
 *             calls in TRI to prevent crashes.
 *
* ========================================================================
 */

/* ========================================================================
 *   INCLUDE FILES
 * ========================================================================
 */

#define _XOPEN_SOURCE 500
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdint.h>
#include <unistd.h>
#include <sys/types.h>
#include <dirent.h>
#include <errno.h>
#include <lttng/lttng.h>
#include <lttng/snapshot.h>
#include <syslog.h>
#include <ftw.h>
#include <shell.h>
#include "itc.h"
#include "tri_server_cmd.h"
#include "tri_server.h"
#include "cello_te_group.h"


/* ========================================================================
 *   DEFINITIONS
 * ========================================================================
 */

/*
** This is the main TRI server command in the shell
*/
#define MAIN_CMD           "te"

/*
** Define syntax for the different parameters
*/
#define GROUP_SYNTAX                                                    \
   "<group> = check | error | enter | return | info | trace1 .. trace9 |\n\
             state_change | bus_send | bus_receive | rec_sig | send_sig |\n\
             param | user1 .. user4 | all\n"

/*
** Wait for 10 seconds for a confirm of command
*/
#define CONFIRM_TMO          10000

/*
** Te command mailbox name
*/
#define TE_CMD_MAILBOX_NAME "teCmdMbox"

/*
** Define value to represent all core id's
*/
#define ALL_CORES            (-1)

/*
** Default path where traces are stored
*/
#define TELOG_PATH "/var/volatile/log/telog"

/*
** Default path where traces are stored
*/
#define LTTNG_SES_NAME "s1"

/*
** Constants
*/

struct groupTable
{
  char         *groupName;
  uint32_t      groupType;
};

const struct groupTable groupTable[] =
{
  {"check",        GROUP_CHECK},
  {"error",        GROUP_ERROR},
  {"enter",        GROUP_ENTER},
  {"return",       GROUP_RETURN},
  {"info",         GROUP_INFO},
  {"trace1",       GROUP_TRACE1},
  {"trace2",       GROUP_TRACE2},
  {"trace3",       GROUP_TRACE3},
  {"trace4",       GROUP_TRACE4},
  {"trace5",       GROUP_TRACE5},
  {"trace6",       GROUP_TRACE6},
  {"trace7",       GROUP_TRACE7},
  {"trace8",       GROUP_TRACE8},
  {"trace9",       GROUP_TRACE9},
  {"state_change", GROUP_STATE_CHANGE},
  {"bus_send",     GROUP_BUS_SEND},
  {"bus_receive",  GROUP_BUS_RECEIVE},
  {"rec_sig",      GROUP_REC_SIG},
  {"send_sig",     GROUP_SEND_SIG},
  {"param",        GROUP_PARAM},
  {"interface",    GROUP_INTERFACE},
  {"object",       GROUP_TRACE_OBJ},
  {"user1",        GROUP_USER1},
  {"user2",        GROUP_USER2},
  {"user3",        GROUP_USER3},
  {"user4",        GROUP_USER4},
  {NULL,           GROUP_RESERVED2},
  {NULL,           GROUP_RESERVED3},
  {NULL,           GROUP_RESERVED4},
  {NULL,           GROUP_RESERVED5},
};


/* ========================================================================
 *   TYPE DEFINITIONS
 * ========================================================================
 */
union itc_msg
{
   uint32_t               msgNo;
   GroupMaskReq           groupMaskReq;
   GroupMaskCfm           groupMaskCfm;
   SetDefaultGroupMaskReq setDefaultGroupMaskReq;
   SetDefaultGroupMaskCfm setDefaultGroupMaskCfm;
   StatusReq              statusReq;
   StatusInfoInd          statusInfoInd;
   StatusCfm              statusCfm;
   SaveGroupMaskReq       saveGroupMaskReq;
   SaveGroupMaskCfm       saveGroupMaskCfm;
   LogReadReq             logReadReq;
   LogReadCfm             logReadCfm;
   TeLogReadAck2Ind       logReadAck2Ind;
   LogClearReq            logClearReq;
   LogClearCfm            logClearCfm;

};

/* ========================================================================
 *   DATA DECLARATIONS
 * ========================================================================
 */
static const char shortTraceAndErrorUsage[] ="Take cmd 'man te' for help info\n";

/* LTTng session name */
static char *tri_lttng_ses_name = NULL;

/* telog path */
static char *tri_telog_path = NULL;

extern char tri_server_mbox_name[];

/* ========================================================================
 *   FUNCTION PROTOTYPES
 * ========================================================================
 */

/* ===================================================================== */
/**
 *   Converts a string to an trace group type.
 *
 *   @param group   The trace group name to convert
 *
 *   @return     The corresponding trace group type. If no corresponding
 *               trace group type is found then GROUP_RESERVED2
 *               is returned.
 *
 *   @par Globals:
 *               --
 */
/* ===================================================================== */
static uint32_t
convertGroupType(char *group)
{
  int i;

  for (i = 0; groupTable[i].groupName; i++)
  {
    if (strcmp(group, groupTable[i].groupName) == 0)
    {
      return groupTable[i].groupType;
    }
  }

  return GROUP_RESERVED2;
}

/* ===================================================================== */
/**
 *   Checks and prints the result of the operation.
 *
 *   @param result   The result to check
 *
 *   @param output   Where to print the result
 *
 *   @return     RET_SUCCESS if the command executed sucessfully.
 *               RET_WARNING if an error that is to be classified
 *                           as an warning occurred.
 *               RET_ERROR   if an error has occured.
 *
 *   @par Globals:
 *               --
 */
/* ===================================================================== */
static int
printResult(TeCmdResultType result, FILE *output)
{
   switch (result)
   {
      case TE_CMD_RESULT_OK:
         return RET_SUCCESS;

      case TE_CMD_RESULT_NOT_OK:
         (void)fprintf(output, "Command failed\n");
         return RET_ERROR;

      case TE_CMD_RESULT_PROCESS_NOT_FOUND:
         (void)fprintf(output, "The specified process was not found\n");
         return RET_WARNING;

      case TE_CMD_RESULT_SYNTAX_ERROR:
         (void)fprintf(output, "Syntax error\n");
         return RET_WARNING;

      default:
         (void)fprintf(output, "Unknown command result\n");
         return RET_ERROR;
   }
}


/* ===================================================================== */
/**
 *   Checks and prints the result of the log operation.
 *
 *   @param result   The result to check
 *
 *   @param core     The coreid, but -1 if not applicable
 *
 *   @param output   Where to print the result
 *
 *   @return     RET_SUCCESS if the command executed sucessfully.
 *               RET_WARNING if an error that is to be classified
 *                                    as an warning occurred.
 *               RET_ERROR   if an error has
 *   occured.
 *
 *   @par Globals:
 *               --
 */
/* ===================================================================== */
static int
printLogResult(TeResultType result,
               int core,
               FILE *output)
{
   switch (result)
   {
      case TE_LOG_RESULT_OK:
         return RET_SUCCESS;

      case TE_LOG_RESULT_NOT_OK:
         (void)fprintf(output, "Command failed\n");
         return RET_ERROR;

      case TE_LOG_RESULT_EMPTY:
         (void)fprintf(output, "The Trace & Error Log Core %02d is empty\n",
                       core);
         return RET_WARNING;

      case TE_LOG_RESULT_BUSY:
         (void)fprintf(output, "The Trace & Error Log Core %02d is "
                       "already being read\n", core);
         return RET_WARNING;

      default:
         (void)fprintf(output, "Unknown command result\n");
         return RET_ERROR;
   }
}


/* ===================================================================== */
/**
 *   Prints out all enabled trace groups in the specified trace group mask.
 *
 *   @param output     Print enabled trace groups on this output
 *
 *   @param groupMask  The trace group mask to print enabled groups in
 *
 *   @return     -
 *
 *   @par Globals:
 *               --
 *
 *  Post-conditions:
 *    All enabled trace groups has been printed to the specified FILE *.
 */
/* ===================================================================== */
static void
printEnabledTraceGroups(FILE *output,
                        U32 groupMask)
{
   Boolean first = True;
   int i;

   for (i = 0; groupTable[i].groupName; i++)
   {
      if (groupMask & (1 << groupTable[i].groupType))
      {
         if (! first)
         {
            (void)fputs(" ", output);
         }
         else
         {
            first = False;
         }
         (void)fputs(groupTable[i].groupName, output);
      }
   }
   (void)fputs("\n", output);
}

/* ===================================================================== */
/**
 *   Locate mailbox of tri server
 *
 *   @param output     Output to shell
 *   @param common_mb  returns flag if current mbox is common or not
 *   @param mbox       returns mailbox of current thread
 *
 *   @return     Function returns located mailbox of tri server
 *
 *   @par Globals:
 *               tri_server_mbox_name
 */
/* ===================================================================== */
static itc_mbox_id_t find_server_mb(FILE *output, int *common_mb,
             itc_mbox_id_t *mbox)
{
    itc_mbox_id_t triServerMbox = ITC_NO_ID;

    if ((*mbox = itc_current_mbox()) == ITC_NO_ID) {
   *mbox = itc_create_mailbox(TE_CMD_MAILBOX_NAME, 0);
   *common_mb = 0;
    }

    triServerMbox = itc_locate(tri_server_mbox_name);

    if (triServerMbox == ITC_NO_ID) {
   (void)fprintf(output, "Failed to locate TRI server mailbox\n");
   if (!*common_mb)
       itc_delete_mailbox(*mbox);
    }
    return triServerMbox;
}

/* ===================================================================== */
/**
 *   This function handles the enable and disable command.
 *
 *   @param argc     Argument counter
 *
 *   @param argv     Arguments
 *
 *   @param output   Output to shell
 *
 *   @return     RET_SUCCESS if the command executed sucessfully.
 *               RET_WARNING if an error that is to be classified
 *                                    as an warning occurred.
 *               RET_ERROR   if an error has
 *   occured.
 *
 *   @par Globals:
 *               --
 */
/* ===================================================================== */
static int
handleEnableDisableCommand(int argc,
                           char **argv,
                           FILE *output)
{
   union itc_msg *sendMsg;
   union itc_msg *recMsg;
   itc_mbox_id_t mbox, triServerMbox;
   Boolean noWildCard;
   uint32_t confirmFilter[] = {1, GROUP_MASK_CFM};
   int result;
   MaskChange change;
   uint32_t groupType;
   uint32_t groupMask;
   unsigned int len;
   int i;
   int common_mb = 1;

   /*
   ** Shall we enable or disable?
   */
   if (argc < 3)
   {
      (void)fprintf(output, "Usage: %s", shortTraceAndErrorUsage);
      return RET_ERROR;
   }

   if (argv[0][0] == 'e')
   {
      change = TE_MASK_ENABLE;
   }
   else
   {
      change = TE_MASK_DISABLE;
   }
   /*
   ** Build the trace group mask to use...
   */
   groupMask = 0;
   noWildCard = False;
   for (i = 1; i < argc - 1; i++)
   {
      if (strcmp(argv[i], "all") == 0)
      {
         /*
         ** Enable or disable *all* trace groups...
         */
         groupMask = 0xFFFFFFFF;

         /*
         ** Make sure that no wild card is specified for the process
         ** name. This is to ensure that it is not to easy to enable
         ** *all* trace groups for *all* processes.
         */
         noWildCard = True;
      }
      else
      {
         groupType = convertGroupType(argv[i]);
         if (groupType == GROUP_RESERVED2)
         {
            (void)fprintf(output,
                          "Unknown trace group '%s'\n%s",
                          argv[i],
                          GROUP_SYNTAX);

            return RET_ERROR;
         }
         groupMask |= (1 << groupType);
      }
   }

   len = strlen(argv[i]);
   if (len > 0 && argv[i][len - 1] == '*' && noWildCard)
   {
      (void)fprintf(output,
                    "Not allowed to specify all when using wild card on process name\n");

      return RET_ERROR;
   }

   /*
   ** Clear signal queue for "old" confirm signals
   */
   //while ((recMsg = itc_receive(confirmFilter, ITC_NO_TMO, ITC_FROM_ALL)) != NULL)
   {
      // itc_free(&recMsg);
   }

   /*
   ** Build and send the request to the TRI server
   */
   if ((triServerMbox = find_server_mb(output, &common_mb, &mbox)) == ITC_NO_ID) {
       return RET_ERROR;
   }

   sendMsg = itc_alloc(sizeof(GroupMaskReq), GROUP_MASK_REQ);
   sendMsg->groupMaskReq.change = change;
   sendMsg->groupMaskReq.groupMask = groupMask;
   strncpy(sendMsg->groupMaskReq.procName, argv[i], TRI_MAX_PROC_NAME_LEN);
   sendMsg->groupMaskReq.procName[TRI_MAX_PROC_NAME_LEN - 1] = '\0';

   itc_send(&sendMsg, triServerMbox, ITC_MY_MBOX);

   recMsg = itc_receive(confirmFilter, CONFIRM_TMO, ITC_FROM_ALL);
   if (recMsg == NULL)
   {
      (void)fprintf(output, "No response received from the TRI server\n");

      if (!common_mb)
     itc_delete_mailbox(mbox);

      return RET_WARNING;
   }

   result = printResult(recMsg->groupMaskCfm.result, output);
   itc_free(&recMsg);

   if (!common_mb)
       itc_delete_mailbox(mbox);

   return result;
}

/* ===================================================================== */
/**
 *   This function handles the default command.
 *
 *   @param argc     Argument counter
 *
 *   @param argv     Arguments
 *
 *   @param output   Output to shell
 *
 *   @return     RET_SUCCESS if the command executed sucessfully.
 *               RET_WARNING if an error that is to be classified
 *                           as an warning occurred.
 *               RET_ERROR   if an error has
 *   occured.
 *
 *   @par Globals:
 *               --
 */
/* ===================================================================== */
static int
handleDefaultCommand(int argc,
                     char **argv,
                     FILE *output)
{
   int result;
   union itc_msg *sendMsg;
   union itc_msg *recMsg;
   itc_mbox_id_t mbox, triServerMbox;
   uint32_t confirmFilter[] = {1, SET_DEFAULT_GROUP_MASK_CFM};
   char *procName;
   int common_mb = 1;

   /*
   ** If no process name is given then we assume all running
   ** processes, i.e. we use only the wild card '*'...
   */
   procName = "*";

   if (argc != 2)
   {
      (void)fprintf(output, "Usage: %s", shortTraceAndErrorUsage);
      return RET_ERROR;
   }
   else
   {
      procName = argv[1];
   }

   /*
   ** Clear signal queue for "old" confirm signals
   */
   //while ((rec_p = receive_w_tmo(0, confirmSignal)) != NIL)
   {
      // free_buf(&rec_p);
   }

   /*
   ** Build and send request to the TRI server
   */
   if ((triServerMbox = find_server_mb(output, &common_mb, &mbox)) == ITC_NO_ID) {
       return RET_ERROR;
   }

   sendMsg = itc_alloc(sizeof(SetDefaultGroupMaskReq),
                       SET_DEFAULT_GROUP_MASK_REQ);
   strncpy(sendMsg->setDefaultGroupMaskReq.procName, procName,
           TRI_MAX_PROC_NAME_LEN);
   sendMsg->setDefaultGroupMaskReq.procName[TRI_MAX_PROC_NAME_LEN - 1] = '\0';

   itc_send(&sendMsg,triServerMbox, ITC_MY_MBOX);

   recMsg = itc_receive(confirmFilter, CONFIRM_TMO, ITC_FROM_ALL);

   if (recMsg == NULL)
   {
      (void)fprintf(output, "No response received from the TRI server\n");

      if (!common_mb)
     itc_delete_mailbox(mbox);

      return RET_WARNING;
   }

   result = printResult(recMsg->setDefaultGroupMaskCfm.result, output);

   itc_free(&recMsg);

   if (!common_mb)
       itc_delete_mailbox(mbox);

   return result;
}

/* ===================================================================== */
/**
 *   Prints the status signals received for te status
 *   commands.
 *
 *   @param statusType  Status type RUNNING or ALT RUNNING
 *
 *   @param output      Output to shell
 *
 *   @return     RET_SUCCESS if the command executed sucessfully.
 *               RET_WARNING if an error that is to be classified
 *                           as an warning occurred.
 *               RET_ERROR   if an error has
 *   occured.
 *
 *   @par Globals:
 *               --
 */
/* ===================================================================== */
static int
handleStatusSignals(FILE *output)
{
   Boolean cfmReceived;
   Boolean first;
   unsigned int i;
   unsigned int max_procname_len;
   unsigned int j;
   int result;
   union itc_msg *recMsg;
   uint32_t confirmFilter[] = {2, STATUS_CFM, STATUS_INFO_IND};

   /*
   ** Loop and receive status signals until the
   ** status confirm signal is recieved.
   */
   first = True;
   cfmReceived = False;
   while (! cfmReceived)
   {
      recMsg = itc_receive(confirmFilter, CONFIRM_TMO, ITC_FROM_ALL);

      if (recMsg == NULL)
      {
         (void)fprintf(output, "No response received from the TRI server\n");
         return RET_WARNING;
      }

      switch (recMsg->msgNo)
      {
         case STATUS_CFM:
         {
            cfmReceived = True;
            break;
         }
         case STATUS_INFO_IND:
         {
            max_procname_len = 0;
            for (i = 0; i < recMsg->statusInfoInd.noOfProc; i++)
            {
          if (strlen(recMsg->statusInfoInd.status[i].procName) > max_procname_len)
                  max_procname_len = strlen(recMsg->statusInfoInd.status[i].procName);
            }
            if (max_procname_len < 8)
       {
               max_procname_len = 8;
       }
            if (first)
       {
               (void)fprintf(output, "%8s %s","pid","name");
               for (j = 0; j < (max_procname_len + 1 - 4); j++)
                  (void)fprintf(output, " ");
               (void) fprintf(output,"%s\n","enabled groups");
               first = False;
            }
            for (i = 0; i < recMsg->statusInfoInd.noOfProc; i++)
            {
               if (recMsg->statusInfoInd.status[i].mbox != 0)
               {
                  if (recMsg->statusInfoInd.status[i].mbox != ITC_NO_ID)
                     (void)fprintf(output, "%08x",
                     recMsg->statusInfoInd.status[i].mbox);
                  else
                     (void)fprintf(output, "%8c", '-');
               }
               else
               {
                  (void)fprintf(output, "%8s", "-");
               }
               (void)fprintf(output, " %s",
                             recMsg->statusInfoInd.status[i].procName);

               for (j = 0; j < (max_procname_len + 1 -
                                strlen(recMsg->statusInfoInd.status[i].procName)); j++)
               {
                  (void)fprintf(output, " ");
               }
               printEnabledTraceGroups(output,
                                       recMsg->statusInfoInd.status[i].groupMask);
               fflush(output);
            }
            itc_free(&recMsg);
            break;
         }
         default:
         {
            (void)fprintf(output, "Unknown response %d received\n",
                          recMsg->msgNo);
            itc_free(&recMsg);
            return RET_WARNING;
         }
      }
   }

   result = printResult(recMsg->statusCfm.result, output);
   itc_free(&recMsg);
   return result;
}

/* ===================================================================== */
/**
 *   This function handles the status command.
 *
 *   @param argc     Argument counter
 *
 *   @param argv     Arguments
 *
 *   @param output   Output to shell
 *
 *   @return     RET_SUCCESS if the command executed sucessfully.
 *               RET_WARNING if an error that is to be classified
 *                           as an warning occurred.
 *               RET_ERROR   if an error has occured.
 *
 *   @par Globals:
 *               --
 */
/* ===================================================================== */
static int
handleStatusCommand(int argc,
                    char **argv,
                    FILE *output)
{
   union itc_msg *sendMsg;
   itc_mbox_id_t mbox, triServerMbox;
   int result;
   char *procName;
   int common_mb = 1;

   /*
   ** If no process name is given then we assume all running
   ** processes, i.e. we use only the wild card '*'...
   */
   procName = "*";

   if (argc > 2)
   {
      (void)fprintf(output, "Usage: %s", shortTraceAndErrorUsage);
      return RET_ERROR;
   }
   if (argc == 2)
   {
      procName = argv[1];
   }

   /*
   ** Build and send request to the global the TRI server
   */
   if ((triServerMbox = find_server_mb(output, &common_mb, &mbox)) == ITC_NO_ID) {
       return RET_ERROR;
   }
   sendMsg = itc_alloc(sizeof(StatusReq), STATUS_REQ);
   strncpy(sendMsg->statusReq.procName, procName, TRI_MAX_PROC_NAME_LEN);
   sendMsg->statusReq.procName[TRI_MAX_PROC_NAME_LEN - 1] = '\0';

   itc_send(&sendMsg, triServerMbox, ITC_MY_MBOX);

   result = handleStatusSignals(output);

   if (!common_mb)
       itc_delete_mailbox(mbox);

   return result;
}


/* ===================================================================== */
/**
 *   This function handles the save command.
 *
 *   @param argc     Argument counter
 *
 *   @param argv     Arguments
 *
 *   @param output   Output to shell
 *
 *   @return     RET_SUCCESS if the command executed sucessfully.
 *               RET_WARNING if an error that is to be classified
 *                                    as an warning occurred.
 *               RET_ERROR   if an error has occured.
 *
 *   @par Globals:
 *               --
 */
/* ===================================================================== */
static int
handleSaveCommand(int argc,
                  char **argv,
                  FILE *output)
{
   int result;
   union itc_msg *sendMsg;
   union itc_msg *recMsg;
   itc_mbox_id_t mbox, triServerMbox;
   uint32_t confirmFilter[] = {1, SAVE_GROUP_MASK_CFM};
   int common_mb = 1;

   if (argc != 2)
   {
      (void)fprintf(output, "Usage: %s", shortTraceAndErrorUsage);
      return RET_ERROR;
   }

   /*
   ** Clear message queue for "old" messages
   */
   //while ((recMsg = itc_receive(confirmFilter, 0, ITC_FROM_ALL)) != NIL)
   {
      // free_buf(&rec_p);
   }

   /*
   ** Build and send request to the TRI server ...
   */
   if ((triServerMbox = find_server_mb(output, &common_mb, &mbox)) == ITC_NO_ID) {
       return RET_ERROR;
   }

   sendMsg = itc_alloc(sizeof(SaveGroupMaskReq), SAVE_GROUP_MASK_REQ);
   strncpy(sendMsg->saveGroupMaskReq.procName, argv[1], TRI_MAX_PROC_NAME_LEN);
   sendMsg->saveGroupMaskReq.procName[TRI_MAX_PROC_NAME_LEN - 1] = '\0';
   itc_send(&sendMsg, triServerMbox, ITC_MY_MBOX);

   recMsg = itc_receive(confirmFilter, CONFIRM_TMO, ITC_FROM_ALL);
   if (recMsg == NULL)
   {
      (void)fprintf(output, "No response received from the TRI server\n");
      if (!common_mb)
      itc_delete_mailbox(mbox);
      return RET_WARNING;
   }

   result = printResult(recMsg->saveGroupMaskCfm.result, output);
   itc_free(&recMsg);
   if (!common_mb)
       itc_delete_mailbox(mbox);

   return result;
}

/* ===================================================================== */
/**
 *   Callback to ntfw. Deletes all files.
 *
 *   @return     0 if the command executed sucessfully.
 *               -1   if an error has occured.
 *
 *   @par Globals:
 *               --
 */
/* ===================================================================== */
static int myrm(const char *path, const struct stat *sb,
      int flag, struct FTW *ftwbuf)
{
   int (*rm_func)(const char *);
   int rc;

   if (ftwbuf->level == 0)
      return 0;

   switch(flag) {
   case FTW_DP:
      rm_func = rmdir;
      break;
   default:
      rm_func = unlink;
   }
   if ((rc = rm_func(path)) != 0)
      syslog(LOG_NOTICE, "Not removed: %s\n", path);
   return rc;
}

/* ===================================================================== */
/**
 *   This function handles the log read command.
 *
 *   @return     RET_SUCCESS if the command executed sucessfully.
 *               RET_ERROR   if an error has occured.
 *
 *   @par Globals:
 *               --
 */
/* ===================================================================== */
static int tri_clean_path()
{
   if (nftw(tri_telog_path, myrm, FOPEN_MAX, FTW_DEPTH) < 0)
      return RET_ERROR;
   return RET_SUCCESS;
}

/* ===================================================================== */
/**
 *   Callback to ntfw. Deletes all files.
 *
 *   @return     0 if the command executed sucessfully.
 *               -1   if an error has occured.
 *
 *   @par Globals:
 *               --
 */
/* ===================================================================== */
static int mychmod(const char *path, const struct stat *sb,
         int flag, struct FTW *ftwbuf)
{
   int rc;
   struct stat st;

   if (ftwbuf->level == 0)
      return 0;

   if (stat(path, &st) < 0)
      return -1;

   if ((rc = chmod(path, st.st_mode | S_IROTH | S_IXOTH)) < 0)
      syslog(LOG_NOTICE, "Permissions change failed: %s, err: %s\n",
             path, strerror(errno));
   return rc;
}


/* ===================================================================== */
/**
 *   This function handles the log read command.
 *
 *   @param argc     Argument counter
 *
 *   @param argv     Arguments
 *
 *   @param output   Output to shell
 *
 *   @return     RET_SUCCESS if the command executed sucessfully.
 *               RET_WARNING if an error that is to be classified
 *                                    as an warning occurred.
 *               RET_ERROR   if an error has occured.
 *
 *   @par Globals:
 *               --
 *   Command usage is : te log read
 */
/* ===================================================================== */
static int
handleLogReadCommand()
{
   char *cmd;
   const char *cmd1 = "babeltrace --clock-date -n none ";
   struct lttng_snapshot_output *lttng_out;
   int r;

   /* Remove all previous snapshots from output folder
    * This way filesystem will not be flooded by snapshots
    * taken every time user reads the log. OBS, All dirs under
    * path will be deleted.
    */
   if (tri_clean_path() == RET_ERROR)
      return RET_ERROR;

   /*
    * Take a snapshot of lttng buffers  for a given session
    */
   /* Create a output handle */
   if ((lttng_out = lttng_snapshot_output_create()) == NULL) {
      syslog(LOG_ERR, "lttng_snapshot_output_create failed");
      printLogResult(TE_LOG_RESULT_NOT_OK, 0, stderr);
      return RET_ERROR;
   }
   /* Take a snapshot, output path used is same as configured for the session */
   r = lttng_snapshot_record(tri_lttng_ses_name, lttng_out, 0);
   if (r < 0) {
      syslog(LOG_ERR, "lttng_snapshot_record failed, err: %s",
             lttng_strerror(r));
      printLogResult(TE_LOG_RESULT_NOT_OK, 0, stderr);
      return RET_ERROR;
   }
   lttng_snapshot_output_destroy(lttng_out);

   /* Read snapshot with babeltrace  */
   cmd = malloc(strlen(cmd1) + strlen(tri_telog_path) + 1);
        if (!cmd)
        {
           syslog(LOG_ERR, "TRI server cmd %s: %s (%d).",
                  __FUNCTION__, strerror(errno), errno);
           return RET_ERROR;
        }

   sprintf(cmd, "%s%s", cmd1, tri_telog_path);
   if (shell_run_cmd(cmd, NULL) != SHELL_SUCCESS) {
      free(cmd);
      return RET_ERROR;
   }
   free(cmd);

   /*
    * snapshot folder created by root lttng-sessiond and all its
    * subfolders have permissions set to 770, meaning non-privileged
    * user cant read/write from/to that folder. A regular user should
    * be able to read traces generated by root sessiond.
    * Changing permissions to this file tree.
    */
   nftw(tri_telog_path, mychmod, FOPEN_MAX, FTW_DEPTH);

   return RET_SUCCESS;
}


/* ===================================================================== */
/**
 *   Initiates global variables by reading environment
 *
 *   @return  -
 *   @par Globals:
 *               --
 */
/* ===================================================================== */
void init_env()
{
   /* see if alternative path is set */
   if ((tri_telog_path = getenv("LTTNG_TELOG_PATH")) == NULL) {
      tri_telog_path = TELOG_PATH;
   }
   /* Get session name */
   if ((tri_lttng_ses_name = getenv("LTTNG_SES_NAME")) == NULL) {
      tri_lttng_ses_name = LTTNG_SES_NAME;
   }
}


/* ===================================================================== */
/**
 *   This function handles the log clear command.
 *
 *   @param argc     Argument counter
 *
 *   @param argv     Argument
 *
 *   @param output   Output to shell
 *
 *   @return     RET_SUCCESS if the command executed sucessfully.
 *               RET_WARNING if an error that is to be classified
 *                                    as an warning occurred.
 *               RET_ERROR   if an error has occured.
 *
 *   @par Globals:
 *               --
 *   Command usage is : te log clear
 */
/* ===================================================================== */
static int
handleLogClearCommand()
{
   /* Optimal here would be to clear both existing snapshots
    * in file system and clear buffers of lttng trace session.
    * The latter is currently impossible without destroying
    * the session. For now just deleting existing snapshots
    */

   /* Remove all snapshots from output folder */
   return tri_clean_path();
}


/* ===================================================================== */
/**
 *   This function handles the log command.
 *
 *   @param argc     Argument counter
 *
 *   @param argv     Arguments
 *
 *   @param output   Output to shell
 *
 *   @return     RET_SUCCESS if the command executed sucessfully.
 *               RET_WARNING if an error that is to be classified
 *                                    as an warning occurred.
 *               RET_ERROR   if an error has occured.
 *
 *   @par Globals:
 *               --
 */
/* ===================================================================== */
static int
handleLogCommand(int argc,
                 char **argv,
                 FILE *output)
{
   if (argc > 1)
   {
      if (strcmp(argv[1], "read") == 0)
      {
         return handleLogReadCommand();
      }
      else if (strcmp(argv[1], "clear") == 0)
      {
         /** TODO **/
         return handleLogClearCommand(argc, argv, output);
      }
   }

   (void)fprintf(output, "Usage: %s", shortTraceAndErrorUsage);
   return RET_ERROR;
}


/* ===================================================================== */
/**
 *   Called whenever a trace and error handling  command is issued in the
 *   Colish shell.
 *
 *   @param argc    Argument counter
 *
 *   @param argv    Arguments
 *
 *   @return     RET_SUCCESS if the command executed sucessfully.
 *               RET_WARNING if an error that is to be classified
 *                           as an warning occurred.
 *               RET_ERROR   if an error has occured.
 *
 *   @par Globals:
 *               --
 */
/* ===================================================================== */
int
traceAndErrorCommand(int argc, char **argv)
{
   FILE *output = stdout;

   if (argc == 1)
   {
      (void)fprintf(output, "Usage: %s", shortTraceAndErrorUsage);
      return RET_ERROR;
   }

   if(strcmp(argv[1], "-p") == 0)
   {
      (void)fprintf(output,
                    "Option '-p' is obsolete. Use command lhsh instead\n");
      return RET_WARNING;
   }

   /*
   ** Skip command name...
   */
   argv += 1;
   argc -= 1;

   if (strcmp(argv[0], "help") == 0)
   {
      (void)fprintf(output, "Usage: %s", shortTraceAndErrorUsage);

      return RET_SUCCESS;
   }
   else if (strcmp(argv[0], "e") == 0 || strcmp(argv[0], "enable") == 0 ||
            strcmp(argv[0], "d") == 0 || strcmp(argv[0], "disable") == 0)
   {
      return handleEnableDisableCommand(argc, argv, output);
   }
   else if (strcmp(argv[0], "default") == 0)
   {
      return handleDefaultCommand(argc, argv, output);
   }
   else if (strcmp(argv[0], "s") == 0 || strcmp(argv[0], "status") == 0)
   {
      return handleStatusCommand(argc, argv, output);
   }
   else if (strcmp(argv[0], "save") == 0)
   {
      return handleSaveCommand(argc, argv, output);
   }
   else if (strcmp(argv[0], "log") == 0)
   {
      return handleLogCommand(argc, argv, output);
   }
   else if (strcmp(argv[0], "log") == 0)
   {
      (void)fprintf(output, "Command not supported");
      return RET_SUCCESS;
   }

   (void)fprintf(output, "Usage: %s", shortTraceAndErrorUsage);
   return RET_ERROR;
}


/* ===================================================================== */
/**
 *   This is a late system start hook that adds a shell command to the
 *    OSE Shell.
 *
 *   @param -
 *
 *   @return     -
 *
 *   @par Globals:
 *               --
 *
 *   Post-conditions:
 *     The trace and error command has been added.
 */
/* ===================================================================== */
void
addTraceAndErrorCmd(void)
{
   char *cmd;
   if ((cmd = getenv("TRI_MAIN_CMD")) == NULL) {
      cmd = MAIN_CMD;
   }

   shell_add_cmd(cmd,
            shortTraceAndErrorUsage,
            "Trace & Error Handling commands",
            traceAndErrorCommand);
}
