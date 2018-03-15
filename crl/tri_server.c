/**
 *   TRI server which handles the 'te' command.
 *
 *   Copyright (C) 2015 by Ericsson AB. All rights reserved. The
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
 *   Revised : 2013-04-22 Anette Schött
 *   Change  : First version.
 *
 *   Revised : 2013-10-09 Anette Schött
 *   Change  : Updated with list functionality provided by sys/queue.h
 *
 *   Revised : 2013-11-12 Stanislav Vovk
 *   Change  : Setting correct umask, removed some syslog errors which
 *             which are not errors
 *
 *   Revised : 2013-11-18 Adam Wujek
 *   Change  : Fix determining length of string
 *
 *   Revised : 2013-12-11 M. Winberg
 *   Change  : Added handling of item deregistration request.
 *
 *   Revised : 2014-02-04 Stanislav Vovk
 *   Change  : Save pid(mbox) of an actual thread for later usage in
 *             status request command
 *
 *   Revised : 2014-02-27 Anette Schött
 *   Change  : Added TE_CMD_RESULT_PROCESS_NOT_FOUND as reslut in statusCfm
 *             when no matching item is found.
 *
 *   Revised : 2014-09-18 K V Ranganath
 *   Change  : TRI server reads the saved group mask file and sends
 *             new group mask information to TRI daemon.
 *             Support for "te config" and "te preset" added.
 *
 *   Revised : 2014-11-11 K V Ranganath
 *   Change  : Added support for "te default -restart, te status -restart".
 *             Also fixed: Bugzilla case: 1824
 *
 *   Revised : 2014-11-24 K.V.Ranganadh
 *   Change  : Added support for filter handling
 *
 *   Revised : 2014-11-24 Anette Schött
 *   Change  : Corrected a debug print.
 *
 *   Revised : 2014-12-02 Niranjan Kumar
 *   Change  : Error traces are seen when a process which
 *             has several trace items with same name is killed.
 *             This is fixed by breaking the loop when an daemon is found
 *             rather than continuing further.
 *
 *   Revised : 2015-01-14 Nils Carlson
 *   Change  : Fix compilation warnings.
 *
 *   Revised : 2015-03-02 K.V.Ranganadh
 *   Change  : HT50015 Disabled traces were getting logged
 *
 *   Revised : 2015-06-02 Anette Schött
 *   Change  : Correction of HT50015 and removed resource leak in
 *             saveGroupMaskReq.
 *
 *   Revised : 2015-06-02 Fredrik Skog
 *   Change  : TR HT78355: Added checks for return value of all malloc
 *             calls in TRI to prevent crashes.
 *
 *   Revised : 2015-10-14 Ranganadh, Niranjan
 *   Change  : Added instance handling support in TRI.
 *
 *   Revised : 2015-11-03 Anette Schött
 *   Change  : Removed inclusion of ose.h and usage of OS_PROCESS macro,
 *             to make TRI not dependent on LITS. Changed return value
 *             from bool to int for checkForValidInstanceId. 
 * ========================================================================
 */

/* ========================================================================
 *   INCLUDE FILES
 * ========================================================================
 */

#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <unistd.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <syslog.h>
#include <errno.h>
#include <sys/queue.h>
#include <pthread.h>
#include "itc.h"
#include "tri_server.h"
#include "tri_server_cmd.h"
#include "tri_filter.h"
#ifdef LTTNG_STARTUP_TP
#define TRACEPOINT_DEFINE
#include "com_ericsson_system_start.h"
#endif

/* ========================================================================
 *   DEFINITIONS
 * ========================================================================
 */

/*
** Name of the temporary file used for 'te save' command
*/
#define TRI_SERVER_TMP_TE_SAVE_FILE_NAME "tmp_te_save.txt"

/*
** Size for allocating a STATUS_INFO_IND signal with max no of items
*/
#define STATUS_INFO_IND_SIZE(SIZE) \
    (sizeof(StatusInfoInd) - sizeof(StatusInfo) * (TE_MAX_STATUS_ITEMS - (SIZE)))

/* debug print */
#ifdef TRI_DEBUG
#define DBG printf
#else
#define DBG no_printf
static void no_printf(const char *format, ...){}
#endif

/* Maximum preset trace item count that can be saved */
#define MAX_SAVED_PRESET_COUNT           500

/* Maximum preset trace item count */
#define MAX_PRESET_COUNT                 3000

/* ========================================================================
 *   TYPE DEFINITIONS
 * ========================================================================
 */

union itc_msg
{
   uint32_t                      msgNo;
   TriServerRegisterDaemonReq    triServerRegisterDaemonReq;
   TriServerRegisterDaemonRsp    triServerRegisterDaemonRsp;
   TriServerRegisterItemReq      triServerRegisterItemReq;
   TriServerRegisterItemRsp      triServerRegisterItemRsp;
   TriServerSetMaskInd           triServerSetMaskInd;
   TriServerDaemonMonitorInd     triServerDaemonMonitorInd;
   TriServerItemKilledInd        triServerItemKilledInd;
   TriServerDeregisterItemReq    triServerDeregisterItemReq;
   TriServerDeregisterItemRsp    triServerDeregisterItemRsp;
   TriServerUpdateFilterInd      triServerUpdateFilterInd;
   GroupMaskReq                  groupMaskReq;
   GroupMaskCfm                  groupMaskCfm;
   SetDefaultGroupMaskReq        setDefaultGroupMaskReq;
   SetDefaultGroupMaskCfm        setDefaultGroupMaskCfm;
   StatusReq                     statusReq;
   StatusCfm                     statusCfm;
   StatusInfoInd                 statusInfoInd;
   SaveGroupMaskReq              saveGroupMaskReq;
   SaveGroupMaskCfm              saveGroupMaskCfm;
   FilterSetReq                  filterSetReq;
   FilterSetRsp                  filterSetRsp;
   PresetGroupMaskReq            presetGroupMaskReq;
   PresetGroupMaskCfm            presetGroupMaskCfm;
};

/*
 * Information for a daemon
 */
struct daemonListElement
{
   /* List head */
   TAILQ_ENTRY(daemonListElement)  list;

   itc_mbox_id_t                   daemonMbox;

   /* Registered process pid(mbox) */
   itc_mbox_id_t       pid;
};

TAILQ_HEAD(daemonListHead, daemonListElement);

/*
 * Information for a trace item (name, group mask ... )
 */
struct traceItemListElement
{
   /* List head of trace items */
   TAILQ_ENTRY(traceItemListElement)             list;

   /* List head of daemon mboxes for a specific trace item */
   struct daemonListHead                         daemonList;

   TriServerItemInfo                             itemInfo;
};


/* ========================================================================
 *   DATA DECLARATIONS
 * ========================================================================
 */

/*
 * TRI server mailbox
 */
static itc_mbox_id_t myBox;

char tri_server_mbox_name[ITC_NAME_MAXLEN] = {0};

/*
 * List to store all trace items using TRI interface
 */
TAILQ_HEAD(traceItemListHead, traceItemListElement);
struct traceItemListHead traceItemList;

/*
 * List of the saved (by te save command) trace item names
 * and their group mask
 */
TAILQ_HEAD(savedGroupMaskListHead, savedItemListElement);
struct savedGroupMaskListHead savedGroupMaskList;


/*
 * Full path and name of file to read and save group masks
 */
static char *teSaveFile;

/*
 * Counter which holds the current preset count
 */
static int totalPresetCount = 0;
/*
 * Counter which holds the current preset count which are saved
 */
static int totalSavedPresetCount = 0;
/*
 * Flag to represent that saved preset count has reached its limit
 */
static bool saveOverFlow = false;

/* ========================================================================
 *   FUNCTION PROTOTYPES
 * ========================================================================
 */

static TeCmdResultType saveGroupMaskReq(char *name);

#ifdef TE_CMD
/* initiate environment */
extern void init_env();
#endif

/* ===================================================================== */
/**
 *   Checks for digits in the string from index 0 to len-1
 *
 *   @param str      Input string.
 *   @param len      Length of input string.
 *
 *   @return
 *            1 if the string has digits from index 0 till len-1
 *            0 if the string has non digits from index 0 till len-1
 *
 *   @par Globals:
 */
/* ===================================================================== */
static int
checkForValidInstanceId(const char *str,
                        int len)
{
   int i;
   /* check for valid instance id */
   /* Return '1' if all the bytes are digits (0 to 9) */
   /* otherwise return 0 */

   for (i = 0; i < len; i++)
   {
      /* ASCII Value of 0 is 48 and 9 is 57 */
      if ( (str[i] < 48) || (str[i] > 57) )
      {
         /* Non digit found*/
         return 0;
      }
   }
   return 1;
}

/* ===================================================================== */
/**
 *   Compares two strings upto a given length excluding instance id.
 *
 *   @param s1       Compare string 1.
 *   @param s2       Compare string 2.
 *   @param s1len    Length of s1.
 *
 *   @return    0 if strings doesn't match,
 *              1 on successful match.
 *
 *   @par Globals:
 */
/* ===================================================================== */
static int
strMatch(const char *s1,
         char *s2,
         int s1len)
{
   int i, j;
   int s2len;
   int result = 1;

   /* Search if process name entered (s1) is found in the trace entries (s2)*/
   /* as a direct string */
   if (strncmp(s1, s2, s1len) != 0)
   {
         result = 0;
   }
   else
   {
         return result;
   }
   

   /* If not found as a direct string, Try to search if the processname */
   /* entered (s1) is found after '/' in trace entries (s2) */
   if (result == 0)
   {
      s2len = strlen(s2);

      for (i = 0 ;i < s2len; i++)
      {
         /* Theoretical value of instance id can be 10 digits */
         /* No Need to check for '/' after 10 digits */
         if (i == 11)
         {
            return 0;
         }
         if (s2[i] == '/' )
         {
            for (j = 0; j < s1len; j++)
            {
               if(s1[j] != s2[j + i + 1])
               {
                  return 0;
               }
            }

            /* Entered Process name is found after '/' */
            /* Now check for valid instance id */
            if (checkForValidInstanceId(s2,i))
            {
               result = 1;
               break; /* process name found, break the for loop */
            }
            else
            {
               /* Valid instance id not found*/
               return 0;
            }
         }
      }
   }
   return result;
}


/* ===================================================================== */
/**
 *   This function reads the stored group masks save with 'te save'
 *   command. The read entries are added to the trace item refernce list.
 *
 *   @param -
 *
 *   @return    -
 *
 *   @par Globals:
 *               savedGroupMaskList
 *               saveOverFlow
 *               totalSavedPresetCount
 *               totalPresetCount
 */
/* ===================================================================== */
static void
readSavedGroupMasks(char *fileName)
{
   FILE                 *fh;
   char                  nameStr[TRI_MAX_PROC_NAME_LEN];
   unsigned int          groupMask;
   struct savedItemListElement *elem = NULL;
   int num;


   saveOverFlow = false;
   totalSavedPresetCount = 0;
   if ((fh = fopen(fileName, "r")) == NULL)
   {
      if (errno != ENOENT) {
         /* file exists but couldn't be opened */
         syslog(LOG_ERR, "TRI local thread failed to open %s to read "
                "saved trace groups, %s(%d)\n", fileName, strerror(errno), errno);
      }
      return;
   }

   /* Read one trace item entry from file */
   while((num = fscanf(fh, "%x %s", &groupMask, nameStr)) > 0)
   {
      elem =
         (struct savedItemListElement*)malloc(sizeof(struct savedItemListElement));
      if (!elem)
      {
         syslog(LOG_ERR, "TRI server %s: %s (%d).",
                __FUNCTION__, strerror(errno), errno);
         break;
      }
      strncpy(elem->itemInfo.procName, nameStr, TRI_MAX_PROC_NAME_LEN);
      elem->itemInfo.procName[TRI_MAX_PROC_NAME_LEN - 1] = '\0';
      elem->itemInfo.groupMask = groupMask;
      elem->itemInfo.saved = true;
      totalSavedPresetCount ++;
      totalPresetCount ++;


      /* Add the trace item to the trace item list */
      TAILQ_INSERT_TAIL(&savedGroupMaskList, elem, list);
      if(totalSavedPresetCount == MAX_SAVED_PRESET_COUNT)
      {
         saveOverFlow = true;
         break;
      }
   }

   fclose(fh);
}


/* ===================================================================== */
/**
 *   Calculates the length of a string with or without Wildcard '*'.
 *
 *   @param *string  Pointer to the string to handle
 *
 *   @return    Prefix length as described above. e.g.,
 *              getlength("foo") = 4
 *              getlength("fo*") = 2
 *              getlength("*")   = 0
 *
 *   @par Globals:
 *               --
 *
 *   A string with wildcard does not include the
 *   string terminator '\0' in the length.
 *   A string without wildcard returns length
 *   including string terminator.
 *   Wildcard must only be used at the end of
 *   a string.
 */
/* ===================================================================== */
static int
getlength(const char *string)
{
   int i;


   i=strlen(string);


   if(i > TRI_MAX_PROC_NAME_LEN)
   {
      syslog(LOG_ERR, "TRI server receives too long item name %s, "
             "lenght %d is now cut to max length %d\n",
             string, i, TRI_MAX_PROC_NAME_LEN);
      i = TRI_MAX_PROC_NAME_LEN;
   }
   else if (string[i-1] == '*')
   {
      i--;
   }
   else
   {
      /* Include nullchar */
      i++;
   }
   return i;
}


/* ===================================================================== */
/**
 *   This function removes a preset from the preset list.
 *
 *   @param name       Trace item name.
 *   @param saveFlag   true, remove trace items whose group mask survives
 *                           a restart(Presets added because of te config
 *                           & te save commands)
 *                     false,remove trace items whose group mask wont
 *                           survive a restart(Presets added because of
 *                           te preset command).
 *
 *   @return           -
 *
 *   @par Globals:
 *                     totalSavedPresetCount
 *                     totalPresetCount
 */
/* ===================================================================== */
static void
removePreset(char *name,
             bool saveFlag)
{
   struct savedItemListElement *saveItem;
   struct savedItemListElement *elem;
   int length;


   length = getlength(name);

   /*
    * Walk through the list of presets and try to locate an already
    * existing preset with the same name as the one about to be removed.
    */
   for (saveItem = TAILQ_FIRST(&savedGroupMaskList);
        saveItem != NULL ;
        saveItem = elem)
   {
       elem = TAILQ_NEXT(saveItem, list);
       if  ((strncmp(name, saveItem->itemInfo.procName, length) == 0) &&
            (saveItem->itemInfo.saved == saveFlag))
       {
          if (saveItem->itemInfo.saved)
          {
             /* The Requested element is a saved Preset
              */
              totalSavedPresetCount --;
              saveOverFlow = false;
          }
          TAILQ_REMOVE(&savedGroupMaskList, saveItem, list);
          free(saveItem);
          totalPresetCount --;
      }
   }
}


/* ===================================================================== */
/**
 *   This function adds a new preset to the preset list.
 *   The preset it always placed first in the preset list.
 *   If a preset with the same name already exist then it
 *   is moved first.
 *
 *   @param name       Trace item name.
 *   @param groupMask  Group mask.
 *   @param saved      true, trace item(s) is to be saved.
 *                     false, trace item(s) not to be saved.
 *
 *   @return           -
 *
 *   @par Globals:
 *                     savedGroupMaskList
 *                     totalSavedPresetCount
 *                     totalPresetCount
 */
/* ===================================================================== */
static void
addPreset(char *name,
          uint32_t groupMask,
          bool saved)
{
   struct savedItemListElement *saveItem;
   struct savedItemListElement *elem;
   bool elemFound = false;

   /*
   ** Walk through the list of presets and try to locate an already
   ** existing preset with the same name as the one about to be added.
   */

   TAILQ_FOREACH(saveItem, &savedGroupMaskList, list)
   {

      if (!strcmp(saveItem->itemInfo.procName,
                  name))
      {
         elemFound = true;
         break;
      }
   }

   if (groupMask == TRI_DEFAULT_GROUP_MASK)
   {
      if (elemFound)
      {
         removePreset(name, saved);
      }

      /*
       ** ...and then just skip this preset!
       */
      return;
   }

   if (elemFound)
   {
      /*
       * A preset for this process name already existed. Move this preset
       * first in the list. This is to assure that the latest performed
       * command that updates the preset list is always the command that
       * is applicable. This is especially important with the respect to
       * commands where the process names has been specified with a wild
       * card, in contrast to complete process names without wild card.
       */

      if ((TAILQ_PREV(saveItem, savedGroupMaskListHead, list) != NULL))
      {
         /*
          * This is not the first preset, but it is present some where
          * in preset list. Move it to the first position in the list
          */
         elem = (struct savedItemListElement*)malloc(sizeof(struct savedItemListElement));
         if (!elem)
         {
            syslog(LOG_ERR, "TRI server %s: %s (%d).",
                   __FUNCTION__, strerror(errno), errno);
            return;
         }
         elem->itemInfo.groupMask = groupMask;
         elem->itemInfo.saved = saved;
         strcpy(elem->itemInfo.procName, name);
         TAILQ_INSERT_HEAD(&savedGroupMaskList, elem, list);
         TAILQ_REMOVE(&savedGroupMaskList, saveItem, list);
         free(saveItem);
         saveItem = TAILQ_FIRST(&savedGroupMaskList);
      }
      else
      {
         saveItem->itemInfo.groupMask = groupMask;
      }

      /*
       * Check if this preset is change to or from a saved one and increase
       * or decrease the saved presets counter accordingly if needed...
       */
      if (!saved && saveItem->itemInfo.saved)
      {
        /*
         * Previously this Preset group was saved and now it was requested to change to
         * a non saved preset item.This use case usally appears when te config is given
         * on a process and then te preset was again applied on the same process.
         */

           saved = true;
      }
      else if (saved && ! saveItem->itemInfo.saved)
      {
         /*
          * Previousy this was a non saved Preset item. Now it was requested to change to
          * a saved preset item. This case happens usually when te preset is applied on
          * a process and then te config/save is applied on the same process. But we dont
          * change if we already maximum limit for saving presets.
          */
         if (totalSavedPresetCount < MAX_SAVED_PRESET_COUNT)
         {
            totalSavedPresetCount ++;
         }
         else
         {
            saved = 0;
            saveOverFlow = true;
         }
      }

      /*
       * Update the save flag of that preset
       */
      saveItem->itemInfo.saved = saved;
   }
   else if(!elemFound && (totalPresetCount < MAX_PRESET_COUNT))
   {
      /*
       * Allocate and initiate a new preset entry in the preset list
       */
      saveItem = (struct savedItemListElement*)malloc(sizeof(struct savedItemListElement));
      if (!saveItem)
      {
         syslog(LOG_ERR, "TRI server %s: %s (%d).",
                __FUNCTION__, strerror(errno), errno);
         return;
      }
      saveItem ->itemInfo.groupMask = groupMask;
      strcpy(saveItem ->itemInfo.procName,name);
      saveItem ->itemInfo.saved = false; /* Initialising with Default value */

      /*
       * Check if this preset is a saved one and increase
       * the saved presets counter if needed...
       */
      if (saved)
      {
         /* This preset was requested to be saved. Check if we reached limit
          */
         if (totalSavedPresetCount < MAX_SAVED_PRESET_COUNT)
         {
            saveItem ->itemInfo.saved = true;
            totalSavedPresetCount ++;
         }
         else
         {
            /* Since saved Preset count limit was already reached we will not save the
             * preset but just adds the preset
             */
            saveItem ->itemInfo.saved = false;
            saveOverFlow = true;
         }
      }
      /* Increment total preset count as we are requested for a new preset.
       */
      totalPresetCount ++;

      /*
       * Extend the preset list with this new entry
       */

      if (TAILQ_EMPTY(&savedGroupMaskList))
      {
         /*
          * This is the first entry to be put in the preset list
          */
         TAILQ_INSERT_TAIL(&savedGroupMaskList, saveItem, list);
      }
      else
      {
         /*
          * There already exist another preset first in the list.
          * Put this new one before that first preset...
          */
         TAILQ_INSERT_HEAD(&savedGroupMaskList, saveItem, list);
      }
   }

}


/* ===================================================================== */
/**
 *   This function sends the bus filter data to the daemons.
 *
 *   recMsg      Pointer to signal to handle.
 *
 *   @return     TE_CMD_RESULT_PROCESS_NOT_FOUND
 *                - if process requested is not found.
 *               TE_CMD_RESULT_OK
 *                - if succesfully updated atleast one.
 *
 *   @par Globals:
 *                traceItemList
 */
/* ===================================================================== */
static uint32_t
updateBusFilter(union itc_msg *recMsg)
{
   union itc_msg *sendMsg;
   struct traceItemListElement *elem = NULL;
   struct daemonListElement *elem1 = NULL;
   int length;
   int count = 0;
   int temp[MAX_FILTER_LEN];
   int len = 0;


   length = getlength(recMsg->filterSetReq.procName);
   /*

    * Update the trace item list with this filter request and send
    * update filter to corresponding daemons.
    */
   if (recMsg->filterSetReq.type == 0)
   {
      if (!compileFilter(recMsg->filterSetReq.filterExpr, &temp[0]))
      {
        return TE_CMD_FILTER_UNSUPPORTED;
      }
   }
   else
   {
      temp[0] = 0;;
   }


   TAILQ_FOREACH(elem, &traceItemList, list)
   {
      /* Sending filter data to all the daemons
       */
      if ((strMatch(recMsg->filterSetReq.procName,
                   elem->itemInfo.procName,
                   length)))
      {
         TAILQ_FOREACH(elem1, &(elem->daemonList), list)
         {
            count ++;
            sendMsg = itc_alloc(sizeof(TriServerUpdateFilterInd),
                                TRI_SERVER_UPDATE_FILTER_IND);

            len = sizeof(sendMsg->triServerUpdateFilterInd.itemName);
            snprintf(sendMsg->triServerUpdateFilterInd.itemName, len,
                     "%s", elem->itemInfo.procName);

            memcpy((char *)sendMsg->triServerUpdateFilterInd.compiledFilter,
                   (char *)temp,
                   MAX_FILTER_LEN * sizeof(int));
            itc_send(&sendMsg, elem1->daemonMbox, ITC_MY_MBOX);
         }
      }
   }

   if (count == 0)
   {
      return TE_CMD_RESULT_PROCESS_NOT_FOUND;
   }
   else
   {
      return TE_CMD_RESULT_OK;
   }
}


/* ===================================================================== */
/**
 *   Handles the TRI_SERVER_REGISTER_DAEMON_REQ.
 *
 *   @param recMsg  Pointer to signal to handle
 *
 *   @return    -
 *
 *   @par Globals:
 *               traceItemList
 */
/* ===================================================================== */
void
handleRegisterDaemonReq(union itc_msg *recMsg)
{
   union itc_msg        *sendMsg, *monitorMsg;
   itc_mbox_id_t        requesterMbox = itc_sender(recMsg);


   /* Attach to the daemon */
   monitorMsg = itc_alloc(sizeof(TriServerDaemonMonitorInd),
                          TRI_SERVER_DAEMON_MONITOR_IND);
   monitorMsg->triServerDaemonMonitorInd.daemonMbox = requesterMbox;
   (void)itc_monitor(requesterMbox, &monitorMsg);

   sendMsg = itc_alloc(sizeof(TriServerRegisterDaemonRsp),
                       TRI_SERVER_REGISTER_DAEMON_RSP);

   sendMsg->triServerRegisterDaemonRsp.daemonMbox = requesterMbox;
   sendMsg->triServerRegisterDaemonRsp.result = 0;

   itc_send(&sendMsg, requesterMbox, ITC_MY_MBOX);
}

/* ===================================================================== */
/**
 *   Handles the TRI_SERVER_DEREGISTER_ITEM_REQ.
 *
 *   @param recMsg  Pointer to signal to handle
 *
 *   @return    -
 *
 *   @par Globals:
 *               traceItemList
 */
/* ===================================================================== */
void
handleDeregisterItemReq(union itc_msg *recMsg)
{
   union itc_msg               *sendMsg;
   struct traceItemListElement *elem = NULL;
   struct traceItemListElement *item;
   struct daemonListElement    *daemonElem;
   struct daemonListElement    *foundDaemonElem;
   char                        *name;
   uint32_t                    result;

  name = recMsg->triServerDeregisterItemReq.itemInfo.procName;

   /* Check if the item name exists in the trace item list */
   item = NULL;
   TAILQ_FOREACH(elem, &traceItemList, list)

   {
      if (!strcmp(elem->itemInfo.procName, name))
      {
         item = elem;
         break;
      }
   }


   /* if found check if daemon is found in the item's daemon list */
   if (item == NULL)
   {
      DBG("TRI server could not deregister %s for daemon 0x%x %s",
          name, itc_sender(recMsg),
          " - was not found in trace item list\n");
      result = 1;

   }
   else
   {
      foundDaemonElem = NULL;
      TAILQ_FOREACH(daemonElem, &(item->daemonList), list)
      {
         if((daemonElem->daemonMbox) ==  (itc_sender(recMsg)))
         {
            foundDaemonElem = daemonElem;
            break;
         }
      }

      if (foundDaemonElem == NULL)
      {
         DBG("TRI server could not deregister %s 0x%x for daemon 0x%x %s",
              item->itemInfo.procName, item->itemInfo.groupMask, itc_sender(recMsg),
              " - as daemon was not found in trace item daemon list\n");

         result = 1;
      }
      else
      {
         /* remove the found daemon element from the item's daemon list */
         TAILQ_REMOVE(&(item->daemonList), foundDaemonElem, list);
         DBG("TRI server removed daemon 0x%x from item %s 0x%x daemon list\n",
             itc_sender(recMsg), item->itemInfo.procName, item->itemInfo.groupMask);

         free(foundDaemonElem);
         /* if item's daemon list now is empty, remove the item from item list also */
         if (TAILQ_EMPTY(&(item->daemonList)))
         {
            TAILQ_REMOVE(&traceItemList, item, list);
            DBG("TRI server removed item %s 0x%x from trace item list\n",
                 item->itemInfo.procName, item->itemInfo.groupMask);
            free(item);
         }
         result = 0;
      }

   }

   /* Confirm to the TRI proxy */
   sendMsg = itc_alloc(sizeof(TriServerDeregisterItemRsp),
                       TRI_SERVER_DEREGISTER_ITEM_RSP);
   sendMsg->triServerDeregisterItemRsp.result = result;
   strcpy(sendMsg->triServerDeregisterItemRsp.itemName,
          recMsg->triServerDeregisterItemReq.itemInfo.procName);

   itc_send(&sendMsg, itc_sender(recMsg), ITC_MY_MBOX);
}

/* ===================================================================== */
/**
 *   Handles the TRI_SERVER_REGISTER_ITEM_REQ.
 *
 *   @param recMsg  Pointer to signal to handle
 *
 *   @return    -
 *
 *   @par Globals:
 *               traceItemList
 */
/* ===================================================================== */
void
handleRegisterItemReq(union itc_msg *recMsg)
{
   union itc_msg               *sendMsg;
   struct traceItemListElement *elem = NULL;
   struct traceItemListElement *item;
   struct savedItemListElement *saveItem;
   struct daemonListElement    *daemonElem;
   char                        *name;
   uint32_t                    groupMask;
   itc_mbox_id_t               pid;
   int                         length = 0;
   bool                        savedTraceItemFound = false;


   name = recMsg->triServerRegisterItemReq.itemInfo.procName;
   groupMask = recMsg->triServerRegisterItemReq.itemInfo.groupMask;
   pid = recMsg->triServerRegisterItemReq.pid;

   /* Check if the item name already exists in the trace item list */
   item = NULL;
   TAILQ_FOREACH(elem, &traceItemList, list)
   {
      if (!strcmp(elem->itemInfo.procName, name))
      {
         item = elem;
         break;
      }
   }

   /*
    * Check if the trace item already exists in the list of saved
    * group mask list, if found, update the group mask.
    */
   TAILQ_FOREACH(saveItem, &savedGroupMaskList, list)
   {
      length = getlength(saveItem->itemInfo.procName);
      if (strMatch(saveItem->itemInfo.procName,
                   name,
                   length))
      {
         savedTraceItemFound = true;
         groupMask = saveItem->itemInfo.groupMask;
         break;
      }
   }

   /*
    * Trace item does not exist, allocate a new trace item
    * and add it to the list
    */
   if (!item)
   {
      item = (struct traceItemListElement*)malloc(sizeof(struct traceItemListElement));
      if (!item)
      {
         syslog(LOG_ERR, "TRI server %s: %s (%d).",
                __FUNCTION__, strerror(errno), errno);
         return;
      }
      /* Copy the trace item struct to the traceItemList */
      strncpy(item->itemInfo.procName, name, TRI_MAX_PROC_NAME_LEN);
      item->itemInfo.procName[TRI_MAX_PROC_NAME_LEN - 1] = '\0';
      item->itemInfo.groupMask = groupMask;

      /* Add the trace item to the trace item list */
      TAILQ_INSERT_TAIL(&traceItemList, item, list);

      /* Create and initialize the TRI daemon list for this new trace item */
      TAILQ_INIT(&(item->daemonList));
   }

   /* Add the daemon to the daemon list for this trace item */
   daemonElem = (struct daemonListElement*)malloc(sizeof(struct daemonListElement));
   if (!daemonElem)
   {
      syslog(LOG_ERR, "TRI server %s: %s (%d).",
             __FUNCTION__, strerror(errno), errno);
      free(item);
      return;
   }
   daemonElem->daemonMbox = itc_sender(recMsg);
   daemonElem->pid = pid;
   TAILQ_INSERT_TAIL(&(item->daemonList), daemonElem, list);

   DBG("TRI server register %s 0x%x for daemon 0x%x\n",
       item->itemInfo.procName, item->itemInfo.groupMask, daemonElem->daemonMbox);

   /* Confirm to the TRI proxy */
   sendMsg = itc_alloc(sizeof(TriServerRegisterItemRsp),
                       TRI_SERVER_REGISTER_ITEM_RSP);
   sendMsg->triServerRegisterItemRsp.result = 0;
   strcpy(sendMsg->triServerRegisterItemRsp.itemName,
          recMsg->triServerRegisterItemReq.itemInfo.procName);
   itc_send(&sendMsg, itc_sender(recMsg), ITC_MY_MBOX);

  /*
   * Send a message to the daemon to update group mask of
   * the newly registered item
   */
   if (savedTraceItemFound)
   {
      sendMsg = itc_alloc(sizeof(TriServerSetMaskInd),
                          TRI_SERVER_SET_MASK_IND);
      strncpy(sendMsg->triServerSetMaskInd.itemInfo.procName,
              name,
              TRI_MAX_PROC_NAME_LEN);
      sendMsg->triServerSetMaskInd.itemInfo.procName[TRI_MAX_PROC_NAME_LEN - 1] = '\0';
      sendMsg->triServerSetMaskInd.itemInfo.groupMask = groupMask;
      itc_send(&sendMsg, itc_sender(recMsg), ITC_MY_MBOX);
   }

}


/* ===================================================================== */
/**
 *   This function handles a monitor indication for a daemon which
 *   has died.
 *
 *   @param recMsg   Pointer to the message to handle
 *
 *   @return    -
 *
 *   @par Globals:
 *               traceItemList
 */
/* ===================================================================== */
static void
handleDaemonMonitorInd(union itc_msg *msg)
{
   struct traceItemListElement  *elem;
   struct daemonListElement     *elem1;
   struct traceItemListElement  *tmp_elem;
   struct daemonListElement     *tmp_elem1;


   DBG("TRI server receive monitor ind for daemon 0x%x\n",
       msg->triServerDaemonMonitorInd.daemonMbox);

   /*
    * Search for the daemon and release the daemon elements from list
    * as this daemon has died
    */
   for (elem = TAILQ_FIRST(&traceItemList); elem != NULL; elem = tmp_elem) {
      tmp_elem = TAILQ_NEXT(elem, list);

      for (elem1 = TAILQ_FIRST(&(elem->daemonList)); elem1 != NULL; elem1 = tmp_elem1) {
         tmp_elem1 = TAILQ_NEXT(elem1, list);

         /*
          * Remove the daemon from the daemon list of this
          * trace item. A trace item can be registered by several
          * daemons. If this is the only daemon for the
          * trace item, remove the trace item as well
          */
         if (elem1->daemonMbox == msg->triServerDaemonMonitorInd.daemonMbox)
         {
            DBG("TRI server remove daemon 0x%x\n", elem1->daemonMbox);
            TAILQ_REMOVE(&(elem->daemonList), elem1, list);
            free(elem1);

            /*
             * Remove the trace item if no more daemons have this item
             * registered
             */
            if (TAILQ_EMPTY(&(elem->daemonList)))
            {
               DBG("TRI server remove item 0x%x\n", elem->itemInfo.procName);
               TAILQ_REMOVE(&traceItemList, elem, list);
               free(elem);
            }
         }
      }
   }
}


/* ===================================================================== */
/**
 *   This function handles a kill indication for a trace item. It removes
 *   the daemon mbox from the list and if no more daemon mboxes exist
 *   for the item, the item is removed as well.
 *
 *   @param recMsg   Pointer to the message to handle
 *
 *   @return    -
 *
 *   @par Globals:
 *               traceItemList
 */
/* ===================================================================== */
static void
handleItemKilledInd(union itc_msg *msg)
{
   struct traceItemListElement  *elem;
   struct traceItemListElement  *item = NULL;
   struct daemonListElement     *elem1;
   struct daemonListElement     *tmp_elem1;
   char                         *name;
   itc_mbox_id_t                daemonMbox;


   name = msg->triServerItemKilledInd.itemName;
   daemonMbox = msg->triServerItemKilledInd.daemonMbox;

   DBG("TRI server receive monitor ind for item %s and daemon 0x%x\n", name, daemonMbox);

   /*
      Release the daemon element from list as this item is no
    * longer registered to the daemon
    */
   TAILQ_FOREACH(elem, &traceItemList, list)
   {
      if (strcmp(elem->itemInfo.procName, name) == 0)
      {
         item = elem;
         break;
      }
   }

   if(item)
   {
      for (elem1 = TAILQ_FIRST(&(elem->daemonList)); elem1 != NULL; elem1 = tmp_elem1) {
         tmp_elem1 = TAILQ_NEXT(elem1, list);

         /*
          * Remove the daemon from the daemon list of this
          * trace item. A trace item can be registered by several
          * daemons. If this is the only daemon for the
          * trace item, remove the trace item as well
          */
         if (elem1->daemonMbox == daemonMbox)
         {
            DBG("TRI server remove daemon 0x%x for %s\n",
                daemonMbox, name);
            TAILQ_REMOVE(&(elem->daemonList), elem1, list);
            free(elem1);

            /*
             * Remove the trace item if no more daemons have this item
             * registered
             */
            if (TAILQ_EMPTY(&(elem->daemonList)))
            {
               DBG("TRI server remove %s\n", name);
               TAILQ_REMOVE(&traceItemList, elem, list);
               free(elem);
            }
            /* There can be several trace items with same process name
             * in one process. This means there will be several daemonList
             * members for a trace item with same daemonMbox id. Continuing
             * further will remove remaining deamonList members though item is
             * still alive. Hence break the loop.
             */
            break;
         }
      }
   }
   else
   {
      syslog(LOG_ERR, "TRI server received kill indication from "
             "daemon 0x%x for unknown item %s", daemonMbox, name);
   }

}

/* ===================================================================== */
/**
 *   Searches for all entries whose matches 'procName' field from signal.
 *
 *   @param recMsg  Pointer to signal to handle
 *
 *   @return    1 on a matching entry being found in the database
 *              0 otherwise.
 *
 *   @par Globals:
 *               --
 *
 *   This function searches the trace item list for all entries whose
 *   'procName' field matches the 'procName' field of the input signal.
 *   All matching entries have their 'groupMask' field updated in
 *   accordance to the 'change' and 'groupMask' fields of the input
 *   signal and a signal is sent to TRI daemon in each application
 *   where the 'procNmae' exists.
 */
/* ===================================================================== */
static int
handleNewGroupMaskReq(union itc_msg *recMsg)
{
   int                         count = 0;
   int                         wildcardLen, fullLen;
   int                         retval;
   union itc_msg               *sendMsg;
   struct traceItemListElement *elem;
   struct daemonListElement    *elem1;
   itc_mbox_id_t               requesterMbox = itc_sender(recMsg);
   char                        *name;
   uint32_t                    groupMask;


   name = recMsg->groupMaskReq.procName;
   groupMask = recMsg->groupMaskReq.groupMask;
   wildcardLen = getlength(name);

   TAILQ_FOREACH(elem, &traceItemList, list)
   {
      if (strMatch(name, elem->itemInfo.procName, wildcardLen))
      {
         count++;
         switch(recMsg->groupMaskReq.change)
         {
            case TE_MASK_SET:
            {
               elem->itemInfo.groupMask = groupMask;
               break;
            }
            case TE_MASK_ENABLE:
            {
               elem->itemInfo.groupMask |= groupMask;
               break;
            }
            case TE_MASK_DISABLE:
            {
               elem->itemInfo.groupMask &= ~groupMask;
               break;
            }
            default:
            {
               syslog(LOG_ERR, "TRI server received unknown trace group "
                      "mask change: %d", recMsg->groupMaskReq.change);
               return 0;
            }
         }

         DBG("TRI server new mask %s 0x%x\n",
             elem->itemInfo.procName, elem->itemInfo.groupMask);

         /*
          * Send the new group mask to all daemons where the
          * thread name exist
          */
         TAILQ_FOREACH(elem1, &(elem->daemonList), list)
         {
           sendMsg = itc_alloc(sizeof(TriServerSetMaskInd),
                                TRI_SERVER_SET_MASK_IND);
           fullLen = sizeof(sendMsg->triServerSetMaskInd.itemInfo.procName);
           strncpy(sendMsg->triServerSetMaskInd.itemInfo.procName,
                   elem->itemInfo.procName,
                   fullLen);
            sendMsg->triServerSetMaskInd.itemInfo.procName[fullLen - 1] = '\0';
            sendMsg->triServerSetMaskInd.itemInfo.groupMask = elem->itemInfo.groupMask;

            DBG("TRI server sends new mask %s 0x%x to daemon=0x%x\n",
                sendMsg->triServerSetMaskInd.itemInfo.procName,
                sendMsg->triServerSetMaskInd.itemInfo.groupMask,
                elem1->daemonMbox);

            itc_send(&sendMsg, elem1->daemonMbox, ITC_MY_MBOX);
         }
      }

   }

   if (count)
   {
      sendMsg = itc_alloc(sizeof(GroupMaskCfm), GROUP_MASK_CFM);
      sendMsg->groupMaskCfm.result = TE_CMD_RESULT_OK;
      DBG("TRI server sends comfirm to requester=0x%x\n",
          requesterMbox);
      itc_send(&sendMsg, requesterMbox, ITC_MY_MBOX);
      retval = 1;
   }
   else
   {
      retval = 0;
   }
   return retval;
}

/* ===================================================================== */
/**
 *   Store the specified preset trace group request in the preset list.
 *   If save option is provided, save the trace group request to a file.
 *
 *   @param recMsg  Pointer to signal to handle
 *
 *   @return     --
 *
 *   @par Globals:
 *               --
 */
/* ===================================================================== */
static void
handlePresetGroupMaskReq(union itc_msg *recMsg)
{
   uint32_t groupMask;
   union itc_msg *sendMsg;
   TeCmdResultType result;
   struct traceItemListElement *elem = NULL;
   struct daemonListElement *elem1 = NULL;
   int length;


   groupMask = recMsg->presetGroupMaskReq.groupMask;

   addPreset(recMsg->presetGroupMaskReq.procName,
             groupMask,
             recMsg->presetGroupMaskReq.savePreset);

   length = getlength(recMsg->presetGroupMaskReq.procName);

   /*
    * This preset has to be saved...
    */
   if (recMsg->presetGroupMaskReq.savePreset)
   {
      result = saveGroupMaskReq(recMsg->presetGroupMaskReq.procName);
   }
   else
   {
      result = TE_CMD_RESULT_OK;
   }

   /*
    * Update the trace item list with this mask and send update mask request
    * to corresponding daemons.
    */
   TAILQ_FOREACH(elem, &traceItemList, list)
   {
      if ((strMatch(recMsg->presetGroupMaskReq.procName,
           elem->itemInfo.procName,
           length)))
      {
         elem->itemInfo.groupMask = groupMask;

         TAILQ_FOREACH(elem1, &(elem->daemonList), list)
         {
            sendMsg = itc_alloc(sizeof(TriServerSetMaskInd),
                                TRI_SERVER_SET_MASK_IND);
            strncpy(sendMsg->triServerSetMaskInd.itemInfo.procName,
                    recMsg->presetGroupMaskReq.procName,
                    length);
            sendMsg->triServerSetMaskInd.itemInfo.procName[length] = '\0';
            sendMsg->triServerSetMaskInd.itemInfo.groupMask =
               elem->itemInfo.groupMask;
            itc_send(&sendMsg, elem1->daemonMbox, ITC_MY_MBOX);
         }
      }
   }

   if (saveOverFlow)
   {
      result = TE_CMD_RESULT_PRESET_OVERFLOW;
   }
   else
   {
      result = TE_CMD_RESULT_OK;
   }

   sendMsg = itc_alloc(sizeof(PresetGroupMaskCfm), PRESET_GROUP_MASK_CFM);
   sendMsg->presetGroupMaskCfm.result = result;
   itc_send(&sendMsg, itc_sender(recMsg), ITC_MY_MBOX);
}


/* ===================================================================== */
/**
 *   Handles the GROUP_MASK_REQ.
 *
 *   @param recMsg  Pointer to signal to handle
 *
 *   @return    -
 *
 *   @par Globals:
 *               traceItemList
 *
 *   This function responds with the saved trace item and updated
 *   group masks register for this daemon.
 */
/* ===================================================================== */
void
handleGroupMaskReq(union itc_msg *recMsg)
{
   union itc_msg        *sendMsg;
   char                 *name;
   uint32_t             groupMask;


   name = recMsg->groupMaskReq.procName;
   groupMask = recMsg->groupMaskReq.groupMask;

   /* Allocte the response to shell */
   sendMsg = itc_alloc(sizeof(GroupMaskCfm), GROUP_MASK_CFM);

   /*
    * Check if command is te enable/disable. It is not
    * allowed to use 'all' or wild character '*' for
    * "te enable/disable command
    */
   if ((recMsg->groupMaskReq.change == TE_MASK_ENABLE ||
        recMsg->groupMaskReq.change == TE_MASK_DISABLE ) &&
       // TODO earlier (groupMask ==  0xFF7FFFFF || Why??
       (groupMask ==  0xFFFFFFFF && name[strlen(name)-1] == '*'))
   {
      sendMsg->groupMaskCfm.result = TE_CMD_RESULT_SYNTAX_ERROR;
   }
   else
   {
      if (handleNewGroupMaskReq(recMsg))
      {
         sendMsg->groupMaskCfm.result = TE_CMD_RESULT_OK;
      }
      else
      {
         sendMsg->groupMaskCfm.result = TE_CMD_RESULT_PROCESS_NOT_FOUND;
      }
   }

   itc_send(&sendMsg, itc_sender(recMsg), ITC_MY_MBOX);
}


/* ===================================================================== */
/**
 *   Set the default trace group mask for the specified processes.
 *
 *   @param recMsg  Pointer to signal to handle
 *
 *   @return    1 on a matching entry being found in the database
 *              0 otherwise.
 *
 *   @par Globals:
 *               --
 */
/* ===================================================================== */
static int
setDefaultGroupMaskReq(union itc_msg *recMsg)
{
   TeCmdResultType             result;
   unsigned int                length, len;
   struct traceItemListElement *elem;
   struct daemonListElement    *elem1;
   union itc_msg               *sendMsg;
   char                        *name;


   name = recMsg->setDefaultGroupMaskReq.procName;
   length = getlength(name);

   /*
   ** Default result type
   */
   result = TE_CMD_RESULT_PROCESS_NOT_FOUND;


   /* Set all group masks to default */
   TAILQ_FOREACH(elem, &traceItemList, list)
   {
      if (strMatch(name, elem->itemInfo.procName, length))
      {
         elem->itemInfo.groupMask = TRI_DEFAULT_GROUP_MASK;

         DBG("TRI server default mask %s 0x%x\n",
             elem->itemInfo.procName, elem->itemInfo.groupMask);

         /*
          * Send the new group mask to all daemons where the
          * thread name exist
          */
         TAILQ_FOREACH(elem1, &(elem->daemonList), list)
         {
            sendMsg = itc_alloc(sizeof(TriServerSetMaskInd),
                                TRI_SERVER_SET_MASK_IND);
            len = sizeof(sendMsg->triServerSetMaskInd.itemInfo.procName);
            strncpy(sendMsg->triServerSetMaskInd.itemInfo.procName,
                    elem->itemInfo.procName,
                    len);
            sendMsg->triServerSetMaskInd.itemInfo.procName[len - 1] = '\0';
            sendMsg->triServerSetMaskInd.itemInfo.groupMask =
               elem->itemInfo.groupMask;

            DBG("TRI server sends new mask %s 0x%x\n",
                sendMsg->triServerSetMaskInd.itemInfo.procName,
                sendMsg->triServerSetMaskInd.itemInfo.groupMask);

            itc_send(&sendMsg, elem1->daemonMbox, ITC_MY_MBOX);
         }
         result = TE_CMD_RESULT_OK;
      }
   }
   return result;
}

/* ===================================================================== */
/**
 *   Set the default trace group mask for the specifed preset processes.
 *
 *   @param type : Char pointer to handle commmand argument.
 *
 *   @return    TE_CMD_RESULT_OK : Always success
 *
 *   @par Globals:
 *               --
 */
/* ===================================================================== */
static int
setDefaultPresetList(uint32_t scope, char *pname)
{
       if(scope == TE_RESET_RESTART)
       {
           removePreset(pname, true);
           saveGroupMaskReq("*");
       }
       else
       {
           removePreset(pname, false);
       }

       return TE_CMD_RESULT_OK;
}
/* ===================================================================== */
/**
 *   Handles the SET_DEFAULT_GROUP_MASK__REQ.
 *
 *   @param recMsg  Pointer to signal to handle
 *
 *   @return    -
 *
 *   @par Globals:
 *               traceItemList
 *
 */
/* ===================================================================== */
void
handleSetDefaultGroupMaskReq(union itc_msg *recMsg)
{
   union itc_msg        *sendMsg;

   sendMsg = itc_alloc(sizeof(GroupMaskCfm), SET_DEFAULT_GROUP_MASK_CFM);

   if(recMsg->setDefaultGroupMaskReq.scope != TE_RESET_RUNNING)
   {
      sendMsg->setDefaultGroupMaskCfm.result =
               setDefaultPresetList(recMsg->setDefaultGroupMaskReq.scope,
                                  recMsg->setDefaultGroupMaskReq.procName);
   }
   else
   {
      sendMsg->setDefaultGroupMaskCfm.result = setDefaultGroupMaskReq(recMsg);
   }

   itc_send(&sendMsg, itc_sender(recMsg), ITC_MY_MBOX);

}

/* ===================================================================== */
/**
 *   Search for all entries in trace item list for items which match
 *   the 'procName' field.
 *
 *   @param recMsg   Pointer to the message to handle
 *
 *   @return     1 if a mathcing entry was found in the new database
 *               0 otherwise.
 *
 *   @par Globals:
 *               --
 *
 *   We go through the new database and search for all entries
 *   which match the 'procName' field in the input signal.
 *   We then collect all of the matching entry's 'goupMask'
 *   fields and build reply signals (OMCSF_STATUS_INFO_IND).
 *   Each reply signal will contain at most TE_MAX_STATUS_ITEMS
 *   matching trace item list entries. We send reply signals to
 *   the sender of the input signal.
 */
/* ===================================================================== */
static int
statusReq(union itc_msg *recMsg)
{
   unsigned int     count;    /* Total number of matching entries */
   unsigned int     leftOver; /* number of left over entries after count%450 */
   union itc_msg               *sendMsg;
   union itc_msg               *sendLastMsg;
   struct traceItemListElement *elem = NULL;
   struct daemonListElement    *dElem = NULL;
   itc_mbox_id_t               requesterMbox = itc_sender(recMsg);
   int                         length, i;
   char                        *name;

   name = recMsg->statusReq.procName;
   length = getlength(name);

   count = 0;
   leftOver = 0;
   sendMsg = itc_alloc(STATUS_INFO_IND_SIZE(TE_MAX_STATUS_ITEMS),
                       STATUS_INFO_IND);

   /*
    * Start to fill in the staus indication, several signals might
    * be needed. Iterate through items.
    */
   TAILQ_FOREACH(elem, &traceItemList, list)
   {
      if (strMatch(name, elem->itemInfo.procName, length))
      {
        /*
         * Add items which match the requested input to status
         * indication.For items with equal name, registered by
         * different daemons, will be added as one element each
         * in status indication. Iterate through all registered
         * daemons for this item.
         */
         TAILQ_FOREACH(dElem, &elem->daemonList, list) {

            sendMsg->statusInfoInd.status[leftOver].mbox = dElem->pid;
            sendMsg->statusInfoInd.status[leftOver].groupMask =
               elem->itemInfo.groupMask;
            strcpy(sendMsg->statusInfoInd.status[leftOver].procName,
                   elem->itemInfo.procName);
            count++;
            leftOver++;

            /*
             * Check it the message is filled up. If so se status indication
             * and allocate a new message when there is more to send.
             */
            if(leftOver == TE_MAX_STATUS_ITEMS)
              {
                sendMsg->statusInfoInd.noOfProc = TE_MAX_STATUS_ITEMS;
                itc_send(&sendMsg, requesterMbox, ITC_MY_MBOX);
                leftOver = 0;
                sendMsg = itc_alloc(STATUS_INFO_IND_SIZE(TE_MAX_STATUS_ITEMS),
                                    STATUS_INFO_IND);
              }
         }
      }
   }

   /*
    * Send the last item/items if the status indication was not filled up..
    */
   if(leftOver)
   {
      sendLastMsg = itc_alloc(STATUS_INFO_IND_SIZE(leftOver),
                              STATUS_INFO_IND);

      for(i = 0; i < leftOver; i++)
      {
         strcpy(sendLastMsg->statusInfoInd.status[i].procName,
                sendMsg->statusInfoInd.status[i].procName);
         sendLastMsg->statusInfoInd.status[i].mbox =
            sendMsg->statusInfoInd.status[i].mbox;
         sendLastMsg->statusInfoInd.status[i].groupMask =
            sendMsg->statusInfoInd.status[i].groupMask;
      }
      sendLastMsg->statusInfoInd.noOfProc = leftOver;
      itc_send(&sendLastMsg, requesterMbox, ITC_MY_MBOX);
   }

   if (sendMsg)
     {
       itc_free(&sendMsg);
     }

   return count;
}

/* ===================================================================== */
/**
 *   Search for all entries in trace item list for items
 *
 *   @param recMsg   Pointer to the message to handle
 *
 *   @return     count : Value is the last no of processes list which
 *                       which were sent
 *
 *   @par Globals:
 *               --
 *
 *   We go through the new database and search for all entries
 *   and filter them according to input data (restart/preset)
 *   We then collect all of the matching entry's 'groupMask'
 *   fields and build reply signals (OMCSF_STATUS_INFO_IND).
 *   Each reply signal will contain at most TE_MAX_STATUS_ITEMS
 *   matching trace item list entries. We send reply signals to
 *   the sender of the input signal.
 */
/* ===================================================================== */
static int
statusPresetReq(union itc_msg *recMsg)
{
   unsigned int count = 0;    /* Total number of matching entries */
   unsigned int leftOver = 0; /* number of left over entries after count%450 */
   union itc_msg *sendMsg;
   struct savedItemListElement *saveItem = NULL;
   struct savedItemListElement *tmp_elem1;
   bool itemFound = false;
   itc_mbox_id_t requesterMbox = itc_sender(recMsg);
   int preset = 0;

   sendMsg = itc_alloc(STATUS_INFO_IND_SIZE(TE_MAX_STATUS_ITEMS),
                       STATUS_INFO_IND);

   /*
    * Start to fill in the staus indication, several signals might
    * be needed. Iterate through items.
    */
   if (!strcmp(recMsg->statusReq.procName, "-preset"))
   {
      preset = 1;
   }

   /* Here we traverse inversely such that recent added preset will be in
    * sent last
    */
   for (saveItem = TAILQ_LAST(&savedGroupMaskList, savedGroupMaskListHead);
        saveItem != NULL; saveItem = tmp_elem1)
   {
      tmp_elem1 = TAILQ_PREV(saveItem,savedGroupMaskListHead, list);

      if (saveItem->itemInfo.saved || preset)
      {
         sendMsg->statusInfoInd.status[leftOver].mbox = ITC_NO_ID;
         sendMsg->statusInfoInd.status[leftOver].groupMask =
             saveItem->itemInfo.groupMask;
         strcpy(sendMsg->statusInfoInd.status[leftOver].procName,
                saveItem->itemInfo.procName);
         count++;
         leftOver++;
         itemFound = true;

         /*
          * Check it the message is filled up. If so se status indication
          * and allocate a new message when there is more to send.
          */
          if (leftOver == TE_MAX_STATUS_ITEMS)
          {
             sendMsg->statusInfoInd.noOfProc = TE_MAX_STATUS_ITEMS;
             itc_send(&sendMsg, requesterMbox, ITC_MY_MBOX);
             leftOver = 0;
             sendMsg = itc_alloc(STATUS_INFO_IND_SIZE(TE_MAX_STATUS_ITEMS),
                                 STATUS_INFO_IND);
           }
      }
   }

   /*
    * Send the last item/items if the status indication was not filled up..
    */
   if (leftOver || !itemFound)
   {
      sendMsg->statusInfoInd.noOfProc = leftOver;
      itc_send(&sendMsg, requesterMbox, ITC_MY_MBOX);
   }
   return count;
}

/* ===================================================================== */
/**
 *   Handles the STATUS_REQ.
 *
 *   @param recMsg  Pointer to signal to handle the signal
 *
 *   @return    -
 *
 *   @par Globals:
 *               -
 *
 */
/* ===================================================================== */
void
handleStausReq(union itc_msg *recMsg)
{
   union itc_msg        *sendMsg;

   /* Handle the status request */
   sendMsg = itc_alloc(sizeof(StatusCfm), STATUS_CFM);

     if (!strcmp(recMsg->statusReq.procName, "-restart") ||
         !strcmp(recMsg->statusReq.procName, "-preset"))
     {
        statusPresetReq(recMsg);
        sendMsg->statusCfm.result = TE_CMD_RESULT_OK;
     }
     else
     {
        if (statusReq(recMsg))
        {
          sendMsg->statusCfm.result = TE_CMD_RESULT_OK;
        }
        else
        {
          sendMsg->statusCfm.result = TE_CMD_RESULT_PROCESS_NOT_FOUND;
        }
    }
   itc_send(&sendMsg, itc_sender(recMsg), ITC_MY_MBOX);
}

/* ===================================================================== */
/**
 *   This function handles the save trace group request.
 *
 *   @param recMsg   Pointer to the message to handle
 *
 *   @return    TE_CMD_RESULT_OK if everything was OK. Otherwise an
 *              appropriate result code is returned.
 *
 *   @par Globals:
 *               teSaveFile, traceItemList
 */
/* ===================================================================== */
static TeCmdResultType
saveGroupMaskReq(char *name)
{
   FILE                        *tmpFh = NULL;
   int                         num;
   struct savedItemListElement *elem;
   char                        *tmpTeSaveFile = NULL;
   char                        *tmp;
   TeCmdResultType             result = TE_CMD_RESULT_NOT_OK;

   /*
    * Allocate the temporary file, which shall include the new set of
    * trace items and group masks
   */
   tmpTeSaveFile = (char*)malloc(strlen(teSaveFile) + strlen("tmp_te_save.txt") + 2);
   if (!tmpTeSaveFile)
   {
      syslog(LOG_ERR, "TRI server %s: %s (%d).",
             __FUNCTION__, strerror(errno), errno);
      goto error_label;
   }
   strcpy(tmpTeSaveFile, teSaveFile);
   tmp = strrchr(tmpTeSaveFile, '/');
   strcpy(tmp + 1, "tmp_te_save.txt");
   errno = 0;

   /* Open a temporary file to store the group masks */
   if ((tmpFh = fopen(tmpTeSaveFile, "w+")) == NULL)
   {
      syslog(LOG_ERR, "Tri server failed to open %s for writing, "
             "%s (%d)", teSaveFile, strerror(errno), errno);
      goto error_label;
   }

   errno = 0;
   if (chmod(tmpTeSaveFile, S_IROTH | S_IRUSR | S_IWUSR))
   {
      syslog(LOG_ERR, "Tri server failed to change permissions for "
             "%s, %s (%d)", teSaveFile, strerror(errno), errno);
      goto error_label;
   }

   /* Now add the requested item to the new temporary save file */
   TAILQ_FOREACH(elem, &savedGroupMaskList, list)
   {
      /* Now walk through all trace groupmask list and save only
       * whose group mask was requested to save (te save/config)
       */
      if ((elem->itemInfo.groupMask != TRI_DEFAULT_GROUP_MASK) &&
          (elem->itemInfo.saved == true))
      {
         errno = 0;
         num = fprintf(tmpFh, "%x %s\n", elem->itemInfo.groupMask,
                       elem->itemInfo.procName);
         if (num < 0)
         {
            syslog(LOG_ERR, "Tri server failed to write %s %x to %s, "
                   "%s (%d)",  elem->itemInfo.procName,
                   elem->itemInfo.groupMask, tmpTeSaveFile,
                   strerror(errno), errno);
            goto error_label;
         }
      }
   }

   fclose(tmpFh);
   tmpFh = NULL;

   /*
    * Remove 'old' file and rename the temporary file
    */
   errno = 0;
   if (remove(teSaveFile) != 0 && errno != ENOENT)
   {
      syslog(LOG_INFO, "TRI server remove %s, %s (%d)",
             teSaveFile, strerror(errno), errno);
      goto error_label;
   }

   errno = 0;
   if (rename(tmpTeSaveFile, teSaveFile) != 0)
   {
      syslog(LOG_ERR, "Tri server failed to open %s for reading, "
             "%s (%d)", teSaveFile, strerror(errno), errno);
      goto error_label;
   }

   result = TE_CMD_RESULT_OK;

 error_label:
         free(tmpTeSaveFile);
         if (tmpFh)
           fclose(tmpFh);
         return result;
}

/* ===================================================================== */
/**
 *   This function handles a SAVE_GROUP_MASK_REQ.
 *
 *   @param recMsg   Pointer to the message to handle
 *
 *   @return    -
 *
 *   @par Globals:
 *               -
 */
/* ===================================================================== */
static void
handleSaveGroupMaskReq(union itc_msg *recMsg)
{
   union itc_msg *sendMsg;
   TeCmdResultType result;
   int length;
   struct traceItemListElement *elem = NULL;
   unsigned int groupMask;
   bool elemFound = false;


   length = getlength(recMsg->saveGroupMaskReq.procName);
   sendMsg = itc_alloc(sizeof(SaveGroupMaskCfm), SAVE_GROUP_MASK_CFM);

   TAILQ_FOREACH(elem, &traceItemList, list)
   {
      if ((strMatch(recMsg->saveGroupMaskReq.procName,
           elem->itemInfo.procName,
           length)))
      {

         groupMask = elem->itemInfo.groupMask;
         addPreset(elem->itemInfo.procName,
                   groupMask,
                   true);
         elemFound = true;
      }
   }

   if (elemFound == true)
   {
      /*
       * This preset has to be saved...
       */
       result = saveGroupMaskReq(recMsg->saveGroupMaskReq.procName);

       if (result == TE_CMD_RESULT_OK)
       {
          if (saveOverFlow)
          {
             result = TE_CMD_RESULT_PRESET_OVERFLOW;
          }
       }
   }
   else
   {
      /* Specified process not found*/
      result = TE_CMD_RESULT_PROCESS_NOT_FOUND;
   }
   sendMsg->groupMaskCfm.result = result;
   itc_send(&sendMsg, itc_sender(recMsg), ITC_MY_MBOX);

}

/* ===================================================================== */
/**
 *   This function handles TRI_SERVER_FILTER_REQ.
 *
 *   @param recMsg   Pointer to the message to handle
 *
 *   @return    -
 *
 *   @par Globals:
 *               -
 */
/* ===================================================================== */
static void
handleFilterReq(union itc_msg *recMsg)
{
   union itc_msg *sendMsg;
   uint32_t result = 0;


   result = updateBusFilter(recMsg);

   sendMsg = itc_alloc(sizeof(FilterSetRsp), FILTER_SET_CFM);
   sendMsg->filterSetRsp.result = result;
   itc_send(&sendMsg, itc_sender(recMsg), ITC_MY_MBOX);
}

#ifdef TRI_TRID
/* ===================================================================== */
/**
 *   This function handles initialization for ITC, shall only be done
 *   when TRI server executes as a stand alone process.
 *
 *   @param -
 *
 *   @return    -
 *
 *   @par Globals:
 *              -
 */
/* ===================================================================== */
static void
triInitItc(void)
{
   int32_t proc_max = TRI_MAX_PROCS_DEFAULT;
   char *env;
   int ret;

  /* Get values from environment */
   env = getenv("TRI_MAX_PROCS");
   if (env != NULL)
   {
      errno = 0;
      proc_max = (int32_t) strtol(env, NULL, 10);
      if (errno) {
         syslog(LOG_ERR, "Failed to read MAX_PROCS environment");
      }
   }

   ret = itc_init(proc_max, ITC_MALLOC, NULL, ITC_NO_NAMESPACE, 0);

   if ((ret != 0) && (ret != ITC_EALREADY_INITIALISED)) {
     syslog(LOG_ERR, "itc_init failed, ret=%d", ret);
     abort();
   }
   ret = 0;
}
#endif

/* ===================================================================== */
/**
 *   This function handles initialization for TRI server
 *
 *   @param -
 *
 *   @return    -
 *
 *   @par Globals:
 *              myBox, teSaveFile, traceItemList
 */
/* ===================================================================== */
static void
initTriServer(void)
{
   char *pfsPath;
   char *teSaveDir;
   int ret = -1;

   /* Open syslog for logging */
   openlog("TRI_SERVER", 0, LOG_DAEMON);

   /*
    * Initialize ITC if TRI server executes a own process and is not
    * provided via a library
    */
#ifdef TRI_TRID
   triInitItc();
#endif

   /* Create an ITC mailbox */
   if ((myBox = itc_current_mbox()) == ITC_NO_ID) {
      myBox = itc_create_mailbox(TRI_SERVER_MAILBOX_NAME, 0);
      strncpy(tri_server_mbox_name, TRI_SERVER_MAILBOX_NAME, ITC_NAME_MAXLEN);
   } else {
      if (!itc_get_name(myBox, tri_server_mbox_name, ITC_NAME_MAXLEN)) {
         syslog(LOG_ERR, "Failed to get TRI server name");
         abort();
      }
   }

   /* Create and initialte the trace item list */
   TAILQ_INIT(&traceItemList);

   /* Create and intialise the group mask saved list */
   TAILQ_INIT(&savedGroupMaskList);

   /* initiate environment */
#if TE_CMD
   init_env();
#endif

   /* set umask to 022 for this program to get
    * correct permissions later at creation
    */
   umask(022);

   /*
    * Set the full path and file name to where the saved group
    * masks is shall be stored
    */
   pfsPath = getenv("PFS_PATH");
   if (!pfsPath)
   {
      /* Do not free teSaveFile.*/
      teSaveFile =
         (char*)malloc(strlen(TRI_SERVER_TE_SAVE_DEFAULT_PATH) +
                       strlen(TRI_SERVER_TE_SAVE_FILE_NAME) + 1);
      if (!teSaveFile)
      {
         syslog(LOG_ERR, "TRI server %s: %s (%d).",
                __FUNCTION__, strerror(errno), errno);
         return;
      }
      sprintf(teSaveFile, "%s%s",
              TRI_SERVER_TE_SAVE_DEFAULT_PATH,
              TRI_SERVER_TE_SAVE_FILE_NAME);
   }
   else
   {
      /* Do not free teSaveFile.*/
      teSaveFile =
         (char*)malloc(strlen(pfsPath) +
                       strlen(TRI_SERVER_TE_SAVE_SUBDIR_NAME) +
                       strlen(TRI_SERVER_TE_SAVE_FILE_NAME) + 3);
      if (!teSaveFile)
      {
         syslog(LOG_ERR, "TRI server %s: %s (%d).",
                __FUNCTION__, strerror(errno), errno);
         return;
      }
      sprintf(teSaveFile, "%s/%s/%s",
               pfsPath, TRI_SERVER_TE_SAVE_SUBDIR_NAME,
               TRI_SERVER_TE_SAVE_FILE_NAME);

      /*
       * Check if the tri sub-directory exists and create it if it doesn't.
       * The root directories are assumed to exist as this is used by
       * severeal features.
       */
      teSaveDir  =
         (char*)malloc(strlen(pfsPath) +
                       strlen(TRI_SERVER_TE_SAVE_SUBDIR_NAME) + 2);
      if (!teSaveDir)
      {
         syslog(LOG_ERR, "TRI server %s: %s (%d).",
                __FUNCTION__, strerror(errno), errno);
         free(teSaveFile);
         return;
      }
      sprintf(teSaveDir, "%s/%s",
              pfsPath, TRI_SERVER_TE_SAVE_SUBDIR_NAME);

      errno = 0;
      if (access(teSaveDir, F_OK != F_OK))
      {
         ret = mkdir(teSaveDir, S_IRWXU | S_IRGRP |
                     S_IXGRP | S_IROTH | S_IXOTH);
      }
      if (errno != 0 || ret != 0)
      {
         syslog(LOG_ERR, "TRI server failed to create directory %s "
                ": %s (%d)", teSaveDir, strerror(errno), errno);
      }
      free(teSaveDir);
   }

   readSavedGroupMasks(teSaveFile);
}


/* ===================================================================== */
/**
 *   TRI server which handles the 'te' command and updates all
 *   TRI daemons with changes of trace group masks.
 *
 *   @par Globals:
 *                     traceItemList
 */
/* ===================================================================== */
#ifdef TRI_TRID
int main(int argc, char *argv[])
#else
void *triServer(void *arg)
#endif
{
   union itc_msg *msg;
   uint32_t rxFilter[] = {0};

   /* Initiation stuff */
   initTriServer();

#ifdef LTTNG_STARTUP_TP
   event_system_start("ns_main name server thread ready");
#endif

   while(1)
   {
      msg = itc_receive(rxFilter, ITC_NO_TMO, ITC_FROM_ALL);

      switch(msg->msgNo)
      {
         case TRI_SERVER_REGISTER_DAEMON_REQ:
         {
            handleRegisterDaemonReq(msg);
            break;
         }
         case TRI_SERVER_REGISTER_ITEM_REQ:
         {
            handleRegisterItemReq(msg);
            break;
         }
         case TRI_SERVER_DEREGISTER_ITEM_REQ:
         {
            handleDeregisterItemReq(msg);
            break;
         }
         case TRI_SERVER_DAEMON_MONITOR_IND:
         {
            handleDaemonMonitorInd(msg);
            break;
         }
         case TRI_SERVER_ITEM_KILLED_IND:
         {
            handleItemKilledInd(msg);
            break;
         }
         case PRESET_GROUP_MASK_REQ :
         {
            handlePresetGroupMaskReq(msg);
            break;
         }
         case GROUP_MASK_REQ:
         {
            handleGroupMaskReq(msg);
            break;
         }
         case SET_DEFAULT_GROUP_MASK_REQ:
         {
            handleSetDefaultGroupMaskReq(msg);
            break;
         }
         case STATUS_REQ:
         {
            handleStausReq(msg);
            break;
         }
         case SAVE_GROUP_MASK_REQ:
         {
            handleSaveGroupMaskReq(msg);
            break;
         }
         case FILTER_SET_REQ:
         {
            handleFilterReq(msg);
            break;
         }
         default:
         {
            syslog(LOG_ERR, "TRI server: Unknown message 0x%x received "
                   "from 0x%x", msg->msgNo, itc_sender(msg));
            break;
         }
      }
      itc_free(&msg);
   }
}

/* ========================================================================
 *   CONSTRUCTOR
 * ========================================================================
 */
void __attribute__((constructor)) createTriServer(void)
{
   /* Multi triserver thread protection */
   static int tri_srv_run = 0;

   /* Open syslog for logging */
   openlog("TRI_SERVER", 0, LOG_DAEMON);

   if (tri_srv_run)
      return;

#ifndef TRI_TRID
   pthread_t tid;
   int ret;

   ret = pthread_create(&tid, NULL, triServer, NULL);
   if (ret != 0) {
      syslog(LOG_ERR,
            "createTriServer: Failed to create TRI server: %s",
            strerror(ret));
      abort();
   } else {
      tri_srv_run = 1;
   }
#endif

   closelog();

   return;
}
