/**
 *   TRI daemon, which communicate with the TRI server.
 *
 *   Copyright (C) 2014-2015 by Ericsson AB. All rights reserved. The
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
 *   Revised : 2013-04-19 Anette Schött
 *   Change  : First version.
 *
 *   Revised : 2013-07-05 Anette Schött
 *   Change  : SMBB bug 260, each tri daemon mailbox need to have a uniqe
 *             name as itc_locate will not locate the local mailbox in the
 *             calling process.
 *
 *   Revised : 2013-10-09 Anette Schött
 *   Change  : Updated with list functionality provided by sys/queue.h
 *
 *   Revised : 2013-11-12 Stanislav Vovk
 *   Change  : Removed creation of directories and files, handled by
 *             tri server
 *
 *   Revised : 2013-12-02 Marcus Rosendahl
 *   Change  : Extended the mail_box name to support 32bytes string length
 *
 *   Revised : 2013-12-13 M.Winberg
 *   Change  : Deregistration function added.
 *
 *   Revised : 2014-02-04 Stanislav Vovk
 *   Change  : Set pid to registered thread's mailbox
 *
 *   Revised : 2014-05-02 Stanislav Vovk
 *   Change  : Fixed faulty usage of ITC_MY_MBOX
 *
 *   Revised : 2014-09-18 K V Ranganath
 *   Change  : Moved reading of group mask from file to TRI server.
 *             TRI server reads the file and sends back the new group
 *             mask information to TRI daemon.
 *
 *   Revised : 2014-10-16 K V Ranganath
 *   Change  : Bug:1842 Work around for itc issue, replaced itc_locate
 *             with itc_sender.
 *
 *   Revised : 2014-11-24 K.V.Ranganadh
 *   Change  : support for filter handling
 *
 *   Revised : 2015-03-02 K.V.Ranganadh
 *   Change  : HT50015 Disabled traces were getting logged
 *
 *   Revised : 2015-03-16 Anette Schött
 *   Change  : Add missing free of thread specific data when an attach is
 *             received. Change to send pointer to pointer of struct
 *             ItemInfo instead for pointer to struct ItemInfo.
 *
 *   Revised : 2015-04-22 Anette Schött
 *   Change  : Added usage of mallocRef to be used when freeing the heap
 *             memory allocated at registration of a thread.
 *
 *   Revised : 2015-06-02 Fredrik Skog
 *   Change  : TR HT78355: Added checks for return value of all malloc
 *             calls in TRI to prevent crashes.
 *
 *   Revised : 2015-06-12 Anette Schött
 *   Change  : HT78355, updates to make deregistration of trace item work.
 *             The contents of trace item referencies can not be used when
 *             deregistering.
 *
 *   Revised : 2015-06-24 Anette Schött
 *   Change  : Corrected the linking out of list element in
 *             handleDeregisterItemRequest.
 *
 *   Revised : 2015-09-25 Anette Schött
 *   Change  : Changed to a no LITS implementation and to not rely on that
 *             a client has ITC mailbox created.
 * ========================================================================
 */

/* ========================================================================
 *   INCLUDE FILES
 * ========================================================================
 */

#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <unistd.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <errno.h>
#include <stdlib.h>
#include "itc.h"
#include <sys/queue.h>
#include <pthread.h>
#include <syslog.h>
#include "cello_te_group.h"
#include "cello_te_handlers.h"
#include "tri_server.h"
#include "tri_daemon.h"

/* ========================================================================
 *   DEFINITIONS
 * ========================================================================
 */

/* debug print */
#ifdef TRI_DEBUG
#define DBG printf
#else
#define DBG no_printf
static void no_printf(const char *format, ...){}
#endif

/* ========================================================================
 *   TYPE DEFINITIONS
 * ========================================================================
 */

union itc_msg
{
   uint32_t                      msgNo;
   TriDaemonRegisterItemReq      triDaemonRegisterItemReq;
   TriDaemonRegisterItemRsp      triDaemonRegisterItemRsp;
   TriDaemonDeregisterItemReq    triDaemonDeregisterItemReq;
   TriDaemonItemMonitorInd       triDaemonItemMonitorInd;
   TriServerRegisterItemReq      triServerRegisterItemReq;
   TriServerRegisterItemRsp      triServerRegisterItemRsp;
   TriServerRegisterDaemonReq    triServerRegisterDaemonReq;
   TriServerRegisterDaemonRsp    triServerRegisterDaemonRsp;
   TriServerSetMaskInd           triServerSetMaskInd;
   TriServerItemKilledInd        triServerItemKilledInd;
   TriServerDeregisterItemReq    triServerDeregisterItemReq;
   TriServerDeregisterItemRsp    triServerDeregisterItemRsp;
   TriServerUpdateFilterInd      triServerUpdateFilterInd;
};

/* ========================================================================
 *   DATA DECLARATIONS
 * ========================================================================
 */

/* Processes */
static itc_mbox_id_t triServerMbox = ITC_NO_ID;

/* List of referencies to item structs for all registered trace item */
TAILQ_HEAD(traceItemRefListHead, traceItemRefListElement);
struct traceItemRefListHead traceItemRefList;

/* ========================================================================
 *   FUNCTION PROTOTYPES
 * ========================================================================
 */

/* ===================================================================== */
/**
 *   This function handles an update of trace group mask.
 *
 *   @param msg     Pointer to the message to handle
 *
 *   @return    -
 *
 *   @par Globals:
 *               traceItemRefList
 */
/* ===================================================================== */
static void
handleSetMaskInd(union itc_msg *msg)
{
   struct traceItemRefListElement *elem;


   /* Search for the item and update the group mask */
   TAILQ_FOREACH(elem, &traceItemRefList, list)
   {
      if (strcmp(elem->procName,
                 msg->triServerSetMaskInd.itemInfo.procName) == 0)
      {
         (*(elem->ref))->groupMask = msg->triServerSetMaskInd.itemInfo.groupMask;
         elem->groupMask = msg->triServerSetMaskInd.itemInfo.groupMask;

         DBG("TRI daemon new mask %s 0x%x at daemon 0x%x\n",
             (*(elem->ref))->procName, (*(elem->ref))->groupMask, itc_current_mbox());
      }
   }
}

/* ===================================================================== */
/**
 *   This function deregister a trace item (thread, object or interface).
 *
 *   @param msg     Pointer to the message to handle
 *
 *   @return    -
 *
 *   @par Globals:
 *               traceItemRefList
 *               triServerMbox
 */
/* ===================================================================== */
static void
handleDeregisterItemRequest(union itc_msg *msg)
{
   union itc_msg *sendMsg;
   TriDaemonDeregisterItemReq *req;
   struct traceItemRefListElement *elem;
   struct traceItemRefListElement *tmpElem;


   /* Note when entering this function the trace item struct which 
    * traceItemRef is referring to is already fred in deRegisterIfObj,
    * so it can only be used to search for the correct item in list.
    */
   req = &(msg->triDaemonDeregisterItemReq);

   for (elem = TAILQ_FIRST(&traceItemRefList); elem != NULL; elem = tmpElem) 
   {
      tmpElem = TAILQ_NEXT(elem, list);

      if ((elem->ref == req->traceItemRef) &&
          (strcmp(elem->procName, req->procName) == 0))
      {
         DBG("TRI daemon deregistered %s 0x%x at daemon 0x%x\n",
             elem->procName, elem->groupMask, itc_current_mbox());

         /* Update TRI server about the deregistration of trace item */
         sendMsg = itc_alloc(sizeof(TriServerDeregisterItemReq),
                             TRI_SERVER_DEREGISTER_ITEM_REQ);
         strncpy(sendMsg->triServerDeregisterItemReq.itemInfo.procName,
                 elem->procName, TRI_MAX_PROC_NAME_LEN);
         sendMsg->triServerDeregisterItemReq.itemInfo.procName[TRI_MAX_PROC_NAME_LEN - 1] = '\0';
         sendMsg->triServerDeregisterItemReq.itemInfo.groupMask = elem->groupMask;

         itc_send(&sendMsg, triServerMbox, ITC_MY_MBOX);

         /*
          * Delete the element from the trace item list.
          */
         TAILQ_REMOVE(&traceItemRefList, elem, list);
         free(elem);

         return;
      }
   }

   syslog(LOG_ERR, "TRI daemon could not deregister pointer=%p, daemon 0x%x %s",
          req->traceItemRef, itc_current_mbox(), "  - not found in internal list");
}

/* ===================================================================== */
/**
 *   This function register a trace item (thread, object or interface).
 *
 *   @param msg     Pointer to the message to handle
 *
 *   @return    -
 *
 *   @par Globals:
 *               traceItemRefList
 *               triServerMbox
 */
/* ===================================================================== */
static void
handleRegisterItemRequest(union itc_msg *msg)
{
   union itc_msg *sendMsg;
   TriDaemonRegisterItemReq *req;
   struct traceItemRefListElement *elem;
   uint32_t result = TRI_DAEMON_OK;

   req = &(msg->triDaemonRegisterItemReq);

   /* Allocate a new list item with default group mask. */
   elem = (struct traceItemRefListElement*)malloc(sizeof(struct traceItemRefListElement));
   if (elem)
   {
      elem->ref = req->traceItemRef;
      elem->mallocRef = req->mallocRef;
      strncpy(elem->procName, req->procName, TRI_MAX_PROC_NAME_LEN);
      elem->procName[TRI_MAX_PROC_NAME_LEN - 1] = '\0';
      elem->groupMask = req->groupMask;
      DBG("TRI daemon register %s 0x%x at daemon 0x%x\n",
          elem->procName, elem->groupMask, itc_current_mbox());
   }
   else
   {
      result = TRI_DAEMON_MALLOC_ERROR;
      syslog(LOG_ERR, "TRI daemon %s: %s (%d).",
             __FUNCTION__, strerror(errno), errno);
   }

   if (req->itemType == TRI_ITEMTYPE_THREAD)
   {
      /*
       * Only respond if the registered trace item is a thread.
       * Object and interface are registered in OSE_HOOK2 and
       * can therefore not recieve signals
       */
      sendMsg = itc_alloc(sizeof(TriDaemonRegisterItemRsp),
                          TRI_DAEMON_REGISTER_ITEM_RSP);

      sendMsg->triDaemonRegisterItemRsp.result = result;
      itc_send(&sendMsg, itc_sender(msg), ITC_MY_MBOX);
   }

   /*
    * Update TRI server with information about the new trace item if
    * the registration is successful and add trace item to the list.
    */
   if (result == TRI_DAEMON_OK)
   {
      TAILQ_INSERT_TAIL(&traceItemRefList, elem, list);

      sendMsg = itc_alloc(sizeof(TriServerRegisterItemReq),
                          TRI_SERVER_REGISTER_ITEM_REQ);
      strncpy(sendMsg->triServerRegisterItemReq.itemInfo.procName,
              elem->procName, TRI_MAX_PROC_NAME_LEN);
      sendMsg->triServerRegisterItemReq.itemInfo.procName[TRI_MAX_PROC_NAME_LEN - 1] = '\0';
      sendMsg->triServerRegisterItemReq.itemInfo.groupMask =
         elem->groupMask;

      if (req->itemType == TRI_ITEMTYPE_THREAD)
      {
         sendMsg->triServerRegisterItemReq.pid = itc_sender(msg);
      }
      else
      {
         sendMsg->triServerRegisterItemReq.pid = ITC_NO_ID;
      }

      itc_send(&sendMsg, triServerMbox, ITC_MY_MBOX);
   }
   else if (result == TRI_DAEMON_MALLOC_ERROR)
   {
      return;
   }
   else
   {
      free(elem);
   }
}

/* ===================================================================== */
/**
 *   This function removes an item from the trace item reference list
 *   and deallocate the trace item struct allocated at registration
 *   when a thread has died.
 *
 *   @param msg     Pointer to the message to handle
 *
 *   @return    -
 *
 *   @par Globals:
 *               traceItemRefList
 */
/* ===================================================================== */
static void
handleItemMonitorInd(union itc_msg *msg)
{
   struct traceItemRefListElement *elem;
   struct traceItemRefListElement *tmp_elem;
   struct ItemInfo *mallocRef;
   union itc_msg *sendMsg;

   mallocRef = msg->triDaemonItemMonitorInd.mallocRef;

   DBG("TRI daemon receive monitor ind for item %s at daemon 0x%x\n",
       mallocRef->procName, itc_current_mbox());

   /* Inform TRI server of the removed trace item */
   sendMsg = itc_alloc(sizeof(TriServerItemKilledInd),
                       TRI_SERVER_ITEM_KILLED_IND);
   strncpy(sendMsg->triServerItemKilledInd.itemName,
           mallocRef->procName, TRI_MAX_PROC_NAME_LEN);
   sendMsg->triServerItemKilledInd.daemonMbox = itc_current_mbox();

   itc_send(&sendMsg, triServerMbox, ITC_MY_MBOX);

   /*
    * A client (thread) has died. Delete the element from the
    * trace item list and delete the trace item struct allocated
    * at registration.
    */
   for (elem = TAILQ_FIRST(&traceItemRefList); elem != NULL; elem = tmp_elem) {
      tmp_elem = TAILQ_NEXT(elem, list);

      if (elem->mallocRef == mallocRef)
      {
         DBG("TRI daemon remove item %s at  daemon 0x%x\n",
             mallocRef->procName, itc_current_mbox());
         /*
          * Free the heap memory befor setting pointer to NULL
          * and the pointer to this data
          */
         if (elem->mallocRef)
         {
            free(elem->mallocRef);
            elem->mallocRef = NULL;
         }
         else
         {
           syslog(LOG_ERR, "TRI daemon: Monitor indication received "
                  "where trace item reference is not available");
         }

         TAILQ_REMOVE(&traceItemRefList, elem, list);
         free(elem);

         break;
      }
   }
}

/* ===================================================================== */
/**
 *   This function handles an update of bus filters.
 *
 *   @param msg     Pointer to the message to handle
 *
 *   @return    -
 *
 *   @par Globals:
 *               traceItemRefList
 */
/* ===================================================================== */
static void
handleUpdateFilter(union itc_msg *msg)
{
   struct traceItemRefListElement *elem;


   /* Search for the item and update the group mask */
   TAILQ_FOREACH(elem, &traceItemRefList, list)
   {
     if ((strcmp((*(elem->ref))->procName,
                 msg->triServerUpdateFilterInd.itemName)) == 0)
      {
         if (msg->triServerUpdateFilterInd.compiledFilter[0] != 0)
         {
            if ((*(elem->ref))->busLogFilter == NULL)
            {
              (*(elem->ref))->busLogFilter = malloc((sizeof(int) *
                                                 MAX_FILTER_LEN));
              if ((*(elem->ref))->busLogFilter == NULL)
              {
                 syslog(LOG_ERR, "TRI daemon %s: %s (%d).",
                        __FUNCTION__, strerror(errno), errno);
                 return;
              }
            }

            memcpy((char *)(*(elem->ref))->busLogFilter,
                   (char *)msg->triServerUpdateFilterInd.compiledFilter,
                   MAX_FILTER_LEN * sizeof(int));
         }
         else
         {
            if ((*(elem->ref))->busLogFilter != NULL)
            {
               free((*(elem->ref))->busLogFilter);
            }
            (*(elem->ref))->busLogFilter = NULL;
         }
      }
   }
}

/* ===================================================================== */
/**
 *   This function handles initialization for TRI daemon
 *
 *   @param -
 *
 *   @return    -
 *
 *   @par Globals:
 *              traceItemRefList, savedGroupMaskList,
 *              triServerMbox
 */
/* ===================================================================== */
static void
initTriDaemon(void)
{
   union itc_msg *sendMsg;
   itc_mbox_id_t myBox;
   /*The extra 32 bytes in the below statement is to be able to store a
     64bit string representation of a pid*/
   char name[sizeof(TRI_DAEMON_MAILBOX_NAME) + 32 + 1];

   /* Open syslog for logging */
   openlog("TRI_DAEMON", 0, LOG_DAEMON);

   /* Create an ITC mailbox. Each daemon must have a unique mailbox
    * name as linx does not first choose the one in calling process. Use
    * process pid to differentiate the mailboxes.
    */
   snprintf(name, sizeof(name) - 1, "%s_%u",
            TRI_DAEMON_MAILBOX_NAME, getpid());

   /* Do not delete mail box as this is referenced in tri server. */
   if ((myBox = itc_current_mbox()) == ITC_NO_ID)
      myBox = itc_create_mailbox(name, 0);

   /* Create and initialte the list to save trace item referncies in*/
   TAILQ_INIT(&traceItemRefList);

   /* Locate the TRI server mailbox */
   while((triServerMbox = itc_locate(TRI_SERVER_MAILBOX_NAME)) == ITC_NO_ID)
   {
      if ((triServerMbox = itc_locate(TRI_SERVER_NAME)) != ITC_NO_ID)
      {
          break;
      }
      syslog(LOG_INFO, "TriDaemon tries to locate TRI server");
      sleep(1);
   }

   /*
    * Register to TRI server.
    */
   sendMsg = itc_alloc(sizeof(TriServerRegisterDaemonReq),
                       TRI_SERVER_REGISTER_DAEMON_REQ);
   itc_send(&sendMsg, triServerMbox, ITC_MY_MBOX);
}

/** ==================================================================== */
/**
 *   TRI daemon thread
 *
 *   @param         -
 *
 *   @return        -
 *
 *   @par Globals:  --
 *
 */
/* ===================================================================== */
void *triDaemon(void *arg)
{
   union itc_msg *msg;
   uint32_t rxFilter[] = {0};

   initTriDaemon();

   while(1)
   {
      msg = itc_receive(rxFilter, ITC_NO_TMO, ITC_FROM_ALL);

      switch(msg->msgNo)
      {
         case TRI_SERVER_REGISTER_DAEMON_RSP:
         {
            if (msg->triServerRegisterDaemonRsp.result != 0)
            {
               syslog(LOG_ERR, "TRI daemon: Failed to register daemon 0x%x "
                      "to TRI server",
                      msg->triServerRegisterDaemonRsp.daemonMbox);
            }
            break;
         }
         case TRI_SERVER_REGISTER_ITEM_RSP:
         {
            if (msg->triServerRegisterItemRsp.result != 0)
            {
               syslog(LOG_ERR, "TRI daemon: Failed to register item %s "
                      "to TRI server at daemon 0x%x\n",
                      msg->triServerRegisterItemRsp.itemName, itc_current_mbox());
            }
            break;
         }
         case TRI_SERVER_SET_MASK_IND:
         {
            handleSetMaskInd(msg);

            break;
         }
         case TRI_DAEMON_REGISTER_ITEM_REQ:
         {
            handleRegisterItemRequest(msg);

            break;
         }
         case TRI_DAEMON_DEREGISTER_ITEM_REQ:
         {
            handleDeregisterItemRequest(msg);
            break;
         }
         case TRI_SERVER_DEREGISTER_ITEM_RSP:
         {
            if (msg->triServerDeregisterItemRsp.result != 0)
            {
               syslog(LOG_ERR, "TRI daemon: Failed to deregister item %s "
                      "to TRI server at daemon 0x%x\n",
                      msg->triServerDeregisterItemRsp.itemName, itc_current_mbox());
            }
            break;
         }
         case TRI_DAEMON_ITEM_MONITOR_IND:
         {
            handleItemMonitorInd(msg);
            break;
         }
         case TRI_SERVER_UPDATE_FILTER_IND:
         {
            handleUpdateFilter(msg);
            break;
         }
         default:
         {
            syslog(LOG_ERR, "TRI daemon: Unknown message 0x%x "
                   "received from 0x%x\n",
                   msg->msgNo, itc_sender(msg));
            break;
         }
      }
      itc_free(&msg);
   }
   return NULL;

}
