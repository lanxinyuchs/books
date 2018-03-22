/**
 *   TRI daemon, which communicate with the TRI server.
 *
 *   Copyright (C) 2014-2016 by Ericsson AB. All rights reserved. The
 *   information in this document is the property of Ericsson. Except
 *   as specifically authorized in writing by Ericsson, the receiver
 *   of this document shall keep the information contained herein
 *   confidential and shall protect the same in whole or in part from
 *   disclosure and dissemination to third parties. Disclosure and
 *   disseminations to the receiver's employees shall only be made on
 *   a strict need to know basis.1
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
 *
 *   Revised : 2015-11-10 Anette Schött
 *   Change  : Change of TRI daemon list handling of regsitered trace items.
 *
 *   Revised : 2015-12-09 Anette Schött
 *   Change  : Corrected list handling in handleSetMaskInd.
 *
 *   Revised : 2015-12-17 Anette Schött
 *   Change  : Add wrappers for malloc/free for debug purpose.
 *
 *   Revised : 2016-02-01 Anette Schött
 *   Change  : Changed to use POSIX message queue instead for ITC monitor
 *             indication to receive indication for a killed thread. ITC
 *             can not be used in pthread_cretae_key destroy functions.
 *
 *   Revised : 2016-02-02 Anette Schött
 *   Change  : Decrease the maxmsg and msgsize for the message queue
 *             in triMsgqHandler.
 *
 *   Revised : 2016-02-23 Anette Schött
 *   Change  : Replace usage of POSIX message queue to pipe.
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
#include <mqueue.h>
#include <fcntl.h>
#include <sys/stat.h>
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

#ifdef TRI_DEBUG1
#define DBG1 syslog
#else
#define DBG1 no_syslog
static void no_syslog(int priority, const char *format, ...){}
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

/* ITC mail boxes */
static itc_mbox_id_t triServerMbox = ITC_NO_ID;

static itc_mbox_id_t triDaemonMbox = ITC_NO_ID;

/* List of registered trace items */
struct traceItemRefListHead traceItemRefList;

/* Pipe used between TRI daemon and dying threads */
int __tri_cleanup_fds[2] = {-1, -1};

/* ========================================================================
 *   FUNCTION PROTOTYPES
 * ========================================================================
 */

/*
 * Wrapper to be used for debug purpose to check freeing of heap memory.
 * To be linked with -Wl,--wrap=malloc -Wl,--wrap=free
 */

#ifdef TRI_DEBUG
void*
__wrap_malloc(size_t s)
{
   extern void* __real_malloc(size_t s);
   void* ret = __real_malloc(s);
   printf("malloc %zd %p\n", s, ret);
   return ret;
}

void
__wrap_free(void* p)
{
   extern void __real_free();
   printf("free %p\n", p);
   __real_free(p);
}
#endif

/* ===================================================================== */
/**
 *   Prints the TRI daemon linked list to syslog.
 *   name.
 *
 *   @param -
 *
 *   @return    -
 *
 *   @par Globals:
 *               traceItemRefList
 */
/* ===================================================================== */
void printDaemonList(char *string)
{
#ifdef TRI_DEBUG1
   struct traceItemRefListElement *elem;
   struct refListElement *refElem;

   /* Search for the item and update the group mask */
   TAILQ_FOREACH(elem, &traceItemRefList, list)
   {
      syslog(LOG_INFO, "TRI daemon list %s for %s (elem=%p):",
             string, elem->procName, elem);

         /* Make the update for all references trace items */
         TAILQ_FOREACH(refElem, &elem->refList, list)
         {
            syslog(LOG_INFO, "    refElem=%p ref=%p mallocRef=%p:",
                   refElem, refElem->ref, refElem->mallocRef);
         }
   }
#endif
}

/* ===================================================================== */
/**
 *   This function search for a list element for the provided trace item
 *   name.
 *
 *   @param name     Trace item name to search for
 *
 *   @param elem     Pointer to list element or NULL
 *
 *   @return    True if found.
 *
 *   @par Globals:
 *               traceItemRefList
 */
/* ===================================================================== */
static bool
findItemInList(char *name, struct traceItemRefListElement **elem)
{
   struct traceItemRefListElement *elem1;

   /* Search for the item */
   TAILQ_FOREACH(elem1, &traceItemRefList, list)
   {
      if (strcmp(elem1->procName, name) == 0)
      {
         *elem = elem1;
         return true;
      }
   }
   elem = NULL;
   return false;
}

/* ===================================================================== */
/**
 *   This function allocates and set a trace item list element.
 *   name.
 *
 *   @param name      Trace item name to add in elem.
 *
 *   @param groupMask Group mask to add in elem.
 *
 *   @param elem     Pointer to list element or NULL
 *
 *   @return    TRI_DAEMON_OK at success, otherwise TRI_DAEMON_MALLOC_ERROR.
 *
 *   @par Globals:
 *               -
 */
/* ===================================================================== */
static uint32_t
allocAndSetItemStruct(char *name, uint32_t groupMask,
                      struct traceItemRefListElement **elem)
{
   /* Allocate a new list item with default group mask. */
   *elem = (struct traceItemRefListElement*)
      malloc(sizeof(struct traceItemRefListElement));

   if (*elem)
   {
      strncpy((*elem)->procName, name, TRI_MAX_PROC_NAME_LEN);
      (*elem)->procName[TRI_MAX_PROC_NAME_LEN - 1] = '\0';
      (*elem)->groupMask = groupMask;
      TAILQ_INSERT_TAIL(&traceItemRefList, *elem, list);
      TAILQ_INIT(&((*elem)->refList));

      DBG1(LOG_INFO, "TRI daemon register %s grp=0x%x (elem: %p size=%d) at daemon 0x%x\n",
           (*elem)->procName, (*elem)->groupMask, (*elem),
           sizeof(*(*elem)), itc_current_mbox());
      return TRI_DAEMON_OK;
   }
   else
   {
      *elem = NULL;
      syslog(LOG_ERR, "TRI daemon %s: %s (%d).",
             __FUNCTION__, strerror(errno), errno);
      return TRI_DAEMON_MALLOC_ERROR;
   }
}

/* ===================================================================== */
/**
 *   This function allocates and set a trace item reference list element.
 *   name.
 *
 *   @param req       Received registration signal.
 *
 *   @param refElem   Pointer to the allocated reference list element.
 *
 *   @return    TRI_DAEMON_OK at success, otherwise TRI_DAEMON_MALLOC_ERROR.
 *
 *   @par Globals:
 *               -
 */
/* ===================================================================== */
static uint32_t
allocAndSetRefItemStruct(TriDaemonRegisterItemReq *req,
                         struct refListElement **refElem)
{
   *refElem = (struct refListElement*)malloc(sizeof(struct refListElement));
   if (*refElem)
   {
      (*refElem)->ref = req->traceItemRef;
      (*refElem)->mallocRef = req->mallocRef;

      DBG1(LOG_INFO, "TRI daemon adding %s references ref=%p mallocRef=%p "
           "(refElem: %p size=%d) at daemon 0x%x\n",
           req->procName, (*refElem)->ref, (*refElem)->mallocRef, *refElem,
           sizeof(*(*refElem)), itc_current_mbox());

      return TRI_DAEMON_OK;
   }
   else
   {
      *refElem = NULL;
      syslog(LOG_ERR, "TRI daemon %s: %s (%d).",
             __FUNCTION__, strerror(errno), errno);
      return TRI_DAEMON_MALLOC_ERROR;
   }
}

/* ===================================================================== */
/**
 *   This function allocates and sends a TRI_SERVER_REGISTER_ITEM_REQ
 *   to TRI server.
 *
 *   @param name     Trace item name to add to signal.
 *
 *   @return    -
 *
 *   @par Globals:
 *               triServerMbox
 */
/* ===================================================================== */
static void
sendServerRegisterItemReq(char *name, uint32_t groupMask,
                          TriDaemonRegisterItemReq *req)
{
   union itc_msg *sendMsg;

   sendMsg = itc_alloc(sizeof(TriServerRegisterItemReq),
                       TRI_SERVER_REGISTER_ITEM_REQ);
   strncpy(sendMsg->triServerRegisterItemReq.itemInfo.procName,
           name, TRI_MAX_PROC_NAME_LEN);
   sendMsg->triServerRegisterItemReq.itemInfo.procName[TRI_MAX_PROC_NAME_LEN - 1] = '\0';
   sendMsg->triServerRegisterItemReq.itemInfo.groupMask = groupMask;

   if (req->itemType == TRI_ITEMTYPE_THREAD)
   {
      sendMsg->triServerRegisterItemReq.pid = itc_sender((union itc_msg*)req);
   }
   else
   {
      sendMsg->triServerRegisterItemReq.pid = ITC_NO_ID;
   }
   DBG1(LOG_INFO, "TRI daemon register %s to server at daemon 0x%x\n",
        name, itc_current_mbox());

   itc_send(&sendMsg, triServerMbox, ITC_MY_MBOX);
}

/* ===================================================================== */
/**
 *   This function allocates and sends a TRI_DAEMON_REGISTER_ITEM_RSP
 *   to TRI proxy.
 *
 *   @param result     Result of the registration.
 *
 *   @param mbox       ITC mail box to send message to.
 *
 *   @return    -
 *
 *   @par Globals:
 *               -
 */
/* ===================================================================== */
static void
sendDaemonRegisterItemRsp(uint32_t result, itc_mbox_id_t mbox)
{
   union itc_msg *sendMsg;

   sendMsg = itc_alloc(sizeof(TriDaemonRegisterItemRsp),
                       TRI_DAEMON_REGISTER_ITEM_RSP);

   sendMsg->triDaemonRegisterItemRsp.result = result;
   itc_send(&sendMsg, mbox, ITC_MY_MBOX);
}

/* ===================================================================== */
/**
 *   This function allocates and sends a TRI_SERVER_ITEM_KILLED_IND
 *   to TRI server.
 *
 *   @param name     Trace item name to add to signal.
 *
 *   @return    -
 *
 *   @par Globals:
 *               triServerMbox
 */
/* ===================================================================== */
static void
sendServerItemKilledInd(char *name)
{
   union itc_msg *sendMsg;

   sendMsg = itc_alloc(sizeof(TriServerItemKilledInd),
                       TRI_SERVER_ITEM_KILLED_IND);
   strncpy(sendMsg->triServerItemKilledInd.itemName,
           name, TRI_MAX_PROC_NAME_LEN);
   sendMsg->triServerItemKilledInd.itemName[TRI_MAX_PROC_NAME_LEN - 1] = '\0';
   sendMsg->triServerItemKilledInd.daemonMbox = itc_current_mbox();
   DBG1(LOG_INFO, "TRI daemon send kill ind for %s to server at daemon 0x%x\n",
        name, itc_current_mbox());

   itc_send(&sendMsg, triServerMbox, ITC_MY_MBOX);
}

/* ===================================================================== */
/**
 *   This function allocates and sends a TRI_SERVER_DEREGISTER_ITEM_REQ
 *   to TRI server.
 *
 *   @param name     Trace item name to add to signal.
 *
 *   @return    -
 *
 *   @par Globals:
 *               triServerMbox
 */
/* ===================================================================== */
static void
sendServerDeregisterItemReq(char *name)
{
   union itc_msg *sendMsg;

   sendMsg = itc_alloc(sizeof(TriServerDeregisterItemReq),
                       TRI_SERVER_DEREGISTER_ITEM_REQ);
   strncpy(sendMsg->triServerDeregisterItemReq.itemInfo.procName,
           name, TRI_MAX_PROC_NAME_LEN);
   sendMsg->triServerDeregisterItemReq.itemInfo.procName[TRI_MAX_PROC_NAME_LEN - 1] = '\0';
   /* groupMask and saved elements are not used at deregistration. */
   sendMsg->triServerDeregisterItemReq.itemInfo.groupMask = 0;
   sendMsg->triServerDeregisterItemReq.itemInfo.saved = 0;
   DBG("TRI daemon deregister %s to server at daemon 0x%x\n",
       name, itc_current_mbox());

   itc_send(&sendMsg, triServerMbox, ITC_MY_MBOX);
}

/* ===================================================================== */
/**
 *   This function allocates and sends a TRI_DAEMON_ITEM_MONITOR_IND
 *   to TRI daemon thread.
 *
 *   @param name     Trace item name to add to signal.
 *   @param name     Malloc reference for the trace item.
 *
 *   @return    -
 *
 *   @par Globals:
 *               triDaemonMbox
 */
/* ===================================================================== */
static void
sendMonitorInd(struct ItemInfo *ref ,char *name)
{
   union itc_msg *sendMsg;


   sendMsg = itc_alloc(sizeof(TriDaemonItemMonitorInd),
                       TRI_DAEMON_ITEM_MONITOR_IND);
   sendMsg->triDaemonItemMonitorInd.mallocRef = ref;
   strncpy(sendMsg->triDaemonItemMonitorInd.procName, name,
           TRI_MAX_PROC_NAME_LEN);
   sendMsg->triDaemonItemMonitorInd.procName[TRI_MAX_PROC_NAME_LEN - 1] = '\0';
   DBG("TRI pipe handler send %s (%p)", name, ref);
   itc_send(&sendMsg, triDaemonMbox, ITC_MY_MBOX);
}

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
   struct refListElement *refElem;

   /* Search for the item and update the group mask */
   TAILQ_FOREACH(elem, &traceItemRefList, list)
   {
      if (strcmp(elem->procName,
                 msg->triServerSetMaskInd.itemInfo.procName) == 0)
      {
         elem->groupMask = msg->triServerSetMaskInd.itemInfo.groupMask;

         TAILQ_FOREACH(refElem, &(elem->refList), list)
         {
            (refElem->mallocRef)->groupMask =
               msg->triServerSetMaskInd.itemInfo.groupMask;

            DBG("TRI daemon new mask %s (%s) 0x%x (0x%x) at daemon 0x%x\n",
                elem->procName, (refElem->mallocRef)->procName,
                elem->groupMask, (refElem->mallocRef)->groupMask,
                itc_current_mbox());
         }
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
   struct traceItemRefListElement *elem;
   struct traceItemRefListElement *tmpElem;
   struct refListElement *refElem;
   struct refListElement *tmpRefElem;
   struct ItemInfo **traceItemRef;
   char *name;

   traceItemRef = msg->triDaemonDeregisterItemReq.traceItemRef;
   name = &(msg->triDaemonDeregisterItemReq.procName[0]);

   DBG("TRI daemon deregistered %s at daemon 0x%x\n",
       name, itc_current_mbox());

   /* Update TRI server about the deregistration of trace item */
   sendServerDeregisterItemReq(name);

   /* Note when entering this function the trace item struct which 
    * traceItemRef is referring to is already fred in deRegisterIfObj,
    * so it can only be used to search for the correct item in list.
    */
   for (elem = TAILQ_FIRST(&traceItemRefList); elem != NULL; elem = tmpElem) 
   {
      tmpElem = TAILQ_NEXT(elem, list);

      if (strcmp(elem->procName, name) == 0)
      {
         for (refElem = TAILQ_FIRST(&elem->refList); refElem != NULL; refElem = tmpRefElem)
         {
            tmpRefElem = TAILQ_NEXT(refElem, list);

            if (refElem->mallocRef && refElem->ref == traceItemRef)
            {
               DBG1(LOG_INFO, "TRI daemon remove item %s (%p %p %p) at daemon 0x%x\n",
                    name, refElem->mallocRef, elem, refElem, itc_current_mbox());

               /*
                * Delete the element from the reference list.
                */
               DBG1(LOG_INFO, "TRI daemon remove reference item (%p size=%d)",
                    refElem, sizeof(*refElem));
               TAILQ_REMOVE(&elem->refList, refElem, list);
               free(refElem);

               /*
                * Remove the trace item if no referencies exist in reference list.
                */
               if TAILQ_EMPTY(&(elem->refList))
               {
                  DBG1(LOG_INFO, "TRI daemon remove item (%p size=%d)",
                       elem, sizeof(*elem));
                  TAILQ_REMOVE(&traceItemRefList, elem, list);
                  free(elem);
               }
               printDaemonList("after free");
               return;
            }
         }
      }
   }
   printDaemonList("after failing free");
   syslog(LOG_ERR, "TRI daemon could not deregister object %s "
          "pointer=%p, daemon=0x%x", name, traceItemRef,
          itc_current_mbox());

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
   TriDaemonRegisterItemReq *req;
   struct traceItemRefListElement *elem = NULL;
   struct refListElement *refElem = NULL;
   itc_monitor_id_t monRef = ITC_NO_ID;
   char *name;
   uint32_t groupMask;
   uint32_t result = TRI_DAEMON_OK;
   bool alreadyInList;

   req = &(msg->triDaemonRegisterItemReq);
   name = &(msg->triDaemonRegisterItemReq.procName[0]);
   groupMask =  msg->triDaemonRegisterItemReq.groupMask;

   DBG("TRI daemon registered %s at daemon 0x%x\n",
       name, itc_current_mbox());
   /*
    * Only add new trace item element if name doesn't exists.
    */
   if ((alreadyInList = findItemInList(name, &elem)) == false)
   {
      result |= allocAndSetItemStruct(name, groupMask, &elem);
   }
   /*
    * A new trace item reference element shall always be added
    *  to then trace item element
    */
   if (result == TRI_DAEMON_OK)
   {
      result |= allocAndSetRefItemStruct(req, &refElem);
   }

   DBG("TRI daemon add and monitor item %s (%d) at daemon 0x%x\n",
       name, result, itc_current_mbox());

   if (result == TRI_DAEMON_OK)
   {
      TAILQ_INSERT_TAIL(&(elem->refList), refElem, list);
   }
   else
   {
      itc_unmonitor(monRef);
      free(refElem);
      if (alreadyInList == false)
      {
         free(elem);
      }
   }

   printDaemonList("after allocate");

   /*
    * Only respond if the registered trace item is a thread.
    * Object and interface are registered in OSE_HOOK2 and
    * can therefore not recieve signals.
    */
   if (req->itemType == TRI_ITEMTYPE_THREAD)
   {
      sendDaemonRegisterItemRsp(result, itc_sender(msg));
   }
   /*
    * Update TRI server with information about the new trace item if
    * the registration is successful.
    */
   if ((result == TRI_DAEMON_OK) && (alreadyInList == false))
   {
      sendServerRegisterItemReq(elem->procName, elem->groupMask, req);
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
 *               triServerMbox
 */
/* ===================================================================== */
static void
handleItemMonitorInd(union itc_msg *msg)
{
   struct traceItemRefListElement *elem;
   struct traceItemRefListElement *tmpElem;
   struct refListElement *refElem;
   struct refListElement *tmpRefElem;
   struct ItemInfo *mallocRef;
   char *name;

   mallocRef = msg->triDaemonItemMonitorInd.mallocRef;
   name = &(msg->triDaemonItemMonitorInd.procName[0]);

   DBG("TRI daemon receive monitor ind for item %s at daemon 0x%x\n",
        name, itc_current_mbox());

   sendServerItemKilledInd(name);

   printDaemonList("before free");

   /*
    * A client (thread) has died. Delete the element from the
    * trace item list and delete the trace item struct allocated
    * at registration.
    */
   for (elem = TAILQ_FIRST(&traceItemRefList); elem != NULL; elem = tmpElem)
   {
      tmpElem = TAILQ_NEXT(elem, list);

      DBG1(LOG_INFO, "TRI daemon remove compare item %s (%s)\n",
           name, elem->procName);

      if (strcmp(elem->procName, name) == 0)
      {
         for (refElem = TAILQ_FIRST(&elem->refList); refElem != NULL; refElem = tmpRefElem)
         {
            tmpRefElem = TAILQ_NEXT(refElem, list);

            DBG1(LOG_INFO, "TRI daemon remove compare %p (%p)\n",
                 mallocRef, refElem->mallocRef);

            if ((refElem->mallocRef) && (refElem->mallocRef == mallocRef))
            {
               DBG1(LOG_INFO, "TRI daemon remove item %s (%p) at daemon 0x%x\n",
                    name, refElem->mallocRef, itc_current_mbox());
               /*
                * Free the heap memory befor setting pointer to NULL
                * and the pointer to this data
                */
               free(refElem->mallocRef);
               refElem->mallocRef = NULL;

               /*
                * Delete the refernce element from the reference list.
                */
               TAILQ_REMOVE(&elem->refList, refElem, list);
               free(refElem);

               /*
                * Remove the trace item if no referencies exist in reference list.
                */
               if TAILQ_EMPTY(&(elem->refList))
               {
                  TAILQ_REMOVE(&traceItemRefList, elem, list);
                  free(elem);
               }
               return;
            }
         }
      }
   }
   syslog(LOG_ERR, "TRI daemon could not deregister thread %s "
          "pointer=%p, daemon=0x%x", name, mallocRef,
          itc_current_mbox());
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
   struct refListElement *refElem;

   /* Search for the item and update the group mask */
   TAILQ_FOREACH(elem, &traceItemRefList, list)
   {
      if ((strcmp(elem->procName,
                  msg->triServerUpdateFilterInd.itemName)) == 0)
      {
         /* Make the update for all references trace items */
         TAILQ_FOREACH(refElem, &elem->refList, list)
         {
            if (msg->triServerUpdateFilterInd.compiledFilter[0] != 0)
            {
               if ((*(refElem->ref))->busLogFilter == NULL)
               {
                  (*(refElem->ref))->busLogFilter = malloc((sizeof(int) *
                                                          MAX_FILTER_LEN));
                  if ((*(refElem->ref))->busLogFilter == NULL)
                  {
                     syslog(LOG_ERR, "TRI daemon %s: %s (%d).",
                            __FUNCTION__, strerror(errno), errno);
                     return;
                  }

                  memcpy((char *)(*(refElem->ref))->busLogFilter,
                         (char *)msg->triServerUpdateFilterInd.compiledFilter,
                         MAX_FILTER_LEN * sizeof(int));
               }
            }
            else
            {
               if ((*(refElem->ref))->busLogFilter != NULL)
               {
                  free((*(refElem->ref))->busLogFilter);
               }
               (*(refElem->ref))->busLogFilter = NULL;
            }
         }
      }
   }
}

/* ===================================================================== */
/**
 *   This thread will receive a message in the pipe. When message
 *   is received a TRI_DAEMON_ITEM_MONITOR_IND is sent to parent.
 *
 *   @param -    -
 *
 *   @return    -
 *
 *   @par Globals:
 *              triDaemonMbox
 */
/* ===================================================================== */
static void*
triPipeHandler(void *arg)
{
   int size = sizeof(struct threadCleanupData);
   char *buf;
   struct ItemInfo *mallocRef;
   char *procName;
   itc_mbox_id_t mbox;
   int ret;

   ret = pipe(__tri_cleanup_fds);
   if (ret)
   {
     syslog(LOG_ERR, "%s: Failed to create pipe, pid=%d, errno=%d\n",
            __func__, (int)getpid(), errno);
     abort();
     return NULL;
   }

   buf = (char*)malloc(size);
   if (buf == NULL)
    {
       syslog(LOG_ERR, "%s: Failed to do malloc, pid=%d, errno=%d",
             __func__, (int)getpid(), errno);
       abort();
       return NULL;
    }

    mbox = itc_create_mailbox(TRI_DAEMON_CLEANUP_MAILBOX_NAME, 0);

    /* Wait for messages. */
    while(1)
    {
      ret = read(__tri_cleanup_fds[0], buf, size);

      if (ret < 0)
      {
        if (errno == EINTR)
          continue;
        if (errno != 0)
        {
          syslog(LOG_ERR, "%s: Failed to read() , pid=%d errno=%d\n",
                 __func__, (int)getpid(), -errno);
          abort();
          return NULL;
        }
      }
      else if (ret == 0)
      {
        continue;
      }

      mallocRef = ((struct threadCleanupData*)buf)->mallocRef;
      procName = ((struct threadCleanupData*)buf)->procName;
      syslog(LOG_INFO, "mallocRef=%p procName=%s", mallocRef, procName);

      /* Send indication to TRI daemon thread */
      sendMonitorInd(mallocRef, procName);
    }

    free(buf);
    itc_delete_mailbox(mbox);
    return NULL;
}

/* ===================================================================== */
/**
 *   This function handles initialization for TRI daemon.
 *
 *   @param -
 *
 *   @return    -
 *
 *   @par Globals:
 *              traceItemRefList, savedGroupMaskList,
 *              triServerMbox, triDaemonMbox
 */
/* ===================================================================== */
static void
initTriDaemon(void)
{
   union itc_msg *sendMsg;
   /*The extra 32 bytes in the below statement is to be able to store a
     64bit string representation of a pid*/
   char name[sizeof(TRI_DAEMON_MAILBOX_NAME) + 32 + 1];
   pthread_t tid;
   int ret;

   /* Open syslog for logging */
   openlog("TRI_DAEMON", 0, LOG_DAEMON);

   /* Create an ITC mailbox. Each daemon must have a unique mailbox
    * name as linx does not first choose the one in calling process. Use
    * process pid to differentiate the mailboxes.
    */
   snprintf(name, sizeof(name) - 1, "%s_%u",
            TRI_DAEMON_MAILBOX_NAME, getpid());

   /* Do not delete mail box as this is referenced in tri server. */
   if ((triDaemonMbox = itc_current_mbox()) == ITC_NO_ID)
      triDaemonMbox = itc_create_mailbox(name, 0);

   /*
    * Create a thread which handles message queue for killed thread
    * nofications.
    */
   ret = pthread_create(&tid, NULL, triPipeHandler, NULL);
   if (ret != 0) {
      syslog(LOG_ERR,
             "Failed to create TRI message queue thread, ret=%d", ret);
      abort();
   }

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
