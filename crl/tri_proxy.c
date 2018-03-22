/**
 *   TRI proxy code for the TRI macros.
 *
 *   Copyright (C) 2013-2015 by Ericsson AB. All rights reserved. The
 *   information in this document is the property of Ericss2on. Except
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
 *   Revised : 2013-04-10 Anette Schött
 *   Change  : First version.
 *
 *   Revised : 2013-07-05 Anette Schött
 *   Change  : SMBB bug 260, each tri daemon mailbox need to have a uniqe
 *             name as itc_locate will not locate the local mailbox in the
 *             calling process.
 *
 *   Revised : 2013-08-16 Anette Schött
 *   Change  : Corrected syslog print-outs. TRI bus traces do not work as
 *             expected.
 *
 *   Revised : 2013-10-29 Niranjan Kumar
 *   Change  : process name and interface name clubbed, so as file and line
 *             interface or object as maximum number of arguments is 20 for
 *             tracepoint function.
 *
 *   Revised : 2013-12-02 Marcus Rosendahl
 *   Change  : Extended the mail_box name to support 32bytes string length
 *
 *   Revised : 2013-12-10 M. Winberg
 *   Change  : Changed defaultItemInfo, added field for altGroupMask.
 *             Added deregistration function.
 *             Added deprecated OMCSF functions. They only call their
 *             lttngXXX equivalents. This in order to force deprecated
 *             warnings at build from the __attribute__ in their
 *             declarations in include file te_handlers.h.
 *
 *   Revised : 2014-01-14 M. Winberg
 *   Change  : Added missing deprecated OMCSF_deRegisterInterface and
 *             OMCSF_registerInterface which only calls its Linux
 *             equivalent deregisterIfObj and registerIfObj respectively.
 *             Added deprecated OMCSF_logTrace and OMCSF_logBusTrace which
 *             call their Linux equivalents.
 *
 *   Revised : 2014-02-27 Anette Schött
 *   Change  : Changed name from deregisterIfObj to deRegisterIfObj.
 *
 *   Revised : 2014-04-10 Stanislav Vovk
 *   Change  : Added OMCSF_sendObjLogTrace and OMCSF_recObjLogTrace
 *             in Linux part.
 *
 *   Revised : 2014-04-24 Stanislav Vovk
 *   Change  : Fixed thread concurrency issue. Now using thread local memory
 *             to construct strings which are input to tracepoint().
 *             This fixes concurrent writes of uninitialized threads(i.e.
 *             those which dont include cello_te_ose.h). This
 *             in turn lead to complete LTTng buffer corruption for all
 *             users on same system.
 *
 *   Revised : 2014-08-28 Stanislav Vovk
 *   Change  : If there is no TRI local thread object registration will be
 *             aborted.
 *
 *   Revised : 2014-09-26 Anette Schött
 *   Change  : Added support for TSL usages of TRI.
 *
 *   Revised : 2014-11-24 K.V.Ranganadh
 *   Change  : Added support for bus filter handling
 *
 *   Revised : 2014-11-10 Anette Schött
 *   Change  : Added support for REC_SIG and SEND_SIG in lttngTriIfObjTrace.
 *
 *   Revised : 2015-01-12 Anette Schött
 *   Change  : Added support for CHECK for lttngTriIfObjTrace and
 *             OMCSF_logObjTrace.
 *
 *   Revised : 2015-01-26 Anette Schött
 *   Change  : Add support for both traceObjInfo ## OBJNAME and
 *             OMCSF_traceObjInfo ## OBJNAME ## _p reference name as
 *             arguments in registerIfObj.
 *
 *   Revised : 2015-01-31 Anette Schött
 *   Change  : Add missing deallocation of trace item struct in
 *             deRegisterIfObj.
 *
 *   Revised : 2015-04-02 Anette Schött
 *   Change  : Change to send pointer to pointer of struct
 *             ItemInfo instead for pointer to struct ItemInfo to daemon.
 *
 *   Revised : 2015-04-22 Anette Schött
 *   Change  : Added usage of mallocRef to be used when freeing the heap
 *             memory allocated at registration of a thread.
 *
 *   Revised : 2015-04-28 Anette Schött
 *   Change  : Add support for OMCSF_raiseUnexpectedSig in Linux.
 *
 *   Revised : 2015-05-21 Anette Schött
 *   Change  : Revert the struct ItemInfo back to not break legacy.
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
 *   Revised : 2015-09-14 Fredrik Skog
 *   Change  : Added possibility to set scheduling policy and priority for
 *             TRI daemon thread. (G2 WP4686)
 *
 *   Revised : 2015-10-14 Ranganadh, Niranjan
 *   Change  : Added instance handling support in TRI.
 *
 *   Revised : 2015-11-02 Anette Schött
 *   Change  : Removed the deprectaed notes from some OMCSF_xxx functions
 *             as they now are officially exported.
 *
 *   Revised : 2015-10-30 Anette Schött
 *   Change  : Changed to a no LITS implementation and to not rely on that
 *             a client has ITC mailbox created. triDaemon startup and
 *             configuration is moved to new file tri_tace.c. Note that now
 *             is the thread name registered to TRI limited to 16 characters.
 *             prctl has this limitation.
 *
 *   Revised : 2015-11-18 Anette Schött
 *   Change  : Corrected the freeing of trace item struct in
 *             deRegisterIfObj.
 *
 *   Revised : 2015-11-23 Anette Schött
 *   Change  : Remove limitation of 15 characters for trace item name for
 *             a thread. Changed casting from * to ** for itemPtr1/2 in
 *
 *   Revised : 2015-11-18 Anette Schött
 *   Change  : Corrected the freeing of trace item struct in
 *             deRegisterIfObj.
 *
 *   Revised : 2015-12-01 Anette Schött
 *   Change  : Set client pointer to NUll after freeing it and changed the
 *             checking of which pointer can be set to null before freeing
 *             the trace item struct at deregistration of trace object.
 * ========================================================================
 */

/* ========================================================================
 *   INCLUDE FILES
 * ========================================================================
 */

#include <stdint.h>
#include <stdlib.h>
#include <sys/types.h>
#include <unistd.h>
#include <string.h>
#include <stdarg.h>
#include <malloc.h>
#include <errno.h>
#include <syslog.h>
#include <pthread.h>
#include <ctype.h>
#include <sys/prctl.h>

#define TRACEPOINT_DEFINE
#include "tri_ust_thread.h"
#include "tri_ust_obj_if.h"

#include "itc.h"
#include "tri_daemon.h"
#include "cello_te_handlers.h"
#include "cello_te_group.h"
#include "tri_osetypes.h"

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

#define TRI_PROXY_MAILBOX_NAME "triProxyMbox"
/*
 ** Maximum size of the evaluation stack
 */
#define STACK_SIZE     10

#define CRL_INSTANCE_ID_ENV "ITC_NAMESPACE"

/*
** The different kind of symbols in an infix or RPN expression
*/
typedef enum
{
  TOK_NONE,
  TOK_NULL,
  TOK_NUM,
  TOK_LEN,
  TOK_LEFT_BRACKET,
  TOK_RIGHT_BRACKET,
  TOK_LEFT_PAR,
  TOK_RIGHT_PAR,
  TOK_COLON,
  TOK_RANGE,
  TOK_AND,
  TOK_OR,
  TOK_LESS,
  TOK_GRT,
  TOK_LESS_EQU,
  TOK_GRT_EQU,
  TOK_EQU,
  TOK_NOT_EQU,
  TOK_ELEMENT
} tokKindE;


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
};

/* ========================================================================
 *   DATA DECLARATIONS
 * ========================================================================
 */

/*
 *   This is a global variable that keeps the process information
 *   structure for all processes that has not registered itselfs
 *   to Trace & Error Handling.
 *
 *   backward compatibility reason. See comment at declaration
 *   of struct ItemInfo in header file.
 *
 */
struct ItemInfo defaultItemInfo =
{
  0,                       /* Signal number when being used as signal */
  0,                       /* Administrative trace information        */
  0,                       /* Not in use - see comment above          */
  TRI_DEFAULT_GROUP_MASK,  /* Mask with enabled trace groups          */
  0,                       /* Mask with raised errors                 */
  NULL,                    /* Compiled bus log filter                 */
  "-",                     /* The name of this process                */
  NULL,                    /* Pointer to be used by client if needed  */
  NULL,                    /* Pointer to trace object struct          */
  NULL                     /* A second pointer to the same trace      */
                           /* object struct pointing to by itemPtr1   */
};

/* Thread private data */
__thread struct ItemInfo *procInfo = (struct ItemInfo*)&defaultItemInfo;

/* Used by thread to store formatted string */
static __thread char fmt_str[TRI_MAX_FORMAT_STR_LEN] = {0};

/* Used by thread to store name of current object */
static __thread ProcAndObjIfName objname = {0};

/* Used by thread to store formatted file and line */
static __thread FileAndLine fileline = {0};

/* Used to indicate if application is a LITS application or not */
static int8_t is_lits_client = -1;

/* ========================================================================
 *   FUNCTION PROTOTYPES
 * ========================================================================
 */

/* ========================================================================
 *   FUNCTIONS
 * ========================================================================
 */

/* ===================================================================== */
 /**
 *   Returns name to of a thread (LITS or non-LITS thread).
 *
 *   @param name          Thread name
 *
 *   @return         TRI_OK at success otherwise TRI_INTERNAL_ERROR.
 *
 *   @par Globals:
 *                   is_lits_client
 */
/* ===================================================================== */
static uint32_t
getThreadName(char *name)
{
   int ret = TRI_INTERNAL_ERROR;

   if (is_lits_client == 1)
   {
      struct OS_pcb* pcb = NULL;
      uint32_t curpid;

      curpid = itc_current_mbox();
      pcb = zzget_pcb(curpid);
      if (pcb)
      {
         snprintf(name, TRI_MAX_PROC_NAME_LEN, "%s",
                  &(pcb->strings[pcb->name]));
         itc_free( (union itc_msg**) &pcb);
         return TRI_OK;
      }
   }
   else
   {
      if ((ret = prctl(PR_GET_NAME, (unsigned long)name, 0, 0, 0)) == 0)
      {
         return TRI_OK;
      }
   }
   return ret;
}

/* ===================================================================== */
 /**
 *   Returns the instance identity for the program instance. Checks in
 *   the envrionment varibles for ITC_NAMESPACE.
 *   
 *
 *   @param pid      Process identity to get instance identity for
 *
 *   @return         The instance identity or zero
 *
 *   @par Globals:
 *                   -
 */
/* ===================================================================== */
static uint32_t
getInstanceId(pid_t pid)
{
  uint32_t instanceId = 0;
  char *idStr = NULL;

    /* idStr should not be freed/modified as it points to array of
     * envrionment varibles
     */
    idStr = getenv(CRL_INSTANCE_ID_ENV);
    if (idStr)
    {
      instanceId = strtoul(idStr, NULL, 0);
      if (errno == EINVAL)
      {
        syslog(LOG_ERR, "Failed to convert instance id %s for 0x%x",
                        idStr, pid);
      }
      else if (errno == ERANGE)
      {
        /* Ending up in this scenario is highly improbable
         */
        syslog(LOG_ERR, "Instance id out of range %s for 0x%x",
                        idStr, pid);
        instanceId = 0;
      }
    }

  return instanceId;
}

/* ===================================================================== */
 /**
 *   Returns a concatenated name of instance identity and thread name.
 *
 *   @param instanceId    Instance identity
 *
 *   @param name          Thread name
 *
 *   @param itemName      Concatenated name
 *
 *   @return         TRI_OK
 *
 *   @par Globals:
 *                   -
 */
/* ===================================================================== */
static uint32_t
setTraceItemName(uint32_t instanceId, char *name, char *itemName)
{
   if (instanceId != 0)
   {
      snprintf(itemName, TRI_MAX_PROC_NAME_LEN, "%d/%s",
               instanceId, name);
   }
   else
   {
      snprintf(itemName, TRI_MAX_PROC_NAME_LEN, "%s",
               name);
   }
   return TRI_OK;
}

/* ===================================================================== */
/**
 *   This function acts as filter and  evaluates the data from BUS_RECEIVE
 *   and BUS_SEND by a thread trace item
 *
 *   @param busLogFilter      This is the compiled Bus filter
 *   @param data              Data to trace
 *   @param len               Length of data
 *
 *   @return         True   :If busLogFilter is NULL or data satisfies the
 *                           filter conditions
 *                   False  :If data doesnot satify the filter conditions
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
static Boolean
filterEvaluate(int *busLogFilter,
               uint8_t *data,
               uint16_t len)
{
   int *i;
   int  index;
   int  startBit;
   int  stopBit;
   int  mask;
   int  value;
   int  stack[STACK_SIZE];
   int *stack_p = stack;


   /** evaluate the busfilter and send the result **/
   *stack_p = 0;
   i = busLogFilter;
   while (*i)
   {
       switch (*i++)
       {
         case TOK_LEN:
           *++stack_p = (int)len;
           break;

         case TOK_ELEMENT:
           index    = *i++;
           startBit = *i++;
           stopBit  = *i++;
           if (index >= (int)len)
           {
             *++stack_p = 0;
           }
           else
           {
             mask = (1 << (stopBit + 1)) - 1;
             *++stack_p = (data[index] & mask) >> startBit;
           }
           break;

         case TOK_NUM:
           *++stack_p = *i++;
           break;

         case TOK_LESS:
           value = *stack_p--;
           *stack_p = *stack_p < value;
           break;

         case TOK_GRT:
           value = *stack_p--;
           *stack_p = *stack_p > value;
           break;

         case TOK_LESS_EQU:
           value = *stack_p--;
           *stack_p = *stack_p <= value;
           break;

         case TOK_GRT_EQU:
           value = *stack_p--;
           *stack_p = *stack_p >= value;
           break;

         case TOK_EQU:
           value = *stack_p--;
           *stack_p = *stack_p == value;
           break;

         case TOK_NOT_EQU:
           value = *stack_p--;
           *stack_p = *stack_p != value;
           break;

         case TOK_AND:
           value = *stack_p--;
           *stack_p = value && *stack_p;
           break;

         case TOK_OR:
           value = *stack_p--;
           *stack_p = value || *stack_p;
           break;

         default:
           syslog(LOG_ERR, "Illegal op: %d", *(i - 1));
       }
   }

   return (Boolean)*stack_p;
}

/* ===================================================================== */
/**
 *   Handles evaluation of the filter along with filter data
 *
 *   @param procName   Name of the thread to be registered
 *
 *   @return           True : if busLogFilter is NULL or
 *                            busLogFilter is set and data satisfy
 *                            filter condition.
 *                     False: if busLogfilter is set and data doesnot
 *                            satisfy filter condition.
 *
 *   @par Globals:
 *
 */
/* ===================================================================== */
static Boolean
lttgTriBusFilterEval(int *busLogFilter,
                     uint8_t *data,
                     uint16_t len)
{
   if (busLogFilter == NULL)
   {
      return True;
   }
   else
   {
      return filterEvaluate(busLogFilter, data, len);
   }
}

/* ===================================================================== */
/**
 *   Handles registration of the trace item thread.
 *
 *   @param procName   Name of the thread to be registered
 *
 *   @return           -
 *
 *   @par Globals:
 *                     procInfo, defaultItemInfo
 */
/* ===================================================================== */
void
initPriProc(const char *procName)
{
   itc_mbox_id_t mbox;
   itc_mbox_id_t daemonMbox = ITC_NO_ID;
   union itc_msg *msg, *recMsg;
   uint32_t rxFilter[] = {1, TRI_DAEMON_REGISTER_ITEM_RSP};
   /*The extra 32 bytes in the below statement is to be able to store a
     64bit string representation of a pid*/
   char name[sizeof(TRI_DAEMON_MAILBOX_NAME) + 32 + 1];
   char lits_name[80];
   int is_itc_mb = 0;
   uint32_t instanceId;
   char tid_name[TRI_MAX_PROC_NAME_LEN];
   int ret;
   int print = 0;

   /* 
    * Create an ITC mailbox. Must be done before any ITC functionallity
    * is used as the client might not have a mailbox.
    */
   if ((mbox = itc_current_mbox()) == ITC_NO_ID) {
      mbox = itc_create_mailbox(TRI_PROXY_MAILBOX_NAME, 0);
   } else {
      is_itc_mb = 1;
   }

   /*
    * Set client global variable to indicate if this is a LITS
    * application or not.
    */
   if (is_lits_client == -1)
   {
      sprintf(lits_name, "lits_daemon_%d", getpid());
      if (itc_locate(lits_name) != ITC_NO_ID)
      {
         is_lits_client = 1;
      }
      else
      {
         is_lits_client = 0;
      }
   }

   /*
    * Locate the TRI daemon mailbox. Each daemon has a unique mailbox
    * name as linx does not first choose the one in calling process.
    * Use process pid to differentiate the mailboxes.
    */
   snprintf(name, sizeof(name) - 1, "%s_%u",
            TRI_DAEMON_MAILBOX_NAME, getpid());

   while((daemonMbox = itc_locate(name)) == ITC_NO_ID)
   {
      sleep(1);
      if (!print)
      {
         syslog(LOG_INFO, "TRI proxy initPriProc(%s): Tries to locate TRI daemon",
                procName);
         print++;
      }
   }
   if (print)
   {
      syslog(LOG_INFO, "TRI proxy initPriProc(%s): TRI daemon located (retries=%d)",
             procName, print);
   }

   /* Allocate and set the trace item */
   procInfo = (struct ItemInfo*)malloc(sizeof(struct ItemInfo));
   if (!procInfo)
   {
      syslog(LOG_ERR, "TRI proxy %s: %s (%d).",
             __FUNCTION__, strerror(errno), errno);
      if (mbox != ITC_NO_ID)
         itc_delete_mailbox(mbox);
      return;
   }

   *procInfo = defaultItemInfo;
   procInfo->groupMask = TRI_DEFAULT_GROUP_MASK;

   if ((ret = getThreadName(&(tid_name[0]))) != TRI_OK)
   {
      syslog(LOG_ERR, "TRI proxy %s: failed to get thread name for "
             "%s (%d).", __FUNCTION__, procName, ret);
      return;
   }

   instanceId = getInstanceId(getpid());

   if ((ret = setTraceItemName(instanceId,
                               &(tid_name[0]),
                               procInfo->procName)) != TRI_OK)
   {
      syslog(LOG_ERR, "TRI proxy %s: failed to set thread name for "
             "%s (%d).", __FUNCTION__, procName, ret);
      return;
   }

   if (triAllocAndSetThreadCleanupData(procInfo, daemonMbox))
   {
      return;
   }

   /* Register the trace item to TRI daemon */
   msg = itc_alloc(sizeof(TriDaemonRegisterItemReq),
                   TRI_DAEMON_REGISTER_ITEM_REQ);
   msg->triDaemonRegisterItemReq.itemType = TRI_ITEMTYPE_THREAD;
   msg->triDaemonRegisterItemReq.traceItemRef = &procInfo;
   msg->triDaemonRegisterItemReq.mallocRef = procInfo;
   strncpy(msg->triDaemonRegisterItemReq.procName, procInfo->procName,
           TRI_MAX_PROC_NAME_LEN);
   msg->triDaemonRegisterItemReq.procName[TRI_MAX_PROC_NAME_LEN - 1] = '\0';
   msg->triDaemonRegisterItemReq.groupMask = TRI_DEFAULT_GROUP_MASK;
   itc_send(&msg, daemonMbox, mbox);

   recMsg = itc_receive(rxFilter, ITC_NO_TMO, ITC_FROM_ALL);
   if(recMsg->triDaemonRegisterItemRsp.result != TRI_DAEMON_OK)
   {
      syslog(LOG_ERR, "TRI proxy initPriProc: Failed to register thread %s to daemon, "
             "result %d", procName, recMsg->triDaemonRegisterItemRsp.result);
   }
   itc_free(&recMsg);

   if (!is_itc_mb) {
      itc_delete_mailbox(mbox);
   }
}

/* ===================================================================== */
/**
 *  Register a name for the current thread.
 *  Optional. This is similar to the process name in LITS/ITS. If you don't use
 *  this function then the name and group mask from the "default object" will be
 *  used when tracing.
 *
 *  @param  procName   Name of the thread to be registered
 *
 *   @return           -
 *
 *   @par Globals:
 *                     -
 */
/* ===================================================================== */
void
tri_register_thread_name(const char *procName)
{
   initPriProc(procName);
}

/* ===================================================================== */
/**
 *   Handles deregistration of the trace item interface or object.
 *
 *   See header description for registerIfObj for a more detailed
 *   description of how the pointer referencies are handled.
 *
 *   @param ref        Reference to interface or object information struct
 *
 *   @return           -
 *
 *   @par Globals:
 *                     defaultItemInfo
 */
/* ===================================================================== */
void
deRegisterIfObj(struct ItemInfo **ref)
{
   itc_mbox_id_t mbox;
   itc_mbox_id_t daemonMbox = ITC_NO_ID;
   union itc_msg *msg;
   /*The extra 32 bytes in the below statement is to be able to store a
     64bit string representation of a pid*/
   char name[sizeof(TRI_DAEMON_MAILBOX_NAME) + 32 + 1];
   int is_itc_mb = 0;
   struct ItemInfo **itemPtr1;
   struct ItemInfo **itemPtr2;

   if (*ref == NULL) {
     syslog(LOG_ERR, "TRI proxy deregisterIfObj received null pointer");
     return;
   }

   /* Create an ITC mailbox */
   if ((mbox = itc_current_mbox()) == ITC_NO_ID) {
       mbox = itc_create_mailbox(TRI_PROXY_MAILBOX_NAME, 0);
   } else {
      is_itc_mb = 1;
   }

   /*
    * Locate the TRI daemon mailbox. Each daemon has a unique mailbox
    * name as linx does not first choose the one in calling process.
    * Use process pid to differentiate the mailboxes.
    */
   snprintf(name, sizeof(name) - 1, "%s_%u",
            TRI_DAEMON_MAILBOX_NAME, getpid());
   while((daemonMbox = itc_locate(name)) == ITC_NO_ID)
   {
      sleep(1);
      syslog(LOG_INFO, "TRI proxy deregisterIfObj(%s): Try to locate TRI daemon",
             (*ref)->procName);
   }

   /* Deregister the trace item to TRI daemon.
    */
   msg = itc_alloc(sizeof(TriDaemonDeregisterItemReq),
                   TRI_DAEMON_DEREGISTER_ITEM_REQ);
   /* Note, ref shall only be used by daemon to search for the
    * trace item in it's internal list.The referenced memory is
    * fred last in this function.
    */
   msg->triDaemonDeregisterItemReq.traceItemRef = ref;
   strncpy(msg->triDaemonDeregisterItemReq.procName, (*ref)->procName,
           TRI_MAX_PROC_NAME_LEN);
   msg->triDaemonDeregisterItemReq.procName[TRI_MAX_PROC_NAME_LEN - 1] = '\0';
   itc_send(&msg, daemonMbox, mbox);

   if (!is_itc_mb) {
      itc_delete_mailbox(mbox);
   }

   /* Deregister has one of the pointers stored in item struct as
    * in parameter,i.e. ref points to the same pointer as itemPtr1 or
    * itemPtr2. All pointers point to the same item struct.
    * Free the struct and set pointers to NULL.
    * Set the second pointer to NULL might not be needed, depends
    * on how the trace object was registered but clear it anyway).
    */
   itemPtr1 = (struct ItemInfo**)(*ref)->itemPtr1;
   itemPtr2 = (struct ItemInfo**)(*ref)->itemPtr2;

   DBG("TRI proxy free object %s (ref=%p *ref=%p itemPtr1=%p itemPtr2=%p size=%d)\n",
       (*ref)->procName, ref, *ref, itemPtr1, itemPtr2, (int)sizeof(**ref));
   /*
    * The memory pointer referenced in function call shall be fred.
    * Set the pointer not referenced in functiona call to NULL.
    * t
    */
   if (itemPtr1 && (itemPtr1 != ref))
   {
      DBG("Set itemRef1 %p (ref=%p) to NULL\n", *itemPtr1, *ref);
      *itemPtr1 = NULL;
   }
   if (itemPtr2 && (itemPtr2 != ref))
   {
      DBG("Set itemRef2 %p (ref=%p) to NULL\n", *itemPtr2, *ref);
      *itemPtr2 = NULL;
   }

   /*
    * Free the trace item struct memory.
    */
   DBG("TRI proxy free (ref=%p size=%d)\n",
       *ref, (int)sizeof(**ref));

   if (*ref != &defaultItemInfo)
   {
      free(*ref);
      *ref = NULL;
   }
}

/* ===================================================================== */
/**
 *   Handles deregistration of the trace item interface or object.
 *   This is a deprecated function, it only calls its equivalent in the
 *   Linux implementation of TRI.
 *
 *
  *   @param traceObj_p   Reference to interface or object information struct
 *
 *   @return           -
 */
/* ===================================================================== */
void
OMCSF_deRegisterInterface(struct OMCSF_procInfoS  **traceObj_p)
{

   deRegisterIfObj(traceObj_p);

}

/* ===================================================================== */
/**
 *   Handles registration of the trace item interface or object.
 *
 *   Input to this function can be either one or two pointer referencies,
 *   traceObjInfo ## OBJNAME and/or OMCSF_traceObjInfo ## OBJNAME ## _p.
 *   These pointers shall point to the same trace item struct after
 *   registration and the reason for this is backward compatible reasons.
 *   At deregistration of the trace items, one of these pointers are
 *   provided, dependent on how the registration was done by the application.
 *   The deregistration function shall free that memory, referenced in the
 *   input to the deregistration function. The other pointer shall be set to
 *   NULL.
 *
 *   @param itemName   Name of the interface or object to be registered
 *
 *   @param ref        Reference or references to interface and object
 *                     information struct.
 *                     If set to NULL indicates that both
 *                     traceObjInfo ## OBJNAME and
 *                     OMCSF_traceObjInfo ## OBJNAME ## _p references
 *                     are provided as function arguments.
 *                     If not set to NULL, only one of the pointer
 *                     references is provided as argument.
 *
 *   @return           -
 *
 *   @par Globals:
 *                     defaultItemInfo
 */
/* ===================================================================== */
void
registerIfObj(const char *itemName,
              struct ItemInfo **ref,
               ...)
{
   itc_mbox_id_t mbox;
   itc_mbox_id_t daemonMbox = ITC_NO_ID;
   union itc_msg *msg;
   /*The extra 32 bytes in the below statement is to be able to store a
     64bit string representation of a pid */
   char name[sizeof(TRI_DAEMON_MAILBOX_NAME) + 32 + 1];
   int is_itc_mb = 0, loc_cnt = 0;
   va_list ap;
   /* Stores a reference to interface and object information
    * struct if a second reference is provided as argument */
   struct ItemInfo **ref2 = NULL;
   int instanceId = 0;

   /*
    * Create an ITC mailbox. Do ITC initialization as this might be
    * a non-LITS process.
    */
   if ((mbox = itc_current_mbox()) == ITC_NO_ID) {
      mbox = itc_create_mailbox(TRI_PROXY_MAILBOX_NAME, 0);
   } else {
      is_itc_mb = 1;
   }

   /*
    * Locate the TRI daemon mailbox. Each daemon has a unique mailbox
    * name as linx does not first choose the one in calling process.
    * Use process pid to differentiate the mailboxes.
    */
   snprintf(name, sizeof(name) - 1, "%s_%u",
            TRI_DAEMON_MAILBOX_NAME, getpid());
   while((daemonMbox = itc_locate(name)) == ITC_NO_ID)
   {
      sleep(1);
      syslog(LOG_INFO, "TRI proxy registerIfObj(%s): Try to locate TRI daemon",
             itemName);
      if (++loc_cnt > 20) {
         /* This object will be discarded, since TRI is initiated incorrectly */
         syslog(LOG_ERR, "Failed to register TRI object '%s'."
                "TRI local daemon thread not started. You probably need to include"
                "'tri.con' in your 'osemain.con' to start it.",
                itemName);
         goto out;
      }
   }

   /* Read the first and second refernce to interface and object
    * struct when 'ref' argument is set to NULL. Otherwise the
    * single reference is provided in 'ref' argument.
    */
   if (ref == NULL)
   {
     va_start(ap, ref);
     ref = va_arg(ap, struct ItemInfo **);
     ref2 = va_arg(ap, struct ItemInfo **);
     va_end(ap);
   }

   /* Ensure that a reference is provided */
   if (ref == NULL)
   {
      syslog(LOG_ERR, "No trace item struct declared for this trace item. "
             "Ensure that macro DECLARE_TRACE_OBJ or/and DECLARE_INTERFACE "
             "is declared");
      goto out;
   }

   /* Allocate and initiate the trace item */
   *ref = malloc(sizeof(struct ItemInfo));
   if (!*ref)
   {
      syslog(LOG_ERR, "TRI proxy %s: %s (%d).",
            __FUNCTION__, strerror(errno), errno);
      goto out;
   }

   /* Set default values for the item */
   *(*ref) = defaultItemInfo;

   /* Store the pointer address in the struct as its needed
    * when deregister the trace item. Store address to first
    * provided pointer in itemPtr1.
    */
   ((*ref)->itemPtr1) = (void**)ref;

   /* Second reference is provided, set this to point to the
    * same interface and object struct as first reference.
    * Now either of the refernces can be used by the application
    * for trace logging.
    * Store the address to second pointer in the struct.
    */
   if (ref2)
   {
     *ref2 = *ref;
     (*ref)->itemPtr2 = (void**)ref2;
   }

   instanceId = getInstanceId(getpid());
   if (instanceId != 0)
   {
      snprintf((*ref)->procName, TRI_MAX_PROC_NAME_LEN, "%d/%s", instanceId, itemName);
   }
   else
   {
      snprintf((*ref)->procName, TRI_MAX_PROC_NAME_LEN, "%s", itemName);
   }
   (*ref)->procName[TRI_MAX_PROC_NAME_LEN - 1] = '\0';
   (*ref)->groupMask = TRI_DEFAULT_GROUP_MASK;

   DBG("TRI proxy allocate object %s (ref=%p mallocRef=%p size=%d)\n",
       (*ref)->procName, ref, *ref, (int)sizeof(**ref));

   /* Register the trace item to TRI daemon. No confirmation as the
    * registration is done in an OSE_HOOK2.
    */
   msg = itc_alloc(sizeof(TriDaemonRegisterItemReq),
                   TRI_DAEMON_REGISTER_ITEM_REQ);
   msg->triDaemonRegisterItemReq.itemType = TRI_ITEMTYPE_IFOBJ;
   msg->triDaemonRegisterItemReq.traceItemRef = ref;
   msg->triDaemonRegisterItemReq.mallocRef = *ref;
   strncpy(msg->triDaemonRegisterItemReq.procName, itemName,
           TRI_MAX_PROC_NAME_LEN);
   msg->triDaemonRegisterItemReq.procName[TRI_MAX_PROC_NAME_LEN - 1] = '\0';
   msg->triDaemonRegisterItemReq.groupMask = TRI_DEFAULT_GROUP_MASK;
   itc_send(&msg, daemonMbox, mbox);

 out:
   if (!is_itc_mb) {
      itc_delete_mailbox(mbox);
   }
}

/* ===================================================================== */
/**
 *   Handles registration of the trace item interface and object.
 *   This is a deprecated function, it only calls its equivalent in the
 *   Linux implementation of TRI.
 *
 *   @param interfaceName
 *                         Name of the interface or object  to be registered
 *
 *   @param interfaceInfo_pp
 *                         Reference to interface and object information struct
 *
 *   @return           -
 */
/* ===================================================================== */
void
OMCSF_registerInterface(const char *interfaceName,
                        struct OMCSF_procInfoS **interfaceInfo_pp)
{

   registerIfObj(interfaceName, interfaceInfo_pp);

}

/* ===================================================================== */
/**
 *   This function builds a formatted string and stores in the
 *   process information structure for the current process.
 *
 *   @param format    String containing format information
  *
 *   @param ...        Variable number of parameters according to format
 *
 *   @return           A pointer to the formatted string.
 *
 *   @par Globals:
 *                     -
 */
/* ===================================================================== */
char *
formatStr(const char *format, ...)
{
   va_list args;  /* Argument list */

   va_start(args, format);
   vsnprintf(fmt_str, TRI_MAX_FORMAT_STR_LEN, format, args);
   va_end(args);

   return fmt_str;
}

/* ===================================================================== */
/**
 *   This function builds a formatted string and stores in the
 *   process information structure for the current process.
 *   This is a deprecated function, it only calls its equivalent in the
 *   Linux implementation of TRI.
 *
 *
 *   @param format    String containing format information
 *
 *   @param ...        Variable number of parameters according to format
 *
 *   @return           A pointer to the formatted string.
 *
 *   @par Globals:
 *                     -
 */
/* ===================================================================== */
char *
OMCSF_formatStr(const char *format, ...)
{
   va_list args;  /* Argument list */

   va_start(args, format);
   vsnprintf(fmt_str, TRI_MAX_FORMAT_STR_LEN, format, args);
   va_end(args);

   return fmt_str;
}

/* ===================================================================== */
/**
 *   This function traces data for a thread trace item.
 *
 *   @param group      Group mask for this function call
 *   @param file       Name of file where this function was called
 *   @param line       Line in file where this function was called
 *   @param msg        String to trace
 *   @param procInfo   Pointer to thread specific data
 *
 *   @return           Always True.
 *
 *   @par Globals:
 *                     -
 */
/* ===================================================================== */
Boolean
lttngTriTrace(uint32_t group,
              const char *file,
              uint16_t line,
              const char *msg,
              struct ItemInfo *procInfo)
{

   snprintf(fileline, TRI_MAX_FILE_AND_LINE_LEN,
            "%s:%d", file, line);

   switch (group)
   {
      case GROUP_CHECK:
      {
         tracepoint(com_ericsson_trithread, CHECK, procInfo->procName,
                    strlen(procInfo->procName),
                    fileline, strlen(fileline),
                    msg, strlen(msg));
         break;
      }
      case GROUP_ERROR:
      {
         tracepoint(com_ericsson_trithread, ERROR, procInfo->procName,
                    strlen(procInfo->procName),
                    fileline, strlen(fileline),
                    msg, strlen(msg));
         break;
      }
      case GROUP_ENTER:
      {
         tracepoint(com_ericsson_trithread, ENTER, procInfo->procName,
                    strlen(procInfo->procName),
                    fileline, strlen(fileline),
                    msg, strlen(msg));
         break;
      }
      case GROUP_RETURN:
      {
         tracepoint(com_ericsson_trithread, RETURN, procInfo->procName,
                    strlen(procInfo->procName),
                    fileline, strlen(fileline)
                    );
         break;
      }
      case GROUP_INFO:
      {
         tracepoint(com_ericsson_trithread, INFO, procInfo->procName,
                    strlen(procInfo->procName), fileline,
                    strlen(fileline),
                    msg, strlen(msg));
         break;
      }
      case GROUP_TRACE1:
      {
         tracepoint(com_ericsson_trithread, TRACE1, procInfo->procName,
                    strlen(procInfo->procName),
                    fileline, strlen(fileline),
                    msg, strlen(msg));
         break;
      }
      case GROUP_TRACE2:
      {
         tracepoint(com_ericsson_trithread, TRACE2, procInfo->procName,
                    strlen(procInfo->procName),
                    fileline, strlen(fileline),
                    msg, strlen(msg));
         break;
      }
      case GROUP_TRACE3:
      {
         tracepoint(com_ericsson_trithread, TRACE3, procInfo->procName,
                    strlen(procInfo->procName),
                    fileline, strlen(fileline),
                    msg, strlen(msg));
         break;
      }
      case GROUP_TRACE4:
      {
         tracepoint(com_ericsson_trithread, TRACE4, procInfo->procName,
                    strlen(procInfo->procName),
                    fileline, strlen(fileline),
                    msg, strlen(msg));
         break;
      }
      case GROUP_TRACE5:
      {
         tracepoint(com_ericsson_trithread, TRACE5, procInfo->procName,
                    strlen(procInfo->procName),
                    fileline, strlen(fileline),
                    msg, strlen(msg));
         break;
      }
      case GROUP_TRACE6:
      {
         tracepoint(com_ericsson_trithread, TRACE6, procInfo->procName,
                    strlen(procInfo->procName),
                    fileline, strlen(fileline),
                    msg, strlen(msg));
         break;
      }
      case GROUP_TRACE7:
      {
         tracepoint(com_ericsson_trithread, TRACE7, procInfo->procName,
                    strlen(procInfo->procName),
                    fileline, strlen(fileline),
                    msg, strlen(msg));
         break;
      }
      case GROUP_TRACE8:
      {
         tracepoint(com_ericsson_trithread, TRACE8, procInfo->procName,
                    strlen(procInfo->procName),
                    fileline, strlen(fileline),
                    msg, strlen(msg));
         break;
      }
      case GROUP_TRACE9:
      {
         tracepoint(com_ericsson_trithread, TRACE9, procInfo->procName,
                    strlen(procInfo->procName),
                    fileline, strlen(fileline),
                    msg, strlen(msg));
         break;
      }
      case GROUP_STATE_CHANGE:
      {
         tracepoint(com_ericsson_trithread, STATE_CHANGE, procInfo->procName,
                    strlen(procInfo->procName),
                    fileline, strlen(fileline),
                    msg, strlen(msg));
         break;
      }
       case GROUP_REC_SIG:
      {
         tracepoint(com_ericsson_trithread, REC_SIG, procInfo->procName,
                    strlen(procInfo->procName),
                    fileline, strlen(fileline),
                    msg, strlen(msg));
         break;
      }
      case GROUP_SEND_SIG:
      {
         tracepoint(com_ericsson_trithread, SEND_SIG, procInfo->procName,
                    strlen(procInfo->procName),
                    fileline, strlen(fileline),
                    msg, strlen(msg));
         break;
      }
      case GROUP_PARAM:
      {
         tracepoint(com_ericsson_trithread, PARAM, procInfo->procName,
                    strlen(procInfo->procName),
                    fileline, strlen(fileline),
                    msg, strlen(msg));
         break;
      }
      case GROUP_TRACE_OBJ:
      {
         tracepoint(com_ericsson_trithread, TRACE_OBJ, procInfo->procName,
                    strlen(procInfo->procName),
                    fileline, strlen(fileline),
                    msg, strlen(msg));
         break;
      }
      case GROUP_INTERFACE:
      {
        tracepoint(com_ericsson_trithread, INTERFACE, procInfo->procName,
                   strlen(procInfo->procName),
                   fileline, strlen(fileline),
                   msg, strlen(msg));
         break;
      }
      case GROUP_USER1:
      {
        tracepoint(com_ericsson_trithread, USER1, procInfo->procName,
                   strlen(procInfo->procName),
                   fileline, strlen(fileline),
                   msg, strlen(msg));
         break;
      }
      case GROUP_USER2:
      {
        tracepoint(com_ericsson_trithread, USER2, procInfo->procName,
                   strlen(procInfo->procName),
                   fileline, strlen(fileline),
                   msg, strlen(msg));
         break;
      }
      case GROUP_USER3:
      {
        tracepoint(com_ericsson_trithread, USER3, procInfo->procName,
                   strlen(procInfo->procName),
                   fileline, strlen(fileline),
                   msg, strlen(msg));
         break;
      }
      case GROUP_USER4:
      {
        tracepoint(com_ericsson_trithread, USER4, procInfo->procName,
                   strlen(procInfo->procName),
                   fileline, strlen(fileline),
                   msg, strlen(msg));
         break;
      }
      default:
      {
         tracepoint(com_ericsson_trithread, DEFAULT, group);
         break;
      }
   }

   /* Always return True */
   return True;
}

/* ===================================================================== */
/**
 *   This function traces data for a thread trace item.
 *
 *   @param group        Group mask for this function call
 *   @param file         Name of file where this function was called
 *   @param line         Line in file where this function was called
 *   @param msg          String to trace
 *   @param traceObj_p   Pointer to thread specific data
 *
 *   @return             True on Success, else False.
 *
 *   @par Globals:
 *                     -
 */
/* ===================================================================== */
inline Boolean
OMCSF_logTrace(OMCSF_groupE group,
               const char   *file,
               U16          line,
               const char    *msg,
               struct OMCSF_procInfoS *traceObj_p)
{

   return lttngTriTrace(group, file, line, msg, traceObj_p);

}

/* ===================================================================== */
/**
 *   This function traces data for SEND_SIG requested by a thread
 *   trace item.
 *
 *   @param file       Name of file where this function was called
 *   @param line       Line in file where this function was called
 *   @param signo      Signal number for the sent signal
 *   @param recpid     Reciever pid for the sent signal
 *   @param msg        String to trace
 *   @param procInfo   Pointer to thread specific data
 *
 *   @return           Always True.
 *
 *   @par Globals:
 *                     -
 */
/* ===================================================================== */
Boolean
lttngTriSendSigTrace(const char *file,
                     uint16_t line,
                     uint32_t signo,
                     uint32_t recpid,
                     const char *msg,
                     struct ItemInfo *procInfo)
{
   char *msg1;
   char *newMsg = (char *)malloc(strlen(msg) + 1);
   if (!newMsg)
   {
      syslog(LOG_ERR, "TRI proxy %s: %s (%d).",
            __FUNCTION__, strerror(errno), errno);
      return False;
   }
   memcpy(newMsg, msg, strlen(msg) + 1);

   msg1 = formatStr("signo:%x receiver :%x %s",
                    signo, recpid, newMsg);

   snprintf(fileline, TRI_MAX_FILE_AND_LINE_LEN,
            "%s:%d", file, line);

   tracepoint(com_ericsson_trithread, SEND_SIG, procInfo->procName,
              strlen(procInfo->procName),
              fileline, strlen(fileline),
              msg1, strlen(msg1));
   free(newMsg);

   /* Always return True */
   return True;
}

/* ===================================================================== */
/**
 *   This function traces data for REC_SIG requested by a thread
 *   trace item.
 *
 *   @param file       Name of file where this function was called
 *   @param line       Line in file where this function was called
 *   @param sig        Signal pointer for the received signal
 *   @param msg        String to trace
 *   @param procInfo   Pointer to thread specific data
 *
 *   @return           Always True.
 *
 *   @par Globals:
 *                     -
 */
/* ===================================================================== */
Boolean
lttngTriRecSigTrace(const char *file,
                    uint16_t line,
                    uint32_t *sig,
                    const char *msg,
                    struct ItemInfo *procInfo)
{
   char *msg1;
   char *newMsg = (char *)malloc(strlen(msg) + 1);
   if (!newMsg)
   {
      syslog(LOG_ERR, "TRI proxy %s: %s (%d).",
             __FUNCTION__, strerror(errno), errno);
      return False;
   }
   memcpy(newMsg, msg, strlen(msg) + 1);

   msg1 = formatStr("signo:%x sender :%x %s",
                    *sig, itc_sender((union itc_msg*)sig), newMsg);

   snprintf(fileline, TRI_MAX_FILE_AND_LINE_LEN,
           "%s:%d", file, line);

   tracepoint(com_ericsson_trithread, REC_SIG, procInfo->procName,
              strlen(procInfo->procName),
                     fileline, strlen(fileline),
                     msg1, strlen(msg1));
   free(newMsg);

   /* Always return True */
   return True;
}

/* ===================================================================== */
/**
 *   This function traces data for BUS_SEND and BUS_RECEIVE requested
 *   by a thread trace item.
 *
 *   @param procInfo   Pointer to thread specific data
 *   @param group      Group mask for this function call
 *   @param file       Name of file where this function was called
 *   @param line       Line in file where this function was called
 *   @param msg        String to trace
 *   @param data       Data to trace
 *   @param len        Length of data
 *
 *   @return           Always True.
 *
 *   @par Globals:
 *                     -
 */
/* ===================================================================== */
Boolean
lttngTriBusTrace(struct ItemInfo *procInfo,
                 uint32_t group,
                 const char *file,
                 uint16_t line,
                 const char *msg,
                 uint8_t *data,
                 uint16_t len)
{
   /* Limit the total amount of data to log */
   if (len > (16 * 1024))
   {
      syslog(LOG_ERR,
             "TRI proxy lttngTriBusTrace: Data to trace exceed 16kB, "
             "(%d) data will not be logged", len);
      return True;
   }

   if (!lttgTriBusFilterEval((int *)procInfo->busLogFilter, data, len))
   {
      return True;
   }

   snprintf(fileline, TRI_MAX_FILE_AND_LINE_LEN,
           "%s:%d", file, line);

   switch (group)
   {
      case GROUP_BUS_RECEIVE:
      {
         tracepoint(com_ericsson_trithread, BUS_RECEIVE, procInfo->procName,
                    strlen(procInfo->procName),
                    fileline, strlen(fileline),
                    msg, strlen(msg), (char *) data, len);
         break;
      }
      case GROUP_BUS_SEND:
      {
         tracepoint(com_ericsson_trithread, BUS_SEND, procInfo->procName,
                    strlen(procInfo->procName),
                    fileline, strlen(fileline),
                    msg, strlen(msg), (char *) data, len);
         break;
      }
      default:
      {
         tracepoint(com_ericsson_trithread, DEFAULT, group);
         break;
      }
   }

   /* Always return True */
   return True;
}

/* ===================================================================== */
/**
 *   This function traces data for BUS_SEND and BUS_RECEIVE requested
 *   by a thread trace item.
 *
 *   @param procInfo_p   Pointer to thread specific data
 *   @param group        Group mask for this function call
 *   @param file         Name of file where this function was called
 *   @param line         Line in file where this function was called
 *   @param msg          String to trace
 *   @param data         Data to trace
 *   @param lengtn       Length of data
 *
 *   @return             True on Success, else False.
 *
 *   @par Globals:
 *                     -
 */
/* ===================================================================== */
inline Boolean
OMCSF_logBusTrace(struct OMCSF_procInfoS *procInfo_p,
                  OMCSF_groupE    group,
                  const char      *file,
                  U16             line,
                  const  char     *msg,
                  U8              *data,
                  U16             length)
{

   return lttngTriBusTrace(procInfo_p, group, file, line, msg, data, length);

}

/* ===================================================================== */
/**
 *   This function traces data for user time stamp trace requested by
 *   a thread trace item.
 *
 *   @param group      Group mask for this function call
 *   @param sec        User provided time stamp in seconds
 *   @param usec       User provided time stamp in microseconds
 *   @param file       Name of file where this function was called
 *   @param line       Line in file where this function was called
 *   @param procName   Name of thread calling this function
 *   @param msg        String to trace
 *   @param data       Data to trace
 *   @param len        Length of data
 *   @param procInfo   Pointer to thread specific data
 *
 *   @return           Always return True
 *
 *   @par Globals:
 *                     -
 */
/* ===================================================================== */
Boolean
lttngTriUserTrace(uint32_t group,
                  uint32_t sec,
                  uint32_t usec,
                  const char *file,
                  uint16_t line,
                  const char *procName,
                  const char *msg,
                  uint8_t *data,
                  uint16_t len,
                  struct ItemInfo *procInfo)
{
   /* Limit the total amount of data to log */
   if (len > (16 * 1024))
   {
      syslog(LOG_ERR,
             "TRI proxy lttngTriUserTrace: Data to trace exceed 16kB, "
             "(%d) data will not be logged", len);
      return True;
   }

   snprintf(fileline, TRI_MAX_FILE_AND_LINE_LEN,
           "%s:%d", file, line);

   tracepoint(com_ericsson_trithread, TRACE_UTS,
              sec, usec,
              procInfo->procName, strlen(procInfo->procName),
              fileline, strlen(fileline),
              msg, strlen(msg),
              (char *) data, len);

   /* Always return True */
   return True;
}

/* ===================================================================== */
/**
 *   This function raises the specified status in the statusMask
 *   and generates a log entry.
 *
 *   @param status     Error status to raise
 *   @param file       Name of the file where the error occured
 *   @param line       Line in the file on which the error occured
 *   @param msg        A message to log
 *
 *   @return           -
 *
 *   @par Globals:
 *                     -
 */
/* ===================================================================== */
void
triRaiseStatus(uint32_t      status,
               const char    *file,
               uint16_t      line,
               const char    *msg)
{
   /* Set bit in the status field */
   RAISE_STATUS(status);

   if (GROUP_IS_ENABLED(status))
   {
      /* Log the error message if this trace group is enabled */
      (void)lttngTriTrace(status, file, line, msg, procInfo);
   }
}

/* ===================================================================== */
/**
 *   This function raises the specified status in the statusMask
 *   and generates a log entry consisting of the signal number
 *   and the sender of the specified signal.
 *
 *   @param status     Error status to raise
 *   @param file       Name of the file where the error occured
 *   @param line       Line in the file on which the error occured
 *   @param sig        The unexpectedly received signal
 *
 *   @return           -
 *
 *   @par Globals:
 *                     -
 */
/* ===================================================================== */
void
triRaiseUnexpectedSig(uint32_t      status,
                      const char    *file,
                      uint16_t      line,
                      union itc_msg *sig)
{
   char          name[TRI_MAX_PROC_NAME_LEN] = "An unknown process";
   char          *msg;
   itc_mbox_id_t  mbox;

   /*
   ** Set bit in the status field
   */
   RAISE_STATUS(status);

   if (GROUP_IS_ENABLED(status))
   {
      /* Generate and log an error message if this trace group is enabled */
      mbox = itc_sender(sig);
      if (mbox != ITC_NO_ID)
      {
         if(itc_get_name(mbox, name, sizeof(name)) == False)
         {
            syslog(LOG_INFO, "Failed to get mailbox name."
                   "Is this macro called from a thread without ITC?");
         }
      }

      msg = formatStr("Unexpected signal %d received from %s",
                      sig->msgNo, name);

      (void)lttngTriTrace(status, file, line, msg, procInfo);
   }
}

/* ===================================================================== */
/**
 *   This function raises the specified status in the statusMask
 *   and generates a log entry consisting of the signal number
 *   and the sender of the specified signal.
 *   This is a deprecated function, it only calls its equivalent in the
 *   Linux implementation of TRI.
 *
 *   @param status     Error status to raise
 *   @param file       Name of the file where the error occured
 *   @param line       Line in the file on which the error occured
 *   @param sig        The unexpectedly received signal
 *
 *   @return           -
 *
 *   @par Globals:
 *                     -
 */
/* ===================================================================== */
inline void
OMCSF_raiseUnexpectedSig(OMCSF_groupE  status,
                         const char    *file,
                         U16           line,
                         union itc_msg  *sig_p)
{
   triRaiseUnexpectedSig(status, file, line, sig_p);
}

/* ===================================================================== */
/**
 *   This function traces data for a interface or object trace item.
 *
 *   @param group      Group mask for this function call
 *   @param file       Name of file where this function was called
 *   @param line       Line in file where this function was called
 *   @param msg        String to trace
 *   @param procInfo   Pointer to thread specific data
 *   @param ref        Pointer to interface or object specific data
 *
 *   @return           Always return True.
 *
 *   @par Globals:
 *                     -
 */
/* ===================================================================== */
Boolean
lttngTriIfObjTrace(uint32_t group,
                   const char *file,
                   uint16_t line,
                   const char *msg,
                   struct ItemInfo *procInfo,
                   struct ItemInfo *ref)
{

  if (procInfo == ref)
  {
     return lttngTriTrace(group, file, line, msg, procInfo);
  }
  else
  {
     snprintf(fileline, TRI_MAX_FILE_AND_LINE_LEN,
              "%s:%d", file, line);

     snprintf(objname, TRI_MAX_PROC_AND_OBJ_IF_NAME_LEN,
              "%s(%s)", procInfo->procName, ref->procName);

     switch (group)
     {
        case GROUP_CHECK:
        {
           tracepoint(com_ericsson_triobjif, CHECK,
                      objname,
                      strlen(objname),
                      fileline, strlen(fileline),
                      msg, strlen(msg));
           break;
        }
        case GROUP_ERROR:
        {
           tracepoint(com_ericsson_triobjif, ERROR,
                      objname,
                      strlen(objname),
                      fileline, strlen(fileline),
                      msg, strlen(msg));
           break;
        }
        case GROUP_ENTER:
        {
           tracepoint(com_ericsson_triobjif, ENTER,
                      objname,
                      strlen(objname),
                      fileline, strlen(fileline),
                      msg, strlen(msg));
           break;
        }
        case GROUP_RETURN:
        {
           tracepoint(com_ericsson_triobjif, RETURN,
                      objname,
                      strlen(objname),
                      fileline, strlen(fileline));
           break;
        }
        case GROUP_INFO:
        {
           tracepoint(com_ericsson_triobjif, INFO,
                      objname,
                      strlen(objname),
                      fileline, strlen(fileline),
                      msg, strlen(msg));
           break;
        }
        case GROUP_TRACE1:
        {
           tracepoint(com_ericsson_triobjif, TRACE1,
                      objname,
                      strlen(objname),
                      fileline, strlen(fileline),
                      msg, strlen(msg));
           break;
        }
        case GROUP_TRACE2:
        {
           tracepoint(com_ericsson_triobjif, TRACE2,
                      objname,
                      strlen(objname),
                      fileline, strlen(fileline),
                      msg, strlen(msg));
           break;
        }
        case GROUP_TRACE3:
        {
           tracepoint(com_ericsson_triobjif, TRACE3,
                      objname,
                      strlen(objname),
                      fileline, strlen(fileline),
                      msg, strlen(msg));
           break;
        }
        case GROUP_TRACE4:
        {
           tracepoint(com_ericsson_triobjif, TRACE4,
                      objname,
                      strlen(objname),
                      fileline, strlen(fileline),
                      msg, strlen(msg));
           break;
        }
        case GROUP_TRACE5:
        {
           tracepoint(com_ericsson_triobjif, TRACE5,
                      objname,
                      strlen(objname),
                      fileline, strlen(fileline),
                      msg, strlen(msg));
           break;
        }
        case GROUP_TRACE6:
        {
           tracepoint(com_ericsson_triobjif, TRACE6,
                      objname,
                      strlen(objname),
                      fileline, strlen(fileline),
                      msg, strlen(msg));
           break;
        }
        case GROUP_TRACE7:
        {
           tracepoint(com_ericsson_triobjif, TRACE7,
                      objname,
                      strlen(objname),
                      fileline, strlen(fileline),
                      msg, strlen(msg));
           break;
        }
        case GROUP_TRACE8:
        {
           tracepoint(com_ericsson_triobjif, TRACE8,
                      objname,
                      strlen(objname),
                      fileline, strlen(fileline),
                      msg, strlen(msg));
           break;
        }
        case GROUP_TRACE9:
        {
           tracepoint(com_ericsson_triobjif, TRACE9,
                      objname,
                      strlen(objname),
                      fileline, strlen(fileline),
                      msg, strlen(msg));
           break;
        }
        case GROUP_STATE_CHANGE:
        {
           tracepoint(com_ericsson_triobjif, STATE_CHANGE,
                      objname,
                      strlen(objname),
                      fileline, strlen(fileline),
                      msg, strlen(msg));
           break;
        }
        case GROUP_REC_SIG:
        {
           tracepoint(com_ericsson_triobjif, REC_SIG,
                      objname,
                      strlen(objname),
                      fileline, strlen(fileline),
                      msg, strlen(msg));
           break;
        }
        case GROUP_SEND_SIG:
        {
           tracepoint(com_ericsson_triobjif, SEND_SIG,
                      objname,
                      strlen(objname),
                      fileline, strlen(fileline),
                      msg, strlen(msg));
           break;
        }
        case GROUP_PARAM:
        {
           tracepoint(com_ericsson_triobjif, PARAM,
                      objname,
                      strlen(objname),
                      fileline, strlen(fileline),
                      msg, strlen(msg));
           break;
        }
        case GROUP_TRACE_OBJ:
        {
           tracepoint(com_ericsson_triobjif, TRACE_OBJ,
                      objname,
                      strlen(objname),
                      fileline, strlen(fileline),
                      msg, strlen(msg));
           break;
        }
        case GROUP_INTERFACE:
        {
           tracepoint(com_ericsson_triobjif, INTERFACE,
                      objname,
                      strlen(objname),
                      fileline, strlen(fileline),
                      msg, strlen(msg));
           break;
        }
        case GROUP_USER1:
        {
           tracepoint(com_ericsson_triobjif, USER1,
                      objname,
                      strlen(objname),
                      fileline, strlen(fileline),
                      msg, strlen(msg));
           break;
        }
        case GROUP_USER2:
        {
           tracepoint(com_ericsson_triobjif, USER2,
                      objname,
                      strlen(objname),
                      fileline, strlen(fileline),
                      msg, strlen(msg));
           break;
        }
        case GROUP_USER3:
        {
           tracepoint(com_ericsson_triobjif, USER3,
                      objname,
                      strlen(objname),
                      fileline, strlen(fileline),
                      msg, strlen(msg));
           break;
        }
        case GROUP_USER4:
        {
           tracepoint(com_ericsson_triobjif, USER4,
                      objname,
                      strlen(objname),
                      fileline, strlen(fileline),
                      msg, strlen(msg));
           break;
        }
        default:
        {
           tracepoint(com_ericsson_triobjif, DEFAULT, group);
           break;
        }
     }
     /* Always return True */
     return True;
  }
}

/* ===================================================================== */
/**
 *   This function traces data for a interface or object trace item.
 *
 *   @param group         Group mask for this function call
 *   @param file          Name of file where this function was called
 *   @param line          Line in file where this function was called
 *   @param msg           String to trace
 *   @param traceProc_p   Pointer to thread specific data
 *   @param traceObj_p    Pointer to interface or object specific data
 *
 *   @return              True on success, else False.
 *
 *   @par Globals:
 *                     -
 */
/* ===================================================================== */
inline Boolean
OMCSF_logObjTrace(OMCSF_groupE  group,
                  const char    *file,
                  U16           line,
                  const char    *msg,
                  struct OMCSF_procInfoS *traceProc_p,
                  struct OMCSF_procInfoS *traceObj_p)
{

  return lttngTriIfObjTrace(group,
                            file,
                            line,
                            msg,
                            traceProc_p,
                            traceObj_p);

}

/* ===================================================================== */
/**
 *   This function traces data for SEND_SIG requested by a interface
 *   or object trace item.
 *
 *   @param file       Name of file where this function was called
 *   @param line       Line in file where this function was called
 *   @param signo      Signal number for the sent signal
 *   @param recpid     Reciever pid for the sent signal
 *   @param msg        String to trace
 *   @param procInfo   Pointer to thread specific data
 *   @param ref        Pointer to interface or object specific data
 *
 *   @return           Always return True.
 *
 *   @par Globals:
 *                     -
 */
/* ===================================================================== */
Boolean
lttngTriIfObjSendSigTrace(const char *file,
                          uint16_t line,
                          uint32_t signo,
                          uint32_t recpid,
                          const char *msg,
                          struct ItemInfo *procInfo,
                          struct ItemInfo *ref)
{
   char *msg1;
   char *newMsg;

   if (procInfo == ref)
   {
      return lttngTriSendSigTrace(file, line, signo, recpid, msg, procInfo);
   }
   else
   {
      newMsg = (char *)malloc(strlen(msg) + 1);
      if (!newMsg)
      {
         syslog(LOG_ERR, "TRI proxy %s: %s (%d).",
                __FUNCTION__, strerror(errno), errno);
         return False;
      }
      memcpy(newMsg, msg, strlen(msg) + 1);

      msg1 = formatStr("signo:%x receiver :%x %s",
                       signo, recpid, newMsg);

      snprintf(fileline, TRI_MAX_FILE_AND_LINE_LEN,
               "%s:%d", file, line);

      snprintf(objname, TRI_MAX_PROC_AND_OBJ_IF_NAME_LEN,
               "%s(%s)", procInfo->procName, ref->procName);

      tracepoint(com_ericsson_triobjif, SEND_SIG,
                 objname, strlen(objname),
                 fileline, strlen(fileline),
                 msg1, strlen(msg1));
      free(newMsg);

      /* Always return True */
      return True;
   }
}

/* ===================================================================== */
/**
 *   This function traces data for REC_SIG requested by a interface
 *   or object trace item.
 *
 *   @param file       Name of file where this function was called
 *   @param line       Line in file where this function was called
 *   @param sig        Signal pointer for the received signal
 *   @param msg        String to trace
 *   @param procInfo   Pointer to thread specific data
 *   @param ref        Pointer to interface or object specific data
 *
 *   @return           Always return True.
 *
 *   @par Globals:
 *                     -
 */
/* ===================================================================== */
Boolean
lttngTriIfObjRecSigTrace(const char *file,
                         uint16_t line,
                         uint32_t *sig,
                         const char *msg,
                         struct ItemInfo *procInfo,
                         struct ItemInfo *ref)
{
   char *msg1;
   char *newMsg;

   if (procInfo == ref)
   {
      return lttngTriRecSigTrace(file, line, sig, msg, procInfo);
   }
   else
   {
      newMsg = (char *)malloc(strlen(msg) + 1);
      if (!newMsg)
      {
         syslog(LOG_ERR, "TRI proxy %s: %s (%d).",
                __FUNCTION__, strerror(errno), errno);
         return False;
      }

      memcpy(newMsg, msg, strlen(msg) + 1);

      msg1 = formatStr("signo:%x sender :%x %s",
                       *sig, itc_sender((union itc_msg*)sig), newMsg);

      snprintf(fileline, TRI_MAX_FILE_AND_LINE_LEN,
               "%s:%d", file, line);

      snprintf(objname, TRI_MAX_PROC_AND_OBJ_IF_NAME_LEN,
               "%s(%s)", procInfo->procName, ref->procName);

      tracepoint(com_ericsson_triobjif, REC_SIG,
                 objname, strlen(objname),
                 fileline, strlen(fileline),
                 msg1, strlen(msg1));

      free(newMsg);

      /* Always return True */
      return True;
   }
}

/* ===================================================================== */
/**
 *   This function traces data for BUS_SEND and BUS_RECEIVE requested
 *   by an interface or object trace item.
 *
 *   @param procInfo   Pointer to thread specific data
 *   @param ref        Pointer to interface or object specific data
 *   @param group      Group mask for this function call
 *   @param file       Name of file where this function was called
 *   @param line       Line in file where this function was called
 *   @param msg        String to trace
 *   @param data       Data to trace
 *   @param len        Length of data
 *
 *   @return           Always return True.
 *
 *   @par Globals:
 *                     -
 */
/* ===================================================================== */
Boolean
lttngTriIfObjBusTrace(struct ItemInfo *procInfo,
                      struct ItemInfo *ref,
                      uint32_t group,
                      const char *file,
                      uint16_t line,
                      const char *msg,
                      uint8_t *data,
                      uint16_t len)
{

   /* Limit the total amount of data to log */
   if (len > (16 * 1024))
   {
      syslog(LOG_ERR,
             "TRI proxy lttngTriIfObjBusTrace: Data to trace exceed 16kB, "
             "(%d) data will not be logged", len);
      return True;
   }
   if (!lttgTriBusFilterEval((int *)ref->busLogFilter, data, len))
   {
      return True;
   }
   if (procInfo == ref)
   {
      return lttngTriBusTrace(procInfo, group, file, line, msg, data, len);
   }
   else
   {
      snprintf(fileline, TRI_MAX_FILE_AND_LINE_LEN,
               "%s:%d", file, line);

      snprintf(objname, TRI_MAX_PROC_AND_OBJ_IF_NAME_LEN,
               "%s(%s)", procInfo->procName, ref->procName);

      switch (group)
      {
         case GROUP_BUS_SEND:
         {
            tracepoint(com_ericsson_triobjif, BUS_SEND,
                       objname, strlen(objname),
                       fileline, strlen(fileline),
                       msg, strlen(msg),
                       (char *) data, len);
            break;
         }
         case GROUP_BUS_RECEIVE:
         {
            tracepoint(com_ericsson_triobjif, BUS_RECEIVE,
                       objname, strlen(objname),
                       fileline, strlen(fileline),
                       msg, strlen(msg),
                       (char *) data, len);
            break;
         }
         default:
         {
            tracepoint(com_ericsson_triobjif, DEFAULT, group);
            break;
         }
      }
      /* Always return True */
      return True;
   }
}

/* ===================================================================== */
/**
 *   This function traces data for BUS_SEND and BUS_RECEIVE requested
 *   by an interface or object trace item.
 *
 *   @param procInfo_p   Pointer to interface or object
 *   @param ref        Pointer to interface or object specific data
 *   @param group      Group mask for this function call
 *   @param file       Name of file where this function was called
 *   @param line       Line in file where this function was called
 *   @param msg        String to trace
 *   @param data       Data to trace
 *   @param length     Length of data
 *
 *   @return           True on success, else False.
 *
 *   @par Globals:
 *                     -
 */
/* ===================================================================== */
inline Boolean
OMCSF_logObjBusTrace(struct OMCSF_procInfoS *procInfo_p,
                     struct OMCSF_procInfoS *objInfo_p,
                     OMCSF_groupE            group,
                     const char             *file,
                     U16                     line,
                     const char             *msg,
                     U8                     *data,
                     U16                     length)
{

   return lttngTriIfObjBusTrace(procInfo_p,
                                objInfo_p,
                                group,
                                file,
                                line,
                                msg,
                                data,
                                length);

}

/* ===================================================================== */
/**
 *   This function traces data for SEND_SIG requested by a interface
 *   or object trace item.
 *
 *   @param file       Name of file where this function was called
 *   @param line       Line in file where this function was called
 *   @param signo      Signal number for the sent signal
 *   @param recpid     Reciever pid for the sent signal
 *   @param msg        String to trace
 *   @param procInfo_p Pointer to thread specific data
 *   @param objInfo_p  Pointer to interface or object specific data
 *
 *   @return           Always return True.
 *
 *   @par Globals:
 *                     -
 */
/* ===================================================================== */
inline Boolean
OMCSF_sendObjLogTrace(const char             *file,
                      U16                     line,
                      U32                     signo,
                      U32                     recpid,
                      const char             *msg,
                      struct OMCSF_procInfoS *procInfo_p,
                      struct OMCSF_procInfoS *objInfo_p)
{
   return lttngTriIfObjSendSigTrace(file,
                                    line,
                                    signo,
                                    recpid,
                                    msg,
                                    procInfo_p,
                                    objInfo_p);
}

/* ===================================================================== */
/**
 *   This function traces data for REC_SIG requested by a interface
 *   or object trace item.
 *
 *   @param file       Name of file where this function was called
 *   @param line       Line in file where this function was called
 *   @param sig        Signal pointer for the received signal
 *   @param msg        String to trace
 *   @param procInfo_p Pointer to thread specific data
 *   @param objInfo_p  Pointer to interface or object specific data
 *
 *   @return           Always return True.
 *
 *   @par Globals:
 *                     -
 */
/* ===================================================================== */
inline Boolean
OMCSF_recObjLogTrace(const char             *file,
                     U16                     line,
                     void                   *signal,
                     const char             *msg,
                     struct OMCSF_procInfoS *procInfo_p,
                     struct OMCSF_procInfoS *objInfo_p)
{
   return lttngTriIfObjRecSigTrace(file,
                                   line,
                                   signal,
                                   msg,
                                   procInfo_p,
                                   objInfo_p);
}

/* ===================================================================== */
/**
 *   This function traces data for user time stamp trace requested by
 *   an interface or object trace item.
 *
 *   @param group      Group mask for this function call
 *   @param sec        User provided time stamp in seconds
 *   @param usec       User provided time stamp in microseconds
 *   @param file       Name of file where this function was called
 *   @param line       Line in file where this function was called
 *   @param procName   Name of thread calling this function
 *   @param msg        String to trace
 *   @param data       Data to trace
 *   @param len        Length of data
 *   @param procInfo   Pointer to thread specific data
 *   @param ref        Pointer to interface or object specific data
 *
 *   @return           Always return True.
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
Boolean
lttngTriIfObjUserTrace(uint32_t group,
                       uint32_t sec,
                       uint32_t usec,
                       const char *file,
                       uint16_t line,
                       const char *procName,
                       const char *msg,
                       uint8_t *data,
                       uint16_t len,
                       struct ItemInfo *procInfo,
                       struct ItemInfo *ref)
{
   /* Limit the total amount of data to log */
   if (len > (16 * 1024))
   {
      syslog(LOG_ERR,
             "TRI proxy lttngTriIfObjBusTrace: Data to trace exceed 16kB, "
             "(%d) data will not be logged", len);
      return True;
   }

   if (procInfo == ref)
   {
      return lttngTriUserTrace(group, sec, usec, file, line, procName,
                               msg, data, len, procInfo);
   }
   else
   {
      snprintf(fileline, TRI_MAX_FILE_AND_LINE_LEN,
               "%s:%d", file, line);

      snprintf(objname, TRI_MAX_PROC_AND_OBJ_IF_NAME_LEN,
               "%s(%s)", procInfo->procName, ref->procName);

      tracepoint(com_ericsson_triobjif, TRACE_UTS,
                 objname, strlen(objname),
                 sec, usec,
                 fileline, strlen(fileline),
                 msg, strlen(msg),
                 (char *) data, len);

      /* Always return True */
      return True;
   }
}

/* ===================================================================== */
/**
 *   Shortens path to a file to just a filename
 *
 *   @param fname        Pointer to absolute path
 *
 *   @return           Pointer to a string(filename)
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
const char *short_fname(const char *fname) {
        const char *fileName = strrchr(fname,'/');
        return fileName ? fileName + 1 : fname;
}
