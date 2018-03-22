/**
 *   Header file for TRI daemon.
 *
 *   ******* THIS FILE IS TRI INTERNAL.***************
 *   ******* TRI CLIENTS SHALL NOT INCLUDE THIS HEADER FILE.******
 *
 *   Copyright (C) 2013-2016 by Ericsson AB. All rights reserved. The
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
 *   Revised : 2013-04-10 Anette Schött
 *   Change  : First version.
 *
 *   Revised : 2013-10-09 Anette Schött
 *   Change  : Updated with list functionality provided by sys/queue.h
 *
 *   Revised : 2013-12-10 M. Winberg
 *   Change  : Added deregistration signal.
 *
 *   Revised : 2015-04-02 Anette Schött
 *   Change  : Change to send pointer to pointer of struct
 *             ItemInfo instead for pointer to struct ItemInfo.
 *
 *   Revised : 2015-04-22 Anette Schött
 *   Change  : Added mallocRef to be used when freeing the heap memory
 *             alloctaed at registration of a thread.
 *
 *   Revised : 2015-06-02 Fredrik Skog
 *   Change  : TR HT78355: Added checks for return value of all malloc
 *             calls in TRI to prevent crashes.
 *
 *   Revised : 2015-06-12 Anette Schött
 *   Change  : HT78355, Add procName and groupMask in traceItemRefListElement,
 *             TRI_DAEMON_REGISTER_ITEM_REQ and TRI_DAEMON_DEREGISTER_ITEM_REQ.
 *
 *   Revised : 2015-11-10 Anette Schött
 *   Change  : Change of TRI daemon list handling of regsitered trace items.
 *             Remove element itemType in TRI_DAEMON_DEREGISTER_ITEM_REQ.
 *
 *   Revised : 2015-12-17 Anette Schött
 *   Change  : Add wrappers for malloc/free for debug purpose.
 *
 *   Revised : 2016-02-01 Anette Schött
 *   Change  : Add threadCleanupData, previously declared in tri_trace.c
 *             and defines for Posix message queue.
 *
 *   Revised : 2016-02-22 Anette Schött
 *   Change  : Remove defines for Posix message queue.
 * ========================================================================
 */

#ifndef __TRI_DAEMON_H
#define __TRI_DAEMON_H


#ifdef __cplusplus
extern "C" {
#endif

/* ========================================================================
 *   INCLUDE FILES
 * ========================================================================
 */

#include <stdint.h>
#include <sys/queue.h>
#include "cello_te_handlers.h"
#include "tri_server.h"

/* ========================================================================
 *   DEFINITIONS
 * ========================================================================
 */

#define TRI_DAEMON_MAILBOX_NAME "triDaemonMbox"

#define TRI_DAEMON_CLEANUP_MAILBOX_NAME "triDaemonCleanupMbox"

#define TRI_DAEMON_MSGBASE (TRI_MSGBASE + 0x40)

/* TRI daemon result codes (Note, bitwise setting)*/
#define TRI_DAEMON_OK                (1)
#define TRI_DAEMON_THREAD_NOT_EXIST  (2)
#define TRI_DAEMON_MALLOC_ERROR      (4)

/* ========================================================================
 *   TYPE DEFINITIONS
 * ========================================================================
 */

/*
 * Data provided to the pthread_create_key destructor
 */
struct threadCleanupData
{
  struct ItemInfo  *mallocRef;
  itc_mbox_id_t    daemonMbox;
  ProcName         procName;
};

/*
 * Type of trace item
 */
typedef uint32_t TriItemType;
#define TRI_ITEMTYPE_THREAD    (0)
#define TRI_ITEMTYPE_IFOBJ     (1)

/*
 * List of trace item referencies.
 */
struct refListElement
{
   /* List head of trace item referencies */
   TAILQ_ENTRY(refListElement)  list;

   struct ItemInfo                       **ref;
   struct ItemInfo                       *mallocRef;
};

TAILQ_HEAD(refListHead, refListElement);


/*
 * List of trace registered items.
 */
struct traceItemRefListElement
{
   /* List head of trace items */
   TAILQ_ENTRY(traceItemRefListElement)  list;

   /* Local copies of procName and groupMask is needed at deregistration
    * of a trace item. The referencies above might already been fred.
    */ 
   ProcName                              procName;
   uint32_t                              groupMask;

   /* List of referencies which has registered this item name. */
   struct refListHead                    refList;
};

TAILQ_HEAD(traceItemRefListHead, traceItemRefListElement);

/** ==================================================================== */
/** @struct TriDaemonRegisterItemReq
 *
 *   Register a trace item to TRI daemon.
 *
 *   @param itemType     Type of trace item, thread or interface/object.
 *
 *   @param traceItemRef Pointer to the pointer of a trace item struct.
 *
 *   @param mallocRef    Pointer to heap memory allocted ar registration.
 *                       Is used when freeing memory at attach from
 *                       registrated thread.
 *
 *   @param procName     Trace item name.
 *
 *   @param groupMask    Group mask for the trace item.
 */
/* ===================================================================== */
#define TRI_DAEMON_REGISTER_ITEM_REQ (TRI_DAEMON_MSGBASE)
/*!- SIGNO(TriDaemonRegisterItemReq) -!*/

typedef struct
{
   uint32_t               msgNo;
   TriItemType            itemType;
   struct ItemInfo        **traceItemRef;
   struct ItemInfo        *mallocRef;
   ProcName               procName;
   uint32_t               groupMask;
}TriDaemonRegisterItemReq;

/** ==================================================================== */
/** @struct TriDaemonRegisterItemRsp
 *
 *   Response signal of TRI_DAEMON_REGISTER_ITEM_REQ.
 *
 *   @param result       Result of the registartion.
 */
/* ===================================================================== */
#define TRI_DAEMON_REGISTER_ITEM_RSP (TRI_DAEMON_MSGBASE + 1)
/*!- SIGNO(TriDaemonRegisterItemRsp) -!*/

typedef struct
{
   uint32_t               msgNo;
   uint32_t               result;
}TriDaemonRegisterItemRsp;

/** ==================================================================== */
/** @struct TriDaemonItemMonitorInd
 *
 *   Monitor indication for a killed trace thread.
 *
 *
 *   @param mallocRef    Pointer to heap memory allocted ar registration.
 *                       Is used when freeing memory at attach from
 *                       registrated thread.
 *   @procName           Name of the killed thread.
 */
/* ===================================================================== */
#define TRI_DAEMON_ITEM_MONITOR_IND (TRI_DAEMON_MSGBASE + 2)
/*!- SIGNO(TriDaemonItemMonitorInd) -!*/

typedef struct
{
   uint32_t               msgNo;
   void                   *mallocRef;
   ProcName               procName;
}TriDaemonItemMonitorInd;

/** ==================================================================== */
/** @struct TriDaemonDeregisterItemReq
 *
 *   Deregister a trace item to TRI daemon.
 *
 *   @param traceItemRef Pointer to the pointer of a trace item struct.
 *
 *   @param procName     Trace item name.
 */
/* ===================================================================== */
#define TRI_DAEMON_DEREGISTER_ITEM_REQ (TRI_DAEMON_MSGBASE+3)
/*!- SIGNO(TriDaemonDeregisterItemReq) -!*/

typedef struct
{
   uint32_t               msgNo;
   struct ItemInfo        **traceItemRef;
   ProcName               procName;
}TriDaemonDeregisterItemReq;

/* ========================================================================
 *   DATA DECLARATIONS
 * ========================================================================
 */ 

/* ========================================================================
 *   VARIABLES
 * ========================================================================
 */ 

/* ========================================================================
 *   FUNCTIONS
 * ========================================================================
 */

#ifdef TRI_DEBUG
void* __wrap_malloc(size_t s);
void __wrap_free(void* p);
#endif

/* ===================================================================== */
/**
 *   Allocated thread specific key and assign a cleanup function
 *   which shall do the necessary cleanup if thread disappear.
 *   
 *
 *   @param      Pointer to structure with thread specific data.
 *
 *   @return           TRI_OK at success otherwise TRI_INTERNAL_ERROR.
 *
 *   @par Globals:
 *                tri_cleanup_key
 */
/* ===================================================================== */
int
triAllocAndSetThreadCleanupData(struct ItemInfo *procInfo,
                                itc_mbox_id_t mbox);


#ifdef __cplusplus
}
#endif

#endif   /* ifndef __TRI_DAEMON_H */
