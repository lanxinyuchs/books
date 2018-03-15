/**
 *   Header file for TRI daemon.
 *
 *   ******* THIS FILE IS TRI INTERNAL.***************
 *   ******* TRI CLIENTS SHALL NOT INCLUDE THIS HEADER FILE.******
 *
 *   Copyright (C) 2013-2015 by Ericsson AB. All rights reserved. The
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

#define TRI_DAEMON_MSGBASE (TRI_MSGBASE + 0x40)

/* TRI daemon result codes */
#define TRI_DAEMON_OK                (0)
#define TRI_DAEMON_THREAD_NOT_EXIST  (1)
#define TRI_DAEMON_MALLOC_ERROR      (2)

/* ========================================================================
 *   TYPE DEFINITIONS
 * ========================================================================
 */

/*
 * Type of trace item
 */
typedef uint32_t TriItemType;
#define TRI_ITEMTYPE_THREAD    (0)
#define TRI_ITEMTYPE_IFOBJ     (1)

struct traceItemRefListElement
{
   /* List head of trace items */
   TAILQ_ENTRY(traceItemRefListElement)  list;

   /*
    * Pointer to a trace item information struct (name, group mask ... )
    * registered to TRI daemon
    */
   struct ItemInfo                       **ref;
   struct ItemInfo                       *mallocRef;
   /* Local copies of procName and groupMask is needed at deregistration
    * of a trace item. The referencies above might already been fred.
    */ 
   ProcName                              procName;
   uint32_t                              groupMask;
};

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
 */
/* ===================================================================== */
#define TRI_DAEMON_ITEM_MONITOR_IND (TRI_DAEMON_MSGBASE + 2)
/*!- SIGNO(TriDaemonItemMonitorInd) -!*/

typedef struct
{
   uint32_t               msgNo;
   struct ItemInfo        *mallocRef;
}TriDaemonItemMonitorInd;

/** ==================================================================== */
/** @struct TriDaemonDeregisterItemReq
 *
 *   Deregister a trace item to TRI daemon.
 *
 *   @param itemType     Type of trace item, thread or interface/object.
 *                       include it in the description.]
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
   TriItemType            itemType;
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
