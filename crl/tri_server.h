/**
 *   Header file for TRI server functionality.
 *
 *   ******* THIS FILE IS TRI INTERNAL.***************
 *   ******* TRI CLIENTS SHALL NOT INCLUDE THIS HEADER FILE.******
 *
 *
 *   Copyright (C) 2013-2014 by Ericsson AB. All rights reserved. The
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
 *   Revised : 2013-12-11 Mats Winberg
 *   Change  : Added  deregistration request and response  signal for item
 *
 *   Revised : 2014-02-04 Stanislav Vovk
 *   Change  : Added msgRevision to all signals
 *
 *   Revised : 2014-09-18 V Ranganath
 *   Change  : Added saved field to TriServerItemInfo.
 *
 *   Revised : 2014-11-24 K.V.Ranganadh
 *   Change  : Support for bus filter.
 *
 *   Revised : 2015-10-09 Anette Schött
 *   Change  : Added TRI_MAX_PROCS_DEFAULT.
 *
 * ========================================================================
 */

#ifndef __TRI_SERVER_SIG
#define __TRI_SERVER_SIG


#ifdef __cplusplus
extern "C" {
#endif

/* ========================================================================
 *   INCLUDE FILES
 * ========================================================================
 */

#include <stdint.h>
#include <stdbool.h>
#include <sys/queue.h>
#include "cello_te_handlers.h"

/* ========================================================================
 *   DEFINITIONS
 * ========================================================================
 */

#define TRI_SERVER_MAILBOX_NAME "triServerMbox"
#define TRI_SERVER_NAME "triServer"

#define TRI_SERVER_TE_SAVE_FILE_NAME "te_save.txt"

#define MAX_FILTER_LEN 128

#define TRI_SERVER_TE_SAVE_SUBDIR_NAME "tri/"

#define TRI_SERVER_TE_SAVE_DEFAULT_PATH "/var/log/"

#define TRI_MSGBASE (0x61280)

#define TRI_SERVER_MSGBASE (TRI_MSGBASE + 0x20)

/* 
 * Default value for maximum number of ITC mailboxes,
 * same value as defined in LITS. Used for itc_init().
 */
#define TRI_MAX_PROCS_DEFAULT 512

/* ========================================================================
 *   TYPE DEFINITIONS
 * ========================================================================
 */

/*
 * TRI server internal list for registered trace items
 */
typedef struct
{
   ProcName            procName;        /* A trace item name */
   uint32_t            groupMask;       /* A trace item group mask */
   bool                saved;           /* Trace item is saved or not */
}TriServerItemInfo;

/*
 * Struct containing information (name, group mask) for a trace item
 * saved with 'te save' command
 */
struct savedItemListElement
{
   /* List head of saved trace items */
   TAILQ_ENTRY(savedItemListElement)   list;

   /* A trace item struct */
   TriServerItemInfo                   itemInfo;

};

/** ==================================================================== */
/** @struct signal_struct_name
 *
 *   [Enter a brief one-line function comment and leave next line empty.]
 *
 *   [@param param1    Description of param1. The signal number is not
 *                     regarded as parameter, so there is no need to
 *                     include it in the description.]
 *   [@param param2    Description of param2]
 *
 *   [A longer multi-line and multi-sentence description of the signal
 *   can be entered here. This way of tagging the comments is based
 *   on the function doxygen - A documentation system for generating
 *   descriptions from source code.]
 *
 *   [The short-line description is required, but the longer description
 *   is optional]
 */
/* ===================================================================== */
#define TRI_SERVER_REGISTER_DAEMON_REQ (TRI_SERVER_MSGBASE)
/*!- SIGNO(TriServerRegisterDaemonReq) -!*/

typedef struct
{
   uint32_t            msgNo;
   uint32_t            msgRevision;
} TriServerRegisterDaemonReq;


#define TRI_SERVER_REGISTER_DAEMON_RSP (TRI_SERVER_MSGBASE + 1)
/*!- SIGNO(TriServerRegisterDaemonRsp) -!*/

typedef struct
{
   uint32_t            msgNo;
   uint32_t            msgRevision;
   uint32_t            result;
   itc_mbox_id_t       daemonMbox;
} TriServerRegisterDaemonRsp;


#define TRI_SERVER_REGISTER_ITEM_REQ (TRI_SERVER_MSGBASE + 2)
/*!- SIGNO(TriServerRegisterItemReq) -!*/

typedef struct
{
   uint32_t            msgNo;
   uint32_t            msgRevision;
   TriServerItemInfo   itemInfo;
   itc_mbox_id_t       pid;
} TriServerRegisterItemReq;


#define TRI_SERVER_REGISTER_ITEM_RSP (TRI_SERVER_MSGBASE + 3)
/*!- SIGNO(TriServerRegisterItemRsp) -!*/

typedef struct
{
   uint32_t            msgNo;
   uint32_t            msgRevision;
   uint32_t            result;
   ProcName            itemName;
} TriServerRegisterItemRsp;

#define TRI_SERVER_SET_MASK_IND (TRI_SERVER_MSGBASE + 4)
/*!- SIGNO(TriServerSetMaskInd) -!*/

typedef struct
{
   uint32_t            msgNo;
   uint32_t            msgRevision;
   TriServerItemInfo   itemInfo;
} TriServerSetMaskInd;


#define TRI_SERVER_DAEMON_MONITOR_IND (TRI_SERVER_MSGBASE + 5)
/*!- SIGNO(TriServerDaemonMonitorInd) -!*/

typedef struct
{
   uint32_t            msgNo;
   uint32_t            msgRevision;
   itc_mbox_id_t       daemonMbox;
}TriServerDaemonMonitorInd;

#define TRI_SERVER_ITEM_KILLED_IND (TRI_SERVER_MSGBASE + 6)
/*!- SIGNO(TriServerItemKilledInd) -!*/

typedef struct
{
   uint32_t            msgNo;
   uint32_t            msgRevision;
   ProcName            itemName;
   itc_mbox_id_t       daemonMbox;
}TriServerItemKilledInd;

#define TRI_SERVER_DEREGISTER_ITEM_REQ (TRI_SERVER_MSGBASE + 7)
/*!- SIGNO(TriServerDeregisterItemReq) -!*/

typedef struct
{
   uint32_t            msgNo;
   uint32_t            msgRevision;
   TriServerItemInfo   itemInfo;
} TriServerDeregisterItemReq;

#define TRI_SERVER_DEREGISTER_ITEM_RSP (TRI_SERVER_MSGBASE + 8)
/*!- SIGNO(TriServerDeregisterItemRsp) -!*/

typedef struct
{
   uint32_t            msgNo;
   uint32_t            msgRevision;
   uint32_t            result;
   ProcName            itemName;
} TriServerDeregisterItemRsp;

#define TRI_SERVER_UPDATE_FILTER_IND (TRI_SERVER_MSGBASE + 9)
/*!- SIGNO(TriServerUpdateFilterInd) -!*/

typedef struct
{
   uint32_t            msgNo;
   uint32_t            msgRevision;
   ProcName            itemName;
   uint32_t            compiledFilter[MAX_FILTER_LEN];
} TriServerUpdateFilterInd;

#ifdef __cplusplus
}
#endif

#endif   /* ifndef __TRI_SERVER_SIG */

