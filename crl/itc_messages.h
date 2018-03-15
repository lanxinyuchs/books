/**
 *   Header file for ITC messages used internally by ITC.
 *
 *   @file itc_messages.h
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
 *   Revised : 2013-02-07 Magnus Lindberg
 *   Change  : First version.
 *
 *   Revised : 2014-01-28 Magnus Lindberg
 *   Change  : Implemented itc_get_name between processes.
 *
 *   Revised : 2014-02-05 Magnus Lindberg magnus.k.lindberg@ericsson.com
 *   Change  : Major overhaul of how monitors are implemented.
 *
 *   Revised : 2014-10-28 Magnus Lindberg magnus.k.lindberg@ericsson.com
 *   Change  : Added itc_events implementation.
 * ========================================================================
 */

#ifndef __ITC_MESSAGES_H
#define __ITC_MESSAGES_H

#ifdef __cplusplus
extern "C" {
#endif

/* ========================================================================
 *   INCLUDE FILES
 * ========================================================================
 */
#include "itc.h"
#include "itc_impl.h"

/* ========================================================================
 *   DEFINITIONS
 * ========================================================================
 */
#define ITC_MESS_BASE (ITC_MSG_BASE + 0x100)

/* ========================================================================
 *   TYPE DEFINITIONS
 * ========================================================================
 */

#define ITC_LOCATE_WORLD      (ITC_MESS_BASE + 0)
struct itc_locate_world {
        uint32_t       msgno;
};

#define ITC_LOCATE_WORLD_REPL (ITC_MESS_BASE + 1)
struct itc_locate_world_repl {
        uint32_t      msgno;
        itc_mbox_id_t my_id;
        itc_mbox_id_t world_mask;
        itc_mbox_id_t world_mbox_id;
};

#define ITC_ADD_MBOX (ITC_MESS_BASE + 2)
#define ITC_REM_MBOX (ITC_MESS_BASE + 3)
#define ITC_ADD_NAME (ITC_MESS_BASE + 4)
struct itc_add_rem_mailbox {
        uint32_t      msgno;
        itc_mbox_id_t mbox_id;
        char          mbox_name[1];
};

#define ITC_LOCATE (ITC_MESS_BASE + 5)
struct itc_locate {
        uint32_t      msgno;
        itc_mbox_id_t from_mbox;
        char          mbox_name[1];
};

#define ITC_LOCATE_REPL (ITC_MESS_BASE + 6)
struct itc_locate_repl {
        uint32_t       msgno;
        itc_mbox_id_t  mbox_id;
        itc_transports transport;
        char           mbox_name[1];
};

#define ITC_LOCATE_ASYNC (ITC_MESS_BASE + 7)
struct itc_locate_async {
        uint32_t       msgno;
        itc_mbox_id_t  from_mbox;
        void          *data;
        char           mbox_name[1];
};

#define ITC_LOCATE_ASYNC_REPL (ITC_MESS_BASE + 8)
struct itc_locate_async_repl {
        uint32_t       msgno;
        itc_mbox_id_t  from_mbox;
        void          *data;
        itc_mbox_id_t  mbox_id;
        itc_transports transport;
        char           mbox_name[1];
};

/* These are the old monitor messages, they have been
   redefined but will be kept here for information and
   to keep from reusing the old message numbers.*/
#if 0
#define ITC_MONITOR (ITC_MESS_BASE + 9)
struct itc_monitor {
        uint32_t       msgno;
        itc_mbox_id_t  target_mbox_id;
        itc_mbox_id_t  monitor_mbox_id;
        int32_t        msgsize;
        char           monmsg[1];
};

#define ITC_MONITOR_REPL (ITC_MESS_BASE + 10)
struct itc_monitor_repl {
        uint32_t         msgno;
        itc_monitor_id_t mon_id;
};

#define ITC_UNMONITOR (ITC_MESS_BASE + 11)
struct itc_unmonitor {
        uint32_t         msgno;
        itc_monitor_id_t mon_id;
};
#endif

#define ITC_ASSIGN_LNH (ITC_MESS_BASE + 12)
#define ITC_DEASSIGN_LNH (ITC_MESS_BASE + 13)
struct itc_assign_lnh {
        uint32_t         msgno;
        itc_mbox_id_t    mbox_id;
        char             lnhpath[1];
};

#define ITC_GET_ALL_MBOXES (ITC_MESS_BASE + 14)
struct itc_get_all_mboxes {
        uint32_t         msgno;
        itc_mbox_id_t    mbox_id;
};

#define ITC_GET_MBOX_INFO (ITC_MESS_BASE + 15)
struct itc_get_mbox_info {
        uint32_t         msgno;
        itc_mbox_id_t    mbox_id;
};

#define ITC_GET_MBOX_INFO_REPL (ITC_MESS_BASE + 16)
struct itc_get_mbox_info_repl {
        uint32_t             msgno;
        itc_mbox_id_t        mbox_id;
        int32_t              mbox_found;
        struct itc_mbox_info mbox_info;
};

#define ITC_GET_ALLOC_INFO (ITC_MESS_BASE + 17)
struct itc_get_alloc_info {
        uint32_t         msgno;
};

#define ITC_GET_ALLOC_INFO_REPL (ITC_MESS_BASE + 18)
struct itc_get_alloc_info_repl {
        uint32_t              msgno;
        struct itc_alloc_info alloc_info;
};

#define ITC_SET_NAMESPACE (ITC_MESS_BASE + 19)
struct itc_set_namespace {
        uint32_t         msgno;
        itc_mbox_id_t    world_id;
        char             name_space[1];
};

#define ITC_GET_NAME (ITC_MESS_BASE + 20)
struct itc_get_name {
        uint32_t         msgno;
        itc_mbox_id_t    mbox_id;
};

#define ITC_GET_NAME_REPL (ITC_MESS_BASE + 21)
struct itc_get_name_repl {
        uint32_t         msgno;
        itc_mbox_id_t    mbox_id;
        int32_t          found;
        char             mbox_name[1];
};

#define ITC_MONITOR (ITC_MESS_BASE + 22)
struct itc_monitor {
        uint32_t         msgno;
        itc_mbox_id_t    from_mbox_id;
        itc_mbox_id_t    target_mbox_id;
        itc_mbox_id_t    coord_mbox_id;
        itc_monitor_id_t mon_id;
};

#define ITC_UNMONITOR (ITC_MESS_BASE + 23)
struct itc_unmonitor {
        uint32_t         msgno;
        itc_mbox_id_t    from_mbox_id;
        itc_mbox_id_t    target_mbox_id;
        itc_monitor_id_t mon_id;
};

#define ITC_TRIG_MONITOR (ITC_MESS_BASE + 24)
struct itc_trig_monitor {
        uint32_t         msgno;
        itc_mbox_id_t    from_mbox_id;
        itc_mbox_id_t    target_mbox_id;
        itc_monitor_id_t mon_id;
};

#define ITC_SUBSCRIBE_EVENTS (ITC_MESS_BASE + 25)
#define ITC_UNSUBSCRIBE_EVENTS (ITC_MESS_BASE + 26)
struct itc_subscribe_events {
        uint32_t         msgno;
        itc_mbox_id_t    mbox_id;
        uint32_t         events;
};

/* ========================================================================
 *   DATA DECLARATIONS
 * ========================================================================
 */




#ifdef __cplusplus
}
#endif

#endif   /* ifndef __ITC_MESSAGES_H */
