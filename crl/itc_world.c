/**
 *   itc world inplenetation
 *
 *   @file itc_world.c
 *
 *   ITC world implementation, needs to be started before all
 *   other ITC client programs.
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
 *   Revised : 2013-02-12 Magnus Lindberg
 *   Change  : First version.
 *
 *   Revised : 2014-01-16 Magnus Lindberg magnus.k.lindberg@ericsson.com
 *   Change  : Added support for multiple itcworld instances.
 *
 *   Revised : 2014-01-28 Magnus Lindberg
 *   Change  : Implemented itc_get_name between processes.
 *
 *   Revised : 2014-02-20 Magnus Lindberg
 *   Change  : Changed errors in rm_sysv_msgq to be non fatal to avoid
 *             races at restarts.
 *
 *   Revised : 2014-02-21 Magnus Lindberg magnus.k.lindberg@ericsson.com
 *   Change  : Major overhaul of how monitors are implemented.
 *             Improved error messages in ITC_ERROR.
 *
 *   Revised : 2014-09-29 Magnus Lindberg magnus.k.lindberg@ericsson.com
 *   Change  : Changed sockets so that they do not use an "abstract
 *             socket address" to allow ITC to be used between
 *             network namespaces.
 *
 *   Revised : 2014-10-28 Magnus Lindberg magnus.k.lindberg@ericsson.com
 *   Change  : Added itc_events implementation and added an atexit
 *             handler.
 *
 *   Revised : 2014-12-17 Magnus Lindberg magnus.k.lindberg@ericsson.com
 *   Change  : Corrected an mutual exclusion problem when closing unix
 *             local sockets and removing the corresponding file.
 *
 *   Revised : 2015-02-06 Magnus Lindberg magnus.k.lindberg@ericsson.com
 *   Change  : Corrected a potential race condition in rm_itc_client.
 *
 *   Revised : 2015-10-06 Magnus Lindberg magnus.k.lindberg@ericsson.com
 *   Change  : Changed handling of locate world when world can not
 *             accept any more ITC "clients" so that itc_init fails.
 *
 *   Revised : 2015-11-09 Magnus Lindberg magnus.k.lindberg@ericsson.com
 *   Change  : Corrected handling of the same name several times in
 *             itc_add_name.
 *
 *   Revised : 2015-11-10 Magnus Lindberg magnus.k.lindberg@ericsson.com
 *   Change  : Corrected find_proc function such that itcworld program does
 *             not go for intentional crash but prints error trace when
 *             mailbox id received to find_proc is not proper.
 *
 *   Revised : 2016-06-22 Attila Hadnagy attila.hadnagy@ericsson.com
 *   Change  : Daemonize option added
 *
 * ========================================================================
 */

/* ========================================================================
 *   INCLUDE FILES
 * ========================================================================
 */
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <unistd.h>

#include <signal.h>

#include <search.h>

#include "pthread.h"

#include <sys/socket.h>
#include <sys/select.h>
#include <sys/un.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/ipc.h>
#include <sys/msg.h>

#include <errno.h>

#include "itc.h"
#include "itc_system.h"
#include "itc_impl.h"
#include "itc_list.h"
#include "itc_messages.h"

#ifdef ITC_LTTNG
/*
 * The header containing our TRACEPOINT_EVENTs.
 */
#define TRACEPOINT_DEFINE
#include "itc_world_lttng.h"

#include <stdarg.h>
#endif

/* ========================================================================
 *   DEFINITIONS
 * ========================================================================
 */


#define MAX_LNHLENGTH 32

#define WORLD_MASK 0xFFF00000

#ifdef ITC_LTTNG

#define ITC_WORLD_TRACE(...) tracepoint(com_ericsson_itc_world, __VA_ARGS__)
#define ITC_WORLD_TRACE_ERROR(txt, ...) itc_trace_error(__FILE__, __LINE__, txt, __VA_ARGS__)

#else /* ITC_LTTNG */

#define ITC_WORLD_TRACE(...)
#define ITC_WORLD_TRACE_ERROR(...)

#endif /* ITC_LTTNG */

/* ========================================================================
 *   TYPE DEFINITIONS
 * ========================================================================
 */
typedef enum {
        unused = 0,
        listening,
        connected,
        illegal
} itc_process_state;

union itc_msg {
        uint32_t                     msgno;

        struct itc_add_rem_mailbox   itc_add_rem_mailbox;

        struct itc_locate            itc_locate;
        struct itc_locate_repl       itc_locate_repl;
        struct itc_locate_async      itc_locate_async;
        struct itc_locate_async_repl itc_locate_async_repl;
        struct itc_monitor           itc_monitor;
        struct itc_unmonitor         itc_unmonitor;
        struct itc_trig_monitor      itc_trig_monitor;
        struct itc_assign_lnh        itc_assign_lnh;
        struct itc_locate_lnh        itc_locate_lnh;
        struct itc_locate_lnh_reply  itc_locate_lnh_reply;
        struct itc_get_all_mboxes    itc_get_all_mboxes;
        struct itc_set_namespace     itc_set_namespace;

        struct itc_mbox_added        itc_mbox_added;
	struct itc_mbox_removed      itc_mbox_removed;
        struct itc_lnh_added         itc_lnh_added;
	struct itc_lnh_removed       itc_lnh_removed;
	struct itc_locate_unresolved itc_locate_unresolved;

        struct itc_get_name          itc_get_name;
        struct itc_get_name_repl     itc_get_name_repl;

	struct itc_subscribe_events  itc_subscribe_events;
};

struct walk_data {
	union itc_msg *prevmsg;
	itc_mbox_id_t  send_id;
};

struct fifo_list {
        struct itc_process *head;
        struct itc_process *tail;
};

struct world_mon_mbox_record {
        struct world_mon_mbox_record *next;
        struct world_mon_mbox_record *prev;
        uint32_t                      flags;

        itc_mbox_id_t                 coord_mbox_id;
        itc_monitor_id_t              mon_id;

        struct itc_mbox              *mbox;
        struct world_mon_mbox_record *rec;
};

struct locate_record {
        struct locate_record *next;
        struct locate_record *mbox_next;
        itc_mbox_id_t         sender_mbox;
        itc_mbox_id_t         from_mbox;
        void                 *data;
        char                  mbox_name[1];
};

struct lnh_entry {
        struct lnh_entry *next;
        struct lnh_entry *prev;
        uint32_t          flags;

        struct lnh_entry *mblist_next;

        itc_mbox_id_t     mbox_id;
};

struct lnh_tree {
        struct lnh_entry *next;
        struct lnh_entry *prev;
        uint32_t          flags;

        char              lnhpath[1];
};

struct event_record {
        struct event_record *next;
        itc_mbox_id_t        mbox_id;
	uint32_t             event_flags;
};

struct addname {
        struct addname  *next;
        struct addname  *dupl_next;

        struct itc_mbox *mbox;
        char             name[1];
};

struct itc_mbox {
        struct itc_mbox              *next;
        struct itc_mbox              *proc_list_next;

        struct lnh_entry             *lnhlist;

        struct locate_record         *loc_list;

        struct addname               *addname_list;

        itc_mbox_id_t                 mbox_id;

        struct world_mon_mbox_record *monitors;      /* linkage of mboxes monitoring me */

        struct world_mon_mbox_record *mymonitors;    /* linkage of mboxes I am monitoring */

        char                          mbox_name[1];
};

struct itc_process {
        struct itc_process *next;
        struct itc_mbox    *proc_list;

        uint32_t            world_id;

        int                 sd;
        struct sockaddr_un  addr;
        itc_process_state   state;
        char               *name_space;

        struct itc_process *lnh;
};

struct world_instance {
        struct fifo_list        *freelist;
        struct fifo_list        *usedlist;

        void                    *mbox_tree;
        void                    *addname_tree;
        void                    *locate_tree;
        void                    *lnh_tree;

        uint32_t                 world_mask;
        uint32_t                 world_shift;

        char                    *rundir_path;
        char                    *msgq_file;

        itc_mbox_id_t            mbox_id;
        int                      mbox_fd;

        int                      sd;

        struct event_record     *event_sub_list;

        struct itc_process       procs[SUPPORTED_PROCESSES + 1];
};

/* ========================================================================
 *   DATA DECLARATIONS
 * ========================================================================
 */
static struct world_instance wi;

static struct walk_data walk_data;

/* Used to locate all previous locates that
   a new lnhpath needs to be informed
   about. */
static struct itc_assign_lnh *asslnhmsg = NULL;

/* ========================================================================
 *   FUNCTION PROTOTYPES
 * ========================================================================
 */
static void rm_sysv_msgq(itc_mbox_id_t proc_id);
static void event_add_mbox(uint32_t mbox_id, char *mbox_name);
static void event_remove_mbox(uint32_t mbox_id, char *mbox_name);
static void event_add_lnh(uint32_t mbox_id, char *lnhpath);
static void event_remove_lnh(uint32_t mbox_id, char *lnhpath);
static void remove_event_subscription(itc_mbox_id_t mbox_id, uint32_t flags);
static void event_locate_unresolved(uint32_t locating_id, char *name);
static void itc_world_exithandler(void);

/* ===================================================================== */
/**
 *   itc_world_error
 *
 *   @param errtext      Error descriptive text.
 *
 *   @return           -
 *
 *   @par Globals:     wi
 *                     ITC world global instance structure.
 *
 *   Function that clears the itcworld run environment and then calls the
 *   ITC errorhandler.
 */
/* ===================================================================== */
static void itc_world_error(const char *errtext, ...)
{
        va_list args;
        char buffer[256];

        rm_sysv_msgq(wi.mbox_id & wi.world_mask);

        if(wi.msgq_file != NULL) {
                unlink(wi.msgq_file);
        }

        if(wi.rundir_path) {
		strcpy(buffer, wi.rundir_path);
		strcat(buffer, ITC_WORLD_LOC_NAME);
		unlink(buffer);
                rmdir(wi.rundir_path);
        }

        va_start(args, errtext);
        vsnprintf(buffer, sizeof buffer, errtext, args);
        va_end(args);

        ITC_WORLD_TRACE_ERROR(buffer, 0);

        itc_call_errh(buffer, ITC_ERROR_FATAL);
}

/* ===================================================================== */
/**
 *   create_fifo
 *
 *   @return           Pointer to fifo list.
 *
 *   @par Globals:     --
 *
 *   Create a fifo list.
 */
/* ===================================================================== */
static struct fifo_list *create_fifo(void)
{
        struct fifo_list *tmp;

        tmp = malloc(sizeof(struct fifo_list));
        if(tmp == NULL) {
                return NULL;
        }

        tmp->head = NULL;
        tmp->tail = NULL;

        return tmp;
}

/* ===================================================================== */
/**
 *   rm_from_fifo
 *
 *   @param list       Pointer to fifo list.
 *
 *   @param itc_proc   Pointer to itc process to be removed from fifo.
 *
 *   @return           -
 *
 *   @par Globals:     --
 *
 *   Remove ITC process from fifo list.
 */
/* ===================================================================== */
static void rm_from_fifo(struct fifo_list *list,
                         struct itc_process *itc_proc)
{
        struct itc_process *tmp, *prev = NULL;


        for(tmp = list->head ; tmp != NULL ; tmp = tmp->next) {
                if(tmp == itc_proc) {
                        break;
                }
                prev = tmp;
        }

        if(tmp != NULL) {
                if(prev == NULL) {
                        if(list->tail == list->head) {
                                list->tail = tmp->next;
                        }
                        list->head = tmp->next;
                } else {
                        if(tmp == list->tail) {
                                list->tail = prev;
                        }
                        prev->next = tmp->next;
                }
        }
}

/* ===================================================================== */
/**
 *   add_to_fifo
 *
 *   @param list       Pointer to fifo list.
 *
 *   @param new        Pointer to itc process to be added to fifo.
 *
 *   @return           -
 *
 *   @par Globals:     --
 *
 *   Add ITC process to fifo list.
 */
/* ===================================================================== */
static void add_to_fifo(struct fifo_list *list, struct itc_process *new)
{
        new->next        = NULL;

        if(list->head == NULL) {
                list->head = new;
                list->tail = new;
        } else {
                list->tail->next = new;
                list->tail       = new;
        }
}

/* ===================================================================== */
/**
 *   get_from_fifo
 *
 *   @param list       Pointer to fifo list.
 *
 *   @return           Pointer to ITC process retrieved from fifo list,
 *                     Null if fifo is empty.
 *
 *   @par Globals:     --
 *
 *   Get ITC process from fifo list.
 */
/* ===================================================================== */
static struct itc_process *get_from_fifo(struct fifo_list *list)
{
        struct itc_process *tmp;

        tmp = list->head;
        if(tmp != NULL) {
                list->head = tmp->next;
                tmp->next = NULL;
        }

        if(list->head == NULL) {
                list->tail = NULL;
        }

        return tmp;
}

/* ===================================================================== */
/**
 *   find_proc
 *
 *   @param list       Pointer to fifo list.
 *
 *   @param mbox_id    Mailbox id to find in fifo list.
 *
 *   @return           Pointer to found ITC process or NULL if not found.
 *
 *   @par Globals:     wi
 *                     ITC world global instance structure.
 *
 *   Find process in fifo list.
 */
/* ===================================================================== */
static struct itc_process *find_proc(itc_mbox_id_t mbox_id)
{
        int offset;

        offset = ((mbox_id & wi.world_mask) >> wi.world_shift);

        if(offset > SUPPORTED_PROCESSES) {
                ITC_WORLD_TRACE_ERROR("Illegal world id (0x%08x),"
                                      " out of range, when finding process",
                                      mbox_id);
                return NULL;
        }

        return &wi.procs[offset];
}

/* ===================================================================== */
/**
 *   find_mbox
 *
 *   @param mbox_id    Mailbox id to find used fifo list.
 *
 *   @return           Pointer to found ITC mailbox or NULL if not found.
 *
 *   @par Globals:     wi
 *                     ITC world global instance structure.
 *
 *   Find mailbox in used fifo list.
 */
/* ===================================================================== */
static struct itc_mbox *find_mbox(itc_mbox_id_t mbox_id)
{
        struct itc_process *itc_proc;
        struct itc_mbox *tmp;

        itc_proc = find_proc(mbox_id);
        if(itc_proc == NULL) {
                return NULL;
        } else if(itc_proc->state != connected) {
                /* Process not present any more, ignore this mailbox */
                return NULL;
        }

        for(tmp = itc_proc->proc_list ; tmp != NULL ; tmp = tmp->proc_list_next) {
                if(tmp->mbox_id == mbox_id) {
                        break;
                }
        }

        return tmp;
}

/* ===================================================================== */
/**
 *   compare_mb_name
 *
 *   @param pa         Pointer to mailbox name.
 *
 *   @param pb         Pointer to mailbox.
 *
 *   @return           String compare return value for the mailbox names.
 *
 *   @par Globals:     --
 *
 *   Compare mailbox name function used together with the binary tree
 *   function tfind.
 */
/* ===================================================================== */
static int compare_mb_name(const void *pa, const void *pb)
{
        const char             *name = pa;
        const struct itc_mbox  *mbox = pb;

        return strcmp(name, mbox->mbox_name);
}

/* ===================================================================== */
/**
 *   compare_mb_name_add
 *
 *   @param pa         Pointer to mailbox A.
 *
 *   @param pb         Pointer to mailbox B.
 *
 *   @return           String compare return value for the mailbox names.
 *
 *   @par Globals:     --
 *
 *   Compare mailbox name function used together with the binary tree
 *   functions tfind, tsearch and tdelete.
 */
/* ===================================================================== */
static int compare_mb_name_add(const void *pa, const void *pb)
{
        const struct itc_mbox  *mbox1 = pa;
        const struct itc_mbox  *mbox2 = pb;

        return strcmp(mbox1->mbox_name, mbox2->mbox_name);
}

/* ===================================================================== */
/**
 *   compare_addname
 *
 *   @param pa         Pointer to addname struct A.
 *
 *   @param pb         Pointer to addname struct B.
 *
 *   @return           String compare return value for the additional
 *                     mailbox names.
 *
 *   @par Globals:     --
 *
 *   Compare additional mailbox name function used together with the
 *   binary tree function tfind.
 */
/* ===================================================================== */
static int compare_addname(const void *pa, const void *pb)
{
        const char           *name = pa;
        const struct addname *an   = pb;

        return strcmp(name, an->name);
}

/* ===================================================================== */
/**
 *   compare_addname_add
 *
 *   @param pa         Pointer to addname struct A.
 *
 *   @param pb         Pointer to addname struct B.
 *
 *   @return           String compare return value for the additional
 *                     mailbox names.
 *
 *   @par Globals:     --
 *
 *   Compare additional mailbox name function used together with the
 *   binary tree functions tfind, tsearch and tdelete.
 */
/* ===================================================================== */
static int compare_addname_add(const void *pa, const void *pb)
{
        const struct addname *an1 = pa;
        const struct addname *an2 = pb;

        return strcmp(an1->name, an2->name);
}

/* ===================================================================== */
/**
 *   compare_loc_name
 *
 *   @param pa         Pointer to name.
 *
 *   @param pb         Pointer to location struct.
 *
 *   @return           String compare return value for the location names.
 *
 *   @par Globals:     --
 *
 *   Compare location name function used together with the binary tree
 *   function tfind.
 */
/* ===================================================================== */
static int compare_loc_name(const void *pa, const void *pb)
{
        const char                  *name = pa;
        const struct locate_record  *loc  = pb;

        return strcmp(name, loc->mbox_name);
}

/* ===================================================================== */
/**
 *   compare_loc_name_add
 *
 *   @param pa         Pointer to location struct A.
 *
 *   @param pb         Pointer to location struct B.
 *
 *   @return           String compare return value for the location names.
 *
 *   @par Globals:     --
 *
 *   Compare location name function used together with the binary tree
 *   functions tfind, tsearch and tdelete.
 */
/* ===================================================================== */
static int compare_loc_name_add(const void *pa, const void *pb)
{
        const struct locate_record  *loc1 = pa;
        const struct locate_record  *loc2 = pb;

        return strcmp(loc1->mbox_name, loc2->mbox_name);
}

/* ===================================================================== */
/**
 *   compare_lnh_loc_name
 *
 *   @param pa         Pointer to location struct A.
 *
 *   @param pb         Pointer to location struct B.
 *
 *   @return           String compare return value for the location names.
 *
 *   @par Globals:     --
 *
 *   Compare location name function used together with the binary tree
 *   functions tfind, tsearch and tdelete.
 */
/* ===================================================================== */
static int compare_lnh_loc_name(const void *pa, const void *pb)
{
        const char                  *lnhpath = pa;
        const struct locate_record  *loc2 = pb;

        return strncmp(lnhpath, loc2->mbox_name, strlen(lnhpath));
}

/* ===================================================================== */
/**
 *   compare_lnh_name
 *
 *   @param pa         Pointer to location struct A.
 *
 *   @param pb         Pointer to location struct B.
 *
 *   @return           String compare return value for the location names.
 *
 *   @par Globals:     --
 *
 *   Compare location name function used together with the binary tree
 *   functions tfind, tsearch and tdelete.
 */
/* ===================================================================== */
static int compare_lnh_name(const void *pa, const void *pb)
{
        const char            *name = pa;
        const struct lnh_tree *lnh  = pb;

        return strncmp(name, lnh->lnhpath, strlen(lnh->lnhpath));
}

/* ===================================================================== */
/**
 *   compare_lnh_path
 *
 *   @param pa         Pointer to location struct A.
 *
 *   @param pb         Pointer to location struct B.
 *
 *   @return           String compare return value for the location names.
 *
 *   @par Globals:     --
 *
 *   Compare location name function used together with the binary tree
 *   functions tfind, tsearch and tdelete.
 */
/* ===================================================================== */
static int compare_lnh_path(const void *pa, const void *pb)
{
        const char            *lnhpath = pa;
        const struct lnh_tree *lnh     = pb;

        return strcmp(lnhpath, lnh->lnhpath);
}

/* ===================================================================== */
/**
 *   compare_lnh_name_add
 *
 *   @param pa         Pointer to lnh struct A.
 *
 *   @param pb         Pointer to lnh struct B.
 *
 *   @return           String compare return value for the location names.
 *
 *   @par Globals:     --
 *
 *   Compare lnh name function used together with the binary tree
 *   functions tfind, tsearch and tdelete.
 */
/* ===================================================================== */
static int compare_lnh_name_add(const void *pa, const void *pb)
{
        const struct lnh_tree  *lnh1 = pa;
        const struct lnh_tree  *lnh2 = pb;

        return strcmp(lnh1->lnhpath, lnh2->lnhpath);
}

/* ===================================================================== */
/**
 *   add_lnh_to_tree
 *
 *   @param mbox       Pointer to mailbox of new linkhandler.
 *
 *   @param lnhpath    Linkhandler path, including '/'.
 *
 *   @return           -
 *
 *   @par Globals:     wi
 *                     ITC world global instance structure.
 *
 *   Add a new linkhandler assignment to the linkhandler tree.
 */
/* ===================================================================== */
static void add_lnh_to_tree(struct itc_mbox *mbox, char *lnhpath)
{
        struct lnh_tree  **tmp, *lnht;
        struct lnh_entry *lnhe;

        tmp = tfind(lnhpath, &wi.lnh_tree, compare_lnh_path);
        if(tmp == NULL) {
                lnht = malloc(sizeof(struct lnh_tree) + strlen(lnhpath));
                if(lnht == NULL) {
                        itc_world_error("Failed to allocate for lnh tree in world");
                }
                LL_NEW(struct lnh_tree, (struct lnh_entry *)lnht);
                strcpy(lnht->lnhpath, lnhpath);

                tsearch(lnht, &wi.lnh_tree, compare_lnh_name_add);
        } else {
                lnht = *tmp;
        }

        lnhe = malloc(sizeof(struct lnh_entry));
        if(lnhe == NULL) {
                itc_world_error("Failed to allocate for lnh entry in world");
        }
        lnhe->mbox_id = mbox->mbox_id;
        LL_AT_END(struct lnh_entry, (struct lnh_entry *)lnht, lnhe);

        lnhe->mblist_next = mbox->lnhlist;
        mbox->lnhlist = lnhe;

        return;
}

/* ===================================================================== */
/**
 *   rem_lnh_from_tree
 *
 *   @param mbox       Pointer to mailbox of old linkhandler.
 *
 *   @param lnhpath    Linkhandler path, including '/'.
 *
 *   @return           -
 *
 *   @par Globals:     wi
 *                     ITC world global instance structure.
 *
 *   Remove a linkhandler assignment from the linkhandler tree.
 */
/* ===================================================================== */
static void rem_lnh_from_tree(struct itc_mbox *mbox, char *lnhpath)
{
        struct lnh_tree  **tmp, *lnht;
        struct lnh_entry *lnhe, *ltmp, *prev = NULL;

        tmp = tfind(lnhpath, &wi.lnh_tree, compare_lnh_path);
        if(tmp == NULL) {
                return;
        }
        lnht = *tmp;

        for(lnhe = LL_FIRST(struct lnh_tree, lnht) ;
            lnhe != NULL ;
            LL_NEXT(struct lnh_entry, lnhe)) {
                if(lnhe->mbox_id == mbox->mbox_id) {
                        break;
                }
        }

        if(lnhe == NULL) {
                return;
        }

        LL_OUT(struct lnh_entry, lnhe);

        for(ltmp = mbox->lnhlist ;
            ltmp != NULL         ;
            ltmp = ltmp->mblist_next) {
                if(ltmp == lnhe) {
                        break;
                }

                prev = ltmp;
        }

        if(ltmp != NULL) {
                if(prev == NULL) {
                        mbox->lnhlist = ltmp->mblist_next;
                } else {
                        prev->mblist_next = ltmp->mblist_next;
                }
        }

        free(lnhe);

        if(lnht == (struct lnh_tree *)lnht->prev) {
                tdelete(lnht, &wi.lnh_tree, compare_lnh_name_add);
        }
}

/* ===================================================================== */
/**
 *   rem_all_lnh_for_mbox
 *
 *   @param mbox       Pointer to the mailbox that shall have all
 *                     linkhandlers removed.
 *
 *   @return           -
 *
 *   @par Globals:     wi
 *                     ITC world global instance structure.
 *
 *   Removes all linkhandlers assigned by a mailbox.
 */
/* ===================================================================== */
static void rem_all_lnh_for_mbox(struct itc_mbox *mbox)
{
        struct lnh_tree  *lnht;
        struct lnh_entry *lnhe, *tmp;

        for(lnhe = mbox->lnhlist       ;
            lnhe != NULL               ;
            lnhe = lnhe->mblist_next) {
                tmp = lnhe;
                while((tmp->next->flags & LL_ROOT) == 0) {
                        tmp = tmp->next;
                }
                lnht = (struct lnh_tree *)tmp->next;
		event_remove_lnh(mbox->mbox_id, lnht->lnhpath);
                mbox->lnhlist = lnhe->mblist_next;

                LL_OUT(struct lnh_entry, lnhe);
                free(lnhe);
                if(lnht == (struct lnh_tree *)lnht->next) {
                        tdelete(lnht, &wi.lnh_tree, compare_lnh_name_add);
                        free(lnht);
                }
        }
}

/* ===================================================================== */
/**
 *   world_settings
 *
 *   @param world_mask   Pointer to world mask to be configured.
 *
 *   @param world_shift  Pointer to world shift to be configured.
 *
 *   @return           -
 *
 *   @par Globals:     --
 *
 *   Function that sets up the settings in ITC world when the ITC world
 *   is running. This needs to be the same as the one in itc.c.
 *
 *   TODO: I should figure out a better solution for this.
 */
/* ===================================================================== */
static void world_settings(uint32_t *world_mask, uint32_t *world_shift)
{
        uint32_t tmp_mask, tmp_shift;

        tmp_mask = WORLD_MASK;
        tmp_shift = 32;
        while(tmp_mask & 0x80000000) {
                tmp_mask = tmp_mask << 1;
                tmp_shift--;
        }

        *world_mask  = WORLD_MASK;
        *world_shift = tmp_shift;
}

/* ===================================================================== */
/**
 *   unreg_monitor
 *
 *   @param rec        Monitor record to be unregistered.
 *
 *   @return           -
 *
 *   @par Globals:     wi
 *                     ITC world global instance structure.
 *
 *   The unreg_monitor unregisters a monitor either when unmonitor is
 *   called or for all created monitors for a process when it terminates.
 */
/* ===================================================================== */
static void unreg_monitor(struct itc_mbox *mbox, itc_monitor_id_t mon_id)
{
        struct world_mon_mbox_record *tar_rec, *mon_rec;

        for(mon_rec = LL_FIRST(struct world_mon_mbox_record, mbox->mymonitors) ;
            mon_rec != NULL ;
            LL_NEXT(struct world_mon_mbox_record, mon_rec)) {
                if(mon_rec->mon_id == mon_id) {
                        break;
                }
        }

        if(mon_rec == NULL) {
                return;
        }

        tar_rec  = mon_rec->rec;

        LL_OUT(struct world_mon_mbox_record, tar_rec);
        LL_OUT(struct world_mon_mbox_record, mon_rec);

        itc_free((union itc_msg **)&tar_rec);
        itc_free((union itc_msg **)&mon_rec);
}

/* ===================================================================== */
/**
 *   return_monitors
 *
 *   @param mbox       Pointer to mailbox.
 *
 *   @return           -
 *
 *   @par Globals:     wi
 *                     ITC world global instance structure.
 *
 *   The return monitor_function is called when a mailbox is deleted or
 *   an ITC process terminates. It will send all current monitors to their
 *   originators and also unregister all created monitors by tis mailbox.
 */
/* ===================================================================== */
static void return_monitors(struct itc_mbox *mbox)
{
        struct world_mon_mbox_record *tar_rec, *mon_rec;
        struct itc_mbox *tar_mbox, *mon_mbox;
        union itc_msg *msg;

        while(1) {
                LL_GETFIRST(struct world_mon_mbox_record, mbox->monitors, tar_rec);

                if(tar_rec == NULL) {
                        break;
                }

                mon_rec = tar_rec->rec;

                LL_OUT(struct world_mon_mbox_record, mon_rec);

                tar_mbox = mbox;
                mon_mbox = tar_rec->mbox;

                msg = itc_alloc(sizeof(struct itc_trig_monitor),
                                ITC_TRIG_MONITOR);
                msg->itc_trig_monitor.from_mbox_id   = mon_mbox->mbox_id;
                msg->itc_trig_monitor.target_mbox_id = tar_mbox->mbox_id;
                msg->itc_trig_monitor.mon_id         = tar_rec->mon_id;
                itc_send(&msg, tar_rec->coord_mbox_id, ITC_MY_MBOX);

                itc_free((union itc_msg **)&tar_rec);
                itc_free((union itc_msg **)&mon_rec);
        }

        while((mon_rec = LL_FIRST(struct world_mon_mbox_record, mbox->mymonitors)) != NULL) {
                unreg_monitor(mbox, mon_rec->rec->mon_id);
        }
}

/* ===================================================================== */
/**
 *   remove_locates
 *
 *   @param mbox       Pointer to mailbox.
 *
 *   @return           -
 *
 *   @par Globals:     wi
 *                     ITC world global instance structure.
 *
 *   The remove_locates function is called when a mailbox is deleted or
 *   an ITC process terminates. It will remove all locates that the
 *   mailbox has outstanding.
 */
/* ===================================================================== */
static void remove_locates(struct itc_mbox *mbox)
{
        struct locate_record *loc, *tmp, **ptmp;

        loc = mbox->loc_list;
        while(loc != NULL) {
                ptmp = tfind(loc, &wi.locate_tree, compare_loc_name_add);
                if(ptmp == NULL) {
                        itc_world_error("Failed to find lacation entry in locate tree for removal");
                } else {
                        if(*ptmp == loc) {
                                tdelete(loc, &wi.locate_tree, compare_loc_name_add);
                                if(loc->next != NULL) {
                                        tsearch(loc->next, &wi.locate_tree,
                                                compare_loc_name_add);
                                }
                        } else {
                                tmp = *ptmp;
                                while(tmp->next != NULL) {
                                        if(tmp->next == loc) {
                                                tmp->next = loc->next;
                                                break;
                                        }
                                        tmp = tmp->next;
                                }
                        }
                }

                loc = loc->mbox_next;
        }
}

/* ===================================================================== */
/**
 *   remove_mbox_from_tree
 *
 *   @param mbox       Pointer to mailbox.
 *
 *   @return           -
 *
 *   @par Globals:     wi
 *                     ITC world global instance structure.
 *
 *   Removes a mailbox from the locate treelist. Return the pointer to
 *   the mailbox in the treelist.
 */
/* ===================================================================== */
static struct itc_mbox *remove_mbox_from_tree(struct itc_mbox *mbox)
{
        struct itc_mbox **ptmp, *tmp, *retp = NULL;

        ptmp = tfind(mbox, &wi.mbox_tree, compare_mb_name_add);

        if(ptmp == NULL) {
                /* This shall never happen, I do not know about error handling here.
                   I will exit the world process for now.                           */
                ITC_WORLD_TRACE_ERROR("Failed to find mbox to remove, mid: 0x%08x",
                                      mbox->mbox_id);

                return NULL;
        } else {
                tmp = *ptmp;

                if(tmp->mbox_id == mbox->mbox_id) {
                        tdelete(tmp, &wi.mbox_tree, compare_mb_name_add);
                        if(tmp->next != NULL) {
                                tsearch(tmp->next, &wi.mbox_tree, compare_mb_name_add);
                        }
                        retp = tmp;
                } else {
                        while(tmp->next != NULL) {
                                if(tmp->next->mbox_id == mbox->mbox_id) {
                                        retp = tmp->next;
                                        tmp->next = tmp->next->next;
                                        break;
                                }
                                tmp = tmp->next;
                        }
                }
        }

        return retp;
}

/* ===================================================================== */
/**
 *   remove_addname_from_tree
 *
 *   @param an         Pointer to addname structure to remove.
 *
 *   @return           -
 *
 *   @par Globals:     wi
 *                     ITC world global instance structure.
 *
 *   Remove an added name from the addname tree.
 */
/* ===================================================================== */
static void remove_addname_from_tree(struct addname *an)
{
        struct addname **ptmp, *tmp;

        ptmp = tfind(an, &wi.addname_tree, compare_addname_add);
        if(*ptmp != an) {
                tmp = *ptmp;
                while(tmp->dupl_next != NULL) {
                        if(tmp->dupl_next == an) {
                                tmp->dupl_next = an->dupl_next;
				break;
                        }
                        tmp = tmp->dupl_next;
                }
        } else {
                tdelete(an, &wi.addname_tree, compare_addname_add);
                if(an->dupl_next != NULL) {
                        tsearch(an->dupl_next, &wi.addname_tree,
                                compare_addname_add);
                }
        }
}

/* ===================================================================== */
/**
 *   remove_all_addname_from_mbox
 *
 *   @param an         Pointer to addname structure to remove.
 *
 *   @return           -
 *
 *   @par Globals:     wi
 *                     ITC world global instance structure.
 *
 *   Remove an added name from the addname tree.
 */
/* ===================================================================== */
static void remove_all_addname_from_mbox(struct itc_mbox *mbox)
{
        struct addname *tmp, *an;

        tmp = mbox->addname_list;
        while(tmp != NULL) {
                an = tmp;
                tmp = tmp->next;
                remove_addname_from_tree(an);
		free(an);
        }
        mbox->addname_list = NULL;
}

/* ===================================================================== */
/**
 *   check_mbox_in_locates
 *
 *   @param mbox_id    Mailbox id of added mailbox.
 *
 *   @param mbox_name  Mailbox name of added mailbox.
 *
 *   @param itc_proc   Not NULL if you have to take the process namespace
 *                     of the added mailbox into account.
 *
 *   @return           -
 *
 *   @par Globals:     wi
 *                     ITC world global instance structure.
 *
 *   Check for a mailbox name in the outstanding locate tree and
 *   send back the locate reply message if any locates found.
 */
/* ===================================================================== */
static void check_mbox_in_locates(itc_mbox_id_t mbox_id,
                                  char *mbox_name,
                                  struct itc_process *itc_proc)
{
        union itc_msg         *msg;
        struct itc_process    *loc_proc;
        struct itc_mbox       *loc_mbox;
        void                 **ptmp;
        struct locate_record  *loc, *prev = NULL, *loctmp, *rem_locs = NULL;

        ptmp = tfind(mbox_name, &wi.locate_tree, compare_loc_name);
        if(ptmp) {
                loc = *ptmp;
                tdelete(*ptmp, &wi.locate_tree, compare_loc_name_add);
                while(loc != NULL) {
                        if(itc_proc != NULL) {
                                loc_proc = find_proc(loc->sender_mbox);
                                if(loc_proc == NULL             ||
                                   loc_proc->state != connected ||
                                   loc_proc->name_space == NULL ||
                                   strcmp(loc_proc->name_space, itc_proc->name_space) != 0) {
                                        prev = loc;
                                        loc = loc->next;
                                        prev->next = rem_locs;
                                        rem_locs = prev;
                                        continue;
                                }
                        }

                        loc_mbox = find_mbox(loc->from_mbox);
                        if(loc_mbox == NULL) {
                                ITC_TRACE_ERROR("Locate resolve send to deleted mailbox: 0x%08x",
                                                loc->from_mbox);
                                prev = loc;
                                loc = loc->next;
                                free(prev);
                                continue;
                        }
                        prev = NULL;
                        loctmp = loc_mbox->loc_list;
                        while(loctmp) {
                                if(loctmp == loc) {
                                        if(prev == NULL) {
                                                loc_mbox->loc_list = loctmp->mbox_next;
                                        } else {
                                                prev->mbox_next = loctmp->mbox_next;
                                        }
                                        break;
                                }
                                prev = loctmp;
                                loctmp  = loctmp->mbox_next;
                        }
                        if(loctmp == NULL) {
                                itc_world_error("Could not find locate rec in mbox");
                        }

                        msg = itc_alloc((sizeof(struct itc_locate_async_repl) +
                                         strlen(loc->mbox_name)),
                                        ITC_LOCATE_ASYNC_REPL);
                        msg->itc_locate_async_repl.mbox_id   = mbox_id;
                        msg->itc_locate_async_repl.from_mbox = loc->from_mbox;
                        msg->itc_locate_async_repl.data      = loc->data;
                        if((mbox_id & wi.world_mask) == (loc->from_mbox & wi.world_mask)) {
                                msg->itc_locate_async_repl.transport = ITC_TRANSPORT_LOCAL;
                        } else {
                                msg->itc_locate_async_repl.transport = ITC_TRANSPORT_SOCK;
                        }
                        strcpy(msg->itc_locate_async_repl.mbox_name, loc->mbox_name);

                        ITC_WORLD_TRACE(itc_world_ITC_LOCATE_ASYNC_REPL, msg->msgno,
                                        msg->itc_locate_async_repl.from_mbox,
                                        (unsigned long)msg->itc_locate_async_repl.data,
                                        msg->itc_locate_async_repl.mbox_id,
                                        msg->itc_locate_async_repl.transport,
                                        msg->itc_locate_async_repl.mbox_name);

                        itc_send(&msg, loc->sender_mbox, ITC_MY_MBOX);
                        prev = loc;
                        loc = loc->next;
                        free(prev);
                }

                if(rem_locs != NULL) {
                        tsearch(rem_locs, &wi.locate_tree, compare_loc_name_add);
                }
        }
}

/* ===================================================================== */
/**
 *   add_mbox
 *
 *   @param mbox_id    Mailbox id of added mailbox.
 *
 *   @param mbox_name  Mailbox name of added mailbox.
 *
 *   @return           -
 *
 *   @par Globals:     wi
 *                     ITC world global instance structure.
 *
 *   Add a new mailbox.
 */
/* ===================================================================== */
static void add_mbox(itc_mbox_id_t mbox_id, char *mbox_name)
{
        struct itc_mbox *mbox, *tmp;
        struct itc_process *itc_proc;
        void **ptmp;

        itc_proc = find_proc(mbox_id);
        if(itc_proc == NULL) {
                itc_world_error("Trying to add a mailbox with an illegal"
                                " id, world id out of range: 0x%08x",
                                mbox_id);
        } else if(itc_proc->state != connected) {
                /* Process not present any more, ignore this mailbox */
                return;
        }

        mbox = malloc(sizeof(struct itc_mbox) + strlen(mbox_name));
        if(mbox == NULL) {
                /* World should generate an error here and exit */
                itc_world_error("Failed to allocate for mbox in world");
        }

        /* Do not do this for LINX mailboxes for now */
        mbox->proc_list_next = itc_proc->proc_list;
        itc_proc->proc_list  = mbox;

        mbox->next      = NULL;
        mbox->loc_list  = NULL;
        mbox->mbox_id   = mbox_id;
        strcpy(mbox->mbox_name, mbox_name);

        /* Initialise monitors */
        mbox->monitors = (struct world_mon_mbox_record *)
                         itc_alloc(sizeof(struct world_mon_mbox_record), 0);
        memset(mbox->monitors, 0, sizeof(struct world_mon_mbox_record));
        mbox->mymonitors = (struct world_mon_mbox_record *)
                           itc_alloc(sizeof(struct world_mon_mbox_record), 0);
        memset(mbox->mymonitors, 0, sizeof(struct world_mon_mbox_record));

        LL_NEW(struct world_mon_mbox_record, mbox->monitors);
        LL_NEW(struct world_mon_mbox_record, mbox->mymonitors);

        mbox->lnhlist      = NULL;
        mbox->addname_list = NULL;

        ptmp = tfind(mbox, &wi.mbox_tree, compare_mb_name_add);
        if(ptmp != NULL) {
                tmp = *ptmp;
                while(tmp->next != NULL) {
                        tmp = tmp->next;
                }
                tmp->next = mbox;
        } else {
                tsearch(mbox, &wi.mbox_tree, compare_mb_name_add);
        }

	event_add_mbox(mbox_id, mbox_name);

        /* Look through outstanding locates to see if any is resolved. */
        if(itc_proc->name_space != NULL) {
                char *tmpname;

                tmpname = strchr(mbox_name, '/');
                if(tmpname != NULL) {
                        check_mbox_in_locates(mbox_id, &tmpname[1], itc_proc);
                }
        }

        check_mbox_in_locates(mbox_id, mbox_name, NULL);
}

/* ===================================================================== */
/**
 *   remove_mbox
 *
 *   @param mbox_id    Mailbox id of added mailbox.
 *
 *   @param mbox_name  Mailbox name of added mailbox.
 *
 *   @return           -
 *
 *   @par Globals:     wi
 *                     ITC world global instance structure.
 *
 *   Remove a mailbox.
 */
/* ===================================================================== */
static void remove_mbox(itc_mbox_id_t mbox_id, char *mbox_name)
{
        struct itc_mbox *tmp, *prev = NULL;
        struct itc_process *itc_proc;

        itc_proc = find_proc(mbox_id);
        if(itc_proc == NULL) {
                itc_world_error("Trying to remove a mailbox with an"
                                " illegal id, world id out of range: 0x%08x",
                                mbox_id);
        } else if(itc_proc->state != connected) {
                /* Process not present any more, ignore this mailbox */
                return;
        }

        for(tmp = itc_proc->proc_list ; tmp != NULL ; tmp = tmp->proc_list_next) {
                if(tmp->mbox_id == mbox_id) {
                        break;
                }
                prev = tmp;
        }

        if(tmp != NULL) {
                if(prev == NULL) {
                        itc_proc->proc_list = tmp->proc_list_next;
                } else {
                        prev->proc_list_next = tmp->proc_list_next;
                }
        } else {
                ITC_WORLD_TRACE_ERROR("Failed to find mbox to remove in proc list, mid: 0x%08x",
                                      mbox_id);
                return;
        }

        tmp = remove_mbox_from_tree(tmp);
        if(tmp != NULL) {
                remove_all_addname_from_mbox(tmp);
                rem_all_lnh_for_mbox(tmp);
                remove_event_subscription(tmp->mbox_id, ITC_EVENT_ALL);
                return_monitors(tmp);
                itc_free((union itc_msg **)&tmp->monitors);
                itc_free((union itc_msg **)&tmp->mymonitors);
                remove_locates(tmp);
                free(tmp);
		event_remove_mbox(mbox_id, mbox_name);
        }
}

/* ===================================================================== */
/**
 *   remove_all_mbox_for_proc
 *
 *   @param proc       Pointer to ITC process structure.
 *
 *   @return           -
 *
 *   @par Globals:     --
 *
 *   Removes all mailboxes for a process.
 */
/* ===================================================================== */
static void remove_all_mbox_for_proc(itc_mbox_id_t world_id)
{
        struct itc_mbox *tmp, *mbox;
        struct itc_process *proc;

        proc = find_proc(world_id);
        if(proc == NULL ||
           proc->state != unused) {
                ITC_WORLD_TRACE_ERROR("Could not find the process 0x%08x",
                                      world_id);
                itc_world_error("Could not find the process to remove all mboxes");
                return;
        }

        mbox = proc->proc_list;
        while(mbox != NULL) {
                tmp = mbox->proc_list_next;
                mbox = remove_mbox_from_tree(mbox);
                if(mbox != NULL) {
                        remove_all_addname_from_mbox(mbox);
                        rem_all_lnh_for_mbox(mbox);
                        remove_event_subscription(mbox->mbox_id, ITC_EVENT_ALL);
                        return_monitors(mbox);
                        itc_free((union itc_msg **)&mbox->monitors);
                        itc_free((union itc_msg **)&mbox->mymonitors);
                        remove_locates(mbox);
                        free(mbox);
                }
                mbox = tmp;
        }

        proc->proc_list = NULL;
}

/* ===================================================================== */
/**
 *   add_name_to_mbox
 *
 *   @param mbox_id    Mailbox id.
 *
 *   @param mbox_name  Name to add to mailbox.
 *
 *   @return           -
 *
 *   @par Globals:     wi
 *                     ITC world global instance structure.
 *
 *   Add a name to a mailbox.
 */
/* ===================================================================== */
static void add_name_to_mbox(itc_mbox_id_t mbox_id, char *name)
{
        struct itc_process *itc_proc;
        struct itc_mbox *mbox;
        struct addname *an, **ptmp;

        itc_proc = find_proc(mbox_id);
        if(itc_proc == NULL) {
                itc_world_error("Trying to add name to a mailbox with "
                                "an illegal id, world id out of "
                                "range: 0x%08x",
                                mbox_id);
        } else if(itc_proc->state != connected) {
                /* Process not present any more, ignore this mailbox */
                return;
        }

        mbox = find_mbox(mbox_id);
        if(mbox == NULL) {
                /* Mailbox not found probably deleted
                   Ignore this request. */
                ITC_WORLD_TRACE_ERROR("Mailbox not found 0x%08x",
                                      mbox_id);
                return;
        }

        an = malloc(sizeof(struct addname) + strlen(name));
        if(an == NULL) {
                itc_world_error("Out of memory error");
        }

        an->mbox = mbox;
        an->dupl_next = NULL;
        strcpy(an->name, name);

        ptmp = tfind(an, &wi.addname_tree, compare_addname_add);
        if(ptmp != NULL) {
                an->dupl_next = (*ptmp)->dupl_next;
                (*ptmp)->dupl_next = an;
        } else {
                tsearch(an, &wi.addname_tree, compare_addname_add);
        }

        an->next = mbox->addname_list;
        mbox->addname_list = an;


        /* Look through outstanding locates to see if any is resolved. */
        if(itc_proc->name_space != NULL) {
                char *tmpname;

                tmpname = strchr(name, '/');
                if(tmpname != NULL) {
                        check_mbox_in_locates(mbox_id, &tmpname[1], itc_proc);
                }
        }

        check_mbox_in_locates(mbox_id, name, NULL);
}

/* ===================================================================== */
/**
 *   locate_mbox
 *
 *   @param msg        Pointer to itc_locate message.
 *
 *   @return           -
 *
 *   @par Globals:     wi
 *                     ITC world global instance structure.
 *
 *   Handles the itc_locate message and sends a response.
 */
/* ===================================================================== */
static void locate_mbox(union itc_msg *msg)
{
        struct itc_process *itc_proc;
        union itc_msg *repl_msg;
        /* antmp is used to search in the addname tree and ptmp in
           the mboxtree*/
        struct itc_mbox *mbox = NULL, **ptmp;
        struct addname **antmp;

        ITC_WORLD_TRACE(itc_world_ITC_LOCATE, msg->msgno,
                        itc_sender(msg),
                        msg->itc_locate.from_mbox,
                        msg->itc_locate.mbox_name);

        repl_msg = itc_alloc((sizeof(struct itc_locate_repl) +
                              strlen(msg->itc_locate.mbox_name)),
                             ITC_LOCATE_REPL);

        itc_proc = find_proc(itc_sender(msg));
        if(itc_proc == NULL) {
                itc_world_error("Trying to locate a mailbox from "
                                "an illegal id, world id out of "
                                "range: 0x%08x",
                                itc_sender(msg));
        } else if(itc_proc->name_space != NULL) {
                char tmpname[ITC_NAME_MAXLEN];

                strcpy(tmpname, itc_proc->name_space);
                strcat(tmpname, msg->itc_locate.mbox_name);
                ptmp = tfind(tmpname, &wi.mbox_tree, compare_mb_name);
                if(ptmp != NULL) {
                        mbox = *ptmp;
                }
                if(mbox == NULL) {
                        antmp = tfind(tmpname, &wi.addname_tree,
                                      compare_addname);
                        if(antmp != NULL) {
                                mbox = (*antmp)->mbox;
                        }
                }
        }

        if(mbox == NULL) {
                ptmp = tfind(msg->itc_locate.mbox_name, &wi.mbox_tree, compare_mb_name);
                if(ptmp != NULL) {
                        mbox = *ptmp;
                } else {
                        antmp = tfind(msg->itc_locate.mbox_name,
                                      &wi.addname_tree,
                                      compare_addname);
                        if(antmp != NULL) {
                                mbox = (*antmp)->mbox;
                        }
                }
        }

        if(mbox != NULL) {
                repl_msg->itc_locate_repl.mbox_id = mbox->mbox_id;
                if((mbox->mbox_id & wi.world_mask) == (msg->itc_locate.from_mbox & wi.world_mask)) {
                        repl_msg->itc_locate_repl.transport = ITC_TRANSPORT_LOCAL;
                } else {
                        repl_msg->itc_locate_repl.transport = ITC_TRANSPORT_SOCK;
                }
        } else {
                repl_msg->itc_locate_repl.mbox_id = ITC_NO_ID;
		event_locate_unresolved(msg->itc_locate.from_mbox,
					msg->itc_locate.mbox_name);
        }
        strcpy(repl_msg->itc_locate_repl.mbox_name, msg->itc_locate.mbox_name);

        ITC_WORLD_TRACE(itc_world_ITC_LOCATE_REPL, repl_msg->msgno,
                        repl_msg->itc_locate_repl.mbox_id,
                        repl_msg->itc_locate_repl.transport,
                        repl_msg->itc_locate_repl.mbox_name);

        itc_send(&repl_msg, itc_sender(msg), ITC_MY_MBOX);
}

/* ===================================================================== */
/**
 *   check_lnh
 *
 *   @param name       Mailbox name to be checked.
 *
 *   @param lnhlen     Length of LNH part of mailbox name.
 *
 *   @param from       Mailbox ID where locate reques originates from.
 *
 *   @return           -
 *
 *   @par Globals:     wi
 *                     ITC world global instance structure.
 *
 *   Check if locate_async needs to go to a linkhandler.
 */
/* ===================================================================== */
static void check_lnh(char *name, itc_mbox_id_t from)
{
        struct lnh_tree *lnht, **tmp;
        struct lnh_entry *lnhe;
        union itc_msg *msg;

        tmp = tfind(name, &wi.lnh_tree, compare_lnh_name);
        if(tmp != NULL) {
                lnht = *tmp;

                for(lnhe = LL_FIRST(struct lnh_tree, lnht);
                    lnhe != NULL ;
                    LL_NEXT(struct lnh_entry, lnhe)) {
                        msg = itc_alloc((sizeof(struct itc_locate_lnh) + strlen(name)),
                                        ITC_LOCATE_LNH);
                        strcpy(msg->itc_locate_lnh.name, name);
                        msg->itc_locate_lnh.from = from;
                        itc_send(&msg, lnhe->mbox_id, ITC_MY_MBOX);
                }
        }
}

/* ===================================================================== */
/**
 *   locate_mbox_async
 *
 *   @param msg        Pointer to itc_locate_async message.
 *
 *   @return           -
 *
 *   @par Globals:     wi
 *                     ITC world global instance structure.
 *
 *   Handles the itc_locate_async message and sends a response.
 */
/* ===================================================================== */
static void locate_mbox_async(union itc_msg *msg)
{
        struct itc_process *itc_proc;
        union itc_msg *repl_msg;
        void **ptmp;
        struct itc_mbox *mbox = NULL, *from_mbox;
        struct locate_record *loc, *tmp;
        struct addname **antmp;
	char *lnhterm;

        ITC_WORLD_TRACE(itc_world_ITC_LOCATE_ASYNC, msg->msgno,
                        itc_sender(msg),
                        msg->itc_locate_async.from_mbox,
                        (unsigned long)msg->itc_locate_async.data,
                        msg->itc_locate_async.mbox_name);

        from_mbox = find_mbox(msg->itc_locate_async.from_mbox);
        if(from_mbox == NULL) {
                ITC_WORLD_TRACE_ERROR("Mailbox that locate comes from, not found 0x%08x",
                                      msg->itc_locate_async.from_mbox);
                return;
        }

        itc_proc = find_proc(itc_sender(msg));
        if(itc_proc == NULL) {
                itc_world_error("Trying to locate a mailbox from "
                                "an illegal id, world id out of "
                                "range: 0x%08x",
                                itc_sender(msg));
        } else if(itc_proc->name_space != NULL) {
                char tmpname[ITC_NAME_MAXLEN];

                strcpy(tmpname, itc_proc->name_space);
                strcat(tmpname, msg->itc_locate_async.mbox_name);
                ptmp = tfind(tmpname, &wi.mbox_tree, compare_mb_name);
                if(ptmp != NULL) {
                        mbox = *ptmp;
                }
                if(mbox == NULL) {
                        antmp = tfind(tmpname, &wi.addname_tree,
                                      compare_addname);
                        if(antmp != NULL) {
                                mbox = (*antmp)->mbox;
                        }
                }
        }

        if(mbox == NULL) {
                ptmp = tfind(msg->itc_locate_async.mbox_name,
                             &wi.mbox_tree, compare_mb_name);
                if(ptmp != NULL) {
                        mbox = *ptmp;
                }
        }

        if(mbox == NULL) {
                antmp = tfind(msg->itc_locate_async.mbox_name,
                              &wi.addname_tree, compare_addname);
                if(antmp != NULL) {
                        mbox = (*antmp)->mbox;
                }
        }

        if(mbox != NULL) {
                repl_msg = itc_alloc((sizeof(struct itc_locate_async_repl) +
                                      strlen(msg->itc_locate_async.mbox_name)),
                                     ITC_LOCATE_ASYNC_REPL);
                repl_msg->itc_locate_async_repl.mbox_id   = mbox->mbox_id;
                repl_msg->itc_locate_async_repl.from_mbox = msg->itc_locate_async.from_mbox;
                repl_msg->itc_locate_async_repl.data      = msg->itc_locate_async.data;
                if((mbox->mbox_id & wi.world_mask) ==
                   (msg->itc_locate_async.from_mbox & wi.world_mask)) {
                        repl_msg->itc_locate_async_repl.transport = ITC_TRANSPORT_LOCAL;
                } else {
                        repl_msg->itc_locate_async_repl.transport = ITC_TRANSPORT_SOCK;
                }
                strcpy(repl_msg->itc_locate_async_repl.mbox_name,
                       msg->itc_locate_async.mbox_name);

                ITC_WORLD_TRACE(itc_world_ITC_LOCATE_ASYNC_REPL, repl_msg->msgno,
                                repl_msg->itc_locate_async_repl.from_mbox,
                                (unsigned long)repl_msg->itc_locate_async_repl.data,
                                repl_msg->itc_locate_async_repl.mbox_id,
                                repl_msg->itc_locate_async_repl.transport,
                                repl_msg->itc_locate_async_repl.mbox_name);

                itc_send(&repl_msg, itc_sender(msg), ITC_MY_MBOX);

        } else {
		event_locate_unresolved(msg->itc_locate_async.from_mbox,
					msg->itc_locate_async.mbox_name);

                lnhterm = strstr(msg->itc_locate_async.mbox_name, "/");
                if(lnhterm != NULL) {
                        check_lnh(msg->itc_locate_async.mbox_name,
                                  msg->itc_locate_async.from_mbox);
                }

                /* Mailbox not created yet add request to locate_tree */
                loc = malloc(sizeof(struct locate_record) +
                             strlen(msg->itc_locate_async.mbox_name));
                loc->next        = NULL;
                loc->sender_mbox = itc_sender(msg);
                loc->from_mbox   = msg->itc_locate_async.from_mbox;
                loc->data        = msg->itc_locate_async.data;
                strcpy(loc->mbox_name, msg->itc_locate_async.mbox_name);

                loc->mbox_next      = from_mbox->loc_list;
                from_mbox->loc_list = loc;

                ptmp = tfind(loc, &wi.locate_tree, compare_loc_name_add);
                if(ptmp == NULL) {
                        tsearch(loc, &wi.locate_tree, compare_loc_name_add);
                } else {
                        tmp = *ptmp;
                        while(tmp->next != NULL) {
                                tmp = tmp->next;
                        }
                        tmp->next = loc;
                }
        }
}

/* ===================================================================== */
/**
 *   monitor
 *
 *   @param msg        Pointer to itc_monitor message.
 *
 *   @return           -
 *
 *   @par Globals:     wi
 *                     ITC world global instance structure.
 *
 *   Handles the itc_monitor message and sends a response.
 */
/* ===================================================================== */
static void monitor(union itc_msg *msg)
{
        struct itc_mbox *mon_mbox, *tar_mbox;
        union itc_msg *tmp;
        struct world_mon_mbox_record *tar_rec, *mon_rec;

        mon_mbox = find_mbox(msg->itc_monitor.from_mbox_id);
        if(mon_mbox == NULL) {
                /* Monitoring mailbox not found probobly deleted
                   Ignore this request. */
                ITC_WORLD_TRACE_ERROR("Monitoring mailbox not found 0x%08x",
                                      msg->itc_monitor.from_mbox_id);
                return;
        }

        tar_mbox = find_mbox(msg->itc_monitor.target_mbox_id);
        if(tar_mbox == NULL) {
                /* Monitored mailbox not found, trigger monitor*/
                tmp = itc_alloc(sizeof(struct itc_trig_monitor),
                                ITC_TRIG_MONITOR);
                tmp->itc_trig_monitor.from_mbox_id   = msg->itc_monitor.from_mbox_id;
                tmp->itc_trig_monitor.target_mbox_id = msg->itc_monitor.target_mbox_id;
                tmp->itc_trig_monitor.mon_id         = msg->itc_monitor.mon_id;
                itc_send(&tmp, msg->itc_monitor.coord_mbox_id, ITC_MY_MBOX);
        } else {
                mon_rec = (struct world_mon_mbox_record *)
                          itc_alloc(sizeof(struct world_mon_mbox_record), 0);
                memset(mon_rec, 0, sizeof(struct world_mon_mbox_record));
                tar_rec = (struct world_mon_mbox_record *)
                          itc_alloc(sizeof(struct world_mon_mbox_record), 0);
                memset(tar_rec, 0, sizeof(struct world_mon_mbox_record));

                mon_rec->coord_mbox_id = msg->itc_monitor.coord_mbox_id;
                mon_rec->mon_id        = msg->itc_monitor.mon_id;
                mon_rec->mbox          = tar_mbox;
                mon_rec->rec           = tar_rec;
                LL_AT_FRONT(struct world_mon_mbox_record,
                            mon_mbox->mymonitors, mon_rec);

                tar_rec->coord_mbox_id = msg->itc_monitor.coord_mbox_id;
                tar_rec->mon_id        = msg->itc_monitor.mon_id;
                tar_rec->mbox          = mon_mbox;
                tar_rec->rec           = mon_rec;
                LL_AT_FRONT(struct world_mon_mbox_record,
                            tar_mbox->monitors, tar_rec);
        }
}

/* ===================================================================== */
/**
 *   unmonitor
 *
 *   @param msg        Pointer to itc_unmonitor message.
 *
 *   @return           -
 *
 *   @par Globals:     wi
 *                     ITC world global instance structure.
 *
 *   Handles the itc_unmonitor message and sends a response.
 */
/* ===================================================================== */
static void unmonitor(union itc_msg *msg)
{
        struct itc_mbox *mbox;

        mbox = find_mbox(msg->itc_unmonitor.from_mbox_id);
        if(mbox == NULL) {
                return;
        }

        unreg_monitor(mbox, msg->itc_unmonitor.mon_id);
}

/* ===================================================================== */
/**
 *   walk_loc_tree
 *
 *   @param nodep      Pointer to current locate record in tree.
 *
 *   @param which      Tells which type of node pass, see search.h.
 *
 *   @param depth      Depth in tree.
 *
 *   @return           -
 *
 *   @par Globals:     wi
 *                     ITC world global instance structure.
 *
 *   Function that is run for all entries in the locate tree when a new
 *   linkhandler is assigned.
 */
/* ===================================================================== */
void walk_loc_tree(const void *nodep,
                   const VISIT which,
                   const int depth)
{
        struct locate_record *loc, **tmp;
        union itc_msg *msg;

        tmp = (struct locate_record **)nodep;
        loc = *tmp;

        if(which == leaf ||
           which == endorder) {
                if(strncmp(asslnhmsg->lnhpath, loc->mbox_name,
                           strlen(asslnhmsg->lnhpath)) == 0) {
                        msg = itc_alloc((sizeof(struct itc_locate_lnh) +
                                         strlen(loc->mbox_name)),
                                        ITC_LOCATE_LNH);
                        strcpy(msg->itc_locate_lnh.name, loc->mbox_name);
                        msg->itc_locate_lnh.from = loc->from_mbox;
                        itc_send(&msg, asslnhmsg->mbox_id, ITC_MY_MBOX);
                }
        }
}

/* ===================================================================== */
/**
 *   set_namespace
 *
 *   @param msg        Pointer to itc_set_namespace message.
 *
 *   @return           -
 *
 *   @par Globals:     wi
 *                     ITC world global instance structure.
 *
 *   Handles the itc_set_namespace message.
 */
/* ===================================================================== */
static void set_namespace(union itc_msg *msg)
{
        struct itc_process *itc_proc;

        itc_proc = find_proc(msg->itc_set_namespace.world_id);
        if(itc_proc == NULL) {
                itc_world_error("Trying to set namespace for an "
                                "illegal world id, out of range: 0x%08x",
                                msg->itc_set_namespace.world_id);
        }

        itc_proc->name_space = malloc(strlen(msg->itc_set_namespace.name_space) + 1);
        if(itc_proc->name_space == NULL) {
                ITC_WORLD_TRACE_ERROR("Failed to allocate for process name_space %d",
                                      strlen(msg->itc_set_namespace.name_space));
                itc_world_error("Failed to allocate for process name_space");
        }

        strcpy(itc_proc->name_space, msg->itc_set_namespace.name_space);
}

/* ===================================================================== */
/**
 *   assign_lnh
 *
 *   @param msg        Pointer to itc_assign_lnh message.
 *
 *   @return           -
 *
 *   @par Globals:     wi
 *                     ITC world global instance structure.
 *
 *   Handles the itc_assign_lnh message.
 */
/* ===================================================================== */
static void assign_lnh(union itc_msg *msg)
{
        struct locate_record **tmp;
        struct itc_mbox *mbox;

        mbox = find_mbox(msg->itc_assign_lnh.mbox_id);
        if(mbox == NULL) {
                /* Trying to assign linkhandler to an nonexisting mailbox.
                   We log this as an error for now */
                ITC_TRACE_ERROR("Mailbox not found for linkhandler assignment, mbox id: 0x%08x lnhpath: %s",
                                msg->itc_assign_lnh.mbox_id, msg->itc_assign_lnh.lnhpath);
                return;
        }

        add_lnh_to_tree(mbox,
                        msg->itc_assign_lnh.lnhpath);

	event_add_lnh(msg->itc_assign_lnh.mbox_id,
		      msg->itc_assign_lnh.lnhpath);

        tmp = tfind(msg->itc_assign_lnh.lnhpath, &wi.locate_tree,
                    compare_lnh_loc_name);
        if(tmp != NULL) {
                asslnhmsg = &msg->itc_assign_lnh;
                twalk(wi.locate_tree, walk_loc_tree);
                asslnhmsg = NULL;
        }
}

/* ===================================================================== */
/**
 *   deassign_lnh
 *
 *   @param msg        Pointer to itc_assign_lnh message.
 *
 *   @return           -
 *
 *   @par Globals:     wi
 *                     ITC world global instance structure.
 *
 *   Handles the itc_deassign_lnh message.
 */
/* ===================================================================== */
static void deassign_lnh(union itc_msg *msg)
{
        struct itc_mbox *mbox;

        mbox = find_mbox(msg->itc_assign_lnh.mbox_id);
        if(mbox == NULL) {
                /* Trying to assign linkhandler to an nonexisting mailbox.
                   We log this as an error for now */
                ITC_TRACE_ERROR("Mailbox not found for linkhandler assignment, mbox id: 0x%08x lnhpath: %s",
                                msg->itc_assign_lnh.mbox_id, msg->itc_assign_lnh.lnhpath);
                return;
        }

        rem_lnh_from_tree(mbox,
                          msg->itc_assign_lnh.lnhpath);

	event_remove_lnh(msg->itc_assign_lnh.mbox_id,
			 msg->itc_assign_lnh.lnhpath);
}

/* ===================================================================== */
/**
 *   locate_lnh_reply
 *
 *   @param msg        Pointer to itc_locate_lnh_reply message.
 *
 *   @return           -
 *
 *   @par Globals:     wi
 *                     ITC world global instance structure.
 *
 *   Handles the itc_locate_lnh_reply message.
 */
/* ===================================================================== */
static void locate_lnh_reply(union itc_msg *msg)
{
/* ITC needs to do nothing for the LNH return signal, the MBOX is added
 * when the linkhandler clones the target mailbox. Why the linkhandler
 * ever should return found == false is beyond me. */
#if 0
        if(msg->itc_locate_lnh_reply.found) {
                add_mbox(msg->itc_locate_lnh_reply.mbox_id,
                         msg->itc_locate_lnh_reply.name);
        } else {
                remove_mbox(msg->itc_locate_lnh_reply.mbox_id,
                            msg->itc_locate_lnh_reply.name);
        }
#endif
}

/* ===================================================================== */
/**
 *   event_add_mbox
 *
 *   @param mbox_id    Mailbox id added.
 *
 *   @param mbox_name  Mailbox name added.
 *
 *   @return           -
 *
 *   @par Globals:     wi
 *                     ITC world global instance structure.
 *
 *   Handles the event of an added mailbox.
 */
/* ===================================================================== */
static void event_add_mbox(uint32_t mbox_id, char *mbox_name)
{
        struct event_record *event_rec;
	union itc_msg *msg;

        for(event_rec = wi.event_sub_list  ;
            event_rec != NULL            ;
            event_rec = event_rec->next) {
		if(event_rec->event_flags & ITC_EVENT_MBOXES_ADDED) {
			msg = itc_alloc((sizeof(struct itc_mbox_added) +
					 strlen(mbox_name)),
					ITC_MBOX_ADDED);
			msg->itc_mbox_added.mbox_id = mbox_id;
			/* Since last has not meaning for these messages
			   we set it to -1 just as an indication. */
			msg->itc_mbox_added.last = -1;
			strcpy(msg->itc_mbox_added.mbox_name, mbox_name);
			itc_send(&msg, event_rec->mbox_id, ITC_MY_MBOX);
		}
	}
}

/* ===================================================================== */
/**
 *   event_remove_mbox
 *
 *   @param mbox_id    Mailbox id removed.
 *
 *   @param mbox_name  Mailbox name removed.
 *
 *   @return           -
 *
 *   @par Globals:     wi
 *                     ITC world global instance structure.
 *
 *   Handles the event of a removed mailbox.
 */
/* ===================================================================== */
static void event_remove_mbox(uint32_t mbox_id, char *mbox_name)
{
        struct event_record *event_rec;
	union itc_msg *msg;

        for(event_rec = wi.event_sub_list  ;
            event_rec != NULL            ;
            event_rec = event_rec->next) {
		if(event_rec->event_flags & ITC_EVENT_MBOXES_REMOVED) {
			msg = itc_alloc((sizeof(struct itc_mbox_removed) +
					 strlen(mbox_name)),
					ITC_MBOX_REMOVED);
			msg->itc_mbox_removed.mbox_id = mbox_id;
			strcpy(msg->itc_mbox_removed.mbox_name, mbox_name);
			itc_send(&msg, event_rec->mbox_id, ITC_MY_MBOX);
		}
	}
}

/* ===================================================================== */
/**
 *   event_add_lnh
 *
 *   @param mbox_id    Mailbox id of linkhandler.
 *
 *   @param lnhpath    Linkhandler path added.
 *
 *   @return           -
 *
 *   @par Globals:     wi
 *                     ITC world global instance structure.
 *
 *   Handles the event of an added linkhandler.
 */
/* ===================================================================== */
static void event_add_lnh(uint32_t mbox_id, char *lnhpath)
{
        struct event_record *event_rec;
	union itc_msg *msg;

        for(event_rec = wi.event_sub_list  ;
            event_rec != NULL            ;
            event_rec = event_rec->next) {
		if(event_rec->event_flags & ITC_EVENT_LNH_ADDED) {
			msg = itc_alloc((sizeof(struct itc_lnh_added) +
					 strlen(lnhpath)),
					ITC_LNH_ADDED);
			msg->itc_lnh_added.mbox_id = mbox_id;
			msg->itc_lnh_added.last = -1;
			strcpy(msg->itc_lnh_added.lnhpath, lnhpath);
			itc_send(&msg, event_rec->mbox_id, ITC_MY_MBOX);
		}
	}
}

/* ===================================================================== */
/**
 *   event_remove_lnh
 *
 *   @param mbox_id    Mailbox id of linkhandler.
 *
 *   @param lnhpath    Linkhandler path removed.
 *
 *   @return           -
 *
 *   @par Globals:     wi
 *                     ITC world global instance structure.
 *
 *   Handles the event of a removed linkhandler.
 */
/* ===================================================================== */
static void event_remove_lnh(uint32_t mbox_id, char *lnhpath)
{
        struct event_record *event_rec;
	union itc_msg *msg;

        for(event_rec = wi.event_sub_list  ;
            event_rec != NULL            ;
            event_rec = event_rec->next) {
		if(event_rec->event_flags & ITC_EVENT_LNH_REMOVED) {
			msg = itc_alloc((sizeof(struct itc_lnh_removed) +
					 strlen(lnhpath)),
					ITC_LNH_REMOVED);
			msg->itc_lnh_removed.mbox_id = mbox_id;
			strcpy(msg->itc_lnh_removed.lnhpath, lnhpath);
			itc_send(&msg, event_rec->mbox_id, ITC_MY_MBOX);
		}
	}
}

/* ===================================================================== */
/**
 *   event_locate_unresolved
 *
 *   @param locating_id   Mailbox id of the locating mailbox.
 *
 *   @param name          Name that failed to be located.
 *
 *   @return              -
 *
 *   @par Globals:        wi
 *                        ITC world global instance structure.
 *
 *   Handles the event of an unresolved locate.
 */
/* ===================================================================== */
static void event_locate_unresolved(uint32_t locating_id, char *name)
{
        struct event_record *event_rec;
	union itc_msg *msg;

        for(event_rec = wi.event_sub_list  ;
            event_rec != NULL            ;
            event_rec = event_rec->next) {
		if(event_rec->event_flags & ITC_EVENT_LOCATE_UNRESOLVED) {
			msg = itc_alloc((sizeof(struct itc_locate_unresolved) +
					 strlen(name)),
					ITC_LOCATE_UNRESOLVED);
			msg->itc_locate_unresolved.mbox_id = locating_id;
			strcpy(msg->itc_locate_unresolved.mbox_name, name);
			itc_send(&msg, event_rec->mbox_id, ITC_MY_MBOX);
		}
	}
}

/* ===================================================================== */
/**
 *   walk_mboxes
 *
 *   @param nodep      Pointer to current mailbox in tree.
 *
 *   @param which      Tells which type of node pass, see search.h.
 *
 *   @param depth      Depth in tree.
 *
 *   @return           -
 *
 *   @par Globals:     wi
 *                     ITC world global instance structure.
 *
 *   Walk linkhandler tree to send notifications of existing
 *   mailboxes.
 */
/* ===================================================================== */
static void walk_mboxes(const void *nodep,
                        const VISIT which,
                        const int depth)
{
        union itc_msg *msg;
        struct itc_mbox *mbox, **res;

        res = (struct itc_mbox **)nodep;
        mbox = *res;

        if(which == leaf ||
           which == endorder) {
                while(mbox != NULL) {
                        msg = itc_alloc((sizeof(struct itc_mbox_added) +
                                         strlen(mbox->mbox_name)),
                                        ITC_MBOX_ADDED);
                        msg->itc_mbox_added.mbox_id = mbox->mbox_id;
                        msg->itc_mbox_added.last = 0;
                        strcpy(msg->itc_mbox_added.mbox_name, mbox->mbox_name);

                        if(walk_data.prevmsg != NULL) {
                                itc_send(&walk_data.prevmsg,
					 walk_data.send_id, ITC_MY_MBOX);
                        }

                        walk_data.prevmsg = msg;

                        mbox = mbox->next;
                }
        }
}

/* ===================================================================== */
/**
 *   get_all_mboxes
 *
 *   @param mbox_id    Mailbox id that shall be notified of all
 *                     mailboxes.
 *
 *   @return           -
 *
 *   @par Globals:     wi
 *                     ITC world global instance structure.
 *
 *   Send notification of all existing mailboxes to a mailbox.
 */
/* ===================================================================== */
static void get_all_mboxes(itc_mbox_id_t mbox_id)
{
        walk_data.prevmsg = NULL;
        walk_data.send_id = mbox_id;

        twalk(wi.mbox_tree, walk_mboxes);

        if(walk_data.prevmsg != NULL) {
                walk_data.prevmsg->itc_mbox_added.last = 1;
                itc_send(&walk_data.prevmsg, walk_data.send_id, ITC_MY_MBOX);
        }
}

/* ===================================================================== */
/**
 *   walk_mboxes
 *
 *   @param nodep      Pointer to current lnh in tree.
 *
 *   @param which      Tells which type of node pass, see search.h.
 *
 *   @param depth      Depth in tree.
 *
 *   @return           -
 *
 *   @par Globals:     wi
 *                     ITC world global instance structure.
 *
 *   Walk linkhandler tree to send notifications of existing
 *   linkhandlers.
 */
/* ===================================================================== */
static void walk_lnhs(const void *nodep,
		      const VISIT which,
		      const int depth)
{
        union itc_msg    *msg;
        struct lnh_tree  *lnht, **res;
        struct lnh_entry *lnhe;

        res = (struct lnh_tree **)nodep;
        lnht = *res;

        if(which == leaf ||
           which == endorder) {

                for(lnhe = LL_FIRST(struct lnh_tree, lnht);
                    lnhe != NULL ;
                    LL_NEXT(struct lnh_entry, lnhe)) {
                         msg = itc_alloc((sizeof(struct itc_lnh_added) +
                                         strlen(lnht->lnhpath)),
                                        ITC_LNH_ADDED);
                        msg->itc_lnh_added.mbox_id = lnhe->mbox_id;
                        msg->itc_lnh_added.last = 0;
                        strcpy(msg->itc_lnh_added.lnhpath, lnht->lnhpath);

                        if(walk_data.prevmsg != NULL) {
                                itc_send(&walk_data.prevmsg,
					 walk_data.send_id, ITC_MY_MBOX);
                        }

                        walk_data.prevmsg = msg;
                }
        }
}

/* ===================================================================== */
/**
 *   get_all_linkhandlers
 *
 *   @param mbox_id    Mailbox id that shall be notified of all
 *                     linkhandlers.
 *
 *   @return           -
 *
 *   @par Globals:     wi
 *                     ITC world global instance structure.
 *
 *   Send notifications of all existing linkhandlers.
 */
/* ===================================================================== */
static void get_all_linkhandlers(itc_mbox_id_t mbox_id)
{
        walk_data.prevmsg = NULL;
        walk_data.send_id = mbox_id;

        twalk(wi.lnh_tree, walk_lnhs);

        if(walk_data.prevmsg != NULL) {
                walk_data.prevmsg->itc_mbox_added.last = 1;
                itc_send(&walk_data.prevmsg, walk_data.send_id, ITC_MY_MBOX);
        } else {
		union itc_msg *msg;

		/* If no linkhandler present send a message with no id and
		   no string but last set to indicate that no linkhandlers
		   are yet created. */
		msg = itc_alloc((sizeof(struct itc_lnh_added) +
				 strlen("")),
				ITC_LNH_ADDED);
		msg->itc_lnh_added.mbox_id = ITC_NO_ID;
		msg->itc_lnh_added.last = 1;
		strcpy(msg->itc_lnh_added.lnhpath, "");

		itc_send(&msg,
			 walk_data.send_id, ITC_MY_MBOX);
 	}
}

/* ===================================================================== */
/**
 *   handle_new_event_sub
 *
 *   @param mbox_id    Mailbox ID to update events subscriptions for.
 *
 *   @param flags      Event flag field of which events to add
 *                     subscription for.
 *
 *   @return           -
 *
 *   @par Globals:     wi
 *                     ITC world global instance structure.
 *
 *   Adds event subscriptions to a mailbox.
 */
/* ===================================================================== */
static void handle_new_event_sub(itc_mbox_id_t mbox_id, uint32_t flags)
{
	if(flags & ITC_EVENT_MBOXES_ADDED) {
		get_all_mboxes(mbox_id);
	}

	if(flags & ITC_EVENT_LNH_ADDED) {
		get_all_linkhandlers(mbox_id);
	}
}

/* ===================================================================== */
/**
 *   update_event_subscription
 *
 *   @param mbox_id    Mailbox ID to update events subscriptions for.
 *
 *   @param flags      Event flag field of which events to add
 *                     subscription for.
 *
 *   @return           -
 *
 *   @par Globals:     wi
 *                     ITC world global instance structure.
 *
 *   Adds event subscriptions to a mailbox.
 */
/* ===================================================================== */
static void update_event_subscription(itc_mbox_id_t mbox_id,
				      uint32_t flags)
{
	struct event_record *event_rec;
	uint32_t new_flags;

	for(event_rec = wi.event_sub_list ;
	    event_rec != NULL             ;
	    event_rec = event_rec->next) {
		if(event_rec->mbox_id == mbox_id) {
			break;
		}
	}

	if(event_rec != NULL) {
		new_flags = flags & event_rec->event_flags;
		event_rec->event_flags |= flags;
	} else {
		event_rec = malloc(sizeof(struct event_record));
		if(event_rec == NULL) {
			itc_world_error("Out of memory error");
		}
		new_flags = flags;
		event_rec->mbox_id = mbox_id;
		event_rec->event_flags = flags;

		event_rec->next = wi.event_sub_list;
		wi.event_sub_list = event_rec;
	}

	handle_new_event_sub(mbox_id, new_flags);
}

/* ===================================================================== */
/**
 *   remove_event_subscription
 *
 *   @param mbox_id    Mailbox id to remove events from.
 *
 *   @param flags      Which event flags to remove.
 *
 *   @return           -
 *
 *   @par Globals:     wi
 *                     ITC world global instance structure.
 *
 *   Removes events subscriptions from a mailbox.
 */
/* ===================================================================== */
static void remove_event_subscription(itc_mbox_id_t mbox_id, uint32_t flags)
{
        struct event_record *event_rec, *prev = NULL;

        for(event_rec = wi.event_sub_list  ;
            event_rec != NULL            ;
            event_rec = event_rec->next) {
                if(event_rec->mbox_id == mbox_id) {
			event_rec->event_flags &= ~flags;

			if(event_rec->event_flags == 0) {
				if(prev == NULL) {
					wi.event_sub_list = event_rec->next;
				} else {
					prev->next = event_rec->next;
				}
				free(event_rec);
			}
			break;
		}
                prev = event_rec;
        }
}

/* ===================================================================== */
/**
 *   get_name
 *
 *   @param msg        Pointer to itc_get_name message.
 *
 *   @return           -
 *
 *   @par Globals:     wi
 *                     ITC world global instance structure.
 *
 *   Handles the itc_get_name message.
 */
/* ===================================================================== */
static void get_name(union itc_msg *msg)
{
        union itc_msg *repl_msg;
        struct itc_mbox *mbox;

        mbox = find_mbox(msg->itc_get_name.mbox_id);

        if(mbox != NULL) {
                repl_msg = itc_alloc((sizeof(struct itc_get_name_repl) +
                                      strlen(mbox->mbox_name)),
                                     ITC_GET_NAME_REPL);
                repl_msg->itc_get_name_repl.found = 1;
                repl_msg->itc_get_name_repl.mbox_id = mbox->mbox_id;
                strcpy(repl_msg->itc_get_name_repl.mbox_name,
                       mbox->mbox_name);
        } else {
                repl_msg = itc_alloc(sizeof(struct itc_get_name_repl),
                                     ITC_GET_NAME_REPL);
                repl_msg->itc_get_name_repl.found = 0;
                repl_msg->itc_get_name_repl.mbox_id = ITC_NO_ID;
                strcpy(repl_msg->itc_get_name_repl.mbox_name,
                       "");
        }

        itc_send(&repl_msg, itc_sender(msg), ITC_MY_MBOX);
}

/* ===================================================================== */
/**
 *   world_receive
 *
 *   @return           -
 *
 *   @par Globals:     wi
 *                     ITC world global instance structure.
 *
 *   Function to handle ITC world receive messages.
 */
/* ===================================================================== */
static void world_receive(void)
{
        union itc_msg      *msg;
        uint32_t            any[] = { 0 };

        msg = itc_receive(any, ITC_NO_TMO, ITC_FROM_ALL);
        switch(msg->msgno) {
                case ITC_ADD_MBOX:

                        ITC_WORLD_TRACE(itc_world_ADD_MBOX, msg->msgno,
                                        msg->itc_add_rem_mailbox.mbox_id,
                                        msg->itc_add_rem_mailbox.mbox_name);

                        add_mbox(msg->itc_add_rem_mailbox.mbox_id,
                                 msg->itc_add_rem_mailbox.mbox_name);
                        break;

                case ITC_REM_MBOX:

                        ITC_WORLD_TRACE(itc_world_REM_MBOX, msg->msgno,
                                        msg->itc_add_rem_mailbox.mbox_id,
                                        msg->itc_add_rem_mailbox.mbox_name);

                        remove_mbox(msg->itc_add_rem_mailbox.mbox_id,
                                    msg->itc_add_rem_mailbox.mbox_name);
                        break;

                case ITC_LOCATE:
                        locate_mbox(msg);
                        break;

                case ITC_LOCATE_ASYNC:
                        locate_mbox_async(msg);
                        break;

                case ITC_MONITOR:
                        monitor(msg);
                        break;

                case ITC_UNMONITOR:
                        unmonitor(msg);
                        break;

                case ITC_SET_NAMESPACE:
                        set_namespace(msg);
                        break;

                case ITC_ASSIGN_LNH:
                        assign_lnh(msg);
                        break;

                case ITC_DEASSIGN_LNH:
                        deassign_lnh(msg);
                        break;

		case ITC_LOCATE_LNH_REPLY:
		        locate_lnh_reply(msg);
			break;

		case ITC_GET_ALL_MBOXES:
		        update_event_subscription(msg->itc_get_all_mboxes.mbox_id,
						  ITC_EVENT_MBOXES_ADDED);
			break;

		case ITC_ADD_NAME:
		        add_name_to_mbox(msg->itc_add_rem_mailbox.mbox_id,
                                         msg->itc_add_rem_mailbox.mbox_name);
			break;

		case ITC_GET_NAME:
		        get_name(msg);
			break;

		case ITC_SUBSCRIBE_EVENTS:
		        update_event_subscription(msg->itc_subscribe_events.mbox_id,
						  msg->itc_subscribe_events.events);
			break;

		case ITC_UNSUBSCRIBE_EVENTS:
		        remove_event_subscription(msg->itc_subscribe_events.mbox_id,
						  msg->itc_subscribe_events.events);
			break;

                default:
                        /* ERROR trace here */
                        ITC_WORLD_TRACE_ERROR("Unexpected message received, msgno 0x%08x sender 0x%08x",
                                              msg->msgno, itc_sender(msg));
                        break;
        }
        itc_free(&msg);
}

/* ===================================================================== */
/**
 *   locate_world
 *
 *   @param sd         Socket descriptor.
 *
 *   @return           -
 *
 *   @par Globals:     wi
 *                     ITC world global instance structure.
 *
 *   The locate_world function handles a request to locate world from an
 *   ITC process.
 */
/* ===================================================================== */
static void locate_world(int sd)
{
        struct itc_locate_world_repl *itc_locate_world_repl;
        struct itc_process *tmp;
        int tmp_sd, res;
        struct sockaddr_un addr;
        socklen_t addrlen = sizeof(struct sockaddr_un);

        ITC_WORLD_TRACE(itc_world_ITC_LOCATE_WORLD, sd);

        tmp_sd = accept(sd, (struct sockaddr *)&addr, &addrlen);
        if(tmp_sd < 0) {
                itc_world_error("accept in itc_world error: %d (%d:%s)",
                                tmp_sd, errno, strerror(errno));
        }

        tmp = get_from_fifo(wi.freelist);

	if(tmp != NULL) {
		add_to_fifo(wi.usedlist, tmp);

		/* Setup new socket for the ITC client */
		tmp->sd = socket(AF_LOCAL, SOCK_STREAM, 0);
		if(tmp->sd < 0) {
			itc_world_error("Socket create failed: %d (%d:%s)",
					tmp->sd, errno, strerror(errno));
		}
		memset(&tmp->addr, 0, sizeof(struct sockaddr_un));
		tmp->addr.sun_family = AF_LOCAL;
		sprintf(tmp->addr.sun_path, "%s%s_0x%08x",
			wi.rundir_path, ITC_WORLD_NAME, tmp->world_id);
		res = bind(tmp->sd, (struct sockaddr *) &tmp->addr, sizeof(tmp->addr));
		if(res < 0) {
			itc_world_error("Socket bind failed: %d (%d:%s)",
					res, errno, strerror(errno));
		}

		res = chmod(tmp->addr.sun_path, 0777);
		if(res < 0) {
			itc_world_error("Failed to chmod socket %s, cause: %d:%s",
					tmp->addr.sun_path, errno, strerror(errno));
		}

		res = listen(tmp->sd, 1);
		if(res < 0) {
			itc_world_error("Socket listen failed: %d (%d:%s)",
					res, errno, strerror(errno));
		}
		tmp->state = listening;
	}

        /* Send response to locate world */
        itc_locate_world_repl = malloc(sizeof(struct itc_locate_world_repl));
        if(itc_locate_world_repl == NULL) {
                itc_world_error("Out of memory error");
        }

        itc_locate_world_repl->msgno         = ITC_LOCATE_WORLD_REPL;
	if(tmp == NULL)
		itc_locate_world_repl->my_id         = ITC_NO_ID;
	else
		itc_locate_world_repl->my_id         = tmp->world_id;
        itc_locate_world_repl->world_mask    = wi.world_mask;
        itc_locate_world_repl->world_mbox_id = wi.mbox_id;

        ITC_WORLD_TRACE(itc_world_ITC_LOCATE_WORLD_REPL,
                        itc_locate_world_repl->my_id,
                        itc_locate_world_repl->world_mask,
                        itc_locate_world_repl->world_mbox_id);

        res = send(tmp_sd, itc_locate_world_repl,
                   sizeof(struct itc_locate_world_repl), 0);
        if(res < 0) {
                itc_world_error("Send world response failed: %d (%d:%s)",
                                res, errno, strerror(errno));
        }
        free(itc_locate_world_repl);

        res = close(tmp_sd);
        if(res < 0) {
                itc_world_error("Close socket failed: %d (%d:%s)",
                                res, errno, strerror(errno));
        }
}

/* ===================================================================== */
/**
 *   locate_world
 *
 *   @param sd         Socket descriptor.
 *
 *   @return           -
 *
 *   @par Globals:     wi
 *                     ITC world global instance structure.
 *
 *   The locate_world function handles a request to locate world from an
 *   ITC process.
 */
/* ===================================================================== */
static void connect_to_itcproc(struct itc_process *itc_proc)
{
        int tmp_sd, res;
        struct sockaddr_un addr;
        socklen_t addrlen = sizeof(struct sockaddr_un);

        ITC_WORLD_TRACE(itc_world_CONNECT_PROC,
                        itc_proc->sd, itc_proc->world_id);

        tmp_sd = accept(itc_proc->sd, (struct sockaddr *)&addr, &addrlen);
        if(tmp_sd < 0) {
                itc_world_error("accept connection in itc_world error: %d (%d:%s)",
                                tmp_sd, errno, strerror(errno));
        }

        res = close(itc_proc->sd);
        if(res < 0) {
                itc_world_error("Close socket failed: %d (%d:%s)",
                                res, errno, strerror(errno));
        }

        itc_proc->sd    = tmp_sd;
        itc_proc->state = connected;
}

/* ===================================================================== */
/**
 *   rm_itc_client
 *
 *   @param itc_proc   Pointer to an ITC process
 *
 *   @return           -
 *
 *   @par Globals:     --
 *
 *   Removes an ITC client.
 */
/* ===================================================================== */
static void rm_itc_client(struct itc_process *itc_proc)
{
	char sockname[256];
        int res, tmp_sd;

        if(itc_proc->sd != -1) {
	        tmp_sd = itc_proc->sd;
		itc_proc->sd = -1;

		while(1) {
			res = close(tmp_sd);
			if(res != 0) {
				if(errno == EINTR)
					continue;
				else if(errno == EBADF) {
					/* Socket as been closed already, we are done. */
					return;
				} else {
					itc_world_error("Socket close failed: %d (%d:%s)",
							tmp_sd, errno, strerror(errno));
				}
			} else {
				break;
			}
		}

		sprintf(sockname, "%s%s_0x%08x",
			wi.rundir_path, ITC_WORLD_NAME, itc_proc->world_id);
		if(unlink(sockname) == -1 &&
		   errno != ENOENT) {
                        itc_world_error("Socket unlink failed: %d (%d:%s)",
					tmp_sd, errno, strerror(errno));
		}
	}


}

/* ===================================================================== */
/**
 *   rm_sysv_msgq
 *
 *   @param proc_id        ITC world process id.
 *
 *   @return               -
 *
 *   @par Globals:         --
 *
 *   Removes an ITC client.
 */
/* ===================================================================== */
static void rm_sysv_msgq(itc_mbox_id_t proc_id)
{
        key_t key;
        int mqid;

        if(wi.msgq_file == NULL) {
                ITC_WORLD_TRACE_ERROR("ITC msgq file path not setup yet", 0);
                return;
        }

        key = ftok(wi.msgq_file, (proc_id >> 20));
        if(key == -1) {
                ITC_WORLD_TRACE_ERROR("Failed to generate key: %d(%s)",
                                      errno, strerror(errno));
                return;
        }

        mqid = msgget(key, 0);
        if(mqid != -1) {
                if(msgctl(mqid, IPC_RMID, NULL) == -1) {
                        ITC_WORLD_TRACE_ERROR("Failed to remove msgq: %d(%s)",
                                              errno, strerror(errno));
                }
        }
}

/* ===================================================================== */
/**
 *   disconnect_from_itcproc
 *
 *   @param itc_proc   Pointer to an ITC process
 *
 *   @return           -
 *
 *   @par Globals:     --
 *
 *   Disconnects from an ITC process removing all mailboxes connected to
 *   the process.
 */
/* ===================================================================== */
static void disconnect_from_itcproc(struct itc_process *itc_proc)
{

        ITC_WORLD_TRACE(itc_world_DISCONNECT_PROC,
                        itc_proc->sd, itc_proc->world_id);

	rm_from_fifo(wi.usedlist, itc_proc);
        itc_proc->state = unused;

        rm_itc_client(itc_proc);

        rm_sysv_msgq(itc_proc->world_id);

        remove_all_mbox_for_proc(itc_proc->world_id);

        itc_proc->name_space = NULL;
        memset(&itc_proc->addr, 0, sizeof(struct sockaddr_un));
        add_to_fifo(wi.freelist, itc_proc);
}

/* ===================================================================== */
/**
 *   is_world_running
 *
 *   @return           0 if no world runnang and -1 if this world
 *                     instance is already running.
 *
 *   @par Globals:     wi
 *                     Global ITC world instance structure.
 *
 *   Checks if this instance of itcworld is already running.
 */
/* ===================================================================== */
static int is_world_running(void)
{
        struct stat s;

        wi.rundir_path = itc_get_rundir();
        if(wi.rundir_path == NULL) {
                return -1;
        }

        if(stat(wi.rundir_path, &s) == 0) {
                /* Directory exists, world running */
                return -1;
        }

        if(errno != ENOENT) {
                return -1;
        }

        return 0;
}

/* ===================================================================== */
/**
 *   create_runenv
 *
 *   @return           0 at success and -1 at failure.
 *
 *   @par Globals:     wi
 *                     Global ITC world instance structure.
 *
 *   Creates the ITC directory and message file.
 */
/* ===================================================================== */
static int create_runenv(void)
{
        FILE *fd;
        int res;

        wi.msgq_file = malloc(strlen(wi.rundir_path) + strlen(ITC_MSGQ_FILE) + 1);
        if(wi.msgq_file == NULL) {
                return -1;
        }

        strcpy(wi.msgq_file, wi.rundir_path);
        strcat(wi.msgq_file, ITC_MSGQ_FILE);

        res = mkdir(wi.rundir_path, 0777);
        if(res < 0) {
                ITC_TRACE_ERROR("Failed to create ITC rundir %s, cause: %d:%s",
                                wi.rundir_path, errno, strerror(errno));
                return -1;
        }

        res = chmod(wi.rundir_path, 0777);
        if(res < 0) {
                ITC_TRACE_ERROR("Failed to chmod ITC rundir %s, cause: %d:%s",
                                wi.rundir_path, errno, strerror(errno));
                return -1;
        }

        fd = fopen(wi.msgq_file, "w");
        if(fd == NULL) {
                ITC_TRACE_ERROR("MSGQ file creation error, cause: %d:%s",
                                errno, strerror(errno));
                return -1;
        }

        if(fclose(fd) != 0) {
                ITC_TRACE_ERROR("MSGQ file close error, cause: %d:%s",
                                errno, strerror(errno));
                return -1;
        }

        return 0;
}

/* ===================================================================== */
/**
 *   main
 *
 *   @param argc       Number of arguments, unused by itc_world.
 *
 *   @param argv       pointer to array of arguments, unused by itc_world.
 *
 *   @return           -
 *
 *   @par Globals:     --
 *
 *   ITC world main function.
 */
/* ===================================================================== */
int main(int argc, char *argv[])
{
        int                 max_sd, res;
        struct sockaddr_un  world_addr;
        fd_set              world_set;
        struct itc_process *tmp;
        struct itc_process *itc_proc;
        int                 i;
        int opt, daemonize = 0;

        while((opt = getopt(argc, argv, "d")) != -1) {
            switch(opt) {
                case 'd':
                    daemonize = 1;
                    break;
                default:
                    break;
            }
        }

        if(daemonize) {
            if(daemon(1,1)) {
                perror("daemon");
                return -1;
            }
        }

        if(is_world_running() != 0) {
                fprintf(stderr, "ERROR: ITC world instance already running\n\n");
                fprintf(stderr, "If itcworld has died uncontrollably you might need\n");
                fprintf(stderr, "to remove dir %s before it can be started again\n",
                       wi.rundir_path);
                exit(-1);
        }

        if(create_runenv() != 0) {
                fprintf(stderr, "ERROR: ITC world create run environment failed\n\n");
                exit(-1);
        }

        world_settings(&wi.world_mask, &wi.world_shift);

	atexit(itc_world_exithandler);

        if(itc_init(2, ITC_MALLOC, NULL, ITC_NO_NAMESPACE,
                    ITC_INIT_FLAG_I_AM_WORLD) != 0) {
                itc_world_error("itc_init failed");
        }

        wi.mbox_id = itc_create_mailbox(ITC_WORLD_NAME, 0);
        if(wi.mbox_id == ITC_NO_ID) {
                itc_world_error("itc_create_mailbox failed");
        }
        wi.mbox_fd = itc_get_fd();

        wi.freelist = create_fifo();
        if(wi.freelist == NULL) {
                itc_world_error("create_fifo freelist failed");
        }

        wi.usedlist = create_fifo();
        if(wi.usedlist == NULL) {
                itc_world_error("create_fifo usedlist failed");
        }

        /* Set world process 0 to be illegal. */
        itc_proc = &wi.procs[0];
        itc_proc->world_id = 0;
        itc_proc->sd       = -1;
        itc_proc->state    = illegal;

        /* Set world process 1 to be the world process.*/
        itc_proc = &wi.procs[1];
        itc_proc->world_id = 1 << wi.world_shift;
        itc_proc->sd       = -1;
        itc_proc->state    = connected;

        for(i=2 ; i<SUPPORTED_PROCESSES ; i++) {
                itc_proc = &wi.procs[i];
                memset(itc_proc, 0, sizeof(struct itc_process));
                itc_proc->world_id = (i) << wi.world_shift;
                itc_proc->sd = -1;
                add_to_fifo(wi.freelist, itc_proc);
        }

        wi.sd = socket(AF_LOCAL, SOCK_STREAM, 0);
	if(wi.sd == -1) {
                itc_world_error("Failed to create socket: %d(%s)\n",
                                errno, strerror(errno));
	}

        memset(&world_addr, 0, sizeof(struct sockaddr_un));
        world_addr.sun_family = AF_LOCAL;
        strcpy(world_addr.sun_path, wi.rundir_path);
        strcat(world_addr.sun_path, ITC_WORLD_LOC_NAME);

        res = bind(wi.sd, (struct sockaddr *) &world_addr, sizeof(world_addr));
        if(res < 0) {
                itc_world_error("bind error: %d (%d:%s)",
                                res, errno, strerror(errno));
        }

        res = chmod(world_addr.sun_path, 0777);
        if(res < 0) {
                itc_world_error("Failed to chmod socket %s, cause: %d:%s",
                                world_addr.sun_path, errno, strerror(errno));
        }

        res = listen(wi.sd, 10);
        if(res < 0) {
                itc_world_error("listen error: %d (%d:%s)",
                                res, errno, strerror(errno));
        }

        for(;;) {
                FD_ZERO(&world_set);
                FD_SET(wi.sd, &world_set);
                max_sd = wi.sd + 1;
                FD_SET(wi.mbox_fd, &world_set);
                if(wi.mbox_fd >= max_sd) {
                        max_sd = wi.mbox_fd + 1;
                }
                for(tmp = wi.usedlist->head ; tmp != NULL ; tmp = tmp->next) {
                        if(tmp->sd != -1) {
                                FD_SET(tmp->sd, &world_set);
                                if(tmp->sd >= max_sd) {
                                        max_sd = tmp->sd + 1;
                                }
                        }
                }
                res = select(max_sd, &world_set, NULL, NULL, NULL);
                if(res < 0) {
                        itc_world_error("select in itc_world error: %d (%d:%s)",
                                        res, errno, strerror(errno));
                } else {
                        if(FD_ISSET(wi.sd, &world_set)) {
                                locate_world(wi.sd);
                        }

                        for(tmp = wi.usedlist->head ;
                            tmp != NULL ;
                            tmp = tmp->next) {
                                if(tmp->sd != -1 &&
				   FD_ISSET(tmp->sd, &world_set)) {
                                        if(tmp->state == listening) {
                                                connect_to_itcproc(tmp);
                                        } else if(tmp->state == connected) {
                                                disconnect_from_itcproc(tmp);
                                                break;
                                        } else {
                                                itc_world_error("select triggered in unexpected state");
                                        }
                                }
                        }

                        if(FD_ISSET(wi.mbox_fd, &world_set)) {
                                world_receive();
                        }
                }
        }

        return 0;
}

/* ===================================================================== */
/**
 *   itc_world_exithandler
 *
 *   @return           -
 *
 *   @par Globals:     --
 *
 *   ITC world handler at program exit.
 */
/* ===================================================================== */
static void itc_world_exithandler(void)
{
	char buffer[256];
	int i;

        if(wi.msgq_file != NULL) {
                unlink(wi.msgq_file);
        }

	for(i=1 ; i<SUPPORTED_PROCESSES ; i++) {
		rm_itc_client(find_proc(i << wi.world_shift));
	}

        if(wi.rundir_path) {
		strcpy(buffer, wi.rundir_path);
		strcat(buffer, ITC_WORLD_LOC_NAME);
		unlink(buffer);
                rmdir(wi.rundir_path);
        }
}

/* ===================================================================== */
/**
 *   itc_world_sighandler
 *
 *   @param signo      Signal number received.
 *
 *   @return           -
 *
 *   @par Globals:     --
 *
 *   ITC world signals handler to handle unexpected termination of ITC
 *   world.
 */
/* ===================================================================== */
static void itc_world_sighandler(int signo)
{
	itc_world_exithandler();

        signal(signo, SIG_DFL);
        raise(signo);
}

/* ===================================================================== */
/**
 *   itc_world_init
 *
 *   @return           -
 *
 *   @par Globals:     --
 *
 *   ITC world init function, only installs the ITC world signals handler.
 *   world.
 */
/* ===================================================================== */
static void itc_world_init(void) {
        /* Dump on all signals */
        signal(SIGSEGV, itc_world_sighandler);
        signal(SIGILL,  itc_world_sighandler);
        signal(SIGABRT, itc_world_sighandler);
        signal(SIGFPE,  itc_world_sighandler);
        signal(SIGTERM, itc_world_sighandler);
        signal(SIGINT,  itc_world_sighandler);
}

/* ===================================================================== */
/**
 *   itc_init_constructor
 *
 *   @return           -
 *
 *   @par Globals:     --
 *
 *   The itc_init_constructor is run before main to install run the
 *   itc_world_init function.
 */
/* ===================================================================== */
void itc_init_constructor() __attribute__ ((constructor)); void itc_init_constructor() {
        itc_world_init();
}
