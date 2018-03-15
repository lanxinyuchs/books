/**
 *   OSE implementation of the ITC API.
 *
 *   @file itc_ose.c
 *
 *
 *   Copyright (C) 2010 by Ericsson AB. All rights reserved. The
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
 *   Revised : [2013-01-14 Magnus Lindberg, magnus.k.lindberg@ericsson.com]
 *   Change  : First version.
 * ========================================================================
 */

/* ========================================================================
 *   INCLUDE FILES
 * ========================================================================
 */
#include "itc.h"
#include "ose.h"

#include "stdbool.h"
#include "string.h"

#ifdef OSE_DELTA
#include "ose_spi.h"
#else
#include "tcb.h"
#endif

/* ========================================================================
 *   DEFINITIONS
 * ========================================================================
 */

/* ========================================================================
 *   TYPE DEFINITIONS
 * ========================================================================
 */

/* ========================================================================
 *   DATA DECLARATIONS
 * ========================================================================
 */

/* ========================================================================
 *   FUNCTION PROTOTYPES
 * ========================================================================
 */

/* ===================================================================== */
/**
 *   Since this implementation only implements what amounts to short
 *   wrapper functions and there are good function header explanations
 *   for the function in itc.h we have no function header comments in
 *   this file.
 */
/* ===================================================================== */
int __itc_init (int32_t mailbox_count,
                 itc_alloc_scheme alloc_scheme,
                 union itc_scheme *scheme,
                 char *name_space,
                 uint32_t flags,
                 const char *file,
                 int line);
{
        return 0;
}

void __itc_exit (const char *file, int line)
{
}

union itc_msg *__itc_alloc(size_t size,
                           uint32_t msgno,
                           const char *file,
                           int line)
{
        return (union itc_msg *)alloc(size, msgno);
}

void __itc_free(union itc_msg **msg, const char *file, int line)
{
        free_buf((union SIGNAL **)msg);
}

itc_mbox_id_t __itc_sender(union itc_msg *msg, const char *file, int line)
{
        return sender((union SIGNAL **)&msg);
}

itc_mbox_id_t __itc_receiver(union itc_msg *msg, const char *file, int line)
{
#ifdef OSE_DELTA
        return addressee((union SIGNAL **)&msg);
#else
        return ITC_NO_ID;
#endif
}

size_t __itc_size(union itc_msg *msg, const char *file, int line)
{
        size_t size = 0;

#ifdef OSE_DELTA
        size = sigsize((union SIGNAL **)&msg);
#else
        extern __thread  tcb_t  *lits_data;

        size = linx_sigsize(lits_data->lh, (union SIGNAL **)&msg);
#endif

        return size;
}

itc_mbox_id_t __itc_create_mailbox(const char *name,
                              uint32_t flags,
                              const char *file,
                              int line)
{
        return current_process();
}

void __itc_delete_mailbox(itc_mbox_id_t mbox_id, const char *file, int line)
{
}

itc_mbox_id_t __itc_current_mbox(const char *file, int line)
{
        return current_process();
}

itc_mbox_id_t __itc_clone_mailbox(itc_mbox_id_t mbox_id,
                             const char *name,
                             const char *file,
                             int line)
{
        /* Not supported now !!! */
        return ITC_NO_ID;
}

int __itc_add_name(itc_mbox_id_t mbox_id,
                   const char *name,
                   const char *file,
                   int line)

{
        /* Not supported now !!! */
        return ITC_NO_ID;
}

itc_mbox_id_t __itc_locate(const char *name, const char *file, int line)
{
        PROCESS pid;
        bool res;

        res = hunt(name, 0, &pid, NULL);

        if(!res)
                return ITC_NO_ID;

        return pid;
}

void __itc_locate_async(const char *name,
                        union itc_msg **msg,
                        itc_mbox_id_t from,
                        const char *file,
                        int line)
{
        union SIGNAL *sig;
        PROCESS pid;

        if(msg == NULL) {
                sig = alloc(sizeof(SIGSELECT),
                            ITC_MONITOR_DEFAULT_NO);
        } else {
                sig = (union SIGNAL *)*msg;
                *msg = NULL;
        }

        hunt(name, 0, &pid, &sig);
}

int __itc_get_fd(const char *file, int line)
{
        return -1;
}

int32_t __itc_get_name(itc_mbox_id_t mbox_id,
                    char *name,
                    uint32_t name_len,
                    const char *file,
                    int line)
{
        struct OS_pcb *pcb = NULL;
        int32_t ret = 0;

        pcb = get_pcb(mbox_id);

        if (!pcb)
                return ret;

        if (strlen(&pcb->strings[pcb->name]) < name_len) {
                strcpy(name, &pcb->strings[pcb->name]);
                ret = 1;
        }

        free_buf((union SIGNAL**)(void*)&pcb);
        return ret;
}

void __itc_send(union itc_msg **msg,
                itc_mbox_id_t to,
                itc_mbox_id_t from,
                const char *file,
                int line)
{
        if(from == ITC_MY_MBOX) {
                send((union SIGNAL **)msg, to);
        } else {
                send_w_s((union SIGNAL **)msg, from, to);
        }
}

union itc_msg *__itc_receive(const uint32_t *filter,
                             int32_t tmo,
                             itc_mbox_id_t from,
                             const char *file,
                             int line)
{
        SIGSELECT *use_filter, any[] = { 0 };
        union itc_msg *msg;

        if(filter == NULL) {
                use_filter = any;
        } else {
                use_filter = (SIGSELECT *)filter;
        }

        if(from == ITC_FROM_ALL) {
                if(tmo == ITC_NO_TMO) {
                        msg = (union itc_msg *)receive((SIGSELECT *)use_filter);
                } else {
                        msg = (union itc_msg *)receive_w_tmo(tmo, (SIGSELECT *)use_filter);
                }
        } else {
                msg = (union itc_msg *)receive_from(tmo, (SIGSELECT *)use_filter, from);
        }

        return msg;
}

itc_monitor_id_t __itc_monitor(itc_mbox_id_t who,
                               union itc_msg **msg,
                               const char *file,
                               int line)
{
        OSATTREF attref;

        if(msg == NULL) {
                union SIGNAL *sig;

                sig = alloc(sizeof(SIGSELECT), ITC_MONITOR_DEFAULT_NO);
                attref = attach(&sig, who);
        } else {
                attref = attach((union SIGNAL **)msg, who);
        }

        return (itc_monitor_id_t)attref;
}

void __itc_unmonitor(itc_monitor_id_t monitor_id,
                     const char *file,
                     int line)
{
        OSATTREF attref = (OSATTREF)monitor_id;

        detach(&attref);
}

void __itc_register_errorhandler(itc_errorhandler errh)
{
        return;
}
