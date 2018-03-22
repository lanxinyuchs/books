/**
 *   [Enter a brief one-liner file comment and leave next line empty.]
 *
 *   @file itc_local.c
 *
 *   [Enter a longer description about the file. More than one line
 *   can be used, but leave an empty line before the copyright line.]
 *
 *   [All text within [...] should be changed or removed.]
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
 *   Revised : [2010-02-01 Lars Magnus Ericsson]
 *   Change  : First version.
 *
 *   Revised : 2014-02-05 Magnus Lindberg magnus.k.lindberg@ericsson.com
 *   Change  : Added support in ITC to work after a fork.
 *             Removed init of itci_create_ep to NULL.
 *
 *   Revised : 2014-02-18 Magnus Lindberg magnus.k.lindberg@ericsson.com
 *   Change  : Added support for determining max message size.
 *             Added error codes for itc_init.
 * ========================================================================
 */

/* ========================================================================
 *   INCLUDE FILES
 * ========================================================================
 */
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include "itc.h"
#include "itc_impl.h"
#include "itci_internal.h"

/* ========================================================================
 *   DEFINITIONS
 * ========================================================================
 */

/* ========================================================================
 *   TYPE DEFINITIONS
 * ========================================================================
 */
struct local_mbox_data {
        itc_mbox_id_t    mbox_id;
        uint32_t         flags;
        struct llqueue  *rxq;        /* linkage for messages pending    */
};

struct local_instance {
        itc_mbox_id_t           my_world_id;
        itc_mbox_id_t           world_mask;
        itc_mbox_id_t           mbox_mask;

        int                     nr_mboxes;
        struct local_mbox_data *mboxes;
};

/* ========================================================================
 *   DATA DECLARATIONS
 * ========================================================================
 */
static struct local_instance local_instance;

/* ========================================================================
 *   FUNCTION PROTOTYPES
 * ========================================================================
 */
static int local_init(itc_mbox_id_t  my_world_id,
                      itc_mbox_id_t  world_mask,
                      int            mbox_count,
                      uint32_t       flags);

static int local_exit(void);

static int local_create_mailbox(struct itc_mailbox *mbox, uint32_t flags);

static int local_delete_mailbox(struct itc_mailbox *mbox);

static int local_send(struct itc_mailbox *mbox, struct itc_message *message,
                      itc_mbox_id_t to, itc_mbox_id_t from);

static struct itc_message *local_receive(struct itc_mailbox *mbox,
                                         const uint32_t *filter,
                                         long tmo,
                                         itc_mbox_id_t from,
                                         bool recursive_call);

static struct itc_message *local_remove(struct itc_mailbox *mbox,
                                        struct itc_message *message);

struct itci_transport_funcs local_itc_funcs = { NULL,
                                                local_init,
                                                local_exit,
                                                local_create_mailbox,
                                                local_delete_mailbox,
                                                local_send,
                                                local_receive,
                                                local_remove,
                                                NULL
};

/* ===================================================================== */
/**
 *   queue_new
 *
 *   @return           Pointer to new queue, NULL at failure.
 *
 *   @par Globals:     --
 *
 *   Create a new local receive queue.
 */
/* ===================================================================== */
struct llqueue *queue_new(void)
{
        struct llqueue *llq;

        llq = malloc(sizeof(struct llqueue));
        if(llq != NULL) {
                llq->head   = NULL;
                llq->tail   = NULL;
                llq->search = NULL;
        }

        return llq;
}

/* ===================================================================== */
/**
 *   queue_enqueue
 *
 *   @param q          Pointer to receive queue.
 *
 *   @param message    Pointer to message to add to queue.
 *
 *   @return           -
 *
 *   @par Globals:     --
 *
 *   Enqueue message to receive queue.
 */
/* ===================================================================== */
void queue_enqueue(struct llqueue *q,
                   struct itc_message *message)
{
        if(q == NULL) {
                return;
        }

        if(q->tail == NULL) {
                q->head = message;
                q->tail = message;
        } else {
                q->tail->next = message;
                q->tail = message;
        }
        message->next  = NULL;
        message->flags |= ITC_MSG_INRX;
}

/* ===================================================================== */
/**
 *   queue_dequeue
 *
 *   @param q          Pointer to receive queue.
 *
 *   @return           Pointer to message retrieved from queue, NULL if
 *                     queue empty.
 *
 *   @par Globals:     --
 *
 *   Dequeue message from receive queue.
 */
/* ===================================================================== */
struct itc_message *queue_dequeue(struct llqueue *q)
{
        struct itc_message *msg;

        if(q == NULL) {
                return NULL;
        }

        if(q->head == NULL) {
                return NULL;
        }

        msg = q->head;

        if(q->head == q->tail) {
                q->tail = q->head->next;
        }
        q->head = q->head->next;

        msg->next = NULL;
        msg->flags &= ~ITC_MSG_INRX;

        return msg;
}

/* ===================================================================== */
/**
 *   deliver_message
 *
 *   @param message    Pointer to message to check.
 *
 *   @param filter     Message filter configuration.
 *
 *   @param from       Message from filter.
 *
 *   @return           true if message is selected by filter, false
 *                     otherwise.
 *
 *   @par Globals:     --
 *
 *   Checks if a message is selected by the receive filter and from
 *   settings.
 */
/* ===================================================================== */
static bool deliver_message(struct itc_message  *message,
                            const uint32_t      *filter,
                            itc_mbox_id_t        from)
{
        int i;
        bool deliver_msg = false;
        uint32_t *filter_nrs;
        int32_t   n_filter;

        if(filter == ITC_NOFILTER) {
                n_filter = 0;
        } else {
                n_filter = (int32_t)filter[0];
        }

        filter_nrs = (uint32_t *)&filter[1];

        if(from == ITC_FROM_ALL ||
           from == message->sender) {
                if(n_filter > 0) {
                        for(i=0 ; i<n_filter ; i++) {
                                if(message->msgno == filter_nrs[i]) {
                                         deliver_msg = true;
                                         break;
                                }
                        }
                } else if(n_filter < 0) {
                        deliver_msg = true;
                        for(i=0 ; i<abs(n_filter) ; i++) {
                                if(message->msgno == filter_nrs[i]) {
                                        deliver_msg = false;
                                        break;
                                }
                        }
                } else {
                        deliver_msg = true;
                }
        }

        return deliver_msg;
}


/* ===================================================================== */
/**
 *   queue_search
 *
 *   @param q          Pointer to receive queue.
 *
 *   @param cont       Continuation from a previous search.
 *
 *   @param filter     Message filter configuration.
 *
 *   @param from       Message from filter.
 *
 *   @return           Pointer to message found or NULL if no applicable
 *                     message found in queue.
 *
 *   @par Globals:     --
 *
 *   Search through receive queue for a message selected by filter and
 *   from.
 */
/* ===================================================================== */
struct itc_message *queue_search(struct llqueue *q,
                                 bool            cont,
                                 const uint32_t *filter,
                                 itc_mbox_id_t   from)
{
        struct itc_message *tmp, *prev = NULL;

        if(q == NULL) {
                return NULL;
        }

        if(cont &&
           q->search != NULL) {
                tmp = q->search;
        } else {
                tmp = q->head;
        }

        while(tmp != NULL) {
                if(deliver_message(tmp, filter, from)) {
                        q->search = NULL;
                        break;
                }
                prev = tmp;
                tmp = tmp->next;
        }

        if(tmp == NULL) {
                q->search = prev;
        } else {
                if(tmp == q->head) {
                        q->head = tmp->next;
                        if(q->head == NULL) {
                                q->tail = NULL;
                        }
                } else {
                        prev->next = tmp->next;
                        if(q->tail == tmp) {
                                q->tail = prev;
                        }
                }

                tmp->next   = NULL;
                tmp->flags &= ~ITC_MSG_INRX;
       }

        return tmp;
}

/* ===================================================================== */
/**
 *   queue_remove
 *
 *   @param q          Pointer to receive queue.
 *
 *   @param message    Pointer to message to remove from queue.
 *
 *   @return           Pointer to message found or NULL if no applicable
 *                     message found in queue.
 *
 *   @par Globals:     --
 *
 *   Remove message from queue.
 */
/* ===================================================================== */
struct itc_message *queue_remove(struct llqueue *q,
                                 struct itc_message *message)
{
        struct itc_message *tmp, *prev = NULL;

        tmp = q->head;
        while(tmp != NULL) {
                if(tmp == message) {
                        break;
                }
                prev = tmp;
                tmp = tmp->next;
        }

        if(tmp != NULL) {
                if(tmp == q->head) {
                        q->head = tmp->next;
                        if(q->head == NULL) {
                                q->tail = NULL;
                        }
                } else {
                        prev->next = tmp->next;
                        if(q->tail == tmp) {
                                q->tail = prev;
                        }
                }

                tmp->flags &= ~ITC_MSG_INRX;
        }

        return tmp;
}

/* ===================================================================== */
/**
 *   find_local_mbox
 *
 *   @param mbox_id    Mailbox id of mailbox to find.
 *
 *   @return           Pointer to Linx mailbox, or NULL if no mailbox
 *                     found.
 *
 *   @par Globals:     local_instance
 *                     Local instance data structure.
 *
 *   Find a local Linx mailbox.
 *   from.
 */
/* ===================================================================== */
struct local_mbox_data *find_local_mbox(itc_mbox_id_t mbox_id)
{
        if(local_instance.mboxes == NULL) {
                return NULL;
        }

        if((mbox_id & local_instance.world_mask) == local_instance.my_world_id) {
                return &(local_instance.mboxes[mbox_id & local_instance.mbox_mask]);
        }

        return NULL;
}

/* ===================================================================== */
/**
 *   free_local_resources
 *
 *   @return           -
 *
 *   @par Globals:     local_instance
 *                     Local instance data structure.
 *
 *   Function that frees all of the ITC local transports resources.
 *   Used after fork.
 */
/* ===================================================================== */
static void free_local_resources(void)
{
        struct local_mbox_data *local_mbox;
        struct itc_message *message;
        union itc_msg *msg;
        int i;

        for(i=0 ; i<local_instance.nr_mboxes ; i++) {
                local_mbox = find_local_mbox(local_instance.my_world_id | i);

                if(!(local_mbox->flags & ITC_CREATE_CLONE)) {
                        while((message = queue_dequeue(local_mbox->rxq)) != NULL) {
                                msg = MESSAGE_TO_MSG(message);
                                itc_free(&msg);
                        }

                        free(local_mbox->rxq);
                }
                local_mbox->rxq = NULL;
        }

        free(local_instance.mboxes);
        memset(&local_instance, 0, sizeof(struct local_instance));
}

/* ===================================================================== */
/**
 *   local_init
 *
 *   @param my_world_id  My world id, from world.
 *
 *   @param world_mask   World mask.
 *
 *   @param mbox_count   How many local mailboxes shall be supported.
 *
 *   @return           0 at success.
 *
 *   @par Globals:     local_instance
 *                     Local instance data structure.
 *
 *   Init function of the ITC by local transport functionality, called
 *   by itc_init.
 */
/* ===================================================================== */
static int local_init(itc_mbox_id_t  my_world_id,
                      itc_mbox_id_t  world_mask,
                      int            mbox_count,
                      uint32_t       flags)
{
        uint32_t mask, nr_mboxes;

        if(local_instance.mboxes != NULL) {
                if(flags & ITC_INIT_FORCE_REDO) {
                        free_local_resources();
                } else {
                        return ITC_EALREADY_INITIALISED;
                }
        }
        local_instance.my_world_id = my_world_id;
        local_instance.world_mask  = world_mask;

        mask = 0xFFFFFFFF >> FFS(mbox_count);
        nr_mboxes = mask + 1;

        local_instance.mbox_mask = mask;
        local_instance.mboxes = malloc(nr_mboxes * sizeof(struct local_mbox_data));
        if(local_instance.mboxes == NULL) {
                ITC_TRACE_ERROR("Failed to allocate memory for the %d local mboxes", nr_mboxes);
                return ITC_EOUT_OF_MEMORY;
        }
        memset(local_instance.mboxes, 0, (nr_mboxes * sizeof(struct local_mbox_data)));
        local_instance.nr_mboxes = nr_mboxes;

        return 0;
}

/* ===================================================================== */
/**
 *   local_exit
 *
 *   @param mbox       Pointer to mailbox.
 *
 *   @return           0 at success.
 *
 *   @par Globals:     local_instance
 *                     Local instance data structure.
 *
 *   Exit function of the ITC by local transport  functionality, called
 *   by itc_exit.
 */
/* ===================================================================== */
static int local_exit(void)
{
        struct local_mbox_data *mbox;
        int i, open_mboxes = 0;

        for(i=0 ; i<local_instance.nr_mboxes ; i++) {
                mbox = find_local_mbox(local_instance.my_world_id | i);
                if(mbox == NULL) {
                        ITC_TRACE_ERROR("Can not find local mbox in itc_exit", 0);
                        return -1;
                }

                if(mbox->rxq != NULL) {
                        open_mboxes++;
                }

                if(open_mboxes > ITC_USED_MBOXES) {
                        ITC_TRACE_ERROR("Local mailboxes in use at itc_exit", 0);
                        return -1;
                }
        }

        free(local_instance.mboxes);
        local_instance.mboxes = NULL;

        return 0;
}

/* ===================================================================== */
/**
 *   local_create_mailbox
 *
 *   @param mbox       Pointer to mailbox.
 *
 *   @return           0 at success.
 *
 *   @par Globals:     --
 *
 *   Create mailbox function of the ITC by Linxlocal transport
 *   functionality, called by itc_create_mailbox.
 */
/* ===================================================================== */
static int local_create_mailbox(struct itc_mailbox *mbox, uint32_t flags)
{
        struct local_mbox_data *local_mbox;

        local_mbox = find_local_mbox(mbox->mbox_id);
        if(local_mbox == NULL) {
                ITC_TRACE_ERROR("Failed to find the local mbox 0x%08x", mbox->mbox_id);
                return -1;
        }

        local_mbox->mbox_id = mbox->mbox_id;
        local_mbox->flags   = flags;

        if(flags & ITC_CREATE_CLONE) {
                struct local_mbox_data *parent_mbox;

                parent_mbox = find_local_mbox(mbox->parent_mbox->mbox_id);
                if(parent_mbox == NULL) {
                        ITC_TRACE_ERROR("Missing parent mbox at clone create", 0);
                }
                local_mbox->rxq = parent_mbox->rxq;
        } else {
                local_mbox->rxq = queue_new();
                if(local_mbox->rxq == NULL) {
                        ITC_TRACE_ERROR("Failed to allocate memory for RX local queue", 0);
                        return -1;
                }
        }

        return 0;
}

/* ===================================================================== */
/**
 *   local_delete_mailbox
 *
 *   @param mbox       Pointer to mailbox.
 *
 *   @return           0 at success.
 *
 *   @par Globals:     --
 *
 *   Delete mailbox function of the ITC by local transport functionality,
 *   called by itc_delete_mailbox.
 */
/* ===================================================================== */
static int local_delete_mailbox(struct itc_mailbox *mbox)
{
        struct local_mbox_data *local_mbox;
        struct itc_message *message;
        union itc_msg *msg;

        local_mbox = find_local_mbox(mbox->mbox_id);
        if(local_mbox == NULL) {
                ITC_TRACE_ERROR("Failed to find the local mbox 0x%08x(0x%08x) to delete",
                                mbox->mbox_id, local_instance.my_world_id);
                return -1;
        }

        if(!(local_mbox->flags & ITC_CREATE_CLONE)) {
                while((message = queue_dequeue(local_mbox->rxq)) != NULL) {
                        msg = MESSAGE_TO_MSG(message);
                        itc_free(&msg);
                }

                free(local_mbox->rxq);
        }
        local_mbox->rxq = NULL;

        return 0;
}

/* ===================================================================== */
/**
 *   local_send
 *
 *   @param mbox       Pointer to mailbox.
 *
 *   @param message    Pointer to message to be sent.
 *
 *   @param to         Mailbox id of message receipient.
 *
 *   @param from       Mailbox id of message sender.
 *
 *   @return           0 at success.
 *
 *   @par Globals:     --
 *
 *   Send message function of the ITC by local transport functionality,
 *   called by itc_send.
 */
/* ===================================================================== */
static int local_send(struct itc_mailbox *mbox, struct itc_message *message,
                      itc_mbox_id_t to, itc_mbox_id_t from)
{
        struct local_mbox_data *to_mbox;

        to_mbox = find_local_mbox(to);
        if(to_mbox == NULL) {
                /* Not local send return */
                return 1;
        }

        queue_enqueue(to_mbox->rxq, message);

        return 0;
}

/* ===================================================================== */
/**
 *   local_receive
 *
 *   @param mbox       Pointer to mailbox.
 *
 *   @param filter     Receive message filter.
 *
 *   @param tmo        Receive timeout.
 *
 *   @param from       Mailbox id of message sender. ITC_FROM_ALL to
 *                     receive from all mailboxes.
 *
 *   @param recursive_call  Recursive call from the same receive.
 *
 *   @return           Pointer to received message or NULL if no message
 *                     received within timeout.
 *
 *   @par Globals:     --
 *
 *   Receive message function of the ITC by local transport functionality,
 *   called by itc_receive.
 */
/* ===================================================================== */
static struct itc_message *local_receive(struct itc_mailbox *mbox,
                                         const uint32_t *filter,
                                         long tmo,
                                         itc_mbox_id_t from,
                                         bool recursive_call)
{
        struct local_mbox_data *local_mbox;
        struct itc_message *message;

        local_mbox = find_local_mbox(mbox->mbox_id);
        if(local_mbox == NULL) {
                ITC_TRACE_ERROR("Failed to find local mbox 0x%08x to receive from",
                                mbox->mbox_id);
                return NULL;
        }

        if(recursive_call) {
                local_mbox->rxq->search = NULL;
        }

        if(from == ITC_FROM_ALL && ((filter == NULL) ||
                                    *filter == 0)) {
                /* receive all */
                message = queue_dequeue(local_mbox->rxq);
        } else {
                message = queue_search(local_mbox->rxq, false, filter, from);

        }

        if(message != NULL) {
                return message;
        } else {
                return NULL;
        }
}

/* ===================================================================== */
/**
 *   local_receive
 *
 *   @param mbox       Pointer to mailbox.
 *
 *   @param message    Pointer to message to be removed.
 *
 *   @return           Pointer to removed message or NULL if message
 *                     not found in RX queue.
 *
 *   @par Globals:     --
 *
 *   Remove message function of the ITC by local transport functionality,
 *   called by itc_remove.
 */
/* ===================================================================== */
static struct itc_message *local_remove(struct itc_mailbox *mbox,
                                        struct itc_message *message)
{
        struct local_mbox_data *local_mbox;
        struct itc_message *tmp;

        local_mbox = find_local_mbox(mbox->mbox_id);
        if(local_mbox == NULL) {
                ITC_TRACE_ERROR("Failed to find local mbox 0x%08x to receive from",
                                mbox->mbox_id);
                return NULL;
        }

        tmp = queue_remove(local_mbox->rxq, message);

        return tmp;
}
