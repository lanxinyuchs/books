/**
 *   ITC queue implementation
 *
 *   @file itc_q.c
 *
 *   ITC queue implementation both lock free and mtuex protected
 *   implementations available for now.
 *
 *   The requirement on the entries in the queue is that they need
 *   to be 16 byte aligned to leave the last 4 bits as ABA field.
 *   The entry also need to start with a next pointer.
 *
 *   For the queue structure it needs to start with head, then tail
 *   and then if needed a pthread_mutex pointer.
 *
 *   Copyright (C) 2014 by Ericsson AB. All rights reserved. The
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
 *   Revised : 2014-01-03 Magnus Lindberg
 *   Change  : First version.
 * ========================================================================
 */

/* ========================================================================
 *   INCLUDE FILES
 * ========================================================================
 */

#include <stdlib.h>
#include <malloc.h>

#ifndef ITC_NOLOCKQ
#include <pthread.h>
#endif

#include "itc_q.h"


/* ========================================================================
 *   DEFINITIONS
 * ========================================================================
 */
#define ABA_MASK 0xF

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
 *   atomic_cas_p
 *
 *   @param v          Pointer that shall be updated.
 *
 *   @param oldval     Pointer to old value.
 *
 *   @param newval     Pointer to new value.
 *
 *   @return           true if the swap was successfull.
 *
 *   @par Globals:     --
 *
 *   Atomic compare and swap function. Relies on gcc specific
 *   functionality.
 */
/* ===================================================================== */
static inline int atomic_cas_p( void *v, void *oldval, void *newval )
{
        return __sync_bool_compare_and_swap((long *)v, (long)oldval, (long)newval);
}

/* ===================================================================== */
/**
 *   q_new
 *
 *   @param first      First entry in the queue.
 *
 *   @return           Pointer to queue, or NULL if queue creation fails.
 *
 *   @par Globals:     --
 *
 *   Creates a new queue.
 */
/* ===================================================================== */
struct itcq *q_new(void *first)
{
        struct itcq *q;

        q = malloc(sizeof(struct itcq));
        if(q == NULL) {
                return NULL;
        }

        ((struct itcq_entry *)first)->next = NULL;
        q->head = first;
        q->tail = first;
#ifndef ITC_NOLOCKQ
        q->lock = malloc(sizeof(pthread_mutex_t));
        if(q->lock == NULL) {
                free(q);
                return NULL;
        }
        if(pthread_mutex_init(q->lock, NULL) != 0) {
                free(q->lock);
                free(q);
                return NULL;
        }
#endif

        return q;
}

/* ===================================================================== */
/**
 *   q_destroy
 *
 *   @param q          Queue (empty) as returned by q_new.
 *
 *   @return           -
 *
 *   @par Globals:     --
 *
 *   Free the resources allocated with q_new.
 */
/* ===================================================================== */
void q_destroy(struct itcq *q)
{
#ifndef ITC_NOLOCKQ
        if(q->lock != NULL) {
                free(q->lock);
        }
#endif
        free(q);
}

/* ===================================================================== */
/**
 *   q_enqueue
 *
 *   @param q          Pointer to queue.
 *
 *   @param add        Entry to add to queue.
 *
 *   @return           -
 *
 *   @par Globals:     --
 *
 *   Enqueue entry to queue.
 */
/* ===================================================================== */
void q_enqueue(struct itcq *q, void *add)
{
#ifdef ITC_NOLOCKQ
        struct itcq_entry *tmp, *tail, *newtail;

        ((struct itcq_entry *)add)->next = NULL;
        while (1) {
                tmp = (struct itcq_entry *)q->tail;
                newtail = (struct itcq_entry *)((unsigned long)add |
                                                (((unsigned long)tmp + 1) &
                                                 ABA_MASK));
                tail = (struct itcq_entry *)((unsigned long)tmp & ~ABA_MASK);

                if (tmp != q->tail) {
                        continue;
                }
                if (atomic_cas_p(&q->tail, tmp, newtail)) {
                        break;
                }
        }

        if (!atomic_cas_p(&tail->next, NULL, add)) {
                abort();
        }
#else
        if(pthread_mutex_lock(q->lock) != 0) {
                abort();
        }
        ((struct itcq_entry *)add)->next = NULL;
        q->tail->next = add;
        q->tail = add;
        if(pthread_mutex_unlock(q->lock) != 0) {
                abort();
        }
#endif
}

/* ===================================================================== */
/**
 *   q_dequeue
 *
 *   @param q          Pointer to queue.
 *
 *   @return           Pointer to entry retrieved from queue or NULL if
 *                     queue empty.
 *
 *   @par Globals:     --
 *
 *   Dequeue message from queue.
 */
/* ===================================================================== */
void *q_dequeue(struct itcq *q)
{
#ifdef ITC_NOLOCKQ
        struct itcq_entry *tmp, *head, *next, *newhead;

        while (1) {
                tmp  = (struct itcq_entry *)q->head;
                head = (struct itcq_entry *)((unsigned long)tmp & ~ABA_MASK);
                next = head->next;
                if (next == NULL) {
                        /* We always keep one entry in the queue to get an optimised
                           implementation.                                           */
                        return NULL; // Empty
                }

                newhead = (struct itcq_entry *)((unsigned long)next |
                                                ((unsigned long)tmp + 1) & ABA_MASK);

                if(tmp != q->head) {
                        continue;
                }
                if (atomic_cas_p(&q->head, tmp, newhead))
                        break;
        }
        head->next = NULL;
#else
        struct itcq_entry *head;

        if(pthread_mutex_lock(q->lock) != 0) {
                abort();
        }
        if(q->head->next != NULL) {
                head = q->head;
                q->head = q->head->next;
        } else {
                head = NULL;
        }
        if(pthread_mutex_unlock(q->lock) != 0) {
                abort();
        }
#endif

        return head;
}
