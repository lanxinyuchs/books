/**
 *   ITC queue implementation
 *
 *   @file itc_q.h
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

#ifndef __ITCQ_H
#define __ITCQ_H

#ifdef __cplusplus
extern "C" {
#endif

/* ========================================================================
 *   INCLUDE FILES
 * ========================================================================
 */

/* ========================================================================
 *   DEFINITIONS
 * ========================================================================
 */

/* ========================================================================
 *   TYPE DEFINITIONS
 * ========================================================================
 */
struct itcq_entry {
        struct itcq_entry *next;
};

struct itcq {
#ifdef ITC_NOLOCKQ
        volatile struct itcq_entry *head;
        volatile struct itcq_entry *tail;
#else
        struct itcq_entry          *head;
        struct itcq_entry          *tail;
        pthread_mutex_t            *lock;
#endif
};

/* ========================================================================
 *   DATA DECLARATIONS
 * ========================================================================
 */
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
struct itcq *q_new(void *first);

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
void q_destroy(struct itcq *q);

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
void q_enqueue(struct itcq *q, void *add);

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
void *q_dequeue(struct itcq *q);


#ifdef __cplusplus
}
#endif

#endif   /* ifndef __ITCQ_H */
