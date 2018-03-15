/**
 *   [Enter a brief one-liner file comment and leave next line empty.]
 *
 *   @file itc_pool.c
 *
 *   [Enter a longer description about the file. More than one line
 *   can be used, but leave an empty line before the copyright line.]
 *
 *   [All text within [...] should be changed or removed.]
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
 *   Revised : 2010-02-06 Magnus Lindberg
 *   Change  : First version.
 *
 *   Revised : 2014-02-05 Magnus Lindberg magnus.k.lindberg@ericsson.com
 *   Change  : Added support in ITC to work after a fork.
 *             Collected queue handling into itc_q.c.
 *
 *   Revised : 2014-02-18 Magnus Lindberg magnus.k.lindberg@ericsson.com
 *   Change  : Added support for determining max message size.
 *             Added error codes for itc_init.
 *
 *   Revised : 2014-10-28 Magnus Lindberg magnus.k.lindberg@ericsson.com
 *   Change  : Corrected itc_alloc to support more than 65500 byte
 *             messages.
 * ========================================================================
 */

/* ========================================================================
 *   INCLUDE FILES
 * ========================================================================
 */
#include "stdlib.h"
#include "stdint.h"
#include "string.h"

#include "itc.h"
#include "itc_impl.h"
#include "itci_alloc.h"
#include "itc_q.h"

/* ========================================================================
 *   DEFINITIONS
 * ========================================================================
 */

/* ========================================================================
 *   TYPE DEFINITIONS
 * ========================================================================
 */
struct pool_data {
        unsigned int        pool_size;
        unsigned char      *pool_start;
        unsigned char      *pool_end;
        unsigned char      *pool_free;
        pthread_mutex_t     pool_m;

        int                 max_msgsize;
        int                 max_index;

        int                 buffs_per_new[ITC_POOL_BUFF_VALS];

        int                 allocated[ITC_POOL_BUFF_VALS];
        int                 free[ITC_POOL_BUFF_VALS];

        struct itcq        *pool_free_queue[ITC_POOL_BUFF_VALS];
};

/* ========================================================================
 *   DATA DECLARATIONS
 * ========================================================================
 */
static struct pool_data pool_data;

/* ========================================================================
 *   FUNCTION PROTOTYPES
 * ========================================================================
 */
static int pool_init_alloc(union itc_scheme *alloc_params,
                           int max_msgsize);
static int pool_exit_alloc(void);
static struct itc_message *pool_alloc(size_t size);
static void pool_free(struct itc_message *message);
static struct itc_alloc_info *pool_info(void);

struct itci_alloc_funcs pool_funcs = {
        pool_init_alloc,
        pool_exit_alloc,
        pool_alloc,
        pool_free,
        pool_info
};

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
 *   atomic_add
 *
 *   @param v          Pointer that shall be updated.
 *
 *   @param val        Value that shall be added.
 *
 *   @return           -
 *
 *   @par Globals:     --
 *
 *   Atomic add function. Relies on gcc specific functionality.
 */
/* ===================================================================== */
static inline void atomic_add( int *v, int val )
{
        __sync_add_and_fetch(v, val);
}

/* ===================================================================== */
/**
 *   atomic_sub
 *
 *   @param v          Pointer that shall be updated.
 *
 *   @param val        Value that shall be subtracted.
 *
 *   @return           -
 *
 *   @par Globals:     --
 *
 *   Atomic subtract function. Relies on gcc specific functionality.
 */
/* ===================================================================== */
static inline void atomic_sub( int *v, int val )
{
        __sync_sub_and_fetch(v, val);
}

/* ===================================================================== */
/**
 *   get_pool_index
 *
 *   @param size       Message size.
 *
 *   @return           Index into pools array for this size or
 *                     -1 at error.
 *
 *   @par Globals:     --
 *
 *   Get array index for a message size.
 */
/* ===================================================================== */
static int get_pool_index(size_t size)
{
        int order, index = 0;
        int totsize;

        totsize = size;

        order = (32 - FFS(totsize-1)) - 5;

        index = order / 2;

        if (index > pool_data.max_index) {
                index = -1;
        }

        return index;
}

/* ===================================================================== */
/**
 *   get_buffer_size
 *
 *   @param index      Pool array index.
 *
 *   @return           Size of buffers for a pool index or -1 at error.
 *
 *   @par Globals:     --
 *
 *   Get message size for an array index.
 */
/* ===================================================================== */
static int get_buffer_size(int index)
{
        int totsize;

        if(index > pool_data.max_index) {
                return -1;
        }

        totsize = 0x40 << (index * 2);

        if(totsize > pool_data.max_msgsize) {
                totsize = pool_data.max_msgsize;
        }

        return (totsize - 1);
}

/* ===================================================================== */
/**
 *   get_allbuffer_size
 *
 *   @param index      Pool array index.
 *
 *   @return           Size of buffers for a pool index or -1 at error.
 *
 *   @par Globals:     --
 *
 *   Get total message buffer size including administrative area and
 *   endmark for an array index.
 */
/* ===================================================================== */
static int get_allbuffer_size(int index)
{
        if(index > pool_data.max_index) {
                return -1;
        }

        return 0x40 << (index * 2);
}

/* ===================================================================== */
/**
 *   get_new_buffers
 *
 *   @param size       Size of messages.
 *
 *   @param num        How many messages that were gotten.
 *
 *   @return           Pointer to message retrieved from queue or NULL if
 *                     queue empty.
 *
 *   @par Globals:     pool_data
 *                     Pool data global structure.
 *
 *   Get new messages from total free pool area.
 */
/* ===================================================================== */
static unsigned char *get_new_buffers(size_t size, int *num)
{
        int howmany, index, tmpsize, chunksize;
        unsigned char *freebuf;

        index     = get_pool_index(size);
        howmany   = pool_data.buffs_per_new[index];
        tmpsize   = get_allbuffer_size(index);
        chunksize = tmpsize * howmany;

        MUTEX_LOCK(&pool_data.pool_m);

        if((pool_data.pool_free + chunksize) > pool_data.pool_end) {
                return NULL;
        }
        freebuf = pool_data.pool_free;
        pool_data.pool_free += chunksize;
        *num = howmany;

        MUTEX_UNLOCK(&pool_data.pool_m);

        return freebuf;
}

/* ===================================================================== */
/**
 *   add_new_buffers
 *
 *   @param size       Size of messages.
 *
 *   @return           0 at success.
 *
 *   @par Globals:     pool_data
 *                     Pool data global structure.
 *
 *   Add new messages to pool for a certain size.
 */
/* ===================================================================== */
static int add_new_buffers(size_t size)
{
        int howmany, i, pooli;
        unsigned char *bufptr;
        struct itcq *poolq;

        bufptr = get_new_buffers(size, &howmany);

        if(bufptr == NULL) {
                return -1;
        }

        pooli = get_pool_index(size);
        poolq = pool_data.pool_free_queue[pooli];
        for(i=0; i<howmany ; i++) {
                ((struct itc_message *)bufptr)->buffsize = pooli;

                atomic_add(&(pool_data.free[pooli]), 1);
                q_enqueue(poolq, bufptr);

                bufptr += get_allbuffer_size(pooli);
        }

        return 0;
}

/* ===================================================================== */
/**
 *   pool_init_alloc
 *
 *   @param alloc_params  Parameter structure for this allocation scheme.
 *
 *   @param max_msgsize   Max message size supported by transports.
 *
 *   @return           0 at success.
 *
 *   @par Globals:     pool_data
 *                     Pool data global structure.
 *
 *   Pool allocation scheme init function, called by itc_init.
 */
/* ===================================================================== */
static int pool_init_alloc(union itc_scheme *alloc_params,
                           int max_msgsize)
{
        int i, j, pool_size, numbuff;
        unsigned char *tmp;

        pool_data.max_msgsize = max_msgsize;
        pool_data.max_index   = ITC_POOL_BUFF_VALS - 1;
        pool_data.max_index   = get_pool_index(max_msgsize);
        if(pool_data.max_index >= ITC_POOL_BUFF_VALS) {
                pool_data.max_msgsize = get_buffer_size(ITC_POOL_BUFF_VALS - 1);
                pool_data.max_index   = ITC_POOL_BUFF_VALS - 1;
        }

        /* Setup hardcoded values so that we break at leas 4KB from unused
           pool every time we use it */
        pool_data.buffs_per_new[0] = 64;
        pool_data.buffs_per_new[1] = 16;
        pool_data.buffs_per_new[2] = 4;
        pool_data.buffs_per_new[3] = 1;
        pool_data.buffs_per_new[4] = 1;
        pool_data.buffs_per_new[5] = 1;

        if(alloc_params->pool_parameters.size >= 0x80000000) {
                ITC_TRACE_ERROR("To big pool requested, requested size: %d (0x%08x)",
                                alloc_params->pool_parameters.size,
                                alloc_params->pool_parameters.size);
                return ITC_EILLEGAL_ALLOC_CFG;
        }

        /* Move pool size to next larger magnitude */
        pool_size = 0x80000000 >> (FFS(alloc_params->pool_parameters.size - 1) - 1);

        pool_data.pool_start = malloc(pool_size);
        if(pool_data.pool_start == NULL) {
                ITC_TRACE_ERROR("Pool scheme failed to allocate pool area", 0);
                return ITC_EOUT_OF_MEMORY;
        }
        memset(pool_data.pool_start, 0, pool_size);

        pool_data.pool_free = pool_data.pool_start;
        pool_data.pool_end  = pool_data.pool_start + pool_size;
        pool_data.pool_size = pool_size;

        if(pthread_mutex_init(&pool_data.pool_m, NULL) != 0) {
                return ITC_EINTERNAL_ERROR;
        }

        for(i=0 ; i<=pool_data.max_index ; i++) {
                tmp = get_new_buffers(get_buffer_size(i), &numbuff);
                if(tmp == NULL) {
                        return ITC_EOUT_OF_MEMORY;
                }

                ((struct itc_message *)tmp)->buffsize = i;
                pool_data.pool_free_queue[i] = q_new(tmp);

                if(pool_data.pool_free_queue[i] == NULL) {
                        ITC_TRACE_ERROR("Failed to allocate memory for the free queues", 0);
                        return ITC_EOUT_OF_MEMORY;
                }

                for(j=1; j<numbuff ; j++) {
                        tmp += get_allbuffer_size(i);
                        ((struct itc_message *)tmp)->buffsize = i;

                        atomic_add(&(pool_data.free[i]), 1);
                        q_enqueue(pool_data.pool_free_queue[i], tmp);
                }
        }

        return 0;
}

/* ===================================================================== */
/**
 *   pool_exit_alloc
 *
 *   @return           0 at success.
 *
 *   @par Globals:     --
 *
 *   Pool allocation scheme exit function, called by itc_exit.
 */
/* ===================================================================== */
static int pool_exit_alloc(void)
{
        int i;

        for(i=0 ; i<=pool_data.max_index ; i++) {
                free(pool_data.pool_free_queue[i]);
        }

        free(pool_data.pool_start);
        memset(&pool_data, 0, sizeof(struct pool_data));

        return 0;
}

/* ===================================================================== */
/**
 *   pool_alloc
 *
 *   @param size       Size of allocated message.
 *
 *   @return           Pointer to struct itc_message.
 *
 *   @par Globals:     --
 *
 *   Pool allocation scheme alloc function, called by itc_alloc.
 */
/* ===================================================================== */
static struct itc_message *pool_alloc(size_t size)
{
        struct itc_message *message;
        struct itcq *poolq;
        int pooli;

        pooli = get_pool_index(size);
        if(pooli == -1 ||
           size > pool_data.max_msgsize) {
                ITC_TRACE_ERROR("To large message requested, requested %d, max %d",
                                size, pool_data.max_msgsize);
                return NULL;
        }

        poolq = pool_data.pool_free_queue[pooli];

        atomic_add(&(pool_data.allocated[pooli]), 1);
        atomic_sub(&(pool_data.free[pooli]), 1);
        while((message = q_dequeue(poolq)) == NULL) {
                if(add_new_buffers(size) != 0) {
                        return NULL;
                }
        }

        return message;
}

/* ===================================================================== */
/**
 *   pool_free
 *
 *   @param message    Pointer to message to be freed.
 *
 *   @return           -
 *
 *   @par Globals:     --
 *
 *   Pool allocation scheme free function, called by itc_free.
 */
/* ===================================================================== */
static void pool_free(struct itc_message *message)
{
        struct itcq *poolq;
        int pooli;

        pooli = message->buffsize;
        poolq = pool_data.pool_free_queue[pooli];

        atomic_sub(&(pool_data.allocated[pooli]), 1);
        atomic_add(&(pool_data.free[pooli]), 1);
        q_enqueue(poolq, message);
}

/* ===================================================================== */
/**
 *   pool_info
 *
 *   @return           Pointer to an itc_pool_info structure.
 *
 *   @par Globals:     --
 *
 *   Pool allocation scheme info function, called by itc_get_alloc_info.
 *   Returns a buffer allocated with itc_alloc that the caller needs to
 *   free with itc_free.
 */
/* ===================================================================== */
static struct itc_alloc_info *pool_info(void)
{
        struct itc_alloc_info *tmp;
        int i;

        tmp = (struct itc_alloc_info *)
                itc_alloc(sizeof(struct itc_alloc_info), 0);
        memset(tmp, 0, sizeof(struct itc_alloc_info));

        tmp->scheme = ITC_POOL;

        tmp->info.pool_info.totsize = pool_data.pool_size;
        tmp->info.pool_info.totfree = (pool_data.pool_end -
                                       pool_data.pool_free);
        for(i=0 ; i<=pool_data.max_index ; i++) {
                tmp->info.pool_info.size[i] = (get_buffer_size(i) + 1);
                tmp->info.pool_info.allocated[i] = pool_data.allocated[i];
                tmp->info.pool_info.free[i] = pool_data.free[i];
        }

        return tmp;
}
