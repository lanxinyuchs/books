
/**
 *   ITC flexible pool allocation scheme implementation.
 *
 *   @file itc_poolflex.c
 *
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
 *             Corrected error in the pool_data struct declaration.
 *
 *   Revised : 2014-02-18 Magnus Lindberg magnus.k.lindberg@ericsson.com
 *   Change  : Added support for determining max message size.
 *             Added error codes for itc_init.
 *
 *   Revised : 2014-10-28 Magnus Lindberg magnus.k.lindberg@ericsson.com
 *   Change  : Corrected itc_alloc to support more than 65500 byte
 *             messages.
 *
 *   Revised : 2014-10-28 Magnus Lindberg magnus.k.lindberg@ericsson.com
 *   Change  : Corrected handling of allocation of too large messages.
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

        int                 pool_sizes[ITC_POOLFLEX_BUFF_VALS];
        int                 allocated[ITC_POOLFLEX_BUFF_VALS];
        int                 free[ITC_POOLFLEX_BUFF_VALS];

        struct itcq        *pool_free_queue[ITC_POOLFLEX_BUFF_VALS];
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
static int poolflex_init_alloc(union itc_scheme *alloc_params,
                               int max_msgsize);
static int poolflex_exit_alloc(void);
static struct itc_message *poolflex_alloc(size_t size);
static void poolflex_free(struct itc_message *message);
static struct itc_alloc_info *poolflex_info(void);

struct itci_alloc_funcs poolflex_funcs = {
        poolflex_init_alloc,
        poolflex_exit_alloc,
        poolflex_alloc,
        poolflex_free,
        poolflex_info
};

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
 *   @return           Index into pools array for this size.
 *
 *   @par Globals:     --
 *
 *   Get array index for a message size.
 */
/* ===================================================================== */
static int get_pool_index(size_t size)
{
        int i;

        for(i=0 ; i<ITC_POOLFLEX_BUFF_VALS ; i++) {
                if(size <= pool_data.pool_sizes[i]) {
                        break;
                }
        }

        if(i == ITC_POOLFLEX_BUFF_VALS) {
                return -1;
        }

        return i;
}

/* ===================================================================== */
/**
 *   get_buffer_size
 *
 *   @param index      Pool array index.
 *
 *   @return           Size of buffers for a pool index.
 *
 *   @par Globals:     --
 *
 *   Get message size for an array index.
 */
/* ===================================================================== */
static int get_buffer_size(int index)
{
        if(index >= ITC_POOLFLEX_BUFF_VALS) {
                return -1;
        }

        return (pool_data.pool_sizes[index] - 1);
}

/* ===================================================================== */
/**
 *   get_allbuffer_size
 *
 *   @param index      Pool array index.
 *
 *   @return           Size of buffers for a pool index.
 *
 *   @par Globals:     --
 *
 *   Get total message buffer size including administrative area and
 *   endmark for an array index.
 */
/* ===================================================================== */
static int get_allbuffer_size(int index)
{
        if(index >= ITC_POOLFLEX_BUFF_VALS) {
                return -1;
        }

        return pool_data.pool_sizes[index];
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

        index = get_pool_index(size);
	if(index == -1)
		return NULL;

        tmpsize   = get_allbuffer_size(index);
        howmany   = (4*1024) / tmpsize;
        if(howmany == 0) {
                howmany = 1;
        }
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
 *   poolflex_init_alloc
 *
 *   @param alloc_params  Parameter structure for this allocation scheme.
 *
 *   @param max_msgsize   Max message size supported by transports.
 *
 *   @return              0 at success.
 *
 *   @par Globals:        pool_data
 *                        Pool data global structure.
 *
 *   Pool allocation scheme init function, called by itc_init.
 */
/* ===================================================================== */
static int poolflex_init_alloc(union itc_scheme *alloc_params,
                               int max_msgsize)
{
        int i, j, pool_size, numbuff, lastsize = 0;
        unsigned char *tmp;
        struct itc_pool_flex_parameters *params;

        params = (struct itc_pool_flex_parameters *)alloc_params;

        if(max_msgsize < (params->msg_sizes[ITC_POOLFLEX_BUFF_VALS - 1] + ITC_MSG_ADM_OFFSET + 1)) {
                ITC_TRACE_ERROR("%s, supported: %d pool max: %d",
                                "itc_poolflex has larger buffers than transport supports",
                                max_msgsize,
                                params->msg_sizes[ITC_POOLFLEX_BUFF_VALS - 1]);
                return ITC_EILLEGAL_ALLOC_CFG;
        }

        for(i=0 ; i<ITC_POOLFLEX_BUFF_VALS ; i++) {
                if(params->msg_sizes[i] <= lastsize) {
                        ITC_TRACE_ERROR("Message size not in increasing order at size %d",
                                        params->msg_sizes[i]);
                        return ITC_EILLEGAL_ALLOC_CFG;
                }
                pool_data.pool_sizes[i] = params->msg_sizes[i];
                lastsize = params->msg_sizes[i];
        }

        if(params->size >= 0x80000000) {
                ITC_TRACE_ERROR("To big pool requested, requested size: %d (0x%08x)",
                                params->size,
                                params->size);
                return ITC_EILLEGAL_ALLOC_CFG;
        }

        pool_size = params->size;

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

        for(i=0 ; i<ITC_POOLFLEX_BUFF_VALS ; i++) {
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
 *   poolflex_exit_alloc
 *
 *   @return           0 at success.
 *
 *   @par Globals:     --
 *
 *   Pool allocation scheme exit function, called by itc_exit.
 */
/* ===================================================================== */
static int poolflex_exit_alloc(void)
{
        int i;

        for(i=0 ; i<ITC_POOLFLEX_BUFF_VALS ; i++) {
                free(pool_data.pool_free_queue[i]);
        }

        free(pool_data.pool_start);
        memset(&pool_data, 0, sizeof(struct pool_data));

        return 0;
}

/* ===================================================================== */
/**
 *   poolflex_alloc
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
static struct itc_message *poolflex_alloc(size_t size)
{
        struct itc_message *message;
        struct itcq *poolq;
        int pooli;

        pooli = get_pool_index(size);
	if(pooli == -1)
		return NULL;

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
 *   poolflex_free
 *
 *   @param msg        Pointer to message to be freed.
 *
 *   @return           -
 *
 *   @par Globals:     --
 *
 *   Pool allocation scheme free function, called by itc_free.
 */
/* ===================================================================== */
static void poolflex_free(struct itc_message *message)
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
 *   poolflex_info
 *
 *   @return           Pointer to an itc_pool_flex_info structure.
 *
 *   @par Globals:     --
 *
 *   Pool_flex allocation scheme info function, called by itc_get_alloc_info.
 *   Returns a buffer allocated with itc_alloc that the caller needs to
 *   free with itc_free.
 */
/* ===================================================================== */
static struct itc_alloc_info *poolflex_info(void)
{
        struct itc_alloc_info *tmp;
        int i;

        tmp = (struct itc_alloc_info *)
                itc_alloc(sizeof(struct itc_alloc_info), 0);

        tmp->scheme = ITC_POOL;

        tmp->info.pool_flex_info.totsize = pool_data.pool_size;
        tmp->info.pool_flex_info.totfree = (pool_data.pool_end -
                                       pool_data.pool_free);
        for(i=0 ; i<ITC_POOLFLEX_BUFF_VALS ; i++) {
                tmp->info.pool_flex_info.size[i]      = pool_data.pool_sizes[i];
                tmp->info.pool_flex_info.allocated[i] = pool_data.allocated[i];
                tmp->info.pool_flex_info.free[i]      = pool_data.free[i];
        }

        return tmp;
}
