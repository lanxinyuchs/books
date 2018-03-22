/**
 *   [Enter a brief one-liner file comment and leave next line empty.]
 *
 *   @file itc_malloc.c
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
 *   Revised : 2013-02-06 Magnus Lindberg
 *   Change  : First version.
 *
 *   Revised : 2014-02-18 Magnus Lindberg magnus.k.lindberg@ericsson.com
 *   Change  : Added support for determining max message size.
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

#include "itc.h"
#include "itc_impl.h"
#include "itci_alloc.h"

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
static int max_mallocsize;

/* ========================================================================
 *   FUNCTION PROTOTYPES
 * ========================================================================
 */
static int malloc_init_alloc(union itc_scheme *alloc_params,
                             int max_msgsize);
static int malloc_exit_alloc(void);
static struct itc_message *malloc_alloc(size_t size);
static void malloc_free(struct itc_message *message);
static struct itc_alloc_info *malloc_info(void);

struct itci_alloc_funcs malloc_funcs = {
        malloc_init_alloc,
        malloc_exit_alloc,
        malloc_alloc,
        malloc_free,
        malloc_info
};

/* ===================================================================== */
/**
 *   malloc_init_alloc
 *
 *   @param alloc_params  Parameter structure for this allocation scheme.
 *                        Unused for malloc.
 *
 *   @param max_msgsize   Max message size supported by transports.
 *
 *   @return           0 at success.
 *
 *   @par Globals:     --
 *
 *   Malloc allocation scheme init function, called by itc_init.
 */
/* ===================================================================== */
static int malloc_init_alloc(union itc_scheme *alloc_params,
                             int max_msgsize)
{
        /* Store max message size to check at alloc.
           Need to make room for message header and endmark. */
        max_mallocsize = max_msgsize;

        return 0;
}

/* ===================================================================== */
/**
 *   malloc_exit_alloc
 *
 *   @return           0 at success.
 *
 *   @par Globals:     --
 *
 *   Malloc allocation scheme exit function, called by itc_exit.
 */
/* ===================================================================== */
static int malloc_exit_alloc(void)
{
        return 0;
}

/* ===================================================================== */
/**
 *   malloc_alloc
 *
 *   @param size       Size of allocated message.
 *
 *   @return           Pointer to struct itc_message or NULL if error
 *                     or no memory available.
 *
 *   @par Globals:     --
 *
 *   Malloc allocation scheme alloc function, called by itc_alloc.
 */
/* ===================================================================== */
static struct itc_message *malloc_alloc(size_t size)
{
        struct itc_message *message;

        if(size > max_mallocsize) {
                ITC_TRACE_ERROR("To large message requested, requested %d, max %d",
                                size, max_mallocsize);
                return NULL;
        }

        message = malloc(size);
        if(message == NULL) {
                return NULL;
        }
        message->buffsize = 0;

        return message;
}

/* ===================================================================== */
/**
 *   malloc_free
 *
 *   @param message    Pointer to message to be freed.
 *
 *   @return           -
 *
 *   @par Globals:     --
 *
 *   Malloc allocation scheme free function, called by itc_free.
 */
/* ===================================================================== */
static void malloc_free(struct itc_message *message)
{
        free(message);
}

/* ===================================================================== */
/**
 *   malloc_info
 *
 *   @return           Pointer to an itc_malloc_info structure.
 *
 *   @par Globals:     --
 *
 *   Malloc allocation scheme info function, called by itc_get_alloc_info.
 *   Returns a buffer allocated with itc_alloc that the caller needs to
 *   free with itc_free.
 */
/* ===================================================================== */
static struct itc_alloc_info *malloc_info(void)
{
        struct itc_alloc_info *tmp;

        tmp = (struct itc_alloc_info *)
                itc_alloc(sizeof(struct itc_alloc_info), 0);

        tmp->scheme = ITC_MALLOC;
        tmp->info.malloc_info.tot_malloc = 0;

        return tmp;
}

