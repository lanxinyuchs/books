/**
 *   ITC command main file.
 *
 *   @file itc_gen_cmd.h
 *
 *   ITC command implementation support functions header file.
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
 *   Revised : 2013-08-25 Magnus Lindberg, magnus.k.lindberg@ericsson.com
 *   Change  : First version.
 * ========================================================================
 */

/* ========================================================================
 *   INCLUDE FILES
 * ========================================================================
 */
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "itc.h"

/* ========================================================================
 *   DEFINITIONS
 * ========================================================================
 */
#define ML_NEW(list)                    \
        do {                            \
                list->next   = list;    \
                list->prev   = list;    \
                list->root   = 1;       \
        } while(0)

#define ML_AT_FRONT(list, in)                    \
        do {                                     \
                in->next         = list->next;   \
                in->prev         = list;         \
                list->next->prev = in;           \
                list->next       = in;           \
                in->root         = 0;            \
        } while(0)

#define ML_AT_END(list, in)                      \
        do {                                     \
                in->next         = list;         \
                in->prev         = list->prev;   \
                list->prev->next = in;           \
                list->prev       = in;           \
                in->root         = 0;            \
        } while(0)

#define ML_OUT(out)                             \
          do {                                  \
                  out->next->prev = out->prev;  \
                  out->prev->next = out->next;  \
                  out->prev       = NULL;       \
                  out->next       = NULL;       \
           } while(0)

#define ML_NEXT(en) \
        en = en->next->root ? NULL : en->next

#define ML_FIRST(list)                          \
        list->next->root ? NULL : list->next

#define ML_BEFORE(en, new)                      \
        do {                                    \
                new->next       = en;           \
                new->prev       = en->prev;     \
                en->prev->next  = new;          \
                en->prev        = new;          \
                new->root       = 0;            \
        } while(0)

/* ========================================================================
 *   TYPE DEFINITIONS
 * ========================================================================
 */
typedef enum {
        UNSORTED = 0,
        SORT_MBOXID,
        SORT_MBOXNAME,
} sortmethod;

/* ========================================================================
 *   FUNCTION PROTOTYPES
 * ========================================================================
 */

/* ===================================================================== */
/**
 *   Prints command helptext
 *
 *   @return           -
 *
 *   @par Globals:     --
 *
 *   Prints ITC command helptext.
 */
/* ===================================================================== */
void printhelp(void);

/* ===================================================================== */
/**
 *   Print a brief output of all ITC mailboxes.
 *
 *   @param sm         Method for sorting the mailboxes in the list.
 *
 *   @return           -
 *
 *   @par Globals:     mboxlist
 *                     Global mailbox list.
 *
 *   Prints a list with all ITC mailboxes sorted according to sort
 *   methology. The list only contains mailbox id and name.
 */
/* ===================================================================== */
void list_brief(sortmethod sm);

/* ===================================================================== */
/**
 *   Print the default output list for all ITC mailboxes.
 *
 *   @param sm         Method for sorting the mailboxes in the list.
 *
 *   @return           -
 *
 *   @par Globals:     mboxlist
 *                     Global mailbox list.
 *
 *   Prints a list with all ITC mailboxes sorted according to sort
 *   methology. The list contains the default information.
 */
/* ===================================================================== */
void list_normal(sortmethod sm);

/* ===================================================================== */
/**
 *   Print the verbose output list for all ITC mailboxes.
 *
 *   @param sm         Method for sorting the mailboxes in the list.
 *
 *   @return           -
 *
 *   @par Globals:     mboxlist
 *                     Global mailbox list.
 *
 *   Prints a list with all ITC mailboxes sorted according to sort
 *   methology. The list contains detailed information.
 */
/* ===================================================================== */
void list_verbose(sortmethod sm);

/* ===================================================================== */
/**
 *   Print detailed information about one mailbox.
 *
 *   @param mbox_id    Mailbox ID to print information about.
 *
 *   @return           -
 *
 *   @par Globals:     --
 *
 *   Prints detailed information about one mailbox.
 */
/* ===================================================================== */
void print_mbox(itc_mbox_id_t mbox_id);

/* ===================================================================== */
/**
 *   Print information about ITC allocations for an ITC process.
 *
 *   @param mbox_id    Mailbox ID for an mailbox in the desired process.
 *
 *   @return           -
 *
 *   @par Globals:     --
 *
 *   Print information about ITC allocations for an ITC process.
 */
/* ===================================================================== */
void print_allocinfo(itc_mbox_id_t mbox_id);
