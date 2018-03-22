/**
 *   Doubly linked list macros used within ITC.
 *
 *   itc_list.h
 *
 *   Doubly linked list macros used within ITC. They rely on that the
 *   struct making up the list contains the fields next, prev and flags.
 *   The next and prev filds are pointers to the next and previous entry
 *   in the list. The flags field only has a single flga 0x8000 to denote
 *   the root entry. Apart from that the list can be used for any list as
 *   they are identified by the struct definition in each macro.
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
 *   Revised : 2013-03-28 Magnus Lindberg
 *   Change  : First version.
 * ========================================================================
 */

#ifndef __ITCLIST_H
#define __ITCLIST_H

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
#define LL_ROOT 0x8000

#define LL_NEW(str, list)                        \
        do {                                     \
                ((str *)list)->next   = list;    \
                ((str *)list)->prev   = list;    \
                ((str *)list)->flags |= LL_ROOT; \
        } while(0)

#define LL_AT_FRONT(str, list, in)                               \
        do {                                                     \
                ((str *)in)->next         = ((str *)list)->next; \
                ((str *)in)->prev         = list;                \
                ((str *)list)->next->prev = in;                  \
                ((str *)list)->next       = in;                  \
                ((str *)in)->flags       &= ~LL_ROOT;            \
        } while(0)

#define LL_AT_END(str, list, in)                                 \
        do {                                                     \
                ((str *)in)->next         = list;                \
                ((str *)in)->prev         = ((str *)list)->prev; \
                ((str *)list)->prev->next = in;                  \
                ((str *)list)->prev       = in;                  \
                ((str *)in)->flags       &= ~LL_ROOT;            \
        } while(0)

#define LL_OUT(str, out)                                         \
          do {                                                   \
                  ((str *)out)->next->prev = ((str *)out)->prev; \
                  ((str *)out)->prev->next = ((str *)out)->next; \
                  ((str *)out)->prev       = NULL;               \
                  ((str *)out)->next       = NULL;               \
           } while(0)

#define LL_NEXT(str, en) \
        en = ((((str *)en)->next->flags & LL_ROOT) ? NULL : ((str *)en)->next)

#define LL_FIRST(str, list)                                            \
        (((str *)list)->next->flags & LL_ROOT) ? NULL : ((str *)list)->next

#define LL_LAST(str, list)                                             \
        (((str *)list)->prev->flags & LL_ROOT) ? NULL : ((str *)list)->prev

#define LL_GETFIRST(str, list, out)                              \
        do {                                                     \
                out = LL_FIRST(str, list);                       \
                if(out != NULL)                                  \
                        LL_OUT(str, out);                        \
        } while(0)

#define LL_GETLAST(str, list, out)                               \
        do {                                                     \
                out = LL_LAST(str, list);                        \
                if(out != NULL)                                  \
                        LL_OUT(str, out);                        \
        } while(0)

/* ========================================================================
 *   TYPE DEFINITIONS
 * ========================================================================
 */

/* ========================================================================
 *   DATA DECLARATIONS
 * ========================================================================
 */

#ifdef __cplusplus
}
#endif

#endif   /* ifndef __ITCLIST_H */
