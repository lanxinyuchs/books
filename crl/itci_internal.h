/**
 *   [Enter a brief one-liner file comment and leave next line empty.]
 *
 *   @file itci_internal.h
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
 *   Revised : 2013-02-06 Magnus Lindberg
 *   Change  : Added flags to the itci_init call and removed the
 *             itci_create_ep function.
 *
 *   Revised : 2014-02-18 Magnus Lindberg magnus.k.lindberg@ericsson.com
 *   Change  : Added support for determining max message size.
 *             Removed unused world_transport variable.
 * ========================================================================
 */

#ifndef __ITCI_INTERNAL_H
#define __ITCI_INTERNAL_H

#ifdef __cplusplus
extern "C" {
#endif

/* ========================================================================
 *   INCLUDE FILES
 * ========================================================================
 */
#include <stdbool.h>

#include "itc.h"
#include "itc_impl.h"

/* ========================================================================
 *   DEFINITIONS
 * ========================================================================
 */

/* ========================================================================
 *   TYPE DEFINITIONS
 * ========================================================================
 */
typedef bool (itci_locate_world)(itc_mbox_id_t  *my_world_id,
                                 itc_mbox_id_t  *world_mask,
                                 itc_mbox_id_t  *world_mbox_id);

typedef int (itci_init)(itc_mbox_id_t  my_world_id,
                        itc_mbox_id_t  world_mask,
                        int            mbox_count,
                        uint32_t       flags);

typedef int (itci_exit)(void);

typedef int (itci_create_mailbox)(struct itc_mailbox *mbox,
                                  uint32_t flags);

typedef int (itci_delete_mailbox)(struct itc_mailbox *mbox);

typedef int (itci_send)(struct itc_mailbox *mbox,
                        struct itc_message *message,
                        itc_mbox_id_t       to,
                        itc_mbox_id_t       from);

typedef struct itc_message *(itci_receive)(struct itc_mailbox *mbox,
                                           const uint32_t     *filter,
                                           long                tmo,
                                           itc_mbox_id_t       from,
                                           bool                recursive_call);

typedef struct itc_message *(itci_remove)(struct itc_mailbox *mbox,
                                          struct itc_message *message);

typedef int (itci_get_maxmsgsize)(void);

struct itci_transport_funcs {
        itci_locate_world    *itci_locate_world;
        itci_init            *itci_init;
        itci_exit            *itci_exit;
        itci_create_mailbox  *itci_create_mailbox;
        itci_delete_mailbox  *itci_delete_mailbox;
        itci_send            *itci_send;
        itci_receive         *itci_receive;
        itci_remove          *itci_remove;
        itci_get_maxmsgsize  *itci_get_maxmsgsize;
};

/* ========================================================================
 *   DATA DECLARATIONS
 * ========================================================================
 */

/* Copy header and declararion of global functions, if applicable, from
 * the C file and remove this comment.
 */


#ifdef __cplusplus
}
#endif

#endif   /* ifndef __ITCI_INTERNAL_H */
