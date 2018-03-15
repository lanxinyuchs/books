/**
 *   [Enter a brief one-liner file comment and leave next line empty.]
 *
 *   @file itci_alloc.h
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
 * ========================================================================
 */

#ifndef __ITCI_ALLOC_H
#define __ITCI_ALLOC_H

#ifdef __cplusplus
extern "C" {
#endif

/* ========================================================================
 *   INCLUDE FILES
 * ========================================================================
 */
#include "itc.h"

/* ========================================================================
 *   DEFINITIONS
 * ========================================================================
 */
#define ENDP_ID (char)0xAA

/* ========================================================================
 *   TYPE DEFINITIONS
 * ========================================================================
 */
typedef int                    (itci_init_alloc)(union itc_scheme *alloc_params,
                                                 int max_msgsize);
typedef int                    (itci_exit_alloc)(void);
typedef struct itc_message    *(itci_alloc)(size_t size);
typedef void                   (itci_free)(struct itc_message *message);
typedef struct itc_alloc_info *(itci_info)(void);

struct itci_alloc_funcs {
        itci_init_alloc *itci_init_alloc;
        itci_exit_alloc *itci_exit_alloc;
        itci_alloc      *itci_alloc;
        itci_free       *itci_free;
        itci_info       *itci_info;
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

#endif   /* ifndef __xxx_H */
