/*
 * %EricssonCopyright%
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2012. All Rights Reserved.
 * 
 * The program may be used and/or copied only with the written permission from
 * Ericsson AB, or in accordance with the terms and conditions stipulated in
 * the agreement/contract under which the program has been supplied.
 * 
 * %CopyrightEnd%
 * 
 * ----------------------------------------------------------------------
 */

#ifndef _SAFC_LIST_H
#define _SAFC_LIST_H

// Singly linked list structure
struct safc_slist {
  struct safc_slist *next;
  void *data;
};

typedef struct safc_slist* SafcSListT;

SafcSListT safc_slist_prepend(SafcSListT list, void *item);
SafcSListT safc_slist_remove(SafcSListT list, void *item);
SafcSListT safc_slist_remove_first(SafcSListT list, void **item);

#endif
