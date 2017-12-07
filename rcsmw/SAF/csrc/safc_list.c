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
 *  Purpose : Linked list datatype for handling arbitrary data.
 * ----------------------------------------------------------------------
 *
 */

#include <stdlib.h>
#include "safc_list.h"

SafcSListT
safc_slist_prepend(SafcSListT list, void *item)
{
  SafcSListT elem;

  if (item == NULL) return list;

  elem = malloc(sizeof(*elem));
  if (elem == NULL) return NULL;

  elem->data = item;
  elem->next = list;

  return elem;
}

SafcSListT
safc_slist_remove(SafcSListT list, void *item)
{
  SafcSListT tmp = list;
  SafcSListT prev = NULL;

  if (list == NULL) return list;

  while (tmp != NULL) {
    if (tmp->data == item) {
      if (prev == NULL) {
	list = tmp->next;
      } else {
	prev->next = tmp->next;
      }
      free(tmp);
      break;
    }
    prev = tmp;
    tmp = tmp->next;
  }

  return list;
}

SafcSListT
safc_slist_remove_first(SafcSListT list, void **item)
{
  SafcSListT new = NULL;

  if (list == NULL) {
    *item = NULL;
    return new;
  }

  *item = list->data;
  new = list->next;
  free(list);

  return new;
}
