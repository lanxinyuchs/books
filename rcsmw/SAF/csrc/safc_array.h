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
#ifndef _SAFC_ARRAY_H
#define _SAFC_ARRAY_H

typedef struct safc_array
{
   int max;
   int used;
   void** array;

   void (*print)(void* elem);
   void (*delete)(void* elem);
   int (*find)(void* elem, void* svalue);
} safc_array;

#define SAFC_ARRAY_INITIALIZER {0, 0, NULL, NULL, NULL, NULL}

void safc_array_init(safc_array* p, 
		     void (*print)(void* elem),
		     void (*delete)(void* elem),
		     int (*find)(void* elem, void* sval));
void safc_array_finalize(safc_array* p);
int safc_array_insert(safc_array* p, void* elem);
void safc_array_remove(safc_array* p, int ref);
void safc_array_delete(safc_array* p, int ref);
void* safc_array_get(safc_array* p, int ref);
void safc_array_print(safc_array* p);
int safc_array_find(safc_array* p, void* sval);
void safc_array_delete_all(safc_array* p);

#endif
