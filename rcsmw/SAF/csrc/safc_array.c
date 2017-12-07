/*
 * %EricssonCopyright%
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2010-2017. All Rights Reserved.
 * 
 * The program may be used and/or copied only with the written permission from
 * Ericsson AB, or in accordance with the terms and conditions stipulated in
 * the agreement/contract under which the program has been supplied.
 * 
 * %CopyrightEnd%
 * 
 * ----------------------------------------------------------------------
 *  Purpose : Array datatype for handling arbitrary data.
 * ----------------------------------------------------------------------
 * 
 */

#include <stdlib.h>
#include <stdio.h>
#include "safc_array.h"
#include "safc_trace.h"

static int safc_array_find_slot(safc_array* p)
{
   int i;

   for(i = 0; p->array[i] != NULL; i++);
   
   return i;
}

static void safc_array_slot_init(safc_array* p, int start, int number)
{
   int i;

   for(i = start; i < start + number; p->array[i++] = NULL);   
   
}

void safc_array_init(safc_array* p,
		     void (*print)(void* elem),
		     void (*delete)(void* elem),
		     int (*find)(void* elem, void* sval)) 
{
   p->used = 0;
   p->max = 3;
   p->print = print;
   p->delete = delete;
   p->find = find;

   p->array = (void**) malloc(p->max*sizeof(void*));
   safc_array_slot_init(p, 0, 3);

   return;   
}

void safc_array_finalize(safc_array* p) 
{

   safc_array_delete_all(p);

   free(p->array);

   return;   
}

int safc_array_insert(safc_array* p, void* elem) 
{
   int ref;
   
   if (!p->array) {
      p->used = 0;
      p->max = 3;
      p->array = (void**) malloc(p->max*sizeof(void*));
      safc_array_slot_init(p, 0, 3);     
   } else
      if(p->used == p->max) {
	 int old_max = p->max;       
	 p->max = p->max*2;
	 p->array = (void **) realloc(p->array, p->max*sizeof(void*));
	 safc_array_slot_init(p, old_max, old_max);
      }
   
   ref = safc_array_find_slot(p);
   p->array[ref] = elem;
   p->used++; 
   return ref+1;
}

void safc_array_remove(safc_array* p, int ref) 
{
   if((ref > 0) && (ref <= p->max)) {
      p->array[ref-1] = NULL;
      p->used--;
   }
}

void safc_array_delete(safc_array* p, int ref) 
{
   if(p->delete && (ref > 0) && (ref <= p->max)) {
      p->delete(p->array[ref]);
      p->array[ref] = NULL;
      p->used--;
   }
}

void* safc_array_get(safc_array* p, int ref)
{
   
   if((ref > 0) && (ref <= p->max))
      return p->array[ref-1];
   else
      return NULL;
}


void safc_array_print(safc_array* p)
{
   int i;

   for(i = 0; i < p->max; i++) {
      printf("\nElement no: %d\n", i);
      if (!(p->array[i]))
	 printf("Empty Slot\n");
      else
	 p->print(p->array[i]);
   }
}

int safc_array_find(safc_array* p, void* sval) 
{
   int i;

   if(p->find) {
      for(i = 0;  i < p->max; i++) {

	 if(p->array[i] != NULL && p->find(p->array[i], sval))
	    break;
      }
      if(i >= p->max)
	 return 0;
      else
	 return i+1;
   }
   else 
      return 0;
}

void safc_array_delete_all(safc_array* p) 
{
   int i;

   if(p->delete)
      for(i = 0;  i < p->max; i++) {
	 p->delete(p->array[i]);
	 p->array[i] = NULL;
	 p->used--;
      }
}
