/**
 *   Routines for handling search trees for the bios functions.
 *
 *   @file
 *
 *   This file is a part of the Legacy IPC and Task Support (lits)
 *   library.
 *
 *   Copyright (C) 2011 by Ericsson AB. All rights reserved. The
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
 *   Revisded: 2012-02-06
 *   Change  : First version.
 * ========================================================================
 */

/* ========================================================================
 *   INCLUDE FILES
 * ========================================================================
 */
#define _GNU_SOURCE
#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <search.h>
#include <pthread.h>
#include <ose_spi/bios.h>

/* ========================================================================
 *   DEFINITIONS
 * ========================================================================
 */
/*
**  Macros for protecting critical regions when modifying the
**  TCB tables
*/
#define protect_bios()          pthread_mutex_lock(&bios_mutex)
#define unprotect_bios()        pthread_mutex_unlock(&bios_mutex)

/* ========================================================================
 *   TYPE DEFINITIONS
 * ========================================================================
 */

/* ========================================================================
 *   DATA DECLARATIONS
 * ========================================================================
 */
/*
**  Mutex for protecting critical regions when modifying the
**  BIOS tables
*/
static pthread_mutex_t bios_mutex = PTHREAD_MUTEX_INITIALIZER;

static void *bios_name_tab = NULL;

/* ========================================================================
 *   FUNCTION PROTOTYPES
 * ========================================================================
 */

/** ==================================================================== */
/**
 *   Compare algorithm for binary tree search.
 *
 *   @param pa         Element A
 *   @param pb         Element B
 *
 *   @return           -1 if A < B, 0 if A = B, 1 A > B
 *
 *   @par Globals:
 *                     --
 *
 *   This algorthm compares the name of bios functions.
 */
/* ===================================================================== */
static int
compare_bios_by_name(const void *pa, const void *pb)
{
  const struct BiosList  *bios1 = pa;
  const struct BiosList  *bios2 = pb;

  return strcmp(bios1->name, bios2->name);
}

/** ==================================================================== */
/**
 *   Compare algorithm for binary tree search.
 *
 *   @param pa         Element A
 *   @param pb         Element B
 *
 *   @return           -1 if A < B, 1 A >= B
 *
 *   @par Globals:
 *                     --
 *
 *   This algorthm compares the name of bios functions and is used
 *   when inserting elements into the name based tree.
 */
/* ===================================================================== */
static int
insert_bios_by_name(const void *pa, const void *pb)
{
  const struct BiosList  *bios1 = pa;
  const struct BiosList  *bios2 = pb;
  int          res;

  res = strcmp(bios1->name, bios2->name);

  return (res != 0 ? res : 1);
}


/** ==================================================================== */
/**
 *   Dummy for removing the root of a binary tree.
 *
 *   @param nodep      Pointer to the element
 *
 *   @return           -
 *
 *   @par Globals:
 *                     --
 *
 *   This is used when removing the process/thread named base binary
 *   tree, where the element is just a pointe to the elements of the
 *   pid based tree.
 */
/* ===================================================================== */
static void
free_name_node(void *nodep)
{
  (void)nodep;
}

/** ==================================================================== */
/**
 *   Destroys the name based binary trees.
 *
 *   @param            -
 *
 *   @return           0 if OK, otherwise non-zero.
 *
 *   @par Globals:
 *                     bios_name_tab
 */
/* ===================================================================== */
int
destroy_bios_tab(void)
{
  tdestroy(bios_name_tab, free_name_node);

  return 0;
}

/** ==================================================================== */
/**
 *   Adds a new bios function to the named based binary trees.
 *
 *   @param bios       Pointer to the new element
 *
 *   @return           0 if OK, otherwise non-zero.
 *
 *   @par Globals:
 *                     bios_name_tab
 */
/* ===================================================================== */
int
add_bios(struct BiosList *bios)
{
  struct BiosList  **res;

  protect_bios();
  res = tsearch(bios, &bios_name_tab, insert_bios_by_name);
  unprotect_bios();

  if ( (res == 0) || (*res != bios) )
     return -2;

  return 0;
}

/** ==================================================================== */
/**
 *   Searches for a bios function with the specified name.
 *
 *   @param name       Process name
 *
 *   @return           Pointer to the bios entry or 0 is not found
 *
 *   @par Globals:
 *                     bios_tab
 */
/* ===================================================================== */
struct BiosList *
get_bios_by_name(const char *name)
{
  struct BiosList bios;
  struct BiosList  **res;

  strncpy(bios.name, name, BIOS_NAME_SIZE - 1);
  bios.name[BIOS_NAME_SIZE - 1] = '\0';

  protect_bios();
  res = tfind(&bios, &bios_name_tab, compare_bios_by_name);
  unprotect_bios();

  return res ? *res : NULL;
}

/** ==================================================================== */
/**
 *   Delete a bios function from the named based binary trees.
 *
 *   @param pid        Process ID
 *
 *   @return           0 if OK, otherwise non-zero.
 *
 *   @par Globals:
 *                     bios_name_tab
 */
/* ===================================================================== */
int
delete_bios(char* name)
{
  struct BiosList  *bios;
  struct BiosList  **res;

  if ( (bios = get_bios_by_name(name)) == NULL ) {
    /*lits_error_handler(0, 0x32, current_process());*/
    return -1;
  }

  protect_bios();
  res = tdelete(bios, &bios_name_tab, compare_bios_by_name);
  unprotect_bios();

  if ( res == 0 )
     return -3;

  free(bios);
  return 0;
}

long
biosInstall(const char *name, BiosFunction *entrypoint, unsigned long flags)
{
  struct BiosList *bios;
  bios = get_bios_by_name(name);
  if (!bios)
  {
    bios = (struct BiosList *)malloc(sizeof(struct BiosList));
    strncpy(bios->name, name, BIOS_NAME_SIZE - 1);
    bios->flags = flags;
    bios->entrypoint = entrypoint;
    bios->runlevel = 0;
    bios->handle = (long)entrypoint;
    if ( add_bios(bios) != 0 )
    {
      /*lits_error("Failed to add bios entry");*/
      free(bios);
      return BIOS_ENO_TABLE_SPACE;
    }
    return BIOS_SUCCESS;
  }
  return BIOS_EDUPLICATE_NAME;
}

unsigned long
biosOpen(const char *name)
{
  struct BiosList *bios;
  bios = get_bios_by_name(name);
  if (bios)
  {
    return bios->handle;
  }
  return 0;
}

long
biosCall(unsigned long handle, ... /* arg1..arg7 */)
{
  va_list args;
  long l1;
  long l2;
  long l3;
  long l4;
  long l5;
  long l6;
  long l7;

  va_start(args, handle);
  l1 = va_arg(args, long);
  l2 = va_arg(args, long);
  l3 = va_arg(args, long);
  l4 = va_arg(args, long);
  l5 = va_arg(args, long);
  l6 = va_arg(args, long);
  l7 = va_arg(args, long);
  va_end(args);

  return ((BiosFunction*)(void*)handle)(l1, l2, l3, l4, l5, l6, l7);
}
