/**
 *   The help command.
 * 
 *   @file
 * 
 *   This file is a part of the COLI shell.
 * 
 *   Copyright (C) 2011-2012 by Ericsson AB. All rights reserved. The
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
 *   Revised : 2014-10-14 Ravineet Singh EAB/FJP/HB
 *   Change  : Replaced dupstr with strdup
 *
 *   Revised : 2013-12-20 Ravineet Singh EAB/FJP/HB
 *   Change  : Changed IPC mechanism from linx to OSE(LITS).
 *
 *   Revised : 2012-01-10 Lars Jönsson EAB/FJP/TB
 *   Change  : Fixed problem with printing empty lines in help description.
 *
 *   Revised : 2011-11-30 Lars Jönsson EAB/FJP/TB
 *   Change  : All command names are now allocated dynamically. Cleaned up
 *             list_commands().
 *
 *   Revised : 2011-11-23 Lars Jönsson EAB/FJP/TB
 *   Change  : First version.
 * ========================================================================
 */

/* ========================================================================
 *   INCLUDE FILES
 * ========================================================================
 */
#include <coli.sig>
#include <errno.h>
#include <local_cmd.h>
#include <ose.h>
#include <remote_cmd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

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

/* ========================================================================
 *   FUNCTION PROTOTYPES
 * ========================================================================
 */ 

/** ==================================================================== */
/** 
 *   Prints one row in the list of commands.
 * 
 *   @param name       Command name
 *   @param help       Short help string
 * 
 *   @return           -
 *
 *   @par Globals:     
 *                     --
 */
/* ===================================================================== */
static void
print_short_help(char *name, char *help)
{
   printf("%-16s%s\n", name, help);
}

/** ==================================================================== */
/** 
 *   Prints a list and short help of all available commands.
 * 
 *   @param expr       Start of command name
 * 
 *   @return           0 if OK, otherwise non-zero
 *
 *   @par Globals:     
 *                     --
 */
/* ===================================================================== */
static int
list_commands(char *expr)
{
   char  *prev;
   char  *next_local;
   char  *next_remote;
   char  *next;
   char  *help;

   if ( (prev = malloc(1)) == NULL )
      return -1;

   *prev = '\0';

   print_short_help("Command", "Description");

   while ( 1 )
   {
      next_local  = next_local_command(expr, prev);
      next_remote = next_remote_command(expr, prev);

      if ( !next_local )
	 next = next_remote;
      else if ( !next_remote )
	 next = next_local;
      else if ( strcmp(next_local, next_remote) < 0 )
	 next = next_local;
      else
	 next = next_remote;

      if ( next )
      {
	 if ( next == next_local )
	    help = local_command_help(next);
	 else
	    help = remote_command_help(next);

	 if ( help )
	 {
	    print_short_help(next, help);

	    if ( next_remote == next )
	       free(help);
	 }
      }

      free(prev);

      if ( next == NULL )
	 break;

      if ( (prev = strdup(next)) == NULL )
	 break;
      
      if ( next_remote )
	 free(next_remote);
   }

   if ( next_remote )
      free(next_remote);

   if ( prev == NULL )
      return -1;

   return 0;
}

/** ==================================================================== */
/** 
 *   Prints the help about a command.
 * 
 *   @param name       Command name
 *   @param usage      Usage string
 *   @param description Description string
 * 
 *   @return           -
 *
 *   @par Globals:     
 *                     --
 */
/* ===================================================================== */
static void
print_help(char *name, char *usage, char *description)
{
   char  *ptr = strchr(description, '\n');
   char  *descr = description;
   int   len;

   if ( ptr )
   {
      len = ptr - description;
      descr = malloc(len + 1);
      strncpy(descr, description, len);
      descr[len] = '\0';
   }

   printf("NAME\n");
   printf("    %s - %s\n", name, descr);
   printf("\nSYNOPSIS\n");
   printf("    %s\n\n", usage);

   if ( ptr )
   {
      char  *str;
      char  *saveptr;

      ptr++;

      if ( (str = malloc(strlen(ptr))) != NULL )
      {
	 strcpy(str, ptr);
	 ptr = strtok_r(str, "\n", &saveptr);

	 printf("DESCRIPTION\n");

	 while ( ptr != NULL )
	 {
	    printf("    %s\n", ptr);

	    /* Simple fix to handle multiple '\n' */
	    while ( *saveptr == '\n' )
	    {
	       printf("\n");
	       saveptr++;
	    }

	    ptr = strtok_r(NULL, "\n", &saveptr);
	 }

	 printf("\n");
	 free(str);
      }

      free(descr);
   }
}

/** ==================================================================== */
/** 
 *   Locates help about a command and prints it.
 * 
 *   @param name       Command name
 * 
 *   @return           0 if OK, otherwise non-zero
 *
 *   @par Globals:     
 *                     local_cmds[]
 */
/* ===================================================================== */
static int
help_command(char *name)
{
   char  *usage;
   char  *descr;

   if ( local_command(name) )
   {
      print_help(name, local_command_usage(name), local_command_descr(name));
      return 0;
   }

   if ( (usage = remote_command_usage(name)) == NULL )
   {
      printf("No help for %s\n", name);

      return -1;
   }

   if ( (descr = remote_command_descr(name)) != NULL )
      print_help(name, usage, descr);
   else
      print_help(name, usage, "");

   if ( usage ) free(usage);
   if ( descr ) free(descr);

   return 0;
}

/** ==================================================================== */
/** 
 *   Prints help about a command or lists all commands.
 * 
 *   @param argc       Number of arguments
 *   @param argv       Pointer to argument strings
 * 
 *   @return           0 if OK, otherwise non-zero (result from command)
 *
 *   @par Globals:     
 *                     --
 */
/* ===================================================================== */
static int
run_help(int argc, char **argv)
{
   if ( argc < 2 )
      return list_commands("");

   if ( argv[1][strlen(argv[1])-1] != '*' )
      return help_command(argv[1]);

   argv[1][strlen(argv[1])-1] = '\0';

   return list_commands(argv[1]);
}

/** ==================================================================== */
/** 
 *   Prints help for commands.
 * 
 *   @param argc       Number of arguments
 *   @param argv       Pointer to argument strings
 * 
 *   @return           0 if OK, otherwise non-zero (result from command)
 *
 *   @par Globals:     
 *                     --
 */
/* ===================================================================== */
int
cmd_help(int argc, char **argv)
{
   return run_help(argc, argv);
}
