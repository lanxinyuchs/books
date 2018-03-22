/**
 *   Handling of local commands.
 * 
 *   @file
 * 
 *   This file is a part of the COLI shell.
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
 *   Revised : 2014-10-15 Ravineet Singh EAB/FJP/HB
 *   Change  : Removed inclusion of libedit header files
 *
 *   Revised : 2014-10-09 Ravineet Singh EAB/FJP/HB
 *   Change  : Removed colish as a command line shell
 *
 *   Revised : 2014-06-30 Ravineet Singh EAB/FJP/HB
 *   Change  : Disabled colish as a command line shell when built/running
 *             in INTEGRATED_SHELL mode.
 *
 *   Revised : 2014-01-21 Ravineet Singh EAB/FJP/HB
 *   Change  : Removed faulty comment.
 *
 *   Revised : 2013-12-20 Ravineet Singh EAB/FJP/HB
 *   Change  : Added SIGINT handler via a constructor.
 *
 *   Revised : 2013-11-27 Ravineet Singh EAB/FJP/HB
 *   Change  : Added support for coli commands integrated into host shell.
 *             If colish is called via a soft link, execute it directly. 
 *
 *   Revised : 2012-03-08 Zabihullah Bayat EAB/FJP/RK
 *   Change  : -c option added to just execute commands without starting
 *             shell.
 *
 *   Revised : 2011-11-30 Lars Jönsson EAB/FJP/TB
 *   Change  : All command names are now allocated dynamically.
 *
 *   Revised : 2011-11-25 Lars Jönsson EAB/FJP/TB
 *   Change  : Moved the remaining stuff from run_cmd.c into this file.
 *
 *   Revised : 2011-11-23 Lars Jönsson EAB/FJP/TB
 *   Change  : First version.
 * ========================================================================
 */

/* ========================================================================
 *   INCLUDE FILES
 * ========================================================================
 */
#include <assert.h>
#include <ctype.h>
#include <libgen.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <unistd.h>

#include <local_cmd.h>
#include <remote_cmd.h>

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
 *   Executes the command in argv[0]. 
 * 
 *   @param argc       Standard argc
 *   @param argv       Standard argv
 * 
 *   @return           0 if OK, otherwise non-zero
 *
 *   @par Globals:     
 *                     --
 */
/* ===================================================================== */
static int
exec_command(int argc, char **argv)
{
   if ( argc < 1 )
      return -1;

   if ( local_command(argv[0]) )
      return run_local_command(argc, argv);

   return run_remote_command(argc, argv);
}

/** ==================================================================== */
/** 
 *   Main entry point of COLI shell (colish).
 * 
 *   @param argc       Number of arguments
 *   @param argv       Pointer to argument strings
 * 
 *   @return           0 if OK, otherwise non-zero (result from command)
 *                     -2 in case (non supported) colish is executed.
 *
 *   @par Globals:     
 *                     --
 */
/* ===================================================================== */
int
main (int argc, char **argv)
{
   /* Check if called by a soft link */
   if (0 != strcmp(basename(argv[0]), "colish"))
   {
      argv[0]=basename(argv[0]);
      return exec_command(argc, argv);
   }

   fprintf(stderr, "colish as an interactive command line shell is not allowed!\n");
   return -2;
}
