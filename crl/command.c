/**
 *   Example program for COLI commands.
 * 
 *   @file
 * 
 *   This file is a part of the COLI command package.
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
 *   Revised : 2011-11-23 Lars Jönsson EAB/FJP/TB
 *   Change  : First version.
 * ========================================================================
 */

/* ========================================================================
 *   INCLUDE FILES
 * ========================================================================
 */
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#include <ose.h>
#include <shell.h>

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
static FILE  *out = NULL;
static int   state = 0;

static const char led_synopsis[] = "led [ on | off | toggle ]";
static const char led_descr[] = "Set or display LED state\n"
"The led command sets or displays the LED state.\n";

/* ========================================================================
 *   FUNCTION PROTOTYPES
 * ========================================================================
 */ 

/** ==================================================================== */
/** 
 *   The led command.
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
cmd_led(int argc, char **argv)
{
   if ( argc > 1 )
   {
      int changed = 1;

      if ( strncmp(argv[1], "o", strlen(argv[1])) == 0 )
	 changed = 0;
      else if ( strcmp(argv[1], "on") == 0 )
	 state = 1;
      else if ( strncmp(argv[1], "off", strlen(argv[1])) == 0 )
	 state = 0;
      else if ( strncmp(argv[1], "toggle", strlen(argv[1])) == 0 )
	 state = !state;
      else
	 changed = 0;

      if ( changed )
      {
	 fprintf(out, "\b\b%c ", state ? 'O' : '-');
	 fflush(out);
	 
	 printf("LED is set to %s\n", state ? "on" : "off");
	 
	 return 0;
      }
      else
      {
	 printf("Usage: %s\n", led_synopsis);
	 return -1;
      }
   }

   printf("LED is %s\n", state ? "on" : "off");

   return 0;
}

/** ==================================================================== */
/** 
 *   Command process.
 * 
 *   @param            -
 * 
 *   @return           -
 *
 *   @par Globals:     
 *                     --
 */
/* ===================================================================== */
OS_PROCESS(command_server)
{
   SIGSELECT     selAll[] = {0};
   union SIGNAL  *sig;

   if ( (out = fdopen(dup(STDOUT_FILENO), "a")) != 0 )
   {
      fprintf(out, "LED: - ");
      fflush(out);

      shell_add_cmd("led", led_synopsis, led_descr, cmd_led);

      while (1)
      {
	 sig = receive(selAll);
	 free_buf(&sig);
      }

      fclose(out);
   }

   kill_proc(current_process());
}
