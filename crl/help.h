/**
 *   Definitions for the help command.
 * 
 *   @file
 * 
 *   This file is a part of the COLI command server.
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

#ifndef __HELP_H
#define __HELP_H

#ifdef __cplusplus
extern "C" {
#endif

/* ========================================================================
 *   INCLUDE FILES
 * ========================================================================
 */

/* ========================================================================
 *   DEFINITIONS
 * ========================================================================
 */ 
#define HELP_USAGE \
   "help [<command>]"

#define HELP_SHORT \
   "Print help for one or all commands" \

#define HELP_DESCR \
   HELP_SHORT "\n" \
   "'help' prints a brief description of matching commands.\n\n" \
   "When a command is specified, 'help' also prints the synopsis and a\n" \
   "long description."
   
/* ========================================================================
 *   TYPE DEFINITIONS
 * ========================================================================
 */ 

/* ========================================================================
 *   DATA DECLARATIONS
 * ========================================================================
 */ 

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
extern int cmd_help(int argc, char **argv);


#ifdef __cplusplus
}
#endif

#endif   /* ifndef __xxx_H */
