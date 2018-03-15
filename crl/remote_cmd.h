/**
 *   Definitions for remote commands.
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
 *   Revised : 2013-12-20 Ravineet Singh EAB/FJP/HB
 *   Change  : Changed IPC mechanism from linx to OSE(LITS).
 *
 *   Revised : 2011-11-25 Lars Jönsson EAB/FJP/TB
 *   Change  : First version.
 * ========================================================================
 */

#ifndef __REMOTE_CMD_H
#define __REMOTE_CMD_H

#ifdef __cplusplus
extern "C" {
#endif

/* ========================================================================
 *   INCLUDE FILES
 * ========================================================================
 */
#include <ose.h>

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
/** ==================================================================== */
/** 
 *   Executes the remote command in argv[0]. 
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
extern int run_remote_command(int argc, char **argv);

/** ==================================================================== */
/** 
 *   Returns the next remote command where the name starts with expr.
 * 
 *   @param expr       Start of command name
 *   @param prev       Previous command name found. Empty string if this
 *                     is the first search
 * 
 *   @return           Pointer to updated prev. Note that prev has to be
 *                     large enough to hold the result.
 *
 *   @par Globals:     
 *                     --
 */
/* ===================================================================== */
extern char *next_remote_command(const char *expr, char *prev);

/** ==================================================================== */
/** 
 *   Return the short help string for the specified command.
 * 
 *   @param name       Command name
 * 
 *   @return           The help string if found, otherwise NULL
 *
 *   @par Globals:     
 *                     --
 */
/* ===================================================================== */
extern char *remote_command_help(char *name);

/** ==================================================================== */
/** 
 *   Return the usage string for the specified command.
 * 
 *   @param name       Command name
 * 
 *   @return           The usage string if found, otherwise NULL (must be
 *                     freed after use)
 *
 *   @par Globals:     
 *                     --
 */
/* ===================================================================== */
extern char *remote_command_usage(char *name);

/** ==================================================================== */
/** 
 *   Return the descr string for the specified command.
 * 
 *   @param name       Command name
 * 
 *   @return           The descr string if found, otherwise NULL (must be
 *                     freed after use)
 *
 *   @par Globals:     
 *                     --
 */
/* ===================================================================== */
extern char *remote_command_descr(char *name);


#ifdef __cplusplus
}
#endif

#endif   /* ifndef __xxx_H */
