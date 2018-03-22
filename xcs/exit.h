/**
 *   Definitions for the exit command.
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

#ifndef __EXIT_H
#define __EXIT_H

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
#define EXIT_USAGE \
   "exit"

#define EXIT_SHORT \
   "Exits the command shell"

#define EXIT_DESCR \
   EXIT_SHORT

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
 *   Exits the shell.
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
extern int cmd_exit(int argc, char **argv);


#ifdef __cplusplus
}
#endif

#endif   /* ifndef __xxx_H */
