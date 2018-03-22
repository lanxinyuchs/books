/**
 *   The exit command.
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
 *   Revised : 2011-11-23 Lars Jönsson EAB/FJP/TB
 *   Change  : First version.
 * ========================================================================
 */

/* ========================================================================
 *   INCLUDE FILES
 * ========================================================================
 */
#include <stdlib.h>
#include <unistd.h>
#include <termios.h>

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
int cmd_exit(int argc, char **argv)
{
	struct termios attr;

	(void)argc;
	(void)argv;

	tcgetattr(STDIN_FILENO, &attr);
	attr.c_lflag |= ECHO;
	tcsetattr(STDIN_FILENO, TCSANOW, &attr);

	exit(0); /* Will never return */

	return 0;
}
