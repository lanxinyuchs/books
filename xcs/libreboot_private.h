/**
 *   @copyright
 *   Copyright (C) 2015 by Ericsson AB. All rights reserved. The
 *   information in this document is the property of Ericsson. Except
 *   as specifically authorized in writing by Ericsson, the receiver
 *   of this document shall keep the information contained herein
 *   confidential and shall protect the same in whole or in part from
 *   disclosure and dissemination to third parties. Disclosure and
 *   disseminations to the receiver's employees shall only be made on
 *   a strict need to know basis.
 */

/**
 * This file contains internal libreboot functions for the XCS platform.
 * They can change at any time and are not supported outside the platform team.
 */
#ifndef _LIBREBOOT_PRIVATE_H_
#define _LIBREBOOT_PRIVATE_H_

#include <sys/types.h>
#include <unistd.h>

/******************************************************************************
 *
 * Global function:
 *      reboot_crash
 *
 * Parameters:
 *      program    - the name of the program that crashed.
 *      pid        - the pid of the program that crashed.
 *      signal     - the signal which killed the program.
 *      pmd        - pmd file name with full path of the program that crashed.
 *
 * Return value:
 *
 * Description:
 *      This function will restart the board immediatly with AUBOOT.
 *
 * Side effects:
 *****************************************************************************/

extern void reboot_crash(char *program, pid_t pid, int signal, char *pmd);

#endif /* _LIBREBOOT_PRIVATE_H_ */
