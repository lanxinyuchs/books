/**
 *   Routines for retrieving file descriptors via unix sockets.
 *
 *   @file
 *
 *   This file is a part of the COLI command lib.
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
 *   Revised : 2014-01-09 Ravineet Singh EAB/FJP/HB
 *   Change  : Merged lib/fdh.h and colish/fdh.h to this file.
 *
 *   Revised : 2011-10-10 Lars Jönsson EAB/FJP/TB
 *   Change  : First version.
 * ========================================================================
 */

#ifndef __FDH_H
#define __FDH_H

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
 *   Opens a unix socket
 *
 *   @param path       Socket name, should end with XXXXXX
 *
 *   @return           Socket handler, -1 if error
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
extern int open_socket(char *path);

/** ==================================================================== */
/**
 *   Closes a socket
 *
 *   @param fd         Socket descriptor
 *
 *   @return           -
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
extern void close_socket(int fd);

/** ==================================================================== */
/**
 *   Receive file descriptor(s) via the socket
 *
 *   @param fd         Socket descriptor
 *
 *   @return           File descriptor, -1 if error
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
extern int receive_fd(int fd);

/** ==================================================================== */
/**
 *   Send file descriptor(s) via the socket
 *
 *   @param path       Socket name
 *
 *   @return           -
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
extern int send_fd(char *path);

#ifdef __cplusplus
}
#endif

#endif   /* ifndef __FDH_H */
