/**
 *   Routines for save/set of standard streams.
 * 
 *   @file
 * 
 *   This file is common to the COLI shell and command lib.
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
 *   Revised : 2014-09-13 Ravineet Singh EAB/FJP/HB
 *   Change  : set_std_stream_socket returns error in case fd:s were
 *             not read.
 *
 *   Revised : 2014-05-22 Henrik Wallin
 *   Change  : Fix problem with restoring file descriptors.
 *             See comment in set_std_stream_helper()
 *
 *   Revised : 2014-01-15 Ravineet Singh EAB/FJP/HB
 *   Change  : First version.
 * ========================================================================
 */

/* ========================================================================
 *   INCLUDE FILES
 * ========================================================================
 */
#include <assert.h>
#include <fdh.h>
#include <std_stream.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/socket.h>
#include <unistd.h>
#include <errno.h>

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
 *   Save standard stream
 * 
 *   @param fds  Descriptor pointer
 * 
 *   @return     copy of descriptors
 *
 *   @par Globals:     
 *                     --
 */
/* ===================================================================== */
void 
save_std_stream(stdiofd_t* fds)
{
   assert(NULL != fds);

   fds->stdin = dup(STDIN_FILENO);
   fds->stdout = dup(STDOUT_FILENO);
   fds->stderr = dup(STDERR_FILENO);
   assert( (-1 != fds->stdin) && (-1 != fds->stdout) && (-1 != fds->stderr) );
}

/** ==================================================================== */
/** 
 *   Set standard stream
 * 
 *   @param fds  Descriptor pointer
 * 
 *   @return     -
 *
 *   @par Globals:     
 *               --
 */
/* ===================================================================== */
static void
set_std_stream_helper(int from, int to)
{
   unsigned int i;
   int ret;

   /* We want to dup2(from, to) and close(from)
    *
    * dup2 can returtn EBUSY in some cases and we need to take care of that
    * It seems to trigger easily under lisim, while almost not at all on
    * target systems.
    * A guess is that the thread that used the temporary fds, is busy exiting
    * and the "close-all-fds" operation race with this dup2.
    *
    * We will retry dup2 a number of times with a progressive delay in between.
    * If we don't manage after N tries we call assert().
    * 100 loops -> total usleep <1s
    */

   for (i = 1; i < 100; i++) {
      ret = dup2(from, to);
      if (ret != -1) {
         close(from);
         return;
      }
      if (errno != EBUSY) {
         break;
      }
      usleep(100 * i);
   }
   assert(0);
}

void
set_std_stream(stdiofd_t* fds)
{
   set_std_stream_helper(fds->stdin, STDIN_FILENO);
   set_std_stream_helper(fds->stdout, STDOUT_FILENO);
   set_std_stream_helper(fds->stderr, STDERR_FILENO);
}

/** ==================================================================== */
/**
 *   Set standard stream from socket
 *
 *   @param socket_name  Socket name
 *
 *   @return             0 if OK, -1 otherwise
 *
 *   @par Globals:
 *                       --
 */
/* ===================================================================== */
int
set_std_stream_socket(int sock_fd)
{
   stdiofd_t fds;

   fds.stdin = receive_fd(sock_fd);
   fds.stdout = receive_fd(sock_fd);
   fds.stderr = receive_fd(sock_fd);

   close_socket(sock_fd);
   if ( (-1 == fds.stdin) || (-1 == fds.stdout) || (-1 == fds.stderr))
      return -1;
   else
      set_std_stream(&fds);

   return 0;
}

