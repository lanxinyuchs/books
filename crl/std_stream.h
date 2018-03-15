/**
 *   Routines for save/set of standard streams
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
 *   Revised : 2014-09-13 Ravineet Singh EAB/FJP/HB
 *   Change  : set_std_stream_socket now returns status.
 *
 *   Revised : 2014-01-15 Ravineet Singh EAB/FJP/HB
 *   Change  : First version.
 * ========================================================================
 */

#ifndef __STD_STREAMS_H
#define __STD_STREAMS_H

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
typedef struct stdiofd_t_ 
{
   int     stdin;
   int     stdout;
   int     stderr;
} stdiofd_t;

/* ========================================================================
 *   DATA DECLARATIONS
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
extern void save_std_stream(stdiofd_t* fds);

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
extern void set_std_stream(stdiofd_t* fds);

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
extern int set_std_stream_socket(int sock_fd);

#ifdef __cplusplus
}
#endif

#endif   /* ifndef __STD_STREAMS_H */
