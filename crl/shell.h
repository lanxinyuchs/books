/**
 *   Routines the command shell.
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
 *   Revised : 2014-07-14 Christoffer Cederwall EAB/FJP/HB
 *   Change  : Added shell_remove_cmd.
 *
 *   Revised : 2011-12-01 Lars Jönsson EAB/FJP/TB
 *   Change  : Added shell_add_cmd_attrs(), which is just mapped to
 *             shell_add_cmd().
 *
 *   Revised : 2011-11-22 Lars Jönsson EAB/FJP/TB
 *   Change  : Added defines for return values.
 *
 *   Revised : 2011-10-13 Lars Jönsson EAB/FJP/TB
 *   Change  : First version.
 * ========================================================================
 */

#ifndef __SHELL_H
#define __SHELL_H

#ifdef __cplusplus
extern "C" {
#endif

/* ========================================================================
 *   INCLUDE FILES
 * ========================================================================
 */
#include "ose.h"
#include "osetypes.h"

/* ========================================================================
 *   DEFINITIONS
 * ========================================================================
 */ 
#define	RET_SUCCESS	0
#define	RET_WARNING	10
#define	RET_ERROR	20

#define	DEFAULT_PRIORITY
#define	DEFAULT_PROC_TYPE
#define	DEFAULT_STACK_SIZE

enum ShellRunCmdTag
{
    RUNCMD_TAGEND        = 0x0,
    RUNCMD_EFS_CLONE     = 0x1,
    RUNCMD_CLONE_ENV     = 0x2,
    RUNCMD_NOPARENT      = 0x3,
    RUNCMD_ADDUSER       = 0x4,
    RUNCMD_PIDPTR        = 0x5,
    RUNCMD_ASYNCHRONOUS	 = 0x6,
    RUNCMD_INTERRUPTABLE = 0x8,
    RUNCMD_RETURN_STATUS = 0x9,
    RUNCMD_RETURN_ERRNO	 = 0xA,
    RUNCMD_FDARRAY       = 0xC,
    RUNCMD_ENVLIST       = 0xD
};

enum ShellRunCmdStatus
{
    SHELL_SUCCESS         = 0x0,
    SHELL_EUNKNOWN_TAG    = 0x1,
    SHELL_EMISSING_ARG    = 0x2,
    SHELL_EMISSING_CMD	  = 0x3,
    SHELL_EUNKNOWN_CMD	  = 0x4,
    SHELL_ETERMINATED	  = 0x5,
    SHELL_ECLONEFD_FAILED = 0x6,
    SHELL_EINVALID_FD	  = 0x7,
    SHELL_EILLEGAL_CMD	  = 0x8
};

/*
**  Just a wrapper around shell_add_cmd. The extra functionaly in compared
**  to shell_add_cmd is not supported and silently ignored.
*/
#define shell_add_cmd_attrs(name, usage, description, func, \
			    proc_type, priority, stack_size) \
   shell_add_cmd(name, usage, description, func)


/* ========================================================================
 *   TYPE DEFINITIONS
 * ========================================================================
 */ 
#define CmdType	cmd_func_t
typedef int (* cmd_func_t) (int argc, char ** argv);

/* ========================================================================
 *   DATA DECLARATIONS
 * ========================================================================
 */ 

/** ==================================================================== */
/** 
 *   Add a command to the shell.
 * 
 *   @param name       Command name
 *   @param usage      Usage string
 *   @param description Description
 *   @param func       Name of the function that handler the command
 * 
 *   @return           -
 *
 *   @par Globals:     
 *                     --
 *
 *   First line of description os treated as a short description. The
 *   rest (if more than one line) is the long description.
 */
/* ===================================================================== */
extern void shell_add_cmd(const char *name, const char *usage,
			  const char *description, cmd_func_t func);

/** ==================================================================== */
/** 
 *   Remove a command from the shell.
 * 
 *   @param name       Command name
 *   @param func       Name of the function that handler the command
 * 
 *   @return           -
 *
 *   @par Globals:     
 *                     --
 *
 */
/* ===================================================================== */
extern void shell_remove_cmd(const char *name, cmd_func_t func);

extern U32 shell_run_cmd(const char *cmd, const OSADDRESS *taglist);

#ifdef __cplusplus
}
#endif

#endif   /* ifndef __xxx_H */
