/**
 *   The file declares TRI server command message.
 *
 *   Copyright (C) 2013-2014 by Ericsson AB. All rights reserved. The
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
 *   Revised : 2013-04-30 Anette Schött
 *   Change  : First version.
 *
 *   Revised : 2013-09-20 K V Ranganath
 *   Change  : Added PresetGroupMaskReq and PresetGroupMaskRsp
 *             signals.
 *
 *   Revised : 2014-11-24 K.V.Ranganadh
 *   Change  : Support for bus filter.
 *
 *   Revised : 2015-03-30 K.V.Ranganadh
 *   Change  : Improvement in default command
 * ========================================================================
 */

#ifndef TRI_SERVER_CMD_H
#define TRI_SERVER_CMD_H

/* ========================================================================
 *   INCLUDE FILES
 * ========================================================================
 */

#include "cello_te_handlers.h"
#include "itc.h"
#include "tri_server.h"

/* ========================================================================
 *   DEFINITIONS
 * ========================================================================
 */
/*
** Message base for signal between the shell process and TRI server
*/
#define TE_MSGBASE     (TRI_MSGBASE)

/*
** Maximum number of items in one OMCSF_STATUS_INFO_IND signal.
*/
#define TE_MAX_STATUS_ITEMS 450

/*
** The process package delimiter for java
** processes. In java an argument should be
** supplied to the local process and this
** is the package name. To avoid changing
** the te shell command it is concatenated
** with the process (thread) name like
** <PACKAGE_NAME>PROCESS_PACKAGE_DELIMITER<PROCESS_NAME>
*/
#define PROCESS_PACKAGE_DELIMITER ":"

/* ========================================================================
 *   TYPE DEFINITIONS
 * ========================================================================
 */

typedef uint32_t TeResultType;
#define TE_LOG_RESULT_OK     (0) /* Requested operation was performed alright  */
#define TE_LOG_RESULT_NOT_OK (1) /* Requested operation could not be performed */
#define TE_LOG_RESULT_EMPTY  (2)
#define TE_LOG_RESULT_BUSY   (3)

typedef uint32_t TeCmdResultType;
#define TE_CMD_RESULT_OK                (0) /* Command was performed alright     */
#define TE_CMD_RESULT_NOT_OK            (1) /* Command could not be performed    */
#define TE_CMD_RESULT_PROCESS_NOT_FOUND (2) /* Could not find the specified process */
#define TE_CMD_RESULT_SYNTAX_ERROR      (3) /* Syntax error                      */
#define TE_CMD_RESULT_PRESET_OVERFLOW   (4) /* Too many groupmask save requests */
#define TE_CMD_FILTER_UNSUPPORTED       (5) /* Requeset filter was unsupported */

typedef uint32_t MaskChange;
#define TE_MASK_SET      (0)
#define TE_MASK_ENABLE   (1)
#define TE_MASK_DISABLE  (2)

/* Scope of default command issued */
#define   TE_RESET_RUNNING  (0)
#define   TE_RESET_RESTART  (1)
#define   TE_RESET_PRESET   (2)


/*
** Information sent back in a STATUS_INFO_IND
*/
typedef struct
{
  itc_mbox_id_t   mbox;
  uint32_t        groupMask;
  ProcName        procName;
}StatusInfo;

/* ===================================================================== */
/** @GroupMaskReq
 *
 *   Enable or disable a number of trace groups or assertions for a process.
 *   A wild card, '*', may be used as the last character in the process
 *   name to specify a set of processes.
 *
 *  @param     change     In which way shall the group mask be changed:
 *                        set, enable or disable.
 *  @param     groupMask  The group mask to be used according to 'change'.
 *
 *  @param     procName   The name of the process(es) to enable or disable.
 */
/* ===================================================================== */
#define GROUP_MASK_REQ (TE_MSGBASE) /* !- SIGNO(GroupMaskReq) -! */

typedef struct
{
   uint32_t      msgNo;
   MaskChange    change;
   uint32_t      groupMask;
   ProcName      procName;
}GroupMaskReq;

/* ===================================================================== */
/** @GroupMaskCfm
 *
 *  A confirmation to a GROUP_MASK_REQ signal.
 *
 *  @param result  The result of the read request
 */
/* ===================================================================== */
#define GROUP_MASK_CFM (TE_MSGBASE + 1) /* !- SIGNO(GroupMaskCfm) -! */

typedef struct
{
   uint32_t         msgNo;
   TeCmdResultType  result;
}GroupMaskCfm;

/* ===================================================================== */
/** @SetDefaultGroupMaskReq
 *
 *  Set default trace group mask for the specified processes.
 *
 *  @param procName      The name of the process(es) to set the default
 *                       trace group mask.
 */
/* ===================================================================== */
#define SET_DEFAULT_GROUP_MASK_REQ (TE_MSGBASE + 2)
/* !- SIGNO(SetDefaultGroupMaskReq) -! */

typedef struct
{
   uint32_t         msgNo;
   uint32_t         scope;
   ProcName         procName;
}SetDefaultGroupMaskReq;

/* ===================================================================== */
/** @SetDefaultGroupMaskCfm
 *
 *  A confirmation to a OMCSF_SET_DEFAULT_GROUP_MASK_REQ.
 *
 *  @param result  The result of the set default mask request
 */
/* ===================================================================== */
#define SET_DEFAULT_GROUP_MASK_CFM (TE_MSGBASE + 3)
/* !- SIGNO(SetDefaultGroupMaskCfm) -! */

typedef struct
{
   uint32_t           msgNo;
   TeCmdResultType    result;
}SetDefaultGroupMaskCfm;

/* ===================================================================== */
/** @StatusReq
 *
 *  This signal is used for requesting the status of all processes
 *   currently being Trace & Error handled.
 *
 *  @param procName    The name(s) of the process(es) to get status for.
 */
/* ===================================================================== */
#define STATUS_REQ (TE_MSGBASE + 4) /* !- SIGNO(StatusReq) -! */

typedef struct
{
   uint32_t      msgNo;
   ProcName      procName;
}StatusReq;

/* ===================================================================== */
/** @StatusInfoInd
 *
 *  This signal is used for sending back status information
 *   as a response to a OMCSF_STATUS_REQ.
 *
 *  @param  noOfProc   Number of processes
 *  @param  status     The actual status informaion
 */
/* ===================================================================== */
#define STATUS_INFO_IND (TE_MSGBASE + 5) /* !- SIGNO(StatusInfoInd) -! */

typedef struct
{
   uint32_t      msgNo;
   unsigned int  noOfProc;
   StatusInfo    status[TE_MAX_STATUS_ITEMS];
}StatusInfoInd;

/* ===================================================================== */
/** @StatusCfm
 *
 *  A confirmation to a STATUS_REQ.
 *
 *  @param result  The result of the status request
 */
/* ===================================================================== */
#define STATUS_CFM (TE_MSGBASE + 6) /* !- SIGNO(StatusCfm) -! */

typedef struct
{
   uint32_t         msgNo;
   TeCmdResultType  result;
}StatusCfm;

/* ===================================================================== */
/** @SaveGroupMaskReq
 *
 *  This signal is use for saving the current enabled trace
 *  group mask for the specified process(es).
 *
 *  @param procName    The name of the process(es) to save current trace
 *                     groups for.
 */
/* ===================================================================== */
#define SAVE_GROUP_MASK_REQ (TE_MSGBASE + 7) /* !- SIGNO(SaveGroupMaskReq) -!  */

typedef struct
{
  SIGSELECT       sigNo;
  ProcName        procName;
}SaveGroupMaskReq;

/* ===================================================================== */
/** @SaveGroupMaskCfm
 *
 *  A confirmation to a OMCSF_SAVE_GROUP_MASK_REQ.
 *
 *  @param result      The result of the SAVE_GROUP_MASK_REQ.
 */
/* ===================================================================== */
#define SAVE_GROUP_MASK_CFM (TE_MSGBASE + 8) /* !- SIGNO(SaveGroupMaskCfm) -!  */

typedef struct
{
  SIGSELECT           sigNo;
  TeCmdResultType     result;
}SaveGroupMaskCfm;

/* ===================================================================== */
/** @LogReadReq
 *
 *  T&E log read request
 *
 *  @param oldestTime  Time in sec of oldest log entry to be read
 *  @param monitor     Receiver is Monitor true or false
 *  @param coreid      CoreId and index to the trace buffer to read from.
 */
/* ===================================================================== */
#define LOG_READ_REQ (TE_MSGBASE + 9) /* !- SIGNO(LogReadReq) -! */

typedef struct
{
   uint32_t  msgNo;
   int       oldestTime;
   Boolean   monitor;
   int       coreid;
}LogReadReq;

/* ===================================================================== */
/** @LogReadCfm
 *
 *  A confirmation to a LOG_READ_REQ signal.
 *
 *  @param result  The result of the read request
 */
/* ===================================================================== */
#define LOG_READ_CFM (TE_MSGBASE + 10) /* !- SIGNO(LogReadCfm) -! */

typedef struct
{
   uint32_t        msgNo;
   TeCmdResultType result;
}LogReadCfm;

/* ===================================================================== */
/** @LogReadAttachInd
 *
 *  Used by TRI server log process to attach to the log receiver whenever a
 *  LOG_READ_REQ has been received.
 *
 */
/* ===================================================================== */
#define LOG_READ_ATTACH_IND (TE_MSGBASE + 11)
/* !- SIGNO(LogReadAttachInd) -! */

typedef struct
{
   uint32_t msgNo;
}LogReadAttachInd;

/* ===================================================================== */
/** @LogReadAckTmoInd
 *
 *  Used by the TRI server log to supervise that the log read acknowledge
*   signal is received within a specifed timeout.
 *
 *  @param coreid  CoreId and index to the trace buffer to acknowledge.
 */
/* ===================================================================== */
#define LOG_READ_ACK_TMO_IND (TE_MSGBASE + 12)
/* !- SIGNO(struct LReadAckTmoIndS) -! */

typedef struct
{
   uint32_t  msgNo;
   int       coreid;
}LogReadAckTmoInd;

/* ===================================================================== */
/** @LogClearReq
 *
 *  This signal is used for clearing the Trace & Error Log.
 *
 *  @param coreid
 */
/* ===================================================================== */
#define LOG_CLEAR_REQ (TE_MSGBASE + 13) /* !- SIGNO(LogClearReq) -! */

typedef struct
{
   uint32_t  msgNo;
   int       coreid;
}LogClearReq;

/* ===================================================================== */
/** @LogClearCfm
 *
 *  A confirmation to a LOG_CLEAR_REQ signal.
 *
 *  @param result The result of the clear request
 */
/* ===================================================================== */
#define LOG_CLEAR_CFM (TE_MSGBASE + 14) /* !- SIGNO(LogClearCfm) -! */

typedef struct
{
   uint32_t         msgNo;
   TeCmdResultType  result;
}LogClearCfm;

/* ===================================================================== */
/** @TeLogReadAck2Ind
 *
 *  Used by the log reader to acknowledge
 *
 *  @param coreid  Core Id to acknowledge.
 *
 *  This signal is used by the log reader to acknowledge a received
 *  log read indication that has been marked to be acknowledged.
 */
/* ===================================================================== */
#define TE_LOG_READ_ACK2_IND (TE_MSGBASE + 15)
/* !- SIGNO(TeLogReadAck2Ind) -! */

typedef struct
{
   uint32_t  msgNo;
   int       coreid;
}TeLogReadAck2Ind;


/* ===================================================================== */
/** @struct teLogHuntHwdiIndS
 *
 *  Used by the log to hunt for hardware distribution server
 *
 *  @param -
 */
/* ===================================================================== */
#define TE_LOG_HUNT_HWDI_IND (TE_MSGBASE + 16)
/* !- SIGNO(TeLogHuntHwdiInd) -! */

typedef struct
{
   uint32_t msgNo;
}TeLogHuntHWdiInd;

/* ===================================================================== */
/** @PresetGroupMaskReq
 *
 *   Enable or disable a number of trace groups or assertions for a process.
 *   A wild card, '*', may be used as the last character in the process
 *   name to specify a set of processes if all is not given.
 *
 *  @param     groupMask  The group mask to be used.
 *
 *  @param     savePreset To check if preset needs to be saved.
 *
 *  @param     procName   The name of the process(es) to enable or disable.
 */
/* ===================================================================== */
#define PRESET_GROUP_MASK_REQ (TE_MSGBASE + 17)
/* !- SIGNO(PresetGroupMaskReq) -! */

typedef struct
{
   uint32_t      msgNo;
   Boolean       savePreset;
   uint32_t      groupMask;
   ProcName      procName;
} PresetGroupMaskReq;

/* ===================================================================== */
/** @PresetGroupMaskReq
 *
 *   Enable or disable a number of trace groups or assertions for a process.
 *   A wild card, '*', may be used as the last character in the process
 *   name to specify a set of processes if all is not given.
 *
 *  @param     groupMask  The group mask to be used.
 *
 *  @param     savePreset To check if preset needs to be saved.
 *
 *  @param     procName   The name of the process(es) to enable or disable.
 */
/* ===================================================================== */
#define PRESET_GROUP_MASK_CFM (TE_MSGBASE + 18)
/* !- SIGNO(PresetGroupMaskReq) -! */

typedef struct
{
   uint32_t      msgNo;
   uint32_t      result;
} PresetGroupMaskCfm;


/* ===================================================================== */
/** @FilterSetReq
 *
 *   Set or reset bus filter on given item.
 *
 *  @param     filterExpr  Filter Expression.
 *
 *  @param     procName    The name of the process on which filter shall
 *                         be set.
 *
 *  @param     type       To distinguish whether to set or reset a filter
 */
/* ===================================================================== */
#define FILTER_SET_REQ (TE_MSGBASE + 19)
/*!- SIGNO(FilterSetReq) -!*/

typedef struct
{
   uint32_t            msgNo;
   char                filterExpr[MAX_FILTER_LEN];
   ProcName            procName;
   uint32_t            type;
} FilterSetReq;

/* ===================================================================== */
/** @FilterSetCfm
 *
 *  A confirmation to FILTER_SET_REQ signal.
 *
 *  @param result The result of the filter set request
 */
/* ===================================================================== */
#define FILTER_SET_CFM (TE_MSGBASE + 20)
/*!- SIGNO(FilterSetCfm) -!*/

typedef struct
{
   uint32_t            msgNo;
   uint32_t            result;
} FilterSetRsp;


#endif  /* TRI_SERVER_CMD_H */

