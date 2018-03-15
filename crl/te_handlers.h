/**
 *   The contents of this file declares the interface to the T & E
 *   handlers.
 * 
 *   @file
 *
 *   Copyright (C) 2013-2015 by Ericsson AB. All rights reserved. The
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
 *   Revised : 1997-08-15
 *   Change  : Added the function OMCSF_raiseUnexpectedSig.
 *
 *   Revised : 1997-09-16
 *   Change  : Changed type of parameters to save space in log entries.
 *
 *   Revised : 1997-09-25
 *   Change  : Added function for creating a log write indication signal.
 *
 *   Revised : 1997-12-01
 *   Change  : Added a new member to the OMCSF_procInfoS struct to be able
 *             to decide wether the currently executing process supports
 *             floating-point instructions or not? Added to be able to
 *             solve TR HA67528.
 *
 *   Revised : 09-05-25 qtxleco
 *   Change  : Renamed fields 'floatSupport' and 'interfaceInfo' to
 *             'admFlags' and 'altGroupMask'. These fields were not used and admFlags 
 *             is needed for changed STR handling introduced in multicore.
 *             Both these new fields will be needed for T&E overload protection so this
 *             change is also a preparation for that functionality.
 *
 *   Revised : 2009-06-25 xcssvad
 *   Change  : UABtr71552 - deprecated warnings removal
 *
 *   Revised : 2009-09-30 xcssamo
 *   Change  : WRNae57637 - Added macro to set 24-bit in  admFlags when STR 
 *             is used inside other trace primitives.
 *
 *   Revised : 2009-06-25 xcssamo
 *   Change  : UABtr71552 - deprecated warnings removal.
 *
 *   Revised : 2009-10-01 xcssadh
 *   Change  : UABtr75501 - Removed GCC4 warnings in C++ environment.
 *
 *   Revised : 2009-12-18 xcssamo
 *   Change  : WRNae70553 - Removed SET_STR_FLAG to avoid warnings.
 *
 *   Revised: 2010-12-20 xcssamo
 *   Change : Removed ADM_FLAGS_STR_WRITE_AREA due to new way of 
 *            STR implementation.
 *
 *   Revised: 2010-06-15 xcssamo
 *   Change : UABtr80492 : Added code to provide facility for the
 *            user to input its own information when writing trace logs.
 *
 *   Revised: 2009-12-25 Niranjan (XCSNIRD)
 *   Change : Trace Overload Protection CR implementation (WRNae15217)
 *
 *   Revised : 2013-05-14 Anette Schött
 *   Change  : Updated according to template and added macros for Linux.
 *
 *   Revised : 2013-09-22 Satish Muchala
 *   Change  : Implementation for Selective Trace function feature.
 *
 *   Revised : 2013-12-10 M. Winberg
 *   Change  : Defined OMCSF_procInfoS to ItemInfo in Linux part.
 *             Made ItemInfo textual equivalent to OMCSF_procInfoS.
 *             Defined OMCSF_procNameA to ProcName in Linux part.
 *             Defined OMCSF_MAX_FORMAT_STR_LEN to TRI_MAX_FORMAT_STR_LEN.
 *             Declared OMCSF_logObjTrace with attribute deprecated.
 *             Declared OMCSF_logObjBusTrace with attribute deprecated.
 *             Defined OMCSF_formatStr to formatStr.
 * 
 *  Revised  : 2014-01-14 
 *  Change   : Added (missing) declaration OMCSF_deRegisterInterface 
 *             with attribute deprecated in Linux part. Also added 
 *             OMCSF_registerInterface declaration with attribute 
 *             deprecated. Declared OMCSF_logTrace and OMCSF_logBusTrace
 *             with attribute deprecated in Linux part. Defined 
 *             OMCSF_procInfo_p to procInfo, that is, a pointer to
 *             OMCSF_procInfoS defined to a pointer to ItemInfo.
 *
 *   Revised : 2014-01-23 Naresh Angala
 *   Change  : WCDMA00017891 : CR to improve the robustness of STR
 *             macro so that compilation errors or warnings are seen
 *             when STR is called with wrong number of arguments.
 *
 *  Revised  : 2014-02-05
 *  Change   : Defined OMCSF_defaultProcInfo to defaultItemInfo, that is,
 *             to its Linux implementation equivalent.
 *             Defined OMCSF_MAX_PROC_NAME_LEN to its Linux implementation 
 *             equivalent TRI_MAX_PROC_NAME_LEN. 
 *
 *   Revised : 2014-04-10 Stanislav Vovk
 *   Change  : Added OMCSF_sendObjLogTrace and OMCSF_recObjLogTrace
 *             in Linux part.
 *
 *   Revised : 2014-04-24 Stanislav Vovk
 *   Change  : Linux: Removed fileandline and objectname from struct ItemInfo
 *             Not needed anymore
 *
 *   Revised : 2014-06-10 Anette Schött
 *   Change  : Added deRegisterIfObj.
 *
 *   Revised : 2014-06-16 Niranjan Kumar D
 *   Change  : Design Object : Scalable TRI.
 *
 *   Revised : 2015-01-31 Anette Schött
 *   Change  : Update of struct ItemInfo to include pointers to trace object
 *             struct and support clientPtr for both 32 and 64 bit machines.
 *
 *   Revised : 2015-04-20 Anette Schött
 *   Change  : Add support for OMCSF_raiseUnexpectedSig in Linux.
 *
 *   Revised : 2015-05-21 Anette Schött
 *   Change  : Revert the struct ItemInfo back to not break legacy.
 *
 *   Revised : 2015-10-28 Anette Schött
 *   Change  : Updated with low level TRI additions.
 *
 *   Revised : 2015-09-09 Anette Schött
 *   Change  : Add inclusion of tri_osetypes.h to be used when not using
 *             LITS.
 * ========================================================================
 */

#ifndef CELLO_TE_HANDLERS_H
#define CELLO_TE_HANDLERS_H
 

/* >   External dependencies
** 
*/

#ifdef NO_LITS
#include "tri_osetypes.h"
#include "itc.h"
#else
/* This is done to allow including ose files in a special way. Needed for host
 * applications like host monitor */
#ifndef ZZOSE_H
#include "ose.h"
#endif
#ifndef _OSETYPES_H
#include <osetypes.h>
#endif
#endif

#include "cello_te_group.h"       /* Internal trace group declarations */

#ifdef __cplusplus
extern "C" {
#endif

/* ===================================================================== */
#ifndef LNX    /* OSE implementation */
/* ===================================================================== */

/* >   Definitions
**
**   Macros
*/

/*
 * Pointer to the trace item (process) information structure.
 */
#ifdef TRI_MULTICORE
#define TRI_PROC_INFO_PTR (OMCSF_procInfo_p)[ose_cpu_id()].traceItem_p
#else
#define TRI_PROC_INFO_PTR OMCSF_procInfo_p
#endif

/*
** Maxmimum length of a process name including
** terminating null character
*/
#define OMCSF_MAX_PROC_NAME_LEN       128

/*
** Maximum length of the temporary string created
** with the OMCSF_formatStr function for background process.
*/
#define OMCSF_MAX_FORMAT_STR_LEN      512

/* Bit 23 is used for Trace overload protection */
#define ADM_FLAGS_ALTMASK_INUSE_BIT   0x100

/* Bit 22 is used for user timestamp decision   */
#define USER_TIME_STAMP_INUSE_BIT     0x200

#define OMCSF_MAX_NUM_CORES           32

/*
** Version of TRI proxy.
*/
#define TRI_VER                       0x01

/*
**   Typedefs
*/

/*
** String types for process names and format strings
*/
typedef char OMCSF_procNameA[OMCSF_MAX_PROC_NAME_LEN];
typedef char OMCSF_formatStrA[OMCSF_MAX_FORMAT_STR_LEN];

/* >   Struct: OMCSF_procInfoS
**
** Description:
**   Structure holding all relevant trace and error handling info
**   for all types of processed except background process.
**
**   The admFlags has the following interpretation:
**   Bit 0 (zero) = MSB. Bit 31 = LSB.
**   Bit 25-31: Allocated STR write area.
*/
struct OMCSF_procInfoS
{
  SIGSELECT        sigNo;         /* Signal number when being used as signal */
  U32              admFlags;      /* Administrative trace information        */
  U32              altGroupMask;  /* Alternative mask used during overload   */
  U32              groupMask;     /* Mask with enabled trace groups          */
  U32              statusMask;    /* Mask with raised errors                 */
  void            *busLogFilter;  /* Compiled bus log filter                 */
  OMCSF_procNameA  procName;      /* The name of this process                */
};

struct ProcInfo
{
  struct OMCSF_procInfoS *traceItem_p;
  U32 groupMask;
};

/* >   Struct: OMCSF_procInfoS1
**
** Description:
**   Structure holding all relevant trace and error handling info
**   for a background process.
**   See also struct OMCSF_procInfoS.
*/
struct OMCSF_procInfoS1
{
  SIGSELECT        sigNo;         /* Signal number when being used as signal */
  U32              admFlags;      /* Administrative trace information        */
  U32              altGroupMask;  /* Alternative mask used during overload   */
  U32              groupMask;     /* Mask with enabled trace groups          */
  U32              statusMask;    /* Mask with raised errors                 */
  void            *busLogFilter;  /* Compiled bus log filter                 */
  OMCSF_procNameA  procName;      /* The name of this process                */
  OMCSF_formatStrA formatStr;     /* Temporary formatted string              */
};

/* >   Struct: userData 
**
** Description:
**   Structure holding user time in secs and microsecs and process name
**   input by user. This is now used when user uses the trace macro TRACE_UTS
**   to generate a trace entry with its own timestamp and process information.
*/

typedef struct
{
  U32             tsec;           /* User time in seconds                   */
  U32             tusec;          /* User time in micro seconds             */
  OMCSF_procNameA procName;       /* Process name given as input by user    */
}userData;


/* >   Struct: strData
**
** Description:
**   Structure holding the str_root pointer and the teStrBufferProc
**   str_root points to the STR buffer memory from application
**   pool. teStrBufferProc is the process to which all STR buffers
**   that are allocated are forwarded . strBufCount is the number of
**   STR buffers allocated for the LM. The structure is only updated
**   for Movable LM.
*/
struct strData
{
    void *str_root;
    PROCESS strProcId;
    U32  strBufCount;
};


/* 
**   Constants
*/


/*
**   Variables
*/


/* >   Global variable: OMCSF_defaultProcInfo
**
** Description:
**   This is a global variable that keeps the process information
**   structure for all processes that has not registered itselfs
**   to Trace & Error Handling.
**
*/
extern struct OMCSF_procInfoS OMCSF_defaultProcInfo;


/* >   Global variable: OMCSF_procInfo_p
**
** Description:
**   This is a global variable that is unique to the context of the
**   currently executing process. This is accomplished using the
**   process variables services of the RTOSI.
**
**   This variable contains a pointer to the process information structure
**   for the currently executing process.
*/
extern struct OMCSF_procInfoS *OMCSF_procInfo_p;

/* >   Global variable: selTraceFunc
**
** Description:
**   This is a global variable that is used to store address of selective
**   trace function. This variable is dereferenced on each TRACE_SEL macro
**   call.
**
*/
extern Boolean (*selTraceFunc)(void *);

/* >   Global variable: selTraceIfFunc
**
** Description:
**   This is a global variable that is used to store address of selective
**   trace if function. This variable is dereferenced on each TRACE_IF_SEL
**   macro.
**
*/
extern Boolean (*selTraceIfFunc)(void *);

/* >   Global variable: selTraceObjFunc
**
** Description:
**   This is a global variable that is used to store address of selective
**   trace if function. This variable is dereferenced on each TRACE_OBJ_SEL
**   macro.
**
*/
extern Boolean (*selTraceObjFunc)(void *);

/* >   Declarations
**
** 
*/
/* >   Public function: defaultSelTraceFunc()
**
** Description:
**   Default selective trace function.
**
** Pre-conditions:
**   None.
**
** Post-conditions:
**   None.
**
** Returns:
**   True always.
**
*/
extern Boolean defaultSelTraceFunc(void *);

/* >   Public function: OMCSF_initPriProc()
**
** Description:
**   Initiate trace and error handling information for the current process.
**   (non multicore version)
**
** Pre-conditions:
**   None.
**
** Post-conditions:
**   None.
**
** Returns:
**   N/A
**
*/
extern void
OMCSF_initPriProc(void);

/* >   Public function: OMCSF_initPriProcMc()
**
** Description:
**   Initiate trace and error handling information for the current process.
**   (multicore version)
**
** Pre-conditions:
**   None.
**
** Post-conditions:
**   None.
**
** Returns:
**   N/A
**
*/
extern void
OMCSF_initPriProcMc(void);

/* >   Public function: OMCSF_runIntProc()
**
** Description:
**   This function encapsulates the call to an interrupt process
**   (non multicore version).
**
** Pre-conditions:
**   None.
**
** Post-conditions:
**   None.
**
** Returns:
**   N/A
**
*/
extern void
OMCSF_runIntProc(
  void (*entry)(void),  /* I: Pointer to the int proc to run  */
  void **procInfo_pp);  /* I: Info struct for process to run  */

/* >   Public function: OMCSF_runIntProcMc()
**
** Description:
**   This function encapsulates the call to an interrupt process
**   (multicore version).
**
** Pre-conditions:
**   None.
**
** Post-conditions:
**   None.
**
** Returns:
**   N/A
**
*/
extern void
OMCSF_runIntProcMc(
  void (*entry)(void),  /* I: Pointer to the int proc to run  */
  void **procInfo_pp);  /* I: Info struct for process to run  */

/* >   Public function: OMCSF_registerInterface()
**
** Description:
**   This function register an interface to be able to control
**   its interface traces.
**
** Pre-conditions:
**   None.
**
** Post-conditions:
**   None.
**
** Returns:
**   N/A
**
*/
extern void
OMCSF_registerInterface(
  const char             *interfaceName,     /* I: Name of the interface */
  struct OMCSF_procInfoS **interfaceInfo_pp); /* I: Interface info struct */


/* >   Public function: OMCSF_raiseStatus()
**
** Description:
**   This function raises the specified status in the statusMask
**   and generates a log entry.(Non multicore version)
**
** Pre-conditions:
**   None.
**
** Post-conditions:
**   The specified status bit has been set in OMCSF_statusMask
**
** Returns:
**   N/A
**
*/
extern void
OMCSF_raiseStatus(
  OMCSF_groupE  status,  /* I: Error status to raise                       */
  const char    *file,   /* I: Name of the file where the error occured    */
  U16           line,    /* I: Line in the file on which the error occured */
  const char    *msg);   /* I: A message to log                      */

/* >   Public function: OMCSF_raiseStatusMc()
**
** Description:
**   This function raises the specified status in the statusMask
**   and generates a log entry.(Multicore version)
**
** Pre-conditions:
**   None.
**
** Post-conditions:
**   The specified status bit has been set in OMCSF_statusMask
**
** Returns:
**   N/A
**
*/
extern void
OMCSF_raiseStatusMc(
  OMCSF_groupE  status,  /* I: Error status to raise                       */
  const char    *file,   /* I: Name of the file where the error occured    */
  U16           line,    /* I: Line in the file on which the error occured */
  const char    *msg);   /* I: A message to log */

/* >   Public function: OMCSF_raiseUnexpectedSig()
**
** Description:
**   This function raises the specified status in the statusMask
**   and generates a log entry consisting of the signal number
**   and the sender of the specified signal.(Non multicore version)
**
** Pre-conditions:
**   None.
**
** Post-conditions:
**   The specified status bit has been set in OMCSF_statusMask
**
** Returns:
**   N/A
**
*/
extern void
OMCSF_raiseUnexpectedSig(
  OMCSF_groupE  status,  /* I: Error status to raise                       */
  const char    *file,   /* I: Name of the file where the error occured    */
  U16           line,    /* I: Line in the file on which the error occured */
  union SIGNAL *sig_p);  /* I: The unexpectedly received signal            */

/* >   Public function: OMCSF_raiseUnexpectedSigMc()
**
** Description:
**   This function raises the specified status in the statusMask
**   and generates a log entry consisting of the signal number
**   and the sender of the specified signal.(Multicore version)
**
** Pre-conditions:
**   None.
**
** Post-conditions:
**   The specified status bit has been set in OMCSF_statusMask
**
** Returns:
**   N/A
**
*/
extern void
OMCSF_raiseUnexpectedSigMc(
  OMCSF_groupE  status,  /* I: Error status to raise                       */
  const char    *file,   /* I: Name of the file where the error occured    */
  U16           line,    /* I: Line in the file on which the error occured */
  union SIGNAL *sig_p);  /* I: The unexpectedly received signal            */

/* >   Public function: OMCSF_logTrace()
**
** Description:
**   This function logs a trace from the specified trace group.
**
** Pre-conditions:
**   None.
**
** Post-conditions:
**   None.
**
** Returns:
**   Always returns True.
**
*/
extern Boolean
OMCSF_logTrace(
  OMCSF_groupE  group, /* I: The log belongs to this trace group       */
  const char    *file,  /* I: File name from where the trace is logged  */
  U16           line,  /* I: Line in the file where trace is logged    */
  const char    *msg,  /* I: Message to log                      */
  struct OMCSF_procInfoS *traceObj_p);

extern Boolean
OMCSF_logObjTrace(
  OMCSF_groupE  group, /* I: The log belongs to this trace group       */
  const char    *file,  /* I: File name from where the trace is logged  */
  U16           line,  /* I: Line in the file where trace is logged    */
  const char    *msg,  /* I: Message to log                      */
  struct OMCSF_procInfoS *traceProc_p,
  struct OMCSF_procInfoS *traceObj_p);

/* >   Public function: OMCSF_logBusTrace()
**
** Description:
**   This function logs a trace from specified bus trace group.
**
** Pre-conditions:
**   None.
**
** Post-conditions:
**   None.
**
** Returns:
**   Always returns True.
**
*/
extern Boolean
OMCSF_logBusTrace(
  struct OMCSF_procInfoS *procInfo_p, /* I: Which filter to apply        */
  OMCSF_groupE            group,      /* I: Trace group, send or receive */
  const char              *file,       /* I: File name                    */
  U16                     line,       /* I: Line number                  */
  const  char            *msg,       /* I: Text message                 */
  U8                     *data,       /* I: The bus message data         */
  U16                     length);    /* I: The length of the bus data   */

extern Boolean
OMCSF_logObjBusTrace(
  struct OMCSF_procInfoS *procInfo_p, /* I: Process name                 */
  struct OMCSF_procInfoS *objInfo_p,  /* I: Which filter to apply        */
  OMCSF_groupE            group,      /* I: Trace group, send or receive */
  const char             *file,       /* I: File name                    */
  U16                     line,       /* I: Line number                  */
  const char             *msg,        /* I: Text message                 */
  U8                     *data,       /* I: The bus message data         */
  U16                     length);    /* I: The length of the bus data   */


/* >   Public function: OMCSF_recLogTrace()
**
** Description:
**   This function logs a trace for a received signal.
**
** Pre-conditions:
**   None.
**
** Post-conditions:
**   None.
**
** Returns:
**   Always returns True.
**
*/
extern Boolean
OMCSF_recLogTrace(
  const char             *file,        /* I: File name             */
  U16                     line,        /* I: Line number           */
  void                   *signal,      /* I: Signal                */
  const char             *msg,         /* I: Text message          */
  struct OMCSF_procInfoS *procInfo_p); /* I: Trace object data     */

extern Boolean
OMCSF_recObjLogTrace(
  const char             *file,        /* I: File name             */
  U16                     line,        /* I: Line number           */
  void                   *signal,      /* I: Signal                */
  const char             *msg,         /* I: Text message          */
  struct OMCSF_procInfoS *procInfo_p,  /* I: Trace process data    */
  struct OMCSF_procInfoS *objInfo_p);  /* I: Trace object data     */

/* >   Public function: OMCSF_sendLogTrace()
**
** Description:
**   This function logs a trace for a send signal.
**
** Pre-conditions:
**   None.
**
** Post-conditions:
**   None.
**
** Returns:
**   Always returns True.
**
*/
extern Boolean
OMCSF_sendLogTrace(
  const char             *file,        /* I: File name             */
  U16                     line,        /* I: Line number           */
  U32                     signo,       /* I: Signal number         */
  U32                     recpid,      /* I: Receiving process     */
  const char             *msg,         /* I: Text message          */
  struct OMCSF_procInfoS *procInfo_p); /* I: Trace object data     */

extern Boolean
OMCSF_sendObjLogTrace(
  const char             *file,        /* I: File name             */
  U16                     line,        /* I: Line number           */
  U32                     signo,       /* I: Signal number         */
  U32                     recpid,      /* I: Receiving process     */
  const char             *msg,         /* I: Text message          */
  struct OMCSF_procInfoS *procInfo_p,  /* I: Trace object data     */
  struct OMCSF_procInfoS *ObjInfo_p);  /* I: Trace object data     */


/* >   Public function: OMCSF_formatStr()
**
** Description:
**   This function builds a formatted string and stores in the
**   process information structure for the current process.
**
** Pre-conditions:
**   None.
**
** Post-conditions:
**   None.
**
** Returns:
**   A pointer to the formatted string.
**
*/
#ifdef TRI_STR_CHECK
extern char *
OMCSF_formatStr(const char *format, ...) __attribute__((format(printf,1,2)));
                /* I: String containing format information              */
                /* I: Variable number of parameters according to format */
#else
extern char *
OMCSF_formatStr(const char *format, ...);
                /* I: String containing format information              */
                /* I: Variable number of parameters according to format */
#endif


/* >   Public function: OMCSF_createLogWriteInd()
**
** Description:
**   This function create a log write indication signal
**   ready to be sent to the Trace & Error Log process.
**
** Pre-conditions:
**   None.
**
** Post-conditions:
**   None.
**
** Returns:
**   A pointer to the allocated and created log write
**   indication signal.
**
*/
extern union SIGNAL *
OMCSF_createLogWriteInd(
  OMCSF_groupE  group,     /* I: The kind of trace group to log from  */
  const char   *file,      /* I: File name from where log is reported */
  U16           line,      /* I: Line in file where log is reported   */
  const char    *msg,      /* I: The actual message to log            */
  const U8     *binData,   /* I: Binary data to log                   */
  U16           dataLen,   /* I: Length of binary data to log         */
  struct OMCSF_procInfoS *traceProc_p,
  struct OMCSF_procInfoS *traceObj_p);



/*Added by eplmigr */

/* >   Public function: OMCSF_deRegisterInterface()
**
**   Descriptions of public functions are found in the
**   corresponding header files.
*/
void
OMCSF_deRegisterInterface(struct OMCSF_procInfoS  **traceObj_p);


/* >   Public function: OMCSF_logUserTrace()
**
** Description:
**   This function logs a trace for with user timestamp.
**   process, file and line information.
**
** Pre-conditions:
**   None.
**
** Post-conditions:
**   None.
**
** Returns:
**   Always returns True.
**
*/

extern Boolean
OMCSF_logUserTrace(
  OMCSF_groupE  group,
  U32 tsec,
  U32 tusec,
  const char *file,
  U16   line,
  char *procName,
  const char *msg,
  U8 *binData,
  U16 dataLength,
  struct OMCSF_procInfoS *procInfo_p);


/* >   Public function: OMCSF_logObjUserTrace()
**
** Description:
**   This function logs a trace for with user timestamp.
**   process, file and line information. This is used
**   for interfaces.
**
** Pre-conditions:
**   None.
**
** Post-conditions:
**   None.
**
** Returns:
**   Always returns True.
**
*/

extern Boolean
OMCSF_logObjUserTrace(
  OMCSF_groupE  group,
  U32 tsec,
  U32 tusec,
  const char *file,
  U16   line,
  char *procName,
  const char *msg,
  U8 *binData,
  U16 dataLength,
  struct OMCSF_procInfoS *procInfo_,
  struct OMCSF_procInfoS *interfaceInfo_p);


/* >   Public function: OMCSF_logSelectiveTrace
**
** Description:
**   This function logs a selective traces with specified trace group
**
** Pre-conditions:
**   None.
**
** Post-conditions:
**   None.
**
** Returns:
**   Always returns True.
**
*/
extern Boolean
OMCSF_logSelectiveTrace(OMCSF_groupE group, const char *file, U16 line,
                        const char *msg,
                        struct OMCSF_procInfoS *procInfo_p);


/* >   Public function: OMCSF_logObjSelectiveTrace
**
** Description:
**   This function logs a selective traces with specified trace group
**   This is used for both interfaces and objects.
**
** Pre-conditions:
**   None.
**
** Post-conditions:
**   None.
**
** Returns:
**   Always returns True.
**
*/
extern Boolean
OMCSF_logObjSelectiveTrace(OMCSF_groupE group, const char *file, U16 line,
                           const char *msg,
                           struct OMCSF_procInfoS *procInfo_p,
                           struct OMCSF_procInfoS *traceObj_p);

/* >   End of double inclusion protection
**
*/

/* ===================================================================== */
#else    /* Linux implementation */
/* ===================================================================== */

/* ========================================================================
 *   DEFINITIONS
 * ========================================================================
 */

/*
 * Pointer to the trace item (process) information structure.
 */
#define TRI_PROC_INFO_PTR procInfo

/*
** Maxmimum length of a process name including
** terminating null character
*/
#define TRI_MAX_PROC_NAME_LEN       128
/*
** Maximum length of the temporary string created
** with the OMCSF_formatStr function for background process.
*/
#define TRI_MAX_FORMAT_STR_LEN      512

/* Bit 23 is used for Trace overload protection */
#define TRI_ADM_FLAGS_ALTMASK_INUSE_BIT   0x100

/* Bit 22 is used for user timestamp decision   */
#define TRI_USER_TIME_STAMP_INUSE_BIT     0x200

/*
** Maxmimum length of process name + inteface name including            
** interface or object name.            
*/              
#define TRI_MAX_PROC_AND_OBJ_IF_NAME_LEN  256           
                        
/*              
** Maxmimum length of file and line information including               
** terminating null character           
*/              
#define TRI_MAX_FILE_AND_LINE_LEN         128

/* Remap OMCSF defines to Linux equivalents */
#define OMCSF_MAX_PROC_NAME_LEN  TRI_MAX_PROC_NAME_LEN
#define OMCSF_MAX_FORMAT_STR_LEN TRI_MAX_FORMAT_STR_LEN 

/*
**  Remap OMCSF typedefs to Linux equivalents 
*/
#define OMCSF_procInfoS ItemInfo
#define OMCSF_procNameA ProcName

/*
 * Result codes for TRI with no LITS dependancy.
 */
#define  TRI_OK                  0
#define  TRI_INVALID_PARAM       1
#define  TRI_NOT_ALLOWED         2
#define  TRI_NOT_INITIALIZED     3
#define  TRI_ALREADY_INITIALIZED 4
#define  TRI_INTERNAL_ERROR      99

/*
 * Thread specific pointer, pointing to a trace item struct.
 */
extern __thread struct ItemInfo *procInfo;

/* 
** Remap OMCSF variables to Linux equivalents 
*/
#define OMCSF_procInfo_p procInfo
#define OMCSF_defaultProcInfo defaultItemInfo

/* ========================================================================
 *   TYPE DEFINITIONS
 * ========================================================================
 */


/*
** String types for process names
*/
typedef char ProcName[TRI_MAX_PROC_NAME_LEN];

/*              
** String types for process and interface name together.                
** interface and object name            
*/              
typedef char ProcAndObjIfName[TRI_MAX_PROC_AND_OBJ_IF_NAME_LEN];                
                        
/*              
** String types for file name and line number together.         
*/              
typedef char FileAndLine[TRI_MAX_FILE_AND_LINE_LEN];

/*   Struct: ItemInfo
**
** Description:
**   Structure holding all relevant trace and error handling info
**   for all types of processes except background process.
**
**   The admFlags has the following interpretation:
**   Bit 0 (zero) = MSB. Bit 31 = LSB.
**   Bit 25-31: Allocated STR write area.
**
*/
struct ItemInfo 
{
  SIGSELECT        sigNo;         /* Signal number when being used as signal */
  U32              admFlags;      /* Administrative trace information        */
  U32              altGroupMask;  /* Not in use - see comment above          */
  U32              groupMask;     /* Mask with enabled trace groups          */
  U32              statusMask;    /* Mask with raised errors                 */
  void             *busLogFilter; /* Compiled bus log filter                 */
  ProcName         procName;      /* The name of this process                */
  void             *clientPtr;    /* Pointer to be used by client if needed  */
  void             **itemPtr1;    /* Pointer to trace object struct          */
  void             **itemPtr2;    /* A second pointer to the same trace      */
                                  /* object struct pointing to by itemPtr1   */
};

/* ========================================================================
 *   DATA DECLARATIONS
 * ========================================================================
 */

/* Pointer to thread private data */
extern __thread struct ItemInfo *procInfo;

/* >   Global variable: defaultItemInfo
**
** Description:
**   This is a global variable that keeps the trace item information
**   structure for all threads that has not registered itselfs
**   to Trace & Error Handling.
**
*/
extern struct ItemInfo defaultItemInfo;

/* ========================================================================
 *   FUNCTION DECLARATIONS
 * ========================================================================
 */

/**
 *   deregisterIfObj()
 *
 *   @param   ref       Trace item information struct for a interface
 *                      or object.
 *
 *   @return            -
 *
 *   This function deregister an trace interface or trace object.
 */
extern void 
deregisterIfObj(struct ItemInfo **ref);

/**
 *   OMCSF_registerInterface()
 *
 *   @param   interfaceName      Name of the trace item interface.
 *
 *   @param   interfaceInfo_pp   Trace item information struct for
 *                               a interface.
 *
 *   @return            -
 *
 *   NOTE: This is a deprecated function and should not be used
 *         in new development.
 */
extern void
OMCSF_registerInterface(const char             *interfaceName,
                        struct OMCSF_procInfoS **interfaceInfo_pp)
   __attribute__ ((deprecated));

/**
 *   OMCSF_deRegisterInterface()
 *
 *   @param   traceObj_p   Trace item information struct for
 *                         a interface.
 *
 *   @return            -
 *
 *   NOTE: This is a deprecated function and should not be used
 *         in new development.
 */
extern void
OMCSF_deRegisterInterface(struct OMCSF_procInfoS  **traceObj_p)
   __attribute__ ((deprecated));

/**
 *   OMCSF_raiseUnexpectedSig()
 *
 *   @param   status   Error status to raise.
 *   @param   file     Name of the file where the error occured.
 *   @param   line     Line in the file on which the error occured.
 *   @param   sig_p    The unexpectedly received signal.
 *
 *   @return            -
 *
 */
extern void
OMCSF_raiseUnexpectedSig(OMCSF_groupE  status,
                         const char    *file,
                         U16           line,
                         union itc_msg *sig);

/**
 *   OMCSF_logTrace()
 *
 *   @param   group      Trace group where the message is logged.
 *   @param   file       File name from where the trace is logged.
 *   @param   line       Line in the file where trace is logged.
 *   @param   msg        Message to log.
 *   @param   traceObj_p Trace item information struct for the thread.
 *
 *   @return            True at sucess, false otherwise.
 *
 */
extern Boolean
OMCSF_logTrace(OMCSF_groupE  group,
               const char    *file,
               uint16_t      line,
               const char    *msg,
               struct OMCSF_procInfoS *traceObj_p);

/**
 *   OMCSF_logObjTrace()
 *
 *   @param   group       Trace group where the message is logged.
 *   @param   file        File name from where the trace is logged.
 *   @param   line        Line in the file where trace is logged.
 *   @param   msg         Message to log.
 *   @param   traceProc_p Trace item information struct for the thread.
 *   @param   traceObj_p  Trace item information struct for interface
 *                        or object.
 *
 *   @return            True at sucess, false otherwise.
 *
 */
extern Boolean
OMCSF_logObjTrace(OMCSF_groupE  group,
                  const char    *file,
                  uint16_t      line,
                  const char    *msg,
                  struct OMCSF_procInfoS *traceProc_p,
                  struct OMCSF_procInfoS *traceObj_p);

/**
 *   OMCSF_logBusTrace
 *
 *   @param   procInfo_p  Trace item information struct fot the thread.
 *   @param   group       Trace group where the message is logged.
 *   @param   file        File name from where the trace is logged.
 *   @param   line        Line in the file where trace is logged.
 *   @param   msg         Message to log.
 *   @param   data        Bus message data to log.
 *   @param   length      The length of the bus data.
 *
 *   @return            True at sucess, false otherwise.
 *
 */
extern Boolean
OMCSF_logBusTrace(struct OMCSF_procInfoS *procInfo_p,
                  OMCSF_groupE           group,
                  const char             *file,
                  uint16_t               line,
                  const  char            *msg,
                  uint8_t                *data,
                  uint16_t               length);

/**
 *   OMCSF_logObjBusTrace
 *
 *   @param   procInfo_p  Trace item information struct fot the
 *                        interface or object.
 *   @param   objInfo_p   Trace item information struct fot the thread.
 *   @param   group       Trace group where the message is logged.
 *   @param   file        File name from where the trace is logged.
 *   @param   line        Line in the file where trace is logged.
 *   @param   msg         Message to log.
 *   @param   data        Bus message data to log.
 *   @param   length      The length of the bus data.
 *
 *   @return            True at sucess, false otherwise.
 *
 */
extern Boolean
OMCSF_logObjBusTrace(struct OMCSF_procInfoS *procInfo_p,
                     struct OMCSF_procInfoS *objInfo_p,
                     OMCSF_groupE           group,
                     const char             *file,
                     uint16_t               line,
                     const char             *msg,
                     uint8_t                *data,
                     uint16_t               length);

/**
 *   OMCSF_sendObjLogTrace
 *
 *   @param   file        File name from where the trace is logged.
 *   @param   line        Line in the file where trace is logged.
 *   @param   signo       Signal number to log.
 *   @param   recpid      Receiving process of signal to log.
 *   @param   msg         Message to log.
 *   @param   procInfo_p  Trace item information struct fot the thread.
 *   @param   objInfo_p   Trace item information struct fot the
 *                        interface or object.
 *
 *   @return            True at sucess, false otherwise.
 *
 */
extern Boolean
OMCSF_sendObjLogTrace(const char             *file,
                      uint16_t               line,
                      uint32_t               signo,
                      uint32_t               recpid,
                      const char             *msg,
                      struct OMCSF_procInfoS *procInfo_p,
                      struct OMCSF_procInfoS *objInfo_p);

/**
 *   OMCSF_recObjLogTrace
 *
 *   @param   file        File name from where the trace is logged.
 *   @param   line        Line in the file where trace is logged.
 *   @param   signal      Signal.
 *   @param   msg         Message to log.
 *   @param   procInfo_p  Trace item information struct fot the thread.
 *   @param   objInfo_p   Trace item information struct fot the
 *                        interface or object.
 *
 *   @return            True at sucess, false otherwise.
 *
 */
extern Boolean
OMCSF_recObjLogTrace(const char             *file,
                     uint16_t               line,
                     void                   *signal,
                     const char             *msg,
                     struct OMCSF_procInfoS *procInfo_p,
                     struct OMCSF_procInfoS *objInfo_p);

/**
 *   OMCSF_formatStr()
 *
 *   @param   format      String containing format information.
 *   @param   ...         Variable number of parameters according to format.
 *
 *   @return            Formatted string.
 *
 *   NOTE: This is a deprecated function and should not be used
 *         in new development.
 */
extern char *
OMCSF_formatStr(const char *format,
                ...)
   __attribute__ ((deprecated));

/**
 *   Initiates the thread handling for TRI with no LITS dependency.
 *
 *   @param   -
 *
 *   @return TRI_OK                  Success
 *           TRI_ALREADY_INITIALIZED The thread has already been started,
 *                                   by you or someone else. If you get
 *                                   this then your default_trace_name has
 *                                   not been set, but can continue to use
 *                                   the rest of the functionality.
 *           TRI_INTERNAL_ERROR      Failed to start the trace mask handling
 *                                   thread.
 *                                   This best regarded as a fatal error,
 *                                   either pthread_create or itc_locate
 *                                   falied. (Default trace masks will be enabled,
 *                                   so reporting the fault using INFO or
 *                                   TRACE_ERROR is possible.)
 *
 *   This is a mandatory function to be the first TRI function to call.
 */
int tri_init(void);

/* >   Public function: OMCSF_initPriProc()
**
** Description:
**   Initiate trace and error handling information for the current process.
**   (non multicore version)
**
** Pre-conditions:
**   None.
**
** Post-conditions:
**   None.
**
** Returns:
**   N/A
**
*/
extern void
OMCSF_initPriProc(void);

/* ===================================================================== */
#endif    /* OSE implementation */
/* ===================================================================== */


#ifdef __cplusplus
}
#endif

#endif /* CELLO_TE_HANDLERS_H */
