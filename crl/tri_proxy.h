/**
 *   Header file for TRI proxy functionality.
 *
 *   ******* THIS FILE IS TRI INTERNAL.***************
 *   ******* TRI CLIENTS SHALL NOT INCLUDE THIS HEADER FILE.******
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
 *   Revised : 2013-04-11 Anette Schött
 *   Change  : First version.
 *
 *   Revised : 2013-08-16 Anette Schött
 *   Change  : Minor corrections.
 *
 *   Revised : 2014-01-24 Naresh Angala
 *   Change  : WCDMA00017891 : CR to improve the robustness of STR
 *             macro so that compilation errors or warnings are seen
 *             when STR is called with wrong number of arguments.
 *
 *   Revised : 2014-02-27 Anette Schött
 *   Change  : Add deRegisterIfObj as a public function. Changed name
 *             from deregisterIfObj to deRegisterIfObj.
 *
 *   Revised : 2015-01-26 Anette Schött
 *   Change  : Add support for both traceObjInfo ## OBJNAME and
 *             OMCSF_traceObjInfo ## OBJNAME ## _p reference name as
 *             arguments in registerIfObj.
 *
 *   Revised : 2015-11-02 Anette Schött
 *   Change  : Change from union SIGNAL to union itc_msg in 
 *             triRaiseUnexpectedSig declaration.
 * ========================================================================
 */

#ifndef __TRI_PROXY_H
#define __TRI_PROXY_H
 
#ifdef __cplusplus
extern "C" {
#endif

/* ========================================================================
 *   INCLUDE FILES
 * ========================================================================
 */

#include <stdint.h>
#include "cello_te_handlers.h"

/* ========================================================================
 *   DEFINITIONS
 * ========================================================================
 */ 

/* ========================================================================
 *   TYPE DEFINITIONS
 * ========================================================================
 */ 

#define TRI_PROXY_MAILBOX_NAME "triProxy_mbox"

/* Use this instead of __FILE__ to have a filename only, no path */
#define __SHORT_FILE__ short_fname(__FILE__)

/* ========================================================================
 *   DATA DECLARATIONS
 * ========================================================================
 */ 

/* ========================================================================
 *   VARIABLES
 * ========================================================================
 */ 

/* ========================================================================
 *   FUNCTIONS
 * ========================================================================
 */

/* ===================================================================== */
/** 
 *   Handles registration of the trace item thread.
 * 
 *   @param procName   Name of the thread to be registered
 * 
 *   @return           -
 *
 *   @par Globals:     
 *                     procInfo, defaultItemInfo
 */
/* ===================================================================== */
void 
initPriProc(const char *procName);


/* ===================================================================== */
/** 
 *   Handles deregistration of the trace item interface and object.
 * 
 *   @param ref        Reference to interface and object information struct
 * 
 *   @return           -
 *
 *   @par Globals:     
 *                     defaultItemInfo
 */
/* ===================================================================== */
void 
deRegisterIfObj(struct ItemInfo **ref);

/* ===================================================================== */
/** 
 *   Handles registration of the trace item interface and object.
 *
 *   @param itemName   Name of the interface or object  to be registered
 *
 *   @param ref        Reference or references to interface and object
 *                     information struct.
 *                     If set to NULL indicates that both
 *                     traceObjInfo ## OBJNAME and
 *                     OMCSF_traceObjInfo ## OBJNAME ## _p references
 *                     are provided as function arguments.
 *                     If not set to NULL, only traceObjInfo ## OBJNAME
 *                     reference is provided as argument.
 *
 *   @return           -
 *
 *   @par Globals:     
 *                     defaultItemInfo
 */
/* ===================================================================== */
void 
registerIfObj(const char *itemName,
              struct ItemInfo **ref,
               ...);

/* ===================================================================== */
/** 
 *   This function builds a formatted string and stores in the
 *   process information structure for the current process.
 * 
  *   @param format    String containing format information
  *
 *   @param ...        Variable number of parameters according to format
 * 
 *   @return           A pointer to the formatted string.
 *
 *   @par Globals:     
 *                     procInfo
 */
/* ===================================================================== */
#ifdef TRI_STR_CHECK
char *
formatStr(const char *format, ...) __attribute__((format(printf,1,2)));
#else
char *
formatStr(const char *format, ...);
#endif


/* ===================================================================== */
/** 
 *   This function traces data for a thread trace item.
 * 
 *   @param group      Group mask for this function call
 *   @param file       Name of file where this function was called
 *   @param line       Line in file where this function was called
 *   @param msg        String to trace
 *   @param procInfo   Pointer to thread specific data
 * 
 *   @return           Always returns True.
 *
 *   @par Globals:     
 *                     --
 */
/* ===================================================================== */
Boolean 
lttngTriTrace(int group, 
              const char *file, 
              uint16_t line, 
              const char *msg, 
              struct ItemInfo *procInfo);


/* ===================================================================== */
/** 
 *   This function traces data for SEND_SIG requested by a thread 
 *   trace item.
 * 
 *   @param file       Name of file where this function was called
 *   @param line       Line in file where this function was called
 *   @param signo      Signal number for the sent signal
 *   @param recpid     Reciever pid for the sent signal
 *   @param msg        String to trace
 *   @param procInfo   Pointer to thread specific data
 * 
 *   @return           Always returns True.
 *
 *   @par Globals:     
 *                     --
 */
/* ===================================================================== */
Boolean
lttngTriSendSigTrace(const char *file, 
                     uint16_t line, 
                     uint32_t signo, 
                     uint32_t recpid, 
                     const char *msg, 
                     struct ItemInfo *procInfo);


/* ===================================================================== */
/** 
 *   This function traces data for REC_SIG requested by a thread 
 *   trace item.
 * 
 *   @param file       Name of file where this function was called
 *   @param line       Line in file where this function was called
 *   @param sig        Signal pointer for the received signal
 *   @param msg        String to trace
 *   @param procInfo   Pointer to thread specific data
 *
 *   @return           Always returns True.
 *
 *   @par Globals:     
 *                     --
 */
/* ===================================================================== */
Boolean
lttngTriRecSigTrace(const char *file, 
                    uint16_t line, 
                    uint32_t *sig, 
                    const char *msg, 
                    struct ItemInfo *procInfo);


/* ===================================================================== */
/** 
 *   This function traces data for BUS_SEND and BUS_RECEIVE requested 
 *   by a thread trace item.
 * 
 *   @param procInfo   Pointer to thread specific data
 *   @param group      Group mask for this function call
 *   @param file       Name of file where this function was called
 *   @param line       Line in file where this function was called
 *   @param msg        String to trace
 *   @param data       Data to trace
 *   @param len        Length of data
 * 
 *   @return           Always returns True.
 *
 *   @par Globals:     
 *                     --
 */
/* ===================================================================== */
Boolean
lttngTriBusTrace(struct ItemInfo *procInfo, 
                 uint32_t group, 
                 const char *file, 
                 uint16_t line, 
                 const char *msg, 
                 uint8_t *data, 
                 uint16_t len);



/* ===================================================================== */
/** 
 *   This function traces data for BUS_RECEIVE requested by a thread
 *   trace item.
 * 
 *   @param procInfo   Pointer to thread specific data
 *   @param group      Group mask for this function call
 *   @param file       Name of file where this function was called
 *   @param line       Line in file where this function was called
 *   @param msg        String to trace
 *   @param data       Data to trace
 *   @param len        Length of data
 * 
 *   @return           Always returns True.
 *
 *   @par Globals:     
 *                     --
 */
/* ===================================================================== */
Boolean
lttngTriBusRecTrace(struct ItemInfo *procInfo, 
                    uint32_t group, 
                    const char *file, 
                    uint16_t line, 
                    const char *msg, 
                    uint8_t *data, 
                    uint16_t len);


/* ===================================================================== */
/** 
 *   This function traces data for user time stamp trace requested by
 *   a thread trace item.
 * 
 *   @param group      Group mask for this function call
 *   @param sec        User provided time stamp in seconds
 *   @param usec       User provided time stamp in microseconds
 *   @param file       Name of file where this function was called
 *   @param line       Line in file where this function was called
 *   @param procName   Name of thread calling this function
 *   @param msg        String to trace
 *   @param data       Data to trace
 *   @param len        Length of data
 *   @param procInfo   Pointer to thread specific data
 * 
 *   @return           Always returns True.
 *
 *   @par Globals:     
 *                     --
 */
/* ===================================================================== */
Boolean
lttngTriUserTrace(uint32_t group, 
                  uint32_t sec, 
                  uint32_t usec,
                  const char *file, 
                  uint16_t line, 
                  const char *procName,  
                  const char *msg, 
                  uint8_t *data, 
                  uint16_t len, 
                  struct ItemInfo *procInfo);


/* ===================================================================== */
/** 
 *   This function raises the specified status in the statusMask
 *   and generates a log entry.
 * 
 *   @param status     Error status to raise
 *   @param file       Name of the file where the error occured
 *   @param line       Line in the file on which the error occured
 *   @param msg        A message to log
 * 
 *   @return           -
 *
 *   @par Globals:     
 *                     --
 */
/* ===================================================================== */
void 
triRaiseStatus(uint32_t      status,
               const char    *file,
               uint16_t      line,
               const char    *msg);

/* ===================================================================== */
/** 
 *   This function raises the specified status in the statusMask
 *   and generates a log entry consisting of the signal number
 *   and the sender of the specified signal.
 * 
 *   @param status     Error status to raise
 *
 *   @param file       Name of the file where the error occured
 *
 *   @param line       Line in the file on which the error occured
 *
 *   @param sig        The unexpectedly received signal
 * 
 *   @return           -
 *
 *   @par Globals:     
 *                     --
 */
/* ===================================================================== */
void
triRaiseUnexpectedSig(uint32_t      status,
                      const char    *file,
                      uint16_t      line,
                      union itc_msg *sig);


/* ===================================================================== */
/** 
 *   This function traces data for a interface or object trace item.
 * 
 *   @param group      Group mask for this function call
 *   @param file       Name of file where this function was called
 *   @param line       Line in file where this function was called
 *   @param msg        String to trace
 *   @param procInfo   Pointer to thread specific data
 *   @param ref        Pointer to interface or object specific data
 * 
 *   @return           Always returns True.
 *
 *   @par Globals:     
 *                     --
 */
/* ===================================================================== */
Boolean
lttngTriIfObjTrace(uint32_t group, 
                   const char *file, 
                   uint16_t line, 
                   const char *msg, 
                   struct ItemInfo *procInfo,
                   struct ItemInfo *ref);


/* ===================================================================== */
/** 
 *   This function traces data for SEND_SIG requested by a interface 
 *   or object trace item.
 * 
 *   @param file       Name of file where this function was called
 *   @param line       Line in file where this function was called
 *   @param signo      Signal number for the sent signal
 *   @param recpid     Reciever pid for the sent signal
 *   @param msg        String to trace
 *   @param procInfo   Pointer to thread specific data
 *   @param ref        Pointer to interface or object specific data
 * 
 *   @return           Always returns True.
 *
 *   @par Globals:     
 *                     --
 */
/* ===================================================================== */
Boolean 
lttngTriIfObjSendSigTrace(const char *file, 
                          uint16_t line, 
                          uint32_t signo, 
                          uint32_t recpid, 
                          const char *msg, 
                          struct ItemInfo *procInfo, 
                          struct ItemInfo *ref);


/* ===================================================================== */
/** 
 *   This function traces data for REC_SIG requested by a interface 
 *   or object trace item.
 * 
 *   @param file       Name of file where this function was called
 *   @param line       Line in file where this function was called
 *   @param sig        Signal pointer for the received signal
 *   @param msg        String to trace
 *   @param procInfo   Pointer to thread specific data
 *   @param ref        Pointer to interface or object specific data
 * 
 *   @return           Always returns True.
 *
 *   @par Globals:     
 *                     --
 */
/* ===================================================================== */
Boolean
lttngTriIfObjRecSigTrace(const char *file, 
                         uint16_t line, 
                         uint32_t *sig, 
                         const char *msg, 
                         struct ItemInfo *procInfo, 
                         struct ItemInfo *ref);


/* ===================================================================== */
/** 
 *   This function traces data for BUS_SEND and BUS_RECEIVE requested 
 *   by an interface or object trace item.
 * 
 *   @param procInfo   Pointer to thread specific data
 *   @param ref        Pointer to interface or object specific data
 *   @param group      Group mask for this function call
 *   @param file       Name of file where this function was called
 *   @param line       Line in file where this function was called
 *   @param msg        String to trace
 *   @param data       Data to trace
 *   @param len        Length of data
 * 
 *   @return           Always returns True.
 *
 *   @par Globals:     
 *                     --
 */
/* ===================================================================== */
Boolean
lttngTriIfObjBusTrace(struct ItemInfo *procInfo, 
                      struct ItemInfo *ref,
                      uint32_t group, 
                      const char *file, 
                      uint16_t line, 
                      const char *msg, 
                      uint8_t *data, 
                      uint16_t len);


/* ===================================================================== */
/** 
 *   This function traces data for user time stamp trace requested by
 *   an interface or object trace item.
 * 
 *   @param group      Group mask for this function call
 *   @param sec        User provided time stamp in seconds
 *   @param usec       User provided time stamp in microseconds
 *   @param file       Name of file where this function was called
 *   @param line       Line in file where this function was called
 *   @param procName   Name of thread calling this function
 *   @param msg        String to trace
 *   @param data       Data to trace
 *   @param len        Length of data
 *   @param procInfo   Pointer to thread specific data
 *   @param ref        Pointer to interface or object specific data
 * 
 *   @return           Always returns True.
 *
 *   @par Globals:     
 *                     --
 */
/* ===================================================================== */
Boolean
lttngTriIfObjUserTrace(uint32_t group, 
                       uint32_t sec, 
                       uint32_t usec,
                       const char *file, 
                       uint16_t line, 
                       const char *procName,  
                       const char *msg, 
                       uint8_t *data, 
                       uint16_t len, 
                       struct ItemInfo *procInfo, 
                       struct ItemInfo *ref);


/* ===================================================================== */
/** 
 *   Shortens path to a file to just a filename
 * 
 *   @param fname        Pointer to absolute path
 * 
 *   @return           Pointer to a string(filename)
 *
 *   @par Globals:     
 *                     --
 */
/* ===================================================================== */
extern const char *short_fname(const char *fname);

#ifdef __cplusplus
}
#endif

#endif /* TRI_PROXY_H */
