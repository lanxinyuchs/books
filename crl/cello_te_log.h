/**
 *   Trace and Errror Log header file.
 * 
 *   @file
 *   @version @(#) ClearCase ID: 
 *
 *   Copyright (C) 2013 by Ericsson AB. All rights reserved. The
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
 *   Revised : 2010-09-23 Anette Schött
 *   Change  : Updated according to template and added ifdef for Linux.
 * ========================================================================
 */

/* ===================================================================== */
#ifndef LNX    /* OSE implementation */
/* ===================================================================== */

#ifndef CELLO_TE_LOG_H
#define CELLO_TE_LOG_H

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

/**
 * Max len of log data        
 */
#define OMCSF_LOG_MAX_DATA_LEN          512   

#define OMCSF_LOG_DATA_SIZE(SIZE)  \
        (sizeof(struct OMCSF_logDataS) + \
	 sizeof(U8) * ((SIZE) - OMCSF_LOG_MAX_DATA_LEN))

#define OMCSF_LOG_DATA_SIZE2(SIZE2)  \
        (sizeof(struct OMCSF_logData2S) + \
	 sizeof(U8) * ((SIZE2) - (OMCSF_LOG_MAX_DATA_LEN - 8)))

/* ========================================================================
 *   TYPE DEFINITIONS
 * ========================================================================
 */ 

/**
 * The actual data that are stored in the Trace & Error Log. All
 * strings are stored in data delimited with a null character. The
 * position of the message, the process name, the file name and the
 * binary data are all  indicated by its position parameter. If no
 * binary data is stored, the variable binDataLen should be set to zero.
 * Fields absTimeSec and absTimeTick are for T&E internal logging, 
 * user input of timestamp using these two fields is not allowed.
 */
struct OMCSF_logDataS
{
  U32             absTimeSec;  /* The absolute time stamp in seconds        */
  U32             absTimeTick; /* The tick corresponding to absolute time   */
  U16             lineNo;      /* Line number from where log is performed   */
  U16             fileNamePos; /* Name pos of file where log is performed   */
  U16             procNamePos; /* Name pos of process generating log entry  */
  U16             msgPos;      /* Position in data of the logged message    */
  U16             binDataPos;  /* Pos in data where binary data is stored   */
  U16             binDataLen;  /* The length of the binary data in data     */
  OMCSF_groupE    group;       /* Trace group this log entry belongs to     */
  U8              data[OMCSF_LOG_MAX_DATA_LEN];  /* The actual logged data  */
};

/**
 * The data structure has to be used for user input of timestamp.
 * The actual data that are stored in the Trace & Error Log. All
 * strings are stored in data delimited with a null character. The
 * position of the message, the process name, the file name and the
 * binary data are all  indicated by its position parameter. If no
 * binary data is stored, the variable binDataLen should be set to zero.
 * NOTE :
 * 1. Fields absTimeSec and absTimeTick are for T&E internal logging, 
 *    user input of timestamp using these two fields is not allowed.
 * 2. Fields srcAbsTimeSec and srcAbsTimeUSec are provided for user 
 *    input of timestamp
 * 3. Field fileNamePos has to be set to 8 (fileNamePos = 8) in order
 *    to use "source" timestamps.
 *    
 */
struct OMCSF_logData2S
{
  U32             absTimeSec;  /* Absolute time stamp in seconds            */ 
  U32             absTimeTick; /* Tick corresponding to absolute time       */
  U16             lineNo;      /* Line number from where log is performed   */
  U16             fileNamePos; /* Name pos of file where log is performed   */
  U16             procNamePos; /* Name pos of process generating log entry  */
  U16             msgPos;      /* Position in data of the logged message    */
  U16             binDataPos;  /* Pos in data where binary data is stored   */
  U16             binDataLen;  /* The length of the binary data in data     */
  OMCSF_groupE    group;       /* Trace group this log entry belongs to     */
  U32             srcAbsTimeSec; /* Source time stamp,  user input allowed  */
  U32             srcAbsTimeUSec; /* Source microsecond, user input allowed */
  U8              data[OMCSF_LOG_MAX_DATA_LEN - 8];  /* Actual logged data  */
};

/* ========================================================================
 *   DATA DECLARATIONS
 * ========================================================================
 */ 


#ifdef __cplusplus
}
#endif

#endif   /* ifndef CELLO_TE_LOG_H */

/* ===================================================================== */
#endif    /* OSE implementation */
/* ===================================================================== */
