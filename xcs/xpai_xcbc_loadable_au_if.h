/******************************************************************************
 *
 *      COPYRIGHT (C)                 Ericsson Radio Systems AB, Sweden
 *
 *      The copyright to the computer program(s) herein is the property
 *      of Ericsson Radio Systems AB.
 *
 *      The program(s) may be used and/or copied only with the written
 *      permission from Ericsson Radio Systems AB or in accordance with
 *      the terms and conditions stipulated in the agreement/contract
 *      under which the program(s) have been supplied.
 *
 *****************************************************************************/

/******************************************************************************
 *
 * Product name:
 *      XPAI/XCBC
 *
 * File:
 *      xpai_xcbc_loadable_au_if.h
 *
 * Author:
 *      Sven Löfgren (qlofsve)
 *
 * Description:
 *      This file defines XPAI XCBC external interface functions.
 *
 * Reviewed:
 *      2006-xx-xx ...
 *
 * Revision history:
 *      2006-03-20 Sven Löfgren (qlofsve)
 *              Created for Combined Interface (Wanja).
 *
 *      2008-06-30 Sven Löfgren (xedsven)
 *              Max lm block size changed from 0x1fff to 0x1000.
 *
 *      2014-04-24, WENXIN YAO (ewenyao)
 *              CR WCDMA00018905, Support XPAI_LoadFileDelete(NULL, <pid>)
 *              force one slot in background/garbage deleting to normal delete
 *
 *****************************************************************************/

#ifndef XPAI_XCBC_LOADABLE_AU_IF_H
#define XPAI_XCBC_LOADABLE_AU_IF_H

/*----------------------------  Include files  ------------------------------*/

#include <ose.h>
#include <osetypes.h>

/*----------------------------  Constants  ----------------------------------*/

/* General load file values. 
 *   XPAI_LOAD_FILE_MAX_LM_BLOCKSIZE
 *       The maximum blocksize into which the load module container (LMC) 
 *       may be chopped (8 kByte).
 *
 *   XPAI_LOAD_FILE_MAX_PID_STRINGLENGTH
 *       Maximum length of Cello lmid string: <product number>_<revision>, 
 *       1553-CSX 101 09/1 Userguide for Cello 2.5.
 *
 *       The lmid is input to the xlf_gen program (option -l) when
 *       generating a load module container (LMC),
 *       1551-LXA 119 181/1 XLF_GEN - XP Load File Generator.
 *
 *       The same <product number> and <revision> is located in the 
 *       generated load module container (LMC) IBOOT header, 
 *       1/1551-CEH 101 90/1 XLF - XP Load File Format.
 *
 *       <product number>:            max 23 char
 *       <revision>:                  max  9 char
 *       <product number>_<revision>: max 32 char
 *
 *       The maximum length includes null termination.
 */
#define XPAI_LOAD_FILE_MAX_LM_BLOCKSIZE      0x1000
#define XPAI_LOAD_FILE_MAX_PID_STRINGLENGTH  33


/* Return codes for XPAI_LoadFileInit.
 * Success:
 *   XPAI_LOAD_FILE_INIT_OK
 * Error codes:
 *   XPAI_LOAD_FILE_INIT_NOK_WRONG_PARAM        Wrong parameter.
 *   XPAI_LOAD_FILE_INIT_NOK_SERVER             Server not found.
 *   XPAI_LOAD_FILE_INIT_NOK_WRONG_STATE        Load procedure is going on.
 *   XPAI_LOAD_FILE_INIT_NOK_WRONG_CONFIG_DATA  The load module container
 *                                              (LMC) is already loaded in
 *                                              flash.
 *   XPAI_LOAD_FILE_INIT_NOK_RESOURCE_SHORTAGE  No room available in flash.
 *   XPAI_LOAD_FILE_INIT_NOK_OTHER              Other error.
 */
#define XPAI_LOAD_FILE_INIT_OK                     0
#define XPAI_LOAD_FILE_INIT_NOK_WRONG_PARAM        1
#define XPAI_LOAD_FILE_INIT_NOK_SERVER             2
#define XPAI_LOAD_FILE_INIT_NOK_WRONG_STATE        3
#define XPAI_LOAD_FILE_INIT_NOK_WRONG_CONFIG_DATA  4
#define XPAI_LOAD_FILE_INIT_NOK_RESOURCE_SHORTAGE  5
#define XPAI_LOAD_FILE_INIT_NOK_OTHER              6


/* Return codes for XPAI_LoadFileData.
 * Success:
 *   XPAI_LOAD_FILE_DATA_OK
 * Error codes:
 *   XPAI_LOAD_FILE_DATA_NOK_WRONG_PARAM  Wrong parameter.
 *   XPAI_LOAD_FILE_DATA_NOK_SERVER       Server not found.
 *   XPAI_LOAD_FILE_DATA_NOK_WRONG_STATE  Load procedure not going on.
 *   XPAI_LOAD_FILE_DATA_NOK_OTHER        Other error.
 *   XPAI_LOAD_FILE_DATA_NOK_WRONG_SEQ_NR   Wrong sequence number. 
 */
#define XPAI_LOAD_FILE_DATA_OK               0
#define XPAI_LOAD_FILE_DATA_NOK_WRONG_PARAM  1
#define XPAI_LOAD_FILE_DATA_NOK_SERVER       2
#define XPAI_LOAD_FILE_DATA_NOK_WRONG_STATE  3
#define XPAI_LOAD_FILE_DATA_NOK_OTHER        4
#define XPAI_LOAD_FILE_DATA_NOK_WRONG_SEQ_NR 5


/* Return codes for XPAI_LoadFileDataGetSeq.
 * Success:
 *   XPAI_LOAD_FILE_DATA_GET_SEQ_OK
 * Error codes:
 *   XPAI_LOAD_FILE_DATA_GET_SEQ_NOK_SERVER       Server not found.
 *   XPAI_LOAD_FILE_DATA_GET_SEQ_NOK_WRONG_STATE  Load procedure not going on.
 *   XPAI_LOAD_FILE_DATA_GET_SEQ_NOK_OTHER        Other error.
 */
#define XPAI_LOAD_FILE_DATA_GET_SEQ_OK               0
#define XPAI_LOAD_FILE_DATA_GET_SEQ_NOK_SERVER       1
#define XPAI_LOAD_FILE_DATA_GET_SEQ_NOK_WRONG_STATE  2
#define XPAI_LOAD_FILE_DATA_GET_SEQ_NOK_OTHER        3

/* Return codes for XPAI_LoadFileEnd.
 * Success:
 *   XPAI_LOAD_FILE_END_OK
 * Error codes:
 *   XPAI_LOAD_FILE_END_NOK_SERVER       Server not found.
 *   XPAI_LOAD_FILE_END_NOK_WRONG_STATE  Load procedure not going on.
 *   XPAI_LOAD_FILE_END_NOK_OTHER        Other error.
 */
#define XPAI_LOAD_FILE_END_OK               0
#define XPAI_LOAD_FILE_END_NOK_SERVER       1
#define XPAI_LOAD_FILE_END_NOK_WRONG_STATE  2
#define XPAI_LOAD_FILE_END_NOK_OTHER        3


/* Parameter values for loadResult in XPAI_LoadFileEnd.
 * Success:
 *   XPAI_LOAD_FILE_END_RESULT_LOAD_COMPLETED  
 *       Load module container (LMC) is available in flash.
 * Error codes:
 *   XPAI_LOAD_FILE_END_RESULT_LOAD_ABORTED
 *       Error when receiving load module container (LMC) or aborted
 *       load procedure. The received data is erased from the flash.
 */
#define XPAI_LOAD_FILE_END_RESULT_LOAD_COMPLETED  0
#define XPAI_LOAD_FILE_END_RESULT_LOAD_ABORTED    1


/* Return codes for XPAI_LoadFileDelete.
 * Success:
 *   XPAI_LOAD_FILE_DELETE_OK
 * Error codes:
 *   XPAI_LOAD_FILE_DELETE_NOK_WRONG_PARAM                 Wrong parameter.
 *   XPAI_LOAD_FILE_DELETE_NOK_SERVER                      Server not found.
 *   XPAI_LOAD_FILE_DELETE_NOK_WRONG_STATE                 Load procedure is going on.
 *   XPAI_LOAD_FILE_DELETE_NOK_UNEXPECTED_PARAMETER_VALUE  The load module container
 *                                                         (LMC) is protected
 *                                                         (deletion is not allowed).
 *   XPAI_LOAD_FILE_DELETE_NOK_OTHER                       Other error.
 */
#define XPAI_LOAD_FILE_DELETE_OK                              0
#define XPAI_LOAD_FILE_DELETE_NOK_WRONG_PARAM                 1
#define XPAI_LOAD_FILE_DELETE_NOK_SERVER                      2
#define XPAI_LOAD_FILE_DELETE_NOK_WRONG_STATE                 3
#define XPAI_LOAD_FILE_DELETE_NOK_UNEXPECTED_PARAMETER_VALUE  4
#define XPAI_LOAD_FILE_DELETE_NOK_OTHER                       5


/* Parameter values for result in signal XPAI_LOAD_FILE_DELETE_IND
 * Success:
 *   XPAI_LOAD_FILE_DELETE_RESULT_OK
 * Error codes: 
 *   XPAI_LOAD_FILE_DELETE_RESULT_UNEXPECTED_PARAMETER_VALUE
 *       The load module container (LMC) is protected (deletion is not allowed).
 *   XPAI_LOAD_FILE_DELETE_RESULT_OTHER_ERROR
 *       XPP has no access to the flash or other error.
 */
#define XPAI_LOAD_FILE_DELETE_RESULT_OK                          0
#define XPAI_LOAD_FILE_DELETE_RESULT_UNEXPECTED_PARAMETER_VALUE  1
#define XPAI_LOAD_FILE_DELETE_RESULT_OTHER_ERROR                 2

/* Parameter values for result in signal XPAI_ReadLoadFileData
 * Success:
 *   XPAI_READ_LOAD_FILE_DATA_OK
 * Error codes: 
 *   XPAI_READ_LOAD_FILE_DATA_NOK_WRONG_PARAM
 *   XPAI_READ_LOAD_FILE_DATA_NOK_SERVER
 *   XPAI_READ_LOAD_FILE_DATA_NOK_OTHER        Other error .
 *
 */
#define XPAI_READ_LOAD_FILE_DATA_OK               0
#define XPAI_READ_LOAD_FILE_DATA_NOK_WRONG_PARAM  1
#define XPAI_READ_LOAD_FILE_DATA_NOK_SERVER       2
#define XPAI_READ_LOAD_FILE_DATA_NOK_OTHER       3

/* XPSIM stub XPAI functions. */
#ifdef SOFT
#define XPAI_FNO_LOAD_FILE_INIT   ((U32)XPAI_LoadFileInit)
#define XPAI_FNO_LOAD_FILE_DATA   ((U32)XPAI_LoadFileData)
#define XPAI_FNO_LOAD_FILE_END    ((U32)XPAI_LoadFileEnd)
#define XPAI_FNO_LOAD_FILE_DELETE ((U32)XPAI_LoadFileDelete)
#endif


/* Signal numbers.
 * 0x0100E000 = XCBC_SIGBASE. Redefined here to avoid dependency to xp.h.
 * Offset ranges within the XCBC offset range (0x00 - 0xFF) reserved for this 
 * interface: 
 *    0xA0 - 0xAF
 */
#define XPAI_LOAD_FILE_DATA_IND    (0x0100E000 + 0xA0) /* !-SIGNO( struct XPAI_LoadFileDataIndS )-! */
#define XPAI_LOAD_FILE_DELETE_IND  (0x0100E000 + 0xA1) /* !-SIGNO( struct XPAI_LoadFileDeleteIndS )-! */


/*----------------------------  Macros  -------------------------------------*/

/*----------------------------  Structs and Typedefs  -----------------------*/

/******************************************************************************
 * Signal:
 *      XPAI_LOAD_FILE_DATA_IND
 *
 * Parameters:
 *      lmSeqNr  Sequence number of the call to XPAI_LoadFileData.
 *      
 * Description:
 *      This signal acknowledges the call to the function XPAI_LoadFileData
 *      and is sent to the process indicated with the pid parameter to the 
 *      function XPAI_LoadFileInit.
 *
 *****************************************************************************/
struct XPAI_LoadFileDataIndS
{
  SIGSELECT sigNo;
  U16       lmSeqNr;
};


/******************************************************************************
 * Signal:
 *      XPAI_LOAD_FILE_DELETE_IND
 *
 * Parameters:
 *      result  The result of the call to the function XPAI_LoadFileDelete.
 *              Value: XPAI_LOAD_FILE_DELETE_RESULT_OK
 *                     XPAI_LOAD_FILE_DELETE_RESULT_UNEXPECTED_PARAMETER_VALUE
 *                     XPAI_LOAD_FILE_DELETE_RESULT_OTHER_ERROR
 *      
 * Description:
 *      This signal acknowledges the call to the function XPAI_LoadFileDelete
 *      and is sent to the process indicated with the pid parameter to the
 *      function XPAI_LoadFileDelete.
 *
 *****************************************************************************/
struct XPAI_LoadFileDeleteIndS
{
  SIGSELECT sigNo;
  U32       result;
};


/*----------------------------  Declaration of Global Variables  ------------*/

/*----------------------------  Declaration of Global Functions  ------------*/

/******************************************************************************
 *
 * Global function:
 *      XPAI_LoadFileInit
 *
 * Parameters:
 *      loadModule    Null terminated string with the product ID of the 
 *                    load module container (LMC) to be loaded in flash.
 *                    Format: Cello lmid: <product number>_<revision>, 
 *                            1553-CSX 101 09/1 Userguide for Cello 2.5
 *                    Max string length: XPAI_LOAD_FILE_MAX_PID_STRINGLENGTH.
 *      maxBlockSize  Pointer to return value with maximum allowed size in
 *                    bytes into which the load module container (LMC) may be
 *                    chopped.
 *                    Value: <= XPAI_LOAD_FILE_MAX_LM_BLOCKSIZE.
 *      pid           Pid (process id) for process that shall receive
 *                    XPAI_LOAD_FILE_DATA_IND.
 *      
 * Return value:
 *      XPAI_LOAD_FILE_INIT_OK
 *      XPAI_LOAD_FILE_INIT_NOK_WRONG_PARAM
 *      XPAI_LOAD_FILE_INIT_NOK_SERVER
 *      XPAI_LOAD_FILE_INIT_NOK_WRONG_STATE
 *      XPAI_LOAD_FILE_INIT_NOK_WRONG_CONFIG_DATA
 *      XPAI_LOAD_FILE_INIT_NOK_RESOURCE_SHORTAGE
 *      XPAI_LOAD_FILE_INIT_NOK_OTHER
 *
 * Description:
 *      This function initiates load of a load module container (LMC) into the
 *      flash memory.
 *
 *      The function checks that the load file is not already loaded in flash,
 *      that there is enough room in the flash and opens the slot for write.
 *
 *****************************************************************************/
extern U32 XPAI_LoadFileInit(char *loadModule,   /* !- FUNC -! */
                             U32  *maxBlockSize, 
                             U32   pid);


/******************************************************************************
 *
 * Global function:
 *      XPAI_LoadFileData
 *
 * Parameters:
 *      lmBlockSize  Number of bytes present in lmBlock.
 *      lmSeqNr      Sequence number of the XPAI_LoadFileData.
 *      lmBlock      Data to loaded.
 *      
 * Return value:
 *      XPAI_LOAD_FILE_DATA_OK
 *      XPAI_LOAD_FILE_DATA_NOK_WRONG_PARAM
 *      XPAI_LOAD_FILE_DATA_NOK_SERVER
 *      XPAI_LOAD_FILE_DATA_NOK_WRONG_STATE
 *      XPAI_LOAD_FILE_DATA_NOK_OTHER
 *      XPAI_LOAD_FILE_DATA_NOK_WRONG_SEQ_NR
 *
 * Description:
 *      This function loads a block of data into flash.
 *
 *      The sequence number shall be incremented with 1 for each call starting
 *      from 0. There may be two unacknowledged calls outstanding.
 *
 *      The function is acknowledged with the signal XPAI_LOAD_FILE_DATA_IND
 *      which is sent to the process indicated with the pid parameter to the
 *      function XPAI_LoadFileInit.
 *
 *****************************************************************************/
extern U32 XPAI_LoadFileData(U32   lmBlockSize, /* !- FUNC -! */
                             U16   lmSeqNr, 
                             char *lmBlock);


/******************************************************************************
 *
 * Global function:
 *      XPAI_LoadFileEnd
 *
 * Parameters:
 *      loadResult  Pointer to return value indicating load result.
 *                  Value: XPAI_LOAD_FILE_END_RESULT_LOAD_COMPLETED
 *                         XPAI_LOAD_FILE_END_RESULT_LOAD_ABORTED
 *                  Valid if XPAI_LOAD_FILE_END_OK.
 *      
 * Return value:
 *      XPAI_LOAD_FILE_END_OK
 *      XPAI_LOAD_FILE_END_NOK_SERVER
 *      XPAI_LOAD_FILE_END_NOK_WRONG_STATE
 *      XPAI_LOAD_FILE_END_NOK_OTHER
 *
 * Description:
 *      This function ends the loading procedure, closes the the slot and 
 *      checks the checksum. 
 *
 *      This function is also used to abort a loading that is going on.
 *
 *      The loadResult is only valid if the return value is
 *      XPAI_LOAD_FILE_END_OK and indicates if the load module container
 *      (LMC) is available in flash after successful completion of the 
 *      load procedure. When problems occurred during the transmission of the
 *      data blocks or when the load procedure was aborted the loadResult will
 *      be XPAI_LOAD_FILE_END_RESULT_LOAD_ABORTED.
 *
 *****************************************************************************/
extern U32 XPAI_LoadFileEnd(U16 *loadResult); /* !- FUNC -! */


/******************************************************************************
 *
 * Global function:
 *      XPAI_LoadFileDelete
 *
 * Parameters:
 *      loadModule  Null terminated string with the product ID 
 *                  of the load module container (LMC) to be deleted from flash.
 *                  Format: Cello lmid: <product number>_<revision>, 
 *                          1553-CSX 101 09/1 Userguide for Cello 2.5.
 *                  NULL or points to an empty string indicates that the
 *                  first slot in XPAI_XMH_STATE_ERPROG/XPAI_XMH_STATE_ERPROG_BG
 *                  state will be forced to normal delete.
 *                  Max string length: XPAI_LOAD_FILE_MAX_PID_STRINGLENGTH.
 *      pid         Pid (process id) for process that shall receive
 *                  XPAI_LOAD_FILE_DELETE_IND.
 *      
 * Return value:
 *      XPAI_LOAD_FILE_DELETE_OK
 *      XPAI_LOAD_FILE_DELETE_NOK_WRONG_PARAM
 *      XPAI_LOAD_FILE_DELETE_NOK_SERVER
 *      XPAI_LOAD_FILE_DELETE_NOK_WRONG_STATE
 *      XPAI_LOAD_FILE_DELETE_NOK_UNEXPECTED_PARAMETER_VALUE
 *      XPAI_LOAD_FILE_DELETE_NOK_OTHER
 *
 * Description:
 *      This function deletes the specified load module container (LMC) from
 *      flash. If loadModule parameter is NULL or points to an empty string
 *      indicates that the first slot in XPAI_XMH_STATE_ERPROG/XPAI_XMH_STATE_ERPROG_BG
 *      state will be forced from background/garbage deleting to normal delete.
 *
 *      The function is acknowledged with the signal XPAI_LOAD_FILE_DELETE_IND
 *      which is sent to the process indicated with the pid parameter.
 *
 *****************************************************************************/
extern U32 XPAI_LoadFileDelete(char *loadModule, U32 pid); /* !- FUNC -! */


/******************************************************************************
 *
 * Global function:
 *      XPAI_LoadFileDataGetSeq
 *
 * Parameters:
 *      seqNr  Pointer to return value indicating sequence number.
 *      
 * Return value:
 *         XPAI_LOAD_FILE_DATA_GET_SEQ_OK               0
 *         XPAI_LOAD_FILE_DATA_GET_SEQ_NOK_SERVER       1
 *         XPAI_LOAD_FILE_DATA_GET_SEQ_NOK_WRONG_STATE  2
 *         XPAI_LOAD_FILE_DATA_GET_SEQ_NOK_OTHER        3
 *
 * Description:
 *      This function returns the sequence number during the loading procedure.
 *      The seqNr is valid only if the return value is XPAI_LOAD_FILE_DATA_GET_SEQ_OK.
 *
 *****************************************************************************/
extern U32 XPAI_LoadFileDataGetSeq(U32 *seqNr); /* !- FUNC -! */

#endif /* XPAI_XCBC_LOADABLE_AU_IF_H */

