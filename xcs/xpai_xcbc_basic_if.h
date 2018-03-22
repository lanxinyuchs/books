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
 *      xpai_xcbc_basic_if.h
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
 *      2008-10-27 Viveka SJöblom (xedvisj)
 *              Implemented CR: WRNae08468 (lte_r1),
 *              i.e. add func getBoardInfo2
 *
 *      2008-11-24 Viveka SJöblom (xedvisj)
 *              Updated after review CR: WRNae08468 (lte_r1)
 *
 *      2009-04-17 Sven Löfgren (xedsven)
 *              WP216: XPAI_LoadFileDeleteRestartBoard added.
 *
 *      2010-03-04, Sven Löfgren (xedsven)
 *              CR WRNae70403 Restart board must include trace info from appl.
 *
 *****************************************************************************/

#ifndef XPAI_XCBC_BASIC_IF_H
#define XPAI_XCBC_BASIC_IF_H

/*----------------------------  Include files  ------------------------------*/

#include <ose.h>
#include <osetypes.h>

#ifdef __cplusplus
extern "C" {
#endif

/*----------------------------  Constants  ----------------------------------*/

/* Return codes for XPAI_GetBoardInfo.
 * Success:
 *   XPAI_BOARD_INFO_OK
 * Error codes:
 *   XPAI_BOARD_INFO_NOK_SERVER  Server not found.
 *   XPAI_BOARD_INFO_NOK_OTHER   Other error.
 */
#define XPAI_BOARD_INFO_OK          0
#define XPAI_BOARD_INFO_NOK_SERVER  1
#define XPAI_BOARD_INFO_NOK_OTHER   2


/* Parameter values for parameters in XPAI_GetBoardInfo. */
#define XPAI_BOARD_INFO_NO   0
#define XPAI_BOARD_INFO_YES  1


/* Return codes for XPAI_PublishCBCI_AU_Server.
 * Success:
 *   XPAI_PUBLISH_CBCI_OK
 * Error codes:
 *   XPAI_PUBLISH_CBCI_NOK_SERVER  Server not found.
 *   XPAI_PUBLISH_CBCI_NOK_OTHER   Other error.
 */
#define XPAI_PUBLISH_CBCI_OK          0
#define XPAI_PUBLISH_CBCI_NOK_SERVER  1
#define XPAI_PUBLISH_CBCI_NOK_OTHER   2


/* Return codes for XPAI_SelfTest.
 * Success:
 *   XPAI_SELF_TEST_OK
 * Error codes:
 *   XPAI_SELF_TEST_NOK_WRONG_PARAM  Wrong parameter.
 *   XPAI_SELF_TEST_NOK_SERVER       Server not found.
 *   XPAI_SELF_TEST_NOK_WRONG_STATE  Self test is going on.
 *   XPAI_SELF_TEST_NOK_OTHER        Other error.
 */
#define XPAI_SELF_TEST_OK               0
#define XPAI_SELF_TEST_NOK_WRONG_PARAM  1
#define XPAI_SELF_TEST_NOK_SERVER       2
#define XPAI_SELF_TEST_NOK_WRONG_STATE  3
#define XPAI_SELF_TEST_NOK_OTHER        4


/* Parameter values for result in signal XPAI_SELF_TEST_IND.
 * Success:
 *   XPAI_SELF_TEST_RESULT_PASSED
 * Error codes:
 *   XPAI_SELF_TEST_RESULT_FAILED
 *   XPAI_SELF_TEST_RESULT_INTERRUPTED
 */
#define XPAI_SELF_TEST_RESULT_PASSED       0
#define XPAI_SELF_TEST_RESULT_FAILED       1
#define XPAI_SELF_TEST_RESULT_INTERRUPTED  2


/* XPSIM stub XPAI functions. */
#ifdef SOFT
#define XPAI_FNO_GET_BOARD_INFO          ((U32)XPAI_GetBoardInfo)
#define XPAI_FNO_GET_BOARD_INFO2         ((U32)XPAI_GetBoardInfo2)
#define XPAI_FNO_PUBLISH_CBCI_AU_SERVER  ((U32)XPAI_PublishCBCI_AU_Server)
#define XPAI_FNO_RESTART_BOARD           ((U32)XPAI_RestartBoard)
#define XPAI_FNO_RESTART_BOARD2          ((U32)XPAI_RestartBoard2)
#define XPAI_FNO_LOAD_FILE_DELETE_RESTART_BOARD  ((U32)XPAI_LoadFileDeleteRestartBoard)
#define XPAI_FNO_LOAD_FILE_DELETE_RESTART_BOARD2 ((U32)XPAI_LoadFileDeleteRestartBoard2)
#define XPAI_FNO_SELF_TEST               ((U32)XPAI_SelfTest)
#endif


/* Signal numbers.
 * 0x0100E000 = XCBC_SIGBASE. Redefined here to avoid dependency to xp.h.
 * Offset ranges within the XCBC offset range (0x00 - 0xFF) reserved for this
 * interface:
 *    0x80 - 0x8F
 */
#define XPAI_SELF_TEST_IND  (0x0100E000 + 0x80) /* !-SIGNO( struct XPAI_SelfTestIndS )-! */


/*----------------------------  Macros  -------------------------------------*/

/*----------------------------  Structs and Typedefs  -----------------------*/

/******************************************************************************
 * Signal:
 *      XPAI_SELF_TEST_IND
 *
 * Parameters:
 *      result  The result of the self-test.
 *              Value: XPAI_SELF_TEST_RESULT_PASSED
 *                     XPAI_SELF_TEST_RESULT_FAILED
 *                     XPAI_SELF_TEST_RESULT_INTERRUPTED
 *
 * Description:
 *      When the self-test has been completed the signal XPAI_SELF_TEST_IND
 *      is sent with the result to the process indicated with the pid parameter
 *      to the function XPAI_SelfTest.
 *
 *****************************************************************************/
struct XPAI_SelfTestIndS {
	SIGSELECT sigNo;
	U16       result;
};


/*----------------------------  Declaration of Global Variables  ------------*/

/*----------------------------  Declaration of Global Functions  ------------*/

/******************************************************************************
 *
 * Global function:
 *      XPAI_GetBoardInfo()
 *
 * Parameters:
 *      loadable          Pointer to return value specifying whether the unit
 *                        must be loaded or not.
 *                        Value: XPAI_BOARD_INFO_YES
 *                               XPAI_BOARD_INFO_NO
 *                        Not valid if XPAI_BOARD_INFO_NOK.
 *      flash             Pointer to return value specifying whether the unit
 *                        has access to flash memory for storing the load module
 *                        container (LMC).
 *                        Value: XPAI_BOARD_INFO_YES
 *                               XPAI_BOARD_INFO_NO
 *                        Not valid if XPAI_BOARD_INFO_NOK.
 *      filesystemAccess  Pointer to return value specifying whether the unit
 *                        has access to CPP file system for retreiving the load
 *                        module container (LMC).
 *                        Value: XPAI_BOARD_INFO_YES
 *                               XPAI_BOARD_INFO_NO
 *                        Not valid if XPAI_BOARD_INFO_NOK.
 *
 * Return value:
 *      XPAI_BOARD_INFO_OK
 *      XPAI_BOARD_INFO_NOK_SERVER
 *      XPAI_BOARD_INFO_NOK_OTHER
 *
 * Description:
 *      This function returns information about the board.
 *
 *****************************************************************************/
extern U32 XPAI_GetBoardInfo(U8 *loadable,          /* !- FUNC -! */
                             U8 *flash,
                             U8 *filesystemAccess);

/******************************************************************************
 *
 * Global function:
 *      XPAI_GetBoardInfo2()
 *
 * Parameters:
 *      loadable          Pointer to return value specifying whether the unit
 *                        must be loaded or not.
 *                        Value: XPAI_BOARD_INFO_YES
 *                               XPAI_BOARD_INFO_NO
 *                        Not valid if XPAI_BOARD_INFO_NOK.
 *      flash             Pointer to return value specifying whether the unit
 *                        has access to flash memory for storing the load module
 *                        container (LMC).
 *                        Value: XPAI_BOARD_INFO_YES
 *                               XPAI_BOARD_INFO_NO
 *                        Not valid if XPAI_BOARD_INFO_NOK.
 *      filesystemAccess  Pointer to return value specifying whether the unit
 *                        has access to CPP file system for retreiving the load
 *                        module container (LMC).
 *                        Value: XPAI_BOARD_INFO_YES
 *                               XPAI_BOARD_INFO_NO
 *                        Not valid if XPAI_BOARD_INFO_NOK.
 *      maxBlockSize      Maximum allowed size in bytes into which a load module
 *                        is chopped. Defined in  xpai_xcbc_loadable_au_if.h
 *                        as XPAI_LOAD_FILE_MAX_LM_BLOCKSIZE.
 *
 * Return value:
 *      XPAI_BOARD_INFO_OK
 *      XPAI_BOARD_INFO_NOK_SERVER
 *      XPAI_BOARD_INFO_NOK_OTHER
 *
 * Description:
 *      This function returns the result of XPAI_GetBoardInfo() with a extra
 *      additional information of MAX_LM_BLOCKSIZE. i.e. it returns information
 *      about the board and the value of XPAI_LOAD_FILE_MAX_LM_BLOCKSIZE.
 *
 *****************************************************************************/
extern U32 XPAI_GetBoardInfo2(U8 *loadable,          /* !- FUNC -! */
                              U8 *flash,
                              U8 *filesystemAccess,
                              U32 *maxBlockSize );


/******************************************************************************
 *
 * Global function:
 *      XPAI_PublishCBCI_AU_Server()
 *
 * Parameters:
 *      None.
 *
 * Return value:
 *      XPAI_PUBLISH_CBCI_OK
 *      XPAI_PUBLISH_CBCI_NOK_SERVER
 *      XPAI_PUBLISH_CBCI_NOK_OTHER
 *
 * Description:
 *      This function initiates publishing of the CBCI AU server in the XP
 *      platform (XPP). If the user application has not implemented the server
 *      the XPP server with the same name (XcbcServer) must be started.
 *      If there is no user application in the current executing load module
 *      container (LMC) the server is started automagically by XPP.
 *
 *****************************************************************************/
extern U32 XPAI_PublishCBCI_AU_Server(void); /* !- FUNC -! */


/******************************************************************************
 *
 * Global function:
 *      XPAI_RestartBoard()
 *
 * Parameters:
 *      loadModule  Null terminated string with the product ID
 *                  of the load module container (LMC) to be started.
 *                  Format: Cello lmid: <product number>_<revision>,
 *                          1553-CSX 101 09/1 Userguide for Cello 2.5
 *                  NULL or points to an empty string indicates that the
 *                  current load module container (LMC) shall be used.
 *                  Max string length: XPAI_LOAD_FILE_MAX_PID_STRINGLENGTH,
 *                                     see xpai_xcbc_loadable_au_if.h.
 *
 * Return value:
 *      None (does not return).
 *
 * Description:
 *      This function initiates a restart of the board.
 *
 *      The loadModule points to the name of the load module container (LMC)
 *      to be started. If loadModule is NULL or points to an empty string the
 *      current load module container is used.
 *
 *      If the load module container is found in flash it is started, else the
 *      youngest load module container of type AUBOOT found in flash is started.
 *
 *      The function does not return.
 *
 *      If there is an error in the parameter or the server is not found the
 *      board will be restarted with a call to the function error.
 *
 *****************************************************************************/
extern void XPAI_RestartBoard(char *loadModule); /* !- FUNC -! */


/******************************************************************************
 *
 * Global function:
 *      XPAI_RestartBoard2()
 *
 * Parameters:
 *      loadModule  Null terminated string with the product ID
 *                  of the load module container (LMC) to be started.
 *                  Format: Cello lmid: <product number>_<revision>,
 *                          1553-CSX 101 09/1 Userguide for Cello 2.5
 *                  NULL or points to an empty string indicates that the
 *                  current load module container (LMC) shall be used.
 *                  Max string length: XPAI_LOAD_FILE_MAX_PID_STRINGLENGTH,
 *                                     see xpai_xcbc_loadable_au_if.h.
 *      traceInformation  String to be written to error log and T&E log.
 *                        Null terminated string with max length
 *                        XPAI_FAULT_MAX_FAULT_DESCRIPTION_STRINGLENGTH
 *                        including null term, see xpai_xcbc_fault_if.h.
 *
 * Return value:
 *      None (does not return).
 *
 * Description:
 *      This function initiates a restart of the board.
 *
 *      The loadModule points to the name of the load module container (LMC)
 *      to be started. If loadModule is NULL or points to an empty string the
 *      current load module container is used.
 *
 *      If the load module container is found in flash it is started, else the
 *      youngest load module container of type AUBOOT found in flash is started.
 *
 *      The traceInformation is written to the T&E log which is read with the
 *      shell command te.
 *      The traceInformation is written to the error log which is read with the
 *      shell command llog.
 *
 *      The function does not return.
 *
 *      If there is an error in the parameter or the server is not found the
 *      board will be restarted with a call to the function error.
 *
 *****************************************************************************/
extern void XPAI_RestartBoard2(char *loadModule,  /* !- FUNC -! */
                               char *traceInformation);

/******************************************************************************
 *
 * Global function:
 *      XPAI_LoadFileDeleteRestartBoard()
 *
 * Parameters:
 *      loadModuleToDelete Null terminated string with the product ID
 *                         of the load module container (LMC) to be deleted.
 *                         Format: Cello lmid: <product number>_<revision>,
 *                                 1553-CSX 101 09/1 Userguide for Cello 2.5
 *                         NULL or points to an empty string indicates that the
 *                         current load module container (LMC) shall be used.
 *                         Max string length: XPAI_LOAD_FILE_MAX_PID_STRINGLENGTH,
 *                                            see xpai_xcbc_loadable_au_if.h.
 *      loadModuleToStart  Null terminated string with the product ID
 *                         of the load module container (LMC) to be started.
 *                         Format: Cello lmid: <product number>_<revision>,
 *                                 1553-CSX 101 09/1 Userguide for Cello 2.5
 *                         NULL or points to an empty string indicates that the
 *                         current load module container (LMC) shall be used.
 *                         Max string length: XPAI_LOAD_FILE_MAX_PID_STRINGLENGTH,
 *                                            see xpai_xcbc_loadable_au_if.h.
 *
 * Return value:
 *      None (does not return).
 *
 * Description:
 *      This function deletes a load module container (LMC) and initiates a
 *      restart of the board.
 *
 *      The loadModuleToDelete points to the name of the load module container
 *      (LMC)  to be deleted. If loadModuleToDelete is NULL or points to an
 *      empty string the current load module container is used.
 *
 *      The loadModuleToStart points to the name of the load module container
 *      (LMC) to be started. If loadModuleToStart is NULL or points to an
 *      empty string the current load module container is used.
 *      If the load module container is found in flash it is started, else the
 *      youngest load module container of type AUBOOT found in flash is started.
 *
 *      The function does not return and a restart is initiated even if the
 *      delete fails. The delete fails if the load file or sub file in the
 *      load file is open.
 *
 *      If there is an error in the parameter or the server is not found the
 *      board will be restarted with a call to the function error.
 *
 *****************************************************************************/
extern void XPAI_LoadFileDeleteRestartBoard(char
                *loadModuleToDelete,  /* !- FUNC -! */
                char *loadModuleToStart);


/******************************************************************************
 *
 * Global function:
 *      XPAI_LoadFileDeleteRestartBoard2()
 *
 * Parameters:
 *      loadModuleToDelete Null terminated string with the product ID
 *                         of the load module container (LMC) to be deleted.
 *                         Format: Cello lmid: <product number>_<revision>,
 *                                 1553-CSX 101 09/1 Userguide for Cello 2.5
 *                         NULL or points to an empty string indicates that the
 *                         current load module container (LMC) shall be used.
 *                         Max string length: XPAI_LOAD_FILE_MAX_PID_STRINGLENGTH,
 *                                            see xpai_xcbc_loadable_au_if.h.
 *      loadModuleToStart  Null terminated string with the product ID
 *                         of the load module container (LMC) to be started.
 *                         Format: Cello lmid: <product number>_<revision>,
 *                                 1553-CSX 101 09/1 Userguide for Cello 2.5
 *                         NULL or points to an empty string indicates that the
 *                         current load module container (LMC) shall be used.
 *                         Max string length: XPAI_LOAD_FILE_MAX_PID_STRINGLENGTH,
 *                                            see xpai_xcbc_loadable_au_if.h.
 *      traceInformation  String to be written to error log and T&E log.
 *                        Null terminated string with max length
 *                        XPAI_FAULT_MAX_FAULT_DESCRIPTION_STRINGLENGTH
 *                        including null term, see xpai_xcbc_fault_if.h.
 *
 * Return value:
 *      None (does not return).
 *
 * Description:
 *      This function deletes a load module container (LMC) and initiates a
 *      restart of the board.
 *
 *      The loadModuleToDelete points to the name of the load module container
 *      (LMC)  to be deleted. If loadModuleToDelete is NULL or points to an
 *      empty string the current load module container is used.
 *
 *      The loadModuleToStart points to the name of the load module container
 *      (LMC) to be started. If loadModuleToStart is NULL or points to an
 *      empty string the current load module container is used.
 *      If the load module container is found in flash it is started, else the
 *      youngest load module container of type AUBOOT found in flash is started.
 *
 *      The traceInformation is written to the T&E log which is read with the
 *      shell command te.
 *      The traceInformation is written to the error log which is read with the
 *      shell command llog.
 *
 *      The function does not return and a restart is initiated even if the
 *      delete fails. The delete fails if the load file or sub file in the
 *      load file is open.
 *
 *      If there is an error in the parameter or the server is not found the
 *      board will be restarted with a call to the function error.
 *
 *****************************************************************************/
extern void XPAI_LoadFileDeleteRestartBoard2(char
                *loadModuleToDelete,  /* !- FUNC -! */
                char *loadModuleToStart,
                char *traceInformation);


/******************************************************************************
 *
 * Global function:
 *      XPAI_SelfTest()
 *
 * Parameters:
 *      pid  Pid (process id) for process that shall receive XPAI_SELF_TEST_IND.
 *
 * Return value:
 *      XPAI_SELF_TEST_OK
 *      XPAI_SELF_TEST_NOK_WRONG_PARAM
 *      XPAI_SELF_TEST_NOK_SERVER
 *      XPAI_SELF_TEST_NOK_WRONG_STATE
 *      XPAI_SELF_TEST_NOK_OTHER
 *
 * Description:
 *      This function initiates a self-test. A thorough test that is as
 *      complete as possible is made. The test is allowed to take maximum
 *      1 minute.
 *
 *      The self-test will be executed at board level and might interfere
 *      with functionality at other interfaces.
 *
 *      Parallel self-tests are not allowed.
 *
 *      All active faults (faults present at the moment) before the test and
 *      detected faults during the test are sent to all subscribers of fault
 *      indications with the signal XPAI_FAULT_IND, see xpai_xcbc_fault_if.h.
 *
 *      When the test has been completed the signal XPAI_SELF_TEST_IND
 *      is sent with the result to the process indicated with the pid
 *      parameter. The result is only affected by detected faults during the
 *      test (not the active faults before the test).
 *
 *****************************************************************************/
extern U32 XPAI_SelfTest(U32 pid); /* !- FUNC -! */

#ifdef __cplusplus
}
#endif

#endif /* XPAI_XCBC_BASIC_IF_H */

