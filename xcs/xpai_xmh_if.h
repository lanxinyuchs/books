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
 *      XPAI/XMH
 *
 * File:         
 *      xpai_xmh_if.h
 *      
 * Author:       
 *      Anders Hallqvist (QRAHALT)
 *
 * Description:  
 *      This file defines XPAI XMH external interface functions.
 *
 * Reviewed:
 *      2000-08-29, Henrik Stöckel
 *
 * Revision history:
 *      2000-04-17, Jan Lindqvist (QRAJANL)
 *        Correcting some function declarations and defining some error codes.
 *      2000-09-04, Anders Hallqvist (QRAHALT)
 *        Major rewrite.
 *      2000-10-05, Anders Hallqvist (QRAHALT)
 *        Added seqno to loadfilelist struct.  
 *        loadfileindex base changed to 0 from 1.
 *      2000-11-23, Anders Hallqvist (QRAHALT)
 *        Added prototype to function XPAI_Checksum
 *              
 *      2006-04-20 Sven Löfgren (qlofsve)
 *              Updated with FUNC-tags. (Wanja).
 *
 *      2009-03-27 Sven Löfgren (xedsven)
 *              WP216: X2_GetHwInfo added, 
 *                     XPAI_LoadSubFileList2 added,
 *                     XPAI_LoadSubFileOpen2 added, 
 *                     XPAI_LoadSubFileRead2 added,
 *                     XPAI_LoadFileList2 added,
 *                     XPAI_XMH_LOCKED_SLOT added.
 *
 *      2009-11-06 Sven Löfgren (xedsven)
 *              Comments corrected.
 *
 *      2010-08-24 Conny Bonet (qraconb)
 *              New flash type added.
 *
 *	2013-03-14 Amir Mohammad Koosha (eamikoo)
 *		CR BSDB00008540: Add support for (64MB) Spansion S29GL-S and
 *		Micron M29EW on WARP3 and WARP4.
 *              Code refactored and redundant code removed.
 *
 *****************************************************************************/

#ifndef _XPAI_XMH_IF_H
#define _XPAI_XMH_IF_H

/*----------------------------  Include files  ------------------------------*/
#include <osetypes.h> /*lint !e537*/

/*---------------------------   Constants   ---------------------------------*/

/* General Constants */
#define XPAI_XMH_MAX_LOADFILES     4
#define XPAI_XMH_MAX_SUBFILES     16
#define XPAI_XMH_SWPID_LENGTH     32

#define XPAI_XMH_FILETYPE_AUBOOT    1
#define XPAI_XMH_FILETYPE_AUAPPLIC  2

#define XPAI_XMH_STATE_VALID        11
#define XPAI_XMH_STATE_ERASED       12
#define XPAI_XMH_STATE_ERPROG       13
#define XPAI_XMH_STATE_WRPROG       14
#define XPAI_XMH_STATE_ERROR        15
#define XPAI_XMH_STATE_NON_EXISTENT 16

#define XPAI_XMH_FORMAT_RPDOUT         1
#define XPAI_XMH_FORMAT_USERDATA       2

/* XMH exported within XP platform. */
#define XPAI_XMH_OPENFILE_READ      1
#define XPAI_XMH_OPENFILE_WRITE     2

/* Return codes used in the result member */
#define XPAI_XMH_SUCCEED                0
#define XPAI_XMH_FAIL                  -1

#define XPAI_XMH_ILLEGAL_ADDRESS      -11
#define XPAI_XMH_ILLEGAL_HANDLE       -12
#define XPAI_XMH_ILLEGAL_INDEX        -13
#define XPAI_XMH_NO_LOAD_FILE         -14
#define XPAI_XMH_LAST_AUBOOT_FILE     -15
#define XPAI_XMH_CUR_EXE_LOADFILE     -16
#define XPAI_XMH_ALREADY_IN_USE       -17
#define XPAI_XMH_WRITE_IN_PROGRESS    -18
#define XPAI_XMH_ERASE_IN_PROGRESS    -19
#define XPAI_XMH_TO_MANY_SUBFILES     -20
#define XPAI_XMH_OUT_OF_HANDLES       -21
#define XPAI_XMH_NOT_VALID            -22
#define XPAI_XMH_LOCKED_SLOT          -23

#ifdef SOFT
#define XPAI_FNO_FLASH_ERASE         ((U32)XPAI_FlashErase)
#define XPAI_FNO_FLASH_WRITE         ((U32)XPAI_FlashWrite)
#define XPAI_FNO_FLASH_READ          ((U32)XPAI_FlashRead)
#define XPAI_FNO_LOAD_FILE_LIST      ((U32)XPAI_LoadFileList)
#define XPAI_FNO_LOAD_FILE_LIST2     ((U32)XPAI_LoadFileList2)
#define XPAI_FNO_LOAD_SUB_FILE_LIST  ((U32)XPAI_LoadSubFileList)
#define XPAI_FNO_LOAD_SUB_FILE_LIST2 ((U32)XPAI_LoadSubFileList2)
#define XPAI_FNO_LOAD_SUB_FILE_OPEN  ((U32)XPAI_LoadSubFileOpen)
#define XPAI_FNO_LOAD_SUB_FILE_OPEN2 ((U32)XPAI_LoadSubFileOpen2)
#define XPAI_FNO_LOAD_SUB_FILE_READ  ((U32)XPAI_LoadSubFileRead)
#define XPAI_FNO_LOAD_SUB_FILE_READ2 ((U32)XPAI_LoadSubFileRead2)
#define XPAI_FNO_LOAD_SUB_FILE_CLOSE ((U32)XPAI_LoadSubFileClose)
#endif

/*---------------------------   Structs and typedefs  -----------------------*/
struct XPAI_LoadFileEntryS
{
  U32 state;        /* XPAI_XMH_STATE_VALID,  XPAI_XMH_STATE_ERASED       */
                    /* XPAI_XMH_STATE_ERPROG, XPAI_XMH_STATE_WRPROG       */
                    /* XPAI_XMH_STATE_ERROR,  XPAI_XMH_STATE_NON_EXISTENT */
  U8  isCurrentLoadFile; /* 1 if this is current loadfile 0 if not        */
  U32 size;              /* Size of slot                                  */
  U32 lmc_size;               /* Size of the LMC                                  */
  U32 seqNo;             /* Sequence no of loadfile                       */
  U32 fileType;          /* XPAI_XMH_FILETYPE_AUBOOT                      */
                         /* XPAI_XMH_FILETYPE_AUAPPLIC                    */
  U32 prodDateTime;
  char swPid[XPAI_XMH_SWPID_LENGTH + 1];
  U32 noLoadSubFiles;    /* No of sub-files in lmc */
  U32 working;           /* 1 if marked as working, 0 if not */
};

struct XPAI_LoadFileEntry2S
{
  U32 state;        /* XPAI_XMH_STATE_VALID,  XPAI_XMH_STATE_ERASED       */
                    /* XPAI_XMH_STATE_ERPROG, XPAI_XMH_STATE_WRPROG       */
                    /* XPAI_XMH_STATE_ERROR,  XPAI_XMH_STATE_NON_EXISTENT */
  U8  isCurrentLoadFile; /* 1 if this is current loadfile 0 if not        */
  U32 size;              /* Size of slot                                  */
  U32 lmc_size;               /* Size of the LMC                                  */
  U32 seqNo;             /* Sequence no of loadfile                       */
  U32 fileType;          /* XPAI_XMH_FILETYPE_AUBOOT                      */
                         /* XPAI_XMH_FILETYPE_AUAPPLIC                    */
  U32 prodDateTime;
  char swPid[XPAI_XMH_SWPID_LENGTH + 1];
  U32 noLoadSubFiles;    /* No of sub-files in lmc */
  U32 working;           /* 1 if marked as working, 0 if not */
  U32 locked;            /* 0=unlocked slot, 1=locked slot (board param) */
};

struct XPAI_LoadFileListS
{
  S32 result;
  struct XPAI_LoadFileEntryS info[XPAI_XMH_MAX_LOADFILES];
};

struct XPAI_LoadFileList2S
{
  S32 result;
  struct XPAI_LoadFileEntry2S info[XPAI_XMH_MAX_LOADFILES];
};

struct XPAI_SubFileListS
{
  S32 result;
  U32 noLoadSubFiles;
  char info[XPAI_XMH_MAX_SUBFILES][XPAI_XMH_SWPID_LENGTH + 1];
  U32 autoStart[XPAI_XMH_MAX_SUBFILES]; /* 1=autoStart, 0=No autoStart */
  U32 format[XPAI_XMH_MAX_SUBFILES];    /* XPAI_XMH_FORMAT_RPDOUT,  
                                           XPAI_XMH_FORMAT_USERDATA */
};

/*----------------------------  Declaration of Global Functions  ------------*/

/******************************************************************************
 *
 * Function:     
 *      Global function XPAI_FlashErase()
 *
 * Parameters:   
 *      startAddr, Absolute address to erase in FLASH.
 *                 Use non-cacheable address.
 *      length,    size of area to erase.   
 *
 * Return value:
 *      XPAI_XMH_SUCCEED, XPAI_XMH_FAIL, XPAI_XMH_ILLEGAL_ADDRESS
 *
 * Description:
 *      Erases an area in the flash memory. Note the flash is erased
 *      in blocks, so all blocks containing at least one byte of the
 *      buffer specified will be erased. 
 *
 *****************************************************************************/
extern S32 XPAI_FlashErase(U8 *startAddr, U32 length); /* !- FUNC -! */

/******************************************************************************
 *
 * Function:     
 *      Global function XPAI_FlashWrite()
 *
 * Parameters:   
 *      startAddr, Absolute address to write in FLASH.
 *                 Use non-cacheable address.
 *      buffer,    Pointer to buffer to write from.   
 *      length,    Size of buffer to write.   
 *
 * Return value: 
 *      XPAI_XMH_SUCCEED, XPAI_XMH_FAIL, XPAI_XMH_ILLEGAL_ADDRESS
 *
 * Description:
 *      Writes the contents of a buffer to FLASH. Note startAddr and size
 *      must be even numbers i.e halfword aligned. 
 *
 *****************************************************************************/
extern S32 XPAI_FlashWrite(U8 *startAddr, /* !- FUNC -! */
                           U8 *buffer, 
                           U32 length); 

/******************************************************************************
 *
 * Function:     
 *      Global function XPAI_FlashRead()
 *
 * Parameters:   
 *      startAddr, Absolute address to read from FLASH.
 *                 Use non-cacheable address.
 *      buffer,    Pointer to buffer for read data.
 *      length,    size of buffer to read.   
 *
 * Return value: 
 *      XPAI_XMH_SUCCEED, XPAI_XMH_FAIL, XPAI_XMH_ILLEGAL_ADDRESS
 *
 * Description:
 *      Reads the contents of  FLASH to a buffer.
 *
 *****************************************************************************/
extern S32 XPAI_FlashRead(U8 *startAddr, /* !- FUNC -! */
                          U8 *buffer, 
                          U32 length);

extern S32 XPAI_ReadLoadFileData(U32 loadFileIx, /* !- FUNC -! */
			  U32 addrOffset,
                          U8 *buffer, 
                          U32 length);
/******************************************************************************
 *
 * Function:     
 *      Global function XPAI_LoadFileList()
 *
 * Parameters:   
 *      None.   
 *
 * Return value: 
 *      List of load files. One entry for each load file in flash. 
 *      The user of the function must make sure to free the struct returned.
 *      after use.
 *
 * Description:
 *      Returns information on what loadfiles currently exists in flash.
 *      The result member is currently always set to XPAI_XMH_SUCCEED.
 *
 *****************************************************************************/
extern struct XPAI_LoadFileListS *XPAI_LoadFileList(void); /* !- FUNC -! */

/******************************************************************************
 *
 * Function:     
 *      Global function XPAI_LoadFileList2()
 *
 * Parameters:   
 *      None.   
 *
 * Return value: 
 *      List of load files. One entry for each load file in flash. 
 *      The user of the function must make sure to free the struct returned.
 *      after use.
 *
 * Description:
 *      Returns information on what loadfiles currently exists in flash.
 *      The result member is currently always set to XPAI_XMH_SUCCEED.
 *
 *****************************************************************************/
extern struct XPAI_LoadFileList2S *XPAI_LoadFileList2(void); /* !- FUNC -! */

/******************************************************************************
 *
 * Function:     
 *      Global function XPAI_LoadSubFileList()
 *
 * Parameters:   
 *      None.   
 *
 * Return value: 
 *      List of load sub-files. The user of the function must make sure 
 *      to free the struct returned, after use. The member result in the
 *      returned struct could be set to XPAI_XMH_SUCCEED, 
 *      XPAI_XMH_NO_LOAD_FILE or XPAI_XMH_TO_MANY_SUBFILES.
 *
 * Description:
 *      Copies and returns the contents of the sub-files list associated with
 *      the current load file. If there are more sub-files than
 *      XPAI_XMH_MAX_SUBFILES in the current loadfile only information on the 
 *      first XPAI_XMH_MAX_SUBFILES are returned.
 *
 *****************************************************************************/
extern struct XPAI_SubFileListS  *XPAI_LoadSubFileList(void); /* !- FUNC -! */

/******************************************************************************
 *
 * Function:     
 *      Global function XPAI_LoadSubFileList2()
 *
 * Parameters:   
 *      loadFileIx,   Which load file to list. 
 *                    Index from XPAI_LoadFileListS or XPAI_LoadFileList2S.
 *
 * Return value: 
 *      List of load sub-files. The user of the function must make sure 
 *      to free the struct returned, after use. The member result in the
 *      returned struct could be set to XPAI_XMH_SUCCEED, 
 *      XPAI_XMH_NO_LOAD_FILE or XPAI_XMH_TO_MANY_SUBFILES.
 *
 * Description:
 *      Copies and returns the contents of the sub-files list associated with
 *      the load file loadFileIx. If there are more sub-files than
 *      XPAI_XMH_MAX_SUBFILES in the current loadfile only information on the 
 *      first XPAI_XMH_MAX_SUBFILES are returned.
 *
 *****************************************************************************/
extern struct XPAI_SubFileListS  *XPAI_LoadSubFileList2(U32 loadFileIx); /* !- FUNC -! */

/******************************************************************************
 *
 * Function:     
 *      Global function XPAI_LoadSubFileOpen()
 *
 * Parameters:   
 *      loadSubFileIx,   Which sub-file to open. 0,1,2,3 ...
 *                       Index from XPAI_SubFileListS.
 *
 * Return value: 
 *      A Handle to  use with calls to XPAI_LoadSubFileRead, 
 *      XPAI_LoadSubFileRead2 and XPAI_LoadSubFileClose. 
 *      A valid handle is positive or 0.
 *      If an Error occurs a negative errorcode is returned. 
 *      XPAI_XMH_NO_LOADFILE    means that no current loadfile exists.
 *      XPAI_XMH_ILLEGAL_INDEX  means that sub-file index does not exist.
 *      XPAI_XMH_OUT_OF_HANDLES means to many users tries to open files.
 *
 * Description:
 *      Opens a sub-file in the currently executing load file.
 *      Maximum XPAI_XMH_MAX_SUBFILES may be open at the same time.
 *
 *****************************************************************************/
extern S32 XPAI_LoadSubFileOpen(U32 loadSubFileIx); /* !- FUNC -! */

/******************************************************************************
 *
 * Function:     
 *      Global function XPAI_LoadSubFileOpen2()
 *
 * Parameters:   
 *      loadFileIx,      Which load file to open sub-file in. 
 *                       Index from XPAI_LoadFileListS or XPAI_LoadFileList2S.
 *      loadSubFileIx,   Which sub-file to open. 0,1,2,3 ...
 *                       Index from XPAI_SubFileListS.
 *
 * Return value: 
 *      A Handle to  use with calls to XPAI_LoadSubFileRead2 and
 *      XPAI_LoadSubFileClose. 
 *      A valid handle is positive or 0.
 *      If an Error occurs a negative errorcode is returned. 
 *      XPAI_XMH_NO_LOADFILE    means that no current loadfile exists.
 *      XPAI_XMH_ILLEGAL_INDEX  means that sub-file index does not exist.
 *      XPAI_XMH_OUT_OF_HANDLES means to many users tries to open files.
 *
 * Description:
 *      Opens a sub-file in the load file with index loadFileIx.
 *      Maximum XPAI_XMH_MAX_SUBFILES may be open at the same time.
 *
 *****************************************************************************/
extern S32 XPAI_LoadSubFileOpen2(U32 loadFileIx, U32 loadSubFileIx); /* !- FUNC -! */

/******************************************************************************
 *
 * Function:     
 *      Global function XPAI_LoadSubFileRead()
 *
 * Parameters:   
 *      handle,      returned from XPAI_LoadSubFileOpen.
 *      buffer,      buffer to read into.
 *      pos,         offset in file to start read at.   
 *      size,        size of buffer.
 *
 * Return value: 
 *      XPAI_XMH_SUCCEED, XPAI_XMH_FAIL, XPAI_XMH_ILLEGAL_HANDLE
 *
 * Description:
 *      Reads a piece of a sub-file to a buffer.
 *
 *****************************************************************************/
extern S32 XPAI_LoadSubFileRead(S32 handle, /* !- FUNC -! */
                                U8 *buffer, 
                                U32 pos, 
                                U32 size);

/******************************************************************************
 *
 * Function:     
 *      Global function XPAI_LoadSubFileRead2()
 *
 * Parameters:   
 *      handle,    returned from XPAI_LoadSubFileOpen or XPAI_LoadSubFileOpen2.
 *      buffer,    buffer to read into.
 *      pos,       offset in file to start read at.   
 *      size,      size of buffer.
 *
 * Return value: 
 *      The number of bytes read (positive) or error (negative).
 *      If the returned number of bytes read are less than size of buffer,
 *      the end of the sub-file is reached. 
 *      The negative error codes are:
 *      XPAI_XMH_FAIL, XPAI_XMH_ILLEGAL_HANDLE
 *
 * Description:
 *      Reads a piece of a sub-file to a buffer.
 *
 *****************************************************************************/
extern S32 XPAI_LoadSubFileRead2(S32 handle, /* !- FUNC -! */
				 U8 *buffer, 
				 U32 pos, 
				 U32 size);

/******************************************************************************
 *
 * Function:     
 *      Global function XPAI_LoadSubFileClose()
 *
 * Parameters:   
 *      handle,  returned from XPAI_LoadSubFileOpen or XPAI_LoadSubFileOpen2.
 *
 * Return value: 
 *      XPAI_XMH_SUCCEED, XPAI_XMH_FAIL, XPAI_XMH_ILLEGAL_INDEX
 *
 * Description:
 *      Close a sub-file.
 *
 *****************************************************************************/
extern S32 XPAI_LoadSubFileClose(S32 handle); /* !- FUNC -! */

#endif /* _XPAI_XMH_IF_H */
