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
 *      XPAI/XHL
 *
 * File:         
 *      xpai_xhl_if.h
 *      
 * Author:       
 *      Peter Bergsten        11 July 2001
 *
 * Description:  
 *      Contains declarations of functions, macros and constants for 
 *      the hardware log function exported by XHL to XPAI.
 *
 * Reviewed:
 *      2001-10-01, Peter Bergsten (QRAPEBE)
 *
 * Revision history:
 *      2001-07-11, Peter Bergsten (QRAPEBE)
 *              Created.
 *      2001-10-01, Anders Hallqvist (QRAHALT)
 *              Updated after internal review
 *              
 *      2006-03-20 Sven Löfgren (qlofsve)
 *              Updated for Combined Interface (Wanja), XPAI_HWLogErase added.
 *              More comments added. FUNC-tags added.
 *
 *      2008-06-30 Sven Löfgren (xedsven)
 *              XPAI_ReadNodeId and XPAI_WriteNodeId added.
 *
 *      2008-10-02 Sven Löfgren (xedsven)
 *              XPAI_ReadNodeId and XPAI_WriteNodeId: struct S32 -> S32.
 *
 *      2009-10-28 Magnus Lindgren (xmaglin)
 *              CR WRNae62653 "HWlog logID has wrong range in whole BCP"
 *              Added XPAI_HWLogWrite2(char* logId, char* string)
 *
 *      2010-02-25 Magnus Lindgren (emagpal)
 *              CR WRNae63077 - New read logId specified procedure
 *                              Added XPAI_HWLogReadSpecifiedId
 *
 *****************************************************************************/

#ifndef XPAI_XHL_IF_H
#define XPAI_XHL_IF_H

/*----------------------------  Include files  ------------------------------*/
#include "osetypes.h"

/*----------------------------  CONSTANTS  ----------------------------------*/

/* Return values for XPAI_HWLogWrite and result values for XPAI_HWLogS.
 * Success:
 *   XPAI_XHL_OK
 * Error code:
 *   XPAI_XHL_NOT_OK    Other error (entry not written).
 *   XPAI_XHL_FILTERED  Entry filtered (entry not written).
 *   XPAI_XHL_FULL      Area full (entry not written).
 */
#define XPAI_XHL_OK                    0
#define XPAI_XHL_NOT_OK                1
#define XPAI_XHL_FILTERED              2
#define XPAI_XHL_FULL                  3


/* Parameter values for parameter area to XPAI_HWLogRead and XPAI_HWLogErase. */
#define XPAI_XHL_AREA_REPAIR           0
#define XPAI_XHL_AREA_SYSTEM           1


/* Parameter values for parameter noOfEntries in XPAI_HWLogS. 
 * XPAI_XHL_NO_REPAIR_ENTRIES  Max number of repair entries.
 * XPAI_XHL_NO_SYSTEM_ENTRIES  Max number of system entries.
 */
#define XPAI_XHL_NO_REPAIR_ENTRIES       8
#define XPAI_XHL_NO_SYSTEM_ENTRIES      55


/* Max length of returned entry in XPAI_HWLogS. */
#define XPAI_XHL_ENTRY_LENGTH          128


/* Return values for XPAI_HWLogErase
 * Success:
 *   XPAI_HW_LOG_ERASE_OK
 * Error code:
 *   XPAI_HW_LOG_ERASE_NOK_PARAM   Parameter error.
 *   XPAI_HW_LOG_ERASE_NOK_SERVER  Server not found.
 *   XPAI_HW_LOG_ERASE_NOK_OTHER   Other error.
 */
#define XPAI_HW_LOG_ERASE_OK          0
#define XPAI_HW_LOG_ERASE_NOK_PARAM   1
#define XPAI_HW_LOG_ERASE_NOK_SERVER  2
#define XPAI_HW_LOG_ERASE_NOK_OTHER   3


/* Value for buffer in XPAI_ReadNodeId. */
#define XPAI_READ_NODE_ID_MIN_BUFFER_SIZE   50


/* Return codes for XPAI_ReadNodeId.
 * Success:
 *   XPAI_READ_NODE_ID_OK
 * Error codes:
 *   XPAI_READ_NODE_ID_NOK_PARAM         Wrong parameter.
 *   XPAI_READ_NODE_ID_NOK_SERVER        Server not found.
 *   XPAI_READ_NODE_ID_NOK_NOT_READABLE  Not readable.
 *   XPAI_READ_NODE_ID_NOK_OTHER         Other error.
 */
#define XPAI_READ_NODE_ID_OK                0
#define XPAI_READ_NODE_ID_NOK_PARAM         -1
#define XPAI_READ_NODE_ID_NOK_SERVER        -2
#define XPAI_READ_NODE_ID_NOK_NOT_READABLE  -3
#define XPAI_READ_NODE_ID_NOK_OTHER         -4


/* Parameter value for length in XPAI_WriteNodeId. */
#define XPAI_WRITE_NODE_ID_MAX_LENGTH       50


/* Return codes for XPAI_WriteNodeId.
 * Success:
 *   XPAI_WRITE_NODE_ID_OK
 * Error codes:
 *   XPAI_WRITE_NODE_ID_NOK_PARAM          Wrong parameter.
 *   XPAI_WRITE_NODE_ID_NOK_SERVER         Server not found.
 *   XPAI_WRITE_NODE_ID_NOK_NOT_WRITEABLE  Not writeable (flash full).
 *   XPAI_WRITE_NODE_ID_NOK_OTHER          Other error.
 */
#define XPAI_WRITE_NODE_ID_OK                 0
#define XPAI_WRITE_NODE_ID_NOK_PARAM          -1
#define XPAI_WRITE_NODE_ID_NOK_SERVER         -2
#define XPAI_WRITE_NODE_ID_NOK_NOT_WRITEABLE  -3
#define XPAI_WRITE_NODE_ID_NOK_OTHER          -4


#ifdef SOFT
#define XPAI_FNO_HW_LOG_READ         ((U32)XPAI_HWLogRead)
#define XPAI_FNO_HW_LOG_READ_SPEC_ID ((U32)XPAI_HWLogReadSpecifiedId)
#define XPAI_FNO_HW_LOG_WRITE        ((U32)XPAI_HWLogWrite)
#define XPAI_FNO_HW_LOG_WRITE2       ((U32)XPAI_HWLogWrite2)
#define XPAI_FNO_HW_LOG_ERASE        ((U32)XPAI_HWLogErase)
#define XPAI_FNO_READ_NODE_ID        ((S32)XPAI_ReadNodeId)
#define XPAI_FNO_WRITE_NODE_ID       ((S32)XPAI_WriteNodeId)
#endif

/*----------------------------  MACROS  -------------------------------------*/

/*----------------------------  Structs and typedefs  -----------------------*/

/* Returned struct XPAI_HWLogS. 
 * Parameters:
 *      result       Result of HW log read.
 *                   Value: XPAI_XHL_OK
 *                          XPAI_XHL_NOT_OK
 *      noOfEntries  Number of log entries.
 *                   Max values: XPAI_XHL_NO_REPAIR_ENTRIES for repair entries.
 *                               XPAI_XHL_NO_SYSTEM_ENTRIES for system entries.
 *      list         List of entries.
 *                   Max entry length: XPAI_XHL_ENTRY_LENGTH
 */
struct XPAI_HWLogS
{
  U32 result;
  U32 noOfEntries;
  char list [XPAI_XHL_NO_SYSTEM_ENTRIES][XPAI_XHL_ENTRY_LENGTH];
};

/*----------------------------  Declaration of Global Variables  ------------*/

/*----------------------------  Declaration of Global Functions  ------------*/

/******************************************************************************
 *
 * Global function:     
 *      XPAI_HWLogWrite
 *
 * Parameters:
 *      dateTimeStr  Date an time "YYMMDD HHMMSS", if empty current
 *                   date and time is used
 *      logId        Id of the string. Used to filter out duplicates.
 *      logStr       Zero terminated string of a maximum total of 106 bytes.
 *                   If the string is too long it is truncated.
 *
 * Return value:
 *      XPAI_XHL_OK
 *      XPAI_XHL_NOT_OK
 *      XPAI_XHL_FILTERED
 *      XPAI_XHL_FULL
 *
 * Description:
 *      NOTE: This function is for backwards compatibility only and should
 *      not be used anymore! Instead, use XPAI_HWLogWrite2. See CR WRNae62653
 *
 *      This function writes an entry to the system area of the hardware log.
 *
 *      The function should be called when a hardware error is detected. The
 *      saved entry (logString) is marked with a sequence number, date and 
 *      time by the platform.
 *
 *      The logId is used to avoid duplicates. If logId is 0 or 2, the logStr 
 *      contents is used to avoid duplicates.
 *
 *****************************************************************************/
U32 XPAI_HWLogWrite(char *dateTimeStr,  /* !- FUNC -! */
                    U32   logId, 
                    char *logStr);

/******************************************************************************
 *
 * Global function:     
 *      XPAI_HWLogWrite2
 *
 * Parameters:
 *      dateTimeStr  Date an time "YYMMDD HHMMSS", if empty current
 *                   date and time is used
 *      logIdStr     Id of the string, three charcter array (no string
 *                   terminator needed). Used to filter out duplicates.
 *      logStr       Zero terminated string of a maximum total of 106 bytes.
 *                   If the string is too long it is truncated.
 *
 * Return value:
 *      XPAI_XHL_OK
 *      XPAI_XHL_NOT_OK
 *      XPAI_XHL_FILTERED
 *      XPAI_XHL_FULL
 *
 * Description:
 *      This function writes an entry to the system area of the hardware log.
 *
 *      The function should be called when a hardware error is detected. The
 *      saved entry (logString) is marked with a sequence number, date and 
 *      time by the platform.
 *
 *      The logId is used to avoid duplicates. If logId = "000" or "002", the
 *      logStr contents is used to avoid duplicates.
 *
 *****************************************************************************/
U32 XPAI_HWLogWrite2(char *dateTimeStr,  /* !- FUNC -! */
                    char *logIdStr, 
                    char *logStr);

/******************************************************************************
 *
 * Global function:     
 *      XPAI_HWLogRead
 *
 * Parameters:
 *      area  Type of hardware log area.
 *            Value: XPAI_XHL_AREA_REPAIR
 *                   XPAI_XHL_AREA_SYSTEM
 *
 * Return value:
 *      A struct containing result and read entries.
 *
 * Description:
 *      This function reads either the system or the repair log area
 *      and returns the entries in a struct.
 *
 *****************************************************************************/
struct XPAI_HWLogS *XPAI_HWLogRead(U32 area); /* !- FUNC -! */

/******************************************************************************
 *
 * Global function:     
 *      XPAI_HWLogReadSpecifiedId
 *
 * Parameters:
 *      logId - which logId to read from hardware log system area
 *
 * Return value:
 *      A struct containing result and read entries.
 *
 * Description:
 *      This function works as XPAI_HWLogRead but only reads from the system
 *      area and filters the result on logId given as input.
 *
 *****************************************************************************/
struct XPAI_HWLogS *XPAI_HWLogReadSpecifiedId(char* logId); /* !- FUNC -! */

/******************************************************************************
 *
 * Global function:     
 *      XPAI_HWLogErase
 *
 * Parameters:
 *      area  Type of hardware log area.
 *            Value: XPAI_XHL_AREA_REPAIR
 *                   XPAI_XHL_AREA_SYSTEM
 *
 * Return value:
 *      XPAI_HW_LOG_ERASE_OK
 *      XPAI_HW_LOG_ERASE_NOK_PARAM
 *      XPAI_HW_LOG_ERASE_NOK_SERVER
 *      XPAI_HW_LOG_ERASE_NOK_OTHER
 *
 * Description:
 *      This procedure erases the system or repair area of the hardware log. 
 *
 *      The procedure shall only be used at production since the erase
 *      procedure cannot be done safe. This means that in case of a restart
 *      or power-off during this procedure other areas in the flash may be
 *      lost or the hardware log may end up in a corrupt state.
 *
 *****************************************************************************/
U32 XPAI_HWLogErase(U32 area); /* !- FUNC -! */

/******************************************************************************
 *
 * Global function:     
 *      XPAI_ReadNodeId
 *
 * Parameters:
 *      length     Pointer to returned length of node identity buffer.
 *      buffer     Pointer to buffer where the node identity should be written.
 *                 The buffer must have a size of at least
 *                 XPAI_READ_NODE_ID_MIN_BUFFER_SIZE bytes.
 *
 * Return value:
 *      XPAI_READ_NODE_ID_OK
 *      XPAI_READ_NODE_ID_NOK_PARAM
 *      XPAI_READ_NODE_ID_NOK_SERVER
 *      XPAI_READ_NODE_ID_NOK_NOT_READABLE
 *      XPAI_READ_NODE_ID_NOK_OTHER
 *
 * Description:
 *      This function is used to read the node identity.
 *
 *****************************************************************************/
S32 XPAI_ReadNodeId(U32 *length, U8 *buffer); /* !- FUNC -! */

/******************************************************************************
 *
 * Global function:     
 *      XPAI_WriteNodeId
 *
 * Parameters:
 *      length     Length of node identity buffer.
 *                 The maximal length is XPAI_WRITE_NODE_ID_MAX_LENGTH.
 *      buffer     Pointer to buffer containing the node identity.
 *
 * Return value:
 *      XPAI_WRITE_NODE_ID_OK
 *      XPAI_WRITE_NODE_ID_NOK_PARAM
 *      XPAI_WRITE_NODE_ID_NOK_SERVER
 *      XPAI_WRITE_NODE_ID_NOK_NOT_WRITEABLE
 *      XPAI_WRITE_NODE_ID_NOK_OTHER
 *
 * Description:
 *      This function is used to write the node identity to flash. On boards 
 *      with a strata flash memory only a limited number of writes can be done.
 *
 *****************************************************************************/
S32 XPAI_WriteNodeId(U32 length, U8 *buffer); /* !- FUNC -! */

#endif /* XPAI_XHL_IF_H */

