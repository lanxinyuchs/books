/*****************************************************************************
 *
 *
 * Copyright (c) Ericsson AB  2015-2017 All rights reserved.
 *
 * The information in this document is the property of Ericsson. Except
 * as specifically authorized in writing by Ericsson,the receiver of this
 * document shall keep the information contained herein confidential
 * and shall protect the same in whole or in part from disclosure and
 * dissemination to third parties. Disclosure and disseminations to the
 * receiver's employees shall only be made on a strict need to know basis.
 *
 ****************************************************************************/
#ifndef LOG_ESI_SIG
#define LOG_ESI_SIG

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

#define LOG_ESI_SERVER_MBOX    "log_esi"

#define LOG_ESI_EXT_NAMESPACE_MAX_LENGTH     256
#define LOG_ESI_EXT_MAX_PROD_NUMBER_LENGTH   400
#define LOG_ESI_EXT_MAX_IP_ADD_LENGTH         32
#define LOG_ESI_EXT_ERROR_STRING_MAX_LENGTH  150
#define LOG_ESI_EXT_DIR_PATH_MAX_LENGTH      400

/******************************************************************************
 *
 * Message Name: LOG_ESI_EXT_CONN_ESTABLISH_UREQ
 *
 * Descr      : CAT initializes a LOG_ESI session
 *
 * Data       
 *
 *****************************************************************************/
#define LOG_ESI_EXT_CONN_ESTABLISH_UREQ (0x0191001)
typedef struct
{
   uint32_t  msgNo;
} LogEsiExtConnEstablishUreq;


/******************************************************************************
 *
 * Message Name: LOG_ESI_EXT_GET_BOARDS_RREQ
 *
 * Descr      : message to CAT to send information about all boards
 *              for which an ESI is to be generated
 *
 * Data       : 
 *
 *****************************************************************************/
#define LOG_ESI_EXT_GET_BOARDS_RREQ (0x0191002)
typedef struct
{
   uint32_t  msgNo;
} LogEsiExtGetBoardsRreq;


/******************************************************************************
 *
 * Message Name: LOG_ESI_EXT_GET_BOARDS_RCFM
 *
 * Descr      : Reply msg to LOG_ESI_EXT_GET_BOARDS_RREQ
 *              Contains a list of boards 
 *
 * Data       : fruId           Number of boardInfo. A NULL terminated string.
 *            : numberOfBoards  Number of boardInfo
 *            : boardInfo       Array of boardInfo
 *
 *****************************************************************************/
#define LOG_ESI_EXT_GET_BOARDS_RCFM (0x0191003)

typedef struct
{
  char     fruId[LOG_ESI_EXT_MAX_PROD_NUMBER_LENGTH];
} boardInfoS;

typedef struct
{
   uint32_t   msgNo;
   uint32_t   numberOfBoards;
   boardInfoS boardInfo[1];
} LogEsiExtGetBoardsRcfm;


/******************************************************************************
 *
 * Message Name: LOG_ESI_EXT_GET_BOARD_INFO_RREQ
 *
 * Descr      : message to CAT to send the directories to be included
 *              in the ESI
 *
 * Data       : fruId  Specifies the board 
 *                     A NULL terminated string.
 *
 *****************************************************************************/
#define LOG_ESI_EXT_GET_BOARD_INFO_RREQ (0x0191004)
typedef struct
{
   uint32_t  msgNo;
   char      fruId[LOG_ESI_EXT_MAX_PROD_NUMBER_LENGTH];
} LogEsiExtGetBoardInfoRreq;


/******************************************************************************
 *
 * Message Name: LOG_ESI_EXT_GET_BOARD_INFO_RCFM
 *
 * Descr : Reply to LOG_ESI_EXT_GET_BOARD_INFO_RREQ
 *         Contains information needed to collect files for the ESI
 *
 * Data  : directorySize  Total size of the included files in the directory
 *                        in bytes.
 *       : path           Path to the directort. A NULL terminated string.
 *       : fruId          Specifies the board. A NULL terminated string.
 *       : ipAddress      Ip Address in string format.
 *                        A NULL terminated string.
 *       : port           An integer. Port to use when fetching the files
 *       : fruNameSpace to be used when fetching the files from the boards
 *       : numberOfDirectories
 *       : directoryInfo  Array of directoryInfo
 *
 *****************************************************************************/
#define LOG_ESI_EXT_GET_BOARD_INFO_RCFM (0x0191005)

typedef struct
{
  uint32_t directorySize;
  char     path[LOG_ESI_EXT_DIR_PATH_MAX_LENGTH];
} directoryInfoS;

typedef struct
{
   uint32_t       msgNo;
   char           fruId[LOG_ESI_EXT_MAX_PROD_NUMBER_LENGTH];
   char           ipAddress[LOG_ESI_EXT_MAX_IP_ADD_LENGTH];
   uint32_t       port;
   char           fruNameSpace[LOG_ESI_EXT_NAMESPACE_MAX_LENGTH];   
   uint32_t       numberOfDirectories;  
   directoryInfoS directoryInfo[1]; 
} LogEsiExtGetboardInfoRcfm;


/******************************************************************************
 *
 * Message Name: LOG_ESI_EXT_GET_BOARD_INFO_RREJ
 *
 * Descr      : Reply to LOG_ESI_EXT_GET_BOARD_INFO_RREQ
 *              Informs that no ESI is possible to generate for this board
 *
 * Data       : fruId       Specifies the board 
 *                          A NULL terminated string.
 *            : errorString Error information
 *                          A NULL terminated string.
 *
 *****************************************************************************/
#define LOG_ESI_EXT_GET_BOARD_INFO_RREJ (0x0191006)
typedef struct
{
   uint32_t  msgNo;
   char      fruId[LOG_ESI_EXT_MAX_PROD_NUMBER_LENGTH];
   char      errorString[LOG_ESI_EXT_ERROR_STRING_MAX_LENGTH];
} LogEsiExtGetBoardInfoRrej;


/******************************************************************************
 *
 * Message Name: LOG_ESI_EXT_GET_BOARD_INFO_READY_RUREQ
 *
 * Descr      : message to to CAT informing that the ESI is fetched
 *
 * Data       : fruId  Specifies the board 
 *                     A NULL terminated string.
 *
 *****************************************************************************/
#define LOG_ESI_EXT_GET_BOARD_INFO_READY_RUREQ (0x0191007)
typedef struct
{
   uint32_t  msgNo;
   char      fruId[LOG_ESI_EXT_MAX_PROD_NUMBER_LENGTH];
} LogEsiExtGetBoardInfoReadyRureq;





#ifdef __cplusplus
}
#endif

#endif /* LOG_ESI_SIG */

