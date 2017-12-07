#ifndef LMHI_SIG
#define LMHI_SIG

/* ----------------------------------------------------------------------
 * %CCaseFile:	appm_lmhi.sig %
 * %CCaseRev:	/main/R8A/R10A/R11A/2 %
 * %CCaseDate:	2017-09-28 %
 * %CCaseDocNo: %
 * Author:	etxarnu
 * Author: <name>, <e-mail address>
 *
 * Short description:
 * <Some rows here>
 * ----------------------------------------------------------------------
 * %CCaseTemplateFile:	template.c %
 * %CCaseTemplateId: CCver: /main/1 %
 *
 * %CCaseCopyrightBegin%
 * Copyright (c) Ericsson AB 2016-2017 All rights reserved.
 * 
 * The information in this document is the property of Ericsson.
 * 
 * Except as specifically authorized in writing by Ericsson, the 
 * receiver of this document shall keep the information contained 
 * herein confidential and shall protect the same in whole or in 
 * part from disclosure and dissemination to third parties.
 * 
 * Disclosure and disseminations to the receivers employees shall 
 * only be made on a strict need to know basis.
 * %CCaseCopyrightEnd%
 *
 * ----------------------------------------------------------------------
 *
 * Revision history:
 *
 * Rev        Date       Name        What
 * -----      -------    --------    --------------------------
 * R8A/1      2016-12-16 etxarnu     Created
 * R10A/1     2017-05-30 uabesvi     Added singals for external board ESI handling
 * R11A/1     2017-07-14 uabesvi     Moved external board ESI handling to LOG
 * R11A/2     2017-09-13 etxarnu     Added signals for program crash/termination
 * ----------------------------------------------------------------------
 */

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif


#define LMHI_SIG_BASE 0x01860000
#define LMHI_MAX_PRODUCT_NUMBER_LEN   25
#define LMHI_MAX_PRODUCT_REVISION_LEN 8
#define LMHI_MAX_REASON_LEN           64

/******************************************************************************
 * Signal range: 0x01860000 - 0x01860006
 ******************************************************************************/

/******************************************************************************
 * REQUESTS
 ******************************************************************************/


/******************************************************************************
 * RESPONSES
 ******************************************************************************/

/******************************************************************************
 *
 * Message Name: LMHI_ADD_BOARD_RSP
 *
 * Descr   : Message from service which indicates successful board addition
 *
 * Data	   : Board added
 *
 *****************************************************************************/
#define LMHI_ADD_BOARD_RSP (LMHI_SIG_BASE + 0)
typedef struct
{
   uint32_t  msgno;
   char  BoardNo[LMHI_MAX_PRODUCT_NUMBER_LEN];
   char  BoardRev[LMHI_MAX_PRODUCT_REVISION_LEN];
} LmhiAddBoardRsp_t;

/******************************************************************************
 *
 * Message Name: LMHI_ADD_BOARD_REJ
 *
 * Descr   : Message from service which indicates unsuccessful board addition
 *
 * Data	   : Board not added and reason for failure
 *
 *****************************************************************************/
#define LMHI_ADD_BOARD_REJ (LMHI_SIG_BASE + 1)
typedef struct
{
   uint32_t  msgno;
   char  BoardNo[LMHI_MAX_PRODUCT_NUMBER_LEN];
   char  BoardRev[LMHI_MAX_PRODUCT_REVISION_LEN];
   char  RejReason[LMHI_MAX_REASON_LEN];
} LmhiAddBoardRej_t;


/******************************************************************************
 *
 * Message Name: LMHI_ADD_EXT_BOARD_RSP
 *
 * Descr   : Message from service which indicates successful external board addition
 *
 * Data	   : External board added
 *
 *****************************************************************************/
#define LMHI_ADD_EXT_BOARD_RSP (LMHI_SIG_BASE + 2)
typedef struct
{
   uint32_t  msgno;
   char  BoardNo[LMHI_MAX_PRODUCT_NUMBER_LEN];
   char  BoardRev[LMHI_MAX_PRODUCT_REVISION_LEN];
} LmhiAddExtBoardRsp_t;

/******************************************************************************
 *
 * Message Name: LMHI_DELETE_EXT_BOARD_RSP
 *
 * Descr   : Message from service which indicates successful board deletion
 *
 * Data	   : External board deleted
 *
 *****************************************************************************/
#define LMHI_DELETE_EXT_BOARD_RSP (LMHI_SIG_BASE + 3)
typedef struct
{
   uint32_t  msgno;
   char  BoardNo[LMHI_MAX_PRODUCT_NUMBER_LEN];
   char  BoardRev[LMHI_MAX_PRODUCT_REVISION_LEN];
} LmhiDeleteExtBoardRsp_t;


/******************************************************************************
 *
 * Message Name: LMHI_PGM_TERM_IND
 *
 * Descr   : Message from service which indicates a program termination
 *
 * Data	   : Termination reason
 *
 *****************************************************************************/
#define LMHI_PGM_TERM_IND (LMHI_SIG_BASE + 4)
typedef struct
{
   uint32_t  msgno;
   uint32_t  pgmId;
   uint32_t  reason;
} LmhiPgmTermInd_t;

/******************************************************************************
 *
 * Message Name: LMHI_PGM_CRASH_IND
 *
 * Descr   : Message from service which indicates a program crash
 *
 * Data	   : crash reason
 *
 *****************************************************************************/
#define LMHI_PGM_CRASH_IND (LMHI_SIG_BASE + 5)
typedef struct
{
   uint32_t  msgno;
   uint32_t  pgmId;
} LmhiPgmCrashInd_t;



/*****************************************************************************/

union itc_msg {
  uint32_t                 msgno;
  LmhiAddBoardRsp_t        LmhiAddBoardRsp;
  LmhiAddBoardRej_t        LmhiAddBoardRej;
  LmhiAddExtBoardRsp_t     LmhiAddExtBoardRsp;
  LmhiDeleteExtBoardRsp_t  LmhiDeleteExtBoardRsp;
  LmhiPgmTermInd_t  	   LmhiPgmTermInd;
  LmhiPgmCrashInd_t  	   LmhiPgmCrashInd;
};	    
    	    
#ifdef __cplusplus
}
#endif

#endif /* LMHI_SIG */
