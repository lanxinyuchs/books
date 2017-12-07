#ifndef APPM_LMHI_INCLUDED
#define APPM_LMHI_INCLUDED

/* ----------------------------------------------------------------------
 * %CCaseFile:	appm_lmhi.h %
 * %CCaseRev:	/main/R1A/R2A/R3A/R4A/R5A/R6A/R8A/R10A/R11A/1 %
 * %CCaseDate:	2017-09-28 %
 * %CCaseDocNo: %
 * Author:	etxarnu
 * Author: Arto Nummelin, <arto.nummelin@ericsson.com>
 *
 * Short description:
 * This file contains type definitions for the LMHI interface.
 * ----------------------------------------------------------------------
 * %CCaseTemplateFile:  template.h %
 * %CCaseTemplateId: CCver: /main/1 %
 *
 * %CCaseCopyrightBegin%
 * Copyright (c) Ericsson AB 2012-2017 All rights reserved.
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
 * R1A/2      2012-08-17   etxarnu     Created
 * R2A/1      2013-01-09   etxarnu     Added #ifdef __cplusplus
 * R2A/2      2013-01-10   etxarnu     Just a dummy change to force re-release
 * R2A/5      2014-01-10   etxarnu     Added Lmhi_signal_to_pgm and new
 *                                     LMHI_ERR_SIG_NO result code
 * R3A/1      2014-11-27   etxarnu     Added Lmhi_get_lms_2
 *                                     Added Lmhi_start_pgm_2
 * R5A/1      2016-03-10   etxarnu     Added Lmhi_get_pids/Lmhi_free_pids
 * R6A/1      2016-06-16   etxberb     Added Lmhi_get_lms_3 (addition of
 *                                     hwcategory and hwmodel on top of
 *                                     Lmhi_get_lms_2).
 * R8A/1      2016-12-15   etxarnu     Added Lmhi_add_board/3
 * R10A/1     2017-05-30   uabesvi     Added Lmhi_add_ext_board/3
 * R11A/1     2017-09-13   etxarnu     Added Lmhi_start_pgm_3
 * ----------------------------------------------------------------------
 */



#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>

/* --- enums --- */

typedef enum  {
  LMHI_OK = 0,
  LMHI_ERR_UNPACK_MSG = 1,
  LMHI_ERR_PEER_ERROR = 2,
  LMHI_ERR_NO_MEMORY = 3,
  LMHI_ERR_ZERO_LENGTH = 4,
  LMHI_ERR_SIG_NO = 5,
  LMHI_ERR_CEC_OPEN = 6,
  LMHI_ERR_CEC_SEND = 7,
  LMHI_ERR_RECV_TMO = 8,
  LMHI_ERR_NO_DATA = 9,
  LMHI_WAIT = 10
} LmhiResultCode;

typedef enum  {
  LMHI_PHASE_NORMAL = 0,
  LMHI_PHASE_PRELOAD = 1
} LmhiPhase;

/* --- structs --- */

typedef struct
{
  char *type;
  char *path;
} LmhiFileInfo;

typedef struct
{
  char * name;
  char * id;
  char * rev;
  size_t n_file;
  LmhiFileInfo *file;
} LmhiLmData;

typedef struct
{
  size_t n_lmdata;
  LmhiLmData *lmdata;
} LmhiGetLMsResult;

typedef struct
{
  uint32_t pgmId;
} LmhiStartPgmResult;

typedef struct
{
  size_t   n_pids;
  uint32_t *pdata;
} LmhiGetPidsResult;

/* --- functions --- */

LmhiResultCode Lmhi_add_board(char *boardNo,  
			      char *boardRev, 
			      uint32_t mboxId);

LmhiResultCode Lmhi_add_ext_board(char *boardNo,
				  char *boardRev, 
				  uint32_t mboxId);

LmhiResultCode Lmhi_delete_ext_board(char *boardNo,
				     char *boardRev, 
				     uint32_t mboxId);

LmhiResultCode Lmhi_get_lms(char *boardtype,  char *tag,
			    LmhiGetLMsResult* lmhi_result_buffer);

LmhiResultCode Lmhi_get_lms_2(char *boardtype, char *boardrev,  
			      char *tag, LmhiPhase phase,
			      LmhiGetLMsResult* lmhi_result_buffer);

LmhiResultCode Lmhi_get_lms_3(char *hwcategory, char *hwmodel,
			      char *boardtype, char *boardrev,  
			      char *tag, LmhiPhase phase,
			      LmhiGetLMsResult* lmhi_result_buffer);

void Lmhi_free_buffers(LmhiGetLMsResult *lmhi_result_buffer);

LmhiResultCode Lmhi_start_pgm(uint32_t duId, uint32_t cpuSet, char *lmId,
			      char *pgmName, char *const argv[],
			      LmhiStartPgmResult *startPgmRes);

LmhiResultCode Lmhi_start_pgm_2(uint32_t duId, uint32_t cpuSet, char *lmId,
				char *pgmName, char *nameSpace,
				char *const argv[], 
				LmhiStartPgmResult *startPgmRes);

LmhiResultCode Lmhi_start_pgm_3(uint32_t duId, uint32_t cpuSet, char *lmId,
				char *pgmName, char *nameSpace,
				uint32_t mboxId,
				char *const argv[], 
				LmhiStartPgmResult *startPgmRes);

LmhiResultCode Lmhi_stop_pgm(uint32_t duId, uint32_t pgmId);

LmhiResultCode Lmhi_heartbeat(char *pgmName, char *lmId);

LmhiResultCode Lmhi_signal_to_pgm(uint32_t duId, uint32_t pgmId, uint32_t sigNo);

LmhiResultCode Lmhi_get_pids(uint32_t duId, uint32_t pgmId,
			     LmhiGetPidsResult* lmhi_result_buffer );

void Lmhi_free_pids(LmhiGetPidsResult* lmhi_result_buffer);
#ifdef __cplusplus
}
#endif

#endif
