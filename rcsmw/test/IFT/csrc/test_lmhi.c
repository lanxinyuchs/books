/* ----------------------------------------------------------------------
 * %CCaseFile:	test_lmhi.c %
 * %CCaseRev:	/main/R2A/R4A/R6A/1 %
 * %CCaseDate:	2016-06-17 %
 * %CCaseDocNo: %
 * Author:	erarafo
 * Author: <name>, <e-mail address>
 *
 * Short description:
 * Tests of LMHI use this module.
 * ----------------------------------------------------------------------
 * %CCaseTemplateFile:	template.c %
 * %CCaseTemplateId: CCver: /main/1 %
 *
 * %CCaseCopyrightBegin%
 * Copyright (c) Ericsson AB 2013-2016 All rights reserved.
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
 * R4A/1      2015-11-26 erarafo     Wrapper around TRI trace macros
 * R6A/1      2016-06-17 etxberb     Added Lmhi_get_lms_2 & Lmhi_get_lms_3.
 * ----------------------------------------------------------------------
 */
#include "master.h"
#include "appm_lmhi.h"

#include "cello_te_trace.h"

#define Lmhi_get_lms_no      1
#define Lmhi_free_buffers_no 2
#define Lmhi_start_pgm_no    3
#define Lmhi_stop_pgm_no     4

extern bool TRI_trace_enabled;

static void lmhi_error(ei_x_buff* resp, LmhiResultCode result);

ei_x_buff
send_sig_lmhi(void **ptr, int func, ei_x_buff args) {
  LmhiResultCode result;
  ei_x_buff resp;
  
  QTRACE3(1, "send_sig_lmhi func = %d\n", func);
  
  ei_x_new(&resp);
  
  switch (func) {
  case Lmhi_get_lms_no:
    {
      int arity, size;
      char *hwcategory;
      char *hwmodel;
      char *boardtype;
      char *boardrev;
      char *tag;
      long phase;
      LmhiGetLMsResult* lmhi_result_buffer;
      int type;

      if (ei_decode_tuple_header(args.buff, &args.index, &arity) == -1) {
	ei_x_format(&resp, "{error, badarg}");
	return resp;
      }

      switch (arity) {
      case 2:
	{
	  ei_get_type(args.buff, &args.index, &type, &size);
	  /* If it is an empty string, set boardtype to NULL */
	  if (size != 0)
	    boardtype = malloc(size+1);
	  else
	    boardtype = NULL;
	  if (ei_decode_string(args.buff, &args.index, boardtype) == -1) {
	    ei_x_format(&resp, "{error, {{badarg, boardtype}, Lmhi_get_lms}}");
	    free(boardtype);
	    return resp;
	  }
	  
	  ei_get_type(args.buff, &args.index, &type, &size);
	  tag = malloc(size+1);
	  if (ei_decode_string(args.buff, &args.index, tag) == -1) {
	    ei_x_format(&resp, "{error, {{badarg, tag}, Lmhi_get_lms}}");
	    free(tag);
	    free(boardtype);
	    return resp;
	  }
	  
	  *ptr = lmhi_result_buffer = malloc(sizeof(*lmhi_result_buffer));
	  result = Lmhi_get_lms(boardtype, tag, lmhi_result_buffer);
	  break;
	}
      case 4:
	{
	  ei_get_type(args.buff, &args.index, &type, &size);
	  /* If it is an empty string, set boardtype to NULL */
	  if (size != 0)
	    boardtype = malloc(size+1);
	  else
	    boardtype = NULL;
	  if (ei_decode_string(args.buff, &args.index, boardtype) == -1) {
	    ei_x_format(&resp, "{error, {{badarg, boardtype},Lmhi_get_lms_2}}");
	    free(boardtype);
	    return resp;
	  }
	  
	  ei_get_type(args.buff, &args.index, &type, &size);
	  /* If it is an empty string, set boardrev to NULL */
	  if (size != 0)
	    boardrev = malloc(size+1);
	  else
	    boardrev = NULL;
	  if (ei_decode_string(args.buff, &args.index, boardrev) == -1) {
	    ei_x_format(&resp, "{error, {{badarg, boardrev}, Lmhi_get_lms_2}}");
	    free(boardrev);
	    free(boardtype);
	    return resp;
	  }
	  
	  ei_get_type(args.buff, &args.index, &type, &size);
	  tag = malloc(size+1);
	  if (ei_decode_string(args.buff, &args.index, tag) == -1) {
	    ei_x_format(&resp, "{error, {{badarg, tag}, Lmhi_get_lms_2}}");
	    free(tag);
	    free(boardrev);
	    free(boardtype);
	    return resp;
	  }
	  
	  ei_get_type(args.buff, &args.index, &type, &size);
	  if (ei_decode_long(args.buff, &args.index, phase) == -1) {
	    ei_x_format(&resp, "{error, {{badarg, phase}, Lmhi_get_lms_2}}");
	    return resp;
	  }
	  
	  *ptr = lmhi_result_buffer = malloc(sizeof(*lmhi_result_buffer));
	  result = Lmhi_get_lms_2(boardtype, boardrev,
				  tag, phase,
				  lmhi_result_buffer);
	  break;
	}
      case 6:
	{
	  ei_get_type(args.buff, &args.index, &type, &size);
	  /* If it is an empty string, set hwcategory to NULL */
	  if (size != 0)
	    hwcategory = malloc(size+1);
	  else
	    hwcategory = NULL;
	  if (ei_decode_string(args.buff, &args.index, hwcategory) == -1) {
	    ei_x_format(&resp, "{error, {{badarg,hwcategory},Lmhi_get_lms_3}}");
	    free(hwcategory);
	    return resp;
	  }
	  
	  ei_get_type(args.buff, &args.index, &type, &size);
	  /* If it is an empty string, set hwmodel to NULL */
	  if (size != 0)
	    hwmodel = malloc(size+1);
	  else
	    hwmodel = NULL;
	  if (ei_decode_string(args.buff, &args.index, hwmodel) == -1) {
	    ei_x_format(&resp, "{error, {{badarg, hwmodel}, Lmhi_get_lms_3}}");
	    free(hwmodel);
	    free(hwcategory);
	    return resp;
	  }
	  
	  ei_get_type(args.buff, &args.index, &type, &size);
	  /* If it is an empty string, set boardtype to NULL */
	  if (size != 0)
	    boardtype = malloc(size+1);
	  else
	    boardtype = NULL;
	  if (ei_decode_string(args.buff, &args.index, boardtype) == -1) {
	    ei_x_format(&resp, "{error, {{badarg, boardtype},Lmhi_get_lms_3}}");
	    free(boardtype);
	    free(hwmodel);
	    free(hwcategory);
	    return resp;
	  }
	  
	  ei_get_type(args.buff, &args.index, &type, &size);
	  /* If it is an empty string, set boardrev to NULL */
	  if (size != 0)
	    boardrev = malloc(size+1);
	  else
	    boardrev = NULL;
	  if (ei_decode_string(args.buff, &args.index, boardrev) == -1) {
	    ei_x_format(&resp, "{error, {{badarg, boardrev}, Lmhi_get_lms_3}}");
	    free(boardrev);
	    free(boardtype);
	    free(hwmodel);
	    free(hwcategory);
	    return resp;
	  }
	  
	  ei_get_type(args.buff, &args.index, &type, &size);
	  tag = malloc(size+1);
	  if (ei_decode_string(args.buff, &args.index, tag) == -1) {
	    ei_x_format(&resp, "{error, {{badarg, tag}, Lmhi_get_lms_3}}");
	    free(tag);
	    free(boardrev);
	    free(boardtype);
	    free(hwmodel);
	    free(hwcategory);
	    return resp;
	  }
	  
	  ei_get_type(args.buff, &args.index, &type, &size);
	  if (ei_decode_long(args.buff, &args.index, phase) == -1) {
	    ei_x_format(&resp, "{error, {{badarg, phase}, Lmhi_get_lms_3}}");
	    return resp;
	  }
	  
	  *ptr = lmhi_result_buffer = malloc(sizeof(*lmhi_result_buffer));
	  result = Lmhi_get_lms_3(hwcategory, hwmodel,
				  boardtype, boardrev,
				  tag, phase,
				  lmhi_result_buffer);
	  break;
	}
      default:
	ei_x_format(&resp, "{error, {badarg, arity}}");
	return resp;
      }
      
      switch (result) {
      case LMHI_OK:
	{
	  unsigned int i, j;
	  
	  ei_x_encode_version(&resp);
	  ei_x_encode_tuple_header(&resp, 2);
	  ei_x_encode_atom(&resp, "ok");
	  ei_x_encode_list_header(&resp, lmhi_result_buffer->n_lmdata);
	  for (i = 0; i < lmhi_result_buffer->n_lmdata; i++) {
	    ei_x_encode_tuple_header(&resp, 4);
	    ei_x_encode_string(&resp, lmhi_result_buffer->lmdata[i].name);
	    ei_x_encode_string(&resp, lmhi_result_buffer->lmdata[i].id);
	    ei_x_encode_string(&resp, lmhi_result_buffer->lmdata[i].rev);
	    ei_x_encode_list_header(&resp, lmhi_result_buffer->lmdata[i].n_file);
	    for (j = 0; j < lmhi_result_buffer->lmdata[i].n_file; j++) {
	      ei_x_encode_tuple_header(&resp, 2);
	      ei_x_encode_string(&resp,
	  			 lmhi_result_buffer->lmdata[i].file[j].type);
	      ei_x_encode_string(&resp,
	  			 lmhi_result_buffer->lmdata[i].file[j].path);
	    }
	    ei_x_encode_empty_list(&resp);
	  }
	  ei_x_encode_empty_list(&resp);
	  break;
	}
      default:
	lmhi_error(&resp, result);
      }
      
      switch (arity) {
      case 6:
	{
	  free(hwcategory);
	  free(hwmodel);
	}
      case 4:
	{
	  free(boardrev);
	}
      case 2:
	{
	  free(boardtype);
	  free(tag);
	  break;
	}
      }
      
      return resp;
    }
  case Lmhi_free_buffers_no:
    Lmhi_free_buffers(*ptr);
    free(*ptr);

    ei_x_format(&resp, "ok");

    return resp;
  case Lmhi_start_pgm_no:
    {
      int arity, size, type, i;
      uint32_t duId, cpuSet;
      char *lmId;
      char *pgmName;
      char **argv;
      int argc;
      LmhiStartPgmResult startPgmRes;

      if (ei_decode_tuple_header(args.buff, &args.index, &arity) == -1 ||
	  arity != 5) {
	ei_x_format(&resp, "{error, badarg}");
	return resp;
      }

      if (ei_decode_ulong(args.buff, &args.index,
			  (long unsigned int *)&duId) == -1) {
	ei_x_format(&resp, "{error, badarg}");
	return resp;
      }

      if (ei_decode_ulong(args.buff, &args.index,
			  (long unsigned int *)&cpuSet) == -1) {
	ei_x_format(&resp, "{error, badarg}");
	return resp;
      }

      ei_get_type(args.buff, &args.index, &type, &size);
      lmId = malloc(size+1);
      if (ei_decode_string(args.buff, &args.index, lmId) == -1) {
	ei_x_format(&resp, "{error, badarg}");
	goto free_lmId;
      }

      ei_get_type(args.buff, &args.index, &type, &size);
      pgmName = malloc(size+1);
      if (ei_decode_string(args.buff, &args.index, pgmName) == -1) {
	ei_x_format(&resp, "{error, badarg}");
	goto free_pgmName;
      }

      ei_get_type(args.buff, &args.index, &type, &argc);
      argv = malloc((argc+1)*sizeof(*argv));
      for(i = 0; i < argc; i++)
	argv[i] = NULL;

      for(i = 0; i < argc; i++) {
	ei_get_type(args.buff, &args.index, &type, &size);
	argv[i] = malloc(size+1);
	if (ei_decode_string(args.buff, &args.index, argv[i]) == -1) {
	  ei_x_format(&resp, "{error, badarg}");
	  goto free_argv;
	}
      }
      argv[i] = NULL;

      result = Lmhi_start_pgm(duId, cpuSet, lmId, pgmName, argv, &startPgmRes);
      switch (result) {
      case LMHI_OK:
	ei_x_format(&resp, "{ok, ~u}", startPgmRes.pgmId);
	break;
      default:
	lmhi_error(&resp, result);
      }

    free_argv:
      for(i = 0; i < argc; i++)
	free(argv[i]);
      free(argv);

    free_pgmName:
      free(pgmName);

    free_lmId:
      free(lmId);

      return resp;
    }
  case Lmhi_stop_pgm_no:
    {
      int arity;
      uint32_t duId, pgmId;

      if (ei_decode_tuple_header(args.buff, &args.index, &arity) == -1 ||
	  arity != 2) {
	ei_x_format(&resp, "{error, badarg}");
	return resp;
      }

      if (ei_decode_ulong(args.buff, &args.index,
			  (long unsigned int *)&duId) == -1) {
	ei_x_format(&resp, "{error, badarg}");
	return resp;
      }

      if (ei_decode_ulong(args.buff, &args.index,
			  (long unsigned int *)&pgmId) == -1) {
	ei_x_format(&resp, "{error, badarg}");
	return resp;
      }

      result = Lmhi_stop_pgm(duId, pgmId);

      switch (result) {
      case LMHI_OK:
	ei_x_format(&resp, "{ok}");
	break;
      default:
	lmhi_error(&resp, result);
      }

      return resp;
    }
  }

  ei_x_format(&resp, "{ok, function_does_not_exist}");
  return resp;
}

static void
lmhi_error(ei_x_buff* resp, LmhiResultCode result)
{
  switch (result) {
  case LMHI_ERR_UNPACK_MSG:
    ei_x_format(resp, "{error, unpack_msg}");
    break;
  case LMHI_ERR_PEER_ERROR:
    ei_x_format(resp, "{error, peer_error}");
    break;
  case LMHI_ERR_NO_MEMORY:
    ei_x_format(resp, "{error, no_memory}");
    break;
  case LMHI_ERR_ZERO_LENGTH:
    ei_x_format(resp, "{error, zero_length}");
    break;
  default:
    ei_x_format(resp, "{error, unknown_error}");
  }
}
