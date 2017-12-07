/* ----------------------------------------------------------------------
 * %CCaseFile:	test_seci.c %
 * %CCaseRev:	/main/R3A/R5A/R6A/2 %
 * %CCaseDate:	2016-06-20 %
 * %CCaseDocNo: %
 * Author:	etxasta
 * Author: <name>, <e-mail address>
 *
 * Short description:
 * <Some rows here>
 * ----------------------------------------------------------------------
 * %CCaseTemplateFile:	template.c %
 * %CCaseTemplateId: CCver: /main/1 %
 *
 * %CCaseCopyrightBegin%
 * Copyright (c) Ericsson AB 2014-2016 All rights reserved.
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
 * Rev      Date        Name        What
 * -------  ----------  --------    -------------------------------------
 * R2A/3    2014-11-18  etxasta     Created
 * R5A      2016-01-25  ehsake      Added test for seci_get_vc
 * R6A/1    2016-06-16  etxasta     Added test for seci_verify_peer
 * ----------------------------------------------------------------------
 */

#include <stdint.h>
#include "cert_seci.h"

#include "master.h"

/* must match $RCS_TOP/CERT/CERT_CNX9012719/test/suites/test_seci.hrl */
#define SECI_GET_CERT      1
#define SECI_READ          2
#define SECI_WRITE         3
#define SECI_DELETE        4
#define SECI_LOG           5
#define SECI_SUB_INIT      6
#define SECI_ADD_SUB       7
#define SECI_DEL_SUB       8
#define SECI_GET_SUB_EVENT 9
#define SECI_GET_VC        10
#define SECI_GET_NC        11
#define SECI_GET_TCAT      12
#define SECI_VERIFY_PEER   13


/*
 * Translates numeric result code to name.
 *
 */
static char*
getResult(SeciResultT result) {
    switch (result) {
        case SECI_OK:
            return "SECI_OK";
        case SECI_NOT_FOUND:
            return "SECI_NOT_FOUND";
        case SECI_ERROR:
            return "SECI_ERROR";
        case SECI_INVALID_PARAMETER_RESULTP:
            return "SECI_INVALID_PARAMETER_RESULTP";
        case SECI_SEND_ERROR:
            return "SECI_SEND_ERROR";
        case SECI_RECEIVE_ERROR:
            return "SECI_RECEIVE_ERROR";
        case SECI_DELETE_FAILED_WRONG_ID:
            return "SECI_DELETE_FAILED_WRONG_ID";
        case SECI_UNINITIALIZED_SUB:
            return "SECI_UNINITIALIZED_SUB";
        case SECI_VERIFY_PEER_VALID:
            return "SECI_VERIFY_PEER_VALID";
        case SECI_VERIFY_PEER_NOT_VALID:
            return "SECI_VERIFY_PEER_NOT_VALID";
        case SECI_VERIFY_PEER_UNKNOWN:
            return "SECI_VERIFY_PEER_UNKNOWN";
        default:
            return "unknown result code";
    }
}

ei_x_buff
send_sig_seci(int func, ei_x_buff args) {
  ei_x_buff resp;
  int type, size;
  int arity;
  char* dn   = NULL;
  char* outData = NULL;

  ei_x_new(&resp);

  ei_decode_tuple_header(args.buff, &args.index, &arity);

  switch (func) {
  case SECI_GET_VC:
      {
          SeciResultT result;
          ei_get_type(args.buff, &args.index, &type, &size);
          dn = malloc(size + 1);
          ei_decode_string(args.buff, &args.index, dn);
          outData = NULL;
          result = seci_get_vc(dn, &outData);
          free(dn);
          if (result == SECI_OK) {
              ei_x_encode_version(&resp);
              ei_x_encode_tuple_header(&resp, 3);
              ei_x_encode_atom(&resp, "ok");
              ei_x_encode_long(&resp, result);
              ei_x_encode_string(&resp,outData);
              ei_x_encode_empty_list(&resp);
          } else {
              ei_x_format(&resp, "{ok, ~i, ~s}", result,
                      getResult(result));
          }
          seci_free(outData);
          break;
      }

  case SECI_GET_NC:
    {
          SeciResultT result;
          ei_get_type(args.buff, &args.index, &type, &size);
          dn = malloc(size + 1);
          ei_decode_string(args.buff, &args.index, dn);
          outData = NULL;
          char* ptr;
          result = seci_get_nc(dn, &outData);
          free(dn);
          if (result == SECI_OK) {
              ei_x_encode_version(&resp);
              ei_x_encode_tuple_header(&resp, 3);
              ei_x_encode_atom(&resp, "ok");
              ei_x_encode_long(&resp, result);
              ptr = outData;
              while (1) {
                  unsigned int len = *(unsigned int*)ptr;
                  if (len == 0)
                        break;
                  ptr+=sizeof (unsigned int);
                  ei_x_encode_list_header(&resp, 1);
                  ei_x_encode_binary(&resp, ptr, len);
                  ptr+=len;
              }
              ei_x_encode_empty_list(&resp);
          } else {
              ei_x_format(&resp, "{ok, ~i, ~s}", result,
                      getResult(result));
          }
          seci_free(outData);
          break;
      }

  case SECI_GET_TCAT:
    {
          SeciResultT result;
          ei_get_type(args.buff, &args.index, &type, &size);
          dn = malloc(size + 1);
          ei_decode_string(args.buff, &args.index, dn);
          outData = NULL;
          char* ptr;
          result = seci_get_tcat(dn, &outData);
          free(dn);
          if (result == SECI_OK) {
              ei_x_encode_version(&resp);
              ei_x_encode_tuple_header(&resp, 3);
              ei_x_encode_atom(&resp, "ok");
              ei_x_encode_long(&resp, result);
              ptr = outData;
              ei_x_encode_long(&resp, *(unsigned int*)ptr);
              ptr+=sizeof (unsigned int);
              while (1) {
                  unsigned int len = *(unsigned int*)ptr;
                  if (len == 0)
                        break;
                  ptr+=sizeof (unsigned int);
                  ei_x_encode_list_header(&resp, 1);
                  ei_x_encode_binary(&resp, ptr, len);
                  ptr+=len;
              }
              ei_x_encode_empty_list(&resp);
          } else {
              ei_x_format(&resp, "{ok, ~i, ~s}", result,
                      getResult(result));
          }
          seci_free(outData);
          break;
      }

  case SECI_GET_CERT:
    {
          SeciResultT result;
          ei_get_type(args.buff, &args.index, &type, &size);
          dn = malloc(size + 1);
          ei_decode_string(args.buff, &args.index, dn);
          outData = NULL;
          char* ptr;
          result = seci_get_cert(dn, &outData);
          free(dn);
          if (result == SECI_OK) {
              ei_x_encode_version(&resp);
              ei_x_encode_tuple_header(&resp, 3);
              ei_x_encode_atom(&resp, "ok");
              ei_x_encode_long(&resp, result);
              ptr = outData;
              while (1) {
                  unsigned int len = *(unsigned int*)ptr;
                  if (len == 0)
                        break;
                  ptr+=sizeof (unsigned int);
                  ei_x_encode_list_header(&resp, 1);
                  ei_x_encode_binary(&resp, ptr, len);
                  ptr+=len;
              }
              ei_x_encode_empty_list(&resp);
          } else {
              ei_x_format(&resp, "{ok, ~i, ~s}", result,
                      getResult(result));
          }
          seci_free(outData);
          break;
      }
   case SECI_READ:
      { 
          SeciResultT result;
          char *id;
          char *index;
          char* ptr;
          ei_get_type(args.buff, &args.index, &type, &size);
          id = malloc(size+1);
          ei_decode_string(args.buff, &args.index, id);
          ei_get_type(args.buff, &args.index, &type, &size);
          index = malloc(size+1);
          ei_decode_string(args.buff, &args.index, index);
          result = seci_read(id, index, &outData);
          free(id);
          free(index);
          if (result == SECI_OK) {
              ei_x_encode_version(&resp);
              ei_x_encode_tuple_header(&resp, 3);
              ei_x_encode_atom(&resp, "ok");
              ei_x_encode_long(&resp, result);
              ptr = outData;
              unsigned int len = *(unsigned int*)ptr;
              ptr+=sizeof (unsigned int);
              ei_x_encode_list_header(&resp, 1);
              ei_x_encode_binary(&resp, ptr, len);
              ei_x_encode_empty_list(&resp);
          } else {
              ei_x_format(&resp, "{ok, ~i, ~s}", result,
                      getResult(result));
          }
          free(outData);
          break;
      }
    case SECI_WRITE:
      {
          SeciResultT result;
          char *id;
          char *index;
          char *data;
          char *sendData;
          long dataSize;
          // id
          ei_get_type(args.buff, &args.index, &type, &size);
          id = malloc(size+1);
          ei_decode_string(args.buff, &args.index, id);
          // index
          ei_get_type(args.buff, &args.index, &type, &size);
          index = malloc(size+1);
          ei_decode_string(args.buff, &args.index, index);
          // data
          ei_get_type(args.buff, &args.index, &type, &size);
          data = sendData = malloc(size + 4);
          *(int*)data = size;
          data+=4;
          ei_decode_binary(args.buff, &args.index, data, &dataSize);
          result = seci_write(id, index, sendData);
          free(id);
          free(index);
          free(sendData);
          if (result == SECI_OK) {
              ei_x_encode_version(&resp);
              ei_x_encode_tuple_header(&resp, 2);
              ei_x_encode_atom(&resp, "ok");
              ei_x_encode_long(&resp, result);
          } else {
              ei_x_format(&resp, "{ok, ~i, ~s}", result,
                      getResult(result));
          }
          break;
      }
    case SECI_DELETE:
      {
          SeciResultT result;
          char *id;
          char *index;
          ei_get_type(args.buff, &args.index, &type, &size);
          id = malloc(size+1);
          ei_decode_string(args.buff, &args.index, id);
          ei_get_type(args.buff, &args.index, &type, &size);
          index = malloc(size+1);
          ei_decode_string(args.buff, &args.index, index);
          result = seci_delete(id, index);
          free(id);
          free(index);
          if (result == SECI_OK) {
              ei_x_encode_version(&resp);
              ei_x_encode_tuple_header(&resp, 2);
              ei_x_encode_atom(&resp, "ok");
              ei_x_encode_long(&resp, result);
          } else {
              ei_x_format(&resp, "{ok, ~i, ~s}", result,
                      getResult(result));
          }
          break;
      }
    case SECI_LOG:
      {
          SeciResultT result;
          SeciLogTypeT logType;
          int  facility;
          SeciSeverityT severity;
          char *message;
          ei_decode_long(args.buff, &args.index, (long*)&logType);
          ei_decode_long(args.buff, &args.index, (long*)&facility);
          ei_decode_long(args.buff, &args.index, (long*)&severity);
          ei_get_type(args.buff, &args.index, &type, &size);
          message = malloc(size+1);
          ei_decode_string(args.buff, &args.index, message);
          result = seci_log(logType, facility, severity, message);
          free(message);
          if (result == SECI_OK) {
              ei_x_encode_version(&resp);
              ei_x_encode_tuple_header(&resp, 2);
              ei_x_encode_atom(&resp, "ok");
              ei_x_encode_long(&resp, result);
          } else {
              ei_x_format(&resp, "{ok, ~i, ~s}", result,
                      getResult(result));
          }
          break;
      }
  case SECI_SUB_INIT:
      {
          SeciHandleT result;
          char *id;
          char *dn;
          char* ptr;
          ei_get_type(args.buff, &args.index, &type, &size);
          id = malloc(size+1);
          ei_decode_string(args.buff, &args.index, id);
          ei_get_type(args.buff, &args.index, &type, &size);
          dn = malloc(size+1);
          ei_decode_string(args.buff, &args.index, dn);
          /* Initialize  subscription */
          result = seci_sub_initialize(id);
          if (result == -1) {
              free(id);
              free(dn);
          ei_x_encode_version(&resp);
          ei_x_encode_tuple_header(&resp, 3);
          ei_x_encode_atom(&resp, "ok");
          ei_x_encode_long(&resp, result);
          break;
          } 
          
          /* Add subscription */
          result = seci_add_sub(id, dn);
          if (result != SECI_OK) {
              free(id);
              free(dn);
              ei_x_encode_version(&resp);
              ei_x_encode_tuple_header(&resp, 3);
              ei_x_encode_atom(&resp, "ok");
              ei_x_encode_long(&resp, -2);
              break;
          }

          /* Get subscription event */
          result = seci_get_sub_event(id, &outData);
          if (result != SECI_OK) {
              free(outData);
              free(id);
              free(dn);
              ei_x_encode_version(&resp);
              ei_x_encode_tuple_header(&resp, 3);
              ei_x_encode_atom(&resp, "ok");
              ei_x_encode_long(&resp, -3);
              break;
          }
                    
          /* Delete subscription */
          result = seci_del_sub(id, dn);
          if (result != SECI_OK) {
              free(id);
              free(dn);
              free(outData);
              ei_x_encode_version(&resp);
              ei_x_encode_tuple_header(&resp, 3);
              ei_x_encode_atom(&resp, "ok");
              ei_x_encode_long(&resp, -4);
              break;
          }
          ei_x_encode_version(&resp);
          ei_x_encode_tuple_header(&resp, 3);
          ei_x_encode_atom(&resp, "ok");
          ei_x_encode_long(&resp, result);
          ptr = outData;
          while (1) {
              unsigned int len = *(unsigned int*)ptr;
              if (len == 0)
                  break;
              ptr+=sizeof (unsigned int);
              ei_x_encode_list_header(&resp, 1);
              ei_x_encode_binary(&resp, ptr, len);
              ptr+=len;
          }
          ei_x_encode_empty_list(&resp);
          free(id);
          free(dn);
          free(outData);
          break;
      }
  case SECI_VERIFY_PEER:
      {
          SeciResultT result;
          char *index;
          char *peer;
          char *sendPeer;
          long peerSize;
          // index
          ei_get_type(args.buff, &args.index, &type, &size);
          index = malloc(size+1);
          ei_decode_string(args.buff, &args.index, index);
          // peer
          ei_get_type(args.buff, &args.index, &type, &size);
          peer = sendPeer = malloc(size + 4);
          *(int*)peer = size;
          peer+=4;
          ei_decode_binary(args.buff, &args.index, peer, &peerSize);
          result = seci_verify_peer(index, sendPeer);
          free(index);
          free(sendPeer);
          ei_x_encode_version(&resp);
          ei_x_encode_tuple_header(&resp, 2);
          ei_x_encode_atom(&resp, "ok");
          ei_x_encode_long(&resp, result);
          break;
      }
  default:
      {
          ei_x_format(&resp, "{ok, ~s, ~i}", "function not supported", func);
      }
  }
  return resp;
}

