/* ----------------------------------------------------------------------
 * %CCaseFile:	cstnn.c %
 * %CCaseRev:	/main/R3A/R8A/R9A/R10A/R11A/3 %
 * %CCaseDate:	2017-09-08 %
 * %CCaseDocNo: %
 * Author:      
 * Author: <name>, <e-mail address>
 *
 * Short description:
 * CTN - CS TN Net, currently used to perform DN -> Network Namespace mapping
 * i.e. Cs has the OaMaccespoint pointing to something and need to know the
 * the corresponding networknamespace (if used).
 * ----------------------------------------------------------------------
 * %CCaseTemplateFile:  template.c %
 * %CCaseTemplateId: CCver: /main/1 %
 *
 * %CCaseCopyrightBegin%
 * Copyright (c) Ericsson AB 2014-2017 All rights reserved.
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
 * R3A/1      2014-10-05 etxlg       Created
 * R3A/2      2014-10-09 etxlg       Continued
 * R3A/3      2014-10-12 etxlg       Namechanged headerfile
 * R10A/1     2017-05-09 ecaiyan     Support initialize3
 * R10A/9     2017-07-11 egabing     Removed cstn version 1
 * R11A/1     2017-08-02 eolaand     Add LTTng trace
 * ----------------------------------------------------------------------
 */

#define _GNU_SOURCE
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <unistd.h>
#include <string.h>
#include "cec.h"
#include "cstnn.h"

#define TRACEPOINT_DEFINE
#include "cstnn_trace.h"

#define INFO( fmt,...)  TRACE_HELPER(info_trace, fmt, __VA_ARGS__)
#define ERROR( fmt,...) TRACE_HELPER(error_trace, fmt, __VA_ARGS__)
#define DEBUG( fmt,...) TRACE_HELPER(debug_trace, fmt, __VA_ARGS__)

#define TRACE_HELPER(type, fmt,...) do {				\
    char *err_str;							\
    asprintf(&err_str,"%s:%d:%s():" fmt, __FILE__,__LINE__, __func__, ##__VA_ARGS__); \
    tracepoint(com_ericsson_rcs_cstn, type , err_str);			\
    free(err_str);							\
  } while(0)

#define SIGNATURE {'C', 'S', 'T', 'N', 'N', 'E', 'T', 'N', 'S'}
#define CEC_REC_TMO 3000

#define CSTN_ERROR -1
#define CSTN_OK 0

typedef enum {
  INITIALIZE_1   = 1,
  NAME_SPACE_1   = 2,
  INITIALIZE_2   = 3,
  NAME_SPACE_2   = 4,
  UPDATE         = 5,
  INITIALIZE_3   = 6,
  DoOamDNtoNsName   = 7,
  DoOamDNtoNsNameAndIp   = 8,
  DoUnsubscribe   = 9
} FunctionCodeT;


/* global connection information, only ONE connection is supported */
static cec_handle_t    *thehandle = NULL;
static CsTnCallbacks2T *thecallbacks2 = NULL;
static CsTnCallbacks3T *thecallbacks3 = NULL;

static int version = 1;


CsTnHandleT
CsTnFinishInitialize(cec_packet_t *packet) {

  if (packet)
    free(packet->data);

  if (thehandle) {
    cec_close(thehandle);
    thehandle = NULL;
  }

  return CSTN_ERROR;
 }


/*
 * Initialize version 2 or 3 
 *
 * (This function contains the shared message sending code)
 */
static CsTnHandleT
CsTnSendInitializeMessage(int init_code)
{
  
  char signature[] = SIGNATURE;
  
  if(thehandle != NULL){
    ERROR("%s", "CsTnInitialize() called on already initialized API");
    fprintf(stderr, "cstnn: Warning, initialize called on already initialized API\n");
    cec_close(thehandle);
  }
  thehandle = cec_open(signature, sizeof signature);
  if(thehandle == NULL){
    ERROR("%s", "Unable to initialize API");
    fprintf(stderr, "cstnn: Error, unable to initialize API\n");
    INFO("Exit %d", CSTN_ERROR);
    return CSTN_ERROR;
  }
  
  /*
   *===========================================
   * encode inialize 2 or 3 message
   *===========================================
   */
  
  cec_packet_t packet = {.data=NULL};
  char *ptr;
  
  char *buffer = calloc(2, sizeof(uint32_t));
  
  packet.length = 2*sizeof(uint32_t);
  ptr = packet.data = buffer;
  
  // function code
  *(uint32_t*)ptr = init_code;
  ptr+= sizeof(uint32_t);
  
  if (cec_send(thehandle, &packet) < 0) {
    ERROR("%s", "Could not send INITIALIZE message");
    fprintf(stderr, "cstnn: ERROR. Could not send initialize message\n");
    INFO("Exit %d", CSTN_ERROR);
    return CsTnFinishInitialize(&packet);
  }
  
  return thehandle->socket;
}

/*
 * Initialize version 2 
 *
 * callbacks2 specifies the application function to be invoked
 */
CsTnHandleT
CsTnInitialize2(CsTnCallbacks2T *callbacks2)
{
  INFO("%s", "Enter");
  version = 2;
  CsTnHandleT result = CsTnSendInitializeMessage(INITIALIZE_2);
  if (result == CSTN_ERROR) {
  	return result;
  }
  thecallbacks2 = callbacks2;

  INFO("Exit %u", result);
  return result;
}

/*
 * Initialize version 3
 *
 * callbacks3 specifies the application function to be invoked
 */
CsTnHandleT
CsTnInitialize3(CsTnCallbacks3T *callbacks3)
{
  INFO("%s", "Enter");
  version = 3;
  CsTnHandleT result = CsTnSendInitializeMessage(INITIALIZE_3);
  if (result == CSTN_ERROR) {
  	return result;
  }
  thecallbacks3 = callbacks3;
  
  INFO("Exit %u", result);
  return result;
}


CsTnResult cstnFinishUpdate(CsTnResult result, char *buffer) {

  free(buffer);
  return result;

}


/*
 * Reply back the result 
 *  NameSpace if version 1 is used,
 *  NameSpace and IpAddress in case of version 2
 *
 * This function is also used for updates of the NameSpace/IpAddress
 * when using version 2
 *
 * Function code specifies the reply message
 */

CsTnResult 
CsTnNameSpace(int functionCode, const char *ldn, const CsTnInfoT *cstnInfo)
{
  cec_packet_t packet = {.data=NULL};
  char *ptr;

  /*
   *===========================================
   * count the total length of CsTnUpdate
   *===========================================
   */
  int totLen = 0;
  
  // function code
  totLen = totLen + sizeof(uint32_t);
  
  // length of LDN
  totLen = totLen + sizeof(uint32_t);
  
  // LDN
  totLen = totLen + strlen(ldn);
  
  // length of Name space
  totLen = totLen + sizeof(uint32_t);
  
  // name space
  totLen = totLen + CSTN_NSNAME_STRLEN;
  
  // length of Ip address
  totLen = totLen + sizeof(uint32_t);
  
  // IP address
  totLen = totLen + INET6_ADDRSTRLEN;


  /*
   *===========================================
   * encode the Update message
   *===========================================
   */
  
  int strlength;
  
  char *buffer = calloc(totLen, 8);
  
  packet.length = totLen + sizeof(uint32_t);
  ptr = packet.data = buffer;
  
  // function code
  *(uint32_t*)ptr = functionCode;
  ptr+= sizeof(uint32_t);
  
  // length of LDN
  strlength = strlen(ldn);
  *(uint32_t*)ptr = strlength;
  ptr+= sizeof(uint32_t);
  
  // LDN
  memcpy(ptr, ldn, strlength);
  ptr+= strlength;
  
  // length of Name space
  *(uint32_t*)ptr = CSTN_NSNAME_STRLEN;
  ptr+= sizeof(uint32_t);
  
  // Name space
  //  memcpy(ptr, tnInfo->NsName, strlength);
  memcpy(ptr, cstnInfo->NsName, CSTN_NSNAME_STRLEN);
  ptr+= CSTN_NSNAME_STRLEN;
  
  // length of IP address
  *(uint32_t*)ptr = INET6_ADDRSTRLEN;
  ptr+= sizeof(uint32_t);
  
  // IP address
  //  memcpy(ptr, tnInfo->IpAddress, strlength);
  memcpy(ptr, cstnInfo->IpAddress, INET6_ADDRSTRLEN);
  ptr+= INET6_ADDRSTRLEN;
  
  //===========================================
  // Send the Update message
  //===========================================
  


  CsTnResult result;

  if (cec_send(thehandle, &packet) < 0) {
    result =  CSTN_ERROR;
  } else {
    result =  CSTN_OK;
  }

  return cstnFinishUpdate(result, buffer);
}



CsTnResult 
CsTnGetNameSpace_2(const char *ldn, const CsTnInfoT *cstnInfo)
{
  return CsTnNameSpace(NAME_SPACE_2, ldn, cstnInfo);
}


CsTnResult 
CsTnUpdate(const char *ldn, const CsTnInfoT *cstnInfo)
{
  INFO("Enter ldn = %s, NsName = %s, IpAddress = %s", ldn, cstnInfo->NsName,
       cstnInfo->IpAddress);
  int len;
  CsTnInfoT tmp;

  len = strnlen(cstnInfo->NsName, CSTN_NSNAME_STRLEN);
  strncpy(tmp.NsName, cstnInfo->NsName, CSTN_NSNAME_STRLEN);
  memset(tmp.NsName + len, 0, CSTN_NSNAME_STRLEN - len);

  len = strnlen(cstnInfo->IpAddress, INET6_ADDRSTRLEN);
  strncpy(tmp.IpAddress, cstnInfo->IpAddress, INET6_ADDRSTRLEN);
  memset(tmp.IpAddress + len, 0, INET6_ADDRSTRLEN - len);

  return CsTnNameSpace(UPDATE, ldn, &tmp);
}



/*
 * Interface version 2
 *
 * Input parameter is LDN
 * This is received in cec_packet.
 * LDN and an empty CsTnInfoT are given to the application callback.
 * The application fills CsTnInfoT with the NameSpace and IpAddress.
 * LDN and CsTnInfoT are then send back to CS
 */  

int CsTnDispatch_2() 
{
  cec_packet_t packet = {.data=NULL};
  
  INFO("%s", "Enter CsTnDispatch_2()");

  // Pre checks
  if(!thehandle){
    ERROR("%s", "dispatch 2 called on uninitialized API");
    fprintf(stderr,  "cstnn: Error, dispatch 2 called on uninitialized API\n");
    return CSTN_ERROR;
  }
  memset(&packet, 0, sizeof packet);
  if(cec_receive_w_tmeout(thehandle, &packet, CEC_REC_TMO) == -1){
    ERROR("%s", "Timeout reading from CEC handle, dispatch 2");
    fprintf(stderr, "cstnn: Error, timeout reading from CEC handle, dispatch 3\n");
    goto error_return;
  }
  if(!thecallbacks2->OamDNtoNsNameAndIp){
    ERROR("%s", "dispatch 2 called with unconfigured callback");
    fprintf(stderr,  "cstnn: Error, dispatch 2 called with unconfigured callback\n");
    goto error_return;
  }

  // Invoke TN callback
  CsTnInfoT cstnInfo;
  memset(cstnInfo.NsName, 0, CSTN_NSNAME_STRLEN);
  memset(cstnInfo.IpAddress, 0, INET6_ADDRSTRLEN);

  int len = (int)packet.length; 
  char *ldn = malloc(len);
  char *ptr = packet.data;

  memcpy(ldn, ptr, len);

  if((*ldn) == DoOamDNtoNsNameAndIp){
    thecallbacks2->OamDNtoNsNameAndIp(ldn+1, &cstnInfo);
    // Reply to CS
    if (CsTnGetNameSpace_2(ldn+1, &cstnInfo) == -1) {
      ERROR("%s", "Failure in send dispatch 2");
      fprintf(stderr,  "cstnn: Error, failure in send dispatch 2\n");
      free(ldn);
      goto error_return;
    }
  }else{
    ERROR("%s", "dispatch 2 received unknown function code");
    fprintf(stderr,"cstnn: Error, dispatch 2 received unknown function code\n");
    free(ldn);
    goto error_return;
  }

  free(packet.data);
  free(ldn);
  INFO("Exit %d", CSTN_OK);
  return CSTN_OK;
  
 error_return:
  cec_close(thehandle);
  thehandle = NULL;
  thecallbacks2 = NULL;
  free(packet.data);
  INFO("Exit %d", CSTN_ERROR);
  return CSTN_ERROR;
}


/*
 * Interface version 3
 *
 * Input parameter is LDN and callback function identifier
 * This is received in cec_packet.
 * LDN and an empty CsTnInfoT are given to the application callback.
 * The application fills CsTnInfoT with the NameSpace and IpAddress.
 * LDN and CsTnInfoT are then send back to CS
 */  

int CsTnDispatch_3() 
{
  cec_packet_t packet = {.data=NULL};
  
  INFO("%s", "Enter CsTnDispatch_3()");

  // Pre checks
  if(!thehandle){
    ERROR("%s", "dispatch 3 called on uninitialized API");
    fprintf(stderr,  "cstnn: Error, dispatch 3 called on uninitialized API\n");
    INFO("Exit %d", CSTN_ERROR);
    return CSTN_ERROR;
  }
  memset(&packet, 0, sizeof packet);
  if(cec_receive_w_tmeout(thehandle, &packet, CEC_REC_TMO) == -1){
    ERROR("%s", "Timeout reading from CEC handle, dispatch 3");
    fprintf(stderr, "cstnn: Error, timeout reading from CEC handle, dispatch 3\n");
    goto error_return;
  }
  if(!thecallbacks3->OamDNtoNsNameAndIp){
    ERROR("%s", "dispatch 3 called with unconfigured callback");
    fprintf(stderr,  "cstnn: Error, dispatch 3 called with unconfigured callback\n");
    goto error_return;
  }
  
  // Invoke TN callback
  CsTnInfoT cstnInfo;
  memset(cstnInfo.NsName, 0, CSTN_NSNAME_STRLEN);
  memset(cstnInfo.IpAddress, 0, INET6_ADDRSTRLEN);

  int len = (int)packet.length;
  char *ldn = malloc(len);
  char *ptr = packet.data;

  memcpy(ldn, ptr, len);

  if((*ldn) == DoUnsubscribe){
    INFO("Executing Unsubscribe callback for ldn = %s", ldn+1);
    thecallbacks3->Unsubscribe(ldn+1);
  }else if((*ldn) == DoOamDNtoNsNameAndIp){
    INFO("Executing OamDNtoNsNameAndIp callback for ldn = %s", ldn+1);
    thecallbacks3->OamDNtoNsNameAndIp(ldn+1, &cstnInfo);
    // Reply to CS
    if (CsTnGetNameSpace_2(ldn+1, &cstnInfo) == -1) {
      ERROR("%s", "Failure in send dispatch 3");
      fprintf(stderr,  "cstnn: Error, failure in send dispatch 3\n");
      free(ldn);
      goto error_return;
    }
  }else{
    ERROR("%s", "dispatch 3 received unknown function code");
    fprintf(stderr,"cstnn: Error, dispatch 3 received unknown function code\n");
    free(ldn);
    goto error_return;
  }

  free(packet.data);
  free(ldn);
  INFO("Exit %d", CSTN_OK);
  return CSTN_OK;
  
 error_return:
  cec_close(thehandle);
  thehandle = NULL;
  thecallbacks3 = NULL;
  free(packet.data);
  INFO("Exit %d", CSTN_ERROR);
  return CSTN_ERROR;
}


/*
 * Dispatch, check which version was used in the initialization
 */
int
CsTnDispatch(void)
{
  INFO("%s", "Enter CsTnDispatch()");
  int result;
  switch (version) {
  case 2:
    //    return CsTnDispatch_2();
    result = CsTnDispatch_2();
    INFO("Exit %d", result);
    return result;
  case 3:
    //    return CsTnDispatch_3();
    result = CsTnDispatch_3();
    INFO("Exit %d", result);
    return result;
  default:
    INFO("Exit %d", CSTN_ERROR);
    return CSTN_ERROR;
  }
}

