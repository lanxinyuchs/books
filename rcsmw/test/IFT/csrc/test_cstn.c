/* ----------------------------------------------------------------------
 * %CCaseFile:	test_cstn.c %
 * %CCaseRev:	/main/R10A/R11A/2 %
 * %CCaseDate:	2017-09-06 %
 * %CCaseDocNo: %
 * Author:    erarafo
 * Author: <name>, <e-mail address>
 *
 * Short description:
 * TBD
 * ----------------------------------------------------------------------
 * %CCaseTemplateFile:    template.c %
 * %CCaseTemplateId: CCver: /main/1 %
 *
 * %CCaseCopyrightBegin%
 * Copyright (c) Ericsson AB 2017 All rights reserved.
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
 * R         2017-03-28      uabhten    created
 * R10A/4    2017-06-22      ecaiyan    Add code for intilize3
 * R11A/1    2017-07-20	     egabing    Add notifications to CSTN
 * ----------------------------------------------------------------------
 */
#include "master.h"
#include "cstnn.h"

#include "cello_te_trace.h"

#define CsTnInitialize_no   1
#define CsTnInitialize2_no 2
#define CsTnDispatch_no 3
#define CsTnUpdate_no 4
#define SetOamDNtoNsNameRsp_no 5
#define CsTnInitialize3_no 6

CsTnInfoT cstnData;
CsTnCallbacks2T cb2;
CsTnCallbacks3T cb3;
CsTnHandleT csTnHandle;

//static union SIGNAL* cstnService_p; // assume just one active client for now

extern bool TRI_trace_enabled;

CsTnResult 
    dn_to_tn(const char *ldn, CsTnInfoT *cstnInfo)
{
  APPLOG_I("%s", " dn_to_tn");
  APPLOG(" ldn: %s", ldn);
   
  strcpy(cstnInfo->NsName, cstnData.NsName);
  strcpy(cstnInfo->IpAddress, cstnData.IpAddress);
  APPLOG(" ipAddress: %s", cstnInfo->IpAddress);
  APPLOG(" nsName: %s", cstnInfo->NsName);

  // Notify CSTN
  ei_x_buff b;
  ei_x_new(&b);
  ei_x_format(&b, "{signal,{cstnDnToTn,{~s, {~s, ~s}}}}", ldn, cstnData.NsName,
	      cstnData.IpAddress);
  APPLOG( "%s", "Send callback to test case");
  sendCallback(b);

  // Empty the global struct   
  strcpy(cstnData.NsName, "");
  strcpy(cstnData.IpAddress, "");

  return CSTN_OK;
}

void doUnsubscribe(const char* ldn){
  // Notify CSTN
  APPLOG_I("%s", " doUnsubscribe");
  APPLOG(" ldn: %s", ldn);
  ei_x_buff b;
  ei_x_new(&b);
  ei_x_format(&b, "{signal,{cstnDoUnsubscribe,{~s}}}", ldn);
  APPLOG( "%s", "Send callback to test case");
  sendCallback(b);
}

int csTnDispatcher(char *handle) {
 
  //APPLOG_I("csTnDispatcher:handle=%s", handle);

  CsTnResult r = CsTnDispatch();
 
  if (r != CSTN_OK) {
    //APPLOG_I(" if loop:thread=%lu handle= %s r=%d", pthread_self(), handle, r);

    QTRACE4(3, "csTnDispatch failed, handle: %s, status: %d", handle, r);
    return DISPATCH_FAILED;
  }
  else {
       
    printf("csTnDispatch succeeded, handle: %s\n", handle);
    QTRACE3(3, "csTnDispatch succeeded, handle: %s", handle);
    return DISPATCH_OK;
  }
}

ei_x_buff
send_sig_cstn(int func, ei_x_buff args) {
  CsTnResult cstn_result;
  ei_x_buff resp;

  //APPLOG( "send_sig_cstn func = %d", func);

  ei_x_new(&resp);

  switch (func) {
    
  case CsTnUpdate_no:
    {
      //APPLOG ("%s","CsTnUpdate");

      CsTnInfoT cstnInfo;
      int type, size;
      /* tuple {Ldn, NsName[101], IpAddress[46]} */
      ei_decode_tuple_header(args.buff, &args.index, NULL);
      ei_get_type(args.buff, &args.index, &type, &size);
      char * ldnIpAddressMo = malloc(size+1);
      ei_decode_string(args.buff, &args.index, ldnIpAddressMo);      
      
      ei_decode_string(args.buff, &args.index, cstnInfo.NsName);
      ei_decode_string(args.buff, &args.index, cstnInfo.IpAddress);

      cstn_result = CsTnUpdate(ldnIpAddressMo,
                   &cstnInfo);
      //APPLOG_I("CsTnUpdate: ldnIpAddressMo=%s",ldnIpAddressMo );
      //APPLOG_I("CsTnUpdate: NsName=%s",cstnInfo.NsName );
      //APPLOG_I("CsTnUpdate: IpAddress=%s",cstnInfo.IpAddress );

      ei_x_format(&resp, "{ok, ~i}", (unsigned int)cstn_result);
      return resp;                   
  
    }
   case CsTnInitialize2_no:
     {
       APPLOG_I("%s", "CsTnInitialize2\n");
       
       cb2.OamDNtoNsNameAndIp = dn_to_tn;      
       csTnHandle = CsTnInitialize2(&cb2);
       
       char *handle = "csTn";

       addPollScheduleItem(handle, 
		 csTnHandle, 
		 (DispatcherT)&csTnDispatcher);

       ei_x_format(&resp, "{ok, ~i}", (unsigned int)csTnHandle);
       //APPLOG( "CsTnInitialize2: csTnHandle = %d", (unsigned int)csTnHandle);
       return resp;
     }
   case CsTnInitialize3_no:
     {
       APPLOG_I("%s", "CsTnInitialize3\n");

       cb3.OamDNtoNsNameAndIp = dn_to_tn;
       cb3.Unsubscribe = doUnsubscribe;
       csTnHandle = CsTnInitialize3(&cb3);
       
       char *handle = "csTn";

       addPollScheduleItem(handle, 
		 csTnHandle, 
		 (DispatcherT)&csTnDispatcher);

       ei_x_format(&resp, "{ok, ~i}", (unsigned int)csTnHandle);
       //APPLOG( "CsTnInitialize3: csTnHandle = %d", (unsigned int)csTnHandle);
       return resp;
     }
  case SetOamDNtoNsNameRsp_no:
    {
      //APPLOG_I("%s","in SetOamDNtoNsNameRsp");
      int type, size;
      /* tuple {Ldn, NsName[101], IpAddress[46]} */
      ei_decode_tuple_header(args.buff, &args.index, NULL);
      ei_get_type(args.buff, &args.index, &type, &size);
      char * ldnIpAddressMo = malloc(size+1);
      ei_decode_string(args.buff, &args.index, ldnIpAddressMo);      
      
      ei_decode_string(args.buff, &args.index, cstnData.NsName);
      ei_decode_string(args.buff, &args.index, cstnData.IpAddress);

      ei_x_format(&resp, "{ok, ~i}", 1);
      return resp;
    }
  }

  ei_x_format(&resp, "{ok, function_does_not_exist}");
  return resp;  
}



   
