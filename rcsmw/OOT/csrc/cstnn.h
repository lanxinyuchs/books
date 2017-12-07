/* ----------------------------------------------------------------------
 * %CCaseFile:	cstnn.h %
 * %CCaseRev:	/main/R3A/R8A/R9A/R10A/1 %
 * %CCaseDate:	2017-06-14 %
 * %CCaseDocNo: %
 * Author:
 * Author: <name>, <e-mail address>
 *
 * Short description:
 * CTN, Definition of the C interface between CS and TN
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
 * R3A/3      2014-10-12 etxlg       Name changed
 * R3A/4      2016-12-02 erasipe     New Prototypes CsTnInitialize2 
 * 				     & CsTnUpdate
 * R10A/1     2017-05-09 ecaiyan     Support initialize3
 * ----------------------------------------------------------------------
 */
#ifndef CSTN_H
#define CSTN_H

#ifdef __cplusplus
extern "C" {

#endif

#include <arpa/inet.h>

#define CSTN_NSNAME_STRLEN 101 /*with NUL termination*/

#define CSTN_OK 0
#define CSTN_FAIL -1

typedef int CsTnHandleT;
typedef int CsTnResult;

/**
 * CsTnCallbacksT to be Deprecated 
 */
typedef struct	{
   /** 
    *Return the NameSpace Name (NsName) for the given IpAddress MO
    *@param[in,out] pointer to int giving the size of the char array.
    *@param[in,out] pointer to char array,
    *               [in] the Distinguished Name of the IpAddress used for OAM access
    *               [out] the NsName.  
    */
   void (*OamDNtoNsName)(int *, char *);
} CsTnCallbacksT;


 
typedef struct {
  char NsName[CSTN_NSNAME_STRLEN];
  char IpAddress[INET6_ADDRSTRLEN];
} CsTnInfoT;

/**
 * CsTnCallbacks2T
 * contains all callbacks CSTN API supports 
 */
typedef struct {
   /**
    * Return CsTnInfoT for given IpAddress MO
    * @param[in] pointer to char array containing the distinguished name of the IpAddress MO
    * @param[in,out] pointer to a CsTnInfoT struct, 
    *           [in] just pointer empty struct
    *           [out] struct with values, if NsName or IpAddress not known set to empty string "".
    * @return CSTN_OK or one of the other CsTnResult return codes.
    */
   CsTnResult (*OamDNtoNsNameAndIp) (const char *, CsTnInfoT *);
} CsTnCallbacks2T;

/**
 * CsTnCallbacks3T
 * contains all callbacks CSTN API supports 
 */
typedef struct {
   /**
    * Return CsTnInfoT for given IpAddress MO
    * @param[in] pointer to char array containing the distinguished name of the IpAddress MO
    * @param[in,out] pointer to a CsTnInfoT struct, 
    *           [in] just pointer empty struct
    *           [out] struct with values, if NsName or IpAddress not known set to empty string "".
    * @return CSTN_OK or one of the other CsTnResult return codes.
    */
   CsTnResult (*OamDNtoNsNameAndIp) (const char *, CsTnInfoT *);
   void (*Unsubscribe) (const char *);
} CsTnCallbacks3T;

/* Prototypes */

/**
 * CsTnInitialize to be deprecated
 */
CsTnHandleT CsTnInitialize(CsTnCallbacksT *);

/**
 * CsTnInitialize2 connect to RCS inform of which callbacks
 * TN supports
 *
 * @param   pointer to struct CsTnCallbacks2T 
 * @return  CsTnHandleT the socket to poll for activity on
 */
CsTnHandleT CsTnInitialize2(CsTnCallbacks2T *);

/**
 * CsTnInitialize3 connect to RCS inform of which callbacks
 * TN supports
 *
 * @param   pointer to struct CsTnCallbacks3T 
 * @return  CsTnHandleT the socket to poll for activity on
 */
CsTnHandleT CsTnInitialize3(CsTnCallbacks3T *);

/**
 * CsTnDispatch
 * Call is pending invoke callback on TN
 * @return  CSTN_OK or one of the other CsTnResult return codes.
 */
CsTnResult CsTnDispatch(void);

/**
 * CsTnUpdate
 * CsTnInfoT has been updated
 * @param pointer to char array containing the LDN of the IpAddress MO for which data has been changed.
 * @param pointer to struct CsTnInfoT containing the changed information.
 *        The trailing non used bytes in the strings in CsTnInfoT must 
 *        be set to \0 (NULL) 
 * @return CSTN_OK or one of the other CsTnResult return codes.
 */
CsTnResult CsTnUpdate( const char *, const CsTnInfoT *);

#ifdef __cplusplus
}
#endif

#endif
