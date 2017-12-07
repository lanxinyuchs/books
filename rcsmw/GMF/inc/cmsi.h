/*
 *
 * Copyright (c) Ericsson AB  2013-2016 All rights reserved.
 *
 * The information in this document is the property of Ericsson. Except
 * as specifically authorized in writing by Ericsson,the receiver of this
 * document shall keep the information contained herein confidential
 * and shall protect the same in whole or in part from disclosure and
 * dissemination to third parties. Disclosure and disseminations to the
 * receiver's employees shall only be made on a strict need to know basis.
 *
 *
 * IDENTIFICATION
 * --------------
 * Description: This file contains the interfaces provided by CMSI.
 *
 *
 * REVISION HISTORY
 * ----------------
 *
 * Revised: Per Norberg 2013-04-24
 * Change:  Copied and updated from lmhi.h
 */

#ifndef __CMSI_H
#define __CMSI_H

#ifdef  __cplusplus
extern "C" {
#endif

  /*
******************************************************************************
* INCLUDE FILES
******************************************************************************
*/

#include <stdint.h>
#include <stdlib.h>
#include <unistd.h>

#ifndef  __cplusplus
#include <stdbool.h>
#endif

#include <saImm.h>

  /*
******************************************************************************
* MACROS
******************************************************************************
*/

#define DN_TI_SIGNAL_REVISION 0


  /* This type specifies function return values when using the CMSI interface */
  typedef uint32_t CmsiResultT;
#define CMSI_OK                          0
#define CMSI_INVALID_PARAMETER_DIRECTION 1
#define CMSI_INVALID_PARAMETER_TODNP     2
#define CMSI_SEND_ERROR                  3
#define CMSI_RECEIVE_ERROR               4
#define CMSI_OBJECT_CLASS_NOT_FOUND      5
#define CMSI_CLASS_NOT_FOUND             6
#define CMSI_ATTRIBUTE_NOT_FOUND         7
#define CMSI_ATTRIBUTE_NO_STRUCT_REF     8
#define CMSI_LDN_NOT_FOUND               9
#define CMSI_ACTION_NOT_FOUND           10
#define CMSI_NO_SUBSCRIBER              11

  typedef uint32_t CmsiTransformDirectionT;
#define MIM_TO_IMM  1
#define IMM_TO_MIM  2

#define CMSI_DN_MAX_SIZE (400)
#define CMSI_ACTION_MAX_SIZE (80)

#define CMSI_RESERVED_OBJECTIDS_START 16677216
#define CMSI_RESERVED_OBJECTIDS_END   16777215

  /*
******************************************************************************
* TYPES
******************************************************************************
*/

  struct CmsiOapData
  {
    int32_t  fd;   /* namespace file descriptor */
    uint32_t addr; /* source IPv4 address       */
    uint32_t dscp; /* DSCP value                */
  };

  struct CmsiOapData2
  {
    int32_t          fd;      /* namespace file descriptor             */
    int              domain;  /* AF_INET or AF_INET6. Use in socket(2) */
    struct sockaddr *addr;    /* source IP address. Use in bind(2)     */
    socklen_t        addrlen; /* size of *addr. Use in bind(2)         */
    uint32_t         dscp;    /* DSCP value                            */
  };

  struct CmsiCardinalityValue
  {
    int32_t value;
  };

  struct CmsiCardinality
  {
    struct CmsiCardinalityValue* min; /* NULL if not defined in meta data */
    struct CmsiCardinalityValue* max; /* NULL if not defined in meta data */
  };

  struct CmsiHasClass
  {
    SaImmClassNameT  name; /* ECIM class name (immClassName= mimName(if isScope==true)+ecimClassName) */
    char* mimName;
    bool isScoped;	/* even if there is a mimName,  */
  };

  struct CmsiAssociationEnd
  {
    SaImmAttrNameT  name;
    struct CmsiHasClass hasClass;
    struct CmsiCardinality* cardinality; /* NULL if not defined in meta data */
    bool isReserving;
  };

  struct CmsiBiDirectionalAssociation
  {
    struct CmsiAssociationEnd reservingAssociationEnd;
    struct CmsiAssociationEnd reservedAssociationEnd;
  };

  typedef void (*cmsiOapDataSubscribeCallbackT) (struct CmsiOapData2 *);

  /*
******************************************************************************
* FUNCTION PROTOTYPES
******************************************************************************
*/

  /*****************************************************************************
   *
   *  Name  : cmsiInitiateService
   *
   *  Descr.: Initialize configure management support service.
   *          This function sets up a connection to the service.
   *          If a service is not initiated a new connection
   *          will be set up for each request.
   *
   *  Return: Pointer to the connection handle
   *
   ****************************************************************************/
  void*
  cmsiInitiateService(void);



  /*****************************************************************************
   *
   *  Name  : cmsi_terminateService
   *
   *  Descr.: Terminate connection to the configure management support service
   *
   *  Args  : IN:  cmsiHandleP       A pointer to the connection handle
   *
   *  Return: CmsiResult
   *
   ****************************************************************************/
  void
  cmsiTerminateService(void *cmsiHandleP);



  /*****************************************************************************
   *
   *  Name  : cmsiTransform
   *
   *  Descr.: Translate DN from mim format to imm format or vice versa.
   *          If NULL pointer is given as the connection handle
   *          a new connection is set up and released for each request.
   *
   *  Args  : IN:     cmsiHandleP      A pointer to the connection handle
   *
   *                  direction        MIM_TO_IMM | IMM_TO_MIM
   *
   *                  fromDnP          from DN
   *
   *          IN/OUT: toDnP            to DN | error string
   *                                   The memory must be freed after usage
   *                                   by a call to cmsiFree().
   *                                   Only valid if the return value is
   *                                   CMSI_OK.
   *
   *  Return: CmsiResultT
   *
   ****************************************************************************/
  CmsiResultT
  cmsiTransform(void                   *cmsiHandleP,
		CmsiTransformDirectionT direction,
		char                   *fromDnP,
		char                  **toDnP);

  /*****************************************************************************
   *
   *  Name  : cmsiIsMoClassStruct
   *
   *  Descr.: Check if an IMM MO class is a struct
   *          If NULL pointer is given as the connection handle
   *          a new connection is set up and released for each request.
   *
   *  Args  : IN:     cmsiHandleP      A pointer to the connection handle
   *
   *                  className        The name of the IMM MO class
   *
   *          IN/OUT: isMoClassStruct  A positive integer if the IMM MO class
   *                                   is a struct, otherwise zero (0)
   *                                   Only valid if the return value is
   *                                   CMSI_OK.
   *
   *  Return: CmsiResultT
   *
   ****************************************************************************/
  CmsiResultT
  cmsiIsMoClassStruct(void                 *cmsiHandleP,
		      const SaImmClassNameT className,
		      int                  *isMoClassStruct);


  /*****************************************************************************
   *
   *  Name  : cmsiIsAttrRef2Struct
   *
   *  Descr.: Check if an attribute is refering to a struct
   *          If NULL pointer is given as the connection handle
   *          a new connection is set up and released for each request.
   *
   *  Args  : IN:     cmsiHandleP      A pointer to the connection handle
   *
   *                  className        The name of the IMM MO class
   *
   *                  attrName         The name of the attribute
   *
   *          IN/OUT: isAttrRef2Struct  A positive integer if the attribute
   *                                    is refering to a struct,
   *                                    otherwise zero (0)
   *                                    Only valid if the return value is
   *                                    CMSI_OK.
   *
   *  Return: CmsiResultT
   *
   ****************************************************************************/
  CmsiResultT
  cmsiIsAttrRef2Struct(void                 *cmsiHandleP,
		       const SaImmClassNameT className,
		       SaImmAttrNameT        attrName,
		       int                  *isAttrRef2Struct);


  /*****************************************************************************
   *
   *  Name  : cmsiGetStructRefForAttr
   *
   *  Descr.: Return the corresponding IMM MO class referred by the attribute
   *          If NULL pointer is given as the connection handle
   *          a new connection is set up and released for each request.
   *
   *  Args  : IN:     cmsiHandleP      A pointer to the connection handle
   *
   *                  className        The name of the IMM MO class
   *
   *                  attrName         The name of the attribute
   *
   *          IN/OUT: structRefName    The name of the corresponding
   *                                   IMM MO class. Only valid if the return
   *                                   value is CMSI_OK. The memory must be
   *                                   freed after usage by a call to cmsiFree().
   *
   *  Return: CmsiResultT
   *
   ****************************************************************************/
  CmsiResultT
  cmsiGetStructRefForAttr(void                 *cmsiHandleP,
			  const SaImmClassNameT className,
			  const SaImmAttrNameT  attrName,
			  SaImmClassNameT      *structRefName);

  /****************************************************************************
   *
   *  Name  : cmsiGetObjectId
   *
   *  Descr.: Translate Ldn to unique objectId
   *          If NULL pointer is given as the connection handle
   *          a new connection is set up and released for each request.
   *
   *
   *  Args  : IN:     cmsiHandleP  A pointer to the connection handle
   *
   *
   *                  Ldn          The input Ldn
   *
   *          IN/OUT: objectId     The transformed Ldn to its equivalent integer
   *                               Only valid if the return value is CMSI_OK.
   *
   *  Return: CmsiResultT
   *
   ***************************************************************************/
  CmsiResultT
  cmsiGetObjectId(void *cmsiHandleP,
		  char *Ldn,
		  int  *objectId);

  /****************************************************************************
   *
   *  Name  : cmsiGetObjectIds
   *
   *  Descr.: Translate a list of Ldns to unique objectIds
   *          If NULL pointer is given as the connection handle
   *          a new connection is set up and released for each request.
   *
   *  Args  : IN:     cmsiHandleP  A pointer to the connection handle
   *
   *                  Ldns         The list of input Ldns
   *
   *                  member       Number of Ldns in the list
   *
   *          IN/OUT: objectIds    The transformed Ldn to its equivalent integer
   *                               Only valid if the return value is CMSI_OK.
   *
   *  Return: CmsiResultT
   *
   ***************************************************************************/
  CmsiResultT
  cmsiGetObjectIds(void  *cmsiHandleP,
		   char **Ldns,
		   int    members,
		   int   *objectIds);

  /****************************************************************************
   *
   *  Name  : cmsiGetLdn
   *
   *  Descr.: Translate unique objectId to Ldn
   *          If NULL pointer is given as the connection handle
   *          a new connection is set up and released for each request.
   *
   *  Args  : IN:     cmsiHandleP A pointer to the connection handle
   *
   *                  objectId
   *
   *          IN/OUT: Ldn         The equivalent Ldn representing the objectId
   *                              Only valid if the return value is CMSI_OK.
   *
   *  Return: CmsiResultT
   *
   ***************************************************************************/
  CmsiResultT
  cmsiGetLdn(void  *cmsiHandleP,
	     int    objectId,
	     char  *Ldn);

  /****************************************************************************
   *
   *  Name  : cmsiGetLdns
   *
   *  Descr.: Translate unique objectId to Ldn
   *          If NULL pointer is given as the connection handle
   *          a new connection is set up and released for each request.
   *
   *  Args  : IN:     cmsiHandleP A pointer to the connection handle
   *
   *                  objectIds   A list of objectIds
   *
   *                  member      Number of objectIds in the list
   *
   *          IN/OUT: Ldn         The equivalent Ldn representing the objectId
   *                                   Only valid if the return value is
   *                                   CMSI_OK.
   *
   *  Return: CmsiResultT
   *
   ***************************************************************************/
  CmsiResultT
  cmsiGetLdns(void  *cmsiHandleP,
	      int   *objectIds,
	      int    members,
	      char **Ldns);

  /****************************************************************************
   *
   *  Name  : cmsiGetManagedElementId
   *
   *  Descr.: Return the managedElementId
   *          If NULL pointer is given as the connection handle
   *          a new connection is set up and released for each request.
   *
   *  Args  : IN:     cmsiHandleP A pointer to the connection handle
   *
   *          IN/OUT: managedElementId A pointer to a buffer
   *                                   Only valid if the return value is
   *                                   CMSI_OK.
   *
   *          IN/OUT:     size  Size of the buffer
   *
   *  Return: CmsiResultT
   *
   ***************************************************************************/
  CmsiResultT
  cmsiGetManagedElementId(void   *cmsiHandleP,
			  char   *managedElementId,
			  size_t *size);

  /*****************************************************************************
   *
   *  Name  : cmsiGetAdmOpId
   *
   *  Descr.: Translate an action name used in COM to an identifier used by IMM
   *          If NULL pointer is given as the connection handle
   *          a new connection is set up and released for each request.
   *
   *  Args  : IN:     cmsiHandleP A pointer to the connection handle
   *
   *                  className        The name of the IMM MO class
   *
   *                  actionName       The name of the action
   *
   *          OUT:    admOpId          The identifier of the action.
   *                                   Only valid if the return value is
   *                                   CMSI_OK.
   *
   *  Return: CmsiResultT
   *
   ****************************************************************************/
  CmsiResultT
  cmsiGetAdmOpId(void                   *cmsiHandleP,
		 const SaImmClassNameT   className,
		 const char*             actionName,
		 SaImmAdminOperationIdT *admOpId);

  /*****************************************************************************
   *
   *  Name  : cmsiGetActionName
   *
   *  Descr.: Translate an identifier used by IMM to an action name used in COM
   *          If the action name does not fit in the buffer, it will be
   *          truncated to a zero terminated string.
   *          If NULL pointer is given as the connection handle
   *          a new connection is set up and released for each request.
   *
   *  Args  : IN:     cmsiHandleP A pointer to the connection handle
   *
   *                  className        The name of the IMM MO class
   *
   *                  admOpId          The identifier of the action
   *
   *          OUT:    actionName       The name of the action. It must point
   *                                   to a memory area, which size is at least
   *                                   CMSI_ACTION_MAX_SIZE
   *                                   Only valid if the return value is
   *                                   CMSI_OK.
   *
   *  Return: CmsiResultT
   *
   ****************************************************************************/
  CmsiResultT
  cmsiGetActionName(void                  *cmsiHandleP,
		    const SaImmClassNameT  className,
		    SaImmAdminOperationIdT admOpId,
		    char*                  actionName);

  /*****************************************************************************
   *
   *  Name  : cmsiGetOapData
   *
   *  Descr.: Return data describing the OamAccessPoint:
   *          * The file descriptor referring to a namespace
   *          * The source IPv4 address
   *          * The DSCP value
   *
   *          Note: The caller must close the namespace file descriptor after
   *                the call to setns()
   *
   *          If NULL pointer is given as the connection handle
   *          a new connection is set up and released for each request.
   *
   *  Args  : IN:     cmsiHandleP A pointer to the connection handle
   *
   *          OUT:    oapData     A pointer to a struct CmsiOapData instance
   *
   *                              Only valid if the return value is CMSI_OK.
   *
   *  Return: CmsiResultT
   *
   ****************************************************************************/
  CmsiResultT
  cmsiGetOapData(void               *cmsiHandleP,
		 struct CmsiOapData *oapData);

  /*****************************************************************************
   *
   *  Name  : cmsiGetOapData2
   *
   *  Descr.: Return data describing the OamAccessPoint:
   *          * The file descriptor referring to a namespace
   *          * The source IP address (IPv4 or IPv6)
   *          * The DSCP value
   *
   *          Note: The caller must close the namespace file descriptor after
   *                the call to setns()
   *
   *          If NULL pointer is given as the connection handle
   *          a new connection is set up and released for each request.
   *
   *  Args  : IN:     cmsiHandleP A pointer to the connection handle
   *
   *          OUT:    oapData     A pointer to a struct CmsiOapData2 instance
   *
   *                              Only valid if the return value is CMSI_OK.
   *
   *  Return: CmsiResultT
   *
   ****************************************************************************/
  CmsiResultT
  cmsiGetOapData2(void                *cmsiHandleP,
		  struct CmsiOapData2 *oapData);

  /****************************************************************************
   *
   *  Name  : cmsiGetManagedElementUserLabel
   *
   *  Descr.: Return userLabel from the ManagedElement MO
   *          If NULL pointer is given as the connection handle
   *          a new connection is set up and released for each request.
   *
   *  Args  : IN:     cmsiHandleP A pointer to the connection handle
   *
   *          IN/OUT: userLabel A pointer to a buffer
   *                            Only valid if the return value is CMSI_OK.
   *
   *          IN/OUT: size  Size of the buffer
   *
   *  Return: CmsiResultT
   *
   ***************************************************************************/
  CmsiResultT
  cmsiGetManagedElementUserLabel(void   *cmsiHandleP,
				 char   *userLabel,
				 size_t *size);

  /****************************************************************************
   *
   *  Name  : cmsiGetImmClassName
   *
   *  Descr.: Return the corresponding IMM MO class name referred by the IMM DN
   *          If NULL pointer is given as the connection handle
   *          a new connection is set up and released for each request.
   *
   *  Args  : IN:     cmsiHandleP  A pointer to the connection handle
   *
   *                  immDnP       A pointer to the IMM DN
   *
   *          OUT:    immClassName A pointer to the name of the IMM MO class
   *
   *  Return: CmsiResultT
   *
   ***************************************************************************/
  CmsiResultT
  cmsiGetImmClassName(void            *cmsiHandleP,
		      const char      *immDnP,
		      SaImmClassNameT *immClassName);

  /*****************************************************************************
   *
   *  Name  : cmsiFree
   *
   *  Descr.: Free the memory space pointed to by ptr.
   *
   *  Args  : IN:     ptr      A pointer to the allocated memory
   *
   ****************************************************************************/
  void
  cmsiFree(void *ptr);

  /*****************************************************************************
   *
   *  Name  : cmsiGetBiDirAssociationForClient
   *
   *  Descr.:
   *
   *  Args  : IN:     cmsiHandleP A pointer to the connection handle
   *
   *  Return: CmsiResultT
   *
   ****************************************************************************/
  CmsiResultT
  cmsiGetBiDirAssociationForClient(void *cmsiHandleP,
				   const SaImmClassNameT reservingImmMoClass,
				   struct CmsiBiDirectionalAssociation*** pToppNullTermArray);

  /*****************************************************************************
   *
   *  Name  : cmsiFreeBiDir
   *
   *  Descr.:
   *
   *  Args  : IN:     cmsiHandleP A pointer to the connection handle
   *
   *  Return: CmsiResultT
   *
   ****************************************************************************/
  CmsiResultT
  cmsiFreeBiDir(void *cmsiHandleP,
		struct CmsiBiDirectionalAssociation** ppNullTermArray);

  /*****************************************************************************
   *
   *  Name  : cmsiGetDnPrefix
   *
   *  Descr.: Return dnPrefix from the ManagedElement MO
   *          If NULL pointer is given as the connection handle
   *          a new connection is set up and released for each request.
   *
   *  Args  : IN:     cmsiHandleP A pointer to the connection handle
   *
   *          IN/OUT: dnPrefix A pointer to a buffer
   *                           Only valid if the return value is CMSI_OK.
   *
   *          IN/OUT: size  Size of the buffer
   *
   *  Return: CmsiResultT
   *
   ****************************************************************************/
  CmsiResultT
  cmsiGetDnPrefix(void   *cmsiHandleP,
		  char   *dnPrefix,
		  size_t *size);

  /****************************************************************************
   *
   *  Name  : cmsiOapDataSubscribe
   *
   *  Descr.: Creates. updates or removes an existing subscription of
   *          getting updates when data describing the OamAccessPoint
   *          has been updated.
   *          Only one subscription per thread is possible.
   *          The value in oapDataFd is only valid when the function returns
   *          CMSI_OK.
   *
   *  Args  : IN:     oapDataCb    A pointer to a callback function.
   *                               If set to NULL, an existing subscription
   *                               will be deleted.
   *                               When a new subscription is created, the
   *                               OamAccessData related data is regarded as
   *                               changed.
   *                               It is possible to change an existing
   *                               subscription by using an new
   *                               callback function
   *                               In that case, the OamAccessData related data
   *                               is not regarded as changed and the value of
   *                               oapDataFd will not be changed.
   *
   *          OUT:    oapDataFd    A pointer to an integer that contains
   *                               the value of the file descriptor
   *
   *  Return: CmsiResultT
   *
   ***************************************************************************/
  CmsiResultT
  cmsiOapDataSubscribe(cmsiOapDataSubscribeCallbackT oapDataCb,
		       int* oapDataFd);

  /****************************************************************************
   *
   *  Name  : cmsiOapDataDispatch
   *
   *  Descr.: The callback function oapDataCb provided in
   *          cmsiOapDataSubscribe() will be executed.
   *          This function should only be called when the
   *          file descriptor oapDataFd from cmsiSubscribeOapData() is ready
   *          to perform I/O. This can be checked by using poll(), select() or
   *          epoll() or an event notification library.
   *          This occurs either when the OamAccessData related data is changed
   *          or when a new subscription is created.
   *
   *  Args  : -
   *
   *  Return: CmsiResultT
   *
   ***************************************************************************/
  CmsiResultT
  cmsiOapDataDispatch(void);

#ifdef  __cplusplus
}
#endif

#endif /* __CMSI_H */
