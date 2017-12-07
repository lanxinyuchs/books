/*****************************************************************************
* 
*  COPYRIGHT (C) by Ericsson AB
* 
*  The copyright to the computer program(s) herein is the property
*  of Ericsson AB. 
* 
*  The program(s) may be used and/or copied only with the written
*  permission from Ericsson AB or in accordance with the terms 
*  and conditions stipulated in the agreement/contract under which 
*  the program(s) have been supplied. 
*
*
*****************************************************************************/

/******************************* Description ********************************/
/*

This interface is used for handling of the optional feature license part of 
the LIHI (LIicense Handler Interface).
The License Feature Control Interface (LFCI) is registered in the name 
server with the name LICENSE_FEATURE_CONTROL_I_SERVICE_NAME.

The LFCI  product has the following procedures:
################################################################
- Connection Establish
- Subscription
- Unsubscription
- Change
- Disconnect

 Current protocol revision number: 2

In protocol revision 2  the following signals has been added:
LFCI_FEATURE_LICENSE_SUBSCRIBE2_CFM
(replacing LFCI_FEATURE_LICENSE_SUBSCRIBE_CFM)
LFCI_FEATURE_LICENSE_CHANGE2_IND
(replacing LFCI_FEATURE_LICENSE_CHANGE_IND)

Subscriptions on this interface will trigger internal subscriptions for 
information in the License Key File.
Multiple subscriptions towards the same license key is supported.
When the first License User subscribes for an optional feature license this 
will trigger a MO creation operation.
When the last License User removes his subscription this will result in that 
the MO will be marked for deletion.

The License Users will be notified when the information in the License Key 
File, or the MO information, is updated.
No reject signals are specified for the subscription/unsubscription 
procedures. Incorrect behavior by the License User will be regarded as a 
critical fault.
In these scenarios the License Handler will send the signal 
LfciFeatureLicenseDisconnectInd and disconnect the License User.

-------------------------------------------------------------------------
Generic description
-------------------------------------------------------------------------
Frequently used data fields:

sigNo              Indicates the signal number the struct represents.
addressInfo clientRef identifies the requesting client.
addressInfo serverRef identifies the providing server.

All character strings shall be null terminated.

----------------------------------------------------------------------------
*/


/*********************** Double inclusion protection ************************/
#ifndef LICENSE_FEATURE_CONTROL_I_SIG
#define LICENSE_FEATURE_CONTROL_I_SIG

/******************************** Includes **********************************/
#ifdef MAKE_WITH_LPP
#include "lpp.h"
#endif

#include "licenseFeatureControlICommon.h"

#define LICENSE_FEATURE_CTRL_SIGBASE_CC (0x1741d00)

#define LFCI_CONN_TO_SERVER_REQ (LICENSE_FEATURE_CTRL_SIGBASE_CC + 0)    /* !- SIGNO(struct LfciConnToServerReqS) -! */

/*****************************************************************************
 * Signal: LFCI_CONN_TO_SERVER_REQ (0x1741d00)
 *//**
 * @brief                      The License User (client) shall send the 
 *                             signal to establish a connection with the 
 *                             License Handler (server).
 *
 *                             Valid for Protocol revision: 1,2
 *
 * @param sizeOfProtocolRevisions
 *                             Indicates number of valid elements in dynamic 
 *                             array.
 *                             
 * @param protocolRevisions[]  Dynamic array containing protocol revisions. 
 *                             The first wanted protocol revision is 
 *                             included at index 0. 
 *                             
 ****************************************************************************/
typedef struct LfciConnToServerReqS
{
  uint32_t            sigNo; 
  LfciAddressInfoS    addressInfo; 
  uint32_t            sizeOfProtocolRevisions; 
  uint32_t            protocolRevisions[1]; /* Placeholder to create pointer to array, length indicated by sizeOfProtocolRevisions */
} LfciConnToServerReqS;


#define LFCI_CONN_TO_SERVER_CFM (LICENSE_FEATURE_CTRL_SIGBASE_CC + 1)    /* !- SIGNO(struct LfciConnToServerCfmS) -! */

/*****************************************************************************
 * Signal: LFCI_CONN_TO_SERVER_CFM (0x1741d01)
 *//**
 * @brief                      Indicates that a connection has been 
 *                             established and that the License User can 
 *                             continue with the other procedures
 *
 *                             Valid for Protocol revision: 1,2
 *                             
 * @param protocolRevision     Selected protocol revision.
 *                             
 ****************************************************************************/
typedef struct LfciConnToServerCfmS
{
  uint32_t            sigNo; 
  LfciAddressInfoS    addressInfo; 
  uint32_t            protocolRevision; 
} LfciConnToServerCfmS;


#define LFCI_CONN_TO_SERVER_REJ (LICENSE_FEATURE_CTRL_SIGBASE_CC + 2)    /* !- SIGNO(struct LfciConnToServerRejS) -! */

/*****************************************************************************
 * Signal: LFCI_CONN_TO_SERVER_REJ (0x1741d02)
 *//**
 * @brief                      Indicates that the connection establish 
 *                             procedure has failed and that the License 
 *                             User not can continue with any other 
 *                             procedure.
 *
 *                             Valid for Protocol revision: 1,2
 *                             
 * @param errorMsg[]           Error information indicating why the 
 *                             connection establish request has been 
 *                             rejected.
 *                             
 ****************************************************************************/
typedef struct LfciConnToServerRejS
{
  uint32_t            sigNo; 
  LfciAddressInfoS    addressInfo; 
  char                errorMsg[MAX_SIZE_OF_LFCI_ERROR_MSG]; 
} LfciConnToServerRejS;


#define LFCI_FEATURE_LICENSE_SUBSCRIBE_REQ (LICENSE_FEATURE_CTRL_SIGBASE_CC + 3)    /* !- SIGNO(struct LfciFeatureLicenseSubscribeReqS) -! */

/*****************************************************************************
 * Signal: LFCI_FEATURE_LICENSE_SUBSCRIBE_REQ (0x1741d03)
 *//**
 * @brief                      License User subscribes for feature licenses.
 *                             Corresponding feature MO instances will be 
 *                             created, in case they not already exist.
 *
 *                             Valid for Protocol revision: 1,2
 *                             
 * @param sizeOfLicenseData    Indicates number of valid elements in dynamic 
 *                             array. Minimum allowed value is 1 and maximum 
 *                             allowed value is 100 licenses.
 *                             
 * @param licenseData[]        Dynamic array containing information for 
 *                             several licenses. 
 *                             
 ****************************************************************************/
typedef struct LfciFeatureLicenseSubscribeReqS
{
  uint32_t            sigNo; 
  LfciAddressInfoS    addressInfo; 
  uint32_t            sizeOfLicenseData; 
  LfciLicenseDataS    licenseData[1]; /* Placeholder to create pointer to array, length indicated by sizeOfLicenseData */
} LfciFeatureLicenseSubscribeReqS;


#define LFCI_FEATURE_LICENSE_SUBSCRIBE_CFM (LICENSE_FEATURE_CTRL_SIGBASE_CC + 4)    /* !- SIGNO(struct LfciFeatureLicenseSubscribeCfmS) -! */

/*****************************************************************************
 * Signal: LFCI_FEATURE_LICENSE_SUBSCRIBE_CFM (0x1741d04)
 *//**
 * @brief                      Confirmation of a feature license
 *                             subscription request.
 *
 *                             Valid for Protocol revision: 1
 *
 * @param sizeOfLicenseInfo    Indicates number of valid elements in dynamic
 *                             array.
 *
 * @param licenseInfo[]        Dynamic array containing information for
 *                             several licenses.
 *
 ****************************************************************************/
typedef struct LfciFeatureLicenseSubscribeCfmS
{
  uint32_t            sigNo;
  LfciAddressInfoS    addressInfo;
  uint32_t            sizeOfLicenseInfo;
  LfciLicenseInfoS    licenseInfo[1]; /* Placeholder to create pointer to array, length indicated by sizeOfLicenseInfo */
} LfciFeatureLicenseSubscribeCfmS;


#define LFCI_FEATURE_LICENSE_UNSUBSCRIBE_REQ (LICENSE_FEATURE_CTRL_SIGBASE_CC + 5)    /* !- SIGNO(struct LfciFeatureLicenseUnsubscribeReqS) -! */

/*****************************************************************************
 * Signal: LFCI_FEATURE_LICENSE_UNSUBSCRIBE_REQ (0x1741d05)
 *//**
 * @brief                      License User request that subscriptions of 
 *                             feature licenses are removed. The server will 
 *                             respond positively with a confirmation, even 
 *                             if the license does not exist. MO instances, 
 *                             where the corresponding license have no other 
 *                             subscribers, will be marked for deletion.
 *
 *                             Valid for Protocol revision: 1,2
 *                             
 * @param sizeOfLicenseData    Indicates number of valid elements in dynamic 
 *                             array. Minimum allowed value is 1 and maximum 
 *                             allowed value is 100 licenses.
 *                             
 * @param licenseData[]        Dynamic array containing license key id(s). 
 *                             
 ****************************************************************************/
typedef struct LfciFeatureLicenseUnsubscribeReqS
{
  uint32_t            sigNo; 
  LfciAddressInfoS    addressInfo; 
  uint32_t            sizeOfLicenseData; 
  LfciKeyDataS        licenseData[1]; /* Placeholder to create pointer to array, length indicated by sizeOfLicenseData */
} LfciFeatureLicenseUnsubscribeReqS;


#define LFCI_FEATURE_LICENSE_UNSUBSCRIBE_CFM (LICENSE_FEATURE_CTRL_SIGBASE_CC + 6)    /* !- SIGNO(struct LfciFeatureLicenseUnsubscribeCfmS) -! */

/*****************************************************************************
 * Signal: LFCI_FEATURE_LICENSE_UNSUBSCRIBE_CFM (0x1741d06)
 *//**
 * @brief                      Confirmation of a feature license remove 
 *                             subscription request.
 *
 *                             Valid for Protocol revision: 1,2
 *                             
 * @param sizeOfLicenseInfo    Indicates number of valid elements in dynamic 
 *                             array.
 *                             
 * @param licenseInfo[]        Dynamic array containing information for 
 *                             several licenses.
 *                             
 ****************************************************************************/
typedef struct LfciFeatureLicenseUnsubscribeCfmS
{
  uint32_t            sigNo; 
  LfciAddressInfoS    addressInfo; 
  uint32_t            sizeOfLicenseInfo; 
  LfciKeyDataS        licenseInfo[1]; /* Placeholder to create pointer to array, length indicated by sizeOfLicenseInfo */
} LfciFeatureLicenseUnsubscribeCfmS;


#define LFCI_FEATURE_LICENSE_CHANGE_IND (LICENSE_FEATURE_CTRL_SIGBASE_CC + 7)    /* !- SIGNO(struct LfciFeatureLicenseChangeIndS) -! */

/*****************************************************************************
 * Signal: LFCI_FEATURE_LICENSE_CHANGE_IND (0x1741d07)
 *//**
 * @brief                      This signal is sent to indicate that the
 *                             content in the LKF, or the featureState
 *                             attribute in the OptionalFeatureLicense MO
 *                             instance, has been changed for the license.
 *
 *                             Valid for Protocol revision: 1
 *
 * @param licenseInfo          License information.
 *
 ****************************************************************************/
typedef struct LfciFeatureLicenseChangeIndS
{
  uint32_t            sigNo;
  LfciAddressInfoS    addressInfo;
  LfciLicenseInfoS    licenseInfo;
} LfciFeatureLicenseChangeIndS;


#define LFCI_FEATURE_LICENSE_DISCONNECT_IND (LICENSE_FEATURE_CTRL_SIGBASE_CC + 8)    /* !- SIGNO(struct LfciFeatureLicenseDisconnectIndS) -! */

/*****************************************************************************
 * Signal: LFCI_FEATURE_LICENSE_DISCONNECT_IND (0x1741d08)
 *//**
 * @brief                      This signal indicates that the server has 
 *                             terminated the connection, due to incorrect 
 *                             behavior by the client.
 *
 *                             Valid for Protocol revision: 1,2
 *                             
 * @param errorInfo[]          Error information indicating why the client 
 *                             has been disconnected.
 *                             
 ****************************************************************************/
typedef struct LfciFeatureLicenseDisconnectIndS
{
  uint32_t            sigNo; 
  LfciAddressInfoS    addressInfo; 
  char                errorInfo[MAX_SIZE_OF_LFCI_FEATURE_LICENSE_DISCONNECT_IND_S_ERROR_INFO]; 
} LfciFeatureLicenseDisconnectIndS;


#define LFCI_OSE_ATTACH_CLIENT_DOWN (LICENSE_FEATURE_CTRL_SIGBASE_CC + 9)    /* !- SIGNO(struct LfciOseAttachClientDownS) -! */

/*****************************************************************************
 * Signal: LFCI_OSE_ATTACH_CLIENT_DOWN (0x1741d09)
 *//**
 * @brief                      Signal used by the License Handler, indicates 
 *                             that a client has gone down.
 *
 *                             Valid for Protocol revision: 1,2
 *                             
 ****************************************************************************/
typedef struct LfciOseAttachClientDownS
{
  uint32_t            sigNo; 
  LfciAddressInfoS    addressInfo; 
} LfciOseAttachClientDownS;


#define LFCI_FEATURE_LICENSE_SUBSCRIBE2_CFM (LICENSE_FEATURE_CTRL_SIGBASE_CC + 10)    /* !- SIGNO(struct LfciFeatureLicenseSubscribe2CfmS) -! */

/*****************************************************************************
 * Signal: LFCI_FEATURE_LICENSE_SUBSCRIBE2_CFM (0x1741d0a)
 *//**
 * @brief                      Confirmation of a feature license
 *                             subscription request.
 *
 *                             Valid for Protocol revision: 2
 *
 * @param sizeOfLicenseInfo    Indicates number of valid elements in dynamic
 *                             array.
 *
 * @param licenseInfo[]        Dynamic array containing information for
 *                             several licenses.
 *
 ****************************************************************************/
typedef struct LfciFeatureLicenseSubscribe2CfmS
{
  uint32_t            sigNo;
  LfciAddressInfoS    addressInfo;
  uint32_t            sizeOfLicenseInfo;
  LfciLicenseInfo2S   licenseInfo[1]; /* Placeholder to create pointer to array, length indicated by sizeOfLicenseInfo */
} LfciFeatureLicenseSubscribe2CfmS;


#define LFCI_FEATURE_LICENSE_CHANGE2_IND (LICENSE_FEATURE_CTRL_SIGBASE_CC + 11)    /* !- SIGNO(struct LfciFeatureLicenseChange2IndS) -! */

/*****************************************************************************
 * Signal: LFCI_FEATURE_LICENSE_CHANGE2_IND (0x1741d0b)
 *//**
 * @brief                      This signal is sent to indicate that the
 *                             content in the LKF, or the featureState
 *                             attribute in the OptionalFeatureLicense MO
 *                             instance, has been changed for the license.
 *
 *                             Valid for Protocol revision: 2
 *
 * @param licenseInfo          License information.
 *
 ****************************************************************************/
typedef struct LfciFeatureLicenseChange2IndS
{
  uint32_t            sigNo;
  LfciAddressInfoS    addressInfo;
  LfciLicenseInfo2S   licenseInfo;
} LfciFeatureLicenseChange2IndS;

/*************************** OSE signal: attach *****************************/
#define LFCI_ATTACH (LICENSE_FEATURE_CTRL_SIGBASE_CC + 12)

/**************************** OSE signal: hunt ******************************/
#define LFCI_HUNT (LICENSE_FEATURE_CTRL_SIGBASE_CC + 13)

/******************** Double inclusion protection (end) *********************/
#endif /* LICENSE_FEATURE_CONTROL_I_SIG */
