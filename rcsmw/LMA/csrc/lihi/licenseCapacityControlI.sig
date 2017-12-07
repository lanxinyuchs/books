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

This interface is used for handling of the capacity feature license part of 
the LIHI (LIicense Handler Interface).
The License Capacity Control Interface (LCCI) is registered in the name 
server with the name LICENSE_CAPACITY_CONTROL_I_SERVICE_NAME.

The LCCI product has the following procedures:
################################################################
- Connection Establish
- Subscription
- Unsubscription
- Change
- Disconnect
- Grace Period Activated

Subscriptions on this interface will trigger internal subscriptions for 
information in the License Key File.
Multiple subscriptions towards the same license key is supported.
When the first License User subscribes for an capacity feature license this 
will trigger a MO creation operation.
When the last License User removes his subscription this will result in that 
the MO will be marked for deletion.

The License Users will be notified when the license status is updated.
No reject signals are specified for the subscribe/unsubscribe procedures. 
Incorrect behavior by the License User will be regarded as a critical fault.
In these scenarios the License Handler will send the signal 
LcciCapacityLicenseDisconnectInd and disconnect the License User.

-------------------------------------------------------------------------
Generic description
-------------------------------------------------------------------------
Frequently used data fields:

sigNo           Indicates the signal number the struct represents.
addressInfo     clientRef identifies the requesting client.
addressInfo     serverRef identifies the providing server.

All character strings shall be null terminated.

----------------------------------------------------------------------------
*/


/*********************** Double inclusion protection ************************/
#ifndef LICENSE_CAPACITY_CONTROL_I_SIG
#define LICENSE_CAPACITY_CONTROL_I_SIG

/******************************** Includes **********************************/
#if (defined MAKE_WITH_LPP) || (defined MAKE_OSE_2_LINX_TYPES)

#ifdef MAKE_WITH_LPP
#include "lpp.h"
#endif

#ifdef MAKE_OSE_2_LINX_TYPES
/* Used to convert from OSE types names to LINX types name for signal description
 * files used both in OSE and LINX Linux environment.
 */
#include "daoOse2LinxTypes.h"
#endif

#endif

#include "licenseCapacityControlICommon.h"

#define LICENSE_CAPACITY_CTRL_SIGBASE_CC (0x1741e00)

#define LCCI_CONN_TO_SERVER_REQ (LICENSE_CAPACITY_CTRL_SIGBASE_CC + 0)    /* !- SIGNO(struct LcciConnToServerReqS) -! */

/*****************************************************************************
 * Signal: LCCI_CONN_TO_SERVER_REQ (0x1741e00)
 *//**
 * @brief                      The License User (client) shall send the 
 *                             signal to establish a connection with the 
 *                             License Handler (server).
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
typedef struct LcciConnToServerReqS
{
  uint32_t            sigNo; 
  LcciAddressInfoS    addressInfo; 
  uint32_t            sizeOfProtocolRevisions; 
  uint32_t            protocolRevisions[1]; /* Placeholder to create pointer to array, length indicated by sizeOfProtocolRevisions */
} LcciConnToServerReqS;


#define LCCI_CONN_TO_SERVER_CFM (LICENSE_CAPACITY_CTRL_SIGBASE_CC + 1)    /* !- SIGNO(struct LcciConnToServerCfmS) -! */

/*****************************************************************************
 * Signal: LCCI_CONN_TO_SERVER_CFM (0x1741e01)
 *//**
 * @brief                      Indicates that a connection has been 
 *                             established and that the License User can 
 *                             continue with the other procedures
 *                             
 * @param protocolRevision     Selected protocol revision.
 *                             
 ****************************************************************************/
typedef struct LcciConnToServerCfmS
{
  uint32_t            sigNo; 
  LcciAddressInfoS    addressInfo; 
  uint32_t            protocolRevision; 
} LcciConnToServerCfmS;


#define LCCI_CONN_TO_SERVER_REJ (LICENSE_CAPACITY_CTRL_SIGBASE_CC + 2)    /* !- SIGNO(struct LcciConnToServerRejS) -! */

/*****************************************************************************
 * Signal: LCCI_CONN_TO_SERVER_REJ (0x1741e02)
 *//**
 * @brief                      Indicates that the connection establish 
 *                             procedure has failed and that the License 
 *                             User not can continue with any other 
 *                             procedure.
 *                             
 * @param errorMsg[]           Error information indicating why the 
 *                             connection establish request has been 
 *                             rejected.
 *                             
 ****************************************************************************/
typedef struct LcciConnToServerRejS
{
  uint32_t            sigNo; 
  LcciAddressInfoS    addressInfo; 
  char                errorMsg[MAX_SIZE_OF_LCCI_ERROR_MSG]; 
} LcciConnToServerRejS;


#define LCCI_CAPACITY_LICENSE_SUBSCRIBE_REQ (LICENSE_CAPACITY_CTRL_SIGBASE_CC + 3)    /* !- SIGNO(struct LcciCapacityLicenseSubscribeReqS) -! */

/*****************************************************************************
 * Signal: LCCI_CAPACITY_LICENSE_SUBSCRIBE_REQ (0x1741e03)
 *//**
 * @brief                      License User subscribes for capacity feature 
 *                             licenses.
 *                             Corresponding capacity feature MO instances 
 *                             will be created, in case they not already 
 *                             exist.
 *                             
 * @param sizeOfLicenseData    Indicates number of valid elements in dynamic 
 *                             array. Minimum allowed value is 1 and maximum 
 *                             allowed value is 100 licenses.
 *                             
 * @param licenseData[]        Dynamic array containing information for 
 *                             several licenses. 
 *                             
 ****************************************************************************/
typedef struct LcciCapacityLicenseSubscribeReqS
{
  uint32_t            sigNo; 
  LcciAddressInfoS    addressInfo; 
  uint32_t            sizeOfLicenseData; 
  LcciLicenseDataS    licenseData[1]; /* Placeholder to create pointer to array, length indicated by sizeOfLicenseData */
} LcciCapacityLicenseSubscribeReqS;


#define LCCI_CAPACITY_LICENSE_SUBSCRIBE_CFM (LICENSE_CAPACITY_CTRL_SIGBASE_CC + 4)    /* !- SIGNO(struct LcciCapacityLicenseSubscribeCfmS) -! */

/*****************************************************************************
 * Signal: LCCI_CAPACITY_LICENSE_SUBSCRIBE_CFM (0x1741e04)
 *//**
 * @brief                      Confirmation of a capacity feature license 
 *                             subscription request.
 *                             
 * @param sizeOfLicenseInfo    Indicates number of valid elements in dynamic 
 *                             array.
 *                             
 * @param licenseInfo[]        Dynamic array containing information for 
 *                             several licenses.
 *                             
 ****************************************************************************/
typedef struct LcciCapacityLicenseSubscribeCfmS
{
  uint32_t            sigNo; 
  LcciAddressInfoS    addressInfo; 
  uint32_t            sizeOfLicenseInfo; 
  LcciLicenseInfoS    licenseInfo[1]; /* Placeholder to create pointer to array, length indicated by sizeOfLicenseInfo */
} LcciCapacityLicenseSubscribeCfmS;


#define LCCI_CAPACITY_LICENSE_UNSUBSCRIBE_REQ (LICENSE_CAPACITY_CTRL_SIGBASE_CC + 5)    /* !- SIGNO(struct LcciCapacityLicenseUnsubscribeReqS) -! */

/*****************************************************************************
 * Signal: LCCI_CAPACITY_LICENSE_UNSUBSCRIBE_REQ (0x1741e05)
 *//**
 * @brief                      License User request that subscriptions of 
 *                             capacity feature licenses are removed. The 
 *                             server will respond positively with a 
 *                             confirmation, even if the license does not 
 *                             exist. MO instances, where the corresponding 
 *                             license have no other subscribers, will be 
 *                             marked for deletion.
 *                             
 * @param sizeOfLicenseData    Indicates number of valid elements in dynamic 
 *                             array. Minimum allowed value is 1 and maximum 
 *                             allowed value is 100 licenses.
 *                             
 * @param licenseData[]        Dynamic array containing license key id.
 *                             
 ****************************************************************************/
typedef struct LcciCapacityLicenseUnsubscribeReqS
{
  uint32_t            sigNo; 
  LcciAddressInfoS    addressInfo; 
  uint32_t            sizeOfLicenseData; 
  LcciKeyDataS        licenseData[1]; /* Placeholder to create pointer to array, length indicated by sizeOfLicenseData */
} LcciCapacityLicenseUnsubscribeReqS;


#define LCCI_CAPACITY_LICENSE_UNSUBSCRIBE_CFM (LICENSE_CAPACITY_CTRL_SIGBASE_CC + 6)    /* !- SIGNO(struct LcciCapacityLicenseUnsubscribeCfmS) -! */

/*****************************************************************************
 * Signal: LCCI_CAPACITY_LICENSE_UNSUBSCRIBE_CFM (0x1741e06)
 *//**
 * @brief                      Confirmation of a capacity feature license 
 *                             remove subscription request.
 *                             
 * @param sizeOfLicenseInfo    Indicates number of valid elements in dynamic 
 *                             array.
 *                             
 * @param licenseInfo[]        Dynamic array containing information for 
 *                             several licenses.
 *                             
 ****************************************************************************/
typedef struct LcciCapacityLicenseUnsubscribeCfmS
{
  uint32_t            sigNo; 
  LcciAddressInfoS    addressInfo; 
  uint32_t            sizeOfLicenseInfo; 
  LcciKeyDataS        licenseInfo[1]; /* Placeholder to create pointer to array, length indicated by sizeOfLicenseInfo */
} LcciCapacityLicenseUnsubscribeCfmS;


#define LCCI_CAPACITY_LICENSE_CHANGE_IND (LICENSE_CAPACITY_CTRL_SIGBASE_CC + 7)    /* !- SIGNO(struct LcciCapacityLicenseChangeIndS) -! */

/*****************************************************************************
 * Signal: LCCI_CAPACITY_LICENSE_CHANGE_IND (0x1741e07)
 *//**
 * @brief                      Used to indicate that the capacityLimit, or 
 *                             another attribute, has been updated.
 *                             
 * @param licenseInfo          License information.
 *                             
 ****************************************************************************/
typedef struct LcciCapacityLicenseChangeIndS
{
  uint32_t            sigNo; 
  LcciAddressInfoS    addressInfo; 
  LcciLicenseInfoS    licenseInfo; 
} LcciCapacityLicenseChangeIndS;


#define LCCI_CAPACITY_LICENSE_DISCONNECT_IND (LICENSE_CAPACITY_CTRL_SIGBASE_CC + 8)    /* !- SIGNO(struct LcciCapacityLicenseDisconnectIndS) -! */

/*****************************************************************************
 * Signal: LCCI_CAPACITY_LICENSE_DISCONNECT_IND (0x1741e08)
 *//**
 * @brief                      This signal indicates that the server has 
 *                             terminated the connection, due to incorrect 
 *                             behavior by the client.
 *                             
 * @param errorInfo[]          Error information indicating why the client 
 *                             has been disconnected.
 *                             
 ****************************************************************************/
typedef struct LcciCapacityLicenseDisconnectIndS
{
  uint32_t            sigNo; 
  LcciAddressInfoS    addressInfo; 
  char                errorInfo[MAX_SIZE_OF_LCCI_CAPACITY_LICENSE_DISCONNECT_IND_S_ERROR_INFO]; 
} LcciCapacityLicenseDisconnectIndS;


#define LCCI_CAPACITY_LICENSE_GP_ACTIVATED_FWD (LICENSE_CAPACITY_CTRL_SIGBASE_CC + 9)    /* !- SIGNO(struct LcciCapacityLicenseGpActivatedFwdS) -! */

/*****************************************************************************
 * Signal: LCCI_CAPACITY_LICENSE_GP_ACTIVATED_FWD (0x1741e09)
 *//**
 * @brief                      This signal is sent from the License User 
 *                             when the Grace Period state is activated.
 *                             
 * @param licenseKeyId[]       The unique identifier, currently a CXC 
 *                             number, for the license.
 *                             
 ****************************************************************************/
typedef struct LcciCapacityLicenseGpActivatedFwdS
{
  uint32_t            sigNo; 
  LcciAddressInfoS    addressInfo; 
  char                licenseKeyId[MAX_SIZE_OF_LCCI_CAPACITY_LICENSE_GP_ACTIVATED_FWD_S_LICENSE_KEY_ID]; 
} LcciCapacityLicenseGpActivatedFwdS;


#define LCCI_OSE_ATTACH_CLIENT_DOWN (LICENSE_CAPACITY_CTRL_SIGBASE_CC + 10)    /* !- SIGNO(struct LcciOseAttachClientDownS) -! */

/*****************************************************************************
 * Signal: LCCI_OSE_ATTACH_CLIENT_DOWN (0x1741e0a)
 *//**
 * @brief                      Signal used by the License Handler, indicates 
 *                             that a client has gone down.
 *                             
 ****************************************************************************/
typedef struct LcciOseAttachClientDownS
{
  uint32_t            sigNo; 
  LcciAddressInfoS    addressInfo; 
} LcciOseAttachClientDownS;

#define LCCI_CAPACITY_LICENSE_SUBSCRIBE2_CFM (LICENSE_CAPACITY_CTRL_SIGBASE_CC + 11)    /* !- SIGNO(struct LcciCapacityLicenseSubscribe2CfmS) -! */

/*****************************************************************************
 * Signal: LCCI_CAPACITY_LICENSE_SUBSCRIBE2_CFM (0x1741e0b)
 *//**
 * @brief                      Confirmation of a capacity feature license 
 *                             subscription request.
 *                             
 * @param sizeOfLicenseInfo    Indicates number of valid elements in dynamic 
 *                             array.
 *                             
 * @param licenseInfo[]        Dynamic array containing information for 
 *                             several licenses.
 *                             
 ****************************************************************************/
typedef struct LcciCapacityLicenseSubscribe2CfmS
{
  uint32_t            sigNo; 
  LcciAddressInfoS    addressInfo; 
  uint32_t            sizeOfLicenseInfo; 
  LcciLicenseInfo2S    licenseInfo[1]; /* Placeholder to create pointer to array, length indicated by sizeOfLicenseInfo */
} LcciCapacityLicenseSubscribe2CfmS;


#define LCCI_CAPACITY_LICENSE_CHANGE2_IND (LICENSE_CAPACITY_CTRL_SIGBASE_CC + 12)    /* !- SIGNO(struct LcciCapacityLicenseChange2IndS) -! */

/*****************************************************************************
 * Signal: LCCI_CAPACITY_LICENSE_CHANGE2_IND (0x1741e0c)
 *//**
 * @brief                      Used to indicate that the capacityLimit, or 
 *                             another attribute, has been updated.
 *                             
 * @param licenseInfo          License information.
 *                             
 ****************************************************************************/
typedef struct LcciCapacityLicenseChange2IndS
{
  uint32_t            sigNo; 
  LcciAddressInfoS    addressInfo; 
  LcciLicenseInfo2S   licenseInfo;
} LcciCapacityLicenseChange2IndS;



/*************************** OSE signal: attach *****************************/
#define LCCI_ATTACH (LICENSE_CAPACITY_CTRL_SIGBASE_CC + 13)

/**************************** OSE signal: hunt ******************************/
#define LCCI_HUNT (LICENSE_CAPACITY_CTRL_SIGBASE_CC + 14)


/******************** Double inclusion protection (end) *********************/
#endif /* LICENSE_CAPACITY_CONTROL_I_SIG */
