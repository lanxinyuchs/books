/*
 *
 * Copyright (c) Ericsson AB  2017 All rights reserved.
 *
 * The information in this document is the property of Ericsson. Except
 * as specifically authorized in writing by Ericsson,the receiver of this
 * document shall keep the information contained herein confidential
 * and shall protect the same in whole or in part from disclosure and
 * dissemination to third parties. Disclosure and disseminations to the
 * receiver's employees shall only be made on a strict need to know basis.
 */

/**
 *  @file
 *  @brief Signal file for GLMS Adapter interface.
 *
 *  This file contains the signal definitions of the Generic
 *  License Management Solution (GLMS) adapter interface.
 *
 *  !!! Signals from 0x01900100 to 0x019001ef are reserved for GLMS_ADPI !!!
 */

#ifndef GLMS_ADAPTER_SIG
#define GLMS_ADAPTER_SIG

/** @defgroup adpisig Adapter Interface Signals
 * @{
 */

#ifdef __cplusplus
extern "C" {
#endif

#include "glmsadpi/glmsDataTypes.h"

/**************************************************************************//**
 *
 * Signal : GlmsAdpiActivateReq
 *
 * Descr  : Used by the adaptation layer to activate the GLMS component. After
 *          activation the GLMS component will publish the LIHI interface to
 *          in the nameserver. It will be possible for LIHI clients to use the
 *          LIHI service and it will be possible for the adaptation layer to
 *          send other requests, such as emergency unlock or set fingerprint,
 *          to GLMS.
 *
 * Dir    : Adapter --> GLMS
 *
 * Data   : protocolVersion    - The protocol version as selected by the
 *                               adapter. If the protocol version is not
 *                               supported the activate request will be
 *                               rejected by GLMS.
 *
 *          logLevel           - Set the log level of GLMS. Log events can
 *                               be expected by the adapter from the moment
 *                               it receives the GlmsAdpiActivateReq message.
 *
 *          restrictedLicenses - A list of all licenses that are restricted.
 *                               null terminated string where no spaces are 
 *                               allowed, licenses should be separated by 
 *                               semicolons (;).
 *
 *****************************************************************************/
typedef struct
{
   uint32_t      sigNo;
   uint32_t      protocolVersion;
   GlmsLogLevel  logLevel;
   char          restrictedLicenses[1];
} GlmsAdpiActivateReq;
#define GLMS_ADPI_ACTIVATE_REQ (0x1900100) /**< @brief GlmsAdpiActivateReq */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiActivateRsp
 *
 * Descr  : The response to an activation request.
 *
 * Dir    : GLMS --> Adapter
 *
 * Data   : result             - GLMS_OK if GLMS was activated.
 *                               GLMS_NOK if the GLMS was not activated.
 *
 *          highestSupportedPv - GLMS highest supported protocol version
 *
 *          resultInfo         - Information about reject cause if result is NOK
 *
 *****************************************************************************/
typedef struct
{
   uint32_t     sigNo;
   GlmsResult   result;
   uint32_t     highestSupportedPv;
   char         resultInfo[GLMS_RESULT_INFO_LEN];
} GlmsAdpiActivateRsp;
#define GLMS_ADPI_ACTIVATE_RSP (0x1900101) /**< @brief GlmsAdpiActivateRsp */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiSetFingerprintReq
 *
 * Descr  : Used by the adaptation layer to request GLMS to update
 *          the fingerprint.
 *
 * Dir    : Adapter --> GLMS
 *
 * Data   : fingerprint        - The new fingerprint.
 *
 *****************************************************************************/
typedef struct
{
   uint32_t     sigNo;
   char         fingerprint[GLMS_FINGERPRINT_LEN];
} GlmsAdpiSetFingerprintReq;
#define GLMS_ADPI_SET_FINGERPRINT_REQ (0x1900102) /**< @brief GlmsAdpiSetFingerprintReq */

/**************************************************************************//**
 *
 * Signal : GlmsAdpiSetFingerprintRsp
 *
 * Descr  : The response to a set fingerprint request.
 *
 * Dir    : GLMS --> Adapter
 *
 * Data   : result             - GLMS_OK if the set was accepted and successful.
 *                               GLMS_NOK if the set was rejected or failed.
 *
 *          resultInfo         - Information about reject cause if result is NOK
 *
 *****************************************************************************/
typedef struct
{
   uint32_t     sigNo;
   GlmsResult   result;
   char         resultInfo[GLMS_RESULT_INFO_LEN];
} GlmsAdpiSetFingerprintRsp;
#define GLMS_ADPI_SET_FINGERPRINT_RSP (0x1900103) /**< @brief GlmsAdpiSetFingerprintRsp */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiInstallKeyFileReq
 *
 * Descr  : Used by the adaptation layer to request GLMS to install
 *          a new Key File.
 *
 * Dir    : Adapter --> GLMS
 *
 * Data   : kfUri        - The full URI to the Key File.
 *                          Ex. 1: "file://data/dir/subdir/rbs006.xml"
 *                          Ex. 2: "/data/dir/subdir/rbs006.xml"
 *                          Ex. 3: "sftp://myuser@myhostname.ericsson.local/dir/subdir/rbs006.xml"
 *                          Ex. 4: "ftpes://myuser@myhostname.ericsson.local/dir/subdir/rbs006.xml"
 *
 *          password      - The password to the FTP server if the file is located
 *                          on one. If no password is required this shall be an
 *                          empty string.
 *
 *****************************************************************************/
typedef struct
{
   uint32_t    sigNo;
   char        kfUri[GLMS_KEYFILE_URI_LEN];
   char        password[GLMS_PASSWORD_LEN];
} GlmsAdpiInstallKeyFileReq;
#define GLMS_ADPI_INSTALL_KEY_FILE_REQ (0x1900104) /**< @brief GlmsAdpiInstallKeyFileReq */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiInstallKeyFileRsp
 *
 * Descr  : The response to a install key file request.
 *
 * Dir    : GLMS --> Adapter
 *
 * Data   : result               - GLMS_OK if the request was accepted and successful.
 *                                 GLMS_NOK if the request was rejected or failed.
 *
 *          actionId             - The id of the install action.
 *
 *****************************************************************************/
typedef struct
{
   uint32_t     sigNo;
   GlmsResult   result;
   uint32_t     actionId;
} GlmsAdpiInstallKeyFileRsp;
#define GLMS_ADPI_INSTALL_KEY_FILE_RSP (0x1900105) /**< @brief GlmsAdpiInstallKeyFileRsp */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiInstallKeyFileInd
 *
 * Descr  : The result of an install key file action.
 *
 * Dir    : GLMS --> Adapter
 *
 * Data   : result               - GLMS_OK if the set was accepted and successful.
 *                                 GLMS_NOK if the set was rejected or failed.
 *
 *          additionalInfo       - Contains information why the action failed in the
 *                                 case when the result is GLMS_NOK.
 *
 *****************************************************************************/
typedef struct
{
   uint32_t     sigNo;
   GlmsResult   result;
   char         additionalInfo[GLMS_ADDITIONAL_INFO_LEN];
} GlmsAdpiInstallKeyFileInd;
#define GLMS_ADPI_INSTALL_KEY_FILE_IND (0x1900106) /**< @brief GlmsAdpiInstallKeyFileInd */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiDownloadKeyFileReq
 *
 * Descr  : A request from GLMS to the adapter layer to download a key file.
 *
 * Dir    : GLMS --> Adapter
 *
 * Data   : kfUri        - The full URI to the Key File.
 *                          Ex. 1: "sftp://myuser@myhostname.ericsson.local/dir/subdir/rbs006.xml"
 *                          Ex. 2: "ftp://myhostname.ericsson.local/dir/subdir/rbs006.xml"
 *                          Ex. 3: "ftpes://myuser@myhostname.ericsson.local/dir/subdir/rbs006.xml"
 *
 *          password      - The password to the FTP server. If no password is required
 *                          this shall be an empty string.
 *
 *****************************************************************************/
typedef struct
{
   uint32_t     sigNo;
   char         kfUri[GLMS_KEYFILE_URI_LEN];
   char         password[GLMS_PASSWORD_LEN];
} GlmsAdpiDownloadKeyFileReq;
#define GLMS_ADPI_DOWNLOAD_KEY_FILE_REQ (0x1900107) /**< @brief GlmsAdpiDownloadKeyFileReq */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiDownloadKeyFileRsp
 *
 * Descr  : A response to GlmsAdpiDownloadKeyFileReq.
 *
 * Dir    : Adapter --> GLMS
 *
 * Data   : result        - GLMS_OK if the request was accepted and successful.
 *                          GLMS_NOK if the request was rejected or failed.
 *
 *          kfLocation    - The location of the file on local disk. Ex. "/my/path/my_keyfile.xml"
 *
 *****************************************************************************/
typedef struct
{
   uint32_t     sigNo;
   GlmsResult   result;
   char         kfLocation[GLMS_KEYFILE_LOCATION_LEN];
} GlmsAdpiDownloadKeyFileRsp;
#define GLMS_ADPI_DOWNLOAD_KEY_FILE_RSP (0x1900108) /**< @brief GlmsAdpiDownloadKeyFileRsp */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiStoreKeyFileReq
 *
 * Descr  : A request from GLMS to the adapter layer to store the key file. The
 *          file is also expected to be stored for later use, in which case it
 *          will be requested with a call to GlmsAdpiGetKeyFileLocationReq.
 *
 * Dir    : GLMS --> Adapter
 *
 * Data   : kfLocation    - The path to the Key File.
 *                          Ex.: "/my/path/my_keyfile.xml"
 *
 *****************************************************************************/
typedef struct
{
   uint32_t     sigNo;
   char         kfLocation[GLMS_KEYFILE_LOCATION_LEN];
} GlmsAdpiStoreKeyFileReq;
#define GLMS_ADPI_STORE_KEY_FILE_REQ (0x1900109) /**< @brief GlmsAdpiStoreKeyFileReq */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiStoreKeyFileRsp
 *
 * Descr  : A response to GlmsAdpiStoreKeyFileReq.
 *
 * Dir    : Adapter --> GLMS
 *
 * Data   : result        - GLMS_OK if the request was accepted and successful.
 *                          GLMS_NOK if the request was rejected or failed.
 *
 *          kfLocation    - The path to the Key File.
 *                          Ex.: "/my/path/my_keyfile.xml"
 *
 *****************************************************************************/
typedef struct
{
   uint32_t     sigNo;
   GlmsResult   result;
   char         kfLocation[GLMS_KEYFILE_LOCATION_LEN];
} GlmsAdpiStoreKeyFileRsp;
#define GLMS_ADPI_STORE_KEY_FILE_RSP (0x190010a) /**< @brief GlmsAdpiStoreKeyFileRsp */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiCreateFeatureKeyMoReq
 *
 * Descr  : A request from GLMS to the adapter layer to create a new FeatureKey
 *          MO instance.
 *
 * Dir    : GLMS --> Adapter
 *
 * Data   : validFrom     - The start date of the license.
 *
 *          expiration    - The end date of the license.
 *
 *          shared        - True if the key is from an area key file. False otherwise.
 *
 *          featureKeyId  - The feature key id. This is the key value and will later
 *                          be used to identify the feature key.
 *
 *          keyId         - The license number (CXC...)
 *
 *          name          - The name of the license.
 *
 *          productType   - The product type of the license.
 *
 *****************************************************************************/
typedef struct
{
   uint32_t          sigNo;
   time_t32          validFrom;
   time_t32          expiration;
   GlmsBool          shared;
   char              featureKeyId[GLMS_MO_KEY_LEN];
   char              keyId[GLMS_KEY_ID_LEN];
   char              name[GLMS_KEY_NAME_LEN];
   char              productType[GLMS_PRODUCT_TYPE_LEN];
} GlmsAdpiCreateFeatureKeyMoReq;
#define GLMS_ADPI_CREATE_FEATURE_KEY_MO_REQ (0x190010b) /**< @brief GlmsAdpiCreateFeatureKeyMoReq */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiCreateFeatureKeyMoRsp
 *
 * Descr  : The response to a GlmsAdpiCreateFeatureKeyMoReq.
 *
 * Dir    : Adapter --> GLMS
 *
 * Data   : result        - GLMS_OK if the request was accepted and successful.
 *                          GLMS_NOK if the request was rejected or failed.
 *
 *          featureKeyId  - The feature key id as specified in the request signal.
 *          
 *****************************************************************************/
typedef struct
{
   uint32_t          sigNo;
   GlmsResult        result;
   char              featureKeyId[GLMS_MO_KEY_LEN];
} GlmsAdpiCreateFeatureKeyMoRsp;
#define GLMS_ADPI_CREATE_FEATURE_KEY_MO_RSP (0x190010c) /**< @brief GlmsAdpiCreateFeatureKeyMoRsp */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiSetFeatureStateReq
 *
 * Descr  : A request from adapter layer to GLMS to set the feature state of
 *          a FeatureKey MO.
 *
 * Dir    : Adapter --> GLMS
 *
 * Data   : featureState    - The new feature state.
 *
 *          featureStateId  - The id of the FeatureState MO that shall be updated.
 *
 *****************************************************************************/
typedef struct
{
   uint32_t          sigNo;
   GlmsFeatureState  featureState;
   char              featureStateId[GLMS_MO_KEY_LEN];
} GlmsAdpiSetFeatureStateReq;
#define GLMS_ADPI_SET_FEATURE_STATE_REQ (0x190010d) /**< @brief GlmsAdpiSetFeatureStateReq */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiSetFeatureStateRsp
 *
 * Descr  : The response to a GlmsAdpiSetFeatureStateReq.
 *
 * Dir    : GLMS --> Adapter
 *
 * Data   : result          - GLMS_OK if the request was accepted and successful.
 *                            GLMS_NOK if the request was rejected or failed.
 *
 *          resultInfo      - Information about reject cause if result is NOK.
 *
 *          featureStateId  - The id of the FeatureState MO that was updated.
 *
 *****************************************************************************/
typedef struct
{
   uint32_t          sigNo;
   GlmsResult        result;
   char              resultInfo[GLMS_RESULT_INFO_LEN];
   char              featureStateId[GLMS_MO_KEY_LEN];
} GlmsAdpiSetFeatureStateRsp;
#define GLMS_ADPI_SET_FEATURE_STATE_RSP (0x190010e) /**< @brief GlmsAdpiSetFeatureStateRsp */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiGetKeyFileLocationReq
 *
 * Descr  : A request from GLMS to adapter to ask for the location of the most
 *          recently stored KeyFile.
 *
 * Dir    : GLMS --> Adapter
 *
 * Data   : -
 *
 *****************************************************************************/
typedef struct
{
   uint32_t          sigNo;
} GlmsAdpiGetKeyFileLocationReq;
#define GLMS_ADPI_GET_KEY_FILE_LOCATION_REQ (0x190010f) /**< @brief GlmsAdpiGetKeyFileLocationReq */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiGetKeyFileLocationRsp
 *
 * Descr  : Response to GlmsAdpiGetKeyFileLocationReq.
 *
 * Dir    : Adapter --> GLMS
 *
 * Data   : result        - GLMS_OK if the request was accepted and successful.
 *                          GLMS_NOK if the request was rejected or failed.
 *
 *          kfLocation    - The path to the Key File.
 *                          Ex.: "/my/path/my_keyfile.xml"
 *
 *****************************************************************************/
typedef struct
{
   uint32_t          sigNo;
   GlmsResult        result;
   char              kfLocation[GLMS_KEYFILE_LOCATION_LEN];
} GlmsAdpiGetKeyFileLocationRsp;
#define GLMS_ADPI_GET_KEY_FILE_LOCATION_RSP (0x1900110) /**< @brief GlmsAdpiGetKeyFileLocationRsp */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiDeactivateReq
 *
 * Descr  : Used by the adaptation layer to deactivate the GLMS component. All
 *          LIHI clients will be disconnected and all runtime data are cleared
 *          by GLMS. After deactivation the GLMS component will unpublish the
 *          LIHI interfaces from the nameserver.
 *
 * Dir    : Adapter --> GLMS
 *
 * Data   : -
 *
 *****************************************************************************/
typedef struct
{
   uint32_t     sigNo;
} GlmsAdpiDeactivateReq;
#define GLMS_ADPI_DEACTIVATE_REQ (0x1900111) /**< @brief GlmsAdpiDeactivateReq */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiDeactivateRsp
 *
 * Descr  : The response to a deactivation request.
 *
 * Dir    : GLMS --> Adapter
 *
 * Data   : result             - GLMS_OK if GLMS was deactivated.
 *                               GLMS_NOK if the GLMS was not deactivated.
 *
 *          resultInfo         - Information about reject cause if result is NOK
 *
 *****************************************************************************/
typedef struct
{
   uint32_t     sigNo;
   GlmsResult   result;
   char         resultInfo[GLMS_RESULT_INFO_LEN];
} GlmsAdpiDeactivateRsp;
#define GLMS_ADPI_DEACTIVATE_RSP (0x1900112) /**< @brief GlmsAdpiDeactivateRsp */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiDeleteFeatureKeyMoReq
 *
 * Descr  : A request from GLMS to the adapter layer to delete a FeatureKey
 *          MO instance.
 *
 * Dir    : GLMS --> Adapter
 *
 * Data   : featureKeyId  - The feature key id of the FeatureKey MO to delete.
 *
 *****************************************************************************/
typedef struct
{
   uint32_t          sigNo;
   char              featureKeyId[GLMS_MO_KEY_LEN];
} GlmsAdpiDeleteFeatureKeyMoReq;
#define GLMS_ADPI_DELETE_FEATURE_KEY_MO_REQ (0x1900113) /**< @brief GlmsAdpiDeleteFeatureKeyMoReq */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiDeleteFeatureKeyMoRsp
 *
 * Descr  : The response to a GlmsAdpiDeleteFeatureKeyMoReq.
 *
 * Dir    : Adapter --> GLMS
 *
 * Data   : result        - GLMS_OK if the request was accepted and successful.
 *                          GLMS_NOK if the request was rejected or failed.
 *
 *          featureKeyId  - The feature key id of the FeatureKey MO to delete.
 *
 *****************************************************************************/
typedef struct
{
   uint32_t          sigNo;
   GlmsResult        result;
   char              featureKeyId[GLMS_MO_KEY_LEN];
} GlmsAdpiDeleteFeatureKeyMoRsp;
#define GLMS_ADPI_DELETE_FEATURE_KEY_MO_RSP (0x1900114) /**< @brief GlmsAdpiDeleteFeatureKeyMoRsp */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiPkiVerificationReq
 *
 * Descr  : A request from GLMS to the adapter layer to verify a key file.
 *
 * Dir    : GLMS --> Adapter
 *
 * Data   : kfLocation  - The location of the key file.
 *
 *****************************************************************************/
typedef struct
{
   uint32_t          sigNo;
   char              kfLocation[GLMS_KEYFILE_LOCATION_LEN];
} GlmsAdpiPkiVerificationReq;
#define GLMS_ADPI_PKI_VERIFICATION_REQ (0x1900115) /**< @brief GlmsAdpiPkiVerificationReq */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiPkiVerificationRsp
 *
 * Descr  : A response to GlmsAdpiPkiVerificationReq.
 *
 * Dir    : Adapter --> GLMS
 *
 * Data   : result        - GLMS_OK, the key file was verfied to be ok.
 *                          GLMS_NOK, the key file had a bad PKI signature
 *                                    or the verification failed.
 *
 *          kfLocation    - The location of the key file.
 *
 *****************************************************************************/
typedef struct
{
   uint32_t          sigNo;
   GlmsResult        result;
   char              kfLocation[GLMS_KEYFILE_LOCATION_LEN];
} GlmsAdpiPkiVerificationRsp;
#define GLMS_ADPI_PKI_VERIFICATION_RSP (0x1900116) /**< @brief GlmsAdpiPkiVerificationRsp */

/**************************************************************************//**
 *
 * Signal : GlmsAdpiHeartbeatReq
 *
 * Descr  : A request from the adapter layer to GLMS to know if it is responding or hanging.
 *
 * Dir    : Adapter --> GLMS
 *
 * Data   : -
 *
 *****************************************************************************/
typedef struct
{
   uint32_t          sigNo;
} GlmsAdpiHeartbeatReq;
#define GLMS_ADPI_HEARTBEAT_REQ (0x1900117) /**< @brief GlmsAdpiHeartbeatReq */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiHeartbeatRsp
 *
 * Descr  : A response to GlmsAdpiHeartbeatReq.
 *
 * Dir    : GLMS --> Adapter
 *
 * Data   : -
 *
 *****************************************************************************/
typedef struct
{
   uint32_t          sigNo;
} GlmsAdpiHeartbeatRsp;
#define GLMS_ADPI_HEARTBEAT_RSP (0x1900118) /**< @brief GlmsAdpiHeartbeatRsp */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiActivateEuReq
 *
 * Descr  : A request from the adapter layer to request the GLMS to activate the emergency unlock.
 *
 * Dir    : Adapter --> GLMS
 *
 * Data   : -
 *
 *****************************************************************************/
typedef struct
{
   uint32_t          sigNo;
} GlmsAdpiActivateEuReq;
#define GLMS_ADPI_ACTIVATE_EU_REQ (0x1900119) /**< @brief GlmsAdpiActivateEuReq */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiActivateEuRsp
 *
 * Descr  : A response to GlmsAdpiActivateEuReq.
 *
 * Dir    : GLMS --> Adapter
 *
 * Data   : -
 *
 *****************************************************************************/
typedef struct
{
   uint32_t          sigNo;
   GlmsResult        result;
   char              resultInfo[GLMS_RESULT_INFO_LEN];
} GlmsAdpiActivateEuRsp;
#define GLMS_ADPI_ACTIVATE_EU_RSP (0x190011a) /**< @brief GlmsAdpiActivateEuRsp */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiRefreshLicenseInventoryReq
 *
 * Descr  : A request from the adapter layer to request the GLMS to refresh the
 *          license inventory. This will result in GLMS to attempt to re-read
 *          the installed KeyFile and revalidate all licenses.
 *
 * Dir    : Adapter --> GLMS
 *
 * Data   : -
 *
 *****************************************************************************/
typedef struct
{
   uint32_t          sigNo;
} GlmsAdpiRefreshLicenseInventoryReq;
#define GLMS_ADPI_REFRESH_LICENSE_INVENTORY_REQ (0x190011b) /**< @brief GlmsAdpiRefreshLicenseInventoryReq */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiRefreshLicenseInventoryRsp
 *
 * Descr  : Response to GlmsAdpiRefreshLicenseInventoryReq
 *
 * Dir    : GLMS --> Adapter
 *
 * Data   : result        - GLMS_OK, the key file was read and licenses validated
 *                          GLMS_NOK, failed to read the key file or validate licenses
 *          resultInfo    - Information about reject cause if result is NOK
 *
 *****************************************************************************/
typedef struct
{
   uint32_t          sigNo;
   GlmsResult        result;
   char              resultInfo[GLMS_RESULT_INFO_LEN];
} GlmsAdpiRefreshLicenseInventoryRsp;
#define GLMS_ADPI_REFRESH_LICENSE_INVENTORY_RSP (0x190011c) /**< @brief GlmsAdpiRefreshLicenseInventoryRsp */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiLogInd
 *
 * Descr  : A log indication from GLMS to the adapter. The adapter shall
 *          log the events for debugging purposes.
 *
 * Dir    : GLMS --> Adapter
 *
 * Data   : -
 *
 *****************************************************************************/
typedef struct
{
   uint32_t          sigNo;
   char              log[1];
} GlmsAdpiLogInd;
#define GLMS_ADPI_LOG_IND (0x190011d) /**< @brief GlmsAdpiLogInd */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiDumpGlmsStateDataReq
 *
 * Descr  : The adapter can use this signal to request a dump of current GLMS
 *          status data.
 *
 * Dir    : Adapter --> GLMS
 *
 * Data   : -
 *
 *****************************************************************************/
typedef struct
{
   uint32_t          sigNo;
} GlmsAdpiDumpGlmsStateDataReq;
#define GLMS_ADPI_DUMP_GLMS_STATE_DATA_REQ (0x190011e) /**< @brief GlmsAdpiDumpGlmsStateDataReq */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiDumpGlmsStateDataRsp
 *
 * Descr  : Dump containing runtime data of GLMS.
 *
 * Dir    : GLMS --> Adapter
 *
 * Data   : result       - GLMS_OK, request was handled properly.
 *                         GLMS_NOK, the request failed. Dump should not be used.
 *
 *          lastResponse - Dumps larger than 50k characters will be split in
 *                         multiple response signals. The last response signal
 *                         have lastResponse set to 1, otherwise it is 0. The
 *                         dump string of each response signal will be NULL
 *                         terminated.
 *
 *          sizeOfDump   - The size of the dump including the terminating NULL.
 *
 *          resultInfo   - Information about reject cause if result is NOK.
 *
 *          dump         - The dump. A text string with tab and newline
 *                         characters for formatting.
 *
 *****************************************************************************/
typedef struct
{
   uint32_t            sigNo;
   GlmsResult          result;
   uint32_t            lastResponse;
   uint32_t            sizeOfDump;
   char                resultInfo[GLMS_RESULT_INFO_LEN];
   char                dump[1];
} GlmsAdpiDumpGlmsStateDataRsp;
#define GLMS_ADPI_DUMP_GLMS_STATE_DATA_RSP (0x190011f) /**< @brief GlmsAdpiDumpGlmsStateDataRsp */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiDumpLfciClientDataReq
 *
 * Descr  : The adapter can use this signal to request a dump of current LFCI
 *          client statuses from GLMS.
 *
 * Dir    : Adapter --> GLMS
 *
 * Data   : -
 *
 *****************************************************************************/
typedef struct
{
   uint32_t          sigNo;
} GlmsAdpiDumpLfciClientDataReq;
#define GLMS_ADPI_DUMP_LFCI_CLIENT_DATA_REQ (0x1900120) /**< @brief GlmsAdpiDumpLfciClientDataReq */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiDumpLfciClientDataRsp
 *
 * Descr  : Dump containing runtime data of the LFCI clients.
 *
 * Dir    : GLMS --> Adapter
 *
 * Data   : result       - GLMS_OK, request was handled properly.
 *                         GLMS_NOK, the request failed. Dump should not be used.
 *
 *          lastResponse - Dumps larger than 50k characters will be split in
 *                         multiple response signals. The last response signal
 *                         have lastResponse set to 1, otherwise it is 0. The
 *                         dump string of each response signal will be NULL
 *                         terminated.
 *
 *          sizeOfDump   - The size of the dump including the terminating NULL.
 *
 *          resultInfo   - Information about reject cause if result is NOK.
 *
 *          dump         - The dump. A text string with tab and newline
 *                         characters for formatting.
 *
 *****************************************************************************/
typedef struct
{
   uint32_t            sigNo;
   GlmsResult          result;
   uint32_t            lastResponse;
   uint32_t            sizeOfDump;
   char                resultInfo[GLMS_RESULT_INFO_LEN];
   char                dump[1];
} GlmsAdpiDumpLfciClientDataRsp;
#define GLMS_ADPI_DUMP_LFCI_CLIENT_DATA_RSP (0x1900121) /**< @brief GlmsAdpiDumpLfciClientDataRsp */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiDumpLcciClientDataReq
 *
 * Descr  : The adapter can use this signal to request a dump of current LCCI
 *          client statuses from GLMS.
 *
 * Dir    : Adapter --> GLMS
 *
 * Data   : -
 *
 *****************************************************************************/
typedef struct
{
   uint32_t          sigNo;
} GlmsAdpiDumpLcciClientDataReq;
#define GLMS_ADPI_DUMP_LCCI_CLIENT_DATA_REQ (0x1900122) /**< @brief GlmsAdpiDumpLcciClientDataReq */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiDumpLcciClientDataRsp
 *
 * Descr  : Dump containing runtime data of the LCCI clients.
 *
 * Dir    : GLMS --> Adapter
 *
 * Data   : result       - GLMS_OK, request was handled properly.
 *                         GLMS_NOK, the request failed. Dump should not be used.
 *
 *          lastResponse - Dumps larger than 50k characters will be split in
 *                         multiple response signals. The last response signal
 *                         have lastResponse set to 1, otherwise it is 0. The
 *                         dump string of each response signal will be NULL
 *                         terminated.
 *
 *          sizeOfDump   - The size of the dump including the terminating NULL.
 *
 *          resultInfo   - Information about reject cause if result is NOK.
 *
 *          dump         - The dump. A text string with tab and newline
 *                         characters for formatting.
 *
 *****************************************************************************/
typedef struct
{
   uint32_t            sigNo;
   GlmsResult          result;
   uint32_t            lastResponse;
   uint32_t            sizeOfDump;
   char                resultInfo[GLMS_RESULT_INFO_LEN];
   char                dump[1];
} GlmsAdpiDumpLcciClientDataRsp;
#define GLMS_ADPI_DUMP_LCCI_CLIENT_DATA_RSP (0x1900123) /**< @brief GlmsAdpiDumpLcciClientDataRsp */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiDumpFeatureStateDataReq
 *
 * Descr  : The adapter can use this signal to request a dump of current
 *          FeatureState and FeatureKey data from GLMS.
 *
 * Dir    : Adapter --> GLMS
 *
 * Data   : -
 *
 *****************************************************************************/
typedef struct
{
   uint32_t          sigNo;
} GlmsAdpiDumpFeatureStateDataReq;
#define GLMS_ADPI_DUMP_FEATURE_STATE_DATA_REQ (0x1900124) /**< @brief GlmsAdpiDumpFeatureStateDataReq */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiDumpFeatureStateDataRsp
 *
 * Descr  : Dump containing FeatueState and FeatureKey runtime data. It also 
 *          contains subscriber status.
 *
 * Dir    : GLMS --> Adapter
 *
 * Data   : result       - GLMS_OK, request was handled properly.
 *                         GLMS_NOK, the request failed. Dump should not be used.
 *
 *          lastResponse - Dumps larger than 50k characters will be split in
 *                         multiple response signals. The last response signal
 *                         have lastResponse set to 1, otherwise it is 0. The
 *                         dump string of each response signal will be NULL
 *                         terminated.
 *
 *          sizeOfDump   - The size of the dump including the terminating NULL.
 *
 *          resultInfo   - Information about reject cause if result is NOK.
 *
 *          dump         - The dump. A text string with tab and newline
 *                         characters for formatting.
 *
 *****************************************************************************/
typedef struct
{
   uint32_t            sigNo;
   GlmsResult          result;
   uint32_t            lastResponse;
   uint32_t            sizeOfDump;
   char                resultInfo[GLMS_RESULT_INFO_LEN];
   char                dump[1];
} GlmsAdpiDumpFeatureStateDataRsp;
#define GLMS_ADPI_DUMP_FEATURE_STATE_DATA_RSP (0x1900125) /**< @brief GlmsAdpiDumpFeatureStateDataRsp */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiDumpCapacityStateDataReq
 *
 * Descr  : The adapter can use this signal to request a dump of current
 *          CapacityState and CapacityKey data from GLMS.
 *
 * Dir    : Adapter --> GLMS
 *
 * Data   : -
 *
 *****************************************************************************/
typedef struct
{
   uint32_t          sigNo;
} GlmsAdpiDumpCapacityStateDataReq;
#define GLMS_ADPI_DUMP_CAPACITY_STATE_DATA_REQ (0x1900126) /**< @brief GlmsAdpiDumpCapacityStateDataReq */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiDumpCapacityStateDataRsp
 *
 * Descr  : Dump containing CapacityState and CapacityKey runtime data.
 *
 * Dir    : GLMS --> Adapter
 *
 * Data   : result       - GLMS_OK, request was handled properly.
 *                         GLMS_NOK, the request failed. Dump should not be used.
 *
 *          lastResponse - Dumps larger than 50k characters will be split in
 *                         multiple response signals. The last response signal
 *                         have lastResponse set to 1, otherwise it is 0. The
 *                         dump string of each response signal will be NULL
 *                         terminated.
 *
 *          sizeOfDump   - The size of the dump including the terminating NULL.
 *
 *          resultInfo   - Information about reject cause if result is NOK.
 *
 *          dump         - The dump. A text string with tab and newline
 *                         characters for formatting.
 *
 *****************************************************************************/
typedef struct
{
   uint32_t            sigNo;
   GlmsResult          result;
   uint32_t            lastResponse;
   uint32_t            sizeOfDump;
   char                resultInfo[GLMS_RESULT_INFO_LEN];
   char                dump[1];
} GlmsAdpiDumpCapacityStateDataRsp;
#define GLMS_ADPI_DUMP_CAPACITY_STATE_DATA_RSP (0x1900127) /**< @brief GlmsAdpiDumpCapacityStateDataRsp */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiActivateIuReq
 *
 * Descr  : A request from the adapter layer to request the GLMS to activate the Integration unlock.
 *
 * Dir    : Adapter --> GLMS
 *
 * Data   : -
 *
 *****************************************************************************/
typedef struct
{
   uint32_t          sigNo;
} GlmsAdpiActivateIuReq;
#define GLMS_ADPI_ACTIVATE_IU_REQ (0x1900128) /**< @brief GlmsAdpiActivateIuReq */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiActivateIuRsp
 *
 * Descr  : A response to GlmsAdpiActivateIuReq.
 *
 * Dir    : GLMS --> Adapter
 *
 * Data   : -
 *
 *****************************************************************************/
typedef struct
{
   uint32_t          sigNo;
   GlmsResult        result;
   char              resultInfo[GLMS_RESULT_INFO_LEN];
} GlmsAdpiActivateIuRsp;
#define GLMS_ADPI_ACTIVATE_IU_RSP (0x1900129) /**< @brief GlmsAdpiActivateIuRsp */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiActivatePuReq
 *
 * Descr  : A request from the adapter layer to request the GLMS to activate the Production unlock.
 *
 * Dir    : Adapter --> GLMS
 *
 * Data   : -
 *
 *****************************************************************************/
typedef struct
{
   uint32_t          sigNo;
} GlmsAdpiActivatePuReq;
#define GLMS_ADPI_ACTIVATE_PU_REQ (0x190012a) /**< @brief GlmsAdpiActivatePuReq */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiActivatePuRsp
 *
 * Descr  : A response to GlmsAdpiActivatePuReq.
 *
 * Dir    : GLMS --> Adapter
 *
 * Data   : -
 *
 *****************************************************************************/
typedef struct
{
   uint32_t             sigNo;
   time_t32             expiration;
   GlmsResult           result;
   GlmsActivationState  activationState;
   uint32_t             activationsLeft;
   uint32_t             puDeactivated;
   char                 resultInfo[GLMS_RESULT_INFO_LEN];
} GlmsAdpiActivatePuRsp;
#define GLMS_ADPI_ACTIVATE_PU_RSP (0x190012b) /**< @brief GlmsAdpiActivatePuRsp */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiDeactivatePuReq
 *
 * Descr  : A request from the adapter layer to request the GLMS to deactivate
 *          the Production unlock.
 *
 * Dir    : Adapter --> GLMS
 *
 * Data   : -
 *
 *****************************************************************************/
typedef struct
{
   uint32_t          sigNo;
} GlmsAdpiDeactivatePuReq;
#define GLMS_ADPI_DEACTIVATE_PU_REQ (0x190012c) /**< @brief GlmsAdpiDeactivatePuReq */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiDeactivatePuRsp
 *
 * Descr  : A response to GlmsAdpiDeactivatePuReq.
 *
 * Dir    : GLMS --> Adapter
 *
 * Data   : -
 *
 *****************************************************************************/
typedef struct
{
   uint32_t             sigNo;
   time_t32             expiration;
   GlmsResult           result;
   GlmsActivationState  activationState;
   uint32_t             activationsLeft;
   uint32_t             puDeactivated;
   char                 resultInfo[GLMS_RESULT_INFO_LEN];
} GlmsAdpiDeactivatePuRsp;
#define GLMS_ADPI_DEACTIVATE_PU_RSP (0x190012d) /**< @brief GlmsAdpiDeactivatePuRsp */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiCreateFeatureStateMoReq
 *
 * Descr  : A request from GLMS to the adapter layer to create a new FeatureState
 *          MO instance.
 *
 * Dir    : GLMS --> Adapter
 *
 * Data   : featureState    - The feature state of the license.
 *
 *          licenseState    - The license state of the license.
 *
 *          serviceState    - The service state of the license.
 *
 *          featureStateId  - The FeatureState MO id. This is the key value and 
 *                            will later be used to identify the feature key.
 *
 *          description     - The description of the license as received by the 
 *                            LIHI client.
 *
 *          keyId           - The license number (CXC...)
 *
 *          activeFeatureKeyId - The featureKeyId of the FeatureKey MO that is
 *                            currently the active license key.
 *
 *****************************************************************************/
typedef struct
{
   uint32_t          sigNo;
   GlmsFeatureState  featureState;
   GlmsLicenseState  licenseState;
   GlmsServiceState  serviceState;
   char              featureStateId[GLMS_MO_KEY_LEN];
   char              description[GLMS_DESCRIPTION_LEN];
   char              keyId[GLMS_KEY_ID_LEN];
   char              activeFeatureKeyId[GLMS_MO_KEY_LEN];
} GlmsAdpiCreateFeatureStateMoReq;
#define GLMS_ADPI_CREATE_FEATURE_STATE_MO_REQ (0x190012e) /**< @brief GlmsAdpiCreateFeatureStateMoReq */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiCreateFeatureStateMoRsp
 *
 * Descr  : The response to a GlmsAdpiCreateFeatureStateMoReq.
 *
 * Dir    : Adapter --> GLMS
 *
 * Data   : result          - GLMS_OK if the request was accepted and successful.
 *                            GLMS_NOK if the request was rejected or failed.
 *
 *          featureStateId  - The FeatureState MO id as specified in the
 *                            request signal.
 *          
 *****************************************************************************/
typedef struct
{
   uint32_t          sigNo;
   GlmsResult        result;
   char              featureStateId[GLMS_MO_KEY_LEN];
} GlmsAdpiCreateFeatureStateMoRsp;
#define GLMS_ADPI_CREATE_FEATURE_STATE_MO_RSP (0x190012f) /**< @brief GlmsAdpiCreateFeatureStateMoRsp */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiDeleteFeatureStateMoReq
 *
 * Descr  : A request from GLMS to the adapter layer to delete a FeatureState
 *          MO instance.
 *
 * Dir    : GLMS --> Adapter
 *
 * Data   : featureStateId  - The ID of the FeatureState to delete.
 *
 *****************************************************************************/
typedef struct
{
   uint32_t          sigNo;
   char              featureStateId[GLMS_MO_KEY_LEN];
} GlmsAdpiDeleteFeatureStateMoReq;
#define GLMS_ADPI_DELETE_FEATURE_STATE_MO_REQ (0x1900130) /**< @brief GlmsAdpiDeleteFeatureStateMoReq */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiDeleteFeatureStateMoRsp
 *
 * Descr  : The response to a GlmsAdpiDeleteFeatureStateMoReq.
 *
 * Dir    : Adapter --> GLMS
 *
 * Data   : result          - GLMS_OK if the request was accepted and successful.
 *                            GLMS_NOK if the request was rejected or failed.
 *
 *          featureStateId  - The FeatureState MO id as specified in the 
 *                            request signal.
 *
 *****************************************************************************/
typedef struct
{
   uint32_t          sigNo;
   GlmsResult        result;
   char              featureStateId[GLMS_MO_KEY_LEN];
} GlmsAdpiDeleteFeatureStateMoRsp;
#define GLMS_ADPI_DELETE_FEATURE_STATE_MO_RSP (0x1900131) /**< @brief GlmsAdpiDeleteFeatureStateMoRsp */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiCreateCapacityKeyMoReq
 *
 * Descr  : A request from GLMS to the adapter layer to create a new CapacityKey
 *          MO instance.
 *
 * Dir    : GLMS --> Adapter
 *
 * Data   : licensedCapacityValue - The licensed value of the capacity feature.
 *
 *          licensedCapacityLimit.noLimit = True means that no limit is enforced
 *                                          for the capacity.
 *
 *          licensedCapacityLimit.noLimit = False means that licensedCapacityLimit.
 *                            value is the value that is enforced for the capacity.
 *
 *          validFrom      - The start date of the license.
 *
 *          expiration     - The end date of the license.
 *
 *          licensedCapacityLimitReached - For GLMS this will always be False.
 *
 *          grantedCapacityLevel - For GLMS this will always be the same as 
 *                                 licensedCapacityLimit.value.
 *
 *          capacityUnit   - The unit of measurement for the capacity feature.
 *
 *          capacityKeyId  - The capacity key id. This is the key value and will later
 *                           be used to identify the capacity key.
 *
 *          keyId          - The license number (CXC...)
 *
 *          name           - The name of the license.
 *
 *          productType    - The product type of the license.
 *
 *****************************************************************************/
typedef struct
{
   uint32_t          sigNo;
   GlmsCapacityValue licensedCapacityValue;
   time_t32          validFrom;
   time_t32          expiration;
   GlmsBool          licensedCapacityLimitReached;
   int32_t           grantedCapacityLevel;
   char              capacityUnit[GLMS_CAPACITY_UNIT_LEN];
   char              capacityKeyId[GLMS_MO_KEY_LEN];
   char              keyId[GLMS_KEY_ID_LEN];
   char              name[GLMS_KEY_NAME_LEN];
   char              productType[GLMS_PRODUCT_TYPE_LEN];
} GlmsAdpiCreateCapacityKeyMoReq;
#define GLMS_ADPI_CREATE_CAPACITY_KEY_MO_REQ (0x1900132) /**< @brief GlmsAdpiCreateCapacityKeyMoReq */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiCreateCapacityKeyMoRsp
 *
 * Descr  : The response to a GlmsAdpiCreatecapacityKeyMoReq.
 *
 * Dir    : Adapter --> GLMS
 *
 * Data   : result         - GLMS_OK if the request was accepted and successful.
 *                           GLMS_NOK if the request was rejected or failed.
 *
 *          capacityKeyId  - The capacity key id as specified in the request signal.
 *
 *****************************************************************************/
typedef struct
{
   uint32_t          sigNo;
   GlmsResult        result;
   char              capacityKeyId[GLMS_MO_KEY_LEN];
} GlmsAdpiCreateCapacityKeyMoRsp;
#define GLMS_ADPI_CREATE_CAPACITY_KEY_MO_RSP (0x1900133) /**< @brief GlmsAdpiCreateCapacityKeyMoRsp */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiDeleteCapacityKeyMoReq
 *
 * Descr  : A request from GLMS to the adapter layer to delete a CapacityKey
 *          MO instance.
 *
 * Dir    : GLMS --> Adapter
 *
 * Data   : capacityKeyId  - The capacity key id of the CapacityKey MO to delete.
 *
 *****************************************************************************/
typedef struct
{
   uint32_t          sigNo;
   char              capacityKeyId[GLMS_MO_KEY_LEN];
} GlmsAdpiDeleteCapacityKeyMoReq;
#define GLMS_ADPI_DELETE_CAPACITY_KEY_MO_REQ (0x1900134) /**< @brief GlmsAdpiDeleteCapacityKeyMoReq */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiDeleteCapacityKeyMoRsp
 *
 * Descr  : The response to a GlmsAdpiDeleteCapacityKeyMoReq.
 *
 * Dir    : Adapter --> GLMS
 *
 * Data   : result         - GLMS_OK if the request was accepted and successful.
 *                           GLMS_NOK if the request was rejected or failed.
 *
 *          capacityKeyId  - The feature key id of the CapacityKey MO to delete.
 *
 *****************************************************************************/
typedef struct
{
   uint32_t          sigNo;
   GlmsResult        result;
   char              capacityKeyId[GLMS_MO_KEY_LEN];
} GlmsAdpiDeleteCapacityKeyMoRsp;
#define GLMS_ADPI_DELETE_CAPACITY_KEY_MO_RSP (0x1900135) /**< @brief GlmsAdpiDeleteCapacityKeyMoRsp */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiCreateCapacityStateMoReq
 *
 * Descr  : A request from GLMS to the adapter layer to create a new CapacityState
 *          MO instance.
 *
 * Dir    : GLMS --> Adapter
 *
 * Data   : currentCapacityValue - The licensed value of the capacity feature.
 *
 *          licensedCapacityLimitReached - For GLMS this will always be False.
 *
 *          grantedCapacityLevel   - For GLMS this will always be the same as
 *                                   licensedCapacityLimit.value.
 *
 *          licenseState     - The license state of the license.
 *
 *          capacityUnit     - The unit of measurement for the capacity feature.
 *
 *          capacityStateId  - The CapacityState MO id. This is the key value and 
 *                             will later be used to identify the capacity key.
 *
 *          description      - The description of the license as received by the 
 *                             LIHI client.
 *
 *          keyId            - The license number (CXC...)
 *
 *          activeCapacityKeyId - The capacityKeyId of the CapacityKey MO that is
 *                             currently the active license key.
 *
 *****************************************************************************/
typedef struct
{
   uint32_t           sigNo;   
   GlmsCapacityValue  currentCapacityValue;  
   GlmsBool           licensedCapacityLimitReached;  
   int32_t            grantedCapacityLevel;
   GlmsLicenseState   licenseState;
   char               capacityUnit[GLMS_CAPACITY_UNIT_LEN];
   char               capacityStateId[GLMS_MO_KEY_LEN];
   char               description[GLMS_DESCRIPTION_LEN];
   char               keyId[GLMS_KEY_ID_LEN];
   char               activeCapacityKeyId[GLMS_MO_KEY_LEN];
} GlmsAdpiCreateCapacityStateMoReq;
#define GLMS_ADPI_CREATE_CAPACITY_STATE_MO_REQ (0x1900136) /**< @brief GlmsAdpiCreateCapacityStateMoReq */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiCreateCapacityStateMoRsp
 *
 * Descr  : The response to a GlmsAdpiCreateCapacityStateMoReq.
 *
 * Dir    : Adapter --> GLMS
 *
 * Data   : result           - GLMS_OK if the request was accepted and successful.
 *                             GLMS_NOK if the request was rejected or failed.
 *
 *          capacityStateId  - The capacityState MO id as specified in the
 *                             request signal.
 *
 *****************************************************************************/
typedef struct
{
   uint32_t          sigNo;
   GlmsResult        result;
   char              capacityStateId[GLMS_MO_KEY_LEN];
} GlmsAdpiCreateCapacityStateMoRsp;
#define GLMS_ADPI_CREATE_CAPACITY_STATE_MO_RSP (0x1900137) /**< @brief GlmsAdpiCreateCapacityStateMoRsp */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiDeleteCapacityStateMoReq
 *
 * Descr  : A request from GLMS to the adapter layer to delete a CapacityState
 *          MO instance.
 *
 * Dir    : GLMS --> Adapter
 *
 * Data   : capacityStateId  - The ID of the CapacityState to delete.
 *
 *****************************************************************************/
typedef struct
{
   uint32_t          sigNo;
   char              capacityStateId[GLMS_MO_KEY_LEN];
} GlmsAdpiDeleteCapacityStateMoReq;
#define GLMS_ADPI_DELETE_CAPACITY_STATE_MO_REQ (0x1900138) /**< @brief GlmsAdpiDeleteCapacityStateMoReq */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiDeleteCapacityStateMoRsp
 *
 * Descr  : The response to a GlmsAdpiDeleteCapacityStateMoReq.
 *
 * Dir    : Adapter --> GLMS
 *
 * Data   : result           - GLMS_OK if the request was accepted and successful.
 *                             GLMS_NOK if the request was rejected or failed.
 *
 *          capacityStateId  - The CapacityState MO id as specified in the 
 *                             request signal.
 *
 *****************************************************************************/
typedef struct
{
   uint32_t          sigNo;
   GlmsResult        result;
   char              capacityStateId[GLMS_MO_KEY_LEN];
} GlmsAdpiDeleteCapacityStateMoRsp;
#define GLMS_ADPI_DELETE_CAPACITY_STATE_MO_RSP (0x1900139) /**< @brief GlmsAdpiDeleteCapacityStateMoRsp */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiCreateGracePeriodMoReq
 *
 * Descr  : A request from GLMS to the adapter layer to create a GracePeriod
 *          MO instance.
 *
 * Dir    : GLMS --> Adapter
 *
 * Data   : gracePeriod      -  The struct containing the grace period data.
 *****************************************************************************/
typedef struct
{
   uint32_t          sigNo;
   GlmsGracePeriod   gracePeriod;
} GlmsAdpiCreateGracePeriodMoReq;
#define GLMS_ADPI_CREATE_GRACE_PERIOD_MO_REQ (0x190013a) /**< @brief GlmsAdpiCreateGracePeriodMoReq */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiCreateGracePeriodMoRsp
 *
 * Descr  : The response to a GlmsAdpiCreateGracePeriodMoReq.
 *
 * Dir    : Adapter --> GLMS
 *
 * Data   : result           - GLMS_OK if the request was accepted and successful.
 *                             GLMS_NOK if the request was rejected or failed.
 *
 *          gracePeriodId    - The GracePeriod MO id as specified in the 
 *                             request signal.
 *
 *****************************************************************************/
typedef struct
{
   uint32_t          sigNo;
   GlmsResult        result;
   char              gracePeriodId[GLMS_MO_KEY_LEN];
} GlmsAdpiCreateGracePeriodMoRsp;
#define  GLMS_ADPI_CREATE_GRACE_PERIOD_MO_RSP (0x190013b) /**< @brief GlmsAdpiCreateGracePeriodMoRsp */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiDeleteGracePeriodMoReq
 *
 * Descr  : A request from GLMS to the adapter layer to delete a GracePeriod
 *          MO instance.
 *
 * Dir    : GLMS --> Adapter
 *
 * Data   : gracePeriodId      -  The GracePeriodMO Id.
 *****************************************************************************/
typedef struct
{
   uint32_t          sigNo;
   char              gracePeriodId[GLMS_MO_KEY_LEN];
} GlmsAdpiDeleteGracePeriodMoReq;
#define GLMS_ADPI_DELETE_GRACE_PERIOD_MO_REQ (0x190013c) /**< @brief GlmsAdpiDeleteGracePeriodMoReq */


/**************************************************************************//**
 *
 * Signal : GlmsAdpideleteGracePeriodMoRsp
 *
 * Descr  : The response to a GlmsAdpiDeleteGracePeriodMoReq.
 *
 * Dir    : Adapter --> GLMS
 *
 * Data   : result           - GLMS_OK if the request was accepted and successful.
 *                             GLMS_NOK if the request was rejected or failed.
 *
 *          gracePeriodId    - The GracePeriod MO id as specified in the
 *                             request signal.
 *
 *****************************************************************************/
typedef struct
{
   uint32_t          sigNo;
   GlmsResult        result;
   char              gracePeriodId[GLMS_MO_KEY_LEN];
} GlmsAdpiDeleteGracePeriodMoRsp;
#define  GLMS_ADPI_DELETE_GRACE_PERIOD_MO_RSP (0x190013d) /**< @brief GlmsAdpiDeleteGracePeriodMoRsp */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiUpdateGracePeriodAttributesReq
 *
 * Descr  : A request from adapter to GLMS to update the Grace Period
 *          configurable attributes.
 *
 * Dir    : Adapter --> GLMS
 *
 * Data   : gracePeriodId          - The CapacityState MO Id.
 *
 *          gpActivationThreshold  - The allowed overshoot. Allowed
 *                                   values are 0 to 100, in per mille.
 *                                   100 means 10% overshoot.
 *
 *          gpResetThreshold       - The minimum capacity increase to
 *                                   reset GP. Allowed values are 0 to
 *                                   100, in percent.
 *
 *          gpLength               - The length of the GP. Allowed
 *                                   values are 0 to 180, in days.
 *****************************************************************************/
typedef struct
{
   uint32_t   sigNo;
   char       capacityStateId[GLMS_MO_KEY_LEN];
   uint32_t   gpActivationThreshold;
   uint32_t   gpResetThreshold;
   uint32_t   gpLength;
} GlmsAdpiUpdateGracePeriodAttributesReq;
#define GLMS_ADPI_UPDATE_GRACE_PERIOD_ATTRIBUTES_REQ (0x190013e) /**< @brief GlmsAdpiUpdateGracePeriodAttributesReq */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiUpdateGracePeriodAttributesRsp
 *
 * Descr  : The response to a GlmsAdpiUpdateGracePeriodAttributesReq.
 *
 * Dir    : GLMS --> Adapter
 *
 * Data   : result           - GLMS_OK if the request was accepted and successful.
 *                             GLMS_NOK if the request was rejected or failed.
 *
 *          capacityStateId  - The CapacityState MO id as specified in the 
 *                             request signal.
 *
 *          resultInfo       - Information about reject cause if result is NOK.
 *
 *****************************************************************************/
typedef struct
{
   uint32_t          sigNo;
   GlmsResult        result;
   char              capacityStateId[GLMS_MO_KEY_LEN];
   char              resultInfo[GLMS_RESULT_INFO_LEN];
} GlmsAdpiUpdateGracePeriodAttributesRsp;
#define  GLMS_ADPI_UPDATE_GRACE_PERIOD_ATTRIBUTES_RSP (0x190013f) /**< @brief GlmsAdpiUpdateGracePeriodAttributesRsp */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiFeatureConfigurationListReq
 *
 * Descr  : GLMS use this signal to ask the adapter for the feature license
 *          configuration data.
 *
 * Dir    : GLMS --> Adapter
 *
 * Data   : -
 *
 *****************************************************************************/
typedef struct
{
   uint32_t sigNo;
} GlmsAdpiFeatureConfigurationListReq;
#define GLMS_ADPI_FEATURE_CONFIGURATION_LIST_REQ (0x1900140) /**< @brief GlmsAdpiFeatureConfigurationListReq */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiFeatureConfigurationListRsp
 *
 * Descr  : The response to a GlmsAdpiFeatureConfigurationListReq.
 *
 *          A list of the licensed features that are configured on the node.
 *
 * Dir    : Adapter --> GLMS
 *
 * Data   : result           - GLMS_OK if the request was accepted and successful.
 *                             GLMS_NOK if the request was rejected or failed.
 *
 *          featureConfListLen - The number of elements in the featureConfList
 *                               array.
 *
 *          featureConfList    - The list of features supported on the node.
 *
 *****************************************************************************/
typedef struct
{
   uint32_t   sigNo;
   GlmsResult result;
   uint32_t   featureConfListLen;
   GlmsFeatureConfigurationData featureConfList[1];
} GlmsAdpiFeatureConfigurationListRsp;
#define  GLMS_ADPI_FEATURE_CONFIGURATION_LIST_RSP (0x1900141) /**< @brief GlmsAdpiFeatureConfigurationListRsp */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiStateMoAuditReq
 *
 * Descr  : This signal will trigger an audit of the State MO:s.
 *          Any FeatureState MO that do not have an LIHI subscriber will be
 *          immediately deleted.
 *
 * Dir    : Adapter --> GLMS
 *
 * Data   : -
 *
 *****************************************************************************/
typedef struct
{
   uint32_t   sigNo;
} GlmsAdpiStateMoAuditReq;
#define GLMS_ADPI_STATE_MO_AUDIT_REQ (0x1900142) /**< @brief GlmsAdpiStateMoAuditReq */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiStateMoAuditRsp
 *
 * Descr  : The response to a GlmsAdpiStateMoAuditReq.
 *
 * Dir    : GLMS --> Adapter
 *
 * Data   : result           - GLMS_OK if the request was accepted and successful.
 *                             GLMS_NOK if the request was rejected or failed.
 *
 *          resultInfo       - Information about reject cause if result is NOK.
 *
 *****************************************************************************/
typedef struct
{
   uint32_t          sigNo;
   GlmsResult        result;
   char              resultInfo[GLMS_RESULT_INFO_LEN];
} GlmsAdpiStateMoAuditRsp;
#define  GLMS_ADPI_STATE_MO_AUDIT_RSP (0x1900143) /**< @brief GlmsAdpiStateMoAuditRsp */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiUpdateAreaIdReq
 *
 * Descr  : A request from adapter to GLMS to set the Area Id. Setting a new
 *          Area Id will remove all previously installed Area License Keys.
 *          Changing the area id will also reset the sequence number to 0.
 *
 * Dir    : Adapter --> GLMS
 *
 * Data   : areaId      - The new Area Id.
 *
 *****************************************************************************/
typedef struct
{
   uint32_t   sigNo;
   char       areaId[GLMS_AREA_ID_LEN];
} GlmsAdpiUpdateAreaIdReq;
#define GLMS_ADPI_UPDATE_AREA_ID_REQ (0x1900144) /**< @brief GlmsAdpiUpdateAreaIdReq */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiUpdateAreaIdRsp
 *
 * Descr  : The response to a GlmsAdpiUpdateAreaIdReq.
 *
 * Dir    : GLMS --> Adapter
 *
 * Data   : result           - GLMS_OK if the request was accepted and successful.
 *                             GLMS_NOK if the request was rejected or failed.
 *
 *          resultInfo       - Information about reject cause if result is NOK.
 *
 *****************************************************************************/
typedef struct
{
   uint32_t          sigNo;
   GlmsResult        result;
   char              resultInfo[GLMS_RESULT_INFO_LEN];
} GlmsAdpiUpdateAreaIdRsp;
#define  GLMS_ADPI_UPDATE_AREA_ID_RSP (0x1900145) /**< @brief GlmsAdpiUpdateAreaIdRsp */


/**************************************************************************//**
 *
 * Signal: GlmsAdpiInstallAreaLicenseKeysReq
 *
 * Descr  : A request from adapter to GLMS to install new area license keys.
 *          Installing new area license keys will remove all previously
 *          installed area license keys.
 *
 *          The keys string is given as a string with the following pattern:
 *          <version>@<area hash>@<area id>@<seq nr>@<key1>@<key2>
 *
 * @cond
 *          Example:
 *          - Feature key cxc001 with start time 1000 and stop time 2000.
 *          - Feature key cxc002 with start time 1000 and no stop time.
 *          - Emergency reset key with start time 1000 and stop time 2000.
 *          - Capacity key cxc003 with start time 1000, stop time 2000
 *            and value 100.
 *          v1.0@%#¤!%##@AREA12345Kista@1@f:cxc001:1000:2000@f:cxc002:1000:0@emergencyReset:1000:2000@c:cxc003:1000:2000:100
 * @endcond
 *
 * Dir    : Adapter --> GLMS
 *
 * Data   : createStateMOs   - True if FeatureState MOs shall be created for all
 *                             area license keys, otherwise false and the FeatureState MO
 *                             will be created when a LFCI client subscribes.
 *
 * Data   : keys             - The null-terminated string with the Area License Keys.
 *
 *****************************************************************************/
typedef struct
{
   uint32_t   sigNo;
   GlmsBool   createStateMOs;
   char       keys[1];
} GlmsAdpiInstallAreaLicenseKeysReq;
#define GLMS_ADPI_INSTALL_AREA_LICENSE_KEYS_REQ (0x1900146) /**< @brief GlmsAdpiInstallAreaLicenseKeysReq */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiInstallAreaLicenseKeysRsp
 *
 * Descr  : The response to a GlmsAdpiInstallAreaLicenseKeysReq.
 *
 * Dir    : GLMS --> Adapter
 *
 * Data   : resultCode  - The result code of the action. See glmsDataTypes.h
 *                        for description of the codes.
 *
 *****************************************************************************/
typedef struct
{
   uint32_t          sigNo;
   GlmsInstallAreaLicenseKeyResult resultCode;
} GlmsAdpiInstallAreaLicenseKeysRsp;
#define  GLMS_ADPI_INSTALL_AREA_LICENSE_KEYS_RSP (0x1900147) /**< @brief GlmsAdpiInstallAreaLicenseKeysRsp */


/* ==============================================================================
   ==============================================================================

   READ MO ATTRIBUTES

   ==============================================================================
   ============================================================================== */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiReadMoKeyFileInformationReq
 *
 * Descr  : Adapter requests the MO attributes of the MO KeyFileInformation.
 *
 * Dir    : Adapter --> GLMS
 *
 * Data   : -
 *
 *****************************************************************************/
typedef struct
{
   uint32_t       sigNo;
} GlmsAdpiReadMoKeyFileInformationReq;
#define GLMS_ADPI_READ_MO_KEY_FILE_INFORMATION_REQ (0x1900150) /**< @brief GlmsAdpiReadMoKeyFileInformationReq */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiReadMoKeyFileInformationRsp
 *
 * Descr  : Response to GlmsAdpiReadMoKeyFileInformationReq.
 *
 * Dir    : GLMS --> Adapter
 *
 * Data   : result               - GLMS_OK if the request was accepted and successful.
 *                                 GLMS_NOK if the request was rejected or failed.
 *
 *          sequenceNumber       - The sequence number of the most recently
 *                                 installed Key File.
 *
 *          installationTime     - The time stamp of when the Key File was installed.
 *
 *          locatable            - Not used by GLMS and will always be set to false.
 *
 *          productType          - The product type of the Key File.
 *
 *          resultInfo           - Information about reject cause if result is NOK.
 *
 *****************************************************************************/
typedef struct
{
   uint32_t       sigNo;
   time_t32       installationTime;
   GlmsResult     result;
   uint32_t       sequenceNumber;
   GlmsBool       locatable;
   char           productType[GLMS_PRODUCT_TYPE_LEN];
   char           resultInfo[GLMS_RESULT_INFO_LEN];
} GlmsAdpiReadMoKeyFileInformationRsp;
#define GLMS_ADPI_READ_MO_KEY_FILE_INFORMATION_RSP (0x1900151) /**< @brief GlmsAdpiReadMoKeyFileInformationRsp */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiReadMoInstallKeyFileReportProgressReq
 *
 * Descr  : Adapter requests the reportProgress on the KeyFileManagement MO.
 *
 * Dir    : Adapter --> GLMS
 *
 * Data   : -
 *
 *****************************************************************************/
typedef struct
{
   uint32_t       sigNo;
} GlmsAdpiReadMoInstallKeyFileReportProgressReq;
#define GLMS_ADPI_READ_MO_INSTALL_KEY_FILE_REPORT_PROGRESS_REQ (0x1900152) /**< @brief GlmsAdpiReadMoInstallKeyFileReportProgressReq */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiReadMoInstallKeyFileReportProgressRsp
 *
 * Descr  : Adapter requests the reportProgress on the KeyFileManagement MO.
 *
 * Dir    : Adapter --> GLMS
 *
 * Data   : reportProgress       - The reportProgress attribute.
 *
 *          result               - GLMS_OK if the request was accepted and successful.
 *                                 GLMS_NOK if the request was rejected or failed.
 *
 *          resultInfo           - Information about reject cause if result is NOK.
 *
 *****************************************************************************/
typedef struct
{
   uint32_t          sigNo;
   GlmsAsyncAction   reportProgress;
   GlmsResult        result;
   char              resultInfo[GLMS_RESULT_INFO_LEN];
} GlmsAdpiReadMoInstallKeyFileReportProgressRsp;
#define GLMS_ADPI_READ_MO_INSTALL_KEY_FILE_REPORT_PROGRESS_RSP (0x1900153) /**< @brief GlmsAdpiReadMoInstallKeyFileReportProgressRsp */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiReadMoLmReq
 *
 * Descr  : Adapter requests the reportProgress on the Lm MO.
 *
 * Dir    : Adapter --> GLMS
 *
 * Data   : -
 *
 *****************************************************************************/
typedef struct
{
   uint32_t       sigNo;
} GlmsAdpiReadMoLmReq;
#define GLMS_ADPI_READ_MO_LM_REQ (0x1900154) /**< @brief GlmsAdpiReadMoLmReq */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiReadMoLmRsp
 *
 * Descr  : Response to GlmsAdpiReadMoLmReq.
 *
 * Dir    : GLMS --> Adapter
 *
 * Data   : lastInventoryChange    - The last time when a change happened to license
 *                                   inventory.
 *
 *          lastLicenseInventoryRefresh  - The last time a license validation was performed.
 *
 *          result                 - GLMS_OK if the request was accepted and successful.
 *                                   GLMS_NOK if the request was rejected or failed.
 *
 *          fingerprintUpdateable  - True if it is possible to update the fingerprint,
 *                                   otherwise false.
 *
 *          lmState                - The LM state of GLMS. See the FD for description.
 *
 *          fingerprint            - The fingerprint
 *
 *          resultInfo             - Information about reject cause if result is NOK.
 *
 *          referenceToLicenseServer  - Not used be GLMS. Will always be empty string.
 *
 *****************************************************************************/
typedef struct
{
   uint32_t       sigNo;
   time_t32       lastInventoryChange;
   time_t32       lastLicenseInventoryRefresh;
   GlmsResult     result;
   GlmsBool       fingerprintUpdateable;
   GlmsLmState    lmState;
   char           fingerprint[GLMS_FINGERPRINT_LEN];
   char           resultInfo[GLMS_RESULT_INFO_LEN];
   char           referenceToLicenseServer[1];
} GlmsAdpiReadMoLmRsp;
#define GLMS_ADPI_READ_MO_LM_RSP (0x1900155) /**< @brief GlmsAdpiReadMoLmRsp */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiReadMoFeatureKeyReq
 *
 * Descr  : Adapter requests the FeatureKey MO with the given featureKeyId. If
 *          no MO with the given featureKeyId exist the response will have a
 *          result of GLMS_NOK.
 *
 * Dir    : Adapter --> GLMS
 *
 * Data   : featureKeyId    - The Feature Key Id of the requested FeatureKey MO.
 *
 *****************************************************************************/
typedef struct
{
   uint32_t       sigNo;
   char           featureKeyId[GLMS_MO_KEY_LEN];
} GlmsAdpiReadMoFeatureKeyReq;
#define GLMS_ADPI_READ_MO_FEATURE_KEY_REQ (0x1900156) /**< @brief GlmsAdpiReadMoFeatureKeyReq */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiReadMoFeatureKeyRsp
 *
 * Descr  : Response to GlmsAdpiReadMoFeatureKeyReq.
 *
 * Dir    : GLMS --> Adapter
 *
 * Data   : validFrom         - The date when the license key was, or will be, enabled.
 *
 *          expiration        - The date when the license key was, or will be, disabled.
 *
 *          result            - GLMS_OK if the request was accepted and successful.
 *                              GLMS_NOK if the request was rejected or failed.
 *
 *          resultInfo        - Information about reject cause if result is NOK.
 *
 *          featureKeyId      - The Feature Key Id of the requested FeatureKey MO.
 *
 *          keyId             - The license number (CXC..)
 *
 *          name              - The name of the Feature.
 *
 *          productType       - The product type of the license.
 *
 *          shared            - True if the key is from an area key file. False otherwise.
 *
 *****************************************************************************/
typedef struct
{
   uint32_t          sigNo;
   time_t32          validFrom;
   time_t32          expiration;
   GlmsResult        result;
   char              resultInfo[GLMS_RESULT_INFO_LEN];
   char              featureKeyId[GLMS_MO_KEY_LEN];
   char              keyId[GLMS_KEY_ID_LEN];
   char              name[GLMS_KEY_NAME_LEN];
   char              productType[GLMS_PRODUCT_TYPE_LEN];
   GlmsBool          shared;
} GlmsAdpiReadMoFeatureKeyRsp;
#define GLMS_ADPI_READ_MO_FEATURE_KEY_RSP (0x1900157) /**< @brief GlmsAdpiReadMoFeatureKeyRsp */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiReadEuMoReq
 *
 * Descr  : A request from the adapter layer to GLMS get the attributes of the
 *          EmergencyUnlock MO.
 *
 * Dir    : Adapter --> GLMS
 *
 * Data   : -
 *
 *****************************************************************************/
typedef struct
{
   uint32_t          sigNo;
} GlmsAdpiReadEuMoReq;
#define GLMS_ADPI_READ_EU_MO_REQ (0x1900158) /**< @brief GlmsAdpiReadEuMoReq */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiReadEuMoRsp
 *
 * Descr  : A response to GlmsAdpiReadEuMoReq.
 *
 * Dir    : GLMS --> Adapter
 *
 * Data   : expiration        - The date and time when Emergency Unlock expires.
 *
 *          result            - GLMS_OK if the request was accepted and successful.
 *                              GLMS_NOK if the request was rejected or failed.
 *
 *          activationState   - The activation state of EU.
 *
 *          activationsLeft   - Remaining activations of EU.
 *
 *          resultInfo        - Information about reject cause if result is NOK.
 *
 *
 *****************************************************************************/
typedef struct
{
   uint32_t             sigNo;
   time_t32             expiration;
   GlmsResult           result;
   GlmsActivationState  activationState;
   uint32_t             activationsLeft;
   char                 resultInfo[GLMS_RESULT_INFO_LEN];
} GlmsAdpiReadEuMoRsp;
#define GLMS_ADPI_READ_EU_MO_RSP (0x1900159) /**< @brief GlmsAdpiReadEuMoRsp */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiSubscribeMoUpdatesReq
 *
 * Descr  : A request from the adapter layer to GLMS to subscribe to MO update Indications.
 *
 * Dir    : Adapter --> GLMS
 *
 * Data   : -
 *
 *****************************************************************************/
typedef struct
{
   uint32_t          sigNo;
} GlmsAdpiSubscribeMoUpdatesReq;
#define GLMS_ADPI_SUBSCRIBE_MO_UPDATES_REQ (0x190015a) /**< @brief GlmsAdpiSubscribeMoUpdatesReq */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiSubscribeMoUpdatesRsp
 *
 * Descr  : A response to GlmsAdpiSubscribeMoUpdatesReq.
 *
 * Dir    : GLMS --> Adapter
 *
 * Data   : result            - GLMS_OK if the request was accepted and successful.
 *                              GLMS_NOK if the request was rejected or failed.
 *
 *          resultInfo        - Information about reject cause if result is NOK.
 *
 *****************************************************************************/
typedef struct
{
   uint32_t          sigNo;
   GlmsResult        result;
   char              resultInfo[GLMS_RESULT_INFO_LEN];
} GlmsAdpiSubscribeMoUpdatesRsp;
#define GLMS_ADPI_SUBSCRIBE_MO_UPDATES_RSP (0x190015b) /**< @brief GlmsAdpiSubscribeMoUpdatesRsp */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiMoUpdateEuInd
 *
 * Descr  : Indication when One or more attributes in the Eu MO has been updated.
 *
 * Dir    : GLMS --> Adapter
 *
 * Data   : expiration        - The date and time when Emergency Unlock expires.
 *
 *          activationState   - The activation state of EU.
 *
 *          activationsLeft   - Remaining activations of EU.
 *
 *****************************************************************************/
typedef struct
{
   uint32_t            sigNo;
   time_t32            expiration;
   GlmsActivationState activationState;
   uint32_t            activationsLeft;
} GlmsAdpiMoUpdateEuInd;
#define GLMS_ADPI_MO_UPDATE_EU_IND (0x190015c) /**< @brief GlmsAdpiMoUpdateEuInd */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiMoUpdateIuInd
 *
 * Descr  : Indication when One or more attributes in the Eu MO has been updated.
 *
 * Dir    : GLMS --> Adapter
 *
 * Data   : expiration        - The date and time when Integration Unlock expires.
 *
 *          activationState   - The activation state of IU.
 *
 *          activationsLeft   - Remaining activations of IU.
 *
 *****************************************************************************/
typedef struct
{
   uint32_t            sigNo;
   time_t32            expiration;
   GlmsActivationState activationState;
   uint32_t            activationsLeft;
} GlmsAdpiMoUpdateIuInd;
#define GLMS_ADPI_MO_UPDATE_IU_IND (0x190015d) /**< @brief GlmsAdpiMoUpdateIuInd */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiMoUpdateAmInd
 *
 * Descr  : Indication when One or more attributes in the AutonomousMode MO has been updated.
 *
 * Dir    : GLMS --> Adapter
 *
 * Data   : expiration        - The date and time when Autonomous Mode expires.
 *
 *          activationState   - The activation state of AM.
 *
 *****************************************************************************/
typedef struct
{
   uint32_t            sigNo;
   time_t32            expiration;
   GlmsActivationState activationState;
} GlmsAdpiMoUpdateAmInd;
#define GLMS_ADPI_MO_UPDATE_AM_IND (0x190015e) /**< @brief GlmsAdpiMoUpdateAmInd */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiMoUpdateKeyfileInd
 *
 * Descr  : Indication when One or more attributes in the KeyFileManagement or KeyFileInformation
 *          MO has been updated.
 *
 * Dir    : GLMS --> Adapter
 *
 * Data   : sequenceNumber       - The sequence number of the most recently
 *                                 installed Key File.
 *
 *          installationTime     - The time stamp of when the Key File was installed.
 *
 *          locatable            - Not used by GLMS and will always be set to false.
 *
 *          installKeyFile       - The reportProgress attribute of KeyFileManagement MO.
 *
 *          productType          - The product type of the Key File.
 *
 *****************************************************************************/
typedef struct
{
   uint32_t          sigNo;
   GlmsAsyncAction   installKeyFile;
   time_t32          installationTime;
   uint32_t          sequenceNumber;
   GlmsBool          locatable;
   char              productType[GLMS_PRODUCT_TYPE_LEN];
} GlmsAdpiMoUpdateKeyfileInd;
#define GLMS_ADPI_MO_UPDATE_KEY_FILE_IND (0x190015f) /**< @brief GlmsAdpiMoUpdateKeyfileInd */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiMoUpdateLmInd
 *
 * Descr  : Indication when One or more attributes in the LM
 *          MO has been updated.
 *
 * Dir    : GLMS --> Adapter
 *
 * Data   : lastInventoryChange    - The last time when a change happened to license
 *                                   inventory.
 *
 *          lastLicenseInventoryRefresh  - The last time a license validation was performed.
 *
 *          fingerprintUpdateable  - True if it is possible to update the fingerprint,
 *                                   otherwise false.
 *
 *          lmState                - The LM state of GLMS. See the FD for description.
 *
 *          fingerprint            - The fingerprint
 *
 *          referenceToLicenseServer  - Not used be GLMS. Will always be empty string.
 *
 *****************************************************************************/
typedef struct
{
   uint32_t     sigNo;
   time_t32     lastInventoryChange;
   time_t32     lastLicenseInventoryRefresh;
   GlmsBool     fingerprintUpdateable;
   GlmsLmState  lmState;
   char         fingerprint[GLMS_FINGERPRINT_LEN];
   char         referenceToLicenseServer[1];
} GlmsAdpiMoUpdateLmInd;
#define GLMS_ADPI_MO_UPDATE_LM_IND (0x1900160) /**< @brief GlmsAdpiMoUpdateLmInd */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiMoUpdateFeatureStateInd
 *
 * Descr  : Indication when One or more attributes in the FeatureState MO has 
 *          been updated.
 *
 * Dir    : GLMS --> Adapter
 *
 * Data   : featureState      - The feature state of the license.
 *
 *          licenseState      - The license state.
 *
 *          serviceState      - The service state.
 *
 *          featureStateId    - The MO id of the FeatureState MO.
 *
 *          description       - The description as received from the LIHI client.
 *
 *          keyId             - The license number (CXC...)
 *
 *          activeFeatureKeyId - The featureKeyId of the FeatureKey MO that is
 *                              currently the active license key.
 *
 *****************************************************************************/
typedef struct
{
   uint32_t          sigNo;
   GlmsFeatureState  featureState;
   GlmsLicenseState  licenseState;
   GlmsServiceState  serviceState;
   char              featureStateId[GLMS_MO_KEY_LEN];
   char              description[GLMS_DESCRIPTION_LEN];
   char              keyId[GLMS_KEY_ID_LEN];
   char              activeFeatureKeyId[GLMS_MO_KEY_LEN];
} GlmsAdpiMoUpdateFeatureStateInd;
#define GLMS_ADPI_MO_UPDATE_FEATURE_STATE_IND (0x1900161) /**< @brief GlmsAdpiMoUpdateFeatureStateInd */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiMoUpdateFeatureKeyNameInd
 *
 * Descr  : Indication when the name attribute has been updated in a FeatureKey MO.
 *
 * Dir    : GLMS --> Adapter
 *
 * Data   : featureKeyId   - The MO id of the FeatureKey MO.
 *
 *          name           - The new name of the Feature Key.
 *
 *****************************************************************************/
typedef struct
{
   uint32_t          sigNo;
   char              featureKeyId[GLMS_MO_KEY_LEN];
   char              name[GLMS_KEY_NAME_LEN];
} GlmsAdpiMoUpdateFeatureKeyNameInd;
#define GLMS_ADPI_MO_UPDATE_FEATURE_KEY_NAME_IND (0x1900162) /**< @brief GlmsAdpiMoUpdateFeatureKeyNameInd */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiMoUpdateCapacityStateInd
 *
 * Descr  : Indication when One or more attributes in the CapacityKey MO has been updated.
 *          (Except name attribute)
 * Dir    : GLMS --> Adapter
 *
 * Data   : currentCapacityValue - The licensed value of the capacity feature.
 *
 *          licensedCapacityLimitReached - For GLMS this will always be False.
 *
 *          licenseState     - The license state of the license.
 *
 *          grantedCapacityLevel   - For GLMS this will always be the same as
 *                                   licensedCapacityLimit.value.
 *
 *          capacityStateId  - The CapacityState MO id. This is the key value and 
 *                             will later be used to identify the capacity key.
 *
 *          description      - The description of the license as received by the 
 *                             LIHI client.
 *
 *          keyId            - The license number (CXC...)
 *
 *          activeCapacityKeyId - The capacityKeyId of the CapacityKey MO that is
 *                             currently the active license key.
 *
 *****************************************************************************/
typedef struct
{
   uint32_t          sigNo;
   GlmsCapacityValue currentCapacityValue;
   GlmsBool          licensedCapacityLimitReached;
   GlmsLicenseState  licenseState;
   int32_t           grantedCapacityLevel;
   char              capacityStateId[GLMS_KEY_ID_LEN];
   char              description[GLMS_DESCRIPTION_LEN];
   char              keyId[GLMS_KEY_ID_LEN]; 
   char              activeCapacityKeyId[GLMS_MO_KEY_LEN];
} GlmsAdpiMoUpdateCapacityStateInd;
#define GLMS_ADPI_MO_UPDATE_CAPACITY_STATE_IND (0x1900163) /**< @brief GlmsAdpiMoUpdateCapacityStateInd */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiMoUpdateCapacityKeyNameInd
 *
 * Descr  : Indication when Name in the CapacityKey MO has been updated.
 *
 * Dir    : GLMS --> Adapter
 *
 * Data   : capacityKeyId  - The ID of the capacity key for which MO the attributes
 *                           was requested.
 *
 *          name           - The name of the license, provided by the license key subscriber 
 *
 *****************************************************************************/
typedef struct
{
   uint32_t          sigNo;
   char              capacityKeyId[GLMS_MO_KEY_LEN];
   char              name[GLMS_KEY_NAME_LEN];
} GlmsAdpiMoUpdateCapacityKeyNameInd;
#define GLMS_ADPI_MO_UPDATE_CAPACITY_KEY_NAME_IND (0x1900164) /**< @brief GlmsAdpiMoUpdateCapacityKeyNameInd */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiReadIuMoReq
 *
 * Descr  : A request from the adapter layer to GLMS get the attributes of the
 *          IntegrationUnlock MO.
 *
 * Dir    : Adapter --> GLMS
 *
 * Data   : -
 *
 *****************************************************************************/
typedef struct
{
   uint32_t          sigNo;
} GlmsAdpiReadIuMoReq;
#define GLMS_ADPI_READ_IU_MO_REQ (0x1900165) /**< @brief GlmsAdpiReadIuMoReq */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiReadIuMoRsp
 *
 * Descr  : A response to GlmsAdpiReadIuMoReq.
 *
 * Dir    : GLMS --> Adapter
 *
 * Data   : expiration        - The date and time when Integration Unlock expires.
 *
 *          result            - GLMS_OK if the request was accepted and successful.
 *                              GLMS_NOK if the request was rejected or failed.
 *
 *          activationState   - The activation state of IU.
 *
 *          activationsLeft   - Remaining activations of IU.
 *
 *          resultInfo        - Information about reject cause if result is NOK.
 *
 *****************************************************************************/
typedef struct
{
   uint32_t             sigNo;
   time_t32             expiration;
   GlmsResult           result;
   GlmsActivationState  activationState;
   uint32_t             activationsLeft;
   char                 resultInfo[GLMS_RESULT_INFO_LEN];
} GlmsAdpiReadIuMoRsp;
#define GLMS_ADPI_READ_IU_MO_RSP (0x1900166) /**< @brief GlmsAdpiReadIuMoRsp */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiGetFeatureKeyMoListReq
 *
 * Descr  : A request from the adapter layer to GLMS to get the list of available
 *          Feature Keys.
 *
 * Dir    : Adapter --> GLMS
 *
 * Data   : -
 *
 *****************************************************************************/
typedef struct
{
   uint32_t          sigNo;
} GlmsAdpiGetFeatureKeyMoListReq;
#define GLMS_ADPI_GET_FEATURE_KEY_MO_LIST_REQ (0x1900167) /**< @brief GlmsAdpiGetFeatureKeyMoListReq */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiGetFeatureKeyMoListRsp
 *
 * Descr  : Response to GlmsAdpiGetFeatureKeyMoListReq
 *
 * Dir    : GLMS --> Adapter
 *
 * Data   : result            - GLMS_OK if the request was accepted and successful.
 *                              GLMS_NOK if the request was rejected or failed.
 *
 *          nrOfFeatureKeyIds - The number of FeatureKey MO:s.
 *
 *          featureKeyId      - An array of the featureKeyId:s of the MO:s.
 *
 *****************************************************************************/
typedef struct
{
   uint32_t          sigNo;
   GlmsResult        result;
   uint32_t          nrOfFeatureKeyIds;
   char              featureKeyId[1][GLMS_MO_KEY_LEN];
} GlmsAdpiGetFeatureKeyMoListRsp;
#define GLMS_ADPI_GET_FEATURE_KEY_MO_LIST_RSP (0x1900168) /**< @brief GlmsAdpiGetFeatureKeyMoListRsp */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiGetCapacityKeyMoListReq
 *
 * Descr  : A request from the adapter layer to GLMS to get the list of available
 *          Capacity Keys.
 *
 * Dir    : Adapter --> GLMS
 *
 * Data   : -
 *
 *****************************************************************************/
typedef struct
{
   uint32_t          sigNo;
} GlmsAdpiGetCapacityKeyMoListReq;
#define GLMS_ADPI_GET_CAPACITY_KEY_MO_LIST_REQ (0x1900169) /**< @brief GlmsAdpiGetCapacityKeyMoListReq */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiGetCapacityKeyMoListRsp
 *
 * Descr  : Response to GlmsAdpiGetCapacityKeyMoListReq
 *
 * Dir    : GLMS --> Adapter
 *
 * Data   : result            - GLMS_OK if the request was accepted and successful.
 *                              GLMS_NOK if the request was rejected or failed.
 *
 *          nrOfCapacityKeyIds - The number of CapacityKey MO:s.
 *
 *          capacityKeyId      - An array of the capacityKeyId:s of the MO:s.
 *
 *****************************************************************************/
typedef struct
{
   uint32_t          sigNo;
   GlmsResult        result;
   uint32_t          nrOfCapacityKeyIds;
   char              capacityKeyId[1][GLMS_MO_KEY_LEN];
} GlmsAdpiGetCapacityKeyMoListRsp;
#define GLMS_ADPI_GET_CAPACITY_KEY_MO_LIST_RSP (0x190016a) /**< @brief GlmsAdpiGetCapacityKeyMoListRsp */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiReadAmMoReq
 *
 * Descr  : A request from the adapter layer to GLMS get the attributes of the
 *          AutonomousMode MO.
 *
 * Dir    : Adapter --> GLMS
 *
 * Data   : -
 *
 *****************************************************************************/
typedef struct
{
   uint32_t          sigNo;
} GlmsAdpiReadAmMoReq;
#define GLMS_ADPI_READ_AM_MO_REQ (0x190016b) /**< @brief GlmsAdpiReadAmMoReq */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiReadAmMoRsp
 *
 * Descr  : A response to GlmsAdpiReadAmMoReq.
 *
 * Dir    : GLMS --> Adapter
 *
 * Data   : expiration        - The date and time when Autonomous Mode expires.
 *
 *          activationState   - The activation state of AM.
 *
 *          result            - GLMS_OK if the request was accepted and successful.
 *                              GLMS_NOK if the request was rejected or failed.
 *
 *          resultInfo        - Information about reject cause if result is NOK.
 *
 *****************************************************************************/
typedef struct
{
   uint32_t             sigNo;
   time_t32             expiration;
   GlmsActivationState  activationState;
   GlmsResult           result;
   char                 resultInfo[GLMS_RESULT_INFO_LEN];
} GlmsAdpiReadAmMoRsp;
#define GLMS_ADPI_READ_AM_MO_RSP (0x190016c) /**< @brief GlmsAdpiReadAmMoRsp */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiGetFeatureStateMoListReq
 *
 * Descr  : A request from the adapter layer to GLMS to get the list of available
 *          Feature State MO's.
 *
 * Dir    : Adapter --> GLMS
 *
 * Data   : -
 *
 *****************************************************************************/
typedef struct
{
   uint32_t          sigNo;
} GlmsAdpiGetFeatureStateMoListReq;
#define GLMS_ADPI_GET_FEATURE_STATE_MO_LIST_REQ (0x190016d) /**< @brief GlmsAdpiGetFeatureStateMoListReq */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiGetFeatureStateMoListRsp
 *
 * Descr  : Response to GlmsAdpiGetFeatureStateMoListReq
 *
 * Dir    : GLMS --> Adapter
 *
 * Data   : result            - GLMS_OK if the request was accepted and successful.
 *                              GLMS_NOK if the request was rejected or failed.
 *
 *          nrOfFeatureStateIds - The number of FeatureState MO:s.
 *
 *          featureStateId      - An array of the featureStateId:s of the MO:s.
 *
 *****************************************************************************/
typedef struct
{
   uint32_t          sigNo;
   GlmsResult        result;
   uint32_t          nrOfFeatureStateIds;
   char              featureStateId[1][GLMS_MO_KEY_LEN];
} GlmsAdpiGetFeatureStateMoListRsp;
#define GLMS_ADPI_GET_FEATURE_STATE_MO_LIST_RSP (0x190016e) /**< @brief GlmsAdpiGetFeatureStateMoListRsp */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiReadMoFeatureStateReq
 *
 * Descr  : Adapter requests the FeatureState MO with the given featureStateId. If
 *          no MO with the given featureStateId exist the response will have a
 *          result of GLMS_NOK.
 *
 * Dir    : Adapter --> GLMS
 *
 * Data   : featureStateId    - The FeatureState MO Id.
 *
 *****************************************************************************/
typedef struct
{
   uint32_t       sigNo;
   char           featureStateId[GLMS_MO_KEY_LEN];
} GlmsAdpiReadMoFeatureStateReq;
#define GLMS_ADPI_READ_MO_FEATURE_STATE_REQ (0x190016f) /**< @brief GlmsAdpiReadMoFeatureStateReq */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiReadMoFeatureStateRsp
 *
 * Descr  : Response to GlmsAdpiReadMoFeatureStateReq.
 *
 * Dir    : GLMS --> Adapter
 *
 * Data   : result            - GLMS_OK if the request was accepted and successful
 *                              GLMS_NOK otherwise.
 *
 *          featureState      - The feature state of the license.
 *
 *          licenseState      - The license state.
 *
 *          serviceState      - The service state.
 *
 *          featureStateId    - The MO id of the FeatureState MO.
 *
 *          description       - The description as received from the LIHI client.
 *
 *          keyId             - The license number (CXC...)
 *
 *          resultInfo        - Information about reject cause if result is NOK.
 *
 *          activeFeatureKeyId - The featureKeyId of the FeatureKey MO that is
 *                              currently the active license key.
 *
 *****************************************************************************/
typedef struct
{
   uint32_t          sigNo;
   GlmsResult        result;
   GlmsFeatureState  featureState;
   GlmsLicenseState  licenseState;
   GlmsServiceState  serviceState;
   char              featureStateId[GLMS_MO_KEY_LEN];
   char              description[GLMS_DESCRIPTION_LEN];
   char              keyId[GLMS_KEY_ID_LEN];
   char              resultInfo[GLMS_RESULT_INFO_LEN];
   char              activeFeatureKeyId[GLMS_MO_KEY_LEN];
} GlmsAdpiReadMoFeatureStateRsp;
#define GLMS_ADPI_READ_MO_FEATURE_STATE_RSP (0x1900170) /**< @brief GlmsAdpiReadMoFeatureStateRsp */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiReadMoCapacityKeyReq
 *
 * Descr  : Adapter requests the CapacityKey MO with the given capacityKeyId. If
 *          no MO with the given capacityKeyId exist the response will have a
 *          result of GLMS_NOK.
 *
 * Dir    : Adapter --> GLMS
 *
 * Data   : capacityKeyId    - The Capacity Key Id of the requested CapacityKey MO.
 *
 *****************************************************************************/
typedef struct
{
   uint32_t       sigNo;
   char           capacityKeyId[GLMS_MO_KEY_LEN];
} GlmsAdpiReadMoCapacityKeyReq;
#define GLMS_ADPI_READ_MO_CAPACITY_KEY_REQ (0x1900171) /**< @brief GlmsAdpiReadMoCapacityKeyReq */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiReadMoCapacityKeyRsp
 *
 * Descr  : Response to GlmsAdpiReadMoCapacityKeyReq.
 *
 * Dir    : GLMS --> Adapter
 *
 * Data   : licensedCapacityValue - The licensed value of the capacity feature.
 *
 *          validFrom         - The date when the license key was, or will be, enabled.
 *
 *          expiration        - The date when the license key was, or will be, disabled.
 *
 *          licensedCapacityLimitReached - For GLMS this will always be False.
 *
 *          result            - GLMS_OK if the request was accepted and successful.
 *                              GLMS_NOK if the request was rejected or failed.
 *
 *          grantedCapacityLevel - For GLMS this will always be the same as
 *                                 licensedCapacityLimit.value
 *
 *          capacityUnit      - The unit of measurement for the capacity feature.
 *
 *          resultInfo        - Information about reject cause if result is NOK.
 *
 *          capacityKeyId     - The Capacity Key Id of the requested CapacityKey MO.
 *
 *          keyId             - The license number (CXC..)
 *
 *          name              - The name of the Capacity.
 *
 *          productType       - The product type of the license.
 *
 *****************************************************************************/
typedef struct
{
   uint32_t          sigNo;
   GlmsCapacityValue licensedCapacityValue;
   time_t32          validFrom;
   time_t32          expiration;
   GlmsBool          licensedCapacityLimitReached;
   GlmsResult        result;
   int32_t           grantedCapacityLevel;
   char              capacityUnit[GLMS_CAPACITY_UNIT_LEN];
   char              resultInfo[GLMS_RESULT_INFO_LEN];
   char              capacityKeyId[GLMS_MO_KEY_LEN];
   char              keyId[GLMS_KEY_ID_LEN];
   char              name[GLMS_KEY_NAME_LEN];
   char              productType[GLMS_PRODUCT_TYPE_LEN];
} GlmsAdpiReadMoCapacityKeyRsp;
#define GLMS_ADPI_READ_MO_CAPACITY_KEY_RSP (0x1900172) /**< @brief GlmsAdpiReadMoCapacityKeyRsp */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiGetCapacityStateMoListReq
 *
 * Descr  : A request from the adapter layer to GLMS to get the list of available
 *          Capacity State MO's.
 *
 * Dir    : Adapter --> GLMS
 *
 * Data   : -
 *
 *****************************************************************************/
typedef struct
{
   uint32_t          sigNo;
} GlmsAdpiGetCapacityStateMoListReq;
#define GLMS_ADPI_GET_CAPACITY_STATE_MO_LIST_REQ (0x1900173) /**< @brief GlmsAdpiGetCapacityStateMoListReq */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiGetCapacityStateMoListRsp
 *
 * Descr  : Response to GlmsAdpiGetCapacityStateMoListReq
 *
 * Dir    : GLMS --> Adapter
 *
 * Data   : result            - GLMS_OK if the request was accepted and successful.
 *                              GLMS_NOK if the request was rejected or failed.
 *
 *          nrOfCapacityStateIds - The number of CapacityState MO:s.
 *
 *          capacityStateId      - An array of the capacityStateId:s of the MO:s.
 *
 *****************************************************************************/
typedef struct
{
   uint32_t          sigNo;
   GlmsResult        result;
   uint32_t          nrOfCapacityStateIds;
   char              capacityStateId[1][GLMS_MO_KEY_LEN];
} GlmsAdpiGetCapacityStateMoListRsp;
#define GLMS_ADPI_GET_CAPACITY_STATE_MO_LIST_RSP (0x1900174) /**< @brief GlmsAdpiGetCapacityStateMoListRsp */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiReadMoCapacityStateReq
 *
 * Descr  : Adapter requests the CapacityState MO with the given capacityStateId. If
 *          no MO with the given capacityStateId exist the response will have a
 *          result of GLMS_NOK.
 *
 * Dir    : Adapter --> GLMS
 *
 * Data   : capacityStateId    - The CapacityState MO Id.
 *
 *****************************************************************************/
typedef struct
{
   uint32_t       sigNo;
   char           capacityStateId[GLMS_MO_KEY_LEN];
} GlmsAdpiReadMoCapacityStateReq;
#define GLMS_ADPI_READ_MO_CAPACITY_STATE_REQ (0x1900175) /**< @brief GlmsAdpiReadMoCapacityStateReq */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiReadMoCapacityStateRsp
 *
 * Descr  : Response to GlmsAdpiReadMoCapacityStateReq.
 *
 * Dir    : GLMS --> Adapter
 *
 * Data   : licensedCapacityValue - The licensed value of the capacity feature.
 *
 *          licensedCapacityLimitReached - For GLMS this will always be False.
 *
 *          result            - GLMS_OK if the request was accepted and successful
 *                              GLMS_NOK otherwise.
 *
 *          licenseState      - The license state.
 *
 *          grantedCapacityLevel - For GLMS this will always be the same as
 *                                 licensedCapacityLimit.value.
 *
 *          capacityUnit      - The unit of measurement for the capacity feature.
 *
 *          capacityStateId   - The MO id of the CapacityState MO.
 *
 *          description       - The description as received from the LIHI client.
 *
 *          keyId             - The license number (CXC...)
 *
 *          resultInfo        - Information about reject cause if result is NOK.
 *
 *          activeCapacityKeyId - The capacityKeyId of the CapacityKey MO that is
 *                              currently the active license key.
 *
 *****************************************************************************/
typedef struct
{
   uint32_t          sigNo;
   GlmsCapacityValue currentCapacityValue;
   GlmsBool          licensedCapacityLimitReached;
   GlmsResult        result;
   GlmsLicenseState  licenseState;
   int32_t           grantedCapacityLevel;
   char              capacityUnit[GLMS_CAPACITY_UNIT_LEN];
   char              capacityStateId[GLMS_MO_KEY_LEN];
   char              description[GLMS_DESCRIPTION_LEN];
   char              keyId[GLMS_KEY_ID_LEN];
   char              resultInfo[GLMS_RESULT_INFO_LEN];
   char              activeCapacityKeyId[GLMS_MO_KEY_LEN];
} GlmsAdpiReadMoCapacityStateRsp;
#define GLMS_ADPI_READ_MO_CAPACITY_STATE_RSP (0x1900176) /**< @brief GlmsAdpiReadMoCapacityStateRsp */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiReadMoGracePeriodReq
 *
 * Descr  : Adapter requests the GracePeriod MO with the given capacityStateId. If
 *          no MO with the given capacityStateId exist the response will have a
 *          result of GLMS_NOK.
 *
 * Dir    : Adapter --> GLMS
 *
 * Data   : capacityStateId    - The CapacityState MO Id.
 *
 *****************************************************************************/
typedef struct
{
   uint32_t       sigNo;
   char           capacityStateId[GLMS_MO_KEY_LEN];
} GlmsAdpiReadMoGracePeriodReq;
#define GLMS_ADPI_READ_MO_GRACE_PERIOD_REQ (0x1900177) /**< @brief GlmsAdpiReadMoGracePeriodReq */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiReadMoGracePeriodRsp
 *
 * Descr  : Response to GlmsAdpiReadMoGracePeriodReq.
 *
 * Dir    : GLMS --> Adapter
 *
 * Data   : gracePeriod       - The struct containing the grace period data
 *
 *          result            - GLMS_OK if the request was accepted and successful
 *                              GLMS_NOK otherwise.
 *
 *          capacityStateId   - The ID of GracePeriod MO.
 *
 *          resultInfo        - Information about reject cause if result is NOK.
 *
 *****************************************************************************/
typedef struct
{
   uint32_t           sigNo;
   GlmsGracePeriod    gracePeriod;
   GlmsResult         result;
   char               capacityStateId[GLMS_MO_KEY_LEN];
   char               resultInfo[GLMS_RESULT_INFO_LEN];
} GlmsAdpiReadMoGracePeriodRsp;
#define GLMS_ADPI_READ_MO_GRACE_PERIOD_RSP (0x1900178) /**< @brief GlmsAdpiReadMoGracePeriodRsp */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiMoUpdateGracePeriodInd
 *
 * Descr  : Indication from GLMS that GracePeriod Attribute has been altered.
 *
 * Dir    : GLMS --> Adapter
 *
 * Data   : gracePeriod       - The struct containing the grace period data
 *
 *****************************************************************************/
typedef struct
{
   uint32_t           sigNo;
   GlmsGracePeriod    gracePeriod;
} GlmsAdpiMoUpdateGracePeriodInd;
#define GLMS_ADPI_MO_UPDATE_GRACE_PERIOD_IND (0x1900179) /**< @brief GlmsAdpiMoUpdateGracePeriodInd */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiMoUpdateCapacityKeyCapacityUnitInd
 *
 * Descr  : Indication when CapacityUnit in the CapacityKey MO has been updated.
 *
 * Dir    : GLMS --> Adapter
 *
 * Data   : capacityUnit   - The unit of measurement for the capacity feature.
 *
 *          capacityKeyId  - The ID of the capacity key for which MO the
 *                           attribute are updated.
 *
 *****************************************************************************/
typedef struct
{
   uint32_t          sigNo;
   char              capacityUnit[GLMS_CAPACITY_UNIT_LEN];
   char              capacityKeyId[GLMS_MO_KEY_LEN];
} GlmsAdpiMoUpdateCapacityKeyCapacityUnitInd;
#define GLMS_ADPI_MO_UPDATE_CAPACITY_KEY_CAPACITY_UNIT_IND (0x190017a) /**< @brief GlmsAdpiMoUpdateCapacityKeyCapacityUnitInd */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiMoUpdateCapacityStateCapacityUnitInd
 *
 * Descr  : Indication when CapacityUnit in the CapacityState MO has been updated.
 *
 * Dir    : GLMS --> Adapter
 *
 * Data   : capacityUnit    - The unit of measurement for the capacity feature.
 *
 *          capacityStateId - The ID of the capacity state for which MO the
 *                            attribute is updated.
 *
 *****************************************************************************/
typedef struct
{
   uint32_t          sigNo;
   char              capacityUnit[GLMS_CAPACITY_UNIT_LEN];
   char              capacityStateId[GLMS_MO_KEY_LEN];
} GlmsAdpiMoUpdateCapacityStateCapacityUnitInd;
#define GLMS_ADPI_MO_UPDATE_CAPACITY_STATE_CAPACITY_UNIT_IND (0x190017b) /**< @brief GlmsAdpiMoUpdateCapacityStateCapacityUnitInd */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiMoUpdateLicenseSupportInd
 *
 * Descr  : Indication when the LicenseSupport MO has been updated.
 *
 * Dir    : GLMS --> Adapter
 *
 * Data   : areaId - The area Id.
 *
 *****************************************************************************/
typedef struct
{
   uint32_t          sigNo;
   char              areaId[GLMS_AREA_ID_LEN];
} GlmsAdpiMoUpdateLicenseSupportInd;
#define GLMS_ADPI_MO_UPDATE_LICENSE_SUPPORT_IND (0x190017c) /**< @brief GlmsAdpiMoUpdateLicenseSupportInd */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiReadMoLicenseSupportReq
 *
 * Descr  : Adapter requests the LicenseSupport MO.
 *
 * Dir    : Adapter --> GLMS
 *
 * Data   : -
 *
 *****************************************************************************/
typedef struct
{
   uint32_t       sigNo;
} GlmsAdpiReadMoLicenseSupportReq;
#define GLMS_ADPI_READ_MO_LICENSE_SUPPORT_REQ (0x190017d) /**< @brief GlmsAdpiReadMoLicenseSupportReq */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiReadMoLicenseSupportRsp
 *
 * Descr  : Response to GlmsAdpiReadMoLicenseSupportReq.
 *
 * Dir    : GLMS --> Adapter
 *
 * Data   : result     - GLMS_OK if the request was accepted and successful.
 *                       GLMS_NOK otherwise.
 *
 *          resultInfo - The result info, set only if result is GLMS_NOK.
 *
 *          areaId     - The Area Id.
 *
 *****************************************************************************/
typedef struct
{
   uint32_t    sigNo;
   GlmsResult  result;
   char        resultInfo[GLMS_RESULT_INFO_LEN];
   char        areaId[GLMS_AREA_ID_LEN];
} GlmsAdpiReadMoLicenseSupportRsp;
#define GLMS_ADPI_READ_MO_LICENSE_SUPPORT_RSP (0x190017e) /**< @brief GlmsAdpiReadMoLicenseSupportRsp */


/* ==============================================================================
   ==============================================================================

   ALARMS

   ==============================================================================
   ============================================================================== */

/**************************************************************************//**
 *
 * Signal : GlmsAdpiEuAlarmInd
 *
 * Descr  : Indication from GLMS that the alarm has been activated or ceased.
 *
 * Dir    : GLMS --> Adapter
 *
 * Data   : alarmState      - The new alarm state of the Emergency Unlock Reset Key
 *                            Required alarm.
 *
 *          alarmSeverity   - The new severity of the alarm.
 *
 *****************************************************************************/
typedef struct
{
   uint32_t           sigNo;
   GlmsAlarmState     alarmState;
   GlmsAlarmSeverity  alarmSeverity;
} GlmsAdpiEuAlarmInd;
#define GLMS_ADPI_EU_ALARM_IND (0x19001a2) /**< @brief GlmsAdpiEuAlarmInd */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiAmAlarmInd
 *
 * Descr  : Indication from GLMS that the alarm has been activated or ceased.
 *
 * Dir    : GLMS --> Adapter
 *
 * Data   : alarmState      - The new alarm state of the Autonomous Mode alarm.
 *
 *****************************************************************************/
typedef struct
{
   uint32_t           sigNo;
   GlmsAlarmState     alarmState;
} GlmsAdpiAmAlarmInd;
#define GLMS_ADPI_AM_ALARM_IND (0x19001a4) /**< @brief GlmsAdpiAmAlarmInd */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiGpAlarmInd
 *
 * Descr  : Indication from GLMS that the alarm has been activated or ceased.
 *
 * Dir    : GLMS --> Adapter
 *
 * Data   : alarmState      - The new alarm state of the Grace Period alarm.
 *
 *          alarmServerity  - The new severity of the alarm.
 *
 *          gracePeriodIf   - The MO id of the GracePeriod MO.
 *
 *****************************************************************************/
typedef struct
{
   uint32_t           sigNo;
   GlmsAlarmState     alarmState;
   GlmsAlarmSeverity  alarmSeverity;
   char               gracePeriodId[GLMS_KEY_ID_LEN];
} GlmsAdpiGpAlarmInd;
#define GLMS_ADPI_GP_ALARM_IND (0x19001a5) /**< @brief GlmsAdpiGpAlarmInd */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiKeyFileFaultAlarmReq
 *
 * Descr  : Request from GLMS to activate or cease the alarm.
 *
 * Dir    : GLMS --> Adapter
 *
 * Data   : alarmState - The new alarm state of the KF fault alarm.
 *
 *****************************************************************************/
typedef struct
{
   uint32_t       sigNo;
   GlmsAlarmState alarmState;
} GlmsAdpiKeyFileFaultAlarmReq;
#define GLMS_ADPI_KEY_FILE_FAULT_ALARM_REQ (0x19001a0) /**< @brief GlmsAdpiKeyFileFaultAlarmReq */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiKeyFileFaultAlarmRsp
 *
 * Descr  : Respons to GlmsAdpiKeyFileFaultAlarmReq.
 *
 * Dir    : Adapter --> GLMS
 *
 * Data   : kffAlarmEventId - The event id of the alarm. 0 if ceased.
 *
 *****************************************************************************/
typedef struct
{
   uint32_t       sigNo;
   uint32_t       kffAlarmEventId;
} GlmsAdpiKeyFileFaultAlarmRsp;
#define GLMS_ADPI_KEY_FILE_FAULT_ALARM_RSP (0x19001a6) /**< @brief GlmsAdpiKeyFileFaultAlarmRsp */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiLicenseKeyNotAvailableAlarmReq
 *
 * Descr  : Request from GLMS to activate or cease the alarm.
 *
 * Dir    : GLMS --> Adapter
 *
 * Data   : moId         - The featureStateId or capacityStateId of the MO that
 *                         shall have its alarm status updated.
 *
 *          licenseType  - FeatureState or CapacityState.
 *
 *          alarmState   - The new alarm state.
 *
 *          alarmReason  - The Reason for the alarm. See GlmsAlarmReason
 *                         definition in glmsDataTypes.h.
 *
 *****************************************************************************/
typedef struct
{
   uint32_t         sigNo;
   char             moId[GLMS_MO_KEY_LEN];
   GlmsLicenseType  licenseType;
   GlmsAlarmState   alarmState;
   GlmsAlarmReason  alarmReason;
} GlmsAdpiLicenseKeyNotAvailableAlarmReq;
#define GLMS_ADPI_LICENSE_KEY_NOT_AVAILABLE_ALARM_REQ (0x19001a1) /**< @brief GlmsAdpiLicenseKeyNotAvailableAlarmReq */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiLicenseKeyNotAvailableAlarmRsp
 *
 * Descr  : Response to GlmsAdpiLicenseKeyNotAvailableAlarmRsp.
 *
 * Dir    : Adapter --> GLMS
 *
 * Data   : moId              - The featureStateId or capacityStateId of the MO
 *                              that had its alarm status updated.
 *
 *          licenseType       - FeatureState or CapacityState.
 *
 *          lknaAlarmEventId  - The event id of the alarm. 0 if ceased.
 *
 *****************************************************************************/
typedef struct
{
   uint32_t         sigNo;
   char             moId[GLMS_MO_KEY_LEN];
   GlmsLicenseType  licenseType;
   uint32_t         lknaAlarmEventId;
} GlmsAdpiLicenseKeyNotAvailableAlarmRsp;
#define GLMS_ADPI_LICENSE_KEY_NOT_AVAILABLE_ALARM_RSP (0x19001a7) /**< @brief GlmsAdpiLicenseKeyNotAvailableAlarmRsp */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiLicenseKeyExpirationAlarmInd
 *
 * Descr  : Indicate to GLMS that the alarm has been activated or ceased.
 *
 * Dir    : GLMS --> Adapter
 *
 * Data   : alarmState     - The mew alarm state.
 *
 *          additionalInfo - String with the affected License Keys.
 *
 *****************************************************************************/
typedef struct
{
   uint32_t          sigNo;
   GlmsAlarmState    alarmState;
   char              additionalInfo[1];
} GlmsAdpiLicenseKeyExpirationAlarmInd;
#define GLMS_ADPI_LICENSE_KEY_EXPIRATION_ALARM_IND (0x19001a8) /**< @brief GlmsAdpiLicenseKeyExpirationAlarmInd */


/* ==============================================================================
   ==============================================================================

   PERSISTENT PARAMETERS INTERFACE

   ==============================================================================
   ============================================================================== */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiPersistentParameterIndexListReq
 *
 * Descr  : GLMS requests the number if existing indexes stored by the adapter.
 *
 * Dir    : GLMS --> Adapter
 *
 * Data   : requestId            - A request id, assigned by GLMS and returned
 *                                 by the adapter in the response.
 *
 *          table                - The name of the table for which the index
 *                                 list is requested.
 *
 *****************************************************************************/
typedef struct
{
   uint32_t       sigNo;
   uint32_t       requestId;
   char           table[GLMS_TABLE_NAME_LEN];
} GlmsAdpiPersistentParameterIndexListReq;
#define GLMS_ADPI_PERSISTENT_PARAMETER_INDEX_LIST_REQ (0x19001b0) /**< @brief GlmsAdpiPersistentParameterIndexListReq */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiPersistentParameterIndexListRsp
 *
 * Descr  : Response to GlmsAdpiPersistentParameterIndexListReq.
 *
 * Dir    : Adapter --> GLMS
 *
 * Data   : requestId            - A request id, assigned by GLMS and returned
 *                                 by the adapter in the response.
 *
 *          result               - GLMS_OK if the request was accepted and successful.
 *                                 GLMS_NOK if the request was rejected or failed.
 *
 *          nrOfIndexes          - The number of elements that can be read from
 *                                 the index array.
 *
 *          index                - The array with the existing indexes.
 *
 *****************************************************************************/
typedef struct
{
   uint32_t    sigNo;
   uint32_t    requestId;
   GlmsResult  result;
   uint32_t    nrOfIndexes;
   uint32_t    index[1];
} GlmsAdpiPersistentParameterIndexListRsp;
#define GLMS_ADPI_PERSISTENT_PARAMETER_INDEX_LIST_RSP (0x19001b1) /**< @brief GlmsAdpiPersistentParameterIndexListRsp */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiPersistentParameterSetReq
 *
 * Descr  : GLMS sets a persistent parameter.
 *
 * Dir    : GLMS --> Adapter
 *
 * Data   : requestId            - A request id, assigned by GLMS and returned
 *                                 by the adapter in the response.
 *
 *          table                - The name of the table in which the parameter
 *                                 shall be stored.
 *
 *          index                - The index of the parameter. This is used when
 *                                 GLMS later updates the parameter or reads
 *                                 the parameter.
 *
 *          value                - The parameter as a NULL terminated string.
 *
 *****************************************************************************/
typedef struct
{
   uint32_t       sigNo;
   uint32_t       requestId;
   char           table[GLMS_TABLE_NAME_LEN];
   uint32_t       index;
   char           value[1];
} GlmsAdpiPersistentParameterSetReq;
#define GLMS_ADPI_PERSISTENT_PARAMETER_SET_REQ (0x19001b2) /**< @brief GlmsAdpiPersistentParameterSetReq */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiPersistentParameterSetRsp
 *
 * Descr  : Response to GlmsAdpiPersistentParameterSetReq.
 *
 * Dir    : Adapter --> GLMS
 *
 * Data   : requestId            - A request id, assigned by GLMS and returned
 *                                 by the adapter in the response.
 *
 *          result               - GLMS_OK if the request was accepted and successful.
 *                                 GLMS_NOK if the request was rejected or failed.
 *
 *****************************************************************************/
typedef struct
{
   uint32_t    sigNo;
   uint32_t    requestId;
   GlmsResult  result;
} GlmsAdpiPersistentParameterSetRsp;
#define GLMS_ADPI_PERSISTENT_PARAMETER_SET_RSP (0x19001b3) /**< @brief GlmsAdpiPersistentParameterSetRsp */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiPersistentParameterGetReq
 *
 * Descr  : GLMS reads a persistent parameter.
 *
 * Dir    : GLMS --> Adapter
 *
 * Data   : requestId            - A request id, assigned by GLMS and returned
 *                                 by the adapter in the response.
 *
 *          table                - The name of the table that is read.
 *
 *****************************************************************************/
typedef struct
{
   uint32_t       sigNo;
   uint32_t       requestId;
   char           table[GLMS_TABLE_NAME_LEN];
} GlmsAdpiPersistentParameterGetReq;
#define GLMS_ADPI_PERSISTENT_PARAMETER_GET_REQ (0x19001b4) /**< @brief GlmsAdpiPersistentParameterGetReq */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiPersistentParameterGetRsp
 *
 * Descr  : Response to GlmsAdpiPersistentParameterGetReq.
 *
 * Dir    : Adapter --> GLMS
 *
 * Data   : requestId            - A request id, assigned by GLMS and returned
 *                                 by the adapter in the response.
 *
 *          result               - GLMS_OK if the request was accepted and successful.
 *                                 GLMS_NOK if the request was rejected or failed.
 *
 *          table                - The name of the table that is read.
 *
 *          index                - The index of the parameter returned in 'value'.
 *
 *          value                - The stored parameter.
 *
 *****************************************************************************/
typedef struct
{
   uint32_t       sigNo;
   uint32_t       requestId;
   GlmsResult     result;
   char           table[GLMS_TABLE_NAME_LEN];
   uint32_t       index;
   char           value[1];
} GlmsAdpiPersistentParameterGetRsp;
#define GLMS_ADPI_PERSISTENT_PARAMETER_GET_RSP (0x19001b5) /**< @brief GlmsAdpiPersistentParameterGetRsp */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiPersistentParameterDeleteIndexReq
 *
 * Descr  : GLMS deletes a persistent parameter.
 *
 * Dir    : GLMS --> Adapter
 *
 * Data   : requestId            - A request id, assigned by GLMS and returned
 *                                 by the adapter in the response.
 *
 *          table                - The name of the table that is read.
 *
 *          index                - The index of the parameter that shall be deleted.
 *
 *****************************************************************************/
typedef struct
{
   uint32_t       sigNo;
   uint32_t       requestId;
   char           table[GLMS_TABLE_NAME_LEN];
   uint32_t       index;
} GlmsAdpiPersistentParameterDeleteIndexReq;
#define GLMS_ADPI_PERSISTENT_PARAMETER_DELETE_INDEX_REQ (0x19001b6) /**< @brief GlmsAdpiPersistentParameterDeleteIndexReq */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiPersistentParameterDeleteIndexRsp
 *
 * Descr  : Response to GlmsAdpiPersistentParameterDeleteIndexReq.
 *
 * Dir    : Adapter --> GLMS
 *
 * Data   : requestId            - A request id, assigned by GLMS and returned
 *                                 by the adapter in the response.
 *
 *          result               - GLMS_OK if the request was accepted and successful.
 *                                 GLMS_NOK if the request was rejected or failed.
 *
 *****************************************************************************/
typedef struct
{
   uint32_t    sigNo;
   uint32_t    requestId;
   GlmsResult  result;
} GlmsAdpiPersistentParameterDeleteIndexRsp;
#define GLMS_ADPI_PERSISTENT_PARAMETER_DELETE_INDEX_RSP (0x19001b7) /**< @brief GlmsAdpiPersistentParameterDeleteIndexRsp */





/* ==============================================================================
   ==============================================================================

   SOFTWARE PARAMETERS INTERFACE

   ==============================================================================
   ============================================================================== */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiSoftwareParameterIndexListReq
 *
 * Descr  : GLMS requests the number if existing indexes stored by the adapter.
 *
 * Dir    : GLMS --> Adapter
 *
 * Data   : requestId            - A request id, assigned by GLMS and returned
 *                                 by the adapter in the response.
 *
 *          table                - The name of the table for which the index
 *                                 list is requested.
 *
 *****************************************************************************/
typedef struct
{
   uint32_t       sigNo;
   uint32_t       requestId;
   char           table[GLMS_TABLE_NAME_LEN];
} GlmsAdpiSoftwareParameterIndexListReq;
#define GLMS_ADPI_SOFTWARE_PARAMETER_INDEX_LIST_REQ (0x19001c0) /**< @brief GlmsAdpiSoftwareParameterIndexListReq */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiSoftwareParameterIndexListRsp
 *
 * Descr  : Response to GlmsAdpiSoftwareParameterIndexListReq.
 *
 * Dir    : Adapter --> GLMS
 *
 * Data   : requestId            - A request id, assigned by GLMS and returned
 *                                 by the adapter in the response.
 *
 *          result               - GLMS_OK if the request was accepted and successful.
 *                                 GLMS_NOK if the request was rejected or failed.
 *
 *          nrOfIndexes          - The number of elements that can be read from
 *                                 the index array.
 *
 *          index                - The array with the existing indexes.
 *
 *****************************************************************************/
typedef struct
{
   uint32_t    sigNo;
   uint32_t    requestId;
   GlmsResult  result;
   uint32_t    nrOfIndexes;
   uint32_t    index[1];
} GlmsAdpiSoftwareParameterIndexListRsp;
#define GLMS_ADPI_SOFTWARE_PARAMETER_INDEX_LIST_RSP (0x19001c1) /**< @brief GlmsAdpiSoftwareParameterIndexListRsp */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiSoftwareParameterSetReq
 *
 * Descr  : GLMS sets a persistent parameter.
 *
 * Dir    : GLMS --> Adapter
 *
 * Data   : requestId            - A request id, assigned by GLMS and returned
 *                                 by the adapter in the response.
 *
 *          table                - The name of the table in which the parameter
 *                                 shall be stored.
 *
 *          index                - The index of the parameter. This is used when
 *                                 GLMS later updates the parameter or reads
 *                                 the parameter.
 *
 *          value                - The parameter as a NULL terminated string.
 *
 *****************************************************************************/
typedef struct
{
   uint32_t       sigNo;
   uint32_t       requestId;
   char           table[GLMS_TABLE_NAME_LEN];
   uint32_t       index;
   char           value[1];
} GlmsAdpiSoftwareParameterSetReq;
#define GLMS_ADPI_SOFTWARE_PARAMETER_SET_REQ (0x19001c2) /**< @brief GlmsAdpiSoftwareParameterSetReq */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiSoftwareParameterSetRsp
 *
 * Descr  : Response to GlmsAdpiSoftwareParameterSetReq.
 *
 * Dir    : Adapter --> GLMS
 *
 * Data   : requestId            - A request id, assigned by GLMS and returned
 *                                 by the adapter in the response.
 *
 *          result               - GLMS_OK if the request was accepted and successful.
 *                                 GLMS_NOK if the request was rejected or failed.
 *
 *****************************************************************************/
typedef struct
{
   uint32_t    sigNo;
   uint32_t    requestId;
   GlmsResult  result;
} GlmsAdpiSoftwareParameterSetRsp;
#define GLMS_ADPI_SOFTWARE_PARAMETER_SET_RSP (0x19001c3) /**< @brief GlmsAdpiSoftwareParameterSetRsp */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiSoftwareParameterGetReq
 *
 * Descr  : GLMS reads a persistent parameter.
 *
 * Dir    : GLMS --> Adapter
 *
 * Data   : requestId            - A request id, assigned by GLMS and returned
 *                                 by the adapter in the response.
 *
 *          table                - The name of the table that is read.
 *
 *****************************************************************************/
typedef struct
{
   uint32_t       sigNo;
   uint32_t       requestId;
   char           table[GLMS_TABLE_NAME_LEN];
} GlmsAdpiSoftwareParameterGetReq;
#define GLMS_ADPI_SOFTWARE_PARAMETER_GET_REQ (0x19001c4) /**< @brief GlmsAdpiSoftwareParameterGetReq */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiSoftwareParameterGetRsp
 *
 * Descr  : Response to GlmsAdpiSoftwareParameterGetReq.
 *
 * Dir    : Adapter --> GLMS
 *
 * Data   : requestId            - A request id, assigned by GLMS and returned
 *                                 by the adapter in the response.
 *
 *          result               - GLMS_OK if the request was accepted and successful.
 *                                 GLMS_NOK if the request was rejected or failed.
 *
 *          table                - The name of the table that is read.
 *
 *          index                - The index of the parameter returned in 'value'.
 *
 *          value                - The stored parameter.
 *
 *****************************************************************************/
typedef struct
{
   uint32_t       sigNo;
   uint32_t       requestId;
   GlmsResult     result;
   char           table[GLMS_TABLE_NAME_LEN];
   uint32_t       index;
   char           value[1];
} GlmsAdpiSoftwareParameterGetRsp;
#define GLMS_ADPI_SOFTWARE_PARAMETER_GET_RSP (0x19001c5) /**< @brief GlmsAdpiSoftwareParameterGetRsp */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiSoftwareParameterDeleteIndexReq
 *
 * Descr  : GLMS deletes a persistent parameter.
 *
 * Dir    : GLMS --> Adapter
 *
 * Data   : requestId            - A request id, assigned by GLMS and returned
 *                                 by the adapter in the response.
 *
 *          table                - The name of the table that is read.
 *
 *          index                - The index of the parameter that shall be deleted.
 *
 *****************************************************************************/
typedef struct
{
   uint32_t       sigNo;
   uint32_t       requestId;
   char           table[GLMS_TABLE_NAME_LEN];
   uint32_t       index;
} GlmsAdpiSoftwareParameterDeleteIndexReq;
#define GLMS_ADPI_SOFTWARE_PARAMETER_DELETE_INDEX_REQ (0x19001c6) /**< @brief GlmsAdpiSoftwareParameterDeleteIndexReq */


/**************************************************************************//**
 *
 * Signal : GlmsAdpiSoftwareParameterDeleteIndexRsp
 *
 * Descr  : Response to GlmsAdpiSoftwareParameterDeleteIndexReq.
 *
 * Dir    : Adapter --> GLMS
 *
 * Data   : requestId            - A request id, assigned by GLMS and returned
 *                                 by the adapter in the response.
 *
 *          result               - GLMS_OK if the request was accepted and successful.
 *                                 GLMS_NOK if the request was rejected or failed.
 *
 *****************************************************************************/
typedef struct
{
   uint32_t    sigNo;
   uint32_t    requestId;
   GlmsResult  result;
} GlmsAdpiSoftwareParameterDeleteIndexRsp;
#define GLMS_ADPI_SOFTWARE_PARAMETER_DELETE_INDEX_RSP (0x19001c7) /**< @brief GlmsAdpiSoftwareParameterDeleteIndexRsp */


#ifdef __cplusplus
}
#endif

/**
 * @}
 */

#endif /* GLMS_ADAPTER_SIG */
