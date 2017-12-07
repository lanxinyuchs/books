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

/*********************** Double inclusion protection ************************/
#ifndef LICENSE_FEATURE_CONTROL_I_COMMON_H
#define LICENSE_FEATURE_CONTROL_I_COMMON_H

/******************************** Includes **********************************/
#ifdef MAKE_WITH_LPP
#include "lpp.h"
#endif


/*****************************************************************************
 * constantgroup: LfciConstants
 *//**
 * @brief                      Definitions that states the maximum sizes 
 *                             have the following naming convention: 
 *                             MAX_SIZE_OF + data type name + attribute name.
 *                             
 ****************************************************************************/
#define MAX_SIZE_OF_LFCI_LICENSE_DATA_S_LICENSE_KEY_ID                  24
#define MAX_SIZE_OF_LFCI_LICENSE_DATA_S_LICENSE_NAME                    128
#define MAX_SIZE_OF_LFCI_LICENSE_DATA_S_LICENSE_MO_ID                   30
#define MAX_SIZE_OF_LFCI_LICENSE_INFO_S_LICENSE_KEY_ID                  24
#define MAX_SIZE_OF_LFCI_FEATURE_LICENSE_DISCONNECT_IND_S_ERROR_INFO    128
#define MAX_SIZE_OF_LFCI_KEY_DATA_S_LICENSE_KEY_ID                      24
#define MAX_SIZE_OF_LFCI_ERROR_MSG                                      128


/*****************************************************************************
 * constantgroup: LfciServiceName
 *//**
 * @brief                      The service name for the License Feature 
 *                             Control Interface (LFCI) service.
 *                             
 ****************************************************************************/
#define LICENSE_FEATURE_CONTROL_I_SERVICE_NAME    "LICENSE_FEATURE_CONTROL_I_SERVICE_NAME"


/*****************************************************************************
 * constantgroup: LfciProtocolVersions
 *//**
 * @brief                      Available protocol versions.
 *                             
 ****************************************************************************/
#define LICENSE_FEATURE_CONTROL_I_VERSION_1    1
#define LICENSE_FEATURE_CONTROL_I_VERSION_2    2


/*****************************************************************************
 * enum: LfciFeatureStateE
 *//**
 * @brief                      Indicates if the feature is activated or not.
 *                             
 ****************************************************************************/
typedef int16_t LfciFeatureStateE;
#define LFCI_FEATURE_STATE_E_DEACTIVATED    0
#define LFCI_FEATURE_STATE_E_ACTIVATED      1


/*****************************************************************************
 * enum: LfciLicenseStateE
 *//**
 * @brief                      Indicates if the license is installed in the 
 *                             LKF or not.
 *                             
 ****************************************************************************/
typedef int16_t LfciLicenseStateE;
#define LFCI_LICENSE_STATE_E_DISABLED    0
#define LFCI_LICENSE_STATE_E_ENABLED     1


/*****************************************************************************
 * enum: LfciServiceStateE
 *//**
 * @brief                      Indicates if the feature provides service or 
 *                             not.
 *                             
 ****************************************************************************/
typedef int16_t LfciServiceStateE;
#define LFCI_SERVICE_STATE_E_INOPERABLE    0
#define LFCI_SERVICE_STATE_E_OPERABLE      1


/*****************************************************************************
 * typedef struct: LfciLicenseDataS
 *//**
 * @brief                      Data for one feature license.
 *                             
 * @param licenseKeyId[]       The unique identifier, currently a CXC 
 *                             number, for the license.
 *                             
 * @param licenseMoId[]        The abbreviated name of the feature, used as 
 *                             the MO instance id.
 *                             
 * @param licenseName[]        The full name of the feature.
 *                             
 ****************************************************************************/
typedef struct LfciLicenseDataS
{
  char        licenseKeyId[MAX_SIZE_OF_LFCI_LICENSE_DATA_S_LICENSE_KEY_ID]; 
  char        licenseMoId[MAX_SIZE_OF_LFCI_LICENSE_DATA_S_LICENSE_MO_ID]; 
  uint16_t    padding0; /* Do not care */
  char        licenseName[MAX_SIZE_OF_LFCI_LICENSE_DATA_S_LICENSE_NAME]; 
} LfciLicenseDataS;


/*****************************************************************************
 * typedef struct: LfciLicenseInfoS
 *//**
 * @brief                      Information for one feature license.
 *
 * @param licenseKeyId[]       The unique identifier, currently a CXC
 *                             number, for the license.
 *
 * @param serviceState         Indicates if the feature provides service or
 *                             not.
 *
 * @param licenseState         Indicates if the license is included in the
 *                             License Key File.
 *
 * @param featureState         Indicates if the feature is activated or not.
 *
 ****************************************************************************/
typedef struct LfciLicenseInfoS
{
  char                 licenseKeyId[MAX_SIZE_OF_LFCI_LICENSE_INFO_S_LICENSE_KEY_ID];
  LfciServiceStateE    serviceState;
  LfciLicenseStateE    licenseState;
  LfciFeatureStateE    featureState;
  uint16_t             padding0; /* Do not care */
} LfciLicenseInfoS;


/*****************************************************************************
 * typedef struct: LfciKeyDataS
 *//**
 * @brief                      License key id.
 *                             
 * @param licenseKeyId[]       The unique identifier, currently a CXC 
 *                             number, for the license.
 *                             
 ****************************************************************************/
typedef struct LfciKeyDataS
{
  char       licenseKeyId[MAX_SIZE_OF_LFCI_KEY_DATA_S_LICENSE_KEY_ID]; 
} LfciKeyDataS;


/*****************************************************************************
 * typedef struct: LfciAddressInfoS
 ****************************************************************************/
typedef struct LfciAddressInfoS
{
  int32_t    clientRef; 
  int32_t    serverRef; 
} LfciAddressInfoS;


/*****************************************************************************
 * typedef struct: LfciLicenseInfo2S
 *//**
 * @brief                      Information for one feature license.
 *
 *                             Valid for Protocol revision: 2
 *
 * @param licenseKeyId[]       The unique identifier, currently a CXC
 *                             number, for the license.
 *
 * @param serviceState         Indicates if the feature provides service or
 *                             not.
 *
 * @param licenseState         Indicates if the license is included in the
 *                             License Key File.
 *
 * @param featureState         Indicates if the feature is activated or not.
 *
 * @param alarmCorrelationEventId
 *                             Indicates the eventId for any license alarm,
 *                             will be set to 0 if no alarm is raised.
 *
 ****************************************************************************/
typedef struct LfciLicenseInfo2S
{
  char                 licenseKeyId[MAX_SIZE_OF_LFCI_LICENSE_INFO_S_LICENSE_KEY_ID];
  LfciServiceStateE    serviceState;
  LfciLicenseStateE    licenseState;
  LfciFeatureStateE    featureState;
  uint16_t             padding0; /* Do not care */
  uint32_t             alarmCorrelationEventId;
} LfciLicenseInfo2S;

/******************** Double inclusion protection (end) *********************/
#endif /* LICENSE_FEATURE_CONTROL_I_COMMON_H */
