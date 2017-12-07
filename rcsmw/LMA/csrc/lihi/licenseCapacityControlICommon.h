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
#ifndef LICENSE_CAPACITY_CONTROL_I_COMMON_H
#define LICENSE_CAPACITY_CONTROL_I_COMMON_H

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

#endif /* (defined MAKE_WITH_LPP) || (defined MAKE_OSE_2_LINX_TYPES) */


/*****************************************************************************
 * constantgroup: LcciConstants
 *//**
 * @brief                      Definitions that states the maximum sizes 
 *                             have the following naming convention: 
 *                             MAX_SIZE_OF + data type name + attribute name.
 *                             
 ****************************************************************************/
#define MAX_SIZE_OF_LCCI_LICENSE_DATA_S_LICENSE_KEY_ID                         24
#define MAX_SIZE_OF_LCCI_LICENSE_DATA_S_LICENSE_NAME                           128
#define MAX_SIZE_OF_LCCI_LICENSE_DATA_S_LICENSE_MO_ID                          30
#define MAX_SIZE_OF_LCCI_LICENSE_INFO_S_LICENSE_KEY_ID                         24
#define MAX_SIZE_OF_LCCI_CAPACITY_LICENSE_DISCONNECT_IND_S_ERROR_INFO          128
#define MAX_SIZE_OF_LCCI_KEY_DATA_S_LICENSE_KEY_ID                             24
#define MAX_SIZE_OF_LCCI_CAPACITY_LICENSE_GP_ACTIVATED_FWD_S_LICENSE_KEY_ID    24
#define MAX_SIZE_OF_LCCI_LICENSE_DATA_S_CAPACITY_UNIT                          30
#define MAX_SIZE_OF_LCCI_ERROR_MSG                                             128
#define MAX_SIZE_OF_LCCI_LICENSE_INFO_S_HWAC_STRING                            128


/*****************************************************************************
 * constantgroup: LcciServiceName
 *//**
 * @brief                      The service name for the License Capacity 
 *                             Control Interface (LCCI) service.
 *                             
 ****************************************************************************/
#define LICENSE_CAPACITY_CONTROL_I_SERVICE_NAME    "LICENSE_CAPACITY_CONTROL_I_SERVICE_NAME"


/*****************************************************************************
 * constantgroup: LcciProtocolVersions
 *//**
 * @brief                      Available protocol versions.
 *                             
 ****************************************************************************/
#define LICENSE_CAPACITY_CONTROL_I_VERSION_1    1
#define LICENSE_CAPACITY_CONTROL_I_VERSION_2    2


/*****************************************************************************
 * enum: LcciLicenseStateE
 *//**
 * @brief                      Indicates if the license is included in the 
 *                             License key File.
 *                             
 ****************************************************************************/
typedef int16_t LcciLicenseStateE;
#define LCCI_LICENSE_STATE_E_DISABLED    0
#define LCCI_LICENSE_STATE_E_ENABLED     1


/*****************************************************************************
 * enum: LcciCapacityTypeE
 *//**
 * @brief                      Defines the type for the capacity license, 
 *                             i.e SW, HWAC or both.
 ****************************************************************************/
typedef int16_t LcciCapacityTypeE;
#define LCCI_CAPACITY_TYPE_E_SW        0
#define LCCI_CAPACITY_TYPE_E_HWAC      1
#define LCCI_CAPACITY_TYPE_E_SWHWAC    2


/*****************************************************************************
 * typedef struct: LcciCapacityLimitS
 *//**
 * @brief                      Data used to indicates the current capacity 
 *                             limit, or if the value of noLimit is enforced.
 *                             
 * @param value                The limit for the capacity, invalid in case 
 *                             the noLimit attribute is set.
 *                             
 * @param noLimit              Indicates if the value attribute is valid, or 
 *                             if the limit is set to noLimit.
 *                             
 ****************************************************************************/
typedef struct LcciCapacityLimitS
{
  uint32_t    value; 
  uint16_t    noLimit; 
  uint16_t    padding0; /* Do not care */
} LcciCapacityLimitS;


/*****************************************************************************
 * typedef struct: LcciLicenseDataS
 *//**
 * @brief                      Data for one capacity feature license.
 *                             
 * @param licenseKeyId[]       The unique identifier, currently a CXC 
 *                             number, for the license.
 *                             
 * @param licenseMoId[]        The abbreviated name of the capacity feature, 
 *                             used as the MO instance id.
 *                             
 * @param licenseName[]        The full name of the capacity feature.
 *                             
 * @param capacityUnit[]       The unit of this capacity.
 *                             
 * @param isGracePeriodControlled
 *                             Indicates if Grace Period control is 
 *                             available for this license.
 *                             
 * @param capacityType         The type of this capacity.
 *                             
 *                             
 ****************************************************************************/
typedef struct LcciLicenseDataS
{
  char                 licenseKeyId[MAX_SIZE_OF_LCCI_LICENSE_DATA_S_LICENSE_KEY_ID]; 
  char                 licenseMoId[MAX_SIZE_OF_LCCI_LICENSE_DATA_S_LICENSE_MO_ID]; 
  uint16_t             padding0; /* Do not care */
  char                 licenseName[MAX_SIZE_OF_LCCI_LICENSE_DATA_S_LICENSE_NAME]; 
  char                 capacityUnit[MAX_SIZE_OF_LCCI_LICENSE_DATA_S_CAPACITY_UNIT]; 
  uint16_t             padding1; /* Do not care */
  uint16_t             isGracePeriodControlled; 
  LcciCapacityTypeE    capacityType; 
} LcciLicenseDataS;


/*****************************************************************************
 * typedef struct: LcciLicenseInfoS
 *//**
 * @brief                      Information for one capacity feature license.
 *                             
 * @param licenseKeyId[]       The unique identifier, currently a CXC 
 *                             number, for the license.
 *                             
 * @param capacityLimit        New value for capacity limit to be enforced 
 *                             by the License User.
 *                             
 * @param gracePeriodAvailable Indicates if Grace Period is available for 
 *                             this license.
 *                             
 * @param gracePeriodActivationThreshold
 *                             Shows how much accumulated overshoot (% over 
 *                             24 hours of the current licensed capacity) is 
 *                             allowed before Grace Period is activated.
 *                             
 * @param licenseState         Indicates if the license is included in the 
 *                             License Key File.
 *                             
 *                             
 ****************************************************************************/
typedef struct LcciLicenseInfoS
{
  char                  licenseKeyId[MAX_SIZE_OF_LCCI_LICENSE_INFO_S_LICENSE_KEY_ID]; 
  LcciCapacityLimitS    capacityLimit; 
  uint16_t              gracePeriodAvailable; 
  uint16_t              padding0; /* Do not care */
  uint32_t              gracePeriodActivationThreshold; 
  LcciLicenseStateE     licenseState; 
  uint16_t              padding1; /* Do not care */
} LcciLicenseInfoS;


/*****************************************************************************
 * typedef struct: LcciLicenseInfo2S
 *//**
 * @brief                      Information for one capacity feature license.
 *                             
 * @param licenseKeyId[]       The unique identifier, currently a CXC 
 *                             number, for the license.
 *                             
 * @param capacityLimit        New value for capacity limit to be enforced 
 *                             by the License User.
 *                             
 * @param gracePeriodAvailable Indicates if Grace Period is available for 
 *                             this license.
 *                             
 * @param gracePeriodActivationThreshold
 *                             Shows how much accumulated overshoot (% over 
 *                             24 hours of the current licensed capacity) is 
 *                             allowed before Grace Period is activated.
 *                             
 * @param licenseState         Indicates if the license is included in the 
 *                             License Key File.
 *                             
 * @param hwacString           The HWAC(HardWare Activation Code) information 
 *                             from the LKF.
 *                             
 ****************************************************************************/
typedef struct LcciLicenseInfo2S
{
  char                  licenseKeyId[MAX_SIZE_OF_LCCI_LICENSE_INFO_S_LICENSE_KEY_ID]; 
  LcciCapacityLimitS    capacityLimit; 
  uint16_t              gracePeriodAvailable; 
  uint16_t              padding0; /* Do not care */
  uint32_t              gracePeriodActivationThreshold; 
  LcciLicenseStateE     licenseState; 
  uint16_t              padding1; /* Do not care */
  char                  hwacString[MAX_SIZE_OF_LCCI_LICENSE_INFO_S_HWAC_STRING];
} LcciLicenseInfo2S;

/*****************************************************************************
 * typedef struct: LcciKeyDataS
 *//**
 * @brief                      License key id.
 *                             
 * @param licenseKeyId[]       The unique identifier, currently a CXC 
 *                             number, for the license.
 *                             
 ****************************************************************************/
typedef struct LcciKeyDataS
{
  char    licenseKeyId[MAX_SIZE_OF_LCCI_KEY_DATA_S_LICENSE_KEY_ID]; 
} LcciKeyDataS;


/*****************************************************************************
 * typedef struct: LcciAddressInfoS
 ****************************************************************************/
typedef struct LcciAddressInfoS
{
  int32_t    clientRef; 
  int32_t    serverRef; 
} LcciAddressInfoS;

/******************** Double inclusion protection (end) *********************/
#endif /* LICENSE_CAPACITY_CONTROL_I_COMMON_H */
