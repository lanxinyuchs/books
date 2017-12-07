/*
 * 
 * Copyright (c) Ericsson AB  1999-2010 All rights reserved.
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
 * Description:  This file contains types and constants that is used
 *               in the PRI interface.
 *
 *
 * @(#) ClearCase ID: /vobs/cello/osa_src/OSA_CRX90149_1/PRI_CNX9011186/inc/cppext/cello_piu_types.h /main/cppdev/10 11-04-27 15:47 edanlef #
 * 
 *
 * REVISION HISTORY
 * ----------------
 *
 * Revised: ERA/RL/M Tommy Bergek 1999-03-23
 * Change:  First version
 * 
 * Revised: ERA/RL/ME Amir Saadati 1999-07-09
 * Change:  Modified for cello 2.2.
 *
 * Revised: ERA/RL/ME Amir Saadati 1999-11-25
 * Change:  The type Cello_PidInHW changed and 
 *          The type CelloPiuResourceId removed.
 *          The SMCN is replaced by APN.
 *
 * Revised: ERA/RL/KE Radu Munteanu-Ramnic
 * Change:  CELLO_PIU_IFO_SERVICE_NAME "CelloPiuInfoService" is defined here
 *
 * Revised: UAB/DF/DK Radu Munteanu-Ramnic
 * Change:  Implementation of Design Rules for Cello External and Internal 
 *          Interfaces  
 *
 * Revised: UAB/DF/DK Rolf Eriksson
 * Change:  Added process definitions for PV 2.
 * 
 * Revised: UAB/UKH/KM Beatrice Sandvik 2002-07-11
 * Change:  Types added for Protocol Version 3:   CelloSysParValue
 *                                                SysParList
 *          Constants moved to /vobs/cello/osa_src/configcontrol_swb/piuro_swu/
 *                             piuro_ifu/piuro_swi/inc/cs_equipmentro.sig:
 *                             CELLO_PIU_IFO_SERVICE_NAME
 *                             OSA_PRI_PV2_FACTORY_PROC
 *                             OSA_PRI_PV2_FACTORY_PROC_NAME
 *                             OSA_PRI_PV2_SERVICE_PROC
 *                             OSA_PRI_PV2_SERVICE_PROC_NAME
 *                             CELLO_PIU_CLASS_NAME
 *                             CELLO_PIU2_CLASS_NAME
 *
 * Revised: UAB/UKH/KM Rolf Eriksson
 * Change:  CELLO_PIU_IFO_SERVICE_NAME, CELLO_PIU_CLASS_NAME and
 *          CELLO_PIU2_CLASS_NAME moved to cello_piu_types.h.
 * 
 * Revised: UAB/UKH/KM Svante Salen 2003-12-01
 * Change:  Added definitions for PV4
 *
 * Revised: EAB/UKH/KM Daniel Lefwerth 2004-03-19
 * Change:  Removed unused error codes for CelloPiuRoResult.
 *
 * Revised: EAB/UKH/KM Hama Biglari 2005-11-12
 * Change:  Added new values for PRI PV6
 *
 * Revised: EAB/PKH/KM Hama Biglari 2006-01-29
 * Change:  Moved CelloOperationalInfo from cello_mo-types.h 
 *          into this file (used from PRI PV6 and onwards) 
 *
 * Revised: EAB/PKH/KM Bengt Andreasson 2006-01-31
 * Change:  Removed macros and types that are moved to 
 *          CONTROL-CPPEXT_CNX9010099/.../cello_control_commontypes.h
 *
 * Revised: EAB/PKH/KM Bengt Andreasson 2006-04-21
 * Change:  Includes cello_control_commontypes.h to be backwards compatible
 *
 * Revised: EAB/PKH/KM Hama Biglari 2006-05-22
 * Change:  First version for PRI PV5: New values in CelloPiuRoResult for
 *          subscription to overtemp of board, and also the new values
 *          CELLO_PIU_BOARD_NORMAL_TEMP and CELLO_PIU_BOARD_OVERTEMP.
 *
 * Revised: EAB/UKH/KM Peter Hedman 2007-11-30
 * Change:  Added new values for PRI PV7
 *
 * Revised: EAB/FTP/HX Staffan Törnros 2008-05-07
 * Change:  PRI PV8 PIU Devices.
 *
 * Revised: Srinivasa Reddy Yeduru
 * Change:  PRI PV9 is added.
 *         
 * Revised: Krishna Chaitanya Kolluri 2010-05-19
 * Change:  PRI PV10 is added (WRNae83722).
 *
 * Revised: Daniel Lefwerth 2010-10-06
 * Change:  Added CELLO_PRI_ETHERNET_1G_10G transmission type (EGEM2).
 */

#ifndef CELLO_PIU_TYPES_H
#define CELLO_PIU_TYPES_H

/*
 ******************************************************************************
 * INCLUDE FILES
 ******************************************************************************
 */

#include "osetypes.h"

#ifdef __cplusplus
extern "C" {
#endif

#include "cello_control_commontypes.h"

/*
 ******************************************************************************
 * MACROS
 ******************************************************************************
 */

/* Definition for Protocol Versions */
#define CELLO_PRI_PV0     0
#define CELLO_PRI_PV1     1
#define CELLO_PRI_PV2     2
#define CELLO_PRI_PV3     3
#define CELLO_PRI_PV4     4
#define CELLO_PRI_PV5     5
#define CELLO_PRI_PV6     6
#define CELLO_PRI_PV7     7
#define CELLO_PRI_PV8     8
#define CELLO_PRI_PV9     9    
#define CELLO_PRI_PV10    10 
#define CELLO_PRI_PV11    11
    

/* PIU protocol version 1 class name.
 * To be used when defining the class name for the PIU Object ID
 * used at State Propagation service.
 * Also used by the PIU MAO to find the PIU FRO in the name server.
 * ------------------------------------------------------------*/
#define CELLO_PIU_CLASS_NAME           "CelloPlugInUnit"

/* PIU protocol version class names (except protocol version 1).
 * To be used when defining the class name for the PIU Object ID
 * used at State Propagation service.
 * ------------------------------------------------------------*/
#define CELLO_PIU2_CLASS_NAME          "CelloPlugInUnitPv2"
#define CELLO_PIU3_CLASS_NAME          "CelloPlugInUnitPv3"
#define CELLO_PIU4_CLASS_NAME          "CelloPlugInUnitPv4"
#define CELLO_PIU5_CLASS_NAME          "CelloPlugInUnitPv5"
#define CELLO_PIU6_CLASS_NAME          "CelloPlugInUnitPv6"

/* Maximum size of hunt path returned in signal CelloPiuGetHuntPathCfm */
#define CELLO_PIU_HUNT_PATH_SIZE (80)

/* Subrack related definitions */
#define CELLO_PRI_OWN_SMN     0xffff

/*
 ******************************************************************************
 * TYPES
 ******************************************************************************
 */

/* This type gives additional information of the operational state
 * and is sent to subscribers of operational state changes.
 */
typedef U16 CelloOperationalInfo;
#define CELLO_PIU_OPERATIONAL                0
#define CELLO_PIU_UNOPERATIONAL              1
#define CELLO_PIU_HWTEST_UNOPERATIONAL       2
#define CELLO_PIU_FAILED                     3
#define CELLO_PIU_ALARM                      4

/* This type specifies function return values when using the PRI interface */
typedef U32 CelloPriResult;
#define CELLO_PRI_OK                         1
#define CELLO_PRI_MEMORY_NOT_INITIATED       2
#define CELLO_PRI_SERVICE_NOT_INITIATED      3
#define CELLO_PRI_ERROR_SERVER_NOT_AVAILABLE 4
#define CELLO_PRI_UNKNOWN_PV                 5
#define CELLO_PRI_UNKNOWN_SIGNAL             6
#define CELLO_PRI_WRONG_HW_ADDRESS           7
#define CELLO_PRI_FAILURE_TO_GET_OWN_SMN     8

/* This type specifies signal return values when using the PRI interface
 *
 * CELLO_PIU_RO_NO_SUCH_SERVICE: no such service exists on the PIU
 * CELLO_PIU_RO_SERVICE_NOT_FOUND: the service is temporary unavailable
 */
typedef U32 CelloPiuRoResult;
#define CELLO_PIU_RO_OK                          1
#define CELLO_PIU_RO_WRONG_HW_ADDRESS            2
#define CELLO_PIU_RO_ERROR_PIU_NOT_FOUND         3
#define CELLO_PIU_RO_ERROR_IN_OSPI               4
#define CELLO_PIU_RO_RESTART_NOT_ALLOWED         5
#define CELLO_PIU_RO_NO_SUCH_SERVICE             6
#define CELLO_PIU_RO_SERVICE_NOT_FOUND           7
#define CELLO_PIU_RO_PROPERTIES_NOT_FOUND        8
#define CELLO_PIU_RO_TEMP_FUNC_NOT_SUPPORTED     9
#define CELLO_PIU_RO_TEMP_CLIENT_NOT_SUBSCRIBED  10
#define CELLO_PIU_RO_TEMP_ALREADY_SUBSCRIBED     11
#define CELLO_PIU_RO_TEMP_UNIT_NOT_FOUND         12
#define CELLO_PIU_RO_ERROR_SMN_NOT_FOUND         13
#define CELLO_PIU_RO_HW_PID_NOT_FETCHED          14
#define CELLO_PIU_RO_ERROR_PROGRAM_NOT_FOUND     15

/* These values are used in PV5 notification signal
 * for board overtemp.
 */
typedef U32 CelloPiuOvertempIndication;
#define CELLO_PIU_BOARD_NORMAL_TEMP    0
#define CELLO_PIU_BOARD_OVERTEMP       1

/*
 *  Maximally CELLO_SYSPAR_MAX_NO system parameter names that
 *  may be requested in one CelloPri_getSystemParameter() function call.
 */
#define CELLO_SYSPAR_MAX_NO              (64)

/*
 * The value of a system parameter that may be undefined, e.g. not 
 * defined by user. The CELLO_SYSPAR_NOT_DEFINED value is sent back 
 * to the client in the CelloPriGetSysParameterCfm signal.
 */
typedef U32 CelloSysParValid;
#define CELLO_SYSPAR_NOT_VALID          (0) 
#define CELLO_SYSPAR_VALID              (1) 

/*
 * This type specifies the data structure of the System Parameter List.
 */
typedef struct
{
  U32                sysParRef;
  CelloSysParValid   valid;
  U32                value;
} CelloSysPar;


/* Transmission types */
typedef U32 CelloPriTransmissionType;
#define CELLO_PRI_AMAX_ONLY        1
#define CELLO_PRI_ETHERNET10G      2
#define CELLO_PRI_ETHERNET_CABLE   3
#define CELLO_PRI_ETHERNET_1G_10G  4


#ifdef __cplusplus
}
#endif

#endif /* CELLO_PIU_TYPES_H */
