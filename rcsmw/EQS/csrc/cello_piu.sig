/*
 *
 * Copyright (c) Ericsson AB  1999-2013 All rights reserved.
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
 * Description:  This file contains the response signal interface from
 *               PIU server to the user (client).
 *               This signal should only be considered if the user (client)
 *               has chosen to receive them asynchronously (see the .h file
 *               for this interface).
 *
 *
 * @(#) ClearCase ID: /vobs/cello/osa_src/OSA_CRX90149_1/PRI_CNX9011186/inc/cppext/cello_piu.sig /main/cppdev/13 11-08-17 10:36 eprzrze #
 *
 *
 * REVISION HISTORY
 * ----------------
 *
 * Revised: ERA/RL/ME Amir Saadati 1999-08-16
 * Change:  First version
 *
 * Revised: ERA/RL/KE Radu Munteanu-Ramnic 2000-11-14
 * Change:  Signals with wrong comments are corrected, WRNaa19817.
 *
 * Revised: UAB/J/KO Staffan Törnros 2001-11-01
 * Change:  Add CelloPriRestartPiuCfm & CelloPriRestartPiuRej
 *
 * Revised: UAB/DF/DK Radu Munteanu-Ramnic 2002-02-09
 * Change:  WRNaa77408 correction: CelloPiuGetLHNameCfm changed
 *
 * Revised: UAB/DF/DK Radu Munteanu-Ramnic 2002-02-20
 * Change:  Changes according to Design Rules for Cello External and Internal
 *          Interfaces: new signals defined
 *
 * Revised: UAB/UKH/KM Beatrice Sandvik 2002-07-02
 * Change:  Signals added: CelloPriGetSysParameterCfm
 *                         CelloPriGetSysParameterRej
 *
 * Revised: UAB/UKH/FL Habib Malekdanesh 2002-07-09
 * Change:  Signals added: CELLO_PIU3_SUBSCRIBE_ADM_STATE_CFM
 *                         CELLO_PIU3_SUBSCRIBE_OPER_STATE_CFM
 *                         CelloPiu3ChangeOperStateInd
 *
 * Revised: UAB/UKH/KM Beatrice Sandvik 2002-07-09
 * Change:  Protocol Version 3 introduced
 *          Signal added:    CelloPiu3GetLHNameCfm
 *          Signals removed: CelloPriInitiateServiceSus
 *                           CelloPriServerUnpublishInd
 *          Signals moved to /vobs/cello/osa_src/configcontrol_swb/piuro_swu/
 *                           piuro_ifu/piuro_swi/inc/cs_equipmentro.sig:
 *                           OsaPriInitiateServiceReq
 *                           OsaPriTerminateServiceReq
 *
 * Revised: UAB/UKH/KM Beatrice Sandvik 2002-08-27
 * Change:  Signals added:
 *
 * Revised: UAB/UKH/KM Rolf Eriksson 2003-01-15
 * Change:  UABtr14703: corrected CELLO_PIU2_GET_LH_NAME_CFM  signal number
 *          to (0x105D2).
 *
 * Revised: UAB/UKH/KM Rolf Eriksson 2003-02-24
 * Change:  WRNab51402: removed CELLO_PIU3_GET_LH_NAME_CFM.
 *
 * Revised: UAB/UKH/KM Daniel Lefwerth 2003-04-23
 * Change:  Added client references to PV3.
 *
 * Revised: UAB/UKH/KM Svante Salen 2003-12-05
 * Change:  New signal CELLO_PIU4_GET_BOARD_TEMPERATURE_RSP.
 *
 * Revised: EAB/UKH/KM Daniel Lefwerth 2004-03-19
 * Change:  New signal CELLO_PIU4_GET_BOARD_THERMAL_PROP_RSP.
 *
 * Revised: EAB/PKH/KM Bengt Andreasson 2006-02-13
 * Change:  Included cello_control_commontypes.h instead of
 *          cello_mo_types.h
 *
 * Revised: EAB/PKH/KM Hama Biglari 2006-04-11
 * Change:  New signals for DBM3 (PRI PV5):
 *                     CELLO_PIU5_GET_BOARD_TEMP_FINE_RSP
 *                     CELLO_PIU5_SUBSCRIBE_OVERTEMP_CFM
 *                     CELLO_PIU5_OVERTEMP_IND
 *
 * Revised: EAB/FTP/HKM Staffan Törnros 2007-04-23
 * Change:  System parameters with string values (WP83).
 *
 * Revised: EAB/FTP/HKM Peter Hedman 2007-11-30
 * Change:  New signals
 *                     CELLO_PRI7_GET_SLOT_POSITIONS_CFM
 *                     CELLO_PRI7_GET_TRANSMISSION_TYPE_CFM
 *                     CELLO_PRI7_GET_MAX_POWER_DISSIPATION_CFM
 *
 * Revised: EAB/UKH/P Srilakshmi 2008-02-08
 * Change:  Included the missing "}" symbol at the end of the file.
 *          (UABtr58400).
 *
 * Revised: EAB/FTP/HX Staffan Törnros 2008-05-07
 * Change:  PRI PV8 PIU Devices.
 *
 * Revised: Srinivasa Reddy 2009-04-03
 * Change:  PRI PV9 Signals added.
 *
 * Revised: Srinivasa Reddy 2009-11-04
 * Change:  Correction UABtr74991.
 *
 * Revised: Krishna Chaitanya Kolluri 2010-05-19
 * Change:  Added new function for PRI PV10 (WRNae83722).
 *
 * Revised: Daniel Lefwerth 2011-01-21
 * Change:  Added new function for PRI PV11 (HN50921).
 *
 * Revised: Per Norberg 2013-05-16
 * Change:  Removed all signals that is not implemented in
 *          RBS Control System
 *
 */

#ifndef CELLO_PIU_SIG
#define CELLO_PIU_SIG

/*
 ******************************************************************************
 * INCLUDE FILES
 ******************************************************************************
 */

#include "cello_te_ose.h"
#include "osetypes.h"
#include "cello_piu_types.h"

#ifdef __cplusplus
extern "C" {
#endif

/*
 ******************************************************************************
 * SIGNALS
 ******************************************************************************
 */

/************* PUBLIC SIGNALS RETURNED FROM INTERFACE FUNCTIONS **************/

/*
 * Signals valid for PRI protocol version 1
 * ----------------------------------------
 */

/*
 * Signals valid for PRI protocol version 2
 * ----------------------------------------
 */

/******************************************************************************
 *
 * Signal : CelloPriInitiateServiceCfm
 *
 * Descr  : Signal to client to confirm the initiation of the service
 *
 * Data   :
 *
 *****************************************************************************/
#define CELLO_PRI_INITIATE_SERVICE_CFM (0x105C9) /*!- SIGNO(CelloPriInitiateServiceCfm) -!*/

typedef struct
{
   SIGSELECT   sigNo;
   U32         signalRevision;
   U32         selectedPV;
} CelloPriInitiateServiceCfm;


/******************************************************************************
 *
 * Signal : CelloPriInitiateServiceSus
 *
 * Descr  : Signal to client to suspend the usage of the service
 *
 * Data   :
 *
 *****************************************************************************/
#define CELLO_PRI_INITIATE_SERVICE_SUS (0x105CA) /*!- SIGNO(CelloPriInitiateServiceSus) -!*/

typedef struct
{
   SIGSELECT    sigNo;
   U32          signalRevision;
   U32          highestPV;
} CelloPriInitiateServiceSus;


/******************************************************************************
 *
 * Signal : CelloPriInitiateServiceRej
 *
 * Descr  : Signal to client indicating total reject of the service
 *
 * Data   :
 *
 *****************************************************************************/
#define CELLO_PRI_INITIATE_SERVICE_REJ (0x105CB) /*!- SIGNO(CelloPriInitiateServiceRej) -!*/

typedef struct
{
   SIGSELECT   sigNo;
   U32         signalRevision;
   U32         highestPV;
} CelloPriInitiateServiceRej;


/******************************************************************************
 *
 * Signal : CelloPriServerUpInd
 *
 * Descr  : Signal to client indicating the server is up
 *
 * Data   :
 *
 *****************************************************************************/
#define CELLO_PRI_SERVER_UP_IND (0x105CC) /*!- SIGNO(CelloPriServerUpInd) -!*/

typedef struct
{
   SIGSELECT        sigNo;
} CelloPriServerUpInd;


/******************************************************************************
 *
 * Signal : CelloPriServerDownInd
 *
 * Descr  : Signal to client indicating the server is down
 *
 * Data   :
 *
 *****************************************************************************/
#define CELLO_PRI_SERVER_DOWN_IND (0x105CD) /*!- SIGNO(CelloPriServerDownInd) -!*/

typedef struct
{
   SIGSELECT        sigNo;
} CelloPriServerDownInd;


/******************************************************************************
 *
 * Signal : CelloPriServerUnpublishInd
 *
 * Descr  : Signal to client indicating the server is unpublished
 *
 * Data   :
 *
 *****************************************************************************/
#define CELLO_PRI_SERVER_UNPUBLISH_IND (0x105CE) /*!- SIGNO(CelloPriServerUnpublishInd) -!*/

typedef struct
{
   SIGSELECT        sigNo;
} CelloPriServerUnpublishInd;


/******************************************************************************
 *
 * Signal : CelloPriTerminateServiceCfm
 *
 * Descr  : Signal to client to confirm the termination of the service
 *
 * Data   :
 *
 *****************************************************************************/
#define CELLO_PRI_TERMINATE_SERVICE_CFM (0x105D0) /*!- SIGNO(CelloPriTerminateServiceCfm) -!*/

typedef struct
{
   SIGSELECT        sigNo;
} CelloPriTerminateServiceCfm;

/*
 * Signals valid for PRI protocol version 3
 * ----------------------------------------
 */

/******************************************************************************
 *
 * Signal : CelloPiu3GetHuntPathCfm
 *
 * Descr. : Delivers the hunt path of a process, which the client has
 *          supplied in the call to the interface function.
 *
 * Data   : huntPath - contains the hunt path of the process.
 *          result - informs the client if the operation succeeded or not.
 *          clientId - client reference
 *
 *****************************************************************************/
#define CELLO_PIU3_GET_HUNT_PATH_CFM (0x10738) /*!- SIGNO(CelloPiu3GetHuntPathCfm) -!*/

typedef struct
{
   SIGSELECT        sigNo;
   char             huntPath[CELLO_PIU_HUNT_PATH_SIZE];
   CelloPiuRoResult result;
   U32              clientId;
} CelloPiu3GetHuntPathCfm;


/******************************************************************************
 *
 * Signal : CelloPiu3GetLHNameCfm
 *
 * Descr. : Delivers the link handler name of a processor, of which id
 *          the client has supplied in the call to the interface function.
 *
 * Data   : result - informs the client if the operation succeeded or not.
 *          linkHandlerName - contains the link handler name of the processor.
 *          clientId - client reference
 *
 *****************************************************************************/
#define CELLO_PIU3_GET_LH_NAME_CFM (0x1069B) /*!- SIGNO(CelloPiu3GetLHNameCfm) -!*/

typedef struct
{
   SIGSELECT        sigNo;
   CelloPiuRoResult result;
   char             linkHandlerName[CELLO_PIU_LH_NAME_SIZE];
   U32              clientId;
} CelloPiu3GetLHNameCfm;


/******************************************************************************
 *
 * Signal : CelloPiu3GetOwnIdCfm
 *
 * Descr  :
 *
 * Data   : piuInstanceId  Contains the PIU instance id that was requested for
 *          result         Informs the client if the operation succeeded or not
 *
 *****************************************************************************/
#define CELLO_PIU3_GET_OWN_ID_CFM (0x1073C) /*!- SIGNO(CelloPiu3GetOwnIdCfm) -!*/

typedef struct
{
   SIGSELECT        sigNo;
   U32              piuInstanceId;
   CelloPiuRoResult result;
} CelloPiu3GetOwnIdCfm;

/*
 * Signals valid for PRI protocol version 4
 * ----------------------------------------
 */

/******************************************************************************
 *
 * Signal : CelloPiu4RestartPiuCfm
 *
 * Descr  : This signal indicates that the requested PIU restart has been/will
 *          be performed.
 *
 * Data   : piuInstanceId  Object identity of the restarted PIU.
 *          clientId       Client reference
 *
 *****************************************************************************/
#define CELLO_PIU4_RESTART_PIU_CFM (0x1077C) /*!- SIGNO(CelloPiu4RestartPiuCfm) -!*/

typedef struct CelloPiu4RestartPiuCfm
{
   SIGSELECT        sigNo;
   U32              piuInstanceId;
   U32              clientId;
} CelloPiu4RestartPiuCfm;


/******************************************************************************
 *
 * Signal : CelloPiu4RestartPiuRej
 *
 * Descr  : This signal indicates that the requested PIU restart will not
 *          be performed.
 *
 * Data   : result          Reason for reject
 *          piuInstanceId   Object identity of the not restarted PIU.
 *          clientId        Client reference
 *
 *****************************************************************************/
#define CELLO_PIU4_RESTART_PIU_REJ (0x1077D) /*!- SIGNO(CelloPiu4RestartPiuRej) -!*/

typedef struct CelloPiu4RestartPiuRej
{
   SIGSELECT        sigNo;
   CelloPiuRoResult result;
   U32              piuInstanceId;
   U32              clientId;
} CelloPiu4RestartPiuRej;

/*
 * Signals valid for PRI protocol version 5
 * ----------------------------------------
 */

/*
 * Signals valid for PRI protocol version 6
 * ----------------------------------------
 */

/*
 * Signals valid for PRI protocol version 7
 * ----------------------------------------
 */

/*
 * Signals valid for PRI protocol version 8
 * ----------------------------------------
 */

/******************************************************************************
 *
 * Signal Name:  CelloPri8GetIdentityCfm
 *
 * Descr      :  Response to CelloPri8_getOwnIdentity() or
 *               CelloPri8_getPiuOrDeviceIdentity().
 *
 * Data       :  piuInstanceId    PIU instance identity.
 *               piuOrDeviceId    Device instance identity if relevant,
 *                                else PIU instance identity.
 *               smn              Switch Module Number (subrack).
 *               slotNumber       Slot number (1..max).
 *               apn              ASCC Port Number.
 *               deviceId         Device index within PIU, 0 if not a device.
 *               huntPathPrefix   ["/linkhandler"]["/instance"].
 *               result           Result of operation (see cello_piu_types.h).
 *               clientId         Client reference (from call).
 *
 *****************************************************************************/
#define CELLO_PRI8_GET_IDENTITY_CFM (0x10A5F) /*!- SIGNO(CelloPri8GetIdentityCfm) -!*/

typedef struct
{
   SIGSELECT          sigNo;
   U32                piuInstanceId;
   U32                piuOrDeviceId;
   U32                smn;
   U32                slotNumber;
   U32                apn;
   U32                deviceId;
   char               huntPathPrefix[CELLO_PIU_HUNT_PATH_SIZE];
   CelloPiuRoResult   result;
   U32                clientId;
} CelloPri8GetIdentityCfm;

/*
* Signals for PRI Protocol Version 9
*
*/

/******************************************************************************
 *
 * Signal Name:  CelloPri9GetIdentityCfm
 *
 * Descr      :  Response to CelloPri9_getOwnIdentity() or
 *               CelloPri9_getPiuorDeviceIdentity().
 *
 * Data       :  piuInstanceId        Contains the PIU object instanceId
 *               deviceInstanceId     Device Instance identity
 *               subrackNumber
 *               smn
 *               slotNumber
 *               apn
 *               deviceId
 *               huntPathPrefix
 *               result            Result of operation (see cello_piu_types.h).
 *               clientId          Client reference (from call).
 *
 *****************************************************************************/
#define CELLO_PRI9_GET_IDENTITY_CFM (0x10B0A) /*!- SIGNO(CelloPri9GetIdentityCfm) -!*/

typedef struct
{
   SIGSELECT          sigNo;
   U32                piuInstanceId;
   U32                piuOrDeviceId;
   U32                smn;
   U32                slotNumber;
   U32                apn;
   U32                deviceId;
   U32                subrackNumber;
   char               huntPathPrefix[CELLO_PIU_HUNT_PATH_SIZE];
   CelloPiuRoResult   result;
   U32                clientId;
} CelloPri9GetIdentityCfm;

/******************************************************************************
 *
 * Signal : CelloPiu10OperationalPidCfm
 *
 * Descr. : Response to CelloPiu10_getPid().
 *
 * Data   : piuInstanceId  The PIU instance id that was requested for
 *          pidInHw_r      The operational PID of the requested PIU
 *          result         Result of operation (see cello_piu_types.h)
 *          clientId       Client reference
 *
 *****************************************************************************/
#define CELLO_PIU10_OPERATIONAL_PID_CFM (0x10B63) /*!- SIGNO(CelloPiu10OperationalPidCfm) -!*/

typedef struct
{
   SIGSELECT        sigNo;
   U32              piuInstanceId;
   Cello_PidInHW    pidInHw_r;
   CelloPiuRoResult result;
   U32              clientId;
} CelloPiu10OperationalPidCfm;

#ifdef __cplusplus
}
#endif

#endif /* CELLO_PIU_SIG */
