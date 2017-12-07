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
 * Description: This file contains the interfaces which are provided by PRI.
 *
 *
 * @(#) ClearCase ID: /vobs/cello/osa_src/OSA_CRX90149_1/PRI_CNX9011186/inc/cppext/cello_piu.h /main/cppdev/12 11-02-01 19:22 eprzrze #
 *
 *
 * REVISION HISTORY
 * ----------------
 *
 * Revised: ERA/RL/ME Amir Saadati 1999-08-16
 * Change:  First version
 *
 * Revised: UAB/J/KO Staffan Törnros 2001-11-01
 * Change:  Add CelloPri_restartOwnPiu() & CelloPri_restartOtherPiu()
 *
 * Revised: UAB/DF/DK Radu Munteanu-Ramnic 2002-02-08
 * Change:  Implementation of Design Rules for Cello External and Internal
 *          Interfaces, new functions:
 *               CelloPri_initiateMemory
 *               CelloPri_initiateService
 *               CelloPri_internal
 *               CelloPri_terminateService
 *          Only asynchronous behaviour is supported in Protocol Version 2
 *
 * Revised: EAB/UDF/DK Kersti Östman 2002-04-15
 * Change:  Possibility to demand warm, refresh and cold restart for
 *          piu and node added.
 *
 * Revised: UAB/UKH/KM Beatrice Sandvik 2002-07-11
 * Change:  Protocol Version 3 introduced.
 *          Return type Boolean changed to CelloPriResult
 *          New interface: CelloPri_getSystemParameter
 *
 * Revised: UAB/UKH/KM Beatrice Sandvik 2002-08-27
 * Change:  CelloPri3_terminateService() removed
 *
 * Revised: UAB/UKH/KM Daniel Lefwerth 2003-04-23
 * Change:  Added client references to PV3.
 *
 * Revised: UAB/UKH/KM Svante Salen 2003-12-08
 * Change:  Additions for BTI: getBoardTemperature and new restart rank.
 *
 * Revised: EAB/UKH/KM Daniel Lefwerth 2004-03-19
 * Change:  Changed getBoardTemperature, added getBoardThermalProp.
 *
 * Revised: EAB/UKH/KM Anders Eriksson 2004-12-08
 * Change:  - UABtr25891 'Segmentation fault in PriProxy'.
 *          A new function CelloPri_freeMemory() is added.
 *
 * Revised: EAB/PKH/KM Bengt Andreasson 2006-02-13
 * Change:  Included cello_control_commontypes.h instead of
 *          cello_mo_types.h
 *
 * Revised: EAB/PKH/KM Hama Biglari 2006-04-12
 * Change:  Added PV5 functions: getBoardTempFine, subscribeBoardOvertemp
 *          and unsubscribeBoardOvertemp.
 *
 * Revised: EAB/PKH/KM Hama Biglari 2006-04-27
 * Change:  Added CelloPiu6_subscribe() and CelloPiu6_confirmNotification()
 *
 * Revised: EAB/PKH/KM Peter Hedman 2007-12-10
 * Change:  Added CelloPri7_getSlotPositions() and
 *                CelloPri7_getTransmissionType() and
 *                CelloPri7_getMaxPowerDissipation()
 *
 * Revised: EAB/FTP/HX Staffan Törnros 2008-05-07
 * Change:  PRI PV8 PIU Devices.
 *
 * Revised: Srinivasa Reddy 2009-04-03
 * Change:  added new functions for PRI PV9.
 *
 * Revised: Krishna Chaitanya Kolluri 2010-05-19
 * Change:  Added new function for PRI PV10 (WRNae83722).
 *
 * Revised: Daniel Lefwerth 2011-01-21
 * Change:  Added new function for PRI PV11 (HN50921).
 *
 * Revised: Per Norberg 2013-05-16
 * Change:  Removed all definitions and macros that is not implemented in
 *          RBS Control System
 */

#ifndef CELLO_PIU_H
#define CELLO_PIU_H

/*
******************************************************************************
* INCLUDE FILES
******************************************************************************
*/

#include "cello_piu_types.h"

#ifdef __cplusplus
extern "C" {
#endif

  /*
******************************************************************************
* MACROS
******************************************************************************
*/

#define PRI_SIGNAL_REVISION 0


  /* Restart rank values used in restartOwnPiu() and restartOtherPiu(). */
#define CELLO_PRI_RANK_DEFAULT            0
#define CELLO_PRI_RESTART_WARM            0
#define CELLO_PRI_RESTART_REFRESH         1
#define CELLO_PRI_RESTART_COLD            2
#define CELLO_PRI_RESTART_COLD_WITH_TEST  3

#define CELLO_PIU_RESTART_WARM           CELLO_PRI_RESTART_WARM
#define CELLO_PIU_RESTART_REFRESH        CELLO_PRI_RESTART_REFRESH
#define CELLO_PIU_RESTART_COLD           CELLO_PRI_RESTART_COLD
#define CELLO_PIU_RESTART_COLD_WITH_TEST CELLO_PRI_RESTART_COLD_WITH_TEST

  /*
******************************************************************************
* TYPES
******************************************************************************
*/


  /*
******************************************************************************
* FUNCTION PROTOTYPES
******************************************************************************
*/

  /*
   * Prototypes valid for PRI protocol version 1
   * -------------------------------------------
   */



  /*
   * Prototypes valid for PRI protocol version 2
   * -------------------------------------------
   */

  /*****************************************************************************
   *
   *  Name  : CelloPri_initiateMemory
   *
   *  Descr.: The client initiate the proxy,
   *          reservation of proxy internal memory
   *
   *  Args  : -
   *
   *  Return: A pointer to client's memory area
   *
   ****************************************************************************/
  void*
  CelloPri_initiateMemory(void);

  /*****************************************************************************
   *
   *  Name  : CelloPri_freeMemory
   *
   *  Descr.: The client requests to free all resources allocated by the proxy.
   *
   *  Args  : _
   *
   *  Return: -
   *
   ****************************************************************************/
  void
  CelloPri_freeMemory(void **priMemory_p);

  /*****************************************************************************
   *
   *  Name  : CelloPri_initiateService
   *
   *  Descr.: Initiate the service and negotiate protocol version
   *
   *  Args  : priMemory_p       A pointer to client's memory area
   *
   *          pvFirstWanted     protocol version first wanted
   *
   *          pvSecondWanted    protocol version second wanted
   *
   *          pvThirdWanted     protocol version third wanted
   *
   * Return:  CelloPriResult    See cello_piu_types.h
   *
   ****************************************************************************/
  CelloPriResult
  CelloPri_initiateService(void *priMemory_p,
			   U32   pvFirstWanted,
			   U32   pvSecondWanted,
			   U32   pvThirdWanted);

  /*****************************************************************************
   *
   *  Name  : CelloPri_internal
   *
   *  Descr.: The client shall forward all received interface management
   *          signals to the proxy.
   *
   *  Args  : priMemory_p       A pointer to client's memory area
   *
   *          signal            The signal to be forwarded to the proxy.
   *
   *  Return: CelloPriResult    See cello_piu_types.h
   *
   ****************************************************************************/
  CelloPriResult
  CelloPri_internal(void          *priMemory_p,
		    union SIGNAL  *signal);

  /*****************************************************************************
   *
   *  Name  : CelloPri_terminateService
   *
   *  Descr.: The client wants to terminate the service.
   *
   *  Args  : priMemory_p       A pointer to client's memory area
   *
   *          optionalData      Optional data
   *
   * Return:  CelloPriResult    See cello_piu_types.h
   *
   ****************************************************************************/
  CelloPriResult
  CelloPri_terminateService(void  *priMemory_p,
			    void  *optionalData);

  /*
   * Prototypes valid for PRI protocol version 3
   * -------------------------------------------
   */

  /*****************************************************************************
   *
   *  Name  : CelloPiu3_getHuntPath
   *
   *  Descr.: Request to retrieve a hunt path for a certain process,
   *          of which PIU instance id where the process resides, is known
   *          to the client.
   *          The client will receive the CELLO_PIU3_GET_HUNT_PATH_CFM signal,
   *          with the requested info.
   *
   *  Args  : priMemory_p       A pointer to client's memory area.
   *
   *          piuInstanceId     The id of the PIU object instance, where
   *                            the process resides.
   *
   *          processName_p     The name of the process.
   *
   *          clientId          Client reference, returned in response signal
   *
   * Return:  CelloPriResult    See cello_piu_types.h
   *
   ****************************************************************************/
  CelloPriResult
  CelloPiu3_getHuntPath(void           *priMemory_p,
			U32             piuInstanceId,
			char           *processName_p,
			U32             clientId);

  /*****************************************************************************
   *
   *  Name  : CelloPiu3_getLinkHandlerName
   *
   *  Descr.: Request to retrieve the link handler name for a certain
   *          processor, of which board (i.e. subrack an slot) is known
   *          to the client. The client will receive the
   *          CELLO_PIU3_GET_LH_NAME_CFM signal, with the requested info.
   *
   *  Args  : priMemory_p        A pointer to client's memory area
   *
   *          piuInstanceId      The id of the PIU object instance, of which
   *                             link handler name should be retrieved.
   *
   *          clientId           Client reference, returned in response signal
   *
   * Return:  CelloPriResult     See cello_piu_types.h
   *
   ****************************************************************************/
  CelloPriResult
  CelloPiu3_getLinkHandlerName(void   *priMemory_p,
			       U32     piuInstanceId,
			       U32     clientId);

  /*****************************************************************************
   *
   *  Name  : CelloPiu3_getOwnIdentity
   *
   *  Descr.: Request to retrieve the PIU instance id of the PIU where the
   *          client process resides.
   *          The client will receive the response signal,
   *          with the requested PIU instance id from server.
   *
   *  Args  : priMemory_p       A pointer to client's memory area
   *
   *  Return: CelloPriResult    See cello_piu_types.h
   *
   ****************************************************************************/
  CelloPriResult
  CelloPiu3_getOwnIdentity(void *priMemory_p);

  /*
   * Prototypes valid for PRI protocol version 4
   * -------------------------------------------
   */

  /*****************************************************************************
   *
   *  Name  : CelloPiu4_restartOwnPiu
   *
   *  Descr.: Request to restart own PIU. Only allowed for BPs.
   *          This command is asynchronous. Return is made to caller also when
   *          the command is accepted, restart will happen later.
   *          If command is rejected, no further action is taken.
   *          If command is accepted, the restart operation may still fail.
   *          In this case, a reject signal is received.
   *          If restart operation is successful, no confirm is received. Note
   *          that it may take a short while before the restart is performed.
   *
   *  Args  : priMemory_p       A pointer to client's memory area
   *
   *          restartRank       Type of restart. See definitions above.
   *
   *          restartEscalation True = escalate (same as spontaneous restart).
   *                            False = no escalate (same as MO restart command).
   *
   *          restartCause      Text string giving cause for restart.
   *                            Max 60 characters. Used for logging.
   *
   *          clientId          Client reference, returned in response signal
   *
   *  Return: CelloPriResult    See cello_piu_types.h
   *
   ****************************************************************************/
  CelloPriResult
  CelloPiu4_restartOwnPiu(void           *priMemory_p,
			  U32             restartRank,
			  Boolean         restartEscalation,
			  char           *restartCause,
			  U32             clientId);

  /*****************************************************************************
   *
   *  Name  : CelloPiu4_restartOtherPiu
   *
   *  Descr.: Request to restart another PIU. Only allowed for MPs.
   *          Only BPs may be restarted.
   *          If command is rejected, no further action is taken.
   *          If command is accepted, a confirm or reject signal is received.
   *
   *  Args  : priMemory_p       A pointer to client's memory area
   *
   *          piuInstanceId     The id of the PIU object instance, that
   *                            should be restarted.
   *
   *          restartRank       Type of restart. See definitions above.
   *
   *          restartEscalation True = escalate (same as spontaneous restart).
   *                            False = no escalate (same as MO restart command).
   *
   *          restartCause      Text string giving cause for restart.
   *                            Max 60 characters. Used for logging.
   *
   *          clientId          Client reference, returned in response signal
   *
   *  Return: CelloPriResult    See cello_piu_types.h
   *
   ****************************************************************************/
  CelloPriResult
  CelloPiu4_restartOtherPiu(void           *priMemory_p,
			    U32             piuInstanceId,
			    U32             restartRank,
			    Boolean         restartEscalation,
			    char           *restartCause,
			    U32             clientId);

  /*
   * Prototypes valid for PRI protocol version 5
   * -------------------------------------------
   */

  /*
   * Prototypes valid for PRI protocol version 6
   * -------------------------------------------
   */

  /*
   * Prototypes valid for PRI protocol version 7
   * -------------------------------------------
   */

  /*
   * Prototypes valid for PRI protocol version 8
   * -------------------------------------------
   */

  /*****************************************************************************
   *
   *  Name  : CelloPri8_getOwnIdentity
   *
   *  Descr.: Request to get identity and configuration information
   *          for own PIU or own PIU Device.
   *
   *  Args  : priMemory_p     A pointer to client's memory area
   *          clientId        Client reference, returned in response signal
   *
   *  Return: CelloPriResult  See cello_piu_types.h
   *
   ****************************************************************************/
  CelloPriResult
  CelloPri8_getOwnIdentity(void *priMemory_p,
			   U32   clientId);

  /******************************************************************************
   *
   *  Name  : CelloPri8_getPiuOrDeviceIdentity
   *
   *  Descr.: Request to get identity and configuration information
   *          for specified PIU or specified PIU Device.
   *
   *  Args  : priMemory_p     A pointer to client's memory area
   *          piuOrDeviceId   The id of the PIU or PIU Device object instance,
   *                          of which the information should be retrieved.
   *          clientId        Client reference, returned in response signal
   *
   *  Return: CelloPriResult  See cello_piu_types.h
   *
   *****************************************************************************/
  CelloPriResult
  CelloPri8_getPiuOrDeviceIdentity(void *priMemory_p,
				   U32   piuOrDeviceId,
				   U32   clientId);

  /*
   * Prototypes valid for PRI protocol version 9
   * -------------------------------------------
   */

  /*****************************************************************************
   *
   *  Name  : CelloPri9_getOwnIdentity
   *
   *  Descr.: Request to get identity and configuration information
   *          for own PIU or own PIU Device.
   *
   *  Args  : priMemory_p     A pointer to client's memory area
   *          clientId        Client reference, returned in response signal
   *
   *  Return: CelloPriResult  See cello_piu_types.h
   *
   ****************************************************************************/
  CelloPriResult
  CelloPri9_getOwnIdentity(void *priMemory_p,
			   U32   clientId);

  /******************************************************************************
   *
   *  Name  : CelloPri9_getPiuOrDeviceIdentity
   *
   *  Descr.: Request to get identity and configuration information
   *          for specified PIU or specified PIU Device.
   *
   *  Args  : priMemory_p     A pointer to client's memory area
   *          piuOrDeviceId   The id of the PIU or PIU Device object instance,
   *                          of which the information should be retrieved.
   *          clientId        Client reference, returned in response signal
   *
   *  Return: CelloPriResult  See cello_piu_types.h
   *
   *****************************************************************************/
  CelloPriResult
  CelloPri9_getPiuOrDeviceIdentity(void *priMemory_p,
				   U32   piuOrDeviceId,
				   U32   clientId);

  /*
   * Prototypes valid for PRI protocol version 10
   * --------------------------------------------
   */

  /******************************************************************************
   *
   *  Name  : CelloPiu10_getPid
   *
   *  Descr.: Request to retrieve the operational Product Information Data (PID)
   *          for the specified resource. The client will receive
   *          the CELLO_PIU10_OPERATIONAL_PID_CFM signal, with the requested info.
   *
   *  Args  : priMemory_p        A pointer to client's memory area
   *
   *          piuInstanceId      The id of the PIU object instance.
   *
   *          clientId           Client reference, returned in response signal
   *
   *  Return: CelloPriResult     See cello_piu_types.h
   *
   *****************************************************************************/
  CelloPriResult
  CelloPiu10_getPid(void    *priMemory_p,
		    U32      piuInstanceId,
		    U32      clientId);

  /*
   * Prototypes valid for PRI protocol version 11
   * --------------------------------------------
   */

#ifdef __cplusplus
}
#endif

#endif /* CELLO_PIU_H */
