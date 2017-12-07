/*
 *
 * Copyright (c) Ericsson AB  2006 All rights reserved.
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
 * Description: This is the interface for the object event service, from now
 *              called OEI. The purpose of the OEI is provide a unique event
 *              number for the processes within a node.
 *
 *              The event number is supposed to be connected to an error event
 *              that will be logged by the calling process. When an error is
 *              corrected, the event number must be referred in the correction
 *              log to make an association to the first error log.
 *
 *
 * @(#) ClearCase ID: /vobs/cello/osa_src/OSA_CRX90149_1/OEI_CNX9011183/OEI_CXA1101637/inc/cello_oei.h /main/cppdev/1 07-11-03 14:09 ematbri #
 *
 *
 * REVISION HISTORY
 * ----------------
 *
 * Revised: ERA/FRL/KO Anders Östman 31 Oct 2000
 * Change:  Created
 *
 * Revised: UAB/J/KO Andrew Cheese 29 June 2001
 * Change:  Updated comments to reflect Cello 3.1 implementation.
 *
 * Revised: EAB/UKH/KM Daniel Lefwerth 2003-01-29
 * Change:  Removed internal macro.
 *
 */

#ifndef CELLO_OEI_H
#define CELLO_OEI_H

#ifdef __cplusplus
extern "C" {
#endif

  /********** IMPORT ************************/


  /********** EXPORTED CONSTANTS ************/


  /********** EXPORTED TYPES ****************/

  /* interface call results */
  typedef U16 CelloOeiResult;
#define CELLO_OEI_OK          ((U16)0)
#define CELLO_OEI_TIMEOUT     ((U16)1)
#define CELLO_OEI_NO_SERVER   ((U16)2)
#define CELLO_OEI_SERVER_ERROR ((U16)3)

  /* the oei server will never return this value */
#define CELLO_OEI_UNSPECIFIED_EVENT_ID ((U32)0)

  /* it is possible to specify that calls do not timeout in this interface */
#define CELLO_OEI_NO_TIMEOUT_VALUE ((U32)0)

  /********** EXPORTED FUNCTIONS ************/

  /************************************************************************
   *
   *  Name  : CelloOei_initiateMemory
   *
   *  Descr.: Initiation of the object event service for the calling
   *          process. The initiation data is stored in the CelloOeiService
   *          data type returned by the function.
   *          This data will be used every time object event service is
   *          called.
   *
   *          SHOULD ONLY BE CALLED ONCE, WHEN THE PROCESS IS STARTED.
   *
   *  Args. : NONE
   *
   *  Return: Pointer to initiation data. Must be used calling other
   *          CelloOei functions.
   *
   ************************************************************************/
  union SIGNAL *
  CelloOei_initiateMemory( void );


  /************************************************************************
   *
   *  Name  : CelloOei_getEventIdentity
   *
   *  Descr.: Executes a request to the object event service for an unique
   *          event number. See example above.
   *
   *  Args. : union SIGNAL    *oeiService_p  Pointer to initiation data.
   *                                         The initiation data is created
   *                                         by calling the initiation
   *                                         function CelloOei_initiateMemory.
   *
   *          U32              timeoutValue  Timeout value for the answer
   *                                         from the object event service.
   *                                         Not used in Cello 2.6 .
   *                                         Available from Cello 3.1 onwards.
   *
   *          U32             *eventId_p     Returns the unique event number.
   *
   *  Return: A CelloOeiResult code.
   *
   ************************************************************************/
  CelloOeiResult
  CelloOei_getEventIdentity( union SIGNAL    *oeiService_p,
			     U32              timeoutValue,
			     U32             *eventId_p );


#ifdef __cplusplus
}
#endif


#endif   /* CELLO_OEI_H */


