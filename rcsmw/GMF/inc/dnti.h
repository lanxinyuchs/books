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
 * Description: This file contains the interfaces provided by DNTI.
 *
 *
 * REVISION HISTORY
 * ----------------
 *
 * Revised: Esko Vierumäki 2012-10-31
 * Change:  Created
 */

#ifndef __DN_TI_H
#define __DN_TI_H

#ifdef  __cplusplus
extern "C" {
#endif

/*
******************************************************************************
* INCLUDE FILES
******************************************************************************
*/

#include <stdint.h>

/*
******************************************************************************
* MACROS
******************************************************************************
*/

#define DN_TI_SIGNAL_REVISION 0


/* This type specifies function return values when using the DNTI interface */
typedef uint32_t DntiResultT;
#define DNTI_OK                          0
#define DNTI_INVALID_PARAMETER_DIRECTION 1
#define DNTI_INVALID_PARAMETER_TODNP     2
#define DNTI_SEND_ERROR                  3
#define DNTI_RECEIVE_ERROR               4
#define DNTI_OBJECT_CLASS_NOT_FOUND      5

typedef uint32_t DntiTransformDirectionT;
#define MIM_TO_IMM  1
#define IMM_TO_MIM  2


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

  /*****************************************************************************
   *
   *  Name  : dntiInitiateService
   *
   *  Descr.: Initialize Dn Translate service.
   *          This function sets up a connection to the translator server.
   *          If a translation service is not initiated a new connection
   *          will be set up for each translation request.
   *
   *  Return: Pointer to the connection handle
   *
   ****************************************************************************/
void*
dntiInitiateService(void);



  /*****************************************************************************
   *
   *  Name  : dnti_terminateService
   *
   *  Descr.: Terminate the translate service
   *
   *  Args  : IN:  dntiHandleP       A pointer to the connection handle
   *
   *  Return: DntiResult
   *
   ****************************************************************************/
void
dntiTerminateService(void *dntiHandleP);



  /*****************************************************************************
   *
   *  Name  : dntiTransform
   *
   *  Descr.: Translate DN from mim format to imm format or vice versa.
   *          If NULL pointer is given as the connection handle
   *          a new connection is set up and released for each request.
   *
   *  Args  : IN:     dntiHandleP      A pointer to the connection handle
   *
   *                  direction        MIM_TO_IMM | IMM_TO_MIM
   *
   *                  fromDnP          from DN
   *
   *          IN/OUT: toDnP            to DN | error string
   *
   *
   *  Args  :
   *
   *  Return: DntiResultT
   *
   ****************************************************************************/
DntiResultT
dntiTransform(void *dntiHandleP,
	      DntiTransformDirectionT direction,
	      char *fromDnP,
	      char **toDnP);


#ifdef  __cplusplus
}
#endif

#endif /* __DN_TI_H */
