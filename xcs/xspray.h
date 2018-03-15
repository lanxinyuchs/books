/******************************************************************************
 *
 *      COPYRIGHT (C) Ericsson Radio Systems AB, Sweden
 *
 *      The copyright to the computer program(s) herein is the property
 *      of Ericsson Radio Systems AB.
 *
 *      The program(s) may be used and/or copied only with the written
 *      permission from Ericsson Radio Systems AB or in accordance with
 *      the terms and conditions stipulated in the agreement/contract
 *      under which the program(s) have been supplied.
 *
 *****************************************************************************/

/******************************************************************************
 *
 * Product name:
 *      XSPRAY,  CAH 109 2191
 *
 * File:
 *      xspray.h
 *
 * Author:
 *      Arturo Salinas (esalart)
 *
 * Description:
 *      This header file contains common decclarations of the xspray command.
 *
 * Revision history:
 *      2011-12-18 Arturo Salinas (esalart)
 *                 Initial implementation according to CR: BSDB0006283
 *      2012-08-23 Göran Nordin (egornor)
 *                 HQ34850, XSPRAY often hangs shell and can,t verify high prio
 *                 message echoed to RU.
 *                 Major refactoring to fix blocking issues.
 */

#ifndef _XSPRAY_H_
#define _XSPRAY_H_

/*----------------------------  Include files  ------------------------------*/

#include <ose.h>

/*----------------------------  CONSTANTS  ----------------------------------*/

/*
 * Use start of BCP sigbase range as XSPRAY sigbase.
 * The area BCP_SIGBASE + 0x0000 to BCP_SIGBASE + 0x03FF was previosly
 * allocated for CBCI_DB_SIGBASE but was not used and removed via WRNac27481.
 * In this case it's not a big deal even if XSPRAY_SIGBASE should overlap other
 * signal bases because all signals are internal.
 */
#define XSPRAY_SIGBASE    0x0100C000

#define XSPRAY_CONN_ESTABLISH_REQ (XSPRAY_SIGBASE)      /* !-SIGNO(struct XSPRAY_ConnEstablishReqs)-! */
#define XSPRAY_CONN_ESTABLISH_CFM (XSPRAY_SIGBASE + 1)  /* !-SIGNO(struct XSPRAY_ConnEstablishCfmS)-! */
#define XSPRAY_CONN_ESTABLISH_REJ (XSPRAY_SIGBASE + 2)  /* !-SIGNO(struct XSPRAY_ConnEstablishRejS)-! */
#define XSPRAY_ADD_REQ (XSPRAY_SIGBASE + 3)             /* !-SIGNO(struct XSPRAY_AddReqS)-! */
#define XSPRAY_ADD_CFM (XSPRAY_SIGBASE + 4)             /* !-SIGNO(struct XSPRAY_AddCfmS)-! */
#define XSPRAY_ADD_REJ (XSPRAY_SIGBASE + 5)             /* !-SIGNO(struct XSPRAY_RejS)-! */
#define XSPRAY_REMOVE_REQ (XSPRAY_SIGBASE + 6)          /* !-SIGNO(struct XSPRAY_RemoveReqS)-! */
#define XSPRAY_REMOVE_CFM (XSPRAY_SIGBASE + 7)          /* !-SIGNO(struct XSPRAY_RemoveCfmS)-! */
#define XSPRAY_REMOVE_REJ (XSPRAY_SIGBASE + 8)          /* !-SIGNO(struct XSPRAY_RejS)-! */
#define XSPRAY_GET_REQ (XSPRAY_SIGBASE + 9)             /* !-SIGNO(struct XSPRAY_GetReqS)-! */
#define XSPRAY_GET_CFM (XSPRAY_SIGBASE + 10)            /* !-SIGNO(struct XSPRAY_GetCfmS)-! */
#define XSPRAY_GET_REJ (XSPRAY_SIGBASE + 11)            /* !-SIGNO(struct XSPRAY_RejS)-! */
#define XSPRAY_GET_NEXT_REQ (XSPRAY_SIGBASE + 12)       /* !-SIGNO(struct XSPRAY_GetNextReqS)-! */
#define XSPRAY_GET_NEXT_CFM (XSPRAY_SIGBASE + 13)       /* !-SIGNO(struct XSPRAY_GetNextCfmS)-! */
#define XSPRAY_GET_NEXT_REJ (XSPRAY_SIGBASE + 14)       /* !-SIGNO(struct XSPRAY_RejS)-! */
#define XSPRAY_SET_OPTION_REQ (XSPRAY_SIGBASE + 16)     /* !-SIGNO(struct XSPRAY_SetOptionReqS)-! */
#define XSPRAY_SET_OPTION_CFM (XSPRAY_SIGBASE + 17)     /* !-SIGNO(struct XSPRAY_SetOptionCfmS)-! */
#define XSPRAY_SET_OPTION_REJ (XSPRAY_SIGBASE + 18)     /* !-SIGNO(struct XSPRAY_RejS)-! */
#define XSPRAY_START_REQ (XSPRAY_SIGBASE + 19)          /* !-SIGNO(struct XSPRAY_StartReqS)-! */
#define XSPRAY_START_CFM (XSPRAY_SIGBASE + 20)          /* !-SIGNO(struct XSPRAY_StartCfmS)-! */
#define XSPRAY_START_REJ (XSPRAY_SIGBASE + 21)          /* !-SIGNO(struct XSPRAY_RejS)-! */
#define XSPRAY_STOP_REQ (XSPRAY_SIGBASE + 22)           /* !-SIGNO(struct XSPRAY_StopReqS)-! */
#define XSPRAY_STOP_CFM (XSPRAY_SIGBASE + 23)           /* !-SIGNO(struct XSPRAY_StopCfmS)-! */
#define XSPRAY_STOP_REJ (XSPRAY_SIGBASE + 24)           /* !-SIGNO(struct XSPRAY_StopRejS)-! */
#define XSPRAY_GET_STATUS_REQ (XSPRAY_SIGBASE + 25)     /* !-SIGNO(struct XSPRAY_StatusReqS)-! */
#define XSPRAY_GET_STATUS_CFM (XSPRAY_SIGBASE + 26)     /* !-SIGNO(struct XSPRAY_StatusCfmS)-! */
#define XSPRAY_GET_STATUS_REJ (XSPRAY_SIGBASE + 27)     /* !-SIGNO(struct XSPRAY_RejS)-! */
#define XSPRAY_HUNT_IND (XSPRAY_SIGBASE + 28)           /* !-SIGNO(SIGSELECT)-! */
#define XSPRAY_ATTACH_REQ (XSPRAY_SIGBASE + 29)         /* !-SIGNO(struct XSPRAY_AttachKillReqS)-! */
#define XSPRAY_KILL_REQ (XSPRAY_SIGBASE + 30)           /* !-SIGNO(struct XSPRAY_AttachKillReqS)-! */

/*----------------------------  MACROS  -------------------------------------*/

#define MIN(a,b) ((a < b) ? a : b)
#define MAX(a,b) ((a > b) ? a : b)

/*----------------------------  Structs and typedefs  -----------------------*/

/*
 * Possible error codes returned in REJ signals.
 */
enum XSPRAY_ErrorCode
{
  /* No error */
  XSPRAY_NO_ERR = 0x00,
  /* Message received in wrong state. */
  XSPRAY_ERR_WRONG_STATE = 0x01,
  /* Parameter value out of range. */
  XSPRAY_ERR_WRONG_PARAM = 0x02,
  /* Service is not  supported. */
  XSPRAY_ERR_UNSUPPORTED = 0x03,
  /* Lack of resources. */
  XSPRAY_ERR_RESOURCE = 0x04,
  /* Missing or inconsistent configuration data. */
  XSPRAY_ERR_WRONG_CONFIG = 0x05,
  /* Other serious problems. */
  XSPRAY_ERR_OTHER = 0x06
};

/*----------------------------  Declaration of Global Variables  ------------*/

/* Receive mask to select all signals. */
extern const SIGSELECT xsprayAllSig[];

/* Command line options. */
extern const char *xsprayOptArg[];

/*----------------------------  Declaration of Global Functions  ------------*/

/******************************************************************************
 *
 * Function:
 *      xsprayGetArgNumSuffix
 *
 * Parameters:
 *      index  Index number of argument
 *
 * Return value:
 *      the argument suffix string
 *
 * Description:
 *      Returns the argument suffix string.
 *
 *****************************************************************************/
extern const char *xsprayGetArgNumSuffix(int index);

#endif  /* _XSPRAY_H_ */
