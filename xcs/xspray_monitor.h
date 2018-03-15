/******************************************************************************
 *
 *  COPYRIGHT (C) Ericsson Radio Systems AB, Sweden
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
 *     XSPRAY,  CAH 109 2191
 *
 * File:
 *  xspray_monitor.h
 *
 * Author:
 *  Arturo Salinas (esalart)
 *
 * Description:
 *      This header file exposes the interface of the xsprayMonitor process.
 *
 * Revision history:
 *      2012-01-26 Arturo Salinas (esalart)
 *                 Initial implementation according to CR: BSDB0006283
 *      2012-05-08 ewenyao change for echo
 *      2012-08-23 Göran Nordin (egornor)
 *                 HQ34850, XSPRAY often hangs shell and can,t verify high prio
 *                 message echoed to RU.
 *                 Major refactoring to fix blocking issues.
 */

#ifndef XSPRAY_MONITOR_H_
#define XSPRAY_MONITOR_H_

/*----------------------------  Include files  ------------------------------*/

#include <ose.h>
#include <osetypes.h>
#include "xspray_server.h"

/*----------------------------  CONSTANTS  ----------------------------------*/

/*----------------------------  MACROS  -------------------------------------*/

/*----------------------------  Structs and typedefs  -----------------------*/

/*
 * Common reject structure.
 */
struct XSPRAY_RejS {
  SIGSELECT sigNo;
  U16 errorCode; /* One of errorcodes defined in XSPRAY_ErrorCode */
  char errorDescription[128];
};

/*
 * Signal to request add of entry.
 */
struct XSPRAY_AddReqS {
  SIGSELECT sigNo;
  /* Process name */
  char name[1];
};

struct XSPRAY_AddCfmS {
  SIGSELECT sigNo;
  /* Handle to entry or NULL if entry not found. */
  void *hEntry;
};

/*
 * Signal to request remove of entry.
 */
struct XSPRAY_RemoveReqS {
  SIGSELECT sigNo;
  /* Handle to entry that should be removed. */
  void *hEntry;
};

struct XSPRAY_RemoveCfmS {
  SIGSELECT sigNo;
};

/*
 * Signal that request handle to a named or the first entry.
 */
struct XSPRAY_GetReqS {
  SIGSELECT sigNo;
  /* Process name or '\0' to find first process. */
  char name[1];
};

struct XSPRAY_GetCfmS {
  SIGSELECT sigNo;
  /* Handle to entry or NULL if entry not found. */
  void *hEntry;
};

/*
 * Signal that request handle to a the next entry.
 */
struct XSPRAY_GetNextReqS {
  SIGSELECT sigNo;
  /* Handle to current entry. */
  void *hEntry;
};

struct XSPRAY_GetNextCfmS {
  SIGSELECT sigNo;
  /* Handle to next entry or NULL if end of list. */
  void *hNext;
};


/*
 * Command line option with associated parameters.
 */
struct XSPRAY_OptionInstanceS {
  /*
   * Index in argv where argument were found. Used to give better error
   * descripton for invalid option values.
   */
  int argIndex;
  /*
   * Options XSPRAY_OPT_ECHO, XSPRAY_OPT_MINSZ, XSPRAY_OPT_MAXSZ might have 2
   * values suppled via commandline.
   */
  U32 numOfValues;
  U32 value[2];
};

/*
 * Signal that set options.
 */
struct XSPRAY_SetOptionReqS {
  SIGSELECT sigNo;
  /* Handle to entry for which options should be set. */
  void *hEntry;
  enum XSPRAY_Option option;
  U32 numOfInstances;
  struct XSPRAY_OptionInstanceS instance[1];
};

struct XSPRAY_SetOptionCfmS {
  SIGSELECT sigNo;
};

/*
 * Signal to request the start of the reception or transmittion of messages.
 * This also means to create the respective process.
 */
struct XSPRAY_StartReqS {
  SIGSELECT sigNo;
  /* Handle to entry to start. */
  void *hEntry;
  /* Receiving process name reside. */
  char rxName[1];
};

struct XSPRAY_StartCfmS {
  SIGSELECT sigNo;
};

/*
 * Signal to request the stop of the reception or transmittion of messages.
 * This also means to kill the respective process.
 */
struct XSPRAY_StopReqS {
  SIGSELECT sigNo;
  /* Handle to entry to stop. */
  void *hEntry;
};

struct XSPRAY_StopCfmS {
  SIGSELECT sigNo;
};

/*
 * Request status.
 */
struct XSPRAY_GetStatusReqS {
  SIGSELECT sigNo;
  /* Handle to entry for which process status is requested. */
  void *hEntry;
  /* Defines which status that is requested. */
  enum XSPRAY_StatusTag tag;
};

struct XSPRAY_GetStatusCfmS {
  SIGSELECT sigNo;
  /* Defines which status that is returned. */
  enum XSPRAY_StatusTag tag;
  /* Config and statistics. */
  union XSPRAY_Status u;
};

/*----------------------------  Declaration of Global Variables  ------------*/

/*----------------------------  Declaration of Global Functions  ------------*/

extern OSENTRYPOINT xsprayMonitor;

#endif /* XSPRAY_MONITOR_H_ */
