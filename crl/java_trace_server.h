/**
 *   This file contains the interface to the Java Trace Server.
 *
 *   It actually contains two interfaces, one towards the Java 
 *   communication thread, and one towards the command handler.
 *
 *   @file java_trace_server.h
 *
 *   Copyright (C) 2015 by Ericsson AB. All rights reserved. The
 *   information in this document is the property of Ericsson. Except
 *   as specifically authorized in writing by Ericsson, the receiver
 *   of this document shall keep the information contained herein
 *   confidential and shall protect the same in whole or in part from
 *   disclosure and dissemination to third parties. Disclosure and
 *   disseminations to the receiver's employees shall only be made on
 *   a strict need to know basis.
 */

#ifndef JAVA_TRACE_SERVER_H
#define JAVA_TRACE_SERVER_H

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================
 *   INCLUDE FILES
 * ============================================================================
 */

#include <itc.h>
#include <stdint.h>

/* ============================================================================
 *   DEFINITIONS
 * ============================================================================
 */

/* Change requests in JtsChangeMaskReq */
#define JTS_MASK_ENABLE  (0)
#define JTS_MASK_DISABLE (1)
#define JTS_MASK_DEFAULT (2)

/* Type in JtsStatusReq and JtsChangeMaskReq */
#define JTS_RUNNING      (0)
#define JTS_RESTART      (1)
#define JTS_PRESET       (2)

/* Result codes in the generic response signal */
#define JTS_RESULT_OK    (0)
#define JTS_RESULT_ERROR (1)

/* Maximum length of a trace item, including null terminator */
#define JTS_MAX_TRACE_ITEM_LEN 128

/* The mail box name of the Java Trace Server */
#define JTS_MBOX_NAME   "JavaTraceServer"


#define JTS_MESSAGE_BASE (0x10D50)

/* Group masks */
#define GROUP_CHECK    (1<<0)
#define GROUP_ERROR    (1<<1)
#define GROUP_ENTER    (1<<2)
#define GROUP_RETURN   (1<<3)
#define GROUP_INFO     (1<<4)
#define GROUP_TRACE1   (1<<5)
#define GROUP_TRACE2   (1<<6)
#define GROUP_TRACE3   (1<<7)
#define GROUP_TRACE4   (1<<8)
#define GROUP_TRACE5   (1<<9)
#define GROUP_TRACE6   (1<<10)
#define GROUP_TRACE7   (1<<11)
#define GROUP_TRACE8   (1<<12)
#define GROUP_TRACE9   (1<<13)
#define GROUP_STATE_CHANGE (1<<14)
#define GROUP_REC_SIG  (1<<17)
#define GROUP_SEND_SIG (1<<18)
#define GROUP_PARAM    (1<<19)
#define GROUP_INVALID  (uint32_t)(1<<31)

#define DEFAULT_MASK (GROUP_INFO|GROUP_ERROR)


/* ============================================================================
 *   TYPE DEFINITIONS
 * ============================================================================
 */

/*
 ******************************************************************************
 * This is the Java Trace Server signal interface.
 *
 * It is used by the Java Trace Handler to send information from the 'te'
 * shell command to the Java trace server.
 ******************************************************************************
 */

/*
 * This type is used to hold configuration status for a trace item.
 *
 * id - ITC mailbox id
 * mask - mask in use
 * traceItem - the trace item string
 */
typedef struct {
   uint32_t id;
   uint32_t mask;
   char traceItem[JTS_MAX_TRACE_ITEM_LEN];
} TraceItemInfo;

/*
 * A generic response signal. Currently only used for notifying that 
 * the operations has been completed.
 *
 * result - JTS_RESULT_OK or JTS_RESULT_ERROR
 * info - string containing additional information
 */
#define JTS_GENERIC_RSP (JTS_MESSAGE_BASE + 1)
typedef struct {
   uint32_t msgNo;
   uint32_t result;
   char info[1];
} JtsGenericRsp;


/*
 * Request to change the group mask for a trace item.
 * The server will reply with JTS_GENERIC_RSP.
 *
 * For enable/disable, the trace item will be be created if it does not
 * already exist.
 *
 * change - JTS_MASK_ENABLE, JTS_MASK_DISABLE, or JTS_MASK_DEFAULT
 * mask - the delta group mask (not used for default)
 * scope - JTS_RUNNING, JTS_PRESET, or JTS_RESTART
 * traceItem - the trace item string
 */
#define JTS_CHANGE_MASK_REQ (JTS_MESSAGE_BASE + 2)
typedef struct {
   uint32_t msgNo;
   uint32_t change;
   uint32_t mask;
   uint32_t scope;
   char traceItem[JTS_MAX_TRACE_ITEM_LEN];
} JtsChangeMaskReq;


/*
 * Request to set the group mask for a trace item.
 * The server will reply with JTS_GENERIC_RSP.
 *
 * save - 1 if mask also should be save, 0 otherwise
 * mask - the new group mask
 * traceItem - 
 */
#define JTS_SET_MASK_REQ (JTS_MESSAGE_BASE + 3)
typedef struct {
   uint32_t msgNo;
   uint32_t save;
   uint32_t mask;
   char traceItem[JTS_MAX_TRACE_ITEM_LEN];
} JtsSetMaskReq;


/*
 * Request to save the group mask configuration for a trace item.
 * The server will reply with JTS_GENERIC_RSP.
 *
 * traceItem - 
 */
#define JTS_SAVE_MASK_REQ (JTS_MESSAGE_BASE + 4)
typedef struct {
   uint32_t msgNo;
   char traceItem[JTS_MAX_TRACE_ITEM_LEN];
} JtsSaveMaskReq;



/*
 * Request the current trace configuration.
 * The server will reply with 0 or more JTS_STATUS_IND followed by
 * a JTS_GENERIC_RSP.
 * 
 * type - JTS_RUNNING, JTS_PRESET, or JTS_RESTART
 * traceItem - 
 */
#define JTS_STATUS_REQ (JTS_MESSAGE_BASE + 5)
typedef struct {
   uint32_t msgNo;
   uint32_t type;
   char traceItem[JTS_MAX_TRACE_ITEM_LEN];
} JtsStatusReq;

/*
 * Status indication sent as a reponse to JTS_STATUS_REQ
 *
 * nTraceItems - number of trace items in the signal
 * info - status information for each trace item
 */
#define JTS_STATUS_IND (JTS_MESSAGE_BASE + 6)
typedef struct {
   uint32_t msgNo;
   uint32_t nTraceItems;
   TraceItemInfo info[1];
} JtsStatusInd;


/* ============================================================================
 *   CONSTANTS
 * ============================================================================
 */

/* ============================================================================
 *   VARIABLES
 * ============================================================================
 */

#endif /* JAVA_TRACE_SERVER_H */
