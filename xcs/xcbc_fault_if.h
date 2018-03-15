/**
 *   @copyright
 *   Copyright (C) 2015 by Ericsson AB. All rights reserved. The
 *   information in this document is the property of Ericsson. Except
 *   as specifically authorized in writing by Ericsson, the receiver
 *   of this document shall keep the information contained herein
 *   confidential and shall protect the same in whole or in part from
 *   disclosure and dissemination to third parties. Disclosure and
 *   disseminations to the receiver's employees shall only be made on
 *   a strict need to know basis.
 */

#ifndef _XCBC_FAULT_IF_H_
#define _XCBC_FAULT_IF_H_
#include <stdint.h>
#include "conn-establish.h"

#define FAULT_SERVER_MAILBOX    "xcbc_fault_server"
#define FAULT_SERVER_VERSIONS   1

/* Return values for XPAI_Fault */
#define XCBC_FAULT_OK                   1
#define XCBC_FAULT_NOT_OK               0

/* Recovery actions */
#define XCBC_NO_SUGGESTED_ACTION        0x0000
#define XCBC_ALIGN_STATES               0x0001
#define XCBC_ENTITY_RESTART             0x0002
#define XCBC_ENTITY_FAILURE             0x0003
#define XCBC_ENTITY_DEGRADED            0x0004

/* Fault types */
#define XCBC_GENERAL_SW_ERROR           0x0001
#define XCBC_GENERAL_HW_ERROR           0x0002

#define XCBC_MAX_NOF_FAULT_SUBSCRIBERS  5
#define XCBC_MAX_FAULT_DESCR_LEN        110

/* Return codes for XPAI_SubscribeFaults. */
#define XCBC_SUBSCRIBE_FAULTS_OK                          0
#define XCBC_SUBSCRIBE_FAULTS_NOK_WRONG_PARAM             1
#define XCBC_SUBSCRIBE_FAULTS_NOK_SERVER                  2
#define XCBC_SUBSCRIBE_FAULTS_NOK_UNSUPPORTED_CAPABILITY  3
#define XCBC_SUBSCRIBE_FAULTS_NOK_OTHER                   4

/* Return codes for XPAI_FaultClear. */
#define XCBC_FAULT_CLEAR_OK               0
#define XCBC_FAULT_CLEAR_NOK_WRONG_PARAM  1
#define XCBC_FAULT_CLEAR_NOK_SERVER       2
#define XCBC_FAULT_CLEAR_NOK_WRONG_STATE  3
#define XCBC_FAULT_CLEAR_NOK_OTHER        4

/* Parameter value for parameter faultType to XPAI_FaultClear. */
#define XCBC_CLEAR_FAULT_TYPE_ALL  0xffff


/* Parameter value for parameter recoveryAction to XPAI_FaultClear. */
#define XCBC_CLEAR_RECOVERY_ACTION_ALL  0xffff


/*
 * XCBC tags that may be subscribed with the function XPAI_Subscribe
 * (see xpai_xmr_if.h).
 * Valid tag size (including null term.): 1 - XMR_MAX_TAG_LENGTH
 * The tags are distributed with XPAI_DELIV_IND or XPAI_DELIV_REQ/CFM/REJ.
 *
 *     Tag                  Type            Description
 *     ---                  ----            -----------
 *     "XCBC_LostClient"    IND             All CBCI clients lost.
 *     "XCBC_StartUpTest"   REQ/CFM/REJ     Start up test ordered from CBCI.
 *     "XCBC_SelfTest"      REQ/CFM/REJ     Self test ordered from CBCI/XPAI.
 *     "XCBC_WarmReset"     REQ/CFM/REJ     Warm reset ordered from CBCI.
 *     "XCBC_RestartBoard"  REQ/CFM/REJ     Restart board ordered from CBCI/XPAI.
 *     "XCBC_FaultClear"    REQ/CFM/REJ     Fault clear ordered from XPAI.
 */
#define XCBC_XMR_TAG_FAULT_CLEAR   "XCBC_FaultClear"

/* Signal define */
#ifndef XCBC_SIGBASE
#define XCBC_SIGBASE 0x0100E000
#endif
#ifndef SIGSELECT
#define SIGSELECT uint32_t
#endif

#define XCBC_FAULT_STRUCTS \
	struct xcbc_report_fault     report_fault; \
	struct xcbc_purge_faults_ind purge_faults_ind; \
	struct xcbc_fault_ind        fault_ind; \
	struct subscribe_faults_req  sub_faults_req; \
	struct subscribe_faults_cfm  sub_faults_cfm; \
	struct subscribe_faults_rej  sub_faults_rej; \
	struct fault_clear_req       clear_req; \
	struct fault_clear_cfm       clear_cfm; \
	struct fault_clear_rej       clear_rej;

#define XCBC_FAULT_CONN_ESTABLISH_REQ    (XCBC_SIGBASE + 0x1)
#define XCBC_FAULT_CONN_ESTABLISH_CFM    (XCBC_SIGBASE + 0x2)
#define XCBC_FAULT_CONN_ESTABLISH_REJ    (XCBC_SIGBASE + 0x3)
#define XCBC_FAULT_CONN_DISCONNECT_REQ   (XCBC_SIGBASE + 0x4)
#define XCBC_FAULT_CONN_DISCONNECT_CFM   (XCBC_SIGBASE + 0x5)
#define XCBC_FAULT_CONN_DISCONNECT_REJ   (XCBC_SIGBASE + 0x6)
#define XCBC_FAULT_CONN_MONITOR_FWD      (XCBC_SIGBASE + 0x7)


#define XCBC_REPORT_FAULT         (XCBC_SIGBASE + 0x9)
#define XCBC_PURGE_FAULTS_IND     (XCBC_SIGBASE + 0xA)
#define XCBC_FAULT_IND            (XCBC_SIGBASE + 0xB)

#define XCBC_SUBSCRIBE_FAULTS_REQ (XCBC_SIGBASE + 0xC)
#define XCBC_SUBSCRIBE_FAULTS_CFM (XCBC_SIGBASE + 0xD)
#define XCBC_SUBSCRIBE_FAULTS_REJ (XCBC_SIGBASE + 0xE)

#define XCBC_FAULT_CLEAR_REQ      (XCBC_SIGBASE + 0xF)
#define XCBC_FAULT_CLEAR_CFM      (XCBC_SIGBASE + 0x10)
#define XCBC_FAULT_CLEAR_REJ      (XCBC_SIGBASE + 0x11)

#define XCBC_FAULT_MSG_HEADER \
	uint32_t msgno;         \
	uint32_t procedure_ref; \
	uint32_t connection_ref;

struct xcbc_purge_faults_ind {
	XCBC_FAULT_MSG_HEADER
};

struct xcbc_report_fault {
	XCBC_FAULT_MSG_HEADER
	uint16_t    fault_type;
	uint16_t    recov_act;
	char        fault_description[XCBC_MAX_FAULT_DESCR_LEN];
};

/* XCBC_FAULT_IND is send back to the application directly
   keep the old structure for legacy reason */
struct xcbc_fault_ind {
	SIGSELECT sigNo;
	uint16_t  faultType;
	uint16_t  recoveryAction;
	char      faultDescription[XCBC_MAX_FAULT_DESCR_LEN];
};


struct subscribe_faults_req {
	XCBC_FAULT_MSG_HEADER
	uint32_t mbox;
};

struct subscribe_faults_cfm {
	XCBC_FAULT_MSG_HEADER
};

struct subscribe_faults_rej {
	XCBC_FAULT_MSG_HEADER
	uint32_t err_code;
};

struct fault_clear_req {
	XCBC_FAULT_MSG_HEADER
	uint16_t fault_type;
	uint16_t recov_act;
};

struct fault_clear_cfm {
	XCBC_FAULT_MSG_HEADER
};

struct fault_clear_rej {
	XCBC_FAULT_MSG_HEADER
	uint32_t err_code;
};

#endif
