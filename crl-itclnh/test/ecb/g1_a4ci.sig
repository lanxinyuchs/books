/* -*- c -*- ******************************************************************
 *
 *      COPYRIGHT (C)                 Ericsson Radio Systems AB, Sweden
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

#ifndef _G1_A4CI_SIG_
#define _G1_A4CI_SIG_

#include <stdint.h>

#include "g1_a4ci.h"

/* For interoperability reasons definitions here are the same as in legacy. */

#define A4CI_SIGBASE          0x0100FD00


#define A4CI_CONN_ESTABLISH_REQ (uint32_t)(A4CI_SIGBASE + 0)
struct a4ci_connEstablishReqS {
	uint32_t             sigNo;
	uint16_t             protocolRev;
} __attribute__((packed));

#define A4CI_CONN_ESTABLISH_CFM (uint32_t)(A4CI_SIGBASE + 1)
struct a4ci_connEstablishCfmS {
	uint32_t             sigNo;
} __attribute__((packed));

#define A4CI_CONN_ESTABLISH_REJ (uint32_t)(A4CI_SIGBASE + 2)
struct a4ci_connEstablishRejS {
	uint32_t             sigNo;
	uint16_t             errorCode;
	uint16_t             protocolRev;
} __attribute__((packed));

#define A4CI_DATA_FWD (uint32_t)(A4CI_SIGBASE + 3)
struct a4ci_dataFwdS {
	uint32_t			sigNo;
	uint16_t			port;
	uint8_t				hdlcAddr;
	uint8_t				padding;
	uint16_t			length;
	uint8_t				data[A4CI_MAX_DATA_SIZE];
} __attribute__((packed));

#define A4CI_DATA_REQ (uint32_t)(A4CI_SIGBASE + 4)
struct a4ci_dataReqS {
	uint32_t			sigNo;
	uint16_t			port;
	uint8_t				hdlcAddr;
	uint8_t				padding;
	uint16_t			length;
	uint8_t				data[A4CI_MAX_DATA_SIZE];
} __attribute__((packed));

#define A4CI_DATA_CFM (uint32_t)(A4CI_SIGBASE + 5)
struct a4ci_dataCfmS {
	uint32_t			sigNo;
	uint16_t			port;
	uint8_t				hdlcAddr;
	uint8_t				padding;
	uint16_t			length;
	uint8_t				data[A4CI_MAX_DATA_SIZE];
} __attribute__((packed));

#define A4CI_DATA_REJ (uint32_t)(A4CI_SIGBASE + 6)
struct a4ci_dataRejS {
	uint32_t			sigNo;
	uint16_t			errorCode;
	uint16_t			port;
	uint8_t				hdlcAddr;
} __attribute__((packed));

#define A4CI_DATA2_REQ (uint32_t)(A4CI_SIGBASE + 7)
struct a4ci_data2ReqS {
	uint32_t			sigNo;
	uint32_t			clientRef;
	uint16_t			port;
	uint16_t			length;
	uint8_t				hdlcAddr;
	uint8_t				data[A4CI_MAX_DATA_SIZE];
} __attribute__((packed));

#define A4CI_DATA2_CFM (uint32_t)(A4CI_SIGBASE + 8)
struct a4ci_data2CfmS {
	uint32_t			sigNo;
	uint32_t			clientRef;
	uint16_t			port;
	uint16_t			length;
	uint8_t				hdlcAddr;
	uint8_t				data[A4CI_MAX_DATA_SIZE];
} __attribute__((packed));

#define A4CI_DATA2_REJ (uint32_t)(A4CI_SIGBASE + 9)
struct a4ci_data2RejS {
	uint32_t			sigNo;
	uint32_t			clientRef;
	uint16_t			errorCode;
	uint16_t			port;
	uint8_t				hdlcAddr;
} __attribute__((packed));

#define A4CI_LINK_STAT_REQ (uint32_t)(A4CI_SIGBASE + 10)
struct a4ci_linkStatReqS {
	uint32_t			sigNo;
	uint32_t			clientRef;
	uint16_t			port;
} __attribute__((packed));

#define A4CI_LINK_STAT_CFM (uint32_t)(A4CI_SIGBASE + 11)
struct a4ci_linkStatCfmS {
	uint32_t			sigNo;
	uint32_t			clientRef;
	uint32_t			nrOfReceivedOctets;
	uint32_t			nrOfTransmittedOctets;
	uint32_t			nrOfReceivedHDLCFrames;
	uint32_t			nrOfTransmittedHDLCFrames;
	uint32_t			nrOfFCSErrors;
	uint32_t			nrOfErrorCtrlFields;
	uint32_t			nrOfErrorAddFields;
	uint32_t			nrOfTimeouts;
	uint32_t			nrOfUartOverruns;
	uint32_t			nrOfUartOverrunsIsValid;
	uint16_t			port;
} __attribute__((packed));

#define A4CI_LINK_STAT_REJ (uint32_t)(A4CI_SIGBASE + 12)
struct a4ci_linkStatRejS {
	uint32_t			sigNo;
	uint32_t			clientRef;
	uint16_t			port;
	uint16_t			errorCode;
} __attribute__((packed));

#endif
