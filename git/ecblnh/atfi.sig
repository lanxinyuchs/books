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

#ifndef ATFI_SIG_
#define ATFI_SIG_

#include <stdint.h>

#include "atfi.h"

/* For interoperability reasons definitions here are the same as in legacy. */

#define ATFI_SIGBASE            0x0100FC00


struct atfiAddrInfoS {
	uint32_t              linkHandle;
};

typedef struct {
        uint32_t     supa;
} AtmPhyS;

typedef struct {
#define MAC_ADDR_LENGTH                 6
        uint8_t      macAddr[MAC_ADDR_LENGTH];
        uint8_t      pad[2];
} EthPhyS;

typedef struct {
        uint8_t      transportType;
        uint8_t      pad[3];
        union {
                AtmPhyS atm;
                EthPhyS eth;
        } address;
} PhyEndPointAddrS;

#define ATFI_CONN_ESTABLISH_REQ (ATFI_SIGBASE + 4) /* !- SIGNO(struct atfiConnEstablishReqS) -! */
struct atfiConnEstablishReqS {
	uint32_t              sigNo;
	struct atfiAddrInfoS  addrInfo;
	uint16_t              protocolRev;
};

#define ATFI_CONN_ESTABLISH_CFM (ATFI_SIGBASE + 5) /* !- SIGNO(struct atfiConnEstablishCfmS) -! */
struct atfiConnEstablishCfmS {
	uint32_t              sigNo;
	struct atfiAddrInfoS  addrInfo;
};

#define ATFI_CONN_ESTABLISH_REJ (ATFI_SIGBASE + 6) /* !- SIGNO(struct atfiConnEstablishRejS) -! */
struct atfiConnEstablishRejS {
	uint32_t              sigNo;
	struct atfiAddrInfoS  addrInfo;
	uint16_t              errorCode;
	uint16_t              highestSupportedProtocolRev;
};

#define ATFI_ADD_CONN_MAP_REQ (ATFI_SIGBASE + 7) /* !- SIGNO(struct atfiAddConnMapReqS) -! */
struct atfiAddConnMapReqS {
	uint32_t              sigNo;
	struct atfiAddrInfoS  addrInfo;
	uint16_t              logicalAddress;
	uint16_t              physicalAddress;
};

#define ATFI_ADD_CONN_MAP_CFM (ATFI_SIGBASE + 8) /* !- SIGNO(struct atfiAddConnMapCfmS) -! */
struct atfiAddConnMapCfmS {
	uint32_t              sigNo;
	struct atfiAddrInfoS  addrInfo;
};

#define ATFI_ADD_CONN_MAP_REJ (ATFI_SIGBASE + 9) /* !- SIGNO(struct atfiAddConnMapRejS) -! */
struct atfiAddConnMapRejS {
	uint32_t              sigNo;
	struct atfiAddrInfoS  addrInfo;
	uint16_t              errorCode;
};

#define ATFI_ADD_AU3_MAP_REQ (ATFI_SIGBASE + 30) /* !- SIGNO(struct atfiAddAu3MapReqS) -! */
struct atfiAddAu3MapReqS {
	uint32_t              sigNo;
	struct atfiAddrInfoS  addrInfo;
	uint16_t              logicalAddress;
	char                  auType[ATFI_MAX_PRODUCT_ID_STRINGLENGTH];
};

#define ATFI_ADD_AU3_MAP_CFM (ATFI_SIGBASE + 31) /* !- SIGNO(struct atfiAddAu3MapCfmS) -! */
struct atfiAddAu3MapCfmS {
	uint32_t              sigNo;
	struct atfiAddrInfoS  addrInfo;
};

#define ATFI_ADD_AU3_MAP_REJ (ATFI_SIGBASE + 32) /* !- SIGNO(struct atfiAddAu3MapRejS) -! */
struct atfiAddAu3MapRejS {
	uint32_t              sigNo;
	struct atfiAddrInfoS  addrInfo;
	uint16_t              errorCode;
};

#define ATFI_REMOVE_CONN_MAP_REQ (ATFI_SIGBASE + 19) /* !- SIGNO(struct atfiRemoveConnMapReqS) -! */
struct atfiRemoveConnMapReqS {
	uint32_t              sigNo;
	struct atfiAddrInfoS  addrInfo;
	uint16_t              logicalAddress;
};

#define ATFI_REMOVE_CONN_MAP_CFM (ATFI_SIGBASE + 20) /* !- SIGNO(struct atfiRemoveConnMapCfmS) -! */
struct atfiRemoveConnMapCfmS {
	uint32_t              sigNo;
	struct atfiAddrInfoS  addrInfo;
};

#define ATFI_REMOVE_CONN_MAP_REJ (ATFI_SIGBASE + 21) /* !- SIGNO(struct atfiRemoveConnMapRejS) -! */
struct atfiRemoveConnMapRejS {
	uint32_t              sigNo;
	struct atfiAddrInfoS  addrInfo;
	uint16_t              errorCode;
};

#define ATFI_CONNECT2_REQ (ATFI_SIGBASE + 26) /* !- SIGNO(struct atfiConnect2ReqS) -! */
struct atfiConnect2ReqS {
	uint32_t              sigNo;
	struct atfiAddrInfoS  addrInfo;
	uint16_t              logicalAddress;
	uint16_t              typeOfUnit;
};

#define ATFI_CONNECT2_CFM (ATFI_SIGBASE + 27) /* !- SIGNO(struct atfiConnect2CfmS) -! */
struct atfiConnect2CfmS {
	uint32_t              sigNo;
	struct atfiAddrInfoS  addrInfo;
	uint16_t              state;
};

#define ATFI_CONNECT2_REJ (ATFI_SIGBASE + 28) /* !- SIGNO(struct atfiConnect2RejS) -! */
struct atfiConnect2RejS {
	uint32_t              sigNo;
	struct atfiAddrInfoS  addrInfo;
	uint16_t              errorCode;
};

#define ATFI_CONNECT2_IND (ATFI_SIGBASE + 29) /* !- SIGNO(struct atfiConnect2IndS) -! */
struct atfiConnect2IndS {
	uint32_t              sigNo;
	struct atfiAddrInfoS  addrInfo;
	uint16_t              logicalAddress;
};

#define ATFI_DISCONNECT_REQ (ATFI_SIGBASE + 13) /* !- SIGNO(struct atfiDisconnectReqS) -! */
struct atfiDisconnectReqS {
	uint32_t              sigNo;
	struct atfiAddrInfoS  addrInfo;
	uint16_t              logicalAddress;
};

#define ATFI_DISCONNECT_CFM (ATFI_SIGBASE + 14) /* !- SIGNO(struct atfiDisconnectCfmS) -! */
struct atfiDisconnectCfmS {
	uint32_t              sigNo;
	struct atfiAddrInfoS  addrInfo;
};

#define ATFI_DISCONNECT_REJ (ATFI_SIGBASE + 15) /* !- SIGNO(struct atfiDisconnectRejS) -! */
struct atfiDisconnectRejS {
	uint32_t              sigNo;
	struct atfiAddrInfoS  addrInfo;
	uint16_t              errorCode;
};

#define ATFI_DISCONNECT_IND (ATFI_SIGBASE + 18) /* !- SIGNO(struct atfiDisconnectIndS) -! */
struct atfiDisconnectIndS {
	uint32_t              sigNo;
	struct atfiAddrInfoS  addrInfo;
	uint16_t              logicalAddress;
};

#define ATFI_RESET_REQ (ATFI_SIGBASE + 16) /* !- SIGNO(struct atfiResetReqS) -! */
struct atfiResetReqS {
	uint32_t              sigNo;
	struct atfiAddrInfoS  addrInfo;
};

#define ATFI_RESET_CFM (ATFI_SIGBASE + 17) /* !- SIGNO(struct atfiResetCfmS) -! */
struct atfiResetCfmS {
	uint32_t              sigNo;
	struct atfiAddrInfoS  addrInfo;
};

#define ATFI_RESET_REJ (ATFI_SIGBASE + 57) /* !- SIGNO(struct atfiResetRejS) -! */
struct atfiResetRejS {
	uint32_t              sigNo;
	struct atfiAddrInfoS  addrInfo;
	uint16_t              errorCode;
};

#define ATFI_AUDIT_REQ (ATFI_SIGBASE + 23) /* !- SIGNO(struct atfiAuditReqS) -! */
struct atfiAuditReqS {
	uint32_t              sigNo;
	struct atfiAddrInfoS  addrInfo;
	uint16_t              logicalAddress;
};

#define ATFI_AUDIT_CFM (ATFI_SIGBASE + 24) /* !- SIGNO(struct atfiAuditCfmS) -! */
struct atfiAuditCfmS {
	uint32_t              sigNo;
	struct atfiAddrInfoS  addrInfo;
	uint16_t              state;
};

#define ATFI_AUDIT_REJ (ATFI_SIGBASE + 25) /* !- SIGNO(struct atfiAuditRejS) -! */
struct atfiAuditRejS {
	uint32_t              sigNo;
	struct atfiAddrInfoS  addrInfo;
	uint16_t              errorCode;
};

#define ATFI_GET_AU3_PORT_REQ (ATFI_SIGBASE + 33) /* !- SIGNO(struct atfiGetAu3PortReqS) -! */
struct atfiGetAu3PortReqS {
	uint32_t              sigNo;
	struct atfiAddrInfoS  addrInfo;
	uint16_t              logicalAddress;
};

#define ATFI_GET_AU3_PORT_CFM (ATFI_SIGBASE + 34) /* !- SIGNO(struct atfiGetAu3PortCfmS) -! */
struct atfiGetAu3PortCfmS {
	uint32_t              sigNo;
	struct atfiAddrInfoS  addrInfo;
	uint32_t              portId;
};

#define ATFI_GET_AU3_PORT_REJ (ATFI_SIGBASE + 35) /* !- SIGNO(struct atfiGetAu3PortRejS) -! */
struct atfiGetAu3PortRejS {
	uint32_t              sigNo;
	struct atfiAddrInfoS  addrInfo;
	uint16_t              errorCode;
};

#define ATFI_GET_AISG_UNIQUE_ID_REQ (ATFI_SIGBASE + 75) /* !- SIGNO(struct atfiGetAisgUniqueIdReqS) -! */
struct atfiGetAisgUniqueIdReqS {
	uint32_t              sigNo;
	struct atfiAddrInfoS  addrInfo;
	uint16_t              logicalAddress;
};

#define ATFI_GET_AISG_UNIQUE_ID_CFM (ATFI_SIGBASE + 76) /* !- SIGNO(struct atfiGetAisgUniqueIdCfmS) -! */
struct atfiGetAisgUniqueIdCfmS {
	uint32_t              sigNo;
	struct atfiAddrInfoS  addrInfo;
	uint8_t               uniqueHwIdLength;
	uint8_t               uniqueHwId[ATFI_MAX_UNIQUE_HW_ID_LENGTH];
};

#define ATFI_GET_AISG_UNIQUE_ID_REJ (ATFI_SIGBASE + 77) /* !- SIGNO(struct atfiGetAisgUniqueIdRejS) -! */
struct atfiGetAisgUniqueIdRejS {
	uint32_t              sigNo;
	struct atfiAddrInfoS  addrInfo;
	uint16_t              errorCode;
};

#define ATFI_ADD_AISG_MAP2_REQ (ATFI_SIGBASE + 67) /* !- SIGNO(struct atfiAddAisgMap2ReqS) -! */
struct atfiAddAisgMap2ReqS {
	uint32_t              sigNo;
	struct atfiAddrInfoS  addrInfo;
	uint16_t              logicalAddress;
	uint16_t              deviceType;
	uint8_t               uniqueHwIdLength;
	uint8_t               uniqueHwId[ATFI_MAX_UNIQUE_HW_ID_LENGTH];
};

#define ATFI_ADD_AISG_MAP_CFM (ATFI_SIGBASE + 52) /* !- SIGNO(struct atfiAddAisgMapCfmS) -! */
struct atfiAddAisgMapCfmS {
	uint32_t              sigNo;
	struct atfiAddrInfoS  addrInfo;
};

#define ATFI_ADD_AISG_MAP_REJ (ATFI_SIGBASE + 53) /* !- SIGNO(struct atfiAddAisgMapRejS) -! */
struct atfiAddAisgMapRejS {
	uint32_t              sigNo;
	struct atfiAddrInfoS  addrInfo;
	uint16_t              errorCode;
};

#define ATFI_ADD_CPRI_MAP2_REQ (ATFI_SIGBASE + 74) /* !- SIGNO(struct atfiAddCpriMap2ReqS) -! */
struct atfiAddCpriMap2ReqS {
	uint32_t              sigNo;
	struct atfiAddrInfoS  addrInfo;
	uint16_t              logicalAddress;
	uint16_t              cascadeNo;
	uint16_t              physicalAddress;
};

#define ATFI_ADD_CPRI_MAP_CFM (ATFI_SIGBASE + 55) /* !- SIGNO(struct atfiAddCpriMapCfmS) -! */
struct atfiAddCpriMapCfmS {
	uint32_t              sigNo;
	struct atfiAddrInfoS  addrInfo;
};

#define ATFI_ADD_CPRI_MAP_REJ (ATFI_SIGBASE + 56) /* !- SIGNO(struct atfiAddCpriMapRejS) -! */
struct atfiAddCpriMapRejS {
	uint32_t              sigNo;
	struct atfiAddrInfoS  addrInfo;
	uint16_t              errorCode;
};

#define ATFI_GET_PHY_ENDPOINT_REQ (ATFI_SIGBASE + 58) /* !- SIGNO(struct atfiGetPhyEndpointReqS) -! */
struct atfiGetPhyEndpointReqS {
	uint32_t              sigNo;
	struct atfiAddrInfoS  addrInfo;
};

#define ATFI_GET_PHY_ENDPOINT_CFM (ATFI_SIGBASE + 59) /* !- SIGNO(struct atfiGetPhyEndpointCfmS) -! */
struct atfiGetPhyEndpointCfmS {
	uint32_t              sigNo;
	struct atfiAddrInfoS  addrInfo;
	PhyEndPointAddrS      phyEndPointAddr;
	uint32_t              snid;
};

#define ATFI_GET_PHY_ENDPOINT_REJ (ATFI_SIGBASE + 60) /* !- SIGNO(struct atfiGetPhyEndpointRejS) -! */
struct atfiGetPhyEndpointRejS {
	uint32_t              sigNo;
	struct atfiAddrInfoS  addrInfo;
	uint16_t              errorCode;
};

#define ATFI_SETUP_IDCP_A3_REQ (ATFI_SIGBASE + 61) /* !- SIGNO(struct atfiSetupIdcpA3ReqS) -! */
struct atfiSetupIdcpA3ReqS {
	uint32_t              sigNo;
	struct atfiAddrInfoS  addrInfo;
	uint32_t              snidA;
	uint32_t              snidB;
	PhyEndPointAddrS      phyEndPointAddrB;
};

#define ATFI_SETUP_IDCP_A3_CFM (ATFI_SIGBASE + 62) /* !- SIGNO(struct atfiSetupIdcpA3CfmS) -! */
struct atfiSetupIdcpA3CfmS {
	uint32_t              sigNo;
	struct atfiAddrInfoS  addrInfo;
	uint32_t              cepIdB;
};

#define ATFI_SETUP_IDCP_A3_REJ (ATFI_SIGBASE + 63) /* !- SIGNO(struct atfiSetupIdcpA3RejS) -! */
struct atfiSetupIdcpA3RejS {
	uint32_t              sigNo;
	struct atfiAddrInfoS  addrInfo;
	uint16_t              errorCode;
};

#define ATFI_SETUP_IDCP_B3_REQ (ATFI_SIGBASE + 64) /* !- SIGNO(struct atfiSetupIdcpB3ReqS) -! */
struct atfiSetupIdcpB3ReqS {
	uint32_t              sigNo;
	struct atfiAddrInfoS  addrInfo;
	uint32_t              snidA;
	PhyEndPointAddrS      phyEndPointAddrA;
	uint32_t              snidB;
	uint32_t              cepIdB;
};

#define ATFI_SETUP_IDCP_B3_CFM (ATFI_SIGBASE + 65) /* !- SIGNO(struct atfiSetupIdcpB3CfmS) -! */
struct atfiSetupIdcpB3CfmS {
	uint32_t              sigNo;
	struct atfiAddrInfoS  addrInfo;
};

#define ATFI_SETUP_IDCP_B3_REJ (ATFI_SIGBASE + 66) /* !- SIGNO(struct atfiSetupIdcpB3RejS) -! */
struct atfiSetupIdcpB3RejS {
	uint32_t              sigNo;
	struct atfiAddrInfoS  addrInfo;
	uint16_t              errorCode;
};

#define ATFI_RELEASE_IDCP_A_REQ (ATFI_SIGBASE + 68) /* !- SIGNO(struct atfiReleaseIdcpAReqS) -! */
struct atfiReleaseIdcpAReqS {
	uint32_t              sigNo;
	struct atfiAddrInfoS  addrInfo;
	uint32_t              snidA;
	uint32_t              snidB;
	PhyEndPointAddrS      phyEndPointAddrB;
};

#define ATFI_RELEASE_IDCP_A_CFM (ATFI_SIGBASE + 69) /* !- SIGNO(struct atfiReleaseIdcpACfmS) -! */
struct atfiReleaseIdcpACfmS {
	uint32_t              sigNo;
	struct atfiAddrInfoS  addrInfo;
};

#define ATFI_RELEASE_IDCP_A_REJ (ATFI_SIGBASE + 70) /* !- SIGNO(struct atfiReleaseIdcpARejS) -! */
struct atfiReleaseIdcpARejS {
	uint32_t              sigNo;
	struct atfiAddrInfoS  addrInfo;
	uint16_t              errorCode;
};

#define ATFI_RELEASE_IDCP_B_REQ (ATFI_SIGBASE + 71) /* !- SIGNO(struct atfiReleaseIdcpBReqS) -! */
struct atfiReleaseIdcpBReqS {
	uint32_t              sigNo;
	struct atfiAddrInfoS  addrInfo;
	uint32_t              snidA;
	PhyEndPointAddrS      phyEndPointAddrA;
	uint32_t              snidB;
};

#define ATFI_RELEASE_IDCP_B_CFM (ATFI_SIGBASE + 72) /* !- SIGNO(struct atfiReleaseIdcpBCfmS) -! */
struct atfiReleaseIdcpBCfmS {
	uint32_t              sigNo;
	struct atfiAddrInfoS  addrInfo;
};

#define ATFI_RELEASE_IDCP_B_REJ (ATFI_SIGBASE + 73) /* !- SIGNO(struct atfiReleaseIdcpBRejS) -! */
struct atfiReleaseIdcpBRejS {
	uint32_t              sigNo;
	struct atfiAddrInfoS  addrInfo;
	uint16_t              errorCode;
};

#define ATFI_AISG_DEVICE_SCAN_REQ (ATFI_SIGBASE + 78) /* !- SIGNO(struct atfiAisgDeviceScanReqS) -! */
struct atfiAisgDeviceScanReqS {
	uint32_t              sigNo;
	struct atfiAddrInfoS  addrInfo;
	uint16_t              baudrate;
	uint16_t              timeout;
	uint8_t               uniqueHwIdMaskLength;
	uint8_t               uniqueHwIdMask[ATFI_MAX_UNIQUE_HW_ID_LENGTH];
	uint8_t               uniqueHwIdLength;
	uint8_t               uniqueHwId[ATFI_MAX_UNIQUE_HW_ID_LENGTH];
};

#define ATFI_AISG_DEVICE_SCAN_CFM (ATFI_SIGBASE + 79) /* !- SIGNO(struct atfiAisgDeviceScanCfmS) -! */
struct atfiAisgDeviceScanCfmS {
	uint32_t              sigNo;
	struct atfiAddrInfoS  addrInfo;
	uint16_t              result;
	uint16_t              deviceType;
	uint8_t               uniqueHwIdLength;
	uint8_t               uniqueHwId[ATFI_MAX_UNIQUE_HW_ID_LENGTH];
};

#define ATFI_AISG_DEVICE_SCAN_REJ (ATFI_SIGBASE + 80) /* !- SIGNO(struct atfiAisgDeviceScanRejS) -! */
struct atfiAisgDeviceScanRejS {
	uint32_t              sigNo;
	struct atfiAddrInfoS  addrInfo;
	uint16_t              errorCode;
};

#endif
