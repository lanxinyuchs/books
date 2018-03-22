/* ---------------------------------------------------------------------------
 *
 * © Ericsson AB 2015 All rights reserved.
 * The information in this document is the property of Ericsson.  Except
 * as specifically authorized in writing by Ericsson, the receiver of
 * this document shall keep the information contained herein confidential
 * and shall protect the same in whole or in part from disclosure and
 * dissemination to third parties.  Disclosure and disseminations to the
 * receivers employees shall only be made on a strict need to know basis.
 *
 * ---------------------------------------------------------------------------
 */


#ifndef _IELL_H
#define _IELL_H


#define IEPHI_OK							0
#define IEPHI_MSG_RX_STOPPED				1
#define IEPHI_MSG_POLL_QUEUE_EMPTY			2



#define IEMACI_RET_OK						(0)
#define IPT_IEMACI_PV1          			(1)


#define IEMACI_SERVER_UP_IND            	(0x001B010B)
#define IEMACI_SERVER_DOWN_IND          	(0x001B010C)
#define IEMACI_INIT_SERVICE_CFM         	(0x001B010D)
#define IEMACI_INIT_SERVICE_REJ         	(0x001B010E)
#define IEMACI_TERM_SERVICE_CFM         	(0x001B010F)
#define IEMACI_MSG_FROM_SERVER          	(0x001B0110)
#define IEMACI_RESERVE_ENDPOINT_CFM     	(0x001B0112)
#define IEMACI_RESERVE_ENDPOINT_REJ     	(0x001B0113)
#define IEMACI_RESERVE_ENDPOINT_IND     	(0x001B0114)
#define IEMACI_AUDIT_ENDPOINT_CFM       	(0x001B0117)
#define IEMACI_AUDIT_ENDPOINT_REJ       	(0x001B0118)
#define IEMACI_AUDIT_ENDPOINT_IND       	(0x001B0119)
#define IEMACI_RELEASE_ENDPOINT_CFM     	(0x001B011C)


/**
 * @struct ipt_iemaci_endpoint_t
 * @details Data struct for one reserved endpoint.\n
 */
typedef struct {
    char lport_name[64];
    uint8_t mac_addr[6];
    uint16_t ep_id;
    uint32_t vlan_id;
    uint32_t priority;
} ipt_iemaci_endpoint_t;

/**
 * @struct ipt_iemaci_endpoint_list_t
 * @details A list of endpoints.\n
 * Used when fetching endpoints using ipt_iemaci_get_endpoints().
 */
typedef struct {
    ipt_iemaci_endpoint_t endpoint[8];
    uint32_t num;
} ipt_iemaci_endpoint_list_t;



#endif /* _IELL_H */
