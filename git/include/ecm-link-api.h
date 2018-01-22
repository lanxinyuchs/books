/* ---------------------------------------------------------------------------
 *
 * © Ericsson AB 2014 All rights reserved.
 * The information in this document is the property of Ericsson.  Except
 * as specifically authorized in writing by Ericsson, the receiver of
 * this document shall keep the information contained herein confidential
 * and shall protect the same in whole or in part from disclosure and
 * dissemination to third parties.  Disclosure and disseminations to the
 * receivers employees shall only be made on a strict need to know basis.
 *
 * ---------------------------------------------------------------------------
 */

/**
 * @file ecm-link-api.h
 * @brief ECM link configuration interface description
*/

/** @cond */

#ifndef _ECM_LINK_API_H_
#define _ECM_LINK_API_H_

#include <stdint.h>
#include <itc.h>

#define MAC_ADDR_LEN 6
#define MAX_DEVNAME_LEN 32

#define ULH_ECM_NOVLAN   	0x1000

struct ecm_if;


/*
 * HW specific ECM link configuration parameters
 */

struct ecm_link_config {
	uint8_t		dst_mac[MAC_ADDR_LEN];
	uint16_t        dst_vlan;
	char            device[MAX_DEVNAME_LEN];
};




/*
 * ECM link state indication. Sent to client's mailbox whenever there is
 * a link state transition. Following link creation the link state starts out
 * as LINK_DOWN. When (i.e if) the link is established the client will
 * receive a link up indication.
 */
#define ECM_LINKSTATE_IND				0x0190041a
#define ECM_LINK_UP             		1
#define ECM_LINK_DOWN           		0

struct ecm_linkstate_ind {
	uint32_t	msg_no;
	uint32_t 	link_id;
	int 		state;
};

/** @endcond */


/**
 * @brief Initialize the ECM link interface
 *
 * @param[out] handle - interface handle, should be passed to all further
 * interface calls
 *
 * @return 0 - success, -errno on failure
 */
int  ecm_link_init(struct ecm_if **handle);

/**
 * @brief Shutdown ECM interface
 *
 * This is last call to the interface, user should make sure that there are
 * no other outstanding calls to the interface (if there are, behaviour is
 * undefined)
 *
 * @param[in] handle - interface handle
 */
void ecm_link_shutdown(struct ecm_if **handle);


/**
 * @brief Setup ECM link
 *
 * @param[in] handle - interface handle
 * @param[in] name - link name (a.k.a "hunt path")
 * @param[in] cfg - link configuration
 * @param[out] link_id - link ID
 *
 * @return 0 - success, -errno on failure
 *
 * After succesful call caller should expect ECM_LINKSTATE_IND
 * message to be sent to it.
 * When caller mailbox disappears link is destroyed automatically.
 */
int ecm_link_create(struct ecm_if *handle, const char *name,
		    struct ecm_link_config *cfg, uint32_t *link_id);



/**
 * @brief Destroy ECM link
 *
 * @param[in] handle - interface handle
 * @param[in] link_id - link ID
 *
 * @return 0 - success, -errno on failure
 */

int ecm_link_destroy(struct ecm_if *handle, uint32_t link_id);

/** @cond */

#endif

/** @endcond */
