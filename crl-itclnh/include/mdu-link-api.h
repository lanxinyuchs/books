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

/**
 * @file mdu-link-api.h
 * @brief MDU link configuration interface description
*/

/** @cond */

#ifndef _MDU_LINK_API_H_
#define _MDU_LINK_API_H_

#include <stdint.h>
#include <itc.h>

struct mdu_if;



struct mdu_link_config {
	uint16_t	vlan_id;
	uint8_t		peer_mac[6];
};


/*
 * MDU link state indication. Sent to client's mailbox whenever there is 
 * a link state transition. Following link creation the link state starts out
 * as LINK_DOWN. When (i.e if) the link is established the client will 
 * receive a link up indication.
 */
#define MDU_LINKSTATE_IND				0x01900421
#define MDU_LINK_UP             		1
#define MDU_LINK_DOWN           		0

struct mdu_linkstate_ind {
	uint32_t	msg_no;
	uint32_t 	link_id;
	int 		state;
};

/** @endcond */


/**
 * @brief Initialize the MDU link interface
 *
 * @param[out] handle - interface handle, should be passed to all further
 * interface calls
 *
 * @return 0 - success, -errno on failure
 */
int  mdu_link_init(struct mdu_if **handle);

/**
 * @brief Shutdown MDU interface
 *
 * This is last call to the interface, user should make sure that there are
 * no other outstanding calls to the interface (if there are, behaviour is 
 * undefined)
 *
 * @param[in] handle - interface handle
 */
void mdu_link_shutdown(struct mdu_if **handle);


/**
 * @brief Setup MDU link 
 *
 * @param[in] handle - interface handle
 * @param[in] name - link name (a.k.a "hunt path")
 * @param[in] cfg - link configuration
 * @param[out] link_id - link ID
 *
 * @return 0 - success, -errno on failure
 *
 * After succesful call caller should expect MDU_LINKSTATE_IND 
 * message to be sent to it.
 * When caller mailbox disappears link is destroyed automatically.
 */
int mdu_link_create(struct mdu_if *handle, const char *name,
					struct mdu_link_config *cfg, uint32_t *link_id);



/**
 * @brief Destroy MDU link 
 *
 * @param[in] handle - interface handle
 * @param[in] link_id - link ID
 *
 * @return 0 - success, -errno on failure
 */

int mdu_link_destroy(struct mdu_if *handle, uint32_t link_id);

/** @cond */

#endif

/** @endcond */
