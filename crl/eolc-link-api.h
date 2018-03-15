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
 * @file eolc-link-api.h
 * @brief EOLC link configuration interface description
*/

/** @cond */

#ifndef _EOLC_LINK_API_H_
#define _EOLC_LINK_API_H_

#include <stdint.h>
#include <itc.h>

struct eolc_if;


/*
 * HW specific EOLC link configuration parameters
 */

struct eolc_g1hw_config {
	uint32_t 	emca_id;
};

struct eolc_g2hw_config {
	uint32_t	emca_id;
	uint8_t		emca_mac[6];
	uint32_t 	emca_pcep;
};

union eolc_link_config {
	struct eolc_g1hw_config g1;
	struct eolc_g2hw_config g2;
};




/*
 * EOLC link state indication. Sent to client's mailbox whenever there is 
 * a link state transition. Following link creation the link state starts out
 * as LINK_DOWN. When (i.e if) the link is established the client will 
 * receive a link up indication.
 */
#define EOLC_LINKSTATE_IND				0x0190041a
#define EOLC_LINK_UP             		1
#define EOLC_LINK_DOWN           		0

struct eolc_linkstate_ind {
	uint32_t	msg_no;
	uint32_t 	link_id;
	int 		state;
};

/** @endcond */


/**
 * @brief Initialize the EOLC link interface
 *
 * @param[out] handle - interface handle, should be passed to all further
 * interface calls
 *
 * @return 0 - success, -errno on failure
 */
int  eolc_link_init(struct eolc_if **handle);

/**
 * @brief Shutdown EOLC interface
 *
 * This is last call to the interface, user should make sure that there are
 * no other outstanding calls to the interface (if there are, behaviour is 
 * undefined)
 *
 * @param[in] handle - interface handle
 */
void eolc_link_shutdown(struct eolc_if **handle);


/**
 * @brief Setup EOLC link 
 *
 * @param[in] handle - interface handle
 * @param[in] name - link name (a.k.a "hunt path")
 * @param[in] cfg - link configuration
 * @param[out] link_id - link ID
 *
 * @return 0 - success, -errno on failure
 *
 * After succesful call caller should expect EOLC_LINKSTATE_IND 
 * message to be sent to it.
 * When caller mailbox disappears link is destroyed automatically.
 */
int eolc_link_create(struct eolc_if *handle, const char *name,
						union eolc_link_config *cfg, uint32_t *link_id);



/**
 * @brief Destroy EOLC link 
 *
 * @param[in] handle - interface handle
 * @param[in] link_id - link ID
 *
 * @return 0 - success, -errno on failure
 */

int eolc_link_destroy(struct eolc_if *handle, uint32_t link_id);

/** @cond */

#endif

/** @endcond */
