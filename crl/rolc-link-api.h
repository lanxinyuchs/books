/* ---------------------------------------------------------------------------
 *
 * © Ericsson AB 2013 All rights reserved.
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
 * @file rolc-link-api.h
 * @brief ROLC link configuration interface description
*/

/** @cond */


#ifndef ROLC_H__
#define ROLC_H__

#include <stdint.h>
#include <itc.h>

struct rolc_if;

#define ROLC_LINKNAME_SIZE			32
/*
 * RU status
 */
#define ROLC_INBANDSTATUS_PFA	0x01 /* Power Failure Alarm */
#define ROLC_INBANDSTATUS_LOF	0x02 /* LOF Alarm */
#define ROLC_INBANDSTATUS_LOS	0x04 /* LOS Alarm */
#define ROLC_INBANDSTATUS_SDI	0x08 /* SAP Defect Indication */
#define ROLC_INBANDSTATUS_RAI	0x10 /* Remote Alarm Indication */

/*
 * RU port
 */
#define ROLC_RUPORT_A		0
#define ROLC_RUPORT_B		1

/*
 * OAM link mode
 */
#define ROLC_MASTER		0
#define ROLC_SLAVE		1

/*
 * OAM link configuration
 * @ri_port: RI port
 * @ri_addr: RI address (aka ECP address and usually equal to cascade number)
 * @mode: local end-point mode
 */
struct rolc_link_config {
	uint32_t ri_port;
	uint32_t ri_addr;
	uint32_t mode;

	uint32_t reserved[16]; /* should be set to 0 */
};
 
/*
 * OAM link state indication
 */
#define ROLC_LINKSTATE_IND				(0x01900405)
struct rolc_linkstate_ind {
	uint32_t msg_no;
	uint32_t link_id;
	int available;
};

/*
 * RU inband message indication
 */
#define ROLC_INBAND_IND					(0x01900406)
struct rolc_inband_ind {
	uint32_t msg_no;
	uint32_t ri_port;
	uint32_t ri_addr;
	uint32_t ru_port; /* see ROLC_RUPORT_XXX */
	uint32_t status; /* bitset, see ROLC_INBANDSTATUS_XXX */
};

/** @endcond */

/**
 * @brief Initialize ROLC interface
 *
 * @param[out] handle - interface handle, should be passed to all further
 * interfaces calls
 *
 * @return 0 - success
 */
int32_t rolc_init(struct rolc_if **handle);

/**
 * @brief Shutdown ROLC interface
 *
 * This is last call to the interface, user should make sure that there are
 * no other outstanding calls to the interface (if there are, behaviour is 
 * undefined)
 *
 * @param[in] handle - interface handle
 */
void rolc_shutdown(struct rolc_if **handle);

/**
 * @brief Setup OAM link 
 *
 * @param[in] handle - interface handle
 * @param[in] name - link name ("hunt path")
 * @param[in] cfg - link configuration
 * @param[out] link_id - link ID
 *
 * After succesful call caller should expect ROLC_LINKSTATE_IND 
 * signal/message to be sent to it.
 * When caller mailbox dissappiers link is destroyed automatically.
 */
int32_t rolc_link_setup(struct rolc_if *handle, const char *name,
		struct rolc_link_config *cfg, uint32_t *link_id);

/**
 * @brief Destroy OAM link 
 *
 * @param[in] handle - interface handle
 * @param[in] link_id - link ID
 */
int32_t rolc_link_destroy(struct rolc_if *handle, uint32_t link_id);

/**
 * @brief Subscribe for inband messages
 *
 * @param[in] handle - interface handle
 * @param[in] ri_port - RI port
 * @param[in] ri_addr - RU address (cascade word)
 *
 * After successful call caller should expect ROLC_INBAND_IND
 * signal/message to be sent to it.
 */
int32_t rolc_subscribe_inband(struct rolc_if *handle, uint32_t ri_port,
		uint32_t ri_addr);

/**
 * @brief Unsubscribe for inband messages
 *
 * @param[in] handle - interface handle
 * @param[in] ri_port - RI port
 * @param[in] ri_addr - RU address (cascade word)
 */
void rolc_unsubscribe_inband(struct rolc_if *handle, uint32_t ri_port,
		uint32_t ri_addr);


/** @cond */

#endif /* !ROLC_H__ */

/** @endcond */
