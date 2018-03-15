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


/** @cond */

#ifndef _ECB_LINK_API_H_
#define _ECB_LINK_API_H_

#include <stdint.h>


/** @endcond */


/**
 * @file ecb-link-api.h
 * @brief EC bus link configuration interface description
@verbatim

The structure below is used to specify configuration parameters when creating
a link. The fields have the following 

    address: The HDLC address of the secondary link partner.
    station: Mode of the local link partner. Primary/secondary.
    name: link name a.k.a "hunt_path".

struct ecb_link_config {
    uint32_t	address;
    uint32_t	station;
    char		name[ECB_LINKNAME_SIZE];
};


Link events are sent to the thread that created the link. The link event is sent 
in this message- structure 

struct ecb_link_event {
    uint32_t	msgno;
    uint32_t	link;
    uint32_t	event;
};

where the fields are

    msgno: The itc- message number
    link : specifies which link the event pertains to. This link
           identifier was returned when the link was created.
    event: Whether link came up or went down.


@endverbatim
*/



#define ECB_LINKNAME_SIZE	    32 ///< Max. allowed link name length in bytes
#define ECB_STATION_PRIMARY		1 ///< Local link partner is a primary station
#define ECB_STATION_SECONDARY	0 ///< Local link partner is a secondary station


/**
 * @brief Configurable link parameters are supplied in this structure when
 * creating a link.
 */

struct ecb_link_config {
	uint32_t	address;
	uint32_t	station;
	char		name[ECB_LINKNAME_SIZE];
};



/**
 * @brief Link events are sent to the client's mailbox in this structure.
 *
 * @param[in] msgno: message number
 * @param[in] link : specifies which link the event pertains to. This link
 *         identifier was returned when the link was created.
 * @param[in] event: link came up/ went down
 *
 *  
 */

struct ecb_link_event {
	uint32_t	msgno;
	uint32_t	link;
	uint32_t	event;
};


#define ECB_LINK_EVENT_MSG	  (0x01900400)  ///< Message number
#define ECB_LINK_STATE_DOWN   0  ///< Link down event
#define ECB_LINK_STATE_UP	  1  ///< Link up event


/**
 * @brief Initialize the interface.
 *
 * @param[out] handle - session handle, to be passed to all further
 * interfaces calls
 *
 * @return 0 - success. 
 * @return < 0 - failure. negative errno is returned.
 */
int  ecb_link_init(void **handle);

/**
 * @brief Shutdown the interface.
 *
 * This is last call to the interface, all links created by the client 
 * will be destroyed and all resources freed.
 *
 * @param[in] handle - interface handle
 */
void ecb_link_shutdown(void *handle);


/**
 * @brief Create a link over the EC bus.
 *
 * @param[in] handle - session handle
 * @param[in] cfg - link configuration structure as above.
 * @param[out] link - link ID cookie returned in case of sucess.
 *
 * After succesful creation of a link the caller will 
 * receive an ECB_LINK_STATE_UP message/signal, once the the link
 * is ready to resolve itc- locate requests.
 * In case the clients mailbox disappears the link is destroyed.
 * 
 * @return 0 - success.
 * @return < 0 - failure. negative errno is returned.
 */
int ecb_link_create(void *handle, struct ecb_link_config *cfg, uint32_t *link);

/**
 * @brief Tear down link and free up all resources.
 *
 * @param[in] handle - session handle
 * @param[in] link - link ID cookie returned by crete
 * 
 * @return 0 - success. 
 * @return < 0 - failure. negative errno is returned.
 */
int  ecb_link_destroy(void *handle, uint32_t link);

/** @cond */

#endif

/** @endcond */
