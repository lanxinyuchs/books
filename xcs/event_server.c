/*
 * Copyright (C) 2015 by Ericsson AB. All rights reserved. The
 * information in this document is the property of Ericsson. Except
 * as specifically authorized in writing by Ericsson, the receiver
 * of this document shall keep the information contained herein
 * confidential and shall protect the same in whole or in part from
 * disclosure and dissemination to third parties. Disclosure and
 * disseminations to the receiver's employees shall only be made on
 * a strict need to know basis.
 */

/*
 *
 * File:
 *      event_server.c
 *
 * Author:
 *      Ove Vesterlund, QOVEVES, (Originally: Anders Janson, QRADAAA)
 *
 * Description:
 *      Implements event subscription similar to  legacy XMR - XP Manager.
 *      Handles distribution of data to several subscribers.
 *
 */

/*
 * For legacy reasons this server don't comply to the design rules
 * for signaling interface. An XPAI client should be able to use this
 * functionality without modification. For that reason the signal interface
 * has been kept identical to XPAI XMR interface, (even though the
 * signal/messages names and stuct names have been changed.)
 */

/*----------------------------  Include files  ------------------------------*/

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <pthread.h>
#include <unistd.h>
#include <signal.h>

#include "itc.h"

#include "evti.h"
#include "event_server_internal.h"

#define TRACEPOINT_PROVIDER com_ericsson_xcs_event_server
#include <tpt_create.h>
#include <tpt.h>

#define SLAVE_MBOX "EVENT_SLAVE-%d"
#define MAILBOX_SIZE 11 /*Main loop + 10 slaves*/
#define EXIT_SIGNAL     0xdeadbeef

/* String comparison made readable. */
#define MATCH(a,b)   (strcmp((a),(b))==0)
#define NOMATCH(a,b) (strcmp((a),(b))!=0)

/*Fix for running on Linux 64-bit*/
#if __SIZEOF_POINTER__ == 8
#    define POINTER_UINT uint64_t
#else
#    define POINTER_UINT uint32_t
#endif

#define EVTI_SLAVE_LOCATE_TMO 500

/*
 *  XMR's data base with tag strings and their subscribers.
 *
 *   tagList
 *      |
 *      v
 *    (TagS)
 *     tag "FOO"
 *     subscriberList ----> (SubscriberS)
 *     lastSubscriber        itc_mbox_id_t swu1      itc_mbox_id_t swu2
 *     next                  next --------->   next -------- . . .  --->NULL
 *       |
 *       v
 *     tag "BAR"
 *     subscriberList ---->  itc_mbox_id_t swu1      itc_mbox_id_t swu2
 *     lastSubscriber        next --------->   next -------- . . .  --->NULL
 *     next
 *       |
 *       :
 *       |
 *       v
 *     NULL
 *
 * It is a double linked list. For each tag element there is a list
 * of one or more subscribers.
 */

/* The subscriber list element. */
struct subscriber {
	itc_mbox_id_t subscriber_mbox_id;   /* The subscriber's mbox id. */
	struct subscriber *next; /* Next subscriber element in list. */
};


/* The tag list element. */

struct tag {
	char tag[EVTI_MAX_TAG_LENGTH];    /* NULL terminated tag string. */
	struct subscriber *subscriber_list; /* List of subscribers. */
	struct subscriber *last_subscriber; /* Last subscriber element. */
	struct tag *next;                   /* Next tag element in list. */
};


/* Referenced messages. */
union itc_msg {
	uint32_t  msgno;
	/* Structs of the public interface */
	EVTI_SubscribeIndS      subscribe_ind;
	EVTI_DistributeIndS     distribute_ind;
	EVTI_DistributeReqS     distribute_req;
	EVTI_DistributeCfmS     distribute_cfm;
	EVTI_DistributeRejS     distribute_rej;
	EVTI_DelivIndS          deliv_ind;
	EVTI_DelivReqS          deliv_req;
	EVTI_DelivCfmS          deliv_cfm;
	EVTI_DelivRejS          deliv_rej;
	/* Internal messages struct */
	evti_slave_config_ind_s slave_config_ind;
	evti_subsc_cmd_req_s    subsc_cmd_req;
	evti_subsc_cmd_cfm_s    subsc_cmd_cfm;
	evti_subsc_cmd_rej_s    subsc_cmd_rej;
	evti_subsc_cmd_ind_s    subsc_cmd_ind;
};

static struct tag *create_tag_element(char *tag);
static struct subscriber *create_subscriber_element(itc_mbox_id_t
                subscriber_mbox_id);

static void send_distribute_cfm(itc_mbox_id_t client_mbox_id,
                                uint32_t num_subscribers,
                                uint32_t result);
static void send_distribute_rej(itc_mbox_id_t client_mbox_id,
                                uint32_t num_subscribers,
                                uint32_t result);

static void handle_subscribe_ind(union itc_msg *in_msg);
static void handle_distribute_req(union itc_msg *in_msg);
static void handle_distribute_ind(union itc_msg *in_msg);
static void handle_attach(itc_mbox_id_t mbox_id);


/* Database pointers.  */
static struct tag *tag_list = NULL;
static struct tag *last_tag = NULL;

static void *evti_slave(void *ptr);
static itc_mbox_id_t evti_main_mbox = ITC_NO_ID;

/******************************************************************************
 *
 * Local function:
 *      create_tag_element
 *
 * Parameters:
 *      tag,  The tag string that is subscribed for.
 *
 * Return value:
 *      Pointer to the new element.
 *
 * Description:
 *      Create a new tag element struct for XMR's data base and initate
 *      its member variables.
 *
 *  Side effects:
 *      None.
 *
 *****************************************************************************/
static struct tag *create_tag_element(char *tag)
{
	struct tag *tag_elem;
	TPT_TRACE(1, STR("Creates a new tag element: tag=%s", tag));

	/* Allocate memory space for a new tag element. */

	tag_elem = (struct tag *) malloc(sizeof(struct tag));
	if (!tag_elem) {
		TPT_INFO("Malloc failed when creating tag element");
		return NULL;
	}

	/* Initiate the element. */

	strcpy(tag_elem->tag, tag);
	tag_elem->subscriber_list = NULL;
	tag_elem->last_subscriber = NULL;
	tag_elem->next = NULL;

	return tag_elem;
} /* end create_tag_element() */


/******************************************************************************
 *
 * Local function:
 *      create_subscriber_element
 *
 * Parameters:
 *      subscriber_mbox_id,  The subscriber's mbox id.
 *
 * Return value:
 *      Pointer to the new element.
 *
 * Description:
 *      Create a new subscriber element struct for XMR's data base and
 *      initate its member variables. Attach to the subscriber.
 *
 *  Side effects:
 *
 *****************************************************************************/
static struct subscriber *create_subscriber_element(
        itc_mbox_id_t subscriber_mbox_id)
{
	struct subscriber *sub_elem;
	TPT_TRACE(1, STR("Creates a new Subscriber element: "
	                 "subscriber_mbox_id=0x%x",
	                 subscriber_mbox_id));

	/* Allocate memory space for a new subscriber element. */

	sub_elem = (struct subscriber *) malloc(sizeof(struct subscriber));
	if (!sub_elem) {
		TPT_INFO("Malloc failed when creating sub element");
		return NULL;
	}

	/* Initiate the element. */

	sub_elem->subscriber_mbox_id = subscriber_mbox_id;
	sub_elem->next = NULL;

	/* Attach to the subscriber. */

	if (subscriber_mbox_id != itc_current_mbox()) {
		(void) itc_monitor(subscriber_mbox_id, NULL);
	}

	return sub_elem;
} /* end create_subscriber_element() */


/******************************************************************************
 *
 * Local function:
 *      send_distribute_cfm
 *
 * Parameters:
 *      client_mbox_id,  The distributing client's mbox id.
 *      num_subscribers, Number of subscribers of the tag.
 *      result,  The result of an action.
 *
 * Return value:
 *      None.
 *
 * Description:
 *      Sends a EVTI_DISTRIBUTE_CFM message to the distributing client.
 *
 *  Side effects:
 *      None.
 *
 *****************************************************************************/
static void send_distribute_cfm(itc_mbox_id_t client_mbox_id,
                                uint32_t num_subscribers,
                                uint32_t result)
{
	union itc_msg *out_msg = NULL;

	/* Allocate and send the specified message. */

	out_msg = itc_alloc(sizeof(struct EVTI_DistributeCfmS),
	                    EVTI_DISTRIBUTE_CFM);
	out_msg->distribute_cfm.noOfSubsc = num_subscribers;
	out_msg->distribute_cfm.result = result;

	TPT_SEND_SIG(out_msg->msgno, client_mbox_id, "EVTI_DISTRIBUTE_CFM");
	TPT_TRACE(1, STR("EVTI_DistributeCfm: noOfSubsc=%d, result=%d",
	                 out_msg->distribute_cfm.noOfSubsc,
	                 out_msg->distribute_cfm.result));

	itc_send(&out_msg, client_mbox_id, ITC_MY_MBOX);
} /* end send_distribute_cfm() */


/******************************************************************************
 *
 * Local function:
 *      send_distribute_rej
 *
 * Parameters:
 *      client_mbox_id,  The distributing client's mbox id.
 *      num_subscribers,  Number of subscribers of the tag.
 *      result,  The error cause why the distribution failed.
 *
 * Return value:
 *      None.
 *
 * Description:
 *      Sends a EVTI_DISTRIBUTE_REJ message to the distributing client.
 *
 *  Side effects:
 *      None.
 *
 *****************************************************************************/
static void send_distribute_rej(itc_mbox_id_t client_mbox_id,
                                uint32_t num_subscribers,
                                uint32_t result)
{
	union itc_msg *out_msg = NULL;
	/* Allocate and send the specified message. */

	out_msg = itc_alloc(sizeof(struct EVTI_DistributeRejS),
	                    EVTI_DISTRIBUTE_REJ);
	out_msg->distribute_rej.noOfSubsc = num_subscribers;
	out_msg->distribute_rej.result = result;

	TPT_SEND_SIG(out_msg->msgno, client_mbox_id,
	             STR("evti_DistributeRej: noOfSubsc=%d, result=%d",
	                 out_msg->distribute_rej.noOfSubsc,
	                 out_msg->distribute_rej.result));

	itc_send(&out_msg, client_mbox_id, ITC_MY_MBOX);
} /* end send_distribute_rej() */


/******************************************************************************
 *
 * Local function:
 *      handle_subscribe_ind
 *
 * Parameters:
 *      in_msg,  Received request message.
 *
 * Return value:
 *      None.
 *
 * Description:
 *      Add the sender as a subscriber to the tag.
 *
 *  Side effects:
 *      Modifies the global tag list, but since this function is only called
 *      from the main process in XMR, no other modifications could be made
 *      simultaneously.
 *
 *****************************************************************************/
static void handle_subscribe_ind(union itc_msg *in_msg)
{
	struct tag *tag_elem = NULL;
	struct subscriber *sub_elem = NULL;
	char *tag = in_msg->subscribe_ind.tag;
	itc_mbox_id_t subscriber_mbox_id = itc_sender(in_msg);

	TPT_TRACE(1, STR("The sender 0x%x wants to subscribe for tag '%s'.",
	                 subscriber_mbox_id, tag));

	/* Check the TAG string length. */

	if (strlen(tag) >= EVTI_MAX_TAG_LENGTH) {
		TPT_INFO("Subscribing TAG is too long. No subscription made!!!");
		return;
	}

	if (tag_list == NULL) {
		/* No tags have been registered in XMR yet.
		 * Create the first list element.
		 */
		TPT_TRACE(1, "The tag list is empty. Creates it now.");

		tag_list = create_tag_element(tag); /* First tag element created. */
		last_tag = tag_list;                /* Update the last pointer. */

		/* Register the sender as the tag's first subscriber and
		 * update the last pointer.
		 */
		tag_list->subscriber_list =
		        create_subscriber_element(subscriber_mbox_id);
		tag_list->last_subscriber =
		        tag_list->subscriber_list;

		return;
	}

	/* The tag list is not empty.
	 * Check if this tag has been registered earlier.
	 */
	tag_elem = tag_list;

	while (tag_elem != NULL) {
		if (MATCH(tag, tag_elem->tag)) {
			/* The tag exists in the list. Check if the sender
			 * already has subscribed for this tag.
			 */
			sub_elem = tag_elem->subscriber_list;

			while (sub_elem != NULL) {
				if (subscriber_mbox_id ==
				    sub_elem->subscriber_mbox_id) {
					break; /*while (sub_elem != NULL)*/
				}
				sub_elem = sub_elem->next;
			}
			break; /* while (tag_elem != NULL). */
		}
		tag_elem = tag_elem->next;
	}

	/* Update data base or not? */

	if (tag_elem == NULL) {
		/* The tag list did not contain the specified tag string that
		 * the sender wants to subscribe for. Add both a new tag
		 * element and subscriber element to the end of the list.
		 */

		TPT_TRACE(1, STR("The tag '%s' was not found in list."
		                 " Creates it now.", tag));

		/* Add tag element to end of list. */
		last_tag->next = create_tag_element(tag);
		/* Update the last pointer. */
		last_tag = last_tag->next;

		/* Register the sender as the tag's first subscriber and
		 * update the last pointer.
		 */
		last_tag->subscriber_list =
		        create_subscriber_element(subscriber_mbox_id);
		last_tag->last_subscriber =
		        last_tag->subscriber_list;
	} else if (sub_elem == NULL) {
		/* The sender has not done a previous subscription to the tag.
		 * Add it to the end of the subscriber list and update the last
		 * pointer.
		 */
		tag_elem->last_subscriber->next =
		        create_subscriber_element(subscriber_mbox_id);
		tag_elem->last_subscriber =
		        tag_elem->last_subscriber->next;
	} else {
		/* The sender is already a subscriber. Do nothing. */
		TPT_INFO(STR("The sender 0x%08x is already a subscriber."
		             " Ignoring it.",
		             subscriber_mbox_id));
	}

	return;
} /* end handle_subscribe_ind() */


/******************************************************************************
 *
 * Local function:
 *      handle_distribute_req
 *
 * Parameters:
 *      in_msg,  Received request message.
 *
 * Return value:
 *      None.
 *
 * Description:
 *      Delivers the distributed tag and data from the EVTI_DISTRIBUTE_REQ
 *      message to all subscribers of the specified tag.
 *      This function creates a new slave process which collects and counts
 *      responses from subscribers. The slave process is responsible for
 *      reporting back to the distributing client.
 *
 *  Side effects:
 *      A shortlived dynamic process is created.
 *
 *****************************************************************************/
static void handle_distribute_req(union itc_msg *in_msg)
{
	union itc_msg *out_msg = NULL;
	union itc_msg *config_msg = NULL;
	union itc_msg *locate_msg = NULL;
	uint32_t locate_filter[] = {1, EVTI_SLAVE_LOCATE};
	struct tag *tag_elem;
	struct subscriber *sub_elem;
	itc_mbox_id_t slave_mbox_id;
	uint32_t num_subscribers = 0;
	char *tag = in_msg->distribute_req.tag;
	size_t data_len = strlen(in_msg->distribute_req.data);
	pthread_t slave_tread;
	int res;
	static POINTER_UINT slave_id = 0;
	char slave_name[50];

	if (tag_list == NULL) {
		/* No tags have been registered in XMR yet.
		 * Confirm the request with a EVTI_DISTRIBUTE_CFM,
		 * with zero subscribers, back to the client.
		 */
		TPT_TRACE(1, "No subscribers exists yet.");
		send_distribute_cfm(itc_sender(in_msg), 0, EVTI_TEST_PASSED);
		return;
	}

	/* The tag list is not empty.
	 * Check if this tag has been registered earlier and if so, count the
	 * number of its subscribers.
	 */

	tag_elem = tag_list;

	while (tag_elem != NULL) {
		if (MATCH(tag, tag_elem->tag)) {
			/* The tag exists in the list. Count the number of its
			   subscribers. */
			sub_elem = tag_elem->subscriber_list;
			while (sub_elem != NULL) {
				num_subscribers++;
				sub_elem = sub_elem->next;
			}
			break; /* Jump out of loop when tag is found. */
		}
		tag_elem = tag_elem->next;
	}

	if (num_subscribers == 0) {
		/* No subscribers of the tag exists.
		 * Confirm the request with a EVTI_DISTRIBUTE_CFM, with zero
		 * subscribers, back to the client.
		 */
		TPT_TRACE(1, STR("No subscribers found for tag '%s'.", tag));
		send_distribute_cfm(itc_sender(in_msg), 0, EVTI_TEST_PASSED);
		return;
	}

	/* Create the XMR slave process.
	 * The slave is configured with the number of subscribers and the
	 * distributing client's mbox id by using it as the sender of the
	 * configuration message.
	 */

	res = pthread_create( &slave_tread, NULL,
	                      evti_slave, (void *)++slave_id);
	if(res) {
		TPT_ERROR(STR("Failed to create slave thread (error:%d)", res));
		return; /*Should we crash here?*/
	}
	sprintf(slave_name, SLAVE_MBOX, (uint32_t) slave_id);
	locate_msg = itc_alloc(sizeof(uint32_t), EVTI_SLAVE_LOCATE);
	itc_locate_async( slave_name, &locate_msg, ITC_MY_MBOX );
	locate_msg = itc_receive(locate_filter,
	                         EVTI_SLAVE_LOCATE_TMO,
	                         ITC_FROM_ALL);
	if(!locate_msg) {
		TPT_ERROR(STR("Failed to locate the mailbox %s belonging "
		              "to the started thread",
		              slave_name));
		return; /*Should we crash here?*/
	}
	slave_mbox_id = itc_sender(locate_msg);
	itc_free(&locate_msg);

	res = pthread_detach(slave_tread);
	if(res) {
		TPT_ERROR(STR("Failed to detach slave thread (error:%d)", res));
		return; /*Should we crash here?*/
	}

	config_msg = itc_alloc(sizeof(struct evti_slave_config_ind),
	                       EVTI_SLAVE_CONFIG_IND);
	config_msg->slave_config_ind.num_subscribers = num_subscribers;

	TPT_SEND_SIG(config_msg->msgno, slave_mbox_id, "EVTI_SLAVE_CONFIG_IND");
	TPT_TRACE(1, STR("evti_slave_config_ind: num_subscribers=%d",
	                 num_subscribers));

	itc_send(&config_msg, slave_mbox_id, itc_sender(in_msg)); /* Configure it. */

	/* Distribute the tag and data to each subscriber in a EVTI_DELIV_REQ
	 * message with the Slave as the sender. The slave process is responsible
	 * for reporting back with a EVTI_DISTRIBUTE_CFM/REJ message to the
	 * distributing client.
	 */

	for (sub_elem = tag_elem->subscriber_list;
	     sub_elem != NULL;
	     sub_elem = sub_elem->next) {
		TPT_TRACE(1, STR("Delivering tag '%s' to subscriber 0x%x.",
		                 tag, sub_elem->subscriber_mbox_id));

		out_msg = itc_alloc(sizeof(struct EVTI_DelivReqS) + data_len,
		                    EVTI_DELIV_REQ);
		strcpy(out_msg->deliv_req.tag,
		       in_msg->distribute_req.tag);
		strcpy(out_msg->deliv_req.data,
		       in_msg->distribute_req.data);

		TPT_SEND_SIG(out_msg->msgno, sub_elem->subscriber_mbox_id,
		             "EVTI_DELIV_REQ");
		itc_send(&out_msg, sub_elem->subscriber_mbox_id, slave_mbox_id);
	}

	return;
} /* end handle_distribute_req() */


/******************************************************************************
 *
 * Local function:
 *      handle_distribute_ind
 *
 * Parameters:
 *      in_msg,  Received request message.
 *
 * Return value:
 *      None.
 *
 * Description:
 *      Delivers the distributed tag and data from the EVTI_DISTRIBUTE_IND
 *      message to all subscribers of the specified tag.
 *
 *  Side effects:
 *      None.
 *
 *****************************************************************************/
static void handle_distribute_ind(union itc_msg *in_msg)
{
	union itc_msg *out_msg = NULL;
	struct tag *tag_elem = tag_list;
	struct subscriber *sub_elem;
	uint32_t num_subscribers = 0;
	char *tag = in_msg->distribute_ind.tag;
	size_t data_len = strlen(in_msg->distribute_ind.data);

	if (tag_list == NULL) {
		/* No tags have been registered in XMR yet. */

		TPT_TRACE(1, "No subscribers exists yet.");
		return;
	}

	/* The tag list is not empty.
	 * Check if this tag has been registered earlier and if so, count the
	 * number of its subscribers.
	 */

	tag_elem = tag_list;

	while (tag_elem != NULL) {
		if (MATCH(tag, tag_elem->tag)) {
			/* The tag exists in the list. Count the number of its
			   subscribers. */

			sub_elem = tag_elem->subscriber_list;

			while (sub_elem != NULL) {
				num_subscribers++;
				sub_elem = sub_elem->next;
			}
			break; /* Jump out of loop when tag is found. */
		}
		tag_elem = tag_elem->next;
	}

	if (num_subscribers == 0) {
		/* No subscribers of the tag exists. */

		TPT_TRACE(1, STR("No subscribers found for tag '%s'.", tag));
		return;
	}

	/* Distribute the tag and data to each subscriber in a EVTI_DELIV_IND
	 * msgnal.
	 */

	for (sub_elem = tag_elem->subscriber_list;
	     sub_elem != NULL;
	     sub_elem = sub_elem->next) {
		TPT_TRACE(1, STR("Delivering tag '%s' to subscriber 0x%x.",
		                 tag, sub_elem->subscriber_mbox_id));

		out_msg = itc_alloc(sizeof(struct EVTI_DelivIndS) + data_len,
		                    EVTI_DELIV_IND);
		strcpy(out_msg->deliv_ind.tag,
		       in_msg->distribute_ind.tag);
		strcpy(out_msg->deliv_ind.data,
		       in_msg->distribute_ind.data);

		TPT_SEND_SIG(out_msg->msgno, sub_elem->subscriber_mbox_id,
		             STR("EVTI_DelivInd: tag=%s, data=%X...",
		                 out_msg->deliv_ind.tag,
		                 out_msg->deliv_ind.data[0]));

		itc_send(&out_msg, sub_elem->subscriber_mbox_id, ITC_MY_MBOX);
	}

	return;
} /* end handle_distribute_ind() */


/******************************************************************************
 *
 * Local function:
 *      handle_attach
 *
 * Parameters:
 *      mbox_id,  The sender of the monitor/attach message.
 *
 * Return value:
 *      None.
 *
 * Description:
 *      When the attach message is received, remove any subscriptions made by
 *      the sender of the message.
 *
 *****************************************************************************/
static void handle_attach(itc_mbox_id_t mbox_id)
{
	struct tag *tag_elem, *tag_prev_elem, *removed_tag_elem;
	struct subscriber *sub_elem, *sub_prev_elem;

	/* One of the subscribers has been killed or died for some reason.
	 * Remove all subscriptions done by it.
	 */
	tag_elem = tag_list;
	tag_prev_elem = tag_elem; /* Shut up lint. */

	while (tag_elem != NULL) { /* Go through all registered tags. */
		sub_elem = tag_elem->subscriber_list;
		sub_prev_elem = sub_elem; /* Shut up lint. */

		/* Go through all registered subscribers. */
		while (sub_elem != NULL) {
			if (sub_elem->subscriber_mbox_id == mbox_id) {
				/* Remove the subscriber element from the list.*/
				if (sub_elem == tag_elem->subscriber_list) {
					/* It is the first subscriber entry
					   in list. */
					tag_elem->subscriber_list = sub_elem->next;

					if (sub_elem == tag_elem->last_subscriber) {
						/* It is also the last
						   subscriber entry in list. */
						tag_elem->last_subscriber =
						        sub_elem->next;
					}
				} else {
					sub_prev_elem->next = sub_elem->next;

					if (sub_elem == tag_elem->last_subscriber) {
						/* It is the last subscriber
						   entry in list. */
						tag_elem->last_subscriber =
						        sub_prev_elem;
					}
				}
				/* Free the memory space used by the list entry
				 *  and jump out of the while loop.
				 */
				free(sub_elem);
				sub_elem = NULL;
				break;
			}
			sub_prev_elem = sub_elem;   /* Remeber this element. */
			sub_elem = sub_elem->next; /* Check the next. */
		}

		/* Check if the tag still has any subscribers. */

		if (tag_elem->subscriber_list == NULL) {
			/* No more subscribers of the tag exists.
			 * Remove the tag element from the list.
			 */

			if (tag_elem == tag_list) {
				/* It is the first tag entry in list. */

				tag_list = tag_elem->next;
				tag_prev_elem = tag_list;

				if (tag_elem == last_tag) {
					/* It is also the last subscriber entry
					   in list. */

					last_tag = tag_elem->next;
				}
			} else {
				tag_prev_elem->next = tag_elem->next;

				if (tag_elem == last_tag) {
					/* It is the last tag entry in list. */

					last_tag = tag_prev_elem;
				}
			}
			removed_tag_elem = tag_elem;
			tag_elem = tag_elem->next; /* Check the next. */

			/* Free the memory space used by the list entry. */
			free(removed_tag_elem);
			removed_tag_elem = NULL;
		} else {
			tag_prev_elem = tag_elem;   /* Remeber this element. */
			tag_elem = tag_elem->next; /* Check the next. */
		}
	}

	return;
} /* end handle_attach() */


/******************************************************************************
 *
 * Process name:
 *      evti_slave
 *
 *  Description:
 *      Slave thread created when a EVTI_DISTRIBUTE_REQ is received.
 *      Handles collection of EVTI_DELIV_CFM/REJ messages sent by the
 *      subscribing SWUs. The slave thread is responsible for reporting back
 *      with a EVTI_DISTRIBUTE_CFM/REJ message to the distributing client.
 *
 *      The distribution is considered to be successful if all subscribers have
 *      replied with a EVTI_DELIV_CFM message.
 *      The result value in the EVTI_DISTRIBUTE_CFM message is set according
 *      to the outcome of the ordered action.
 *
 *      1, If one or more of the EVTI_DELIV_CFM messages have result set
 *         to failed, then result is set to failed in EVTI_DISTRIBUTE_CFM.
 *
 *      2, If none of the EVTI_DELIV_CFM messages have result set to failed
 *         but one or more of them with result set to interrupted, then
 *         result is set to interrupted in EVTI_DISTRIBUTE_CFM.
 *
 *      3, If all EVTI_DELIV_CFM messages have result set to passed, then
 *         result is set to passed in EVTI_DISTRIBUTE_CFM.
 *
 *      If one or more subscribers replies with a EVTI_DELIV_REJ message,
 *      the distribution is considered to be unsuccessful and the first
 *      received error cause is passed on to the client in a
 *      EVTI_DISTRIBUTE_REJ message.
 *
 *      Notice: A slave thread is never created when a tag is distributed
 *      with EVTI_DISTRIBUTE_IND. In that case XMR just distributes the tag,
 *      and no responses are collected/reported.
 *
 *  Used Global variables:
 *      None.
 *
 *****************************************************************************/
static void *evti_slave(void *ptr)
{
	union itc_msg *in_msg = NULL;
	itc_mbox_id_t my_mbox;
	char my_mbox_name[50];
	uint32_t config[] = {1, EVTI_SLAVE_CONFIG_IND};
	itc_mbox_id_t client_mbox_id = 0;  /* Distributing mailbox. */
	uint32_t num_subscribers = 0;      /* Number of REJs/CFMs to expect. */
	uint32_t reject_msgs = 0;          /* Number of rejects received. */
	uint32_t confirm_msgs = 0;         /* Number of confirms received. */
	uint32_t test_result = EVTI_TEST_PASSED;
	uint32_t error_cause = 0;
	bool     error_cause_set = false;
	sigset_t mask;

	/* Initiate. */
	sprintf(my_mbox_name, SLAVE_MBOX, (uint32_t) ((POINTER_UINT)ptr));
	my_mbox = itc_create_mailbox(my_mbox_name, 0);
	if (my_mbox == ITC_NO_ID) {
		TPT_ERROR(STR("%s: Unable to create ITC mailbox!",
		              my_mbox_name));
	}

	/* block exit signal */
	sigemptyset(&mask);
	sigaddset(&mask, SIGTERM);
	if (pthread_sigmask(SIG_BLOCK, &mask, NULL) == -1) {
		TPT_ERROR(STR("pthread_sigmask failed"));
		return NULL;
	}
	TPT_TRACE(1, STR("Slave process 0x%x started.", itc_current_mbox()));

	/* Receive the slave process' configuration. */

	in_msg = itc_receive(config, ITC_NO_TMO, ITC_FROM_ALL);

	TPT_REC_SIG(in_msg->msgno,
	            STR("evti_slave_config_ind: num_subscribers=%d",
	                in_msg->slave_config_ind.num_subscribers));

	num_subscribers = in_msg->slave_config_ind.num_subscribers;
	client_mbox_id = itc_sender(in_msg);
	itc_free(&in_msg);

	/* evti_slave process, main loop. */

	for (;;) {
		in_msg = itc_receive(ITC_NOFILTER, ITC_NO_TMO, ITC_FROM_ALL);

		/*Sanity check of received message. Both ...CfmS and ...RejS have
		  same size, need just to check one of them.*/
		if( (itc_size(in_msg) != sizeof(EVTI_DelivCfmS))) {
			TPT_INFO(STR("Message of unexpected size received, "
			             "msgno=0x%x, sender=0x%x",
			             in_msg->msgno, itc_sender(in_msg)));
			itc_free(&in_msg);
			continue;
		}

		switch(in_msg->msgno) {
		/* NOTE: This process does not check the tag in
		   the CFM/REJ message. */

		case EVTI_DELIV_CFM:
			TPT_REC_SIG(in_msg->msgno,
			            STR("EVTI_DelivCfm: tag=%s, result=%d",
			                in_msg->deliv_cfm.tag,
			                in_msg->deliv_cfm.result));

			if (in_msg->deliv_cfm.result == EVTI_TEST_FAILED) {
				test_result = EVTI_TEST_FAILED;
			} else if (in_msg->deliv_cfm.result ==
			           EVTI_TEST_INTERRUPTED) {
				if (test_result != EVTI_TEST_FAILED) {
					test_result = EVTI_TEST_INTERRUPTED;
				}
			}
			confirm_msgs++;
			break;

		case EVTI_DELIV_REJ:
			TPT_REC_SIG(in_msg->msgno,
			            STR("EVTI_DelivRej: tag=%s, result=%d",
			                in_msg->deliv_rej.tag,
			                in_msg->deliv_rej.result));

			if (error_cause_set == false) {
				/* Save the error cause of the first reject. */

				error_cause = in_msg->deliv_rej.result;
				error_cause_set = true;
			}
			reject_msgs++;
			break;

		default:
			TPT_INFO(STR("Unexpected message received, "
			             "msgno=0x%x, sender=0x%x",
			             in_msg->msgno, itc_sender(in_msg)));
			break;
		}


		/* Termination condition.
		 * Check if all subscribers have replied back.
		 */
		if (num_subscribers == confirm_msgs + reject_msgs) {
			if (reject_msgs == 0) { /* All subscribers confirmed.*/
				send_distribute_cfm(client_mbox_id,
				                    num_subscribers,
				                    test_result);
			} else {
				/* One or more SWUs sent a REJ.http://internal.ericsson.com/
				            Distribution failed. */
				send_distribute_rej(client_mbox_id,
				                    num_subscribers,
				                    error_cause);
			}

			TPT_TRACE(1, STR("Slave process 0x%x is done,"
			                 " terminating itself.",
			                 itc_current_mbox()));

			itc_free(&in_msg);
			pthread_exit(NULL); /* Suicide, my mission is over. */
		}

		itc_free(&in_msg);
	}
} /* End evti_slave process. */


/******************************************************************************
 *
 * Local function:
 *      handle_subsc_cmd
 *
 * Parameters:
 *      subsc_cmd_mbox, the maibox of the subsc command.
 *
 * Return value:
 *      None.
 *
 * Description:
 *      Send a list aof all subscribers to the subsc command that prints it.
 *
 *  Side effects:
 *      None.
 *
 *****************************************************************************/
static void handle_subsc_cmd(itc_mbox_id_t subsc_cmd_mbox)
{
	struct tag *tag_elem;
	struct subscriber *sub_elem;
	union itc_msg *send_msg;
	uint32_t  num_subscribers;

	int i;

	if (tag_list == NULL) { /*No subscribers exist.*/
		send_msg = itc_alloc(sizeof(evti_subsc_cmd_rej_s),
		                     EVTI_SUBSC_CMD_REJ );
		itc_send(&send_msg, subsc_cmd_mbox, ITC_MY_MBOX);
	}

	send_msg = itc_alloc(sizeof(evti_subsc_cmd_cfm_s),
	                     EVTI_SUBSC_CMD_CFM );
	itc_send(&send_msg, subsc_cmd_mbox, ITC_MY_MBOX);

	/* Traverse the list, look at all tags. */
	tag_elem = tag_list;

	while (tag_elem != NULL) {
		/* For every tag, collect all subscribers and send back. */

		/*First find the number of subscribers*/
		sub_elem = tag_elem->subscriber_list;
		num_subscribers = 0;
		while (sub_elem != NULL) {
			num_subscribers++;
			sub_elem = sub_elem->next;
		}

		/*Allocate a message big enough to hold the subscribers*/
		send_msg = itc_alloc(sizeof(evti_subsc_cmd_ind_s) +
		                     sizeof(itc_mbox_id_t) * num_subscribers,
		                     EVTI_SUBSC_CMD_IND );
		send_msg->subsc_cmd_ind.num_subscribers = num_subscribers;
		snprintf(send_msg->subsc_cmd_ind.tag, EVTI_TAG_SIZE,
		         "%s", tag_elem->tag);

		/*Put all subscribers of this tag into the message*/
		sub_elem = tag_elem->subscriber_list;
		i = 0;
		while (sub_elem != NULL) {
			send_msg->subsc_cmd_ind.subscribers[i] =
			        sub_elem->subscriber_mbox_id;
			i++;
			sub_elem = sub_elem->next;
		}

		itc_send(&send_msg, subsc_cmd_mbox, ITC_MY_MBOX);
		tag_elem = tag_elem->next;
	}
	/*Send a last ind indicating the end of list. */
	send_msg = itc_alloc(sizeof(evti_subsc_cmd_ind_s),
	                     EVTI_SUBSC_CMD_IND );
	send_msg->subsc_cmd_ind.num_subscribers = 0;
	send_msg->subsc_cmd_ind.tag[0] = '\0';
	send_msg->subsc_cmd_ind.subscribers[0] = ITC_NO_ID;
	itc_send(&send_msg, subsc_cmd_mbox, ITC_MY_MBOX);
	return;
} /* end  handle_subsc_cmd() */


/******************************************************************************
 *
 *  Local function :
 *      main_loop
 *
 * Parameters:
 *      None.
 *
 * Return value:
 *      None.
 *
 *  Description:
 *      This is the main loop in Event Server.
 *
 *****************************************************************************/
void main_loop(void)
{
	union itc_msg *in_msg = NULL;

	/* evti, main loop. */
	for (;;) {
		in_msg = itc_receive(ITC_NOFILTER, ITC_NO_TMO, ITC_FROM_ALL);

		switch(in_msg->msgno) {

		case EVTI_SUBSCRIBE_IND:
			if(itc_size(in_msg) <  sizeof(EVTI_SubscribeIndS)) {
				TPT_INFO(STR("Received a EVTI_SUBSCRIBE_IND "
				             "message of invalid size, "
				             "msgno=0x%x, sender=0x%x, size=%d",
				             in_msg->msgno, itc_sender(in_msg),
				             (uint32_t) itc_size(in_msg)));
				break;
			}
			TPT_REC_SIG(in_msg->msgno,
			            STR("EVTI_SubscribeInd: tag=%s",
			                in_msg->subscribe_ind.tag));
			handle_subscribe_ind(in_msg);
			break;

		case EVTI_DISTRIBUTE_IND:
			if(itc_size(in_msg) <  sizeof(EVTI_DistributeIndS)) {
				TPT_INFO(STR("Received a EVTI_DISTRIBUTE_IND "
				             "message of invalid size, "
				             "msgno=0x%x, sender=0x%x, size=%d",
				             in_msg->msgno, itc_sender(in_msg),
				             (uint32_t) itc_size(in_msg)));
				break;
			}
			TPT_REC_SIG(in_msg->msgno,
			            STR("EVTI_DistributeInd: tag=%s, data=%X...",
			                in_msg->distribute_ind.tag,
			                in_msg->distribute_ind.data[0]));
			handle_distribute_ind(in_msg);
			break;

		case EVTI_DISTRIBUTE_REQ:
			if(itc_size(in_msg) <  sizeof(EVTI_DistributeReqS)) {
				TPT_INFO(STR("Received a EVTI_DISTRIBUTE_REQ "
				             "message of invalid size, "
				             "msgno=0x%x, sender=0x%x, size=%d",
				             in_msg->msgno, itc_sender(in_msg),
				             (uint32_t) itc_size(in_msg)));
				break;
			}
			TPT_REC_SIG(in_msg->msgno,
			            STR("EVTI_DistributeReq: tag=%s, data=%X...",
			                in_msg->distribute_req.tag,
			                in_msg->distribute_req.data[0]));
			handle_distribute_req(in_msg);
			break;

		case ITC_MONITOR_DEFAULT_NO:
			TPT_REC_SIG(in_msg->msgno,
			            STR("Attach/Monitor: sender=0x%x",
				        itc_sender(in_msg)));
			handle_attach(itc_sender(in_msg));
			break;

		case EVTI_SUBSC_CMD_REQ:
			if(itc_size(in_msg) <  sizeof(evti_subsc_cmd_req_s)) {
				TPT_INFO(STR("Received a EVTI_SUBSC_CMD_REQ "
				             "message of invalid size, "
				             "msgno=0x%x, sender=0x%x, size=%d",
				             in_msg->msgno, itc_sender(in_msg),
				             (uint32_t) itc_size(in_msg)));
				break;
			}
			TPT_REC_SIG(in_msg->msgno,
			            STR("EVTI_SUBSC_CMD_REQ: sender=0x%x",
			                itc_sender(in_msg)));
			handle_subsc_cmd(itc_sender(in_msg));
			break;

		case EXIT_SIGNAL:
			TPT_INFO("event_server exiting as ordered");
			itc_free(&in_msg);
			return;
		default:
			TPT_INFO(STR("Unexpected message received, "
			             "msgno=0x%x, sender=0x%x",
			             in_msg->msgno, itc_sender(in_msg)));
			break;
		}
		itc_free(&in_msg);
	}
} /* End main_loop() */

static void print_usage()
{
	printf("Usage: event_server <options>\n\n"
	       "Options:\n"
	       "    -h  Display usage information (this message).\n"
	       "    -d  Daemonize the program.\n\n");
}

/**
 * Function exit_handler
 */
static void exit_handler(int sig)
{
	union itc_msg *msg;

	TPT_INFO(STR("Receive signal 0x%X, terminating", sig));
	msg = itc_alloc(sizeof(uint32_t), EXIT_SIGNAL);
	itc_send(&msg, evti_main_mbox, ITC_MY_MBOX);
}

int main( int argc, char **argv )
{
	int daemonize = 0;
	struct sigaction act;

	if (argc > 1) {
		if (strcmp("-d", argv[1]) == 0) {
			daemonize = 1;
		}
		else if (strcmp("-h", argv[1]) == 0) {
			print_usage();
			exit(0);
		}
		else {
			print_usage();
			exit(1);
		}
	}

	/* Initialize logging */

	TPT_INFO("Starting event server");

	if (!daemonize || !daemon(0, 0)) {
		/*
		 * Initialize ITC
		 */
		if (itc_init(MAILBOX_SIZE, ITC_MALLOC,
		             NULL, ITC_NO_NAMESPACE, 0)) {
			TPT_ERROR("Unable to initialize ITC!");
			return -1;
		}
		evti_main_mbox = itc_create_mailbox(EVTI_SERVER_NAME, 0);
		if (evti_main_mbox == ITC_NO_ID) {
			TPT_ERROR(STR("%s: Unable to create ITC mailbox!",
			              EVTI_SERVER_NAME));
		}
		memset(&act, '\0', sizeof(act));
		act.sa_handler = &exit_handler;
		if (sigaction(SIGTERM, &act, NULL) < 0) {
			TPT_ERROR("Failed to install signal exit handler");
			exit(1);
		}
		main_loop();
	}
	return 0;
}
