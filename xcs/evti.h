/**
 * @copyright
 * Copyright (C) 2015 by Ericsson AB. All rights reserved. The
 * information in this document is the property of Ericsson. Except
 * as specifically authorized in writing by Ericsson, the receiver
 * of this document shall keep the information contained herein
 * confidential and shall protect the same in whole or in part from
 * disclosure and dissemination to third parties. Disclosure and
 * disseminations to the receiver's employees shall only be made on
 * a strict need to know basis.
 */

/**
 * @file evti.h
 * @brief Subscription and distribution of "tags"
 *
 * Implements event subscription similar to  legacy XMR - XP Manager.\n
 * Handles distribution of tags (with optional data) to several subscribers.\n
 * \n
 * This interface needs to be kept in OSE/LITS style, as it will be part of the
 * XPAI interface.\n
 * \n
 * Message/signal numbers are (as in XMR) hardcoded here in order to remove
 * the dependency on xp.h.\n
 * EVTI_SIGBASE = 0x100E200\n
 * \n
 * Terms used in the description of this interface:\n
 * "server" is the event server.\n
 * "client" is a process/mailbox that distributes a tag.\n
 * "subscriber" is a process/mailbox that has subsribed to a specific tag.
 */

#ifndef _EVTI_H_
#define _EVTI_H_

#ifdef __cplusplus
extern "C" {
#endif

#include <xcs-sigbase.h>
#if EVTI_MSG_BASE != 0x100E200
#warning "EVTI_MSG_BASE must be 0x100E200!"
#endif

/*----------------------------  CONSTANTS  ----------------------------------*/

/**
 *
 * @def EVTI_SUBSCRIBE_IND
 * Message number for start of subscription.
 *
 * @def EVTI_DISTRIBUTE_IND
 * Message number for asking the server to distribute a tag
 * without expecting a reply.
 *
 * @def EVTI_DISTRIBUTE_REQ
 * Message number for asking the server to distribute a tag
 * expecting a _CFM or _REJ is expected in return.
 *
 * @def EVTI_DISTRIBUTE_CFM
 * Message number sent back to the client when all subscribers gave a positive
 * answer to a distributed tag.
 *
 * @def EVTI_DISTRIBUTE_REJ
 * Message number sent back to the client when at least one subscriber
 * gave a negative answer to a distributed tag.
 *
 * @def EVTI_DELIV_IND
 * Message number for sending a tag to subscribers, no reply is expected.
 *
 * @def EVTI_DELIV_REQ
 * Message number for sending a tag to subscribers,
  * a _CFM or _REJ is expected in return.
 *
 * @def EVTI_DELIV_CFM
 * Message number for returning a positive answer from a subscriber
 *
 * @def EVTI_DELIV_REJ
 * Message number for returning a negative answer from a subscriber
 */
#define EVTI_SUBSCRIBE_IND  (EVTI_MSG_BASE + 0)
#define EVTI_DISTRIBUTE_IND (EVTI_MSG_BASE + 1)
#define EVTI_DISTRIBUTE_REQ (EVTI_MSG_BASE + 2)
#define EVTI_DISTRIBUTE_CFM (EVTI_MSG_BASE + 3)
#define EVTI_DISTRIBUTE_REJ (EVTI_MSG_BASE + 4)
#define EVTI_DELIV_IND      (EVTI_MSG_BASE + 5)
#define EVTI_DELIV_REQ      (EVTI_MSG_BASE + 6)
#define EVTI_DELIV_CFM      (EVTI_MSG_BASE + 7)
#define EVTI_DELIV_REJ      (EVTI_MSG_BASE + 8)

/**
 * @def EVTI_MAX_TAG_LENGTH
 * Maximum length of the tag string.
 */
#define EVTI_MAX_TAG_LENGTH 32

/**
 * @def EVTI_DYN_SIZE
 * Inticator that data size is dynamic:
 * A minimum of 1 character (terminating NULL) is required in 'data' fields!
 *
 * When allocating a signal, use :
 *   'sizeof(the_struct_you_want) + size_of_your_data_string'
 */
#define EVTI_DYN_SIZE (1)

/**
 * @def EVTI_TEST_PASSED
 * Positive result when the distributed tag was a test request.
 * To be used by the subscriber in a EVTI_DELIV_CFM signal and
 * passed on to the client in a EVTI_DISTRIBUTE_CFM signal.
 *
 * @def EVTI_TEST_FAILED
 * Negative result when the distributed tag was a test request.
 * To be used by the subscriber in a EVTI_DELIV_CFM signal and
 * passed on to the client in a EVTI_DISTRIBUTE_CFM signal.
 *
 * @def EVTI_TEST_INTERRUPTED
 * Used to indicate that the test was not completed when the
 * distributed tag was a test request.
 * To be used by the subscriber in a EVTI_DELIV_CFM signal and
 * passed on to the client in a EVTI_DISTRIBUTE_CFM signal.
 *
 */

#define EVTI_TEST_PASSED       0x00
#define EVTI_TEST_FAILED       0x01
#define EVTI_TEST_INTERRUPTED  0x02

/*----------------------------  MACROS  -------------------------------------*/
/**
 * @def EVTI_SERVER_NAME
 * Name of the Event Server mailbox.
 */
#define EVTI_SERVER_NAME "EVENT_SERVER"

/*----------------------------  Structs and typedefs  -----------------------*/

/*
 * Signal structs.
 */


/*
  Keeping camelCase names. struct member names must not change for legacy
  reasons, and having snake_case struct names with camelCase members seems odd.
*/
/**
 * Message struct for starting subscription to specific tag.
 */
typedef struct EVTI_SubscribeIndS {
	uint32_t sigNo;                /**< Message/signal number */
	char tag[EVTI_MAX_TAG_LENGTH]; /**< The tag to subscribe to. */
} EVTI_SubscribeIndS;

/**
 * Message struct that the client sends to the server to distribute a tag
 * to all subscribers without expecting a reply.
 */
typedef struct EVTI_DistributeIndS {
	uint32_t sigNo;                /**< Message/signal number */
	char tag[EVTI_MAX_TAG_LENGTH]; /**< The tag to distribute. */
	char data[EVTI_DYN_SIZE];      /**< Optional data. Variable sized data
                                         string. See EVTI_DYN_SIZE. */
} EVTI_DistributeIndS;

/**
 * Message struct that the client sends to the server to distribute a tag
 * to all subscribers expecting a reply using _CFM or _REJ.
 */
typedef struct EVTI_DistributeReqS {
	uint32_t sigNo;                /**< Message/signal number */
	char tag[EVTI_MAX_TAG_LENGTH]; /**< The tag to distribute. */
	char data[EVTI_DYN_SIZE];      /**< Optional data. Variable sized data
                                         string. See EVTI_DYN_SIZE. */
} EVTI_DistributeReqS;

/**
 * Message struct that the server sends to the client after collecting replies
 * from all subscribers and all subscribers answered with a _CFM.
 */
typedef struct EVTI_DistributeCfmS {
	uint32_t sigNo;     /**< Message/signal number */
	uint32_t noOfSubsc; /**< The number of subscribers of the tag string. */
	uint32_t result;    /**< The result of an action caused by the distribution. */
} EVTI_DistributeCfmS;

/**
 * Message struct that the server sends to the client after collecting replies
 * from all subscribers and any of subscribers answered with a _REJ.
 */
typedef struct EVTI_DistributeRejS {
	uint32_t sigNo;     /**< Message/signal number */
	uint32_t noOfSubsc; /**< The number of subscribers of the tag string. */
	uint32_t result;    /**< The error cause why the distribution failed. */
} EVTI_DistributeRejS;

/**
 * Message struct that the server sends to the subscriber to distribute a tag
 * not expecting any reply.
 */
typedef struct EVTI_DelivIndS {
	uint32_t sigNo;                /**< Message/signal number */
	char tag[EVTI_MAX_TAG_LENGTH]; /**< The tag to distribute. */
	char data[EVTI_DYN_SIZE];      /**< Optional data. Variable sized data
                                         string. See EVTI_DYN_SIZE. */
} EVTI_DelivIndS;

/**
 * Message struct that the server sends to the subscriber to distribute a tag
 * expecting a reply using _CFM or _REJ.
 */
typedef struct EVTI_DelivReqS {
	uint32_t sigNo;                /**< Message/signal number */
	char tag[EVTI_MAX_TAG_LENGTH]; /**< The tag to distribute. */
	char data[EVTI_DYN_SIZE];      /**< Optional data. Variable sized data
                                         string. See EVTI_DYN_SIZE. */
} EVTI_DelivReqS;

/**
 * Message struct that the subscriber send to the server saying that whatever
 * action the distributed tag triggered has succeeded.
 */
typedef struct EVTI_DelivCfmS {
	uint32_t sigNo;                /**< Message/signal number */
	char tag[EVTI_MAX_TAG_LENGTH]; /**< The tag that was distributed. */
	uint32_t result;               /**< The result of an action caused by
                                          the distribution. */
} EVTI_DelivCfmS;

/**
 * Message struct that the subscriber send to the server saying that whatever
 * action the distributed tag triggered has failed.
 */
typedef struct EVTI_DelivRejS {
	uint32_t sigNo;                /**< Message/signal number */
	char tag[EVTI_MAX_TAG_LENGTH]; /**< The tag that was distributed. */
	uint32_t result;               /**< The reason (error code) why the
                                          distribution failed. */
} EVTI_DelivRejS;

#undef EVTI_DYN_SIZE    /* Don't want anyone to think they can use it!! */

#ifdef __cplusplus
}
#endif

#endif /* _EVTI_H_ */
