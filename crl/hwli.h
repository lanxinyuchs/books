/**
 * @file hwli.h
 * @brief HW Log API
 * @details DETAILED DESCRIPTION MISSING
 */
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

#ifndef HWLI_H_
#define HWLI_H_

/** @cond */
#ifdef __cplusplus
extern "C" {
#endif

/**
 * This interface is not yet stable and users should be aware that API
 * primitives may be modified, renamed or even removed completely.
 */

/**
 * This interface requires that you have initialized itc using itc_init()
 * and created a mailbox for the thread calling this interface. You must
 * not receive itc messages from the hwld daemon in the calling thread,
 * but hwli will only receive messages from the hwld daemon. If you are
 * unsure of which itc messages are claimed in the calling thread, it is
 * recommended that you spawn a separate thread for using this interface.
 */

#include <stdint.h>

/**
 * Interface return codes
 */

#define HWLI_FILTERED   1
#define HWLI_SUCCESS    0
#define HWLI_ERROR     -1
#define HWLI_LOG_FULL  -2


/**
 * Interface info
 */
#define HWLI_TIMEDATE_SZ      13     /* XXX characters + /0 */
#define HWLI_MESSAGE_SZ       112    /* XXX characters + /0 */
#define HWLI_ID_SZ            4      /* XXX characters + /0 */

#define HWLI_NO_FILTER        0

#define HWLI_FILTER_ON_MSG    1
#define HWLI_NO_FILTER_ON_MSG 0


/**
 * struct hwli_entry - hw log entry
 *
 * @id: sequential number
 * @time: time and date when the fault occurred
 * @msg: the actual fault message
 *
 */
struct hwli_entry {
	/* uint32_t id; */
	char id[HWLI_ID_SZ];
	char msg[HWLI_MESSAGE_SZ];
	char time[HWLI_TIMEDATE_SZ];
};


/**
 * Write one entry to hardware log.
 *
 * @param[in] id - identity string (null terminated string of 4 bytes)
 * @param[in] filter - number of log entries to write of a specific log id.
 *            If the max is exceeded, no new log entry will be made.
 *              0    = No filtering,
 *              1-nn = max nn log entries with same id
 * @param[in] filter_on_msg - If not 0, the filter will not only filter on
 *            the logid, but the exact message string as well.
 * @param[in] msg - message string (null terminated string of 110 bytes)
 *
 * @return 0 - success
 * @return 1 - filtered
 * @return <0 - error (see HWLI_XXX)
 *
 */
int hwli_write(char *id, uint32_t filter, int filter_on_msg, char *msg);

/**
 * Read the error log.
 *
 * @param[out] entries - HW log entries
 * @param[out] size - number of entries
 *
 * @return 0 - success
 * @return <0 - error (see HWLI_XXX)
 *
 */
int hwli_readlog(struct hwli_entry **entries, uint32_t *size);

#ifdef __cplusplus
}
#endif

#endif /* !HWLI_H_ */
