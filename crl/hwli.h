/**
 * @file hwli.h
 * @brief HW Log API
 *
 * @copyright Ericsson AB 2016 All rights reserved.
 * The information in this document is the property of Ericsson.  Except
 * as specifically authorized in writing by Ericsson, the receiver of
 * this document shall keep the information contained herein confidential
 * and shall protect the same in whole or in part from disclosure and
 * dissemination to third parties.  Disclosure and disseminations to the
 * receivers employees shall only be made on a strict need to know basis.
 *
 * @par Interface
 * This interface requires that you have initialized itc using itc_init()
 * and created a mailbox for the thread calling this interface. You must
 * not receive itc messages from the hwld daemon in the calling thread,
 * but hwli will only receive messages from the hwld daemon. If you are
 * unsure of which itc messages are claimed in the calling thread, it is
 * recommended that you spawn a separate thread for using this interface.
 *
 * @par Special logid Codes
 * NEW_HW_CONFIG        "000"\n
 * NEW_SW_CONFIG        "003"\n
 * POWER_ON             "004"\n
 * RESTART_COLD_W_TEST  "005"\n
 *
 */

#ifndef HWLI_H_
#define HWLI_H_

/** @cond */
#ifdef __cplusplus
extern "C" {
#endif

/** @endcond */

#include <stdint.h>

/**
 * @name Interface Return Codes
 */
/**@{*/
#define HWLI_FILTERED   1 /**< @brief Message not written because it was filtered out. */
#define HWLI_SUCCESS    0 /**< @brief Operation successful. */
#define HWLI_ERROR     -1 /**< @brief Operation failed. */
#define HWLI_LOG_FULL  -2 /**< @brief HWlog full, entry not written. */
/**@}*/

/**
 * @name HWLI Entry Defines
 */
/**@{*/
#define HWLI_TIMEDATE_SZ      13 /**< @brief Num. of chars. including /0. */
#define HWLI_MESSAGE_SZ       112 /**< @brief Num. of chars. including /0. */
#define HWLI_ID_SZ            4 /**< @brief Num. of chars. including /0. */
/**@}*/

/**
 * @name HWLI Write Filter Options
 * @sa hwli_write
 */
/**@{*/
#define HWLI_NO_FILTER        0 /**< @brief No filter. */
#define HWLI_FILTER_ON_MSG    1 /**< @brief Filter on message. */
#define HWLI_NO_FILTER_ON_MSG 0 /**< @brief No filter on message. */
/**@}*/

/**
 * @struct hwli_entry
 *
 * HW log entry
 *
 * @param id    sequential number
 * @param time  time and date when the fault occurred
 * @param msg   the actual fault message
 *
 */
struct hwli_entry {
	char id[HWLI_ID_SZ]; /**< Entry ID. @sa HWLI_ID_SZ */
	char msg[HWLI_MESSAGE_SZ]; /**< Message size. @sa HWLI_MESSAGE_SZ */
	char time[HWLI_TIMEDATE_SZ]; /**< @sa Entry time and date. HWLI_TIMEDATE_SZ */
};

/** @defgroup group1 Functions
 *  @{
 */

/**
 * Write one entry to hardware log.
 *
 * @param[in] id      identity string (null terminated string of 4 bytes)
 * @param[in] filter  number of log entries to write of a specific log id.
 *                    If the max is exceeded, no new log entry will be made.
 *                    0    = No filtering,
 *                    1-nn = max nn log entries with same id
 *
 * @param[in] filter_on_msg  If not 0, the filter will not only filter on
 *                           the logid, but the exact message string as well.
 *
 * @param[in] msg     message string (null terminated string of 110 bytes)
 *
 * @retval HWLI_SUCCESS Entry was written successfully.
 * @retval HWLI_FILTERED Entry was filtered out.
 * @retval HWLI_ERROR Error has occured, entry was not written.
 * @retval HW_LOG_FULL HWLog is full, entry was not written.
 *
 */
int hwli_write(char *id, uint32_t filter, int filter_on_msg, char *msg);

/**
 * Read the error log.
 *
 * @param[out] entries   HW log entries
 * @param[out] size      number of entries
 *
 * @retval HWLI_SUCCESS Entry read successfully.
 * @retval HWLI_ERROR Error has occured, read failed.
 *
 */
int hwli_readlog(struct hwli_entry **entries, uint32_t *size);
/** @} */

#ifdef __cplusplus
}
#endif

#endif /* !HWLI_H_ */
