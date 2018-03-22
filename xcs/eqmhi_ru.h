/* > Description **************************************************************/
/*
 * Copyright Ericsson AB
 *
 * The copyright to the computer programs herein is the property of Ericsson AB.
 * The programs may be used and/or copied only with the written permission from
 * Ericsson AB or in accordance with the terms conditions stipulated in the
 * agreement/contract under which the programs have been supplied.
 *
 ******************************************************************************/
/**
 * @file eqmhi_ru.h
 * @brief RU Equipment Handling service interface
 *
 * This file defines the RU public EQMH interface (EQMHI)
 *
 ******************************************************************************/

#ifdef __cplusplus
extern "C" {
#endif


#ifndef EQMHI_RU_H
#define EQMHI_RU_H


/* > Includes *****************************************************************/
#include <stdint.h>
#include "eqmhi_common.h"

/* > Defines ******************************************************************/

/**
 * @ingroup eqmhi_cfg_type_t For a Xenon DP the @ref eqmhi_boot command will,
 *                           in addition to starting the DP, set up the link.
 *                           This option makes it possible to turn off the link
 *                           setup, see @ref eqmhi_cfg_ru_disable_link.
 */
#define EQMHI_CFG_RU_DISABLE_LINK        (EQMHI_CFG_RU_BASE + 0x0)

/**
 * @ingroup eqmhi_cfg_type_t How often the CPU will ping the DP, see
 *                           @ref eqmhi_cfg_ru_watchdog_interval.
 */
#define EQMHI_CFG_RU_WATCHDOG_INTERVAL   (EQMHI_CFG_RU_BASE + 0x1)

/**
 * @ingroup eqmhi_cfg_type_t How long the CPU will wait for a pong from the DP
 *                           before determining that the link is down,
 *                           see @ref eqmhi_cfg_ru_watchdog_timeout.
 */
#define EQMHI_CFG_RU_WATCHDOG_TIMEOUT    (EQMHI_CFG_RU_BASE + 0x2)

/**
 * @ingroup eqmhi_cfg_type_t The id of the CPU transmit mailbox. This will
 *                           become the DP receive device, see
 *                           @ref eqmhi_cfg_ru_tx_mbox.
 */
#define EQMHI_CFG_RU_TX_MBOX             (EQMHI_CFG_RU_BASE + 0x3)

/**
 * @ingroup eqmhi_cfg_type_t The id of the CPU receive mailbox. This will
 *                           become the DP transmit device, see
 *                           @ref eqmhi_cfg_ru_rx_mbox.
 */
#define EQMHI_CFG_RU_RX_MBOX             (EQMHI_CFG_RU_BASE + 0x4)

/**
 * @ingroup eqmhi_cfg_type_t The shmem area to use for transmit, see
 *                           @ref eqmhi_cfg_ru_tx_shmem.
 */
#define EQMHI_CFG_RU_TX_SHMEM            (EQMHI_CFG_RU_BASE + 0x5)

/**
 * @ingroup eqmhi_cfg_type_t The shmem area to use for receive, see
 *                           @ref eqmhi_cfg_ru_rx_shmem.
 */
#define EQMHI_CFG_RU_RX_SHMEM            (EQMHI_CFG_RU_BASE + 0x6)

/**
 * @ingroup eqmhi_cfg_type_t Specific configuration of a Xenon DP. This will
 *                           copied to the internal memory of the DP before it
 *                           boots, see @ref eqmhi_cfg_ru_dp.
 */
#define EQMHI_CFG_RU_DP                  (EQMHI_CFG_RU_BASE + 0x7)

/* > Type Declarations ********************************************************/

/**
 * @brief For a Xenon DP the @ref eqmhi_boot command will, in addition to
 *        starting the DP, set up the link. This option makes it possible to
 *        turn off the link set up. If this configuration option is omitted,
 *        the link will be set up.
 */
struct eqmhi_cfg_ru_disable_link {
	eqmhi_cfg_type_t type;               /*!< Configuration type */
	uint32_t disable;                    /*!< Disable link setup if non-zero */
};

/** @brief How often the CPU will ping the DP. */
struct eqmhi_cfg_ru_watchdog_interval {
	eqmhi_cfg_type_t type;               /*!< Configuration type */
	uint32_t ms;                         /*!< Interval in milliseconds. */
};

/**
 * @brief How long the CPU will wait for a pong from the DP before determining
 *        that the link is down.
 */
struct eqmhi_cfg_ru_watchdog_timeout {
	eqmhi_cfg_type_t type;               /*!< Configuration type */
	uint32_t ms;                         /*!< Timeout in milliseconds. */
};

/**
 * @brief The id of the CPU transmit mailbox. This will become the DP
 *        receive device.
 */
struct eqmhi_cfg_ru_tx_mbox {
	eqmhi_cfg_type_t type;               /*!< Configuration type */
	uint32_t id;                         /*!< The id of the mailbox. */
};

/**
 * @brief The id of the CPU receive mailbox. This will become the DP
 *        transmit device.
 */
struct eqmhi_cfg_ru_rx_mbox {
	eqmhi_cfg_type_t type;               /*!< Configuration type */
	uint32_t id;                         /*!< The id of the mailbox. */
};

/** @brief The shmem area to use for transmit. */
struct eqmhi_cfg_ru_tx_shmem {
	eqmhi_cfg_type_t type;               /*!< Configuration type */
	uint32_t start;                      /*!< Physical address of start.
                                              Must be aligned to 4096 bytes. */
	uint32_t size;                       /*!< Size of memory area.
                                              Must be aligned to 4096 bytes. */
};

/** @brief The shmem area to use for receive. */
struct eqmhi_cfg_ru_rx_shmem {
	eqmhi_cfg_type_t type;               /*!< Configuration type */
	uint32_t start;                      /*!< Physical address of start.
                                              Must be aligned to 4096 bytes. */
	uint32_t size;                       /*!< Size of memory area.
                                              Must be aligned to 4096 bytes. */
};

/**
 * @brief Specific configuration of a Xenon DP. This will copied to the
 *        internal memory of the DP before it boots.
 */
struct eqmhi_cfg_ru_dp {
	eqmhi_cfg_type_t type;               /*!< Configuration type */
	uint32_t size;                       /*!< Size of the buffer in bytes. */
	uint32_t offset;                     /*!< Offset in DP data memory
                                              where to copy the buffer. */
	void    *buf;                        /*!< A pointer to the buffer. Note
                                              that when calling
                                              @ref eqmhi_get_cfg the caller
                                              must free the returned buffer. */
};

/* > Type Declarations ********************************************************/

/**
 * @brief Holding configuration types and specifications.
 */
struct eqmhi_cfg_entity {
	/** EQM identity. */
	uint32_t eqm_id;

	/** Configuration */
	union
	{
		/** Configuration type. */
		eqmhi_cfg_type_t type;

		struct eqmhi_cfg_dump_file              dump_file;
		struct eqmhi_cfg_ru_disable_link        disable_link;
		struct eqmhi_cfg_ru_watchdog_interval   watchdog_interval;
		struct eqmhi_cfg_ru_watchdog_timeout    watchdog_timeout;
		struct eqmhi_cfg_ru_tx_mbox             tx_mbox;
		struct eqmhi_cfg_ru_rx_mbox             rx_mbox;
		struct eqmhi_cfg_ru_tx_shmem            tx_shmem;
		struct eqmhi_cfg_ru_rx_shmem            rx_shmem;
		struct eqmhi_cfg_ru_dp                  dp;
	} cfg;
};

#endif /* EQMHI_RU_H */

#ifdef __cplusplus
} /* extern "C" */
#endif
