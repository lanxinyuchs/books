/******************************************************************************
 *
 *      COPYRIGHT (C)                 Ericsson Radio Systems AB, Sweden
 *
 *      The copyright to the computer program(s) herein is the property
 *      of Ericsson Radio Systems AB.
 *
 *      The program(s) may be used and/or copied only with the written
 *      permission from Ericsson Radio Systems AB or in accordance with
 *      the terms and conditions stipulated in the agreement/contract
 *      under which the program(s) have been supplied.
 *
 *****************************************************************************/

/**
 *
 * @file     ecb_unc.h
 *
 * @brief    EC-bus Unbalanced operation Normal response mode Class Interface
 *
 *           This file provides an interface for sending and receiving HDLC
 *           command and response frames in Normal Disconnected and Normal
 *           Response Modes.
 */

#ifndef ECB_UNC_H_
#define ECB_UNC_H_

#include <stdint.h>
#include <time.h>


/**
 * @struct
 *      ecb_unc_stat
 *
 * @brief
 *      Stores statistics.
 *
 * @var ecb_unc_stat::n_tx
 *      Number of transmitted characters.
 *
 * @var ecb_unc_stat::n_rx
 *      Number of received characters.
 *
 * @var ecb_unc_stat::err_addr
 *      Number of frames discarded due to bad address (not an error).
 *
 * @var ecb_unc_stat::err_ctrl
 *      Number of frames discarded due to bad control field.
 *
 * @var ecb_unc_stat::err_crc
 *      Number of frames discarded due to bad FCS field.
 *
 * @var ecb_unc_stat::err_size
 *      Number of frames discarded due to bad size.
 */

struct ecb_unc_stat {
	uint32_t n_tx;
	uint32_t n_rx;
	uint32_t err_addr;
	uint32_t err_ctrl;
	uint32_t err_crc;
	uint32_t err_size;
};


/**
 * @brief
 *
 * Send command and wait for response.
 *
 * @param handle         Pointer to handle.
 * @param stat           Where to update statistics or NULL.
 * @param addr           HDLC Address.
 * @param cmd_ctrl       Command control field.
 * @param cmd_info       Command information field.
 * @param cmd_info_size  Command information field size.
 * @param rsp_ctrl       Where to put response control field.
 * @param rsp_info       Where to put response information field.
 * @param rsp_info_size  Where to put response information field size.
 * @param tmo            Response time-out.
 *
 * @return -1 on error, 0 on time-out, 1 on response received.
 *
 */

int ecb_unc_cmd_rsp(void *handle,
		    struct ecb_unc_stat *stat,
		    uint8_t addr,
		    uint8_t cmd_ctrl,
		    void *cmd_info,
		    uint32_t cmd_info_size,
		    uint8_t *rsp_ctrl,
		    void *rsp_info,
		    uint32_t *rsp_info_size,
		    struct timespec *const tmo);


/**
 * @brief
 *
 * Wait for command or response.
 *
 * @param handle     Pointer to handle.
 * @param stat       Where to update statistics or NULL.
 * @param addr       HDLC Address.
 * @param ctrl       Where to put control field.
 * @param info       Where to put information field.
 * @param info_size  Where to put information field size.
 * @param tmo        Response time-out.
 *
 * @return -1 on error, 0 on time-out, 1 on response received.
 *
 */

int ecb_unc_wait(void *handle,
		 struct ecb_unc_stat *stat,
		 uint8_t addr,
		 uint8_t *ctrl,
		 void *info,
		 uint32_t *info_size,
		 struct timespec *const tmo);


/**
 * @brief
 *
 * Send response or command.
 *
 * @param handle     Pointer to handle.
 * @param stat       Where to update statistics or NULL.
 * @param addr       HDLC Address.
 * @param ctrl       Where to put control field.
 * @param info       Where to put information field.
 * @param info_size  Where to put information field size.
 *
 * @return -1 on error, 0 otherwise.
 *
 */

int ecb_unc_send(void *handle,
		 struct ecb_unc_stat *stat,
		 uint8_t addr,
		 uint8_t ctrl,
		 void *info,
		 uint32_t info_size);


/**
 * @struct ecb_unc_opt_prefix
 *
 * @brief
 *      Defines a frame prefix.
 *
 * @var ecb_unc_opt_prefix::size
 *      Size of prefix.
 *
 * @var ecb_unc_opt_prefix::data
 *      Pointer to prefix.
 */

struct ecb_unc_opt_prefix {
	uint32_t  size;
	uint8_t  *data;
};


/** Maximum number of opening flags. */
#define ECB_UNC_OPT_MAX_OFLAGS 4

/** Maximum number of characters in prefix. */
#define ECB_UNC_OPT_MAX_PREFIX 4


/**
 * @enum ecb_unc_opt
 *
 * @brief
 *      Option enumerator for use with ecb_unc_opt().
 *
 * @var ecb_unc_opt::ECB_UNC_OPT_FCS_BIG_ENDIAN
 *      'value' points to an uint32_t containing 0 for little endian or other
 *      value for big endian.
 *
 * @var ecb_unc_opt::ECB_UNC_OPT_N_OFLAGS
 *      'value' points to an uint32_t containing the number of opening flags
 *
 * @var ecb_unc_opt::ECB_UNC_OPT_PREFIX
 *      'value' points to a struct ecb_unc_opt_prefix.
 *
 */

enum ecb_unc_opt {
	ECB_UNC_OPT_FCS_BIG_ENDIAN = 1,
	ECB_UNC_OPT_N_OFLAGS = 2,
	ECB_UNC_OPT_PREFIX = 3
};


/**
 * @brief
 *
 * Modify option.
 *
 * @param handle  Pointer to handle.
 * @param option  Option to modify.
 * @param value   Pointer to value to be set for option.
 *
 * @return -1 on error, 0 otherwise.
 *
 */

int ecb_unc_opt(void *handle,
		enum ecb_unc_opt option,
		const void *value);

#endif
