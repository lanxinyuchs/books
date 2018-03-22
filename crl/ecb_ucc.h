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
 * @file     ecb_ucc.h
 *
 * @brief    EC-bus Unbalanced operation Connectionless mode Class Interface
 *
 *           This file provides an interface for sending and receiving HDLC
 *           Unnumbered Information command and response frames.
 */

#ifndef ECB_UCC_H_
#define ECB_UCC_H_

#include <stdint.h>
#include <time.h>


/**
 * @struct
 *      ecb_ucc_stat
 *
 * @brief
 *      Stores statistics.
 *
 * @var ecb_ucc_stat::n_tx
 *      Number of transmitted characters.
 *
 * @var ecb_ucc_stat::n_rx
 *      Number of received characters.
 *
 * @var ecb_ucc_stat::n_tx_frames
 *      Number of transmitted frames.
 *
 * @var ecb_ucc_stat::n_rx_frames
 *      Number of received frames.
 *
 * @var ecb_ucc_stat::n_rsp_tmo
 *      Number of timeouted responses.
 *
 * @var ecb_ucc_stat::err_addr
 *      Number of frames discarded due to bad address (not an error).
 *
 * @var ecb_ucc_stat::err_ctrl
 *      Number of frames discarded due to bad control field.
 *
 * @var ecb_ucc_stat::err_crc
 *      Number of frames discarded due to bad FCS field.
 *
 * @var ecb_ucc_stat::err_size
 *      Number of frames discarded due to bad size.
 *
 */

struct ecb_ucc_stat {
	uint32_t n_tx;
	uint32_t n_rx;
	uint32_t n_tx_frames;
	uint32_t n_rx_frames;
	uint32_t n_rsp_tmo;
	uint32_t err_addr;
	uint32_t err_ctrl;
	uint32_t err_crc;
	uint32_t err_size;
};


/**
 * @brief
 *
 * Send UI command and wait for UI response.
 *
 * @param handle         Pointer to handle.
 * @param stat           Where to update statistics.
 * @param addr           HDLC Address.
 * @param cmd_info       Command information field.
 * @param cmd_info_size  Command information field size.
 * @param rsp_info       Where to put response information field.
 * @param rsp_info_size  Where to put response information field size.
 * @param tmo            Response time-out.
 *
 * @return -1 on error, 0 on time-out, 1 on response received.
 *
 */

int ecb_ucc_cmd_rsp(void *handle,
		    struct ecb_ucc_stat *stat,
		    uint8_t addr,
		    void *cmd_info,
		    uint32_t cmd_info_size,
		    void *rsp_info,
		    uint32_t *rsp_info_size,
		    struct timespec *const tmo);


/**
 * @brief
 *
 * Send UI command.
 *
 * @param handle         Pointer to handle.
 * @param stat           Where to update statistics.
 * @param addr           HDLC Address.
 * @param cmd_info       Command information field.
 * @param cmd_info_size  Command information field size.
 *
 * @return -1 on error, 0 otherwise.
 *
 */

int ecb_ucc_cmd(void *handle,
		struct ecb_ucc_stat *stat,
		uint8_t addr,
		void *cmd_info,
		uint32_t cmd_info_size);

#endif
