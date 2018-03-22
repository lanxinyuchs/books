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

#ifndef ECB_UCC_SIM_H_
#define ECB_UCC_SIM_H_

#include <stdint.h>
#include "ucc_sim.h"

/**
 * @brief
 *
 * Send data.
 *
 * @param handle         Pointer to handle.
 * @param addr           HDLC Address.
 * @param data           Response information field.
 * @param size           Response information field size.
 * @param behavior       bit 1 -- simulate UART overrun
 *                       bit 2 -- simulate CRC error
 *                       bit 3 -- simulate CTRL error
 *                       bit 4 -- simulate timeout error
 *                       bit 5 -- simulate address error
 *
 * @return -1 on error, 0 otherwise.
 *
 */

int ecb_ucc_sim_send(void *handle,
                     uint8_t addr,
                     void *rsp_info,
                     uint32_t rsp_info_size,
                     ucc_sim_status behavior);

/**
 * @brief
 *
 * Receive data.
 *
 * @param handle         Pointer to handle.
 * @param addr           HDLC Address.
 * @param cmd_info       Command information field.
 * @param cmd_info_size  Command information field size.
 * @param tmo_sec        Timeout.
 *
 * @return -1 on error, information field otherwise.
 *
 */

int ecb_ucc_sim_receive(void *handle,
                        uint8_t addr,
                        void *cmd_info,
                        uint32_t *cmd_info_size,
                        time_t tmo_sec);

#endif
