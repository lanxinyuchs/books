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
 * @file     aisg.h
 *
 * @brief    Header file for AISG procedures
 *
 *           The procedures are described in 3GPP TS 25.462
 *
 */

#ifndef AISG_H_
#define AISG_H_

#include <stdint.h>


/**
 * @brief
 *
 * Perform AISG Device Scan procedure
 *
 * @param handle         Handle from ECB library (PARMRK enabled)
 * @param timeout        Time to wait for responses in ms
 * @param cmd_uid        Unique Identifer to put in XID command
 * @param cmd_uid_size   Size of the Unique Identifier
 * @param cmd_mask       Mask to put in XID command
 * @param cmd_mask_size  Size of the Mask
 * @param result         Device Scan result (ATFI code)
 * @param device_type    Device Type from XID response
 * @param rsp_uid        Unique Identifier from XID response (max 19 bytes)
 * @param rsp_uid_size   Size of the Unique Identifer from XID response
 *
 * @return 0 on success, other value on failure.
 *
 */

int aisg_device_scan(void *handle,
                     uint32_t timeout,
                     const uint8_t *cmd_uid,
                     const uint8_t cmd_uid_size,
                     const uint8_t *cmd_mask,
                     const uint8_t cmd_mask_size,
                     uint16_t *result,
                     uint8_t *device_type,
                     uint8_t *rsp_uid,
                     uint8_t *rsp_uid_size);


/**
 * @brief
 *
 * Perform AISG Address Assignment procedure
 *
 * @param handle         Handle from ECB library
 * @param timeout        Time to wait for responses in ms
 * @param addr           HDLC Address to be assigned
 * @param cmd_uid        Unique Identifer to put in XID command
 * @param cmd_uid_size   Size of the Unique Identifier
 * @param device_type    Device Type to put in XID command
 *
 * @return 0 on success, other value on failure.
 *
 */

int aisg_address_assignment(void *handle,
                            uint32_t timeout,
                            uint8_t addr,
                            const uint8_t *cmd_uid,
                            const uint8_t cmd_uid_size,
                            uint8_t device_type);


/**
 * @brief
 *
 * Perform AISG Reset Device procedure
 *
 * @param handle         Handle from ECB library
 * @param timeout        Time to wait for responses in ms
 * @param addr           HDLC Address
 *
 * @return 0 on success, other value on failure.
 *
 */

int aisg_reset_device(void *handle, uint32_t timeout, uint8_t addr);

#endif
