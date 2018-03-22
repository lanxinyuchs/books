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
 * @file     ecb_dev.h
 *
 * @brief    EC-bus Device Interface
 *
 *           This file provides an interface for accessing the device (i.e. to
 *           open, close, configure, share, read from and write to the device).
 */

#ifndef ECB_DEV_H_
#define ECB_DEV_H_

#include <stdint.h>
#include <time.h>
#include <sys/inotify.h>

/** Maximum transmission data Unit. */
#define ECB_DEV_MTU     256

/** Maximum Receivable data Unit. */
#define ECB_DEV_MRU     256


/**
 * @brief
 *
 * User defined receive hook called from ecb_dev_receive().
 *
 * @param obj   Pointer to user defined object.
 * @param data  Data received from device.
 * @param size  Size of the received data.
 *
 * @return -1 on error, 0 on receive more, 1 on receive no more.
 *
 */

typedef int (*ecb_dev_rx)(void *obj, uint8_t *data, uint32_t size);

/**
 * @brief
 *
 * User defined monitor hook called from monitoring thread.
 *
 * @param event  Mask with inotify events received
 *
 * @return -1 on error, 0 on OK..
 *
 */

typedef int (*ecb_dev_mon)(uint32_t mask, void *client_ref);

/**
 * @brief
 *
 * Initiate and get handle to device.
 *
 * @param handle  Pointer to pointer which is set to point to handle.
 * @param name    Name of the device file.
 *
 * @return 0 on success, other value on failure.
 *
 */

int ecb_dev_init(void **handle, const char *name);


/**
 * @brief
 *
 * Close device and release handle.
 *
 * @param handle  Pointer to handle to be released.
 *
 * @return 0 on success, other value on failure.
 *
 */

int ecb_dev_shutdown(void *handle);


/**
 * @brief
 *
 * Get exclusive ownership of the device. Other threads will block on
 * ecb_dev_begin() until ecb_dev_end() is called.
 *
 * @param handle  Pointer to handle to be released.
 *
 * @return 0 on success, other value on failure.
 *
 */

int ecb_dev_begin(void *handle);


/**
 * @brief
 *
 * Release exclusive ownership of the device. Other threads will no longer be
 * blocked on ecb_dev_begin().
 *
 * @param handle  Pointer to handle to be released.
 *
 * @return 0 on success, other value on failure.
 *
 */

int ecb_dev_end(void *handle);


/**
 * @brief
 *
 * Read data from the device.
 *
 * @param handle  Pointer to handle.
 * @param data    Pointer to where read data is put.
 * @param size    Pointer to size of data actually read.
 *
 * @return 0 on success, other value on failure.
 *
 */

int ecb_dev_read(void *handle, void *data, uint32_t *size);


/**
 * @brief
 *
 * Write data to the device.
 *
 * @param handle  Pointer to handle.
 * @param data    Pointer to data to be written.
 * @param size    Size of data to be written.
 *
 * @return 0 on success, other value on failure.
 *
 */

int ecb_dev_write(void *handle, const void *data, uint32_t size);


/**
 * @brief
 *
 * Receive frame from the device with time-out.
 *
 * @param handle  Pointer to handle.
 * @param term    Frame terminator character.
 * @param rx      Pointer to user defined receive hook.
 * @param obj     Pointer to user defined object.
 * @param tmo     Frame reception time-out.
 *
 * @return -1 on error, 0 on time-out, 1 on response.
 *
 */

int ecb_dev_receive(void *handle, uint8_t term, ecb_dev_rx rx, void *obj,
		    struct timespec *const tmo);


/**
 * @brief
 *
 * Flush any Input/Output data (i.e. any data not yet read or transmitted).
 *
 * @param handle  Pointer to handle.
 *
 * @return 0 on success, other value on failure.
 *
 */

int ecb_dev_flush(void *handle);


/**
 * @brief
 *
 * Drain any Output data (i.e. wait until all data transmitted).
 *
 * @param handle  Pointer to handle.
 *
 * @return 0 on success, other value on failure.
 *
 */

int ecb_dev_drain(void *handle);


/**
 * @enum ecb_dev_param
 *
 * @brief
 *      Parameter enumerator for use with ecb_dev_prog().
 *
 * @var ecb_dev_param::ECB_DEV_PARAM_BITRATE
 *      'value' points to an uint32_t containing the bitrate.
 *      Supported bit rates are 9600, 38400 and 115200.
 *      115200 is used by default.
 *
 * @var ecb_dev_param::ECB_DEV_PARAM_STOP_BITS
 *      'value' points to an uint32_t containing the number of stop bits.
 *      Supported number of stop bits are 1 and 2. 2 is used by default.
 *
 * @var ecb_dev_param::ECB_DEV_PARAM_MARK_ERRORS
 *      See termios macro PARMRK. 'value' points to an uint32_t. Value 0
 *      disables PARMRK and 1 enables PARMRK. Default is disabled.
 *
 */

enum ecb_dev_param {
	ECB_DEV_PARAM_BITRATE = 1,
	ECB_DEV_PARAM_STOP_BITS = 2,
	ECB_DEV_PARAM_MARK_ERRORS = 3
};


/**
 * @brief
 *
 * Make change to the program.
 *
 * @param handle  Pointer to handle.
 *
 * @return 0 on success, other value on failure.
 *
 */

int ecb_dev_prog(void *handle, enum ecb_dev_param param, const void *value);


/**
 * @brief
 *
 * Set device parameters according to program.
 *
 * @param handle  Pointer to handle.
 *
 * @return 0 on success, other value on failure.
 *
 */

int ecb_dev_set(void *handle);

/**
 * @brief
 *
 * Setup a monitoring of the ECB device
 *
 * @param handle     Pointer to pointer which is set to point to handle.
 *
 * @param mon        Pointer to callback function.
 *
 * @param name       Name of the device file.
 *
 * @param client_ref Pointer to client reference.
 *
 * @return 0 on success, other value on failure.
 *
 */

int ecb_dev_monitor(void **handle, ecb_dev_mon mon, char *name, void *client_ref);

/**
 * @brief
 *
 * Stop monitoring of ECB device
 *
 * @param handle      Pointer to handle.
 *
 * @return 0 on success, other value on failure.
 *
 */

int ecb_dev_unmonitor(void *handle);


/**
 * @struct
 *      ecb_dev_stat_hdlc
 *
 * @brief
 *      Stores HDLC related statistics for this device.
 *
 * @var ecb_hdlc_stat::n_tx
 *      Number of transmitted characters.
 *
 * @var ecb_hdlc_stat::n_rx
 *      Number of received characters.
 *
 * @var ecb_hdlc_stat::n_tx_frames
 *      Number of transmitted frames.
 *
 * @var ecb_hdlc_stat::n_rx_frames
 *      Number of received frames.
 *
 * @var ecb_hdlc_stat::n_rsp_tmo
 *      Number of timeouted responses.
 *
 * @var ecb_hdlc_stat::err_addr
 *      Number of frames discarded due to bad address (not an error).
 *
 * @var ecb_hdlc_stat::err_ctrl
 *      Number of frames discarded due to bad control field.
 *
 * @var ecb_hdlc_stat::err_crc
 *      Number of frames discarded due to bad FCS field.
 *
 * @var ecb_hdlc_stat::err_size
 *      Number of frames discarded due to bad size.
 */

struct ecb_dev_stat_hdlc {
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
 * @enum ecb_dev_stat_opt
 *
 * @brief
 *      Option enumerator for use with ecb_dev_stat_opt().
 *
 * @var ecb_dev_stat_opt::ECB_DEV_STAT_OPT_UPDATE_HDLC
 *      'value' points to an struct ecb_dev_stat_hdlc
 *
 * @var ecb_dev_stat_opt::ECB_DEV_STAT_OPT_READ_HDLC
 *      'value' points to a struct ecb_dev_stat_hdlc.
 *
 * @var ecb_dev_stat_opt::ECB_DEV_STAT_OPT_RESET_HDLC
 *      'value' is not used and may be NULL. Only one
 *      client should call ecb_dev_stat_opt() with that
 *      option to avoid unexpected reset of the counters.
 *
 * @var ecb_dev_stat_opt::ECB_DEV_STAT_OPT_READ_UART
 *      'value' points to a struct serial_icounter_struct.
 *
 */

enum ecb_dev_stat_opt {
	ECB_DEV_STAT_OPT_UPDATE_HDLC = 0,
	ECB_DEV_STAT_OPT_READ_HDLC = 1,
	ECB_DEV_STAT_OPT_RESET_HDLC = 2,
	ECB_DEV_STAT_OPT_READ_UART = 3
};


/**
 * @brief
 *
 * Perform action specified by option
 *
 * @param handle      Pointer to handle.
 *
 * @param option      Species action that shall be executed.
 *
 * @param value       Pointer to the option-specific parameter.
 *
 * @return 0 on success, other value on failure.
 *
 */

int ecb_dev_stat_opt(void *handle, enum ecb_dev_stat_opt option, void *value);

#endif
