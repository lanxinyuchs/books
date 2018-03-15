/**
 *   Copyright (C) 2015 by Ericsson AB. All rights reserved. The
 *   information in this document is the property of Ericsson. Except
 *   as specifically authorized in writing by Ericsson, the receiver
 *   of this document shall keep the information contained herein
 *   confidential and shall protect the same in whole or in part from
 *   disclosure and dissemination to third parties. Disclosure and
 *   disseminations to the receiver's employees shall only be made on
 *   a strict need to know basis.
 */
#ifndef __RHD_SPID_IF_H
#define __RHD_SPID_IF_H

#ifdef __cplusplus
extern "C" {
#endif

#include <conn-establish-helper.h>
#include "rhd-msg-base.h"

/**
        @file rhd-spid-if.h
        @brief Message interface for SPI server

        ### General ###

        This header file describes the message interface for SPI server.
        This file needs to be included in any public interface which
        uses SPI signal interface.
*/

/* > Defines ******************************************************************/
#define RHD_SPI_MAILBOX                            "RHD_SPI"
#ifndef __MACHINE_ZYNQMP_MIMO
#define SPI_NOF_CONTROLLERS                         6
#else
#define SPI_NOF_CONTROLLERS                         2
#endif

#define SPI_SERVER_VERSIONS                         1

/** @brief Values for clock polarity */
#define SPI_CLOCK_POLARITY_LOW                      0
#define SPI_CLOCK_POLARITY_HIGH                     1

/** @brief Values for clock phase */
#define SPI_CLOCK_PHASE_LEADING_EDGE                0
#define SPI_CLOCK_PHASE_TRAILING_EDGE               1
#define SPI_CLOCK_PHASE_LEADING_WRITE_TRAILING_READ 2
#define SPI_CLOCK_PHASE_TRAILING_WRITE_LEADING_READ 3

/** @brief Values for SPI Slave Select (SS) mode */
#define SPI_MODE_NO_AUTO                             0 /** @brief SS is not changed during r/w */
#define SPI_MODE_AUTO                                1 /** @brief SS is driven automaticaly */
#define SPI_MODE_STROBE                              2 /** @brief Manually generated SS strobe */

/** @brief Values for LSB first enable */
#define SPI_MSB_FIRST 0
#define SPI_LSB_FIRST 1

/** @brief Values for bidir mode */
#define SPI_BIDIR_NORMAL 0
#define SPI_BIDIR_SWITCH 1

/** @brief Values for idle time */
#define SPI_IDLE_ONE_HALF 0
#define SPI_IDLE_TWO_HALF 1

/** @brief Values for slave select polarity */
#define SPI_SSPOLARIY_LOW  0
#define SPI_SSPOLARIY_HIGH 1

/** @brief Values for bus width */
#define SPI_BUS_WIDTH_3WIRE 0
#define SPI_BUS_WIDTH_4WIRE 1

/** @brief Values for command mode */
#define SPI_CMD_MODE_WRITE  0
#define SPI_CMD_MODE_READ   1
#define SPI_CMD_MODE_BIDIR  2
#define SPI_CMD_MODE_DUPLEX 3

/** @brief Return value at success. */
#define SPI_OK              0

/** @brief Error Codes. */
#define SPI_ILLEGAL_PARAMETER 1
#define SPI_NOT_SUPPORTED     2
#define SPI_OTHER_ERROR       3

/** @brief Configuration parameters for SPI */
struct spi_config {
	uint32_t clockPolarity;
	uint32_t clockPhase;
	uint32_t lsbFirst;
	uint32_t mode;
	uint32_t biDir;
	uint32_t rate;
	uint32_t idle;
	uint32_t port;
	uint32_t ssPolarity;
	uint32_t busWidth;
};

/**
        @def RHD_SPI_MSG_HEADER
        a macro for compulsory header fields
*/
#define RHD_SPI_MSG_HEADER                      \
	uint32_t msgno;                         \
	uint32_t procedure_ref;                 \
	uint32_t connection_ref;
/**
        ### Message numbers ###
        These messages are used for conn establish mechanism.
        Messages structures can be found in conn-establish.h\n
        @verbatim
	#define SPI_CONN_ESTABLISH_REQ  (RHD_SPI_MSG_BASE + 0x01)
	#define SPI_CONN_ESTABLISH_CFM  (RHD_SPI_MSG_BASE + 0x02)
	#define SPI_CONN_ESTABLISH_REJ  (RHD_SPI_MSG_BASE + 0x03)
	#define SPI_CONN_DISCONNECT_REQ (RHD_SPI_MSG_BASE + 0x04)
	#define SPI_CONN_DISCONNECT_CFM (RHD_SPI_MSG_BASE + 0x05)
	#define SPI_CONN_DISCONNECT_REJ (RHD_SPI_MSG_BASE + 0x06)
	#define SPI_CONN_MONITOR_FWD    (RHD_SPI_MSG_BASE + 0x07)
        @endverbatim

        These messages are used in SPI server.\n
        @verbatim
	#define RHD_SPI_SENDRECEIVE_REQ (RHD_SPI_MSG_BASE + 0x08)
	#define RHD_SPI_SENDRECEIVE_CFM (RHD_SPI_MSG_BASE + 0x09)
	#define RHD_SPI_SENDRECEIVE_REJ (RHD_SPI_MSG_BASE + 0x0A)
	#define RHD_SPI_CFG_REQ (RHD_SPI_MSG_BASE + 0x0B)
	#define RHD_SPI_CFG_REJ (RHD_SPI_MSG_BASE + 0x0C)
	#define RHD_SPI_CFG_CFM (RHD_SPI_MSG_BASE + 0x0D)
        @endverbatim
*/

#define SPI_CONN_ESTABLISH_REQ  (RHD_SPI_MSG_BASE + 0x01)
#define SPI_CONN_ESTABLISH_CFM  (RHD_SPI_MSG_BASE + 0x02)
#define SPI_CONN_ESTABLISH_REJ  (RHD_SPI_MSG_BASE + 0x03)
#define SPI_CONN_DISCONNECT_REQ (RHD_SPI_MSG_BASE + 0x04)
#define SPI_CONN_DISCONNECT_CFM (RHD_SPI_MSG_BASE + 0x05)
#define SPI_CONN_DISCONNECT_REJ (RHD_SPI_MSG_BASE + 0x06)
#define SPI_CONN_MONITOR_FWD    (RHD_SPI_MSG_BASE + 0x07)

#define RHD_SPI_SENDRECEIVE_REQ (RHD_SPI_MSG_BASE + 0x08)
#define RHD_SPI_SENDRECEIVE_CFM (RHD_SPI_MSG_BASE + 0x09)
#define RHD_SPI_SENDRECEIVE_REJ (RHD_SPI_MSG_BASE + 0x0A)
#define RHD_SPI_CFG_REQ (RHD_SPI_MSG_BASE + 0x0B)
#define RHD_SPI_CFG_REJ (RHD_SPI_MSG_BASE + 0x0C)
#define RHD_SPI_CFG_CFM (RHD_SPI_MSG_BASE + 0x0D)

/**
        @brief spi send/receive request struct
        Send to the server when a client want to transfer data
*/
struct spi_send_receive_req {
	RHD_SPI_MSG_HEADER
	uint32_t       slave;
	uint32_t       command_mode;
	uint32_t       length;
	uint32_t       lengthDir;
	uint32_t       send_buffer[1];
};

/**
        @brief spi send/receive confirm struct
        Return to the client when the send/receive request succeed
*/
struct spi_send_receive_cfm {
	RHD_SPI_MSG_HEADER
};

/**
        @brief spi send/receive reject struct
        Return to the client when the send/receive request failed
*/
struct spi_send_receive_rej {
	RHD_SPI_MSG_HEADER
	uint32_t       error_code;
};

/**
        @brief spi configure request struct
        Send to the server to configure
*/
struct spi_conf_req {
	RHD_SPI_MSG_HEADER
	uint32_t       slave;
	struct spi_config spi_config;
};

/**
        @brief spi configure reject struct
        Return to the client when the configure request failed
*/
struct spi_conf_rej {
	RHD_SPI_MSG_HEADER
	uint32_t       error_code;
};

/**
        @brief spi configure confirm struct
        Return to the client when the configure request succeed
*/
struct  spi_conf_cfm {
	RHD_SPI_MSG_HEADER
};

/**
        @brief spi indication struct
        Send to the client to indicate data is received on SPI
	Should use legacy signal structure due to compatibility,
	so this message have no connection establish stuff
*/

#define RHD_SPI_SENDRECEIVE_IND (RHD_SPI_MSG_BASE + 0x0F)
struct spi_send_receive_ind {
	uint32_t      sigNo;
	uint32_t       length;
	uint32_t       buffer[1];
};

#define RHD_SPI_STRUCTS \
	struct spi_send_receive_req spi_send_receive_req;\
	struct spi_send_receive_rej spi_send_receive_rej;\
	struct spi_conf_req        spi_config_req;\
	struct spi_conf_rej        spi_config_rej;


#define SPI_CONN_ESTABLISH_MESSAGES_STRUCT(name)              \
	struct conn_establish_msg_numbers  name =                 \
	{                                                         \
		SPI_CONN_ESTABLISH_REQ,                       \
		SPI_CONN_ESTABLISH_CFM,                       \
		SPI_CONN_ESTABLISH_REJ,                       \
		SPI_CONN_DISCONNECT_REQ,                      \
		SPI_CONN_DISCONNECT_CFM,                      \
		SPI_CONN_DISCONNECT_REJ,                      \
		SPI_CONN_MONITOR_FWD                          \
	}
#ifdef __cplusplus
}
#endif

#endif
