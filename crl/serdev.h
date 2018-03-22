/* COPYRIGHT-ENEA-SRC-R2 *
 **************************************************************************
 * Copyright (C) 2004-2006 by Enea Software AB.
 * All rights reserved.
 *
 * This Software is furnished under a software license agreement and
 * may be used only in accordance with the terms of such agreement.
 * Any other use or reproduction is prohibited. No title to and
 * ownership of the Software is hereby transferred.
 *
 * PROPRIETARY NOTICE
 * This Software consists of confidential information.
 * Trade secret law and copyright law protect this Software.
 * The above notice of copyright on this Software does not indicate
 * any actual or intended publication of such Software.
 **************************************************************************
 * COPYRIGHT-END */
/**
 * @toc Device_Drivers:
 *
 * @file ose_spi/serdev.h
 *
 * @brief Definitions for serial devices.
 *
 */
#ifndef _SERDEV_H
#define _SERDEV_H

#include "osetypes.h"
#include "device.h"

/**
 * @macro DEVICE_TSERIAL_STREAM
 * @brief Manual or polled mode. Reads must be requested one by one.
 */
#define DEVICE_TSERIAL_STREAM 0x1000

/**
 * @macro DEVICE_TSERIAL_PACKET
 * @brief Automatic mode. Reads are automatic and with size of MRU.
 */
#define DEVICE_TSERIAL_PACKET 0x2000

/**
 * @group ConfigurationTags
 * @brief Configuration tags specific to serial device drivers.
 * @member DEVICE_TAGC_BAUDRATE
 * @member DEVICE_TAGC_MTU
 * @member DEVICE_TAGC_MRU
 * @member DEVICE_TAGC_DATABITS
 * @member DEVICE_TAGC_STOPBITS
 * @member DEVICE_TAGC_PARITY
 * @member DEVICE_TAGC_TERMINATOR
 */

/**
 * @macro DEVICE_TAGC_BAUDRATE
 * @brief This tag specifies the baud rate to use.
 */
#define DEVICE_TAGC_BAUDRATE		0x1000

/**
 * @macro DEVICE_TAGC_MTU
 * @brief This tag specifies the Maximum Transfer Unit to use. No write can be
 * larger than this size.
 */
#define DEVICE_TAGC_MTU			0x1001

/**
 * @macro DEVICE_TAGC_MRU
 * @brief This tag specifies the Maximum Receive Unit to use. No read can be
 * larger than this size. In packet mode this is also the size of the packets
 * the driver will try to allocate with the dcAlloc callback.
 */
#define DEVICE_TAGC_MRU			0x1002

/**
 * @macro DEVICE_TAGC_DATABITS
 * @brief This tag specifies the number of data bits to use.
 */
#define DEVICE_TAGC_DATABITS		0x1003

/**
 * @macro DEVICE_TAGC_STOPBITS
 * @brief This tag specifies the number of stop bits to use.
 */
#define DEVICE_TAGC_STOPBITS		0x1004

/**
 * @macro DEVICE_TAGC_PARITY
 * @brief This tag specifies the parity to use, 'O'dd, 'E'ven or 'N'one.
 */
#define DEVICE_TAGC_PARITY		0x1005

/**
 * @macro DEVICE_TAGC_TERMINATOR
 * @brief This tag specifies terminators characters, characters that terminates
 * reception of a packet. The terminator character will be included in the
 * received packet.
 */
#define DEVICE_TAGC_TERMINATOR		0x1006

/**
 * @macro DEVICE_VALNOTERM
 * @brief This tag value (a parameter to the DEVICE_TAGC_TERMINATOR tag)
 * specifies that there should be no terminator characters.
 */
#define DEVICE_VALNOTERM		-1L

#endif /* _SERDEV_H */

