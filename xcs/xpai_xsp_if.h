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

/******************************************************************************
 *
 * Product name:
 *      XPAI/XSP
 *
 * File:
 *      xpai_xsp_if.h
 *
 * Author:
 *      Anders Wallden        4 Jul 2001
 *
 * Description:
 *      This file defines XPAI XSP external interface functions.
 *
 * Reviewed:
 *      2001-12-17, Peter Bergsten (qrapebe)
 *
 * Revision history:
 *      2001-07-04, Anders Wallden (QRAAWAL)
 *              Created.
 *
 *      2001-11-13, Anders Wallden (qraawal)
 *              TR30380: Remove dependency to xp.h.
 *
 *      2002-01-10, Anders Wallden (qraawal)
 *              Updated after code review.
 *
 *      2005-02-22 Fredrik Andersson (qandfre)
 *              Added code for XPSIM.
 *
 *      2005-03-05 Fredrik Andersson (qandfre)
 *              XPSIM development.
 *
 *      2006-04-20 Sven Löfgren (qlofsve)
 *              Updated with FUNC-tags. (Wanja).
 *
 *      2010-06-01 Björn Svensson (ebjorsv)
 *              Serial Peripheral Interface, SPI
 *
 *      2011-02-11 Sergey Venkov (eserven)
 *              Implemented CR5128.
 *
 *      2011-05-11, Sergey Venkov (eserven)
 *              HO27147 Change ss_mode to mode.
 *
 *****************************************************************************/

#ifndef XPAI_XSP_IF_H
#define XPAI_XSP_IF_H

/*----------------------------  Include files  ------------------------------*/

#include "ose.h"
#include "osetypes.h"
#include "rhd-spid-if.h"

#ifdef __cplusplus
extern "C" {
#endif

/*----------------------------  CONSTANTS  ----------------------------------*/

/* Indication sent when data has been received on SPI. */
#define XPAI_SENDRECEIVE_SPI_IND  RHD_SPI_SENDRECEIVE_IND /* !-SIGNO( struct XPAI_SendReceiveSPIIndS )-! */

/* Values for clock polarity */
#define XPAI_XSP_SPI_CLOCK_POLARITY_LOW  SPI_CLOCK_POLARITY_LOW
#define XPAI_XSP_SPI_CLOCK_POLARITY_HIGH SPI_CLOCK_POLARITY_HIGH

/* Values for clock phase */
#define XPAI_XSP_SPI_CLOCK_PHASE_LEADING_EDGE  SPI_CLOCK_PHASE_LEADING_EDGE
#define XPAI_XSP_SPI_CLOCK_PHASE_TRAILING_EDGE SPI_CLOCK_PHASE_TRAILING_EDGE
#define XPAI_XSP_SPI_CLOCK_PHASE_LEADING_WRITE_TRAILING_READ SPI_CLOCK_PHASE_LEADING_WRITE_TRAILING_READ
#define XPAI_XSP_SPI_CLOCK_PHASE_TRAILING_WRITE_LEADING_READ SPI_CLOCK_PHASE_TRAILING_WRITE_LEADING_READ

/* Values for SPI Slave Select (SS) mode */
#define XPAI_XSP_SPI_MODE_NO_AUTO       SPI_MODE_NO_AUTO /* SS is not changed during r/w */
#define XPAI_XSP_SPI_MODE_AUTO          SPI_MODE_AUTO /* SS is driven automaticaly */
#define XPAI_XSP_SPI_MODE_STROBE        SPI_MODE_STROBE /* Manually generated SS strobe */

/* Values for LSB first enable */
#define XPAI_XSP_SPI_MSB_FIRST SPI_MSB_FIRST
#define XPAI_XSP_SPI_LSB_FIRST SPI_LSB_FIRST

/* Values for bidir mode */
#define XPAI_XSP_SPI_BIDIR_NORMAL SPI_BIDIR_NORMAL
#define XPAI_XSP_SPI_BIDIR_SWITCH SPI_BIDIR_SWITCH

/* Values for idle time */
#define XPAI_XSP_SPI_IDLE_ONE_HALF SPI_IDLE_ONE_HALF
#define XPAI_XSP_SPI_IDLE_TWO_HALF SPI_IDLE_TWO_HALF

/* Values for slave select polarity */
#define XPAI_XSP_SPI_SSPOLARIY_LOW  SPI_SSPOLARIY_LOW
#define XPAI_XSP_SPI_SSPOLARIY_HIGH SPI_SSPOLARIY_HIGH

/* Values for bus width */
#define XPAI_XSP_SPI_BUS_WIDTH_3WIRE SPI_BUS_WIDTH_3WIRE
#define XPAI_XSP_SPI_BUS_WIDTH_4WIRE SPI_BUS_WIDTH_4WIRE

/* Values for command mode */
#define XPAI_XSP_SPI_CMD_MODE_WRITE  SPI_CMD_MODE_WRITE
#define XPAI_XSP_SPI_CMD_MODE_READ   SPI_CMD_MODE_READ
#define XPAI_XSP_SPI_CMD_MODE_BIDIR  SPI_CMD_MODE_BIDIR
#define XPAI_XSP_SPI_CMD_MODE_DUPLEX SPI_CMD_MODE_DUPLEX

/* Return value at success. */
#define XPAI_XSP_SPI_OK              SPI_OK

/* Error Codes. */
#define XPAI_XSP_SPI_ILLEGAL_PARAMETER SPI_ILLEGAL_PARAMETER
#define XPAI_XSP_SPI_NOT_SUPPORTED     SPI_NOT_SUPPORTED
#define XPAI_XSP_SPI_OTHER_ERROR       SPI_OTHER_ERROR

/*
 * Signal structs.
 */
/* Indication sent when data has been received on SPI. */
#define XPAI_SendReceiveSPIIndS spi_send_receive_ind

/* Configuration parameters for SPI */
#define XPAI_ConfigSPI_S  spi_config

/*----------------------------  Declaration of Global Variables  ------------*/

/******************************************************************************
 *
 * Global function:
 *      XPAI_ConfSPI
 *
 * Parameters:
 *      slave     -  The SPI slave to configure
 *      configSPI -  Pointer to configuration parameters for slave
 *
 * Return value:
 *      0 if OK.
 *      Error code if value out of range
 *
 * Description:
 *      This function configures the SPI on the WARP3. The configuration
 *      is done per slave device.
 *
 * Side effects:
 *      None.
 *
 *****************************************************************************/
extern U32 XPAI_ConfSPI(U32 slave,
                        struct XPAI_ConfigSPI_S *configSPI);/* !- FUNC -! */

/******************************************************************************
 *
 * Global function:
 *      XPAI_SendReceiveSPI
 *
 * Parameters:
 *      slave       - The SPI slave to access
 *      commandMode - 0=Write, 1=Read, 2=Bidir, 3=Duplex
 *      sendBuffer  - Pointer to send buffer.
 *                    The characters should be justified in each U32 according
 *                    to the lsbFirst parameter in the configuration call.
 *                    The sendBuffer is not used if commandMode is 1=Read.
 *      length      - Total number of bits to transfer, valid range is 1-256.
 *      lengthDir   - Number of bits until direction is changed. Only used
                      if commandMode is 2=Bidir. Valid range is 1-256.
 *
 * Return value:
 *      0 if OK.
 *      Error code if port is not configured or not supported.
 *
 * Description:
 *      This function is used to send or receive characters on the SPI port
 *      on the WARP3. When the characters have been sent and received the
 *      signal XPAI_SENDRECEIVE_SPI_IND, containing the received characters,
 *      is sent to the caller of the function.
 *
 * Side effects:
 *      None.
 *
 *****************************************************************************/
extern U32 XPAI_SendReceiveSPI(U32 slave,/* !- FUNC -! */
                               U32 commandMode,
                               U32 *sendBuffer,
                               U32 length,
                               U32 lengthDir);
#ifdef __cplusplus
}
#endif

#endif /* XPAI_XSP_IF_H */
