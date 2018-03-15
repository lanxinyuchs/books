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
 *      XPP
 *
 * File:
 *      xpai_hdr_i2c_if.h
 *
 * Author:
 *      Jonas Käll        29 Nov 2000
 *
 * Description:
 *      This header file contains the XPAI interface to HDR's I2C driver.
 *
 * Reviewed:
 *      2001-02-16, Rickard Fahlquist (QRAFAST)
 *                  Thomas Lundström (QRALUTH)
 *
 * Revision history:
 *      2002-02-11, Peter Marchall (QPETMAR)
 *              Changed according to TR85196.
 *
 *      2002-02-18, Akbar Rezvanpour (QREZVAK)
 *              Moved to xp.
 *
 *      2002-06-24, Peter Bergsten (QRAPEBE)
 *              Implemented CR:WRNab22287
 *
 *      2003-11-14, Patrik Andersson (EPATAND)
 *              Changed interface for Espresso prodjekt
 *              Added I2C hub ports according to IWD.
 *
 *      2004-06-14 Fredrik Andersson (qandfre)
 *              Added X2_I2CSleepPort() support.
 *
 *      2004-12-07 Fredrik Andersson (qandfre)
 *              Updated according to IR 10/1776-122/FCP 103 4850 Uen.
 *
 *      2005-03-04 Fredrik Andersson (qandfre)
 *              Added code for XPSIM.
 *
 *      2006-04-20 Sven Löfgren (qlofsve)
 *              Updated with FUNC-tags. (Wanja).
 *
 *****************************************************************************/

#ifndef _XPAI_HDR_I2C_IF_H
#define _XPAI_HDR_I2C_IF_H

/*----------------------------  Include files  ------------------------------*/

#include "osetypes.h"

#ifdef __cplusplus
extern "C" {
#endif

/*----------------------------  CONSTANTS  ----------------------------------*/

/* XPSIM stub XPAI functions. */
#ifdef SOFT
#define XPAI_FNO_I2C_READ_PORT      ((U32)XPAI_I2CReadPort)
#define XPAI_FNO_I2C_READ_SUB_PORT  ((U32)XPAI_I2CReadSubPort)
#define XPAI_FNO_I2C_WRITE_PORT     ((U32)XPAI_I2CWritePort)
#define XPAI_FNO_I2C_WRITE_SUB_PORT ((U32)XPAI_I2CWriteSubPort)
#define XPAI_FNO_I2C_SLEEP_PORT     ((U32)X2_I2CSleepPort)
#endif

/*----------------------------  MACROS  -------------------------------------*/

/* Error codes returned by the I2C functions. */

/* This code indicates that an I2C device has locked (acquired) the data bus
 * and that the recovery procedure normally resolving this error has failed.
 * The error is fatal and unrecoverable without power-cycling the device.
 */
#define XPAI_I2C_DATA_BUS_LOCKED   -103

/* This code indicates that the port ID supplied is out of range. More to the
 * point; the ID has no corresponding general IO port defined in the board
 * parameters, resulting in the conclusion that the HW does not support the
 * supplied port ID.
 */
#define XPAI_I2C_WRONG_PORT        -104

/* This code indicates some kind of I2C communication error. A start/stop
 * condition was not met, disturbances caused the I2C access to fail or the
 * request could have been timed out due to wrong device address etc.
 * This error is probably non fatal and recoverable if all static preconditions
 * are met (such as correct device address etc.).
 */
#define XPAI_I2C_OTHER_ERROR       -100


/* IDs for the I2C hub ports.
 * The "port not defined" value is used if the I2C bus is connected directly to
 * the AUC (AUM2), i.e. if the board is not equipped with an I2C hub.
 */
#define XPAI_I2C_PORT_0           (U32)0
#define XPAI_I2C_PORT_1           (U32)1
#define XPAI_I2C_PORT_2           (U32)2
#define XPAI_I2C_PORT_3           (U32)3
#define XPAI_I2C_PORT_NOT_DEFINED (U32)0xffffffff

/*----------------------------  Structs and typedefs  -----------------------*/
/*----------------------------  Declaration of Global Variables  ------------*/

/*----------------------------  Declaration of Global Functions  ------------*/

/******************************************************************************
 *
 * Global function:
 *      XPAI_I2CReadPort
 *
 * Parameters:
 *      portId     - ID of the port to enable on the hub
 *      address    - I2C slave device address
 *      buffer     - buffer used to store the read value
 *      length     - buffer length
 *
 * Return value:
 *      positive value indicates number of bytes read.
 *      negative value indicates error (see error codes above).
 *
 * Description:
 *      Reads from the specified I2C slave device, situated on an I2C bus that
 *      is accessed via the I2C hub port 'portId'.
 *
 * Side effects:
 *      None.
 *
 *****************************************************************************/
extern S32 XPAI_I2CReadPort(U32 portId,  /* !- FUNC -! */
                            U32 address,
                            U8 *buffer,
                            U32 length);


/******************************************************************************
 *
 * Global function:
 *      XPAI_I2CWritePort
 *
 * Parameters:
 *      portId     - ID of the port to enable on the hub
 *      address    - I2C slave device address
 *      buffer     - buffer containing write data
 *      length     - buffer length
 *
 * Return value:
 *      positive value indicates number of bytes written.
 *      negative value indicates error (see error codes above).
 *
 * Description:
 *      Writes to the specified I2C slave device, situated on an I2C bus that
 *      is accessed via the I2C hub port 'portId'.
 *
 * Side effects:
 *      None.
 *
 *****************************************************************************/
extern S32 XPAI_I2CWritePort(U32 portId,  /* !- FUNC -! */
                             U32 address,
                             U8 *buffer,
                             U32 length);


/******************************************************************************
 *
 * Global function:
 *      XPAI_I2CReadSubPort
 *
 * Parameters:
 *      portID     - ID of the port to enable on the hub
 *      address    - I2C slave device address
 *      buffer     - buffer used to store the read value
 *      length     - buffer length
 *      subAddress - I2C slave device sub address
 *
 * Return value:
 *      positive value indicates number of bytes read.
 *      negative value indicates error (see error codes above).
 *
 * Description:
 *      Reads from the specified I2C slave device's sub address, situated on
 *      an I2C bus that is accessed via the I2C hub port 'portId'.
 *
 * Side effects:
 *      None.
 *
 *****************************************************************************/
extern S32 XPAI_I2CReadSubPort(U32 portId,      /* !- FUNC -! */
                               U32 address,
                               U8 *buffer,
                               U32 length,
                               U32 subAddress);


/******************************************************************************
 *
 * Global function:
 *      XPAI_I2CWriteSubPort
 *
 * Parameters:
 *      portId     - ID of the port to enable on the hub
 *      address    - I2C slave device address
 *      buffer     - buffer containing write data
 *      length     - buffer length
 *      subAddress - I2C slave device sub address
 *
 * Return value:
 *      positive value indicates number of bytes written.
 *      negative value indicates error (see error codes above).
 *
 * Description:
 *      Writes to the specified I2C slave device's sub address, situated on
 *      an I2C bus that is accessed via the I2C hub port 'portId'.
 *
 * Side effects:
 *      None.
 *
 *****************************************************************************/
extern S32 XPAI_I2CWriteSubPort(U32 portId,      /* !- FUNC -! */
                                U32 address,
                                U8 *buffer,
                                U32 length,
                                U32 subAddress);


/******************************************************************************
 *
 * Global function:
 *      X2_I2CSleepPort
 *
 * Parameters:
 *      portId     - ID of the port to enable on the hub
 *
 * Return value:
 *      positive value indicates ok.
 *      negative value indicates error (see error codes above).
 *
 * Description:
 *      Orders the I2C driver to queue all request towards the specified I2C
 *      port for 250 ms.
 *
 * Side effects:
 *      None.
 *
 *****************************************************************************/
extern S32 X2_I2CSleepPort(U32 portId);

#ifdef __cplusplus
}
#endif

#endif /* _XPAI_HDR_I2C_IF_H */
