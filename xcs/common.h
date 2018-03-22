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
 *      common.h
 *
 * Author:
 *      Nan Jiang        30 01 2015
 *
 * Description:
 *      This local header file contains the common declaration
 *      which used only within XPAI interface.
 *
 * Reviewed:
 *
 * Revision history:
 *
 *****************************************************************************/

#ifndef _COMMON_H
#define _COMMON_H

/*----------------------------  Include files  ------------------------------*/
#include <stdint.h>
#include <itc.h>

/*----------------------------  CONSTANTS  ----------------------------------*/
/*----------------------------  MACROS  -------------------------------------*/
#define CONN_ESTABLISH_TMO      0 /* NO timeout */
/* Return values of init functions */
#define INIT_OK           0
#define INIT_SERVER_NOK   1
#define INIT_MBOX_NOK     2
#define INIT_OTHER_ERROR  3
/* only used for i2c_init */
#define INIT_I2C_PORT_NOK 4
#define INIT_LH_PORT_NOK  5


/*----------------------------  Structs and typedefs  -----------------------*/

/*----------------------------  Declaration of Global Variables  ------------*/

/*----------------------------  Declaration of Global Functions  ------------*/
/******************************************************************************
 *
 * Global function:
 *      xpai_i2c_port_init
 *
 * Parameters:
 *      portId          - ID of the port to enable on the hub
 *      client_ref      - connection reference for client
 *
 * Return value:
 *      0 indicates ok.
 *      Other value indicates error.
 *
 * Description:
 *      establish the connection between a specific I2C server and client
 *
 * Side effects:
 *      None.
 *
 *****************************************************************************/
int32_t xpai_i2c_port_init(uint32_t portId, uint32_t client_ref);

/******************************************************************************
 *
 * Global function:
 *      xpai_i2c_init
 *
 * Parameters:
 *      client_ref      - connection reference for client
 *
 * Return value:
 *      0 indicates ok.
 *      Other value indicates error.
 *
 * Description:
 *      establish the connection between all I2C servers and client
 *
 * Side effects:
 *      None.
 *
 *****************************************************************************/
int32_t xpai_i2c_init(uint32_t client_ref);

/******************************************************************************
 *
 * Global function:
 *      xpai_xmr_init
 *
 * Parameters:
 *
 * Return value:
 *      0 indicates ok.
 *      Other value indicates error (see error codes above).
 *
 * Description:
 *      As the event subscription mechanism for legacy reasons has no
 *      connection establish or version negotionation. There is really
 *      no need for an init function. But keep things similar for all
 *      sub interfaces it's defined and always return success.
 *
 * Side effects:
 *      None.
 *
 *****************************************************************************/
uint32_t xpai_xmr_init(void);

/******************************************************************************
 *
 * Global function:
 *      xpai_fault_init
 *
 * Parameters:
 *      client_ref      - connection reference for client
 *
 * Return value:
 *      0 indicates ok.
 *      Other value indicates error.
 *
 * Description:
 *      establish the connection between fault server and client
 *
 * Side effects:
 *      None.
 *
 *****************************************************************************/
int32_t xpai_fault_init(uint32_t client_ref);

/******************************************************************************
 *
 * Global function:
 *      xpai_spi_init
 *
 * Parameters:
 *      client_ref  - connection reference for client
 *
 * Return value:
 *      0 if OK.
 *      Error code if connection is failed to establish.
 *
 * Description:
 *      Sends a conn establish request to the all spi servers and negotiates
 *      the interface/protocol version.
 *
 * Side effects:
 *      None.
 *
 *****************************************************************************/
int32_t xpai_spi_init(uint32_t client_ref);

/******************************************************************************
 *
 * Global function:
 *      xpai_spi_slave_init
 *
 * Parameters:
 *      client_ref  - connection reference for client
 *      slave       - The SPI slave to access
 *
 *
 * Return value:
 *      0 if OK.
 *      Error code if connection is failed to establish.
 *
 * Description:
 *      Sends a conn establish request to a specific spi servers and negotiates
 *      the interface/protocol version.
 *
 * Side effects:
 *      None.
 *
 *****************************************************************************/
int32_t xpai_spi_slave_init(uint32_t slave, uint32_t client_ref);

/******************************************************************************
 *
 * Global function:
 *      xpai_vii_init
 *
 * Parameters:
 *      client_ref      - connection reference for client
 *
 * Return value:
 *      0 indicates ok.
 *      Other value indicates error.
 *
 * Description:
 *      establish the connection between VII server and client
 *
 * Side effects:
 *      None.
 *
 *****************************************************************************/
int32_t xpai_vii_init(uint32_t client_ref);

/******************************************************************************
 *
 * Global function:
 *      xdai_mmi_init
 *
 * Parameters:
 *      client_ref      - connection reference for client
 *
 * Return value:
 *      0 indicates ok.
 *      Other value indicates error.
 *
 * Description:
 *      establish the connection between MMI server and client
 *
 * Side effects:
 *      None.
 *
 *****************************************************************************/
int32_t xdai_mmi_init(uint32_t client_ref);

/******************************************************************************
 *
 * Global function:
 *      xpai_restart_init
 *
 * Parameters:
 *
 * Return value:
 *      0 indicates ok.
 *      Other value indicates error.
 *
 * Description:
 *      Initializes the restart block
 *
 * Side effects:
 *      None.
 *
 *****************************************************************************/
int32_t xpai_restart_init(void);
/******************************************************************************
 *
 * Global function:
 *      xpai_lh_port_init
 *
 * Parameters:
 *      portId          - ID of the port
 *      client_ref      - connection reference for client
 *
 * Return value:
 *      0 indicates ok.
 *      Other value indicates error.
 *
 * Description:
 *      establish the connection between a specific lh server and client
 *
 * Side effects:
 *      None.
 *
 *****************************************************************************/
int32_t xpai_lh_port_init(uint32_t portId, uint32_t client_ref);

/******************************************************************************
 *
 * Global function:
 *      xpai_lh_init
 *
 * Parameters:
 *      client_ref      - connection reference for client
 *
 * Return value:
 *      0 indicates ok.
 *      Other value indicates error.
 *
 * Description:
 *      Initializes the lh block
 *
 * Side effects:
 *      None.
 *
 *****************************************************************************/
int32_t xpai_lh_init(uint32_t client_ref);

/******************************************************************************
 *
 * Global function:
 *      xpai_hwlog_init
 *
 * Parameters:
 *
 * Return value:
 *      0 indicates ok.
 *      Other value indicates error.
 *
 * Description:
 *      Initializes the hw log block
 *
 * Side effects:
 *      None.
 *
 *****************************************************************************/
int32_t xpai_hwlog_init(void);


/******************************************************************************
 *
 * Global function:
 *      xpai_lmc_init
 *
 * Parameters:
 *      client_ref      - connection reference for client
 *
 * Return value:
 *      0 indicates ok.
 *      Other value indicates error.
 *
 * Description:
 *      establish the connection between LMC server and client
 *
 * Side effects:
 *      None.
 *
 *****************************************************************************/
extern int32_t xpai_lmc_init(void);

/******************************************************************************
 *
 * Global function:
 *      xpai_nodeid_init
 *
 * Parameters:
 *
 * Return value:
 *      0 indicates ok.
 *      Other value indicates error.
 *
 * Description:
 *      establish the connection between nodeid server and client
 *
 * Side effects:
 *      None.
 *
 *****************************************************************************/
int32_t xpai_nodeid_init(void);


/******************************************************************************
 *
 * Global function:
 *      xpai_locate_mbox
 *
 * Parameters:
 *      mbox_name    - Name of the mailbox
 *      mbox_id      - Where to store the mailbox identifier
 *
 * Return value:
 *      Returns INIT_MBOX_NOK if ITC was not properly setup for the calling
 *      thread, otherwise INIT_OK.
 *
 * Description:
 *      Block until the mailbox is located and then store the mailbox
 *      identifier.
 *
 * Side effects:
 *      None.
 *
 *****************************************************************************/

int32_t xpai_locate_mbox(const char *mbox_name, itc_mbox_id_t *mbox_id);


#endif /* _XPAI_COMMON_H */
