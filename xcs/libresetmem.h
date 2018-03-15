/**
 *   @copyright
 *   Copyright (C) 2015 by Ericsson AB. All rights reserved. The
 *   information in this document is the property of Ericsson. Except
 *   as specifically authorized in writing by Ericsson, the receiver
 *   of this document shall keep the information contained herein
 *   confidential and shall protect the same in whole or in part from
 *   disclosure and dissemination to third parties. Disclosure and
 *   disseminations to the receiver's employees shall only be made on
 *   a strict need to know basis.
 */

/**
 * @file libresetmem.h
 * @brief Restart function used by libreboot.\n
 * \n
 * HW sepcific restart for xenon.\n
 * This interface can not be used directly by application or XCS process.\n
 */
#ifndef _LIBRESETMEM_H_
#define _LIBRESETMEM_H_
#include <stdint.h>


/******************************************************************************
 *
 * Global function:
 *      restart_board
 *
 * Parameters:
 *      slot_number - the slot which is selected to the restart on.
 *      reboot_type - reboot type write to resetmem
 *
 * Return value:
 *
 * Description:
 *      This function will restart the board on selected slot and
 *      write reboot type to resetmem.
 *
 * Side effects:
 *****************************************************************************/
extern void resetmem_restart_board(int32_t slot_number, uint32_t reboot_type);

/******************************************************************************
 *
 * Global function:
 *      resetmem_clear_restart_counter
 *
 * Parameters:
 *
 * Return value:
 *      0  Success
 *      1  Error
 *
 * Description:
 *      This function will reset the reset counter in resetmem
 *
 * Side effects:
 *****************************************************************************/
extern int resetmem_clear_restart_counter(void);

/******************************************************************************
 *
 * Global function:
 *      resetmem_clear_restart_in_boot_counter
 *
 * Parameters:
 *
 * Return value:
 *      0  Success
 *      1  Error
 *
 * Description:
 *      This function will reset the reset counter in resetmem
 *
 * Side effects:
 *****************************************************************************/
extern int resetmem_clear_restart_in_boot_counter(void);

/******************************************************************************
 *
 * Global function:
 *      wk_pa_protection
 *
 * Parameters:
 *
 * Return value:
 *      0  Success
 *      1  Error
 *
 * Description:
 *      This function will provide a workaroud for pa protection
 *
 * Side effects:
 *****************************************************************************/
extern int wk_pa_protection(void);
#endif
