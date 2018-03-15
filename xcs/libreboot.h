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
 * @file libreboot.h
 * @brief Restart functions and tags
 *
 * There are 3 different ways to restart the board.\n
 * The application can call XPAI_RestartBoard2() or reboot_immediate()
 * to restart the board\n
 * Users can use restart_board shell command to restart the board.\n
 * \n
 * This interface can be used directly by application or XCS process.\n
 */
#ifndef _LIBREBOOT_H_
#define _LIBREBOOT_H_
#include <stdint.h>

/*
 * XLL tags that may be subscribed with the function XPAI_Subscribe
 * (see xpai_xmr_if.h).
 *
 * XLL_RESTART
 *     Distributed before the board is restarted.
 */
#define XLL_RESTART "XLL_Restart"



/******************************************************************************
 *
 * Global function:
 *      reboot_with_pid
 *
 * Parameters:
 *      program    - the name of the program which is ordering the restart.
 *      reason     - restart reason which is written to llog
 *      loadmodule - production ID on which selected to restart
 *
 * Return value:
 *
 * Description:
 *      This function will restart the board with selected loadmodule.
 *      If loadmodule string is empty, the current LMC is used. If neither
 *      the stated LMC nor the current LMC is to be found, the youngest AUBOOT
 *      will be used.
 *
 * Side effects:
 *
 *****************************************************************************/
extern void reboot_with_pid(char *program, char *reason, char *loadmodule);

/******************************************************************************
 *
 * Global function:
 *      reboot_with_slot
 *
 * Parameters:
 *      program     - the name of the program which is ordering the restart.
 *      reason      - restart reason which is written to llog
 *      slot_number - slot number on which selected to restart
 *
 * Return value:
 *      1 on failure
 *      0 on success
 * Description:
 *      This function will restart the board with selected loadmodule. If
 *      selected loadmodule cannot be found, the function will return without
 *      restart.
 *
 * Side effects:
 *      Return value 0 will never be received since the board is restarted
 *      before return
 *****************************************************************************/
extern int reboot_with_slot(char *program, char *reason, int32_t slot_number);

/******************************************************************************
 *
 * Global function:
 *      reboot_immediate
 *
 * Parameters:
 *      program    - the name of the program which is ordering the restart.
 *      reason     - restart reason which is written to llog
 *
 * Return value:
 *
 * Description:
 *      This function will restart the board immediatly with AUBOOT.
 *
 * Side effects:
 *****************************************************************************/
extern void reboot_immediate(char *program, char *reason);

/******************************************************************************
 *
 * Global function:
 *      reboot_with_test_on_slot
 *
 * Parameters:
 *      program     - the name of the program which is ordering the restart.
 *      reason      - restart reason which is written to llog
 *      slot_number - slot number on which selected to restart
 *
 * Return value:
 *      1 on failure
 *      0 on success
 * Description:
 *      This function will restart the board with selected loadmodule after
 *      performing a cold restart with test. If the selected loadmodule cannot
 *      be found, the function will return without restart.
 *
 * Side effects:
 *      Return value 0 will never be received since the board is restarted
 *      before return
 *****************************************************************************/
extern int reboot_with_test_on_slot(char *program, char *reason,
                                    int32_t slot_number);
#endif
