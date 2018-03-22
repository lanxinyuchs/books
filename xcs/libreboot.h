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

/** @{ */
/**
 * @name XLL tags
 * Tags that may be subscribed with the function XPAI_Subscribe
 * (see xpai_xmr_if.h).
*/
/**
 * Distributed before the board is restarted.
 */
#define XLL_RESTART "XLL_Restart"
/** @} */

/** @{ */
/**
 * @name Error code return in managed reboot
 */
#define LIBREBOOT_SERVER_ERROR -1
#define LIBREBOOT_LMC_INVALID  -2
/** @} */

/** @defgroup group1 API Functions in libreboot.h
 *  @{
 */
/*****************************************************************************/
/**
 *   @param program     The name of the program which is ordering the restart.
 *   @param reason      Restart reason which is written to llog
 *   @param loadmodule  Production ID on which selected to restart
 *
 *   @return  -
 *
 *   This function will restart the board with selected loadmodule.
 *   If loadmodule string is empty, the current LMC is used. If neither
 *   the stated LMC nor the current LMC is to be found, the youngest AUBOOT
 *   will be used.
 *
 */
/*****************************************************************************/
extern void reboot_with_pid(char *program, char *reason, char *loadmodule);

/*****************************************************************************/
/**
 *   @param program      The name of the program which is ordering the restart.
 *   @param reason       Restart reason which is written to llog
 *   @param slot_number  Slot number on which selected to restart
 *
 *   @return  LIBREBOOT_SERVER_ERROR or LIBREBOOT_LMC_INVALID on failure
 *
 *   This function will restart the board with selected loadmodule. If
 *   selected loadmodule cannot be found, the function will return without
 *   restart.
 *
 */
/*****************************************************************************/
extern int reboot_with_slot(char *program, char *reason, int32_t slot_number);

/*****************************************************************************/
/**
 *   @param program      The name of the program which is ordering the restart.
 *   @param reason       Restart reason which is written to llog
 *
 *   @return  -
 *
 *   This function will restart the board immediatly with AUBOOT.
 *
 */
/*****************************************************************************/
extern void reboot_immediate(char *program, char *reason);

/*****************************************************************************/
/**
 *   @param program      The name of the program which is ordering the restart.
 *   @param reason       Restart reason which is written to llog
 *   @param slot_number  Slot number on which selected to restart
 *
 *   @return  LIBREBOOT_SERVER_ERROR or LIBREBOOT_LMC_INVALID on failure
 *
 *   This function will restart the board with selected loadmodule after
 *   performing a cold restart with test. If the selected loadmodule cannot
 *   be found, the function will return without restart.
 *
 */
/*****************************************************************************/
extern int reboot_with_test_on_slot(char *program, char *reason,
                                    int32_t slot_number);
/** @} */

#endif
