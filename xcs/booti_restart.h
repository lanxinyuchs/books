/* > Description **************************************************************/
/**
 * @file booti_restart.h
 * @brief BOOT restart interface.
 *
 * This file defines the usage of bits in the reset scratch pad register
 * (MISC_RESET_MEM_0 on Xenon).
 */

/*
 * Copyright (C) 2015 by Ericsson AB. All rights reserved. The
 * information in this document is the property of Ericsson. Except
 * as specifically authorized in writing by Ericsson, the receiver
 * of this document shall keep the information contained herein
 * confidential and shall protect the same in whole or in part from
 * disclosure and dissemination to third parties. Disclosure and
 * disseminations to the receiver's employees shall only be made on
 * a strict need to know basis.
 *
 ******************************************************************************/

#ifdef __cplusplus
extern "C" {
#endif

#ifndef BOOTI_RESTART_H
#define BOOTI_RESTART_H

/* > Includes *****************************************************************/
#include "booti_version.h"

/* > Defines ******************************************************************/

/**
 * Mask to select LMC slot (0-14).
 * Value 0x000000f0 is reserved, see @ref BOOTI_RESTART_LMC_AUBOOT.
 * Field is set by:
 * - LMC when it makes an ordered restart.
 * - Secondary boot loader (U-Boot) if the supplied slot is not valid, set to
 *   @ref BOOTI_RESTART_LMC_AUBOOT or if restart is not ordered, i.e. if it is
 *   not a system reset or if it is a system reset where restart type is @ref
 *   BOOTI_RESTART_TYPE_CRASH.
 */
#define BOOTI_RESTART_LMC_MASK                0x0000000f

/** Value to select youngest AUBOOT. */
#define BOOTI_RESTART_LMC_AUBOOT              BOOTI_RESTART_LMC_MASK

/**
 * Mask to select restart count (0-3).
 * Field is incremented by Secondary boot loader (U-Boot) when @ref
 * BOOTI_RESTART_RECOVER_MODE is set.
 * Field is cleared by Secondary boot loader (U-Boot) when it receives an
 * ordered restart.
 * Field is cleared by LMC when it has been running for 30 minutes.
 */
#define BOOTI_RESTART_COUNT_MASK              0x00000030

/** Shift count for restart count. */
#define BOOTI_RESTART_COUNT_SHIFT             4

/**
 * If the AUBOOT crashes then this bit is set and recover mode is entered.
 * In this mode then attempts are made to start another LMC. The order to
 * attempt the other LMCs should be from the youngest AUBOOT to oldest AUBOOT
 * and after that from youngest AUAPPLIC to oldest AUAPPLIC. If every LMC fails
 * to start after 4 attempts then BOOT should retry from the beginning again
 * with the youngest AUBOOT. The recover mode should be exited if a LMC makes an
 * ordered restart or has been running for 30 minutes.
 * Bit is set by Secondary boot loader (U-Boot) when AUBOOT crashes.
 * Bit is reset by Secondary boot loader (U-Boot) when it receives an ordered
 * restart.
 */
#define BOOTI_RESTART_RECOVER_MODE            0x00000040

/**
 * If set then spare partition should be used if it is valid else the backup
 * partition should be used if its working status is set else the default
 * (Type 2) partition should be used.
 * If cleared then the spare partition should be used if its working status is
 * set else the backup partition should be used if its working status is set
 * else the default (Type 2) partition should be used.
 * Bit is set by special LMC that has upgraded spare partition.
 * Bit is reset by:
 * - Primary boot loader if it is not an ordered restart, i.e. if it is not a
 *   system reset or if it is a system reset where restart type is @ref
 *   BOOTI_RESTART_TYPE_CRASH.
 * - Special LMC that checks that upgrade of spare partition has succeeded.
 */
#define BOOTI_RESTART_USE_LATEST              0x00000080

/**
 * Mask to select restart type.
 * (0x4..0x7) << BOOTI_RESTART_TYPE_SHIFT reserved for future use.
 * Field is set by:
 * - Secondary boot loader (U-Boot) to @ref BOOTI_RESTART_TYPE_CRASH to detect
 *   a crash.
 * - LMC when it makes an ordered restart.
 */
#define BOOTI_RESTART_TYPE_MASK               0x00000700

/** Shift count for restart type. */
#define BOOTI_RESTART_TYPE_SHIFT              8

/** Indicates a restart caused by crash. */
#define BOOTI_RESTART_TYPE_CRASH              0x00000000

/** Indicates a software ordered restart. */
#define BOOTI_RESTART_TYPE_ORDERED            0x00000100

/**
 * Indicates a software ordered restart where test should be performed at
 * startup.
 */
#define BOOTI_RESTART_TYPE_ORDERED_WITH_TEST  0x00000200

/** Indicates a software ordered immediate restart. */
#define BOOTI_RESTART_TYPE_ORDERED_IMMEDIATE  0x00000300

/**
 * In boot phase counter, which is used to to detect if crash occurs during boot
 * phase.
 * Primary boot loader:
 * - Sets counter to one if restart is not caused by crash, i.e. it is not core,
 *   watchdog and debug reset and system reset where restart type is @ref
 *   BOOTI_RESTART_TYPE_CRASH.
 * - Increments the counter if it is less than 2 and restart is caused by crash,
 *   i.e. it is core, watchdog or debug reset or system reset where restart type
 *   is @ref BOOTI_RESTART_TYPE_CRASH.
 * - Sets counter to zero if it is 2 and selects type 2 Secondary boot loader
 *   (U-Boot) and type 2 U-Boot environment.
 * The LMC should set counter to zero as early as possible.
 */
#define BOOTI_RESTART_IN_BOOT_COUNT_MASK      0x00001800


/** Shift count for in boot phase counter. */
#define BOOTI_RESTART_IN_BOOT_COUNT_SHIFT     11

/** Spare bits. */
#define BOOTI_RESTART_SPARE_MASK              0xffffe000

/* > Type Declarations ********************************************************/

/* > Function Declarations ****************************************************/

#endif /* BOOTI_RESTART_H */

#ifdef __cplusplus
}
#endif
