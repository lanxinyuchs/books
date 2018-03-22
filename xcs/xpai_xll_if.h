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
 *      XPAI/XLL
 *
 * File:
 *      xpai_xll_if.h
 *
 * Author:
 *      Anders Janson, QRADAAA
 *
 * Description:
 *      Contains declarations of functions exported by XLL to the XPAI.
 *      The function definitions are found in the files xpai_xll.c and
 *      xpai_xll_asm.s
 *
 * Reviewed:
 *      2000-09-18 Mathias Lundberg (QRALUNG)
 *
 * Revision history:
 *      2000-05-19, Jan Lindqvist (QRAJANL)
 *              First version.
 *
 *      2000-08-15, Anders Janson (QRADAAA)
 *              Capitalized function names.
 *              Renamed file to "xpai_xll_if.h"
 *      2000-10-12, Anders Janson (QRADAAA)
 *              Added "reviewed" info.
 *      2000-11-23, Anders Janson (QRADAAA)
 *              Added prototype for...
 *              - XPAI_MemoryProtect(), and constants for memory protection.
 *              - XPAI_SetBankRegisters().
 *              - XPAI_InstallPowerFailHandler()
 *              - XPAI_ConfigureInterrupt()
 *      2000-11-23, Rickard Fahlquist (QRAFAST)
 *              Added protoytypes for...
 *              - XPAI_GenIOConf()
 *              - XPAI_GenIORead()
 *              - XPAI_GenIO_Write()
 *              - XPAI_PWMFreq()
 *              - XPAI_PWMSet()
 *              - XPAI_PulseCount()
 *      2001-02-01, Anders Janson (QRADAAA)
 *              Code review remarks corrected:
 *              Added more info in comments for memory protection
 *      2001-04-23, Anders Hallqvist (QRAHALT)
 *              First version for Wilma, XPAI_InstallPowerFailHandler altered
 *      2001-06-29, Anders Hallqvist (QRAHALT)
 *              Version for incr 3 of Wilma adaptions to AUC
 *      2001-11-08, Anders Hallqvist (QRAHALT)
 *              Removed inclusion of xp.h
 *
 *      2006-04-20 Sven Löfgren (qlofsve)
 *              Updated with FUNC-tags. (Wanja).
 *
 *      2009-05-28 Magnus Lindgren (xmaglin)
 *              WRNae50212 'XDP might fail resetting applications Bank
 *              registers after FPGA load' corrected.
 *              Added getBankRegisters interface
 *
 *      2010-05-05, Björn Svensson (ebjorsv)
 *              CR WRNae85048 XPP shall let RU SW application turn off
 *              components before restart
 *
 *      2010-09-02 Sven Löfgren (qlofsve)
 *              Updated for WARP3:XPAI_GenIOPortConf/Read2/Write.
 *
 *      2011-02-10 Henrik Stöckel (qrahsto)
 *              Updated for WARP3:XPAI_ConfigureInterrupts
 *
 *****************************************************************************/

#ifndef XPAI_XLL_IF_H
#define XPAI_XLL_IF_H

/*----------------------------  Include files  ------------------------------*/
#include "ose.h"
#include "osetypes.h"
#include "libreboot.h"

#ifdef __cplusplus
extern "C" {
#endif

/*----------------------------  CONSTANTS and Macros ------------------------*/

/* Memory protection constants */
#define XPAI_XLL_EXECUTE      (1 <<  9)
#define XPAI_XLL_CACHEABLE    (1 <<  2)
#define XPAI_XLL_NONCACHEABLE (1 << 10)
#define XPAI_XLL_WRITE        (1 <<  8)
#define XPAI_XLL_READ         (1 << 11)

/* Error codes for memory protection */
#define XPAI_XLL_OUT_OF_RANGE      (1<<12)
#define XPAI_XLL_NOT_PAGE_ALIGNED  (1<<13)
#define XPAI_XLL_ILLEGAL_PAGE_SIZE  (1<<1)

/* Return codes for XPAI_SetBankRegisters() */
#define XPAI_XLL_BANKREGISTER_OK        0
#define XPAI_XLL_BANKREGISTER_NOK       1

/* Return codes for XPAI_ConfigureInterrupt.
   A sum of the error codes may be returned. */
#define XPAI_XLL_INTERRUPT_OK                   0
#define XPAI_XLL_INTERRUPT_ILLEGAL_PIN          (1<<0)
#define XPAI_XLL_INTERRUPT_ILLEGAL_TRIG         (1<<1)
#define XPAI_XLL_INTERRUPT_ILLEGAL_LEVEL        (1<<2)

/* Error codes for the XPAI_GenIOPort functions. */
#define XPAI_XLL_GEN_IO_PORT_OK               0x0
#define XPAI_XLL_GEN_IO_PORT_ILLEGAL_PORT     0x80000000
#define XPAI_XLL_GEN_IO_PORT_ILLEGAL_MASK     0x80000001
#define XPAI_XLL_GEN_IO_PORT_ILLEGAL_DIR      0x80000002
#define XPAI_XLL_GEN_IO_PORT_ILLEGAL_DATA     0x80000003

/* Error codes for the XPAI_XLL_PWM functions. */
#define XPAI_PWM_OK                             0
#define XPAI_PWM_FREQ_ILLEGAL_FREQ              (1<<0)
#define XPAI_PWM_SET_ILLEGAL_PIN                (1<<1)
#define XPAI_PWM_SET_ILLEGAL_WIDTH              (1<<2)

/* Error codes for the XPAI_PulseCount functions. */
#define XPAI_PULSE_COUNT_OK                     0
#define XPAI_PULSE_COUNT_BUSY                   (1<<0)
#define XPAI_PULSE_COUNT_ILLEGAL_COUNTER        (1<<1)

/* Error codes for the XPAI_InstallPowerFailHandler */
#define XPAI_XLL_HANDLER_OK                     0
#define XPAI_XLL_NULL_HANDLER                   1

/*
 * XLL tags that may be subscribed with the function XPAI_Subscribe
 * (see xpai_xmr_if.h).
 *
 * XPAI_XLL_RESTART
 *     Distributed before the board is restarted.
 */
#define XPAI_XLL_RESTART XLL_RESTART


/* Indication sent when a pulse count request has been performed               */
/* 0x100E300 = XLL_SIGBASE. This is redefined here to avoid dependency to xp.h */
#define XPAI_PULSE_COUNT_IND (0x100E300 + 80) /* !-SIGNO( struct XPAI_PulseCountIndS )-! */

#ifdef SOFT
#define XPAI_FNO_DISABLE_INTERRUPTS ((U32)XPAI_DisableInterrupts)
#define XPAI_FNO_RESTORE_INTERRUPTS ((U32)XPAI_RestoreInterrupts)
#define XPAI_FNO_MEMORY_PROTECT ((U32)XPAI_MemoryProtect)
#define XPAI_FNO_SET_BANK_REGISTER ((U32)XPAI_SetBankRegister)
#define XPAI_FNO_GET_BANK_REGISTER ((U32)X2_GetBankRegister)
#define XPAI_FNO_CONFIGURE_INTERRUPT ((U32)XPAI_ConfigureInterrupt)
#define XPAI_FNO_GEN_IO_PORT_CONF ((U32)XPAI_GenIOPortConf)
#define XPAI_FNO_GEN_IO_PORT_READ ((U32)XPAI_GenIOPortRead)
#define XPAI_FNO_GEN_IO_PORT_READ2 ((U32)XPAI_GenIOPortRead2)
#define XPAI_FNO_GEN_IO_PORT_WRITE ((U32)XPAI_GenIOPortWrite)
#define XPAI_FNO_PWM_FREQ ((U32)XPAI_PWMFreq)
#define XPAI_FNO_PWM_SET ((U32)XPAI_PWMSet)
#define XPAI_FNO_PULSE_COUNT ((U32)XPAI_PulseCount)
#define XPAI_FNO_INSTALL_POWER_FAIL_HANDLER ((U32)XPAI_InstallPowerFailHandler)
#endif

/*----------------------------  Structs and typedefs  -----------------------*/
/* Indication sent when a pulse count request has been performed */
struct XPAI_PulseCountIndS {
	SIGSELECT sigNo;
	U32 counts;                   /* Number of counts. 0-255 */
	U32 counter;                  /* Counter 0-2 */
	U32 time;                     /* Monitor window time (ms) */
};

/*----------------------------  Declaration of Global Variables  ------------*/
/*----------------------------  Declaration of Global Functions  ------------*/
/******************************************************************************
 *
 * Function:
 *      Global function XPAI_DisableInterrupts().
 *
 * Parameters:
 *      None.
 *
 * Return value:
 *      Old MSR value..
 *
 * Description:
 *      Clears the EE bit in the MSR.
 *
 *  Side effects:
 *     None.
 *
 *****************************************************************************/
extern U32 XPAI_DisableInterrupts(void); /* !- FUNC -! */

/******************************************************************************
 *
 * Function:
 *      global function XPAI_RestoreInterrupts().
 *
 * Parameters:
 *      oldMsr - a U32 number, for example the return value from a previous
 *      call to XPAI_DisableInterrupts().
 *
 * Return value:
 *      None.
 *
 * Description:
 *      Sets the EE bit in the MSR to whatever value it has in oldMsr.
 *
 *  Side effects:
 *     None.
 *
 *****************************************************************************/
extern void XPAI_RestoreInterrupts(U32 oldMsr); /* !- FUNC -! */

/******************************************************************************
 *
 * Function:
 *      global function XPAI_MemoryProtect().
 *
 * Parameters:
 *      address         Memory address to protect. Must be N*size.
 *
 *      size            Size of memory block to protect.
 *                      Must be 1k, 4k, 16k, 64k, 256k, 1M, 4M or 16M.
 *
 *      protection      Describes the protection to be applied:
 *                      - XPAI_XLL_CACHEABLE
 *                      - XPAI_XLL_NONCACHEABLE
 *                      - XPAI_XLL_EXECUTE (not functionally implemented)
 *                      - XPAI_XLL_READ (not functionally implemented)
 *                      - XPAI_XLL_WRITE (not functionally implemented)
 *
 *                      The protection constants may be combined:
 *                        XPAI_XLL_CACHEABLE | XPAI_XLL_WRITE
 *
 * Return value:
 *      Previous protection (if any), or error code. Current implementation
 *      always returns READ|WRITE|EXECUTE + current cacheability as previous
 *      protection.
 *
 *      Possible error codes:   XPAI_XLL_OUT_OF_RANGE
 *                              XPAI_XLL_NOT_PAGE_ALIGNED
 *                              XPAI_XLL_ILLEGAL_PAGE_SIZE
 *
 * Description:
 *      The only implemented action is to set data cacheability in the DCCR
 *      register.
 *
 *  Side effects:
 *     None.
 *
 *****************************************************************************/
extern U32 XPAI_MemoryProtect(void *address,     /* !- FUNC -! */
                              U32   size,
                              U32   protection);

/******************************************************************************
 *
 * Function:
 *      global function XPAI_SetBankRegister().
 *
 * Parameters:
 *      number - the bankregister to set (BRn) n=[1,3,4,5,6]
 *      low    - value for BRn
 *      high   - value for BRHn
 *
 * Return value:
 *      XPAI_BANKREGISTER_OK  - OK,
 *      XPAI_BANKREGISTER_NOK - Illegal number value.
 *
 * Description:
 *      Sets up PPC403 bankregister.
 *
 *  Side effects:
 *      None.
 *
 *****************************************************************************/
extern U32 XPAI_SetBankRegister(U32 number,  /* !- FUNC -! */
                                U32 low,
                                U32 high);

/******************************************************************************
 *
 * Function:
 *      global function XPAI_ConfigureInterrupt().
 *
 * Parameters:
 *      pin          - the interrupt pin to configure, 1-4 on AUM1, 1 on AUM2,
 *                     interrupt vector number on WARP3 0, 2, 5, 15-31
 *      trig         - 0: Level-triggered, 1:Edge-triggered.
 *      activeLevel  - 0: Neg. edge / low level  1: Pos. edge /high level
 *
 * Return value:
 *      XPAI_XLL_INTERRUPT_OK  - OK,
 *      XPAI_XLL_INTERRUPT_ILLEGAL_PIN,
 *      XPAI_XLL_INTERRUPT_ILLEGAL_TRIG,
 *      XPAI_XLL_INTERRUPT_ILLEGAL_LEVEL.
 *      If more that one illegal value is detected, the return value is
 *      the sum of two or more of the values above.
 *
 * Description:
 *      Configures one of four external interrupt pins
 *
 *  Side effects:
 *      None.
 *
 *****************************************************************************/
extern U32 XPAI_ConfigureInterrupt(U32 pin,          /* !- FUNC -! */
                                   U32 trig,
                                   U32 activeLevel);

/******************************************************************************
 *
 * Global function:
 *      XPAI_GenIOPortConf
 *
 * Parameters:
 *        port   - 1 port A, corresponds to GEN_IO[7:0] on AUM1
 *                           and to GEN_IO_A[15:0] on AUM2
 *                           and to GPIO_A on WARP3
 *                 2 port B, corresponds to GEN_IO_B[15:0] on AUM2LP
 *                           and to GPIO_B on WARP3
 *                 3 port C, corresponds to GEN_IO_C[15:0] on AUM2
 *        mask   - Only pins selected by the mask is affected by the command
 *                 0-255 on AUM1
 *                 0-65535 on AUM2
 *                 0-0xFFFFFFFF on WARP3
 *        IO_dir - Direction 0=input, 1=output. 1 bit per I/O pin.
 *                 0-255 on AUM1
 *                 0-65535 on AUM2
 *                 0-0xFFFFFFFF on WARP3
 *
 * Return value:
 *      XPAI_XLL_GEN_IO_PORT_OK
 *      XPAI_XLL_GEN_IO_PORT_ILLEGAL_PORT
 *      XPAI_XLL_GEN_IO_PORT_ILLEGAL_MASK
 *      XPAI_XLL_GEN_IO_PORT_ILLEGAL_DIR
 *
 * Description:
 *      Sets the direction of the general IO pins.
 *
 * Side effects:
 *      None.
 *
 *****************************************************************************/
extern U32 XPAI_GenIOPortConf(U32 port,    /* !- FUNC -! */
                              U32 mask,
                              U32 IO_dir);

/******************************************************************************
 *
 * Global function:
 *      XPAI_GenIOPortRead
 *
 * Parameters:
 *        port   - 1 port A, corresponds to GEN_IO[7:0] on AUM1
 *                           and to GEN_IO_A[15:0] on AUM2
 *                 2 port B, corresponds to GEN_IO_B[15:0] on AUM2LP
 *                 3 port C, corresponds to GEN_IO_C[15:0] on AUM2
 *        mask   - Only pins selected by the mask is affected by the command
 *                 0-255 on AUM1
 *                 0-65535 on AUM2
 *
 * Return value:
 *      XPAI_XLL_GEN_IO_PORT_OK
 *      XPAI_XLL_GEN_IO_PORT_ILLEGAL_PORT
 *      XPAI_XLL_GEN_IO_PORT_ILLEGAL_MASK
 *      Read value 0-255 AUM1 or 0-65535 on AUM2. Error codes are > 65535.
 *
 * Description:
 *      Reads the general I/O pins.
 *      NOTE: THIS FUNCTION DOES NOT SUPPORT 32 BIT GENERAL I/O's.
 *
 * Side effects:
 *      None.
 *
 *****************************************************************************/
extern U32 XPAI_GenIOPortRead(U32 port, U32 mask); /* !- FUNC -! */

/******************************************************************************
 *
 * Global function:
 *      XPAI_GenIOPortRead2
 *
 * Parameters:
 *        port      - 1 port A, corresponds to GEN_IO[7:0] on AUM1
 *                              and to GEN_IO_A[15:0] on AUM2
 *                              and to GPIO_A on WARP3
 *                    2 port B, corresponds to GEN_IO_B[15:0] on AUM2LP
 *                              and to GPIO_B on WARP3
 *                    3 port C, corresponds to GEN_IO_C[15:0] on AUM2
 *        mask      - Only pins selected by the mask is affected by the command
 *                    0-255 on AUM1
 *                    0-65535 on AUM2
 *                    0-0xFFFFFFFF on WARP3
 *        readValue - Pointer where to store read value.
 *                    0-255 AUM1
 *                    0-65535 on AUM2.
 *                    0-0xFFFFFFFF on WARP3
 *
 * Return value:
 *      XPAI_XLL_GEN_IO_PORT_OK
 *      XPAI_XLL_GEN_IO_PORT_ILLEGAL_PORT
 *      XPAI_XLL_GEN_IO_PORT_ILLEGAL_MASK
 *
 * Description:
 *      Reads the general I/O pins.
 *      NOTE: THIS FUNCTION SUPPORTS 32 BIT GENERAL I/O's.
 *
 * Side effects:
 *      None.
 *
 *****************************************************************************/
extern U32 XPAI_GenIOPortRead2(U32 port, U32 mask,
                               U32 *readValue); /* !- FUNC -! */

/******************************************************************************
 *
 * Global function:
 *      XPAI_GenIOPortWrite
 *
 * Parameters:
 *        port   - 1 port A, corresponds to GEN_IO[7:0] on AUM1
 *                           and to GEN_IO_A[15:0] on AUM2
 *                           and to GPIO_A on WARP3
 *                 2 port B, corresponds to GEN_IO_B[15:0] on AUM2LP
 *                           and to GPIO_B on WARP3
 *                 3 port C, corresponds to GEN_IO_C[15:0] on AUM2
 *        mask   - Only pins selected by the mask is affected by the command
 *                 0-255 on AUM1
 *                 0-65535 on AUM2
 *                 0-0xFFFFFFFF on WARP3
 *        data   - Data to write.
 *                 0-255 on AUM1
 *                 0-65535 on AUM2
 *                 0-0xFFFFFFFF on WARP3
 *
 * Return value:
 *      XPAI_XLL_GEN_IO_PORT_OK
 *      XPAI_XLL_GEN_IO_PORT_ILLEGAL_PORT
 *      XPAI_XLL_GEN_IO_PORT_ILLEGAL_MASK
 *      XPAI_XLL_GEN_IO_PORT_ILLEGAL_DATA
 *
 * Description:
 *      Writes to the general I/O pins.
 *
 * Side effects:
 *      None.
 *
 *****************************************************************************/
extern U32 XPAI_GenIOPortWrite(U32 port, U32 mask, U32 data); /* !- FUNC -! */

/******************************************************************************
 *
 * Global function:
 *      XPAI_PWMFreq
 *
 * Parameters:
 *        freq       - Frequency of PWM in khz, 10 or 1.
 *
 * Return value:
 *      Error code if illegal frequency.
 *
 * Description:
 *      Sets the frequency of the ouput from the pulse modulator.
 *
 * Side effects:
 *      None.
 *
 *****************************************************************************/
extern U32 XPAI_PWMFreq(U32 freq); /* !- FUNC -! */

/******************************************************************************
 *
 * Global function:
 *      XPAI_PWMSet
 *
 * Parameters:
 *        PWM_pin    - PWM_pin to set, 0, 1 or 2.
 *        width      - Pulse width 1-1000. 0=off, maximum 1000
 *
 * Return value:
 *      Error if illegal value.
 *
 * Description:
 *      Sets the high pulse width for the specified pulse width modulated output.
 *
 * Side effects:
 *      None.
 *
 *****************************************************************************/
extern U32 XPAI_PWMSet(U32 PWM_pin, U32 width); /* !- FUNC -! */

/******************************************************************************
 *
 * Global function:
 *      XPAI_PulseCount
 *
 * Parameters:
 *        counter    - Which counter to read, 0, 1 or 2
 *        time       - Time in ms to count pulses.
 *
 * Return value:
 *      Error code if faulty counter number or busy.
 *
 * Description:
 *      Counts pulses for a specified time. The function call will return
 *      immediately and perform the counting in the bckground. When the
 *      counting is finished the signal 'XPAI_PULSE_COUNT_IND' will be
 *      sent to the caller containing the count result. If any errors occur
 *      no signal will be sent.
 *
 * Side effects:
 *      None.
 *
 *****************************************************************************/
extern U32 XPAI_PulseCount(U32 counter, U32 time); /* !- FUNC -! */

/******************************************************************************
 *
 * Global function:
 *      XPAI_InstallPowerFailHandler
 *
 * Parameters:
 *        handler - Pointer to function to be called when power fail occurrs.
 *
 *                  The function takes 2 parameters flashAddress and logString.
 *                  logstring    - Pointer to string to be logged in flash.
 *                                 The string should be set by the handler
 *                                 function. The string should be max 32 bytes.
 *                                 This string will be prefixed with a timestamp
 *                                 and written in flash by XLL at the address
 *                                 given in parameter flashAddress.
 *                  flashAddress - pointer to Address in flash where to write
 *                                 the logstring.
 *                                 The address should be set by the handler
 *                                 function. The address must be in the interval
 *                                 0x7FFF0000 and 0x7FFF3FFF.
 *
 * Return value:
 *      none
 *
 * Description:
 *      This function will install a handler to be called in case of power fail.
 *      Only one handler can be installed at any time. New calls to
 *      XPAI_InstallPowerFailHandler will override previous ones.
 *      The handler MUST NOT use any OSE-calls, or XPP-calls.
 *      It must be optimized to execute fast (0.5 ms).
 *      It can use a stack of Max 2048 bytes.
 *
 * Side effects:
 *      Previously installed handler will be deinstalled.
 *
 *****************************************************************************/
void XPAI_InstallPowerFailHandler(void (*handler)(U8
                                  **flashAddress, /* !- FUNC -! */
                                  char **logString));

/******************************************************************************
 *
 * Function:
 *      platform internal function X2_GetBankRegister.
 *
 * Parameters:
 *      number - the bankregister to get (BRn) where
 *                n=[1,3,4,5,6] for AUM1
 *                n=[1,2,3]     for AUM2
 *      lowP    - pointer where to store the result value for BRn
 *      highP   - pointer where to store the result value for BRHn
 *
 * Return value:
 *      XPAI_BANKREGISTER_OK  - OK,
 *      XPAI_BANKREGISTER_NOK - Illegal number value.
 *
 * Description:
 *      Get data from PPC403/PPC405 bankregister. Each bank register number
 *      consists of two physical registers, low and high.
 *
 *  Side effects:
 *      None.
 *
 * NOTE:
 *      THIS IS A PLATFORM EXTENTION FUNCTION AND SHOULD NOT BE USED
 *      BY APPLICATION SOFTWARE!!!
 *****************************************************************************/
extern U32 X2_GetBankRegister(U32 number,
                              U32 *lowP,
                              U32 *highP);

#ifdef __cplusplus
}
#endif

#endif /* XPAI_XLL_IF_H */
