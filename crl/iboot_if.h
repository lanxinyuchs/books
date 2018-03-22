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
 *      IBOOT
 *
 * File:
 *      iboot_if.h
 *
 * Author:
 *      Peter Bergsten
 *
 * Description:
 *      Includes the application interface definition.
 *
 * Reviewed:
 *      2004-08-17 Anders Hallqvist, Henrik Stöckel
 *              IR: 3/1776-122/FCP1034850
 *
 * Revision history:
 *      2004-03-30 Peter Bergsten
 *              Created
 *      2004-08-17 Peter Bergsten (QRAPEBE)
 *              Updated after code review
 *
 *      2012-10-05 Zabihullah Bayat (ezabbay)
 *              Removed osetypes.h dependency and added new type for IBOOT
 *
 *      2015-08-28 Amir Mohammad Koosha (eamikoo)
 *              New blob type is introduced
 *
 *****************************************************************************/

#ifndef IBOOT_IF_H
#define IBOOT_IF_H

/*----------------------------  Include files  ------------------------------*/

/*----------------------------  CONSTANTS  ----------------------------------*/
/* Revision number of the IBOOT-XPL interface */
#define IBOOT_XPL_IF_REV_NO             ((unsigned int)0x1)

/* Length of the IBOOT SW PID string */
#define IBOOT_SW_PID_LENGTH             32

/* Length of the PID and REV string */
#define IBOOT_PROD_NUMBER_LENGTH        23
#define IBOOT_REV_NUMBER_LENGTH         9

/* Length of the blob name string */
#define XLF_IBOOT_BLOB_NAME_LENGTH      32

/* Start-cause passed as argument to ABOOT and application */
#define IBOOT_COLD_START                1
#define IBOOT_ORDERED_RESET             2
#define IBOOT_CRASH                     3

/* ABOOT-status codes passed to application */
#define ABOOT_STATUS_SUCCESS            0
#define ABOOT_STATUS_NOT_AVAILABLE      1
#define ABOOT_STATUS_PREV_RESET         2

/* Reset-cause codes determined by IBOOT and passed to application */
#define IBOOT_CORE_RESET                1
#define IBOOT_CPLD_UART_RESET           2
#define IBOOT_WATCHDOG_RESET            3
#define IBOOT_RESET_IN_APP_N            4
#define IBOOT_SW_RESET                  5
#define IBOOT_RESET_SWITCH              6
#define IBOOT_SYSTEM_RESET              7
#define IBOOT_POWER_ON_RESET            8
#define IBOOT_CHIP_RESET                9
#define IBOOT_UNKNOWN_CAUSE             0

/* The status of the protect-boot strap on the XP-board */
#define IBOOT_PROTECT_BOOT_FALSE        0
#define IBOOT_PROTECT_BOOT_TRUE         1

/* Valid IBOOT-header field-values */
#define XLF_IBOOT_HDR_MAGIC             ((unsigned int)0x58504C46)
#define XLF_IBOOT_HDR_AU_BOOT           ((unsigned int)0)
#define XLF_IBOOT_HDR_AU_APPLIC         ((unsigned int)1)
#define XLF_IBOOT_HDR_BOOT              ((unsigned int)2)
#define XLF_IBOOT_HDR_AU_BOOT_LINUX     ((unsigned int)3)
#define XLF_IBOOT_HDR_AU_APPLIC_LINUX   ((unsigned int)4)
#define XLF_IBOOT_HDR_AU_ENV            ((unsigned int)5)
#define XLF_IBOOT_HDR_BOOT_ENV          ((unsigned int)6)

#define XLF_IBOOT_HDR_SPARE1            ((unsigned short)0xffff)
#define XLF_IBOOT_HDR_SPARE2            ((unsigned short)0xffff)
#define XLF_IBOOT_HDR_INIT_UPDATE       ((unsigned int)0xffffffff)
#define XLF_IBOOT_HDR_INIT_UPDATE_INI   ((unsigned int)0xfffffffe)
#define XLF_IBOOT_HDR_INIT_SEQNO        ((unsigned int)0xffffffff)
#define XLF_IBOOT_HDR_INIT_SEQNO_INI    ((unsigned int)0x00000000)
#define XLF_IBOOT_HDR_SUID_LEN          ((unsigned int)32)

/* IBOOT-wrapper magic for different types of application files. */
#define XLF_IBOOT_WPR_RPDOUT_MAGIC      ((unsigned int)0x5250444f)

#define XLF_IBOOT_WPR_BLOB_MAGIC        ((unsigned int)0x424c4f42)

/* Types and number of elements in the memory-map exported to application */
#define XP_MEM_MAP_ELEMENTS             3

#define XP_DRAM_TYPE                    0
#define XP_MMIO_TYPE                    1
#define XP_FLASH_TYPE                   2
#define XP_NOT_USED_ELEMENT_TYPE        3

/*----------------------------  Structs and typedefs  -----------------------*/

/*----------------------------  MACROS  -------------------------------------*/

/*----------------------------  Structs and typedefs  -----------------------*/

/******************************************************************************
 *
 * Global function:     IBOOT_WatchDogP_t
 *
 * Description:         Type-definintion of the callback function
 *                      feeding the watchdog
 *
 *****************************************************************************/
typedef void IBOOT_WatchDogP_t(void);

/******************************************************************************
 *
 * Global function:     IBOOT_PpcSpuPrintStrP_t
 *
 * Parameters:          str - string to be printed on the terminal port.
 *
 * Description:         Type-definintion of the terminal-port print-function
 *
 *****************************************************************************/
typedef void IBOOT_PpcSpuPrintStrP_t(char *str);

/******************************************************************************
 *
 * Global function:     IBOOT_ABootP_t
 *
 * Parameters:          watchDog - function feeding the watchdog
 *                      printStr - Terminal-port print-function
 *                      startRestartCause - start/restart-cause from IBOOT
 *
 * Return value:        Return-code passed further from IBOOT to the
 *                      application to be started.
 *
 * Description:         Prototype of ABOOT
 *
 *****************************************************************************/
typedef unsigned int IBOOT_ABootP_t(IBOOT_WatchDogP_t *watchDog,
                                    IBOOT_PpcSpuPrintStrP_t *printStr,
                                    unsigned int startRestartCause);

/******************************************************************************
 *
 * Type:                IBOOT_XlfIbootHeaderS_t
 *
 * Description:         XLF IBOOT header format type-declaration
 *
 *****************************************************************************/
typedef struct IBOOT_XlfIbootHeaderS
{
	unsigned int   magic;
	unsigned int   type;
	unsigned char majorVersion;
	unsigned char minorVersion;
	unsigned short   spare1;
	char  suid[XLF_IBOOT_HDR_SUID_LEN];
	unsigned int   prodDateTime;
	unsigned int   xplOffset;
	unsigned int   crc2Offset;
	unsigned short   crc1;
	unsigned short   spare2;
	unsigned int   update;
	unsigned int   seqNo;
	unsigned int   fileCount;
	unsigned int   fileOffset[1];
} IBOOT_XlfIbootHeaderS_t;

/******************************************************************************
 *
 * Type:                IBOOT_XlfIbootWrapperS_t
 *
 * Description:         XLF IBOOT wrapper format type-declaration
 *
 *****************************************************************************/
typedef struct IBOOT_XlfIbootWrapperS
{
	unsigned int   endOffset;
	unsigned int   magic;
} IBOOT_XlfIbootWrapperS_t;

/******************************************************************************
 *
 * Type:                IBOOT_XlfIbootFooterS_t
 *
 * Description:         XLF IBOOT footer format type-declaration
 *
 *****************************************************************************/
typedef struct IBOOT_XlfIbootFooterS
{
	unsigned short   crc2;
} IBOOT_XlfIbootFooterS_t;


/******************************************************************************
 * Type:                IBOOT_XpMemMapElementS_t
 *
 * Description:         Memory-map element definition
 *
 *****************************************************************************/
typedef struct IBOOT_XpMemMapElementS
{
	unsigned int type;             /* one of the three types defined above */
	unsigned char*rawAccBase;       /* base-address of the non-cached memory-region */
	unsigned char*optAccBase;       /* base-address of the cached memory-region */
	unsigned int size;             /* size of the memory-region */
} IBOOT_XpMemMapElementS_t;

/******************************************************************************
 * Type:                IBOOT_ExportDataS_t
 *
 * Description:         IBOOT export parameters definition
 *
 *****************************************************************************/
typedef struct IBOOT_ExportDataS
{
	unsigned int   ibootXplIfRev;
	unsigned char   ibootSwPid[IBOOT_SW_PID_LENGTH];
	IBOOT_WatchDogP_t *watchDog;
	IBOOT_PpcSpuPrintStrP_t *ppcSpuPrintStr;
	IBOOT_XlfIbootHeaderS_t *xlfIbootHdr;
	unsigned char   reserved1;
	unsigned char   reserved2;
	unsigned int   reserved3;
	unsigned int   ibootStartRestartCause;
	unsigned int   startRestartCause;
	unsigned int   coreDumpInd;
	unsigned int   abootStatus;
	unsigned int   abootReturnValue;
	unsigned int   protectBootStatus;
	IBOOT_XpMemMapElementS_t xpMemMap[XP_MEM_MAP_ELEMENTS];
} IBOOT_ExportDataS_t;

/******************************************************************************
 * Type:                IBOOT_XlfBlobTypeE_t
 *
 * Description:         XLF IBOOT possible blob types enum
 *
 *****************************************************************************/
typedef enum IBOOT_XlfBlobTypeE
{
	IBOOT_XlfBlobTypeE_UENVIMAGE,
	IBOOT_XlfBlobTypeE_DTB,
	IBOOT_XlfBlobTypeE_ROOTFS,
	IBOOT_XlfBlobTypeE_UIMAGE,
	IBOOT_XlfBlobTypeE_ASCII_DB,
	IBOOT_XlfBlobTypeE_XILINX_BIT,
	IBOOT_XlfBlobTypeE_LAST /* Should be the last one */
} IBOOT_XlfBlobTypeE_t;

/******************************************************************************
 * Type:                IBOOT_XlfBlobHeaderS_t
 *
 * Description:         XLF IBOOT blob header format type-declaration
 *
 *****************************************************************************/
typedef struct IBOOT_XlfBlobHeaderS
{
	unsigned int   crc32;
	unsigned int   headerSize;
	unsigned int   type;       /* IBOOT_XlfBlobTypeE_t */
	unsigned int   majorVer;
	unsigned int   minorVer;
	char           suid[XLF_IBOOT_HDR_SUID_LEN];
	unsigned int   prodDateTime;
	char           name[XLF_IBOOT_BLOB_NAME_LENGTH];
} IBOOT_XlfBlobHeaderS_t;

/******************************************************************************
 *
 * Global function:     IBOOT_XplStartP_t
 *
 * Parameters:          xplImportData - pointer to the data being exported
 *                                      by IBOOT.
 *
 * Description:         Prototype of XPL.
 *                      Shall never return in normal operation.
 *
 *****************************************************************************/
typedef void IBOOT_XplStartP_t(IBOOT_ExportDataS_t *xplImportData);

/*----------------------------  Declaration of Global Variables  ------------*/

/*----------------------------  Declaration of Global Functions  ------------*/

/*----------------------------  Declaration of Global Functions  ------------*/

#endif /* IBOOT_IF_H */
