/* ---------------------------------------------------------------------------
 *
 * © Ericsson AB 2015 All rights reserved.
 * The information in this document is the property of Ericsson.  Except
 * as specifically authorized in writing by Ericsson, the receiver of
 * this document shall keep the information contained herein confidential
 * and shall protect the same in whole or in part from disclosure and
 * dissemination to third parties.  Disclosure and disseminations to the
 * receivers employees shall only be made on a strict need to know basis.
 *
 * ---------------------------------------------------------------------------
 */
#ifndef MPU_API_H_
#define MPU_API_H_

#include <stdint.h>
#include <math.h>

/* error codes */
#define MPU_EREGID          1   /* invalid region id */
#define MPU_EREGSIZE        2   /* invalid region size */
#define MPU_EREGALIGN       3   /* region address and size are not aligned */

/* bit positions */
#define MPU_RASR_SRD_POS   8
#define MPU_RASR_AP_POS    24
#define MPU_RASR_XN_POS    28
#define MPU_RASR_TEX_POS   19
#define MPU_RASR_XX_POS    MPU_RASR_TEX_POS
#define MPU_RASR_C_POS     17
#define MPU_RASR_B_POS     16
#define MPU_RASR_YY_POS    MPU_RASR_B_POS
#define MPU_RASR_S_POS     18

/* attributes */
/* access permissions */
#define MPU_RASR_AP_NO_ACCESS     0x0
#define MPU_RASR_AP_FULL_ACCESS   (0x3UL << MPU_RASR_AP_POS)
#define MPU_RASR_AP_READ_ONLY     (0x7UL << MPU_RASR_AP_POS)

/* sub-region disable */
#define MPU_RASR_DISABLE_SR(x)    (0x1 << (MPU_RASR_SRD_POS + (x)))

/* execute never */
#define MPU_RASR_XN               (0x1 << MPU_RASR_XN_POS)

/******************************************************************************
 *  Memory type, cacheable properties, shareablility.
 *
 *  S bit is ignored, marked by S where shareable, N where not
 *  shareable. Otherwise use S bit only.
 *
 *  TEX | C | B |  S
 *  000   0   0    S  - Strongly ordered (MPU_RASR_TEXCB_SO)
 *  000   0   1    S  - Shared device (MPU_RASR_TEXCB_SD)
 *  000   1   0   0/1 - Outer and inner write-through, no write alloc
 *                      (MPU_RASR_TEXCB_OIWTNWA)
 *  000   1   1   0/1 - Outer and inner write-back, no write alloc
 *                      (MPU_RASR_TEXCB_OIWBNWA)
 *  001   0   0   0/1 - Outer and inner non-cacheable (MPU_RASR_TEXCB_OINC)
 *  001   1   1   0/1 - Outer and inner write-back, write and read alloc
 *                      (MPU_RASR_TEXCB_OIWBWRA)
 *  010   0   0    N  - Non shared device
 *                      (MPU_RASR_TEXCB_NSD)
 *  1XX   Y   Y   0/1 - Cached memory with various cacheability policies.
 *                      See cacheability policy table below for possible XX and
 *                      YY values.
 *
 *
 */

/* S bit only */
#define MPU_RASR_SHAREABLE           (0x1 << MPU_RASR_S_POS)
#define MPU_RASR_NOT_SHAREABLE       0x0

/* TEX, C and B */
#define MPU_RASR_TEXCB_SO        0x0
#define MPU_RASR_TEXCB_SD        (0x1 << MPU_RASR_B_POS)
#define MPU_RASR_TEXCB_OIWTNWA   (0x1 << MPU_RASR_C_POS)
#define MPU_RASR_TEXCB_OIWBNWA   ((0x1 << MPU_RASR_B_POS) | \
                                  (0x1 << MPU_RASR_C_POS))
#define MPU_RASR_TEXCB_OINC      (0x1 << MPU_RASR_TEX_POS)
#define MPU_RASR_TEXCB_OIWBWRA   ((0x1 << MPU_RASR_TEX_POS) | \
                                  (0x1 << MPU_RASR_C_POS) | \
                                  (0x1 << MPU_RASR_B_POS))
#define MPU_RASR_TEXCB_NSD       (0x2 << MPU_RASR_TEX_POS)
#define MPU_RASR_TEXCB_VAR       (0x4 << MPU_RASR_TEX_POS)

/* cacheability policy
 *
 * NC - non-cacheable
 * WBWRA - Write-back, write and read alloc
 * WTNWA - Write-through, no write alloc
 * WBNWA - Write-back, no write alloc
 *
 *
 * Example mask:
 * MPU_RASR_TEXCB_VAR |
 * (MPU_CP_WBWRA << MPU_RASR_XX_POS) |
 * (MPU_CP_WBNWA << MPU_RASR_YY_POS) |
 * MPU_RASR_SHAREABLE
 */
#define MPU_CP_NC 0x0
#define MPU_CP_WBWRA 0x1
#define MPU_CP_WTNWA 0x2
#define MPU_CP_WBNWA 0x3

/******************************************************************************
 *  End of memory type, cacheable properties, shareablility.
 */

/**
 *  @brief
 *
 *  Enables MPU by setting all bits in MPU_CTRL to 1.
 *  Additionaly it sets MEMFAULTENA bit in SCB_SHCSC to 1.
 *
 */
void mpu_enable(void);

/**
 * @brief
 * Disables MPU by setting all bits in MPU_CTRL to 0.
 * Sets MEMFAULTENA bit in SCB_SHCSC to 1.
 *
 */
void mpu_disable(void);

/**
 * @brief
 * Setup of a new or existing region, where attributes can be changed.
 *
 * @param id    Region id, has to be between 0 and 7. Also defines
 *              region priority. 7 has the highest.
 * @param addr  Region base address. Must be a multiple
 *              of region size.
 * @param size  Size in bytes, valid values are only powers 2, between, and
 *              including 32B and 4GB. For 4GB use 0xFFFFFFFF.
 * @param attr  Various attributes affecting access and execution
 *              permissions, memory type, etc. Used for MPU_RASR.
 *              All attributes are bit masks and can be combined with each
 *              other.
 *
 *              Supported attributes are:
 *                 Access permissions:
 *                    MPU_RASR_AP_NO_ACCESS
 *                    MPU_RASR_AP_FULL_ACCESS
 *                    MPU_RASR_AP_READ_ONLY
 *                 Sub-region disable bits:
 *                    MPU_RASR_DISABLE_SR(x) - x is the disabled sub-region.
 *                       One of these is needed for *every* sub-region we want
 *                       to disable.
 *                 Execute never bit:
 *                    MPU_RASR_XN
 *                 Memory type and cacheability:
 *                    MPU_RASR_TEXCB_SO
 *                    MPU_RASR_TEXCB_SD
 *                    MPU_RASR_TEXCB_OIWTNWA
 *                    MPU_RASR_TEXCB_OIWBNWA
 *                    MPU_RASR_TEXCB_OINC
 *                    MPU_RASR_TEXCB_OIWBWRA
 *                    MPU_RASR_TEXCB_NSD
 *                    MPU_RASR_TEXCB_VAR
 *                       Combined with:
 *                          MPU_CP_NC
 *                          MPU_CP_WBWRA
 *                          MPU_CP_WTNWA
 *                          MPU_CP_WBNWA
 *                 Shareability (where applicable):
 *                    MPU_RASR_SHAREABLE
 *                    MPU_RASR_NOT_SHAREABLE
 * @return      Error code, or 0 if successful.
 *
 * @example     mpu_configure_region(0, 0x40000000, 256, //256 bytes at given address
 *                      MPU_RASR_AP_NO_ACCESS | // access permission
 *                      MPU_RASR_DISABLE_SR(0) | // disable first subregion
 *                      MPU_RASR_TEXCB_VAR | // separate caching for inner and outer
 *                      (MPU_CP_WBWRA << MPU_RASR_XX_POS) | //inner caching policy
 *                      (MPU_CP_WBNWA << MPU_RASR_YY_POS) | //outer caching policy
 *                      MPU_RASR_SHAREABLE); //shareable
 *
 *
 */
int mpu_configure_region(uint32_t id, uint32_t addr, uint64_t size, uint32_t attr);

/**
 * @brief
 *
 * Removes and disables region.
 *
 * @param id Region that will be removed.
 *
 */
int mpu_clear_region(uint32_t id);

#endif /* MPU_API_H_ */
