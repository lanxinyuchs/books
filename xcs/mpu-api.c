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

#include "mpu-api.h"

/* registers */
#define SCB_SHCSR       0xe000ed24
#define REG_MPU_TYPE        0xe000ed90
#define REG_MPU_CTRL        0xe000ed94
#define REG_MPU_RBAR        0xe000ed9C
#define REG_MPU_RASR        0xe000eda0

/* bit positions */
#define SCB_SHCSR_MEMFAULT_ENABLE_POS 16
#define RASR_SIZE_POS  1

/* masks */
#define SCB_SHCSR_MEMFAULT_ENABLE_MSK (1UL << SCB_SHCSR_MEMFAULT_ENABLE_POS)
#define CTRL_ENABLE_ALL      0x5
#define REGION_ENABLE_MSK  0x00000001
#define REGION_VALID_MSK  0x00000010

/* defines */
#define MAX_REGION_SIZE 31
#define MIN_REGION_SIZE 4
#define MAX_REGION_NUM 7

/* region sizes, internal use only */
enum region_size {
   REGION_SIZE_32B = 4,
   REGION_SIZE_64B,
   REGION_SIZE_128B,
   REGION_SIZE_256B,
   REGION_SIZE_512B,
   REGION_SIZE_1KB,
   REGION_SIZE_2KB,
   REGION_SIZE_4KB,
   REGION_SIZE_8KB,
   REGION_SIZE_16KB,
   REGION_SIZE_32KB,
   REGION_SIZE_64KB,
   REGION_SIZE_128KB,
   REGION_SIZE_256KB,
   REGION_SIZE_512KB,
   REGION_SIZE_1MB,
   REGION_SIZE_2MB,
   REGION_SIZE_4MB,
   REGION_SIZE_8MB,
   REGION_SIZE_16MB,
   REGION_SIZE_32MB,
   REGION_SIZE_64MB,
   REGION_SIZE_128MB,
   REGION_SIZE_256MB,
   REGION_SIZE_512MB,
   REGION_SIZE_1GB,
   REGION_SIZE_2GB,
   REGION_SIZE_4GB
};

static void flush_dp(void) {
        asm("DSB");
        asm("ISB");
}
/* Sets SHCSR_MEMFAULTENA to 1. */
static void enable_memfault(void) {
        volatile uint32_t *word = (uint32_t *) SCB_SHCSR;
        *word |= (uint32_t) SCB_SHCSR_MEMFAULT_ENABLE_MSK;
        flush_dp();
}

/* Sets SHCSR_MEMFAULTENA to 0. */
static void disable_memfault(void) {
        volatile uint32_t *word = (uint32_t *) SCB_SHCSR;
        *word &= (uint32_t) ~(SCB_SHCSR_MEMFAULT_ENABLE_MSK);
        flush_dp();
}

/* region size lookup */
static enum region_size convert_size(uint64_t size_in_bytes) {
        struct conversion_t {
                uint64_t size_in_bytes;
                enum region_size value;
        };

        struct conversion_t table[] = {
                {32, REGION_SIZE_32B},
                {64, REGION_SIZE_64B},
                {128, REGION_SIZE_128B},
                {256, REGION_SIZE_256B},
                {512, REGION_SIZE_512B},
                {1024, REGION_SIZE_1KB},
                {2048, REGION_SIZE_2KB},
                {4096, REGION_SIZE_4KB},
                {8192, REGION_SIZE_8KB},
                {16384, REGION_SIZE_16KB},
                {32768, REGION_SIZE_32KB},
                {65536, REGION_SIZE_64KB},
                {131072, REGION_SIZE_128KB},
                {262144, REGION_SIZE_256KB},
                {524288, REGION_SIZE_512KB},
                {1048576, REGION_SIZE_1MB},
                {2097152, REGION_SIZE_2MB},
                {4194304, REGION_SIZE_4MB},
                {8388608, REGION_SIZE_8MB},
                {16777216, REGION_SIZE_16MB},
                {33554432, REGION_SIZE_32MB},
                {67108864, REGION_SIZE_64MB},
                {134217728, REGION_SIZE_128MB},
                {268435456, REGION_SIZE_256MB},
                {536870912, REGION_SIZE_512MB},
                {1073741824, REGION_SIZE_1GB},
                {2147483648, REGION_SIZE_2GB},
                {4294967296, REGION_SIZE_4GB}
        };
        for (struct conversion_t *p = table; (p->value >= REGION_SIZE_32B) &&
             (p->value <= REGION_SIZE_4GB); ++p) {
                if (p->size_in_bytes == size_in_bytes)
                        return p->value;
        }
        return -MPU_EREGSIZE;
}

static int count_trailing_zeros(uint32_t number) {
        int c = __builtin_clz(number & -number);
        return number ? 31 - c : c;
}

static int region_id_check(uint32_t id) {
        /* Read max region num from MPU_TYPE */
        uint32_t max_region_num = (*((uint32_t *) REG_MPU_TYPE) >> 8) & 0x000000FF;
        if (id >= max_region_num)
                return -MPU_EREGID;
        return 0;
}

static int region_params_check(uint32_t id, uint32_t addr,
                enum region_size size, uint32_t attr) {

        int ret = region_id_check(id);
        if (ret)
                return ret;

        if ((size < REGION_SIZE_32B) || (size > REGION_SIZE_4GB) )
                return -MPU_EREGSIZE;
        /*
         * this is the itchy part, the regions size and base address
         * have to be aligned. If we want a 1GB region, it needs to be
         * aligne to 1GB addresses (e.g. 0x40000000 (1gb mark)
         */
        uint32_t addr_zeros = count_trailing_zeros(addr);

        /* size in bytes is 2^(size+1) */
        uint32_t size_zeros = count_trailing_zeros(0x1 << (size+1));

        /* if addr has more or the same amount of zeros on the right side
         * than size, that means that it is a multiple of size.
         */
        if (addr_zeros < size_zeros)
                return -MPU_EREGALIGN;

        return 0;
}

void mpu_enable(void) {
        enable_memfault();
        volatile uint32_t *mpu_ctrl = (uint32_t *) REG_MPU_CTRL;
        *mpu_ctrl = (uint32_t) CTRL_ENABLE_ALL;
        flush_dp();
}

void mpu_disable(void) {
        volatile uint32_t *mpu_ctrl = (uint32_t *) REG_MPU_CTRL;
        *mpu_ctrl = (uint32_t) ~(CTRL_ENABLE_ALL);
        flush_dp();
        disable_memfault();
}

static void configure_mpu_rbar(uint32_t rbar) {
        volatile uint32_t *mpu_rbar = (uint32_t *) REG_MPU_RBAR;
        *mpu_rbar = rbar;
        flush_dp();
}

static void configure_mpu_rasr(uint32_t rasr) {
        volatile uint32_t *mpu_rasr = (uint32_t *) REG_MPU_RASR;
        *mpu_rasr = rasr;
        flush_dp();
}

int mpu_configure_region(uint32_t id, uint32_t addr, uint64_t size, uint32_t attr) {

        enum region_size reg_size = convert_size(size);

        int ret = region_params_check(id, addr, reg_size, attr);
        if (ret)
                return ret;

        /* bit MPU_REGION_VALID_MSK allows setting of RNR through RBAR */
        configure_mpu_rbar(id |
                        addr |
                        REGION_VALID_MSK);

        configure_mpu_rasr((((uint32_t) reg_size) << RASR_SIZE_POS) |
                        REGION_ENABLE_MSK |
                        attr);
        return 0;
}

int mpu_clear_region(uint32_t id) {

        int ret = region_id_check(id);
        if (ret)
                return ret;

        configure_mpu_rbar(id | 0x0 | REGION_VALID_MSK);
        configure_mpu_rasr(0x0);

        return 0;
}
