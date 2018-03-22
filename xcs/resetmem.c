/**
 *   Copyright (C) 2015 by Ericsson AB. All rights reserved. The
 *   information in this document is the property of Ericsson. Except
 *   as specifically authorized in writing by Ericsson, the receiver
 *   of this document shall keep the information contained herein
 *   confidential and shall protect the same in whole or in part from
 *   disclosure and dissemination to third parties. Disclosure and
 *   disseminations to the receiver's employees shall only be made on
 *   a strict need to know basis.
 */
#include <stdlib.h>
#include <stdint.h>
#include <unistd.h>
#include <sys/reboot.h>
#include <uio_helper.h>
#include "booti_restart.h"
#include "libresetmem.h"
#include "ma_mimo_trxmRegisters.h"

#define TRACEPOINT_PROVIDER com_ericsson_xcs_libresetmem
#include <tpt_create.h>
#include <tpt.h>

#define UIO_DEV_RESET_MEM "misc_reset"
/* reset_crtl register (0xFF5E0000) */
#define UIO_DEV_SLCR_RESET "reset_ctrl"
#define SLCR_RESET_OFFSET     (uint32_t)(0x218)

#define REG_RESET_MEM   (volatile uint32_t *)((uintptr_t) reset_mem_base)

#define REG_RESET_CTRL \
	(volatile uint32_t *)((uintptr_t) slcr_reset_base + SLCR_RESET_OFFSET)

#define LMC_AUBOOT BOOTI_RESTART_LMC_AUBOOT

#define REG_RESET_MASK         0x00000010
#define RESET_SYSTEM           0x1


/**
 * Restart board
 */
void resetmem_restart_board(int32_t slot_number, uint32_t reboot_type)
{
	void *handle;
	void *reset_mem_base = NULL;
	void *slcr_reset_base = NULL;

	if (slot_number < 0)
		slot_number = LMC_AUBOOT;

	handle = (void *) uio_open(UIO_DEV_RESET_MEM);
	if (handle  == UIO_OPEN_FAILED) {
		TPT_ERROR(STR("Failed to open UIO device \"%s\"",
		              UIO_DEV_RESET_MEM));
		goto kernel_restart;
	}

	reset_mem_base = uio_mmap(handle);
	if (reset_mem_base == MAP_FAILED || reset_mem_base == NULL) {
		TPT_ERROR("Unable to perform UIO memory mapping");
		uio_close(handle);
		goto kernel_restart;
	}

	handle = (void *) uio_open(UIO_DEV_SLCR_RESET);
	if (handle  == UIO_OPEN_FAILED) {
		TPT_ERROR(STR("Failed to open UIO device \"%s\"",
		              UIO_DEV_SLCR_RESET));
		goto kernel_restart;
	}

	slcr_reset_base = uio_mmap(handle);
	if (slcr_reset_base == MAP_FAILED || slcr_reset_base == NULL) {
		TPT_ERROR("Unable to perform UIO memory mapping");
		uio_close(handle);
		goto kernel_restart;
	}
	/* Clear the register */
	if (reboot_type != BOOTI_RESTART_TYPE_CRASH) {
		/* SW ordered restart */
		*REG_RESET_MEM &= ~(BOOTI_RESTART_TYPE_MASK |
		                    BOOTI_RESTART_LMC_MASK);
		*REG_RESET_MEM |= reboot_type | slot_number;
	} else {
		/* Restart due to crash */
		*REG_RESET_MEM &= ~BOOTI_RESTART_TYPE_MASK;
		*REG_RESET_MEM |= reboot_type;
	}

kernel_restart:
	TPT_TRACE(1,"Rebooting system");
#if 0
        /*FIXME: workaround for trxm pa protection */
        if(wk_pa_protection())
            TPT_ERROR(STR("Failed to set PA Protection"));
#endif
	sync();
        /* FIXME: workaround for llog missing issue */
	system("/etc/init.d/resetmem_shutdown.sh");
	/* FIXME: why doesn't reboot(RB_AUTOBOOT) work? */
	system("/sbin/reboot -d -f");
	
        TPT_ERROR(STR("Kernel restart failed, set the reset register"));
	if (slcr_reset_base)
		*REG_RESET_CTRL &= ~REG_RESET_MASK;
        *REG_RESET_CTRL |= (RESET_SYSTEM << 4);

	TPT_ERROR(STR("Failed to set the reset register, killing watchdog"));
	while (1);
	/* We should never return in this function */
	return;

}

static int clear_mask(uint32_t mask)
{
	void *handle;
	void *reset_mem_base = NULL;

	handle = (void *) uio_open(UIO_DEV_RESET_MEM);
	if (handle  == UIO_OPEN_FAILED) {
		TPT_ERROR(STR("Failed to open UIO device \"%s\"",
		              UIO_DEV_RESET_MEM));
		return 1;
	}

	reset_mem_base = uio_mmap(handle);
	if (reset_mem_base == MAP_FAILED || reset_mem_base == NULL) {
		TPT_ERROR("Unable to perform UIO memory mapping");
		uio_close(handle);
		return 1;
	}
	*REG_RESET_MEM &= ~(mask);
	uio_close(handle);
	return 0;
}

int resetmem_clear_restart_counter(void)
{
	int res = clear_mask(BOOTI_RESTART_COUNT_MASK);
	if(!res)
		TPT_TRACE(1,"RESET_MEM \"restart counter\" has been cleared");
	return res;
}

int resetmem_clear_restart_in_boot_counter(void)
{
	int res = clear_mask(BOOTI_RESTART_IN_BOOT_COUNT_MASK);
	if(!res)
		TPT_TRACE(1,"RESET_MEM \"restart in boot counter\" has been cleared");
	return res;
}

static inline void write_fpga_reg(void *base, uint32_t off, uint32_t val)
{
      (*(volatile uint32_t *)((intptr_t)base + off*4)) = val;
}

int wk_pa_protection(void)
{   
	void *handle;       
    void *pa_protect_base = NULL; 

    if(strcmp(getenv("SYS_BOARD_TYPE"),"TRXM") == 0)
        {
        	handle = (void *) uio_open("trxm_fpga");
            if (handle  == UIO_OPEN_FAILED) {
                TPT_ERROR(STR("Failed to open UIO device trxm_fpga"));
                return 1;
                }
            
            pa_protect_base = uio_mmap(handle);
            if (pa_protect_base == MAP_FAILED || pa_protect_base == NULL) {
                TPT_ERROR("Unable to perform UIO memory mapping");
                uio_close(handle);
                return 1;
                }

            write_fpga_reg(pa_protect_base, MM_T_DUC_C0_SRC_SEL_0, 0x77777777);
            write_fpga_reg(pa_protect_base, MM_T_DUC_C0_SRC_SEL_1, 0x77777777);
            write_fpga_reg(pa_protect_base, MM_T_MISC_TD_SW_CTRL, 0x80001111);
            write_fpga_reg(pa_protect_base, MM_T_MISC_TD_SW_CTRL_PA, 0x0);

            uio_munmap(handle);
            uio_close(handle);
            return 0;
     }
    return 0;
}

