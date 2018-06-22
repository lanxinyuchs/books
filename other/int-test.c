/**
 *   Copyright (C) 2018 by Ericsson AB. All rights reserved. The
 *   information in this document is the property of Ericsson. Except
 *   as specifically authorized in writing by Ericsson, the receiver
 *   of this document shall keep the information contained herein
 *   confidential and shall protect the same in whole or in part from
 *   disclosure and dissemination to third parties. Disclosure and
 *   disseminations to the receiver's employees shall only be made on
 *   a strict need to know basis.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <uio_helper.h>

#include "ma_mimo_trxmRegisters.h"
#include "ma_mimo_backplaneRegisters.h"

#define BP             0
#define TRXM           1

void *fpga_uio_handle = NULL;
void *fpga_mmap_base = NULL;
static char dev_name[64];
static uint32_t board = 255;
static uint32_t ecp_num = 0;

uint32_t bp_ecp_trap[6] = { MM_B_ECP0_IRQ_TRAP, MM_B_ECP1_IRQ_TRAP, MM_B_ECP2_IRQ_TRAP,
                            MM_B_ECP3_IRQ_TRAP, MM_B_ECP4_IRQ_TRAP, MM_B_ECP5_IRQ_TRAP,
                            };

uint32_t bp_ecp_mask[6] = { MM_B_ECP0_IRQ_MASK, MM_B_ECP1_IRQ_MASK, MM_B_ECP2_IRQ_MASK,
                            MM_B_ECP3_IRQ_MASK, MM_B_ECP4_IRQ_MASK, MM_B_ECP5_IRQ_MASK,
                            };

uint32_t bp_ecp_force[6] = { MM_B_ECP0_IRQ_FORCE, MM_B_ECP1_IRQ_FORCE, MM_B_ECP2_IRQ_FORCE,
                             MM_B_ECP3_IRQ_FORCE, MM_B_ECP4_IRQ_FORCE, MM_B_ECP5_IRQ_FORCE,
                             };

uint32_t trxm_ecp_trap[1] =  { MM_T_ECP0_IRQ_TRAP };
uint32_t trxm_ecp_mask[1] =  { MM_T_ECP0_IRQ_MASK };
uint32_t trxm_ecp_force[1] = { MM_T_ECP0_IRQ_FORCE };

static uint32_t* ecp_trap = NULL;
static uint32_t* ecp_mask = NULL;
static uint32_t* ecp_force = NULL;

static uint32_t misc_trap = NULL;
static uint32_t misc_mask = NULL;
static uint32_t misc_force = NULL;

void print_usage(char *program);

static inline void write_fpga_reg(void *base, uint32_t off, uint32_t val)
{
    (*(volatile uint32_t *)((intptr_t)base + off*4)) = val;
}

static inline uint32_t read_fpga_reg(void *base, uint32_t off)
{
    uint32_t value = 0;
    value = (*(volatile uint32_t *)((intptr_t)base + off*4));

    return value;
}

int main(int argc, char **argv)
{
    uint32_t ecp_trap_val = 0;
    uint32_t ecp_mask_val = 0;
    uint32_t misc_trap_val = 0;
    uint32_t misc_mask_val = 0;
    uint32_t ecp_loops = 0;
    uint32_t misc_loops = 0;

    if (argc != 4) {
        print_usage(argv[0]);
        return 0;
    }

    memset(dev_name,0,sizeof(dev_name));
    if (strcmp(argv[1], "bp") == 0)
    {
        board = BP;
        strcpy(dev_name, "bp_reg");

        ecp_trap = bp_ecp_trap;
        ecp_mask = bp_ecp_mask;
        ecp_force = bp_ecp_force;
        ecp_num = 6;

        misc_trap = MM_B_MISC_IRQ_TRAP;
        misc_mask = MM_B_MISC_IRQ_MASK;
        misc_force = MM_B_MISC_IRQ_FORCE;
    }
    else if (strcmp(argv[1], "trxm") == 0)
    {
        board = TRXM;
        strcpy(dev_name, "trxm_reg");

        ecp_trap = trxm_ecp_trap;
        ecp_mask = trxm_ecp_mask;
        ecp_force = trxm_ecp_force;
        ecp_num = 1;

        misc_trap = MM_T_MISC_IRQ_TRAP;
        misc_mask = MM_T_MISC_IRQ_MASK;
        misc_force = MM_T_MISC_IRQ_FORCE;
    }

    fpga_uio_handle = (void *) uio_open(dev_name);
    if (fpga_uio_handle == (UIO_HANDLE_) - 1)
    {
        printf("fpga uio_open funciton return error\n");
        return -1;
    }

    fpga_mmap_base = uio_mmap(fpga_uio_handle);
    if (fpga_mmap_base == MAP_FAILED)
    {
        fpga_mmap_base = NULL;
        printf("fpga uio_mmap funciton return error\n");
        uio_close(fpga_uio_handle);
        return -1;
    }
    printf("fpga reg map success\n");

    ecp_loops = strtoul(argv[2], NULL, 0);

    if(ecp_loops > 0)
    {
        printf("check ecp irq reg status:\n");
        for(uint32_t i=0; i<ecp_num; i++)
        {
            ecp_trap_val = read_fpga_reg(fpga_mmap_base, ecp_trap[i]);
            ecp_mask_val = read_fpga_reg(fpga_mmap_base, ecp_mask[i]);
            printf("ecp%d, trap = 0x%x, mask = 0x%x \n", i, ecp_trap_val, ecp_mask_val);
        }

        printf("trigger ecp interrupt:\n");

        for(uint32_t cnt=0; cnt<ecp_loops; cnt++)
        {
            for(uint32_t i=0; i<ecp_num; i++)
            {
                while(1)
                {
                    if(read_fpga_reg(fpga_mmap_base, ecp_trap[i]) != 0)
                        continue;
                    write_fpga_reg(fpga_mmap_base, ecp_force[i], 0xffffffff);
                    break;
                }
            }
        }

        printf("ecp interrupt %d times done.\n", ecp_loops);

    }

    misc_loops = strtoul(argv[3], NULL, 0);

    if(misc_loops > 0)
    {
        printf("check misc irq reg status:\n");

        misc_trap_val = read_fpga_reg(fpga_mmap_base, misc_trap);
        misc_mask_val = read_fpga_reg(fpga_mmap_base, misc_mask);
        printf("misc interrupt, trap = 0x%x, mask = 0x%x \n",  misc_trap_val, misc_mask_val);

        printf("trigger misc interrupt:\n");

        for(uint32_t cnt=0; cnt<misc_loops; cnt++)
        {
            while(1)
            {
                if(read_fpga_reg(fpga_mmap_base, misc_mask) != 0) //wait for rhd-int complete
                    continue;
                write_fpga_reg(fpga_mmap_base, misc_mask, 0xffffffff); //enable mask
                write_fpga_reg(fpga_mmap_base, misc_force, 0xffffffff); //trigger again
                break;
            }
        }

        printf("misc interrupt %d times done.\n", misc_loops);

    }

    return 0;
}

void print_usage(char *program) {
    printf("Built: %s %s\n", __DATE__, __TIME__);
    printf("Run with arguments %s <board> <ecp loops> <misc loop>\n", program);
    printf("Example, trigger 10 times ecp interrupt and 10 misc interrupt on BP board: %s bp 10 10 \n", program);
    printf("Example, trigger 10 times ecp interrupt and 10 misc interrupt on TRXM board: %s trxm 10 10 \n", program);
}

