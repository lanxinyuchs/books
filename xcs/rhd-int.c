/* ---------------------------------------------------------------------------
 *
 * © Ericsson AB 2014 All rights reserved.
 * The information in this document is the property of Ericsson.  Except
 * as specifically authorized in writing by Ericsson, the receiver of
 * this document shall keep the information contained herein confidential
 * and shall protect the same in whole or in part from disclosure and
 * dissemination to third parties.  Disclosure and disseminations to the
 * receivers employees shall only be made on a strict need to know basis.
 *
 * ---------------------------------------------------------------------------
 */

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <unistd.h>
#include <stdbool.h>
#include <string.h>
#include <sys/types.h>
#include <signal.h>
#include <itc.h>
#include <uio_helper.h>
#include <time.h>
#include "rhd-int-if.h"
#include "rhd-common.h"

#define TRACEPOINT_PROVIDER     com_ericsson_xcs_rhd_int
#include "tpt_create.h"
#include "tpt.h"
/*--------------------------  CONSTANTS  -----------------------------------*/

#if 0
#undef TPT_ERROR
#undef TPT_INFO
#undef TPT_TRACE
#undef TPT_REC_SIG
#undef TPT_SEND_SIG

#define TPT_ERROR(msg) printf("Line %d %s \n", __LINE__, msg)
#define TPT_INFO(msg) printf("Line %d %s \n", __LINE__, msg)
#define TPT_TRACE(group, msg) printf("Line %d %s \n", __LINE__, msg)
#define TPT_REC_SIG(sig, msg) printf("Line %d Receive message %s \n", __LINE__, msg)
#define TPT_SEND_SIG(sig, pid, msg) printf("Line %d Send message %s \n", __LINE__,  msg)
#endif

/*----------------------------  MACROS  -------------------------------------*/
static char *daemon_name = "rhd-intd";


/*----------------------------  Structs and typedefs  -----------------------*/
union itc_msg
{
    uint32_t sigNo;
    interruptChangeIndS_t                  interruptChangeInd;
    RicrInternalLinkStatusChangeS      ricrInternalLinkStatusChange;
    MricrInternalLinkStatusChangeS     mricrInternalLinkStatusChange;
};

/*----------------------------  Definition of Global Variables  -------------*/

/*----------------------------  Definition of Local Variables  --------------*/

/*----------------------------  Declaration of Local Functions  -------------*/

/*----------------------------  Declaration of External Functions  ----------*/

/*----------------------------  Function Definitions  -----------------------*/
void (*bp_int_handler[32])(uint32_t irq_no) =
{
    bp_int_notify,   bp_int_cpri,     bp_int_cpri,     bp_int_cpri,
    bp_int_m_cpri,   bp_int_m_cpri,   bp_int_m_cpri,   bp_int_m_cpri,
    bp_int_notify,   bp_int_notify,   bp_int_notify,   fpga_int_dummy,
    fpga_int_dummy,  fpga_int_dummy,  fpga_int_dummy,  bp_int_notify,
    bp_int_notify,   bp_int_notify,   bp_int_notify,   bp_int_notify,
    bp_int_notify,   bp_int_notify,   bp_int_mmi,      bp_int_mmi,
    bp_int_ewtimer,  bp_int_dbuart,   bp_int_notify,   bp_int_notify,
    bp_int_notify,   bp_int_ac_ctrl,  bp_int_ac_ctrl,  fpga_int_dummy};

void (*trxm_int_handler[32])(uint32_t irq_no) =
{
    trxm_int_notify,  trxm_int_notify,  trxm_int_notify,  trxm_int_notify,
    trxm_int_notify,  trxm_int_notify,  trxm_int_notify,  trxm_int_notify,
    trxm_int_notify,  trxm_int_notify,  trxm_int_m_cpri,  trxm_int_notify,
    trxm_int_notify,  trxm_int_notify,  trxm_int_notify,  trxm_int_notify,
    trxm_int_notify,  trxm_int_notify,  trxm_int_notify,  trxm_int_notify,
    trxm_int_notify,  trxm_int_notify,  fpga_int_dummy,   fpga_int_dummy,
    fpga_int_dummy,   fpga_int_dummy,   fpga_int_dummy,   fpga_int_dummy,
    fpga_int_dummy,   fpga_int_dummy,   fpga_int_dummy,   fpga_int_dummy
};

static int get_mbox(const char *mbox_name, itc_mbox_id_t *mbox_id)
{
    TPT_TRACE(1, STR("Try to get mbox %s.", mbox_name));

    *mbox_id= itc_locate(mbox_name);
    if (*mbox_id== ITC_NO_ID) {
        TPT_ERROR(STR("%s does not exist", mbox_name));
        return -1;
    }

    TPT_TRACE(1, STR("Get mbox %s success.", mbox_name));
    return 0;
}

static inline void write_fpga_reg(void *base, uint32_t off, uint32_t val)
{
    TPT_TRACE(1, STR("Write: base = 0x%x, off = 0x%x, val = 0x%x .", (uint32_t)base, off, val));
    (*(volatile uint32_t *)((intptr_t)base + off*4)) = val;
}

static inline uint32_t read_fpga_reg(void *base, uint32_t off)
{
    uint32_t value = 0;
    value = (*(volatile uint32_t *)((intptr_t)base + off*4));

    TPT_TRACE(1, STR("Read: base = 0x%x, off = 0x%x, val = 0x%x .", (uint32_t)base, off, value));

    return value;
}

static inline void write_fpga_reg_mask(void *base, uint32_t off, uint32_t val, uint32_t mask)
{
    uint32_t oldData = 0;
    oldData = read_fpga_reg(base, off);
    write_fpga_reg(base, off, (uint32_t)((mask & val)|(oldData & ~(mask))));
}

void disableBpSubInt(uint32_t irq_no)
{
    uint32_t subIntTrap = 0;

    TPT_TRACE(1, STR("Disable BP sub Int, irq_no = %d .", irq_no));

    if (bpSubIntMask[irq_no][MASK] != 0)
    {
        //read sub-module interrupt trap
        subIntTrap = read_fpga_reg(bp_reg_mmap_base, bpSubIntMask[irq_no][TRAP]);
        //only clear the bits that interrupt happens
        write_fpga_reg_mask(bp_reg_mmap_base, bpSubIntMask[irq_no][MASK], 0x00, subIntTrap);
    }
}

void disableTrxmSubInt(uint32_t irq_no)
{
    uint32_t subIntMask = NULL;
    uint32_t subIntTrap = NULL;
    uint32_t index = 0;
    uint32_t ddc_reg[2][2] = {
                             {MM_T_DDC_OFL_0_TRAP, MM_T_DDC_OFL_0_MASK},
                             {MM_T_DDC_OFL_0_TRAP, MM_T_DDC_OFL_0_MASK}
                             };

    uint32_t duc_reg[2][2] = {
                             {MM_T_DDC_OFL_0_TRAP, MM_T_DDC_OFL_0_MASK},
                             {MM_T_DDC_OFL_0_TRAP, MM_T_DDC_OFL_0_MASK}
                             };

    uint32_t fubf_reg[9][2] = {
                              {MM_T_IFFT_FBUF_IRQ_ZERO_OUT_C0_TRAP, MM_T_IFFT_FBUF_IRQ_ZERO_OUT_C0_MASK},
                              {MM_T_IFFT_FBUF_IRQ_ZERO_OUT_C1_TRAP, MM_T_IFFT_FBUF_IRQ_ZERO_OUT_C1_MASK},
                              {MM_T_IFFT_FBUF_IRQ_ZERO_OUT_C2_TRAP, MM_T_IFFT_FBUF_IRQ_ZERO_OUT_C2_MASK},
                              {MM_T_IFFT_FBUF_IRQ_RFS_MISS_C0_TRAP, MM_T_IFFT_FBUF_IRQ_RFS_MISS_C0_MASK},
                              {MM_T_IFFT_FBUF_IRQ_RFS_MISS_C1_TRAP, MM_T_IFFT_FBUF_IRQ_RFS_MISS_C1_MASK},
                              {MM_T_IFFT_FBUF_IRQ_RFS_MISS_C2_TRAP, MM_T_IFFT_FBUF_IRQ_RFS_MISS_C2_MASK},
                              };

    TPT_TRACE(1, STR("Disable TRXM sub Int, irq_no = %d .", irq_no));

    if(trxmSubIntMask[irq_no][MASK] != 0)
    {
        //read sub-module interrupt trap
        subIntTrap = read_fpga_reg(trxm_reg_mmap_base, trxmSubIntMask[irq_no][TRAP]);
        //only clear the bits that interrupt happens
        write_fpga_reg_mask(trxm_reg_mmap_base, trxmSubIntMask[irq_no][MASK], 0x00, subIntTrap);
    }

    if(irq_no == T_DDC_IRQ)
    {

        for(index=0; index<2; index++)
        {
            subIntTrap = read_fpga_reg(trxm_reg_mmap_base, ddc_reg[index][TRAP]);
            subIntMask = read_fpga_reg(trxm_reg_mmap_base, ddc_reg[index][MASK]);
            if(subIntMask & subIntTrap) //if the top interrupt from carrier 0
            {
                //only clear the bits that interrupt happens
                write_fpga_reg_mask(trxm_reg_mmap_base, ddc_reg[index][MASK], 0x00, subIntTrap);
            }
        }
    }

    if(irq_no == T_DUC_IRQ)
    {
        for(index=0; index<2; index++)
        {
            subIntTrap = read_fpga_reg(trxm_reg_mmap_base, duc_reg[index][TRAP]);
            subIntMask = read_fpga_reg(trxm_reg_mmap_base, duc_reg[index][MASK]);
            if(subIntMask & subIntTrap) //if the top interrupt from carrier 0
            {
                //only clear the bits that interrupt happens
                write_fpga_reg_mask(trxm_reg_mmap_base, duc_reg[index][MASK], 0x00, subIntTrap);
            }
        }
    }

    if(irq_no == T_FUBF_IRQ)
    {
        for(index=0; index<9; index++)
        {
            subIntTrap = read_fpga_reg(trxm_reg_mmap_base, fubf_reg[index][TRAP]);
            subIntMask = read_fpga_reg(trxm_reg_mmap_base, fubf_reg[index][MASK]);
            if(subIntMask & subIntTrap) //if the top interrupt from carrier 0
            {
                //only clear the bits that interrupt happens
                write_fpga_reg_mask(trxm_reg_mmap_base, fubf_reg[index][MASK], 0x00, subIntTrap);
            }
        }
    }

    if(irq_no == T_204B_RX_IRQ)
    {
        subIntTrap = read_fpga_reg(trxm_reg_mmap_base, MM_T_JESD204B_RX_GLB_L0_IRQ);
        write_fpga_reg_mask(trxm_reg_mmap_base, MM_T_JESD204B_RX_GLB_L0_IRQ_MASK, 0x00, subIntTrap);

        subIntTrap = read_fpga_reg(trxm_reg_mmap_base, MM_T_JESD204B_RX_GLB_L0_IRQ1);
        write_fpga_reg_mask(trxm_reg_mmap_base, MM_T_JESD204B_RX_GLB_L0_IRQ1_MASK, 0x00, subIntTrap);
    }
}

void fpga_int_dummy(uint32_t irq_no)
{
    TPT_ERROR(STR("Should not happen, receive unexpect interrupt, irq_no = %d.", irq_no));
}

// send message to APP intHandler process
void bp_int_notify(uint32_t irq_no)
{
    union itc_msg *out;

    disableBpSubInt(irq_no);

    //clear interrupt status
    write_fpga_reg_mask(bp_int_mmap_base, MM_B_IRQ_IFR, (0x1 << irq_no), (0x1 << irq_no));

    TPT_TRACE(1, STR("irq_no(%d), int_app_mbox = 0x%x", irq_no, int_app_mbox));

    if(int_app_mbox == ITC_NO_ID)
    {
        if(get_mbox("intHandler", &int_app_mbox))
        {
            TPT_ERROR(STR("Receive irq_no = %d, but APP is not ready.", irq_no));
            return;
        }
    }

    out = itc_alloc(sizeof(interruptChangeIndS_t), INTERRUPT_CHANGE_IND);
    out->interruptChangeInd.intProcId = irq_no + FPGA_INT_OFFSET;

    TPT_SEND_SIG(out->sigNo, int_app_mbox, STR("Send out INTERRUPT_CHANGE_IND, irq_no = %d", irq_no));
    itc_send(&out, int_app_mbox, ITC_MY_MBOX);
}

void bp_int_ac_ctrl(uint32_t irq_no)
{
    union itc_msg *out;

    disableBpSubInt(irq_no);

    //clear interrupt status
    write_fpga_reg_mask(bp_int_mmap_base, MM_B_IRQ_IFR, (0x1 << irq_no), (0x1 << irq_no));

    TPT_TRACE(1, STR("irq_no(%d)", irq_no));

    if(int_ac_mbox == ITC_NO_ID)
    {
        if(get_mbox(AC_DISPATCH_MAILBOX, &int_ac_mbox))
        {
            TPT_ERROR(STR("Receive irq_no = %d, but ac control is not ready.", irq_no));
            return;
        }
    }

    out = itc_alloc(sizeof(interruptChangeIndS_t), AC_INT_CHANGE_IND);
    out->interruptChangeInd.intProcId = irq_no + FPGA_INT_OFFSET;

    TPT_SEND_SIG(out->sigNo, int_ac_mbox, STR("Send out AC_INT_CHANGE_IND, irq_no = %d", irq_no));
    itc_send(&out, int_ac_mbox, ITC_MY_MBOX);
}

// cpri int process
void bp_int_cpri(uint32_t irq_no)
{
    union itc_msg *out;
    uint32_t link_no;
    uint32_t status1, status2;
    uint32_t trap1, trap2;
    uint32_t mask1, mask2;
    uint32_t mon1, mon2;
    uint32_t ctrl, debug;

    TPT_TRACE(1, STR("irq_no(%d)", irq_no));

//    printf("irq_no(%d) in %s\n", irq_no, __func__);

    if (irq_no == B_DU_CPRI_LINK_0_IRQ)
    {
        link_no = 0;
        trap1 = read_fpga_reg(bp_reg_mmap_base, MM_B_CPRI0_STATUS_TRAP);
        mask1 = read_fpga_reg(bp_reg_mmap_base, MM_B_CPRI0_STATUS_MASK);
        trap2 = read_fpga_reg(bp_reg_mmap_base, MM_B_CPRI0_STATUS2_TRAP);
        mask2 = read_fpga_reg(bp_reg_mmap_base, MM_B_CPRI0_STATUS2_MASK);

        write_fpga_reg_mask(bp_reg_mmap_base, MM_B_CPRI0_STATUS_MASK, 0x00, trap1); //disable cpri sub-module interrupt
        write_fpga_reg_mask(bp_reg_mmap_base, MM_B_CPRI0_STATUS2_MASK, 0x00, trap2); //disable cpri sub-module interrupt
        //clear top interrupt status bit
        write_fpga_reg_mask(bp_int_mmap_base, MM_B_IRQ_IFR, (0x1 << irq_no), (0x1 << irq_no));

        status1 =read_fpga_reg(bp_reg_mmap_base, MM_B_CPRI0_STATUS);
        mon1 = read_fpga_reg(bp_reg_mmap_base, MM_B_CPRI0_MON);

        status2 =read_fpga_reg(bp_reg_mmap_base, MM_B_CPRI0_STATUS2);
        mon2 = read_fpga_reg(bp_reg_mmap_base, MM_B_CPRI0_MON2);

        ctrl = read_fpga_reg(bp_reg_mmap_base, MM_B_CPRI0_CTRL);
        debug = read_fpga_reg(bp_reg_mmap_base, MM_B_CPRI0_DEBUG);

   }
    else if (irq_no == B_DU_CPRI_LINK_1_IRQ)
    {
        link_no = 1;
        trap1 = read_fpga_reg(bp_reg_mmap_base, MM_B_CPRI1_STATUS_TRAP);
        mask1 = read_fpga_reg(bp_reg_mmap_base, MM_B_CPRI1_STATUS_MASK);
        trap2 = read_fpga_reg(bp_reg_mmap_base, MM_B_CPRI1_STATUS2_TRAP);
        mask2 = read_fpga_reg(bp_reg_mmap_base, MM_B_CPRI1_STATUS2_MASK);

        write_fpga_reg_mask(bp_reg_mmap_base, MM_B_CPRI1_STATUS_MASK, 0x00, trap1); //disable cpri sub-module interrupt
        write_fpga_reg_mask(bp_reg_mmap_base, MM_B_CPRI1_STATUS2_MASK, 0x00, trap2); //disable cpri sub-module interrupt
        //clear top interrupt status bit
        write_fpga_reg_mask(bp_int_mmap_base, MM_B_IRQ_IFR, (0x1 << irq_no), (0x1 << irq_no));

        status1 =read_fpga_reg(bp_reg_mmap_base, MM_B_CPRI1_STATUS);
        mon1 = read_fpga_reg(bp_reg_mmap_base, MM_B_CPRI1_MON);

        status2 =read_fpga_reg(bp_reg_mmap_base, MM_B_CPRI1_STATUS2);
        mon2 = read_fpga_reg(bp_reg_mmap_base, MM_B_CPRI1_MON2);

        ctrl = read_fpga_reg(bp_reg_mmap_base, MM_B_CPRI1_CTRL);
        debug = read_fpga_reg(bp_reg_mmap_base, MM_B_CPRI1_DEBUG);

    }
    else if (irq_no == B_DU_CPRI_LINK_2_IRQ)
    {
        link_no = 2;
        trap1 = read_fpga_reg(bp_reg_mmap_base, MM_B_CPRI2_STATUS_TRAP);
        mask1 = read_fpga_reg(bp_reg_mmap_base, MM_B_CPRI2_STATUS_MASK);
        trap2 = read_fpga_reg(bp_reg_mmap_base, MM_B_CPRI2_STATUS2_TRAP);
        mask2 = read_fpga_reg(bp_reg_mmap_base, MM_B_CPRI2_STATUS2_MASK);

        write_fpga_reg_mask(bp_reg_mmap_base, MM_B_CPRI2_STATUS_MASK, 0x00, trap1); //disable cpri sub-module interrupt
        write_fpga_reg_mask(bp_reg_mmap_base, MM_B_CPRI2_STATUS2_MASK, 0x00, trap2); //disable cpri sub-module interrupt
        //clear top interrupt status bit
        write_fpga_reg_mask(bp_int_mmap_base, MM_B_IRQ_IFR, (0x1 << irq_no), (0x1 << irq_no));

        status1 =read_fpga_reg(bp_reg_mmap_base, MM_B_CPRI2_STATUS);
        mon1 = read_fpga_reg(bp_reg_mmap_base, MM_B_CPRI2_MON);

        status2 =read_fpga_reg(bp_reg_mmap_base, MM_B_CPRI2_STATUS2);
        mon2 = read_fpga_reg(bp_reg_mmap_base, MM_B_CPRI2_MON2);

        ctrl = read_fpga_reg(bp_reg_mmap_base, MM_B_CPRI2_CTRL);
        debug = read_fpga_reg(bp_reg_mmap_base, MM_B_CPRI2_DEBUG);
    }
    else
    {
        TPT_ERROR(STR("wrong irq_no(%d)", irq_no));
        return;
    }

    TPT_TRACE(1, STR("status1 = 0x%x, trap1 = 0x%x, mask1 = 0x%x, mon1 = 0x%x", status1, trap1, mask1, mon1));
    TPT_TRACE(1, STR("status2 = 0x%x, trap2 = 0x%x, mask2 = 0x%x, mon2 = 0x%x", status2, trap2, mask2, mon2));
    TPT_TRACE(1, STR("ctrl = 0x%x, trap2 = 0x%x, link_no %d", ctrl, debug, link_no));

    if(int_ricr_mbox == ITC_NO_ID)
    {
        if(get_mbox("CPRI_COMMON", &int_ricr_mbox))
        {
            TPT_ERROR(STR("Failed hunt for CPRI_COMMON"));
            return;
        }
    }

    if(int_ricr_mbox != ITC_NO_ID)
    {
        /* report event */
        out = itc_alloc(sizeof(RicrInternalLinkStatusChangeS),
        RICR_INTERRUPT_IND);
        out->ricrInternalLinkStatusChange.link_no         = link_no;
        out->ricrInternalLinkStatusChange.status       = status1;
        out->ricrInternalLinkStatusChange.status_trap  = trap1;
        out->ricrInternalLinkStatusChange.status_mask  = mask1;

        out->ricrInternalLinkStatusChange.status2       = status2;
        out->ricrInternalLinkStatusChange.status2_trap  = trap2;
        out->ricrInternalLinkStatusChange.status2_mask  = mask2;

        out->ricrInternalLinkStatusChange.mon          = mon1;
        out->ricrInternalLinkStatusChange.mon2         = mon2;

        out->ricrInternalLinkStatusChange.ctrl         = ctrl;
        out->ricrInternalLinkStatusChange.debug        = debug;

        TPT_SEND_SIG(out->sigNo, int_ricr_mbox, STR("Send out XDOS_RICR_INTERRUPT_IND, irq_no = %d", irq_no));
        itc_send(&out, int_ricr_mbox, ITC_MY_MBOX);
    }
    else
    {
        TPT_ERROR(STR("Receive irq_no = %d, but RICR is not ready.", irq_no));
    }

}

void bp_int_m_cpri(uint32_t irq_no)
{
    union itc_msg *out;
    uint32_t link_no;
    uint32_t status1, status2;
    uint32_t trap1, trap2;
    uint32_t mask1, mask2;
    uint32_t mon1, mon2;
    uint32_t ctrl, debug;

    TPT_TRACE(1, STR("irq_no(%d)", irq_no));

//    printf("irq_no(%d) in %s\n", irq_no, __func__);

    if (irq_no == B_M_CPRI_LINK_0_IRQ)
    {
        link_no = 0;
        trap1 = read_fpga_reg(bp_reg_mmap_base, MM_B_INT_CPRI0_STATUS_TRAP);
        mask1 = read_fpga_reg(bp_reg_mmap_base, MM_B_INT_CPRI0_STATUS_MASK);
        trap2 = read_fpga_reg(bp_reg_mmap_base, MM_B_INT_CPRI0_STATUS2_TRAP);
        mask2 = read_fpga_reg(bp_reg_mmap_base, MM_B_INT_CPRI0_STATUS2_MASK);

        write_fpga_reg_mask(bp_reg_mmap_base, MM_B_INT_CPRI0_STATUS_MASK, 0x00, trap1); //disable cpri sub-module interrupt
        write_fpga_reg_mask(bp_reg_mmap_base, MM_B_INT_CPRI0_STATUS2_MASK, 0x00, trap2); //disable cpri sub-module interrupt
        //clear top interrupt status bit
        write_fpga_reg_mask(bp_int_mmap_base, MM_B_IRQ_IFR, (0x1 << irq_no), (0x1 << irq_no));

        status1 =read_fpga_reg(bp_reg_mmap_base, MM_B_INT_CPRI0_STATUS);
        mon1 = read_fpga_reg(bp_reg_mmap_base, MM_B_INT_CPRI0_MON);

        status2 =read_fpga_reg(bp_reg_mmap_base, MM_B_INT_CPRI0_STATUS2);
        mon2 = read_fpga_reg(bp_reg_mmap_base, MM_B_INT_CPRI0_MON2);

        ctrl = read_fpga_reg(bp_reg_mmap_base, MM_B_INT_CPRI0_CTRL);
        debug = read_fpga_reg(bp_reg_mmap_base, MM_B_INT_CPRI0_DEBUG);

    }
    else if (irq_no == B_M_CPRI_LINK_1_IRQ)
    {
        link_no = 1;
        trap1 = read_fpga_reg(bp_reg_mmap_base, MM_B_INT_CPRI1_STATUS_TRAP);
        mask1 = read_fpga_reg(bp_reg_mmap_base, MM_B_INT_CPRI1_STATUS_MASK);
        trap2 = read_fpga_reg(bp_reg_mmap_base, MM_B_INT_CPRI1_STATUS2_TRAP);
        mask2 = read_fpga_reg(bp_reg_mmap_base, MM_B_INT_CPRI1_STATUS2_MASK);

        write_fpga_reg_mask(bp_reg_mmap_base, MM_B_INT_CPRI1_STATUS_MASK, 0x00, trap1); //disable cpri sub-module interrupt
        write_fpga_reg_mask(bp_reg_mmap_base, MM_B_INT_CPRI1_STATUS2_MASK, 0x00, trap2); //disable cpri sub-module interrupt
        //clear top interrupt status bit
        write_fpga_reg_mask(bp_int_mmap_base, MM_B_IRQ_IFR, (0x1 << irq_no), (0x1 << irq_no));

        status1 =read_fpga_reg(bp_reg_mmap_base, MM_B_INT_CPRI1_STATUS);
        mon1 = read_fpga_reg(bp_reg_mmap_base, MM_B_INT_CPRI1_MON);

        status2 =read_fpga_reg(bp_reg_mmap_base, MM_B_INT_CPRI1_STATUS2);
        mon2 = read_fpga_reg(bp_reg_mmap_base, MM_B_INT_CPRI1_MON2);

        ctrl = read_fpga_reg(bp_reg_mmap_base, MM_B_INT_CPRI1_CTRL);
        debug = read_fpga_reg(bp_reg_mmap_base, MM_B_INT_CPRI1_DEBUG);
    }
    else if (irq_no == B_M_CPRI_LINK_2_IRQ)
    {
        link_no = 2;
        trap1 = read_fpga_reg(bp_reg_mmap_base, MM_B_INT_CPRI2_STATUS_TRAP);
        mask1 = read_fpga_reg(bp_reg_mmap_base, MM_B_INT_CPRI2_STATUS_MASK);
        trap2 = read_fpga_reg(bp_reg_mmap_base, MM_B_INT_CPRI2_STATUS2_TRAP);
        mask2 = read_fpga_reg(bp_reg_mmap_base, MM_B_INT_CPRI2_STATUS2_MASK);

        write_fpga_reg_mask(bp_reg_mmap_base, MM_B_INT_CPRI2_STATUS_MASK, 0x00, trap1); //disable cpri sub-module interrupt
        write_fpga_reg_mask(bp_reg_mmap_base, MM_B_INT_CPRI2_STATUS2_MASK, 0x00, trap2); //disable cpri sub-module interrupt
        //clear top interrupt status bit
        write_fpga_reg_mask(bp_int_mmap_base, MM_B_IRQ_IFR, (0x1 << irq_no), (0x1 << irq_no));

        status1 =read_fpga_reg(bp_reg_mmap_base, MM_B_INT_CPRI2_STATUS);
        mon1 = read_fpga_reg(bp_reg_mmap_base, MM_B_INT_CPRI2_MON);

        status2 =read_fpga_reg(bp_reg_mmap_base, MM_B_INT_CPRI2_STATUS2);
        mon2 = read_fpga_reg(bp_reg_mmap_base, MM_B_INT_CPRI2_MON2);

        ctrl = read_fpga_reg(bp_reg_mmap_base, MM_B_INT_CPRI2_CTRL);
        debug = read_fpga_reg(bp_reg_mmap_base, MM_B_INT_CPRI2_DEBUG);
    }
    else if (irq_no == B_M_CPRI_LINK_3_IRQ)
    {
        link_no = 3;
        trap1 = read_fpga_reg(bp_reg_mmap_base, MM_B_INT_CPRI3_STATUS_TRAP);
        mask1 = read_fpga_reg(bp_reg_mmap_base, MM_B_INT_CPRI3_STATUS_MASK);
        trap2 = read_fpga_reg(bp_reg_mmap_base, MM_B_INT_CPRI3_STATUS2_TRAP);
        mask2 = read_fpga_reg(bp_reg_mmap_base, MM_B_INT_CPRI3_STATUS2_MASK);

        write_fpga_reg_mask(bp_reg_mmap_base, MM_B_INT_CPRI3_STATUS_MASK, 0x00, trap1); //disable cpri sub-module interrupt
        write_fpga_reg_mask(bp_reg_mmap_base, MM_B_INT_CPRI3_STATUS2_MASK, 0x00, trap2); //disable cpri sub-module interrupt
        //clear top interrupt status bit
        write_fpga_reg_mask(bp_int_mmap_base, MM_B_IRQ_IFR, (0x1 << irq_no), (0x1 << irq_no));

        status1 =read_fpga_reg(bp_reg_mmap_base, MM_B_INT_CPRI3_STATUS);
        mon1 = read_fpga_reg(bp_reg_mmap_base, MM_B_INT_CPRI3_MON);

        status2 =read_fpga_reg(bp_reg_mmap_base, MM_B_INT_CPRI3_STATUS2);
        mon2 = read_fpga_reg(bp_reg_mmap_base, MM_B_INT_CPRI3_MON2);

        ctrl = read_fpga_reg(bp_reg_mmap_base, MM_B_INT_CPRI3_CTRL);
        debug = read_fpga_reg(bp_reg_mmap_base, MM_B_INT_CPRI3_DEBUG);
    }
    else
    {
        TPT_ERROR(STR("wrong irq_no(%d)", irq_no));
        return;
    }

    TPT_TRACE(1, STR("status1 = 0x%x, trap1 = 0x%x, mask1 = 0x%x, mon1 = 0x%x", status1, trap1, mask1, mon1));
    TPT_TRACE(1, STR("status2 = 0x%x, trap2 = 0x%x, mask2 = 0x%x, mon2 = 0x%x", status2, trap2, mask2, mon2));
    TPT_TRACE(1, STR("ctrl = 0x%x, trap2 = 0x%x, link_no %d", ctrl, debug, link_no));

    if(int_mricr_mbox == ITC_NO_ID)
    {
        if(get_mbox("mRICR", &int_mricr_mbox))
        {
            TPT_ERROR(STR("Failed hunt for mRICR"));
            return;
        }
    }

    if(int_mricr_mbox != ITC_NO_ID)
    {
        /* report event */
        out = itc_alloc(sizeof(MricrInternalLinkStatusChangeS),
        INTERRUPT_MRICR_IND);
        out->mricrInternalLinkStatusChange.link_no      = link_no;
        out->mricrInternalLinkStatusChange.status       = status1;
        out->mricrInternalLinkStatusChange.status_trap  = trap1;
        out->mricrInternalLinkStatusChange.status_mask  = mask1;

        out->mricrInternalLinkStatusChange.status2       = status2;
        out->mricrInternalLinkStatusChange.status2_trap  = trap2;
        out->mricrInternalLinkStatusChange.status2_mask  = mask2;

        out->mricrInternalLinkStatusChange.mon          = mon1;
        out->mricrInternalLinkStatusChange.mon2         = mon2;

        out->mricrInternalLinkStatusChange.ctrl         = ctrl;
        out->mricrInternalLinkStatusChange.debug        = debug;

        TPT_SEND_SIG(out->sigNo, int_mricr_mbox, STR("Send out INTERRUPT_MRICR_IND, irq_no = %d", irq_no));
        itc_send(&out, int_mricr_mbox, ITC_MY_MBOX);
    }
    else
    {
        TPT_ERROR(STR("Receive irq_no = %d, but M-RICR is not ready.", irq_no));
    }

}

// a4ci dbuart int process
void bp_int_dbuart(uint32_t irq_no)
{
    TPT_INFO(STR("dbuart interrupt happens, irq_no = %d", irq_no));
//reserved
}


void bp_int_ewtimer(uint32_t irq_no)
{
    union itc_msg *out;

    write_fpga_reg_mask(bp_int_mmap_base, MM_B_IRQ_IFR, (0x1 << irq_no), (0x1 << irq_no));

    TPT_TRACE(1, STR("irq_no(%d)", irq_no));

    if(int_ewtimer_mbox == ITC_NO_ID)
    {
        if(get_mbox("ewTimerISR", &int_ewtimer_mbox))
        {
            TPT_ERROR(STR("Receive irq_no = %d, but ewTimerISR is not ready.", irq_no));
            return;
        }
    }

    out = itc_alloc(sizeof(interruptChangeIndS_t), INTERRUPT_CHANGE_IND);
    out->interruptChangeInd.intProcId = irq_no + FPGA_INT_OFFSET;

    TPT_SEND_SIG(out->sigNo, int_ewtimer_mbox, STR("Send out INTERRUPT_CHANGE_IND to ewTimerISR, irq_no = %d", irq_no));
    itc_send(&out, int_ewtimer_mbox, ITC_MY_MBOX);
}


// mmi int process
void bp_int_mmi(uint32_t irq_no)
{
    TPT_INFO(STR("mmi interrupt happens, irq_no = %d", irq_no));
}

static void bp_int_process()
{
    uint32_t isr_value = 0;
    uint32_t ier_value = 0;
    int i;

    if (bp_isr_mbox == ITC_NO_ID)
    {
        bp_isr_mbox = itc_create_mailbox("bp_isr_mbox", 0);
    }

    // get isr value
    isr_value = read_fpga_reg(bp_int_mmap_base, MM_B_IRQ_IFR);
    ier_value = read_fpga_reg(bp_int_mmap_base, MM_B_IRQ_IMR);

    //printf("isr_value = 0x%x, ier_value = 0x%x \n", isr_value, ier_value);

    TPT_TRACE(1, STR("%s:trap=0x%x, mask=0x%x", __func__,isr_value, ier_value));

    // check which int is triggered
    for (i=0; i<=31; i++)
    {
        if (isr_value & ier_value & (0x01 << i))
        {
            (*bp_int_handler[i])(i);
        }
    }

    uio_enable_irq(bp_int_uio_handle);

    TPT_TRACE(1, STR("%s done", __func__));
}

void trxm_int_m_cpri(uint32_t irq_no)
{
    union itc_msg *out;
    uint32_t link_no;
    uint32_t status1, status2;
    uint32_t trap1, trap2;
    uint32_t mask1, mask2;
    uint32_t mon1, mon2;
    uint32_t ctrl, debug;

    TPT_TRACE(1, STR("irq_no(%d)", irq_no));

//    printf("irq_no(%d) in %s\n", irq_no, __func__);

    if (irq_no == T_M_CPRI_LINK_0_IRQ)
    {
        link_no = 0;
        trap1 = read_fpga_reg(trxm_reg_mmap_base, MM_T_CPRI0_STATUS_TRAP);
        mask1 = read_fpga_reg(trxm_reg_mmap_base, MM_T_CPRI0_STATUS_MASK);
        trap2 = read_fpga_reg(trxm_reg_mmap_base, MM_T_CPRI0_STATUS2_TRAP);
        mask2 = read_fpga_reg(trxm_reg_mmap_base, MM_T_CPRI0_STATUS2_MASK);

        write_fpga_reg_mask(trxm_reg_mmap_base, MM_T_CPRI0_STATUS_MASK, 0x00, trap1); //disable cpri sub-module interrupt
        write_fpga_reg_mask(trxm_reg_mmap_base, MM_T_CPRI0_STATUS2_MASK, 0x00, trap2); //disable cpri sub-module interrupt
        //clear top interrupt status bit
        write_fpga_reg_mask(trxm_int_mmap_base, MM_T_IRQ_IFR, (0x1 << irq_no), (0x1 << irq_no));

        status1 =read_fpga_reg(trxm_reg_mmap_base, MM_T_CPRI0_STATUS);
        mon1 = read_fpga_reg(trxm_reg_mmap_base, MM_T_CPRI0_MON);

        status2 =read_fpga_reg(trxm_reg_mmap_base, MM_T_CPRI0_STATUS2);
        mon2 = read_fpga_reg(trxm_reg_mmap_base, MM_T_CPRI0_MON2);

        ctrl = read_fpga_reg(trxm_reg_mmap_base, MM_T_CPRI0_CTRL);
        debug = read_fpga_reg(trxm_reg_mmap_base, MM_T_CPRI0_DEBUG);
    }
    else
    {
        TPT_ERROR(STR("wrong irq_no(%d)", irq_no));
        return;
    }

    TPT_TRACE(1, STR("status1 = 0x%x, trap1 = 0x%x, mask1 = 0x%x, mon1 = 0x%x", status1, trap1, mask1, mon1));
    TPT_TRACE(1, STR("status2 = 0x%x, trap2 = 0x%x, mask2 = 0x%x, mon2 = 0x%x", status2, trap2, mask2, mon2));
    TPT_TRACE(1, STR("ctrl = 0x%x, trap2 = 0x%x, link_no %d", ctrl, debug, link_no));

    if(int_mricr_mbox == ITC_NO_ID)
    {
        if(get_mbox("mRICR", &int_mricr_mbox))
        {
            TPT_ERROR(STR("Failed hunt for mRICR"));
            return;
        }
    }

    if(int_mricr_mbox != ITC_NO_ID)
    {
        /* report event */
        out = itc_alloc(sizeof(MricrInternalLinkStatusChangeS),
        INTERRUPT_MRICR_IND);
        out->mricrInternalLinkStatusChange.link_no         = link_no;
        out->mricrInternalLinkStatusChange.status       = status1;
        out->mricrInternalLinkStatusChange.status_trap  = trap1;
        out->mricrInternalLinkStatusChange.status_mask  = mask1;

        out->mricrInternalLinkStatusChange.status2       = status2;
        out->mricrInternalLinkStatusChange.status2_trap  = trap2;
        out->mricrInternalLinkStatusChange.status2_mask  = mask2;

        out->mricrInternalLinkStatusChange.mon          = mon1;
        out->mricrInternalLinkStatusChange.mon2         = mon2;

        out->mricrInternalLinkStatusChange.ctrl         = ctrl;
        out->mricrInternalLinkStatusChange.debug        = debug;

        TPT_SEND_SIG(out->sigNo, int_mricr_mbox, STR("Send out INTERRUPT_MRICR_IND, irq_no = %d", irq_no));
        itc_send(&out, int_mricr_mbox, ITC_MY_MBOX);
    }
    else
    {
        TPT_ERROR(STR("Receive irq_no = %d, but M-RICR is not ready.", irq_no));
    }
}


// send message to APP intHandler process
void trxm_int_notify(uint32_t irq_no)
{
    union itc_msg *out;

    disableTrxmSubInt(irq_no);

    write_fpga_reg_mask(trxm_int_mmap_base, MM_T_IRQ_IFR, (0x1 << irq_no), (0x1 << irq_no));

    TPT_TRACE(1, STR("irq_no(%d)", irq_no));

    if(int_app_mbox == ITC_NO_ID)
    {
        if(get_mbox("intHandler", &int_app_mbox))
        {
            TPT_ERROR(STR("Failed hunt for intHandler"));
            return;
        }
    }

    out = itc_alloc(sizeof(interruptChangeIndS_t), INTERRUPT_CHANGE_IND);
    out->interruptChangeInd.intProcId = irq_no + FPGA_INT_OFFSET;

    TPT_SEND_SIG(out->sigNo, int_app_mbox, STR("Send out INTERRUPT_CHANGE_IND, irq_no = %d", irq_no));
    itc_send(&out, int_app_mbox, ITC_MY_MBOX);
}


static void trxm_int_process()
{
    uint32_t isr_value = 0;
    uint32_t ier_value = 0;
    int i;

    if (trxm_isr_mbox == ITC_NO_ID)
    {
        trxm_isr_mbox = itc_create_mailbox("trxm_isr_mbox", 0);
    }

    // get isr value
    isr_value = read_fpga_reg(trxm_int_mmap_base, MM_T_IRQ_IFR);
    ier_value = read_fpga_reg(trxm_int_mmap_base, MM_T_IRQ_IMR);

    TPT_TRACE(1, STR("%s:trap=0x%x, mask=0x%x", __func__,isr_value, ier_value));

    // check which int is triggered
    for (i=0; i<=31; i++)
    {
        if (isr_value & ier_value & (0x01 << i))
        {
            (*trxm_int_handler[i])(i);
        }
    }
    uio_enable_irq(trxm_int_uio_handle); //enable uio interrupt

    TPT_TRACE(1, STR("%s done", __func__));
}

bool hunt_xcs_process()
{
    return true;
}


bool hunt_ricr_process()
{
    bool ret = true;

    TPT_TRACE(1, STR("In %s", __func__));

    if(get_mbox("CPRI_COMMON", &int_ricr_mbox))
    {
        TPT_ERROR(STR("Failed hunt for CPRI_COMMON"));
        ret = false;
    }

    return ret;
}

bool hunt_mricr_process()
{
    TPT_TRACE(1, STR("In %s", __func__));

    if(get_mbox("mRICR", &int_mricr_mbox))
    {
        TPT_ERROR(STR("Failed hunt for mRICR"));
        return false;
    }

    return true;
}

bool hunt_ac_process()
{
    TPT_TRACE(1, STR("In %s", __func__));

    if(get_mbox(AC_DISPATCH_MAILBOX, &int_ac_mbox))
    {
        TPT_ERROR(STR("Failed hunt for ac control mbox"));
        return false;
    }

    return true;
}

bool hunt_app_process()
{
    TPT_TRACE(1, STR("In %s", __func__));

    if(get_mbox("ewTimerISR", &int_ewtimer_mbox))
    {
        TPT_ERROR(STR("Failed hunt for ewTimerISR"));
    }

    if(get_mbox("intHandler", &int_app_mbox))
    {
        TPT_ERROR(STR("Failed hunt for intHandler"));
        return false;
    }

    return true;
}

void installBpInt()
{
//map backplane register to user space
//Part 1.
//Mapping interrupt register and configure interrupt
//Top level interrupt register base physical address is 0xa800 0000
//UIO device for interrupt is "bp_int"
    bp_int_uio_handle = (void *) uio_open(BP_FPGA_INT_DEV);
    if (bp_int_uio_handle == (UIO_HANDLE_) - 1)
    {
        TPT_TRACE(1, STR("BP fpga uio_open funciton return error"));
        return;
    }
    TPT_TRACE(1, STR("BP fpga uio_open success"));

    bp_int_mmap_base = uio_mmap(bp_int_uio_handle);
    if (bp_int_mmap_base == MAP_FAILED)
    {
        bp_int_mmap_base = NULL;
        TPT_ERROR(STR("BP fpga uio_mmap funciton return error"));
        uio_close(bp_int_uio_handle);
        return;
    }
    TPT_TRACE(1, STR("BP fpga uio_mmap success! base address is %p", bp_int_mmap_base));

    uio_disable_irq(bp_int_uio_handle);

    //configure interrupt
    write_fpga_reg(bp_int_mmap_base, MM_B_IRQ_IMR, 0x00);
    write_fpga_reg(bp_int_mmap_base, MM_B_IRQ_USE_LEVEL, 0xFFFFFFFF);
    write_fpga_reg(bp_int_mmap_base, MM_B_IRQ_LEVEL, 0xFFFFFFFF);
    write_fpga_reg(bp_int_mmap_base, MM_B_IRQ_USE_POS_EDGE, 0x00);
    write_fpga_reg(bp_int_mmap_base, MM_B_IRQ_USE_NEG_EDGE, 0x00);
    write_fpga_reg(bp_int_mmap_base, MM_B_IRQ_IFR, 0xFFFFFFFF);

//enable uio interrupt
    if (uio_irq_set_notifier(bp_int_uio_handle, bp_int_process, NULL))
    {
        TPT_ERROR(STR("%s: unable to set UIO interrupt notifier", __func__));
        uio_close(bp_int_uio_handle);
        return;
    }

    if (uio_bind_irq_rt(bp_int_uio_handle, 60))
    {
        TPT_ERROR(STR("%s: unable to start UIO interrupt handler", __func__));
        uio_close(bp_int_uio_handle);
        return;
    }

//Part 2
//Map FPGA register, for normal register (NOT Top level interrupt)
//Physical base address is 0xa000 0000
//This base address for sub-module interrupt handling
//UIO device for normal register is "bp_reg"
    bp_reg_uio_handle = (void *) uio_open(BP_FPGA_REG_DEV);
    if (bp_reg_uio_handle == (UIO_HANDLE_) - 1)
    {
        TPT_TRACE(1, STR("BP fpga uio_open funciton return error"));
        return;
    }
    TPT_TRACE(1, STR("BP fpga reg uio_open success"));

    bp_reg_mmap_base = uio_mmap(bp_reg_uio_handle);
    if (bp_reg_mmap_base == MAP_FAILED)
    {
        bp_reg_mmap_base = NULL;
        TPT_ERROR(STR("BP fpga uio_mmap funciton return error"));
        uio_close(bp_reg_uio_handle);
        return;
    }
    TPT_TRACE(1, STR("BP fpga reg uio_mmap success! base address is %p", bp_reg_mmap_base));

//Part 3
//Enable interrupt

    uio_enable_irq(bp_int_uio_handle); //enable uio interrupt

    write_fpga_reg(bp_int_mmap_base, MM_B_IRQ_IMR, 0xFFFFFFFF); //enable all interrupt

    TPT_TRACE(1, STR("Enable BP fpga interrupt done"));
}

void installTrxmInt()
{
//map trxm register to user space
//Part 1.
//Mapping interrupt register and configure interrupt
//Top level interrupt register base physical address is 0xa800 0000
//UIO device for interrupt is "trxm_int"
    trxm_int_uio_handle = (void *) uio_open(TRXM_FPGA_INT_DEV);
    if (trxm_int_uio_handle == (UIO_HANDLE_) - 1)
    {
        TPT_TRACE(1, STR("TRXM fpga uio_open funciton return error"));
        return;
    }
    TPT_TRACE(1, STR("TRXM fpga uio_open success"));

    trxm_int_mmap_base = uio_mmap(trxm_int_uio_handle);
    if (trxm_int_mmap_base == MAP_FAILED)
    {
        trxm_int_mmap_base = NULL;
        TPT_ERROR(STR("TRXM fpga uio_mmap funciton return error"));
        uio_close(trxm_int_uio_handle);
        return;
    }
    TPT_TRACE(1, STR("TRXM fpga uio_mmap success! base address is %p", trxm_int_mmap_base));

    uio_disable_irq(trxm_int_uio_handle);

    //configure interrupt
    write_fpga_reg(trxm_int_mmap_base, MM_T_IRQ_IMR, 0x00);
    write_fpga_reg(trxm_int_mmap_base, MM_T_IRQ_USE_LEVEL, 0xFFFFFFFF);
    write_fpga_reg(trxm_int_mmap_base, MM_T_IRQ_LEVEL, 0xFFFFFFFF);
    write_fpga_reg(trxm_int_mmap_base, MM_T_IRQ_USE_POS_EDGE, 0x00);
    write_fpga_reg(trxm_int_mmap_base, MM_T_IRQ_USE_NEG_EDGE, 0x00);
    write_fpga_reg(trxm_int_mmap_base, MM_T_IRQ_IFR, 0xFFFFFFFF);

//configure uio interrupt
    if (uio_irq_set_notifier(trxm_int_uio_handle, trxm_int_process, NULL))
    {
        TPT_ERROR(STR("%s: unable to set UIO interrupt notifier", __func__));
        uio_close(trxm_int_uio_handle);
        return;
    }

    if (uio_bind_irq_rt(trxm_int_uio_handle, 60))
    {
        TPT_ERROR(STR("%s: unable to start UIO interrupt handler", __func__));
        uio_close(trxm_int_uio_handle);
        return;
    }

//Part 2
//Map FPGA register, for normal register (NOT Top level interrupt)
//Physical base address is 0xa000 0000
//This base address for sub-module interrupt handling
//UIO device for normal register is "trxm_reg"
    trxm_reg_uio_handle = (void *) uio_open(TRXM_FPGA_REG_DEV);
    if (trxm_reg_uio_handle == (UIO_HANDLE_) - 1)
    {
        TPT_TRACE(1, STR("TRXM fpga uio_open funciton return error"));
        return;
    }
    TPT_TRACE(1, STR("TRXM fpga reg uio_open success"));

    trxm_reg_mmap_base = uio_mmap(trxm_reg_uio_handle);
    if (trxm_reg_mmap_base == MAP_FAILED)
    {
        trxm_reg_mmap_base = NULL;
        TPT_ERROR(STR("TRXM fpga uio_mmap funciton return error"));
        uio_close(trxm_reg_uio_handle);
        return;
    }
    TPT_TRACE(1, STR("TRXM fpga reg uio_mmap success! base address is %p", trxm_reg_mmap_base));

//Part 3
//Enable interrupt

    uio_enable_irq(trxm_int_uio_handle); //enable uio interrupt

    write_fpga_reg(trxm_int_mmap_base, MM_T_IRQ_IMR, 0x3FFFFF); //enable all interrupt

    TPT_TRACE(1, STR("Enable TRXM fpga interrupt done"));
}

/**
 * Function exit_handler
 */
static void exit_handler(int sig)
{
    union itc_msg *msg;

    TPT_INFO(STR("Receive signal %d, terminating", sig));
    msg = itc_alloc(sizeof(uint32_t), EXIT_SIGNAL);
    itc_send(&msg, int_mbox, ITC_MY_MBOX);
}

/*
 * Function print_usage
 */
static void print_usage(void)
{
    printf("Usage: rhd-int [options]\n\n"
           "Options:\n"
           "-h  Display usage information (this message).\n"
           "-d  Daemonize the program.\n\n");
}

/**
 * Main function
 * start the rhd_int daemon
 */
int main(int argc, char **argv)
{
    static char short_options[] = "hd";
    int32_t ret = 0;
    int daemonize = 0;
    union itc_msg * inSig;
    uint32_t all[] = { 0 };
    int c;

    while ((c = getopt(argc, argv, short_options)) != -1)
    {
        switch (c)
        {
            case 'h':
                print_usage();
                goto main_end;

            case 'd':
                daemonize = 1;
                break;

            default:
                print_usage();
                ret = -EINVAL;
                goto main_end;
        }
    }

    if (rhd_try_lock(daemon_name))
    {
        printf("failed to obtain lock: %s\n", daemon_name);
        ret = -EFAULT;
        goto main_end;
    }

    if (!daemonize || !daemon(0, 0))
    {
        TPT_INFO(STR("Starting %s %s",
        daemonize ? "daemon" : "foreground process",
        daemon_name));

        if (signal(SIGTERM, exit_handler) == SIG_ERR)
        {
            TPT_ERROR(STR("Failed to install signal"
            " exit handler"));
            exit(1);
        }

        /* Initialize ITC */
        itc_init(MAX_MAILBOX_NUM, ITC_MALLOC, NULL, ITC_NO_NAMESPACE, 0);

        /* Create our mailbox. */
        int_mbox = itc_create_mailbox(RHD_INT_MAILBOX, 0);

        if (int_mbox == ITC_NO_ID)
        {
            ret = -EFAULT;
            goto main_end;
        }

        if(!strcmp(getenv("SYS_BOARD_TYPE"),"BP")) /* if board type is BP */
        {
            boardType = BOARD_BP;
            installBpInt();
        }

        if(!strcmp(getenv("SYS_BOARD_TYPE"),"TRXM"))
        {
            boardType = BOARD_TRXM;
            installTrxmInt();
        }

        if((boardType != BOARD_BP) && (boardType != BOARD_TRXM))
        {
            TPT_ERROR(STR("Unsupport board type."));
            ret = -EFAULT;
            goto main_end;
        }

        hunt_xcs_process();

        printf("RHD-INT is ready. \n");

        while (1)
        {
            inSig = itc_receive(all, ITC_NO_TMO, ITC_FROM_ALL);

            switch(inSig->sigNo)
            {
                case RICR_INT_HANDLER_READY_IND:
                    TPT_INFO("Receive signal RICR_INT_HANDLER_READY_IND");
                    hunt_ricr_process();
                    break;

                case INT_HANDLER_PROCESS_READY_IND:
                    TPT_INFO("Receive signal INT_HANDLER_PROCESS_READY_IND");
                    hunt_app_process();
                    break;

                case INT_MRICR_READY_IND:
                    TPT_INFO("Receive signal INT_MRICR_READY_IND");
                    hunt_mricr_process();
                    break;

                case AC_INT_REGISTER_REQ:
                    TPT_INFO("Receive signal AC_INT_REGISTER_REQ");
                    hunt_ac_process();
                    break;

                case EXIT_SIGNAL:
                    TPT_INFO(STR("%s exiting as ordered", daemon_name));
                    goto main_end;
                    break;

                default:
                    TPT_ERROR(STR("Unexpected signal. SigNo=%X",inSig->sigNo));
                    break;
            }

            itc_free(&inSig);
        }
    }
    else
    {
        TPT_ERROR(STR("Failed to start daemon %s", daemon_name));
        ret = -EFAULT;
    }

    main_end:
    return ret;

}

