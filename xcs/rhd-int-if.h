/******************************************************************************
 *
 *    COPYRIGHT (C)                 Ericsson Radio Systems AB, Sweden
 *
 *        The copyright to the computer program(s) herein is the property
 *        of Ericsson Radio Systems AB.
 *
 *        The program(s) may be used and/or copied only with the written
 *        permission from Ericsson Radio Systems AB or in accordance with
 *        the terms and conditions stipulated in the agreement/contract
 *        under which the program(s) have been supplied.
 *
 *****************************************************************************/

/******************************************************************************
 *
 * Product name:
 *
 * File:
 *    xdos_loc.h
 *
 * Author:
 *
 * Description:
 *
 * Reviewed:
 *
 * Revision history:
 *****************************************************************************/

#ifndef RHD_INT_H
#define RHD_INT_H

/*----------------------------  Include files  ------------------------------*/

#include "ma_mimo_trxmRegisters.h"
#include "ma_mimo_backplaneRegisters.h"

/*----------------------------  Declaration of Global Variables  ------------*/
//Signal number definition
#define INT_HANDLER_PROCESS_READY_IND      (uint32_t)0x88888882   //send from app, means app ready to receive interrupt
#define INTERRUPT_CHANGE_IND               (uint32_t)(0x80000000 + 0x00E00 + 0x014) //send to app, when there is interrupt happen
#define INTERRUPT_SPI_IND                  (uint32_t)(0x80000000 + 0x00E00 + 0x042)

#define RICR_SIGBASE                       (uint32_t)(0x88888000)
#define RICR_INT_HANDLER_READY_IND         (uint32_t)(0x88888000 + 0x01)
#define RICR_INTERRUPT_IND                 (uint32_t)(RICR_SIGBASE + 0x02)

#define INT_MRICR_SIGBASE                  (uint32_t)(0x90000000)
#define INT_MRICR_READY_IND                (uint32_t)( INT_MRICR_SIGBASE+ 0x01)   //send from app, means mRICR ready to receive interrupt
#define INTERRUPT_MRICR_IND                (uint32_t)( INT_MRICR_SIGBASE+ 0x02)   //send to app, when there is mRICR interrupt happen

#define AC_INT_REGISTER_REQ                (uint32_t)(0x7000)
#define AC_INT_CHANGE_IND                  (uint32_t)(0x7001)

#define AC_DISPATCH_MAILBOX                "AC_DISPATCH"

#define EXIT_SIGNAL                        0xdeadbeef

#define FPGA_INT_OFFSET                    32

//others
#define MAX_MAILBOX_NUM                    5
#define RHD_INT_MAILBOX                    "RHD_INT"

#define FPGA_INT_OFFSET                    32

#define BP_FPGA_INT_DEV                    "bp_int"
#define BP_FPGA_REG_DEV                    "bp_reg"

#define TRXM_FPGA_INT_DEV                  "trxm_int"
#define TRXM_FPGA_REG_DEV                  "trxm_reg"

#define BOARD_BP                           0
#define BOARD_TRXM                         1

//CPRI Port definition
#define BP_INT_CPRI0_IRQ_NO                3
#define BP_INT_CPRI1_IRQ_NO                2
#define BP_INT_CPRI2_IRQ_NO                1

static uint32_t boardType = 255;
static itc_mbox_id_t int_ricr_mbox = ITC_NO_ID;
static itc_mbox_id_t int_mricr_mbox = ITC_NO_ID;
static itc_mbox_id_t int_app_mbox = ITC_NO_ID;
static itc_mbox_id_t int_ewtimer_mbox = ITC_NO_ID;
static itc_mbox_id_t int_ac_mbox = ITC_NO_ID;

void (*fpga_int_handler[32])(uint32_t irq_no);

static void *bp_int_uio_handle = NULL;
static void *bp_int_mmap_base = NULL;

static void *bp_reg_uio_handle = NULL;
static void *bp_reg_mmap_base = NULL;

static void *trxm_int_uio_handle = NULL;
static void *trxm_int_mmap_base = NULL;

static void *trxm_reg_uio_handle = NULL;
static void *trxm_reg_mmap_base = NULL;

static volatile itc_mbox_id_t bp_isr_mbox = ITC_NO_ID;
static volatile itc_mbox_id_t trxm_isr_mbox = ITC_NO_ID;
static volatile itc_mbox_id_t int_mbox = ITC_NO_ID;

typedef enum {
    TRAP,
    MASK,
    MAX_REG_TYPE
} reg_type;


enum bpSubIntBit
{
    B_MISC_IRQ = 0,
    B_DU_CPRI_LINK_0_IRQ,  //from cpri0_status
    B_DU_CPRI_LINK_1_IRQ,  //from cpri1_status
    B_DU_CPRI_LINK_2_IRQ,  //from cpri2_status
    B_M_CPRI_LINK_0_IRQ, //Interconnection CPRI Link 0 irq. from int_cpri0_status
    B_M_CPRI_LINK_1_IRQ, //Interconnection CPRI Link 1 irq. from int_cpri1_status
    B_M_CPRI_LINK_2_IRQ, //Interconnection CPRI Link 2 irq. from int_cpri2_status
    B_M_CPRI_LINK_3_IRQ, //Interconnection CPRI Link 3 irq. from int_cpri3_status
    B_DU_CPRI_LINK_0_IQC_IRQ,
    B_DU_CPRI_LINK_1_IQC_IRQ,
    B_DU_CPRI_LINK_2_IQC_IRQ,
    B_M_CPRI_LINK_0_IQC_IRQ, //Interconnection CPRI Link 0 IQC irq.  --reserved
    B_M_CPRI_LINK_1_IQC_IRQ, //Interconnection CPRI Link 1 IQC irq.  --reserved
    B_M_CPRI_LINK_2_IQC_IRQ, //Interconnection CPRI Link 2 IQC irq.  --reserved
    B_M_CPRI_LINK_3_IQC_IRQ, //Interconnection CPRI Link 3 IQC irq.  --reserved
    B_204B_TX_IRQ,
    B_204B_RX_IRQ,
    B_TDD_SW_IRQ,
    B_GAMMA_0_IRQ,
    B_GAMMA_1_IRQ,
    B_GAMMA_2_IRQ,
    B_GAMMA_3_IRQ,
    B_MMI_PRESS_IRQ,
    B_MMI_RELEASE_IRQ,
    B_NMI_6P7S_IRQ,
    B_DBUART_IRQ,
    B_SPI_IRQ,
    B_AC_AFE7689_STATUS,
    B_WIRETAP,
    B_AC_TRANS_DL_IRQ,
    B_AC_TRANS_UL_IRQ,
    B_MAX_SUB_IRQ
};

uint32_t bpSubIntMask[B_MAX_SUB_IRQ][MAX_REG_TYPE] =
{
    {MM_B_MISC_IRQ_TRAP, MM_B_MISC_IRQ_MASK},
    {0, 0}, //cpri0 interrupt, special handling
    {0, 0}, //cpri1 interrupt, special handling
    {0, 0}, //cpri2 interrupt, special handling
    {0, 0}, //m-cpri0 interrupt, special handling
    {0, 0}, //m-cpri1 interrupt, special handling
    {0, 0}, //m-cpri2 interrupt, special handling
    {0, 0}, //m-cpri3 interrupt, special handling
    {MM_B_IQC_IRQ_STATUS_TRAP, MM_B_IQC_IRQ_STATUS_MASK}, //three cpri uses different bit mask
    {MM_B_IQC_IRQ_STATUS_TRAP, MM_B_IQC_IRQ_STATUS_MASK}, //three cpri uses different bit mask
    {MM_B_IQC_IRQ_STATUS_TRAP, MM_B_IQC_IRQ_STATUS_MASK}, //three cpri uses different bit mask
    {0, 0}, //Interconnection CPRI Link 0 IQC irq.  --reserved
    {0, 0}, //Interconnection CPRI Link 1 IQC irq.  --reserved
    {0, 0}, //Interconnection CPRI Link 2 IQC irq.  --reserved
    {0, 0}, //Interconnection CPRI Link 3 IQC irq.  --reserved
    {MM_B_JESD204B_TX_GLB_L0_IRQ, MM_B_JESD204B_TX_GLB_L0_IRQ_MASK},
    {MM_B_JESD204B_RX_GLB_L0_IRQ, MM_B_JESD204B_RX_GLB_L0_IRQ_MASK}, 
    {MM_B_TDD_CTRL_IRQ_TRAP, MM_B_TDD_CTRL_IRQ_MASK}, //B_TDD_SW_IRQ
    {MM_B_GAMMA_IRQ_TRAP, MM_B_GAMMA_IRQ_MASK}, //four cpri uses different bit mask
    {MM_B_GAMMA_IRQ_TRAP, MM_B_GAMMA_IRQ_MASK}, //four cpri uses different bit mask
    {MM_B_GAMMA_IRQ_TRAP, MM_B_GAMMA_IRQ_MASK}, //four cpri uses different bit mask
    {MM_B_GAMMA_IRQ_TRAP, MM_B_GAMMA_IRQ_MASK}, //four cpri uses different bit mask
    {0, 0}, //B_MMI_PRESS_IRQ,
    {0, 0}, //B_MMI_RELEASE_IRQ,
    {0, 0}, //B_NMI_6P7S_IRQ,
    {0, 0}, //B_DBUART_IRQ,
    {MM_B_SPI_STATUS_TRAP, MM_B_SPI_STATUS_MASK},
    {0, 0}, //B_AC_AFE7689_STATUS,
    {0, 0}, //B_WIRETAP,
    {MM_B_AC_BP_IRQ_DL_TRAP, MM_B_AC_BP_IRQ_DL_MASK},
    {MM_B_AC_BP_IRQ_UL_TRAP, MM_B_AC_BP_IRQ_UL_MASK},
};

enum trxmSubIntBit
{
    T_MISC_IRQ,
    T_VCA_PWR_IRQ,
    T_DBG_PWR_IRQ,
    T_DUC_IRQ,
    T_DDC_IRQ,
    T_204B_TX_IRQ,
    T_204B_RX_IRQ,
    T_DL_AC_TRANS_IRQ,
    T_GAMMA_IRQ,
    T_SPI_IRQ,
    T_M_CPRI_LINK_0_IRQ, //Interconection CPRI status IRQ
    T_AFE7689_0_0_1, //# 0  lane # 0 and 1
    T_AFE7689_0_2_3, //# 0 lane # 2 and 3
    T_AFE7689_1_0_1, //# 1  lane # 0 and 1
    T_AFE7689_1_2_3, //# 1 lane # 2 and 3
    T_AFE7689_2_0_1, //# 2  lane # 0 and 1
    T_AFE7689_2_2_3, //# 2 lane # 2 and 3
    T_AFE7689_3_0_1, //# 3  lane # 0 and 1
    T_AFE7689_3_2_3, //# 3 lane # 2 and 3
    T_FUBF_IRQ,
    T_TAP_IRQ,
    T_AGC_IRQ,
    T_TDD_IRQ, // --reserved
    T_MAX_SUB_IRQ
};

uint32_t trxmSubIntMask[T_MAX_SUB_IRQ][MAX_REG_TYPE] =
{
    {MM_T_MISC_IRQ_TRAP, MM_T_MISC_IRQ_MASK},
    {MM_T_PWR_RSLT_IRQ_TRAP, MM_T_PWR_RSLT_IRQ_MASK},
    {MM_T_PWR_RSLT_IRQ_TRAP, MM_T_PWR_RSLT_IRQ_MASK},
    {0, 0}, //ddc, special handling
    {0, 0}, //duc, special handling
    {MM_T_JESD204B_TX_GLB_L0_IRQ, MM_T_JESD204B_TX_GLB_L0_IRQ_MASK},
    {0, 0}, //Two registers need to be checked, special handling
    {0, 0}, //T_DL_AC_TRANS_IRQ, reserved
    {MM_T_GAMMA_IRQ_TRAP, MM_T_GAMMA_IRQ_MASK},
    {MM_T_SPI_STATUS_TRAP, MM_T_SPI_STATUS_MASK}, //spi
    {0, 0}, //T_M_CPRI_LINK_0_IRQ, special handling
    {0, 0}, //T_AFE7689_0_0_1, //# 0  lane # 0 and 1
    {0, 0}, //T_AFE7689_0_2_3, //# 0 lane # 2 and 3
    {0, 0}, //T_AFE7689_1_0_1, //# 1  lane # 0 and 1
    {0, 0}, //T_AFE7689_1_2_3, //# 1 lane # 2 and 3
    {0, 0}, //T_AFE7689_2_0_1, //# 2  lane # 0 and 1
    {0, 0}, //T_AFE7689_2_2_3, //# 2 lane # 2 and 3
    {0, 0}, //T_AFE7689_3_0_1, //# 3  lane # 0 and 1
    {0, 0}, //T_AFE7689_3_2_3, //# 3 lane # 2 and 3
    {0, 0}, //IFFT_FBUF_IRQ, special handling
    {MM_T_TAP_BUFFER_STATUS_TRAP, MM_T_TAP_BUFFER_STATUS_MASK},
    {MM_T_AGC_OFL_IRQ_TRAP, MM_T_AGC_OFL_IRQ_MASK},
    {0, 0} //T_TDD_IRQ // --reserved
};


/*----------------------------  Declaration of Global Functions  ------------*/
void fpga_int_dummy(uint32_t irq_no);
void bp_int_notify(uint32_t irq_no);
void bp_int_ac_ctrl(uint32_t irq_no);
void bp_int_cpri(uint32_t irq_no);
void bp_int_m_cpri(uint32_t irq_no);
void bp_int_dbuart(uint32_t irq_no);
void bp_int_ewtimer(uint32_t irq_no);
void bp_int_mmi(uint32_t irq_no);
void trxm_int_m_cpri(uint32_t irq_no);
void trxm_int_notify(uint32_t irq_no);
void disableBpSubInt(uint32_t irq_no);
void disableTrxmSubInt(uint32_t irq_no);
bool hunt_xcs_process();
bool hunt_ricr_process();
bool hunt_mricr_process();
bool hunt_app_process();
bool hunt_ac_process();
void installBpInt();
void installTrxmInt();
void installTrxmEcpInt();

typedef struct
{
  uint32_t sigNo;
  uint32_t intProcId;
} interruptChangeIndS_t;

typedef struct
{
    uint32_t  sigNo;
    uint32_t  link_no;
    uint32_t  status;
    uint32_t  status_trap;
    uint32_t  status_mask;
    uint32_t  status2;
    uint32_t  status2_trap;
    uint32_t  status2_mask;
    uint32_t  mon;
    uint32_t  mon2;
    uint32_t  ctrl;
    uint32_t  debug;
}RicrInternalLinkStatusChangeS;

typedef struct
{
    uint32_t  sigNo;
    uint32_t  link_no;
    uint32_t  status;
    uint32_t  status_trap;
    uint32_t  status_mask;
    uint32_t  status2;
    uint32_t  status2_trap;
    uint32_t  status2_mask;
    uint32_t  mon;
    uint32_t  mon2;
    uint32_t  ctrl;
    uint32_t  debug;
}MricrInternalLinkStatusChangeS;

#endif /* RHD_INT_H */
