/******************************************************************************
 ************************* Copyright ERICSSON CBC 2014 ************************
 ******************************************************************************
 *
 * The Copyright to the computer programs herein is the property of ERICSSON AB.
 * The programs may be used and/or copied only with the written permission from
 * ERICSSON AB or in accordance with the terms and conditions stipulated in
 * the agreement/contract under which the programs have been supplied.
 *
 *******************************************************************************
 *
 * Revision history
 *    Date                  Author            Description
 * 2015-03-05         ezhirwa           Fix Coverity errors CID #11063
 * 2015-03-14         ezhirwa           Fix Coverity errors CID #10942
 *
 *******************************************************************************
 */
#include "acIntHandler.h"
#include "acRegisters.h"
#include "acSignal.h"
#include "acDispatch.h"
 

#include <uio_helper.h>
#include <stdio.h>
#include <string.h>
#include <shell.h>
#include <stdlib.h>
#include <itc.h>

//#define TRACEPOINT_PROVIDER     com_ericsson_xcs_bpar_config
//#include "tpt_create.h"
//#include "tpt.h" 
#include "ac_defs.h"

#if 0
#include "drvFactory.h"
#include "dataBase.h"

#include "xpai_xte_if_TPT.h"
#include "xpai_xte_if_ose.h"
#include "xpai_xte_if_TPT_TRACE.h"

extern "C"
{
#include <xpai_xhl_if.h>
#include <xpai_xcbc_basic_if.h>
#include <xdai_xdlm_if.h>
#include <xpai_xll_if.h>
#include <xpai_xsp_if.h>
#include <xpai_xmr_if.h>
}
#endif

#define OAM_XDOS_DEV                      "oam_XDOS" 

//extern PROCESS  acControlProc_;

//REGISTER_HANDLER_IN_DRV_FACTORY(AcIntHandler)
//IMPORT_TPT_TRACE(INTERRUPT)

static void *oam_uio_handle = NULL;
static void *oam_mmap_base = NULL;
static volatile U32 *isrAddrOam = NULL;
static volatile U32 *ierAddrOam = NULL;
static itc_mbox_id_t ac_isr_mbox = ITC_NO_ID;
static itc_mbox_id_t ac_main_mbox = ITC_NO_ID;
extern bool get_mbox(const char *mbox_name, itc_mbox_id_t *mbox_id);

AcIntHandler::AcIntHandler()//:m_fpga(NULL)
{
}

AcIntHandler::~AcIntHandler()
{
}

bool AcIntHandler::init(const char* instanceDrvName)
{
    printf("AcIntHandler.cc line %d \n", __LINE__);
#if 0
    m_fpga = (MemoryMappedDevice*) DrvFactory::getInstance(fpgaStr);

    if(!m_fpga)
    {
       TPT_ERROR(STR("failed to get fpga driver \n"));
       return false;
    }
#endif
    installOamInt(); 
    printf("AcIntHandler.cc line %d \n", __LINE__);
}

DefaultDriver* AcIntHandler::creator(void)
{
    //return (DefaultDriver*) (new AcIntHandler());
    return getInstance();
}

AcIntHandler* AcIntHandler::getInstance()
{
    static AcIntHandler instance;
    return &instance;
}

void AcIntHandler::installOamInt()
{
    // This function should be implemented in xpp.
    printf("AcIntHandler.cc line %d \n", __LINE__);
    volatile U32 *iulrAddr = NULL; //int use level register , arm RU set this reg
    volatile U32 *ilrAddr = NULL; //int level register
    volatile U32 *iupeAddr = NULL; //int use positive edge , arm RU set this reg
    volatile U32 *iuneAddr = NULL; //int use negative edge

    //map oam register to user space
    oam_uio_handle = (void *) uio_open(OAM_XDOS_DEV);  //#define OAM_XDOS_DEV                             "oam_XDOS"
    if (oam_uio_handle == (UIO_HANDLE_) - 1) 
    {
        //TPT_TRACE(3, STR("oam fpga uio_open funciton return error"));
        return;
    }
    //TPT_TRACE(3,STR("oam fpga uio_open success"));

    oam_mmap_base = uio_mmap(oam_uio_handle);
    if (oam_mmap_base == MAP_FAILED)
    {
        oam_mmap_base = NULL;
        //TPT_ERROR(STR("oam fpga uio_mmap funciton return error"));
        uio_close(oam_uio_handle);
        return;
    }
    //TPT_TRACE(3,STR("oam fpga uio_mmap success! base address is %p", oam_mmap_base));

    uio_disable_irq(oam_uio_handle);
    
    //configure traffic fpga interrupt
    isrAddrOam = (((volatile uint32_t *)((uintptr_t) oam_mmap_base + MM_Z_IRQ_IFR*4)));
    ierAddrOam = (((volatile uint32_t *)((uintptr_t) oam_mmap_base + MM_Z_IRQ_IMR*4)));
    iulrAddr = (((volatile uint32_t *)((uintptr_t) oam_mmap_base + MM_Z_IRQ_USE_LEVEL*4)));
    ilrAddr = (((volatile uint32_t *)((uintptr_t) oam_mmap_base + MM_Z_IRQ_LEVEL*4)));
    iupeAddr = (((volatile uint32_t *)((uintptr_t) oam_mmap_base + MM_Z_IRQ_USE_POS_EDGE*4)));
    iuneAddr = (((volatile uint32_t *)((uintptr_t) oam_mmap_base + MM_Z_IRQ_USE_NEG_EDGE*4)));

    //configure interrupt
    *ierAddrOam = 0x00; //disable all interrupt
    *iulrAddr = 0xFFFFFFFF; //enable level trigger
    *ilrAddr = 0xFFFFFFFF; //enable level trigger
    *iupeAddr = 0x00; //un-select rising edge
    *iuneAddr = 0x00; //un-select falling edge
    *isrAddrOam = 0xFFFFFFFF;  //clear all interrupt status

    printf("AcIntHandler.cc line %d \n", __LINE__);
    //enable uio interrupt    
    if (uio_irq_set_notifier(oam_uio_handle, oam_int_process, NULL)) 
    {
        //TPT_ERROR(STR("%s: unable to set UIO interrupt notifier\n", __func__));
        uio_close(oam_uio_handle);
        return;
    }

    printf("AcIntHandler.cc line %d \n", __LINE__);
    if (uio_bind_irq_rt(oam_uio_handle, 60)) 
    {
        //TPT_ERROR(STR("%s: unable to start UIO interrupt handler\n", __func__));
        uio_close(oam_uio_handle);
        return;
    }

    printf("AcIntHandler.cc line %d \n", __LINE__);
    uio_enable_irq(oam_uio_handle); //enable uio interrupt

    *ierAddrOam = 0xFFFFFFFF; //enable all interrupt

    //TPT_TRACE(3, STR("Enable oam fpga interrupt done"));

    printf("AcIntHandler.cc line %d \n", __LINE__);
    if(get_mbox(AC_DISPATCH_MAILBOX, &ac_main_mbox))
    {
        //TPT_ERROR(STR("Can not get ac_main_mbox !"));
        //return;
    }

    printf("AcIntHandler.cc line %d \n", __LINE__);
    //printf("Enable oam fpga interrupt done\n");
}

void AcIntHandler::oam_int_process(void *)
{
    //PROCESS acCtrlPoc;
    U32 trap, mask;
    isrAddrOam = (((volatile uint32_t *)((uintptr_t) oam_mmap_base + MM_Z_IRQ_IFR*4)));
    ierAddrOam = (((volatile uint32_t *)((uintptr_t) oam_mmap_base + MM_Z_IRQ_IMR*4)));

    if (ac_isr_mbox == ITC_NO_ID) 
    {
        ac_isr_mbox = itc_create_mailbox("ac_isr_mbox", 0);
    }

    //bit 1 interrupt happen
    trap = *isrAddrOam;
    mask = *ierAddrOam;
    //TPT_TRACE(3,STR("AC interrupt -- trap:0x%x mask:0x%x",trap,mask));
    if(trap & 0x02)
    {
        *ierAddrOam = (mask&0xFFFFFFFD);
        *isrAddrOam = 0x02;
        //TPT_TRACE(3,STR("DL send data done interrupt!"));

#if 0
        if(!hunt(AC_CONTROL_PROC, 0, &acCtrlPoc, 0))
        {
            TPT_ERROR(STR("ac control process can't be found."));
            //free_buf(&carEnableInd);
            return;
        }
#endif

        if(ac_main_mbox == ITC_NO_ID)
        {
            if(get_mbox(AC_DISPATCH_MAILBOX, &ac_main_mbox))
            {
                //TPT_ERROR(STR("Can not get ac_main_mbox !"));
                return;
            }
        }
        
        //int handler
        union itc_msg *sig;
        sig = itc_alloc(sizeof(intTxAcDone_t), INT_TX_AC_DONE_IND);
        sig->txAcDone.carrierId = 100;
        sig->txAcDone.portId = 0;
        //send(&sig, acCtrlPoc);
        itc_send(&sig, ac_main_mbox, ITC_MY_MBOX);

        *ierAddrOam |= 0x02;
    }
    else if(trap & 0x04)
    {
        *ierAddrOam = (mask&0xFFFFFFFB);
        *isrAddrOam = 0x04;
        //TPT_TRACE(3,STR("UL send data done interrupt!"));
        //int handler
#if 0
        if(!hunt(AC_CONTROL_PROC, 0, &acCtrlPoc, 0))
        {
            TPT_ERROR(STR("ac control process can't be found."));
            //free_buf(&carEnableInd);
            return;
        }
#endif
        if(ac_main_mbox == ITC_NO_ID)
        {
            if(get_mbox(AC_DISPATCH_MAILBOX, &ac_main_mbox))
            {
                //TPT_ERROR(STR("Can not get ac_main_mbox !"));
                return;
            }
        }

        union itc_msg *sig;
        sig = itc_alloc(sizeof(intRxAcDone_t), INT_RX_AC_DONE_IND);
        sig->rxAcDone.carrierId = 100;
        //send(&sig, acCtrlPoc);
        itc_send(&sig, ac_main_mbox, ITC_MY_MBOX);

        *ierAddrOam |= 0x04;
    }
    uio_enable_irq(oam_uio_handle);    
}

void AcIntHandler::testInterfaceInit(void)
{
    char usage[128] = "";
    strcat(usage, instanceDrvName);
    strcat(usage, " <trap> <mask> ");
    //shell_add_cmd(instanceDrvName, usage, "acIntHandler test interface", testCmdHandler);
}

int AcIntHandler::testCmdHandler(int argc, char **argv)
{
    //AcIntHandler *pHdl = NULL;
    //Ac *pInst = NULL;
    //U32 cellId = 0;
    PROCESS acCtrlPoc;
    U32 trap, mask;

    if(argc ==3)
    {
        //pHdl = (AcIntHandler *)DrvFactory::getInstance(argv[0]);
        
        trap = (U32)strtoul(argv[1], NULL, 0);
        mask = (U32)strtoul(argv[2], NULL, 0);
        printf("AC interrupt -- trap:0x%x mask:0x%x",trap,mask);
        if(trap & 0x02)
        {
            //*ierAddrOam = (mask&0xFFFFFFFD);
            //*isrAddrOam = 0x02;
            printf("DL send data done interrupt!");
#if 0
            if(!hunt(AC_CONTROL_PROC, 0, &acCtrlPoc, 0))
            {
                printf("ac control process can't be found.");
                //free_buf(&carEnableInd);
                return 0;
            }
#endif

            //int handler
            union itc_msg *sig;
            sig = itc_alloc(sizeof(intTxAcDone_t), INT_TX_AC_DONE_IND);
            sig->txAcDone.carrierId = 100;
            sig->txAcDone.portId = 0;
            //itc_send(&sig, acCtrlPoc);
            printf("DL interrupt send!");
            //*ierAddrOam |= 0x02;
        }
        else if(trap & 0x04)
        {
            //*ierAddrOam = (mask&0xFFFFFFFB);
            //*isrAddrOam = 0x04;
            printf("UL send data done interrupt!");
            //int handler

#if 0
            if(!hunt(AC_CONTROL_PROC, 0, &acCtrlPoc, 0))
            {
                printf("ac control process can't be found.");
                //free_buf(&carEnableInd);
                return 0;
            }
#endif
            union itc_msg *sig;
            sig = itc_alloc(sizeof(intRxAcDone_t), INT_RX_AC_DONE_IND);
            sig->rxAcDone.carrierId = 100;
            //itc_send(&sig, acCtrlPoc);
            printf("UL interrupt send!");
            //*ierAddrOam |= 0x04;
         }        
    }
}

