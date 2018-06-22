/* >  CONTENTS
 ******************************************************************************
 ************************** Copyright ERICSSON CBC ****************************
 ******************************************************************************
 *
 * The copyright to the computer programs herein is the property of
 * ERICSSON China (CBC). The programs may be used and/or copied
 * only with the written permission from ERICSSON China (CBC) or
 * in accordance with the terms and conditions stipulated in the
 * agreement/contract under which the prgrams have been supplied.
 *
 ******************************************************************************
 CONTENTS
 --------
 1  Description
 2  Include files
 3  Declarations and Definitions
 4  Signal Definitions
 5  Function prototypes
 6  Functions
  ******************************************************************************
 */
/* >  1  DESCRIPTION
 *******************************************************************************
 *
 * General ac dispatch
 * hardware specific handling was extrac to acDispatch class
 *
 * Revision history
 *     Date                   Author             Description
 *  2016-06-23           edentao           First Revision
 *  2016-08-05           edentao           2nd Revision
 *  2016-08-18           edentao           introduce ac algorithm lib
 *  2016-11-28           edentao           fpga access
 *
 *******************************************************************************
 */


/* >  2  INCLUDE FILES
 *******************************************************************************
 */
/*******C libary header*******/
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
/*******  XCS header  *******/
#include <uio_helper.h>
#include <itc.h>
#include <unistd.h>
/*******  AC header  *******/
#include "ac_defs.h"
#include "ma_mimo_backplaneRegisters.h"
#include "acDispatch.h"
#include "acSignal.h"
extern "C"{
#include "timer-if.h"
}
#include "time.h"
#include "logFile.h"
#include "fpga.h"
//#include "NE10_dsp.h"
//#include "NE10_macros.h"
extern "C"{
#include "acAlgorithmFuncMassiveMIMO.h"
}

#if 1
#define TRACEPOINT_PROVIDER     com_ericsson_ac_trace_dispatch
#include "tpt_create.h"
#include "tpt.h"
#endif

/* >  3  DECLARATIONS AND DEFINITIONS
 ******************************************************************************
 */

double default_delay_db[ULCELLPE_AC_MAX_ANT_PER_CELL] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                                                      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                                                      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                                                      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};


double default_phase_db[ULCELLPE_AC_MAX_ANT_PER_CELL] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                                                      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                                                      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                                                      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};


double default_amplit_db[ULCELLPE_AC_MAX_ANT_PER_CELL] = {1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                                                       1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                                                       1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                                                       1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1};
/*

double default_delay_db[ULCELLPE_AC_MAX_ANT_PER_CELL] = {274,274,273,274,274,273,275,275,274,274,274,275,274,274,274,274,274,274,273,275,274,275,274,274,272,270,270,269,271,269,270,271,273,273,274,275,274,272,271,269,271,270,271,272,272,271,271,271,274,275,275,274,270,269,269,271,272,271,271,271,273,274,273,272};

S32 default_phase_db[ULCELLPE_AC_MAX_ANT_PER_CELL] = {32258,32882,33316,33332,33482,33432,33314,33277,33889,34021,33892,33297,33327,33503,33297,32900,32882,33332,33432,33277,34021,33297,33503,32900,34794,34473,35162,36089,35020,35790,35272,34547,33316,33432,33889,33297,33297,34794,34733,36089,35358,35272,34327,34585,34722,34872,35128,34076,33332,33277,33297,32900,34473,36089,35790,34547,34585,35066,34835,34076,34660,35960,35080,33924};

 double default_amplit_db[ULCELLPE_AC_MAX_ANT_PER_CELL] = {2266299,2364683,2298674,2271474,2292222,2269173,2236592,2304119,2318170,2234573,2266656,2236493,2271311,2286905,2231594,2214858,2364683,2271474,2269173,2304119,2234573,2236493,2286905,2214858,2687087,2712057,2468615,2497774,2500881,2483459,2763018,2501852,2298674,2269173,2318170,2236493,2231594,2687087,2474624,2497774,2538291,2763018,2576713,2503583,2500895,2464746,2427031,2510082,2271474,2304119,2236493,2214858,2712057,2497774,2483459,2501852,2503583,2465595,2466291,2510082,2387261,2370482,2362161,2403001};
*/

double delay_hwdb[ULCELLPE_AC_MAX_ANT_PER_CELL];
double phase_hwdb[ULCELLPE_AC_MAX_ANT_PER_CELL];
double amplit_hwdb[ULCELLPE_AC_MAX_ANT_PER_CELL];

/*
S32 delay_hwdb[ULCELLPE_AC_MAX_ANT_PER_CELL] = {274,274,273,274,274,273,275,275,274,274,274,275,274,274,274,274,274,274,273,275,274,275,274,274,272,270,270,269,271,269,270,271,273,273,274,275,274,272,271,269,271,270,271,272,272,271,271,271,274,275,275,274,270,269,269,271,272,271,271,271,273,274,273,272};

S32 phase_hwdb[ULCELLPE_AC_MAX_ANT_PER_CELL] = {32258,32882,33316,33332,33482,33432,33314,33277,33889,34021,33892,33297,33327,33503,33297,32900,32882,33332,33432,33277,34021,33297,33503,32900,34794,34473,35162,36089,35020,35790,35272,34547,33316,33432,33889,33297,33297,34794,34733,36089,35358,35272,34327,34585,34722,34872,35128,34076,33332,33277,33297,32900,34473,36089,35790,34547,34585,35066,34835,34076,34660,35960,35080,33924};

 double amplit_hwdb[ULCELLPE_AC_MAX_ANT_PER_CELL] = {2266299,2364683,2298674,2271474,2292222,2269173,2236592,2304119,2318170,2234573,2266656,2236493,2271311,2286905,2231594,2214858,2364683,2271474,2269173,2304119,2234573,2236493,2286905,2214858,2687087,2712057,2468615,2497774,2500881,2483459,2763018,2501852,2298674,2269173,2318170,2236493,2231594,2687087,2474624,2497774,2538291,2763018,2576713,2503583,2500895,2464746,2427031,2510082,2271474,2304119,2236493,2214858,2712057,2497774,2483459,2501852,2503583,2465595,2466291,2510082,2387261,2370482,2362161,2403001};
*/

double* delay_db;
double* phase_db;
double* amplit_db;
 

/* >  3.1  GLOBAL
 ******************************************************************************
 */
#define MAXNUM_OF_AC_FAULT_RETRY         0
// Before starting AC for one carrier, if there has any other carrier is doing  
// AC (in ULCELLPE_ACSES_PROCESS status ), the starting AC operation should wait for 10s.
// The maximum wait times is 3. After 3 times wait, will start AC for the carrier directlly.
#define MAXNUM_OF_WAIT_AC_DONE_RETRY         3

itc_mbox_id_t AcDispatcher::ac_app_mbox = 0;
itc_mbox_id_t AcDispatcher::radio_bp_mbox = 0;
itc_mbox_id_t AcDispatcher::rhd_int_mbox = 0;
itc_mbox_id_t AcDispatcher::app_fault_mgr_mbox = 0;

void * AcDispatcher::traffic_uio_ac_handle = NULL;
void * AcDispatcher::traffic_mmap_ac_base = NULL;
//PROCESS acControlProc_;
extern S16 complexRefSignal20Mhz[SIZE_OF_AC_FREQ_DOMAIN_FFT_SIZE*2];
extern S16 complexSignal20MhzErrDelay[SIZE_OF_AC_FREQ_DOMAIN_FFT_SIZE*2];
extern S16 complexSignal20MhzErrDistb[SIZE_OF_AC_FREQ_DOMAIN_FFT_SIZE*2];
extern S16 complexSignal20MhzErrSigLow[SIZE_OF_AC_FREQ_DOMAIN_FFT_SIZE*2];
//AC start Sequence data
extern S16 complexInputTimeSignal20Mhz[SIZE_OF_AC_TIME_DOMAIN_FFT_SIZE*2];
extern S16 complexInputEVM20dBFS20M[SIZE_OF_AC_TIME_DOMAIN_FFT_SIZE*2];
extern S16 complexInputSingleTone[SIZE_OF_AC_TIME_DOMAIN_FFT_SIZE*2];
extern S16 complexInputEVM15dBFS20M[SIZE_OF_AC_TIME_DOMAIN_FFT_SIZE*2];
extern bool get_mbox(const char *mbox_name, itc_mbox_id_t *mbox_id);


/* >  3.2  LOCAL
 ******************************************************************************
 */
/* >  4  SIGNAL DEFINITIONS
 ******************************************************************************
 */

/* >  5  FUNCTION PROTOTYPES
 ******************************************************************************
 */
/* >  6  FUNCTIONS
 ******************************************************************************
 */
/* >  6.1  GLOBAL
 ******************************************************************************
 */
double ampFirstDetectUpValue = 1.4;
double ampFirstDetectDownValue = 0.7;
U32 acWindowFlag = 0;
U32 acMemBaseAddress = 0x1DC00000;
U16 runPeriodAcImdt = 0;

/* >  6.2  LOCAL
 ******************************************************************************
 */
#define VER_INFO "20170414.01"
/*
void antennaCalibration(S32* ddrinputSamples_p,           //input
                               S16 noOfSc,                  //input
                               S16* delay_p,                //output
                               S16* initPhase_p,            //output
                               S32* amplitude_p,            //output
                               S16* channelSumCentidB_p,    //output
                               U32* disturbance_p,          //output
                               S16* noShift_p               //output
                              )
{
    TPT_INFO(STR("AC algorithm input: noOfSc %d, data[0].re = %d, data[0].im = %d",
                noOfSc, *ddrinputSamples_p>>16, *ddrinputSamples_p&0x0000FFFF));

    TPT_INFO(STR("AC algorithm input: data[%d].re = %d, data[%d].im = %d",
                noOfSc-1, *(ddrinputSamples_p+noOfSc-1)>>16, 
                noOfSc-1, *(ddrinputSamples_p+noOfSc-1)&0x0000FFFF));


    *delay_p = 519;
    *initPhase_p = -181;
    *amplitude_p = 455;
    *channelSumCentidB_p = 531;
    *disturbance_p = 8;
    *noShift_p = 1;
    return;
}
*/

/* >  AcDispatcher::AcDispatcher
 *****************************************************************************
 *
 *  Description :  Construction function of AcDispatcher Class.
 *
 *  Arguments   :  void
 *
 *  Return      :  void
 *
 *****************************************************************************
 */
AcDispatcher::AcDispatcher(void) : DefaultDriver()
{
    instanceDrvName = NULL;
}

AcDispatcher::~AcDispatcher()
{
    ::free(instanceDrvName);
}

/* >  AcDispatcher::creator
 *****************************************************************************
 *
 *  Description :  The creator function of RadioInterfaceCtrl.
 *
 *  Arguments   :    void
 *
 *  Return      :
 *        Type                                  Comments
 *  DefaultDriver*                  Return the RadioInterfaceCtrl instance created.
 *
 *****************************************************************************
 */
DefaultDriver* AcDispatcher::creator(void)
{
    return getInstance();
}

AcDispatcher* AcDispatcher::getInstance()
{
    static AcDispatcher instance;
    return &instance;
}

bool AcDispatcher::storeAcFreqData(ULCELLPE_ACSES_classS* acSes_p)
{
    U16 dataSize = 2*SIZE_OF_AC_FREQ_DOMAIN_FFT_SIZE;
    char bufTemp[12];
    char strBuffer[240];
    U16 i, j;
    S16 * dataFreq;
    U32 carrOffset = 8192 * ULCELLPE_AC_MAX_ANT_PER_CELL;
    U32 directOffset = 8192 *  ULCELLPE_AC_MAX_ANT_PER_CELL * 3;
    U32 portOffset = 8192;
    U32 offsetBase = 0;
    for (i = 0; i < ULCELLPE_AC_MAX_ANT_PER_CELL; i++)
    {
        if(acSes_p->acPortInfo[0][i].carrActive && acSes_p->acPortInfo[1][i].carrActive 
            && acSes_p->acPortInfo[0][i].freqDataStoreFlag 
            && acSes_p->acPortInfo[1][i].freqDataStoreFlag)
        {
            TPT_INFO(STR("AcDispatcher is logging AC Freq Data carrier%d, port%d, TX:", 
                        acSes_p->carrierId, i));
            if ((acSes_p->acPortInfo[0][i].acDoneStatus))
            {
                //store TX data
                //pAcLogFile->storeAcData(acSes_p->carrierId, i, 1, 
                //        (S16 *)acSes_p->acFreqDomainData, 2*SIZE_OF_AC_FREQ_DOMAIN_FFT_SIZE);
                offsetBase = acSes_p->carrierId * carrOffset 
                                   + 0 * directOffset
                                   + i * portOffset;
                freqDataMem->getFreqDataFromAcMem((S16 *)(acSes_p->acFreqDomainData), 
                             SIZE_OF_AC_FREQ_DOMAIN_FFT_SIZE*2, offsetBase/2);
                dataFreq = (S16 *)acSes_p->acFreqDomainData;
                memset(strBuffer, 0, 240);
                for( j=0; j<dataSize; j++)
                {
                    sprintf(bufTemp, "%8d,", dataFreq[j]);
                    strcat(strBuffer, bufTemp);
                    if((j+1)%20 == 0)
                    {
                        TPT_INFO(STR("%s", strBuffer));
                        memset(strBuffer, 0, 240);
                    }
                }
            }
            TPT_INFO(STR("AcDispatcher is logging AC Freq Data carrier%d, port%d, RX:", 
                        acSes_p->carrierId, i));
            if ((acSes_p->acPortInfo[1][i].acDoneStatus))
            {
                //store RX data
                //pAcLogFile->storeAcData(acSes_p->carrierId, i, 0, 
                ///        (S16 *)acSes_p->acFreqDomainData, 2*SIZE_OF_AC_FREQ_DOMAIN_FFT_SIZE);
                offsetBase = acSes_p->carrierId * carrOffset 
                                   + 1 * directOffset
                                   + i * portOffset;
                freqDataMem->getFreqDataFromAcMem((S16 *)(acSes_p->acFreqDomainData), 
                             SIZE_OF_AC_FREQ_DOMAIN_FFT_SIZE*2, offsetBase/2);
                dataFreq = (S16 *)acSes_p->acFreqDomainData;
                memset(strBuffer, 0, 240);
                for( j=0; j<dataSize; j++)
                {
                    sprintf(bufTemp, "%8d,", dataFreq[j]);
                    strcat(strBuffer, bufTemp);
                    if((j+1)%20 == 0)
                    {
                        TPT_INFO(STR("%s", strBuffer));
                        memset(strBuffer, 0, 240);
                    }
                }
            }
        }
    }
    return true;
#if 0
    for (U16 i = 0; i < ULCELLPE_AC_MAX_ANT_PER_CELL; i++)
    {
        if(acSes_p->acPortInfo[0][i].carrActive && acSes_p->acPortInfo[1][i].carrActive )
        {
            TPT_INFO(STR("AcDispatcher is logging AC Freq Data carrier%d, port%d.", 
                        acSes_p->carrierId, i));
            if ((acSes_p->acPortInfo[0][i].acDoneStatus))
            {
                //store TX data
                pAcLogFile->storeAcData(acSes_p->carrierId, i, 1, 
                        (S16 *)acSes_p->acFreqDomainData, 2*SIZE_OF_AC_FREQ_DOMAIN_FFT_SIZE);

            }
            if ((acSes_p->acPortInfo[1][i].acDoneStatus))
            {
                //store RX data
                pAcLogFile->storeAcData(acSes_p->carrierId, i, 0, 
                        (S16 *)acSes_p->acFreqDomainData, 2*SIZE_OF_AC_FREQ_DOMAIN_FFT_SIZE);
            }
        }
    }
    return true;
#endif
}

/* >  AcDispatcher::init
 *****************************************************************************
 *
 *  Description :  The inialization of AcDispatcher.
 *
 *  Arguments   :
 *     Type                     INPUT/OUTPUT          PARAMETER
 *    char *                       INPUT            instanceDbName
 *
 *  Return      :
 *  Type                                  Comments
 *  bool             Return true if success, return false if failed.
 *
 *****************************************************************************
 */
bool AcDispatcher::init(const char* instanceDbName)
{
    instanceDrvName = ::strdup(instanceDbName);
    TPT_INFO(STR("AC init module %s, version %s.", instanceDbName, VER_INFO));

    acSesList.empty();
    //startUpDelay = 300000;     /*5min*60s*1000ms*/
    //intervalPeriod = 600000;    /*10min*60s*1000ms*/
    //startUpDelay = 15000;     /*5min*60s*1000ms*/
    //intervalPeriod = 60000;    /*10min*60s*1000ms*/
    startUpDelay = 60;     // changed to seconds
    startFirstPerAcDelay = 60; // seconds
    intervalPeriod = 60;    // changed to seconds
    waitACDoneDelay = 10; // seconds
    acFreqDataSrc = AC_FREQ_DATA_FROM_FPGA;
	
    //init LogFile Instance
    pAcLogFile = LogFile::getInstance();
    pAcLogFile->init();

    ac_app_mbox = 0;
    if(!get_mbox(AC_DISPATCH_MAILBOX, &ac_app_mbox))
    {
        TPT_ERROR(STR("AC APP Mailbox is not ready."));
        return false;
    }

    //fpga Instance
    fpga = Fpga::getInstance();
    //fpga->init(TRAFFIC_XDOS_FPGA_DEV);
    if(!(fpga->init(TRAFFIC_XDOS_FPGA_DEV)))
    {
        TPT_ERROR(STR("FPGA mem mapping is not ready."));
        //return false;
    }

    //fpga Instance
    freqDataMem = AcFreqDataMem::getInstance();
    if(!(freqDataMem->init(TRAFFIC_XDOS_AC_DEV)))
    {
        TPT_ERROR(STR("AC memory is not ready."));
        //return false;
    }
    /*
    if(!initTrafficBaseAddr())
    {
        TPT_ERROR(STR("AC memory is not ready."));
        //return false;
    }
    */

    radio_bp_mbox = 0;
    if(!get_mbox(RADIO_BP_MAILBOX, &radio_bp_mbox))
    {
        readyRadioApp = false;
        TPT_ERROR(STR("Radio APP BP Mailbox is not ready. %d", radio_bp_mbox));
        //return false;
    }
    else
    {    
        readyRadioApp = true;
    }

    TPT_INFO(STR(" %s, radio_bp_mbox = 0x%x", __func__, radio_bp_mbox));

    //clear fpga ac related register
    fpga->write(MM_B_AC_BP_IRQ_DL_TRAP, MM_B_AC_BP_IRQ_DL_TRAP_ac_dl);
    fpga->write(MM_B_AC_BP_IRQ_UL_TRAP, MM_B_AC_BP_IRQ_UL_TRAP_ac_ul);
    fpga->write(MM_B_AC_BP_IRQ_DL_MASK, MM_B_AC_BP_IRQ_DL_MASK_ac_dl);
    fpga->write(MM_B_AC_BP_IRQ_UL_MASK, MM_B_AC_BP_IRQ_UL_MASK_ac_ul);
    fpga->write(MM_B_UL_AC_ADJUST_CTRL, (U32)0x02);
    fpga->write(MM_B_DL_AC_ADJUST_CTRL, (U32)0x02);

    //interrupt register to RHD_INT process
    rhd_int_mbox = 0;
    if(!get_mbox(RHD_INT_MAILBOX, &rhd_int_mbox))
    {
        TPT_ERROR(STR("RHD INT Mailbox is not ready."));
        //return false;
    }
    else
    {    
        union itc_msg *outSig;
        outSig = itc_alloc(sizeof(IntAcReadyIndS_t), AC_INT_REGISTER_REQ);
        itc_send(&outSig, rhd_int_mbox, ITC_MY_MBOX);
    }

    //initTrafficBaseAddr();
    //AcTraceObjInst.enableAcTraceInfo();
    //config AC Seq data into FPGA before AC start.
    //initAcSeqDataInFPGA();
    TPT_INFO(STR("AC Seq Original data is initializing."));
    configAcSeqData(complexInputTimeSignal20Mhz, 
            SIZE_OF_AC_TIME_DOMAIN_FFT_SIZE);

    //removed EVM data writting for test.
    //testEVMdata();

    getHwDbCompStatus = false;

    char *ptr = NULL;
    ptr = getenv("SYS_PRODUCTREVISION");
    if (ptr == NULL)
    {
        acMemBaseAddress = 0x1DC00000;
    }
    else if (strcmp(ptr, "R1A") == 0)
    {
        acMemBaseAddress = 0x3A800000;
    }
    else if (strcmp(ptr, "R1B") == 0)
    {
        acMemBaseAddress = 0x3A800000;
    }
    TPT_INFO(STR("AC mem base address 0x%08x.", acMemBaseAddress));

    return true;
}

bool AcDispatcher::initTrafficBaseAddr()
{
// #define TRAFFIC_XDOS_DEV               "ac_mem"
    traffic_uio_ac_handle = (void *) uio_open(TRAFFIC_XDOS_AC_DEV);   
    if (traffic_uio_ac_handle == (UIO_HANDLE_) - 1) 
    {
        TPT_INFO(STR("traffic ac_mem uio_open funciton return error"));
        return false;
    }
    TPT_INFO(STR("traffic ac_mem uio_open success"));

    traffic_mmap_ac_base = uio_mmap(traffic_uio_ac_handle);

    if (traffic_mmap_ac_base == MAP_FAILED)
    {
        traffic_mmap_ac_base = NULL;
        TPT_INFO(STR("traffic ac_mem uio_mmap funciton return error"));
        uio_close(traffic_uio_ac_handle);
        return false;
    }
    TPT_INFO(STR("trafficac_mem uio_mmap success! base address is %p", traffic_mmap_ac_base));

    return true;
}

void AcDispatcher::configAcSeqData(S16 * seqData, U32 size)
{
    TPT_INFO(STR("Configure AC Sequence time domain data:"));
    //i->high 16 bit, q->low 16 bit. 
    U32 i = 0;
    S32 highBit = 0;
    S32 lowBit = 0;
    S32 combVal = 0;
    U32 readVal = 0;
    for (i = 0; i < size; i++)
    {
        highBit = seqData[i*2];
        lowBit = seqData[i*2+1];
        combVal = (highBit<<16) + (lowBit & 0xFFFF);
        //write combVal into FPGA
        fpga->write(MM_B_AC_SEQ_WREN, (U32)0x00);
        fpga->write(MM_B_AC_SEQ_WRADDR, i);
        fpga->write(MM_B_AC_SEQ_WRDATA, (U32)combVal);
        fpga->write(MM_B_AC_SEQ_WREN, MM_B_AC_SEQ_WREN_VAL);
        usleep(1000);
    }

#if 1
    TPT_INFO(STR("AC Seq data Check valid:"));
    for (i = 0; i < size; i++)
    {
        //write combVal into FPGA
        //fpga->write(addr, combVal);
        fpga->write(MM_B_AC_SEQ_RDEN, (U32)0x00);
        fpga->write(MM_B_AC_SEQ_RDADDR, i);
        fpga->write(MM_B_AC_SEQ_RDEN, MM_B_AC_SEQ_RDEN_VAL);
        usleep(1000);
        fpga->read(MM_B_AC_SEQ_RDDATA, readVal);
        lowBit = readVal & 0x0000FFFF;
        highBit = readVal >> 16;
        TPT_INFO(STR("Index %d, highBit is 0x%08X, %6d; lowBit is 0x%08X, %6d.", 
            i, highBit, highBit, lowBit, lowBit));
        readVal = 0x00000000;
    }
#endif
}

void AcDispatcher::testInterfaceInit(void)
{
    //could access vector parameters of its low-layer's device.
    //give a better debugging interface
    //char usage[128] = "";
    //strcat(usage, instanceDrvName);
    //strcat(usage, " all | rsc | traceSet | test ");
    //printf("AcDispatcher.cc line %d \n", __LINE__);
    //shell_add_cmd(instanceDrvName, usage, "AcDispatcher test interface", testCmdHandler);
    //printf("AcDispatcher.cc line %d \n", __LINE__);
    //TPT_INFO(STR("AcDispatcher::testInterfaceInit Done"));
}

void AcDispatcher::handleAcCompHwDb(union itc_msg * rec_p)
{
    U32 i = 0;
    TPT_INFO(STR("------AcDispatcher get AC compensation value from APP ----"));

    if(rec_p->acCompHwDb.status ==  true)
    {
        TPT_INFO(STR("------AC compensation value from HWDB ----"));

        for (i = 0; i < ULCELLPE_AC_MAX_ANT_PER_CELL; i++)
        {
            delay_hwdb[i] = ((double)rec_p->acCompHwDb.delayComp[i]);
            phase_hwdb[i] = ((double)rec_p->acCompHwDb.phaseComp[i]);
            amplit_hwdb[i] = ((double)rec_p->acCompHwDb.ampComp[i])/10000;
        }

        delay_db = delay_hwdb;
        phase_db = phase_hwdb;
        amplit_db = amplit_hwdb;

        getHwDbCompStatus = true;
    }
    else
    {
        TPT_INFO(STR("------AC compensation value from default ----"));
        delay_db = default_delay_db;
        phase_db = default_phase_db;
        amplit_db = default_amplit_db;
        getHwDbCompStatus = true;
    }

    for(i=0; i<4; i++)
    {
        TPT_INFO(STR("delay_db[%d - %d] = %f,  %f, %f, %f, %f, %f, %f, %f, %f, %f, %f, %f, %f, %f, %f, %f", i*16, i*16+15,
                              delay_db[i*16+0], delay_db[i*16+1], delay_db[i*16+2], delay_db[i*16+3], delay_db[i*16+4], delay_db[i*16+5], delay_db[i*16+6], delay_db[i*16+7], 
                              delay_db[i*16+8], delay_db[i*16+9], delay_db[i*16+10], delay_db[i*16+11], delay_db[i*16+12], delay_db[i*16+13], delay_db[i*16+14], delay_db[i*16+15]));

        TPT_INFO(STR("phase_db[%d - %d] = %f,  %f, %f, %f, %f, %f, %f, %f, %f, %f, %f, %f, %f, %f, %f, %f", i*16, i*16+15,
                              phase_db[i*16+0], phase_db[i*16+1], phase_db[i*16+2], phase_db[i*16+3], phase_db[i*16+4], phase_db[i*16+5], phase_db[i*16+6], phase_db[i*16+7], 
                              phase_db[i*16+8], phase_db[i*16+9], phase_db[i*16+10], phase_db[i*16+11], phase_db[i*16+12], phase_db[i*16+13], phase_db[i*16+14], phase_db[i*16+15]));


        TPT_INFO(STR("amplit_db[%d - %d] = %f,  %f, %f, %f, %f, %f, %f, %f, %f, %f, %f, %f, %f, %f, %f, %f", i*16, i*16+15,
                              amplit_db[i*16+0], amplit_db[i*16+1], amplit_db[i*16+2], amplit_db[i*16+3], amplit_db[i*16+4], amplit_db[i*16+5], amplit_db[i*16+6], amplit_db[i*16+7], 
                              amplit_db[i*16+8], amplit_db[i*16+9], amplit_db[i*16+10], amplit_db[i*16+11], amplit_db[i*16+12], amplit_db[i*16+13], amplit_db[i*16+14], amplit_db[i*16+15]));

    }
}

void AcDispatcher::handleAcSettingReq(union itc_msg * rec_p)
{
    return;
}

void AcDispatcher::handleAcCarrierEnable(union itc_msg * rec_p)
{
    U16 carrierId;
    U16 deviceId;
    U32 antBitmap[4];
    U32 carrierFrequency;
    U32 readVal = 0;
    U32 bandWidth = 0;
    U32 addr = 0;
    U32 carrOffset = 64;
    U32 portOffset = 1;

    if(getHwDbCompStatus ==  false)
    {
        delay_db = default_delay_db;
        phase_db = default_phase_db;
        amplit_db = default_amplit_db;
        getHwDbCompStatus = true;
    }

    if (acWindowFlag == 0)
    {
        acWindowFlag = 1;

        TPT_INFO(STR("get fault mgr mbox."));
        app_fault_mgr_mbox= 0;
        if(!get_mbox(APP_FAULT_MGR_MBOX, &app_fault_mgr_mbox))
        {
            TPT_ERROR(STR("APP fault mgr Mailbox is not ready."));
            return;
        }
        TPT_INFO(STR("got fault mgr mbox."));

        /*
         * #define MM_B_TDD_CTRL_AC_CFG_DL_AC_STRB_OFFSET   (uint32_t)0x00000c39
         * #define MM_B_TDD_CTRL_AC_CFG_UL_AC_CTRL_OFFSET   (uint32_t)0x00000c3a
         * hwyFpga w 0xc39 0x04090000
         * hwyFpga w 0xc3a 0x7fb60000
        */
        fpga->write(MM_B_TDD_CTRL_AC_CFG_DL_AC_STRB_OFFSET, (U32)0x04090000);
        fpga->write(MM_B_TDD_CTRL_AC_CFG_UL_AC_CTRL_OFFSET, (U32)0x7fb60000);

        //fix the first port0 UL interrupt issue after power on.
        //#define MM_B_AC_PS_INT_DELAY_TIME (uint32_t)0x00002025
        fpga->write(MM_B_AC_PS_INT_DELAY_TIME, (U32)0x0000000F);

        /*first time enable AC, set C0/C1/C2 gain to default value 0x4000*/
        for (U16 k = 0; k < ULCELLPE_AC_MAX_ANT_PER_CELL; k++)
        {
            addr = MM_B_DL_AC_ADJUST_GAIN_C0_ANT_0+carrOffset*(0)+portOffset*k;
            fpga->write(addr, (U32)0x4000);
            addr = MM_B_UL_AC_ADJUST_GAIN_C0_ANT_0+carrOffset*(0)+portOffset*k;
            fpga->write(addr, (U32)0x4000);
            addr = MM_B_DL_AC_ADJUST_GAIN_C0_ANT_0+carrOffset*(1)+portOffset*k;
            fpga->write(addr, (U32)0x4000);
            addr = MM_B_UL_AC_ADJUST_GAIN_C0_ANT_0+carrOffset*(1)+portOffset*k;
            fpga->write(addr, (U32)0x4000);
            addr = MM_B_DL_AC_ADJUST_GAIN_C0_ANT_0+carrOffset*(2)+portOffset*k;
            fpga->write(addr, (U32)0x4000);
            addr = MM_B_UL_AC_ADJUST_GAIN_C0_ANT_0+carrOffset*(2)+portOffset*k;
            fpga->write(addr, (U32)0x4000);
        }
    }

    carrierId = rec_p->carrierEnable.carrierId;
    deviceId = rec_p->carrierEnable.deviceId;
    intervalPeriod = rec_p->carrierEnable.acPeriod * 60;/*Change unit from min to sec*/

    if( intervalPeriod == 0)
    {
        //when ac period is 0, don't start AC, and disable all ac function.
        TPT_INFO(STR("AcDispatcher Disable AC when carrier %d ac period is 0.", 
                carrierId));
        //set to test mode, disable add AC adjust parameter in FPGA.
        fpga->write(MM_B_DL_AC_ADJUST_CTRL, (U32)0x02);
        fpga->write(MM_B_UL_AC_ADJUST_CTRL, (U32)0x02);
        return;
    }
    else
    {
        //when ac period is not zero, Enable AC funciton for all carrier.
        fpga->read(MM_B_DL_AC_ADJUST_CTRL, readVal);
        if(readVal & MM_B_DL_AC_ADJUST_CTRL_TEST_MODE)
        {
            //changed DL mode from the test mode to AC normal mode
            fpga->write(MM_B_DL_AC_ADJUST_CTRL, (U32)0x00);
        }
        fpga->read(MM_B_UL_AC_ADJUST_CTRL, readVal);
        if(readVal & MM_B_UL_AC_ADJUST_CTRL_TEST_MODE)
        {
            //changed UL mode from the test mode to AC normal mode
            fpga->write(MM_B_UL_AC_ADJUST_CTRL, (U32)0x00);
        }

        // Set MM_B_AC_TRANSFER_STATUS 0x2033 value to 
        // MM_B_AC_TRANSFER_STATUS_CLR 0x2034

        fpga->read(MM_B_AC_TRANSFER_STATUS, readVal);
        fpga->write(MM_B_AC_TRANSFER_STATUS_CLR, readVal);
        usleep(1000);
        fpga->read(MM_B_AC_TRANSFER_STATUS, readVal);
        if (readVal == 0)
        {
            fpga->write(MM_B_AC_TRANSFER_STATUS_CLR, (U32)0x00);
        }
        
    }

    antBitmap[0] = rec_p->carrierEnable.usedAntenna0_31;
    antBitmap[1] = rec_p->carrierEnable.usedAntenna32_63;
    antBitmap[2] = rec_p->carrierEnable.usedAntenna64_95;
    antBitmap[3] = rec_p->carrierEnable.usedAntenna96_127;
    carrierFrequency = rec_p->carrierEnable.carrierFrequency;
    bandWidth = rec_p->carrierEnable.bandWidth;

    TPT_INFO(STR("------AcDispatcher handleAcCarrierEnable carrierId %d ----", 
                carrierId));

    if(addAcSession(carrierId, deviceId, carrierFrequency, antBitmap, bandWidth))
    {
        startAcSession(carrierId);
    }
    TPT_INFO(STR("------AcDispatcher handleAcCarrierEnable done-------------"));
    return;
}

void AcDispatcher::handleAcCarrierPending(union itc_msg * rec_p)
{
    U16 carrierId;
    carrierId = rec_p->carrierPending.carrierId;
    setAcSessionStatus(carrierId, ULCELLPE_ACSES_PENDING);
}

void AcDispatcher::handleAcCarrierDisable(union itc_msg * rec_p)
{
    U16 carrierId;
    carrierId = rec_p->carrierDisable.carrierId;
    TPT_INFO(STR("------AcDispatcher handleAcCarrierDisable carrierId %d.------", carrierId));
    stopAcSession(carrierId);
    usleep(10000);
    deleteAcSession(carrierId);
    TPT_INFO(STR("------AcDispatcher handleAcCarrierDisable Done --------------"));
}

void AcDispatcher::handleTxAcDone(union itc_msg * rec_p)
{
    U16 carrierId;
    U16 portId;

    carrierId = rec_p->txAcDone.carrierId;
    portId = rec_p->txAcDone.portId;
    TPT_INFO(STR("--------AcDispatcher TX_DONE_IND carrier%d, port%d:-----------",
                carrierId, portId));

    std::list<ULCELLPE_ACSES_classS *>::iterator it;

    for (it = acSesList.begin(); it != acSesList.end(); ++it)
    {
        if((*it)->carrierId == carrierId)
        {
            //TRACE(2, STR("AcDispatcher: find AC session carrierId %d", carrierId));
            break;
        }
    }
    if(it == acSesList.end())
    {
        TPT_INFO(STR("AcDispatcher: Can't find AC session carrierId %d", carrierId));
        return;
    }
    ULCELLPE_ACSES_classS * acSes_p = *it;
    if( !(acSes_p->acPortInfo[0][portId].carrActive && acSes_p->acPortInfo[1][portId].carrActive) )
    {
        TPT_INFO(STR("AcDispatcher: carrierId %d port %d is not activated.", carrierId, portId));
        return;
    }
    if( acSes_p->state != ULCELLPE_ACSES_PROCESS)
    {
        TPT_INFO(STR("AcDispatcher: carrierId %d AC status not in PROCESS, Reject TX AC data.", 
            carrierId));
        return;
    }
    U16 receiveTransmit = 1;

    //get the freq domain data from DDR which FPGA wrote the ac back data.    
    switch(acFreqDataSrc)
    {
        case AC_FREQ_DATA_SUCCESS:
            {
                memcpy(acSes_p->acFreqDomainData, complexRefSignal20Mhz, 
                                   4*SIZE_OF_AC_FREQ_DOMAIN_FFT_SIZE);
                break;
            }
        case AC_FREQ_DATA_FAILURE_DELAY:
            {
                memcpy(acSes_p->acFreqDomainData, complexSignal20MhzErrDelay, 
                                   4*SIZE_OF_AC_FREQ_DOMAIN_FFT_SIZE);
                TPT_INFO(STR("Warning! This is a Delay error AC data test"));
                break;
            }
        case AC_FREQ_DATA_FAILURE_DISTURB:
            {
                memcpy(acSes_p->acFreqDomainData, complexSignal20MhzErrDistb, 
                                   4*SIZE_OF_AC_FREQ_DOMAIN_FFT_SIZE);
                TPT_INFO(STR("Warning! This is a Disturbance error AC data test"));
                break;
            }
        case AC_FREQ_DATA_FAILURE_SIGLOW:
            {
                memcpy(acSes_p->acFreqDomainData, complexSignal20MhzErrSigLow, 
                                   4*SIZE_OF_AC_FREQ_DOMAIN_FFT_SIZE);
                TPT_INFO(STR("Warning! This is a SignalTooLow error AC data test"));
                break;
            }
        case AC_FREQ_DATA_FROM_FPGA:
            {
                //get ac freq data from ac_mem in DTS, need to FPGA support.
                freqDataMem->getFreqDataFromAcMem((S16 *)(acSes_p->acFreqDomainData), 
                             SIZE_OF_AC_FREQ_DOMAIN_FFT_SIZE*2, 0);
                TPT_INFO(STR("AC Memory acFreqDomainData: byte0 %d, byte1 %d, byte2 %d, byte3 %d.", 
                             acSes_p->acFreqDomainData[0].re, acSes_p->acFreqDomainData[0].im, 
                             acSes_p->acFreqDomainData[1].re, acSes_p->acFreqDomainData[1].im));
                break;
            }
        default:
            //TRACE_ERROR_SHELL(STR("Wrong AC Freq Data source."));
            return;
    }

    S16 noOfSc = 1200;
    //output
    S16 delay = 0;
    S16 initPhase = 0;
    S32 amplitude = 0;
    S16 channelSumCentidB = 0;
    U32 disturbance = 0;
    S16 noShift = 0;

    U32 fftExp = 0;
    fpga->read(MM_B_AC_DL_FFT_EXP, fftExp);

    TPT_INFO(STR("AcDispatcher algorithm process Carrier%d DL Port%d:", carrierId, portId));
    /*call antennaCalibration algorithm*/
    antennaCalibration((S32 *)acSes_p->acFreqDomainData, noOfSc, 
                &delay, &initPhase, &amplitude, &channelSumCentidB, &disturbance, &noShift, 
                (U16)fftExp);

    ULCELLPE_ACSES_calibFactorS acFactor;
    acFactor.delay = delay;
    acFactor.initPhase = initPhase;
    acFactor.amplitude = amplitude;
    acFactor.channelSumCentidB = channelSumCentidB;
    acFactor.disturbance = disturbance;
    acFactor.noShift = noShift;

    updateAcFactor(carrierId, portId, receiveTransmit, acFactor);
    /*
    pAcLogFile->storeAcData(carrierId, portId, receiveTransmit, 
        (S16 *)acSes_p->acFreqDomainData, 2*SIZE_OF_AC_FREQ_DOMAIN_FFT_SIZE);
    */

    if (checkAcSessionDone(acSes_p))
    {
        TPT_INFO(STR("AcDispatcher Carrier%d All Port DL&UL AC Done.", carrierId));
        acSes_p->state = ULCELLPE_ACSES_REPORT;
    }
    if (ULCELLPE_ACSES_REPORT == acSes_p->state)
    {
        detectDelayFault(acSes_p);
        if(checkFaultStatus(acSes_p))
        {
            if(acSes_p->faultRetryCounter < MAXNUM_OF_AC_FAULT_RETRY)
            {
                //retry ac for this carrier all port.
                acSes_p->faultRetryCounter++;
                TPT_INFO(STR("Retry AC for CarrierId %d Times %d.", 
                     carrierId, acSes_p->faultRetryCounter));
                retryAc(acSes_p);
                return;
            }
            //should report Fault
            TPT_INFO(STR("AC Fault Raised after %d times Retry for CarrierId %d.", 
                     acSes_p->faultRetryCounter, carrierId));
        }
        reportAcFactor(acSes_p);
        if(acSes_p->acType == ULCELLPE_ACSES_ACTYPE_PRE)
        {
            TPT_INFO(STR("AC Type: Pre-AC."));
            //Send delay to TRXM Radio APP for 64 ant, TX/RX.
            //sendAcFactorToRadio(carrierId, portId, receiveTransmit, acFactor);
            sendAcFactorToRadio(acSes_p, 0);
            acSes_p->acType = ULCELLPE_ACSES_ACTYPE_PERIOD;
        }
        else
        {
            TPT_INFO(STR("AC Type: Period AC."));
            TPT_INFO(STR("Set AC Compensation into BB FPGA."));
            setAcFactorToFPGA(acSes_p);
        }
        storeAcFreqData(acSes_p);
    }
    TPT_INFO(STR("--------AcDispatcher TX_DONE_IND finished.--------------------"));
}

void AcDispatcher::handleRxAcDone(union itc_msg * rec_p)
{
    U16 carrierId;
    U16 portId;

    carrierId = rec_p->rxAcDone.carrierId;
    portId = rec_p->txAcDone.portId;
    TPT_INFO(STR("--------AcDispatcher RX_DONE_IND carrier%d, port%d:----------", 
                carrierId, portId));

    std::list<ULCELLPE_ACSES_classS *>::iterator it;

    for (it = acSesList.begin(); it != acSesList.end(); ++it)
    {
        if((*it)->carrierId == carrierId)
        {
            //TRACE(2, STR("AcDispatcher: find AC session carrierId %d", carrierId));
            break;
        }
    }
    if(it == acSesList.end())
    {
        TPT_INFO(STR("AcDispatcher: Can't find AC session carrierId %d", carrierId));
        return;
    }
    ULCELLPE_ACSES_classS * acSes_p = *it;
    if( !(acSes_p->acPortInfo[0][portId].carrActive && acSes_p->acPortInfo[1][portId].carrActive) )
    {
        TPT_INFO(STR("AcDispatcher: carrierId %d port %d is not activated.", carrierId, portId));
        return;
    }
    if( acSes_p->state != ULCELLPE_ACSES_PROCESS)
    {
        TPT_INFO(STR("AcDispatcher: carrierId %d AC status not in PROCESS, Reject RX AC data.", 
            carrierId));
        return;
    }

    //algorithm output para
    S16 delay = 0;
    S16 initPhase = 0;
    S32 amplitude = 0;
    S16 channelSumCentidB = 0;
    U32 disturbance = 0;
    S16 noShift = 0;
    S16 noOfSc = 1200;

    ULCELLPE_ACSES_calibFactorS acFactor;
    U16 receiveTransmit = 0;
    U16 i = 0;
		
    //get the freq domain data from DDR which FPGA wrote the ac back data.    
    switch(acFreqDataSrc)
    {
        case AC_FREQ_DATA_SUCCESS:
        {
            memcpy(acSes_p->acFreqDomainData, complexRefSignal20Mhz, 
                               4*SIZE_OF_AC_FREQ_DOMAIN_FFT_SIZE);
            break;
        }
        case AC_FREQ_DATA_FAILURE_DELAY:
        {
            memcpy(acSes_p->acFreqDomainData, complexSignal20MhzErrDelay, 
                          4*SIZE_OF_AC_FREQ_DOMAIN_FFT_SIZE);
            TPT_INFO(STR("Warning! This is a Delay error AC data test"));
             break;
        }
        case AC_FREQ_DATA_FAILURE_DISTURB:
        {
            memcpy(acSes_p->acFreqDomainData, complexSignal20MhzErrDistb, 
                          4*SIZE_OF_AC_FREQ_DOMAIN_FFT_SIZE);
            TPT_INFO(STR("Warning! This is a Disturbance error AC data test"));
            break;
        }
        case AC_FREQ_DATA_FAILURE_SIGLOW:
        {
            memcpy(acSes_p->acFreqDomainData, complexSignal20MhzErrSigLow, 
                          4*SIZE_OF_AC_FREQ_DOMAIN_FFT_SIZE);
            TPT_INFO(STR("Warning! This is a SignalTooLow error AC data test"));
            break;
        }
        case AC_FREQ_DATA_FROM_FPGA:
        {
            //get ac freq data from ac_mem in DTS, need to FPGA support.
            freqDataMem->getFreqDataFromAcMem((S16 *)(acSes_p->acFreqDomainData), 
                             SIZE_OF_AC_FREQ_DOMAIN_FFT_SIZE*2, 0);
            TPT_INFO(STR("AC Memory acFreqDomainData: byte0 %d, byte1 %d, byte2 %d, byte3 %d.", 
                             acSes_p->acFreqDomainData[0].re, acSes_p->acFreqDomainData[0].im, 
                             acSes_p->acFreqDomainData[1].re, acSes_p->acFreqDomainData[1].im));
            break;
        }
        default:
             //TRACE_ERROR_SHELL(STR("Wrong AC Freq Data source."));
            return;
    }
    TPT_INFO(STR("AcDispatcher algorithm process Carrier%d UL Port%d:", 
               carrierId, i));

    U32 fftExp = 0;
    fpga->read(MM_B_AC_DL_FFT_EXP, fftExp);

    /*call antennaCalibration algorithm*/
    antennaCalibration((S32 *)acSes_p->acFreqDomainData, noOfSc, 
               &delay, &initPhase, &amplitude, &channelSumCentidB, &disturbance, &noShift, 
               (U16)fftExp);

    acFactor.delay = delay;
    acFactor.initPhase = initPhase;
    acFactor.amplitude = amplitude;
    acFactor.channelSumCentidB = channelSumCentidB;
    acFactor.disturbance = disturbance;
    acFactor.noShift = noShift;

    updateAcFactor(carrierId, portId, receiveTransmit, acFactor);
    /*
    pAcLogFile->storeAcData(carrierId, portId, receiveTransmit, 
        (S16 *)acSes_p->acFreqDomainData, 2*SIZE_OF_AC_FREQ_DOMAIN_FFT_SIZE);
    */

    if (checkAcSessionDone(acSes_p))
    {
        TPT_INFO(STR("AcDispatcher Carrier%d All Port DL&UL AC Done.", carrierId));
        acSes_p->state = ULCELLPE_ACSES_REPORT;
    }
     if (ULCELLPE_ACSES_REPORT == acSes_p->state)
    {
        detectDelayFault(acSes_p);
        if(checkFaultStatus(acSes_p))
        {
            if(acSes_p->faultRetryCounter < MAXNUM_OF_AC_FAULT_RETRY)
            {
                //retry ac for this carrier all port.
                acSes_p->faultRetryCounter++;
                TPT_INFO(STR("Retry AC for CarrierId %d Times %d.", 
                     carrierId, acSes_p->faultRetryCounter));
                retryAc(acSes_p);
                return;
            }
            //should report Fault
            TPT_INFO(STR("AC Fault Raised after %d times Retry for CarrierId %d.", 
                     acSes_p->faultRetryCounter, carrierId));
        }
        reportAcFactor(acSes_p);
        if(acSes_p->acType == ULCELLPE_ACSES_ACTYPE_PRE)
        {
            TPT_INFO(STR("AC Type: Pre-AC."));
            //Send delay to TRXM Radio APP for 64 ant, TX/RX.
            //sendAcFactorToRadio(carrierId, portId, receiveTransmit, acFactor);
            sendAcFactorToRadio(acSes_p, 0);
            acSes_p->acType = ULCELLPE_ACSES_ACTYPE_PERIOD;
        }
        else
        {
            TPT_INFO(STR("AC Type: Period AC."));
            TPT_INFO(STR("Set AC Compensation into BB FPGA."));
            setAcFactorToFPGA(acSes_p);
        }
        storeAcFreqData(acSes_p);
    }
    TPT_INFO(STR("--------AcDispatcher RX_DONE_IND finished.--------------------"));
}

void AcDispatcher::handleTimeOutInd(union itc_msg * rec_p)
{
    U32 msgId = (rec_p->tmoTimeoutInd.index & 0xF0000000)>>28;
    switch(msgId)
    {
        case ULCELLPE_ACSES_PERIOD_AC:
            {
                handlePeriodAc(rec_p);
                break;
            }
        case ULCELLPE_ACSES_PRE_AC_MSG:
            {
                handlePreAcMsgTimeOut(rec_p);
                break;
            }
        case ULCELLPE_ACSES_SET_PRACH_MSG:
            {
                handleSetPrachMsgTimeOut(rec_p);
                break;
            }
        case ULCELLPE_ACSES_RESET_DELAY_REG_MSG:
            {
                handleResetDelayRegMsgTimeOut(rec_p);
                break;
            }
        default:
            TPT_INFO(STR("Wrong TimeOut messageId %d in AC Control Process.", msgId));
            break;
    }
}

void AcDispatcher::handlePeriodAc(union itc_msg * rec_p)
{
    //U16 carrierId = rec_p->sigNo;
    U16 carrierId = rec_p->tmoTimeoutInd.index;
    uint32_t ret = CREATE_TIMER_FAIL;
    std::list<ULCELLPE_ACSES_classS *>::iterator it;
    time_t t;
    char timeStr[32]; 
    time(&t);
    strftime(timeStr, sizeof(timeStr), "%T", localtime(&t));  
    //printf("%s time's up. index = %d.\n", timeStr, inSig->tmoTimeoutInd.index); 

    for (it = acSesList.begin(); it != acSesList.end(); ++it)
    {
        if((*it)->carrierId == carrierId)
        {
            //TPT_INFO(STR("AcDispatcher: AC session run for carrierId %d", carrierId));
            break;
        }
    }
    if(it == acSesList.end())
    {
        TPT_INFO(STR("PeriodAC Can't find AC session carrierId %d.", carrierId));
        return;
    }

    switch((*it)->state)
    {
        case ULCELLPE_ACSES_SETUP:
            {
                setAcSessionStatus((*it)->carrierId, ULCELLPE_ACSES_IDLE);

                // Reset the delay compensation register on TRXM DUC/DDC
                sendResetDelayRegToRadio(*it, 0);
                
                //(*it)->state = ULCELLPE_ACSES_PROCESS;
                if(startUpDelay)
                {
                   //fRequestTmo((*it)->tmo_subCancel, startUpDelay, acControlProc_, rec_p->sigNo);
                   //seconds , carrierId, client
                   (*it)->acTimer = createTimer(startUpDelay*1000, rec_p->tmoTimeoutInd.index, ac_app_mbox);
                   if((*it)->acTimer == CREATE_TIMER_FAIL)
                   {
                       TPT_INFO(STR("Create carrier %d timer fail. \n",rec_p->tmoTimeoutInd.index));
                   }
                   (*it)->tmo_requested= true;
                   TPT_INFO(STR("%s: PeriodAC start up delay for carrierId %d.", 
                               timeStr, carrierId));
                   break;
                }
            }
        case ULCELLPE_ACSES_IDLE:
        case ULCELLPE_ACSES_REPORT:
            {
                TPT_INFO(STR("------ %s: PeriodAC run for carrierId %d interval %d s------",
                            timeStr, carrierId, intervalPeriod));
                if((*it)->state == ULCELLPE_ACSES_REPORT)
                {
                    restoreAcSession(*it);
                }

                // Check any carrier is in ULCELLPE_ACSES_PROCESS status at this time.
                if (checkAcOnProgress() == true)
                {
                    // If wait times more than 3, then start the AC directly
                    if (((*it)->waitAcDoneRetryCounter) < MAXNUM_OF_WAIT_AC_DONE_RETRY)    
                    {
                        (*it)->waitAcDoneRetryCounter++;
                        
                        ret = cancelTimer((*it)->acTimer);
                        if(ret != REMOVE_TIMER_SUCCESS)
                        {
                            TPT_INFO(STR("Removed carrier %d timer fail. \n", rec_p->tmoTimeoutInd.index));
                        }
                        
                        // Wait 10 seconds to let the current AC processing can be finished.
                        (*it)->acTimer = createTimer(waitACDoneDelay*1000, rec_p->tmoTimeoutInd.index, ac_app_mbox);
                        if((*it)->acTimer == CREATE_TIMER_FAIL)
                        {
                            TPT_INFO(STR("startAcSession: Create carrier %d timer fail. \n",carrierId));
                        }
                        (*it)->tmo_requested= true;
                        TPT_INFO(STR("startAcSession wait other AC done delay for carrierId %d.", carrierId));
                        return;                    
                    }
         
                }
                else
                {
                    (*it)->waitAcDoneRetryCounter = 0;
                }                
                
                sendAcParaToFpga((*it)->carrierId);
                setAcSessionStatus((*it)->carrierId, ULCELLPE_ACSES_PROCESS);
                //1. then wait for FPGA interrupt for ac done per antenna and carrier.
                //2. after wait done, wait for next timer out, next AC loop.
                //fRequestTmo((*it)->tmo_subCancel, intervalPeriod, acControlProc_, rec_p->sigNo);
                ret = cancelTimer((*it)->acTimer);
                if(ret != REMOVE_TIMER_SUCCESS)
                {
                    TPT_INFO(STR("Removed carrier %d timer fail. \n", rec_p->tmoTimeoutInd.index));
                }
                // Execute the first round periodic AC 1 minute after pre-AC, and for the later rounds, 
                // SC524 value will be used as interval (minute)
                if((*it)->acType == ULCELLPE_ACSES_ACTYPE_PRE)
                {
                    (*it)->acTimer = createTimer(startFirstPerAcDelay*1000, rec_p->tmoTimeoutInd.index, ac_app_mbox);
                }
                else 
                {
                    (*it)->acTimer = createTimer(intervalPeriod*1000, rec_p->tmoTimeoutInd.index, ac_app_mbox);
                }
                
                if((*it)->acTimer == CREATE_TIMER_FAIL)
                {
                    TPT_INFO(STR("Create carrier %d timer fail. \n",rec_p->tmoTimeoutInd.index));
                }
                (*it)->tmo_requested = true;
                TPT_INFO(STR("------PeriodAC run for carrierId %d finished-----------", 
                            carrierId));
                break;
            }
        case ULCELLPE_ACSES_PROCESS:
            {
                TPT_INFO(STR("%s: The previous carrier%d AC isn't finished, ignore this request.",
                            timeStr, carrierId));
                //fRequestTmo((*it)->tmo_subCancel, intervalPeriod, acControlProc_, rec_p->sigNo);
                (*it)->tmo_requested = true;
                break;
            }
        default:
            //TRACE_ERROR_SHELL(STR("Wrong AC Status %d in AC Control Process.", (*it)->state));
            return;
    }

    return;
}

void AcDispatcher::handlePreAcMsgTimeOut(union itc_msg * rec_p)
{
    /*timeOutMsg Content: 0xF_F_FF_FFFF indicates contentID_Direct_PortId_CarrierId*/
    //U32 msgId = (rec_p->tmoTimeoutInd.index & 0xF0000000)>>28;
    U32 direct = (rec_p->tmoTimeoutInd.index & 0x0F000000)>>24;
    U32 portId = (rec_p->tmoTimeoutInd.index & 0x00FF0000)>>16;
    U32 carrierId = (rec_p->tmoTimeoutInd.index & 0x0000FFFF);
    uint32_t ret = CREATE_TIMER_FAIL;
    TPT_INFO(STR("Recv Pre-AC msg time out, carrier %d, port %d, direct %d.",
                    carrierId, portId, direct));
				
    std::list<ULCELLPE_ACSES_classS *>::iterator it;

    for (it = acSesList.begin(); it != acSesList.end(); ++it)
    {
        if((*it)->carrierId == carrierId)
        {
            //TRACE(2, STR("AcDispatcher: find AC session carrierId %d", carrierId));
            break;
        }
    }
    if(it == acSesList.end())
    {
        TPT_INFO(STR("AcDispatcher: Can't find AC session carrierId %d", carrierId));
        return;
    }
    ULCELLPE_ACSES_classS * acSes_p = *it;

    ret = cancelTimer(acSes_p->preAcTimer);
    if(ret != REMOVE_TIMER_SUCCESS)
    {
       TPT_INFO(STR("Removed carrier %d Pre-AC timer fail. \n", carrierId));
    }

    if(acSes_p->acPortInfo[direct][portId].preAcReqSendFlag)
    {
        acSes_p->acPortInfo[direct][portId].preAcRespRecvFlag = ULCELLPE_ACSES_PRE_AC_TIMEOUT;
        sendAcFactorToRadio(acSes_p, portId);
    }
    else
    {
        TPT_INFO(STR("Pre-AC msg timer out errors for carrier %d, portId %d.", 
                        carrierId, portId));
    }
}

void AcDispatcher::handleResetDelayRegMsgTimeOut(union itc_msg * rec_p)
{
    /*timeOutMsg Content: 0xF_F_FF_FFFF indicates contentID_Direct_PortId_CarrierId*/
    //U32 msgId = (rec_p->tmoTimeoutInd.index & 0xF0000000)>>28;
    U32 direct = (rec_p->tmoTimeoutInd.index & 0x0F000000)>>24;
    U32 portId = (rec_p->tmoTimeoutInd.index & 0x00FF0000)>>16;
    U32 carrierId = (rec_p->tmoTimeoutInd.index & 0x0000FFFF);
    uint32_t ret = CREATE_TIMER_FAIL;
    TPT_INFO(STR("Recv reset trxm delay register msg time out, carrier %d, port %d, direct %d.",
                    carrierId, portId, direct));

    std::list<ULCELLPE_ACSES_classS *>::iterator it;

    for (it = acSesList.begin(); it != acSesList.end(); ++it)
    {
        if((*it)->carrierId == carrierId)
        {
            break;
        }
    }
    if(it == acSesList.end())
    {
        TPT_INFO(STR("AcDispatcher: Can't find AC session carrierId %d", carrierId));
        return;
    }
    ULCELLPE_ACSES_classS * acSes_p = *it;

    ret = cancelTimer(acSes_p->resetDelayRegTimer);
    acSes_p->resetDelayRegTimer = 0;
    if(ret != REMOVE_TIMER_SUCCESS)
    {
       TPT_INFO(STR("Removed carrier %d Reset TRXM delay register  timer fail. \n", carrierId));
    }

    if(acSes_p->acPortInfo[direct][portId].resetDelayRegReqSendFlag)
    {
        acSes_p->acPortInfo[direct][portId].resetDelayRegReqSendFlag = ULCELLPE_ACSES_RESET_DELAY_REG_TIMEOUT;
        sendResetDelayRegToRadio(acSes_p, portId);
    }
    else
    {
        TPT_INFO(STR("Reset TRXM delay register msg timer out errors for carrier %d, portId %d.", 
                        carrierId, portId));
    }
}


ULCELLPE_ACSES_classS* AcDispatcher::getAcSession(U32 freq)
{
    std::list<ULCELLPE_ACSES_classS *>::iterator it;

    for (it = acSesList.begin(); it != acSesList.end(); ++it)
    {
        if((*it)->carrierFrequency == freq)
        {
            return (ULCELLPE_ACSES_classS*)*it;
        }
    }
    if(it == acSesList.end())
    {
        TPT_INFO(STR("AcDispatcher: Can't find AC session carrier %d", freq));
        return NULL;
    }
}

void AcDispatcher::handleRespCfm(union itc_msg * rec_p)
{
    ULCELLPE_ACSES_classS * acSes_p = getAcSession(rec_p->dcTrModifyCarrierDelayCfm.addressInfo.serverRef);
    if(acSes_p != NULL) 
    {
        if(acSes_p->acType == ULCELLPE_ACSES_ACTYPE_PRE)
        {
            if (acSes_p->state == ULCELLPE_ACSES_IDLE)
            {
                handleResetDelayRegMsgCfm(acSes_p, rec_p->dcTrModifyCarrierDelayCfm.deviceId, rec_p->dcTrModifyCarrierDelayCfm.addressInfo.clientRef);
            }
            else 
            {
                handlePreAcMsgCfm(acSes_p, rec_p->dcTrModifyCarrierDelayCfm.deviceId, rec_p->dcTrModifyCarrierDelayCfm.addressInfo.clientRef);
            }        
        }
        else if(acSes_p->acType == ULCELLPE_ACSES_ACTYPE_PERIOD)
        {
            handleSetPrachMsgCfm(acSes_p, rec_p->dcTrModifyCarrierDelayCfm.deviceId);
        }
    }  
}

void AcDispatcher::handleRespReject(union itc_msg * rec_p)
{
    ULCELLPE_ACSES_classS * acSes_p = getAcSession(rec_p->dcTrModifyCarrierDelayRej.addressInfo.serverRef);
    if(acSes_p != NULL)
    {
        if(acSes_p->acType == ULCELLPE_ACSES_ACTYPE_PRE)
        {
            if (acSes_p->state == ULCELLPE_ACSES_IDLE)
            {
                handleResetDelayRegMsgReject(acSes_p, rec_p->dcTrModifyCarrierDelayRej.deviceId, rec_p->dcTrModifyCarrierDelayRej.addressInfo.clientRef);
            }
            else 
            {
                handlePreAcMsgReject(acSes_p, rec_p->dcTrModifyCarrierDelayRej.deviceId, rec_p->dcTrModifyCarrierDelayRej.addressInfo.clientRef);
            }    
        }
        else if(acSes_p->acType == ULCELLPE_ACSES_ACTYPE_PERIOD)
        {
            handleSetPrachMsgReject(acSes_p, rec_p->dcTrModifyCarrierDelayRej.deviceId);
        }
    }
}

void AcDispatcher::handleResetDelayRegMsgCfm(ULCELLPE_ACSES_classS * acSes_p, U16 portId, U16 directReverse)
{
    uint32_t ret = CREATE_TIMER_FAIL;

    U16 direct = 0;
    if (directReverse == 0)
    {
        //RX
        direct = 1;
    }
    else
    {
        //TX
        direct = 0;
    }
    TPT_INFO(STR("Recv reset trxm delay register CFM from Radio, carrier %07d, port %d, direct %d, directReverse %d.",
                    acSes_p->carrierFrequency, portId, direct, directReverse));

    ret = cancelTimer(acSes_p->resetDelayRegTimer);
    acSes_p->resetDelayRegTimer = 0;
    if(ret != REMOVE_TIMER_SUCCESS)
    {
       TPT_INFO(STR("Removed carrier %d reset trxm delay reg timer fail. \n", acSes_p->carrierFrequency));
    }
    if(acSes_p->acPortInfo[direct][portId].resetDelayRegReqSendFlag)
    {
        acSes_p->acPortInfo[direct][portId].resetDelayRegRespRecvFlag = ULCELLPE_ACSES_RESET_DELAY_REG_CFM;
        sendResetDelayRegToRadio(acSes_p, portId);
    }
    else
    {
        TPT_INFO(STR("Reset trxm delay register msg CFM mismatch for carrier %07d, portId %d.", 
                        acSes_p->carrierFrequency, portId));
    }
}


void AcDispatcher::handlePreAcMsgCfm(ULCELLPE_ACSES_classS * acSes_p, U16 portId, U16 directReverse)
{
    uint32_t ret = CREATE_TIMER_FAIL;

    U16 direct = 0;
    if (directReverse == 0)
    {
        //RX
        direct = 1;
    }
    else
    {
        //TX
        direct = 0;
    }
    TPT_INFO(STR("Recv Pre-AC msg CFM from Radio, carrier %07d, port %d, direct %d, directReverse %d.",
                    acSes_p->carrierFrequency, portId, direct, directReverse));

    ret = cancelTimer(acSes_p->preAcTimer);
    if(ret != REMOVE_TIMER_SUCCESS)
    {
       TPT_INFO(STR("Removed carrier %d Pre-AC timer fail. \n", acSes_p->carrierFrequency));
    }
    if(acSes_p->acPortInfo[direct][portId].preAcReqSendFlag)
    {
        acSes_p->acPortInfo[direct][portId].preAcRespRecvFlag = ULCELLPE_ACSES_PRE_AC_CFM;
        sendAcFactorToRadio(acSes_p, portId);
    }
    else
    {
        TPT_INFO(STR("Pre-AC msg CFM mismatch for carrier %07d, portId %d.", 
                        acSes_p->carrierFrequency, portId));
    }
}

void AcDispatcher::handlePreAcMsgReject(ULCELLPE_ACSES_classS * acSes_p, U16 portId, U16 directReverse)
{
    uint32_t ret = CREATE_TIMER_FAIL;

    U16 direct = 0;
    if (directReverse == 0)
    {
        //RX
        direct = 1;
    }
    else
    {
        //TX
        direct = 0;
    }
    TPT_INFO(STR("Recv Pre-AC msg REJECT from Radio, carrier %07d, port %d, direct %d, directReverse %d.",
                    acSes_p->carrierFrequency, portId, direct, directReverse));

    ret = cancelTimer(acSes_p->preAcTimer);
    if(ret != REMOVE_TIMER_SUCCESS)
    {
       TPT_INFO(STR("Removed carrier %d Pre-AC timer fail. \n", acSes_p->carrierFrequency));
    }
    if(acSes_p->acPortInfo[direct][portId].preAcReqSendFlag)
    {
        acSes_p->acPortInfo[direct][portId].preAcRespRecvFlag = ULCELLPE_ACSES_PRE_AC_REJ;
        sendAcFactorToRadio(acSes_p, portId);
    }
    else
    {
        TPT_INFO(STR("Pre-AC msg REJECT mismatch for carrier %07d, portId %d.", 
                        acSes_p->carrierFrequency, portId));
    }
}


void AcDispatcher::handleResetDelayRegMsgReject(ULCELLPE_ACSES_classS * acSes_p, U16 portId, U16 directReverse)
{
    uint32_t ret = CREATE_TIMER_FAIL;

    U16 direct = 0;
    if (directReverse == 0)
    {
        //RX
        direct = 1;
    }
    else
    {
        //TX
        direct = 0;
    }
    TPT_INFO(STR("Recv reset trxm delay register msg REJECT from Radio, carrier %07d, port %d, direct %d, directReverse %d.",
                    acSes_p->carrierFrequency, portId, direct, directReverse));

    ret = cancelTimer(acSes_p->resetDelayRegTimer);
    acSes_p->resetDelayRegTimer = 0;
    if(ret != REMOVE_TIMER_SUCCESS)
    {
       TPT_INFO(STR("Removed carrier %d  reset trxm delay reg timer fail. \n", acSes_p->carrierFrequency));
    }
    if(acSes_p->acPortInfo[direct][portId].resetDelayRegReqSendFlag)
    {
        acSes_p->acPortInfo[direct][portId].resetDelayRegRespRecvFlag = ULCELLPE_ACSES_RESET_DELAY_REG_REJ;
        sendResetDelayRegToRadio(acSes_p, portId);
    }
    else
    {
        TPT_INFO(STR("Reset trxm delay register msg REJECT mismatch for carrier %07d, portId %d.", 
                        acSes_p->carrierFrequency, portId));
    }
}


void AcDispatcher::handleSetPrachMsgTimeOut(union itc_msg * rec_p)
{
    /*U32 timerContent: 0xFF_FF_FFFF indicates contentID_PortId_CarrierId*/

    U32 direct = (rec_p->tmoTimeoutInd.index & 0x0F000000)>>24;
    U32 portId = (rec_p->tmoTimeoutInd.index & 0x00FF0000)>>16;
    U32 carrierId = (rec_p->tmoTimeoutInd.index & 0x0000FFFF);
    uint32_t ret = CREATE_TIMER_FAIL;
    TPT_INFO(STR("Recv set prach msg time out, carrier %d, port %d, direct %d.",
                    carrierId, portId, direct));

    std::list<ULCELLPE_ACSES_classS *>::iterator it;

    for (it = acSesList.begin(); it != acSesList.end(); ++it)
    {
        if((*it)->carrierId == carrierId)
        {
            //TRACE(2, STR("AcDispatcher: find AC session carrierId %d", carrierId));
            break;
        }
    }
    if(it == acSesList.end())
    {
        TPT_INFO(STR("AcDispatcher: Can't find AC session carrierId %d", carrierId));
        return;
    }
    ULCELLPE_ACSES_classS * acSes_p = *it;

    ret = cancelTimer(acSes_p->setPrachTimer);
    if(ret != REMOVE_TIMER_SUCCESS)
    {
       TPT_INFO(STR("Removed carrier %d setPrachTimer timer fail. \n", carrierId));
    }

    if(acSes_p->acPortInfo[direct][portId].setPrachReqSendFlag)
    {
        acSes_p->acPortInfo[direct][portId].setPrachRespRecvFlag = ULCELLPE_ACSES_SET_PRACH_TIMEOUT;
        sendPrachToRadio(acSes_p, portId);
    }
    else
    {
        TPT_INFO(STR("Set prach msg timer out errors for carrier %d, portId %d.", 
                        carrierId, portId));
    }
}

void AcDispatcher::handleSetPrachMsgCfm(ULCELLPE_ACSES_classS* acSes_p, U16 portId)
{
    uint32_t ret = CREATE_TIMER_FAIL;

    TPT_INFO(STR("Recv set prach msg CFM from Radio, carrier %07d, port %d .",
                    acSes_p->carrierFrequency, portId));

    ret = cancelTimer(acSes_p->setPrachTimer);
    if(ret != REMOVE_TIMER_SUCCESS)
    {
       TPT_INFO(STR("Removed carrier %d set prach timer fail. \n", acSes_p->carrierFrequency));
    }
    if(acSes_p->acPortInfo[UPLINK][portId].setPrachReqSendFlag)
    {
        acSes_p->acPortInfo[UPLINK][portId].setPrachRespRecvFlag = ULCELLPE_ACSES_SET_PRACH_CFM;
        sendPrachToRadio(acSes_p, portId);
    }
    else
    {
        TPT_INFO(STR("set prach msg CFM mismatch for carrier %07d, portId %d.", 
                        acSes_p->carrierFrequency, portId));
    }
}

void AcDispatcher::handleSetPrachMsgReject(ULCELLPE_ACSES_classS* acSes_p, U16 portId)
{
    uint32_t ret = CREATE_TIMER_FAIL;

    TPT_INFO(STR("Recv set prach msg REJECT from Radio, carrier %07d, port %d, directReverse %d.",
                    acSes_p->carrierFrequency, portId));

    ret = cancelTimer(acSes_p->preAcTimer);
    if(ret != REMOVE_TIMER_SUCCESS)
    {
       TPT_INFO(STR("Removed carrier %d set prach timer fail. \n", acSes_p->carrierFrequency));
    }
    if(acSes_p->acPortInfo[UPLINK][portId].preAcReqSendFlag)
    {
        acSes_p->acPortInfo[UPLINK][portId].preAcRespRecvFlag = ULCELLPE_ACSES_SET_PRACH_REJ;
        sendPrachToRadio(acSes_p, portId);
    }
    else
    {
        TPT_INFO(STR("set prach msg REJECT mismatch for carrier %07d, portId %d.", 
                        acSes_p->carrierFrequency, portId));
    }
}

void AcDispatcher::handleAcFpgaAccess(union itc_msg * rec_p)
{
    U16 operFlag = rec_p->trAcFpgaAcce.operFlag;
    U32 addr = rec_p->trAcFpgaAcce.addr;
    U32 data = rec_p->trAcFpgaAcce.data;
    U32 dataRead = 0;
    if(operFlag == 1)
    {
        /*fpga read*/
        fpga->read(addr, dataRead);
        TPT_INFO(STR("AcDispatch FPGA read: addr 0x%08X, data 0x%08X.",
                        addr, dataRead));
        printf("FPGA read: addr 0x%08X, data 0x%08X.\n",addr, dataRead);
#if 0
        S32 mode = 0x0001;
        if (*(char *)&mode)
        {
            printf("memory low mode.\n");
        }
        else
        {
            printf("memory high mode.\n");
        }
        S16 * acBuffer = (S16 *)traffic_mmap_ac_base;
        printf("AC Memory C0A0: byte0 %d, byte1 %d, byte2 %d, byte3 %d.\n", 
                         *acBuffer, *(acBuffer+1), *(acBuffer+2), *(acBuffer+3));
        printf("AC Memory C0A0: byte2396 %d, byte2397 %d, byte2398 %d, byte2399 %d.\n", 
                         *(acBuffer+2396), *(acBuffer+2397), *(acBuffer+2398), *(acBuffer+2399));

        acBuffer = (S16 *)(traffic_mmap_ac_base + 0x2000);
        printf("AC Memory C0A1: byte0 %d, byte1 %d, byte2 %d, byte3 %d.\n", 
                         *acBuffer, *(acBuffer+1), *(acBuffer+2), *(acBuffer+3));
        printf("AC Memory C0A1: byte2396 %d, byte2397 %d, byte2398 %d, byte2399 %d.\n", 
                         *(acBuffer+2396), *(acBuffer+2397), *(acBuffer+2398), *(acBuffer+2399));
#endif
    }
    else if(operFlag == 2)
    {
        /*fpga write*/
        fpga->write(addr, data);
        TPT_INFO(STR("AcDispatch FPGA write: addr 0x%08X, data 0x%08X.",
                        addr, data));
        printf("FPGA write: addr 0x%08X, data 0x%08X.\n",addr, data);
    }
    else
    {
        TPT_INFO(STR("AcDispatch FPGA Operation error flag: %d.",
                        operFlag));
    }
}

void AcDispatcher::handleIntAcDoneInd(union itc_msg * rec_p)
{
    U32 direct = rec_p->intAcDoneInd.intProcId;
    U32 carrierId = 0;   //need to read from FPGA 
    U32 portId = 0;
    U16 receiveTransmit = 0;
    U32 fftExp = 0;
    //fpga->read(0x003A, carrierId);
    fpga->read(MM_B_AC_PS_CARRIER_NUM, carrierId);
    fpga->read(MM_B_AC_PS_ANT_NUM, portId);
    fpga->read(MM_B_AC_DL_FFT_EXP, fftExp);

    if(direct == AC_INT_DL)
    {
        TPT_INFO(STR("AcDispatch recv DL Interupt direct %d.", direct));
        fpga->write(MM_B_AC_BP_IRQ_DL_TRAP, MM_B_AC_BP_IRQ_DL_TRAP_ac_dl);
        fpga->write(MM_B_AC_BP_IRQ_DL_MASK, MM_B_AC_BP_IRQ_DL_MASK_ac_dl);
        receiveTransmit = 1;
        if (fftExp > 11)
        {
            TPT_INFO(STR("DL FFTExp out of range, %d.", fftExp));
        }
    }
    else if(direct == AC_INT_UL)
    {
        TPT_INFO(STR("AcDispatch recv UL Interupt direct %d.", direct));
        fpga->write(MM_B_AC_BP_IRQ_UL_TRAP, MM_B_AC_BP_IRQ_UL_TRAP_ac_ul);
        fpga->write(MM_B_AC_BP_IRQ_UL_MASK, MM_B_AC_BP_IRQ_UL_MASK_ac_ul);
        receiveTransmit = 0;
        if (fftExp > 14)
        {
            TPT_INFO(STR("UL FFTExp out of range, %d.", fftExp));
        }
    }
    else
    {
        TPT_INFO(STR("AcDispatch recv invalid Interupt direct %d.", direct));
        return;
    }

    TPT_INFO(STR("--------AcDispatcher carrier%d, port%d, direct %d, fftExp %d:-----------",
                      carrierId, portId, direct, fftExp));
    //return;

    std::list<ULCELLPE_ACSES_classS *>::iterator it;

    for (it = acSesList.begin(); it != acSesList.end(); ++it)
    {
        if((*it)->carrierId == carrierId)
        {
            //TRACE(2, STR("AcDispatcher: find AC session carrierId %d", carrierId));
            break;
        }
    }
    if(it == acSesList.end())
    {
        TPT_INFO(STR("AcDispatcher: Can't find AC session carrierId %d", carrierId));
        return;
    }
    ULCELLPE_ACSES_classS * acSes_p = *it;
    if( !(acSes_p->acPortInfo[0][portId].carrActive && acSes_p->acPortInfo[1][portId].carrActive) )
    {
        TPT_INFO(STR("AcDispatcher: carrierId %d port %d is not activated.", carrierId, portId));
        return;
    }
    if( acSes_p->state != ULCELLPE_ACSES_PROCESS)
    {
        TPT_INFO(STR("AcDispatcher: carrierId %d AC status not in PROCESS, Reject AC data.", 
            carrierId));
        return;
    }
    //U16 receiveTransmit = 1;

    //get the freq domain data from DDR which FPGA wrote the ac back data.    
    switch(acFreqDataSrc)
    {
        case AC_FREQ_DATA_SUCCESS:
            {
                memcpy(acSes_p->acFreqDomainData, complexRefSignal20Mhz, 
                                   4*SIZE_OF_AC_FREQ_DOMAIN_FFT_SIZE);
                break;
            }
        case AC_FREQ_DATA_FAILURE_DELAY:
            {
                memcpy(acSes_p->acFreqDomainData, complexSignal20MhzErrDelay, 
                                   4*SIZE_OF_AC_FREQ_DOMAIN_FFT_SIZE);
                TPT_INFO(STR("Warning! This is a Delay error AC data test"));
                break;
            }
        case AC_FREQ_DATA_FAILURE_DISTURB:
            {
                memcpy(acSes_p->acFreqDomainData, complexSignal20MhzErrDistb, 
                                   4*SIZE_OF_AC_FREQ_DOMAIN_FFT_SIZE);
                TPT_INFO(STR("Warning! This is a Disturbance error AC data test"));
                break;
            }
        case AC_FREQ_DATA_FAILURE_SIGLOW:
            {
                memcpy(acSes_p->acFreqDomainData, complexSignal20MhzErrSigLow, 
                                   4*SIZE_OF_AC_FREQ_DOMAIN_FFT_SIZE);
                TPT_INFO(STR("Warning! This is a SignalTooLow error AC data test"));
                break;
            }
        case AC_FREQ_DATA_FROM_FPGA:
            {
                //get ac freq data from ac_mem in DTS, need to FPGA support.
                U32 offsetBase = 0;
                U32 carrOffset = 8192 * ULCELLPE_AC_MAX_ANT_PER_CELL;
                U32 directOffset = 8192 *  ULCELLPE_AC_MAX_ANT_PER_CELL * 3;
                U32 portOffset = 8192;
                offsetBase = carrierId * carrOffset 
                                   + (direct - AC_INT_DL) * directOffset
                                   + portId * portOffset;
                freqDataMem->getFreqDataFromAcMem((S16 *)(acSes_p->acFreqDomainData), 
                             SIZE_OF_AC_FREQ_DOMAIN_FFT_SIZE*2, offsetBase/2);
                /*TPT_INFO(STR("AC Memory base 0x%08X: byte0 %d, byte1 %d, byte2 %d, byte3 %d.", 
                             offsetBase, 
                             acSes_p->acFreqDomainData[0].re, acSes_p->acFreqDomainData[0].im, 
                             acSes_p->acFreqDomainData[1].re, acSes_p->acFreqDomainData[1].im));*/
                break;
            }
        default:
            //TRACE_ERROR_SHELL(STR("Wrong AC Freq Data source."));
            return;
    }

    S16 noOfSc = 1200;
    //output
    S16 delay = 0;
    S16 initPhase = 0;
    S32 amplitude = 0;
    S16 channelSumCentidB = 0;
    U32 disturbance = 0;
    S16 noShift = 0;

    /*TPT_INFO(STR("AcDispatcher algorithm process Carrier%d Port%d:", carrierId, portId));*/
    /*call antennaCalibration algorithm*/
    antennaCalibration((S32 *)acSes_p->acFreqDomainData, noOfSc, 
                &delay, &initPhase, &amplitude, &channelSumCentidB, &disturbance, &noShift,
                (U16)fftExp);

    ULCELLPE_ACSES_calibFactorS acFactor;
    acFactor.delay = delay;
    acFactor.initPhase = initPhase;
    acFactor.amplitude = amplitude;
    acFactor.channelSumCentidB = channelSumCentidB;
    acFactor.disturbance = disturbance;
    acFactor.noShift = noShift;

    updateAcFactor(carrierId, portId, receiveTransmit, acFactor);
    /*
    pAcLogFile->storeAcData(carrierId, portId, receiveTransmit, 
        (S16 *)acSes_p->acFreqDomainData, 2*SIZE_OF_AC_FREQ_DOMAIN_FFT_SIZE);
    */

    if (checkAcSessionDone(acSes_p))
    {
        TPT_INFO(STR("AcDispatcher Carrier%d All Port DL&UL AC Done.", carrierId));
        acSes_p->state = ULCELLPE_ACSES_REPORT;
    }
    if (ULCELLPE_ACSES_REPORT == acSes_p->state)
    {
        detectDelayFault(acSes_p);
        detectAmpliFault(acSes_p);
        if(checkFaultStatus(acSes_p))
        {
            if(acSes_p->faultRetryCounter < MAXNUM_OF_AC_FAULT_RETRY)
            {
                //retry ac for this carrier all port.
                acSes_p->faultRetryCounter++;
                TPT_INFO(STR("Retry AC for CarrierId %d Times %d.", 
                     carrierId, acSes_p->faultRetryCounter));
                retryAc(acSes_p);
                return;
            }
            //cease first, in order to update new fault
            reportAcFault(acSes_p, AC_ALARM_CEASE);
            //should report Fault
            reportAcFault(acSes_p, AC_ALARM_REPORT);

            TPT_INFO(STR("AC Fault Raised after %d times Retry for CarrierId %d.",
                     acSes_p->faultRetryCounter, carrierId));
        }
        else
        {
            //no fault, send cease message
            reportAcFault(acSes_p, AC_ALARM_CEASE);
        }

        reportAcFactor(acSes_p);
        if(acSes_p->acType == ULCELLPE_ACSES_ACTYPE_PRE)
        {
            TPT_INFO(STR("AC Type: Pre-AC."));
            //Send delay to TRXM Radio APP for 64 ant, TX/RX.
            //sendAcFactorToRadio(carrierId, portId, receiveTransmit, acFactor);
            sendAcFactorToRadio(acSes_p, 0);
        }
        else
        {
            TPT_INFO(STR("AC Type: Period AC."));
            TPT_INFO(STR("Set AC Compensation into BB FPGA."));
            setAcFactorToFPGA(acSes_p);
            //sendPrachToRadio(acSes_p, 0);
        }
        storeAcFreqData(acSes_p);
    }
    TPT_INFO(STR("--------AcDispatcher AC data handling finished.--------------------"));
}

void AcDispatcher::handleDebugValueSetting(union itc_msg * rec_p)
{
    U16 debugName = 0;
    double debugValue = 0;
    U8 carrierId = 0;
    U8 acOnOff = 0;

    debugName = rec_p->trDebugValueSet.debugName;
    debugValue = (double)rec_p->trDebugValueSet.debugValue;

    switch (debugName)
    {
        case 1:/*set amplitude compensation high threshold*/
            ampFirstDetectUpValue = debugValue;
            TPT_INFO(STR("ampFirstDetectUpValue %f.", ampFirstDetectUpValue));
            break;
        case 2:/*set amplitude compensation low threshold*/
            ampFirstDetectDownValue = debugValue;
            TPT_INFO(STR("ampFirstDetectDownValue %f.", ampFirstDetectDownValue));
            break;
        case 3:/*stop(0) or start(1) period AC*/
            carrierId = ((U8)debugValue & 0xf0) >> 4;
            acOnOff = (U8)debugValue & 0xf;
            TPT_INFO(STR("trigger Period AC manumally %d, carrier id %d, on/off %d.", 
                         debugValue, carrierId, acOnOff));
            triggerPeriodAc(carrierId, acOnOff);
            break;
        case 4:/*run period AC immediately after Pre-AC*/
            runPeriodAcImdt = debugValue;
            TPT_INFO(STR("Run Period AC immediately after Per-AC %d.", runPeriodAcImdt));
        case 5:
            setFactoryCompOnOff((U8)debugValue);
            TPT_INFO(STR("setFactoryCompOnOff %d.", debugValue));
            break;
        default:
            TPT_INFO(STR("Mismatch debugName %d.", debugName));
    }
}
void AcDispatcher::setFactoryCompOnOff(U8 onOff)
{
    uint32_t ret = CREATE_TIMER_FAIL;
    uint8_t i = 0;

    std::list<ULCELLPE_ACSES_classS *>::iterator it;

    for (it = acSesList.begin(); it != acSesList.end(); ++it)
    {
        if ((*it)->state != ULCELLPE_ACSES_REPORT)/*Current AC is not finish, quit*/
        {
            TPT_INFO(STR("Current AC is not finish, quit."));
            return;
        }
    }

    if(it == acSesList.end()) //all current AC already done
    {
        if(onOff == 0)
        {
            delay_db = default_delay_db;
            phase_db = default_phase_db;
            amplit_db = default_amplit_db;
            TPT_INFO(STR("change db to default"));
        }
        else if(onOff == 1)
        {
            delay_db = delay_hwdb;
            phase_db = phase_hwdb;
            amplit_db = amplit_hwdb;
            TPT_INFO(STR("change db to HWDB"));
        }

        for(i=0; i<4; i++)
        {
            TPT_INFO(STR("delay_db[%d - %d] = %f,  %f, %f, %f, %f, %f, %f, %f, %f, %f, %f, %f, %f, %f, %f, %f", i*16, i*16+15,
                                delay_db[i*16+0], delay_db[i*16+1], delay_db[i*16+2], delay_db[i*16+3], delay_db[i*16+4], delay_db[i*16+5], delay_db[i*16+6], delay_db[i*16+7], 
                                delay_db[i*16+8], delay_db[i*16+9], delay_db[i*16+10], delay_db[i*16+11], delay_db[i*16+12], delay_db[i*16+13], delay_db[i*16+14], delay_db[i*16+15]));

            TPT_INFO(STR("phase_db[%d - %d] = %f,  %f, %f, %f, %f, %f, %f, %f, %f, %f, %f, %f, %f, %f, %f, %f", i*16, i*16+15,
                                phase_db[i*16+0], phase_db[i*16+1], phase_db[i*16+2], phase_db[i*16+3], phase_db[i*16+4], phase_db[i*16+5], phase_db[i*16+6], phase_db[i*16+7], 
                                phase_db[i*16+8], phase_db[i*16+9], phase_db[i*16+10], phase_db[i*16+11], phase_db[i*16+12], phase_db[i*16+13], phase_db[i*16+14], phase_db[i*16+15]));


            TPT_INFO(STR("amplit_db[%d - %d] = %f,  %f, %f, %f, %f, %f, %f, %f, %f, %f, %f, %f, %f, %f, %f, %f", i*16, i*16+15,
                                amplit_db[i*16+0], amplit_db[i*16+1], amplit_db[i*16+2], amplit_db[i*16+3], amplit_db[i*16+4], amplit_db[i*16+5], amplit_db[i*16+6], amplit_db[i*16+7], 
                                amplit_db[i*16+8], amplit_db[i*16+9], amplit_db[i*16+10], amplit_db[i*16+11], amplit_db[i*16+12], amplit_db[i*16+13], amplit_db[i*16+14], amplit_db[i*16+15]));

        }
    }

    for (it = acSesList.begin(); it != acSesList.end(); ++it)
    {
        ULCELLPE_ACSES_classS * acSes_p = *it;
        calcDelayDbAvg(acSes_p);
        calcPhaseDbAvg(acSes_p);
        calcAmplitDbAvg(acSes_p);
    }
}

void AcDispatcher::triggerPeriodAc(U16 carrierId, U8 acOnOff)
{
    uint32_t ret = CREATE_TIMER_FAIL;
    std::list<ULCELLPE_ACSES_classS *>::iterator it;

    for (it = acSesList.begin(); it != acSesList.end(); ++it)
    {
        if((*it)->carrierId == carrierId)
        {
            break;
        }
    }
    if(it == acSesList.end())
    {
        TPT_INFO(STR("PeriodAC Can't find AC session carrierId %d.", carrierId));
    }

    if ((*it)->state != ULCELLPE_ACSES_REPORT)/*Current AC is not finish, quit*/
    {
        TPT_INFO(STR("Current AC is not finish, quit."));
        return;
    }

    if (acOnOff == 0)/*stop period AC*/
    {
        ret = cancelTimer((*it)->acTimer);
        if(ret != REMOVE_TIMER_SUCCESS)
        {
            TPT_INFO(STR("Removed carrier %d timer fail.", carrierId));
        }
        (*it)->tmo_requested = false;
    }
    else if(acOnOff == 1)/*start period AC*/
    {
        if ((*it)->tmo_requested != false)
        {
            TPT_INFO(STR("Need stop period AC first.", carrierId));
        }
        else
        {
            (*it)->acTimer = createTimer(10, carrierId, ac_app_mbox);
            if((*it)->acTimer == CREATE_TIMER_FAIL)
            {
                TPT_INFO(STR("Create carrier %d timer fail. \n", carrierId));
            }
            (*it)->tmo_requested = true;
            TPT_INFO(STR("Trigger PeriodAC carrierId %d now.", carrierId));
        }
    }
    else
    {
        TPT_INFO(STR("invalid acOnOff %d.", acOnOff));
    }

    return;
}

void AcDispatcher::handleFreqDataStoreSetting(union itc_msg * rec_p)
{
    U16 carrierId;
    U32 antBitmap[4];

    carrierId = rec_p->trFreqDataStoreSet.carrierId;
    antBitmap[0] = rec_p->trFreqDataStoreSet.usedAntenna0_31;
    antBitmap[1] = rec_p->trFreqDataStoreSet.usedAntenna32_63;
    TPT_INFO(STR("Set Freq Data Store Fpga: carrierId %d, ant: 0x%08X,0x%08X.",
                carrierId, antBitmap[0],antBitmap[1],antBitmap[2],antBitmap[3]));
    std::list<ULCELLPE_ACSES_classS *>::iterator it;
    for (it = acSesList.begin(); it != acSesList.end(); ++it)
    {
        if((*it)->carrierId == carrierId)
            break;
    }
    if(it == acSesList.end())
    {
        TPT_INFO(STR("AcDispatcher: Can't find AC session carrierId %d", carrierId));
        return;
    }
    ULCELLPE_ACSES_classS * acSes_p = *it;

    /*process the antenna bit map*/	
    for (U16 k = 0; k < ULCELLPE_AC_MAX_ANT_PER_CELL; k++)
    {
        if((antBitmap[k/32] >> (k%32)) & 0x01)
        {
            acSes_p->acPortInfo[0][k].freqDataStoreFlag = 1;
            acSes_p->acPortInfo[1][k].freqDataStoreFlag = 1;
            TPT_INFO(STR("FreqData Store Set: CarrierId %d Port %d Store 1.", 
                        carrierId, k));
        }
        else
        {
            acSes_p->acPortInfo[0][k].freqDataStoreFlag = 0;
            acSes_p->acPortInfo[1][k].freqDataStoreFlag = 0;
            TPT_INFO(STR("FreqData Store Set: CarrierId %d Port %d Store 0.", 
                        carrierId, k));
        }
    }
    return;
}

bool AcDispatcher::addAcSession(U16 carrierId, U16 deviceId, U32 freq, U32 antBitmap[], U32 bandWidth)
{
    TPT_INFO(STR("AcDispatcher new AcSes: carrierId %d, diviceId %d, ant: 0x%08X,0x%08X,0x%08X,0x%08X.",
                carrierId, deviceId, antBitmap[0],antBitmap[1],antBitmap[2],antBitmap[3]));
    std::list<ULCELLPE_ACSES_classS *>::iterator it;

    U32 addr = 0;
    U32 carrOffset = 64;
    U32 portOffset = 1;

    for (it = acSesList.begin(); it != acSesList.end(); ++it)
    {
        if((*it)->carrierId == carrierId)
            break;
    }
    if(it == acSesList.end())
    {
        ULCELLPE_ACSES_classS* acSes_p;

        acSes_p = (ULCELLPE_ACSES_classS*)malloc(sizeof(ULCELLPE_ACSES_classS));
        memset(acSes_p, 0, sizeof(ULCELLPE_ACSES_classS));
        TPT_INFO(STR("AcDispatcher add Ac Session carrierId %d in the list.", carrierId));
        acSes_p->carrierId = carrierId;
        acSes_p->deviceId = deviceId;
        acSes_p->carrierFrequency= freq;
        acSes_p->bandWidth = bandWidth;
        switch (acSes_p->bandWidth)
        {
            case BAND20M:
                acSes_p->numRe = 1200;
                break;
            case BAND15M:
                acSes_p->numRe = 900;
                break;
            case BAND10M:
                acSes_p->numRe = 600;
                break;
            case BAND5M:
                acSes_p->numRe = 300;
                break;
            default:
                TPT_INFO(STR("Mismatch carrier bandwidth %d.", 
                         acSes_p->bandWidth));
                acSes_p->numRe = 1200;/*default as 20M*/
        }
        TPT_INFO(STR("Num of RE %d, freq %d, bandwidth %d.", 
                     acSes_p->numRe, 
                     acSes_p->carrierFrequency,
                     acSes_p->bandWidth));
		
        acSes_p->antBitmap[0] = antBitmap[0];
        acSes_p->antBitmap[1] = antBitmap[1];
        acSes_p->antBitmap[2] = antBitmap[2];
        acSes_p->antBitmap[3] = antBitmap[3];
        acSes_p->state = ULCELLPE_ACSES_SETUP;
        acSes_p->acCounter = 0;
        acSes_p->acType = ULCELLPE_ACSES_ACTYPE_PRE;
        /*reference AC port*/
        acSes_p->acReferAntIndex= 0;
        acSes_p->chanSCdBSum = 0;
        acSes_p->chanSCdBAvg = 0;
        acSes_p->faultRetryCounter = 0;
        acSes_p->waitAcDoneRetryCounter = 0;  
        /*init threshold*/
        acSes_p->acFaultUnwrapThreshold = 36;
        acSes_p->acFaultTxChannelThreshold = 285;
        acSes_p->acFaultRxChannelThreshold = 285;
        acSes_p->acFaultDelayThreshold = 500;
        /*init amplitDbAvg*/
        acSes_p->amplitDbAvg[0] = 0;
        acSes_p->amplitDbAvg[1] = 0;
        acSes_p->acFaultAmpThreshold = 10;  //unit:1/10 dBm, SW to alarm for (-1dB, +1dB)
        acSes_p->acAlarmDataLength = 6; /* currently, it should be 6 = 3 alarm types (signal too low, disturbed, amplitude), * 2 (ul + dl)*/
        memset(acSes_p->acFreqDomainData, 0, 4*SIZE_OF_AC_FREQ_DOMAIN_FFT_SIZE);
        /*process the antenna bit map*/	
        for (U16 k = 0; k < ULCELLPE_AC_MAX_ANT_PER_CELL; k++)
        {
            if((antBitmap[k/32] >> (k%32)) & 0x01)
            {
                acSes_p->acPortInfo[0][k].carrActive = 1;
                acSes_p->acPortInfo[1][k].carrActive = 1;
                acSes_p->acPortInfo[0][k].acDoneStatus = 0;
                acSes_p->acPortInfo[1][k].acDoneStatus = 0;
                acSes_p->acPortInfo[0][k].pathFault = 0;
                acSes_p->acPortInfo[1][k].pathFault = 0;
                acSes_p->acPortInfo[0][k].preAcReqSendFlag = 0;
                acSes_p->acPortInfo[1][k].preAcReqSendFlag = 0;
                acSes_p->acPortInfo[0][k].preAcRespRecvFlag = 0;
                acSes_p->acPortInfo[1][k].preAcRespRecvFlag = 0;
                acSes_p->acPortInfo[0][k].resetDelayRegReqSendFlag = 0;
                acSes_p->acPortInfo[1][k].resetDelayRegReqSendFlag = 0;
                acSes_p->acPortInfo[0][k].resetDelayRegRespRecvFlag = 0;
                acSes_p->acPortInfo[1][k].resetDelayRegRespRecvFlag = 0;                
                acSes_p->acPortInfo[0][k].freqDataStoreFlag = 0;
                acSes_p->acPortInfo[1][k].freqDataStoreFlag = 0;
                acSes_p->acPortInfo[0][k].faultRetryStatus[0] = 0;
                acSes_p->acPortInfo[1][k].faultRetryStatus[0] = 0;
                acSes_p->acPortInfo[0][k].faultRetryStatus[1] = 0;
                acSes_p->acPortInfo[1][k].faultRetryStatus[1] = 0;
                acSes_p->acPortInfo[0][k].faultRetryStatus[2] = 0;
                acSes_p->acPortInfo[1][k].faultRetryStatus[2] = 0;
                //AcTraceObjInst.enableAcTraceData(k, carrierId, ACTRACE_DATA_BOTH_ENABLE);
                TPT_INFO(STR("AcDispatcher Ac Session carrierId %d port %d is activated.", 
                            carrierId, k));
                addr = MM_B_DL_AC_ADJUST_GAIN_C0_ANT_0+carrOffset*(acSes_p->carrierId)+portOffset*k;
                fpga->write(addr, (U32)0x4000);
                addr = MM_B_UL_AC_ADJUST_GAIN_C0_ANT_0+carrOffset*(acSes_p->carrierId)+portOffset*k;
                fpga->write(addr, (U32)0x4000);
            }
        }

        /*calc DB amplitude AVG*/
        calcAmplitDbAvg(acSes_p);

        /*calc DB delay AVG*/
        calcDelayDbAvg(acSes_p);

        /*calc DB phase AVG*/
        calcPhaseDbAvg(acSes_p);

        /*recover compensation value*/
        setDefaultCompValue(acSes_p);

        acSesList.push_back(acSes_p);
    }
    else
    {
        //insert parameter failed as which has been already existed in this instance.
        TPT_INFO(STR("AcDispatcher addAcSession: insert paramter failed. carrierId %d.", 
                    carrierId));
        return false;
    }
    return true;
}

bool AcDispatcher::deleteAcSession(U16 carrierId)
{
    std::list<ULCELLPE_ACSES_classS *>::iterator it;
    bool deletedFlag = false;

    for (it = acSesList.begin(); it != acSesList.end(); ++it)
    {
        if((*it)->carrierId == carrierId)
        {
            delete (*it);
            *it = NULL;
            acSesList.erase(it);
            it--;
            TPT_INFO(STR("AcDispatcher deleteAcSession: carrierId %d.", carrierId));
            deletedFlag = true;
            break;
        }
    }
    if(!deletedFlag)
    {
        //INFO(STR("AcDispatcher: deleteAcSession Can't find AC session carrierId %d", carrierId));
    }
    return true;
}

bool AcDispatcher::setAcSessionStatus(U16 carrierId, ULCELLPE_ACSES_stateE state)
{
    std::list<ULCELLPE_ACSES_classS *>::iterator it;

    for ( it = acSesList.begin(); it != acSesList.end(); ++it)
    {
        if((*it)->carrierId == carrierId)
        {
            (*it)->state = state;
            TPT_INFO(STR("AcDispatcher: Set AC session carrierId %d status %d.", 
                        carrierId, (*it)->state));
            break;
        }
    }
    if(it == acSesList.end())
    {
        TPT_INFO(STR("AcDispatcher: Can't find AC session carrierId %d", carrierId));
    }
    return true;
}

bool AcDispatcher::startAcSession(U16 carrierId)
{
    std::list<ULCELLPE_ACSES_classS *>::iterator it;
    union itc_msg *sig;

    TPT_INFO(STR("AcDispatcher startAcSession carrierId %d.", carrierId));

    for (it = acSesList.begin(); it != acSesList.end(); ++it)
    {
        if((*it)->carrierId == carrierId)
        {
            //report cease, in order to clear pervious fault.
            ULCELLPE_ACSES_classS * acSes_p = *it;
            reportAcFault(acSes_p, AC_ALARM_CEASE);

            /* New subscription for Timer tmo*/
            //pCancelInfo = (CANCEL_INFO*)malloc(sizeof(CANCEL_INFO));
            //(*it)->tmo_subCancel = pCancelInfo;
            (*it)->tmo_requested = false;
            TPT_INFO(STR("AcDispatcher: start AC session carrierId %d", carrierId));
            //sig = itc_alloc(sizeof(TosvSignalAc_t), (SIGSELECT)carrierId);
            //sig = itc_alloc(sizeof(TosvSignalAc_t), (uint32_t)carrierId);
            sig = itc_alloc(sizeof(tmoTimeoutIndS_t), SIG_TIMEOUT_IND);
            sig->tmoTimeoutInd.index = carrierId;
            //send(&sig, acControlProc_);
            itc_send(&sig, ac_app_mbox, ITC_MY_MBOX);
            break;
        }
    }
    if(it == acSesList.end())
    {
        TPT_INFO(STR("AcDispatcher startAcSession Can't find AC session carrierId %d", 
                    carrierId));
    }
    return true;
}

bool AcDispatcher::stopAcSession(U16 carrierId)
{
    std::list<ULCELLPE_ACSES_classS *>::iterator it;
    uint32_t ret = CREATE_TIMER_FAIL;

    for (it = acSesList.begin(); it != acSesList.end(); ++it)
    {
        if((*it)->carrierId == carrierId)
        {
            //report cease, in order to clear fault.
            ULCELLPE_ACSES_classS * acSes_p = *it;
            reportAcFault(acSes_p, AC_ALARM_CEASE);

            setAcSessionStatus((*it)->carrierId, ULCELLPE_ACSES_DEACTIVE);

            if((*it)->resetDelayRegTimer != 0)
            {
                ret = cancelTimer((*it)->resetDelayRegTimer);
                if(ret != REMOVE_TIMER_SUCCESS)
               {
                   TPT_INFO(STR("Removed carrier %d Reset TRXM delay register  timer fail. \n",  (*it)->carrierId));
               }                
            }
            
            if((*it)->tmo_requested)
            {
                //fCancelTmo((*it)->tmo_subCancel);
                (*it)->tmo_requested = false;
                //if((*it)->tmo_subCancel) free((*it)->tmo_subCancel);
                ret = cancelTimer((*it)->acTimer);
                if(ret != REMOVE_TIMER_SUCCESS)
                {
                    TPT_INFO(STR("Removed carrier %d timer fail. \n", (*it)->carrierId));
                }
            }
            else
            {
                TPT_INFO("Failed to cancel TMO");
            }
            TPT_INFO(STR("AcDispatcher stopAcSession: carrierId %d", carrierId));
            break;
        }
    }
    if(it == acSesList.end())
    {
        TPT_INFO(STR("AcDispatcher: stopAcSession Can't find AC session carrierId %d", 
                    carrierId));
    }
    return true;
}

void AcDispatcher::setDefaultCompValue(ULCELLPE_ACSES_classS* acSes_p)
{
    U16 i = 0;
    U32 carrOffset = 64;
    U32 portOffset = 1;
    U32 addr1 = 0;
    U32 addr2 = 0;
    U32 addr3 = 0;

    for (i = 0; i < ULCELLPE_AC_MAX_ANT_PER_CELL; i++)
    {
        if(acSes_p->acPortInfo[0][i].carrActive && acSes_p->acPortInfo[1][i].carrActive )
        {
            addr1 = MM_B_DL_AC_ADJUST_GAIN_C0_ANT_0+carrOffset*(acSes_p->carrierId)+portOffset*i;
            fpga->write(addr1, (U32)0x4000);
            addr2 = MM_B_DL_AC_ADJUST_PHASE_C0_ANT_0+carrOffset*(acSes_p->carrierId)+portOffset*i;
            fpga->write(addr2, (U32)0);
            addr3 = MM_B_DL_AC_ADJUST_DLY_C0_ANT_0+carrOffset*(acSes_p->carrierId)+portOffset*i;
            fpga->write(addr3, (U32)0);
            TPT_INFO(STR("Set Default Carrier %d, DL, Port %d: Delay %u, Phase %u, Amp %u.", 
                            acSes_p->carrierId, i, 0, 0, 0));
            TPT_INFO(STR("Set Default FPGA Addr: Delay 0x%08X, Phase 0x%08X, Amp 0x%08X.", 
                            addr3, addr2, addr1));

            addr1 = MM_B_UL_AC_ADJUST_GAIN_C0_ANT_0+carrOffset*(acSes_p->carrierId)+portOffset*i;
            fpga->write(addr1, (U32)0x4000);
            addr2 = MM_B_UL_AC_ADJUST_PHASE_C0_ANT_0+carrOffset*(acSes_p->carrierId)+portOffset*i;
            fpga->write(addr2, (U32)0);
            addr3 = MM_B_UL_AC_ADJUST_DLY_C0_ANT_0+carrOffset*(acSes_p->carrierId)+portOffset*i;
            fpga->write(addr3, (U32)0);

            TPT_INFO(STR("Set Default Carrier %d, UL, Port %d: Delay %u, Phase %u, Amp %u.", 
                            acSes_p->carrierId, i, 0, 0, 0));
            TPT_INFO(STR("Set Defaults FPGA Addr: Delay 0x%08X, Phase 0x%08X, Amp 0x%08X.", 
                            addr3, addr2, addr1));
        }
    }
}

bool AcDispatcher::updateAcFactor(U16 carrierId, U16 portId, U16 receiveTransmit, 
            ULCELLPE_ACSES_calibFactorS acFactor)
{
    /*TPT_INFO(STR("AcDispatcher updateAcFactor carrier%d port%d:", carrierId, portId));*/
    std::list<ULCELLPE_ACSES_classS *>::iterator it;

    for (it = acSesList.begin(); it != acSesList.end(); ++it)
    {
        if((*it)->carrierId == carrierId)
        {
            break;
        }
    }
    if(it == acSesList.end())
    {
        TPT_INFO(STR("AcDispatcher: PeriodAC Can't find AC session carrierId %d", 
                    carrierId));
        return false;
    }

    ULCELLPE_ACSES_classS * acSes_p = *it;
    if( receiveTransmit == 0)    //up link
    {
        acSes_p->acCalibFactor[1][portId].delay = acFactor.delay;
        acSes_p->acCalibFactor[1][portId].initPhase = acFactor.initPhase;
        acSes_p->acCalibFactor[1][portId].amplitude = acFactor.amplitude;
        acSes_p->acCalibFactor[1][portId].channelSumCentidB = acFactor.channelSumCentidB;
        acSes_p->acCalibFactor[1][portId].disturbance = acFactor.disturbance;
        acSes_p->acCalibFactor[1][portId].noShift = acFactor.noShift;
        if (acSes_p->acPortInfo[1][portId].acDoneStatus == 0)
        {
            acSes_p->acPortInfo[1][portId].acDoneStatus = 1;
        }
        else
        {
            TPT_INFO(STR("AC output: CarrierId %d, UL, Port %d, acDoneStatus %d.", 
                            carrierId, portId, acSes_p->acPortInfo[1][portId].acDoneStatus));
            return false;
        }
        TPT_INFO(STR("AcDispatcher: AC CarrierId %d, UL, Port %d, acDoneStatus %d.", 
                        carrierId, portId, acSes_p->acPortInfo[1][portId].acDoneStatus));

        TPT_INFO(STR("AC output: CarrierId %d, UL, Port %d, acDoneStatus %d, \
delay %d, phase %d, amplit %d, chanSCdB %d, disturb %d, noShift %d.", 
                        carrierId, portId, 
                        acSes_p->acPortInfo[1][portId].acDoneStatus,
                        acSes_p->acCalibFactor[1][portId].delay,
                        acSes_p->acCalibFactor[1][portId].initPhase,
                        acSes_p->acCalibFactor[1][portId].amplitude,
                        acSes_p->acCalibFactor[1][portId].channelSumCentidB,
                        acSes_p->acCalibFactor[1][portId].disturbance,
                        acSes_p->acCalibFactor[1][portId].noShift
                        ));
    }
    else                                     //downlink
    {
        acSes_p->acCalibFactor[0][portId].delay = acFactor.delay;
        acSes_p->acCalibFactor[0][portId].initPhase = acFactor.initPhase;
        acSes_p->acCalibFactor[0][portId].amplitude = acFactor.amplitude;
        acSes_p->acCalibFactor[0][portId].channelSumCentidB = acFactor.channelSumCentidB;
        acSes_p->acCalibFactor[0][portId].disturbance = acFactor.disturbance;
        acSes_p->acCalibFactor[0][portId].noShift = acFactor.noShift;
        if (acSes_p->acPortInfo[0][portId].acDoneStatus == 0)
        {
            acSes_p->acPortInfo[0][portId].acDoneStatus = 1;
        }
        else
        {
            TPT_INFO(STR("AC output: CarrierId %d, DL, Port %d, acDoneStatus %d.", 
                            carrierId, portId, acSes_p->acPortInfo[0][portId].acDoneStatus));
            return false;
        }
        TPT_INFO(STR("AcDispatcher: AC run CarrierId %d, DL, Port %d, acDoneStatus %d.", 
                        carrierId, portId, acSes_p->acPortInfo[0][portId].acDoneStatus));
        TPT_INFO(STR("AC output: CarrierId %d, DL, Port %d, acDoneStatus %d, \
delay %d, phase %d, amplit %d, chanSCdB %d, disturb %d, noShift %d.", 
                        carrierId, portId, 
                        acSes_p->acPortInfo[0][portId].acDoneStatus,
                        acSes_p->acCalibFactor[0][portId].delay,
                        acSes_p->acCalibFactor[0][portId].initPhase,
                        acSes_p->acCalibFactor[0][portId].amplitude,
                        acSes_p->acCalibFactor[0][portId].channelSumCentidB,
                        acSes_p->acCalibFactor[0][portId].disturbance,
                        acSes_p->acCalibFactor[0][portId].noShift
                        ));
    }
    return true;
}

bool AcDispatcher::checkAcSessionDone(ULCELLPE_ACSES_classS* acSes_p)
{
    U16 i;
    bool retVal = true;
    for (i = 0; i < ULCELLPE_AC_MAX_ANT_PER_CELL; i++)
    {
        if (acSes_p->acPortInfo[0][i].carrActive == 1)
        {
            if (acSes_p->acPortInfo[0][i].acDoneStatus == 0)
            {
                retVal = false;
                break;
            }
        }
        if (acSes_p->acPortInfo[1][i].carrActive == 1)
        {
            if (acSes_p->acPortInfo[1][i].acDoneStatus == 0)
            {
                retVal = false;
                break;
            }
        }
    }
    return retVal;
}

void AcDispatcher::restoreAcSession(ULCELLPE_ACSES_classS* acSes_p)
{
    //clean AC session acDoneStatus, pathFault
    U16 i;
    TPT_INFO(STR("AcDispatcher: restore AC session para for carrierId %d", 
            acSes_p->carrierId));
    acSes_p->chanSCdBSum = 0;
    acSes_p->chanSCdBAvg = 0;
    acSes_p->faultRetryCounter = 0;
    acSes_p->waitAcDoneRetryCounter = 0;  
    acSes_p->acReferAntIndex = 0;
    for (i = 0; i < ULCELLPE_AC_MAX_ANT_PER_CELL; i++)
    {
        if (acSes_p->acPortInfo[0][i].carrActive == 1)
        {
            acSes_p->acPortInfo[0][i].acDoneStatus = 0;
            acSes_p->acPortInfo[0][i].pathFault = 0;
            acSes_p->acPortInfo[0][i].faultRetryStatus[0] = 0;
            acSes_p->acPortInfo[0][i].faultRetryStatus[1] = 0;
            acSes_p->acPortInfo[0][i].faultRetryStatus[2] = 0;
            acSes_p->acCalibFactor[0][i].delay = 0;
            acSes_p->acCalibFactor[0][i].initPhase = 0;
            acSes_p->acCalibFactor[0][i].amplitude = 0;
            acSes_p->acCalibFactor[0][i].channelSumCentidB = 0;
            acSes_p->acCalibFactor[0][i].disturbance = 0;
            acSes_p->acCalibFactor[0][i].noShift = 0;
        }
        if (acSes_p->acPortInfo[1][i].carrActive == 1)
        {
            acSes_p->acPortInfo[1][i].acDoneStatus = 0;
            acSes_p->acPortInfo[1][i].pathFault = 0;
            acSes_p->acPortInfo[1][i].faultRetryStatus[0] = 0;
            acSes_p->acPortInfo[1][i].faultRetryStatus[1] = 0;
            acSes_p->acPortInfo[1][i].faultRetryStatus[2] = 0;
            acSes_p->acCalibFactor[1][i].delay = 0;
            acSes_p->acCalibFactor[1][i].initPhase = 0;
            acSes_p->acCalibFactor[1][i].amplitude = 0;
            acSes_p->acCalibFactor[1][i].channelSumCentidB = 0;
            acSes_p->acCalibFactor[1][i].disturbance = 0;
            acSes_p->acCalibFactor[1][i].noShift = 0;
        }
    }
}

void AcDispatcher::retryAc(ULCELLPE_ACSES_classS* acSes_p)
{
    //retry AC again for AC fault checking
    acSes_p->state = ULCELLPE_ACSES_PROCESS;
    acSes_p->chanSCdBSum = 0;
    acSes_p->chanSCdBAvg = 0;
    acSes_p->acReferAntIndex = 0;
    for (U16 i = 0; i < ULCELLPE_AC_MAX_ANT_PER_CELL; i++)
    {
        if (acSes_p->acPortInfo[0][i].carrActive == 1)
        {
            acSes_p->acPortInfo[0][i].acDoneStatus = 0;
            acSes_p->acCalibFactor[0][i].delay = 0;
            acSes_p->acCalibFactor[0][i].initPhase = 0;
            acSes_p->acCalibFactor[0][i].amplitude = 0;
            acSes_p->acCalibFactor[0][i].channelSumCentidB = 0;
            acSes_p->acCalibFactor[0][i].disturbance = 0;
            acSes_p->acCalibFactor[0][i].noShift = 0;
        }
        if (acSes_p->acPortInfo[1][i].carrActive == 1)
        {
            acSes_p->acPortInfo[1][i].acDoneStatus = 0;
            acSes_p->acCalibFactor[1][i].delay = 0;
            acSes_p->acCalibFactor[1][i].initPhase = 0;
            acSes_p->acCalibFactor[1][i].amplitude = 0;
            acSes_p->acCalibFactor[1][i].channelSumCentidB = 0;
            acSes_p->acCalibFactor[1][i].disturbance = 0;
            acSes_p->acCalibFactor[1][i].noShift = 0;
        }
    }
    sendAcParaToFpga(acSes_p->carrierId);
}

void AcDispatcher::detectDelayFault(ULCELLPE_ACSES_classS* acSes_p)
{
    S16 acFaultDelayThreshold;
    U16 i;
    double gainComp;

    TPT_INFO(STR("AcDispatcher detectDelayFault carrier%d.", acSes_p->carrierId));
    /*
     * Transfer the threshold from ns to delay
     * Accoring to calculation further down in the code
     * t = delay * 0,0636ns
     * 1/0,0636 = 16
     */
    acFaultDelayThreshold = acSes_p->acFaultDelayThreshold * AC_DELAY_TIME_IN_NS;

    for (i = 0; i < ULCELLPE_AC_MAX_ANT_PER_CELL; i++)
    {
        // result = (U32)AC_FAULT_DELAY;  // [0100] Faultbit for delay
        //result = (U32)AC_CTRL_FAULT_DELAY;  // [0100] Faultbit for delay

        if(acSes_p->acPortInfo[0][i].carrActive && acSes_p->acPortInfo[1][i].carrActive )
        {
            // the dl delay must be between the range [-delayT,+delayT]  (dl,tx)
            if ((acSes_p->acCalibFactor[0][i].delay > acFaultDelayThreshold) ||
                (acSes_p->acCalibFactor[0][i].delay < - acFaultDelayThreshold))
            {
                //acSes_p->acPortInfo[0][i].pathFault |= AC_CTRL_FAULT_DELAY;
                acSes_p->acPortInfo[0][i].faultRetryStatus[acSes_p->faultRetryCounter] 
                    |= AC_CTRL_FAULT_DELAY;
            }

            // the ul delay must be between the range [-delayT,+delayT]  (ul,rx)
            if ((acSes_p->acCalibFactor[1][i].delay > acFaultDelayThreshold) ||
                (acSes_p->acCalibFactor[1][i].delay < - acFaultDelayThreshold))
            {
                acSes_p->acPortInfo[1][i].faultRetryStatus[acSes_p->faultRetryCounter]
                   |= AC_CTRL_FAULT_DELAY;
            }

            /*AC_CTRL_FAULT_PHASE,  disturbance*/
            if ((acSes_p->acCalibFactor[0][i].disturbance > acSes_p->acFaultUnwrapThreshold)
                   ||(acSes_p->acCalibFactor[0][i].noShift > AC_DISTURB_SHIFT_MAX))
            {
                acSes_p->acPortInfo[0][i].faultRetryStatus[acSes_p->faultRetryCounter]
                   |= AC_CTRL_FAULT_PHASE;
            }
            if ((acSes_p->acCalibFactor[1][i].disturbance > acSes_p->acFaultUnwrapThreshold)
                   ||(acSes_p->acCalibFactor[1][i].noShift > AC_DISTURB_SHIFT_MAX))
            {
                acSes_p->acPortInfo[1][i].faultRetryStatus[acSes_p->faultRetryCounter]
                   |= AC_CTRL_FAULT_PHASE;
            }

            /*AC_CTRL_FAULT_AMPLITUDE,  AMPLITUDE,  signal too low*/
            if ((acSes_p->acCalibFactor[0][i].channelSumCentidB< acSes_p->acFaultTxChannelThreshold))
            {
                acSes_p->acPortInfo[0][i].faultRetryStatus[acSes_p->faultRetryCounter]
                   |= AC_CTRL_FAULT_SIG_TOO_LOW;
            }
            if ((acSes_p->acCalibFactor[1][i].channelSumCentidB< acSes_p->acFaultRxChannelThreshold))
            {
                acSes_p->acPortInfo[1][i].faultRetryStatus[acSes_p->faultRetryCounter]
                   |= AC_CTRL_FAULT_SIG_TOO_LOW;
            }

            /*Check amplitude do not equal to 0*/
            if ((acSes_p->acCalibFactor[0][i].amplitude == 0))
            {
                acSes_p->acPortInfo[0][i].faultRetryStatus[acSes_p->faultRetryCounter]
                   |= AC_CTRL_FAULT_AMPLITUDE;
            }
            if ((acSes_p->acCalibFactor[1][i].amplitude == 0))
            {
                acSes_p->acPortInfo[1][i].faultRetryStatus[acSes_p->faultRetryCounter]
                   |= AC_CTRL_FAULT_AMPLITUDE;
            }
        }
    }
}

void AcDispatcher::detectAmpliFault(ULCELLPE_ACSES_classS* acSes_p)
{
    U16 i = 0;
    double gainComp = 0;

    TPT_INFO(STR("AcDispatcher detectAmpliFault carrier%d.", acSes_p->carrierId));

    for (i = 0; i < ULCELLPE_AC_MAX_ANT_PER_CELL; i++)
    {
        if(acSes_p->acPortInfo[0][i].carrActive && acSes_p->acPortInfo[1][i].carrActive )
        {
            // DL
            if (!(acSes_p->acPortInfo[0][i].pathFault) 
                  && !(acSes_p->acPortInfo[0][i].faultRetryStatus[acSes_p->faultRetryCounter]))
            {
                /*1 round, check DL amplitue*/
                TPT_INFO(STR("1 round port %d, DL.", i));
                gainComp = calcAmplitComp(acSes_p, 0, i);
                if (gainComp<ampFirstDetectDownValue || gainComp>ampFirstDetectUpValue)
                {
                    acSes_p->acPortInfo[0][i].ampliFaultFlag = 1;
                    TPT_INFO(STR("1. acSes_p->acPortInfo[0][%d].faultRetryStatus AC_CTRL_FAULT_AMPLITUDE fault.", i));
                }
                else
                {
                    acSes_p->acPortInfo[0][i].ampliFaultFlag = 0;
                }
            }

            // UL
            if (!(acSes_p->acPortInfo[1][i].pathFault) 
                  && !(acSes_p->acPortInfo[1][i].faultRetryStatus[acSes_p->faultRetryCounter]))
            {
                /*1 round, check DL amplitue*/
                TPT_INFO(STR("1 round port %d, UL.", i));
                gainComp = calcAmplitComp(acSes_p, 1, i);
                if (gainComp<ampFirstDetectDownValue || gainComp>ampFirstDetectUpValue)
                {
                    acSes_p->acPortInfo[1][i].ampliFaultFlag = 1;
                    TPT_INFO(STR("1. acSes_p->acPortInfo[1][%d].faultRetryStatus AC_CTRL_FAULT_AMPLITUDE fault.", i));
                }
                else
                {
                    acSes_p->acPortInfo[1][i].ampliFaultFlag = 0;
                }
            }
        }
    }
    for (i = 0; i < ULCELLPE_AC_MAX_ANT_PER_CELL; i++)
    {
        if(acSes_p->acPortInfo[0][i].carrActive && acSes_p->acPortInfo[1][i].carrActive )
        {
            //DL
            if(acSes_p->acPortInfo[0][i].ampliFaultFlag == 1)
            {
                acSes_p->acPortInfo[0][i].faultRetryStatus[acSes_p->faultRetryCounter]
                    |= AC_CTRL_FAULT_AMPLITUDE;
            }
            //UL
            if(acSes_p->acPortInfo[1][i].ampliFaultFlag == 1)
            {
                acSes_p->acPortInfo[1][i].faultRetryStatus[acSes_p->faultRetryCounter]
                    |= AC_CTRL_FAULT_AMPLITUDE;
            }
        }
    }
    for (i = 0; i < ULCELLPE_AC_MAX_ANT_PER_CELL; i++)
    {
        if(acSes_p->acPortInfo[0][i].carrActive && acSes_p->acPortInfo[1][i].carrActive )
        {
            // DL
            if (!(acSes_p->acPortInfo[0][i].pathFault) 
                  && !(acSes_p->acPortInfo[0][i].faultRetryStatus[acSes_p->faultRetryCounter]))
            {
                /*2 round, check DL amplitue*/
                TPT_INFO(STR("2 round port %d, DL.", i));
                gainComp = calcAmplitComp(acSes_p, 0, i);
                if (gainComp<ampFirstDetectDownValue || gainComp>ampFirstDetectUpValue)
                {
                    acSes_p->acPortInfo[0][i].ampliFaultFlag = 1;
                    TPT_INFO(STR("1. acSes_p->acPortInfo[0][%d].faultRetryStatus AC_CTRL_FAULT_AMPLITUDE fault.", i));
                }
                else
                {
                    acSes_p->acPortInfo[0][i].ampliFaultFlag = 0;
                }
            }

            // UL
            if (!(acSes_p->acPortInfo[1][i].pathFault) 
                  && !(acSes_p->acPortInfo[1][i].faultRetryStatus[acSes_p->faultRetryCounter]))
            {
                /*2 round, check DL amplitue*/
                TPT_INFO(STR("2 round port %d, UL.", i));
                gainComp = calcAmplitComp(acSes_p, 1, i);
                if (gainComp<ampFirstDetectDownValue || gainComp>ampFirstDetectUpValue)
                {
                    acSes_p->acPortInfo[1][i].ampliFaultFlag = 1;
                    TPT_INFO(STR("1. acSes_p->acPortInfo[0][%d].faultRetryStatus AC_CTRL_FAULT_AMPLITUDE fault.", i));
                }
                else
                {
                    acSes_p->acPortInfo[1][i].ampliFaultFlag = 0;
                }
            }
        }
    }
    for (i = 0; i < ULCELLPE_AC_MAX_ANT_PER_CELL; i++)
    {
        if(acSes_p->acPortInfo[0][i].carrActive && acSes_p->acPortInfo[1][i].carrActive )
        {
            //DL
            if(acSes_p->acPortInfo[0][i].ampliFaultFlag == 1)
            {
                acSes_p->acPortInfo[0][i].faultRetryStatus[acSes_p->faultRetryCounter]
                    |= AC_CTRL_FAULT_AMPLITUDE;
            }
            //UL
            if(acSes_p->acPortInfo[1][i].ampliFaultFlag == 1)
            {
                acSes_p->acPortInfo[1][i].faultRetryStatus[acSes_p->faultRetryCounter]
                    |= AC_CTRL_FAULT_AMPLITUDE;
            }
        }
    }

}

bool AcDispatcher::checkFaultStatus(ULCELLPE_ACSES_classS* acSes_p)
{
    bool faultStatus = false;
    bool selectReferPort = false;
    for (U16 i = 0; i < ULCELLPE_AC_MAX_ANT_PER_CELL; i++)
    {
        if(acSes_p->acPortInfo[0][i].carrActive && acSes_p->acPortInfo[1][i].carrActive )
        {
            switch (acSes_p->faultRetryCounter)
            {
                case 0:
                    acSes_p->acPortInfo[0][i].pathFault =
                        acSes_p->acPortInfo[0][i].faultRetryStatus[0];

                    acSes_p->acPortInfo[1][i].pathFault = 
                        acSes_p->acPortInfo[1][i].faultRetryStatus[0];
                    break;
                case 1:
                    acSes_p->acPortInfo[0][i].pathFault = 
                        acSes_p->acPortInfo[0][i].faultRetryStatus[0]
                        && acSes_p->acPortInfo[0][i].faultRetryStatus[1];

                    acSes_p->acPortInfo[1][i].pathFault = 
                        acSes_p->acPortInfo[1][i].faultRetryStatus[0]
                        && acSes_p->acPortInfo[1][i].faultRetryStatus[1];
                    break;
                case 2:
                    acSes_p->acPortInfo[0][i].pathFault = 
                        acSes_p->acPortInfo[0][i].faultRetryStatus[0]
                        && acSes_p->acPortInfo[0][i].faultRetryStatus[1]
                        && acSes_p->acPortInfo[0][i].faultRetryStatus[2];

                    acSes_p->acPortInfo[1][i].pathFault = 
                        acSes_p->acPortInfo[1][i].faultRetryStatus[0]
                        && acSes_p->acPortInfo[1][i].faultRetryStatus[1]
                        && acSes_p->acPortInfo[1][i].faultRetryStatus[2];
                    break;
               default:
                    TPT_INFO(STR("AC Fault retry counter is invalid for carrier%d.", 
                        acSes_p->carrierId));
                   break;
           }

            if((acSes_p->acPortInfo[0][i].pathFault != 0)
                || (acSes_p->acPortInfo[1][i].pathFault != 0))
            {
                faultStatus = true;
                TPT_INFO(STR("AC Fault detected for carrier%d, port %02d, retryCounter %d.", 
                    acSes_p->carrierId, i, acSes_p->faultRetryCounter));
                //break;
            }
            else
            {
                //this port has no fault.
                if(!selectReferPort)
                {
                    //select the 1st port which no fault as AC reference Port.
                    TPT_INFO(STR("Select port %d as AC reference port.", i));
                    acSes_p->acReferAntIndex = i;
                    selectReferPort = true;
                }
            }
        }
    }
	TPT_INFO(STR("checkFaultStatus return %d.", faultStatus));
    return faultStatus;
}

void AcDispatcher::reportAcFault(ULCELLPE_ACSES_classS* acSes_p, U16 alarm_action)
{
    U16 acAlarmDataLength = acSes_p->acAlarmDataLength;
    U16 acAlarmDisturbed = 0;
    U16 acAlarmSignalTooLow = 0;
    U16 acAlarmAmplitude = 0;
    U16 bitMapGroup = 0;
    union itc_msg *outSig;

    TPT_INFO(STR("AcDispatcher reportAcFault carrier%d, type %d.", acSes_p->carrierId, alarm_action));

    if(alarm_action == AC_ALARM_CEASE)
    {
#if 0
        outSig = itc_alloc(sizeof(AcAlarmIndS_t), TR_AC_FAULT_CEASE);
        outSig->AcAlarmInd.antpnum = ULCELLPE_AC_MAX_ANT_PER_CELL;
        outSig->AcAlarmInd.carrierID = acSes_p->carrierId;
        outSig->AcAlarmInd.deviceID = acSes_p->deviceId;
        outSig->AcAlarmInd.acAlarmDataLength = 0;
        TPT_INFO(STR("antpnum = %d, carrierID = %d, deviceID = %d",
                outSig->AcAlarmInd.antpnum, outSig->AcAlarmInd.carrierID, outSig->AcAlarmInd.deviceID));
        itc_send(&outSig, app_fault_mgr_mbox, ITC_MY_MBOX);
        TPT_INFO(STR("Send AC_ALARM_CEASE done"));
        return;
#endif
        outSig = itc_alloc(sizeof(AcAlarmCeaseIndS_t), TR_AC_FAULT_CEASE);
        outSig->AcAlarmCeaseInd.carrierID = acSes_p->carrierId;
        outSig->AcAlarmCeaseInd.deviceID = acSes_p->deviceId;
        TPT_INFO(STR("carrierID = %d, deviceID = %d",
                outSig->AcAlarmCeaseInd.carrierID, outSig->AcAlarmCeaseInd.deviceID));
        itc_send(&outSig, app_fault_mgr_mbox, ITC_MY_MBOX);
        TPT_INFO(STR("Send AC_ALARM_CEASE done"));
        return;
    }

    /* if alarm_action is AC_ALARM_REPORT */
    outSig = itc_alloc(sizeof(AcAlarmIndS_t) + (acAlarmDataLength-1)*sizeof(AcAlarmDeteilS_t), TR_AC_FAULT_REPORT);
    outSig->AcAlarmInd.antpnum = ULCELLPE_AC_MAX_ANT_PER_CELL;
    outSig->AcAlarmInd.carrierID = acSes_p->carrierId;
    outSig->AcAlarmInd.deviceID = acSes_p->deviceId;
    outSig->AcAlarmInd.acAlarmDataLength = acAlarmDataLength;
    TPT_INFO(STR("antpnum = %d, carrierID = %d, deviceID = %d",
                  outSig->AcAlarmInd.antpnum, outSig->AcAlarmInd.carrierID, outSig->AcAlarmInd.deviceID));

    /* init AcAlarmDetailS_t */
    for(U16 j=0; j<acAlarmDataLength; j++)
    {
        outSig->AcAlarmInd.acAlarmData[j].type = 0;
        outSig->AcAlarmInd.acAlarmData[j].direction = 0;
        outSig->AcAlarmInd.acAlarmData[j].bitmap[0] = 0;
        outSig->AcAlarmInd.acAlarmData[j].bitmap[1] = 0;
        outSig->AcAlarmInd.acAlarmData[j].bitmap[2] = 0; 
        outSig->AcAlarmInd.acAlarmData[j].bitmap[3] = 0;
        outSig->AcAlarmInd.acAlarmData[j].bitmap[4] = 0; 
        outSig->AcAlarmInd.acAlarmData[j].bitmap[5] = 0;
        outSig->AcAlarmInd.acAlarmData[j].bitmap[6] = 0; 
        outSig->AcAlarmInd.acAlarmData[j].bitmap[7] = 0;
    }

    // report sequence, as APP and NC requirement
    // amplitude UL/DL -> distrubed UL/DL, sigal too low UL/DL
    for (U16 i = 0; i < ULCELLPE_AC_MAX_ANT_PER_CELL; i++)
    {
        if(acSes_p->acPortInfo[0][i].carrActive && acSes_p->acPortInfo[1][i].carrActive )
        {
            /* Uplink Amplitude */
            acAlarmAmplitude = acSes_p->acPortInfo[UPLINK][i].pathFault & AC_CTRL_FAULT_AMPLITUDE;
            if(acAlarmAmplitude > 0)
            {
                bitMapGroup = i/16; //AcAlarmDeteilS_t->bitmap size is U16, means one group for 16 antenna, total 8 array, support 128 antenna
                acAlarmAmplitude = 1 << (i % 16); //set amplitude bit in the corresponding antenna bit, bitmap size is U16, only 16bit, arrary size is 8, can support 128 antennas
                outSig->AcAlarmInd.acAlarmData[0].type = AC_ALARM_AMPLITUDE;
                outSig->AcAlarmInd.acAlarmData[0].direction = UPLINK;
                outSig->AcAlarmInd.acAlarmData[0].bitmap[bitMapGroup] |= acAlarmAmplitude;
                TPT_INFO(STR("UpLink Amplitude, port %d.", i));
            }

            /* DownLink Amplitude */
            acAlarmAmplitude= acSes_p->acPortInfo[DOWNLINK][i].pathFault & AC_CTRL_FAULT_AMPLITUDE;
            if(acAlarmAmplitude > 0)
            {
                bitMapGroup = i/16; //AcAlarmDeteilS_t->bitmap size is U16, means one group for 16 antenna, total 8 array, support 128 antenna
                acAlarmAmplitude = 1 << (i % 16); //set amplitude bit in the corresponding antenna bit, bitmap size is U16, only 16bit, arrary size is 8, can support 128 antennas
                outSig->AcAlarmInd.acAlarmData[1].type = AC_ALARM_AMPLITUDE;
                outSig->AcAlarmInd.acAlarmData[1].direction = DOWNLINK;
                outSig->AcAlarmInd.acAlarmData[1].bitmap[bitMapGroup] |= acAlarmAmplitude;
                TPT_INFO(STR("DownLink Amplitude, port %d.", i));
            }

            /* UpnLink distrubed */
            acAlarmDisturbed = acSes_p->acPortInfo[UPLINK][i].pathFault & (AC_CTRL_FAULT_PHASE | AC_CTRL_FAULT_DELAY);
            if(acAlarmDisturbed > 0)
            {
                bitMapGroup = i/16; //AcAlarmDeteilS_t->bitmap size is U16, means one group for 16 antenna, total 8 array, support 128 antenna
                acAlarmDisturbed = 1 << (i % 16); //set disturbed bit in the corresponding antenna bit, bitmap size is U16, only 16bit, arrary size is 8, can support 128 antennas
                outSig->AcAlarmInd.acAlarmData[2].type = AC_ALARM_DISTURBED;
                outSig->AcAlarmInd.acAlarmData[2].direction = UPLINK;
                outSig->AcAlarmInd.acAlarmData[2].bitmap[bitMapGroup] |= acAlarmDisturbed;
                TPT_INFO(STR("UpLink distrubed, port %d.", i));
            }

            /* DownLink distrubed */
            acAlarmDisturbed = acSes_p->acPortInfo[DOWNLINK][i].pathFault & (AC_CTRL_FAULT_PHASE | AC_CTRL_FAULT_DELAY);
            if(acAlarmDisturbed > 0) //need to set alarm disturbed
            {
                bitMapGroup = i/16; //AcAlarmDeteilS_t->bitmap size is U16, means one group for 16 antenna, total 8 array, support 128 antenna
                acAlarmDisturbed = 1<< (i % 16); //set disturbed bit in the corresponding antenna bit, bitmap size is U16, only 16bit, arrary size is 8, can support 128 antennas
                outSig->AcAlarmInd.acAlarmData[3].type = AC_ALARM_DISTURBED;
                outSig->AcAlarmInd.acAlarmData[3].direction = DOWNLINK;
                outSig->AcAlarmInd.acAlarmData[3].bitmap[bitMapGroup] |= acAlarmDisturbed;
                TPT_INFO(STR("DownLink distrubed, port %d.", i));
            }

            /* Uplink Signal Too Low */
            acAlarmSignalTooLow = acSes_p->acPortInfo[UPLINK][i].pathFault & AC_CTRL_FAULT_SIG_TOO_LOW;
            if(acAlarmSignalTooLow > 0)
            {
                bitMapGroup = i/16; //AcAlarmDeteilS_t->bitmap size is U16, means one group for 16 antenna, total 8 array, support 128 antenna
                acAlarmSignalTooLow = 1 << (i % 16); //set signal too low bit in the corresponding antenna bit, bitmap size is U16, only 16bit, arrary size is 8, can support 128 antennas
                outSig->AcAlarmInd.acAlarmData[4].type = AC_ALARM_SIGNAL_TOO_LOW;
                outSig->AcAlarmInd.acAlarmData[4].direction = UPLINK;
                outSig->AcAlarmInd.acAlarmData[4].bitmap[bitMapGroup] |= acAlarmSignalTooLow;
                TPT_INFO(STR("UpLink Signal Too Low, port %d.", i));
            }

            /* DownLink Signal Too Low */
            acAlarmSignalTooLow = acSes_p->acPortInfo[DOWNLINK][i].pathFault & AC_CTRL_FAULT_SIG_TOO_LOW;
            if(acAlarmSignalTooLow > 0)
            {
                bitMapGroup = i/16; //AcAlarmDeteilS_t->bitmap size is U16, means one group for 16 antenna, total 8 array, support 128 antenna
                acAlarmSignalTooLow = 1 << (i % 16); //set signal too low bit in the corresponding antenna bit, bitmap size is U16, only 16bit, arrary size is 8, can support 128 antennas
                outSig->AcAlarmInd.acAlarmData[5].type = AC_ALARM_SIGNAL_TOO_LOW;
                outSig->AcAlarmInd.acAlarmData[5].direction = DOWNLINK;
                outSig->AcAlarmInd.acAlarmData[5].bitmap[bitMapGroup] |= acAlarmSignalTooLow;
                TPT_INFO(STR("DownLink Signal Too Low, port %d.", i));
            }
        }
    }

    TPT_INFO(STR("antpnum = %d, carrierID = %d, acAlarmDataLength = %d",
                  outSig->AcAlarmInd.antpnum, outSig->AcAlarmInd.carrierID, outSig->AcAlarmInd.acAlarmDataLength));

    for(U16 j=0; j<acAlarmDataLength; j++)
    {
        TPT_INFO(STR("type = %d, direction = %d, bitmap = %04x%04x %04x%04x %04x%04x %04x%04x", outSig->AcAlarmInd.acAlarmData[j].type, outSig->AcAlarmInd.acAlarmData[j].direction,
	                outSig->AcAlarmInd.acAlarmData[j].bitmap[0], outSig->AcAlarmInd.acAlarmData[j].bitmap[1],
	                outSig->AcAlarmInd.acAlarmData[j].bitmap[2], outSig->AcAlarmInd.acAlarmData[j].bitmap[3],
	                outSig->AcAlarmInd.acAlarmData[j].bitmap[4], outSig->AcAlarmInd.acAlarmData[j].bitmap[5],
	                outSig->AcAlarmInd.acAlarmData[j].bitmap[6], outSig->AcAlarmInd.acAlarmData[j].bitmap[7]
                ));
    }

    itc_send(&outSig, app_fault_mgr_mbox, ITC_MY_MBOX);
#if 0
    //only report fault field
    AcAlarmDeteilS_t detail[acSes_p->acAlarmDataLength];
    acAlarmDataLength = 0;
	U16 distrubedDL = 0;
	U16 distrubedUL = 0;
	U16 sigTooLowDL = 0;
	U16 sigTooLowUL = 0;
	U16 ampliDL = 0;
	U16 ampliUL = 0;

    for(U16 j=0; j<acSes_p->acAlarmDataLength; j++)
    {
        detail[j].type = 0;
        detail[j].direction = 0;
        detail[j].bitmap[0] = 0;
        detail[j].bitmap[1] = 0;
        detail[j].bitmap[2] = 0; 
        detail[j].bitmap[3] = 0;
        detail[j].bitmap[4] = 0; 
        detail[j].bitmap[5] = 0;
        detail[j].bitmap[6] = 0; 
        detail[j].bitmap[7] = 0;
    }


    for (U16 i = 0; i < ULCELLPE_AC_MAX_ANT_PER_CELL; i++)
    {
        if(acSes_p->acPortInfo[0][i].carrActive && acSes_p->acPortInfo[1][i].carrActive )
        {
            /* DownLink distrubed */
            acAlarmDisturbed = acSes_p->acPortInfo[DOWNLINK][i].pathFault & (AC_CTRL_FAULT_PHASE | AC_CTRL_FAULT_DELAY);
            if(acAlarmDisturbed > 0) //need to set alarm disturbed
            {
                bitMapGroup = i/16; //AcAlarmDeteilS_t->bitmap size is U16, means one group for 16 antenna, total 8 array, support 128 antenna
                acAlarmDisturbed = 1<< (i % 16); //set disturbed bit in the corresponding antenna bit, bitmap size is U16, only 16bit, arrary size is 8, can support 128 antennas
                detail[0].type = AC_ALARM_DISTURBED;
                detail[0].direction = DOWNLINK;
                detail[0].bitmap[bitMapGroup] |= acAlarmDisturbed;
                distrubedDL = 1;
                TPT_INFO(STR("DownLink distrubed, port %d.", i));
            }

            acAlarmDisturbed = acSes_p->acPortInfo[UPLINK][i].pathFault & (AC_CTRL_FAULT_PHASE | AC_CTRL_FAULT_DELAY);
            if(acAlarmDisturbed > 0)
            {
                bitMapGroup = i/16; //AcAlarmDeteilS_t->bitmap size is U16, means one group for 16 antenna, total 8 array, support 128 antenna
                acAlarmDisturbed = 1 << (i % 16); //set disturbed bit in the corresponding antenna bit, bitmap size is U16, only 16bit, arrary size is 8, can support 128 antennas
                detail[1].type = AC_ALARM_DISTURBED;
                detail[1].direction = UPLINK;
                detail[1].bitmap[bitMapGroup] |= acAlarmDisturbed;
                distrubedUL = 1;
                TPT_INFO(STR("UpLink distrubed, port %d.", i));
            }

            /* DownLink Signal Too Low */
            acAlarmSignalTooLow = acSes_p->acPortInfo[DOWNLINK][i].pathFault & AC_CTRL_FAULT_SIG_TOO_LOW;
            if(acAlarmSignalTooLow > 0)
            {
                bitMapGroup = i/16; //AcAlarmDeteilS_t->bitmap size is U16, means one group for 16 antenna, total 8 array, support 128 antenna
                acAlarmSignalTooLow = 1 << (i % 16); //set signal too low bit in the corresponding antenna bit, bitmap size is U16, only 16bit, arrary size is 8, can support 128 antennas
                detail[2].type = AC_ALARM_SIGNAL_TOO_LOW;
                detail[2].direction = DOWNLINK;
                detail[2].bitmap[bitMapGroup] |= acAlarmSignalTooLow;
                sigTooLowDL++;
                TPT_INFO(STR("DownLink Signal Too Low, port %d.", i));
            }

            /* Uplink Signal Too Low */
            acAlarmSignalTooLow = acSes_p->acPortInfo[UPLINK][i].pathFault & AC_CTRL_FAULT_SIG_TOO_LOW;
            if(acAlarmSignalTooLow > 0)
            {
                bitMapGroup = i/16; //AcAlarmDeteilS_t->bitmap size is U16, means one group for 16 antenna, total 8 array, support 128 antenna
                acAlarmSignalTooLow = 1 << (i % 16); //set signal too low bit in the corresponding antenna bit, bitmap size is U16, only 16bit, arrary size is 8, can support 128 antennas
                detail[3].type = AC_ALARM_SIGNAL_TOO_LOW;
                detail[3].direction = UPLINK;
                detail[3].bitmap[bitMapGroup] |= acAlarmSignalTooLow;
                sigTooLowUL++;
                TPT_INFO(STR("UpLink Signal Too Low, port %d.", i));
            }

            /* DownLink Amplitude */
            acAlarmAmplitude= acSes_p->acPortInfo[DOWNLINK][i].pathFault & AC_CTRL_FAULT_AMPLITUDE;
            if(acAlarmAmplitude > 0)
            {
                bitMapGroup = i/16; //AcAlarmDeteilS_t->bitmap size is U16, means one group for 16 antenna, total 8 array, support 128 antenna
                acAlarmAmplitude = 1 << (i % 16); //set amplitude bit in the corresponding antenna bit, bitmap size is U16, only 16bit, arrary size is 8, can support 128 antennas
                detail[4].type = AC_ALARM_AMPLITUDE;
                detail[4].direction = DOWNLINK;
                detail[4].bitmap[bitMapGroup] |= acAlarmAmplitude;
                ampliDL = 0;
                TPT_INFO(STR("DownLink Amplitude, port %d.", i));
            }

            /* Uplink Amplitude */
            acAlarmAmplitude = acSes_p->acPortInfo[UPLINK][i].pathFault & AC_CTRL_FAULT_AMPLITUDE;
            if(acAlarmAmplitude > 0)
            {
                bitMapGroup = i/16; //AcAlarmDeteilS_t->bitmap size is U16, means one group for 16 antenna, total 8 array, support 128 antenna
                acAlarmAmplitude = 1 << (i % 16); //set amplitude bit in the corresponding antenna bit, bitmap size is U16, only 16bit, arrary size is 8, can support 128 antennas
                detail[5].type = AC_ALARM_AMPLITUDE;
                detail[5].direction = UPLINK;
                detail[5].bitmap[bitMapGroup] |= acAlarmAmplitude;
                ampliDL = 0;
                TPT_INFO(STR("UpLink Amplitude, port %d.", i));
            }
        }
    }

/*
    acAlarmDataLength = 0;
	U16 distrubedDL = 0;
	U16 distrubedUL = 0;
	U16 sigTooLowDL = 0;
	U16 sigTooLowUL = 0;
	U16 ampliDL = 0;
	U16 ampliUL = 0;
*/
    if (distrubedDL)
    {
        acAlarmDataLength++;
    }
    if (distrubedUL)
    {
        acAlarmDataLength++;
    }
    if (sigTooLowDL)
    {
        acAlarmDataLength++;
    }
    if (sigTooLowUL)
    {
        acAlarmDataLength++;
    }
    if (ampliDL)
    {
        acAlarmDataLength++;
    }
    if (ampliUL)
    {
        acAlarmDataLength++;
    }

    outSig = itc_alloc(sizeof(AcAlarmIndS_t) + (acAlarmDataLength-1)*sizeof(AcAlarmDeteilS_t), TR_AC_FAULT_REPORT);
    outSig->AcAlarmInd.antpnum = ULCELLPE_AC_MAX_ANT_PER_CELL;
    outSig->AcAlarmInd.carrierID = acSes_p->carrierId;
    outSig->AcAlarmInd.deviceID = acSes_p->deviceId;
    outSig->AcAlarmInd.acAlarmDataLength = acAlarmDataLength;
    TPT_INFO(STR("antpnum = %d, carrierID = %d, deviceID = %d, acAlarmDataLength = %d",
                  outSig->AcAlarmInd.antpnum, outSig->AcAlarmInd.carrierID, outSig->AcAlarmInd.deviceID, outSig->AcAlarmInd.acAlarmDataLength));

	U16 index = 0;
    for(U16 j=0; j<acSes_p->acAlarmDataLength; j++)
    {
    	if (detail[j].bitmap[0] || detail[j].bitmap[1] || detail[j].bitmap[2] || detail[j].bitmap[3]
         || detail[j].bitmap[4] || detail[j].bitmap[5] || detail[j].bitmap[6] || detail[j].bitmap[7])
        {
            outSig->AcAlarmInd.acAlarmData[index].type = detail[j].type;
            outSig->AcAlarmInd.acAlarmData[index].direction = detail[j].direction;
            outSig->AcAlarmInd.acAlarmData[index].bitmap[0] = detail[j].bitmap[0];
            outSig->AcAlarmInd.acAlarmData[index].bitmap[1] = detail[j].bitmap[1];
            outSig->AcAlarmInd.acAlarmData[index].bitmap[2] = detail[j].bitmap[2];
            outSig->AcAlarmInd.acAlarmData[index].bitmap[3] = detail[j].bitmap[3];
            outSig->AcAlarmInd.acAlarmData[index].bitmap[4] = detail[j].bitmap[4];
            outSig->AcAlarmInd.acAlarmData[index].bitmap[5] = detail[j].bitmap[5];
            outSig->AcAlarmInd.acAlarmData[index].bitmap[6] = detail[j].bitmap[6];
            outSig->AcAlarmInd.acAlarmData[index].bitmap[7] = detail[j].bitmap[7];

            TPT_INFO(STR("type = %d, direction = %d, bitmap = %04x%04x %04x%04x %04x%04x %04x%04x", 
                    outSig->AcAlarmInd.acAlarmData[index].type, 
                    outSig->AcAlarmInd.acAlarmData[index].direction,
	                outSig->AcAlarmInd.acAlarmData[index].bitmap[0], outSig->AcAlarmInd.acAlarmData[index].bitmap[1],
	                outSig->AcAlarmInd.acAlarmData[index].bitmap[2], outSig->AcAlarmInd.acAlarmData[index].bitmap[3],
	                outSig->AcAlarmInd.acAlarmData[index].bitmap[4], outSig->AcAlarmInd.acAlarmData[index].bitmap[5],
	                outSig->AcAlarmInd.acAlarmData[index].bitmap[6], outSig->AcAlarmInd.acAlarmData[index].bitmap[7]));

            index++;
        }
    }

    itc_send(&outSig, app_fault_mgr_mbox, ITC_MY_MBOX);
#endif
}

bool AcDispatcher::reportAcFactor(ULCELLPE_ACSES_classS* acSes_p)
{
    U16 i = 0;
    U32 chanScdBNum = 0;
    acSes_p->chanSCdBSum = 0;
    TPT_INFO(STR("AcDispatcher reportAcFactor carrier%d, referPort is port%d", 
                acSes_p->carrierId, acSes_p->acReferAntIndex));
    /* when AC done, calculate the CalibFactorTrueValue, it is used for BB compensation*/
    for (i = 0; i < ULCELLPE_AC_MAX_ANT_PER_CELL; i++)
    {
        if(acSes_p->acPortInfo[0][i].carrActive && acSes_p->acPortInfo[1][i].carrActive )
        {
            TPT_INFO(STR("AcDispatcher ReportAcFactor update adjustvalue carrier%d, port%d.", 
                        acSes_p->carrierId, i));
            if (!(acSes_p->acPortInfo[0][i].pathFault))
            {
                /*acSes_p->acCalibFactorTrueValue[0][i].delay = 
                                  acSes_p->acCalibFactor[0][i].delay - 
                                  (acSes_p->acCalibFactor[0][acSes_p->acReferAntIndex].delay)/2;*/
                acSes_p->acCalibFactorTrueValue[0][i].delay = 
                                  acSes_p->acCalibFactor[0][i].delay;
                acSes_p->acCalibFactorTrueValue[0][i].initPhase = 
                                  acSes_p->acCalibFactor[0][i].initPhase;
                acSes_p->acCalibFactorTrueValue[0][i].amplitude = 
                                  acSes_p->acCalibFactor[0][i].amplitude;
                acSes_p->acCalibFactorTrueValue[0][i].channelSumCentidB = 
                                  acSes_p->acCalibFactor[0][i].channelSumCentidB;
                acSes_p->chanSCdBSum += acSes_p->acCalibFactor[0][i].channelSumCentidB;
                chanScdBNum++;
            }
            if (!(acSes_p->acPortInfo[1][i].pathFault))
            {
                acSes_p->acCalibFactorTrueValue[1][i].delay = 
                                  acSes_p->acCalibFactor[1][i].delay; 
                acSes_p->acCalibFactorTrueValue[1][i].initPhase = 
                                  acSes_p->acCalibFactor[1][i].initPhase;
                acSes_p->acCalibFactorTrueValue[1][i].amplitude = 
                                  acSes_p->acCalibFactor[1][i].amplitude;
                acSes_p->acCalibFactorTrueValue[1][i].channelSumCentidB = 
                                  acSes_p->acCalibFactor[1][i].channelSumCentidB;
                acSes_p->chanSCdBSum += acSes_p->acCalibFactor[1][i].channelSumCentidB;
                chanScdBNum++;
            }
        }
    }
    if (chanScdBNum != 0)
    {
        acSes_p->chanSCdBAvg = acSes_p->chanSCdBSum/chanScdBNum;
    }
    else
    {
        acSes_p->chanSCdBAvg = 0;
    }
    //acSes_p->chanSCdBAvg = acSes_p->chanSCdBSum/chanScdBNum;
    TPT_INFO(STR("ReportAcFactor carrier%d, chanSCdBAvg %d, dBSum %d, dBNum %d", 
                acSes_p->carrierId, acSes_p->chanSCdBAvg, 
                acSes_p->chanSCdBSum, chanScdBNum));
    return true;
}

bool AcDispatcher::calcDucDdc(U32 use_freq, U32 *duc, U32 *ddc)
{
    U32 cen_freq = CEN_FREQ;
    double delta_freq = 0;

    TPT_INFO(STR("use_freq %d, cen_freq %d, delta freq %d.", 
        use_freq,
        cen_freq,
        (use_freq - cen_freq)));

    if (use_freq < cen_freq)
    {
        delta_freq = cen_freq - use_freq;
        delta_freq /= 1000; //change to MHZ
        *duc = round((1-delta_freq/245.76)*pow(2,32));
        *ddc = round((1-delta_freq/122.88)*pow(2,32));
    }
    else
    {
        delta_freq = use_freq - cen_freq;
        delta_freq /= 1000; //change to MHZ
        *duc = round(delta_freq/245.76*pow(2,32));
        *ddc = round(delta_freq/122.88*pow(2,32));
    }

    return true;
}

bool AcDispatcher::sendAcParaToFpga(U16 carrierId)
{
    std::list<ULCELLPE_ACSES_classS *>::iterator it;

    for (it = acSesList.begin(); it != acSesList.end(); ++it)
    {
        if((*it)->carrierId == carrierId)
        {
            //TRACE(2, STR("AcDispatcher: find AC session carrierId %d", carrierId));
            break;
        }
    }
    if(it == acSesList.end())
    {
        TPT_INFO(STR("AcDispatcher: Can't find AC session carrierId %d", carrierId));
        return false;
    }
    ULCELLPE_ACSES_classS * acSes_p = *it;
    //U32 acReg1Addr = 0;
    //U32 acReg2Addr = 0;
    //fpga->write(acReg1Addr, (U32)carrierId);
    //fpga->write(acReg2Addr, (U32)acCounter);

    TPT_INFO(STR("Set FPGA AC Para Register, carrierId %d.", carrierId));
    //data source, only for test now.
    //fpga->write(MM_B_AC_DUC_NCO_CTL_CFG, (U32)0x50);
    //configure ordered number
    //fpga->write(MM_B_AC_BP_DEBUG, (U32)0x50);
    //configure fpga loop back data.
    fpga->write(MM_B_AC_BP_DEBUG, (U32)0x00);
    //unstart AC
    fpga->write(MM_B_AC_START_FLAG, (U32)0x00);
    fpga->write(MM_B_AC_PS_INT_DELAY_TIME, (U32)0x0000000F);
    //AC mem addr
    fpga->write(MM_B_AC_TRANSFER_BASE_ADDR, (U32)acMemBaseAddress);
    //Antenna Bit Map for 64 Antennas
    fpga->write(MM_B_AC_ANT_INDICATOR0, acSes_p->antBitmap[0]);
    fpga->write(MM_B_AC_ANT_INDICATOR1, acSes_p->antBitmap[1]);
    fpga->write(MM_B_AC_ANT_INDICATOR2, (U32)0x00000000);
    fpga->write(MM_B_AC_ANT_INDICATOR3, (U32)0x00000000);
    //carrier ID
    fpga->write(MM_B_AC_CARRIER_NUM, (U32)acSes_p->carrierId);

    //carrier Frequency
    //#define MM_B_AC_DUC_NCO_PH_INC (uint32_t)0x0000200f
    //#define MM_B_AC_DDC_NCO_PH_INC (uint32_t)0x00002019
    U32 duc = 0;
    U32 ddc = 0;
    calcDucDdc(acSes_p->carrierFrequency, &duc, &ddc);
    fpga->write(MM_B_AC_DUC_NCO_PH_INC, (U32)duc);
    fpga->write(MM_B_AC_DDC_NCO_PH_INC, (U32)ddc);
    TPT_INFO(STR("Set DUC 0x%08x DDC 0x%08x.", duc, ddc));

    usleep(1000);
    //start AC
    fpga->write(MM_B_AC_START_FLAG, (U32)0x01);
    TPT_INFO(STR("Start AC for CarrierId %d.", carrierId));
    return true;
}

// Reset the delay register to default value 0x40 on DUC/DDC TRXM
bool AcDispatcher::sendResetDelayRegToRadio(ULCELLPE_ACSES_classS* acSes_p, U16 startNum)
{
    //remote hunt to Radio APP in TRXM board.
    //content includes carrierId, portId, receiveTransmit, acFactor.delay.
    //refer to msg DC_TR_MODIFY_CARRIER_DELAY_REQ.
    bool sendFlag = false;
    U16 i = 0;
    U16 direct = 0;
    
    for (i = startNum; i < ULCELLPE_AC_MAX_ANT_PER_CELL; i++)
    {
        if(acSes_p->acPortInfo[0][i].carrActive 
                && acSes_p->acPortInfo[1][i].carrActive)
        {
            if (!(acSes_p->acPortInfo[0][i].resetDelayRegReqSendFlag) )
            {
                direct = 0;
                acSes_p->acPortInfo[0][i].resetDelayRegReqSendFlag = 1;
                sendFlag = true;
                break;
            }
            if (!(acSes_p->acPortInfo[1][i].resetDelayRegReqSendFlag))
            {
                direct = 1;
                acSes_p->acPortInfo[1][i].resetDelayRegReqSendFlag = 1;
                sendFlag = true;
                break;
            }
        }

    }
    if(sendFlag)
    {
        /*U32 timerContent: 0xF_F_FF_FFFF indicates contentID_Direct_PortId_CarrierId*/
        U32 timerContent = 0;
        /* 0 indicates carrierId for periodAC, 1 indicates pre-AC msg timerOut.*/
        U32 contentId = ULCELLPE_ACSES_RESET_DELAY_REG_MSG;    
        U32 contDirect = direct;
        U32 contPort = i;
        U32 contCarrier = acSes_p->carrierId;
        timerContent = (contentId<<28) +  (contDirect<<24) + (contPort<<16) + contCarrier;
        acSes_p->resetDelayRegTimer = createTimer(PRE_AC_TIME_OUT, timerContent, ac_app_mbox);
        if(acSes_p->resetDelayRegTimer == CREATE_TIMER_FAIL)
        {
            TPT_INFO(STR("Create reset delay register timer fail. \n"));
        }
        TPT_INFO(STR("Send reset delay register msg to TRXM Radio APP for carrier %d, port %d, direct %d, delay %d, delay %dns.", 
                        acSes_p->carrierId, i, direct, 0, 0));

        union itc_msg *outSig;
        outSig = itc_alloc(sizeof(DcTrModifyCarrierDelayReqS), DC_TR_MODIFY_CARRIER_DELAY_REQ);
        outSig->dcTrModifyCarrierDelayReq.addressInfo.serverRef
                    = acSes_p->carrierFrequency;

        outSig->dcTrModifyCarrierDelayReq.deviceId = i;
        if( direct == 0)
        {
            //TX
            //outSig->dcTrModifyCarrierDelayReq.addressInfo.clientRef = 1;
            outSig->dcTrModifyCarrierDelayReq.ratType = 1;
        }
        else
        {
            //RX
            //outSig->dcTrModifyCarrierDelayReq.addressInfo.clientRef = 0;
            outSig->dcTrModifyCarrierDelayReq.ratType = 0;
        }

        // set delay compensation to 0 as default
        outSig->dcTrModifyCarrierDelayReq.carrierDeltaDelay = 0;

        TPT_INFO(STR(" %s, radio_bp_mbox = 0x%x", __func__, radio_bp_mbox));

        if(!readyRadioApp)
        {
            if(!get_mbox(RADIO_BP_MAILBOX, &radio_bp_mbox))
            {
                readyRadioApp = false;
                TPT_ERROR(STR("Radio APP BP Mailbox is not ready. %d", radio_bp_mbox));
                //return false;
            }
            else
            {    
                readyRadioApp = true;
            }
        }

        if(readyRadioApp)
        {
            itc_send(&outSig, radio_bp_mbox, ITC_MY_MBOX);
        }
    }
    else
    {
        //all carrier/port/TX/RX reset delay register on TRXM request msg send is done. print summary info.
        TPT_INFO(STR("------Reset delay register on TRXM msg handling is done, Result:------"));
        for (i = 0; i < ULCELLPE_AC_MAX_ANT_PER_CELL; i++)
        {
            if(acSes_p->acPortInfo[0][i].carrActive 
                    && acSes_p->acPortInfo[1][i].carrActive )
           {

                direct = 0;
                TPT_INFO(STR("CarrierId %4d, port %2d, direct %d status: send %d, recv %d.", 
                                acSes_p->carrierId, i, direct,
                                acSes_p->acPortInfo[0][i].resetDelayRegReqSendFlag, 
                                acSes_p->acPortInfo[0][i].resetDelayRegReqSendFlag));
                direct = 1;
                TPT_INFO(STR("CarrierId %4d, port %2d, direct %d status: send %d, recv %d.", 
                                acSes_p->carrierId, i, direct,
                                acSes_p->acPortInfo[1][i].resetDelayRegReqSendFlag, 
                                acSes_p->acPortInfo[1][i].resetDelayRegReqSendFlag));

                acSes_p->acPortInfo[0][i].resetDelayRegReqSendFlag = 0;
                acSes_p->acPortInfo[1][i].resetDelayRegReqSendFlag = 0;
                acSes_p->acPortInfo[0][i].resetDelayRegRespRecvFlag = 0;
                acSes_p->acPortInfo[1][i].resetDelayRegRespRecvFlag = 0;                
           }
        }

    }
    return true;
}


bool AcDispatcher::sendAcFactorToRadio(ULCELLPE_ACSES_classS* acSes_p, U16 startNum)
{
    //remote hunt to Radio APP in TRXM board.
    //content includes carrierId, portId, receiveTransmit, acFactor.delay.
    //refer to msg DC_TR_MODIFY_CARRIER_DELAY_REQ.
    bool preAcFlag = false;
    U16 i = 0;
    U16 direct = 0;
    for (i = startNum; i < ULCELLPE_AC_MAX_ANT_PER_CELL; i++)
    {
        if(acSes_p->acPortInfo[0][i].carrActive 
                && acSes_p->acPortInfo[1][i].carrActive)
        {
            if (!(acSes_p->acPortInfo[0][i].preAcReqSendFlag)
                && (!acSes_p->acPortInfo[0][i].pathFault) )
            {
                direct = 0;
                acSes_p->acPortInfo[0][i].preAcReqSendFlag = 1;
                preAcFlag = true;
                break;
            }
            if (!(acSes_p->acPortInfo[1][i].preAcReqSendFlag)
                && (!acSes_p->acPortInfo[1][i].pathFault))
            {
                direct = 1;
                acSes_p->acPortInfo[1][i].preAcReqSendFlag = 1;
                preAcFlag = true;
                break;
            }
        }
    }
    if(preAcFlag)
    {
        /*U32 timerContent: 0xF_F_FF_FFFF indicates contentID_Direct_PortId_CarrierId*/
        U32 timerContent = 0;
        /* 0 indicates carrierId for periodAC, 1 indicates pre-AC msg timerOut.*/
        U32 contentId = ULCELLPE_ACSES_PRE_AC_MSG;    
        U32 contDirect = direct;
        U32 contPort = i;
        U32 contCarrier = acSes_p->carrierId;
        timerContent = (contentId<<28) +  (contDirect<<24) + (contPort<<16) + contCarrier;
        acSes_p->preAcTimer = createTimer(PRE_AC_TIME_OUT, timerContent, ac_app_mbox);
        if(acSes_p->preAcTimer == CREATE_TIMER_FAIL)
        {
            TPT_INFO(STR("Create preAC timer fail. \n"));
        }
        TPT_INFO(STR("Send Pre-AC msg to TRXM Radio APP for carrier %d, port %d, direct %d, delay %d, delay %dns.", 
                        acSes_p->carrierId, i, direct, 
                        acSes_p->acCalibFactorTrueValue[direct][i].delay,
                        (acSes_p->acCalibFactorTrueValue[direct][i].delay/16)));

        /*calculate pre-ac delay
          One TRXM has one delayFft, each antenna has own acDly
          delayFft = delay/32.55ns
          acDly = delay - 32.55*delayFft
        */
        U32 delayFft = 0;
        U32 acDly = 0;
        U8 flagFft = 0;
        U8 flagDly = 0;
        bool ret = 0;
        ret = calcPreAcDelay(acSes_p, direct, i, &delayFft, &flagFft, &acDly, &flagDly);
        if (ret == false)
        {
            /*limit reached, do not send to TRXM*/
            return true;
        }

        /*send to TRXM*/
        TPT_INFO(STR("direct %d, port %2d, delayFft %d, flagFft %d, acDly %d, flagDly %d",
                      direct, i, delayFft, flagFft, acDly, flagDly));
        union itc_msg *outSig;
        outSig = itc_alloc(sizeof(DcTrModifyCarrierDelayReqS), DC_TR_MODIFY_CARRIER_DELAY_REQ);
        outSig->dcTrModifyCarrierDelayReq.addressInfo.serverRef
                    = acSes_p->carrierFrequency;
        //outSig->dcTrModifyCarrierDelayReq.ratType
        //            = acSes_p->carrierId;
        outSig->dcTrModifyCarrierDelayReq.deviceId = i;
        if( direct == 0)
        {
            //TX
            //outSig->dcTrModifyCarrierDelayReq.addressInfo.clientRef = 1;
            outSig->dcTrModifyCarrierDelayReq.ratType = 1;
        }
        else
        {
            //RX
            //outSig->dcTrModifyCarrierDelayReq.addressInfo.clientRef = 0;
            outSig->dcTrModifyCarrierDelayReq.ratType = 0;
        }
        /*outSig->dcTrModifyCarrierDelayReq.carrierDeltaDelay 
                    = acSes_p->acCalibFactorTrueValue[direct][i].delay*10;//0.1ns*/
        outSig->dcTrModifyCarrierDelayReq.carrierDeltaDelay
                    = (flagFft<<23) | (delayFft<<16) | (flagDly<<15) | (acDly);

        TPT_INFO(STR(" %s, radio_bp_mbox = 0x%x", __func__, radio_bp_mbox));

        if(!readyRadioApp)
        {
            if(!get_mbox(RADIO_BP_MAILBOX, &radio_bp_mbox))
            {
                readyRadioApp = false;
                TPT_ERROR(STR("Radio APP BP Mailbox is not ready. %d", radio_bp_mbox));
                //return false;
            }
            else
            {    
                readyRadioApp = true;
            }
        }

        if(readyRadioApp)
        {
            itc_send(&outSig, radio_bp_mbox, ITC_MY_MBOX);
        }
    }
    else
    {
        //changed AC Type
        acSes_p->acType = ULCELLPE_ACSES_ACTYPE_PERIOD;
        TPT_INFO(STR("Changed the acType from Pre-AC to Period AC for carrier %d", 
                acSes_p->carrierFrequency));
        //all carrier/port/TX/RX pre-AC request msg send is done. print summary info.
        TPT_INFO(STR("------Pre-AC msg handling is done, Result:------"));
        for (i = 0; i < ULCELLPE_AC_MAX_ANT_PER_CELL; i++)
        {
            if(acSes_p->acPortInfo[0][i].carrActive 
                    && acSes_p->acPortInfo[1][i].carrActive )
           {
                direct = 0;
                TPT_INFO(STR("CarrierId %4d, port %2d, direct %d status: send %d, recv %d.", 
                                acSes_p->carrierId, i, direct,
                                acSes_p->acPortInfo[0][i].preAcReqSendFlag, 
                                acSes_p->acPortInfo[0][i].preAcRespRecvFlag));
                direct = 1;
                TPT_INFO(STR("CarrierId %4d, port %2d, direct %d status: send %d, recv %d.", 
                                acSes_p->carrierId, i, direct,
                                acSes_p->acPortInfo[1][i].preAcReqSendFlag, 
                                acSes_p->acPortInfo[1][i].preAcRespRecvFlag));
           }
        }

        if (runPeriodAcImdt)
        {
            TPT_INFO(STR("Start Period AC immediately after Per-AC."));
            setAcFactorToFPGA(acSes_p);
            //sendPrachToRadio(acSes_p, 0);
        }
    }
    //usleep(1000);
    return true;
}

bool AcDispatcher::sendPrachToRadio(ULCELLPE_ACSES_classS* acSes_p, U16 startNum)
{
    U16 i = 0;
    for (i = startNum; i < ULCELLPE_AC_MAX_ANT_PER_CELL; i++)
    {
        if(acSes_p->acPortInfo[UPLINK][i].carrActive 
                && (!acSes_p->acPortInfo[UPLINK][i].pathFault) )
        {
            if (!(acSes_p->acPortInfo[UPLINK][i].setPrachReqSendFlag))
            {
                acSes_p->acPortInfo[UPLINK][i].setPrachReqSendFlag = 1;
                break;
            }
        }
    }

    if(i == ULCELLPE_AC_MAX_ANT_PER_CELL)
    {
        TPT_INFO(STR("All the prach already has beed sent to radio app, clean all prach send flag"));

        for (i = 0; i < ULCELLPE_AC_MAX_ANT_PER_CELL; i++)
        {
            acSes_p->acPortInfo[UPLINK][i].setPrachReqSendFlag = 0;
        }

        return true;
    }

    /*U32 timerContent: 0xF_F_FF_FFFF indicates contentID_Direct_PortId_CarrierId*/
    U32 timerContent = 0;
    U32 contentId = ULCELLPE_ACSES_SET_PRACH_MSG;    
    U32 contPort = i;
    U32 contCarrier = acSes_p->carrierId;
    timerContent = (contentId<<28) + (UPLINK << 24) | (contPort<<16) + contCarrier;
    acSes_p->setPrachTimer = createTimer(PRE_AC_TIME_OUT, timerContent, ac_app_mbox);
    if(acSes_p->setPrachTimer == CREATE_TIMER_FAIL)
    {
        TPT_ERROR(STR("Create setPrachTimer fail. \n"));
    }

    TPT_INFO(STR("Send set prach msg to TRXM Radio APP for carrier %d, port %d.", acSes_p->carrierId, i));

    /*calculate prach */
    U32 prach_real = 0;
    U32 prach_img = 0;
    U8 flag_real = 0;
    U8 flag_img = 0;

    calcAcPrach(acSes_p, i, &prach_real, &prach_img);
    if(prach_real < 0)
        flag_real = 1;
  
    if(prach_img < 0)
        flag_img = 1;

    TPT_INFO(STR("port %2d, prach_real %d, prach_img %d", i, prach_real, prach_img));

#if 0
    union itc_msg *outSig;
    outSig = itc_alloc(sizeof(DcTrModifyCarrierDelayReqS), DC_TR_MODIFY_CARRIER_DELAY_REQ);
    outSig->dcTrModifyCarrierDelayReq.addressInfo.serverRef
                = acSes_p->carrierFrequency;

    outSig->dcTrModifyCarrierDelayReq.deviceId = i; //Port

    outSig->dcTrModifyCarrierDelayReq.ratType = UPLINK;

    outSig->dcTrModifyCarrierDelayReq.carrierDeltaDelay  /* bit 31 = 1 means period AC */
                = (1 << 31) | (flag_real << 30) | ((prach_real&0x3FFF)<<16) | (flag_img<<15) | (prach_img&0x7FFF);

    if(readyRadioApp)
    {
        itc_send(&outSig, radio_bp_mbox, ITC_MY_MBOX);
    }
#endif
}

U32 AcDispatcher::calcDelayComp(ULCELLPE_ACSES_classS* acSes_p, U16 direct, U16 port)
{
    double value = acSes_p->acCalibFactorTrueValue[direct][port].delay;
    value -= (delay_db[port] - acSes_p->delayDbAvg);
    TPT_INFO(STR("func %s, hwdb log: delay_db[%d] = %f, delayDbAvg = %f, delta_t = %f, unit 1/16(ns)", __func__, port, delay_db[port], acSes_p->delayDbAvg, value)); 

    //value unit is 1/16 ns
    U32 delayComp = 0;
    double input = value;
    double delayCompDlb = 0;
    delayCompDlb = input/16.0; //unit: ns
    delayCompDlb = delayCompDlb*(1e-6)*15;
    if (value < 0) delayCompDlb++;
    delayCompDlb = delayCompDlb * pow(2,32);
    delayComp = round(delayCompDlb);
    return delayComp;
}
double AcDispatcher::calcPhaseComp(ULCELLPE_ACSES_classS* acSes_p, U16 direct, U16 port)
{
    //input unit is 180/32768 degree.
    double phaseCompDbl = 0;
    double phaseAlgo = acSes_p->acCalibFactorTrueValue[direct][port].initPhase;
    double phaseDb = phase_db[port] - acSes_p->phaseDbAvg;
    double delayDb = delay_db[port] - acSes_p->delayDbAvg;

    TPT_INFO(STR("func %s, hwdb log: phase_db[%d] = %f, phaseDbAvg = %f", __func__, port, phase_db[port], acSes_p->phaseDbAvg)); 

    U32 fc = acSes_p->carrierFrequency;/*KHZ*/
    fc -= 2576*pow(10,3); //kHz
    U16 numRe = acSes_p->numRe;
    double phaseCali = 0;

    phaseAlgo = phaseAlgo*180/32768.0;
    phaseDb = phaseDb*180/32768.0;
    delayDb = delayDb/16.0; //ns
    phaseCali = phaseDb + 360*(fc-15*numRe/2)*delayDb*pow(10,-6);

    phaseCompDbl = phaseAlgo - phaseCali;
    if (phaseCompDbl<0) phaseCompDbl=phaseCompDbl+360;

    TPT_INFO(STR("func %s, phaseDb = %f, phaseCali = %f, phaseCompDbl = %f", __func__, phaseDb, phaseCali, phaseCompDbl));

    return phaseCompDbl;
}

void AcDispatcher::calcAmplitDbAvg(ULCELLPE_ACSES_classS* acSes_p)
{
    U16 i = 0;
    double dbAMPUl = 0;
    double dbAMPDl = 0;
    U16 numAntUl = 0;
    U16 numAntDl = 0;

    for (i = 0; i < ULCELLPE_AC_MAX_ANT_PER_CELL; i++)
    {
        if(acSes_p->acPortInfo[0][i].carrActive && acSes_p->acPortInfo[1][i].carrActive )
        {
            if (!(acSes_p->acPortInfo[0][i].pathFault))/*DL*/
            {
                dbAMPDl += pow(amplit_db[i], 2);
                numAntDl++;
            }
            if (!(acSes_p->acPortInfo[1][i].pathFault))/*UL*/
            {
                dbAMPUl += pow(amplit_db[i], 2);
                numAntUl++;
            }
        }
    }
    dbAMPDl /= numAntDl;
    dbAMPUl /= numAntUl;
    acSes_p->amplitDbAvg[0] = sqrt(dbAMPDl);
    acSes_p->amplitDbAvg[1] = sqrt(dbAMPUl);

    TPT_INFO(STR("func %s, hwdb log: amplitDbAvg[0] = %f, amplitDbAvg[1] = %f", __func__,  acSes_p->amplitDbAvg[0], acSes_p->amplitDbAvg[1]));
}

void AcDispatcher::calcDelayDbAvg(ULCELLPE_ACSES_classS* acSes_p)
{
    U16 i = 0;
    double delaySum = 0;
    U16 numAnt = 0;

    for (i = 0; i < ULCELLPE_AC_MAX_ANT_PER_CELL; i++)
    {
        if(acSes_p->acPortInfo[0][i].carrActive && acSes_p->acPortInfo[1][i].carrActive
           && (!(acSes_p->acPortInfo[0][i].pathFault))
           && (!(acSes_p->acPortInfo[1][i].pathFault)))
        {
            delaySum += delay_db[i];
            numAnt++;
        }
    }

    acSes_p->delayDbAvg = delaySum/numAnt;

    TPT_INFO(STR("func %s, hwdb log: delayDbAvg = %f", __func__,  acSes_p->delayDbAvg));
}

void AcDispatcher::calcPhaseDbAvg(ULCELLPE_ACSES_classS* acSes_p)
{
    U16 i = 0;
    double phaseSum = 0;
    U16 numAnt = 0;

    for (i = 0; i < ULCELLPE_AC_MAX_ANT_PER_CELL; i++)
    {
        if(acSes_p->acPortInfo[0][i].carrActive && acSes_p->acPortInfo[1][i].carrActive
           && (!(acSes_p->acPortInfo[0][i].pathFault))
           && (!(acSes_p->acPortInfo[1][i].pathFault)))
        {
            phaseSum += phase_db[i];
            numAnt++;
        }
    }

    acSes_p->phaseDbAvg = phaseSum/numAnt;

    TPT_INFO(STR("func %s, hwdb log: phaseDbAvg = %f", __func__, acSes_p->phaseDbAvg));
}

double AcDispatcher::calcAmplitComp(ULCELLPE_ACSES_classS* acSes_p, U16 direct, U16 port)
{
    U16 i = 0;
    double ampDbCompX = 0;
    double ampX = 0;
    double ampAvg = 0;
    double gainComp = 0;
    U16 numAnt = 0;

    for (i = 0; i < ULCELLPE_AC_MAX_ANT_PER_CELL; i++)
    {
        if(acSes_p->acPortInfo[0][i].carrActive && acSes_p->acPortInfo[1][i].carrActive )
        {
            if (!(acSes_p->acPortInfo[direct][i].pathFault) 
                && !(acSes_p->acPortInfo[direct][i].faultRetryStatus[acSes_p->faultRetryCounter]))
            {
                ampDbCompX = acSes_p->amplitDbAvg[direct]/amplit_db[i];
                ampX = acSes_p->acCalibFactor[direct][i].amplitude;
                ampX *= ampDbCompX;
                ampAvg += pow(ampX, 2);
                numAnt++;
            }
        }
    }
    ampAvg /= numAnt;
    ampAvg = sqrt(ampAvg);
    /*TPT_INFO(STR("4. ampAvg %f, numAnt %d.", ampAvg, numAnt));*/

    ampDbCompX = acSes_p->amplitDbAvg[direct]/amplit_db[port];
    /*TPT_INFO(STR("5. ampDbCompX %f, direct %d, port %d.", ampDbCompX, direct, port));*/
    ampX = acSes_p->acCalibFactor[direct][port].amplitude;
    /*TPT_INFO(STR("6. ampX %f, direct %d, port %d.", ampX, direct, port));*/
    ampX *= ampDbCompX;
    gainComp = ampAvg/ampX;
    /*TPT_INFO(STR("7. gainComp %f.", gainComp));*/

    return gainComp;
}


bool AcDispatcher::calcPreAcDelay(ULCELLPE_ACSES_classS* acSes_p, U16 direct, U16 port, 
                                    U32 *delayFft, U8 *flagFft, U32 *acDly, U8 *flagDly)
{
    S32 fft = 0;
    S32 dly = 0;

    *delayFft = 0;
    *acDly = 0;
    *flagFft = 0;
    *flagDly = 0;

	fft = 0;
    /*uint is 1/16ns*/
    /*trans to ns*/
    dly = round(((double)acSes_p->acCalibFactorTrueValue[direct][port].delay)/16);

	/*check result*/
	if (dly < -512 || dly > 512)
	{
		TPT_INFO(STR("limit reached, do not send to TRXM: direct %d, port %2d, fft %d, dly %d",
                      direct, port, fft, dly));
		return false;
	}

	if (dly > 0)
    {
        *flagDly = 1;
    }
    *acDly = abs(dly);

    return true;
}

void AcDispatcher::calcAcPrach(ULCELLPE_ACSES_classS* acSes_p, U16 port, U32* prach_real, U32* prach_img)
{
    double amplit_algo = 0;
    double phase_value;
    double phase_amp = 0;
    double gainCompDbl = 0;
    S32 algo_delay = acSes_p->acCalibFactorTrueValue[UPLINK][port].delay;
    U16 i = 0;
    U32 delta_freq = 900000;
    S32 delta_time;

    phase_amp = calcPhaseComp(acSes_p, UPLINK, i);
    TPT_INFO(STR("acSes_p->amplitDbAvg[UPLINK]/amplit_db[%d] = %f/%d", port, acSes_p->amplitDbAvg[UPLINK], amplit_db[port]));
    amplit_algo = acSes_p->acCalibFactor[UPLINK][port].amplitude;
    gainCompDbl = calcAmplitComp(acSes_p, UPLINK, port);
    delta_time = (algo_delay - delay_db[i]) / 16 ; //ns
    TPT_INFO(STR("phase_amp = %f, amplit_algo = %f, gainCompDbl = %f, delta_time = %d", phase_amp,  amplit_algo, gainCompDbl, delta_time));
    phase_value = ((phase_amp + 360 * delta_freq * delta_time) / 180 ) * 3.14 ;
    *prach_real = gainCompDbl * cos(phase_value);
    *prach_img = gainCompDbl * sin(phase_value);
    TPT_INFO(STR("phase_value = %f, prach_real = %d, prach_img = %d", phase_value, prach_real, prach_img));
}

bool AcDispatcher::setAcFactorToFPGA(ULCELLPE_ACSES_classS* acSes_p)
{
    //remote hunt to Radio APP in TRXM board.
    //content includes carrierId, portId, receiveTransmit, acFactor.delay.
    //refer to msg DC_TR_MODIFY_CARRIER_DELAY_REQ.
    U16 i = 0;
    U16 direct = 0;
    U32 delayComp = 0;
    double phaseCompDbl = 0;
    U32 phaseComp = 0;
    double gainCompDbl = 0;
    U32 gainComp = 0;
    S32 gainDiff = 0;
    U32 carrOffset = 64;
    U32 portOffset = 1;
    U32 addr1 = 0;
    U32 addr2 = 0;
    U32 addr3 = 0;
    for (i = 0; i < ULCELLPE_AC_MAX_ANT_PER_CELL; i++)
    {
        if(acSes_p->acPortInfo[0][i].carrActive && acSes_p->acPortInfo[1][i].carrActive )
        {
            if (!(acSes_p->acPortInfo[0][i].pathFault))
            {
                //acSes_p->carrierId;
                //i;
                direct = 0;
                delayComp  = calcDelayComp(acSes_p, 0, i);
                phaseCompDbl = calcPhaseComp(acSes_p, 0, i);
                phaseCompDbl = (phaseCompDbl/360)*pow(2,32);
                phaseComp = round(phaseCompDbl);
                gainCompDbl = calcAmplitComp(acSes_p, 0, i);
                gainComp = round(16384*gainCompDbl);
                addr1 = MM_B_DL_AC_ADJUST_GAIN_C0_ANT_0+carrOffset*(acSes_p->carrierId)+portOffset*i;
                fpga->write(addr1, gainComp);
                addr2 = MM_B_DL_AC_ADJUST_PHASE_C0_ANT_0+carrOffset*(acSes_p->carrierId)+portOffset*i;
                fpga->write(addr2, (U32)phaseComp);
                addr3 = MM_B_DL_AC_ADJUST_DLY_C0_ANT_0+carrOffset*(acSes_p->carrierId)+portOffset*i;
                fpga->write(addr3, (U32)delayComp);

                TPT_INFO(STR("Set AcFactor Carrier %d, DL, Port %d: Delay %u, Phase %u, Amp %u.", 
                                acSes_p->carrierId, i, delayComp, phaseComp, gainComp));
                TPT_INFO(STR("Set AcFactor FPGA Addr: Delay 0x%08X, Phase 0x%08X, Amp 0x%08X.", 
                                addr3, addr2, addr1));
            }
            if (!(acSes_p->acPortInfo[1][i].pathFault))
            {
                //acSes_p->carrierId;
                //i;
                direct = 1;
                delayComp  = calcDelayComp(acSes_p, 1, i);
                phaseCompDbl = calcPhaseComp(acSes_p, 1, i);
                phaseCompDbl = (phaseCompDbl/360)*pow(2,32);
                phaseComp = round(phaseCompDbl);
                gainCompDbl = calcAmplitComp(acSes_p, 1, i);
                gainComp = round(16384*gainCompDbl);
                addr1 = MM_B_UL_AC_ADJUST_GAIN_C0_ANT_0+carrOffset*(acSes_p->carrierId)+portOffset*i;
                fpga->write(addr1, gainComp);
                addr2 = MM_B_UL_AC_ADJUST_PHASE_C0_ANT_0+carrOffset*(acSes_p->carrierId)+portOffset*i;
                fpga->write(addr2, (U32)phaseComp);
                addr3 = MM_B_UL_AC_ADJUST_DLY_C0_ANT_0+carrOffset*(acSes_p->carrierId)+portOffset*i;
                fpga->write(addr3, (U32)delayComp);

                TPT_INFO(STR("Set AcFactor Carrier %d, UL, Port %d: Delay %u, Phase %u, Amp %u.", 
                                acSes_p->carrierId, i, delayComp, phaseComp, gainComp));
                TPT_INFO(STR("Set AcFactor FPGA Addr: Delay 0x%08X, Phase 0x%08X, Amp 0x%08X.", 
                                addr3, addr2, addr1));
            }
        }
    }
    usleep(1000);
    return true;
}

bool AcDispatcher::displayAcSession()
{
    if(!acSesList.size())
    {
        printf("Empty list.\n");
        return false;
    }
	printf("AC SW version %s.", VER_INFO);
    printf("\nThere are %d AC session in the list: \n", acSesList.size());
    std::list<ULCELLPE_ACSES_classS *>::iterator it;
    for (it = acSesList.begin(); it != acSesList.end(); ++it)
    {
        printf("carrierId %d, state %d, acReferPort %d, intervalPeriod %ds : \n", 
            (*it)->carrierId, 
            (*it)->state, 
            (*it)->acReferAntIndex, 
            intervalPeriod);
        for (U16 i = 0; i < ULCELLPE_AC_MAX_ANT_PER_CELL; i++)
        {
            if((*it)->acPortInfo[0][i].carrActive && (*it)->acPortInfo[1][i].carrActive )
            {
                printf("port %03d DL/UL carrier is activated.\n", i);
                printf("\t DL AcDoneStatus %d, pathFault 0x%02X.\n", 
                        (*it)->acPortInfo[0][i].acDoneStatus,
                        (*it)->acPortInfo[0][i].pathFault );
                printf("\t UL AcDoneStatus %d, pathFault 0x%02X.\n", 
                        (*it)->acPortInfo[1][i].acDoneStatus,
                        (*it)->acPortInfo[1][i].pathFault );
            }
        }
    }
    return true;
}

void AcDispatcher::setAcFreqDataSrc(union itc_msg * rec_p)
{
    U16 dataSrc = 0;
    dataSrc = rec_p->trAcDataSrc.dataSrc;
    //printf("acTest Set AcFreqDataSource is %d\n", dataSrc);
    printf("dataSrc:FPGA %d, DATA_SUC %d, DATA_FAIL_DELAY %d, DATA_FAIL_DIST %d, DATA_SIG_LOW %d," 
           " EVM(-20)Signal %d, Single Tone %d, Original Good data %d, EVM(-15)Signal %d.\n",
		AC_FREQ_DATA_FROM_FPGA, 
		AC_FREQ_DATA_SUCCESS,
		AC_FREQ_DATA_FAILURE_DELAY,
		AC_FREQ_DATA_FAILURE_DISTURB,
		AC_FREQ_DATA_FAILURE_SIGLOW, 5, 6, 7, 8);

    setAcFreqDataSrc(dataSrc);
}

void AcDispatcher::setAcFreqDataSrc(U16 dataSrc)
{
    if(dataSrc <= AC_FREQ_DATA_FAILURE_SIGLOW)
    {
        acFreqDataSrc = dataSrc;
        TPT_INFO(STR("DataSource Set AcFreqDataSource is %d", acFreqDataSrc));
    }
    else if (dataSrc == 5)
    {
        //AC Start Sequence data for EVM -20dBFS
        //testEVMdata();
        TPT_INFO(STR("AC Seq EVM data is initializing."));
        //configAcSeqData(complexInputEVM20Mhz, 
        //        SIZE_OF_AC_TIME_DOMAIN_FFT_SIZE);
        configAcSeqData(complexInputEVM20dBFS20M, 
                SIZE_OF_AC_TIME_DOMAIN_FFT_SIZE);
        TPT_INFO(STR("DataSource Set AC Start Seq EVM(-20dBFS) Signal %d", dataSrc));
        printf("DataSource Set AC Start Seq EVM(-20dBFS) Signal %d\n", dataSrc);
    }
    else if (dataSrc == 6)
    {
        //AC start Sequence data for signle tone
        //testSingleToneData();
        TPT_INFO(STR("AC Seq Single Tone is initializing."));
        configAcSeqData(complexInputSingleTone, 
                SIZE_OF_AC_TIME_DOMAIN_FFT_SIZE);
        TPT_INFO(STR("DataSource Set AC Start Sequence data Single Tone %d", dataSrc));
        printf("DataSource Set AC Start Sequence data Single Tone %d\n", dataSrc);
    }
    else if (dataSrc == 7)
    {
        //AC start Sequence data for Good data
        //initAcSeqDataInFPGA();
        TPT_INFO(STR("AC Seq Original data is initializing."));
        configAcSeqData(complexInputTimeSignal20Mhz, 
                SIZE_OF_AC_TIME_DOMAIN_FFT_SIZE);
        TPT_INFO(STR("DataSource Set AC Start Sequence data Original Good %d", dataSrc));
        printf("DataSource Set AC Start Sequence data Original Good %d\n", dataSrc);
    }
    else if (dataSrc == 8)
    {
        //AC Start Sequence data for EVM -15dBFS
        //testEVMdata();
        TPT_INFO(STR("AC Seq EVM data is initializing."));
        //configAcSeqData(complexInputEVM20Mhz, 
        //        SIZE_OF_AC_TIME_DOMAIN_FFT_SIZE);
        configAcSeqData(complexInputEVM15dBFS20M, 
                SIZE_OF_AC_TIME_DOMAIN_FFT_SIZE);
        TPT_INFO(STR("DataSource Set AC Start Seq EVM(-15dBFS) Signal %d", dataSrc));
        printf("DataSource Set AC Start Seq EVM(-15dBFS) Signal %d\n", dataSrc);
    }
    else
    {
        TPT_INFO(STR("AC data source is invalid: %d", dataSrc));
    }
}

void AcDispatcher::getAcFreqDataSrc(U16 & dataSrc)
{
    dataSrc = acFreqDataSrc;
    TPT_INFO(STR("AcDispatcher Get AcFreqDataSource is %d", acFreqDataSrc));
}

int AcDispatcher::testCmdHandler(int argc, char **argv)
{
    if (argc == 2 && (strcmp("all", argv[1]) == 0))
    {
        AcDispatcher* pDrv = AcDispatcher::getInstance();
        if(!pDrv)
        {
            printf("Can not get %s driver",argv[0]);
            return RET_SUCCESS;
        }
        if(!pDrv->acSesList.size())
        {
            printf("Empty list.\n");
            return RET_SUCCESS;
        }
        printf("\nThere are %d AC session in the list: \n", pDrv->acSesList.size());
        std::list<ULCELLPE_ACSES_classS *>::iterator it;
        for (it = pDrv->acSesList.begin(); it != pDrv->acSesList.end(); ++it)
        {
            printf("carrierId %d, state %d, acReferPort %d, intervalPeriod %ds: \n", 
                (*it)->carrierId, 
                (*it)->state, 
                (*it)->acReferAntIndex, 
                pDrv->intervalPeriod);
            for (U16 i = 0; i < ULCELLPE_AC_MAX_ANT_PER_CELL; i++)
            {
                if((*it)->acPortInfo[0][i].carrActive && (*it)->acPortInfo[1][i].carrActive )
                {
                    printf("\t port %03d DL/UL carrier is activated.\n", i);
                }
            }
        }
    }
    else if (argc == 2 && (strcmp("rsc", argv[1]) == 0))
    {
        AcDispatcher* pDrv = AcDispatcher::getInstance();
        if(!pDrv)
        {
            printf("Can not get %s driver",argv[0]);
            return RET_SUCCESS;
        }
        if(!pDrv->acSesList.size())
        {
            printf("Empty list.\n");
            return RET_SUCCESS;
        }
        printf("\nThere are %d AC session in the list: \n", pDrv->acSesList.size());
        std::list<ULCELLPE_ACSES_classS *>::iterator it;
        for (it = pDrv->acSesList.begin(); it != pDrv->acSesList.end(); ++it)
        {
            printf("carrierId %d, state %d, acReferPort %d, intervalPeriod %ds: \n", 
                (*it)->carrierId, 
                (*it)->state, 
                (*it)->acReferAntIndex, 
                pDrv->intervalPeriod);
            for (U16 i = 0; i < ULCELLPE_AC_MAX_ANT_PER_CELL; i++)
            {
                if((*it)->acPortInfo[0][i].carrActive && (*it)->acPortInfo[1][i].carrActive )
                {
                    printf("port %03d DL/UL carrier is activated.\n", i);
                    printf("\t DL AcDoneStatus %d, pathFault 0x%02X.\n", 
						(*it)->acPortInfo[0][i].acDoneStatus,
						(*it)->acPortInfo[0][i].pathFault );
                    printf("\t UL AcDoneStatus %d, pathFault 0x%02X.\n", 
						(*it)->acPortInfo[1][i].acDoneStatus,
						(*it)->acPortInfo[1][i].pathFault );
                }
            }
        }
        //printf("\n print the ac result and port info for 128 antennas! \n");
    }
    else if (argc == 3 && (strcmp("trace", argv[1]) == 0))
    {
        AcDispatcher* pDrv = AcDispatcher::getInstance();
        if(!pDrv)
        {
            printf("Can not get %s driver",argv[0]);
            return RET_SUCCESS;
        }
        if(!pDrv->acSesList.size())
        {
            printf("Empty list.\n");
            return RET_SUCCESS;
        }
        printf("\nThere are %d AC session in the list: \n", pDrv->acSesList.size());
	
        std::list<ULCELLPE_ACSES_classS *>::iterator it;
        ULCELLPE_ACSES_classS * acSes_p = NULL;
        U16 i = 0;
        union itc_msg *sig;
        for (it = pDrv->acSesList.begin(); it != pDrv->acSesList.end(); ++it)
        {
            if((*it)->state == ULCELLPE_ACSES_PROCESS)
            {
                acSes_p = *it;
                for (i = 0; i < ULCELLPE_AC_MAX_ANT_PER_CELL; i++)
                {
                    if(acSes_p->acPortInfo[0][i].carrActive && acSes_p->acPortInfo[1][i].carrActive )
                    {
                        if (strcmp("enable", argv[2]) == 0)
                        {
                            if( acSes_p->carrierId >= AC_CARRIER_NO_BASE)
                            {
                                //AcTraceObjInst.enableAcTraceData(i, acSes_p->carrierId-AC_CARRIER_NO_BASE, ACTRACE_DATA_BOTH_ENABLE);
                            }
                            else
                            {
                                //AcTraceObjInst.enableAcTraceData(i, acSes_p->carrierId, ACTRACE_DATA_BOTH_ENABLE);
                            }
                        }
                        else if (strcmp("disable", argv[2]) == 0)
                        {
                            if( acSes_p->carrierId >= AC_CARRIER_NO_BASE)
                            {
                                //AcTraceObjInst.disableAcTraceData(i, acSes_p->carrierId-AC_CARRIER_NO_BASE, ACTRACE_DATA_BOTH_ENABLE);
                            }
                            else
                            {
                                //AcTraceObjInst.disableAcTraceData(i, acSes_p->carrierId, ACTRACE_DATA_BOTH_ENABLE);
                            }
                        }
                    }
                }
            }
        }
        printf("\n trace setting command: %s AC Data Trace. \n",argv[2]);
    }
    else if (argc == 3 && (strcmp("test", argv[1]) == 0))
    {
        //U16 rfPort = 0;
        printf("\n AC internal verification test: \n");
        if (strcmp("enable", argv[2]) == 0)
        {
            AcDispatcher* pDrv = AcDispatcher::getInstance();
            union itc_msg *sig;
            sig = itc_alloc(sizeof(trCarrierEnable_t), TR_CARRIER_EANBLED);
            sig->carrierEnable.acPeriod = 60;  //seconds
            sig->carrierEnable.carrierId = AC_CARRIER_NO_BASE + 0;

            sig->carrierEnable.usedAntenna0_31 = 0x00000001;
            sig->carrierEnable.usedAntenna32_63 = 0x00000000;
            sig->carrierEnable.usedAntenna64_95 = 0x00000000;
            sig->carrierEnable.usedAntenna96_127 = 0x00000000;

/*
            sig->carrierEnable.usedAntenna0_31 = 0xFFFFFFFF;
            sig->carrierEnable.usedAntenna32_63 = 0xFFFFFFFF;
            sig->carrierEnable.usedAntenna64_95 = 0xFFFFFFFF;
            sig->carrierEnable.usedAntenna96_127 = 0xFFFFFFFF;
*/
/*
            sig->carrierEnable.usedAntenna0_31 = 0x0000000F;
            sig->carrierEnable.usedAntenna32_63 = 0x00000001;
            sig->carrierEnable.usedAntenna64_95 = 0x00000000;
            sig->carrierEnable.usedAntenna96_127 = 0x80000000;
*/

            printf("TR_CAR_EANBLED sent to AC_Control:carrier %d, antBitmap:0x%08X,0x%08X,0x%08X,0x%08X\n", 
                            sig->carrierEnable.carrierId,
                            sig->carrierEnable.usedAntenna0_31,
                            sig->carrierEnable.usedAntenna32_63,
                            sig->carrierEnable.usedAntenna64_95,
                            sig->carrierEnable.usedAntenna96_127);
            usleep(10000);

            //send(&sig, acControlProc_);
            itc_send(&sig, pDrv->ac_app_mbox, ITC_MY_MBOX);
            usleep(10000);

        }
        else if  (strcmp("disable", argv[2]) == 0)
        {
            AcDispatcher* pDrv = AcDispatcher::getInstance();
            union itc_msg *sig;
            sig = itc_alloc(sizeof(trCarrierDisable_t), TR_CARRIER_DISABLED);
            sig->carrierDisable.carrierId = AC_CARRIER_NO_BASE + 0;
            printf("TR_CAR_DISABLED sent to AC_Control:carrier %d\n", 
                             sig->carrierEnable.carrierId);

            usleep(10000);
            //send(&sig, acControlProc_);
            itc_send(&sig, pDrv->ac_app_mbox, ITC_MY_MBOX);
            usleep(10000);
        }
        else if  (strcmp("txDone", argv[2]) == 0)
        {
            AcDispatcher* pDrv = AcDispatcher::getInstance();
            if(!pDrv)
            {
                printf("Can not get %s driver",argv[0]);
                return RET_SUCCESS;
            }
            if(!pDrv->acSesList.size())
            {
                printf("Empty list.\n");
                return RET_SUCCESS;
            }
            printf("\nThere are %d AC session in the list: \n", pDrv->acSesList.size());
		
            std::list<ULCELLPE_ACSES_classS *>::iterator it;
            ULCELLPE_ACSES_classS * acSes_p = NULL;
            U16 i = 0;
            union itc_msg *sig;
            for (it = pDrv->acSesList.begin(); it != pDrv->acSesList.end(); ++it)
            {
                if((*it)->state == ULCELLPE_ACSES_PROCESS)
                {
                    acSes_p = *it;
                    for (i = 0; i < ULCELLPE_AC_MAX_ANT_PER_CELL; i++)
                    {
                        if(acSes_p->acPortInfo[0][i].carrActive && acSes_p->acPortInfo[1][i].carrActive )
                        {
                            sig = itc_alloc(sizeof(intTxAcDone_t), INT_TX_AC_DONE_IND);
                            sig->txAcDone.carrierId = acSes_p->carrierId;
                            sig->txAcDone.portId = i;
                            printf("INT_TX_AC_DONE_IND msg sent to AC_Control: carrierId %d, portId %d.\n", 
                                      sig->txAcDone.carrierId, sig->txAcDone.portId);

                            //send(&sig, acControlProc_);
                            itc_send(&sig, pDrv->ac_app_mbox, ITC_MY_MBOX);
                            usleep(10000);
                        }
                    }
                }
            }
        }
        else if  (strcmp("rxDone", argv[2]) == 0)
        {
            AcDispatcher* pDrv = AcDispatcher::getInstance();
            if(!pDrv)
            {
                printf("Can not get %s driver",argv[0]);
                return RET_SUCCESS;
            }
            if(!pDrv->acSesList.size())
            {
                printf("Empty list.\n");
                return RET_SUCCESS;
            }
            printf("\nThere are %d AC session in the list: \n", pDrv->acSesList.size());
		
            std::list<ULCELLPE_ACSES_classS *>::iterator it;
            union itc_msg *sig;
            for (it = pDrv->acSesList.begin(); it != pDrv->acSesList.end(); ++it)
            {
                if((*it)->state == ULCELLPE_ACSES_PROCESS)
                {
                    sig = itc_alloc(sizeof(intRxAcDone_t), INT_RX_AC_DONE_IND);
                    sig->rxAcDone.carrierId = (*it)->carrierId;
                    printf("INT_RX_AC_DONE_IND msg sent to AC_Control: carrierId %d\n", 
                             sig->rxAcDone.carrierId);

                    //send(&sig, acControlProc_);
                    itc_send(&sig, pDrv->ac_app_mbox, ITC_MY_MBOX);
                    usleep(10000);
                }
            }
        }
        else if  (strcmp("mem", argv[2]) == 0)
        {
            S32 mode = 0x0001;
            if (*(char *)&mode)
            {
                printf("memory low mode.\n");
            }
            else
            {
                printf("memory high mode.\n");
            }

            S16 * acBuffer = (S16 *)traffic_mmap_ac_base;
            printf("AC Memory C0A0: byte0 %d, byte1 %d, byte2 %d, byte3 %d.\n", 
                             *acBuffer, *(acBuffer+1), *(acBuffer+2), *(acBuffer+3));
            printf("AC Memory C0A0: byte2396 %d, byte2397 %d, byte2398 %d, byte2399 %d.\n", 
                             *(acBuffer+2396), *(acBuffer+2397), *(acBuffer+2398), *(acBuffer+2399));

            acBuffer = (S16 *)(traffic_mmap_ac_base + (U16)0x2000);
            printf("AC Memory C0A1: byte0 %d, byte1 %d, byte2 %d, byte3 %d.\n", 
                             *acBuffer, *(acBuffer+1), *(acBuffer+2), *(acBuffer+3));
            printf("AC Memory C0A1: byte2396 %d, byte2397 %d, byte2398 %d, byte2399 %d.\n", 
                             *(acBuffer+2396), *(acBuffer+2397), *(acBuffer+2398), *(acBuffer+2399));
        }
        else
        {
            printf("Invalid test content, select enable or disable.\n");
        }
        printf("\n AC internal verification test done.\n");
    }
    else
    {
        printf("Usage:\n%s  all | rsc | traceSet | test \n", argv[0]);
    }
    return RET_SUCCESS;
}

/*
**
** When any of the carrier is doing the AC (ULCELLPE_ACSES_PROCESS), return true.
** When non of the carrier is doing the AC, return false.
**
*/
bool AcDispatcher::checkAcOnProgress(){

    bool retVal = false;

    std::list<ULCELLPE_ACSES_classS *>::iterator it;

    TPT_INFO(STR("AcDispatcher checkAcOnProgress"));
    for (it = acSesList.begin(); it != acSesList.end(); ++it)
    {
        if((*it)->state == ULCELLPE_ACSES_PROCESS)
        {
            break;
        }
    }
    if(it != acSesList.end())
    {
        TPT_INFO(STR("AcDispatcher checkAcOnProgress AC session carrierId %d is doing AC", 
                    (*it)->carrierId));
        retVal = true;
    }

    return retVal;

}

