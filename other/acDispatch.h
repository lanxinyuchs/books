/* >  CONTENTS
 ******************************************************************************
 ************* Copyright ERICSSON RADIO ACCESS AB *****************************
 ******************************************************************************
 *
 * The copyright to the computer programs herein is the property of
 * ERICSSON RADIO ACCESS AB. The programs may be used and/or copied
 * only with the written permission from ERICSSON RADIO ACCESS AB or
 * in accordance with the terms and conditions stipulated in the
 * agreement/contract under which the prgrams have been supplied.
 *
 ******************************************************************************
        CONTENTS
        --------
        1  Description
        2  Include files
        3  Declarations
        4  Functions
        
 ******************************************************************************
 */


/* >  1  DESCRIPTION
******************************************************************************
*
* <text...>
*
* @(#) ClearCase ID: acDispatch.h /main/...
*
* Revision history
* Revided:  1 Jun 2016 EDENTAO
* Change: first revision
*
******************************************************************************
 */


/* >  Double inclusion protection
 ******************************************************************************
 */
#ifndef __AC_DISPATCH_H__
#define __AC_DISPATCH_H__

/* >  2  INCLUDE FILES
 ******************************************************************************
 */
#include "defaultDriver.h"
#include <vector>
#include <list>
//#include <tosv.h>
#include <itc.h>
#include "ac_defs.h"
//#include "memoryMapped.h"
#include "logFile.h"
#include "fpga.h"
#include "acFreqDataMem.h"

//#define CANCEL_INFO   U32
/*********        AC definition        *********/
#define DOWNLINK                 0
#define UPLINK                   1
#define DOWNLINK_AND_UPLINK      2    /* 0: downlink;  1: uplink.*/
#define complex S16
/**
 * The maximum number of antennas for 1 cell used in antennas calibration.
 * This is the actual number of physical antennas. For AC, we only support
 * 8RX case for beamforming
 */
#define ULCELLPE_AC_MAX_ANT_PER_CELL 64

/* Transfer the delay to ns
 * the format of the output of spLibAc is 2Pi = 2^15*2^4
 *
 *
 *    delay * Nfft    delay * 2048          delay
 * t = ---------- = ----------------- = ------------------ = delay * 0.0636ns
 *      2Pi * fs   2^16*2^4*30,72*10^6   2^9 * 30,72 *10^6
 *
 *      delay
 *  t = -----
 *       16
 */
#define AC_DELAY_TIME_IN_NS          16
#define AC_CONTROL_PROC          "acControlProc"

/* 
Define carrier Frequency
*/
#define FREQ2580 2580000
#define FREQ2600 2600000
#define FREQ2620 2620000
#define CEN_FREQ 2605005
/* 
Define carrier Bandwidth
*/
#define BAND20M 20000
#define BAND15M 15000
#define BAND10M 10000
#define BAND5M 5000

/*
128 antennas * 2(UL/DL)*2(carrierNumber)*1200(subcarriers)*4(16bitI + 16bitQ) = 3686400 bytes 
*/
#define SIZE_OF_AC_FREQ_DOMAIN_DATA 614400
#define SIZE_OF_AC_FREQ_DOMAIN_FFT_SIZE 1200
#define SIZE_OF_AC_TIME_DOMAIN_FFT_SIZE 2048

#define AC_CTRL_FAULT_NONE                   0x0000
#define AC_CTRL_FAULT_PHASE                  0x0001
#define AC_CTRL_FAULT_DELAY                  0x0002
#define AC_CTRL_FAULT_SIG_TOO_LOW            0x0004
#define AC_CTRL_FAULT_AMPLITUDE              0x0008

#define AC_DISTURB_SHIFT_MAX         20

#define AC_FREQ_DATA_FROM_FPGA         0x0000
#define AC_FREQ_DATA_SUCCESS           0x0001
#define AC_FREQ_DATA_FAILURE_DELAY     0x0002
#define AC_FREQ_DATA_FAILURE_DISTURB   0x0003
#define AC_FREQ_DATA_FAILURE_SIGLOW    0x0004
#define CANCEL_INFO                    U32
#define PRE_AC_TIME_OUT            120


typedef enum ULCELLPE_ACSES_stateE
{
    ULCELLPE_ACSES_SETUP,
    ULCELLPE_ACSES_IDLE,
    ULCELLPE_ACSES_PROCESS,
    ULCELLPE_ACSES_PENDING,
    ULCELLPE_ACSES_REPORT,
    ULCELLPE_ACSES_DEACTIVE,
    ULCELLPE_ACSES_RELEASE,
    ULCELLPE_ACSES_STATE_VOID
}ULCELLPE_ACSES_stateE;

typedef enum ULCELLPE_ACSES_acTypeE
{
    ULCELLPE_ACSES_ACTYPE_PRE,
    ULCELLPE_ACSES_ACTYPE_PERIOD
}ULCELLPE_ACSES_acTypeE;

typedef enum ULCELLPE_ACSES_TIMEOUT_MSGID
{
    ULCELLPE_ACSES_PERIOD_AC,
    ULCELLPE_ACSES_PRE_AC_MSG,
    ULCELLPE_ACSES_SET_PRACH_MSG,
    ULCELLPE_ACSES_RESET_DELAY_REG_MSG
}ULCELLPE_ACSES_TIMEOUT_MSGID;

typedef enum ULCELLPE_ACSES_RESET_DELAY_REG_RESP_FLAG
{
    ULCELLPE_ACSES_RESET_DELAY_REG_IDLE,
    ULCELLPE_ACSES_RESET_DELAY_REG_CFM,
    ULCELLPE_ACSES_RESET_DELAY_REG_REJ,
    ULCELLPE_ACSES_RESET_DELAY_REG_TIMEOUT
}ULCELLPE_ACSES_RESET_DELAY_REG_RESP_FLAG;

typedef enum ULCELLPE_ACSES_PRE_AC_RESP_FLAG
{
    ULCELLPE_ACSES_PRE_AC_IDLE,
    ULCELLPE_ACSES_PRE_AC_CFM,
    ULCELLPE_ACSES_PRE_AC_REJ,
    ULCELLPE_ACSES_PRE_AC_TIMEOUT
}ULCELLPE_ACSES_PRE_AC_RESP_FLAG;

typedef enum ULCELLPE_ACSES_SET_PRACH_RESP_FLAG
{
    ULCELLPE_ACSES_SET_PRACH_IDLE,
    ULCELLPE_ACSES_SET_PRACH_CFM,
    ULCELLPE_ACSES_SET_PRACH_REJ,
    ULCELLPE_ACSES_SET_PRACH_TIMEOUT
}ULCELLPE_ACSES_SET_PRACH_RESP_FLAG;


/*****************************************************************************
 * typedef struct: ULCELLPE_ACSES_portInfo
 *//**
 * @brief                      Struct containing UL or DL antenna
 *                             calibration port info for one antenna
 * @param delay                The delay of AC seq,
 * @param initPhase            The angle for the first subcarrier,
 *
 ****************************************************************************/
typedef struct     //ULCELLPE_ACSES_portInfo
{
    U8    carrActive;
    U8    acDoneStatus;
    U8    pathFault;
    U8    faultRetryStatus[3];    //retry 3 times for ac fault detection
    U8    preAcReqSendFlag;
    U8    preAcRespRecvFlag;
    U8    freqDataStoreFlag;
    U8    setPrachReqSendFlag;
    U8    setPrachRespRecvFlag;
    U8    resetDelayRegReqSendFlag;
    U8    resetDelayRegRespRecvFlag;	
    U8    ampliFaultFlag;
}ULCELLPE_ACSES_portInfo;

/*****************************************************************************
 * typedef struct: ULCELLPE_ACSES_calibFactorS
 *//**
 * @brief                      Struct containing UL or DL antenna
 *                             calibration factors for one antenna
 *                             Same as ElibBbBaseCommonAntCalibFactorS but
 *                             using S32 to be able to add several measurements
 *                             before averaging.
 * @param delay                The delay of AC seq,
 * @param initPhase            The angle for the first subcarrier,
 *
 ****************************************************************************/
typedef struct     //ULCELLPE_ACSES_calibFactorS
{
    S16    delay;
    S16    initPhase;
    S32    amplitude;
    S16    channelSumCentidB;
    U32    disturbance;
    S16    noShift;
}  ULCELLPE_ACSES_calibFactorS;

typedef struct     //ULCELLPE_ACSES_calibFactorS
{
    S32    delay;
    S32    initPhase;
    S32    amplitude;
    S32    channelSumCentidB;
}  ULCELLPE_ACSES_adjustFactorS;

typedef struct     //ULCELLPE_ACSES_FreqDomain_DATA
{
    S16    re;    //complex, Real Quantity
    S16    im;    //complex, Imaginary Quantity
}  ULCELLPE_ACSES_FreqDomain_Data;

typedef struct     //ULCELLPE_ACSES_classS
{
    /**
     * node unique cell identifier
     */
    U16 carrierId;
    U16 deviceId;
    U16 numRe;
    U32 carrierFrequency;
    U32 bandWidth;
    U32 antBitmap[4];

    /*timer*/
    bool tmo_requested;
    CANCEL_INFO * tmo_subCancel;
    uint32_t resetDelayRegTimer;
    uint32_t preAcTimer;
    uint32_t acTimer;
    uint32_t setPrachTimer;

    /**
     * AC session state
     */
    ULCELLPE_ACSES_stateE state;

    /**
     * Antenna calibration counter during one period, it indicates which
     *  antenna do calibration for each times
     *
     *  the procedure for all antenna calibration is:
     *  downlink antenna0 for timesPerAnt times
     *  downlink antenna1 for timesPerAnt times
     *                ...
     *  downlink antenna7 for timesPerAnt times
     *  uplink antenna0 for timesPerAnt times
     *  uplink antenna1 for timesPerAnt times
     *                  ...
     *  uplink antenna7 for timesPerAnt times
     */
    U16 acCounter;
    U16 acCounterMax;
    U16 acReferAntIndex;
    U8   faultRetryCounter;
    U8   waitAcDoneRetryCounter;
	
    /**
     * Successful antenna calibration count for each antenna
     * 0: downlink;  1: uplink.
     */
    U16 acSuccCnt[DOWNLINK_AND_UPLINK][ULCELLPE_AC_MAX_ANT_PER_CELL];

    /**
     * Antenna calibration type
     */
    ULCELLPE_ACSES_acTypeE acType;

    /**
     * pointer to FFT related buffer
     */
    U32* inFftBuffer;
    U32* outFftBuffer;

    /**
     * buffer for sum of AC calculation result, Averaging and scope fitting
     * is done before sending result.
     * 0: downlink;  1: uplink.
     */
    ULCELLPE_ACSES_calibFactorS
          acCalibFactor[DOWNLINK_AND_UPLINK][ULCELLPE_AC_MAX_ANT_PER_CELL];

    /**
     * buffer for AC calculation result, it updated with nofaulty values,
     * if there is a fault on an antenna these values are not updated.
     * 0: downlink;  1: uplink.
     */
    ULCELLPE_ACSES_adjustFactorS
          acCalibFactorTrueValue[DOWNLINK_AND_UPLINK][ULCELLPE_AC_MAX_ANT_PER_CELL];

    /**
     * Successful antenna calibration count for each antenna
     * 0: downlink;  1: uplink.
     */
    ULCELLPE_ACSES_portInfo
          acPortInfo[DOWNLINK_AND_UPLINK][ULCELLPE_AC_MAX_ANT_PER_CELL];

    ULCELLPE_ACSES_FreqDomain_Data acFreqDomainData[SIZE_OF_AC_FREQ_DOMAIN_FFT_SIZE];
    /**
     * Systemconstants used for faultdetection.
     */
    U32 acFaultUnwrapThreshold;
    S16 acFaultTxChannelThreshold;
    S16 acFaultRxChannelThreshold;
    S16 acFaultDelayThreshold;
    S16 acFaultAmpThreshold;

    U32 chanSCdBSum;
    U32 chanSCdBAvg;
    /**
     * Result for ul and dl.
     * [ant7 ant6 ....ant1 ant0]
     * for each antenna one bit is used [N/A delayFault unwrapFault channelFault]
     */
    U32 txPathFault;
    U32 rxPathFault;
    U16  acAlarmDataLength; /* currently, it should be 6 = 3 alarm types (disturbed, signal too low, amplitude), * 2 (ul + dl)*/

    double amplitDbAvg[DOWNLINK_AND_UPLINK];
    double delayDbAvg;
    double phaseDbAvg;
}ULCELLPE_ACSES_classS;

class AcDispatcher : public DefaultDriver
{
public:
    AcDispatcher(void);
    ~AcDispatcher();
    static AcDispatcher* getInstance();
    bool init(const char* instanceDbName);  /**< Driver initialization. */
    static DefaultDriver* creator(void);

    static U32 waitTime;
    static U32 capNoRfPorts;
    U32 intervalPeriod;
    U32 startUpDelay;
    // Execute the first round periodic AC startFirstPerAcDelay seconds after pre-AC 
    U32 startFirstPerAcDelay;
    // Wait waitACDoneDelay seconds when any of AC is processing currently 
    // before starting AC for new carrier.
    U32 waitACDoneDelay;
    bool getHwDbCompStatus;

   std::list<ULCELLPE_ACSES_classS*> acSesList;
    static itc_mbox_id_t ac_app_mbox;
    static itc_mbox_id_t radio_bp_mbox;
    static itc_mbox_id_t rhd_int_mbox;
    static itc_mbox_id_t app_fault_mgr_mbox;
    //MemoryMappedDevice * fpga;

    void testInterfaceInit(void);
    static int testCmdHandler(int argc, char **argv);
    void handleAcCompHwDb(union itc_msg * rec_p);
    void handleAcSettingReq(union itc_msg * rec_p);
    void handleAcCarrierEnable(union itc_msg * rec_p);
    void handleAcCarrierPending(union itc_msg * rec_p);
    void handleAcCarrierDisable(union itc_msg * rec_p);
    void handleTxAcDone(union itc_msg * rec_p);
    void handleRxAcDone(union itc_msg * rec_p);
    void handleTimeOutInd(union itc_msg * rec_p);
    void handlePeriodAc(union itc_msg * rec_p);
    void handlePreAcMsgTimeOut(union itc_msg * rec_p);
    void handleRespCfm(union itc_msg * rec_p);
    void handleRespReject(union itc_msg * rec_p);
    void handlePreAcMsgCfm(ULCELLPE_ACSES_classS* acSes_p, U16 portId, U16 directReverse);
    void handlePreAcMsgReject(ULCELLPE_ACSES_classS* acSes_p, U16 portId, U16 directReverse);
    bool sendResetDelayRegToRadio(ULCELLPE_ACSES_classS* acSes_p, U16 startNum);
    void handleResetDelayRegMsgTimeOut(union itc_msg * rec_p);
    void handleResetDelayRegMsgCfm(ULCELLPE_ACSES_classS * acSes_p, U16 portId, U16 directReverse);
    void handleResetDelayRegMsgReject(ULCELLPE_ACSES_classS * acSes_p, U16 portId, U16 directReverse);
    void handleSetPrachMsgTimeOut(union itc_msg * rec_p);
    void handleSetPrachMsgCfm(ULCELLPE_ACSES_classS* acSes_p, U16 portId);
    void handleSetPrachMsgReject(ULCELLPE_ACSES_classS* acSes_p, U16 portId);
    void handleAcFpgaAccess(union itc_msg * rec_p);
    void handleIntAcDoneInd(union itc_msg * rec_p);
    void handleFreqDataStoreSetting(union itc_msg * rec_p);
    void handleDebugValueSetting(union itc_msg * rec_p);
    void triggerPeriodAc(U16 carrierId, U8 acOnOff);
    void setFactoryCompOnOff(U8 onOff);
    bool addAcSession(U16 carrierId, U16 deviceId, U32 freq, U32 antBitmap[], U32 bandWidth);
    ULCELLPE_ACSES_classS* getAcSession(U32 freq);
    bool deleteAcSession(U16 carrierId);

    bool startAcSession(U16 carrierId);
    bool stopAcSession(U16 carrierId);
    void setDefaultCompValue(ULCELLPE_ACSES_classS* acSes_p);

    bool setAcSessionStatus(U16 carrierId, ULCELLPE_ACSES_stateE state);

    bool sendAcParaToFpga(U16 carrierId);
    bool calcDucDdc(U32 use_freq, U32 *duc, U32 *ddc);

    bool sendAcFactorToRadio(ULCELLPE_ACSES_classS* acSes_p, U16 startNum);
    bool sendPrachToRadio(ULCELLPE_ACSES_classS* acSes_p, U16 startNum);

    U32 calcDelayComp(ULCELLPE_ACSES_classS* acSes_p, U16 direct, U16 port);
    double calcPhaseComp(ULCELLPE_ACSES_classS* acSes_p, U16 direct, U16 port);
    void calcAmplitDbAvg(ULCELLPE_ACSES_classS* acSes_p);
    void calcDelayDbAvg(ULCELLPE_ACSES_classS* acSes_p);
    void calcPhaseDbAvg(ULCELLPE_ACSES_classS* acSes_p);
    double calcAmplitComp(ULCELLPE_ACSES_classS* acSes_p, U16 direct, U16 port);
    bool setAcFactorToFPGA(ULCELLPE_ACSES_classS* acSes_p);

    bool updateAcFactor(U16 carrierId, U16 portId, U16 receiveTransmit, 
                ULCELLPE_ACSES_calibFactorS acFactor);

    bool checkAcSessionDone(ULCELLPE_ACSES_classS* acSes_p);

    void restoreAcSession(ULCELLPE_ACSES_classS* acSes_p);

    void retryAc(ULCELLPE_ACSES_classS* acSes_p);

    bool reportAcFactor(ULCELLPE_ACSES_classS* acSes_p);

    void reportAcFault(ULCELLPE_ACSES_classS* acSes_p, U16 alarm_action);

    void detectDelayFault(ULCELLPE_ACSES_classS* acSes_p);
    void detectAmpliFault(ULCELLPE_ACSES_classS* acSes_p);
	
    bool checkFaultStatus(ULCELLPE_ACSES_classS* acSes_p);

    bool storeAcFreqData(ULCELLPE_ACSES_classS* acSes_p);

    bool initTrafficBaseAddr();
    void configAcSeqData(S16 * seqData, U32 size);

    bool displayAcSession();
	
    void setAcFreqDataSrc(union itc_msg * rec_p);
    void setAcFreqDataSrc(U16 dataSrc);
    void getAcFreqDataSrc(U16 & dataSrc);
    bool calcPreAcDelay(ULCELLPE_ACSES_classS* acSes_p, U16 direct, U16 port, 
                U32 *delayFft, U8 *flagFft, U32 *acDly, U8 *flagDly);
    void calcAcPrach(ULCELLPE_ACSES_classS* acSes_p, U16 port, U32* prach_real, U32* prach_img);

    // Check whether any carrier is in ULCELLPE_ACSES_PROCESS status
    bool checkAcOnProgress();

private:
    static bool registeredInDrvFactory;
    static void *traffic_uio_ac_handle;
    static void *traffic_mmap_ac_base;
    U16 acFreqDataSrc;
    LogFile* pAcLogFile;
    bool readyRadioApp;
    Fpga* fpga;
    AcFreqDataMem* freqDataMem;

public:
};

/* >  End of double inclusion protection
 ******************************************************************************
 */
#endif




