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
* @(#) ClearCase ID: acSignal.h /main/...
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
#ifndef __AC_SIGNAL_H__
#define __AC_SIGNAL_H__

/* >  2  INCLUDE FILES
 ******************************************************************************
 */
#include <list>
#include <vector>
#include <complex>
#include <itc.h>
#include "acDispatch.h"

extern "C"{
#include "timer-if.h"
}
/*********        AC Signal definition        *********/

/*Time out signal*/
#define AC_STARTUP_C0_TMO  0x6670
#define AC_STARTUP_C1_TMO  0x6671
#define AC_STARTUP_C2_TMO  0x6672
#define AC_PERIOD_C0_TMO  0x6670
#define AC_PERIOD_C1_TMO  0x6671
#define AC_PERIOD_C2_TMO  0x6672

#define TR_AC_SETTING_REQ  0x6881
#define TR_CARRIER_EANBLED  0x6882
#define TR_CARRIER_PENDING  0x6883
#define TR_CARRIER_DISABLED  0x6884
#define TR_AC_STATUS  0x6885
#define TR_AC_DATASRC  0x6886
#define TR_AC_FPGA_ACCESS  0x6887
#define TR_AC_FREQDATA_STORE_SET  0x6888
#define TR_AC_FAULT_REPORT               0x6889
#define TR_AC_FAULT_CEASE                0x6890
#define TR_AC_DEBUGVALUE 0x7000
#define INT_TX_AC_DONE_IND (0x80000000 + 0x00E00 + 0x081)
#define INT_RX_AC_DONE_IND (0x80000000 + 0x00E00 + 0x082)
#define AC_COMP_VALUE_HWDB  (0x80000000 + 0x00E00 + 0x083)
#define AC_CARRIER_NO_BASE  100

#define AC_ALARM_DISTURBED               0
#define AC_ALARM_SIGNAL_TOO_LOW          1
#define AC_ALARM_AMPLITUDE               2

#define AC_ALARM_REPORT                  0
#define AC_ALARM_CEASE                   1

#define AC_INT_REGISTER_REQ  0x7000
#define AC_INT_IND  0x7001

#define AC_INT_DL  61
#define AC_INT_UL  62
//#define SIGSELECT uint32_t
//#define SIGNAL itc_msg
//introduce TRDCI MSG DC_TR_MODIFY_CARRIER_DELAY_REQ
//#define TR_DCI_SIGBASE 0x70000
//#define DC_TR_MODIFY_CARRIER_DELAY_REQ (TR_DCI_SIGBASE + 0x30)
//#define DC_TR_MODIFY_CARRIER_DELAY_CFM (TR_DCI_SIGBASE + 0x30)
//#define DC_TR_MODIFY_CARRIER_DELAY_REJ (TR_DCI_SIGBASE + 0x30)
#define DC_TR_MODIFY_CARRIER_DELAY_REQ             0x0170602e
#define DC_TR_MODIFY_CARRIER_DELAY_CFM             0x0170602f
#define DC_TR_MODIFY_CARRIER_DELAY_REJ              0x01706030
#define ELIB_CONST_HWU_MAX_ERROR_DESCRIPTION_STRINGLENGTH    110
typedef uint32_t SIGSELECT; 

typedef struct ElibNodeCommonAddressInfoS
{
  S32    clientRef;
  S32    serverRef;
} ElibNodeCommonAddressInfoS;


typedef struct ElibConstRejectCauseS
{
  U16    errorCode;
  U16    padding0; /* Do not care */
  U8     errorDescription[ELIB_CONST_HWU_MAX_ERROR_DESCRIPTION_STRINGLENGTH];
  U16    padding1; /* Do not care */
} ElibConstRejectCauseS;


struct DcTrModifyCarrierDelayReqS
{
  SIGSELECT                     sigNo;
  ElibNodeCommonAddressInfoS    addressInfo;
  U16                           deviceId;
  U32                           ratType;                 /*DC_TR_RAT_TYPE_LTE_FDD, DC_TR_RAT_TYPE_LTE_TDD,
                                                           DC_TR_RAT_TYPE_GSM, DC_TR_RAT_TYPE_WCDMA */
  S32                           carrierDeltaDelay;      /* The delta delay that radio must apply to fully compensate for the antenna calibration */
  																											/* Unit: 0.1[ns],*/
}; 

struct DcTrModifyCarrierDelayCfmS
{
  SIGSELECT                     sigNo;
  ElibNodeCommonAddressInfoS    addressInfo;
  U16                           deviceId;
};

struct DcTrModifyCarrierDelayRejS
{
  SIGSELECT                     sigNo;
  ElibNodeCommonAddressInfoS    addressInfo;
  U16                           deviceId;
  ElibConstRejectCauseS         rejectCause;
}; 


typedef struct
{
    uint32_t sigNo;
    bool acEnable;
    U32 acInterval;
} trAcSetting_t;

typedef struct
{
    uint32_t sigNo;
    U16 carrierId;
    U16 deviceId;
    U32 carrierFrequency;
    U32 usedAntenna0_31;
    U32 usedAntenna32_63;
    U32 usedAntenna64_95;
    U32 usedAntenna96_127;
    U32 acPeriod;
    U32 bandWidth;
} trCarrierEnable_t;

typedef struct
{
    uint32_t sigNo;
    U16 carrierId;
} trCarrierPending_t;

typedef struct
{
    uint32_t sigNo;
    U16 carrierId;
} trCarrierDisable_t;

typedef struct
{
    uint32_t sigNo;
    U16 carrierId;
    U16 portId;
} intTxAcDone_t;

typedef struct
{
    uint32_t sigNo;
    U16 carrierId;
    U16 portId;
} intRxAcDone_t;

typedef struct
{
    uint32_t sigNo;
    U16 sigCarrierId;
} TosvSignalAc_t;

typedef struct
{
    uint32_t sigNo;
    U16 carrierId;
} trAcStatus_t;

typedef struct
{
    uint32_t sigNo;
    U16 dataSrc;
} trAcDataSrc_t;

typedef struct
{
    uint32_t sigNo;
    U16 debugName;
    float debugValue;
} trAcDebugValue_t;

typedef struct
{
    uint32_t sigNo;
    U16 operFlag;
    U32 addr;
    U32 data;
} trAcFpgaAccess_t;

typedef struct
{
    SIGSELECT  sigNo;
} IntAcReadyIndS_t;

/*If there is interrupt happens, rhd-int will send below message to ac application*/
typedef struct
{
  SIGSELECT sigNo;
  U32 intProcId;   // DL:61, UL:62
} IntAcDoneIndS_t;

typedef struct
{
    uint32_t sigNo;
    U16 carrierId;
    U32 usedAntenna0_31;
    U32 usedAntenna32_63;
} trFreqDataStoreSetting_t;

typedef struct
{
    uint32_t sigNo;   /* #define AC_COMP_VALUE_HWDB  (0x80000000 + 0x00E00 + 0x083)  */
    bool status;      /* If APP fail to get the compensation data, set it "FALSE", otherwise it is "TRUE */
    S32 delayComp[ULCELLPE_AC_MAX_ANT_PER_CELL];
    S32 phaseComp[ULCELLPE_AC_MAX_ANT_PER_CELL];
    S32 ampComp[ULCELLPE_AC_MAX_ANT_PER_CELL];
} acCompHwDb_t;

typedef struct
{
    U16 type;
    U16 direction;   //UL or DL
    U16 bitmap[8]; //support 128 ant
}AcAlarmDeteilS_t;

typedef struct
{
    SIGSELECT sigNo;
    U16 antpnum;
    U16 carrierID;
    U16 deviceID;
    U16  acAlarmDataLength;
    AcAlarmDeteilS_t  acAlarmData[1]; /* <alarm order: Tx signal too low, Rx signal too low; Tx disturbed, Rx disturbed; Tx amplitude, Rx amplitude> */
}AcAlarmIndS_t;

typedef struct 
{
    SIGSELECT sigNo; 
    U16 deviceID;
    U16 carrierID;
}AcAlarmCeaseIndS_t;  


union itc_msg
{
    uint32_t sigNo;
    trAcSetting_t acSetting;
    trCarrierEnable_t carrierEnable;
    trCarrierPending_t carrierPending;
    trCarrierDisable_t carrierDisable;
    intTxAcDone_t txAcDone;
    intRxAcDone_t rxAcDone;
    TosvSignalAc_t txsvSigAcNo;
    trAcStatus_t trAcStatus;
    trAcDataSrc_t trAcDataSrc;
    tmoTimeoutIndS_t tmoTimeoutInd;
    DcTrModifyCarrierDelayReqS dcTrModifyCarrierDelayReq;
    DcTrModifyCarrierDelayCfmS dcTrModifyCarrierDelayCfm;
    DcTrModifyCarrierDelayRejS dcTrModifyCarrierDelayRej;
    trAcFpgaAccess_t trAcFpgaAcce;
    IntAcReadyIndS_t intAcReadyInd;
    IntAcDoneIndS_t intAcDoneInd;
    trFreqDataStoreSetting_t trFreqDataStoreSet;
    AcAlarmIndS_t AcAlarmInd;
    AcAlarmCeaseIndS_t AcAlarmCeaseInd;
    acCompHwDb_t acCompHwDb;
    trAcDebugValue_t trDebugValueSet;
}; 

/* >  End of double inclusion protection
 ******************************************************************************
 */
#endif


