//#include "xpai_hdr_i2c_if.h"
//#include "common.h"
#include "stdio.h"
#include "stdlib.h"
#include "string.h"
#include "itc.h"
#include <stdbool.h>
#include "ose.h"
#include "osetypes.h"
#include "time.h"
#include <unistd.h>
//#include "ctype.h"
//#include "gpio.h"

/*mailbox*/
#define AC_DISPATCH_MAILBOX          "AC_DISPATCH"
#define AC_TEST_MAILBOX          "AC_TEST"

#define MAX_MAILBOX_NUM    32

static itc_mbox_id_t ac_app_mbox = 0;

#define isequal(a,b) !strcmp(a,b)
#define CMD_AC        "ac"
#define USAGE_AC      "type 'ac' for command list"
#define DESC_AC       "Access to AC command"

#define NAME_SIZE           100
#define MAX_ARGC            10
#define ULCELLPE_AC_MAX_ANT_PER_CELL 64
/* > TYPE DEFINITIONS
 ******************************************************************************
 */
 typedef struct
 {
	 char argv[MAX_ARGC][NAME_SIZE];
	 int  argc;
	 void (*cmdPtr)(int, char**);
 } argEntry_t;

enum {
    //SET_AC_PARAM,
    //GET_AC_PARAM,
    GET_AC_STATUS,
    START_AC,
    STOP_AC,
    SET_TX_DONE,
    SET_RX_DONE,
    SET_DATA_SRC,
    SET_FPGA,
    SET_FREQ_DATA_STORE,
    SET_DEBUG_VALUE,
    //GET_DATA_SRC,
    NOF_CMD
};

/// The different carrier events
enum CarrierSubscriberEvent_t
{
    CR_EVENT_BASEBAND_RELEASED = 1,
    CR_EVENT_BASEBAND_SETUP    = 1 << 1,
    CR_EVENT_CARRIER_RELEASED  = 1 << 2, //RELEASED
    CR_EVENT_CARRIER_SETUP     = 1 << 3, // ADDED
    CR_EVENT_CARRIER_ENABLED   = 1 << 4, //activate
    CR_EVENT_CARRIER_DISABLED  = 1 << 5, //deactivate
    CR_EVENT_CARRIER_SHUTDOWN  = 1 << 6, // 
    CR_SUBSCRIPTION_EVENT_DL_TIME  = 1 << 7,
    CR_SUBSCRIPTION_EVENT_UL_TIME  = 1 << 8,
    CR_EVENT_CARRIER_MODIFY    = 1 << 9,
    CR_EVENT_CARRIER_DELAY_MODIFY = 1 << 10,
    CR_EVENT_CARRIER_POWER_CLASS_MODIFY= 1 << 11,
    CR_EVENT_CARRIER_POWER_MODIFY      = 1<< 12,
    CR_EVENT_CARRIER_RESETUP   = 1 << 15,
    CR_EVENT_CARRIER_REENABLED   = 1 << 16,
    CR_EVENT_CARRIER_SYNC = 1 << 17,
    CR_EVENT_CARRIER_PENDING = 1 << 18,
    CR_EVENT_UL_TA_UPDATE  = 1 << 19
};


enum TrCarrierDirection_t
{
    TR_CARRIER_DIRECTION_UL = 0,
    TR_CARRIER_DIRECTION_DL
};
#define TR_CARRIER_DIRECTION_UL    0
#define TR_CARRIER_DIRECTION_DL    1

typedef struct
{
    U32 carrierFrequency; 
    U32 carrierBandwidth; 
    U32 usedAntenna0_31;  
    U32 usedAntenna32_63; 
    U32 usedAntenna64_95; 
    U32 usedAntenna96_127;
    U32 acPeriod;
    U8  acWinLength;
    U32 powerFactor;
} acParam_t; 
 
#define TR_AC_SETTING_REQ  0x6881
#define TR_CARRIER_EANBLED  0x6882
#define TR_CARRIER_PENDING  0x6883
#define TR_CARRIER_DISABLED  0x6884
#define TR_AC_STATUS  0x6885
#define TR_AC_DATASRC  0x6886
#define TR_AC_FPGA_ACCESS  0x6887
#define TR_AC_FREQDATA_STORE_SET  0x6888
#define TR_AC_DEBUGVALUE 0x7000
#define INT_TX_AC_DONE_IND (0x80000000 + 0x00E00 + 0x081)
#define INT_RX_AC_DONE_IND (0x80000000 + 0x00E00 + 0x082)
#define AC_CARRIER_NO_BASE  100

//#define SIGSELECT uint32_t
//#define SIGNAL itc_msg
//#define PRINT_USAGE() print_usage()
#define PRINT_ARGUMENT_INVALID() print_argument_invalid()

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
    uint32_t sigNo;
    U16 carrierId;
    U32 usedAntenna0_31;
    U32 usedAntenna32_63;
} trFreqDataStoreSetting_t;

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
    trAcFpgaAccess_t trAcFpgaAcce;
    trFreqDataStoreSetting_t trFreqDataStoreSet;
	trAcDebugValue_t trDebugValueSet;
}; 

/* > LOCAL DECLARATIONS
 ******************************************************************************
 */
static argEntry_t argList[NOF_CMD];
static acParam_t  acParam;
static U32 client_ref = 1234;

/* > FUNCTION PROTOTYPES
 ******************************************************************************
 */
static void initAcCmdTables(void);
static void initCarrierParam(void);
static int  shellCmdAC(int argc, char **argv);
static void shellCmdDesc(int argc, char **argv);
static void setAcParam(int argc, char **argv);
static void getAcParam(int argc, char **argv);
static void startAC(int argc, char **argv);
static void stopAC(int argc, char **argv);
static void getAcStatus(int argc, char **argv);
static void setDataSource(int argc, char **argv);
static void getDataSource(int argc, char **argv);
static void setTxDone(int argc, char **argv);
static void setRxDone(int argc, char **argv);
static void  accessFpga(int argc, char **argv);
static void  setAcFreqDataStore(int argc, char **argv);
static void  setDebugValue(int argc, char **argv);


bool get_mbox(const char *mbox_name, itc_mbox_id_t *mbox_id)
{
    *mbox_id= itc_locate(mbox_name);
    if (*mbox_id== ITC_NO_ID) {
        //TPT_ERROR(STR("%s does not exist", mbox_name));
        return false;
    }
    return true;
}

static int init_itc(void)
{
    /* Initialize ITC and create mailbox, initialize XPAI I2C connection */
    if(itc_init(MAX_MAILBOX_NUM, ITC_MALLOC, NULL, ITC_NO_NAMESPACE, 0)) {
            printf("%s: Unable to initialize ITC!\n", __func__);
            return 1;
    }
    if(itc_create_mailbox(AC_TEST_MAILBOX, 0) == ITC_NO_ID) {
            printf("i2c-test mailbox is failed to create\n");
            return 1;
    }
    ac_app_mbox = 0;
    if(!get_mbox(AC_DISPATCH_MAILBOX, &ac_app_mbox))
    {
        printf("Get mailbox AC_DISPATCH failed.\n");
        return 1;
    }
    return 0;
}

static void delete_itc(void)
{
	itc_mbox_id_t mbox = itc_current_mbox();
	if( mbox != ITC_NO_ID) {
		itc_delete_mailbox(mbox);
	}
}


static void shellCmdDesc(int argc, char **argv)
{
    int tmpC, i, j;

    /* Decrement argc to exclude counting the command itself, only the arguments are
     * the of interest here! The argList only contain information about the arguments.
     */
    tmpC = argc - 1;

    if (tmpC == 0)
    {
        /* Only print out the first argument in argList */
        printf("Usage: {"); i = 0;
        printf("%s", argList[i].argv[0]);
        for (i++; i < NOF_CMD; i++)
        {
            if (!isequal(argList[i].argv[0], argList[i - 1].argv[0]))
            {
                printf("|");
                printf("%s", argList[i].argv[0]);
            }
        }
        printf("}\n");
    }
    else if (tmpC < MAX_ARGC)
    {
        /* find if the given argument exist! */
        for (i = 0; i < NOF_CMD; i++)
        {
            if (isequal(argList[i].argv[tmpC - 1], argv[tmpC])) break;
        }

        if (i < NOF_CMD)
        {
            printf("Usage: %s {", argList[i].argv[0]);

            /* Search through the whole argList and print out all arguments 
             * that fits. Just print out the first one and then search for more.
             * Use {} if several arguments are defined for a element in the
             * argList.
             * Use | if their exist several combinations that fit the given
             * argument.
             */
            if (argList[i].argc > (tmpC + 1)) printf("{");
            for (j = 1; j < argList[i].argc; j++) 
            {
                printf("%s", argList[i].argv[j]);
                if (j < (argList[i].argc - 1)) printf(" ");
            }
            if (argList[i].argc > (tmpC + 1)) printf("}");

            for (i++; i < NOF_CMD; i++)
            {
                if (isequal(argList[i].argv[tmpC - 1], argv[tmpC]))
                {
                    printf(" | ");

                    if (argList[i].argc > (tmpC + 1)) printf("{");
                    for (j = 1; j < argList[i].argc; j++) 
                    {
                        printf("%s", argList[i].argv[j]);
                        if (j < (argList[i].argc - 1)) printf(" ");
                    }
                    if (argList[i].argc > (tmpC + 1)) printf("}");
                }
            }
            printf("}\n");
        }
        else
        {
            printf("error: Not Valid argument!\n");
        }
    }
    else
    {
        printf("Usage: %s\n", USAGE_AC);
    }
}

static void initAcCmdTables(void)
{
/*
    strcpy(argList[SET_AC_PARAM].argv[0],"set");
    strcpy(argList[SET_AC_PARAM].argv[1],"<freq| bandwidth| period| winlength| pwrfactor| rfport>");
    argList[SET_AC_PARAM].argc = 4;
    argList[SET_AC_PARAM].cmdPtr = setAcParam;

    strcpy(argList[GET_AC_PARAM].argv[0],"get");
    strcpy(argList[GET_AC_PARAM].argv[1],"<freq| bandwidth| period| winlength| pwrfactor| rfport| all>");
    argList[GET_AC_PARAM].argc = 3;
    argList[GET_AC_PARAM].cmdPtr = getAcParam;
*/
    strcpy(argList[GET_AC_STATUS].argv[0],"status");
    argList[GET_AC_STATUS].argc = 1;
    argList[GET_AC_STATUS].cmdPtr = getAcStatus;

    strcpy(argList[START_AC].argv[0],"start");
    strcpy(argList[START_AC].argv[1],"<carrierId| freq| bandwidth| rfPortBitmap0| fPortBitmap1| period| \
winlen| pwrFactor>");
    argList[START_AC].argc = 9;
    argList[START_AC].cmdPtr = startAC;

    strcpy(argList[STOP_AC].argv[0],"stop");
    argList[STOP_AC].argc = 2;
    argList[STOP_AC].cmdPtr = stopAC;

    strcpy(argList[SET_TX_DONE].argv[0],"setTxDone");
    argList[SET_TX_DONE].argc = 5;
    argList[SET_TX_DONE].cmdPtr = setTxDone;

    strcpy(argList[SET_RX_DONE].argv[0],"setRxDone");
    argList[SET_RX_DONE].argc = 5;
    argList[SET_RX_DONE].cmdPtr = setRxDone;

    strcpy(argList[SET_DATA_SRC].argv[0],"setDataSource");
    argList[SET_DATA_SRC].argc = 3;
    argList[SET_DATA_SRC].cmdPtr = setDataSource;

    strcpy(argList[SET_FPGA].argv[0],"fpga");
    argList[SET_FPGA].argc = 5;
    argList[SET_FPGA].cmdPtr = accessFpga;

    strcpy(argList[SET_FREQ_DATA_STORE].argv[0],"dataStore");
    argList[SET_FREQ_DATA_STORE].argc = 5;
    argList[SET_FREQ_DATA_STORE].cmdPtr = setAcFreqDataStore;
/*
    strcpy(argList[SET_DATA_SRC].argv[0],"setdatasrc");
    strcpy(argList[SET_DATA_SRC].argv[1],"<0 |1 |2 |3 |4> [pwd]");
    argList[SET_DATA_SRC].argc = 2;
    argList[SET_DATA_SRC].cmdPtr = setDataSource;

    strcpy(argList[GET_DATA_SRC].argv[0],"getdatasrc");
    argList[GET_DATA_SRC].argc = 1;
    argList[GET_DATA_SRC].cmdPtr = getDataSource;
*/
    strcpy(argList[SET_DEBUG_VALUE].argv[0],"debugvalue");
    argList[SET_DEBUG_VALUE].argc = 4;
    argList[SET_DEBUG_VALUE].cmdPtr = setDebugValue;

}

static void initCarrierParam(void)
{
    acParam.carrierFrequency = 3500000;
    acParam.carrierBandwidth = 20000;
    acParam.usedAntenna0_31 = 0xFFFF;
    acParam.usedAntenna32_63 = 0;
    acParam.usedAntenna64_95 = 0;
    acParam.usedAntenna96_127 = 0;
    acParam.acPeriod = 60000; /*1 minute;*/
    acParam.acWinLength = 1;
    acParam.powerFactor = 1;

}

static int shellCmdAC(int argc, char **argv)
{
    U16 i;

    if (argc >= 2)
    {
        for (i = 0; i < NOF_CMD; i++)
        {
            if(isequal(argList[i].argv[0], argv[1]))
            {
                argList[i].cmdPtr(argc, argv);
                break;
            }
        }

        if (i == NOF_CMD)
        {
            shellCmdDesc(argc, argv);
        }
    }
    else
    {
        shellCmdDesc(argc, argv);
    }
    return 0;
}

static void setAcParam(int argc, char **argv)
{
    if(argc == 4)
    {
        U32 value = (U32)atoi(argv[3]);    
        if(isequal(argv[2], "freq"))
        {
            acParam.carrierFrequency = value;
            printf("Set carrierFrequency to :%d \n", value);
        }
        else if(isequal(argv[2], "bandwidth")) 
        {
            acParam.carrierBandwidth = value;
            printf("Set carrierBandwidth to :%d \n", value);
        }
        else if(isequal(argv[2], "period")) 
        {
            acParam.acPeriod = value;
            printf("Set acPeriod to :%d \n", value);
        }
        else if(isequal(argv[2], "winlength")) 
        {
            acParam.acWinLength = (U8)value;
            printf("Set acWinLength to :%d \n", value);
        }
        else if(isequal(argv[2], "pwrfactor")) 
        {
            acParam.powerFactor = value;
            printf("Set powerFactor to :%d \n", value);
        }
        else
        {
            printf("Invalid command! \n");
        }
    }
    else if(argc == 7)
    {
#if 0
        U32 value1 = (U32)atoi(argv[3]);
        U32 value2 = (U32)atoi(argv[4]);
        U32 value3 = (U32)atoi(argv[5]);
        U32 value4 = (U32)atoi(argv[6]);
#endif 
        U32 value1 = (U32)strtoul(argv[3], NULL, 0);
        U32 value2 = (U32)strtoul(argv[4], NULL, 0);
        U32 value3 = (U32)strtoul(argv[5], NULL, 0);
        U32 value4 = (U32)strtoul(argv[6], NULL, 0);   
        if(isequal(argv[2], "rfport"))
        {
            acParam.usedAntenna0_31 = value1;
            acParam.usedAntenna32_63 = value2;
            acParam.usedAntenna64_95 = value3;
            acParam.usedAntenna96_127 = value4;
            printf("Set rfPort to :0x%X 0x%X 0x%X 0x%X \n",
            acParam.usedAntenna0_31 ,
            acParam.usedAntenna32_63 ,
            acParam.usedAntenna64_95 ,
            acParam.usedAntenna96_127);
        }
        else
        {
            printf("Invalid command! \n");
        }
    }
    else
    {
        shellCmdDesc(argc, argv);
        return;
    }
}

static void getAcParam(int argc, char **argv)
{
    if(argc != 3)
    {
        shellCmdDesc(argc, argv);
        return;
    }

    if(isequal(argv[2], "freq"))
    {
         printf("Frequency:%dMHz\n",acParam.carrierFrequency/1000);
    }
    else if(isequal(argv[2], "bandwidth")) 
    {
        printf("Bandwidth:%dMHz\n",acParam.carrierBandwidth/1000);
    }
    else if(isequal(argv[2], "period")) 
    {
        printf("acPeriod:%ds\n",acParam.acPeriod/1000);
    }
    else if(isequal(argv[2], "winlength")) 
    {
        printf("acWinLength:%d \n",acParam.acWinLength);
    }
    else if(isequal(argv[2], "pwrfactor")) 
    {
        printf("powerFactor:%d \n",acParam.powerFactor);
    }
    else if(isequal(argv[2], "rfport")) 
    {
        printf("rfPort:0x%X 0x%X 0x%X 0x%X \n",
            acParam.usedAntenna0_31 ,
            acParam.usedAntenna32_63 ,
            acParam.usedAntenna64_95 ,
            acParam.usedAntenna96_127);
    }
    else if(isequal(argv[2], "all")) 
    {
        printf("Frequency:%d MHz Bandwidth:%d MHz\n"
               "acPeriod:%ds acWinLength:%d powerFactor:%d \n"
               "rfPort:0x%X 0x%X 0x%X 0x%X \n",
            acParam.carrierFrequency/1000,
            acParam.carrierBandwidth/1000,
            acParam.acPeriod/1000,
            acParam.acWinLength,
            acParam.powerFactor,
            acParam.usedAntenna0_31 ,
            acParam.usedAntenna32_63 ,
            acParam.usedAntenna64_95 ,
            acParam.usedAntenna96_127);
    }
    else
    {
        shellCmdDesc(argc, argv);
        return;
    }
}

bool runAC(U32 event, U16 carrierId)
{
    if(event == CR_EVENT_CARRIER_ENABLED)
    {
        //SIGNAL *carEnableInd = alloc(sizeof(trCarrierEnable_t), TR_CARRIER_EANBLED);
        union itc_msg *carEnableInd = itc_alloc(sizeof(trCarrierEnable_t), TR_CARRIER_EANBLED);
        carEnableInd->carrierEnable.carrierId = carrierId;
        carEnableInd->carrierEnable.carrierFrequency = acParam.carrierFrequency;
        carEnableInd->carrierEnable.usedAntenna0_31 = acParam.usedAntenna0_31;
        carEnableInd->carrierEnable.usedAntenna32_63 = acParam.usedAntenna32_63;
        carEnableInd->carrierEnable.usedAntenna64_95 = acParam.usedAntenna64_95;
        carEnableInd->carrierEnable.usedAntenna96_127 = acParam.usedAntenna96_127;
        carEnableInd->carrierEnable.acPeriod = acParam.acPeriod;
        carEnableInd->carrierEnable.bandWidth = acParam.carrierBandwidth;
        printf("Carrier Enable message send, carrierId %d.\n", carEnableInd->carrierEnable.carrierId);

        if(ac_app_mbox == ITC_NO_ID)
        {
            if(!get_mbox(AC_DISPATCH_MAILBOX, &ac_app_mbox))
            {
                //TPT_ERROR(STR("Can not get ac_main_mbox !"));
                return false;
            }
        }
        //send(&carEnableInd, acCtrlPoc);
        itc_send(&carEnableInd, ac_app_mbox, ITC_MY_MBOX);   
    }
    else if(event == CR_EVENT_CARRIER_DISABLED) 
    {
        //SIGNAL *carEnableInd = alloc(sizeof(trCarrierEnable_t), TR_CARRIER_EANBLED);
        union itc_msg *carDisableInd = itc_alloc(sizeof(trCarrierDisable_t), TR_CARRIER_DISABLED);
        carDisableInd->carrierDisable.carrierId = carrierId;
        printf("Carrier Disable message send, carrierId %d.\n", carDisableInd->carrierDisable.carrierId);

        if(ac_app_mbox == ITC_NO_ID)
        {
            if(!get_mbox(AC_DISPATCH_MAILBOX, &ac_app_mbox))
            {
                //TPT_ERROR(STR("Can not get ac_main_mbox !"));
                return false;
            }
        }
        //send(&carEnableInd, acCtrlPoc);
        itc_send(&carDisableInd, ac_app_mbox, ITC_MY_MBOX);   
    }
    else
    {
        printf("Carrier event is invalid.\n");
        return false;
    }
    return true;
}

static void  startAC(int argc, char **argv)
{
    if(argc != 2 && argc != 10)
    {
        shellCmdDesc(argc, argv);
        return;
    }
    printf("startAc test.\n");
    U16 carrierId = 1;  /*1 for DL, 2 for UL*/
    if(argc == 10)
    {
        //CarrierId:
        U32 value2 = (U32)atoi(argv[2]);
        carrierId = (U16)value2;
        //Freq:
        U32 value3 = (U32)atoi(argv[3]);
        acParam.carrierFrequency = value3;
        //Bandwidth:
        U32 value4 = (U32)atoi(argv[4]);
        acParam.carrierBandwidth = value4;
        //rfPortBitmap0, rfPortBitmap1
        U32 value5 = (U32)strtoul(argv[5], NULL, 0);
        U32 value6 = (U32)strtoul(argv[6], NULL, 0);
        acParam.usedAntenna0_31 = value5;
        acParam.usedAntenna32_63 = value6;
        //period
        U32 value7 = (U32)atoi(argv[7]);
        acParam.acPeriod = value7;
        //winlen
        U32 value8 = (U32)atoi(argv[8]);
        acParam.acWinLength = (U8)value8;
        //pwrFactor
        U32 value9 = (U32)atoi(argv[9]);
        acParam.powerFactor = value9;
        printf("Set AC Parameter :CarrierId %d, Freq %d, BW %d, rfPortBitMap0 0x%X, rfPortBitMap1 0x%X,\
 acPeriod %d, winlen %d, pwrFactor %d.\n",
        carrierId,
        acParam.carrierFrequency,
        acParam.carrierBandwidth,
        acParam.usedAntenna0_31,
        acParam.usedAntenna32_63,
        acParam.acPeriod,
        acParam.acWinLength,
        acParam.powerFactor);
    }

    const U32 event = CR_EVENT_CARRIER_ENABLED;

    //Enable AC
    if(!runAC(event,carrierId))
    {
        printf("startAc Failed for UL \n");
    }
}

static void  stopAC(int argc, char **argv)
{
    if(argc != 2 && argc != 3)
    {
        shellCmdDesc(argc, argv);
        return;
    }

    U16 carrierId = 1;  
    const U32 event = CR_EVENT_CARRIER_DISABLED;

    if(argc == 3)
    {
        //CarrierId:
        U32 value2 = (U32)atoi(argv[2]);
        carrierId = (U16)value2;
    }
    printf("Stop AC for CarrierId %d.\n", carrierId);

    //Disable AC
    if(!runAC(event,carrierId))
    {
        printf("stopAc Failed for DL \n");
    } 
}

static void  getAcStatus(int argc, char **argv)
{
    union itc_msg *acStatusReq = itc_alloc(sizeof(trAcStatus_t), TR_AC_STATUS);
    acStatusReq->trAcStatus.carrierId= 0;

    if(ac_app_mbox == ITC_NO_ID)
    {
        if(!get_mbox(AC_DISPATCH_MAILBOX, &ac_app_mbox))
        {
            //TPT_ERROR(STR("Can not get ac_main_mbox !"));
            return false;
        }
    }
    itc_send(&acStatusReq, ac_app_mbox, ITC_MY_MBOX);    
}

/*command:  ./actest dataSrc 0 */
static void  setDataSource(int argc, char **argv)
{
    if(argc != 3)
    {
        shellCmdDesc(argc, argv);
        return;
    }

    U16 acDataSrc = 0;
    if(argc == 3)
    {
       acDataSrc  = (U16)atoi(argv[2]);
    }
    printf("acTest Set AC Data Source %d.\n", acDataSrc);

    union itc_msg *acDataSrcSet = itc_alloc(sizeof(trAcDataSrc_t), TR_AC_DATASRC);
    acDataSrcSet->trAcDataSrc.dataSrc= acDataSrc;

    if(ac_app_mbox == ITC_NO_ID)
    {
        if(!get_mbox(AC_DISPATCH_MAILBOX, &ac_app_mbox))
        {
            //TPT_ERROR(STR("Can not get ac_main_mbox !"));
            return false;
        }
    }
    itc_send(&acDataSrcSet, ac_app_mbox, ITC_MY_MBOX);    
}

static void setDebugValue(int argc, char**argv)
{
    if(argc != 4)
    {
        shellCmdDesc(argc, argv);
        return;
    }

    U16 debugName = 0;
    double debugValue = 0;
    if (argc == 4)
    {
        debugName = (U16)atoi(argv[2]);
        debugValue = (float)atof(argv[3]);
    }
    printf("acTest Set debug name %d to %f.\n", debugName, debugValue);

    union itc_msg *acDebugValueSet = itc_alloc(sizeof(trAcDebugValue_t), TR_AC_DEBUGVALUE);
    acDebugValueSet->trDebugValueSet.debugName = debugName;
    acDebugValueSet->trDebugValueSet.debugValue = debugValue;

    if(ac_app_mbox == ITC_NO_ID)
    {
        if(!get_mbox(AC_DISPATCH_MAILBOX, &ac_app_mbox))
        {
            return false;
        }
    }
    itc_send(&acDebugValueSet, ac_app_mbox, ITC_MY_MBOX);	  
}

static void  getDataSource(int argc, char **argv)
{
/*
    AcDispatcher* AcDpch_p = AcDispatcher::getInstance();

    if(argc == 2)
    {
        U16 value = 0;
        AcDpch_p->getAcFreqDataSrc(value);
        printf("Data source is :%d \n", value);
    }
    else
    {
        shellCmdDesc(argc, argv);
    }
*/
}

/*
./actest txDone 1 0x0000000F 0x00000000
./actest rxDone 1 0x0000000F 0x00000000
*/
static void  setTxDone(int argc, char **argv)
{
    if(argc != 5)
    {
        shellCmdDesc(argc, argv);
        return;
    }
    printf("setTxDone test.\n");
    U16 carrierId = 1;  /*1 for DL, 2 for UL*/
    U32 usedAntenna[2] = {0,0};  
    if(argc == 5)
    {
        //CarrierId:
        U32 value2 = (U32)atoi(argv[2]);
        carrierId = (U16)value2;
        //rfPortBitmap0, rfPortBitmap1
        U32 value3 = (U32)strtoul(argv[3], NULL, 0);
        U32 value4 = (U32)strtoul(argv[4], NULL, 0);
        usedAntenna[0] = value3;
        usedAntenna[1] = value4;
        printf("Set AC TxDone :CarrierId %d, rfPortBitMap0 0x%08X, rfPortBitMap1 0x%08X. \n",
        carrierId,
        usedAntenna[0],
        usedAntenna[1]);
    }

    union itc_msg *msg;
    for (U16 k = 0; k < ULCELLPE_AC_MAX_ANT_PER_CELL; k++)
    {
        if((usedAntenna[k/32] >> (k%32)) & 0x01)
        {
            msg = itc_alloc(sizeof(intTxAcDone_t), INT_TX_AC_DONE_IND);
            msg->txAcDone.carrierId = carrierId;
            msg->txAcDone.portId = k;
            printf("INT_TX_AC_DONE_IND msg sent to AC_Control: carrierId %d, portId %d.\n", 
                  msg->txAcDone.carrierId, msg->txAcDone.portId);
            if(ac_app_mbox == ITC_NO_ID)
            {
                if(!get_mbox(AC_DISPATCH_MAILBOX, &ac_app_mbox))
                {
                    //TPT_ERROR(STR("Can not get ac_main_mbox !"));
                    return false;
                }
            }
            itc_send(&msg, ac_app_mbox, ITC_MY_MBOX);
            usleep(10000);
        }
    }
}

static void  setRxDone(int argc, char **argv)
{
    if(argc != 5)
    {
        shellCmdDesc(argc, argv);
        return;
    }
    printf("setRxDone test.\n");
    U16 carrierId = 1;  /*1 for DL, 2 for UL*/
    U32 usedAntenna[2] = {0,0};  
    if(argc == 5)
    {
        //CarrierId:
        U32 value2 = (U32)atoi(argv[2]);
        carrierId = (U16)value2;
        //rfPortBitmap0, rfPortBitmap1
        U32 value3 = (U32)strtoul(argv[3], NULL, 0);
        U32 value4 = (U32)strtoul(argv[4], NULL, 0);
        usedAntenna[0] = value3;
        usedAntenna[1] = value4;
        printf("Set AC RxDone :CarrierId %d, rfPortBitMap0 0x%08X, rfPortBitMap1 0x%08X. \n",
        carrierId,
        usedAntenna[0],
        usedAntenna[1]);
    }

    union itc_msg *msg;
    for (U16 k = 0; k < ULCELLPE_AC_MAX_ANT_PER_CELL; k++)
    {
        if((usedAntenna[k/32] >> (k%32)) & 0x01)
        {
            msg = itc_alloc(sizeof(intRxAcDone_t), INT_RX_AC_DONE_IND);
            msg->rxAcDone.carrierId = carrierId;
            msg->rxAcDone.portId = k;
            printf("INT_RX_AC_DONE_IND msg sent to AC_Control: carrierId %d, portId %d.\n", 
                  msg->rxAcDone.carrierId, msg->rxAcDone.portId);
            if(ac_app_mbox == ITC_NO_ID)
            {
                if(!get_mbox(AC_DISPATCH_MAILBOX, &ac_app_mbox))
                {
                    //TPT_ERROR(STR("Can not get ac_main_mbox !"));
                    return false;
                }
            }
            itc_send(&msg, ac_app_mbox, ITC_MY_MBOX);
            usleep(10000);
        }
    }
}

/*
./actest fpga r 0x001
./actest fpga w 0x021 0xF0
*/
static void  accessFpga(int argc, char **argv)
{
    if(argc != 4 && argc != 5)
    {
        shellCmdDesc(argc, argv);
        return;
    }
    //printf("FPGA Read/Write test.\n");
    
    union itc_msg *msg;
    if(isequal(argv[2], "r"))
    {
        U32 addr = (U32)strtoul(argv[3],0,0);
        msg = itc_alloc(sizeof(trAcFpgaAccess_t), TR_AC_FPGA_ACCESS);
        msg->trAcFpgaAcce.operFlag = 1;
        msg->trAcFpgaAcce.addr = addr;
        msg->trAcFpgaAcce.data = 0;
        //printf("TR_AC_FPGA_ACCESS Read msg sent to AC: addr 0x%08X.\n", 
        //          msg->trAcFpgaAcce.addr);
        if(ac_app_mbox == ITC_NO_ID)
        {
            if(!get_mbox(AC_DISPATCH_MAILBOX, &ac_app_mbox))
            {
                //TPT_ERROR(STR("Can not get ac_main_mbox !"));
                return;
            }
        }
        itc_send(&msg, ac_app_mbox, ITC_MY_MBOX);
    }
    else if(isequal(argv[2], "w"))
    {
        U32 addr = (U32)strtoul(argv[3],0,0);
        U32 data = (U32)strtoul(argv[4],0,0);
        msg = itc_alloc(sizeof(trAcFpgaAccess_t), TR_AC_FPGA_ACCESS);
        msg->trAcFpgaAcce.operFlag = 2;
        msg->trAcFpgaAcce.addr = addr;
        msg->trAcFpgaAcce.data = data;
        //printf("TR_AC_FPGA_ACCESS Write msg sent to AC: addr 0x%08X, data 0x%08X.\n", 
        //          msg->trAcFpgaAcce.addr, msg->trAcFpgaAcce.data);
        if(ac_app_mbox == ITC_NO_ID)
        {
            if(!get_mbox(AC_DISPATCH_MAILBOX, &ac_app_mbox))
            {
                //TPT_ERROR(STR("Can not get ac_main_mbox !"));
                return;
            }
        }
        itc_send(&msg, ac_app_mbox, ITC_MY_MBOX);
    }
    else
    {
        printf("FPGA Operation error: only support r|w.\n");
        return;
    }
}

static void  setAcFreqDataStore(int argc, char **argv)
{
    if(argc != 5)
    {
        shellCmdDesc(argc, argv);
        return;
    }
    printf("setAcFreqDataStore test.\n");
    U16 carrierId = 1;  /*1 for DL, 2 for UL*/
    //CarrierId:
    U32 value2 = (U32)atoi(argv[2]);
    carrierId = (U16)value2;
    //rfPortBitmap0, rfPortBitmap1
    U32 value3 = (U32)strtoul(argv[3], NULL, 0);
    U32 value4 = (U32)strtoul(argv[4], NULL, 0);

    union itc_msg *acFreqDataStoreSet = itc_alloc(sizeof(trFreqDataStoreSetting_t), 
                TR_AC_FREQDATA_STORE_SET);
    acFreqDataStoreSet->trFreqDataStoreSet.carrierId = carrierId;
    acFreqDataStoreSet->trFreqDataStoreSet.usedAntenna0_31 = value3;
    acFreqDataStoreSet->trFreqDataStoreSet.usedAntenna32_63 = value4;
    printf("TR_AC_FREQDATA_STORE_SET msg send, carrierId %d.\n", carrierId);

    if(ac_app_mbox == ITC_NO_ID)
    {
        if(!get_mbox(AC_DISPATCH_MAILBOX, &ac_app_mbox))
        {
            //TPT_ERROR(STR("Can not get ac_main_mbox !"));
            return false;
        }
    }
    //send(&carEnableInd, acCtrlPoc);
    itc_send(&acFreqDataStoreSet, ac_app_mbox, ITC_MY_MBOX);   
}

int main(int argc, char **argv)
{
    int ret = EXIT_FAILURE;
    U16 i;

    initAcCmdTables();
    initCarrierParam();

    if(init_itc()) {
        return EXIT_FAILURE;
    }
	
    if(argc == 1) {
        shellCmdDesc(argc, argv);
        //PRINT_USAGE();
        return EXIT_FAILURE;
    }

    if (argc >= 2)
    {
        for (i = 0; i < NOF_CMD; i++)
        {
            if(isequal(argList[i].argv[0], argv[1]))
            {
                argList[i].cmdPtr(argc, argv);
                break;
            }
        }

        if (i == NOF_CMD)
        {
            shellCmdDesc(argc, argv);
        }
    }
    else
    {
        shellCmdDesc(argc, argv);
    }
    delete_itc();
    return 0;
}





