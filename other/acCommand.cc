/*
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
 */


/* > DESCRIPTION
 ******************************************************************************
 *
 * Revision history can be found in Clearcase
 *
 ******************************************************************************
 */


/* > INCLUDE FILES
 ******************************************************************************
 */
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include <ose.h>
#include <osetypes.h>
#include "shell.h"
//#include <xpai_xte_if_trace.h>
//#include <xpai_xte_if_ose.h>

//#include "hali.h"
//#include "drvFactory.h"

//#include "test.sig"
//#include "acHandler.h"
//#include "ac_defs.h"
//#define TRACEPOINT_PROVIDER     com_ericsson_ac_trace
//#include "tpt_create.h"
//#include "tpt.h"
#include "ac_defs.h"
#include "acDispatch.h"
#include "acSignal.h"


/* > MACRO DEFINITIONS
 ******************************************************************************
 */
#define CMD_AC        "ac"
#define USAGE_AC      "type 'ac' for command list"
#define DESC_AC       "Access to AC command"

#define NAME_SIZE           100
#define MAX_ARGC            2

static itc_mbox_id_t ac_main_mbox;
//extern static itc_mbox_id_t ac_main_mbox;
extern bool get_mbox(const char *mbox_name, itc_mbox_id_t *mbox_id);
enum {
    SET_AC_PARAM,
    GET_AC_PARAM,
    GET_AC_STATUS,
    START_AC,
    STOP_AC,
    SET_DATA_SRC,
    GET_DATA_SRC,
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
#define isequal(a,b) !strcmp(a,b)
/* > TYPE DEFINITIONS
 ******************************************************************************
 */
 typedef struct
 {
	 char argv[MAX_ARGC][NAME_SIZE];
	 int  argc;
	 void (*cmdPtr)(int, char**);
 } argEntry_t;

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
 
 
/* > LOCAL DECLARATIONS
 ******************************************************************************
 */
static argEntry_t argList[NOF_CMD];
static acParam_t  acParam;
//static TrCarrierConfD_t ulCarrierConf;
//static TrCarrierConfD_t dlCarrierConf;
/* > GLOBAL DECLARATIONS
 ******************************************************************************
 */
 
 
/* SIGNAL DEFINITIONS
 ******************************************************************************
 */

 
 
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

 
/* > GLOBAL FUNCTION DEFINITIONS
 ******************************************************************************
 */
void ruAcTestCmd(void)
{
    initAcCmdTables();
    initCarrierParam();
    shell_add_cmd(CMD_AC, USAGE_AC, DESC_AC, shellCmdAC);
}

/* > LOCAL FUNCTION DEFINITIONS
 ******************************************************************************
 */
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
    strcpy(argList[SET_AC_PARAM].argv[0],"set");
    strcpy(argList[SET_AC_PARAM].argv[1],"<freq| bandwidth| period| winlength| pwrfactor| rfport>");
    argList[SET_AC_PARAM].argc = 4;
    argList[SET_AC_PARAM].cmdPtr = setAcParam;

    strcpy(argList[GET_AC_PARAM].argv[0],"get");
    strcpy(argList[GET_AC_PARAM].argv[1],"<freq| bandwidth| period| winlength| pwrfactor| rfport| all>");
    argList[GET_AC_PARAM].argc = 3;
    argList[GET_AC_PARAM].cmdPtr = getAcParam;

    strcpy(argList[GET_AC_STATUS].argv[0],"status");
    argList[GET_AC_STATUS].argc = 1;
    argList[GET_AC_STATUS].cmdPtr = getAcStatus;

    strcpy(argList[START_AC].argv[0],"start");
    argList[START_AC].argc = 1;
    argList[START_AC].cmdPtr = startAC;

    strcpy(argList[STOP_AC].argv[0],"stop");
    argList[STOP_AC].argc = 1;
    argList[STOP_AC].cmdPtr = stopAC;

    strcpy(argList[SET_DATA_SRC].argv[0],"setdatasrc");
    strcpy(argList[SET_DATA_SRC].argv[1],"<0 |1 |2 |3 |4> [pwd]");
    argList[SET_DATA_SRC].argc = 2;
    argList[SET_DATA_SRC].cmdPtr = setDataSource;

    strcpy(argList[GET_DATA_SRC].argv[0],"getdatasrc");
    argList[GET_DATA_SRC].argc = 1;
    argList[GET_DATA_SRC].cmdPtr = getDataSource;
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

bool runAC(U32 event, U16 deviceId, TrCarrierDirection_t carrierDirection)
{
    U32 usedAntenna;
    U32 clientId = 3;
    for(int i =0; i<128; i++)
    {
        if(i<32) usedAntenna = acParam.usedAntenna0_31;
        else if( i< 64) usedAntenna = acParam.usedAntenna32_63;
        else if( i< 96) usedAntenna = acParam.usedAntenna64_95;
        else if( i< 128) usedAntenna = acParam.usedAntenna96_127;
        
        if(usedAntenna &(1<<(i%32)))
        {
#if 0
            TrCarrierConfD_t carrierConf;
            SALCarrierGeneralPolicy_t salGp;
            U32 rfPortNo = i;
            U32 carrierId = deviceId + (((U32)clientId & 0xFFFF)<<16) + (((U32)rfPortNo & 0xFF)<<8);

            salGp.resourceId = 1;
            salGp.ibwType = SAL_IBW_INVALID;
            salGp.powerClassType = SAL_POWER_CLASS_5W;

            carrierConf.carrierId = carrierId;
            carrierConf.carrierRfPort = rfPortNo;
            carrierConf.carrierDirection = carrierDirection;
            carrierConf.carrierFrequency = acParam.carrierFrequency;
            carrierConf.carrierBandwidth = acParam.carrierBandwidth;
            carrierConf.usedAntenna0_31 = acParam.usedAntenna0_31;
            carrierConf.usedAntenna32_63 = acParam.usedAntenna32_63;
            carrierConf.usedAntenna64_95 = acParam.usedAntenna64_95;
            carrierConf.usedAntenna96_127 = acParam.usedAntenna96_127;
            carrierConf.acPeriod = acParam.acPeriod;

            if (!Hali::set("acHandler", event, carrierConf, salGp))
            {
                return false;
            }
#endif
            //SIGNAL *carEnableInd = alloc(sizeof(trCarrierEnable_t), TR_CARRIER_EANBLED);
            itc_msg *carEnableInd = itc_alloc(sizeof(trCarrierEnable_t), TR_CARRIER_EANBLED);
            carEnableInd->carrierEnable.carrierId = 1;
            carEnableInd->carrierEnable.carrierFrequency = acParam.carrierFrequency;
            carEnableInd->carrierEnable.usedAntenna0_31 = acParam.usedAntenna0_31;
            carEnableInd->carrierEnable.usedAntenna32_63 = acParam.usedAntenna32_63;
            carEnableInd->carrierEnable.usedAntenna64_95 = acParam.usedAntenna64_95;
            carEnableInd->carrierEnable.usedAntenna96_127 = acParam.usedAntenna96_127;
            carEnableInd->carrierEnable.acPeriod = acParam.acPeriod;

            if(ac_main_mbox == ITC_NO_ID)
            {
                if(get_mbox(AC_DISPATCH_MAILBOX, &ac_main_mbox))
                {
                    //TPT_ERROR(STR("Can not get ac_main_mbox !"));
                    return false;
                }
            }
#if 0
        if(!hunt(AC_CONTROL_PROC, 0, &acCtrlPoc, 0))
        {
            TRACE_ERROR(STR("ac control process can't be found."));
            free_buf(&carEnableInd);
            return false;
        }
#endif
            //send(&carEnableInd, acCtrlPoc);
            itc_send(&carEnableInd, ac_main_mbox, ITC_MY_MBOX);     
        }
    }
    return true;
}

static void  startAC(int argc, char **argv)
{
    if(argc != 2)
    {
        shellCmdDesc(argc, argv);
        return;
    }

    const U32 event = CR_EVENT_CARRIER_ENABLED;
    //carrierId = deviceId + (((U32)clientId & 0xFFFF)<<16) + (((U32)rfPortNo & 0xFF)<<8);

    //DL AC
    U16 deviceId = 1;  /*1 for DL, 2 for UL*/
    TrCarrierDirection_t carrierDirection = TR_CARRIER_DIRECTION_DL;
    if(!runAC(event,deviceId,carrierDirection))
    {
        printf("startAc Failed for UL \n");
    } 
    
    //UL AC
    deviceId = 2; 
    carrierDirection = TR_CARRIER_DIRECTION_UL;
    if(!runAC(event,deviceId,carrierDirection))
    {
        printf("startAc Failed for DL \n");
    }
}

static void  stopAC(int argc, char **argv)
{
    if(argc != 2)
    {
        shellCmdDesc(argc, argv);
        return;
    }

    const U32 event = CR_EVENT_CARRIER_DISABLED;

    //DL AC
    U16 deviceId = 1;  /*1 for DL, 2 for UL*/
    TrCarrierDirection_t carrierDirection = TR_CARRIER_DIRECTION_DL;
    if(!runAC(event,deviceId,carrierDirection))
    {
        printf("stopAc Failed for DL \n");
    } 
    
    //UL AC
    deviceId = 2;
    carrierDirection = TR_CARRIER_DIRECTION_UL; 
    if(!runAC(event,deviceId,carrierDirection))
    {
        printf("stopAc Failed for UL \n");
    } 
}

static void  getAcStatus(int argc, char **argv)
{
    AcDispatcher* AcDpch_p = AcDispatcher::getInstance();

    if(argc == 2)
    {
        AcDpch_p->displayAcSession();
    }
    else
    {
        shellCmdDesc(argc, argv);
    }
}
static void  setDataSource(int argc, char **argv)
{
    bool  paramValid = true;
    AcDispatcher* AcDpch_p = AcDispatcher::getInstance();

    if(argc == 4)
    {
        U16 value = (U32)atoi(argv[2]);
        if((value<5) &&(value>0))
        {
            if(!strcmp(argv[3],"mimo"))
            {
                AcDpch_p->setAcFreqDataSrc(value);
                printf("Set data source to :%d \n", value);
            }
            else
            {
                paramValid = false;
            }
        }
        else
        {
            paramValid = false;
        }         
    }
    else if(argc == 3)
    {
        U16 value = (U32)atoi(argv[2]);
        if(value)
        {
            paramValid = false;
        }
        else
        { 
            AcDpch_p->setAcFreqDataSrc(value);
            printf("Set data source to :%d \n", value);
        }
    }
    else
    {
        paramValid = false;
    }

    if(!paramValid)
    {
        shellCmdDesc(argc, argv);
        printf("Data source:\n 0 -- FPGA;\n 1 -- Good data;\n 2 -- Bad delay data; \n");
        printf(" 3 -- Bad disturb data;\n 4 -- Signal level too low data \n");
        printf("For data source 1-4, need password\n");
    }       
}

static void  getDataSource(int argc, char **argv)
{
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
}

