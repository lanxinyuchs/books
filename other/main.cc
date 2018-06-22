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
 *  2016-09-22           edentao           First Revision
 *
 *******************************************************************************
 */

/* >  2  INCLUDE FILES
 *******************************************************************************
 */
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

#include <stdarg.h>
#include <string.h>
#include <math.h> 

 #include <vector>
#include <list>
#include <map>
#include <set>
#include <math.h>
#include <itc.h>
#include "ac_defs.h"
#include "acSignal.h"
#include "acDispatch.h"
#include "acIntHandler.h"
#include "time.h"

/* >  3  DECLARATIONS AND DEFINITIONS
 ******************************************************************************
 */
#if 1
#define TRACEPOINT_PROVIDER     com_ericsson_ac_trace_main
#include "tpt_create.h"
#include "tpt.h"
#endif

#define MAX_NOF_CARRIER        4

/* >  3.1  GLOBAL
 ******************************************************************************
 */
static itc_mbox_id_t ac_mbox = 0;

extern void ruAcTestCmd(void);

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
#if 0
char *ac_format_str(const char *format, ...)
{
	static __thread char internal_trace_buf[TPT_MAX_TRACE_LEN] = {0};
	va_list args;  /* Argument list */

	va_start(args, format);
	vsnprintf(internal_trace_buf, TPT_MAX_TRACE_LEN, format, args);
	va_end(args);

	return internal_trace_buf;
} 
#endif

bool processExist(char *name)
 {
    FILE *fptr;
    bool bret = false;
    char cmd[255] = {'\0'};
    char buf[255] = {'\0'};
    sprintf(cmd,"ps -e | grep -wc %s",name);
    if((fptr = popen(cmd,"r")) != NULL)
    {
        if(fgets(buf,255,fptr) != NULL)
        {
            if(atoi(buf) >= 2)
            {
                TPT_INFO(STR("the %s process number is %d.",name, atoi(buf)));
                bret = true;
            }
        }
    }
    pclose(fptr);
    return bret;
 }


int main(int argc, char** argv) 
{
    char * processName = "ac";
    if (processExist(processName))
    {
        printf("The process %s is existed, failed to start.\n",processName);
        return 0;
    }

    TPT_INFO(STR("AC Control process is initializing."));
    /* Initialize ITC */
    itc_init(MAX_MAILBOX_NUM, ITC_MALLOC, NULL, ITC_NO_NAMESPACE, 0);

    /* Create our mailbox. */
    ac_mbox = itc_create_mailbox(AC_DISPATCH_MAILBOX, 0);
        
    if (ac_mbox == ITC_NO_ID)
    {
        //ret = -EFAULT;
        //goto main_end;
        return 0;
    }
		
    //init AcDispatcher
    AcDispatcher* AcDpch_p = AcDispatcher::getInstance();
    AcDpch_p->init("AcDispatcher");
    AcDpch_p->testInterfaceInit();


    //init Ac Interrupt Handler
    //removed AcIntHandler, since the interrupt is changed in 2.6G
    //AcIntHandler* AcIntHdl_p = AcIntHandler::getInstance();
    //AcIntHdl_p->init("AcIntHandler");
    //AcIntHdl_p->testInterfaceInit();
    //ruAcTestCmd();

    uint32_t anySig[] = {0};
    union itc_msg* recSig;
    for(;;)
    {
        recSig = itc_receive(anySig, ITC_NO_TMO, ITC_FROM_ALL);
        /*for coverity eradication*/
        if(NULL != recSig)
        {
            TPT_INFO(STR("AC Control Process recv signal 0x%X ", (recSig->sigNo)));
            switch (recSig->sigNo)
            {
                case TR_AC_SETTING_REQ:
                    AcDpch_p->handleAcSettingReq(recSig);
                    break;
                case AC_COMP_VALUE_HWDB:
                    AcDpch_p->handleAcCompHwDb(recSig);
                    break;
                case TR_CARRIER_EANBLED:
                    AcDpch_p->handleAcCarrierEnable(recSig);
                    break;
                case TR_CARRIER_PENDING:
                    AcDpch_p->handleAcCarrierPending(recSig);
                    break;
                case TR_CARRIER_DISABLED:
                    AcDpch_p->handleAcCarrierDisable(recSig);
                    break;
                case INT_TX_AC_DONE_IND:
                    AcDpch_p->handleTxAcDone(recSig);
                    break;
                case INT_RX_AC_DONE_IND:
                    AcDpch_p->handleRxAcDone(recSig);
                    break;
                case TR_AC_STATUS:
                    AcDpch_p->displayAcSession();
                    break;
                case TR_AC_DATASRC:
                    AcDpch_p->setAcFreqDataSrc(recSig);
                    break;
                case SIG_TIMEOUT_IND:
                    AcDpch_p->handleTimeOutInd(recSig);
                    break;
                case DC_TR_MODIFY_CARRIER_DELAY_CFM:
                    AcDpch_p->handleRespCfm(recSig);
                    break;
                case DC_TR_MODIFY_CARRIER_DELAY_REJ:
                    AcDpch_p->handleRespReject(recSig);
                    break;
                case TR_AC_FPGA_ACCESS:
                    AcDpch_p->handleAcFpgaAccess(recSig);
                    break;
                case AC_INT_IND:
                    AcDpch_p->handleIntAcDoneInd(recSig);
                    break;
                case TR_AC_FREQDATA_STORE_SET:
                    AcDpch_p->handleFreqDataStoreSetting(recSig);
                    break;
                case TR_AC_DEBUGVALUE:
                    AcDpch_p->handleDebugValueSetting(recSig);
                default:
                    TPT_INFO(STR("AC Control Process recv invalid signal,sigNo = 0x%08X.",
                        recSig->sigNo));
                    break;
            }
            if(recSig)
                 //free_buf(&recSig);
                 itc_free(&recSig);
        }
    }
      
    return 1;
}

