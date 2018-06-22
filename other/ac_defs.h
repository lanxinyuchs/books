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
 3  Declarations
 4  Functions

 ******************************************************************************
 */

/* >  1  DESCRIPTION
 *******************************************************************************
 *
 * <text...>
 *
 *
 *
 * Revision history
 *     Date              Author             Description
 *  2016-09-21           edentao           First Revision
 *
 *******************************************************************************
 */

/* >  Double inclusion protection
 ******************************************************************************
 */

#ifndef __AC_DEFS_H__
#define __AC_DEFS_H__

#include <list>
#include <vector>
#include <complex>
#include <itc.h>
/* >  DECLARATIONS
 ******************************************************************************
 */
#define filename(x) strrchr(x,'/')?strrchr(x,'/')+1:x

#if 0
#include "tpt.h"
#undef TPT_ERROR 
#undef TPT_INFO
//#undef STR 
//#define TPT_MAX_TRACE_LEN    512
//extern char *ac_format_str(const char *format, ...);
//#define STR (ac_format_str) 
#define TPT_INFO(msg) printf("%s %d: %s \n", filename(__FILE__), __LINE__, msg)
#define TPT_ERROR(msg) printf("Line %d %s \n", __LINE__, msg) 
//#define TPT_TRACE(group, msg) printf("File %s Line %d: %s \n", __FILE__, __LINE__, msg) 
#endif


#define U32        uint32_t
#define S32        int32_t
#define U16        uint16_t
#define S16        int16_t
#define U8         uint8_t
#define S8         int8_t

/* Frequency Range */
#define TX_LOW_EDGE            2575000 //kHz
#define TX_HIGH_EDGE           2615000
#define RX_LOW_EDGE            2575000
#define RX_HIGH_EDGE           2615000

#define TX_MAX_BANDWIDTH       60000
#define RX_MAX_BANDWIDTH       60000

#define NOF_TRX_FPGA           2
#define NUM_OF_RF_PORT         64
#define NUM_OF_TX_DEVICE       8
#define NUM_OF_RX_DEVICE       8

#define AC_DISPATCHER_NAME          "AcDispatcher"
#define RADIO_BP_MAILBOX          "trDcProc"
#define APP_FAULT_MGR_MBOX        "faultManagerProc"
#define AC_FAULT_UNWRAP_THR       1000
#define AC_FAULT_CHANNEL_THR      300
#define AC_FAULT_DELAY_THR        300
#define AC_STARTUP_DELAY          20000

/*mailbox*/
#define AC_DISPATCH_MAILBOX          "AC_DISPATCH"

/*mailbox*/
#define RHD_INT_MAILBOX          "RHD_INT"


#define MAX_MAILBOX_NUM    32

#define RET_SUCCESS        0


#define TRAFFIC_XDOS_FPGA_DEV               "bp_ac_control"
#define TRAFFIC_XDOS_AC_DEV                 "ac_mem"
 
// temporarily moved here from SAL_Types.h (to be seen from RX module)
enum TrCarrierDirection_t
{
    TR_CARRIER_DIRECTION_UL = 0,
    TR_CARRIER_DIRECTION_DL
};

#endif /* __AC_DEFS_H__ */

/*********************************** END **************************************/
