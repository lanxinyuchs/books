/* > Description **************************************************************/
/*
 * Copyright Ericsson AB
 *
 * The copyright to the computer programs herein is the property of Ericsson AB.
 * The programs may be used and/or copied only with the written permission from
 * Ericsson AB or in accordance with the terms conditions stipulated in the
 * agreement/contract under which the programs have been supplied.
 *
 ******************************************************************************/
/**
 * @file ulcellpe_ac_common.h
 * @brief Header file for the antenna calibration signal processing lib.
 *
 *
 * @see 159 41-FCP 121 6837 Uen
 *
 ******************************************************************************/
#ifndef ACALGORITHMCOMMON_H
#define ACALGORITHMCOMMON_H

#include <ose.h>
#include <osetypes.h>

/* > Defines ******************************************************************/

#define REF_SIG_NO_OF_WORDS_20_MHZ  2402
#define REF_SIG_NO_OF_WORDS_15_MHZ  1802
#define REF_SIG_NO_OF_WORDS_10_MHZ  1202
#define REF_SIG_NO_OF_WORDS_5_MHZ   602

#define ARCTAN_LUT_SIZE 32

#define REF_SIG_NO_OF_SC_20_MHZ  1201
#define REF_SIG_NO_OF_SC_15_MHZ   901
#define REF_SIG_NO_OF_SC_10_MHZ   601
#define REF_SIG_NO_OF_SC_5_MHZ    301

#define AC_MAX_DATA_LENGTH_IN_WORD  2402

#define Q30_MAX 1073741824
#define AC_DELAY_UPSHIFT 4
#define Q15_PI 32767 //Q15 representation of pi. Largest possible value i.e ~1
#define PI_DEGREE 180UL
/* To compensate for input format for log10 function log10(2^15)<<15
   Reason is integer is interpreted as number with 15 bits decimal.*/
#define LOG10_16BIT_SHIFT_COMP 147962
#define AC_MAX_ALIGN_SIZE 10
#define AC_MAX_WORKAREA_SIZE ((AC_MAX_DATA_LENGTH_IN_WORD * 2) + AC_MAX_ALIGN_SIZE)
#define Q14 16384
#define Q15_2PI 65536L //representation of 2PI
#define Q18_MAX 262144


/* > Typedefs *****************************************************************/

/* > Constant Declarations ****************************************************/

/* > Variable Declarations ****************************************************/
extern S16 ArmComplexRefSignal5Mhz[REF_SIG_NO_OF_WORDS_5_MHZ];
extern S16 ArmComplexRefSignal10Mhz[REF_SIG_NO_OF_WORDS_10_MHZ];
extern S16 ArmComplexRefSignal15Mhz[REF_SIG_NO_OF_WORDS_15_MHZ];
extern S16 ArmComplexRefSignal20Mhz[REF_SIG_NO_OF_WORDS_20_MHZ];
extern U32 LUT[ARCTAN_LUT_SIZE];

#endif /* UP_ULCELLPEBL_SPLIBAC_SRC_ULCELLPE_AC_COMMON_H_*/
