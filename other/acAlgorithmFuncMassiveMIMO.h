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
 * @file ulcellpe_ac.h
 * @brief Header file for the antenna calibration signal processing lib.
 *
 *
 * @see 159 41-FCP 121 6837 Uen
 *
 ******************************************************************************/
#ifndef ACALGORITHMFUNC_H
#define ACALGORITHMFUNC_H


//extern "C" {

/* > Includes *****************************************************************/
//#include <ac_type.h>
#include "acAlgorithmCommonMassiveMIMO.h"
/* > Defines ******************************************************************/

void antennaCalibration(S32* ddrinputSamples_p,
                        S16 noOfSc,
                        S16* delay_p,
                        S16* initPhase_p,
                        S32* amplitude_p,
                        S16* channelSumCentidB_p,
                        U32* disturbance_p,
                        S16* noShift_p,
                        U16  inputSamplesAgcFactor);

/* > Function Declarations*****************************************************/

//}
#endif
