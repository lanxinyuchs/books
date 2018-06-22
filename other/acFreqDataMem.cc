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
 7  Processes

 ******************************************************************************
 */

/* >  1  DESCRIPTION
 *******************************************************************************
 *
 * <text...>
 *
 * Revision history
 *     Date              Author             Description
 *  2016-11-18           edentao           First Revision
 *
 *******************************************************************************
 */

/* >  2  INCLUDE FILES
 *******************************************************************************
 */

/*******C libary header*******/
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

/*******   ac mem header  *******/
#include "ac_defs.h"
#include "acFreqDataMem.h"

/* >  3  DECLARATIONS AND DEFINITIONS
 ******************************************************************************
 */

/* >  3.1  GLOBAL
 ******************************************************************************
 */

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

/* >  6.2  LOCAL
 ******************************************************************************
 */

/* >  AcFreqDataMem::AcFreqDataMem
 *****************************************************************************
 *
 *  Description :  Construction function of AcFreqDataMem Class.
 *
 *  Arguments   :  void
 *
 *  Return      :  void
 *
 *****************************************************************************
 */
AcFreqDataMem::AcFreqDataMem(void) : MemoryMappedDevice()
{   
}

AcFreqDataMem::~AcFreqDataMem()
{
    //::free(instanceDrvName);
}

AcFreqDataMem* AcFreqDataMem::getInstance()
{
    static AcFreqDataMem instance;
    return &instance;
}

/* >  AcFreqDataMem::init
 *****************************************************************************
 *
 *  Description :  This is the init function of class AcFreqDataMem.
 *
 *  Arguments   :
 *     Type                     INPUT/OUTPUT          PARAMETER
 *    char*                        INPUT           instanceDbName
 *
 *  Return      :
 *  Type                                  Comments
 *  bool             Return true if success, return false if failed.
 *
 *****************************************************************************
 */
bool AcFreqDataMem::init(const char* uioDeviceName)
{
    U32 pinNo = 0;

    if (!MemoryMappedDevice::init(uioDeviceName))
    {
        //TRACE_ERROR_SHELL("MemoryMappedDevice::init fail");
        return false;
    }

    return true;
}

/* >  AcFreqDataMem::creator
 *****************************************************************************
 *
 *  Description :  Creator function for driver factory.
 *
 *  Arguments   :  void
 *
 *  Return      :
 *       Type                               Comments
 *  DefaultDriver*                 Return the instance created.
 *
 *****************************************************************************
 */
AcFreqDataMem* AcFreqDataMem::creator(void)
{
    return new AcFreqDataMem();
}


/* >  AcFreqDataMem::init
 *****************************************************************************
 *
 *  Description :  This is the init function of class AcFreqDataMem.
 *
 *  Arguments   :
 *     Type                     INPUT/OUTPUT          PARAMETER
 *    char*                        INPUT           instanceDbName
 *
 *  Return      :
 *  Type                                  Comments
 *  bool             Return true if success, return false if failed.
 *
 *****************************************************************************
 */
bool AcFreqDataMem::getFreqDataFromAcMem(S16 * buffer, U16 size, U32 offset)
{
    U16 freqData = 0;
    U32 addr = 0;
    U16 k = 0;
    U16 tmpData = 0;
    for (k = 0; k < size; k++)
    {
        addr = offset + k;
        read(addr, freqData);
        buffer[k] = freqData;
    }
    //data 32 bit in AC mem, high 16 bit is real, low 16 bit is imag, so need to switch
    for (k = 0; k < size/2; k++)
    {
        tmpData = buffer[2*k];
        buffer[2*k] = buffer[2*k+1];
        buffer[2*k+1] = tmpData;
    }

    return true;
}
/* >  7    PROCESSES
 *****************************************************************************
 */

/************************************** END **********************************/

