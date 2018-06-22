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

/*******   fpga header  *******/
#include "fpga.h"

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

/* >  Fpga::Fpga
 *****************************************************************************
 *
 *  Description :  Construction function of Fpga Class.
 *
 *  Arguments   :  void
 *
 *  Return      :  void
 *
 *****************************************************************************
 */
Fpga::Fpga(void) : MemoryMappedDevice()
{   
}

Fpga::~Fpga()
{
    //::free(instanceDrvName);
}

Fpga* Fpga::getInstance()
{
    static Fpga instance;
    return &instance;
}

/* >  Fpga::init
 *****************************************************************************
 *
 *  Description :  This is the init function of class Fpga.
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
bool Fpga::init(const char* uioDeviceName)
{
    U32 pinNo = 0;

    if (!MemoryMappedDevice::init(uioDeviceName))
    {
        //TRACE_ERROR_SHELL("MemoryMappedDevice::init fail");
        return false;
    }

    return true;
}

/* >  Fpga::creator
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
Fpga* Fpga::creator(void)
{
    return new Fpga();
}

/* >  7    PROCESSES
 *****************************************************************************
 */

/************************************** END **********************************/

