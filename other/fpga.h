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
 **
 * Revision history
 *     Date              Author             Description
 *  2016-11-18      edentao           First Revision
 *
 *******************************************************************************
 */

/* >  Double inclusion protection
 ******************************************************************************
 */


#ifndef __FPGA_H__
#define __FPGA_H__


/* >  2  INCLUDE FILES
 ******************************************************************************
 */
#include "memoryMapped.h"


/* >  3  DECLARATIONS
 ******************************************************************************
 */
class Fpga : public MemoryMappedDevice
{
public:
    Fpga(void);
    ~Fpga();
    static Fpga* getInstance();
    bool init(const char*  uioDeviceName);

    static Fpga* creator(void);

public:
};

/* >  4  FUNCTIONS
 ******************************************************************************
 */

/* >  End of double inclusion protection
 ******************************************************************************
 */

#endif /* __FPGA2_H__ */

/*********************************** END **************************************/
