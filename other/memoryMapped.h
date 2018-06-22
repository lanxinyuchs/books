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
 * @(#) ClearCase ID: memoryMapped.h /main/..  2008-03-01 EZHIBWA
 *
 * Review Info:        2008-03-25         By BP SW team
 *
 * Revision history
 *     Date              Author             Description
 *  2008-03-01           ezhibwa           First Revision
 *  2010-05-26           ehholli           Add master/slave fpga support
 *  2010-05-29           ehholli           Add "Info" command
 *
 *******************************************************************************
 */

/* >  Double inclusion protection
 ******************************************************************************
 */

#ifndef __MEMORY_MAPPED_H__
#define __MEMORY_MAPPED_H__

/* >  2  INCLUDE FILES
 ******************************************************************************
 */

#include <vector>
#include <string>
#include "ac_defs.h"


/* >  3  DECLARATIONS
 ******************************************************************************
 */

class MemoryMappedDevice
{
public:
    MemoryMappedDevice(void);
    bool init(const char*  uioDeviceName);  /**< Driver initialization. */

    /**
    * Reads data from HW Device.
    * @param address Location to read data from.
    * @param data Read data.
    */
    virtual void read (U32 address, U32& data);
    virtual void read (U32 address, U16& data);
    virtual void read (U32 address, U8&  data);
    /**
    * Writes data to HW Device.
    * @param address Location to write data to.
    * @param data Data to be written.
    */
    virtual void write(U32 address, U32 data);
    virtual void write(U32 address, U16 data);
    virtual void write(U32 address, U8  data);
    /**
    * Reads (with mask) data from HW Device.
    * @param address Location to read data from.
    * @param mask Mask to be applied.
    * @param data Read data.
    */
    virtual void read(U32 address, U32 mask, U32& data);
    virtual void read(U32 address, U16 mask, U16& data);
    virtual void read(U32 address, U8  mask, U8&  data);
    /**
    * Writes (with mask) data to HW Device.
    * @param address Location to write data to.
    * @param mask Mask to be applied.
    * @param data Data to be written.
    */
    virtual void write(U32 address, U32 mask, U32 data);
    virtual void write(U32 address, U16 mask, U16 data);
    virtual void write(U32 address, U8  mask, U8  data);

protected:
    volatile U8*    baseAddressU8;
    volatile U16*   baseAddressU16;
    volatile U32*   baseAddressU32;
    
    void *traffic_uio_mem_handle;
    void *traffic_mmap_mem_base;
};

/* >  4  FUNCTIONS
 ******************************************************************************
 */

/* >  End of double inclusion protection
 ******************************************************************************
 */

#endif /* __MEMORY_MAPPED_H__ */

/*********************************** END **************************************/
