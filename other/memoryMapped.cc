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
 * @(#) ClearCase ID: memoryMapped.cc /main/..  2008-03-01 EZHIBWA
 *
 * Review Info:        2008-03-25         By BP SW team
 *
 * Revision history
 *     Date              Author             Description
 *  2008-03-01           ezhibwa           First Revision
 *  2010-05-29           ehholli             Add "Info" command
 *  2011-12-28           efancha           Amend "range" command
 *  2016-11-18           edentao           2.6G maMIMO AC
 *
 *******************************************************************************
 */

/* >  2  INCLUDE FILES
 *******************************************************************************
 */

/*******C libary header*******/
#include <stdio.h>
#include <stdlib.h>

#include <uio_helper.h>
#include <itc.h>
#include <unistd.h>
#include "ac_defs.h"
#include "memoryMapped.h"

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

//bool MemoryMappedDevice::isInfoOn = false;
//U32 MemoryMappedDevice::startReg = 0;
//U32 MemoryMappedDevice::endReg = 0;
/* >  MemoryMappedDevice::MemoryMappedDevice
 *****************************************************************************
 *
 *  Description :  Construction function of MemoryMappedDevice Class.
 *
 *  Arguments   :  void
 *
 *  Return      :  void
 *
 *****************************************************************************
 */
MemoryMappedDevice::MemoryMappedDevice(void)
{
    //isInfoOn = false;
    //startReg = 0;
    //endReg = 0;
    traffic_uio_mem_handle = NULL;
    traffic_mmap_mem_base = NULL;
}

/* >  MemoryMappedDevice::init
 *****************************************************************************
 *
 *  Description :  This is the init function of class MemoryMappedDevice.
 *
 *  Arguments   :
 *     Type                     INPUT/OUTPUT          PARAMETER
 * const char *                    INPUT            instanceDbName
 *
 *  Return      :
 *  Type                                  Comments
 *  bool             Return true if success, return false if failed.
 *
 *****************************************************************************
 */
bool MemoryMappedDevice::init(const char* uioDeviceName)
{
    //traffic_uio_mem_handle = (void *) uio_open(TRAFFIC_XDOS_AC_DEV);
    traffic_uio_mem_handle = (void *) uio_open(uioDeviceName);
    if (traffic_uio_mem_handle == (UIO_HANDLE_) - 1) 
    {
        //TPT_INFO(STR("traffic fpga uio_open funciton return error"));
        return false;
    }
    //TPT_INFO(STR("traffic fpga uio_open success"));

    traffic_mmap_mem_base = uio_mmap(traffic_uio_mem_handle);

    if (traffic_mmap_mem_base == MAP_FAILED)
    {
        traffic_mmap_mem_base = NULL;
        //TPT_INFO(STR("traffic fpga uio_mmap funciton return error"));
        uio_close(traffic_uio_mem_handle);
        return false;
    }
    //TPT_INFO(STR("traffic fpga mem uio_mmap success! base address is %p", traffic_mmap_mem_base));
    baseAddressU8   = (U8*) traffic_mmap_mem_base;
    baseAddressU16  = (U16*)traffic_mmap_mem_base;
    baseAddressU32  = (U32*)traffic_mmap_mem_base;

    return true;
}

/* >  MemoryMappedDevice::read
 *****************************************************************************
 *
 *  Description :  Read data from the MemoryMappedDevice.
 *
 *  Arguments   :
 *     Type                     INPUT/OUTPUT          PARAMETER
 *     U32                         INPUT               address
 *     U32&                        OUTPUT               data
 *
 *  Return      :  void
 *
 *****************************************************************************
 */
void MemoryMappedDevice::read(U32 address, U32& data)
{
    data = baseAddressU32[address];
}

/* >  MemoryMappedDevice::read
 *****************************************************************************
 *
 *  Description :  Read data from the MemoryMappedDevice.
 *
 *  Arguments   :
 *     Type                     INPUT/OUTPUT          PARAMETER
 *     U32                         INPUT               address
 *     U16&                        OUTPUT               data
 *
 *  Return      :  void
 *
 *****************************************************************************
 */
void MemoryMappedDevice::read(U32 address, U16& data)
{
    data = baseAddressU16[address];
}

/* >  MemoryMappedDevice::read
 *****************************************************************************
 *
 *  Description :  Read data from the MemoryMappedDevice.
 *
 *  Arguments   :
 *     Type                     INPUT/OUTPUT          PARAMETER
 *     U32                         INPUT               address
 *     U8&                         OUTPUT               data
 *
 *  Return      :  void
 *
 *****************************************************************************
 */
void MemoryMappedDevice::read(U32 address, U8& data)
{
    data = baseAddressU8[address];
}

/* >  MemoryMappedDevice::write
 *****************************************************************************
 *
 *  Description :  Write data to the MemoryMappedDevice.
 *
 *  Arguments   :
 *     Type                     INPUT/OUTPUT          PARAMETER
 *     U32                         INPUT               address
 *     U32&                        INPUT               data
 *
 *  Return      :  void
 *
 *****************************************************************************
 */
void MemoryMappedDevice::write(U32 address, U32 data)
{
    baseAddressU32[address] = data;
}

/* >  MemoryMappedDevice::write
 *****************************************************************************
 *
 *  Description :  Write data to the MemoryMappedDevice.
 *
 *  Arguments   :
 *     Type                     INPUT/OUTPUT          PARAMETER
 *     U32                         INPUT               address
 *     U16&                        INPUT               data
 *
 *  Return      :  void
 *
 *****************************************************************************
 */
void MemoryMappedDevice::write(U32 address, U16 data)
{
    baseAddressU16[address] = data;
}

/* >  MemoryMappedDevice::write
 *****************************************************************************
 *
 *  Description :  Write data to the MemoryMappedDevice.
 *
 *  Arguments   :
 *     Type                     INPUT/OUTPUT          PARAMETER
 *     U32                         INPUT               address
 *     U8                          INPUT               data
 *
 *  Return      :  void
 *
 *****************************************************************************
 */
void MemoryMappedDevice::write(U32 address, U8 data)
{
    baseAddressU8[address]  = data;
}

/* >  MemoryMappedDevice::read
 *****************************************************************************
 *
 *  Description :  Read data from the MemoryMappedDevice.
 *
 *  Arguments   :
 *     Type                     INPUT/OUTPUT          PARAMETER
 *     U32                         INPUT               address
 *     U32                         INPUT                mask
 *     U32&                        OUTPUT               data
 *
 *  Return      :  void
 *
 *****************************************************************************
 */
void MemoryMappedDevice::read(U32 address, U32 mask, U32& data)
{
    read(address, data);
    data &= mask;
}

/* >  MemoryMappedDevice::read
 *****************************************************************************
 *
 *  Description :  Read data from the MemoryMappedDevice.
 *
 *  Arguments   :
 *     Type                     INPUT/OUTPUT          PARAMETER
 *     U32                         INPUT               address
 *     U16                         INPUT                mask
 *     U16&                        OUTPUT               data
 *
 *  Return      :  void
 *
 *****************************************************************************
 */
void MemoryMappedDevice::read(U32 address, U16 mask, U16& data)
{
    read(address, data);
    data &= mask;
}

/* >  MemoryMappedDevice::read
 *****************************************************************************
 *
 *  Description :  Read data from the MemoryMappedDevice.
 *
 *  Arguments   :
 *     Type                     INPUT/OUTPUT          PARAMETER
 *     U32                         INPUT               address
 *     U8                          INPUT                mask
 *     U8&                         OUTPUT               data
 *
 *  Return      :  void
 *
 *****************************************************************************
 */
void MemoryMappedDevice::read(U32 address, U8  mask, U8&  data)
{
    read(address, data);
    data &= mask;
}

/* >  MemoryMappedDevice::write
 *****************************************************************************
 *
 *  Description :  Write data to the MemoryMappedDevice.
 *
 *  Arguments   :
 *     Type                     INPUT/OUTPUT          PARAMETER
 *     U32                         INPUT               address
 *     U32                         INPUT                mask
 *     U32                         INPUT                data
 *
 *  Return      :  void
 *
 *****************************************************************************
 */
void MemoryMappedDevice::write(U32 address, U32 mask, U32 data)
{
    U32 oldData = 0;
    read(address, oldData);
    write(address, (U32)((mask & data)|(oldData & ~(mask))));
}

/* >  MemoryMappedDevice::write
 *****************************************************************************
 *
 *  Description :  Write data to the MemoryMappedDevice.
 *
 *  Arguments   :
 *     Type                     INPUT/OUTPUT          PARAMETER
 *     U32                         INPUT               address
 *     U16                         INPUT                mask
 *     U16                         INPUT                data
 *
 *  Return      :  void
 *
 *****************************************************************************
 */
void MemoryMappedDevice::write(U32 address, U16 mask, U16 data)
{
    U16 oldData = 0;
    read(address, oldData);
    write(address, (U16)((mask & data)|(oldData & ~(mask))));
}

/* >  MemoryMappedDevice::write
 *****************************************************************************
 *
 *  Description :  Write data to the MemoryMappedDevice.
 *
 *  Arguments   :
 *     Type                     INPUT/OUTPUT          PARAMETER
 *     U32                         INPUT               address
 *     U8                          INPUT                mask
 *     U8                          INPUT                data
 *
 *  Return      :  void
 *
 *****************************************************************************
 */
void MemoryMappedDevice::write(U32 address, U8  mask, U8 data)
{
    U8 oldData = 0;
    read(address, oldData);
    write(address, (U8)((mask & data)|(oldData & ~(mask))));
}

/* >  7    PROCESSES
 *****************************************************************************
 */

/************************************** END **********************************/
