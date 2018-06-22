/******************************************************************************
 ************************* Copyright ERICSSON CBC 2016 ************************
 ******************************************************************************
 *
 * The Copyright to the computer programs herein is the property of ERICSSON AB.
 * The programs may be used and/or copied only with the written permission from
 * ERICSSON AB or in accordance with the terms and conditions stipulated in
 * the agreement/contract under which the programs have been supplied.
 *
 *******************************************************************************
 */

#ifndef __AC_INT_HANDLER_H__
#define __AC_INT_HANDLER_H__

#include "defaultDriver.h"
//#include "interrupt.h"
#include <map>
//#include "memoryMapped.h"

class AcIntHandler: public DefaultDriver
{
public:
    AcIntHandler(); /**< Constructor. */
    virtual ~AcIntHandler(); /**<Destructor. */
    static AcIntHandler* getInstance();
    static DefaultDriver* creator(void);

    bool init(const char* instanceDrvName); /**< Driver initialization. */
    //void registerHalInterface(void);
    void testInterfaceInit(void);
    void installOamInt();
    static void oam_int_process(void *);

private:
    static bool registeredInDrvFactory;
    static int testCmdHandler(int argc, char **argv);
    //MemoryMappedDevice * m_fpga; 
};

#endif

