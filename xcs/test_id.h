#ifndef TEST_ID_H
#define TEST_ID_H

#include "osetypes.h"

enum TestId
{
    TestId_All = 0,
    TestId_SubscribeVii,
    TestId_Fault,
    TestId_I2C,
    TestId_SPI,
    TestId_HwLog,
    TestId_StartLink,
    TestId_Lmc,
    TestId_Mmi,
    TestId_Xte,
    TestId_Alloc,
    TestId_NodeId,
    TestId_Count
};

const U32 TEST_COUNT = TestId_Count - 1;
#endif
