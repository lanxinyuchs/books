#include <itc.h>

#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <inttypes.h>
#include <unistd.h>

#include "com_ericsson_glms.h"

#include "glmsadpi/glmsDataTypes.h"
#include "glmsadpi/glms_adpi.sig"
#include "glms_main.h"
#include "glms_timi.h"
#include "glmsUtils.h"
#include "persistentStorage.h"
#include "data_unlocks.h"
#include "data_lm.h"
#include "clients.h"

int32_t common_handleGlmsAdpiXXIndexListRsp(union itc_msg *sig,
                                            uint32_t *tableIndex[],
                                            uint32_t indexesInTable[]);

int32_t common_prepareForParsingStoredParameters(union itc_msg *sig,
                                                 GlmsBool canReceiveSp,
                                                 GlmsBool canReceivePp);

int32_t common_parseStoredParameters(union itc_msg *sig,
                                     int32_t readStatus[],
                                     int32_t(*getRspExec)(union itc_msg *sig),
                                     int32_t(*listRspExec)(union itc_msg *sig),
                                     GlmsBool checkPp,
                                     GlmsBool checkSp);

void common_findHighestIndexInTable(uint32_t tableIndex[],
                                    uint32_t noOfReadIndexes,
                                    uint32_t *highestIndex);

void common_sanityCheckIndexesInTable(uint32_t tableIndex[],
                                      uint32_t noOfReadIndexes);

void common_resetIndexesInTable(uint32_t *indexesInTable,
                                uint32_t *noOfReadIndexes,
                                uint32_t *tableIndex[]);
