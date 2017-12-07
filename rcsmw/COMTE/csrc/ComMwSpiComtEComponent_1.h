#ifndef ComMwSpiComtEComponent_1_h
#define ComMwSpiComtEComponent_1_h

#include <MafMgmtSpiComponent_1.h>
#include <MafMgmtSpiInterfacePortal_1.h>

// Forward declare for usage in log
typedef struct comte_mw_component comte_mw_component_t;

#include "ComtEUtils_1.h"
#include "ComMwSpiComtELog_1.h"
#include "ComMwSpiComtEAccessManagement_1.h"
#include "ComMwSpiComtECryptoUtil_1.h"
#include "ComMwSpiComtEAvailabilityController_1.h"
#include "ComMwSpiComtEReplicatedList_1.h"

#define MW_COMPONENT_NAME "MwComtEComponent"
#define MW_COMPONENT_VSN "1"

struct comte_mw_component {
    MafMgmtSpiComponent_1T base;
    MafMgmtSpiInterfacePortal_1T* portal;
    comte_log_t* log;
    comte_am_t* am;
    comte_ac_t* ac;
    comte_crypto_util_t* cu;
    comte_trace_t* trace;
    comte_replicated_list_interface_t* rl;
    comte_config_t* config;
};

MafReturnT comte_mw_component_create(comte_config_t* config,
                                     MafMgmtSpiInterfacePortal_1T* portal, comte_mw_component_t* component);
MafReturnT comte_mw_component_destroy(comte_mw_component_t* component);

#endif
