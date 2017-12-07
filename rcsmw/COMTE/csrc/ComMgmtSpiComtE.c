/**
 * @author Lukas Larsson
 * @created 2011-03-01
 */

// System includes
#include <stdio.h>
#include <stdlib.h>

// COM includes
#include <MafMgmtSpiCommon.h>
#include <MafMgmtSpiInterfacePortalAccessor.h>
#include <MafMgmtSpiInterfacePortal_1.h>

// MaftE includes
#include "ComOamSpiComtEComponent_1.h"
#include "ComMwSpiComtEComponent_1.h"
#include "ComtEUtils_1.h"

// Global vars
MafMgmtSpiInterfacePortal_1T* portal = NULL;
comte_oam_component_t* oam_component = NULL;
comte_mw_component_t* mw_component = NULL;
comte_config_t config;

MafReturnT comLCMinit(struct MafMgmtSpiInterfacePortalAccessor *accessor,
                      const char* configString) {

	MafReturnT res = MafOk;

	comte_parse_config(configString, &config);

	portal = (MafMgmtSpiInterfacePortal_1T*) accessor->getPortal("1");

	if (mw_component == NULL && config.start_mw) {
            mw_component = comte_malloc(sizeof(comte_mw_component_t));
            res = comte_mw_component_create(&config, portal, mw_component);

            if (res != MafOk) return res;

            portal->registerComponent(&mw_component->base);

            INFO("ComtE comLCMinit has registered the MW component");
            if (mw_component != NULL) {
                (mw_component->trace->base.traceWrite)
		(0, "ComtE comLCMinit has registered the MW component");
            }
	}

	if (oam_component == NULL) {
		oam_component = comte_malloc(sizeof(comte_oam_component_t));
		res = comte_oam_component_create(&config, portal, oam_component);

                if (res != MafOk) return res;

                portal->registerComponent(&oam_component->base);

                INFO("ComtE comLCMinit has registered the OAM component");
                if (mw_component != NULL) {
                    (mw_component->trace->base.traceWrite)
                        (0, "ComtE comLCMinit has registered the OAM component");
                }
	}

	return res;
}

void comLCMterminate() {

	INFO("ComtE comLCMterminate about to unregister the OAM component");
	if (mw_component != NULL) {
	    (mw_component->trace->base.traceWrite)
		(0,
		 "ComtE comLCMterminate "
		 "about to unregister the OAM component");
	}

	if (oam_component != NULL) {
	    portal->unregisterComponent(&oam_component->base);
	    comte_oam_component_destroy(oam_component);
	    comte_free(oam_component);
	}

	INFO("ComtE comLCMterminate about to unregister the MW component");
	if (mw_component != NULL) {
	    (mw_component->trace->base.traceWrite)
		(0,
		 "ComtE comLCMterminate "
		 "about to unregister the MW component");
	}

	if (mw_component != NULL) {
	    portal->unregisterComponent(&mw_component->base);
	    comte_mw_component_destroy(mw_component);
	    comte_free(mw_component);
	}

        comte_destroy_config(&config);


#ifdef GCOV
extern void __gcov_flush();
	// Flush gcov
	__gcov_flush();
#endif
}
