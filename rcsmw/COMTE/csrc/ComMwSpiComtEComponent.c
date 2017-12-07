
/**
 * @author Lukas Larsson
 * @created 2011-03-01
 */

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <MafOamSpiServiceIdentities_1.h>
#include <MafMwSpiServiceIdentities_1.h>

#include "ComMwSpiComtEComponent_1.h"
#include "ComtEBertServer_1.h"

const MafMgmtSpiInterface_1T MW_COMPONENT_ID = { MW_COMPONENT_NAME,
		"MafMgmtSpiComponent", MW_COMPONENT_VSN };

comte_mw_component_t *mw_comp = NULL;

static MafReturnT start(MafStateChangeReasonT reason) {
	MafReturnT res = MafOk;

	if (mw_comp->config->start_mw) {
	    res = comte_start_bert_server(mw_comp->config);
	}

	return res;
}

static MafReturnT stop(MafStateChangeReasonT reason) {
    ENTER();
    MafReturnT res = MafOk;
    if (mw_comp->config->start_mw)
        comte_stop_bert_server();
    LEAVE();
    return res;
}

static MafMgmtSpiInterface_1T** interface_array_create(
		comte_mw_component_t* component) {
	MafMgmtSpiInterface_1T ** array = comte_malloc(
            sizeof(MafMgmtSpiInterface_1T*) * 7);
	array[0] = &component->log->base.base;
	array[1] = &component->ac->base.base;
	array[2] = &component->trace->base.base;
	array[3] = &component->am->base.base;
	array[4] = &component->rl->base.base;
        array[5] = &component->cu->base.base;
        array[6] = 0;
	return array;
}

static void interface_array_destroy(MafMgmtSpiInterface_1T** array) {
	comte_free(array);
}

static MafMgmtSpiInterface_1T** dependency_array_create() {
	MafMgmtSpiInterface_1T ** array = comte_malloc(
			sizeof(MafMgmtSpiInterface_1T*) * 1);
	array[0] = 0;
	return array;
}

static void dependency_array_destroy(MafMgmtSpiInterface_1T** array) {
	int i = 0;
	while (array[i] != 0) {
		comte_free(array[i]);
		i++;
	}
	comte_free(array);
}

MafReturnT comte_mw_component_create(comte_config_t* config,
		MafMgmtSpiInterfacePortal_1T* portal, comte_mw_component_t* component) {
	MafReturnT res = MafOk;

	mw_comp = component;

	component->portal = portal;
	component->config = config;

	res = comte_log_create(component);
	if (res != MafOk)
            return res;

        res = comte_crypto_util_create(component);
	if (res != MafOk)
            return res;

	res = comte_access_management_create(component);
	if (res != MafOk)
            return res;
        
	res = comte_availability_controller_create(component);
	if (res != MafOk)
            return res;

	res = comte_replicated_list_create(component);
	if (res != MafOk)
            return res;

	component->base.base = MW_COMPONENT_ID;
	component->base.interfaceArray = interface_array_create(component);
	component->base.dependencyArray = dependency_array_create();

	component->base.start = start;
	component->base.stop = stop;
	return res;
}

MafReturnT comte_mw_component_destroy(comte_mw_component_t* component) {
	MafReturnT res = MafOk;

        res = comte_log_destroy(component);
	if (res != MafOk) // Should I break here?
            return res;

        res = comte_crypto_util_destroy(component);
	if (res != MafOk) // Should I break here?
            return res;

        res = comte_access_management_destroy(component);
	if (res != MafOk) // Should I break here?
            return res;

        res = comte_availability_controller_destroy(component);
	if (res != MafOk) // Should I break here?
            return res;
        
        res = comte_replicated_list_destroy(component);
        if (res != MafOk) // Should I break here?
            return res;
        
	interface_array_destroy(component->base.interfaceArray);
	dependency_array_destroy(component->base.dependencyArray);

	return res;
}
