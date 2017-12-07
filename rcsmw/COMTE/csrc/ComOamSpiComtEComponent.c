/**
 * @author Lukas Larsson
 * @created 2011-03-01
 */

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <MafOamSpiServiceIdentities_1.h>
#include <MafMwSpiServiceIdentities_1.h>
#include <MafMgmtSpiServiceIdentities_1.h>

#include "ComOamSpiComtEComponent_1.h"
#include "ComMwSpiComtEComponent_1.h"
#include "ComOamSpiComtEManagedObject_1.h"
#include "ComOamSpiComtETrans_1.h"
#include "ComtEBertServer_1.h"

const MafMgmtSpiInterface_1T OAM_COMPONENT_ID = { OAM_COMPONENT_NAME,
                                                  "MafMgmtSpiComponent",
                                                  OAM_COMPONENT_VSN };

comte_oam_component_t *oam_comp;


static MafReturnT start(MafStateChangeReasonT reason) {
	/*ENTER();*/
	MafReturnT res = MafOk;

        /* Set Trace */
	MafMgmtSpiInterface_1T* trace_interface = NULL;
	if ((res = oam_comp->portal->getInterface(MafMwSpiTrace_1Id,
		(MafMgmtSpiInterface_1T**) &trace_interface)) != MafOk)
	    return res;
	comte_set_tracer((MafMwSpiTrace_1T*) trace_interface);
	TRACE_ARGS(0, "Set tracer");


        /* Set Log */
	MafMgmtSpiInterface_1T* log_interface = NULL;
	if ((res = oam_comp->portal->getInterface(MafMwSpiLog_1Id,
		(MafMgmtSpiInterface_1T**) &log_interface)) != MafOk)

	    return res;
	comte_set_logger((MafMwSpiLog_1T*) log_interface);
	INFO("Set logger");


        /* Thread Context */
	MafMgmtSpiInterface_1T* thread_context_interface = NULL;
	if ((res = oam_comp->portal->getInterface(
		(MafMgmtSpiInterface_1T){"MafMgmtSpiThreadContextService",
					 "MafMgmtSpiThreadContext", "2"},
		(MafMgmtSpiInterface_1T**) &thread_context_interface)) != MafOk)
	    return res;
	comte_set_thread_context((MafMgmtSpiThreadContext_2T*) thread_context_interface);


	/* Register producers only if enabled in configuration */
        if(oam_comp->config->comte_enabled_oam_comps->alarm_producer)
            comte_register_alarm_producer();

        if(oam_comp->config->comte_enabled_oam_comps->cm_event_producer)
            comte_register_cm_event_producer();

        if(oam_comp->config->comte_enabled_oam_comps->pm_event_producer)
	      comte_register_pm_event_producer();


	/* Start callback in Oam if MW is not to be started */
	if (!oam_comp->config->start_mw) {
		res = comte_start_bert_server(oam_comp->config);
	}

        log_comte_config(oam_comp->config);
	LEAVE();

	return res;
}

static MafReturnT stop(MafStateChangeReasonT reason) {
    MafReturnT res = MafOk;
    if (!oam_comp->config->start_mw)
        comte_stop_bert_server();

    return res;
}

static MafMgmtSpiInterface_1T** interface_array_create(
		comte_oam_component_t* component) {
    int sz_comp = 6;

    MafMgmtSpiInterface_1T ** array = comte_malloc(
        sizeof(MafMgmtSpiInterface_1T*) * sz_comp);

    array[0] = &component->mo->base.base;
    array[1] = &component->mo->base_ext.base;
    array[2] = &component->trans->base.base;
    array[3] = &component->object_impl->base.base;
    array[4] = (MafMgmtSpiInterface_1T*)&component->pm->base.base;
    array[5] = 0;

#ifdef COM_PM_MEAS_H
    /* New interface array size */
    array = (MafMgmtSpiInterface_1T **)
        comte_realloc(array, sizeof(MafMgmtSpiInterface_1T*) * (sz_comp+1));
    array[sz_comp-1] = &component->pm_meas->base.base;
    array[sz_comp] = 0;
    sz_comp++;
#endif

    return array;
}



static void interface_array_destroy(MafMgmtSpiInterface_1T** array) {
	comte_free(array);
}

static MafMgmtSpiInterface_1T** dependency_array_create() {
	MafMgmtSpiInterface_1T ** array = comte_malloc(
		sizeof(MafMgmtSpiInterface_1T*) * 5);
	array[0] = comte_malloc(sizeof(MafMgmtSpiInterface_1T));
        array[0]->componentName = MafMwSpiLog_1Id.componentName;
        array[0]->interfaceName = MafMwSpiLog_1Id.interfaceName;
        array[0]->interfaceVersion = MafMwSpiLog_1Id.interfaceVersion;

	array[1] = comte_malloc(sizeof(MafMgmtSpiInterface_1T));
	array[1]->componentName = MafOamSpiEventService_1Id.componentName;
	array[1]->interfaceName = MafOamSpiEventService_1Id.interfaceName;
	array[1]->interfaceVersion = MafOamSpiEventService_1Id.interfaceVersion;

	array[2] = comte_malloc(sizeof(MafMgmtSpiInterface_1T));
	array[2]->componentName = MafMwSpiReplicatedList_1Id.componentName;
	array[2]->interfaceName = MafMwSpiReplicatedList_1Id.interfaceName;
	array[2]->interfaceVersion = MafMwSpiReplicatedList_1Id.interfaceVersion;

	array[3] = comte_malloc(sizeof(MafMgmtSpiInterface_1T));
	array[3]->componentName = "MafMgmtSpiThreadContextService"; // MafMgmtSpiThreadContext_2Id.componentName;
	array[3]->interfaceName = "MafMgmtSpiThreadContext"; // MafMgmtSpiThreadContext_2Id.interfaceName;
	array[3]->interfaceVersion = "2"; // MafMgmtSpiTreadContext_2Id.interfaceVersion;

	array[4] = 0;
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

MafReturnT comte_oam_component_create(comte_config_t* config,
                                      MafMgmtSpiInterfacePortal_1T* portal,
                                      comte_oam_component_t* component) {
	MafReturnT res = MafOk;

	component->portal = portal;
	component->config = config;

	res = comte_transactional_resource_create(component);
	if (res != MafOk)
		return res;

        res = comte_object_implementer_create(component);
	if (res != MafOk)
            return res;

	res = comte_managed_object_create(component);
	if (res != MafOk)
		return res;

	res = comte_alarm_producer_create(component);
	if (res != MafOk)
            return res;

	res = comte_cm_event_producer_create(component);
	if (res != MafOk)
	    return res;

	res = comte_pm_event_producer_create(component);
	if (res != MafOk)
	    return res;

	res = comte_pm_create(component);
	if (res != MafOk)
            return res;

#ifdef COM_PM_MEAS_H
        res = comte_pm_meas_create(component);
	if (res != MafOk)
            return res;
#endif

	component->base.base = OAM_COMPONENT_ID;
	component->base.interfaceArray = interface_array_create(component);
	component->base.dependencyArray = dependency_array_create();

	component->base.start = start;
	component->base.stop = stop;

	oam_comp = component;

	return res;
}

MafReturnT comte_oam_component_destroy(comte_oam_component_t* component) {
	MafReturnT res = MafOk;

	res = comte_transactional_resource_destroy(component);
	if (res != MafOk) // Should I break here?
		return res;

        res = comte_object_implementer_destroy(component);
	if (res != MafOk) // Should I break here?
            return res;

	res = comte_managed_object_destroy(component);
	if (res != MafOk) // Should I break here?
		return res;

	res = comte_alarm_producer_destroy(component);
	if (res != MafOk) // Should I break here?
		return res;

        res = comte_cm_event_producer_destroy(component);
	if (res != MafOk) // Should I break here?
            return res;

        res = comte_pm_destroy(component);
	if (res != MafOk)
            return res;

#ifdef COM_PM_MEAS_H
        res = comte_pm_meas_destroy(component);
	if (res != MafOk)
            return res;
#endif


	interface_array_destroy(component->base.interfaceArray);
	dependency_array_destroy(component->base.dependencyArray);

	return res;
}
