/**
 * @author Lukas Larsson
 * @created 2011-04-13
 */

#include <stdlib.h>
#include <stdio.h>
#include <pthread.h>
#include <unistd.h>

#include <MafMwSpiServiceIdentities_1.h>
#include "ComMwSpiComtEConverter_1.h"
#include "ComMwSpiComtEComponent_1.h"
#include "ComMwSpiComtEAvailabilityController_1.h"
#include "ComtEUtils_1.h"

comte_mw_component_t* ac_component = NULL;
const MafMwSpiAcT* ac_callbacks = NULL;

static MafReturnT acInitialize(const MafMwSpiAcT* acCallbacks) {
	ac_callbacks = acCallbacks;
	GENERIC_BERT_RPC(ac_component->config,encode_acInitialize(buff), decode_acInitialize(buff));
}

static MafReturnT healthCheckReport(MafMwSpiAvailabilityInvocationT invocation,
		MafReturnT healthReport,
		MafMwSpiRecommendedRecoveryT recommendedRecovery) {
	GENERIC_BERT_RPC(ac_component->config,
			encode_healthCheckReport(healthReport, recommendedRecovery, buff),
						decode_healthCheckReport(buff));
}

static MafReturnT haModeAssumed(MafMwSpiAvailabilityInvocationT invocation,
		MafReturnT error) {
	INFO("ComtE sending haModeAssumed");
	GENERIC_BERT_RPC(ac_component->config,
			encode_haModeAssumed(error, buff), decode_haModeAssumed(buff));
}

static MafReturnT prepareTerminationResponse(
		MafMwSpiAvailabilityInvocationT invocation, MafReturnT error) {
	INFO("ComtE sending prepareTerminationResponse");
	GENERIC_BERT_RPC(ac_component->config,
			encode_prepareTerminationResponse(error, buff),
			decode_prepareTerminationResponse(buff));
}

MafReturnT comte_setHaMode(MafMwSpiHaModeT haMode, MafMwSpiHaReasonT haReason) {
	if (ac_callbacks == NULL)
		return MafNotActive;
	INFO("ComtE calling setHaMode");
	ac_callbacks->setHaMode(NULL, haMode, haReason);
	return MafOk;
}

MafReturnT comte_healthCheck() {
	if (ac_callbacks == NULL)
		return MafNotActive;
	ac_callbacks->healthCheck(NULL);
	return MafOk;
}

MafReturnT comte_prepareTermination() {
	if (ac_callbacks == NULL)
		return MafNotActive;
	INFO("ComtE calling prepareTermination");
	ac_callbacks->prepareTermination(NULL);
	return MafOk;
}

MafReturnT comte_terminateProcess() {
	if (ac_callbacks == NULL)
		return MafNotActive;
	INFO("ComtE calling terminateProcess");
	ac_callbacks->terminateProcess();
	return MafOk;
}

MafReturnT comte_availability_controller_create(comte_mw_component_t* comp) {
	MafReturnT res = MafOk;

	ac_component = comp;
	ac_component->ac = comte_malloc(sizeof(comte_ac_t));
	comte_ac_t* ac = ac_component->ac;

	ac->base.base.componentName = MW_COMPONENT_NAME;
	ac->base.base.interfaceName =
			MafMwSpiAvailabilityController_1Id.interfaceName;
	ac->base.base.interfaceVersion =
			MafMwSpiAvailabilityController_1Id.interfaceVersion;

	ac->base.acInitialize = acInitialize;
	ac->base.healthCheckReport = healthCheckReport;
	ac->base.haModeAssumed = haModeAssumed;
	ac->base.prepareTerminationResponse = prepareTerminationResponse;

	return res;
}

MafReturnT comte_availability_controller_destroy(comte_mw_component_t* comp) {
	MafReturnT res = MafOk;
	comte_free(comp->ac);
	return res;
}
