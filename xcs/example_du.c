/* > Description **************************************************************/
/*
 * Copyright Ericsson AB
 *
 * The copyright to the computer programs herein is the property of Ericsson AB.
 * The programs may be used and/or copied only with the written permission from
 * Ericsson AB or in accordance with the terms conditions stipulated in the
 * agreement/contract under which the programs have been supplied.
 *
 ******************************************************************************/
/**
 * @file
 * @brief Short example on how to use the new EQMH API
 *        This example shows that the user can call load, boot, stop, dump
 *        and set configuration asynchronously and handle the responses from EQMH
 *        in the callbacks.
 *
 ******************************************************************************/

/* > Includes *****************************************************************/
#include <sys/select.h>
#include <stddef.h>
#include "eqmhi_api.h"
#include "eqmhi_bb.h"
#include <stdio.h>
#include <unistd.h>

/* > Defines ******************************************************************/

/* > Type Declarations  *******************************************************/
typedef enum
{
	INITIAL,
	LOADED,
	BOOTED,
	STOPPED,
	DUMPED
} state_t;

/* > Global Constant Definitions **********************************************/

/* > Global Variable Definitions **********************************************/

/* > Local Constant Definitions ***********************************************/

/* > Local Variable Definitions ***********************************************/
static state_t state = INITIAL;
static uint32_t error = 0;

/* > Local Function Declarations **********************************************/

/* > Global Function Definitions **********************************************/

/* > Local Function Definitions ***********************************************/

/* > Function: set_state
 ******************************************************************************/
/**
 * @brief Sets the current emca state
 *
 * @param[in]     newState     new state to set
 *
 ******************************************************************************/
static void set_state(state_t newState)
{
	state = newState;
}

/* > Function: get_state
 ******************************************************************************/
/**
 * @brief Gets the current emca state
 *
 * @return        the current state
 *
 ******************************************************************************/
static state_t get_state()
{
	return state;
}


/* > Function: my_load_callback
 ******************************************************************************/
/**
 * @brief Load callback function
 *
 * @param[in]     instance     instance reference
 * @param[in]     user_data_p  user data provided when calling load
 * @param[in]     status       status from the load
 *
 ******************************************************************************/
static void my_load_callback(eqmhi_instance_t instance,
                             void* user_data_p,
                             eqmhi_status_t status)
{
	(void)user_data_p;

	if (status == EQMHI_STATUS_SUCCESS) {
		set_state(LOADED);
		printf("LOADED OK!\n");

		// boot
		status = eqmhi_boot(instance, NULL);
		if (status != EQMHI_STATUS_SUCCESS) {
			printf("Failed to boot! error:%d", status);
			error = 1;
		}
	}
	else {
		printf("Failed to load! error:%d", status);
		error = 1;
	}
}

/* > Function: my_boot_callback
 ******************************************************************************/
/**
 * @brief Boot callback function
 *
 * @param[in]     instance     instance reference
 * @param[in]     user_data_p  user data provided when calling boot
 * @param[in]     status       status from the boot
 *
 ******************************************************************************/
static void my_boot_callback(eqmhi_instance_t instance,
                             void* user_data_p,
                             eqmhi_status_t status)
{
	(void)user_data_p;

	if (status == EQMHI_STATUS_SUCCESS) {
		set_state(BOOTED);
		printf("BOOTED OK!\n");

		// stop
		status = eqmhi_stop(instance, NULL);
		if (status != EQMHI_STATUS_SUCCESS) {
			printf("Failed to stop! error:%d", status);
			error = 1;
		}
	}
	else {
		printf("Failed to boot! error:%d", status);
		error = 1;
	}
}

/* > Function: my_stop_callback
 ******************************************************************************/
/**
 * @brief Stop callback function
 *
 * @param[in]     instance     instance reference
 * @param[in]     user_data_p  user data provided when calling stop
 * @param[in]     status       status from the stop
 *
 ******************************************************************************/
static void my_stop_callback(eqmhi_instance_t instance,
                             void* user_data_p,
                             eqmhi_status_t status)
{
	(void)user_data_p;

	if (status == EQMHI_STATUS_SUCCESS) {
		set_state(STOPPED);
		printf("STOPPED OK!\n");

		// dump
		status = eqmhi_dump(instance, NULL);
		if (status != EQMHI_STATUS_SUCCESS) {
			printf("Failed to dump! error:%d", status);
			error = 1;
		}
	}
	else {
		printf("Failed to stop! error:%d", status);
		error = 1;
	}
}

/* > Function: my_dump_callback
 ******************************************************************************/
/**
 * @brief Dump callback function
 *
 * @param[in]     instance     instance reference
 * @param[in]     user_data_p  user data provided when calling dump
 * @param[in]     status       status from the dump
 *
 ******************************************************************************/
static void my_dump_callback(eqmhi_instance_t instance,
                             void* user_data_p,
                             eqmhi_status_t status)
{
	(void)instance;
	(void)user_data_p;

	if (status == EQMHI_STATUS_SUCCESS) {
		set_state(DUMPED);
		printf("DUMPED OK!\n");
	}
	else {
		printf("Failed to dump! error:%d", status);
		error = 1;
	}
}

/* > Function: my_reload_callback
 ******************************************************************************/
/**
 * @brief Reload callback function
 *
 * @param[in]     instance     instance reference
 * @param[in]     user_data_p  user data provided when calling reload
 * @param[in]     status       status from the reload
 *
 ******************************************************************************/
static void my_reload_callback(eqmhi_instance_t instance,
                               void* user_data_p,
                               eqmhi_status_t status)
{
	(void)instance;
	(void)user_data_p;

	if (status == EQMHI_STATUS_SUCCESS) {
		set_state(LOADED);
		printf("RELOAD OK!\n");
	}
	else {
		printf("Failed to reload! error:%d", status);
		error = 1;
	}
}

/* > Function: my_get_cfg_callback
 ******************************************************************************/
/**
 * @brief Get configuration callback function
 *
 * @param[in]     instance         instance reference
 * @param[in]     user_data_p      user data provided when calling get configuration
 * @param[in]     status           status from the get configuration
 * @param[in]     num_of_entities  number of configurations entities
 * @param[in]     entity           the configuration settings
 *
 ******************************************************************************/
static void my_get_cfg_callback(eqmhi_instance_t instance,
                                void* user_data_p,
                                eqmhi_status_t status,
                                uint32_t num_of_entities,
                                struct eqmhi_cfg_entity *entity)
{
	(void)instance;
	(void)user_data_p;
	uint32_t i;

	for (i = 0; i < num_of_entities; i++) {
		if(EQMHI_CFG_DUMP_FILE == entity[i].cfg.type) {
			printf("EQMH id %d Max number of dumps: %d, Dump location: %s\n",
			     entity[i].eqm_id, entity[i].cfg.dump_file.max_num_of_dumps,
			     entity[i].cfg.dump_file.dump_location);
		}
	}
}

/* > Function: my_set_cfg_callback
 ******************************************************************************/
/**
 * @brief Set configuration callback function
 *
 * @param[in]     instance     instance reference
 * @param[in]     user_data_p  user data provided when calling set configuration
 * @param[in]     status       status from the set configuration
 *
 ******************************************************************************/
static void my_set_cfg_callback(eqmhi_instance_t instance,
                                void* user_data_p,
                                eqmhi_status_t status)
{
	(void)instance;
	(void)user_data_p;

	if (status == EQMHI_STATUS_SUCCESS) {
		printf("Set configuration OK!\n");
	}
	else {
		printf("Failed to set config! error:%d", status);
		error = 1;
	}
}

/* > Function: my_fault_ind_callback
 ******************************************************************************/
/**
 * @brief Fault indication callback function
 *
 * @param[in]     instance     instance reference
 * @param[in]     user_data_p  user data provided when calling subscribe faults
 * @param[in]     fault_p      fault indication
 *
 ******************************************************************************/
static void my_fault_ind_callback(eqmhi_instance_t instance,
                              void *user_data_p,
                              struct eqmhi_fault_ind *fault_p)
{
	(void)instance;
	(void)user_data_p;

	printf("Fault indication received. Fault: %d  Error: %s\n",
		fault_p->fault_type, fault_p->error);
}


/* > Function: my_subscribe_fault_callback
 ******************************************************************************/
/**
 * @brief Subscribe fault callback function
 *
 * @param[in]     instance     instance reference
 * @param[in]     user_data_p  user data provided when calling subscribe faults
 * @param[in]     status       status from the subscribe fault
 *
 ******************************************************************************/
static void my_subscribe_fault_callback(eqmhi_instance_t instance,
					void *user_data_p,
					eqmhi_status_t status)
{
	(void)instance;
	(void)user_data_p;

	if (status == EQMHI_STATUS_SUCCESS) {
		printf("Subscribe on faults OK!\n");
	}
	else {
		printf("Failed to subscribe on faults! error:%d", status);
		error = 1;
	}
}

/* > Function: handle_response
******************************************************************************/
/**
* @brief Handles the responses from EQMH
*
* @param[in]     instance     instance reference
* @param[in]     fd           socket reference
*
******************************************************************************/
void handle_response(eqmhi_instance_t instance,
                     int32_t fd)
{
	fd_set readfds;
	uint64_t fdSize;
	int selectRc;
	int receivedMessages = 0;

	FD_ZERO(&readfds);
	FD_SET(fd, &readfds);
	fdSize = fd;

	do {
		printf("handle_response: Wait for message on FD\n");
		selectRc = select(fdSize + 1, &readfds, NULL, NULL, NULL);

		if (selectRc > 0 && FD_ISSET(fd, &readfds)) {
			receivedMessages++;
			printf("handle_response: Received message number %d\n",
				receivedMessages);
			eqmhi_status_t status = eqmhi_dispatch_callback(instance);

			if (EQMHI_STATUS_SUCCESS != status) {
				printf("handle_response: error:%d\n", status);
				error = 1;
				break;
			}
		}

		usleep(1000);

	} while (get_state() != DUMPED && !error);
}



/* > Function: clean_up
 ******************************************************************************/
/**
 * @brief Clean up after the API example test
 *
 * @param[in]     instance     instance reference
 *
 ******************************************************************************/
void clean_up(eqmhi_instance_t instance)
{
	eqmhi_status_t status = eqmhi_destroy(instance);

	if (status != EQMHI_STATUS_SUCCESS) {
		printf("Failed to destroy instance! error:%d",
		status);
	}
}

static const struct eqmhi_callback my_callbacks =
{
	my_load_callback,
	my_boot_callback,
	my_stop_callback,
	my_dump_callback,
	my_reload_callback,
	my_subscribe_fault_callback,
	my_fault_ind_callback,
	my_set_cfg_callback,
	my_get_cfg_callback,
};


/* > Function: run_eqmh_example
 ******************************************************************************/
/**
 * @brief run_eqmh_example function for the API example test
 *
 *
 ******************************************************************************/
int run_eqmh_example()
{
	uint32_t i;
	int32_t fd;
	eqmhi_status_t status;
	eqmhi_instance_t instance;
	const uint32_t emcaIds[] = {1, 2, 3, 4};
	const uint32_t numberOfEmcas = 4;
	struct eqmhi_fault_ind fault;
	struct eqmhi_cfg_entity cfgEntities[numberOfEmcas];

	for(i = 0; i < numberOfEmcas; i++) {
		cfgEntities[i].eqm_id = emcaIds[i];
		cfgEntities[i].cfg.dump_file.type = EQMHI_CFG_DUMP_FILE;
		cfgEntities[i].cfg.dump_file.max_num_of_dumps = 10;
		snprintf(cfgEntities[i].cfg.dump_file.dump_location,
			EQMHI_MAX_STRING_LENGTH, "/tmp/dspdumps");
	}
	printf("API Example started\n");

	// Create instance
	status = eqmhi_create(&instance, numberOfEmcas,
			emcaIds, &my_callbacks, &fd);

	if (status != EQMHI_STATUS_SUCCESS) {
		printf("Failed to create instance! error:%d", status);
		return 1;
	}

	printf("Success: Instance created\n");

	// Subscribe for faults
	status = eqmhi_subscribe_faults(instance, NULL, &fault,
					EQMHI_FAULT_TYPE_ALL_FAULTS);

	if (status != EQMHI_STATUS_SUCCESS) {
		printf("Failed to subscribe for faults! error:%d",
			status);
		clean_up(instance);
		return 1;
	}

	// Set configuration for dumps for all emcas
	status = eqmhi_set_cfg(instance, NULL, numberOfEmcas, cfgEntities);

	if (status != EQMHI_STATUS_SUCCESS) {
		printf("Failed to set configuration! error:%d",
			status);
		clean_up(instance);
		return 1;
	}

	// Check the configuration previously set
	status = eqmhi_get_cfg(instance,
                               NULL,
                               numberOfEmcas,
                               cfgEntities);

	if (status != EQMHI_STATUS_SUCCESS) {
		printf("Failed to get configuration! error:%d",
			status);
		clean_up(instance);
		return 1;
	}

	// load
	// "Database" containing the LM for each EMCA
	struct eqmhi_load_entity load_entity[4];
	char* lm_id[] = {"trinity1", "trinity2", "trinity3", "trinity4"};

	for (i = 0; i < numberOfEmcas; i++) {
		load_entity[i].eqm_id = emcaIds[i];
		load_entity[i].load.type = EQMHI_LOAD_TYPE_LMC;
		snprintf(load_entity[i].load.lmc.name,
			EQMHI_MAX_STRING_LENGTH, "trinity-container.bin");
		snprintf(load_entity[i].load.lmc.lm_id,
			EQMHI_MAX_STRING_LENGTH, lm_id[i]);
	}

	status = eqmhi_load(instance, NULL, 4, load_entity);

	if (status != EQMHI_STATUS_SUCCESS) {
		printf("Failed to load! error:%d", status);
		clean_up(instance);
		return 1;
	}

	// Handle the responses received from EQMH
	handle_response(instance, fd);

	clean_up(instance);

	return 0;
}
