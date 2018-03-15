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
 * @file eqmhi_api.h
 * @brief Equipment Handling service interface
 *
 * This file defines the public EQMH interface (EQMHI)
 *
 ******************************************************************************/

#ifdef __cplusplus
extern "C" {
#endif


#ifndef EQMHI_API_H
#define EQMHI_API_H


/* > Includes *****************************************************************/
#include "eqmhi_common.h"


/* > Defines ******************************************************************/

/* > Type Declarations ********************************************************/

/* > Function Declarations ****************************************************/

/**
* @brief Create a EQMH instance.
*
* Each EQMH instance will be used for one load and restart domain. It can be a
* baseband resource set or part of a baseband resource set.
*
* @param[out]    instance        EQMH instance.
* @param[in]     num_of_eqm_ids  number of load EQM identities
* @param[in]     eqm_id          Array of EQM identities that should be part of
*                                the instance.
* @param[in]     callbacks       Structure of callback functions for
*                                asynchronous handling.
* @param[out]    fd              File descriptor to poll for async
*                                notifications.
* @return        eqmhi_status_t
*
* @post          Instance is created.
*
*******************************************************************************/
eqmhi_status_t eqmhi_create(eqmhi_instance_t *instance,
                            uint32_t num_of_eqm_ids,
                            const uint32_t eqm_id[],
                            const struct eqmhi_callback *callbacks,
                            int *fd);

/**
* @brief Remove the EQMH instance.
*
* @param[in]     instance       EQMH instance
* @return        eqmhi_status_t
*
* @pre           Instance must have been created by calling eqmhi_create().
* @post          Instance i destroyed.
* @see           eqmhi_create
*
*******************************************************************************/
eqmhi_status_t eqmhi_destroy(eqmhi_instance_t instance);

/**
* @brief Configures an EQM.
*
* @param[in]     instance         EQMH instance.
* @param[in]     user_data        Application context information to be passed
*                                 back to the set configuration callback
*                                 function.
* @param[in]     num_of_entities  number of configuration entities
* @param[in]     entity           Array of configuration parameters for each
*                                 entity to configure.
*
* @return        eqmhi_status_t
*
* @pre           Instance must have been created by calling eqmhi_create().
* @post          Configuration changed the for the EQM.
* @see           eqmhi_create
*
*******************************************************************************/
eqmhi_status_t eqmhi_set_cfg(eqmhi_instance_t instance,
                             const void *user_data,
                             uint32_t num_of_entities,
                             const struct eqmhi_cfg_entity *entity);

/**
* @brief Reads configuration of an EQM.
*
* @param[in]     instance         EQMH instance.
* @param[in]     user_data        Application context information to be passed
*                                 back to the get configuration callback
*                                 function.
* @param[in]     num_of_entities  number of configuration entities
* @param[in]     entity           Array where configuration parameters from each
*                                 entity should be stored. Note that the fields
*                                 eqmhi_cfg_entity.eqm_id and
*                                 eqmhi_cfg_entity.cfg.xxx.type, e.g.
*                                 eqmhi_cfg_dump_file.type, must be set by
*                                 caller. The array is updated with
*                                 configuration parameters and supplied to
*                                 eqmhi_get_cfg_callback_func() when
*                                 eqmhi_dispatch_callback() is called.
*
* @return        eqmhi_status_t
*
* @pre           Instance must have been created by calling eqmhi_create().
* @see           eqmhi_create
*
*******************************************************************************/
eqmhi_status_t eqmhi_get_cfg(eqmhi_instance_t instance,
                             const void *user_data,
                             uint32_t num_of_entities,
                             struct eqmhi_cfg_entity *entity);

/**
* @brief Subscribe for EQMH fault notifications
*
* @param[in]     instance        EQMH instance
* @param[in]     user_data       Application context information to be passed
*                                back to the subscribe_fault callback function.
* @param[in]     user_data_fault Application context information to be passed
*                                back to the fault callback function.
* @param[in]     fault_types     Fault_types to subscribe for.
* @return        eqmhi_status_t
*
* @pre           Instance must have been created by calling eqmhi_create().
* @post          Requested faults will be forwarded to client.
* @see           eqmhi_create
*
*******************************************************************************/
eqmhi_status_t eqmhi_subscribe_faults(eqmhi_instance_t instance,
                                      const void *user_data,
                                      const void *user_data_fault,
                                      eqmhi_fault_types_t fault_types);

/**
* @brief Load the EQMH instance
*
* @param[in]     instance         EQMH instance
* @param[in]     user_data        Application context information to be passed
*                                 back to the load callback function.
* @param[in]     num_of_entities  number of load entities
* @param[in]     entity           Array of load parameter structure for each
*                                 entity to load
* @return        eqmhi_status_t
*
* @pre           Instance must have been created by calling eqmhi_create() and
*                must not be in Booted state.
* @post          Requested entities are loaded. If all entities in instance are
*                loaded then the instance has entered Loaded state.
* @see           eqmhi_create
*
*******************************************************************************/
eqmhi_status_t eqmhi_load(eqmhi_instance_t instance,
                          const void *user_data,
                          uint32_t num_of_entities,
                          const struct eqmhi_load_entity entity[]);
/**
* @brief Boot the EQMH instance
*
* @param[in]     instance       EQMH instance
* @param[in]     user_data      Application context information to be passed
*                               back to the boot callback function.
* @return        eqmhi_status_t
*
* @pre           Instance must be in Loaded state, i.e. all entities in the
*                instance must have been loaded via eqmhi_load().
* @post          Instance is in Booted state.
* @see           eqmhi_load
*
*******************************************************************************/
eqmhi_status_t eqmhi_boot(eqmhi_instance_t instance,
                          const void *user_data);

/**
* @brief Stop the EQMH instance.
*
* @param[in]     instance       EQMH instance
* @param[in]     user_data      Application context information to be passed
*                               back to the stop callback function.
* @return        eqmhi_status_t
*
* @pre           Instance must be in Booted state.
* @post          Instance is in Stopped state.
* @see           eqmhi_boot
*
*******************************************************************************/
eqmhi_status_t eqmhi_stop(eqmhi_instance_t instance,
                          const void *user_data);

/**
* @brief Reload the EQMH instance.
*
* @param[in]     instance       EQMH instance
* @param[in]     user_data      Application context information to be passed
*                               back to the reload callback function.
* @return        eqmhi_status_t
*
* @pre           Instance must be in Stopped state.
* @post          Requested entities are reloaded. If all entities in instance
*                are reloaded then the instance has entered Loaded state.
* @note          Not implemented on RU, EQMHI_STATUS_UNSUPPORTED will be
*                returned.
* @see           eqmhi_stop
*
*******************************************************************************/
eqmhi_status_t eqmhi_reload(eqmhi_instance_t instance,
                            const void *user_data);

/**
* @brief Generate Dump of the EQMH instance
*
* A dump request that is reqested when instance is already in Dumped state will
* be ignored.
*
* @param[in]     instance       EQMH instance
* @param[in]     user_data      Application context information to be passed
*                               back to the dump callback function.
* @return        eqmhi_status_t
*
* @pre           Instance must be in Stopped state.
* @post          Instance is in Dumped state.
* @see           eqmhi_stop
* @see           eqmhi_boot
*
*******************************************************************************/
eqmhi_status_t eqmhi_dump(eqmhi_instance_t instance,
                          const void *user_data);

/**
* @brief dispatch a callback function related to the message waiting on the
*        connection
*
* @param[in]     instance       EQMH instance
* @return        eqmhi_status_t
*
* @pre           The fd created by eqmhi_create() has data
*                needing to be read off the file descriptor.
* @see           eqmhi_create
*
*******************************************************************************/
eqmhi_status_t eqmhi_dispatch_callback(eqmhi_instance_t instance);

#ifdef EQMHI_DOXYGEN
/*
 * TODO:
 * The below Doxygen commands should preferably be put to a separate file.
 */
/**
 * @example Examples
 *
 * @section ru_ex RU Examples
 *
 * @ref ru_ex examplifies a simpler use case where all equipments are handled
 * within one instance.
 * @ref ru_dp_ex exemplifies Loading, configuration, booting, supervision and
 * dumping of DPs.
 * @ref ru_fpga_ex exemplifies Loading and booting of FPGAs.
 *
 * @subsection ru_dp_ex Managing DPs
 *
 * @verbinclude example_ru_dp.c
 *
 * @subsection ru_fpga_ex Managing FPGAs
 *
 * @verbinclude example_ru_fpga.c
 *
 * @section du_ex DU Examples
 *
 * @ref du_ex examplifies a more complex use case where equipments are handled
 * within multiple instances.
 *
 * @verbinclude example_du.c
 */
#endif /* EQMHI_DOXYGEN */

#endif  /* EQMHI_API_H */

#ifdef __cplusplus
} /* extern "C" */
#endif
