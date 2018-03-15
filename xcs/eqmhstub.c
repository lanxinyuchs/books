/**
 *   Copyright (C) 2014 by Ericsson AB. All rights reserved. The
 *   information in this document is the property of Ericsson. Except
 *   as specifically authorized in writing by Ericsson, the receiver
 *   of this document shall keep the information contained herein
 *   confidential and shall protect the same in whole or in part from
 *   disclosure and dissemination to third parties. Disclosure and
 *   disseminations to the receiver's employees shall only be made on
 *   a strict need to know basis.
 */

#include "eqmhi_api.h"

eqmhi_status_t eqmhi_create(eqmhi_instance_t *instance,
                            uint32_t num_of_eqm_ids,
                            const uint32_t *eqm_id,
                            const struct eqmhi_callback *callbacks,
                            int *fd)
{
	return EQMHI_STATUS_SUCCESS;
}

eqmhi_status_t eqmhi_destroy(eqmhi_instance_t instance)
{
	return EQMHI_STATUS_SUCCESS;
}

eqmhi_status_t eqmhi_set_cfg(eqmhi_instance_t instance,
                             const void *user_data,
                             uint32_t num_of_entities,
                             const struct eqmhi_cfg_entity *entity)
{
	return EQMHI_STATUS_SUCCESS;
}

eqmhi_status_t eqmhi_get_cfg(eqmhi_instance_t instance,
                             const void *user_data,
                             uint32_t num_of_entities,
                             struct eqmhi_cfg_entity *entity)
{
	return EQMHI_STATUS_SUCCESS;
}

eqmhi_status_t eqmhi_subscribe_faults(eqmhi_instance_t instance,
                                      const void *user_data,
                                      const void *user_data_fault,
                                      eqmhi_fault_types_t fault_types)
{
	return EQMHI_STATUS_SUCCESS;
}
eqmhi_status_t eqmhi_load(eqmhi_instance_t instance,
                          const void *user_data,
                          uint32_t num_of_entities,
                          const struct eqmhi_load_entity *entity)
{
	return EQMHI_STATUS_SUCCESS;
}

eqmhi_status_t eqmhi_boot(eqmhi_instance_t instance, const void *user_data)
{
	return EQMHI_STATUS_SUCCESS;
}

eqmhi_status_t eqmhi_stop(eqmhi_instance_t instance, const void *user_data)
{
	return EQMHI_STATUS_SUCCESS;
}
eqmhi_status_t eqmhi_reload(eqmhi_instance_t instance, const void *user_data)
{
	return EQMHI_STATUS_SUCCESS;
}

eqmhi_status_t eqmhi_dump(eqmhi_instance_t instance, const void *user_data)
{
	return EQMHI_STATUS_SUCCESS;
}

eqmhi_status_t eqmhi_dispatch_callback(eqmhi_instance_t instance)
{
	return EQMHI_STATUS_SUCCESS;
}
