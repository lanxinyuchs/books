#include <sys/select.h>
#include <stddef.h>
#include "eqmhi_api.h"
#include "eqmhi_ru.h"

static const uint32_t my_eqm_id[] = {1, 2};

static const struct eqmhi_cfg_entity cfg_entity[] =
{
        /** Configuration for eqm 1. */
        {1, .cfg.dump_file = {EQMHI_CFG_DUMP_FILE, 3, "my_dump_dir1"}},
        /** Configuration for eqm 2. */
        {2, .cfg.dump_file = {EQMHI_CFG_DUMP_FILE, 5, "my_dump_dir2"}}
};

static struct eqmhi_cfg_entity retrieved_cfg_entity[] =
{
        /** Configuration for eqm 1. */
        {1, .cfg.dump_file.type = EQMHI_CFG_DUMP_FILE},
        /** Configuration for eqm 2. */
        {2, .cfg.dump_file.type = EQMHI_CFG_DUMP_FILE}
};

static const struct eqmhi_load_entity load_entity[] =
{
        {1, .load.lmc = {EQMHI_LOAD_TYPE_LMC, "my_lmc_name1", "my_lm_id1"}},
        {2, .load.lmc = {EQMHI_LOAD_TYPE_LMC, "my_lmc_name2", "my_lm_id2"}}
};

/** Callback function for load, boot, stop, dump, subscribe fault */
static void my_gen_callback(eqmhi_instance_t instance,
                            void *user_data,
                            eqmhi_status_t status)
{
        /** Return status of request. */
        *((eqmhi_status_t *) user_data) = status;
}

/* Fault callback function. */
static void my_fault_callback(eqmhi_instance_t instance,
                              void *user_data,
                              struct eqmhi_fault_ind *fault)
{
        /** Return found fault. */
        *((struct eqmhi_fault_ind *) user_data) = *fault;
}

static void my_get_cfg_callback(eqmhi_instance_t instance,
                                void *user_data,
                                eqmhi_status_t status,
                                uint32_t num_of_entities,
                                struct eqmhi_cfg_entity *entity)
{
        /** Return status of request. */
        *((eqmhi_status_t *) user_data) = status;
}

static const struct eqmhi_callback my_callbacks =
{
        /** Load Callback function */
        my_gen_callback,
        /** Boot Callback function. */
        my_gen_callback,
        /** Stop Callback function. */
        my_gen_callback,
        /** Dump Callback function. */
        my_gen_callback,
        /** Reload Callback function. */
        NULL,
        /** Subscribe fault callback function. */
        my_gen_callback,
        /** Fault Callback function. */
        my_fault_callback,
        /** Set configuration callback function. */
        my_gen_callback,
        /** Get configuration callback function. */
        my_get_cfg_callback
};

int manage_dps(void)
{
        eqmhi_instance_t instance = NULL;
        eqmhi_status_t status, async_status;
        struct eqmhi_fault_ind fault;
        int fd, retval;
        fd_set rfds;

        fault.fault_type = 0;

        /* Create instance. */
        status = eqmhi_create(&instance,
                              sizeof(my_eqm_id)/sizeof(my_eqm_id[0]),
                              my_eqm_id,
                              &my_callbacks,
                              &fd);
        if (status != EQMHI_STATUS_SUCCESS) {
                goto manage_dps_error;
        }

        /* Subscribe for faults. */
        status = eqmhi_subscribe_faults(instance,
                                        &async_status,
                                        &fault,
                                        EQMHI_FAULT_TYPE_ALL_FAULTS);
        if (status != EQMHI_STATUS_SUCCESS) {
                goto manage_dps_error;
        }

        FD_ZERO(&rfds);
        FD_SET(fd, &rfds);

        /*
         * Configure dump files. Dump of External Memory not applicable for RU.
         */
        status = eqmhi_set_cfg(instance,
                               &async_status,
                               sizeof(cfg_entity)/sizeof(cfg_entity[0]),
                               cfg_entity);
        if (status != EQMHI_STATUS_SUCCESS) {
                goto manage_dps_error;
        }

        retval = select(1, &rfds, NULL, NULL, NULL);
        if (retval > 0) {
                status = eqmhi_dispatch_callback(instance);
                if (status != EQMHI_STATUS_SUCCESS ||
                    async_status != EQMHI_STATUS_SUCCESS ||
                    fault.fault_type) {
                        goto manage_dps_error;
                }
        }

        /* Load */
        status = eqmhi_load(instance,
                            &async_status,
                            sizeof(load_entity)/sizeof(load_entity[0]),
                            load_entity);
        if (status != EQMHI_STATUS_SUCCESS) {
                goto manage_dps_error;
        }

        retval = select(1, &rfds, NULL, NULL, NULL);
        if (retval > 0) {
                status = eqmhi_dispatch_callback(instance);
                if (status != EQMHI_STATUS_SUCCESS ||
                    async_status != EQMHI_STATUS_SUCCESS ||
                    fault.fault_type) {
                        goto manage_dps_error;
                }
        }

        /* Boot */
        status = eqmhi_boot(instance, &async_status);
        if (status != EQMHI_STATUS_SUCCESS) {
                goto manage_dps_error;
        }

        retval = select(1, &rfds, NULL, NULL, NULL);
        if (retval > 0) {
                status = eqmhi_dispatch_callback(instance);
                if (status != EQMHI_STATUS_SUCCESS ||
                    async_status != EQMHI_STATUS_SUCCESS ||
                    fault.fault_type) {
                        goto manage_dps_error;
                }
        }

        /* Read dump file configuration. */
        status = eqmhi_get_cfg(instance,
                               &async_status,
                               sizeof(retrieved_cfg_entity)/
                                 sizeof(retrieved_cfg_entity[0]),
                               retrieved_cfg_entity);
        if (status != EQMHI_STATUS_SUCCESS) {
                goto manage_dps_error;
        }

        retval = select(1, &rfds, NULL, NULL, NULL);
        if (retval > 0) {
                status = eqmhi_dispatch_callback(instance);
                if (status != EQMHI_STATUS_SUCCESS ||
                    async_status != EQMHI_STATUS_SUCCESS ||
                    fault.fault_type) {
                        goto manage_dps_error;
                }
        }

        /* Wait for fault and order dump if fault occurs. */
        retval = select(1, &rfds, NULL, NULL, NULL);
        if (retval > 0) {
                status = eqmhi_dispatch_callback(instance);
                if (!fault.fault_type) {
                        /* Unexpected response */
                        goto manage_dps_error;
                }
        }

        fault.fault_type = 0;

        /* Stop */
        status = eqmhi_stop(instance, &async_status);
        if (status != EQMHI_STATUS_SUCCESS) {
                goto manage_dps_error;
        }

        retval = select(1, &rfds, NULL, NULL, NULL);
        if (retval > 0) {
                status = eqmhi_dispatch_callback(instance);
                if (status != EQMHI_STATUS_SUCCESS ||
                    async_status != EQMHI_STATUS_SUCCESS ||
                    fault.fault_type) {
                        goto manage_dps_error;
                }
        }

        /* Dump */
        status = eqmhi_dump(instance, &async_status);
        if (status != EQMHI_STATUS_SUCCESS) {
                goto manage_dps_error;
        }

        retval = select(1, &rfds, NULL, NULL, NULL);
        if (retval > 0) {
                status = eqmhi_dispatch_callback(instance);
                if (status != EQMHI_STATUS_SUCCESS ||
                    async_status != EQMHI_STATUS_SUCCESS ||
                    fault.fault_type) {
                        goto manage_dps_error;
                }
        }

manage_dps_error:
        if (instance != NULL)
        {
                (void) eqmhi_destroy(instance);
        }
        return -1;
}
