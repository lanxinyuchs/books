#include <sys/select.h>
#include <stddef.h>
#include "eqmhi_api.h"
#include "eqmhi_ru.h"

static const uint32_t my_eqm_id[] = {1, 2};

static const struct eqmhi_load_entity load_entity[] =
{
        {1, .load.lm = {EQMHI_LOAD_TYPE_LM, "my_lm_id1"}},
        {2, .load.lm = {EQMHI_LOAD_TYPE_LM, "my_lm_id2"}}
};

/** Callback function for load, boot, stop, subscribe fault */
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

static const struct eqmhi_callback my_callbacks =
{
        /** Load Callback function */
        my_gen_callback,
        /** Boot Callback function. */
        my_gen_callback,
        /** Stop Callback function. */
        my_gen_callback,
        /** Dump Callback function. */
        NULL,
        /** Reload Callback function. */
        NULL,
        /** Subscribe fault callback function. */
        my_gen_callback,
        /** Fault Callback function. */
        my_fault_callback,
        /** Set configuration callback function. */
        NULL,
        /** Get configuration callback function. */
        NULL,
};

int manage_fpgas(void)
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
                goto manage_fpgas_error;
        }

        /* Subscribe for faults. */
        status = eqmhi_subscribe_faults(instance,
                                        &async_status,
                                        &fault,
                                        EQMHI_FAULT_TYPE_ALL_FAULTS);
        if (status != EQMHI_STATUS_SUCCESS) {
                goto manage_fpgas_error;
        }

        FD_ZERO(&rfds);
        FD_SET(fd, &rfds);


        /* Load */
        status = eqmhi_load(instance,
                            &async_status,
                            sizeof(load_entity)/sizeof(load_entity[0]),
                            load_entity);
        if (status != EQMHI_STATUS_SUCCESS) {
                goto manage_fpgas_error;
        }

        retval = select(1, &rfds, NULL, NULL, NULL);
        if (retval > 0) {
                status = eqmhi_dispatch_callback(instance);
                if (status != EQMHI_STATUS_SUCCESS ||
                    async_status != EQMHI_STATUS_SUCCESS ||
                    fault.fault_type) {
                        goto manage_fpgas_error;
                }
        }

        /* Boot */
        status = eqmhi_boot(instance, &async_status);
        if (status != EQMHI_STATUS_SUCCESS) {
                goto manage_fpgas_error;
        }

        retval = select(1, &rfds, NULL, NULL, NULL);
        if (retval > 0) {
                status = eqmhi_dispatch_callback(instance);
                if (status != EQMHI_STATUS_SUCCESS ||
                    async_status != EQMHI_STATUS_SUCCESS ||
                    fault.fault_type) {
                        goto manage_fpgas_error;
                }
        }

manage_fpgas_error:
        if (instance != NULL)
        {
                (void) eqmhi_destroy(instance);
        }
        return -1;
}
