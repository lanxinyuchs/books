#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <stdint.h>
#include <string.h>
#include <unistd.h>
#include <assert.h>
#include <stdbool.h>
#include <pthread.h>
#include <sys/types.h>

#include <itc.h>
#include <conn-establish-helper.h>

#include "eqmhi_api.h"
#include "eqmhi_ru.h"
#include "eqmh_msg.h"

#define TRACEPOINT_PROVIDER com_ericsson_eqmhi
#include <tpt_create.h>
#include <tpt.h>

#define MBOX_NAME_BASE        "EQMHI_"
#define MBOX_MAX_NAME_SIZE    (sizeof(MBOX_NAME_BASE) + 10)

static itc_mbox_id_t eqmhi_mbox = ITC_NO_ID;
static itc_mbox_id_t eqmhd_mbox = ITC_NO_ID;
static pthread_t eqmhi_thread;
static uint32_t procedure_ref;
static bool thread_failed;

static pthread_mutex_t init_mutex = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t init_cond = PTHREAD_COND_INITIALIZER;

#define READFD   0
#define WRITEFD  1

#define REQUIRE(x, s) if (!(x)) do {                                 \
	TPT_INFO("Required '" #x "' to be true, returning: " #s);        \
	status = (s);                                                    \
	goto exit;                                                       \
} while(0)

union itc_msg {
	uint32_t msgno;
	EQMH_MESSAGE_STRUCTS;
};

struct eqmhi_instance {
	uint32_t server_instance;
	struct eqmhi_callback callbacks;
	void *load_user_data;
	void *boot_user_data;
	void *stop_user_data;
	void *dump_user_data;
	int pipe_fds[2];
	uint32_t server_ref;
	struct eqmhi_cfg_entity *entities;
};

static struct conn_establish_msg_numbers eqmh_conn_establish_msg_numbers = {
	EQMH_CONN_ESTABLISH_REQ,
	EQMH_CONN_ESTABLISH_CFM,
	EQMH_CONN_ESTABLISH_REJ,
	EQMH_CONN_DISCONNECT_REQ,
	EQMH_CONN_DISCONNECT_CFM,
	EQMH_CONN_DISCONNECT_REJ,
	EQMH_CONN_MONITOR_FWD,
};

static void *eqmhi_thr_fxn(void *arg)
{
	char mbox_name[MBOX_MAX_NAME_SIZE];

	snprintf(mbox_name, MBOX_MAX_NAME_SIZE, MBOX_NAME_BASE "%d", getpid());
	eqmhi_mbox = itc_create_mailbox(mbox_name, 0);
	if (eqmhi_mbox == ITC_NO_ID) {
		TPT_ERROR(STR("Failed to create mailbox %s", mbox_name));
		thread_failed = true;
	}

	pthread_mutex_lock(&init_mutex);
	pthread_cond_signal(&init_cond);
	pthread_mutex_unlock(&init_mutex);

	while (!thread_failed) {
		union itc_msg *msg;
		uint32_t msg_size;
		eqmhi_instance_t inst;

		msg = itc_receive(ITC_NOFILTER, ITC_NO_TMO, eqmhd_mbox);
		TPT_REC_SIG(msg->msgno, "dispatching to callback");
		inst = (eqmhi_instance_t) msg->any_msg.connection_ref;
		msg_size = itc_size(msg);
		if (write(inst->pipe_fds[WRITEFD], &msg_size, 4) != 4) {
			TPT_ERROR("Failed to write to pipe");
			thread_failed = true;
		}

		if (write(inst->pipe_fds[WRITEFD], msg, msg_size) != msg_size) {
			TPT_ERROR("Failed to write to pipe");
			thread_failed = true;
		}

		itc_free(&msg);
	}

	return thread_failed ? (void *) 1 : (void *) 0;
}

#define CONN_TIMEOUT 0

static int eqmhi_init()
{
	eqmhd_mbox = itc_locate(EQMH_MAILBOX);
	if (eqmhd_mbox == ITC_NO_ID) {
		TPT_ERROR(STR("Failed to locate mailbox %s", EQMH_MAILBOX));
		return 1;
	}

	if (pthread_create(&eqmhi_thread, NULL, eqmhi_thr_fxn, NULL)) {
		TPT_ERROR("Failed to create eqmh thread");
		return 1;
	}

	pthread_mutex_lock(&init_mutex);
	pthread_cond_wait(&init_cond, &init_mutex);
	pthread_mutex_unlock(&init_mutex);

	return thread_failed ? 1 : 0;
}

static void cleanup(eqmhi_instance_t inst)
{
	close(inst->pipe_fds[READFD]);
	close(inst->pipe_fds[WRITEFD]);

	free(inst);
}

static int connect_to_eqmhd(eqmhi_instance_t inst)
{
	uint32_t selected_version;
	uint32_t requested_versions[] = { EQMH_SERVER_VERSIONS };
	int ret = conn_establish(
	              /*input parameters*/
	              eqmhd_mbox,
	              ++procedure_ref,
	              (uint32_t) inst,
	              sizeof(requested_versions) / sizeof(requested_versions[0]),
	              requested_versions,
	              &eqmh_conn_establish_msg_numbers,
	              CONN_TIMEOUT,
	              /*returned values*/
	              &inst->server_ref,
	              &selected_version);
	if (ret != CONN_ESTABLISH_SUCCESS)
		return 1;

	return 0;
}

eqmhi_status_t eqmhi_create(eqmhi_instance_t *instance,
                            uint32_t num_of_eqm_ids,
                            const uint32_t *eqm_id,
                            const struct eqmhi_callback *callbacks,
                            int *fd)
{
	size_t size;
	union itc_msg *msg;
	union itc_msg *reply = NULL;
	eqmhi_instance_t inst = NULL;
	eqmhi_status_t status = EQMHI_STATUS_SUCCESS;

	REQUIRE(instance, EQMHI_STATUS_INVALID_PARAM);
	REQUIRE(eqm_id, EQMHI_STATUS_INVALID_PARAM);
	REQUIRE(callbacks, EQMHI_STATUS_INVALID_PARAM);
	REQUIRE(num_of_eqm_ids, EQMHI_STATUS_INVALID_PARAM);

	/* Attach to daemon and create thread if not done previously */
	if (eqmhd_mbox == ITC_NO_ID)
		REQUIRE(!eqmhi_init(), EQMHI_STATUS_OTHER);

	inst = calloc(1, sizeof(struct eqmhi_instance));
	REQUIRE(inst, EQMHI_STATUS_OTHER);

	/*
	 * Create a unique connection for each instance to tell them apart when
	 * a reply is received.
	 */
	REQUIRE(!connect_to_eqmhd(inst), EQMHI_STATUS_OTHER);

	size = sizeof(struct eqmh_create_req) +
	                  num_of_eqm_ids * sizeof(uint32_t);
	msg = itc_alloc(size, EQMH_CREATE_REQ);

	msg->eqmh_create_req.procedure_ref = ++procedure_ref;
	msg->eqmh_create_req.connection_ref = inst->server_ref;
	msg->eqmh_create_req.num_of_eqm_ids = num_of_eqm_ids;
	memcpy(msg->eqmh_create_req.eqm_id, eqm_id,
	       num_of_eqm_ids * sizeof(uint32_t));

	TPT_SEND_SIG(EQMH_CREATE_REQ, eqmhd_mbox, "EQMH_CREATE_REQ");
	itc_send(&msg, eqmhd_mbox, ITC_MY_MBOX);

	reply = itc_receive(ITC_NOFILTER, ITC_NO_TMO, eqmhd_mbox);
	TPT_REC_SIG(reply->msgno, reply->msgno == EQMH_CREATE_CFM ?
	            "EQMH_CREATE_CFM" : "EQMH_CREATE_REJ");
	REQUIRE(reply->msgno == EQMH_CREATE_CFM, EQMHI_STATUS_OTHER);

	inst->server_instance = reply->eqmh_create_cfm.instance;
	inst->callbacks = *callbacks;
	REQUIRE(!pipe(inst->pipe_fds), EQMHI_STATUS_OTHER);

	*fd = inst->pipe_fds[READFD];
	*instance = inst;

exit:
	if (status != EQMHI_STATUS_SUCCESS) {
		if (inst) cleanup(inst);
	}

	if (reply)
		itc_free(&reply);

	return status;
}

eqmhi_status_t eqmhi_destroy(eqmhi_instance_t instance)
{
	eqmhi_status_t status = EQMHI_STATUS_SUCCESS;
	union itc_msg *msg;
	union itc_msg *reply = NULL;
	int ret;

	REQUIRE(instance, EQMHI_STATUS_INVALID_PARAM);
	REQUIRE(instance->server_instance, EQMHI_STATUS_WRONG_STATE);
	REQUIRE(eqmhd_mbox != ITC_NO_ID, EQMHI_STATUS_WRONG_STATE);

	msg = itc_alloc(sizeof(struct eqmh_destroy_req), EQMH_DESTROY_REQ);

	msg->eqmh_destroy_req.procedure_ref = ++procedure_ref;
	msg->eqmh_destroy_req.connection_ref = instance->server_ref;
	msg->eqmh_destroy_req.instance = instance->server_instance;

	TPT_SEND_SIG(EQMH_DESTROY_REQ, eqmhd_mbox, "EQMH_DESTROY_REQ");
	itc_send(&msg, eqmhd_mbox, ITC_MY_MBOX);

	reply = itc_receive(ITC_NOFILTER, ITC_NO_TMO, eqmhd_mbox);
	if (reply->msgno == EQMH_DESTROY_REJ) {
		TPT_REC_SIG(reply->msgno, "EQMH_DESTROY_REJ");
		status = reply->eqmh_destroy_rej.status;
	}
	else {
		TPT_REC_SIG(reply->msgno, "EQMH_DESTROY_CFM");
	}

	ret = conn_disconnect(eqmhd_mbox,
	                      ++procedure_ref,
	                      instance->server_ref,
	                      &eqmh_conn_establish_msg_numbers,
	                      CONN_TIMEOUT);
	if (ret != CONN_ESTABLISH_SUCCESS)
		status = EQMHI_STATUS_OTHER;

	cleanup(instance);

exit:
	if (reply)
		itc_free(&reply);

	return status;
}

eqmhi_status_t eqmhi_set_cfg(eqmhi_instance_t instance,
                             const void *user_data,
                             uint32_t num_of_entities,
                             const struct eqmhi_cfg_entity *entity)
{
	eqmhi_status_t status = EQMHI_STATUS_SUCCESS;
	union itc_msg *msg;
	size_t size = sizeof(struct eqmh_set_cfg_req);
	char *dp_data;

	REQUIRE(instance, EQMHI_STATUS_INVALID_PARAM);
	REQUIRE(instance->server_instance, EQMHI_STATUS_WRONG_STATE);
	REQUIRE(eqmhd_mbox != ITC_NO_ID, EQMHI_STATUS_WRONG_STATE);
	REQUIRE(eqmhi_mbox != ITC_NO_ID, EQMHI_STATUS_WRONG_STATE);
	REQUIRE(num_of_entities > 0, EQMHI_STATUS_INVALID_PARAM);
	REQUIRE(num_of_entities < EQMH_MAX_NO_ENTITIES, EQMHI_STATUS_INVALID_PARAM);

	for (int i = 0; i < num_of_entities; i++) {
		if (entity[i].cfg.type == EQMHI_CFG_RU_DP) {
			size += entity->cfg.dp.size;
		}
	}

	msg = itc_alloc(size, EQMH_SET_CFG_REQ);

	/* Pack the dp data in to the signal */
	dp_data = ((char *) msg) + sizeof(struct eqmh_set_cfg_req);
	for (int i = 0; i < num_of_entities; i++) {
		msg->eqmh_set_cfg_req.entities[i] = entity[i];
		if (entity[i].cfg.type == EQMHI_CFG_RU_DP) {
			EQMH_COPY_DP_DATA_SET_INDEX(entity[i],
			    msg->eqmh_set_cfg_req.entities[i], dp_data, msg);
			dp_data += entity[i].cfg.dp.size;
		}
	}

	msg->eqmh_set_cfg_req.num_of_entities = num_of_entities;
	msg->eqmh_set_cfg_req.instance = instance->server_instance;
	msg->eqmh_set_cfg_req.user_data = user_data;
	msg->eqmh_set_cfg_req.procedure_ref = ++procedure_ref;
	msg->eqmh_set_cfg_req.connection_ref = instance->server_ref;

	TPT_SEND_SIG(EQMH_SET_CFG_REQ, eqmhd_mbox, "EQMH_SET_CFG_REQ");
	itc_send(&msg, eqmhd_mbox, eqmhi_mbox);

exit:
	return status;
}

eqmhi_status_t eqmhi_get_cfg(eqmhi_instance_t instance,
                             const void *user_data,
                             uint32_t num_of_entities,
                             struct eqmhi_cfg_entity *entity)
{
	eqmhi_status_t status = EQMHI_STATUS_SUCCESS;
	union itc_msg *msg;

	REQUIRE(instance, EQMHI_STATUS_INVALID_PARAM);
	REQUIRE(instance->server_instance, EQMHI_STATUS_WRONG_STATE);
	REQUIRE(eqmhd_mbox != ITC_NO_ID, EQMHI_STATUS_WRONG_STATE);
	REQUIRE(eqmhi_mbox != ITC_NO_ID, EQMHI_STATUS_WRONG_STATE);
	REQUIRE(num_of_entities > 0, EQMHI_STATUS_INVALID_PARAM);
	REQUIRE(num_of_entities < EQMH_MAX_NO_ENTITIES, EQMHI_STATUS_INVALID_PARAM);
	REQUIRE(instance->entities == NULL, EQMHI_STATUS_WRONG_STATE);

	msg = itc_alloc(sizeof(struct eqmh_get_cfg_req), EQMH_GET_CFG_REQ);

	for (int i = 0; i < num_of_entities; i++)
		msg->eqmh_get_cfg_req.entities[i] = entity[i];

	msg->eqmh_set_cfg_req.num_of_entities = num_of_entities;
	msg->eqmh_get_cfg_req.user_data = user_data;
	msg->eqmh_get_cfg_req.instance = instance->server_instance;
	msg->eqmh_get_cfg_req.procedure_ref = ++procedure_ref;
	msg->eqmh_get_cfg_req.connection_ref = instance->server_ref;

	TPT_SEND_SIG(EQMH_GET_CFG_REQ, eqmhd_mbox, "EQMH_GET_CFG_REQ");
	itc_send(&msg, eqmhd_mbox, eqmhi_mbox);

	instance->entities = entity;

exit:
	return status;
}

eqmhi_status_t eqmhi_load(eqmhi_instance_t instance,
                          const void *user_data,
                          uint32_t num_of_entities,
                          const struct eqmhi_load_entity *entity)
{
	size_t size = sizeof(struct eqmh_load_req);
	union itc_msg *msg;
	eqmhi_status_t status = EQMHI_STATUS_SUCCESS;

	REQUIRE(instance, EQMHI_STATUS_INVALID_PARAM);
	REQUIRE(instance->server_instance, EQMHI_STATUS_WRONG_STATE);
	REQUIRE(num_of_entities, EQMHI_STATUS_INVALID_PARAM);
	REQUIRE(entity, EQMHI_STATUS_INVALID_PARAM);
	REQUIRE(eqmhd_mbox != ITC_NO_ID, EQMHI_STATUS_WRONG_STATE);
	REQUIRE(eqmhi_mbox != ITC_NO_ID, EQMHI_STATUS_WRONG_STATE);

	instance->load_user_data = (void *) user_data;

	size += num_of_entities * sizeof(struct eqmhi_load_entity);
	msg = itc_alloc(size, EQMH_LOAD_REQ);

	msg->eqmh_load_req.procedure_ref = ++procedure_ref;
	msg->eqmh_load_req.connection_ref = instance->server_ref;
	msg->eqmh_load_req.num_of_entities = num_of_entities;
	msg->eqmh_load_req.instance = instance->server_instance;
	memcpy(msg->eqmh_load_req.entities, entity,
	       num_of_entities * sizeof(struct eqmhi_load_entity));

	TPT_SEND_SIG(EQMH_LOAD_REQ, eqmhd_mbox, "EQMH_LOAD_REQ");
	itc_send(&msg, eqmhd_mbox, eqmhi_mbox);

exit:
	return status;
}

eqmhi_status_t eqmhi_boot(eqmhi_instance_t instance,
                          const void *user_data)
{
	union itc_msg *msg;
	eqmhi_status_t status = EQMHI_STATUS_SUCCESS;

	REQUIRE(instance, EQMHI_STATUS_INVALID_PARAM);
	REQUIRE(instance->server_instance, EQMHI_STATUS_WRONG_STATE);
	REQUIRE(eqmhd_mbox != ITC_NO_ID, EQMHI_STATUS_WRONG_STATE);
	REQUIRE(eqmhi_mbox != ITC_NO_ID, EQMHI_STATUS_WRONG_STATE);

	instance->boot_user_data = (void *) user_data;

	msg = itc_alloc(sizeof(struct eqmh_boot_req), EQMH_BOOT_REQ);

	msg->eqmh_boot_req.procedure_ref = ++procedure_ref;
	msg->eqmh_boot_req.connection_ref = instance->server_ref;
	msg->eqmh_boot_req.instance = instance->server_instance;

	TPT_SEND_SIG(EQMH_BOOT_REQ, eqmhd_mbox, "EQMH_BOOT_REQ");
	itc_send(&msg, eqmhd_mbox, eqmhi_mbox);

exit:
	return status;
}

eqmhi_status_t eqmhi_stop(eqmhi_instance_t instance,
                          const void *user_data)
{
	union itc_msg *msg;
	eqmhi_status_t status = EQMHI_STATUS_SUCCESS;

	REQUIRE(instance, EQMHI_STATUS_INVALID_PARAM);
	REQUIRE(instance->server_instance, EQMHI_STATUS_WRONG_STATE);
	REQUIRE(eqmhd_mbox != ITC_NO_ID, EQMHI_STATUS_WRONG_STATE);
	REQUIRE(eqmhi_mbox != ITC_NO_ID, EQMHI_STATUS_WRONG_STATE);

	instance->stop_user_data = (void *) user_data;

	msg = itc_alloc(sizeof(struct eqmh_stop_req), EQMH_STOP_REQ);

	msg->eqmh_stop_req.procedure_ref = ++procedure_ref;
	msg->eqmh_stop_req.connection_ref = instance->server_ref;
	msg->eqmh_stop_req.instance = instance->server_instance;

	TPT_SEND_SIG(EQMH_STOP_REQ, eqmhd_mbox, "EQMH_STOP_REQ");
	itc_send(&msg, eqmhd_mbox, eqmhi_mbox);

exit:
	return status;
}

eqmhi_status_t eqmhi_dump(eqmhi_instance_t instance,
                          const void *user_data)
{
	union itc_msg *msg;
	eqmhi_status_t status = EQMHI_STATUS_SUCCESS;

	REQUIRE(instance, EQMHI_STATUS_INVALID_PARAM);
	REQUIRE(instance->server_instance, EQMHI_STATUS_WRONG_STATE);
	REQUIRE(eqmhd_mbox != ITC_NO_ID, EQMHI_STATUS_WRONG_STATE);
	REQUIRE(eqmhi_mbox != ITC_NO_ID, EQMHI_STATUS_WRONG_STATE);

	instance->dump_user_data = (void *) user_data;

	msg = itc_alloc(sizeof(struct eqmh_dump_req), EQMH_DUMP_REQ);

	msg->eqmh_dump_req.procedure_ref = ++procedure_ref;
	msg->eqmh_dump_req.connection_ref = instance->server_ref;
	msg->eqmh_dump_req.instance = instance->server_instance;

	TPT_SEND_SIG(EQMH_DUMP_REQ, eqmhd_mbox, "EQMH_DUMP_REQ");
	itc_send(&msg, eqmhd_mbox, eqmhi_mbox);

exit:
	return status;
}

static void copy_configs(struct eqmh_get_cfg_cfm *cfm,
                         struct eqmhi_cfg_entity *dst,
                         uint32_t num_of_entities)
{
	int i, j;

	/*
	 * Cycle through the destination structs and find the right eqm_id and
	 * type, and when a match is found copy it from src to dst.
	 */
	for (i = 0; i < num_of_entities; i++) {
		for (j = 0; j < num_of_entities; j++) {
			if (dst[i].eqm_id == cfm->entities[j].eqm_id &&
			    dst[i].cfg.type == cfm->entities[j].cfg.type) {
				memcpy(&dst[i], &cfm->entities[j], sizeof(struct eqmhi_cfg_entity));

				if (cfm->entities[j].cfg.type == EQMHI_CFG_RU_DP)
					dst[i].cfg.dp.buf = EQMH_FROM_INDEX(cfm->entities[j], cfm);

				j = INT_MAX; /* Exit the loop and mark entity as found */
				break;
			}
		}

		/* Internal error, using assert is fine */
		assert(j == INT_MAX);
	}
}

eqmhi_status_t eqmhi_dispatch_callback(eqmhi_instance_t instance)
{
	eqmhi_status_t status = EQMHI_STATUS_SUCCESS;
	eqmhi_status_t cb_status = EQMHI_STATUS_SUCCESS;
	uint32_t size;
	union itc_msg *msg = NULL;
	int fd;

	REQUIRE(instance, EQMHI_STATUS_INVALID_PARAM);
	REQUIRE(instance->server_instance, EQMHI_STATUS_WRONG_STATE);

	fd = instance->pipe_fds[READFD];
	REQUIRE(read(fd, &size, 4) == 4, EQMHI_STATUS_OTHER);

	msg = malloc(size);
	REQUIRE(msg, EQMHI_STATUS_OTHER);

	/* Keep coverity happy */
	msg->msgno = 0;

	REQUIRE(read(fd, msg, size) == size, EQMHI_STATUS_OTHER);

	switch (msg->msgno) {
		case EQMH_SET_CFG_REJ:
			cb_status = msg->eqmh_set_cfg_rej.status;
			REQUIRE(instance->callbacks.set_cfg_callback, EQMHI_STATUS_WRONG_STATE);
			instance->callbacks.set_cfg_callback(instance,
			    msg->eqmh_set_cfg_rej.user_data, cb_status);
			break;
		case EQMH_SET_CFG_CFM:
			REQUIRE(instance->callbacks.set_cfg_callback, EQMHI_STATUS_WRONG_STATE);
			instance->callbacks.set_cfg_callback(instance,
			    msg->eqmh_set_cfg_cfm.user_data, cb_status);
			break;
		case EQMH_GET_CFG_REJ:
			cb_status = msg->eqmh_get_cfg_rej.status;
			REQUIRE(instance->callbacks.get_cfg_callback, EQMHI_STATUS_WRONG_STATE);
			instance->callbacks.get_cfg_callback(instance,
			    msg->eqmh_get_cfg_rej.user_data, cb_status, 0, NULL);
			break;
		case EQMH_GET_CFG_CFM:
			REQUIRE(instance->callbacks.get_cfg_callback, EQMHI_STATUS_WRONG_STATE);

			copy_configs(&msg->eqmh_get_cfg_cfm,
			             instance->entities,
			             msg->eqmh_get_cfg_cfm.num_of_entities);

			instance->callbacks.get_cfg_callback(instance,
			    msg->eqmh_get_cfg_cfm.user_data, cb_status,
				msg->eqmh_get_cfg_cfm.num_of_entities,
				instance->entities);
			instance->entities = NULL;
			break;
		case EQMH_LOAD_REJ:
			cb_status = msg->eqmh_load_rej.status;
		case EQMH_LOAD_CFM:
			REQUIRE(instance->callbacks.load_callback, EQMHI_STATUS_WRONG_STATE);
			instance->callbacks.load_callback(instance, instance->load_user_data,
			                              cb_status);
			break;
		case EQMH_BOOT_REJ:
			cb_status = msg->eqmh_boot_rej.status;
		case EQMH_BOOT_CFM:
			REQUIRE(instance->callbacks.boot_callback, EQMHI_STATUS_WRONG_STATE);
			instance->callbacks.boot_callback(instance, instance->boot_user_data,
			                              cb_status);
			break;
		case EQMH_DUMP_REJ:
			cb_status = msg->eqmh_dump_rej.status;
		case EQMH_DUMP_CFM:
			REQUIRE(instance->callbacks.dump_callback, EQMHI_STATUS_WRONG_STATE);
			instance->callbacks.dump_callback(instance, instance->dump_user_data,
			                              cb_status);
			break;
		case EQMH_STOP_REJ:
			cb_status = msg->eqmh_stop_rej.status;
		case EQMH_STOP_CFM:
			REQUIRE(instance->callbacks.stop_callback, EQMHI_STATUS_WRONG_STATE);
			instance->callbacks.stop_callback(instance, instance->stop_user_data,
			                              cb_status);
			break;
		default:
			TPT_INFO(STR("Received bogus message %#x", msg->msgno));
			status = EQMHI_STATUS_OTHER;
			break;
	}

exit:
	if (msg) free(msg);
	return status;
}
