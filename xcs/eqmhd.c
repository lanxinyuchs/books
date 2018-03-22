#include <stdbool.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <pthread.h>
#include <assert.h>
#include <sys/queue.h>

#include <itc.h>
#include <conn-establish-helper.h>

#include "eqmhi_api.h"
#include "eqmh_msg.h"

#include "eqm.h"

#define TRACEPOINT_PROVIDER com_ericsson_eqmhd
#include <tpt_create.h>
#include <tpt.h>

#define EQMH_SERVER_VERSIONS 1

#define MAILBOX_SIZE (8 + 1 + 1 + 16 * 8)

#define MSGNO_OP(msgno)  (msgno & 0xf)
#define MSGNO_REJ(msgno) (MSGNO_OP(msgno) | EQMH_REJ)

struct eqmh_forward_req {
	EQMH_MSG_COMMON;
	struct instance *instance;
};

struct eqmh_config_req {
	EQMH_MSG_COMMON;
	struct instance *instance;
	const void *user_data;
	uint32_t num_of_entities;
	struct eqmhi_cfg_entity entities[EQMH_MAX_NO_ENTITIES];
};

struct eqmh_config_cfm {
	EQMH_MSG_COMMON;
	void *user_data;
};

struct eqmh_config_rej {
	EQMH_MSG_COMMON;
	void *user_data;
	eqmhi_status_t status;
};

struct eqmh_generic_rej {
	EQMH_MSG_COMMON;
	eqmhi_status_t status;
};

struct eqmh_generic_cfm {
	EQMH_MSG_COMMON;
};

union itc_msg {
	uint32_t msgno;
	struct eqmh_forward_req eqmh_forward_req;
	struct eqmh_config_req eqmh_config_req;
	struct eqmh_config_rej eqmh_config_rej;
	struct eqmh_generic_rej eqmh_generic_rej;
	struct eqmh_generic_cfm eqmh_generic_cfm;
	struct eqmh_config_cfm eqmh_config_cfm;
	EQMH_MESSAGE_STRUCTS;
};

typedef enum {
	EQM_STATE_DONE = 0,
	EQM_STATE_CONFIG,
	EQM_STATE_BUSY,
	EQM_STATE_ERROR,
} eqm_state_t;

struct instance;
struct connection_data;

struct eqm {
	uint32_t id;
	pthread_t thread;
	bool pthread_started;
	itc_mbox_id_t mbox;
	eqm_state_t state;
	eqmhi_status_t status;
	uint32_t configs_left;
	struct instance *instance;
	SLIST_ENTRY(eqm) e;
};

struct instance {
	uint32_t client_ref;
	struct connection_data *conn_data;
	struct eqmh_config_req cfg_req;
	bool configuring;
	SLIST_HEAD(eqm_head, eqm) eqms;
	SLIST_ENTRY(instance) e;
};

struct connection_data {
	SLIST_HEAD(instance_head, instance) instances;
	SLIST_ENTRY(connection_data) e;
};

static SLIST_HEAD(connection_data_head, connection_data) connections;

static struct eqm * get_eqm_from_id(uint32_t eqm_id, struct instance *instance)
{
	struct eqm *eqm;

	SLIST_FOREACH(eqm, &instance->eqms, e) {
		if (eqm_id == eqm->id)
			return eqm;
	}

	return NULL;
}

static bool id_already_in_use(uint32_t id)
{
	struct connection_data *conn_data;
	struct instance *instance;

	SLIST_FOREACH(conn_data, &connections, e) {
		SLIST_FOREACH(instance, &conn_data->instances, e) {
			if (get_eqm_from_id(id, instance)) {
				TPT_INFO(STR("eqm id %d already used", id));
				return true;
			}
		}
	}

	return false;
}

static bool validate_id(uint32_t id)
{
	if (id < 1 || id > 8)
		return false;

	return true;
}

static void reject(union itc_msg *msg, struct conn_client_info *client_info,
                   eqmhi_status_t status)
{
	union itc_msg *reply = itc_alloc(sizeof(struct eqmh_generic_rej),
	                                 MSGNO_REJ(msg->msgno));
	reply->eqmh_generic_rej.status = status;
	reply->any_msg.connection_ref = client_info->client_ref;
	reply->any_msg.procedure_ref = msg->any_msg.procedure_ref;

	TPT_SEND_SIG(reply->msgno, itc_sender(msg), "REJ from dp thread");
	itc_send(&reply, itc_sender(msg), ITC_MY_MBOX);
}

bool check_state(struct eqmh_forward_req *req)
{
	struct eqm *eqm;

	SLIST_FOREACH(eqm, &req->instance->eqms, e) {
		if (eqm->state != EQM_STATE_DONE)
			return false;
	}

	return true;
}

static void destroy_thread(struct eqm *eqm)
{
	uint32_t filter[3] = { 2, EQMH_DESTROY_CFM, EQMH_DESTROY_REJ };
	union itc_msg *reply;
	union itc_msg *msg = itc_alloc(sizeof(struct eqmh_destroy_req),
	                               EQMH_DESTROY_REQ);
	TPT_SEND_SIG(msg->msgno, eqm->mbox, "EQMH_DESTROY_REQ");
	itc_send(&msg, eqm->mbox, ITC_MY_MBOX);

	reply = itc_receive(filter, ITC_NO_TMO, ITC_FROM_ALL);
	TPT_REC_SIG(reply->msgno, reply->msgno == EQMH_DESTROY_CFM ?
	                              "EQMH_DESTROY_CFM" : "EQMH_DESTROY_REJ");
	itc_free(&reply);
	pthread_join(eqm->thread, NULL);
}

static void create_instance(union itc_msg *msg,
                            uint32_t client_ref,
                            struct connection_data *conn_data,
                            itc_mbox_id_t server_mbox)
{
	struct eqmh_create_req *req = &msg->eqmh_create_req;
	struct instance *instance = NULL;
	struct eqm_args args;
	uint32_t filter[3] = { 2, EQM_INSTANCE_CREATED_SUCCESS, EQM_INSTANCE_CREATED_FAIL };
	union itc_msg *reply;
	int status = 1;

	if (!conn_data)
		goto exit;

	instance = calloc(1, sizeof(struct instance));
	if (!instance)
		goto exit;

	SLIST_INIT(&instance->eqms);

	for (int i = 0; i < req->num_of_eqm_ids; i++) {
		if (id_already_in_use(req->eqm_id[i]))
			goto exit;

		if (!validate_id(req->eqm_id[i]))
			goto exit;

		struct eqm *eqm = calloc(1, sizeof(struct eqm));
		if (!eqm)
			goto exit;

		args.id = req->eqm_id[i];
		args.server_mbox = server_mbox;
		if (pthread_create(&eqm->thread, NULL, xenon_dp_thrfxn, (void *) &args)) {
			free(eqm);
			goto exit;
		}

		eqm->pthread_started = true;

		union itc_msg *sync = itc_receive(filter, ITC_NO_TMO, ITC_FROM_ALL);
		if (sync->msgno == EQM_INSTANCE_CREATED_FAIL) {
			TPT_REC_SIG(sync->msgno, "EQM_INSTANCE_CREATED_FAIL");
			pthread_join(eqm->thread, NULL);
			free(eqm);
			itc_free(&sync);
			goto exit;
		}

		TPT_REC_SIG(sync->msgno, "EQM_INSTANCE_CREATED_SUCCESS");
		eqm->mbox = itc_sender(sync);
		eqm->instance = instance;
		eqm->id = req->eqm_id[i];
		itc_free(&sync);

		SLIST_INSERT_HEAD(&instance->eqms, eqm, e);
	}

	instance->client_ref = client_ref;
	instance->conn_data = conn_data;

	status = 0;

exit:
	if (status == 0) {
		SLIST_INSERT_HEAD(&conn_data->instances, instance, e);
		reply = itc_alloc(sizeof(struct eqmh_create_cfm), EQMH_CREATE_CFM);
		reply->eqmh_create_cfm.instance = (uint32_t) instance;
		TPT_SEND_SIG(reply->msgno, itc_sender(msg), "EQMH_CREATE_CFM");
	}
	else {
		if (instance) {
			struct eqm *eqm_next;
			struct eqm *eqm_free = SLIST_FIRST(&instance->eqms);

			while (eqm_free) {
				if (eqm_free->pthread_started)
					destroy_thread(eqm_free);
				eqm_next = SLIST_NEXT(eqm_free, e);
				SLIST_REMOVE(&instance->eqms, eqm_free, eqm, e);
				free(eqm_free);
				eqm_free = eqm_next;
			}
			free(instance);
		}
		reply = itc_alloc(sizeof(struct eqmh_create_rej), EQMH_CREATE_REJ);
		TPT_SEND_SIG(reply->msgno, itc_sender(msg), "EQMH_CREATE_REJ");
	}

	reply->any_msg.connection_ref = client_ref;
	reply->any_msg.procedure_ref = msg->any_msg.procedure_ref;
	itc_send(&reply, itc_sender(msg), ITC_MY_MBOX);
}

static void forward_message(union itc_msg *msg,
                            struct conn_client_info *client_info)
{
	struct eqmh_forward_req *req = &msg->eqmh_forward_req;
	size_t size = itc_size(msg);
	struct eqm *eqm;

	SLIST_FOREACH(eqm, &req->instance->eqms, e) {
		union itc_msg *fwdmsg = itc_alloc(size, msg->msgno);
		memcpy(fwdmsg, msg, size);
		fwdmsg->any_msg.connection_ref = (uint32_t) eqm;
		eqm->state = EQM_STATE_BUSY;
		TPT_SEND_SIG(fwdmsg->msgno, eqm->mbox, "Forwarding to dp thread");
		itc_send(&fwdmsg, eqm->mbox, itc_sender(msg));
	}
}

static void forward_config(union itc_msg *msg,
                           struct conn_client_info *client_info)
{
	struct eqmh_config_req *req = &msg->eqmh_config_req;
	eqmhi_status_t status = EQMHI_STATUS_SUCCESS;

	if (req->instance->configuring)
		status = EQMHI_STATUS_WRONG_STATE;

	if (status == EQMHI_STATUS_SUCCESS) {
		for (int i = 0; i < req->num_of_entities; i++) {
			struct eqm * eqm = get_eqm_from_id(req->entities[i].eqm_id,
					req->instance);

			if (!eqm)
				status = EQMHI_STATUS_INVALID_PARAM;
			else if (eqm->state != EQM_STATE_DONE &&
					eqm->state != EQM_STATE_CONFIG)
				status = EQMHI_STATUS_WRONG_STATE;

			if (status != EQMHI_STATUS_SUCCESS)
				break;

			eqm->configs_left = 0;
		}
	}

	if (status == EQMHI_STATUS_SUCCESS) {
		for (int i = 0; i < req->num_of_entities; i++) {
			size_t size;
			union itc_msg *fwdmsg;
			struct eqm * eqm = get_eqm_from_id(req->entities[i].eqm_id,
		                                       req->instance);

			if (!eqm) {
				TPT_ERROR("eqm is null");
				status = EQMHI_STATUS_OTHER;
				break;
			}

			if (msg->msgno == EQMH_SET_CFG_REQ) {
				size = sizeof(struct eqmh_set_cfg_req);
				if (req->entities[i].cfg.type == EQMHI_CFG_RU_DP)
					size += req->entities[i].cfg.dp.size;
			}
			else {
				size = sizeof(struct eqmh_get_cfg_req);
			}

			fwdmsg = itc_alloc(size, msg->msgno);

			memcpy(fwdmsg, msg, size);
			memcpy(&fwdmsg->eqmh_config_req.entities[0],
			       &req->entities[i], sizeof(struct eqmhi_cfg_entity));
			fwdmsg->eqmh_config_req.num_of_entities = 1;

			if (msg->msgno == EQMH_SET_CFG_REQ &&
			    req->entities[i].cfg.type == EQMHI_CFG_RU_DP)
			{
			    void * dp_data = ((char *) fwdmsg) +
					sizeof(struct eqmh_set_cfg_req);

				/*
				 * Sending a pointer as the eqmh threads are in the
				 * same process.
				 */
				EQMH_COPY_DP_DATA_FROM_INDEX(req->entities[i],
				    fwdmsg->eqmh_set_cfg_req.entities[0],
				    dp_data, msg);
			}

			fwdmsg->any_msg.connection_ref = (uint32_t) eqm;
			fwdmsg->eqmh_config_req.user_data = (void *)
			    msg->eqmh_config_req.user_data;
			TPT_SEND_SIG(fwdmsg->msgno, eqm->mbox, "Forwarding cfg to dp thread");
			itc_send(&fwdmsg, eqm->mbox, itc_sender(msg));
			eqm->state = EQM_STATE_CONFIG;
			eqm->configs_left++;
		}

		req->instance->configuring = true;
		req->instance->cfg_req = *req;
	}

	if (status != EQMHI_STATUS_SUCCESS) {
		union itc_msg *reply;
		reply = itc_alloc(sizeof(struct eqmh_config_rej),
		                  MSGNO_REJ(msg->msgno));

		reply->eqmh_config_rej.status = status;
		reply->eqmh_config_rej.user_data = (void *) req->user_data;
		reply->any_msg.connection_ref = client_info->client_ref;
		reply->any_msg.procedure_ref = msg->any_msg.procedure_ref;

		TPT_SEND_SIG(reply->msgno, itc_sender(msg), "Rejecting cfg");
		itc_send(&reply, itc_sender(msg), ITC_MY_MBOX);
	}
}

static void forward_with_check(union itc_msg *msg,
                               struct conn_client_info *client_info)
{
	if (!check_state(&msg->eqmh_forward_req)) {
		reject(msg, client_info, EQMHI_STATUS_WRONG_STATE);
	}
	else {
		forward_message(msg, client_info);
	}
}

static void destroy_instance(struct instance *instance)
{
	struct connection_data *conn_data = instance->conn_data;
	struct eqm *eqm_free = SLIST_FIRST(&instance->eqms);
	struct eqm *eqm_next;

	while (eqm_free) {
		eqm_next = SLIST_NEXT(eqm_free, e);
		SLIST_REMOVE(&instance->eqms, eqm_free, eqm, e);
		free(eqm_free);
		eqm_free = eqm_next;
	}

	SLIST_REMOVE(&conn_data->instances, instance, instance, e);
	free(instance);
}

static void instance_complete(union itc_msg *msg, struct instance *instance)
{
	eqmhi_status_t status = EQMHI_STATUS_SUCCESS;
	union itc_msg *reply;
	struct eqm *eqm;

	SLIST_FOREACH(eqm, &instance->eqms, e) {
		if (eqm->state == EQM_STATE_BUSY)
			return;

		if (eqm->state == EQM_STATE_ERROR)
			status = eqm->status;
	}

	if (MSGNO_OP(msg->msgno) == MSGNO_OP(EQMH_DESTROY_CFM)) {
		destroy_instance(instance);
	}
	else {
		SLIST_FOREACH(eqm, &instance->eqms, e)
			eqm->state = EQM_STATE_DONE;
	}

	if (status == EQMHI_STATUS_SUCCESS) {
		reply = itc_alloc(sizeof(struct eqmh_generic_cfm), msg->msgno);
		TPT_SEND_SIG(reply->msgno, itc_sender(msg), "Instance complete CFM");
	}
	else {
		uint32_t msgno = MSGNO_REJ(msg->msgno);
		reply = itc_alloc(sizeof(struct eqmh_generic_rej), msgno);
		reply->eqmh_generic_rej.status = status;
		TPT_SEND_SIG(reply->msgno, itc_sender(msg), "Instance complete REJ");
	}

	reply->any_msg.connection_ref = instance->client_ref;
	reply->any_msg.procedure_ref = msg->any_msg.procedure_ref;

	itc_send(&reply, itc_sender(msg), ITC_MY_MBOX);
}

static void got_confirm(union itc_msg *msg)
{
	struct eqm *eqm = (struct eqm *) msg->any_msg.connection_ref;

	assert(eqm);
	assert(eqm->instance);

	if (msg->msgno == EQMH_DESTROY_CFM) {
		void *retval;
		pthread_join(eqm->thread, &retval);
		TPT_INFO(STR("eqm thread quit with status %d", (int) retval));
	}

	eqm->state = EQM_STATE_DONE;

	instance_complete(msg, eqm->instance);
}

static void got_reject(union itc_msg *msg)
{
	struct eqm *eqm = (struct eqm *) msg->any_msg.connection_ref;

	assert(eqm);
	assert(eqm->instance);

	if (msg->msgno == EQMH_DESTROY_REJ) {
		void *retval;
		pthread_join(eqm->thread, &retval);
		TPT_INFO(STR("eqm thread quit with status %d", (int) retval));
	}

	eqm->state = EQM_STATE_ERROR;
	eqm->status = msg->eqmh_generic_rej.status;

	instance_complete(msg, eqm->instance);
}

static union itc_msg *config_success(uint32_t msgno, struct instance *instance)
{
	struct eqmh_config_req *req = &instance->cfg_req;
	union itc_msg *reply;
	size_t size;

	if (msgno == EQMH_SET_CFG_CFM) {
		size = sizeof(struct eqmh_set_cfg_cfm);
	}
	else {
		size = sizeof(struct eqmh_get_cfg_cfm);

		for (int i = 0; i < req->num_of_entities; i++) {
			if (req->entities[i].cfg.type == EQMHI_CFG_RU_DP)
				size += req->entities[i].cfg.dp.size;
		}
	}

	reply = itc_alloc(size, msgno);

	if (msgno == EQMH_GET_CFG_CFM) {
		struct eqmh_get_cfg_cfm *cfm = &reply->eqmh_get_cfg_cfm;
		char *dp_data = ((char *) reply) + sizeof(struct eqmh_get_cfg_cfm);

		memcpy(cfm->entities, req->entities, sizeof(instance->cfg_req.entities));
		cfm->num_of_entities = req->num_of_entities;

		for (int i = 0; i < cfm->num_of_entities; i++) {
			if (cfm->entities[i].cfg.type == EQMHI_CFG_RU_DP) {
				EQMH_COPY_DP_DATA_SET_INDEX(req->entities[i], cfm->entities[i],
				                            dp_data, reply);
				free(req->entities[i].cfg.dp.buf);
				dp_data += req->entities[i].cfg.dp.size;
			}
		}
	}

	return reply;
}

static void config_complete(union itc_msg *msg, struct instance *instance)
{
	eqmhi_status_t status = EQMHI_STATUS_SUCCESS;
	struct eqmh_config_cfm *cfm = &msg->eqmh_config_cfm;
	union itc_msg *reply;
	struct eqm *eqm;

	SLIST_FOREACH(eqm, &instance->eqms, e) {
		if (eqm->state == EQM_STATE_CONFIG)
			return;

		if (eqm->state == EQM_STATE_ERROR)
			status = eqm->status;

		eqm->state = EQM_STATE_DONE;
	}

	if (status != EQMHI_STATUS_SUCCESS) {
		uint32_t msgno = MSGNO_REJ(msg->msgno);
		reply = itc_alloc(sizeof(struct eqmh_config_rej), msgno);
		reply->eqmh_config_rej.user_data = (void *)
		    msg->eqmh_config_req.user_data;
		TPT_SEND_SIG(reply->msgno, itc_sender(msg), "Config complete REJ");
	}
	else {
		reply = config_success(msg->msgno, instance);
		TPT_SEND_SIG(reply->msgno, itc_sender(msg), "Config complete CFM");
	}

	reply->eqmh_config_cfm.user_data = cfm->user_data;
	reply->any_msg.connection_ref = instance->client_ref;
	reply->any_msg.procedure_ref = msg->any_msg.procedure_ref;

	itc_send(&reply, itc_sender(msg), ITC_MY_MBOX);
	instance->configuring = false;
}

static void save_config(struct instance *instance,
                        struct eqmh_get_cfg_cfm *cfm)
{
	struct eqmh_config_req *req = &instance->cfg_req;
	int j;

	for (j = 0; j < req->num_of_entities; j++) {
		if (req->entities[j].eqm_id == cfm->entities[0].eqm_id &&
				req->entities[j].cfg.type == cfm->entities[0].cfg.type)
		{
			memcpy(&req->entities[j], &cfm->entities[0],
			       sizeof(struct eqmhi_cfg_entity));
			if (req->entities[j].cfg.type == EQMHI_CFG_RU_DP) {
				req->entities[j].cfg.dp.buf =
				    malloc(cfm->entities[0].cfg.dp.size);
				assert(req->entities[j].cfg.dp.buf); /* TODO Need proper check */
				memcpy(req->entities[j].cfg.dp.buf,
				       cfm->entities[0].cfg.dp.buf,
				       cfm->entities[0].cfg.dp.size);
			}
			break;
		}
	}

	/* Internal error, assert is ok */
	assert(j != req->num_of_entities);
}

static void got_config_confirm(union itc_msg *msg)
{
	struct eqm *eqm = (struct eqm *) msg->any_msg.connection_ref;

	assert(eqm);
	assert(eqm->instance);

	if (--eqm->configs_left == 0 && eqm->state != EQM_STATE_ERROR)
		eqm->state = EQM_STATE_DONE;

	if (msg->msgno == EQMH_GET_CFG_CFM)
		save_config(eqm->instance, &msg->eqmh_get_cfg_cfm);

	config_complete(msg, eqm->instance);
}

static void got_config_reject(union itc_msg *msg)
{
	struct eqm *eqm = (struct eqm *) msg->any_msg.connection_ref;
	union itc_msg *reply;

	assert(eqm);
	assert(eqm->instance);

	reply = itc_alloc(sizeof(struct eqmh_config_rej), msg->msgno);

	reply->eqmh_config_rej.user_data = msg->eqmh_config_rej.user_data;
	reply->eqmh_config_rej.status = msg->eqmh_config_rej.status;

	reply->any_msg.connection_ref = eqm->instance->client_ref;
	reply->any_msg.procedure_ref = msg->any_msg.procedure_ref;

	TPT_SEND_SIG(reply->msgno, itc_sender(msg), "Config REJ");
	itc_send(&reply, itc_sender(msg), ITC_MY_MBOX);

	eqm->state = EQM_STATE_ERROR;
	eqm->status = msg->eqmh_generic_rej.status;
	eqm->state = EQM_STATE_DONE;
}

uint32_t conn_connecting_cb(struct conn_client_info *client_info)
{
	struct connection_data *conn_data;

	conn_data = calloc(1, sizeof(struct connection_data));
	if (!conn_data)
		return 0xfeeb;

	if (conn_set_client_data(client_info->server_handle,
	                         client_info->server_ref,
	                         conn_data)) {
		free(conn_data);
		return 0xbeef;
	}

	SLIST_INIT(&conn_data->instances);
	SLIST_INSERT_HEAD(&connections, conn_data, e);

	return 0;
}

void conn_disconnecting_cb(struct conn_client_info *client_info)
{
	struct connection_data *conn_data = client_info->client_data;
	uint32_t filter[3] =
	    { 2, EQMH_DESTROY_CFM, EQMH_DESTROY_REJ };
	struct instance *inst = SLIST_FIRST(&conn_data->instances);
	struct instance *next;
	struct eqm *eqm;

	while (inst) {
		SLIST_FOREACH(eqm, &inst->eqms, e) {
			void *retval;
			union itc_msg *reply;
			union itc_msg *msg =
			    itc_alloc(sizeof(struct eqmh_destroy_req), EQMH_DESTROY_REQ);

			TPT_SEND_SIG(msg->msgno, eqm->mbox, "EQMH_DESTROY_REQ");
			itc_send(&msg, eqm->mbox, ITC_MY_MBOX);
			reply = itc_receive(filter, ITC_NO_TMO, ITC_FROM_ALL);
			TPT_REC_SIG(reply->msgno, reply->msgno == EQMH_DESTROY_CFM ?
	                   "EQMH_DESTROY_CFM" : "EQMH_DESTROY_REJ");

			itc_free(&reply);
			pthread_join(eqm->thread, &retval);
			TPT_INFO(STR("eqm thread quit with status %d", (int) retval));
		}

		next = SLIST_NEXT(inst, e);
		destroy_instance(inst);
		inst = next;
	}

	SLIST_REMOVE(&connections, conn_data, connection_data, e);
	free(conn_data);
}

static void handle_messages(itc_mbox_id_t mbox,
                            conn_server_handle_t conn_handle)
{
	union itc_msg *msg;
	struct conn_client_info client_info;

	while (1) {
		struct connection_data *conn_data;

		msg = itc_receive(ITC_NOFILTER, ITC_NO_TMO, ITC_FROM_ALL);

		/*
		 * Handle internal messages from eqm threads before parsing
		 * from external clients.
		 */
		switch (msg->msgno) {
			case EQMH_SET_CFG_CFM:
			case EQMH_GET_CFG_CFM:
				TPT_REC_SIG(msg->msgno, msg->msgno == EQMH_SET_CFG_CFM ?
				            "EQMH_SET_CFG_CFM" : "EQMH_GET_CFG_CFM");
				got_config_confirm(msg);
				itc_free(&msg);
				continue;
			case EQMH_LOAD_CFM:
			case EQMH_BOOT_CFM:
			case EQMH_DUMP_CFM:
			case EQMH_STOP_CFM:
			case EQMH_DESTROY_CFM:
				TPT_REC_SIG(msg->msgno, "CFM from dp thread");
				got_confirm(msg);
				itc_free(&msg);
				continue;
			case EQMH_SET_CFG_REJ:
			case EQMH_GET_CFG_REJ:
				TPT_REC_SIG(msg->msgno, msg->msgno == EQMH_SET_CFG_REJ ?
				            "EQMH_SET_CFG_REJ" : "EQMH_GET_CFG_REJ");
				got_config_reject(msg);
				itc_free(&msg);
				continue;
			case EQMH_LOAD_REJ:
			case EQMH_BOOT_REJ:
			case EQMH_DUMP_REJ:
			case EQMH_STOP_REJ:
			case EQMH_DESTROY_REJ:
				TPT_REC_SIG(msg->msgno, "REJ from dp thread");
				got_reject(msg);
				itc_free(&msg);
				continue;
			default:
				break;
		}

		/* Handle conn establish messages and messages from unknown clients. */
		if(!conn_check_client(conn_handle, &msg, &client_info))
			continue;

		conn_data = client_info.client_data;

		if (!conn_data) {
			TPT_ERROR("internal error, no conn_data!");
			itc_free(&msg);
			continue;
		}

		switch (msg->msgno) {
			case EQMH_CREATE_REQ:
				TPT_REC_SIG(msg->msgno, "EQMH_CREATE_REQ");
				create_instance(msg, client_info.client_ref, conn_data, mbox);
				break;
			case EQMH_SET_CFG_REQ:
			case EQMH_GET_CFG_REQ:
				TPT_REC_SIG(msg->msgno, msg->msgno == EQMH_SET_CFG_REQ ?
				            "EQMH_SET_CFG_REQ" : "EQMH_GET_CFG_REQ");
				forward_config(msg, &client_info);
				break;
			case EQMH_LOAD_REQ:
			case EQMH_BOOT_REQ:
			case EQMH_DUMP_REQ:
			case EQMH_STOP_REQ:
				TPT_REC_SIG(msg->msgno, "REQ from interface lib");
				forward_with_check(msg, &client_info);
				break;
			case EQMH_DESTROY_REQ:
				TPT_REC_SIG(msg->msgno, "EQMH_DESTROY_REQ");
				forward_message(msg, &client_info);
				break;
			default:
				TPT_INFO(STR("Received unsupported message %#x", msg->msgno));
				break;
		}

		itc_free(&msg);
	}
}

static int init_conn_establish(conn_server_handle_t *conn_handle)
{
	uint32_t supported_versions[] = { EQMH_SERVER_VERSIONS };
	struct conn_establish_msg_numbers eqmh_conn_establish_msg_numbers = {
		EQMH_CONN_ESTABLISH_REQ,
		EQMH_CONN_ESTABLISH_CFM,
		EQMH_CONN_ESTABLISH_REJ,
		EQMH_CONN_DISCONNECT_REQ,
		EQMH_CONN_DISCONNECT_CFM,
		EQMH_CONN_DISCONNECT_REJ,
		EQMH_CONN_MONITOR_FWD,
	};
	struct conn_event_callbacks conn_callbacks = {
		conn_connecting_cb,
		conn_disconnecting_cb,
		conn_disconnecting_cb,
		NULL,
	};

	return conn_establish_server_init(conn_handle,
	                                  sizeof(supported_versions) /
	                                      sizeof(supported_versions[0]),
	                                  supported_versions,
	                                  &eqmh_conn_establish_msg_numbers, 0,
	                                  &conn_callbacks);
}

int main()
{
	itc_mbox_id_t mbox;
	conn_server_handle_t conn_handle = NULL;

	if (itc_init(MAILBOX_SIZE, ITC_MALLOC, NULL, ITC_NO_NAMESPACE, 0)) {
		TPT_ERROR("Unable to initialize ITC!");
		exit(EXIT_FAILURE);
	}

	if (init_conn_establish(&conn_handle) != CONN_INIT_OK) {
		TPT_ERROR("Failed to initialize conn establish");
		exit(EXIT_FAILURE);
	}

	mbox = itc_create_mailbox(EQMH_MAILBOX, 0);
	if (mbox == ITC_NO_ID) {
		TPT_ERROR("Unable to create ITC mailbox!");
		exit(EXIT_FAILURE);
	}

	SLIST_INIT(&connections);

	handle_messages(mbox, conn_handle);

	exit(EXIT_SUCCESS);
}
