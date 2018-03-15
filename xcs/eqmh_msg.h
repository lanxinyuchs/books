#ifndef _EQMH_MSG_H
#define _EQMH_MSG_H

#include <conn-establish.h>

#include "eqmhi_api.h"
#include "eqmhi_ru.h"

#define EQMH_MAILBOX               "EQMH"

#define EQMH_SERVER_VERSIONS       1

#define EQMH_MAX_NO_ENTITIES       100

#define EQMH_MSG_BASE              0xa700

#define EQMH_REQ                   (EQMH_MSG_BASE + 0x10)
#define EQMH_CFM                   (EQMH_MSG_BASE + 0x20)
#define EQMH_REJ                   (EQMH_MSG_BASE + 0x40)
#define EQMH_CONN                  (EQMH_MSG_BASE + 0x50)

#define EQMH_CREATE_REQ            (EQMH_REQ + 0x1)
#define EQMH_SET_CFG_REQ           (EQMH_REQ + 0x2)
#define EQMH_GET_CFG_REQ           (EQMH_REQ + 0x3)
#define EQMH_LOAD_REQ              (EQMH_REQ + 0x4)
#define EQMH_BOOT_REQ              (EQMH_REQ + 0x5)
#define EQMH_DUMP_REQ              (EQMH_REQ + 0x6)
#define EQMH_STOP_REQ              (EQMH_REQ + 0x7)
#define EQMH_DESTROY_REQ           (EQMH_REQ + 0x8)

#define EQMH_CREATE_CFM            (EQMH_CFM + 0x1)
#define EQMH_SET_CFG_CFM           (EQMH_CFM + 0x2)
#define EQMH_GET_CFG_CFM           (EQMH_CFM + 0x3)
#define EQMH_LOAD_CFM              (EQMH_CFM + 0x4)
#define EQMH_BOOT_CFM              (EQMH_CFM + 0x5)
#define EQMH_DUMP_CFM              (EQMH_CFM + 0x6)
#define EQMH_STOP_CFM              (EQMH_CFM + 0x7)
#define EQMH_DESTROY_CFM           (EQMH_CFM + 0x8)

#define EQMH_CREATE_REJ            (EQMH_REJ + 0x1)
#define EQMH_SET_CFG_REJ           (EQMH_REJ + 0x2)
#define EQMH_GET_CFG_REJ           (EQMH_REJ + 0x3)
#define EQMH_LOAD_REJ              (EQMH_REJ + 0x4)
#define EQMH_BOOT_REJ              (EQMH_REJ + 0x5)
#define EQMH_DUMP_REJ              (EQMH_REJ + 0x6)
#define EQMH_STOP_REJ              (EQMH_REJ + 0x7)
#define EQMH_DESTROY_REJ           (EQMH_REJ + 0x8)

#define EQMH_CONN_ESTABLISH_REQ    (EQMH_CONN + 0x1)
#define EQMH_CONN_ESTABLISH_CFM    (EQMH_CONN + 0x2)
#define EQMH_CONN_ESTABLISH_REJ    (EQMH_CONN + 0x3)
#define EQMH_CONN_DISCONNECT_REQ   (EQMH_CONN + 0x4)
#define EQMH_CONN_DISCONNECT_CFM   (EQMH_CONN + 0x5)
#define EQMH_CONN_DISCONNECT_REJ   (EQMH_CONN + 0x6)
#define EQMH_CONN_MONITOR_FWD      (EQMH_CONN + 0x7)

#define EQMH_MSG_COMMON                     \
	uint32_t msgno;                         \
	uint32_t procedure_ref;                 \
	uint32_t connection_ref


struct eqmh_create_req {
	EQMH_MSG_COMMON;
	uint32_t num_of_eqm_ids;
	uint32_t eqm_id[0];
};

struct eqmh_create_cfm {
	EQMH_MSG_COMMON;
	uint32_t instance;
};

struct eqmh_create_rej {
	EQMH_MSG_COMMON;
	eqmhi_status_t status;
};

struct eqmh_set_cfg_req {
	EQMH_MSG_COMMON;
	uint32_t instance;
	const void *user_data;
	uint32_t num_of_entities;
	struct eqmhi_cfg_entity entities[EQMH_MAX_NO_ENTITIES];
};

struct eqmh_set_cfg_cfm {
	EQMH_MSG_COMMON;
	void *user_data;
};

struct eqmh_set_cfg_rej {
	EQMH_MSG_COMMON;
	void *user_data;
	eqmhi_status_t status;
};

struct eqmh_get_cfg_req {
	EQMH_MSG_COMMON;
	uint32_t instance;
	const void *user_data;
	uint32_t num_of_entities;
	struct eqmhi_cfg_entity entities[EQMH_MAX_NO_ENTITIES];
};

struct eqmh_get_cfg_cfm {
	EQMH_MSG_COMMON;
	void *user_data;
	uint32_t num_of_entities;
	struct eqmhi_cfg_entity entities[EQMH_MAX_NO_ENTITIES];
};

struct eqmh_get_cfg_rej {
	EQMH_MSG_COMMON;
	void *user_data;
	eqmhi_status_t status;
};

struct eqmh_load_req {
	EQMH_MSG_COMMON;
	uint32_t instance;
	uint32_t num_of_entities;
	struct eqmhi_load_entity entities[0];
};

struct eqmh_load_cfm {
	EQMH_MSG_COMMON;
};

struct eqmh_load_rej {
	EQMH_MSG_COMMON;
	eqmhi_status_t status;
};

struct eqmh_boot_req {
	EQMH_MSG_COMMON;
	uint32_t instance;
};

struct eqmh_boot_cfm {
	EQMH_MSG_COMMON;
};

struct eqmh_boot_rej {
	EQMH_MSG_COMMON;
	eqmhi_status_t status;
};

struct eqmh_dump_req {
	EQMH_MSG_COMMON;
	uint32_t instance;
};

struct eqmh_dump_cfm {
	EQMH_MSG_COMMON;
};

struct eqmh_dump_rej {
	EQMH_MSG_COMMON;
	eqmhi_status_t status;
};

struct eqmh_stop_req {
	EQMH_MSG_COMMON;
	uint32_t instance;
};

struct eqmh_stop_cfm {
	EQMH_MSG_COMMON;
};

struct eqmh_stop_rej {
	EQMH_MSG_COMMON;
	eqmhi_status_t status;
};

struct eqmh_destroy_req {
	EQMH_MSG_COMMON;
	uint32_t instance;
};

struct eqmh_destroy_cfm {
	EQMH_MSG_COMMON;
};

struct eqmh_destroy_rej {
	EQMH_MSG_COMMON;
	eqmhi_status_t status;
};


#define EQMH_MESSAGE_STRUCTS                                      \
	conn_any_msg_t any_msg;                                       \
	struct eqmh_create_req eqmh_create_req;     \
	struct eqmh_create_cfm eqmh_create_cfm;     \
	struct eqmh_create_rej eqmh_create_rej;     \
	struct eqmh_set_cfg_req eqmh_set_cfg_req;   \
	struct eqmh_set_cfg_cfm eqmh_set_cfg_cfm;   \
	struct eqmh_set_cfg_rej eqmh_set_cfg_rej;   \
	struct eqmh_get_cfg_req eqmh_get_cfg_req;   \
	struct eqmh_get_cfg_cfm eqmh_get_cfg_cfm;   \
	struct eqmh_get_cfg_rej eqmh_get_cfg_rej;   \
	struct eqmh_load_req eqmh_load_req;         \
	struct eqmh_load_cfm eqmh_load_cfm;         \
	struct eqmh_load_rej eqmh_load_rej;         \
	struct eqmh_boot_req eqmh_boot_req;         \
	struct eqmh_boot_cfm eqmh_boot_cfm;         \
	struct eqmh_boot_rej eqmh_boot_rej;         \
	struct eqmh_dump_req eqmh_dump_req;         \
	struct eqmh_dump_cfm eqmh_dump_cfm;         \
	struct eqmh_dump_rej eqmh_dump_rej;         \
	struct eqmh_stop_req eqmh_stop_req;         \
	struct eqmh_stop_cfm eqmh_stop_cfm;         \
	struct eqmh_stop_rej eqmh_stop_rej;         \
	struct eqmh_destroy_req eqmh_destroy_req;   \
	struct eqmh_destroy_cfm eqmh_destroy_cfm;   \
	struct eqmh_destroy_rej eqmh_destroy_rej

#define EQMH_FROM_INDEX(entity, start) \
	((void *) ((uint32_t) (entity).cfg.dp.buf + (uint32_t) (start)))

#define EQMH_COPY_DP_DATA_SET_INDEX(src_entity, dst_entity, dst, start)        \
do {                                                                          \
	memcpy((dst), (src_entity).cfg.dp.buf, (src_entity).cfg.dp.size);         \
	(dst_entity).cfg.dp.buf = (void *)                                        \
		((uint32_t) (dst) - (uint32_t) (start));                              \
	(dst_entity).cfg.dp.size = (src_entity).cfg.dp.size;                      \
} while (0)

#define EQMH_COPY_DP_DATA_FROM_INDEX(src_entity, dst_entity, dst, start)      \
do {                                                                          \
	memcpy((dst), EQMH_FROM_INDEX(src_entity, start),                         \
	       (src_entity).cfg.dp.size);                                         \
	(dst_entity).cfg.dp.buf = (dst);                                          \
	(dst_entity).cfg.dp.size = (src_entity).cfg.dp.size;                      \
} while (0)

#endif /* _EQMH_MSG_H */
