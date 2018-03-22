/*
 *   Copyright (C) 2015 by Ericsson AB. All rights reserved. The
 *   information in this document is the property of Ericsson. Except
 *   as specifically authorized in writing by Ericsson, the receiver
 *   of this document shall keep the information contained herein
 *   confidential and shall protect the same in whole or in part from
 *   disclosure and dissemination to third parties. Disclosure and
 *   disseminations to the receiver's employees shall only be made on
 *   a strict need to know basis.
 */
#include <stdint.h>
#include <inttypes.h>
#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/queue.h>
#include "nvpi3_srv.h"
#include "nvpi3_io.h"
#include "nvpi3_fdt.h"
#include "nvpi3_ascii_db.h"
#include "nvpi3_uenvimage.h"
#include "log_tpt.h"

#define RECURSIVE_LIMIT           32
#define MAX_EXCEPTION_SUFFIX_SIZE sizeof("!X")

struct db_dc_func {
	nvpi3_result_t (*get_db_header_size)(uint32_t *size);
	nvpi3_result_t (*get_db_size)(void *header, uint32_t *size);
	nvpi3_result_t (*create_db)(void *address, uint32_t size,
	                            int clear, uint32_t recursive_limit,
	                            void **h_db);
	nvpi3_result_t (*resize_db)(void *h_db, void *address, uint32_t size);
	nvpi3_result_t (*destroy_db)(void *h_db);
	/*
	 * close_option == 0: Do not close h_parent_node.
	 * close_option != 0: Close h_parent_node if operation succeeded.
	 */
	nvpi3_result_t (*open_node)(void *h_db, void *h_parent_node,
	                            uint32_t close_option, const char *name,
	                            uint32_t length, void **h_node);
	nvpi3_result_t (*close_node)(void *h_node);
	uint32_t       (*get_node_path_size)(void *h_node);
	nvpi3_result_t (*get_node_path)(void *h_node, char *path,
	                                uint32_t size);
	nvpi3_result_t (*get_value)(void *h_node, const char *key_name,
	                            nvpi3_key_type_t type,
	                            nvpi3_srv_get_value_callback callback,
	                            void *user_data);
	nvpi3_result_t (*get_value_size)(void *h_node, const char *key_name,
	                                 nvpi3_key_type_t type, uint32_t *size);
	nvpi3_result_t (*add_node)(void *h_node, const char *name,
	                           uint32_t length, void **h_subnode);
	nvpi3_result_t (*delete_node)(void *h_node);
	nvpi3_result_t (*set_value)(void *h_node, const char *key_name,
	                            nvpi3_key_type_t type, uint32_t size,
	                            const union nvpi3_key_value *value);
	nvpi3_result_t (*delete_key)(void *h_node, const char *key_name,
	                             nvpi3_key_type_t type);
	nvpi3_result_t (*enum_nodes)(
	        void *h_db, const char *node_name, int recursive,
	        nvpi3_result_t (*callback) (void *user_data,
	                                    void *h_node),
	        void *user_data);
	nvpi3_result_t (*enum_keys)(
	        void *h_node,
	        nvpi3_result_t (*callback)(void *user_data,
	                                   const char *key_name,
	                                   nvpi3_key_type_t type,
	                                   const char *type_str,
	                                   uint32_t size,
	                                   const union nvpi3_key_value *value),
	        void *user_data);
};

enum nvpi3_db_group_object_type {NVPI3_CREATED_DB_GROUP, NVPI3_OPENED_DB_GROUP};

struct nvpi3_created_db_object {
	void *buf;
	void *h_db;
	uint32_t size;
	struct nvpi3_db_definition definition;
};

struct nvpi3_created_db_group_object {
	uint32_t seq_num;
	char *name;
	uint32_t num_of_dbs;
	struct nvpi3_created_db_object db[1];
};

struct nvpi3_opened_db_group_object {
	struct nvpi3_created_db_group_object *group_handle;
};

struct nvpi3_db_group_object {
	LIST_ENTRY(nvpi3_db_group_object) list;
	void *client_ref;
	enum nvpi3_db_group_object_type type;
	union {
		struct nvpi3_created_db_group_object created;
		struct nvpi3_opened_db_group_object opened;
	} u;
};

struct nvpi3_node_object {
	LIST_ENTRY(nvpi3_node_object) list;
	nvpi3_db_group_handle group_handle;
	uint32_t seq_num;
	char *name;
	uint32_t num_of_dbs;
	struct {
		int found;
		void *h_node;
	} db[];
};

struct enum_nodes_callback_data {
	nvpi3_db_group_handle group_handle;
	uint32_t index;
	int all;
	nvpi3_srv_enum_nodes_callback callback;
	void *user_data;
};

struct enum_keys_callback_data {
	nvpi3_node_handle node_handle;
	uint32_t index;
	int all;
	nvpi3_srv_enum_keys_callback callback;
	void *user_data;
};

enum nvpi3_exception_type
{
	/* Indicates that node/key is removed. */
	NVPI3_EXCEPTION_DELETED,
	/* A standard node/key. */
	NVPI3_EXCEPTION_NONE,
	/*
	 * Indicates that node/key contains default value.
	 * The value should be used if node/key does not exist in any other
	 * database.
	 */
	NVPI3_EXCEPTION_DEFAULT,
	NVPI3_NUM_OF_EXCEPTIONS
};

struct delete_node_contents_data {
	STAILQ_ENTRY(delete_node_contents_data) slist;
	enum {NVPI3_NODE_ITEM, NVPI3_KEY_ITEM} item_type;
	union {
		struct {
			char name[1];
		} node;
		struct {
			nvpi3_key_type_t type;
			char name[1];
		} key;
	} u;
};

static const struct {
	char *str;
	uint32_t length;
} exception_suffix[NVPI3_NUM_OF_EXCEPTIONS] =
{
	/* NVPI3_EXCEPTION_DELETED */
	{"!R", sizeof("!R") - 1},
	/* NVPI3_EXCEPTION_NONE*/
	{"", sizeof("") - 1},
	/* NVPI3_EXCEPTION_DEFAULT*/
	{"!D", sizeof("!D") - 1}
};

const struct db_dc_func dc_func[NVPI3_DB_FORMAT_UENVIMAGE + 1] = {
	/* NVPI3_DB_FORMAT_FTD */
	{
		nvpi3_fdt_get_db_header_size,
		nvpi3_fdt_get_db_size,
		nvpi3_fdt_create_db,
		nvpi3_fdt_resize_db,
		nvpi3_fdt_destroy_db,
		nvpi3_fdt_open_node,
		nvpi3_fdt_close_node,
		nvpi3_fdt_get_node_path_size,
		nvpi3_fdt_get_node_path,
		nvpi3_fdt_get_value,
		nvpi3_fdt_get_value_size,
		nvpi3_fdt_add_node,
		nvpi3_fdt_delete_node,
		nvpi3_fdt_set_value,
		nvpi3_fdt_delete_key,
		nvpi3_fdt_enum_nodes,
		nvpi3_fdt_enum_keys
	},
	/* NVPI3_DB_FORMAT_ASCII_DB */
	{
		nvpi3_ascii_db_get_db_header_size,
		nvpi3_ascii_db_get_db_size,
		nvpi3_ascii_db_create_db,
		nvpi3_ascii_db_resize_db,
		nvpi3_ascii_db_destroy_db,
		nvpi3_ascii_db_open_node,
		nvpi3_ascii_db_close_node,
		nvpi3_ascii_db_get_node_path_size,
		nvpi3_ascii_db_get_node_path,
		nvpi3_ascii_db_get_value,
		nvpi3_ascii_db_get_value_size,
		nvpi3_ascii_db_add_node,
		nvpi3_ascii_db_delete_node,
		nvpi3_ascii_db_set_value,
		nvpi3_ascii_db_delete_key,
		nvpi3_ascii_db_enum_nodes,
		nvpi3_ascii_db_enum_keys
	},
	/* NVPI3_DB_FORMAT_UENVIMAGE */
	{
		nvpi3_uenvimage_get_db_header_size,
		nvpi3_uenvimage_get_db_size,
		nvpi3_uenvimage_create_db,
		nvpi3_uenvimage_resize_db,
		nvpi3_uenvimage_destroy_db,
		nvpi3_uenvimage_open_node,
		nvpi3_uenvimage_close_node,
		nvpi3_uenvimage_get_node_path_size,
		nvpi3_uenvimage_get_node_path,
		nvpi3_uenvimage_get_value,
		nvpi3_uenvimage_get_value_size,
		nvpi3_uenvimage_add_node,
		nvpi3_uenvimage_delete_node,
		nvpi3_uenvimage_set_value,
		nvpi3_uenvimage_delete_key,
		nvpi3_uenvimage_enum_nodes,
		nvpi3_uenvimage_enum_keys
	}
};

static LIST_HEAD(nvpi3_db_group_head, nvpi3_db_group_object) db_group_head =
	LIST_HEAD_INITIALIZER(db_group_head);

static LIST_HEAD(nvpi3_node_head, nvpi3_node_object) node_head =
	LIST_HEAD_INITIALIZER(node_head);

static struct nvpi3_created_db_group_object *get_created_db_group(
	nvpi3_db_group_handle group_handle);

static nvpi3_result_t create_db(struct nvpi3_created_db_object *db_object,
                                int *cleared);

static nvpi3_result_t destroy_db(struct nvpi3_created_db_object *db_object);

static nvpi3_result_t check_db_group(nvpi3_db_group_handle group_handle);

static nvpi3_result_t destroy_db_group(nvpi3_db_group_handle group_handle);

static const char *adjust_for_parent_node(const char *parent_node_name,
                                          const char *name);

static nvpi3_result_t check_node(nvpi3_node_handle node_handle);

static nvpi3_result_t srv_open_node(nvpi3_db_group_handle group_handle,
                                    const char *node_name, int all,
                                    nvpi3_node_handle *node_handle);

static nvpi3_result_t reopen_node(nvpi3_node_handle node_handle, int all);

static nvpi3_result_t open_subnode(nvpi3_node_handle node_handle,
                                   const char **key_name,
                                   nvpi3_node_handle *subnode_handle);

static nvpi3_result_t close_node(nvpi3_node_handle node_handle);

static nvpi3_result_t close_nodes(nvpi3_db_group_handle group_handle);

static nvpi3_result_t get_value(nvpi3_node_handle node_handle,
                                const char *key_name, nvpi3_key_type_t type,
                                nvpi3_srv_get_value_callback callback,
                                void *user_data);

static nvpi3_result_t get_value_size(nvpi3_node_handle node_handle,
                                     const char *key_name,
                                     nvpi3_key_type_t type, uint32_t *size,
                                     uint32_t *index);

static nvpi3_result_t delete_node(
	struct nvpi3_created_db_group_object *created_group,
	const char *node_name);

static nvpi3_result_t set_value(nvpi3_db_group_handle group_handle,
                                const char *key_name, nvpi3_key_type_t type,
                                uint32_t size,
                                const union nvpi3_key_value *value);

static nvpi3_result_t delete_key(
	struct nvpi3_created_db_group_object *created_group,
	const char *key_name, nvpi3_key_type_t type);

static nvpi3_result_t enum_nodes(nvpi3_db_group_handle group_handle,
                                 const char *node_name, int recursive, int all,
                                 nvpi3_srv_enum_nodes_callback callback,
                                 void *user_data);

static nvpi3_result_t enum_keys_callback(void *user_data, const char *key_name,
                                         nvpi3_key_type_t type,
                                         const char *type_str, uint32_t size,
                                         const union nvpi3_key_value *value);

nvpi3_result_t nvpi3_srv_create_db_group(
	void *client_ref,
	const char *name,
	uint32_t num_of_definitions,
	const struct nvpi3_db_definition definition[],
	int cleared[],
	nvpi3_db_group_handle *group_handle)
{
	uint32_t i, name_offset, size;
	nvpi3_result_t result = NVPI3_RESULT_SUCCESS;

	*group_handle = NULL;

	LIST_FOREACH(*group_handle, &db_group_head, list) {
		if ((*group_handle)->type == NVPI3_CREATED_DB_GROUP &&
		    !strcmp((*group_handle)->u.created.name, name)) {
			return NVPI3_RESULT_ACCESS_DENIED;
		}
	}

	name_offset = offsetof(struct nvpi3_db_group_object, u.created.db) +
		sizeof((*group_handle)->u.created.db[0]) * num_of_definitions;
	size = name_offset + strlen(name) + 1;
	*group_handle = malloc(size);
	if (*group_handle == NULL) {
		TPT_ERROR(STR("malloc of size %d failed for db group %s",
		              size, name));
		return NVPI3_RESULT_OTHER_ERROR;
	}

	memset(*group_handle, 0, size);
	(*group_handle)->client_ref = client_ref;
	(*group_handle)->type = NVPI3_CREATED_DB_GROUP;
	(*group_handle)->u.created.name = (char *) *group_handle + name_offset;
	strcpy((*group_handle)->u.created.name, name);
	(*group_handle)->u.created.num_of_dbs = num_of_definitions;

	for (i = 0; i < (*group_handle)->u.created.num_of_dbs; i++) {
		uint32_t length = strlen((*group_handle)->u.created.db[i].
		                          definition.parent_node);
		if (length &&
		    (*group_handle)->u.created.db[i].definition.
		     parent_node[length-1] != '/') {
			TPT_INFO(STR("Warning: Invalid parent node name %s in "
			             "db group %s",
			             (*group_handle)->u.created.db[i].
			             definition.parent_node, name));
			result = NVPI3_RESULT_INVALID_PARAM;
			goto nvpi3_srv_create_db_group_end;
		}

		memcpy(&(*group_handle)->u.created.db[i].definition,
		       &definition[i],
		       sizeof((*group_handle)->u.created.db[i].definition));
		result = create_db(&(*group_handle)->u.created.db[i],
		                   (cleared != NULL) ? &cleared[i] : NULL);
		if (result != NVPI3_RESULT_SUCCESS) {
			goto nvpi3_srv_create_db_group_end;
		}
	}

	LIST_INSERT_HEAD(&db_group_head, *group_handle, list);

nvpi3_srv_create_db_group_end:
	if (result != NVPI3_RESULT_SUCCESS) {
		(void) destroy_db_group(*group_handle);
		*group_handle = NULL;
	}
	return result;
}

nvpi3_result_t nvpi3_srv_destroy_db_group(nvpi3_db_group_handle group_handle)
{
	nvpi3_result_t result = check_db_group(group_handle);
	if (result != NVPI3_RESULT_SUCCESS) {
		goto nvpi3_srv_destroy_db_group_end;
	}

	if (group_handle->type != NVPI3_CREATED_DB_GROUP) {
		TPT_INFO(STR("Database defined by %p is opened and should be "
		             "closed instead of destroyed",
		             (void *)group_handle));
		result = NVPI3_RESULT_INVALID_PARAM;
		goto nvpi3_srv_destroy_db_group_end;
	}

	LIST_REMOVE(group_handle, list);
	result = destroy_db_group(group_handle);

nvpi3_srv_destroy_db_group_end:
	return result;
}

nvpi3_result_t nvpi3_srv_open_db_group(void *client_ref, const char *name,
                                       nvpi3_db_group_handle *group_handle)
{
	nvpi3_db_group_handle h_group;

	*group_handle = NULL;

	LIST_FOREACH(h_group, &db_group_head, list) {
		if (h_group->type != NVPI3_CREATED_DB_GROUP ||
		    strcmp(h_group->u.created.name, name)) {
			continue;
		}

		*group_handle =
		        malloc(offsetof(struct nvpi3_db_group_object, u) +
		               sizeof((*group_handle)->u.opened));
		if (*group_handle == NULL) {
			TPT_ERROR(STR("malloc of size %d failed for db "
			              "group %s",
			              offsetof(struct nvpi3_db_group_object,
			                       u) +
			              sizeof((*group_handle)->u.opened),
			              name));
			return NVPI3_RESULT_OTHER_ERROR;
		}

		(*group_handle)->client_ref = client_ref;
		(*group_handle)->type = NVPI3_OPENED_DB_GROUP;
		(*group_handle)->u.opened.group_handle = &h_group->u.created;
		LIST_INSERT_HEAD(&db_group_head, *group_handle, list);
		return NVPI3_RESULT_SUCCESS;
	}

	return NVPI3_RESULT_NOT_FOUND;
}

nvpi3_result_t nvpi3_srv_close_db_group(nvpi3_db_group_handle group_handle)
{
	nvpi3_result_t result = check_db_group(group_handle);
	if (result != NVPI3_RESULT_SUCCESS) {
		goto nvpi3_srv_close_db_group_end;
	}

	if (group_handle->type != NVPI3_OPENED_DB_GROUP) {
		TPT_INFO(STR("Database defined by %p is created and should be"
		             " destroyed instead of closed",
		             (void *)group_handle));
		result = NVPI3_RESULT_INVALID_PARAM;
		goto nvpi3_srv_close_db_group_end;
	}

	LIST_REMOVE(group_handle, list);
	result = close_nodes(group_handle);
	free(group_handle);

nvpi3_srv_close_db_group_end:
	return result;
}

nvpi3_result_t nvpi3_srv_destroy(void *client_ref)
{
	nvpi3_db_group_handle group_handle;
	nvpi3_result_t result2;
	nvpi3_result_t result = NVPI3_RESULT_SUCCESS;

	do {
		LIST_FOREACH(group_handle, &db_group_head, list) {
			if (group_handle->client_ref != client_ref) {
				continue;
			}

			LIST_REMOVE(group_handle, list);
			if (group_handle->type == NVPI3_CREATED_DB_GROUP) {
				result2 = destroy_db_group(group_handle);
			} else {
				result2 = close_nodes(group_handle);
				free(group_handle);
			}
			if (result == NVPI3_RESULT_SUCCESS) {
				result = result2;
			}
			break;
		}
	} while(group_handle != NULL);

	return result;
}

nvpi3_result_t nvpi3_srv_open_node(nvpi3_db_group_handle group_handle,
                                   const char *node_name,
                                   nvpi3_node_handle *node_handle)
{
	return srv_open_node(group_handle, node_name, 0, node_handle);
}

nvpi3_result_t nvpi3_srv_close_node(nvpi3_node_handle node_handle)
{
	nvpi3_result_t result = check_node(node_handle);
	if (result == NVPI3_RESULT_SUCCESS) {
		LIST_REMOVE(node_handle, list);
		result = close_node(node_handle);
		free(node_handle);
	}

	return result;
}

nvpi3_result_t nvpi3_srv_get_value(nvpi3_node_handle node_handle,
                                   const char *key_name, nvpi3_key_type_t type,
                                   nvpi3_srv_get_value_callback callback,
                                   void *user_data)
{
	const struct nvpi3_created_db_group_object *created_group;
	nvpi3_node_handle subnode_handle = NULL;
	nvpi3_result_t result = check_node(node_handle);
	if (result != NVPI3_RESULT_SUCCESS) {
		goto nvpi3_srv_get_value_end;
	}

	created_group = get_created_db_group(node_handle->group_handle);
	if (node_handle->seq_num != created_group->seq_num) {
		result = reopen_node(node_handle, 0);
		if (result != NVPI3_RESULT_SUCCESS) {
			goto nvpi3_srv_get_value_end;
		}

		node_handle->seq_num = created_group->seq_num;
	}

	result = open_subnode(node_handle, &key_name, &subnode_handle);
	if (result != NVPI3_RESULT_SUCCESS &&
	    result != NVPI3_RESULT_NOT_FOUND) {
		goto nvpi3_srv_get_value_end;
	}
	result = get_value(
		(subnode_handle != NULL) ? subnode_handle : node_handle,
		key_name, type, callback, user_data);

nvpi3_srv_get_value_end:
	if (subnode_handle != NULL) {
		nvpi3_result_t result2 = nvpi3_srv_close_node(subnode_handle);
		if (result == NVPI3_RESULT_SUCCESS) {
			result = result2;
		}
	}

	return result;
}

nvpi3_result_t nvpi3_srv_get_value_size(nvpi3_node_handle node_handle,
                                        const char *key_name,
                                        nvpi3_key_type_t type, uint32_t *size)
{
	const struct nvpi3_created_db_group_object *created_group;
	nvpi3_node_handle subnode_handle = NULL;
	nvpi3_result_t result = check_node(node_handle);
	if (result != NVPI3_RESULT_SUCCESS) {
		goto nvpi3_srv_get_value_size_end;
	}

	created_group = get_created_db_group(node_handle->group_handle);
	if (node_handle->seq_num != created_group->seq_num) {
		result = reopen_node(node_handle, 0);
		if (result != NVPI3_RESULT_SUCCESS) {
			goto nvpi3_srv_get_value_size_end;
		}
		node_handle->seq_num = created_group->seq_num;
	}

	result = open_subnode(node_handle, &key_name, &subnode_handle);
	if (result != NVPI3_RESULT_SUCCESS &&
	    result != NVPI3_RESULT_NOT_FOUND) {
		goto nvpi3_srv_get_value_size_end;
	}
	result = get_value_size(
		(subnode_handle != NULL) ? subnode_handle : node_handle,
		key_name, type, size, NULL);

nvpi3_srv_get_value_size_end:
	if (subnode_handle != NULL) {
		nvpi3_result_t result2 = nvpi3_srv_close_node(subnode_handle);
		if (result == NVPI3_RESULT_SUCCESS) {
			result = result2;
		}
	}

	return result;
}

nvpi3_result_t nvpi3_srv_delete_node(nvpi3_db_group_handle group_handle,
                                     const char *node_name)
{
	uint32_t length;
	struct nvpi3_created_db_group_object *created_group;
	nvpi3_result_t result = check_db_group(group_handle);
	if (result != NVPI3_RESULT_SUCCESS) {
		goto nvpi3_srv_delete_node_end;
	}

	length = strlen(node_name);
	if (!length || node_name[length-1] != '/') {
		TPT_INFO(STR("Warning: Invalid node name %s", node_name));
		result = NVPI3_RESULT_INVALID_PARAM;
		goto nvpi3_srv_delete_node_end;
	}

	created_group = get_created_db_group(group_handle);
	result = delete_node(created_group, node_name);
	if (result != NVPI3_RESULT_SUCCESS) {
		goto nvpi3_srv_delete_node_end;
	}

	created_group->seq_num++;

nvpi3_srv_delete_node_end:
	return result;
}

nvpi3_result_t nvpi3_srv_set_value(nvpi3_db_group_handle group_handle,
                                   const char *key_name, nvpi3_key_type_t type,
                                   uint32_t size,
                                   const union nvpi3_key_value *value)
{
	struct nvpi3_created_db_group_object *created_group;
	nvpi3_result_t result = check_db_group(group_handle);
	if (result != NVPI3_RESULT_SUCCESS) {
		goto nvpi3_srv_set_value_end;
	}

	result = set_value(group_handle, key_name, type, size, value);
	if (result != NVPI3_RESULT_SUCCESS) {
		goto nvpi3_srv_set_value_end;
	}

	created_group = get_created_db_group(group_handle);
	created_group->seq_num++;

nvpi3_srv_set_value_end:
	return result;
}

nvpi3_result_t nvpi3_srv_delete_key(nvpi3_db_group_handle group_handle,
                                    const char *key_name,
                                    nvpi3_key_type_t type)
{
	struct nvpi3_created_db_group_object *created_group;
	nvpi3_result_t result = check_db_group(group_handle);
	if (result != NVPI3_RESULT_SUCCESS) {
		goto nvpi3_srv_delete_key_end;
	}

	created_group = get_created_db_group(group_handle);
	result = delete_key(created_group, key_name, type);
	if (result != NVPI3_RESULT_SUCCESS) {
		goto nvpi3_srv_delete_key_end;
	}

	created_group->seq_num++;

nvpi3_srv_delete_key_end:
	return result;
}

nvpi3_result_t nvpi3_srv_enum_nodes(nvpi3_db_group_handle group_handle,
                                    const char *node_name, int all,
                                    nvpi3_srv_enum_nodes_callback callback,
                                    void *user_data)
{
	nvpi3_result_t result = check_db_group(group_handle);
	if (result != NVPI3_RESULT_SUCCESS) {
		goto nvpi3_srv_enum_nodes;
	}

	result = enum_nodes(group_handle, node_name, 1, all, callback,
	                     user_data);

nvpi3_srv_enum_nodes:
	return result;
}

nvpi3_result_t nvpi3_srv_enum_keys(nvpi3_node_handle node_handle, int all,
                                   nvpi3_srv_enum_keys_callback callback,
                                   void *user_data)
{
	uint32_t i;
	struct enum_keys_callback_data callback_data;
	const struct nvpi3_created_db_group_object *created_group;
	nvpi3_result_t result = check_node(node_handle);
	if (result != NVPI3_RESULT_SUCCESS) {
		goto nvpi3_srv_enum_keys_end;
	}

	created_group = get_created_db_group(node_handle->group_handle);
	callback_data.node_handle = node_handle;
	callback_data.all = all;
	callback_data.callback = callback;
	callback_data.user_data = user_data;

	if (node_handle->seq_num != created_group->seq_num) {
		result = reopen_node(node_handle, all);
		if (result != NVPI3_RESULT_SUCCESS) {
			goto nvpi3_srv_enum_keys_end;
		}

		node_handle->seq_num = created_group->seq_num;
	}

	for (i = 0; i < node_handle->num_of_dbs; i++) {
		nvpi3_db_format_t fmt;
		uint32_t length =
			strlen(created_group->db[i].definition.parent_node);

		if (!node_handle->db[i].found) {
			continue;
		} else if (length) {
			if (strlen(node_handle->name) < length) {
				continue;
			}
		}

		fmt = created_group->db[i].definition.format;
		callback_data.index = i;
		result = dc_func[fmt].enum_keys(node_handle->db[i].h_node,
		                                enum_keys_callback,
		                                &callback_data);
		if (result != NVPI3_RESULT_SUCCESS) {
			break;
		}
	}

nvpi3_srv_enum_keys_end:
	return result;
}

nvpi3_result_t nvpi3_srv_get_node_name(nvpi3_node_handle node_handle,
                                       const char **node_name)
{
	nvpi3_result_t result = check_node(node_handle);

	if (result == NVPI3_RESULT_SUCCESS) {
		*node_name = node_handle->name;
	}

	return result;
}
nvpi3_result_t nvpi3_srv_enum_db_groups(
	nvpi3_srv_enum_db_groups_callback callback, void *user_data)
{
	nvpi3_db_group_handle h_group;
	uint32_t num_of_groups = 0;
	nvpi3_result_t result = NVPI3_RESULT_SUCCESS;

	LIST_FOREACH(h_group, &db_group_head, list) {
		if (h_group->type != NVPI3_CREATED_DB_GROUP) {
			continue;
		}
		num_of_groups++;
	}

	if (!num_of_groups) {
		goto nvpi3_srv_enum_db_groups_end;
	}

	LIST_FOREACH(h_group, &db_group_head, list) {
		uint32_t i;
		struct nvpi3_db_definition *definition;

		if (h_group->type != NVPI3_CREATED_DB_GROUP) {
			continue;
		}

		definition = malloc(sizeof(*definition) *
		                    h_group->u.created.num_of_dbs);
		if (definition == NULL) {
			TPT_ERROR(STR("malloc of size %d failed",
			              sizeof(*definition) *
			              h_group->u.created.num_of_dbs));
			result = NVPI3_RESULT_OTHER_ERROR;
			goto nvpi3_srv_enum_db_groups_end;
		}

		for (i = 0; i < h_group->u.created.num_of_dbs; i++) {
			memcpy(&definition[i],
			       &h_group->u.created.db[i].definition,
			       sizeof(*definition));
		}

		result = callback(user_data, num_of_groups,
		                  h_group->u.created.name,
		                  h_group->u.created.num_of_dbs, definition);
		free(definition);
		if (result != NVPI3_RESULT_SUCCESS) {
			goto nvpi3_srv_enum_db_groups_end;
		}
	}

nvpi3_srv_enum_db_groups_end:
	return result;
}

static struct nvpi3_created_db_group_object *get_created_db_group(
	nvpi3_db_group_handle group_handle)
{
	if (group_handle->type == NVPI3_CREATED_DB_GROUP) {
		return &group_handle->u.created;
	} else {
		return group_handle->u.opened.group_handle;
	}
}

static const char *db_name(const union nvpi3_db_storage *storage)
{
	if (storage->type > NVPI3_DB_STORAGE_TYPE_MTD) {
		return "";
	}

	return (storage->type == NVPI3_DB_STORAGE_TYPE_FILE) ?
	       storage->file.name : storage->mtd.name;
}

static uint32_t db_offset(const union nvpi3_db_storage *storage)
{
	if (storage->type > NVPI3_DB_STORAGE_TYPE_MTD) {
		return 0;
	}

	return (storage->type == NVPI3_DB_STORAGE_TYPE_FILE) ?
	       storage->file.offset : storage->mtd.offset;
}

static uint32_t db_max_size(const union nvpi3_db_storage *storage)
{
	if (storage->type > NVPI3_DB_STORAGE_TYPE_MTD) {
		return 0;
	}

	return (storage->type == NVPI3_DB_STORAGE_TYPE_FILE) ?
	       storage->file.max_size : storage->mtd.max_size;
}

static nvpi3_result_t create_db(struct nvpi3_created_db_object *db_object,
                                int *cleared)
{
	uint32_t header_size;
	void *header = NULL;
	nvpi3_result_t result;
	nvpi3_db_format_t fmt = db_object->definition.format;
	int corrupt = 0;

	db_object->buf = NULL;
	db_object->h_db = NULL;

	if (db_object->definition.storage.type > NVPI3_DB_STORAGE_TYPE_MTD) {
		TPT_INFO(STR("Warning: Invalid storage type %d",
		             db_object->definition.storage.type));
		result = NVPI3_RESULT_INVALID_PARAM;
		goto create_db_end;
	} else if (fmt > NVPI3_DB_FORMAT_UENVIMAGE) {
		TPT_INFO(STR("Warning: Invalid format %d for db %s", fmt,
		             db_name(&db_object->definition.storage)));
		result = NVPI3_RESULT_INVALID_PARAM;
		goto create_db_end;
	} else if (db_object->definition.permission > NVPI3_DB_PERMISSION_WO) {
		TPT_INFO(STR("Warning: Invalid permission %d for db %s",
		             db_object->definition.permission,
		             db_name(&db_object->definition.storage)));
		result = NVPI3_RESULT_INVALID_PARAM;
		goto create_db_end;
	}

	result = dc_func[fmt].get_db_header_size(&header_size);
	if (result != NVPI3_RESULT_SUCCESS) {
		TPT_ERROR(STR("get_db_header_size failed (%d) for db %s",
		              result, db_name(&db_object->definition.storage)));
		goto create_db_end;
	}

	if (header_size) {
		header = malloc(header_size);
		if (header == NULL) {
			TPT_ERROR(STR("malloc of size %d failed for db %s",
			              header_size,
			              db_name(&db_object->definition.storage)));
			result = NVPI3_RESULT_OTHER_ERROR;
			goto create_db_end;
		}

		result = nvpi3_read(header, header_size,
		                    &db_object->definition.storage);
		if (result != NVPI3_RESULT_SUCCESS) {
			TPT_INFO(STR("nvpi3_read failed (%d) for db %s",
			             result,
			             db_name(&db_object->definition.storage)));
		} else {
			result = dc_func[fmt].get_db_size(header,
			                                  &db_object->size);
			if (result != NVPI3_RESULT_SUCCESS) {
				TPT_INFO(STR("get_db_size failed (%d) for db "
				             "%s", result,
				             db_name(&db_object->definition.
				                     storage)));
			}
		}
	}

	if (result != NVPI3_RESULT_SUCCESS &&
	    db_object->definition.permission ==  NVPI3_DB_PERMISSION_RO) {
			TPT_INFO(STR("Failed to read read-only db %s",
			             db_name(&db_object->definition.storage)));
			goto create_db_end;
	}

	if (!header_size || result != NVPI3_RESULT_SUCCESS) {
		result = nvpi3_get_storage_size(&db_object->definition.storage,
		                                &db_object->size);
		if (result != NVPI3_RESULT_SUCCESS) {
			TPT_INFO(STR("nvpi3_get_storage_size failed (%d) for "
			             "db %s", result,
			             db_name(&db_object->definition.storage)));
			goto create_db_end;
		}
	}

	if (!db_object->size) {
		TPT_INFO(STR("Unexpected zero size for db %s",
		             db_name(&db_object->definition.storage)));

		if (db_object->definition.permission ==
		    NVPI3_DB_PERMISSION_RO) {
			result = NVPI3_RESULT_OTHER_ERROR;
			goto create_db_end;
		}

		db_object->size = (db_object->definition.storage.type ==
			NVPI3_DB_STORAGE_TYPE_FILE) ?
			db_object->definition.storage.file.max_size :
			db_object->definition.storage.mtd.max_size;
		corrupt = 1;
	}

	db_object->buf = malloc(db_object->size);
	if (db_object->buf == NULL) {
		TPT_ERROR(STR("malloc of size %d failed for db %s",
		              db_object->size,
		              db_name(&db_object->definition.storage)));
		result = NVPI3_RESULT_OTHER_ERROR;
		goto create_db_end;
	}

	memset(db_object->buf, 0, db_object->size);

	if (!corrupt) {
		result = nvpi3_read(db_object->buf, db_object->size,
		                    &db_object->definition.storage);
		if (result != NVPI3_RESULT_SUCCESS) {
			TPT_ERROR(STR("nvpi3_read failed (%d) for db %s",
			              result,
			              db_name(&db_object->definition.storage)));
			goto create_db_end;
		}
	}

	result = dc_func[fmt].create_db(db_object->buf, db_object->size,
	                                corrupt, RECURSIVE_LIMIT,
	                                &db_object->h_db);
	if (result != NVPI3_RESULT_SUCCESS) {
		if (!corrupt &&
		    db_object->definition.permission !=
		    NVPI3_DB_PERMISSION_RO) {
			corrupt = 1;
			result = dc_func[fmt].create_db(db_object->buf,
			                                db_object->size,
			                                corrupt,
			                                RECURSIVE_LIMIT,
			                                &db_object->h_db);
		}

		if (result != NVPI3_RESULT_SUCCESS) {
			TPT_INFO(STR("create_db failed (%d) for db %s", result,
			             db_name(&db_object->definition.storage)));
			goto create_db_end;
		}

	}

	if (corrupt) {
		result = nvpi3_write(db_object->buf, db_object->size,
		                     &db_object->definition.storage);
	}

create_db_end:
	free(header);
	if (cleared != NULL) {
		*cleared = corrupt;
	}

	if (result != NVPI3_RESULT_SUCCESS) {
		(void) destroy_db(db_object);
		db_object->buf = NULL;
		db_object->h_db = NULL;
	}

	return result;
}

static nvpi3_result_t destroy_db(struct nvpi3_created_db_object *db_object)
{
	nvpi3_db_format_t fmt = db_object->definition.format;
	nvpi3_result_t result = NVPI3_RESULT_SUCCESS;

	if (fmt <= NVPI3_DB_FORMAT_UENVIMAGE && db_object->h_db != NULL) {
		result = dc_func[fmt].destroy_db(db_object->h_db);
		if (result != NVPI3_RESULT_SUCCESS) {
			TPT_ERROR(STR("destroy_db failed (%d) for db %s",
			              result,
			              db_name(&db_object->definition.storage)));
		}
	}

	free(db_object->buf);
	return result;
}

static nvpi3_result_t check_db_group(nvpi3_db_group_handle group_handle)
{
	nvpi3_db_group_handle h_group;

	LIST_FOREACH(h_group, &db_group_head, list) {
		if (h_group == group_handle) {
			return NVPI3_RESULT_SUCCESS;
		}
	}

	TPT_INFO(STR("Invalid db_group_handle %p", (void *)group_handle));
	return NVPI3_RESULT_NOT_FOUND;
}

static nvpi3_result_t destroy_db_group(nvpi3_db_group_handle group_handle)
{
	uint32_t i;
	nvpi3_result_t result, result2;
	nvpi3_db_group_handle h_group;

	result = close_nodes(group_handle);

	h_group = LIST_FIRST(&db_group_head);
	while (h_group != NULL) {
		nvpi3_db_group_handle h_next = LIST_NEXT(h_group, list);

		if (h_group->type == NVPI3_OPENED_DB_GROUP &&
		    h_group->u.opened.group_handle ==
		    &group_handle->u.created) {
			LIST_REMOVE(h_group, list);
			result2 = close_nodes(h_group);
			free(h_group);
			if (result == NVPI3_RESULT_SUCCESS) {
				result = result2;
			}
		}
		h_group = h_next;
	}

	for (i = 0; i < group_handle->u.created.num_of_dbs; i++) {
		result2 = destroy_db(&group_handle->u.created.db[i]);
		if (result == NVPI3_RESULT_SUCCESS) {
			result = result2;
		}
	}
	free(group_handle);
	return result;
}

static const char *adjust_for_parent_node(const char *parent_node_name,
                                          const char *name)
{
	uint32_t length1 = strlen(parent_node_name);

	if (length1) {
		uint32_t length2 = strlen(name);

		if (length2 > length1) {
			length2 = length1;
		}

		if (!strncmp(name, parent_node_name, length2)) {
			name += length2;
			if (length2 >= length1) {
				name--; /* Decrease to keep '/' */
			}
		} else {
			name = NULL;
		}

	}

	return name;
}

static nvpi3_result_t open_node_in_db(
	const struct nvpi3_created_db_object *db_object, int all,
	const char **node_name, enum nvpi3_exception_type *exception_type,
	void **h_node)
{
	uint32_t length;
	const char *src_start;
	char *name = NULL, *dest;
	nvpi3_db_format_t fmt = db_object->definition.format;
	nvpi3_result_t result = NVPI3_RESULT_NOT_FOUND;
	void *h_parent_node = NULL;

	*h_node = NULL; /* Initialized to make coverity happy. */
	length = strlen(*node_name);
	name = malloc(length + 1 + MAX_EXCEPTION_SUFFIX_SIZE);
	if (name == NULL) {
		TPT_ERROR(STR("malloc of size %d failed for node %s",
		              length + 1 +MAX_EXCEPTION_SUFFIX_SIZE,
		              *node_name));
		result = NVPI3_RESULT_OTHER_ERROR;
		goto open_node_in_db_end;
	}

	src_start = *node_name;
	dest = name;
	*exception_type = NVPI3_EXCEPTION_NONE;

	for (;;) {
		int found;
		uint32_t i;
		const char *src_end = strchr(src_start, '/');
							/* first '/' char. */

		if (src_end == NULL) {
			break;
		}

		length = src_end - src_start;

		if (!length) {
			/* Special handling of root node. */
			length = 1;
		}

		memcpy(dest, src_start, length);

		for (i = (!all) ? 0 : NVPI3_EXCEPTION_NONE, found = 0;
		     i < ((!all) ? NVPI3_NUM_OF_EXCEPTIONS :
		          NVPI3_EXCEPTION_NONE + 1);
		     i++) {
			uint32_t length2 = length;

			memcpy(dest + length, exception_suffix[i].str,
			       exception_suffix[i].length+1);

			if (dest[length +
			    exception_suffix[i].length - 1] != '/') {
				dest[length + exception_suffix[i].length] = '/';
				dest[length + exception_suffix[i].length + 1] =
					'\0';
				length2++;
			}

			result = dc_func[fmt].open_node(
				db_object->h_db, h_parent_node, 1, dest,
				length2 + exception_suffix[i].length,
				h_node);
			if (result == NVPI3_RESULT_SUCCESS) {
				if (*exception_type == NVPI3_EXCEPTION_NONE ||
				    i == NVPI3_EXCEPTION_DELETED) {
					*exception_type = i;
				}

				TPT_TRACE(1, STR("Sub node %s found in db %s",
				                 dest,
				                 db_name(&db_object->definition.
				                         storage)));
				found = 1;
				h_parent_node = *h_node;
				break;
			} else if (result != NVPI3_RESULT_NOT_FOUND) {
				TPT_ERROR(STR("open_node failed (%d) for sub "
				              "node %s in db %s", result, dest,
				              db_name(&db_object->definition.
				                      storage)));
				goto open_node_in_db_end;
			}
		}

		if (result != NVPI3_RESULT_SUCCESS && h_parent_node != NULL) {
			nvpi3_result_t result2 =
				dc_func[fmt].close_node(h_parent_node);

			if (result2 != NVPI3_RESULT_SUCCESS) {
				TPT_ERROR(STR("close_node failed (%d) for node "
				              "%s in db %s",result2, name,
				              db_name(&db_object->definition.
				                      storage)));
			}

		}
		if (!found) {
			result = NVPI3_RESULT_NOT_FOUND;
			break;
		}

		src_start = src_end;
		if (*src_end != '\0') {
			src_start++;
		}

		dest += length;

		if (*exception_type == NVPI3_EXCEPTION_DELETED) {
			break;
		}

		/* Add '/' to make name more readable in case of ERROR trace. */
		if (*(dest-1) != '/') {
			*dest = '/';
			dest++;
		}
	}

open_node_in_db_end:
	if (result == NVPI3_RESULT_SUCCESS) {
		*node_name = src_start;
	}
	free(name);
	return result;
}

/* A length of 0 is used to indicate that whole node_name should be printed. */
static nvpi3_result_t close_node_in_db(
	const struct nvpi3_created_db_object *db_object, const char *node_name,
	uint32_t length, void *h_node)
{
	nvpi3_result_t result;

	result = dc_func[db_object->definition.format].close_node(h_node);
	if (result != NVPI3_RESULT_SUCCESS) {
		TPT_ERROR(STR("close_node failed (%d) for node %*s in db %s",
		              result, (!length) ? strlen(node_name) : length,
		              node_name,
		              db_name(&db_object->definition.storage)));
	}

	return result;
}

static nvpi3_result_t check_node(nvpi3_node_handle node_handle)
{
	nvpi3_node_handle h_node;

	LIST_FOREACH(h_node, &node_head, list) {
		if (h_node == node_handle) {
			return NVPI3_RESULT_SUCCESS;
		}
	}

	TPT_TRACE(1, STR("Invalid node_handle %p", (void *)node_handle));
	return NVPI3_RESULT_NOT_FOUND;
}

static nvpi3_result_t open_node(nvpi3_node_handle node_handle, int all)
{
	uint32_t i, default_index, found_count;
	nvpi3_result_t result = NVPI3_RESULT_SUCCESS;
	const struct nvpi3_created_db_group_object *created_group =
		get_created_db_group(node_handle->group_handle);

	for (i = 0, found_count = 0, default_index = UINT32_MAX;
	     i < node_handle->num_of_dbs;
	     i++) {
		enum nvpi3_exception_type exception_type;
		const char *name;

		node_handle->db[i].found = 0;
		name = adjust_for_parent_node(
			created_group->db[i].definition.parent_node,
			node_handle->name);
		if (name == NULL) {
			/* Requested node does not match parent node. */
			TPT_TRACE(1, STR("%s is does not match parent node %s "
			                 "in db %s", node_handle->name,
			                 created_group->db[i].definition.
			                 parent_node,
			                 db_name(&created_group->db[i].
			                         definition.storage)));
			continue;
		} else if (*name == '\0') {
			/*
			 * Requested node is within parent node.
			 * Indicate that we found node but note that
			 * node_handle->db[i].found is zero which stops us from
			 * acessing keys within node.
			 */
			TPT_TRACE(1, STR("%s is within parent node %s in db %s",
			                 node_handle->name,
			                 created_group->db[i].definition.
			                 parent_node,
			                 db_name(&created_group->db[i].
			                         definition.storage)));
			found_count++;
			continue;
		}

		result = open_node_in_db(&created_group->db[i], all, &name,
		                         &exception_type,
		                         &node_handle->db[i].h_node);
		if (result == NVPI3_RESULT_SUCCESS) {
			if (!all) {
				if (exception_type == NVPI3_EXCEPTION_DELETED) {
					result =
						close_node_in_db(
							&created_group->db[i],
							node_handle->name, 0,
							node_handle->db[i].
							h_node);
					if (result != NVPI3_RESULT_SUCCESS) {
						break;
					}

					node_handle->db[i].h_node = NULL;

					if (!found_count) {
						break;
					}
					continue;
				} else if (exception_type ==
				           NVPI3_EXCEPTION_DEFAULT) {
					default_index = i;
				}
			}

			node_handle->db[i].found = 1;
			found_count++;
		} else if (result == NVPI3_RESULT_NOT_FOUND) {
			result = NVPI3_RESULT_SUCCESS;
		} else {
			break;
		}
	}

	if (result != NVPI3_RESULT_SUCCESS) {
		close_node(node_handle);
	} else if (!found_count) {
		result = NVPI3_RESULT_NOT_FOUND;
	} else if (default_index != UINT32_MAX && found_count > 1) {
		result = close_node_in_db(&created_group->db[default_index],
		                          node_handle->name, 0,
		                          node_handle->db[default_index].
		                          h_node);
		node_handle->db[default_index].found = 0;
		node_handle->db[default_index].h_node = NULL;
		found_count--;
	}
	return result;
}

static nvpi3_result_t srv_open_node(nvpi3_db_group_handle group_handle,
                                    const char *node_name, int all,
                                    nvpi3_node_handle *node_handle)
{
	uint32_t name_offset, length;
	nvpi3_result_t result;
	const struct nvpi3_created_db_group_object *created_group;

	*node_handle = NULL;

	result = check_db_group(group_handle);
	if (result != NVPI3_RESULT_SUCCESS) {
		goto srv_open_node_end;
	}

	length = strlen(node_name);
	if (!length || node_name[length-1] != '/') {
		TPT_INFO(STR("Warning: Invalid node name %s", node_name));
		result = NVPI3_RESULT_INVALID_PARAM;
		goto srv_open_node_end;
	}
	created_group = get_created_db_group(group_handle);
	name_offset = offsetof(struct nvpi3_node_object, db) +
		sizeof((*node_handle)->db[0]) * created_group->num_of_dbs;
	length += name_offset + 1;
	*node_handle = malloc(length);
	if (*node_handle == NULL) {
		TPT_ERROR(STR("malloc of size %d failed for node %s", length,
		              node_name));
		result = NVPI3_RESULT_OTHER_ERROR;
		goto srv_open_node_end;
	}

	memset(*node_handle, 0, length);
	(*node_handle)->group_handle = group_handle;
	(*node_handle)->seq_num = created_group->seq_num;
	(*node_handle)->name = (char *) *node_handle + name_offset;
	strcpy((*node_handle)->name, node_name);
	(*node_handle)->num_of_dbs = created_group->num_of_dbs;

	result = open_node(*node_handle, all);
	if (result != NVPI3_RESULT_SUCCESS) {
		goto srv_open_node_end;
	}

	LIST_INSERT_HEAD(&node_head, *node_handle, list);

srv_open_node_end:
	if (result != NVPI3_RESULT_SUCCESS) {
		free(*node_handle);
		*node_handle = NULL;
	}
	return result;
}

static nvpi3_result_t reopen_node(nvpi3_node_handle node_handle, int all)
{
	nvpi3_result_t result = close_node(node_handle);
	if (result != NVPI3_RESULT_SUCCESS) {
		goto reopen_node_end;
	}

	result = open_node(node_handle, all);

reopen_node_end:
	return result;
}

static nvpi3_result_t open_subnode(nvpi3_node_handle node_handle,
                                   const char **key_name,
                                   nvpi3_node_handle *subnode_handle)
{
	char *p, *path = NULL;
	nvpi3_result_t result = NVPI3_RESULT_NOT_FOUND;

	*subnode_handle = NULL;
	p = strrchr(*key_name, '/'); /* last /-char in the node */
	if (p != NULL) {
		/* key_name contains node information */
		uint32_t length, size;

		p++;
		length = strlen(node_handle->name);
		size = p - *key_name + length + 1;
		path = malloc(size);
		if (path == NULL) {
			TPT_ERROR(STR("malloc of size %d failed", size));
			result = NVPI3_RESULT_OTHER_ERROR;
			goto open_subnode_end;
		}

		memcpy(path, node_handle->name, length);
		memcpy(&path[length], *key_name, p - *key_name);
		path[length + p - *key_name] = '\0';
		*key_name = p;

		result =  nvpi3_srv_open_node(node_handle->group_handle,
		                              path, subnode_handle);
	}

open_subnode_end:
	free(path);
	return result;
}

static nvpi3_result_t close_node(nvpi3_node_handle node_handle)
{
	uint32_t i;
	nvpi3_result_t result = NVPI3_RESULT_SUCCESS;
	const struct nvpi3_created_db_group_object *created_group =
		get_created_db_group(node_handle->group_handle);

	for (i = 0; i < node_handle->num_of_dbs; i++) {
		nvpi3_result_t result2;

		if (!node_handle->db[i].found) {
			continue;
		}

		result2 = close_node_in_db(&created_group->db[i],
		                           node_handle->name, 0,
		                           node_handle->db[i].h_node);
		node_handle->db[i].found = 0;

		if (result == NVPI3_RESULT_SUCCESS) {
			result = result2;
		}
	}
	return result;
}

static nvpi3_result_t close_nodes(nvpi3_db_group_handle group_handle)
{
	nvpi3_result_t result = NVPI3_RESULT_SUCCESS;
	nvpi3_node_handle h_node = LIST_FIRST(&node_head);

	while (h_node != NULL) {
		nvpi3_node_handle h_next = LIST_NEXT(h_node, list);

		if (h_node->group_handle == group_handle) {
			nvpi3_result_t result2 = nvpi3_srv_close_node(h_node);
			if (result == NVPI3_RESULT_SUCCESS) {
				result = result2;
			}
		}
		h_node = h_next;
	}
	return result;
}

static nvpi3_result_t get_value_size_in_db(
	const struct nvpi3_created_db_object *db_object, void *h_node,
	char **key_name, nvpi3_key_type_t type, uint32_t *size,
	enum nvpi3_exception_type *exception_type)
{
	uint32_t i, length;
	char *name = NULL;
	nvpi3_db_format_t fmt = db_object->definition.format;
	nvpi3_result_t result;

	length = strlen(*key_name);
	name = malloc(length + MAX_EXCEPTION_SUFFIX_SIZE);
	if (name == NULL) {
		TPT_ERROR(STR("malloc of size %d failed for key %s",
		              length + MAX_EXCEPTION_SUFFIX_SIZE, *key_name));
		result = NVPI3_RESULT_OTHER_ERROR;
		goto get_value_size_in_db_end;
	}

	memcpy(name, *key_name, length);

	for (i = 0;
	     i < sizeof(exception_suffix) / sizeof(exception_suffix[0]);
	     i++) {
		uint32_t sz;

		memcpy(name + length, exception_suffix[i].str,
		       exception_suffix[i].length + 1);
		result = dc_func[fmt].get_value_size(h_node, name, type, &sz);
		if (result == NVPI3_RESULT_SUCCESS) {
			*exception_type = i;
			*size = sz;
			break;
		} else if (result != NVPI3_RESULT_NOT_FOUND) {
			TPT_ERROR(STR("get_value_size failed (%d) for key %s in"
			              " db %s", result, name,
			              db_name(&db_object->definition.storage)));
			break;
		}
	}


get_value_size_in_db_end:
	if (result != NVPI3_RESULT_SUCCESS) {
		free(name);
		name = NULL;
	}

	*key_name = name;
	return result;
}

static nvpi3_result_t get_value(nvpi3_node_handle node_handle,
                                const char *key_name, nvpi3_key_type_t type,
                                nvpi3_srv_get_value_callback callback,
                                void *user_data)
{
	uint32_t i;
	nvpi3_result_t result = NVPI3_RESULT_NOT_FOUND;
	struct {
		void *h_node;
		nvpi3_db_format_t fmt;
		char *key_name;
	} _default = {NULL, -1, NULL};
	const struct nvpi3_created_db_group_object *created_group =
		get_created_db_group(node_handle->group_handle);

	for (i = 0; i < node_handle->num_of_dbs; i++) {
		uint32_t size;
		enum nvpi3_exception_type exception_type;
		char *found_name = (char *) key_name;
		nvpi3_db_format_t fmt = created_group->db[i].definition.format;

		if (!node_handle->db[i].found) {
			continue;
		}

		result = get_value_size_in_db(&created_group->db[i],
		                              node_handle->db[i].h_node,
		                              &found_name, type, &size,
		                              &exception_type);
		if (result == NVPI3_RESULT_SUCCESS) {
			TPT_TRACE(1, STR("Key %s found in node %s in db %s",
			                 found_name, node_handle->name,
			                 db_name(&created_group->db[i].
			                 definition.storage)));
			if (exception_type == NVPI3_EXCEPTION_DEFAULT) {
				if (_default.h_node == NULL) {
					_default.h_node =
						node_handle->db[i].h_node;
					_default.fmt = fmt;
					_default.key_name = found_name;
				} else {
					free(found_name);
				}
				continue;
			}

			if (exception_type != NVPI3_EXCEPTION_DELETED) {
				result = dc_func[fmt].get_value(
					node_handle->db[i].h_node, key_name,
					type, callback, user_data);
			} else {
				result = NVPI3_RESULT_NOT_FOUND;
			}

			free(found_name);
			goto get_value_end;
		} else if (result != NVPI3_RESULT_NOT_FOUND) {
			goto get_value_end;
		}
	}

	if (_default.h_node != NULL) {
		result = dc_func[_default.fmt].get_value(_default.h_node,
		                                         _default.key_name,
		                                         type, callback,
		                                         user_data);
	}

get_value_end:
	free(_default.key_name);
	return result;
}

static nvpi3_result_t get_value_size(nvpi3_node_handle node_handle,
                                     const char *key_name,
                                     nvpi3_key_type_t type, uint32_t *size,
                                     uint32_t *index)
{
	uint32_t i, default_size = 0;
	nvpi3_result_t result = NVPI3_RESULT_NOT_FOUND;
	const struct nvpi3_created_db_group_object *created_group =
		get_created_db_group(node_handle->group_handle);

	for (i = 0; i < node_handle->num_of_dbs; i++) {
		uint32_t sz;
		enum nvpi3_exception_type exception_type;
		char *found_name = (char *) key_name;

		if (!node_handle->db[i].found) {
			continue;
		}

		result = get_value_size_in_db(&created_group->db[i],
		                              node_handle->db[i].h_node,
		                              &found_name, type, &sz,
		                              &exception_type);
		if (result == NVPI3_RESULT_SUCCESS) {
			TPT_TRACE(1, STR("Key %s found in node %s in db %s",
			                 found_name, node_handle->name,
			                 db_name(&created_group->db[i].
			                 definition.storage)));
			free(found_name);

			if (exception_type == NVPI3_EXCEPTION_DEFAULT) {
				if (!default_size) {
					default_size = sz;
					if (index != NULL) {
						*index = i;
					}
				}
				continue;
			}

			if (exception_type == NVPI3_EXCEPTION_DELETED) {
				result = NVPI3_RESULT_NOT_FOUND;
			}

			if (index != NULL) {
				*index = i;
			}

			*size = sz;
			goto get_value_size_end;
		} else if (result != NVPI3_RESULT_NOT_FOUND) {
			goto get_value_size_end;
		}
	}

	if (default_size) {
		*size = default_size;
		result = NVPI3_RESULT_SUCCESS;
	}

get_value_size_end:
	return result;
}

static nvpi3_result_t add_suffix(const char *name, const char *suffix,
                                 uint32_t suffix_length, char **name_and_suffix)
{
	uint32_t length;
	nvpi3_result_t result = NVPI3_RESULT_SUCCESS;

	*name_and_suffix = NULL;
	length = strlen(name);
	*name_and_suffix = malloc(length + suffix_length + 1);
	if (*name_and_suffix == NULL) {
		TPT_ERROR(STR("malloc of size %d failed for %s%s",
		              length + suffix_length + 1, name, suffix));
		result = NVPI3_RESULT_OTHER_ERROR;
		goto add_suffix_end;
	}
	memcpy(*name_and_suffix, name, length);

	(*name_and_suffix)[length + suffix_length] = '\0';
	if ((*name_and_suffix)[length-1] == '/') {
		length--;
		(*name_and_suffix)[length + suffix_length] = '/';
	}

	memcpy((*name_and_suffix) + length, suffix, suffix_length);

add_suffix_end:
	if (result != NVPI3_RESULT_SUCCESS) {
		free(*name_and_suffix);
		*name_and_suffix = NULL;
	}
	return result;
}

static nvpi3_result_t add_subnode_in_db(
	struct nvpi3_created_db_object *db_object, void *h_node,
	const char *node_name, uint32_t length, void *h_subnode)
{
	nvpi3_result_t result;
	nvpi3_db_format_t fmt = db_object->definition.format;
	uint32_t max_size = db_max_size(&db_object->definition.storage);

	for (;;) {
		void *buf;

		result = dc_func[fmt].add_node(h_node, node_name, length,
		                               h_subnode);
		if (result != NVPI3_RESULT_BUFFER_TOO_SMALL) {
			if (result != NVPI3_RESULT_SUCCESS) {
				TPT_ERROR(STR("add_node failed (%d) for node %s"
				              " in db %s", result, node_name,
				              db_name(&db_object->definition.
				                      storage)));
			}
			break;
		}

		if (db_object->size == max_size) {
			TPT_INFO(STR("Max size %d already reached in db %s",
			             max_size,
			             db_name(&db_object->definition.storage)));
			break;
		}

		buf = malloc(max_size);
		if (buf == NULL) {
			TPT_ERROR(STR("malloc of size %d failed for db %s",
			              max_size,
			              db_name(&db_object->definition.storage)));
			result = NVPI3_RESULT_OTHER_ERROR;
			break;
		}

		memset(buf, 0, max_size);
		result = dc_func[fmt].resize_db(db_object->h_db, buf, max_size);
		if (result != NVPI3_RESULT_SUCCESS) {
			TPT_INFO(STR("resize_db failed (%d) for size %d in db "
			             "%s", result, max_size,
			             db_name(&db_object->definition.storage)));
			free(buf);
			break;
		}

		db_object->size = max_size;
		free(db_object->buf);
		db_object->buf = buf;
	}

	return result;
}

static nvpi3_result_t add_node_in_db(struct nvpi3_created_db_object *db_object,
                                     const char **node_name, void **h_node)
{
	const char *src_start;
	void *h_node1 = NULL;
	nvpi3_result_t result = NVPI3_RESULT_INVALID_PARAM;
	nvpi3_db_format_t fmt = db_object->definition.format;
	const char *name =
		adjust_for_parent_node(db_object->definition.parent_node,
		                       *node_name);

	if (name == NULL || *name == '\0') {
		/*
		 * Requested node is within parent node or does not match parent
		 * node, this should never happen.
		 */
		TPT_ERROR(STR("Node name %s is within or do not match parent "
			      "node %s in db %s", *node_name,
		              db_object->definition.parent_node,
		              db_name(&db_object->definition.storage)));
		return NVPI3_RESULT_ACCESS_DENIED;
	}

	src_start = name;
	for (;;) {
		void *h_subnode;
		const char *src_end = strchr(src_start, '/');
							/* first '/' char. */
		if (src_end == NULL) {
			break;
		}

		src_end++;
		result = dc_func[fmt].open_node(db_object->h_db, h_node1, 0,
		                                src_start, src_end - src_start,
		                                &h_subnode);
		if (result == NVPI3_RESULT_NOT_FOUND) {
			result = add_subnode_in_db(db_object, h_node1,
			                           src_start,
			                           src_end - src_start,
			                           &h_subnode);
			if (result != NVPI3_RESULT_SUCCESS) {
				break;
			}
		} else if (result != NVPI3_RESULT_SUCCESS) {
			TPT_ERROR(STR("open_node failed (%d) for node %*s in db"
			              " %s", result, src_end - *node_name,
			              *node_name,
			              db_name(&db_object->definition.storage)));
			break;
		}

		if (h_node1 != NULL) {
			result = close_node_in_db(db_object, *node_name,
			                          src_end - *node_name,
			                          h_node1);
			if (result != NVPI3_RESULT_SUCCESS) {
				break;
		}
		}

		h_node1 = h_subnode;
		src_start = src_end;
	}

	if (result != NVPI3_RESULT_SUCCESS || h_node == NULL) {
		if (h_node1 != NULL) {
			nvpi3_result_t result2 =
				close_node_in_db(db_object, *node_name, 0,
				                 h_node1);
			if (result == NVPI3_RESULT_SUCCESS) {
				result = result2;
			}
			h_node1 = NULL;
		}
	} else {
		*node_name = src_start;
	}

	if (h_node != NULL) {
		*h_node = h_node1;
	}


	return result;
}

/* Note that delete_node requires that last char in node_name is '/'. */
static nvpi3_result_t delete_node(
	struct nvpi3_created_db_group_object *created_group,
	const char *node_name)
{
	nvpi3_result_t result = NVPI3_RESULT_SUCCESS;
	int deleted = 0;
	uint32_t i,  removed_index = UINT32_MAX, writable_index = UINT32_MAX,
		found_ro_index = UINT32_MAX;

	for (i = 0; i < created_group->num_of_dbs; i++) {
		enum nvpi3_exception_type exception_type;
		void *h_node;
		nvpi3_db_format_t fmt = created_group->db[i].definition.format;
		const char *name = adjust_for_parent_node(
			created_group->db[i].definition.parent_node, node_name);
		if (name == NULL || *name == '\0') {
			/*
			 * Requested node is within parent node or does not
			 * match parent node, skip database.
			 */
			TPT_TRACE(1, STR("%s is within or does not match parent"
			                 " node %s in db %s", node_name,
			                 created_group->db[i].definition.
			                 parent_node,
			                 db_name(&created_group->db[i].
			                         definition.storage)));
			continue;
		}

		if (created_group->db[i].definition.format ==
		    NVPI3_DB_FORMAT_FTD) {
			/* Database supports tree structure. */
			if (created_group->db[i].definition.permission !=
			    NVPI3_DB_PERMISSION_RO &&
			    writable_index == UINT32_MAX) {
				writable_index = i;
			}
		}

		result = open_node_in_db(&created_group->db[i], 0, &name,
		                         &exception_type, &h_node);
		if (result == NVPI3_RESULT_NOT_FOUND) {
			result = NVPI3_RESULT_SUCCESS;
			continue;
		} else if (result != NVPI3_RESULT_SUCCESS) {
			goto delete_node_end;
		}

		if (exception_type == NVPI3_EXCEPTION_DEFAULT) {
			/* Skip default node. */
			TPT_TRACE(1, STR("Default node found db %s, skipping it"
			                 " for %s",
			                 db_name(&created_group->db[i].
			                         definition.storage),
			                 node_name));
			result = close_node_in_db(&created_group->db[i],
			                          node_name, 0, h_node);
			if (result != NVPI3_RESULT_SUCCESS) {
				goto delete_node_end;
			}
			continue;
		}

		if (exception_type == NVPI3_EXCEPTION_DELETED) {
			TPT_TRACE(1, STR("Deleted node found in db %s, skipping"
			                 " it for %s",
			                 db_name(&created_group->db[i].
			                         definition.storage),
			                node_name));
			removed_index = i;
			result = close_node_in_db(&created_group->db[i],
			                          node_name, 0, h_node);
			break;
		}

		if (created_group->db[i].definition.permission ==
		    NVPI3_DB_PERMISSION_RO) {
			if (found_ro_index == UINT32_MAX) {
				found_ro_index = i;
			}

			result = close_node_in_db(&created_group->db[i],
			                          node_name, 0, h_node);
			if (result != NVPI3_RESULT_SUCCESS) {
				goto delete_node_end;
			}
			continue;
		}

		if (created_group->db[i].definition.format !=
		    NVPI3_DB_FORMAT_FTD) {
			/* Database does not support tree structure. */
			continue;
		}

		result = dc_func[fmt].delete_node(h_node);
		if (result != NVPI3_RESULT_SUCCESS) {
			TPT_INFO(STR("delete_node failed (%d) for node %s in "
			             "db %s", result, node_name,
			             db_name(&created_group->db[i].definition.
			                     storage)));
			goto delete_node_end;
		}
		deleted = 1;
		result = nvpi3_write(created_group->db[i].buf,
		                     created_group->db[i].size,
		                     &created_group->db[i].definition.storage);
	}

	if (removed_index < found_ro_index) {
		result = NVPI3_RESULT_NOT_FOUND;
	} else if (found_ro_index != UINT32_MAX &&
	           writable_index < found_ro_index) {
		const char *name;
		char *removed_name = NULL;

		result = add_suffix(
			node_name,
			exception_suffix[NVPI3_EXCEPTION_DELETED].str,
			exception_suffix[NVPI3_EXCEPTION_DELETED].length,
			&removed_name);
		if (result != NVPI3_RESULT_SUCCESS) {
			goto delete_node_end;
		}

		name = removed_name;
		result = add_node_in_db(&created_group->db[writable_index],
		                        &name, NULL);
		if (result == NVPI3_RESULT_SUCCESS) {
			TPT_TRACE(1, STR("Added node %s which overrides %s in "
			                 "db %s", removed_name, node_name,
			                 db_name(&created_group->
			                         db[removed_index].definition.
			                         storage)));
			result = nvpi3_write(
				created_group->db[writable_index].buf,
				created_group->db[writable_index].size,
				&created_group->db[writable_index].definition.
				storage);
		}

		free(removed_name);
	} else if (!deleted) {
		if (found_ro_index != UINT32_MAX) {
			TPT_INFO(STR("Node %s cannot be deleted due to db %s is"
			             " read only", node_name,
			             db_name(&created_group->db[found_ro_index].
			                     definition.storage)));
			result = NVPI3_RESULT_ACCESS_DENIED;
		} else {
			result = NVPI3_RESULT_NOT_FOUND;
		}
	}

delete_node_end:
	return result;
}

static nvpi3_result_t set_value_in_db(struct nvpi3_created_db_object *db_object,
                                      void *h_node, const char *key_name,
                                      nvpi3_key_type_t type, uint32_t size,
                                      const union nvpi3_key_value *value)
{
	nvpi3_result_t result;
	nvpi3_db_format_t fmt = db_object->definition.format;
	uint32_t max_size = db_max_size(&db_object->definition.storage);

	for (;;) {
		void *buf;

		result = dc_func[fmt].set_value(h_node, key_name, type, size,
		                                value);
		if (result != NVPI3_RESULT_BUFFER_TOO_SMALL) {
			if (result != NVPI3_RESULT_SUCCESS) {
				TPT_ERROR(STR("set_value failed (%d) for key %s"
				              " of type %d and size %d in db "
				              "%s", result, key_name, type,
				              size,
				              db_name(&db_object->definition.
				                      storage)));
			}
			break;
		}

		if (db_object->size == max_size) {
		  TPT_INFO(STR("Max size %d already reached in db %s",
			             max_size,
			             db_name(&db_object->definition.storage)));
			break;
		}

		buf = malloc(max_size);
		if (buf == NULL) {
			TPT_ERROR(STR("malloc of size %d failed for db %s",
			              max_size,
			              db_name(&db_object->definition.storage)));
			result = NVPI3_RESULT_OTHER_ERROR;
			break;
		}

		memset(buf, 0, max_size);
		result = dc_func[fmt].resize_db(db_object->h_db, buf, max_size);
		if (result != NVPI3_RESULT_SUCCESS) {
			TPT_INFO(STR("resize_db failed (%d) for size %d in db "
			             "%s", result, max_size,
			             db_name(&db_object->definition.storage)));
			free(buf);
			break;
		}

		db_object->size = max_size;
		free(db_object->buf);
		db_object->buf = buf;
	}

	return result;
}

static nvpi3_result_t delete_nodes_callback(
	void *user_data, __attribute__((__unused__)) const char *database_name,
	nvpi3_node_handle node_handle)
{
	STAILQ_HEAD(slist_head, delete_node_contents_data) *head = user_data;
	uint32_t length = strlen(node_handle->name);
	struct delete_node_contents_data *item =
		malloc(offsetof(struct delete_node_contents_data, u.node.name) +
		       length + 1);
	if (item == NULL) {
		TPT_ERROR(STR("malloc of size %d failed",
		              offsetof(struct delete_node_contents_data,
		                       u.node.name) + length + 1));
		return NVPI3_RESULT_OTHER_ERROR;
	}

	item->item_type = NVPI3_NODE_ITEM;
	memcpy(item->u.node.name, node_handle->name, length + 1);
	STAILQ_INSERT_TAIL(head, item, slist);
	return NVPI3_RESULT_SUCCESS;
}

static nvpi3_result_t delete_keys_callback(
	void *user_data,
	__attribute__((__unused__)) const char *database_name,
	const char *key_name, nvpi3_key_type_t type,
	__attribute__((__unused__)) const char *type_str,
	__attribute__((__unused__)) uint32_t size,
	__attribute__((__unused__)) const union nvpi3_key_value *value)
{
	STAILQ_HEAD(slist_head, delete_node_contents_data) *head = user_data;
	uint32_t length = strlen(key_name);
	struct delete_node_contents_data *item =
		malloc(offsetof(struct delete_node_contents_data, u.key.name) +
		       length + 1);
	if (item == NULL) {
		TPT_ERROR(STR("malloc of size %d failed",
		              offsetof(struct delete_node_contents_data,
		                       u.key.name) + length + 1));
		return NVPI3_RESULT_OTHER_ERROR;
	}
	item->item_type = NVPI3_KEY_ITEM;
	item->u.key.type = type;
	memcpy(item->u.key.name, key_name, length + 1);
	STAILQ_INSERT_TAIL(head, item, slist);
	return NVPI3_RESULT_SUCCESS;
}

static nvpi3_result_t delete_node_contents(nvpi3_db_group_handle group_handle,
                                           const char *node_name,
                                           uint32_t length)
{
	nvpi3_result_t result;
	nvpi3_node_handle node_handle = NULL;
	STAILQ_HEAD(slist_head, delete_node_contents_data) head =
		STAILQ_HEAD_INITIALIZER(head);
	char *name = malloc(length + 1);
	if (name == NULL) {
		TPT_ERROR(STR("malloc of size %d failed", length + 1));
		result = NVPI3_RESULT_OTHER_ERROR;
		goto delete_node_contents_end;
	}

	memcpy(name, node_name, length);
	name[length] = '\0';

	result = nvpi3_srv_open_node(group_handle, name, &node_handle);
	if (result != NVPI3_RESULT_SUCCESS) {
		goto delete_node_contents_end;
	}

	result = enum_nodes(group_handle, name, 0, 0, delete_nodes_callback,
	                    &head);
	if (result != NVPI3_RESULT_SUCCESS) {
		goto delete_node_contents_end;
	}

	result = nvpi3_srv_enum_keys(node_handle, 0, delete_keys_callback,
	                             &head);

delete_node_contents_end:
	if (node_handle != NULL) {
		nvpi3_result_t result2 = nvpi3_srv_close_node(node_handle);
		if (result == NVPI3_RESULT_SUCCESS) {
			result = result2;
		}
	}

	for (;;) {
		struct delete_node_contents_data *item = STAILQ_FIRST(&head);
		if (item == NULL) {
			break;
		}

		if (result == NVPI3_RESULT_SUCCESS) {
			if (item->item_type == NVPI3_NODE_ITEM) {
				result = nvpi3_srv_delete_node(
					group_handle, item->u.node.name);
			} else {
				result = nvpi3_srv_delete_key(
					group_handle, item->u.key.name,
					item->u.key.type);
			}
		}

		STAILQ_REMOVE_HEAD(&head, slist);
		free(item);
	}

	free(name);
	return result;
}

static nvpi3_result_t set_value(nvpi3_db_group_handle group_handle,
                                const char *key_name, nvpi3_key_type_t type,
                                uint32_t size,
                                const union nvpi3_key_value *value)
{
	uint32_t i;
	nvpi3_result_t result = NVPI3_RESULT_ACCESS_DENIED;
	char *found_name = NULL;
	struct nvpi3_created_db_group_object *created_group =
		get_created_db_group(group_handle);

	for (i = 0; i < created_group->num_of_dbs; i++) {
		void *h_node;
		const char *path, *name;
		uint32_t tmp;
		enum nvpi3_exception_type exception_type;
		nvpi3_db_format_t fmt = created_group->db[i].definition.format;

		result = NVPI3_RESULT_ACCESS_DENIED;

		if (created_group->db[i].definition.permission ==
		    NVPI3_DB_PERMISSION_RO) {
			continue;
		}

		path = adjust_for_parent_node(
			created_group->db[i].definition.parent_node, key_name);
		if (path == NULL || *path == '\0') {
			/*
			 * Requested node+key is within or does not match parent
			 * node, skip database.
			 */
			TPT_TRACE(1, STR("%s is within or does not match parent"
			                 " node %s in db %s", key_name,
			                 created_group->db[i].definition.
			                 parent_node,
			                 db_name(&created_group->db[i].
			                         definition.storage)));
			continue;
		}

		do {
			const char *suffix;

			name = path;
			result = open_node_in_db(&created_group->db[i], 0,
			                         &name, &exception_type,
			                         &h_node);
			if (result == NVPI3_RESULT_NOT_FOUND) {
				if (created_group->db[i].definition.format !=
				    NVPI3_DB_FORMAT_FTD) {
					/*
					 * Database does not support tree
					 * structure.
					 */
					TPT_TRACE(1,
					          STR("Db %s does not support "
					              "tree structure, skipping"
					              " it for %s",
					              db_name(&created_group->
					                      db[i].definition.
					                      storage),
					              key_name));
					break;
				}
				result = add_node_in_db(&created_group->db[i],
				                        &name, &h_node);
			}

			if (result != NVPI3_RESULT_SUCCESS) {
				goto set_value_end;
			}

			suffix = exception_suffix[exception_type].str;
			if (exception_type == NVPI3_EXCEPTION_DEFAULT) {
				/* Skip default node. */
				TPT_TRACE(1,
				          STR("Node %s%s found in db %s, "
				              "skipping it for %s", name,
				              suffix,
				              db_name(&created_group->db[i].
				                      definition.storage),
				              key_name));
				result = close_node_in_db(&created_group->db[i],
				                          name, 0, h_node);
				if (result != NVPI3_RESULT_SUCCESS) {
					goto set_value_end;
				}
				break;
			}

			if (exception_type == NVPI3_EXCEPTION_DELETED) {
				/*
				 * A node tagged deleted found in node path.
				 * We need to remove this node but at the same
				 * time tag all sub nodes and keys as deleted.
				 */
				TPT_TRACE(1,
				          STR("Node %s%s found in db %s, "
				              "removing it", name,
				              suffix,
				              db_name(&created_group->db[i].
				                      definition.storage)));

				result = dc_func[fmt].delete_node(h_node);
				if (result != NVPI3_RESULT_SUCCESS) {
					TPT_INFO(STR("delete_node failed (%d) "
					             "for node %s%s in db %s",
					              result, name, suffix,
					              db_name(&created_group->
					                      db[i].definition.
					                      storage)));
					goto set_value_end;
				}

				result = delete_node_contents(group_handle,
				                              path,
				                              name - path);
				if (result != NVPI3_RESULT_SUCCESS) {
					goto set_value_end;
				}
			}

		} while (exception_type == NVPI3_EXCEPTION_DELETED);

		if (result == NVPI3_RESULT_NOT_FOUND ||
		    exception_type == NVPI3_EXCEPTION_DEFAULT) {
			/*
			 * Database does not support tree structure or default
			 * node found.
			 */
			continue;
		}

		free(found_name);
		found_name = (char *) name;
		result = get_value_size_in_db(&created_group->db[i], h_node,
		                              &found_name, type, &tmp,
		                              &exception_type);
		if (result == NVPI3_RESULT_SUCCESS) {
			if (exception_type == NVPI3_EXCEPTION_DEFAULT) {
				/* Skip default key. */
				TPT_TRACE(1, STR("Key %s found in db %s, "
				                 "skipping it for %s",
				                 found_name,
				                 db_name(&created_group->db[i].
				                         definition.storage),
				                 key_name));

				result = close_node_in_db(&created_group->db[i],
				                          name, 0, h_node);
				if (result != NVPI3_RESULT_SUCCESS) {
					goto set_value_end;
				}
				continue;
			}

			if (exception_type == NVPI3_EXCEPTION_DELETED) {
				/* Found key tagged deleted, remove it first. */
				TPT_TRACE(1, STR("Key %s found in db %s, "
				                 "removing it", found_name,
				                 db_name(&created_group->db[i].
				                         definition.storage)));
				result = dc_func[fmt].delete_key(h_node,
				                                 found_name,
				                                 type);
				if (result != NVPI3_RESULT_SUCCESS) {
					TPT_ERROR(STR("delete_key failed (%d) "
					              "for key %*s%s of type %d"
					              "  in db %s", result,
					              name - key_name, key_name,
					              found_name, type,
					              db_name(&created_group->
					                      db[i].definition.
					                      storage)));
					(void) close_node_in_db(
							&created_group->db[i],
							name, 0, h_node);
					goto set_value_end;
				}
			}
		} else if (result != NVPI3_RESULT_NOT_FOUND) {
			(void) close_node_in_db(&created_group->db[i], name, 0,
			                        h_node);
			goto set_value_end;
		}

		result = set_value_in_db(&created_group->db[i], h_node, name,
		                         type, size, value);
		if (result == NVPI3_RESULT_UNSUPPORTED) {
			TPT_TRACE(1, STR("Db %s does not support type %d, "
			                 "skipping it for %s",
			                 db_name(&created_group->db[i].
			                         definition.storage),
			                 type, key_name));
			continue;
		}
		result = nvpi3_write(created_group->db[i].buf,
		                     created_group->db[i].size,
		                     &created_group->db[i].definition.storage);
		break;
	}

	if (i == created_group->num_of_dbs) {
		TPT_INFO(STR("All databases in group %s are write protected",
		             created_group->name));
	}
set_value_end:
	free(found_name);
	return result;
}

static nvpi3_result_t delete_key(
	struct nvpi3_created_db_group_object *created_group,
	const char *key_name, nvpi3_key_type_t type)
{
	void *h_node;
	nvpi3_result_t result = NVPI3_RESULT_SUCCESS;
	int deleted = 0;
	char *found_name = NULL;
	uint32_t i,  removed_index = UINT32_MAX, writable_index = UINT32_MAX,
		found_ro_index = UINT32_MAX;

	for (i = 0; i < created_group->num_of_dbs; i++) {
		nvpi3_result_t result2;
		enum nvpi3_exception_type exception_type;
		uint32_t tmp;
		nvpi3_db_format_t fmt = created_group->db[i].definition.format;
		const char *name = adjust_for_parent_node(
			created_group->db[i].definition.parent_node, key_name);

		if (name == NULL || *name == '\0') {
			/*
			 * Requested node is within parent node or does not
			 * match parent node, skip database.
			 */
			TPT_TRACE(1, STR("%s is within or does not match parent"
			                 " node %s in db %s", key_name,
			                 created_group->db[i].definition.
			                 parent_node,
			                 db_name(&created_group->db[i].
			                         definition.storage)));
			continue;
		}

		if (created_group->db[i].definition.permission !=
		    NVPI3_DB_PERMISSION_RO && writable_index == UINT32_MAX) {
			writable_index = i;
		}

		result = open_node_in_db(&created_group->db[i], 0, &name,
		                         &exception_type, &h_node);
		if (result == NVPI3_RESULT_NOT_FOUND) {
			continue;
		} else if (result != NVPI3_RESULT_SUCCESS) {
			goto delete_key_end;
		}

		if (exception_type == NVPI3_EXCEPTION_DEFAULT) {
			/* Skip default node. */
			TPT_TRACE(1, STR("Default node found in db %s, skipping"
			                 " it for %s",
			                 db_name(&created_group->db[i].
			                         definition.storage),
			                 key_name));
			result = close_node_in_db(&created_group->db[i],
			                          key_name, name - key_name,
			                          h_node);
			if (result != NVPI3_RESULT_SUCCESS) {
				goto delete_key_end;
			}
			continue;
		}

		if (exception_type == NVPI3_EXCEPTION_DELETED) {
			removed_index = i;
			result = close_node_in_db(&created_group->db[i],
			                          key_name, name - key_name,
			                          h_node);
			break;
		}

		free(found_name);
		found_name = (char *) name;
		result = get_value_size_in_db(&created_group->db[i], h_node,
		                              &found_name, type, &tmp,
		                              &exception_type);
		if (result == NVPI3_RESULT_NOT_FOUND) {
			result = close_node_in_db(&created_group->db[i],
			                          key_name, name - key_name,
			                          h_node);
			if (result != NVPI3_RESULT_SUCCESS) {
				goto delete_key_end;
			}
			continue;
		} else if (result != NVPI3_RESULT_SUCCESS) {
			(void) close_node_in_db(&created_group->db[i], key_name,
			                        name - key_name, h_node);
			goto delete_key_end;
		}

		if (exception_type == NVPI3_EXCEPTION_DEFAULT) {
			/* Skip default node. */
			TPT_TRACE(1, STR("Default key %s found in db %s, "
			                 "skipping it for %s",
			                 found_name,
			                 db_name(&created_group->db[i].
			                         definition.storage),
			                 key_name));
			result = close_node_in_db(&created_group->db[i],
			                          key_name, name - key_name,
			                          h_node);
			if (result != NVPI3_RESULT_SUCCESS) {
				goto delete_key_end;
			}
			continue;
		}

		if (exception_type == NVPI3_EXCEPTION_DELETED) {
			/* Found key tagged deleted */
			TPT_TRACE(1, STR("Deleted key %s found in db %s, "
			                 "skipping it for %s",
			                 found_name,
			                 db_name(&created_group->db[i].
			                         definition.storage),
			                 key_name));
			removed_index = i;
			result = close_node_in_db(&created_group->db[i],
			                          key_name, name - key_name,
			                          h_node);
			break;
		}


		if (created_group->db[i].definition.permission ==
		    NVPI3_DB_PERMISSION_RO) {
			if (found_ro_index == UINT32_MAX) {
				found_ro_index = i;
			}
			result = close_node_in_db(&created_group->db[i],
			                          key_name, name - key_name,
			                          h_node);
			if (result != NVPI3_RESULT_SUCCESS) {
				goto delete_key_end;
			}
			continue;
		}

		result = dc_func[fmt].delete_key(h_node, name, type);
		result2 = close_node_in_db(&created_group->db[i], key_name,
		                           name - key_name, h_node);
		if (result != NVPI3_RESULT_SUCCESS) {
			TPT_ERROR(STR("delete_key failed (%d) for key %s in "
			              "db %s", result, key_name,
			              db_name(&created_group->db[i].definition.
			                      storage)));
			goto delete_key_end;
		} else if (result2 != NVPI3_RESULT_SUCCESS) {
			result = result2;
			goto delete_key_end;
		}
		deleted = 1;
		result = nvpi3_write(created_group->db[i].buf,
		                      created_group->db[i].size,
		                      &created_group->db[i].definition.storage);
	}

	if (removed_index < found_ro_index) {
		result = NVPI3_RESULT_NOT_FOUND;
	} else if (found_ro_index != UINT32_MAX &&
	           writable_index < found_ro_index) {
		const char *name;
		char *removed_name = NULL;

		result = add_suffix(
			key_name,
			exception_suffix[NVPI3_EXCEPTION_DELETED].str,
			exception_suffix[NVPI3_EXCEPTION_DELETED].length,
			&removed_name);
		if (result != NVPI3_RESULT_SUCCESS) {
			goto delete_key_end;
		}

		name = removed_name;
		result = add_node_in_db(&created_group->db[writable_index],
		                        &name, &h_node);
		if (result == NVPI3_RESULT_SUCCESS) {
			union nvpi3_key_value value;

			value.u32 = 0;
			result = set_value_in_db(
				&created_group->db[writable_index], h_node,
				name, type,
				(type == NVPI3_KEY_TYPE_U32) ?
				sizeof(value.u32) : sizeof(value.u8), &value);
			if (result == NVPI3_RESULT_SUCCESS) {
				TPT_TRACE(1, STR("Added key %s in node %*s "
				                 "which overrides %s in db %s",
				                 name, name - removed_name,
				                 removed_name, key_name,
				                 db_name(&created_group->
				                         db[removed_index].
				                         definition.storage)));
			result = nvpi3_write(
				created_group->db[writable_index].buf,
				created_group->db[writable_index].size,
				&created_group->db[writable_index].definition.
				storage);
			}
		}
		free(removed_name);
	} else if (!deleted) {
		if (found_ro_index != UINT32_MAX) {
			TPT_INFO(STR("Key %s cannot be deleted due to db %s is"
			             " read only", key_name,
			             db_name(&created_group->db[found_ro_index].
			                     definition.storage)));
			result = NVPI3_RESULT_ACCESS_DENIED;
		} else {
			result = NVPI3_RESULT_NOT_FOUND;
		}
	}
delete_key_end:
	free(found_name);
	return result;
}

static char *create_db_name(
	const struct nvpi3_created_db_group_object *created_group,
	uint32_t index)
{
	uint32_t size;
	char *name;
	static const char *permission_str[NVPI3_DB_PERMISSION_WO + 1] =
	{"RO", "RW", "WO"};

	size = strlen(created_group->name);
	size += strlen(db_name(&created_group->db[index].definition.
	                       storage));
	size += sizeof(":4294967295 :0xffffffff") - 1 + sizeof(" (RO)");
	name = malloc(size);
	if (name == NULL) {
		TPT_ERROR(STR("malloc of size %d failed for db %s", size,
		              db_name(&created_group->db[index].definition.
		                      storage)));
		goto create_db_name_end;
	}

	sprintf(name, "%s:%" PRIu32 " %s:0x%x (%s)",
	        created_group->name, index,
	        db_name(&created_group->db[index].definition.storage),
	        db_offset(&created_group->db[index].definition.storage),
	        permission_str[created_group->db[index].definition.
	                       permission]);
create_db_name_end:
	return name;
}

static enum nvpi3_exception_type get_exception_type(const char *name,
                                                    uint32_t length)
{
	uint32_t i;
	enum nvpi3_exception_type exception_type = NVPI3_EXCEPTION_NONE;

	for (i = 0;
	     i < sizeof(exception_suffix) / sizeof(exception_suffix[0]);
	     i++) {
		if (i == NVPI3_EXCEPTION_NONE ||
		    length < exception_suffix[i].length) {
			continue;
		}
		if (!memcmp(&name[length - exception_suffix[i].length],
		            exception_suffix[i].str,
		            exception_suffix[i].length)) {
			exception_type = i;
			TPT_TRACE(1, STR("Found %s of exception type %d",
			                 name, exception_type));
			break;
		}
	}

	return exception_type;
}

static nvpi3_result_t enum_nodes_callback(void *user_data, void *h_node)
{
	uint32_t i, length, size;
	nvpi3_db_format_t fmt;
	nvpi3_result_t result;
	char *node_name = NULL;
	char *database_name = NULL;
	nvpi3_node_handle node_handle = NULL;
	struct enum_nodes_callback_data *ud = user_data;
	const struct nvpi3_created_db_group_object *created_group =
		get_created_db_group(ud->group_handle);

	fmt = created_group->db[ud->index].definition.format;
	length = strlen(created_group->db[ud->index].definition.parent_node);
	if (length) {
		/* Adjust for '/' at end of parent node. */
		length--;
	}

	size = dc_func[fmt].get_node_path_size(h_node);

	node_name = malloc(length + size);
	if (node_name == NULL) {
		TPT_ERROR(STR("malloc of size %d failed for db %s",
		              length + size,
		              db_name(&created_group->db[ud->index].definition.
		                      storage)));
		result = NVPI3_RESULT_OTHER_ERROR;
		goto enum_nodes_callback_end;
	}

	if (length) {
		memcpy(node_name,
		       created_group->db[ud->index].definition.parent_node,
		       length);
	}

	result = dc_func[fmt].get_node_path(h_node, &node_name[length], size);
	if (result != NVPI3_RESULT_SUCCESS) {
		TPT_ERROR(STR("get_node_path failed (%d) for node %p in db %s",
		              result, h_node,
		              db_name(&created_group->db[ud->index].definition.
		                      storage)));
		goto enum_nodes_callback_end;
	}

	if (!ud->all) {
		enum nvpi3_exception_type exception_type =
			get_exception_type(node_name, length + size - 2);

		if (exception_type != NVPI3_EXCEPTION_NONE) {
			/*
			 * Remove suffix and leave filtering to
			 * srv_open_node which is called below.
			 */
			node_name[length + size - 2 -
				exception_suffix[exception_type].length] = '/';
			node_name[length + size - 1 -
				exception_suffix[exception_type].length] = '\0';
		}
	}

	result = srv_open_node(ud->group_handle, node_name, ud->all,
	                       &node_handle);
	if (result != NVPI3_RESULT_SUCCESS) {
		if (result == NVPI3_RESULT_NOT_FOUND) {
			TPT_TRACE(1,STR("nvpi3_srv_open_node failed (%d) for "
			                "node %s in db group %s", result,
			                node_name, created_group->name));
			result = NVPI3_RESULT_SUCCESS;
		} else {
			TPT_ERROR(STR("nvpi3_srv_open_node failed (%d) for node"
			              "%s in db group %s", result, node_name,
			              created_group->name));
		}
		goto enum_nodes_callback_end;
	}

	/*
	 * If node exist in higher prio database then it has already been
	 * enumerated and should be skipped.
	 */
	for (i = 0; i < ud->index; i++) {
		if (!node_handle->db[i].found) {
			continue;
		}

		TPT_TRACE(1, STR("Excluding node %s in db %s at prio %d which "
		                 "have already been enumerated", node_name,
		                 db_name(&created_group->db[ud->index].
		                         definition.storage), i));
		goto enum_nodes_callback_end;
	}


	database_name = create_db_name(created_group, ud->index);
	if (database_name == NULL) {
		goto enum_nodes_callback_end;
	}

	result = ud->callback(ud->user_data, database_name, node_handle);
	if (result != NVPI3_RESULT_SUCCESS) {
		TPT_TRACE(1, STR("callback failed (%d) for node %s in db %s",
		                 result, node_name,
		                 db_name(&created_group->db[ud->index].
		                         definition.storage)));
	}

enum_nodes_callback_end:
	if (node_handle != NULL) {
		nvpi3_result_t result2 = nvpi3_srv_close_node(node_handle);
		if (result2 != NVPI3_RESULT_SUCCESS) {
			TPT_ERROR(STR("nvpi3_srv_close_node failed (%d) for"
			              " node %s in db group %s", result2,
			              node_name, created_group->name));
		}
		if (result == NVPI3_RESULT_SUCCESS) {
			result = result2;
		}
	}

	free(node_name);
	free(database_name);
	return result;
}

static nvpi3_result_t enum_nodes(nvpi3_db_group_handle group_handle,
                                 const char *node_name, int recursive, int all,
                                 nvpi3_srv_enum_nodes_callback callback,
                                 void *user_data)
{
	uint32_t i;
	struct enum_nodes_callback_data callback_data;
	nvpi3_result_t result = NVPI3_RESULT_NOT_FOUND;
	const struct nvpi3_created_db_group_object *created_group =
		get_created_db_group(group_handle);

	callback_data.group_handle = group_handle;
	callback_data.all = all;
	callback_data.callback = callback;
	callback_data.user_data = user_data;

	for (i = 0; i < created_group->num_of_dbs; i++) {
		nvpi3_db_format_t fmt = created_group->db[i].definition.format;
		const char *name = adjust_for_parent_node(
			created_group->db[i].definition.parent_node, node_name);

		if (name == NULL) {
			/* Requested node does not match parent node. */
			break;
		} else if (*name == '\0') {
			/*
			 * Requested node is within parent node, set it to
			 * root node.
			 */
			name = "/";
		}

		callback_data.index = i;
		result = dc_func[fmt].enum_nodes(created_group->db[i].h_db,
		                                 name, recursive,
		                                 enum_nodes_callback,
		                                 &callback_data);
		if (result != NVPI3_RESULT_SUCCESS &&
		    result != NVPI3_RESULT_NOT_FOUND) {
			break;
		}
	}

	return result;
}

static nvpi3_result_t enum_keys_callback(void *user_data, const char *key_name,
                                         nvpi3_key_type_t type,
                                         const char *type_str, uint32_t size,
                                         const union nvpi3_key_value *value)
{
	uint32_t length1, length2, sz, index;
	nvpi3_result_t result = NVPI3_RESULT_SUCCESS;
	char *path = NULL, *database_name = NULL;
	struct enum_keys_callback_data *ud = user_data;
	const struct nvpi3_created_db_group_object *created_group =
		get_created_db_group(ud->node_handle->group_handle);
	nvpi3_db_format_t fmt =
		created_group->db[ud->index].definition.format;
	length1 = strlen(created_group->db[ud->index].definition.parent_node);
	if (length1) {
		/* Adjust for '/' at end of parent node. */
		length1--;
	}

	sz = dc_func[fmt].get_node_path_size(
		ud->node_handle->db[ud->index].h_node);
	length2 = strlen(key_name);

	path = malloc(length1 + sz + length2);
	if (path == NULL) {
		TPT_ERROR(STR("malloc of size %d failed for db %s",
		              length1 + sz + length2,
		              db_name(&created_group->db[ud->index].definition.
		                      storage)));
		result = NVPI3_RESULT_OTHER_ERROR;
		goto enum_keys_callback_end;
	}
	if (length1) {
		memcpy(path,
		       created_group->db[ud->index].definition.parent_node,
		       length1);
	}

	result = dc_func[fmt].get_node_path(
		ud->node_handle->db[ud->index].h_node, &path[length1], sz);
	if (result != NVPI3_RESULT_SUCCESS) {
		TPT_ERROR(STR("get_node_path failed (%d) for node %p in db %s",
		              result, ud->node_handle->db[ud->index].h_node,
		              db_name(&created_group->db[ud->index].definition.
		                      storage)));
		goto enum_keys_callback_end;
	}


	length1 = length1 + sz - 1;
	memcpy(&path[length1], key_name, length2 + 1);
	if (!ud->all) {
		enum nvpi3_exception_type exception_type =
			get_exception_type(path, length1 + length2);

		if (exception_type != NVPI3_EXCEPTION_NONE) {
			/*
			 * Remove suffix and leave filtering to
			 * get_value_size which is called below.
			 */
			path[length1 + length2 -
			     exception_suffix[exception_type].length] =
				'\0';
		}

		result = get_value_size(ud->node_handle, &path[length1], type,
		                        &sz, &index);
		if (result != NVPI3_RESULT_SUCCESS) {
			if (result == NVPI3_RESULT_NOT_FOUND) {
				TPT_TRACE(1, STR("get_value_size failed (%d) "
				                 "for key %s in db group %s",
				                 result,path,
				                 created_group->name));
				result = NVPI3_RESULT_SUCCESS;
			} else {
				TPT_ERROR(STR("get_value_size failed (%d) for "
				              "key %s in db group %s", result,
				               path, created_group->name));
			}
			goto enum_keys_callback_end;
		} else if (index != ud->index) {
			TPT_TRACE(1, STR("Excluding key %s in db %s at prio %d "
			                 "which have already been enumerated",
			                 path,
			                 db_name(&created_group->db[ud->index].
			                         definition.storage),
			                 ud->index));
			goto enum_keys_callback_end;
		}
	}

	database_name = create_db_name(created_group, ud->index);
	if (database_name == NULL) {
		goto enum_keys_callback_end;
	}

	result = ud->callback(ud->user_data, database_name, path, type,
	                      type_str, size, value);
	if (result != NVPI3_RESULT_SUCCESS) {
		TPT_TRACE(1, STR("callback failed (%d) for key %s in db %s",
		                 result, key_name,
		                 db_name(&created_group->db[ud->index].
		                         definition.storage)));
	}

enum_keys_callback_end:
	free(database_name);
	free(path);
	return result;
}
