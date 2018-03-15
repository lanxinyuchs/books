/**
 *   Copyright (C) 2015 by Ericsson AB. All rights reserved. The
 *   information in this document is the property of Ericsson. Except
 *   as specifically authorized in writing by Ericsson, the receiver
 *   of this document shall keep the information contained herein
 *   confidential and shall protect the same in whole or in part from
 *   disclosure and dissemination to third parties. Disclosure and
 *   disseminations to the receiver's employees shall only be made on
 *   a strict need to know basis.
 */
#include <stddef.h>
#include <string.h>
#include <arpa/inet.h>
#include <stdlib.h>
#include "libfdt.h"
#include "nvpi3_fdt.h"
#include "log_tpt.h"

struct nvpi3_fdt_db {
	void *address;
	uint32_t recursive_limit;
	uint32_t recursive_count;
};

struct nvpi3_fdt_node {
	struct nvpi3_fdt_db *db;
	int offset;
	uint32_t name_length;
};


static const struct {
	char *str;
	uint32_t length;
} type_suffix[NVPI3_KEY_TYPE_BIN + 1] = {{"#str", sizeof("#str") - 1},
                                         {"#u8", sizeof("#u8") - 1},
                                         {"#u32", sizeof("#u32") - 1},
                                         {"", sizeof("") - 1}};

static const char *get_type_suffix(const char *key_name,
                                   nvpi3_key_type_t *type);

static char *add_type_suffix(const char *key_name, nvpi3_key_type_t type);

static nvpi3_result_t path_offset(const void *fdt, const char *path,
                                  int *offset);

static nvpi3_result_t get_node_name_length(const void *fdt, int offset,
                                           int *length);

static nvpi3_result_t get_property(const void *fdt, int offset,
                                   const char *key_name, int *size,
                                   const struct fdt_property **property);

static void free_property_value(nvpi3_key_type_t type,
                                union nvpi3_key_value *value);

static nvpi3_result_t get_property_value(const void *property,
                                         const char *key_name,
                                         nvpi3_key_type_t type, uint32_t size,
                                         union nvpi3_key_value **value);

static nvpi3_result_t get_value(void *h_node, const char *key_name,
                                nvpi3_key_type_t type, uint32_t *size,
                                nvpi3_srv_get_value_callback callback,
                                void *user_data);

static nvpi3_result_t enum_nodes(
	const void *h_node, int recursive,
	nvpi3_result_t (*callback) (void *user_data, void *h_node),
	void *user_data);

nvpi3_result_t nvpi3_fdt_get_db_header_size(uint32_t *size)
{
	*size = sizeof(struct fdt_header);
	return NVPI3_RESULT_SUCCESS;
}

nvpi3_result_t nvpi3_fdt_get_db_size(void *address, uint32_t *size)
{
	int ret = fdt_check_header(address);
	if (ret < 0) {
		TPT_INFO(STR("fdt_check_header returned %d", ret));
		return NVPI3_RESULT_DB_CORRUPT;
	}

	*size = fdt_totalsize(address);
	return NVPI3_RESULT_SUCCESS;
}

nvpi3_result_t nvpi3_fdt_create_db(void *address, uint32_t size, int clear,
                                   uint32_t recursive_limit, void **h_db)
{
	int ret;
	struct nvpi3_fdt_db *db = NULL;
	nvpi3_result_t result = NVPI3_RESULT_DB_CORRUPT;

	db = malloc(sizeof(struct nvpi3_fdt_db));
	if (db == NULL) {
		TPT_ERROR(STR("malloc of size %d failed",
		              sizeof(struct nvpi3_fdt_db)));
		result = NVPI3_RESULT_OTHER_ERROR;
		goto nvpi3_fdt_create_db_end;
	}

	if (clear) {
		ret = fdt_create_empty_tree(address, size);
		if (ret < 0) {
			TPT_INFO(STR("fdt_create returned %d", ret));
			goto nvpi3_fdt_create_db_end;
		}
	} else {
		ret = fdt_check_header(address);
		if (ret < 0) {
			TPT_INFO(STR("fdt_check_header returned %d", ret));
			goto nvpi3_fdt_create_db_end;
		}

		if (size < fdt_totalsize(address)) {
			TPT_INFO(STR("Supplied size %d less than %d that fdt "
				     "header indicates",
				     size, fdt_totalsize(address)));
			goto nvpi3_fdt_create_db_end;
		}
	}

	db->address = address;
	db->recursive_limit = recursive_limit;
	db->recursive_count = 0;

	result = NVPI3_RESULT_SUCCESS;

nvpi3_fdt_create_db_end:
	if (result != NVPI3_RESULT_SUCCESS) {
		free(db);
		db = NULL;
	}

	*h_db = db;
	return result;
}

nvpi3_result_t nvpi3_fdt_resize_db(void *h_db, void *address, uint32_t size)
{
	struct nvpi3_fdt_db *db = h_db;
	int ret = fdt_resize(db->address, address, size);
	if (ret < 0) {
		TPT_INFO(STR("fdt_resize returned %d", ret));
		return NVPI3_RESULT_OTHER_ERROR;
	}

	db->address = address;
	return NVPI3_RESULT_SUCCESS;
}

nvpi3_result_t nvpi3_fdt_destroy_db(void *h_db)
{
	free(h_db);
	return NVPI3_RESULT_SUCCESS;
}

nvpi3_result_t nvpi3_fdt_open_node(void *h_db, void *h_parent_node,
                                   uint32_t close_option, const char *name,
                                   uint32_t length, void **h_node)
{
	int offset;
	struct nvpi3_fdt_node *node = NULL;
	struct nvpi3_fdt_db *db = h_db;
	nvpi3_result_t result = NVPI3_RESULT_SUCCESS;

	if (h_parent_node == NULL) {
		result = path_offset(db->address, "/", &offset);
		if (result != NVPI3_RESULT_SUCCESS) {
			goto nvpi3_fdt_open_node_end;
		}
		name++;
		if (length == 1) {
			goto nvpi3_fdt_open_node_1;
		}
	} else {
		offset = ((struct nvpi3_fdt_node *) h_parent_node)->offset;
	}

	offset = fdt_subnode_offset_namelen(db->address, offset, name,
	                                    name[length - 1] == '/' ?
	                                    length - 1 : length);
	if (offset < 0) {
		if (offset == -FDT_ERR_NOTFOUND) {
			result = NVPI3_RESULT_NOT_FOUND;
		} else {
			TPT_INFO(STR("fdt_subnode_offset_namelen returned %d",
			             offset));

			if (offset == -FDT_ERR_BADOFFSET) {
				result = NVPI3_RESULT_INVALID_PARAM;
			} else {
				result = NVPI3_RESULT_DB_CORRUPT;
			}
		}
		goto nvpi3_fdt_open_node_end;
	}

nvpi3_fdt_open_node_1:
	if (h_parent_node == NULL || !close_option) {
		node = malloc(sizeof(struct nvpi3_fdt_node));
		if (node == NULL) {
			TPT_ERROR(STR("malloc of size %d failed",
			              sizeof(struct nvpi3_fdt_node)));
			result = NVPI3_RESULT_OTHER_ERROR;
			goto nvpi3_fdt_open_node_end;
		}
		node->db = db;
		node->name_length = 0;
	}

	if (node == NULL) {
		node = h_parent_node;
	}

	node->name_length += length;
	node->offset = offset;

nvpi3_fdt_open_node_end:
	*h_node = node;
	return result;
}

nvpi3_result_t nvpi3_fdt_close_node(void *h_node)
{
	free(h_node);
	return NVPI3_RESULT_SUCCESS;
}

uint32_t nvpi3_fdt_get_node_path_size(void *h_node)
{
	return ((struct nvpi3_fdt_node *)h_node)->name_length + 1;
}

nvpi3_result_t nvpi3_fdt_get_node_path(void *h_node, char *path, uint32_t size)
{
	int ret;
	struct nvpi3_fdt_node *node = (struct nvpi3_fdt_node *) h_node;

	if (size < node->name_length + 1) {
		return NVPI3_RESULT_BUFFER_TOO_SMALL;
	}

	ret = fdt_get_path(node->db->address, node->offset, path, size);
	if (ret) {
		TPT_INFO(STR("fdt_get_path returned %d", ret));

		if (ret == -FDT_ERR_BADOFFSET) {
			return NVPI3_RESULT_INVALID_PARAM;
		}
		if (ret == -FDT_ERR_NOSPACE) {
			/* Bug if this happens */
			TPT_ERROR(STR("Insufficient size %d supplied", size));
			return NVPI3_RESULT_OTHER_ERROR;
		}
		return NVPI3_RESULT_DB_CORRUPT;
	}

	path[size - 2] = '/';
	path[size - 1] = '\0';
	return NVPI3_RESULT_SUCCESS;
}

nvpi3_result_t nvpi3_fdt_get_value(void *h_node, const char *key_name,
                                   nvpi3_key_type_t type,
                                   nvpi3_srv_get_value_callback callback,
                                   void *user_data)
{
	return get_value(h_node, key_name, type, NULL, callback, user_data);
}

nvpi3_result_t nvpi3_fdt_get_value_size(void *h_node, const char *key_name,
                                        nvpi3_key_type_t type, uint32_t *size)
{
	return get_value(h_node, key_name, type, size, NULL, NULL);
}

nvpi3_result_t nvpi3_fdt_add_node(void *h_node, const char *name,
                                  uint32_t length, void **h_subnode)
{
	struct nvpi3_fdt_node *subnode = NULL;
	struct nvpi3_fdt_node *node = (struct nvpi3_fdt_node *) h_node;
	nvpi3_result_t result = NVPI3_RESULT_SUCCESS;

	subnode = malloc(sizeof(struct nvpi3_fdt_node));
	if (subnode == NULL) {
		TPT_ERROR(STR("malloc of size %d failed",
		              sizeof(struct nvpi3_fdt_node)));
		result = NVPI3_RESULT_OTHER_ERROR;
		goto nvpi3_fdt_add_node_end;
	}

	subnode->db = node->db;
	subnode->name_length = node->name_length + length;
	subnode->offset = fdt_add_subnode_namelen(node->db->address,
	                                          node->offset, name,
	                                          name[length - 1] == '/' ?
	                                          length - 1 : length);
	if (subnode->offset < 0) {
		TPT_INFO(STR("fdt_add_subnode returned %d", subnode->offset));

		switch (subnode->offset) {
		case -FDT_ERR_BADOFFSET:
		case -FDT_ERR_EXISTS:
			result = NVPI3_RESULT_INVALID_PARAM;
			break;
		case -FDT_ERR_NOSPACE:
			result = NVPI3_RESULT_BUFFER_TOO_SMALL;
			break;
		default:
			result = NVPI3_RESULT_DB_CORRUPT;
			break;
		}
	}

nvpi3_fdt_add_node_end:
	if (result != NVPI3_RESULT_SUCCESS) {
		free(subnode);
		subnode = NULL;
	}

	*h_subnode = subnode;
	return result;
}

nvpi3_result_t nvpi3_fdt_delete_node(void *h_node)
{
	int ret;
	nvpi3_result_t result;
	struct nvpi3_fdt_node *node = h_node;
	ret = fdt_del_node(node->db->address, node->offset);
	if (ret) {
		TPT_INFO(STR("fdt_del_node returned %d", ret));
		if (ret == -FDT_ERR_BADOFFSET) {
			result = NVPI3_RESULT_INVALID_PARAM;
		} else {
			result = NVPI3_RESULT_DB_CORRUPT;
		}
		goto nvpi3_fdt_delete_node_end;
	}

	result = nvpi3_fdt_close_node(h_node);

nvpi3_fdt_delete_node_end:
	return result;
}

nvpi3_result_t nvpi3_fdt_set_value(void *h_node, const char *key_name,
                                   nvpi3_key_type_t type, uint32_t size,
                                   const union nvpi3_key_value *value)
{
	int ret;
	nvpi3_result_t result;
	char *name = NULL;
	void *val = NULL;
	struct nvpi3_fdt_node *node = (struct nvpi3_fdt_node *) h_node;

	name = add_type_suffix(key_name, type);
	if (name == NULL) {
		result = NVPI3_RESULT_OTHER_ERROR;
		goto nvpi3_set_property_end;
	}

	if (type == NVPI3_KEY_TYPE_U32) {
		if (size & (sizeof(uint32_t) - 1)) {
			TPT_ERROR(STR("Key \"%s\" has value size (%d) "
			              "that is not a multiple of %d "
			              "bytes", key_name, size,
			              sizeof(uint32_t)));
			result = NVPI3_RESULT_OTHER_ERROR;
			goto nvpi3_set_property_end;
		}

		val = malloc(size);
		if (val == NULL) {
			TPT_ERROR(STR("malloc of size %d failed", size));
			result = NVPI3_RESULT_OTHER_ERROR;
			goto nvpi3_set_property_end;
		}

		for (uint32_t i = 0; i < size / sizeof(uint32_t); i++) {
			((uint32_t *)val)[i] = htonl(value->u32_array[i]);
		}
	} else {
		val = (void *) value;
	}

	ret = fdt_setprop(node->db->address, node->offset, name, val, size);
	if (ret) {
		TPT_INFO(STR("fdt_del_prop returned %d", ret));
		if (ret == -FDT_ERR_NOSPACE) {
			result = NVPI3_RESULT_BUFFER_TOO_SMALL;
		} else if (ret == -FDT_ERR_BADOFFSET) {
			result = NVPI3_RESULT_INVALID_PARAM;
		} else {
			result = NVPI3_RESULT_DB_CORRUPT;
		}
		goto nvpi3_set_property_end;
	}

	result = NVPI3_RESULT_SUCCESS;

nvpi3_set_property_end:
	free(name);
	if (type == NVPI3_KEY_TYPE_U32) {
		free(val);
	}
	return result;
}

nvpi3_result_t nvpi3_fdt_delete_key(void *h_node, const char *key_name,
                                    nvpi3_key_type_t type)
{
	int ret;
	nvpi3_result_t result = NVPI3_RESULT_SUCCESS;
	char *name = NULL;
	struct nvpi3_fdt_node *node = (struct nvpi3_fdt_node *) h_node;

	name = add_type_suffix(key_name, type);
	if (name == NULL) {
		result = NVPI3_RESULT_OTHER_ERROR;
		goto nvpi3_fdt_delete_key_end;
	}

	ret = fdt_delprop(node->db->address, node->offset, name);
	if (ret) {
		TPT_INFO(STR("fdt_del_prop returned %d", ret));
		if (ret == -FDT_ERR_NOTFOUND) {
			result = NVPI3_RESULT_NOT_FOUND;
		} else if (ret == -FDT_ERR_BADOFFSET) {
			result = NVPI3_RESULT_INVALID_PARAM;
		} else {
			result = NVPI3_RESULT_DB_CORRUPT;
		}
	}

nvpi3_fdt_delete_key_end:
	free(name);
	return result;
}

nvpi3_result_t nvpi3_fdt_enum_nodes(
	void *h_db, const char *node_name, int recursive,
	nvpi3_result_t (*callback) (void *user_data, void *h_node),
	void *user_data)
{
	nvpi3_result_t result;
	struct nvpi3_fdt_node node;
	struct nvpi3_fdt_db *db = h_db;

	node.db = db;
	node.name_length = 0;
	node.db->recursive_count = 0;

	result = path_offset(db->address, node_name, &node.offset);
	if (result != NVPI3_RESULT_SUCCESS) {
		return result;
	}

	node.name_length = strlen(node_name);

	return enum_nodes(&node, recursive, callback, user_data);
}

nvpi3_result_t nvpi3_fdt_enum_keys(
	void *h_node,
	nvpi3_result_t (*callback)(void *user_data,
	                           const char *key_name,
	                           nvpi3_key_type_t type,
	                           const char *type_str,
	                           uint32_t size,
	                           const union nvpi3_key_value *value),
	void *user_data)
{
	int offset;
	struct nvpi3_fdt_node *node = (struct nvpi3_fdt_node *) h_node;
	nvpi3_result_t result = NVPI3_RESULT_SUCCESS;
	char *key_name = NULL;

	offset = fdt_first_property_offset(node->db->address, node->offset);

	while (offset >= 0) {
		const void *property;
		const char *name, *type_str;
		nvpi3_key_type_t type;
		union nvpi3_key_value *value;
		int property_size;

		property = fdt_getprop_by_offset(node->db->address, offset,
		                                 &name, &property_size);
		if (property == NULL) {
			TPT_INFO(STR("fdt_getprop_by_offset returned %d",
			             property_size));
			if (property_size == -FDT_ERR_BADOFFSET) {
				result = NVPI3_RESULT_INVALID_PARAM;
			} else {
				result = NVPI3_RESULT_DB_CORRUPT;
			}
			goto nvpi3_fdt_enum_keys_end;
		}

		type_str = get_type_suffix(name, &type);
		key_name = malloc(type_str - name + 1);
		if (key_name == NULL) {
			TPT_ERROR(STR("malloc of size %d failed",
			              type_str - name + 1));
			result = NVPI3_RESULT_OTHER_ERROR;
			goto nvpi3_fdt_enum_keys_end;
		}

		memcpy(key_name, name, type_str - name);
		key_name[type_str - name] = '\0';
		result = get_property_value(property, key_name, type,
		                            property_size, &value);
		if (result == NVPI3_RESULT_SUCCESS) {
			result = callback(user_data, key_name, type, type_str,
			                  (uint32_t) property_size, value);
		}
		free_property_value(type, value);
		free(key_name);
		key_name = NULL;
		if (result != NVPI3_RESULT_SUCCESS) {
			goto nvpi3_fdt_enum_keys_end;
		}

		offset = fdt_next_property_offset(node->db->address, offset);
	}

	if (offset != -FDT_ERR_NOTFOUND) {
		TPT_INFO(STR("fdt_<first/next>_property_offset(...)"
		             " returned %d", offset));

		if (offset == -FDT_ERR_BADOFFSET) {
			result = NVPI3_RESULT_INVALID_PARAM;
		} else {
			result = NVPI3_RESULT_DB_CORRUPT;
		}
	}

nvpi3_fdt_enum_keys_end:
	free(key_name);
	return result;
}

static const char *get_type_suffix(const char *key_name, nvpi3_key_type_t *type)
{
	uint32_t length = strlen(key_name);

	for (uint32_t i = 0;
	     i < sizeof(type_suffix) / sizeof(type_suffix[0]);
	     i++) {
		const char *p;

		if (length <= type_suffix[i].length) {
			continue;
		}

		p = &key_name[length - type_suffix[i].length];
		if (!strcmp(p, type_suffix[i].str)) {
			*type = i;
			return p;
		}
	}

	*type = NVPI3_KEY_TYPE_BIN;
	return &key_name[length];
}

static char *add_type_suffix(const char *key_name, nvpi3_key_type_t type)
{
	uint32_t length = strlen(key_name);
	char *name = malloc(length + type_suffix[type].length + 1);
	if (name == NULL) {
		TPT_ERROR(STR("malloc of size %d failed",
		              length + type_suffix[type].length + 1));
		return NULL;
	}

	memcpy(name, key_name, length);
	memcpy(&name[length], type_suffix[type].str, type_suffix[type].length);
	name[length + type_suffix[type].length] = '\0';
	return name;
}

static nvpi3_result_t path_offset(const void *fdt, const char *path,
                                  int *offset)
{
	*offset = fdt_path_offset(fdt, path);
	if (*offset < 0) {
		if (*offset == -FDT_ERR_NOTFOUND) {
			TPT_TRACE(1, STR("fdt_path_offset(..., \"%s\") "
			                 "returned %d",
			                 path, *offset));
			return NVPI3_RESULT_NOT_FOUND;
		}

		TPT_INFO(STR("fdt_path_offset(..., \"%s\") returned %d",
		             path, *offset));

		if(*offset == -FDT_ERR_BADPATH) {
			return NVPI3_RESULT_INVALID_PARAM;
		}
		return NVPI3_RESULT_DB_CORRUPT;
	}
	return NVPI3_RESULT_SUCCESS;
}

static nvpi3_result_t get_node_name_length(const void *fdt, int offset,
                                           int *length)
{
	(void) fdt_get_name(fdt, offset, length);
	if (*length < 0) {
		TPT_INFO(STR("fdt_path_offset(...) returned %d", *length));

		if(*length == -FDT_ERR_BADOFFSET) {
			return NVPI3_RESULT_INVALID_PARAM;
		}
		return NVPI3_RESULT_DB_CORRUPT;
	}

	(*length) += 1; /* Add space for '/' at end. */

	return NVPI3_RESULT_SUCCESS;
}

static nvpi3_result_t get_property(const void *fdt, int offset,
                                   const char *key_name, int *size,
                                   const struct fdt_property **property)
{
	*property = fdt_get_property(fdt, offset, key_name, size);
	if (*size < 0) {
		if (*size == -FDT_ERR_NOTFOUND) {
			TPT_TRACE(1, STR("fdt_get_property"
			                 "(..., \"%s\") returned %d",
			                 key_name, *size));
			return NVPI3_RESULT_NOT_FOUND;
		}

		TPT_INFO(STR("fdt_get_property(..., \"%s\") returned %d",
		             key_name, *size));

		if (*size == -FDT_ERR_BADOFFSET) {
			return NVPI3_RESULT_INVALID_PARAM;
		}
		return NVPI3_RESULT_DB_CORRUPT;
	}
	return NVPI3_RESULT_SUCCESS;
}

static void free_property_value(nvpi3_key_type_t type,
                                union nvpi3_key_value *value)
{
	if (type == NVPI3_KEY_TYPE_U32) {
		free(value);
	}
}

static nvpi3_result_t get_property_value(const void *property,
                                         const char *key_name,
                                         nvpi3_key_type_t type, uint32_t size,
                                         union nvpi3_key_value **value)
{
	*value = NULL;


	if (type == NVPI3_KEY_TYPE_U32) {
		if (size & (sizeof(uint32_t) - 1)) {
			TPT_ERROR(STR("Key \"%s\" has value size (%d) "
			              "that is not a multiple of %d "
			              "bytes", key_name, size,
			              sizeof(uint32_t)));
			return NVPI3_RESULT_OTHER_ERROR;
		}

		if ((uint32_t) property & (sizeof(uint32_t) - 1)) {
			TPT_ERROR(STR("Key \"%s\" has alignement (%p)"
			              " that is not a multiple of %d "
			              "bytes", key_name, property,
			              sizeof(uint32_t)));
			return NVPI3_RESULT_OTHER_ERROR;
		}

		*value = malloc(size);
		if (*value == NULL) {
			TPT_ERROR(STR("malloc of size %d failed", size));
			return NVPI3_RESULT_OTHER_ERROR;
		}

		for (uint32_t i = 0; i < size / sizeof(uint32_t); i++) {
			(*value)->u32_array[i] =
				ntohl(((uint32_t *)property)[i]);
		}
	} else {
		*value = (union nvpi3_key_value *) property;
	}

	return NVPI3_RESULT_SUCCESS;
}

static nvpi3_result_t get_value(void *h_node, const char *key_name,
                                nvpi3_key_type_t type, uint32_t *size,
                                nvpi3_srv_get_value_callback callback,
                                void *user_data)
{
	const struct fdt_property *property;
	uint32_t value_size;
	union nvpi3_key_value *value;
	char *name = NULL;
	nvpi3_result_t result = NVPI3_RESULT_SUCCESS;
	struct nvpi3_fdt_node *node = (struct nvpi3_fdt_node *) h_node;

	if (type > NVPI3_KEY_TYPE_BIN) {
		TPT_INFO(STR("Invalid type %d supplied", type));
		result = NVPI3_RESULT_INVALID_PARAM;
		goto get_value_end;
	}

	name = add_type_suffix(key_name, type);
	if (name == NULL) {
		result = NVPI3_RESULT_OTHER_ERROR;
		goto get_value_end;
	}

	result = get_property(node->db->address, node->offset, name,
	                      (int *)&value_size, &property);
	if (result != NVPI3_RESULT_SUCCESS) {
		goto get_value_end;
	}

	result = get_property_value(property->data, name, type, value_size,
	                            &value);
	if (result == NVPI3_RESULT_SUCCESS) {
		if (size != NULL) {
			*size = value_size;
		}
		if (callback != NULL) {
			result = callback(user_data, value_size, value);
		}
	}

	free_property_value(type, value);

get_value_end:
	free(name);
	return result;
}

static nvpi3_result_t enum_nodes(
	const void *h_node, int recursive,
	nvpi3_result_t (*callback) (void *user_data, void *h_node),
	void *user_data)
{
	nvpi3_result_t result;
	struct nvpi3_fdt_node next_node;
	struct nvpi3_fdt_node *node = (struct nvpi3_fdt_node *) h_node;

	next_node.db = node->db;
	next_node.name_length = node->name_length;

	next_node.offset = fdt_first_subnode(next_node.db->address,
	                                     node->offset);
	while (next_node.offset != -FDT_ERR_NOTFOUND) {
		int length;

		result = get_node_name_length(next_node.db->address,
		                              next_node.offset, &length);
		if (result != NVPI3_RESULT_SUCCESS) {
			return result;
		}

		next_node.name_length = node->name_length + length;

		result = callback(user_data, &next_node);
		if (result != NVPI3_RESULT_SUCCESS) {
			return result;
		}
		if (recursive) {
			if (node->db->recursive_count >
			    node->db->recursive_limit) {
				TPT_ERROR(STR("Recursive limit %d exceeded",
				              node->db->recursive_limit));
				return NVPI3_RESULT_OTHER_ERROR;
			}

			node->db->recursive_count++;
			result = enum_nodes(&next_node, recursive, callback,
			                    user_data);
			node->db->recursive_count--;
			if (result != NVPI3_RESULT_SUCCESS) {
				return result;
			}
		}

		next_node.offset =
			fdt_next_subnode(node->db->address, next_node.offset);
	}
	return NVPI3_RESULT_SUCCESS;
}
