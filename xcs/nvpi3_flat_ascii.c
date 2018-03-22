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
#include <stdlib.h>
#include "nvpi3_flat_ascii.h"
#include "log_tpt.h"

struct nvpi3_flat_ascii_db {
	void *address;
	uint32_t size;
	char delimiter;
};

static nvpi3_result_t check_flat_ascii_db(struct nvpi3_flat_ascii_db *db);

static nvpi3_result_t get_next_key_and_value(struct nvpi3_flat_ascii_db *db,
                                             char *start, char **name,
                                             uint32_t *name_length,
                                             char **value,
                                             uint32_t *value_length,
                                             char **next);

static nvpi3_result_t get_key_and_value(struct nvpi3_flat_ascii_db *db,
                                        const char *key_name,
                                        nvpi3_key_type_t type, char **name,
                                        char **value, uint32_t *value_length,
                                        char **end);

nvpi3_result_t nvpi3_flat_ascii_get_db_header_size(uint32_t *size)
{
	*size = 0;
	return NVPI3_RESULT_SUCCESS;
}

nvpi3_result_t nvpi3_flat_ascii_get_db_size(
	__attribute__((__unused__)) void *header,
	__attribute__((__unused__)) uint32_t *size)
{
	return NVPI3_RESULT_UNSUPPORTED;
}

nvpi3_result_t nvpi3_flat_ascii_create_db(void *address, uint32_t size,
                                          int clear, char delimiter,
                                          void **h_db)
{
	struct nvpi3_flat_ascii_db *db = NULL;
	nvpi3_result_t result = NVPI3_RESULT_SUCCESS;

	if (size < 1) {
		TPT_INFO(STR("Supplied size %d less than minimum allowed %d",
		             size, 1));
		result = NVPI3_RESULT_DB_CORRUPT;
		goto nvpi3_flat_ascii_create_db_end;
	}

	db = malloc(sizeof(struct nvpi3_flat_ascii_db));
	if (db == NULL) {
		TPT_ERROR(STR("malloc of size %d failed",
		              sizeof(struct nvpi3_flat_ascii_db)));
		result = NVPI3_RESULT_OTHER_ERROR;
		goto nvpi3_flat_ascii_create_db_end;
	}

	db->address = address;
	db->size = size;
	db->delimiter = delimiter;

	if (clear) {
		((char *) address)[0] = '\0';
	} else {
		result = check_flat_ascii_db(db);
		if (result != NVPI3_RESULT_SUCCESS) {
			goto nvpi3_flat_ascii_create_db_end;
		}
	}

nvpi3_flat_ascii_create_db_end:
	if (result != NVPI3_RESULT_SUCCESS) {
		free(db);
		db = NULL;
	}

	*h_db = db;
	return result;
}

nvpi3_result_t nvpi3_flat_ascii_resize_db(void *h_db, void *address,
                                          uint32_t size)
{
	struct nvpi3_flat_ascii_db *db = h_db;
	memmove(address, db->address, db->size);
	db->address = address;
	db->size = size;
	return NVPI3_RESULT_SUCCESS;
}

nvpi3_result_t nvpi3_flat_ascii_destroy_db(void *h_db)
{
	free(h_db);
	return NVPI3_RESULT_SUCCESS;
}

nvpi3_result_t nvpi3_flat_ascii_open_node(
	void *h_db, void *h_parent_node,
	__attribute__((__unused__)) uint32_t close_option, const char *name,
	uint32_t length, void **h_node)
{
	nvpi3_result_t result = NVPI3_RESULT_NOT_FOUND;

	*h_node = NULL;

	if (h_parent_node == NULL && !strcmp(name, "/")) {
		*h_node = h_db;
		result = NVPI3_RESULT_SUCCESS;
	}

	return result;
}

nvpi3_result_t nvpi3_flat_ascii_close_node(
	__attribute__((__unused__)) void *h_node)
{
	return NVPI3_RESULT_SUCCESS;
}

uint32_t nvpi3_flat_ascii_get_node_path_size(
	__attribute__((__unused__)) void *h_node)
{
	return sizeof("/");
}

nvpi3_result_t nvpi3_flat_ascii_get_node_path(
	__attribute__((__unused__)) void *h_node, char *path, uint32_t size)
{
	if (size < sizeof("/")) {
		return NVPI3_RESULT_BUFFER_TOO_SMALL;
	}

	strcpy(path, "/");
	return NVPI3_RESULT_SUCCESS;
}

nvpi3_result_t nvpi3_flat_ascii_get_value(void *h_node, const char *key_name,
                                          nvpi3_key_type_t type, uint32_t *size,
                                          nvpi3_srv_get_value_callback callback,
                                          void *user_data)
{
	char *value_str;
	uint32_t value_length;
	nvpi3_result_t result;

	if (h_node == NULL) {
		TPT_INFO(STR("Warning: Invalid h_node %p supplied", h_node));
		result =  NVPI3_RESULT_INVALID_PARAM;
		goto nvpi3_flat_ascii_get_value_end;
	}

	result =  get_key_and_value((struct nvpi3_flat_ascii_db *) h_node,
	                            key_name, type, NULL, &value_str,
	                            &value_length, NULL);
	if (result != NVPI3_RESULT_SUCCESS) {
		if (result == NVPI3_RESULT_UNSUPPORTED) {
			result = NVPI3_RESULT_NOT_FOUND;
		}
		goto nvpi3_flat_ascii_get_value_end;
	}

	if (size != NULL) {
		*size = value_length + 1;
	}

	if (callback != NULL) {
		union nvpi3_key_value *value = malloc(value_length + 1);
		if (value == NULL) {
			TPT_ERROR(STR("malloc of size %d failed",
			              value_length + 1));
			result = NVPI3_RESULT_OTHER_ERROR;
			goto nvpi3_flat_ascii_get_value_end;
		}

		memcpy(value, value_str, value_length);
		value->str[value_length] = '\0';
		result = callback(user_data, value_length + 1,
		                                  value);
		free(value);
	}

nvpi3_flat_ascii_get_value_end:
	return result;
}

nvpi3_result_t nvpi3_flat_ascii_add_node(
	__attribute__((__unused__)) void *h_node,
	__attribute__((__unused__)) const char *name,
	__attribute__((__unused__)) uint32_t length,
	__attribute__((__unused__)) void **h_subnode)
{
	return NVPI3_RESULT_ACCESS_DENIED;
}

nvpi3_result_t nvpi3_flat_ascii_delete_node(
	__attribute__((__unused__)) void *h_node)
{
	return NVPI3_RESULT_ACCESS_DENIED;
}

nvpi3_result_t nvpi3_flat_ascii_set_value(void *h_db, const char *key_name,
                                          nvpi3_key_type_t type, uint32_t size,
                                          const union nvpi3_key_value *value)
{
	char *value_str, *end, *src, *dest;
	uint32_t value_length;
	nvpi3_result_t result;
	struct nvpi3_flat_ascii_db *db = h_db;

	result = get_key_and_value(db, key_name, type, NULL, &value_str,
	                           &value_length, &end);
	if (result == NVPI3_RESULT_SUCCESS) {
		src = value_str + value_length;
		dest = value_str + size;
	} else if (result == NVPI3_RESULT_NOT_FOUND) {
		src = end;
		/* Advance end "key_name=<value><delimiter>". */
		dest = end + strlen(key_name) + 1 + size +
			sizeof(db->delimiter);
		value_str = end;
	} else {
		goto nvpi3_flat_ascii_set_value_end;
	}

	if (dest > src) {
		if (dest >= (char *) db->address + db->size) {
			result = NVPI3_RESULT_BUFFER_TOO_SMALL;
			goto nvpi3_flat_ascii_set_value_end;
		}

		memmove(dest, src, dest - src);
	} else if (dest < src) {
		memmove(dest, src, src - dest);
	}

	if (result == NVPI3_RESULT_SUCCESS) {
		memcpy(value_str, value, size);
	} else {
		sprintf(value_str, "%s=%s%c", key_name, value->str,
		        db->delimiter);
		result = NVPI3_RESULT_SUCCESS;
	}

nvpi3_flat_ascii_set_value_end:
	return result;
}

nvpi3_result_t nvpi3_flat_ascii_delete_key(void *h_db, const char *key_name,
                                           nvpi3_key_type_t type)
{
	char *name, *value_str;
	uint32_t value_length;
	nvpi3_result_t result;
	struct nvpi3_flat_ascii_db *db = h_db;

	result = get_key_and_value(db, key_name, type, &name, &value_str,
	                           &value_length, NULL);
	if (result == NVPI3_RESULT_SUCCESS) {
		char *src = value_str + value_length + sizeof(db->delimiter);

		memmove(name, src, src - name);
	}

	return result;
}

nvpi3_result_t nvpi3_flat_ascii_enum_nodes(
	void *h_db, const char *node_name,
	__attribute__((__unused__)) int recursive,
	nvpi3_result_t (*callback) (void *user_data, void *h_node),
	void *user_data)
{
	if (h_db == NULL) {
		TPT_INFO(STR("Warning: Invalid h_db %p supplied", h_db));
		return NVPI3_RESULT_INVALID_PARAM;
	}

	if (!strcmp(node_name, "/")) {
		return callback(user_data, h_db);
	} else {
		return NVPI3_RESULT_NOT_FOUND;
	}
}

nvpi3_result_t nvpi3_flat_ascii_enum_keys(
        void *h_node,
        nvpi3_result_t (*callback)(void *user_data,
                                   const char *key_name,
                                   nvpi3_key_type_t type,
                                   const char *type_str,
                                   uint32_t size,
                                   const union nvpi3_key_value *value),
        void *user_data)
{
	char *next;
	struct nvpi3_flat_ascii_db *db = h_node;
	nvpi3_result_t result = NVPI3_RESULT_SUCCESS;

	if (h_node == NULL) {
		TPT_INFO(STR("Warning: Invalid h_node %p supplied", h_node));
		return NVPI3_RESULT_INVALID_PARAM;
	}

	next = db->address;
	for (;;) {
		char *key_name;
		char *name, *value_str;
		uint32_t name_length, value_length;
		union nvpi3_key_value *value;

		result = get_next_key_and_value(db, next, &name, &name_length,
		                                &value_str, &value_length,
		                                &next);
		if (result != NVPI3_RESULT_SUCCESS) {
			if (result == NVPI3_RESULT_NOT_FOUND) {
				result = NVPI3_RESULT_SUCCESS;
			}
			break;
		}

		key_name = malloc(name_length + 2);
		if (key_name == NULL) {
			TPT_ERROR(STR("malloc of size %d failed",
			          name_length + 2));
			result = NVPI3_RESULT_OTHER_ERROR;
			break;
		}

		memcpy(key_name, name, name_length);
		key_name[name_length] = '\0';

		value = malloc(value_length + 1);
		if (value == NULL) {
			TPT_ERROR(STR("malloc of size %d failed",
			              value_length + 1));
			free(key_name);
			result = NVPI3_RESULT_OTHER_ERROR;
			break;
		}

		memcpy(value, value_str, value_length);
		value->str[value_length] = '\0';

		result = callback(user_data, key_name, NVPI3_KEY_TYPE_STR, "",
		                  value_length + 1, value);

		free(key_name);
		free(value);
	}

	return result;
}

static nvpi3_result_t check_flat_ascii_db(struct nvpi3_flat_ascii_db *db)
{
	nvpi3_result_t result;
	char *next = db->address;

	do {
		char *name, *value;
		uint32_t name_length, value_length;

		result = get_next_key_and_value(db, next, &name, &name_length,
		                                &value, &value_length,
		                                &next);
	} while (result == NVPI3_RESULT_SUCCESS);

	if (result == NVPI3_RESULT_NOT_FOUND) {
		result = NVPI3_RESULT_SUCCESS;
	}

	return result;
}

static nvpi3_result_t get_next_key_and_value(struct nvpi3_flat_ascii_db *db,
                                             char *start, char **name,
                                             uint32_t *name_length,
                                             char **value,
                                             uint32_t *value_length,
                                             char **next)
{
	char *p;
	char *end = (char *) db->address + db->size;
	nvpi3_result_t result = NVPI3_RESULT_DB_CORRUPT;

	*name = NULL;
	*name_length = 0;
	*value = NULL;
	*value_length = 0;

	if (*start == '\0' || start >= end) {
		return NVPI3_RESULT_NOT_FOUND;
	}

	for (p = start; p < end; p++) {
		if (*p == db->delimiter) {
			*next = p + 1;
			goto key_and_value_end;
		}

		switch (*p) {
		case '\0':
			goto key_and_value_end;
		case '=':
			if (*name == NULL) {
				goto key_and_value_end;
			}

			if (*value == NULL) {
				if (p < end - 1) {
					*value = p + 1;
				}
				break;
			}
		/* Intentionally continuing into next case. */
		default:
			if (*value != NULL) {
				(*value_length)++;
			} else {
				if (*name == NULL) {
					*name = p;
				}
				(*name_length)++;
			}
			break;
		}
	}


key_and_value_end:
	if (*name == NULL || *value == NULL || *value_length == 0) {
		TPT_INFO(STR("%s not found at offset %d",
		             (*name == NULL) ? "Name" : "Value", p - start));
		*name = NULL;
		*name_length = 0;
		*value = NULL;
		*value_length = 0;
		*next = NULL;
	} else {
		result = NVPI3_RESULT_SUCCESS;
	}
	return result;
}

static nvpi3_result_t get_flat_ascii_db_end(struct nvpi3_flat_ascii_db *db,
                                            char *start, char **end)
{
	char *name, *value;
	uint32_t name_length, value_length;
	nvpi3_result_t result;

	*end = start;

	for (;;) {
		result = get_next_key_and_value(db, *end, &name,
		                                &name_length, &value,
		                                &value_length, end);
		if (result != NVPI3_RESULT_SUCCESS) {
			break;
		}
	}

	if (result == NVPI3_RESULT_NOT_FOUND) {
		result = NVPI3_RESULT_SUCCESS;
	}
	return result;
}

static nvpi3_result_t get_key_and_value(struct nvpi3_flat_ascii_db *db,
                                        const char *key_name,
                                        nvpi3_key_type_t type, char **name,
                                        char **value, uint32_t *value_length,
                                        char **end)
{
	char *next = db->address;
	char *name_str = NULL;
	nvpi3_result_t result = NVPI3_RESULT_INVALID_PARAM;

	if (type > NVPI3_KEY_TYPE_BIN) {
		TPT_INFO(STR("Warning: Invalid type %d supplied", type));
		result = NVPI3_RESULT_INVALID_PARAM;
		goto get_value_end;
	}

	if (type != NVPI3_KEY_TYPE_STR) {
		TPT_INFO(STR("Warning: Unsupported type %d supplied", type));
		result = NVPI3_RESULT_UNSUPPORTED;
		goto get_value_end;
	}

	for (;;) {
		uint32_t name_length;

		result = get_next_key_and_value(db, next, &name_str,
		                                &name_length, value,
		                                value_length, &next);
		if (result == NVPI3_RESULT_SUCCESS) {
			if (name_length != strlen(key_name) ||
			    strncmp(key_name, name_str, name_length)) {
				continue;
			}
		} else if (result != NVPI3_RESULT_NOT_FOUND) {
			break;
		}

		if (end != NULL) {
			result = get_flat_ascii_db_end(db, next, end);
		}
		break;
	}

get_value_end:
	if (name != NULL) {
		*name = name_str;
	}
	return result;
}
