/* > Description **************************************************************/
/**
 * @file nvpi3_common.h
 * @brief NVPI3 common header
 *
 * This file defines the NVPI3 common types.
 */
/*
 * Copyright (C) 2015 by Ericsson AB. All rights reserved. The
 * information in this document is the property of Ericsson. Except
 * as specifically authorized in writing by Ericsson, the receiver
 * of this document shall keep the information contained herein
 * confidential and shall protect the same in whole or in part from
 * disclosure and dissemination to third parties. Disclosure and
 * disseminations to the receiver's employees shall only be made on
 * a strict need to know basis.
 *
 ******************************************************************************/

#ifdef __cplusplus
extern "C" {
#endif

#ifndef NVPI3_COMMON_H
#define NVPI3_COMMON_H

/* > Includes *****************************************************************/
#include <stdint.h>

/* > Defines ******************************************************************/

/**
 * @ingroup nvpi3_result_t
 * Returned when the operation completed successfully.
 */
#define NVPI3_RESULT_SUCCESS                                        0

/**
 * @ingroup nvpi3_result_t
 * Returned when Invalid parameter supplied.
 */
#define NVPI3_RESULT_INVALID_PARAM                                  1

/**
 * @ingroup nvpi3_result_t
 * Returned when requested db, sub-node or key not found.
 */
#define NVPI3_RESULT_NOT_FOUND                                      2

/**
 * @ingroup nvpi3_result_t
 * Returned when supplied buffer is to small for requested value.
 */
#define NVPI3_RESULT_BUFFER_TOO_SMALL                               3

/**
 * @ingroup nvpi3_result_t
 * Returned when access denied.
 */
#define NVPI3_RESULT_ACCESS_DENIED                                  4

/**
 * @ingroup nvpi3_result_t
 * Returned when the service is not supported
 */
#define NVPI3_RESULT_UNSUPPORTED                                    5

/**
 * @ingroup nvpi3_result_t
 * Returned when database is corrupt.
 */
#define NVPI3_RESULT_DB_CORRUPT                                     6

/**
 * @ingroup nvpi3_result_t
 * Returned for other miscellaneous errors.
 */
#define NVPI3_RESULT_OTHER_ERROR                                    7

/**
 * @ingroup nvpi3_key_type_t
 * Key is a string.
 */
#define NVPI3_KEY_TYPE_STR                 0

/**
 * @ingroup nvpi3_key_type_t
 * Key is of type uint8_t.
 */
#define NVPI3_KEY_TYPE_U8                  1

/**
 * @ingroup nvpi3_key_type_t
 * Key is of type uint32_t.
 */
#define NVPI3_KEY_TYPE_U32                 2

/**
 * @ingroup nvpi3_key_type_t
 * Key is a binary array.
 */
#define NVPI3_KEY_TYPE_BIN                 3

/* > Type Declarations ********************************************************/

/**
 * @defgroup nvpi3_result_t nvpi3_result_t
 * @brief Return codes.
 */
/**
 * @ingroup nvpi3_result_t
 * @brief Return codes.
 */
typedef uint32_t nvpi3_result_t;

/** @brief  Holds a database handle, to be used to access a database group. */
typedef struct nvpi3_db_group_object *nvpi3_db_group_handle;

/** @brief Holds a node handle, to be used to access a node. */
typedef struct nvpi3_node_object *nvpi3_node_handle;

/**
 * @defgroup nvpi3_key_type_t nvpi3_key_type_t
 * @brief Key types.
 */
/**
 * @ingroup nvpi3_key_type_t
 * @brief Key types.
 */
typedef uint32_t nvpi3_key_type_t;

/** @brief Holding key values. */
union nvpi3_key_value
{
	/** Key is of type NVPI3_KEY_TYPE_U8 and contains a single value. */
	uint8_t u8;
	/** Key is of type NVPI3_KEY_TYPE_U32 and contains a single value. */
	uint32_t u32;
	/** Key is of type NVPI3_KEY_TYPE_STR. */
	char str[1];
	/** Key is of type NVPI3_KEY_TYPE_BIN. */
	uint8_t bin[1];
	/** Key is of type NVPI3_KEY_TYPE_U8 and contains an array. */
	uint8_t u8_array[1];
	/** Key is of type NVPI3_KEY_TYPE_U32 and contains an array. */
	uint32_t u32_array[1];
};

/* > Function Declarations ****************************************************/

#endif /* NVPI3_COMMON_H */

#ifdef __cplusplus
}
#endif
