/* > Description **************************************************************/
/**
 * @file nvpi3_cfg_common.h
 * @brief NVPI3 configuration common header
 *
 * This file defines the NVPI3 configuration common types.
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

#ifndef NVPI3_CFG_COMMON_H
#define NVPI3_CFG_COMMON_H

/* > Includes *****************************************************************/
#include <stdint.h>

/* > Defines ******************************************************************/

/** @brief Maximum file name length including terminating null. */
#define  NVPI3_MAX_STRING_LENGTH     256

/* > Type Declarations ********************************************************/

/**
 * @brief Database storage types.
 */
typedef enum
{
	/**
	 * The database is stored as file in the file system.
	 * Storage is specified via struct nvpi3_db_storage_file.
	 */
	NVPI3_DB_STORAGE_TYPE_FILE,
	/**
	 * The database is stored in a MTD partition.
	 * Storage is specified via struct nvpi3_db_storage_mtd.
	 */
	NVPI3_DB_STORAGE_TYPE_MTD
} nvpi3_db_storage_type_t;

/**
 * @brief Database formats.
 */
typedef enum
{
	/** The database is of Flat Device Tree format. */
	NVPI3_DB_FORMAT_FTD,
	/** The database is an ASCII Database. */
	NVPI3_DB_FORMAT_ASCII_DB,
	/** The database is of U-Boot environment image format. */
	NVPI3_DB_FORMAT_UENVIMAGE
} nvpi3_db_format_t;

/**
 * @brief Database permissions.
 */
typedef enum
{
	/** The database has Read Only permission. */
	NVPI3_DB_PERMISSION_RO,
	/** The database has Read and Write permission. */
	NVPI3_DB_PERMISSION_RW,
	/**
	 * The database has Write only permission.@n
	 * This can be used if to hide updates until the next restart.
	 */
	NVPI3_DB_PERMISSION_WO
} nvpi3_db_permission_t;


/**
 * @brief Holding storage specification for database stored as file in file
 *        system.
 */
struct nvpi3_db_storage_file
{
	/** Storage type */
	nvpi3_db_storage_type_t type;
	/** Offset to database from beginning of file. */
	uint32_t offset;
	/**
	 * Maximum allowed size for database. Set to zero for unlimited size.
	 */
	uint32_t max_size;
	/** File name including path. */
	char name[NVPI3_MAX_STRING_LENGTH];
};

/**
 * @brief Holding storage specification for database stored in a MTD partition.
 */
struct nvpi3_db_storage_mtd
{
	/** Storage type */
	nvpi3_db_storage_type_t type;
	/**
	 * Set to non zero if unused sector content can be discared after a
	 * sector erase. Set to zero if it must be restored.
	 * If the database does not share sectors then setting it non zero can
	 * minimize flash write operations.
	 */
	uint32_t discard_unused_sector_content;
	/** Offset to database from beginning of MTD partition. */
	uint32_t offset;
	/** Size of database. Set to zero if it is unknown. */
	uint32_t size;
	/**
	 * Maximum allowed size for database. Set to zero for unlimited size.
	 */
	uint32_t max_size;
	/** MTD partition name including path. */
	char name[NVPI3_MAX_STRING_LENGTH];
};

/**
 * @brief Holding storage specification for database.
 */
union nvpi3_db_storage
{
	/** Storage type */
	nvpi3_db_storage_type_t type;
	/** Storage specification for database stored as file in file system. */
	struct nvpi3_db_storage_file file;
	/** Storage specification for database stored in a MTD partition. */
	struct nvpi3_db_storage_mtd mtd;
};

/**
 * @brief Type for commit callback function.
 *
 * This function is called after database has been updated. If the database is
 * included in a container then this function can do post processing like
 * updating header and CRC for the container.
 *
 * @param[in]  user_data  Application context information
 * @param[in]  size       The number of bytes in database.
 * @param[in]  storage    Storage specification for database.
 */

typedef nvpi3_result_t (*nvpi3_db_commit_callback_func)(
	void *user_data,
	uint32_t size,
	const union nvpi3_db_storage *storage);

/**
 * @brief Holding database definitions.
 */
struct nvpi3_db_definition
{
	/**
	 * Optional name of parent node. All nodes and keys will appear as if
	 * they where stored in this parent node. This enables databases that
	 * have a flat structure to appear as if they exist in different nodes
	 * Parent node name should begin with '/' and each node/sub-node should
	 * be separated by '/', e.g. "/node/sub-node1/".
	 */
	char parent_node[NVPI3_MAX_STRING_LENGTH];
	/** Database format */
	nvpi3_db_format_t format;
 	/** Database permission. */
	nvpi3_db_permission_t permission;
	/** Storage specification */
	union nvpi3_db_storage storage;
	/** Commit callback. */
	struct
	{
		/*
		 * Application context information to be passed back to the
		 * commit callback function.
		 */
		void *user_data;
		/**
		 * Commit callback function. Should be set to NULL if not
		 * applicable.
		 */
		nvpi3_db_commit_callback_func callback;
	} commit;
};

/* > Function Declarations ****************************************************/

#endif /* NVPI3_CFG_COMMON_H */

#ifdef __cplusplus
}
#endif
