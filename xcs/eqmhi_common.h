/* > Description **************************************************************/
/*
 * Copyright Ericsson AB
 *
 * The copyright to the computer programs herein is the property of Ericsson AB.
 * The programs may be used and/or copied only with the written permission from
 * Ericsson AB or in accordance with the terms conditions stipulated in the
 * agreement/contract under which the programs have been supplied.
 *
 ******************************************************************************/
/**
 * @file eqmhi_common.h
 * @brief Equipment Handling service common header
 *
 * This file defines the EQMHI common types
 *
 ******************************************************************************/

#ifdef __cplusplus
extern "C" {
#endif


#ifndef EQMHI_COMMON_H
#define EQMHI_COMMON_H


/* > Includes *****************************************************************/
#include <stdint.h>

/* > Defines ******************************************************************/
#define EQMHI_MAX_STRING_LENGTH        256
#define EQMHI_MAX_ERROR_STRING_LENGTH  512
#define EQMHI_MAX_CFG_DUMP_EM_AREAS    8

/**
 * @ingroup eqmhi_cfg_type_t
 * Configuration of dump file location and number of dump files.
 * Configuration data is defined by type struct eqmhi_cfg_dump_file.
 */
#define EQMHI_CFG_DUMP_FILE 0x00000001

/**
 * @ingroup eqmhi_cfg_type_t
 * Configuration for external memory to dump.
 * This configuration option is currently not supported on RU.
 * Configuration data is defined by type struct eqmhi_cfg_dump_em.
 */
#define EQMHI_CFG_DUMP_EM 0x00000002

/**
 * @ingroup eqmhi_cfg_type_t
 * Start of range for Baseband Specific configuration.
 */
#define EQMHI_CFG_BB_BASE 0x00010000

/**
 * @ingroup eqmhi_cfg_type_t
 * Start of range for RU Specific configuration.
 */
#define EQMHI_CFG_RU_BASE 0x00020000

/* > Type Declarations ********************************************************/

struct eqmhi_cfg_entity;

/**
 * @brief The status codes that the EQMHI interface returns to the user.
 */
typedef enum
{
	/** Returned when the operation completed successfully */
	EQMHI_STATUS_SUCCESS = 0,
	/** Returned when Invalid parameter supplied */
	EQMHI_STATUS_INVALID_PARAM,
	/** Returned when the operation requested during wrong state */
	EQMHI_STATUS_WRONG_STATE,
	/** The supplied buffer was to small for the data to return. */
	EQMHI_STATUS_BUFFER_TOO_SMALL,
	/** Returned when the service is not supported */
	EQMHI_STATUS_UNSUPPORTED,
	/** Returned for other miscellaneous errors */
	EQMHI_STATUS_OTHER
} eqmhi_status_t;

/**
 * @brief Error values.
 */
typedef enum
{
	EQMHI_FAULT_TYPE_FATAL_ERROR = 0x1,      /*!< Fatal Error */
	EQMHI_FAULT_TYPE_GEN_HW_ERROR = 0x2,     /*!< H/W General Error */
	EQMHI_FAULT_TYPE_GEN_SW_ERROR = 0x4,     /*!< S/W General Error */
	EQMHI_FAULT_TYPE_CONN_LOST = 0x8,        /*!< Connection Lost */
	EQMHI_FAULT_TYPE_ALL_FAULTS = 0x7FFFFFFF /*!< All Faults */
} eqmhi_fault_types_t;

/**
 * @brief Type of reference to the connection between a client and
 *        Eqmh controller. Also, passed to all interface functions.
 */
typedef struct eqmhi_instance *eqmhi_instance_t;

/**
 * @defgroup eqmhi_cfg_type_t eqmhi_cfg_type_t
 * @brief Configuration types for an EQM.
 */
/**
 * @ingroup eqmhi_cfg_type_t
 * @brief Configuration type of an EQM.
 */
typedef uint32_t eqmhi_cfg_type_t;

/**
 * @brief Holding configuration specification for dump generation.
 */
struct eqmhi_cfg_dump_file
{
	/** Configuration type (EQMHI_CFG_DUMP_FILE). */
	eqmhi_cfg_type_t type;
	/** Maximum number of dumps to be stored on file system. */
	uint32_t max_num_of_dumps;
	/** Path in the file system where dumps should be written. */
	char dump_location[EQMHI_MAX_STRING_LENGTH];
};

/**
 * @brief Holding configuration specification for dump of external memory.
 */
struct eqmhi_cfg_dump_em
{
	/** Configuration type (EQMHI_CFG_DUMP_EM) */
	eqmhi_cfg_type_t type;
	/** Number of of EM areas. */
	uint32_t num_of_em_areas;
	/** Array of EM areas. */
	struct
	{
		/** Size of EM to dump in bytes. */
		uint32_t size;
		/** Address to the EM area to dump. */
		uint32_t address;
	} area[EQMHI_MAX_CFG_DUMP_EM_AREAS];
};

/**
 * @brief Load Module types.
 *
 * It is implentation dependent if EQMH support to store LM:
 * - As LM in Load Module Container (LMC) in file system.
 * - As a separate file in file system.
 */
typedef enum
{
	/**
	 * The LM is stored in LMC in file system. LM is specified via
	 * struct eqmhi_load_type_lmc.
	 */
	EQMHI_LOAD_TYPE_LMC,
	/**
	 * The LM is stored as separate file in file system. LM is specified via
	 * struct eqmhi_load_type_lm.
	 */
	EQMHI_LOAD_TYPE_LM
} eqmhi_load_type_t;

/**
 * @brief Holding Load Module specification for LM stored in LMC in file system.
 */
struct eqmhi_load_type_lmc
{
	/** Load module type (EQMHI_LOAD_TYPE_LMC). */
	eqmhi_load_type_t type;
	/** LMC file name including path. */
	char name[EQMHI_MAX_STRING_LENGTH];
	/* LM id for LM in LMC. */
	char lm_id[EQMHI_MAX_STRING_LENGTH];
};


/**
 * @brief Holding Load Module specification for LM stored as separate file in
 *        file system
 */
struct eqmhi_load_type_lm
{
	/** Load module type (EQMHI_LOAD_TYPE_LM). */
	eqmhi_load_type_t type;
	/** LM file name including path. */
	char name[EQMHI_MAX_STRING_LENGTH];
};

/**
 * @brief Holding Load specification for entity.
 */
struct eqmhi_load_entity
{
	/** EQM identity. */
	uint32_t eqm_id;
	/** Load Module specification */
	union
	{
		/** Load module type. */
		eqmhi_load_type_t type;
		/**
		 * Load Module specification for LM stored in LMC in file
		 * system.
		 */
		struct eqmhi_load_type_lmc lmc;
		/**
		 * Load Module specification for LM stored as separate file in
		 * file system.
		 */
		struct eqmhi_load_type_lm lm;
	} load;
};

/**
 * @brief Additional fault information of H/W General Error
 */
struct eqmhi_fault_type_gen_hw_error
{
	/** HW object type */
	uint32_t object;
	/** HW fault diagnostic */
	uint32_t diagnostic;
	/** HW fault recovery action */
	uint32_t recovery_actions;
	/** Fault recovery time (in seconds) */
	uint32_t recovery_time;
};

/**
* @brief Holding Fault Indication Content.
*/
struct eqmhi_fault_ind
{
	uint32_t eqm_id;                           /*!< EQM identity. */
	eqmhi_fault_types_t fault_type;            /*!< Type of fault. */
	char error[EQMHI_MAX_ERROR_STRING_LENGTH]; /*!< Error string. */
	/** Fault Module specification */
	union
	{
		/** Additional fault information of H/W General Error. */
		struct eqmhi_fault_type_gen_hw_error gen_hw_error;
	} fault;
};

/**
 * @brief Type for callbacks functions for load, boot, stop, dump, reload and
 *        set configuration.
 *
 * @param[in]  instance   EQMH instance
 * @param[in]  user_data  Application context information
 * @param[in]  status     Return status of load, boot, stop, dump, reload and
 *                        set configuration request.
 */
typedef void (*eqmhi_callback_func)(eqmhi_instance_t instance,
                                    void *user_data,
                                    eqmhi_status_t status);

/**
 * @brief Type for fault indication function.
 *
 * @param[in]  instance   EQMH instance
 * @param[in]  user_data  Application context information
 * @param[in]  faults     Fault Indication.
 */
typedef void (*eqmhi_fault_ind_func)(eqmhi_instance_t instance,
                                     void *user_data,
                                     struct eqmhi_fault_ind *fault);

/**
 * @brief Type for get configuration callback function.
 *
 * @param[in]  instance         EQMH instance
 * @param[in]  user_data        Application context information
 * @param[in]  status           Return status of get configuration request.
 * @param[in]  num_of_entities  number of configuration entities
 * @param[in]  entity           Array containing configuration parameters from
 *                              each entity.
 */
typedef void (*eqmhi_get_cfg_callback_func)(
	eqmhi_instance_t instance,
	void *user_data,
	eqmhi_status_t status,
	uint32_t num_of_entities,
	struct eqmhi_cfg_entity *entity);

/**
 * @brief Holding callback functions used for Eqmh Interface/Application
 *        communication.
 */
struct eqmhi_callback
{
	/** Callback function for load. */
	eqmhi_callback_func         load_callback;
	/** Callback function for boot. */
	eqmhi_callback_func         boot_callback;
	/** Callback function for stop. */
	eqmhi_callback_func         stop_callback;
	/** Callback function for dump. */
	eqmhi_callback_func         dump_callback;
	/** Callback function for reload. */
	eqmhi_callback_func         reload_callback;
	/** Callback function for subscribe faults. */
	eqmhi_callback_func         subscribe_faults_callback;
	/** Callback function for fault. */
	eqmhi_fault_ind_func        fault_callback;
	/** Callback function for set configuration. */
	eqmhi_callback_func         set_cfg_callback;
	/** Callback function for set configuration. */
	eqmhi_get_cfg_callback_func get_cfg_callback;
};

#endif /* EQMHI_COMMON_H */

#ifdef __cplusplus
} /* extern "C" */
#endif
