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
 * @file eqmhi_bb.h
 * @brief BB Equipment Handling service interface
 *
 * This file defines the BB public EQMH interface (EQMHI)
 *
 ******************************************************************************/

#ifdef __cplusplus
extern "C" {
#endif


#ifndef EQMHI_BB_H
#define EQMHI_BB_H


/* > Includes *****************************************************************/
#include <stdint.h>
#include <stdbool.h>

#include "eqmhi_common.h"

/* > Defines ******************************************************************/

/**
 * @ingroup eqmhi_cfg_type_t
 * BB specific configuration of stripped mode.
 */
#define EQMHI_CFG_BB_STRIPPED EQMHI_CFG_BB_BASE

/**
 * @ingroup eqmhi_cfg_type_t
 * BB specific configuration of xio for LRAT.
 */
#define EQMHI_CFG_BB_XIO (EQMHI_CFG_BB_BASE + 1)

/**
 * @ingroup eqmhi_cfg_type_t
 * BB specific configuration of BB instance name.
 * It is a symbolic name for the EQMH instance returned by eqmhi_create.
 */
#define EQMHI_CFG_BB_INSTANCE_NAME (EQMHI_CFG_BB_BASE + 2)

/**
 * Total number of configuration types for BB
 */
#define TOT_NUM_OF_CFG_TYPES 5

/* > Type Declarations ********************************************************/

/**
 * @brief Holding configuration specification for stripped mode.
 */
struct eqmhi_cfg_stripped_mode
{
	/** Configuration type. */
	eqmhi_cfg_type_t type;

	/** Status (on(1)/off(0)) of stripped mode */
	bool status;
};

/**
 * @brief Holding configuration specification for xio configuration.
 *        Only to be used by LRAT
 */
struct eqmhi_cfg_xio
{
	/** Configuration type. */
	eqmhi_cfg_type_t type;
};

/**
 * @brief Holding configuration specification for instance name.
 */
struct eqmhi_cfg_instance_name
{
	/** Configuration type. */
	eqmhi_cfg_type_t type;
	/** Instance name, for example "LRAT/A", "LRAT/B" or "GRAT/A". */
	char instance_name[EQMHI_MAX_STRING_LENGTH];
};

/**
 * @brief Holding configuration specification for entity.
 */
struct eqmhi_cfg_entity
{
	/** EQM identity. */
	uint32_t eqm_id;
	/** Configuration */
	union
	{
		/** Configuration type. */
		eqmhi_cfg_type_t type;
		/** Configuration for dump generation. */
		struct eqmhi_cfg_dump_file dump_file;
		/** Configuration for dump of external memory. */
		struct eqmhi_cfg_dump_em dump_em;
		/** Configuration for stripped mode. */
		struct eqmhi_cfg_stripped_mode stripped_mode;
		/** Configuration for xio. */
		struct eqmhi_cfg_xio xio;
		/** Configuration for instance name. */
		struct eqmhi_cfg_instance_name instance_name;
	} cfg;
};

#endif /* EQMHI_BB_H */

#ifdef __cplusplus
} /* extern "C" */
#endif
