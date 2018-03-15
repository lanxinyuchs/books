/**
 *   Header file for LTTng health wrappers.
 *
 *   Copyright (C) 2016 by Ericsson AB. All rights reserved. The
 *   information in this document is the property of Ericsson. Except
 *   as specifically authorized in writing by Ericsson, the receiver
 *   of this document shall keep the information contained herein
 *   confidential and shall protect the same in whole or in part from
 *   disclosure and dissemination to third parties. Disclosure and
 *   disseminations to the receiver's employees shall only be made on
 *   a strict need to know basis.
 */

/* ========================================================================
 *   History of development:
 *   -----------------------
 *   Revised : 2016-01-12 Anette Schött
 *   Change  : First version.
 *
 *   Revised : 2016-04-21 Fredrik Skog
 *   Change  : Added thread_name to struct WrapValues.
 *
 * ========================================================================
 */

#ifndef LTTNG_WRAPPERS_H
#define LTTNG_WRAPPERS_H

/* ========================================================================
 *   INCLUDE FILES
 * ========================================================================
 */

#include <lttng/health.h>

/* ========================================================================
 *   DEFINITIONS
 * ========================================================================
 */

#define PATH_MAX (256)

/* ========================================================================
 *   TYPE DEFINITIONS
 * ========================================================================
 */

enum health_component {
	HEALTH_COMPONENT_SESSIOND,
	HEALTH_COMPONENT_CONSUMERD,
	HEALTH_COMPONENT_RELAYD,

	NR_HEALTH_COMPONENT,
};

struct lttng_health_thread {
	struct lttng_health *p;
	int state;
};

struct lttng_health {
	enum health_component component;
	uint64_t state;
	unsigned int nr_threads;
	char health_sock_path[PATH_MAX];
	/* For consumer health only */
	enum lttng_health_consumerd consumerd_type;
	struct lttng_health_thread thread[];
};

typedef struct {
   int query;
   int state;
   bool wrap_get_nr_threads;
   int get_nr_threads;
   bool wrap_thread_struct;
   struct lttng_health_thread *thread_struct;
   int thread_state;
   int thread_name;
} WrapValues;


/* ========================================================================
 *   DATA DECLARATIONS
 * ========================================================================
 */

/* ========================================================================
 *   FUNCTIONS
 * ========================================================================
 */

#endif /* LTTNG_WRAPPERS_H */
