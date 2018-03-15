/******************************************************************************
 *
 *      COPYRIGHT (C)                 Ericsson Radio Systems AB, Sweden
 *
 *      The copyright to the computer program(s) herein is the property
 *      of Ericsson Radio Systems AB.
 *
 *      The program(s) may be used and/or copied only with the written
 *      permission from Ericsson Radio Systems AB or in accordance with
 *      the terms and conditions stipulated in the agreement/contract
 *      under which the program(s) have been supplied.
 *
 *****************************************************************************/

#ifndef ECB_H_
#define ECB_H_

#include <stdint.h>
#include <termios.h>

#include "ecb_mux.h"
#include "ecb_unc.h"
#include "ecb_dev.h"

struct ecb_ctx {

	struct {
		int            fd; /* Device file descriptor. */
		struct termios ts; /* Device setting. */
	} dev;

	struct {
		void        *handle;  /* Handle for the dynamic library. */
		void        *prog;    /* Stored MUX program. */
		ecb_dl_mux_prog_fn prog_fn;
		ecb_dl_mux_set_fn  set_fn;
	} mux;

	struct {
                uint32_t     fcs_be;   /* FCS Endian. */
		uint32_t     n_oflags; /* Number of opening flags. */
                struct ecb_unc_opt_prefix prefix; /* Prefix. */
	} unc;

	char *log_prefix;

};

struct ecb_monitor_ctx {

	ecb_dev_mon monitor;
	pthread_t mon_thread;
	void *client_ref;
	char *file_name;
	char *file_path;
};


#endif
