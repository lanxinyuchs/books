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

#include <stdio.h>
#include <stdlib.h>
#include <dlfcn.h>

#include "ecb_mux.h"

#include "ecb.h"

#include "log.h"

int ecb_mux_init(void *handle, const char *library)
{
	struct ecb_ctx *ctx = (struct ecb_ctx*) handle;
	char           *error_string;

	if (!ctx) {
		log_err(ECB_DEFAULT_PREFIX, "ecb_mux_init() called with NULL pointer");
		return -1;
	}

	/* Flush any previous dynamic link errors. */
	dlerror();

	if (ctx->mux.handle) {
		dlclose(ctx->mux.handle);
		if ((error_string = dlerror()) != NULL) {
			log_err(ctx->log_prefix, "dlclose() failed, error: \"%s\"",
			       error_string);
			return -1;
		}
	}

	/* Open the dynamic library. */
	ctx->mux.handle = dlopen(library, RTLD_NOW);
	if ((error_string = dlerror()) != NULL) {
		log_err(ctx->log_prefix, "dlopen(\"%s\") failed, error: \"%s\"",
		       library, error_string);
		return -1;
	}

	/* The assignments used below is the POSIX.1-2003 (Technical
	   Corrigendum 1) workaround; see the Rationale for the
	   POSIX specification of dlsym(). */
	*(void**) (&ctx->mux.prog_fn) = dlsym(ctx->mux.handle,
	                                      "ecb_dl_mux_prog");
	if ((error_string = dlerror()) != NULL) {
		log_err(ctx->log_prefix,
		       "dlsym(\"ecb_dl_mux_prog\") failed, error: \"%s\"",
		       error_string);
		return -1;
	}
	*(void**) (&ctx->mux.set_fn) = dlsym(ctx->mux.handle,
	                                     "ecb_dl_mux_set");
	if ((error_string = dlerror()) != NULL) {
		log_err(ctx->log_prefix,
		       "dlsym(\"ecb_dl_mux_set\") failed, error: \"%s\"",
		       error_string);
		return -1;
	}

	return 0;
}

int ecb_mux_prog(void *handle, const char *profile, int channel)
{
	struct ecb_ctx *ctx = (struct ecb_ctx*) handle;

	if (ctx) {
		if (ctx->mux.prog_fn) {
			return ctx->mux.prog_fn(&ctx->mux.prog,
						profile,
						channel);
		}
	} else {
		log_err(ECB_DEFAULT_PREFIX, "ecb_mux_prog() called with NULL pointer");
		return -1;
	}

	return 1;
}

int ecb_mux_set(void *handle)
{
	struct ecb_ctx *ctx = (struct ecb_ctx*) handle;

	if (ctx) {
		if (ctx->mux.set_fn) {
			return ctx->mux.set_fn(ctx->mux.prog);
		}
	} else {
		log_err(ECB_DEFAULT_PREFIX, "ecb_mux_set() called with NULL pointer");
		return -1;
	}

	return 1;
}
