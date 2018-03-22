/**
 * Copyright (C) 2015 by Ericsson AB. All rights reserved. The
 * information in this document is the property of Ericsson. Except
 * as specifically authorized in writing by Ericsson, the receiver
 * of this document shall keep the information contained herein
 * confidential and shall protect the same in whole or in part from
 * disclosure and dissemination to third parties. Disclosure and
 * disseminations to the receiver's employees shall only be made on
 * a strict need to know basis.
 */

/* Program for controlling MUX with a MUX library */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdint.h>
#include <dlfcn.h>

#include <itc.h>


typedef int (*ecb_dl_mux_prog_fn)(void **prog, const char *profile, int channel);
typedef int (*ecb_dl_mux_set_fn)(void *prog);

/* Function pointers for MUX library functions */
static ecb_dl_mux_prog_fn  anp_mux_prog;
static ecb_dl_mux_set_fn   anp_mux_set;
static void               *handle;


static int link_mux_library(const char *library)
{
	char *error_string;

	/* Flush any previous dynamic link errors. */
	dlerror();

	/* Open the dynamic library. */
	handle = dlopen(library, RTLD_NOW);
	if ((error_string = dlerror()) != NULL) {
		printf("dlopen(\"%s\") failed, error: \"%s\"\n",
		       library, error_string);
		return -1;
	}
	if (handle == NULL) {
		printf("dlopen(\"%s\") failed for some unknown reason\n",
		       library);
		return -1;
	}

	/* The assignments used below is the POSIX.1-2003 (Technical
	   Corrigendum 1) workaround; see the Rationale for the
	   POSIX specification of dlsym(). */
	*(void**) (&anp_mux_prog) = dlsym(handle, "ecb_dl_mux_prog");
	if ((error_string = dlerror()) != NULL) {
		printf("dlsym(\"ecb_dl_mux_prog\") failed, error: \"%s\"\n",
		       error_string);
		return -1;
	}
	*(void**) (&anp_mux_set) = dlsym(handle, "ecb_dl_mux_set");
	if ((error_string = dlerror()) != NULL) {
		printf("dlsym(\"ecb_dl_mux_set\") failed, error: \"%s\"\n",
		       error_string);
		return -1;
	}

	return 0;
}


int main(int argc, char *argv[])
{
	itc_mbox_id_t  mid;
	void          *prog;
	int            channel;

	/* Check and extract arguments */
	if (argc != 4) {
		printf("Syntax: %s <MUX library> <profile> <channel>\n",
		       argv[0]);
		exit(EXIT_FAILURE);
	} else if (sscanf(argv[3], "%d", &channel) != 1) {
		printf("%s is not a valid channel\n", argv[3]);
		exit(EXIT_FAILURE);
	}

	/* Link program with the MUX library */
	if (link_mux_library(argv[1]) != 0) {
		exit(EXIT_FAILURE);
	}

	/* ITC is needed for NVPI3 which is used from the MUX library */
	if (itc_init(16, ITC_MALLOC, NULL, ITC_NO_NAMESPACE, 0) != 0) {
		printf("itc_init() failed\n");
		exit(EXIT_FAILURE);
	}
	mid = itc_create_mailbox("anp_mux_test", 0);
	if (mid == ITC_NO_ID) {
		printf("itc_create_mailbox() failed\n");
		exit(EXIT_FAILURE);
	}

	/* Create a MUX program for given profile and channel */
	anp_mux_prog(&prog, argv[2], channel);
	if (prog == NULL) {
		printf("Failed to create MUX program for "
		       "\"%s\" with channel %d\n", argv[2], channel);
		exit(EXIT_FAILURE);
	}

	/* Set the MUX using the MUX program */
	if (anp_mux_set(prog) != 0) {
		printf("MUX program failed to set MUX\n");
		exit(EXIT_FAILURE);
	} else {
		printf("MUX channel %d was successfully set for profile: "
		       "\"%s\"\n", channel, argv[2]);
	}

	dlclose(handle);

	itc_delete_mailbox(mid);

	return 0;
}
