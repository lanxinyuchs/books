#ifndef _DAEMON_H_
#define _DAEMON_H_

#include <signal.h>
#include <stdint.h>

#define HWDB_ENV "sys_hwdb_dev"
#define FWDB_ENV "sys_fwdb"

/* Both ECPX and LH are interested in this number since it
   constitutes the number of possible channel identities (CID)
   available externally. */
#define RHD_NOF_ECP_ADDR 64

typedef void (*exitfunction)();
typedef void (*rhd_exitfunction)();

struct server_info {
	uint32_t server_mbox;
	uint32_t server_ref;
	uint32_t selected_version;
};


#define set_exit_function rhd_set_exit_function
#define set_signal_handlers rhd_set_signal_handlers
void rhd_set_exit_function(rhd_exitfunction f);
void rhd_set_signal_handlers(int foreground, __sighandler_t h);
int rhd_try_lock(const char *name);

#endif //_DAEMON_H_
