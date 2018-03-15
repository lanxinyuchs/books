#include "itc.h"

void *loopback_thread(void *data);

int create_loopback(char *name, pthread_t *tid, itc_mbox_id_t *mbox_id);
int procthread(char *namespace, char *name);
