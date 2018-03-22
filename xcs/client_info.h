#ifndef CLIENT_INFO_H_
#define CLIENT_INFO_H_

#ifdef __cplusplus
extern "C" {
#endif

#include <itc.h>
#include <sys/queue.h>
#include <pthread.h>

struct client_info {
	int clientid;
	itc_mbox_id_t mbox;
	itc_monitor_id_t monitorid;
	pthread_mutex_t *lock;
	void *userdata;         /* opaque user data goes here */
	LIST_ENTRY(client_info) element;
};

/**
 * Pos is the current client_info entry (&client_info).
 * This iterates over the list. The caller should lock the list before performing any operation on it!
 */
#define foreach_client(pos)     \
	for (pos = get_list_head(); pos != NULL; pos = pos->element.le_next)


struct client_info *register_client(itc_mbox_id_t messagebox);
void remove_client(struct client_info *client);
struct client_info *find_client(int clientid);
struct client_info *find_client_by_mbox(itc_mbox_id_t mbox);
struct client_info *get_list_head();
void lock_client_list();
void unlock_client_list();

#ifdef __cplusplus
}
#endif

#endif /* CLIENT_INFO_H_ */
