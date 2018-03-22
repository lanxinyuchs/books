#ifndef CLIENT_INFO_H_
#define CLIENT_INFO_H_

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>
#include <sys/queue.h>

#include <itc.h>

struct client_info {
	uint32_t clientid;
	itc_mbox_id_t mbox;
	itc_monitor_id_t monitorid;
	uint16_t protocol_rev;
	void *userdata;	/* opaque user data goes here */
	LIST_ENTRY(client_info) element;
};

/**
 * Pos is the current client_info entry (&client_info).
 * This iterates over the list. The caller should lock the list before performing any operation on it!
 */
#define foreach_client(pos) \
	for (pos = get_list_head(); pos != NULL; pos = pos->element.le_next)

struct client_info *register_client(itc_mbox_id_t messagebox);
void remove_client(struct client_info *client);
struct client_info *find_client(uint32_t clientid);
struct client_info *find_client_by_mbox(itc_mbox_id_t mbox);
struct client_info *get_list_head(void);

#ifdef __cplusplus
}
#endif

#endif /* CLIENT_INFO_H_ */
