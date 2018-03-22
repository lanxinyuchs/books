#include <stdlib.h>
#include <stdint.h>
#include <pthread.h>

#include "log.h"
#include "client_info.h"

/**
 * Client list.
 */
static LIST_HEAD(clientlist,
		 client_info) _clients = LIST_HEAD_INITIALIZER(_clients);

#define MAX_NUMBER_OF_CLIENTS (1024)
static uint32_t clients_cnt = 0;

/**
 * Unique client id
 */
static uint32_t _client_id = 0;

/**
 * Register a new client
 */
struct client_info *register_client(itc_mbox_id_t messagebox)
{
	if ((clients_cnt + 1) > MAX_NUMBER_OF_CLIENTS) {
		log_info("ABN: exceeded clients limit");
		return NULL;
	}

	struct client_info *info =
		(struct client_info *) malloc(sizeof(struct client_info));
	if (!info) {
		log_info("ABN: can allocate memory for new client!");
		return NULL;
	}

	if (!(_client_id + 1)) {
		log_info("ABN: client_id is wrapped");
		return NULL;
	}
	info->clientid = _client_id++;
	info->mbox = messagebox;
	info->monitorid = 0;
	info->userdata = NULL;

	LIST_INSERT_HEAD(&_clients, info, element);
	clients_cnt++;

	return info;
}

/**
 * Remove a client by the client id.
 */
void remove_client(struct client_info *info)
{
	if (info) {
		LIST_REMOVE(info, element);
		if (clients_cnt) {
			clients_cnt--;
		} else {
			log_info("ABN: no clients to remove");
		}
	} else {
		log_info("ABN: Trying to remove a NULL client!");
	}
	free(info);
}

/**
 * Find a client by its client id.
 */
struct client_info *find_client(uint32_t clientid)
{
	struct client_info *info = _clients.lh_first;

	while (info != NULL && info->clientid != clientid) {
		info = info->element.le_next;
	}
	return info;
}

/**
 * Find a client by its mailbox id.
 */
struct client_info *find_client_by_mbox(itc_mbox_id_t mbox)
{
	struct client_info *info = _clients.lh_first;

	while (info != NULL && info->mbox != mbox) {
		info = info->element.le_next;
	}
	return info;
}

/**
 * Returns the head of the list.
 */
struct client_info *get_list_head()
{
	return _clients.lh_first;
}
