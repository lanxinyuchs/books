
#include <stdlib.h>
#include <pthread.h>

#include "client_info.h"

#define TRACEPOINT_PROVIDER  com_ericsson_xcs_rhd_client_info
#include "tpt_create.h"
#include "tpt.h"
/**
 * Client list.
 */
static LIST_HEAD(clientlist,
                 client_info) _clients = LIST_HEAD_INITIALIZER(_clients);
static pthread_mutex_t _listmutex = PTHREAD_MUTEX_INITIALIZER;

/**
 * Unique client id
 */
static int _client_id = 0;

/**
 * Register a new client
 */
struct client_info *register_client(itc_mbox_id_t messagebox)
{
	struct client_info *info = (struct client_info *) malloc(sizeof(
	                                   struct client_info));
	if (!info) {
		return NULL;
	}

	info->clientid = _client_id++;
	info->mbox = messagebox;
	info->monitorid = 0;
	info->lock = &_listmutex;
	info->userdata = NULL;

	pthread_mutex_lock(&_listmutex);
	LIST_INSERT_HEAD(&_clients, info, element);
	pthread_mutex_unlock(&_listmutex);

	return info;
}

/**
 * Remove a client by the client id.
 */
void remove_client(struct client_info *info)
{
	if (info) {
		pthread_mutex_lock(&_listmutex);
		LIST_REMOVE(info, element);
		pthread_mutex_unlock(&_listmutex);
	} else {
		TPT_INFO("ABN: Trying to remove a NULL client!");
	}
	free(info);
}

/**
 * Find a client by its client id.
 */
struct client_info *find_client(int clientid)
{
	struct client_info *info = _clients.lh_first;

	pthread_mutex_lock(&_listmutex);
	while (info != NULL && info->clientid != clientid) {
		info = info->element.le_next;
	}
	pthread_mutex_unlock(&_listmutex);
	return info;
}

/**
 * Find a client by its mailbox id.
 */
struct client_info *find_client_by_mbox(itc_mbox_id_t mbox)
{
	struct client_info *info = _clients.lh_first;

	pthread_mutex_lock(&_listmutex);
	while (info != NULL && info->mbox != mbox) {
		info = info->element.le_next;
	}
	pthread_mutex_unlock(&_listmutex);
	return info;
}

/**
 * Returns the head of the list.
 */
struct client_info *get_list_head()
{
	return _clients.lh_first;
}

/**
 * Lock the client list.
 */
void lock_client_list()
{
	pthread_mutex_lock(&_listmutex);
}

/**
 * Unlock the client list.
 */
void unlock_client_list()
{
	pthread_mutex_unlock(&_listmutex);
}
