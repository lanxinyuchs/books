#include <itc.h>
#include <time.h>
#include <pthread.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include "timeout_server.h"

/**********************************************************************
 ***** My little timeout server
 **********************************************************************/
#define TRACEPOINT_PROVIDER  com_ericsson_xcs_rhd_timeout_server
#include "tpt_create.h"
#include "tpt.h"

struct tmo_entry {
	struct tmo_entry *next;
	unsigned long long tmo;
	itc_mbox_id_t receiver;
	union itc_msg *tmo_msg;
};


#define TMO_REGISTER_REQ  0x12121201
#define TMO_REGISTER_CFM  0x12121202
#define TMO_CANCEL_REQ    0x12121203
#define TMO_CANCEL_CFM    0x12121204

struct tmo_register_req_s {
	uint32_t msgno;
	unsigned long long tmo;
	union itc_msg *msg;
	itc_mbox_id_t receiver;
};
struct tmo_register_cfm_s {
	uint32_t msgno;
	void *handle;
};

struct tmo_cancel_req_s {
	uint32_t msgno;
	void *handle;
};
struct tmo_cancel_cfm_s {
	uint32_t msgno;
};

union itc_msg {
	uint32_t msgno;
	struct tmo_register_req_s tmo_register_req;
	struct tmo_register_cfm_s tmo_register_cfm;
	struct tmo_cancel_req_s tmo_cancel_req;
	struct tmo_cancel_cfm_s tmo_cancel_cfm;
};


static itc_mbox_id_t mbox_tmo = ITC_NO_ID;
static struct tmo_entry *tmo_list = NULL;


static void *timeout_server(void *mbox_name)
{
	union itc_msg *msg = NULL;
	union itc_msg *reply = NULL;
	struct tmo_entry *entry = NULL;
	struct tmo_entry *new_entry = NULL;
	struct tmo_entry *prev_entry = NULL;

	struct timespec ts_now;
	unsigned long long now;

	mbox_tmo = itc_create_mailbox(mbox_name, 0);
	if (mbox_tmo == ITC_NO_ID) {
                TPT_ERROR("Unable to create ITC mailbox!");
		return NULL;
	}

	for (;;) {
		clock_gettime(CLOCK_MONOTONIC, &ts_now);
		now = TIMESPEC_TO_MS(ts_now);

		if(!tmo_list) {
			/* No pending timeouts - wait forever. */

			msg = itc_receive( ITC_NOFILTER, ITC_NO_TMO, ITC_FROM_ALL );
		} else {
			if( now < tmo_list->tmo ) {
				msg = itc_receive(ITC_NOFILTER,
				                  (uint32_t)(tmo_list->tmo - now),
				                  ITC_FROM_ALL);
			}
		}
		if(msg) {
			switch(msg->msgno) {
			case TMO_REGISTER_REQ:
				new_entry = malloc(sizeof(struct tmo_entry));
				if(!new_entry) {
					TPT_ERROR("Cannot allocate timeout entry, closing down timeout server!");
					itc_free(&(msg->tmo_register_req.msg));
					goto tmo_exit;
				}

				new_entry->tmo      = msg->tmo_register_req.tmo;
				new_entry->tmo_msg  = msg->tmo_register_req.msg;
				new_entry->receiver = msg->tmo_register_req.receiver;
				new_entry->next = NULL;

				prev_entry = NULL;
				entry = tmo_list;
				while(entry) {
					if( entry->tmo > new_entry->tmo ) {
						new_entry->next = entry;
						break; /* break from while */
					}
					prev_entry = entry;
					entry = entry->next;
				}

				if(prev_entry) {
					prev_entry->next = new_entry;
				} else {
					tmo_list = new_entry;
				}

				reply = itc_alloc(sizeof(struct tmo_register_cfm_s), TMO_REGISTER_CFM);
				reply->tmo_register_cfm.handle = new_entry;
				itc_send(&reply, itc_sender(msg), ITC_MY_MBOX);
				break;

			case TMO_CANCEL_REQ:
				if(tmo_list == msg->tmo_cancel_req.handle) {
					tmo_list = ((struct tmo_entry *)msg->tmo_cancel_req.handle)->next;
					itc_free( &((struct tmo_entry *)msg->tmo_cancel_req.handle)->tmo_msg );
					free( msg->tmo_cancel_req.handle );
				} else {
					entry = tmo_list;
					while(entry) {
						if(entry->next == msg->tmo_cancel_req.handle) {
							entry->next = ((struct tmo_entry *)msg->tmo_cancel_req.handle)->next;
							itc_free( &((struct tmo_entry *)msg->tmo_cancel_req.handle)->tmo_msg );
							free( msg->tmo_cancel_req.handle );
							break; /*break out of while loop*/
						}
						entry = entry->next;
					}
				}
				reply = itc_alloc(sizeof(uint32_t), TMO_CANCEL_CFM);
				itc_send(&reply, itc_sender(msg), ITC_MY_MBOX);
				break;
			}

			entry = tmo_list;


			itc_free(&msg);
		} else { /*if(msg)*/
			if(!tmo_list) /* Just to be really sure.. We should never end up here without an entry.*/
				continue;
			clock_gettime(CLOCK_MONOTONIC, &ts_now);
			now = TIMESPEC_TO_MS(ts_now);
			if(now < tmo_list->tmo) /* Just to be really sure.. We should nor end up here uless a tmo has expired.*/
				continue;
			entry = tmo_list;
			itc_send(&entry->tmo_msg, entry->receiver, ITC_MY_MBOX);
			tmo_list = entry->next;
			free(entry);
		}
	}

tmo_exit:
	if(msg)
		itc_free(&msg);

	while(tmo_list) {
		entry = tmo_list->next;
		itc_free(&tmo_list->tmo_msg);
		free(tmo_list);
		tmo_list = entry;
	}
	if(mbox_tmo != ITC_NO_ID) {
		itc_delete_mailbox(mbox_tmo);
		mbox_tmo = ITC_NO_ID;
	}
	return NULL;
}

int init_tmo_server(char *server_name)
{
	pthread_t indicator_thread;
	int rc = 0;

	if(mbox_tmo != ITC_NO_ID) {
		TPT_INFO("Warning: The timeout server is already started!");
		return -1;
	}
	rc = pthread_create( &indicator_thread, NULL,
	                     &timeout_server, server_name );
	if(rc) {
		TPT_ERROR(STR("Could not create timeout server thread (%d)!", rc));
		return rc;
	}

	/* Waiting for timeout server to start.
	 itc_locate_async finds mailbox before global
	 variable mbox_tmo is set.
	 Since register_tmo and cancel_tmo rely on mbox_tmo,
	 we must wait until it's been set.
	*/
	int i;
	for(i = 5000; mbox_tmo == ITC_NO_ID && i > 0; i-- )
		usleep(1000);

	if(mbox_tmo == ITC_NO_ID) {
		TPT_ERROR("Could not find timeout server mailbox!");
		return -1;
	}
	return 0;
}


void *register_tmo(uint32_t tmo, union itc_msg **tmo_msg )
{
	static uint32_t rx_filter[] = { 1, TMO_REGISTER_CFM };
	union itc_msg *msg = NULL;
	struct timespec ts_now;
	void *handle = NULL;
	if(mbox_tmo == ITC_NO_ID) {
		TPT_INFO( "Timeout server is not running!");
		return NULL;
	}
	if(!tmo_msg) {
		TPT_INFO("No ITC message provided!");
		return NULL;
	}
	clock_gettime(CLOCK_MONOTONIC, &ts_now);

	msg = itc_alloc(sizeof(struct tmo_register_req_s), TMO_REGISTER_REQ);
	msg->tmo_register_req.tmo = TIMESPEC_TO_MS(ts_now) + tmo;
	msg->tmo_register_req.msg = *tmo_msg;
	msg->tmo_register_req.receiver = ITC_MY_MBOX;
	*tmo_msg = NULL;
	itc_send(&msg, mbox_tmo, ITC_MY_MBOX);
	msg = itc_receive( rx_filter, ITC_NO_TMO, ITC_FROM_ALL );
	handle = msg->tmo_register_cfm.handle;
	itc_free(&msg);
	return handle;
}

void *register_tmo_absolute_with_sender(unsigned long long tmo,
					union itc_msg **tmo_msg,
					itc_mbox_id_t mbox)
{
	static uint32_t rx_filter[] = { 1, TMO_REGISTER_CFM };
	union itc_msg *msg = NULL;
	void *handle = NULL;
	if(mbox_tmo == ITC_NO_ID) {
		TPT_INFO("Timeout server is not running!");
		return NULL;
	}
	if(!tmo_msg) {
		TPT_INFO("No ITC message provided!");
		return NULL;
	}
	msg = itc_alloc(sizeof(struct tmo_register_req_s), TMO_REGISTER_REQ);
	msg->tmo_register_req.tmo = tmo;
	msg->tmo_register_req.msg = *tmo_msg;
	msg->tmo_register_req.receiver = mbox;
	*tmo_msg = NULL;
	itc_send(&msg, mbox_tmo, ITC_MY_MBOX);
	msg = itc_receive(rx_filter, ITC_NO_TMO, ITC_FROM_ALL);
	handle = msg->tmo_register_cfm.handle;
	itc_free(&msg);
	return handle;
}

void *register_tmo_absolute(unsigned long long tmo, union itc_msg **tmo_msg)
{
	void *handle;

	handle = register_tmo_absolute_with_sender(tmo, tmo_msg, ITC_MY_MBOX);
	return handle;
}

void cancel_tmo(void *handle)
{
	static uint32_t rx_filter[] = { 1, TMO_CANCEL_CFM };
	union itc_msg *msg = NULL;
	if(handle == NULL)
		return;
	if(mbox_tmo == ITC_NO_ID) {
		TPT_INFO("Timeout server is not running!");
		return;
	}
	msg = itc_alloc(sizeof(struct tmo_cancel_req_s), TMO_CANCEL_REQ);
	msg->tmo_cancel_req.handle = handle;
	itc_send(&msg, mbox_tmo, ITC_MY_MBOX);
	msg = itc_receive( rx_filter, ITC_NO_TMO, ITC_FROM_ALL );
	itc_free(&msg);
	return;
}
