/*
 * Copyright (c) 2008-2009, Enea Software AB
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * Redistributions of source code must retain the above copyright notice, this
 * list of conditions and the following disclaimer.
 * Redistributions in binary form must reproduce the above copyright notice,
 * this list of conditions and the following disclaimer in the documentation
 * and/or other materials provided with the distribution.
 * Neither the name of Enea Software AB nor the names of its
 * contributors may be used to endorse or promote products derived from this
 * software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */

#include <stdlib.h>
#include <arpa/inet.h>
#include <errno.h>
#include <string.h>
#include <stddef.h>
#include <syslog.h>
#include <sys/ioctl.h>
#include <sys/time.h>
#include <unistd.h>
#include <pthread.h>
#include <sched.h>

#include <itc.h>

#include "itcgw_msghandler.h"
#include "itcgws.h"

union itc_msg {
	uint32_t msgno;
};

static struct timeval *msec_to_timeval(unsigned long msec, struct timeval *t)
{
        t->tv_sec = msec / 1000;
        t->tv_usec = (msec % 1000) * 1000;
        return t;
}
static int recalc_tmo(unsigned int *tmo, struct timeval *t0, struct timeval *t)
{
        struct timeval now;
        unsigned long dt;

        (void)gettimeofday(&now, 0);

        if (now.tv_usec >= t0->tv_usec) {
                dt =  (now.tv_sec - t0->tv_sec) * 1000 +
                        (now.tv_usec - t0->tv_usec) / 1000;
        } else {
                dt =  (now.tv_sec - 1 - t0->tv_sec) * 1000 +
                        (now.tv_usec + 1000000 - t0->tv_usec) / 1000;
        }

        dt = (dt < *tmo) ? *tmo - dt : 0;
		*tmo = dt;

        (void)msec_to_timeval(dt, t);
		*t0 = now;

	if (dt == 0)
		return -1;
	else
		return 0;
}

static uint32_t *copy_sigselect(struct OseGW_ReceiveRequest *p)
{
        uint32_t *filter;
        size_t filter_len;
        int n;

        filter_len = ntohl(p->sigsel_len);

        if (filter_len == 0)
                return NULL;

        filter = malloc((filter_len + 1) * sizeof(*filter));
        if (filter == NULL) {
                syslog(LOG_ERR, "malloc failed");
                return NULL;
        }
	if (filter_len == 1) {
	        LOG("Gateway client: ReceiveRequest filter == 0");
		*filter = 0; /* Any sig */
	} else {
		*filter = filter_len;

		for (n = 0; n < (int)filter_len; n++) {
			LOG("Gateway client: ReceiveRequest filter[%d] == %u", n, ntohl(p->sigsel_list[n]));
			*(filter + 1 + n) = ntohl(p->sigsel_list[n]);
		}
	}
        return filter;
}

static void *get_command(int s, struct OseGW_TransportHdr *hdr)
{
        void *buf;

        if (recv_data(s, hdr, sizeof(*hdr)) == -1)
                return NULL;

        hdr->payload_type = ntohl(hdr->payload_type);
        hdr->payload_len = ntohl(hdr->payload_len);

        buf = malloc(hdr->payload_len);
        if (buf == NULL)
                return NULL;

        if (recv_data(s, buf, hdr->payload_len) == -1) {
                free(buf);
                return NULL;
        }
        return buf;
}
struct gws_hunter {
	char *hunt_name;
	itc_mbox_id_t mid;
	pthread_t pid;
	int status;
};

static void *gws_hunter_foo(void *data)
{
	struct gws_hunter *gws_hunter = (struct gws_hunter *)data;
        itc_mbox_id_t tmp_mid;

	LOG("Gateway Client: gws_hunter started");

	tmp_mid = itc_create_mailbox("gws_hunter", 0);
        if (tmp_mid == ITC_NO_ID) {
                syslog(LOG_ERR, "itc_create_mailbox() gws_hunter failed");
		gws_hunter->status = OseGW_StatusErr;
		goto done;
	}
	LOG("Gateway Client: gws_hunter mailbox %x", tmp_mid);
	gws_hunter->mid = itc_locate(gws_hunter->hunt_name);
	LOG("Gateway Client: gws_hunter Hunt result mid %x", gws_hunter->mid);
	gws_hunter->status = OseGW_StatusOk;
	itc_delete_mailbox(tmp_mid);
done:
	pthread_exit(NULL);
}

int OseGW_PLT_GenericErrorReply_cbk(int skt, int len, char *payload,
				    struct ClientInfo *cinfo)
{
	return 0;
}

int OseGW_PLT_InterfaceRequest_cbk(int skt, int len, char *payload,
				   struct ClientInfo *cinfo)
{
	struct OseGW_InterfaceRequest *interface_request = NULL;

	interface_request = (struct OseGW_InterfaceRequest *)payload;
	cinfo->client_version = ntohl(interface_request->cli_version);
	cinfo->client_flags = ntohl(interface_request->cli_flags);
	LOG("Linux Gateway daemon client requests interface "
            "specs on socket: %d version: %d flags:%d", skt,
            (int)cinfo->client_version, (int)cinfo->client_flags);

	/* Send the reply */
	return OseGW_PLT_InterfaceReply_cbk(skt, 0, NULL, cinfo);
}

int OseGW_PLT_InterfaceReply_cbk(int skt, int len, char *payload,
				 struct ClientInfo *cinfo)
{
#define NO_OF_PLT 10UL
	OseGW_UL payload_len =
		(sizeof(struct OseGW_InterfaceReply) +
		 (sizeof(OseGW_UL) * (NO_OF_PLT - 1UL)));
	struct OseGW_TransportData *reply = NULL;
	int status = 0;
	int size = 0;

	size = sizeof(struct OseGW_TransportData) + payload_len;
	reply = (struct OseGW_TransportData *)malloc(size);
	if (reply == NULL)
		return -1;

	/* Filling the header */
	reply->hdr.payload_type = htonl(OseGW_PLT_InterfaceReply);
	reply->hdr.payload_len = htonl(payload_len);
	/* Filling the payload */
	reply->payload.interface_reply.status = htonl(OseGW_StatusOk);
	reply->payload.interface_reply.srv_version =
		htonl(OseGW_ProtocolVersion);
	reply->payload.interface_reply.srv_flags =
		htonl(gw_server_flags());
	reply->payload.interface_reply.types_len = htonl(NO_OF_PLT);
	/* Filling the interface specs */
	reply->payload.interface_reply.payload_types[0] =
		htonl(OseGW_PLT_InterfaceRequest);
	reply->payload.interface_reply.payload_types[1] =
		htonl(OseGW_PLT_NameRequest);
	reply->payload.interface_reply.payload_types[2] =
		htonl(OseGW_PLT_LoginRequest);
	reply->payload.interface_reply.payload_types[3] =
		htonl(OseGW_PLT_CreateRequest);
	reply->payload.interface_reply.payload_types[4] =
		htonl(OseGW_PLT_DestroyRequest);
	reply->payload.interface_reply.payload_types[5] =
		htonl(OseGW_PLT_SendRequest);
	reply->payload.interface_reply.payload_types[6] =
		htonl(OseGW_PLT_ReceiveRequest);
	reply->payload.interface_reply.payload_types[7] =
		htonl(OseGW_PLT_HuntRequest);
	reply->payload.interface_reply.payload_types[8] =
		htonl(OseGW_PLT_AttachRequest);
	reply->payload.interface_reply.payload_types[9] =
		htonl(OseGW_PLT_DetachRequest);
	size = sizeof(struct OseGW_TransportHdr) + payload_len;
	status = send(skt, reply, size, 0);
	if (status == size) {
		LOG("Gateway Client: replying interface "
                    "specs on socket %d",skt);
	} else {
		syslog(LOG_INFO, "Linux Gateway daemon failed replying "
		       "interface on socket %d", skt);
		status = -1;
	}

	free(reply);
	return status;
}

int OseGW_PLT_LoginRequest_cbk(int skt, int len, char *payload,
                               struct ClientInfo *cinfo)
{
	return 0;
}

int OseGW_PLT_ChallengeResponse_cbk(int skt, int len, char *payload,
                                    struct ClientInfo *cinfo)
{
	return 0;
}

int OseGW_PLT_ChallengeReply_cbk(int skt, int len, char *payload,
                                 struct ClientInfo *cinfo)
{
	return 0;
}

int OseGW_PLT_LoginReply_cbk(int skt, int len, char *payload,
                             struct ClientInfo *cinfo)
{
	return 0;
}

int OseGW_PLT_CreateRequest_cbk(int skt, int len,
				char *payload, struct ClientInfo *cinfo)
{
	LOG("CreateRequests  on skt: %d version: %d "
            "flags:%d from %s",skt, (int) cinfo->client_version,
            (int) cinfo->client_flags,
				((struct OseGW_CreateRequest*)payload)->my_name);

	cinfo->status = OseGW_StatusOk;

	/* Sending reply */
	return OseGW_PLT_CreateReply_cbk(skt, len, payload, cinfo);
}

int OseGW_PLT_CreateReply_cbk(int skt, int len, char *payload,
                              struct ClientInfo *cinfo)
{

	OseGW_UL payload_len = sizeof(struct OseGW_CreateReply);
	struct OseGW_TransportData *reply = NULL;
	struct OseGW_CreateRequest *create_request =
		(struct OseGW_CreateRequest *) payload;
	int status = 0;
	int size = 0;

	size = sizeof(struct OseGW_TransportHdr) + payload_len;
	reply = (struct OseGW_TransportData *) malloc(size);
	if (reply == NULL) {
		return -1;
	}

	if(itc_init(2, ITC_MALLOC, NULL, ITC_NO_NAMESPACE, 0) != 0) {
                return -1;
        }
	/* install error handler */

	/* Fill the header */
	reply->hdr.payload_type = htonl(OseGW_PLT_CreateReply);
	reply->hdr.payload_len = htonl(payload_len);
	/* Fill the payload */
	reply->payload.create_reply.status = htonl(cinfo->status);
	reply->payload.create_reply.max_sigsize = htonl(MAX_SIGSIZE);
	cinfo->sd = skt;
	cinfo->mbox_id = itc_create_mailbox(create_request->my_name, 0);
	if (cinfo->mbox_id == ITC_NO_ID) {
		syslog(LOG_ERR, "itc_create_mailbox() failed");
		free(reply);
		itc_exit();
		return -1;
	}
	cinfo->mbox_fd = itc_get_fd();
        if (cinfo->mbox_fd == ITC_NO_ID) {
		syslog(LOG_ERR, "itc_create_fd() failed");
                free(reply);
		itc_delete_mailbox(cinfo->mbox_id);
		itc_exit();
		return -1;
	}
	LOG("Gateway Client: create mailbox id %x mailbox name %s", cinfo->mbox_id, create_request->my_name);

	/* mbox_id used instead of pid in itc */
	reply->payload.create_reply.pid = htonl(cinfo->mbox_id);

	/*Send */
	size = sizeof(struct OseGW_TransportHdr) + payload_len;
	status = send(skt, (void *) reply, size, 0);
	if (status == size) {
		LOG("Gateway Client: replying CreateReply "
                    "on socket %d", skt);
	} else {
		syslog(LOG_INFO, "Gateway Client: failed replying "
		       "CreateReply on socket %d", skt);
		status = -1;
	}

	free(reply);
	return status;
}

int OseGW_PLT_DestroyRequest_cbk(int skt, int len, char *payload,
				 struct ClientInfo *cinfo)
{

	return OseGW_PLT_DestroyReply_cbk(skt, len, payload, cinfo);
}

int OseGW_PLT_DestroyReply_cbk(int skt, int len, char *payload,
			       struct ClientInfo *cinfo)
{
	OseGW_UL payload_len = sizeof(struct OseGW_DestroyReply);
	struct OseGW_TransportData *reply = NULL;
	struct OseGW_DestroyRequest *destroy_request = NULL;
	int status = 0;
	int size = 0;

	destroy_request = (struct OseGW_DestroyRequest *) payload;
	size = sizeof(struct OseGW_TransportHdr) + payload_len;
	reply = (struct OseGW_TransportData *) malloc(size);
	if (reply == NULL) {
		return -1;
	}

	reply->hdr.payload_type = htonl(OseGW_PLT_DestroyReply);
	reply->hdr.payload_len = htonl(sizeof(struct OseGW_DestroyReply));
	if (ntohl(destroy_request->pid) == cinfo->mbox_id) {
		LOG("Gateway Client: Destroy: mbox_id: %x",
                    cinfo->mbox_id);
		reply->payload.destroy_reply.status =
			htonl(OseGW_StatusOk);
	} else {
		LOG("Gateway Client: Destroy: "
                    "<unknown pid: %#lx>\n",
                    (long unsigned int) ntohl(destroy_request->pid));
		reply->payload.destroy_reply.status =
			(uint32_t)htonl((uint32_t)OseGW_StatusErr);
	}
	size = sizeof(struct OseGW_TransportHdr) + payload_len;
	status = send(skt, (void *) reply, size, 0);
	if (status == size) {
		LOG("Gateway Client: replying DestroyReply "
                    "on socket %d", skt);
	} else {
		syslog(LOG_INFO,"Gateway Client: failed replying"
		       " DestroyReply on socket %d", skt);
	}

	free(reply);
	itc_delete_mailbox(cinfo->mbox_id);
	itc_exit();
	exit(EXIT_SUCCESS);
}

int OseGW_PLT_SendRequest_cbk(int skt, int len, char *payload,
			      struct ClientInfo *cinfo)
{
	struct OseGW_SendRequest *send_request = NULL;
	union itc_msg     *msg;
	int status = 0;

	send_request = (struct OseGW_SendRequest *) payload;
	send_request->dest_pid = ntohl(send_request->dest_pid);
	send_request->from_pid = ntohl(send_request->from_pid);
	send_request->sig_len = ntohl(send_request->sig_len);
	LOG("Send request in client mid %x from_pid %x dest_pid %x", cinfo->mbox_id, send_request->from_pid, send_request->dest_pid);
	if (send_request->from_pid && send_request->from_pid != cinfo->mbox_id) {
		char name[ITC_NAME_MAXLEN];
		if (!itc_get_name(send_request->from_pid, name, ITC_NAME_MAXLEN)) {
			syslog(LOG_ERR,
			       "Send to remote linx mailbox with sender (0x%08x) in different process",
			       send_request->from_pid);
			return -1;
		}
	}
	msg = itc_alloc(send_request->sig_len, ntohl(send_request->sig_no));
	memcpy(&((char *) msg)[sizeof(msg->msgno)],
	       send_request->sig_data,
	       send_request->sig_len - sizeof(msg->msgno));

	if (send_request->from_pid == 0) {
		itc_send(&msg, send_request->dest_pid, ITC_MY_MBOX);
	} else {
		itc_send(&msg, send_request->dest_pid, send_request->from_pid);
	}
	LOG(" Sent signal: %#x to pid: %#x\n",
            (int) ntohl(send_request->sig_no),
            (int) send_request->dest_pid);
	status = OseGW_PLT_SendReply_cbk(skt, len, payload, cinfo);
	return status;
}

int OseGW_PLT_SendReply_cbk(int skt, int len, char *payload,
                            struct ClientInfo *cinfo)
{
	OseGW_UL payload_len = sizeof(struct OseGW_SendReply);
	struct OseGW_TransportData *reply = NULL;
	int status = 0;
	int size = 0;

	size = sizeof(struct OseGW_TransportHdr) + payload_len;
	reply = (struct OseGW_TransportData *) malloc(size);
	/*Fill the header */
	reply->hdr.payload_type = htonl(OseGW_PLT_SendReply);
	reply->hdr.payload_len = htonl(sizeof(struct OseGW_SendReply));
	/*Fill the payload */
	reply->payload.send_reply.status = htonl(cinfo->status);
	/*Send */
	size = sizeof(struct OseGW_TransportHdr) + payload_len;
	status = send(skt, (void *) reply, size, 0);
	if (status == size) {
	        LOG("Linux Gateway daemon replying SendReply "
                    "on socket %d", skt);
	} else {
		syslog(LOG_INFO, "Linux Gateway daemon failed replying "
		       "SendReply on skt %d", skt);
		status = -1;
	}
	free(reply);
	return status;
}

/*
 * We end up here due to a osegw_receive, osegw_receive_w_tmo or
 * osegw_init_async_receive. While we are waiting for a signal,
 * we must be able to handle "pings" and osegw_cancel_async_receive
 * from the client.
 */
int OseGW_PLT_ReceiveRequest_cbk(int skt, int len, char *payload,
                                 struct ClientInfo *cinfo)
{
	struct OseGW_ReceiveRequest *req;
        struct OseGW_TransportHdr thdr;
        union itc_msg *msg = NULL;
        uint32_t *msg_filter = NULL;
        int nfds, status, size = 0, filter_len;
        fd_set rfds;
        struct timeval tv0, tv, *tvp;
        unsigned int tmo;
        void *buf;

	req = (struct OseGW_ReceiveRequest *)payload;
        filter_len = ntohl(req->sigsel_len);
        buf = NULL;

        /* 0. This may be a osegw_cancel_async_receive... */
        if (filter_len == 0) {
	        LOG("Gateway client: ReceiveRequest filter_len 0");
                msg = NULL;
                size = 0;
                goto out;
        }
	LOG("Gateway client: ReceiveRequest filter_len %d", filter_len);
        /* 1. Setup signal filter that should be used while polling... */
        msg_filter = copy_sigselect(req);
        if (msg_filter == NULL)
                goto e_exit;

        /* 2. Setup time-out... */
	tmo = ntohl(req->timeout);
        if (tmo != (unsigned int)~0) {
		LOG("Gateway client: ReceiveRequequest tmo %d ms", tmo);
                tvp = msec_to_timeval(tmo, &tv);
                if (gettimeofday(&tv0, NULL) == -1)
                        goto e_exit;
        } else
                tvp = NULL; /* Infinite */

  again:

        /* 3. Setup descriptors... */
        FD_ZERO(&rfds);
        FD_SET(cinfo->mbox_fd, &rfds);  /* ITC fd */
        FD_SET(skt, &rfds);      /* TCP socket */
        nfds = cinfo->mbox_fd > skt ? cinfo->mbox_fd : skt;

        /* 4. Wait for a signal, ping or osegw_cancel_async_receive */
        status = select(nfds + 1, &rfds, NULL, NULL, tvp);
        if (status == -1)
                goto e_exit;

        if (status == 0) {
	        LOG("Gateway client: osegw_receive_w_tmo");
                /* osegw_receive_w_tmo has timed out */
                msg = NULL;
                size = 0;
                goto out;
        }

        if (FD_ISSET(cinfo->mbox_fd, &rfds)) {
                /* Mailbox has data but it doesn't have to be
		 * data that the client wants, this is awkvard!
		 * With linx you can set receive filter on the
		 * file descriptor, in itc not so.
		 * If the mailbox have data the fd will trigger
		 * and we're going to buzy loop like crazy here
		 * until the time is up:(
		 */
		msg = itc_receive(msg_filter, 0, ITC_FROM_ALL);
		if (msg == NULL) {
			sched_yield(); /*give up CPU for a while */
                        /* Compensate for time spent in select */
                        if (tvp != NULL) {
                                if (recalc_tmo(&tmo, &tv0, &tv)) {
					/* try one last time before return
					 * in case we sleept for a long time
					 */
					msg = itc_receive(msg_filter, 0, ITC_FROM_ALL);
					if (msg)
						goto done;
					LOG("Gateway client: mbox has data but filter is wrong - give up");
					size = 0;
					goto out;
				}
			}
                        goto again;
		}
	done:
		size = itc_size(msg);
                goto out;
        }
        if (FD_ISSET(cinfo->sd, &rfds)) {
                /* Get command */
                buf = get_command(skt, &thdr);
                if (buf == NULL)
                        goto e_exit;

                switch (thdr.payload_type) {
                case OseGW_PLT_InterfaceRequest:
                        status = OseGW_PLT_InterfaceRequest_cbk(skt, thdr.payload_len, buf, cinfo);
                        if (status == -1)
                                goto e_exit;
                        /* Compensate for the time spent in select */
                        if (tvp != NULL) {
                                if (recalc_tmo(&tmo, &tv0, &tv))
					break;
			}
                        goto again;
                        break;
                case OseGW_PLT_ReceiveRequest:
                        req = (struct OseGW_ReceiveRequest *)buf;
                        filter_len = ntohl(req->sigsel_len);
                        if (filter_len != 0)
                                goto e_exit; /* Only cancel async receive is allowed */
                        msg = NULL;
                        size = 0;
                        goto out;
                        break;
                default:
                        syslog(LOG_INFO, "Gateway protocol violation detected, "
                               "got type %d while in a receive", thdr.payload_type);
                        goto e_exit;
                        break;
                }
        }
  out:
	if (buf)
		free(buf);
	if (msg_filter)
		free(msg_filter);
        return OseGW_PLT_ReceiveReply_cbk(skt, size, (char *)msg, cinfo);

  e_exit:
	if (buf)
		free(buf);
	if (msg_filter)
		free(msg_filter);
        return -1;
}

int OseGW_PLT_ReceiveReply_cbk(int skt, int len, char *payload,
                               struct ClientInfo *cinfo)
{
	union itc_msg *msg = (union itc_msg *)payload;
	int data_len = (msg ? len - sizeof(msg->msgno) : len);
	OseGW_UL payload_len = sizeof(struct OseGW_ReceiveReply) + data_len;
	struct OseGW_TransportData *reply;
	int status = 0;
	int size;

	size = sizeof(struct OseGW_TransportHdr) + payload_len;
	reply = malloc(size);
	if (reply == NULL) {
		syslog(LOG_ERR, "Malloc failure in Receive reply");
		return -1;
	}
	reply->hdr.payload_type = htonl(OseGW_PLT_ReceiveReply);
	reply->hdr.payload_len = htonl(payload_len);
	if (msg) {
		/*Fill the payload */
		reply->payload.receive_reply.status = htonl(OseGW_StatusOk);
		reply->payload.receive_reply.sender_pid =
			htonl(itc_sender(msg));
		reply->payload.receive_reply.addressee_pid =
			htonl(cinfo->mbox_id);
		reply->payload.receive_reply.sig_len = htonl(len);
		reply->payload.receive_reply.sig_no =
			htonl(msg->msgno);
		memcpy(&reply->payload.receive_reply.sig_data[0],
		       &(((char *) msg)[sizeof(msg->msgno)]), data_len);
	} else {
		/* Fill the payload */
		syslog(LOG_ERR, " Received timeout (or was canceled)\n");
		size = sizeof(struct OseGW_ReceiveReply);
		memset(&reply->payload.start_of_payload, 0, size);
		reply->payload.receive_reply.status = htonl(OseGW_StatusOk);
	}

	/*Send */
	size = sizeof(struct OseGW_TransportHdr) + payload_len;
	status = send(skt, (void *) reply, size, 0);
	if (status == size) {
		LOG("Gateway Client: replying ReceiveReply "
                    "on socket %d\n", skt);
	} else {
		syslog(LOG_INFO, "Gateway Client: failed replying "
		       "ReceiveReply on socket %d",  skt);
		status = -1;
	}

	/* Clean up */
	if (msg)
		itc_free(&msg);

	free(reply);
	return status;
}

int OseGW_PLT_HuntRequest_cbk(int skt, int len, char *payload,
                              struct ClientInfo  *cinfo)
{

	return OseGW_PLT_HuntReply_cbk(skt, len, payload, cinfo);
}

int OseGW_PLT_HuntReply_cbk(int skt, int len, char *payload,
                            struct ClientInfo *cinfo)
{
	OseGW_UL payload_len = sizeof(struct OseGW_HuntReply);
	struct OseGW_TransportData *reply = NULL;
	struct OseGW_HuntRequest *hunt_request =
		(struct OseGW_HuntRequest *) payload;
	struct gws_hunter gws_hunter;
	int status = 0;
	int size = 0;

	/*Fill the header */
	size = sizeof(struct OseGW_TransportHdr) + payload_len;
	reply = (struct OseGW_TransportData *) malloc(size);
	if (reply == NULL) {
		syslog(LOG_ERR, "Malloc failure in Hunt reply");
		return -1;
	}
	reply->hdr.payload_type = htonl(OseGW_PLT_HuntReply);
	reply->hdr.payload_len = htonl(sizeof(struct OseGW_HuntReply));
	/* Fill the payload */
	hunt_request->user = ntohl(hunt_request->user);
	hunt_request->sig_len = ntohl(hunt_request->sig_len);
	hunt_request->name_index = ntohl(hunt_request->name_index);

	LOG("Gateway Client: Hunt for %s",
	    &hunt_request->data[hunt_request->name_index]);

	/*
	 * The gateway hunt(...) returns the pid of the hunted process if the
	 * process exist when the hunt is done. The LINX hunt(...) does not so
	 * a hunt/receive_w_tmo/sender is done to get the pid. The gws_hunter
	 * socket is opened to prevent at client from flooding the gw server
	 * with hunt requests that could lead to out-of-memory in the LINX
	 * kernel module. If the hunted process does not exist the hunt is
	 * cleaned up when the gws_hunter socket is closed.
	 */

	if (hunt_request->sig_len != 0) {
		union itc_msg *msg = itc_alloc(hunt_request->sig_len,
					       ntohl(hunt_request->sig_no));

		if (!msg) {
			syslog(LOG_ERR, "Linx alloc failed in Hunt reply");
			free(reply);
			return -1;
		}
		size = hunt_request->sig_len - sizeof(msg->msgno);
		memcpy(&((char *) msg)[sizeof(msg->msgno)],
		       &hunt_request->data[ntohl(hunt_request->sig_index)],
		       size);

		LOG("Gateway Client: locate async for %s, reply sig no %x sig size %u",
		    &hunt_request->data[hunt_request->name_index], ntohl(hunt_request->sig_no), size);

		itc_locate_async(&hunt_request->data[hunt_request->name_index], &msg, ITC_MY_MBOX);
	}
	LOG("Gateway Client: create gws_hunter thread");
	gws_hunter.hunt_name = &hunt_request->data[hunt_request->name_index];
	gws_hunter.status = OseGW_StatusErr;
	if (pthread_create(&gws_hunter.pid, NULL, gws_hunter_foo, &gws_hunter)) {
		syslog(LOG_ERR, "itc_create_mailbox() gws_hunter failed");
		free(reply);
		return OseGW_StatusErr;
	}
	LOG("Gateway Client: wait for gws_hunter thread");
	pthread_join(gws_hunter.pid, NULL);
	LOG("Gateway Client: gws_hunter joined");
	if (gws_hunter.status != OseGW_StatusOk) {
		syslog(LOG_ERR, "gws_hunter thread err exit");
                free(reply);
                return OseGW_StatusErr;
        }
	if (gws_hunter.mid == ITC_NO_ID) {
		/* Client expects zero pid if not found */
		gws_hunter.mid = 0;
	}
	LOG("Gateway Client: Hunt result for %s mid %x ",
	    &hunt_request->data[hunt_request->name_index], gws_hunter.mid);

	reply->payload.hunt_reply.status = htonl(cinfo->status);
	reply->payload.hunt_reply.pid = htonl(gws_hunter.mid);
	/*Send */
	size = sizeof(struct OseGW_TransportHdr) + payload_len;
	status = send(skt, (void *) reply, size, 0);
	if (status == size) {
		LOG("Gateway Client: replying HuntReply "
                    "on socket %d", skt);
	} else {
		syslog(LOG_INFO, "Gateway Clinet: failed replying "
		       "HuntReply on socket %d", skt);
		status = -1;
	}
	free(reply);
	return status;
}

int OseGW_PLT_AttachRequest_cbk(int skt, int len, char *payload,
                                struct ClientInfo *cinfo)
{
	return OseGW_PLT_AttachReply_cbk(skt, len, payload, cinfo);
}

int OseGW_PLT_AttachReply_cbk(int skt, int len, char *payload,
                              struct ClientInfo *cinfo)
{
	OseGW_UL payload_len = sizeof(struct OseGW_AttachReply);
	struct OseGW_TransportData *reply = NULL;
	struct OseGW_AttachRequest *attach_request = NULL;
	int status = 0;
	itc_monitor_id_t mon_ref;
	itc_mbox_id_t mon_mbox_id;
	int size = 0;

	size = sizeof(struct OseGW_TransportHdr) + payload_len;
	reply = (struct OseGW_TransportData *)malloc(size);
	if (reply == NULL) {
		syslog(LOG_ERR, "Malloc failure in Attach reply ");
		return -1;
	}

	attach_request = (struct OseGW_AttachRequest *)payload;
	mon_mbox_id = ntohl(attach_request->pid);
	attach_request->sig_len = ntohl(attach_request->sig_len);
	LOG("Gateway Client: attaching to %#x sig_len %d", mon_mbox_id, attach_request->sig_len);

	if (attach_request->sig_len) {
		union itc_msg *msg = itc_alloc(attach_request->sig_len,
					       ntohl(attach_request->sig_no));

		if (!msg) {
			syslog(LOG_ERR, "ITC alloc failed in Attach reply");
			free(reply);
			return -1;
		}
		LOG("Gateway Client: attach signal no %x len %d",
		    ntohl(attach_request->sig_no), size);
		size = attach_request->sig_len - sizeof(msg->msgno);
		memcpy(&((char *) msg)[sizeof(msg->msgno)],
		       &attach_request->sig_data,
		       size);
		mon_ref = itc_monitor(mon_mbox_id, &msg);
	} else {
		/* give them one anyway and use the default
		 * OSEGW_OS_ATTACH_SIG no, which is what
		 * a gw client expects in this case
		 */
		union itc_msg *msg = itc_alloc(sizeof(uint32_t), OSEGW_OS_ATTACH_SIG);

		mon_ref = itc_monitor(mon_mbox_id, &msg);
	}
	LOG("Gateway Client: attach attref %lx\n", (unsigned long)mon_ref);

	/* Fill the header */
	reply->hdr.payload_type = htonl(OseGW_PLT_AttachReply);
	reply->hdr.payload_len = htonl(sizeof(struct OseGW_AttachReply));
	/* Fill the payload */
	reply->payload.attach_reply.status = htonl(cinfo->status);
	reply->payload.attach_reply.attref = htonl((unsigned long)mon_ref);
	/*Send */
	size = sizeof(struct OseGW_TransportHdr) + payload_len;
	status = send(skt, (void *) reply, size, 0);
	if (status == size) {
		LOG("Gateway Client: replying AttachReply "
                    "on socket %d", skt);
	} else {
		syslog(LOG_INFO, "Gateway Client: failed replying "
		       "AttachReply on socket %d",  skt);
		status = -1;
	}
	free(reply);
	return status;
}

int OseGW_PLT_DetachRequest_cbk(int skt, int len, char *payload,
                                struct ClientInfo *cinfo)
{
	struct OseGW_DetachRequest *detach_request = NULL;
	unsigned long attref;
	itc_monitor_id_t mon_id;
	int status;

	detach_request = (struct OseGW_DetachRequest *)payload;
	attref = (unsigned long)ntohl(detach_request->attref);
	mon_id = (itc_monitor_id_t)attref;

	LOG("Gateway Client: receive DetachReques for attref %lx", attref);
	itc_unmonitor(mon_id);

	status = OseGW_PLT_DetachReply_cbk(skt, len, payload, cinfo);
	return status;
}

int OseGW_PLT_DetachReply_cbk(int skt, int len, char *payload,
                              struct ClientInfo *cinfo)
{
	OseGW_UL payload_len = sizeof(struct OseGW_DetachReply);
	struct OseGW_TransportData *reply = NULL;
	int size = 0;
	int status;

	LOG("Gateway Client: DettachReply_cb ");

	/* Fill the header */
	size = sizeof(struct OseGW_TransportHdr) + payload_len;
	reply = (struct OseGW_TransportData *) malloc(size);
	if (reply == NULL) {
		syslog(LOG_ERR, "Malloc failure in Detach reply");
		return -1;
	}
	reply->hdr.payload_type = htonl(OseGW_PLT_DetachReply);
	reply->hdr.payload_len = htonl(sizeof(struct OseGW_DetachReply));
	/* Fill the payload */
	reply->payload.detach_reply.status = htonl(cinfo->status);
	/*Send */
	size = sizeof(struct OseGW_TransportHdr) + payload_len;
	status = send(skt, (void *)reply, size, 0);
	if (status == size) {
		LOG("Gateway Client: replying DettachReply "
                    "on socket %d", skt);
	} else {
		syslog(LOG_INFO, "Linux Gateway daemon failed replying "
                       "DettachReply on socket %d", skt);
		status = -1;
	}
	free(reply);
	return status;
}

int OseGW_PLT_NameRequest_cbk(int skt, int len, char *payload,
                              struct ClientInfo *cinfo)
{
	return OseGW_PLT_NameReply_cbk(skt, len, payload, cinfo);
}

int OseGW_PLT_NameReply_cbk(int skt, int len, char *payload,
                            struct ClientInfo *cinfo)
{
  OseGW_UL payload_len = offsetof(struct OseGW_NameReply, name) +
                strlen(cinfo->gw_name) + 1;
	struct OseGW_TransportData *reply = NULL;
	int status = 0;
	int size = 0;

	size = sizeof(struct OseGW_TransportHdr) + payload_len;
	reply = (struct OseGW_TransportData *)malloc(size);
	if (reply == NULL) {
		syslog(LOG_ERR, "Malloc failure in Name reply");
		return -1;
	}
	reply->hdr.payload_type = htonl(OseGW_PLT_NameReply);
	reply->hdr.payload_len = htonl(payload_len);
	reply->payload.name_reply.status = htonl(OseGW_StatusOk);
	reply->payload.name_reply.name_len = htonl(strlen(cinfo->gw_name) + 1);
	strcpy(reply->payload.name_reply.name, cinfo->gw_name);
	/*Send */
	size = sizeof(struct OseGW_TransportHdr) + payload_len;
	status = send(skt, (void *) reply, size, 0);
	if (status == size) {
		LOG("Gateway Client: replying NameReply on "
                    "socket %d", skt);
	} else {
		syslog(LOG_INFO, "Gateway Client: failed replying "
		       "NameReply on socket %d", skt);
		status = -1;
	}

	free(reply);
	return status;
}
