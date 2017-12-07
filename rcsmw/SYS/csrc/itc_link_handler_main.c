/**********************************************************************
 * %CCaseFile:	itc_link_handler_main.c %
 * %CCaseRev:	/main/R4A/R5A/1 %
 * %CCaseDate:	2016-03-02 %
 * %CCaseDocNo:  %
 *
 * %CCaseCopyrightBegin%
 * Copyright (c) Ericsson AB 2015-2016 All rights reserved.
 *
 * The information in this document is the property of Ericsson.
 *
 * Except as specifically authorized in writing by Ericsson, the
 * receiver of this document shall keep the information contained
 * herein confidential and shall protect the same in whole or in
 * part from disclosure and dissemination to third parties.
 *
 * Disclosure and disseminations to the receivers employees shall
 * only be made on a strict need to know basis.
 * %CCaseCopyrightEnd%
 *
 * **********************************************************************
 * Rev      Date       Name        What
 * -----    -------    --------    --------------------------
 * R4A/1    20150521   etxpeno     Created
 * R5A/1    20160302   etxpeno     update the internal signal numbers
 * **********************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <syslog.h>
#include <mdu-link-api.h>
#include <itc.h>

#define TRACEPOINT_DEFINE
#include "sys_trace.h"

#define INFO(fmt, ...)  TRACE_HELPER(info_trace, fmt, __VA_ARGS__)
#define ERROR(fmt, ...) TRACE_HELPER(error_trace, fmt, __VA_ARGS__)

#define TRACE_HELPER(type, fmt, ...) do {                               \
    char *err_str;                                                      \
    asprintf(&err_str,"%s:%d:%s():" fmt, __FILE__,__LINE__, __func__, __VA_ARGS__);                                                             \
    tracepoint(com_ericsson_rcs_sys, type, err_str);                    \
    free(err_str);                                                      \
  } while(0)


#define MAX_ITC_LINK_NAME_LEN 80

#define ITC_LNH_CTL_CREATE_LINK      0x01800100
struct itc_lnh_ctl_create_link {
  uint32_t  msgno;
  uint16_t  vlan_id;
  uint8_t   peer_mac[6];
  char      name[MAX_ITC_LINK_NAME_LEN];
};

#define ITC_LNH_CTL_CREATE_LINK_RES  0x01800101
struct itc_lnh_ctl_create_link_res {
  uint32_t msgno;
  uint32_t link_id;
  int32_t  result;
};

#define ITC_LNH_CTL_DESTROY_LINK     0x01800102
struct itc_lnh_ctl_destroy_link {
  uint32_t msgno;
  uint32_t link_id;
};

#define ITC_LNH_CTL_DESTROY_LINK_RES 0x01800103
struct itc_lnh_ctl_destroy_link_res {
  uint32_t msgno;
  int32_t  result;
};

#define ITC_LNH_CTL_LINKSTATE_IND    0x01800104
struct itc_lnh_ctl_linkstate_ind {
  uint32_t msgno;
  uint32_t link_id;
  int32_t  state;
};

union itc_msg {
  uint32_t                        msgno;
  struct mdu_linkstate_ind        mdu_linkstate_ind;
  struct itc_lnh_ctl_create_link  itc_lnh_ctl_create_link;
  struct itc_lnh_ctl_destroy_link itc_lnh_ctl_destroy_link;

  struct itc_lnh_ctl_create_link_res   itc_lnh_ctl_create_link_res;
  struct itc_lnh_ctl_destroy_link_res  itc_lnh_ctl_destroy_link_res;
  struct itc_lnh_ctl_linkstate_ind     itc_lnh_ctl_linkstate_ind;
};

int
main()
{
  int result = EXIT_SUCCESS;
  itc_mbox_id_t mbox_id, mbox_id_erl;
  union itc_msg *msg, *respmsg;
  struct mdu_if *handle;
  bool living = true;
  int res;

  openlog("itc_link_handler", LOG_PID, LOG_USER);

  INFO("Calling itc_init",0);
  res = itc_init(1, ITC_MALLOC, NULL, ITC_NO_NAMESPACE, 0);
  if (res != 0)
    {
      ERROR("itc_init returned %d", res);
      syslog(LOG_ERR, "itc_init returned %d", res);
      return EXIT_FAILURE;
    }

  INFO("Calling itc_create_mailbox", 0);
  mbox_id = itc_create_mailbox("itc_lnh_ctl_c", 0);

  INFO("Calling itc_locate",0);
  mbox_id_erl = itc_locate("itc_lnh_ctl_erl");
  if (mbox_id_erl == ITC_NO_ID)
    {
      ERROR("itc_locate returned %d", mbox_id_erl);
      syslog(LOG_ERR, "itc_locate returned %d", mbox_id_erl);
      result = EXIT_FAILURE;
      goto ERROR;
    }

  INFO("Calling itc_monitor",0);
  itc_monitor(mbox_id_erl, NULL);

  INFO("Calling mdu_link_init",0);
  if (mdu_link_init(&handle) != 0) {
    ERROR("Call to mdu_link_init failed",0);
    syslog(LOG_ERR, "Call to mdu_link_init failed");
    result = EXIT_FAILURE;
    goto ERROR;
  }

  while (living) {
    INFO("Calling itc_receive",0);
    msg = itc_receive(ITC_NOFILTER, ITC_NO_TMO, ITC_FROM_ALL);
    switch(msg->msgno) {
    case ITC_MONITOR_DEFAULT_NO:
      INFO("Received ITC_MONITOR_DEFAULT_NO",0);
      living = false;
      break;
    case MDU_LINKSTATE_IND:
      INFO("Received MDU_LINKSTATE_IND",0);
      respmsg = itc_alloc(sizeof(struct itc_lnh_ctl_linkstate_ind),
			  ITC_LNH_CTL_LINKSTATE_IND);
      respmsg->itc_lnh_ctl_linkstate_ind.link_id =
	msg->mdu_linkstate_ind.link_id;
      respmsg->itc_lnh_ctl_linkstate_ind.state = msg->mdu_linkstate_ind.state;
      itc_send(&respmsg, itc_sender(msg), ITC_MY_MBOX);
      break;
    case ITC_LNH_CTL_CREATE_LINK:
      {
	uint32_t link_id;
	char *name;
	struct mdu_link_config cfg;
	int result;

	INFO("Received ITC_LNH_CTL_CREATE_LINK",0);

	name = msg->itc_lnh_ctl_create_link.name;
	cfg.vlan_id = msg->itc_lnh_ctl_create_link.vlan_id;
	memcpy(cfg.peer_mac, msg->itc_lnh_ctl_create_link.peer_mac, 6);
	result = mdu_link_create(handle, name, &cfg, &link_id);
	respmsg = itc_alloc(sizeof(struct itc_lnh_ctl_create_link_res),
			    ITC_LNH_CTL_CREATE_LINK_RES);
	respmsg->itc_lnh_ctl_create_link_res.link_id = link_id;
	respmsg->itc_lnh_ctl_create_link_res.result = result;
	itc_send(&respmsg, itc_sender(msg), ITC_MY_MBOX);
	break;
      }
    case ITC_LNH_CTL_DESTROY_LINK:
      {
	uint32_t link_id;
	int result;

	INFO("Received ITC_LNH_CTL_DESTROY_LINK",0);

	link_id = msg->itc_lnh_ctl_destroy_link.link_id;
	result = mdu_link_destroy(handle, link_id);
	respmsg = itc_alloc(sizeof(struct itc_lnh_ctl_destroy_link_res),
			    ITC_LNH_CTL_DESTROY_LINK_RES);
	respmsg->itc_lnh_ctl_destroy_link_res.result = result;
	itc_send(&respmsg, itc_sender(msg), ITC_MY_MBOX);
	break;
      }
    default:
      break;
    }
    itc_free(&msg);
  }

 ERROR:
  INFO("Calling itc_delete_mailbox", 0);
  itc_delete_mailbox(mbox_id);
  INFO("Calling itc_exit",0);
  itc_exit();

  return result;
}
