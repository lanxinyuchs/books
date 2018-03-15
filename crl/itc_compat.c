/**
 *   [Enter a brief one-liner file comment and leave next line empty.]
 *
 *   @file itc_compat.c
 *
 *   This file exists to make the ITC library version 2 compatible
 *   with version 1. The macros defined in itc.h must be undefined
 *   before we can make function implementations of them.
 *
 *   Copyright (C) 2014 by Ericsson AB. All rights reserved. The
 *   information in this document is the property of Ericsson. Except
 *   as specifically authorized in writing by Ericsson, the receiver
 *   of this document shall keep the information contained herein
 *   confidential and shall protect the same in whole or in part from
 *   disclosure and dissemination to third parties. Disclosure and
 *   disseminations to the receiver's employees shall only be made on
 *   a strict need to know basis.
 */

/* ========================================================================
 *   History of development:
 *   -----------------------
 *   Revised : 2014-08-29 Magnus Lindberg
 *   Change  : First version.
 * ========================================================================
 */

/* ========================================================================
 *   INCLUDE FILES
 * ========================================================================
 */
#include "itc.h"
#include "itc_system.h"

/* ========================================================================
 *   DEFINITIONS
 * ========================================================================
 */

/* ========================================================================
 *   TYPE DEFINITIONS
 * ========================================================================
 */

/* ========================================================================
 *   DATA DECLARATIONS
 * ========================================================================
 */

/* ========================================================================
 *   FUNCTION PROTOTYPES
 * ========================================================================
 */

/* ========================================================================
 *   Since the below functions are all short wrappers only intended to
 *   provide compatibility between the ITC interface versions no function
 *   headers are present. For information about these function please refer
 *   to itc.h.
 * ========================================================================
 */
#undef itc_init
int itc_init(int32_t mailbox_count,
	     itc_alloc_scheme alloc_scheme,
	     union itc_scheme *scheme,
	     char *name_space,
	     uint32_t flags)
{
	return __itc_init(mailbox_count, alloc_scheme,
			  scheme, name_space, flags, "", 0);
}

#undef itc_exit
void itc_exit(void)
{
	__itc_exit("", 0);
}

#undef itc_alloc
union itc_msg *itc_alloc(size_t size,
			 uint32_t msgno)
{
	return __itc_alloc(size, msgno, "", 0);
}

#undef itc_free
void itc_free(union itc_msg **msg)
{
	__itc_free(msg, "", 0);
}

#undef itc_sender
itc_mbox_id_t itc_sender(union itc_msg *msg)
{
	return __itc_sender(msg, "", 0);
}

#undef itc_receiver
itc_mbox_id_t itc_receiver(union itc_msg *msg)
{
	return __itc_receiver(msg, "", 0);
}

#undef itc_size
size_t itc_size(union itc_msg *msg)
{
	return __itc_size(msg, "", 0);
}

#undef itc_setsize
int32_t itc_setsize(union itc_msg *msg,
		    int32_t newsize)
{
	return __itc_setsize(msg, newsize, "", 0);
}

#undef itc_create_mailbox
itc_mbox_id_t itc_create_mailbox(const char *name,
				 uint32_t flags)
{
	return __itc_create_mailbox(name, flags, "", 0);
}

#undef itc_delete_mailbox
void itc_delete_mailbox(itc_mbox_id_t mbox_id)
{
	__itc_delete_mailbox(mbox_id, "", 0);
}

#undef itc_current_mbox
itc_mbox_id_t itc_current_mbox(void)
{
	return __itc_current_mbox("", 0);
}

#undef itc_locate
itc_mbox_id_t itc_locate(const char *name)
{
	return __itc_locate(name, "", 0);
}

#undef itc_locate_async
void itc_locate_async(const char *name,
		      union itc_msg **msg,
		      itc_mbox_id_t from)
{
	__itc_locate_async(name, msg, from, "", 0);
}

#undef itc_get_fd
int itc_get_fd(void)
{
	return __itc_get_fd("", 0);
}

#undef itc_get_name
int32_t itc_get_name(itc_mbox_id_t mbox_id,
		     char *name,
		     uint32_t name_len)
{
	return __itc_get_name(mbox_id, name, name_len, "", 0);
}

#undef itc_get_real_mbox
itc_mbox_id_t itc_get_real_mbox(itc_mbox_id_t mbox_id)
{
	return __itc_get_real_mbox(mbox_id, "", 0);
}

#undef itc_send
void itc_send(union itc_msg **msg,
	      itc_mbox_id_t to,
	      itc_mbox_id_t from)
{
	__itc_send(msg, to, from, "", 0);
}

#undef itc_receive
union itc_msg *itc_receive(const uint32_t *filter,
			   int32_t tmo,
			   itc_mbox_id_t from)
{
	return __itc_receive(filter, tmo, from, "", 0);
}

#undef itc_monitor
itc_monitor_id_t itc_monitor(itc_mbox_id_t who,
			     union itc_msg **msg)
{
	return __itc_monitor(who, msg, "", 0);
}

#undef itc_unmonitor
void itc_unmonitor(itc_monitor_id_t monitor_id)
{
	__itc_unmonitor(monitor_id, "", 0);
}

#undef itc_register_errorhandler
void itc_register_errorhandler(itc_errorhandler errh)
{
	__itc_register_errorhandler(errh, "", 0);
}

#undef itc_install_hooks
void itc_install_hooks(itc_hook    send_hook,
		       void       *send_user,
		       itc_hook    recv_hook,
		       void       *recv_user)
{
	__itc_install_hooks(send_hook, send_user,
			    recv_hook, recv_user, "", 0);
}

#undef itc_assign_linkhandler
void itc_assign_linkhandler(char *lnhpath,
			    itc_mbox_id_t mbox_id)
{
	__itc_assign_linkhandler(lnhpath, mbox_id, "", 0);
}

#undef itc_deassign_linkhandler
void itc_deassign_linkhandler(char *lnhpath,
			      itc_mbox_id_t mbox_id)
{
	__itc_deassign_linkhandler(lnhpath, mbox_id, "", 0);
}

#undef itc_clone_mailbox
itc_mbox_id_t itc_clone_mailbox(itc_mbox_id_t mbox_id,
				const char *name)
{
	return __itc_clone_mailbox(mbox_id, name, "", 0);
}

#undef itc_add_name
int itc_add_name(itc_mbox_id_t mbox_id,
                          const char *name)
{
	return __itc_add_name(mbox_id, name, "", 0);
}

#undef itc_receive_mbox
union itc_msg *itc_receive_mbox(itc_mbox_id_t mbox_id,
                                  const uint32_t *filter,
                                  int32_t tmo)
{
	return __itc_receive_mbox(mbox_id, filter, tmo, "", 0);
}

#undef itc_remove
union itc_msg *itc_remove(itc_mbox_id_t mbox_id,
                            union itc_msg *msg)
{
	return __itc_remove(mbox_id, msg, "", 0);
}

#undef itc_get_mboxes
void itc_get_mboxes(void)
{
	__itc_get_mboxes("", 0);
}

#undef itc_get_mailbox_info
struct itc_mbox_info *itc_get_mailbox_info(itc_mbox_id_t mbox_id)
{
	return __itc_get_mailbox_info(mbox_id, "", 0);
}

#undef itc_get_alloc_info
struct itc_alloc_info *itc_get_alloc_info(itc_mbox_id_t mbox_id)
{
	return __itc_get_alloc_info(mbox_id, "", 0);
}

#undef itc_call_errh
void itc_call_errh(char *errtext,
		   uint32_t flags)
{
	__itc_call_errh(errtext, flags, "", 0);
}
