/* ----------------------------------------------------------------------
 * %CCaseFile:	ecoli_app.c %
 * %CCaseRev:	/main/R4A/R5A/1 %
 * %CCaseDate:	2016-01-04 %
 * %CCaseDocNo: %
 * Author:	eolaand
 *
 * Short description:
 * A simple application that tests the PM C interface, including show counters.
 * ----------------------------------------------------------------------
 * %CCaseTemplateFile:	template.c %
 * %CCaseTemplateId: CCver: /main/1 %
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
 * ----------------------------------------------------------------------
 *
 * Revision history:
 *
 * Rev        Date       Name        What
 * -----      -------    --------    --------------------------
 * R4A/1      2015-06-24 uabesvi     Created
 * ----------------------------------------------------------------------
 */

#define _GNU_SOURCE

#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <unistd.h>
#include <time.h>
#include <sys/select.h>

#include <pthread.h>
#include <errno.h>
#include <string.h>
#include <stdint.h>

#include "itc.h"
#include "ecoli_coli.sig"


#define VERBOSE true
#define VERBOSE_FILE stdout
#define LEASHING false

void print_f(char *fmt) { if (VERBOSE) fprintf(VERBOSE_FILE, fmt); }
void print_fs(char *fmt, char *s) { if (VERBOSE) fprintf(VERBOSE_FILE, fmt, s); }
void print_fd(char *fmt, int d) { if (VERBOSE) fprintf(VERBOSE_FILE, fmt, d); }
void print_fss(char *fmt, char *s, char *t) { if (VERBOSE) fprintf(VERBOSE_FILE, fmt, s, t); }
void print_fsd(char *fmt, char *s, int d) { if (VERBOSE) fprintf(VERBOSE_FILE, fmt, s, d); }

union itc_msg {
  uint32_t                  msg_no;
  ColiInterfaceRegistration register_msg;
  ColiAddFru                add_fru_msg;
  ColiDeleteFru             delete_fru_msg;
  ColiCommandRequest        command_request_msg;
  ColiCommandReply          command_reply_msg;
};


int main(int argc, char *argv[]) 
{  
  // Everything is done in the main thread.

  int scenarioLength; 
  if (argc > 1) 
    scenarioLength = atoi(argv[1]);
  else
    scenarioLength = 10;


  int32_t mailbox_count = 1;
  itc_alloc_scheme alloc_scheme = ITC_MALLOC;
  union itc_scheme *scheme = NULL;
  char *name_space = ITC_NO_NAMESPACE;
  uint32_t flags = 0;

  union itc_msg *msg;

  size_t msg_size;

  int itc_init_res = itc_init(mailbox_count,
			      alloc_scheme,
			      scheme,
			      name_space,
			      flags);
  
  print_fd("itc_init_res: %d\n", itc_init_res);

  char *my_mbox_name = "coli_test_app";
  itc_mbox_id_t rem_mbox;
  itc_mbox_id_t my_mbox = itc_create_mailbox(my_mbox_name, 0);

  print_fd("my mailbox: %d\n", my_mbox);
  
/*   rem_mbox = itc_locate("ecoli_coli"); */
/*   print_fd("remote mailbox: %d\n", rem_mbox); */
  
  uint32_t locate_response[] = {1, ITC_LOCATE_DEFAULT_NO};
  
  /* Locate mailbox with name using itc
     default message as response. */
  itc_locate_async(COLI_SERVER_MBOX, NULL, ITC_MY_MBOX);
  msg = itc_receive(locate_response, ITC_NO_TMO, ITC_FROM_ALL);
  rem_mbox = itc_sender(msg);
  itc_free(&msg);
  
  print_fd("located rem_mbox: %d\n", rem_mbox);


  /* REGISTER FRU TYPE.  */
  msg_size = sizeof(ColiInterfaceRegistration);
  msg = itc_alloc(msg_size, COLI_INTERFACE_REGISTRATION);
  memset(msg->register_msg.fruTypes, 0, COLI_MAX_FRU_LENGTH);
  strcpy(msg->register_msg.fruTypes, "xmu, ru");
  itc_send(&msg, rem_mbox, ITC_MY_MBOX);
  
  /* ADD FRU  */
  msg_size = sizeof(ColiAddFru);
  msg = itc_alloc(msg_size, COLI_ADD_FRU);
  memset(msg->add_fru_msg.fruType, 0, COLI_MAX_FRU_LENGTH);
  strcpy(msg->add_fru_msg.fruType, "ru");
  memset(msg->add_fru_msg.ldn, 0, COLI_MAX_LDN_LENGTH);
/*   strcpy(msg->add_fru_msg.ldn, "ManagedElement=1,Equipment=1,FieldReplaceableUnit=2,DeviceGroup=1"); */
  strcpy(msg->add_fru_msg.ldn, "A=xxxxxxx");
  itc_send(&msg, rem_mbox, ITC_MY_MBOX);

  msg_size = sizeof(ColiAddFru);
  msg = itc_alloc(msg_size, COLI_ADD_FRU);
  memset(msg->add_fru_msg.fruType, 0, COLI_MAX_FRU_LENGTH);
  strcpy(msg->add_fru_msg.fruType, "ru");
  memset(msg->add_fru_msg.ldn, 0, COLI_MAX_LDN_LENGTH);
/*   strcpy(msg->add_fru_msg.ldn, "ManagedElement=1,Equipment=1,FieldReplaceableUnit=3,DeviceGroup=2"); */
  strcpy(msg->add_fru_msg.ldn, "B=yyyyyyyy");
  itc_send(&msg, rem_mbox, ITC_MY_MBOX);

  msg_size = sizeof(ColiAddFru);
  msg = itc_alloc(msg_size, COLI_ADD_FRU);
  memset(msg->add_fru_msg.fruType, 0, COLI_MAX_FRU_LENGTH);
  strcpy(msg->add_fru_msg.fruType, "xmu");
  memset(msg->add_fru_msg.ldn, 0, COLI_MAX_LDN_LENGTH);
/*   strcpy(msg->add_fru_msg.ldn, "ManagedElement=1,Equipment=1,FieldReplaceableUnit=1,SubDeviceGroup=1"); */
  strcpy(msg->add_fru_msg.ldn, "C=xxxxxxxx");
  itc_send(&msg, rem_mbox, ITC_MY_MBOX);


  int i = 0;

  while (i < scenarioLength) {
    print_fd("Loop %d\n", i);

    union itc_msg *rcv_msg;

    uint32_t cid;
    char *command;
    char rcv_ldn[256];
    char *reply_msg;

    rcv_msg = itc_receive(ITC_NOFILTER, 5000, ITC_FROM_ALL);

    if (rcv_msg != NULL) {

/*       int x = 0; */
/* 	print_fs("COLI_COMMAND_REQUEST MSG:     %s\n", ""); */
/*       while (x < 50) { */
/* 	print_fd("%d,", rcv_msg->command_request_msg.ldn[x]); */
/* 	print_fs("\n", ""); */
/* 	x++; */
/*       } */
	

      switch(rcv_msg->msg_no) {
      case COLI_COMMAND_REQUEST:
	/* message data*/
	print_fd("COLI_COMMAND_REQUEST MSG:     %d\n", (int)rcv_msg);
	cid = rcv_msg->command_request_msg.commandId;
	print_fd("COLI_COMMAND_REQUEST ID:      %d\n", cid);
	command = rcv_msg->command_request_msg.command;
	print_fs("COLI_COMMAND_REQUEST COMMAND: %s\n", rcv_msg->command_request_msg.command);
	print_fs("COLI_COMMAND_REQUEST COMMAND: %s\n", command);
	print_fs("COLI_COMMAND_REQUEST LDN:     %s\n", rcv_msg->command_request_msg.ldn);
	strcpy(rcv_ldn, rcv_msg->command_request_msg.ldn);
/* 	rcv_ldn = rcv_msg->command_request_msg.ldn; */
	print_fs("COLI_COMMAND_REQUEST LDN:     %s\n", rcv_ldn);
	/* reply msg */
	reply_msg = "Please, do not disturb";
	msg_size = sizeof(ColiCommandReply)  + strlen(reply_msg) + 1;
	msg = itc_alloc(msg_size, COLI_COMMAND_REPLY);
	msg->command_reply_msg.result = COLI_RESULT_OK;
	msg->command_reply_msg.commandId = cid;
	strcpy(msg->command_reply_msg.reply, reply_msg);
	itc_send(&msg, rem_mbox, ITC_MY_MBOX);
	break;
      default:
	print_fs("Unknown msg  %s\n", " ");
	break;
      }
      itc_free(&rcv_msg);
    }

    i++;
  }
  


/* 	msg_size = sizeof(ColiCommandReply); */
/* 	msg = itc_alloc(msg_size, COLI_COMMAND_REPLY); */
/* 	msg->command_reply_msg.result = COLI_RESULT_OK; */
/* 	memset(&(msg->command_reply_msg.reply), 0, MAX_REPLY_LENGTH); */
/*  	strcpy(msg->command_reply_msg.reply, "Please, do not disturb"); */
/* 	itc_send(&msg, rem_mbox, ITC_MY_MBOX); */

/* 	print_fs("COLI_COMMAND_REPLY     %s\n", "sent"); */

/* 	msg_size = sizeof(ColiCommandReply); */
/* 	msg = itc_alloc(msg_size, COLI_COMMAND_REPLY); */
/* 	msg->command_reply_msg.result = COLI_RESULT_OK; */
/* 	msg->command_reply_msg.commandId = cid; */
/* 	memset(&(msg->command_reply_msg.reply), 0, MAX_REPLY_LENGTH); */
/*  	strcpy(msg->command_reply_msg.reply, "Please, do not disturb"); */
/* 	itc_send(&msg, rem_mbox, ITC_MY_MBOX); */











  
  
/*   /\* Receive any message as response *\/ */
/*   msg = itc_receive(ITC_NOFILTER, ITC_NO_TMO, ITC_FROM_ALL); */
/*   /\* Handle message! *\/ */
/*   itc_free(&msg); */
  
  
  return 0;
}


