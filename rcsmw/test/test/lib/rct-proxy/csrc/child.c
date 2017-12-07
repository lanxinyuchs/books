#include "master.h"
#define MAX_CLIENTS 9

//extern struct Client *clients;
//extern struct Monitor_flag *monitor_flag;
//extern ETERM *send_sig_lici(void *, union SIGNAL *, int, ETERM *);
//extern ETERM *recv_sig_lici(void *, union SIGNAL *);
//extern char id[];

/* message passing between childs and master */
extern struct Monitor_flag *monitor_flag; /* pointer to first message in linked list */
extern void send_msg(struct Monitor_flag *);
extern int search_and_delete_msg(struct Monitor_flag *, PROCESS);
void print_msgs(struct Monitor_flag *);
/* message passing between childs and master */

/* test_lici.c functions */
extern ETERM *send_sig_lici(void *, union SIGNAL *, int, ETERM *);
extern ETERM *recv_sig_lici(void *, union SIGNAL *);
/* test_lici.c functions */


extern void pthread_exit(void *);

union SIGNAL {
    SIGSELECT sigNo;
};

OS_PROCESS(client)
{
  PROCESS mypid, masterpid;
  union SIGNAL *sig_p, *newsig_p;
  void *proxyMemory_p = NULL;
  SIGSELECT any[] = {0};
  int live = 1;

  struct Monitor_flag *msg = (struct Monitor_flag *) malloc(sizeof(struct Monitor_flag));

  ETERM *fromp;

  mypid = current_process();
  printf("child monitor_flag %p\n",monitor_flag);

  while (!search_and_delete_msg(msg, mypid)) {}
  print_msgs(monitor_flag);
  masterpid = msg->frompid;
  printf("masterpid %i msg->frompid %i\n", (int) masterpid, (int) msg->frompid);
  fromp = msg->fromp;
  msg->topid = masterpid;
  printf("masterpid %i msg->topid %i\n", (int) masterpid, (int) msg->topid);
  msg->childdata = erl_format("{ok, client_started}");
  send_msg(msg);
  print_msgs(monitor_flag);

  while (live) {
    printf("CLIENT\n");
    /* Receive OSE signal */
    if ((newsig_p = receive_w_tmo(50, any)) != NULL) { //50 ms
      printf("CLIENT receive_w_tmo %p\n",newsig_p);
      sig_p = newsig_p;
      msg->childdata = recv_sig_lici(&proxyMemory_p,sig_p);
      msg->topid = masterpid;
      send_msg(msg);
      print_msgs(monitor_flag);
    } else {
      printf("CLIENT check child received msg\n");
      if (search_and_delete_msg(msg, mypid)) {
	printf("CLIENT received msg\n");
	print_msgs(monitor_flag);
	erl_free_term(fromp);
	fromp = msg->fromp;
	if (msg->prot == DIE) {
	  erl_free_term(msg->args);
	  erl_free_term(msg->childdata);
	  erl_free_term(msg->fromp);
	  free(msg);
	  if (proxyMemory_p != NULL)
	    free_buf((union SIGNAL**) &proxyMemory_p);
	  live = 0;
	} else {
	  switch (msg->prot) {
	  case LICI:
	    printf("CLIENT LICI Func: %i Args: %p\n", (int) msg->func, msg->args);
	    msg->childdata = send_sig_lici(&proxyMemory_p, sig_p, msg->func, msg->args);
	    break;
	  case BPAI:
	    printf("Prot: bpai Msg: Pid: %i\n", (int) msg->topid);
	    break;
	  case SCTP:
	    printf("Prot: sctp Msg: Pid: %i\n", (int) msg->topid);
	    break;
	  }
	  msg->topid = masterpid;
	  printf("CLIENT send msg\n");
	  send_msg(msg);
	  print_msgs(monitor_flag);
	  erl_free_term(msg->args);
	}
      }
    }
  }
  pthread_exit((int *)1);
  //printf("asdfasfsfasff\n");

  //exit(1);
}

