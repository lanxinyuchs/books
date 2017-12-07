#include "master.h"
#include <unistd.h>
#define MAX_CLIENTS 3
#define BUFSIZE 1000


extern OS_PROCESS(client);
PROCESS create_child_process(char *);

struct Client *clients;
int check_if_clientId_in_use(char *, int *);
int find_free_client(int *);
int my_listen(int);

/* message passing between master and children */
struct Monitor_flag *monitor_flag; /* pointer to first message in linked list */
void send_msg(struct Monitor_flag *);
int search_and_delete_msg(struct Monitor_flag *, PROCESS);
void print_msgs(struct Monitor_flag *);
/* message passing between master and children */

int main(int argc, char **argv)
{
  /* erl_interface stuff */
  PROCESS mypid;
  int port;                                /* Listen port number */
  int listen;                              /* Listen socket */
  int fd;                                  /* fd to Erlang node */
  ErlConnect conn;                         /* Connection data */
  int loop = 1;                            /* Loop flag */
  int got;                                 /* Result of receive */
  unsigned char buf[BUFSIZE];              /* Buffer for incoming message */
  ErlMessage emsg;                         /* Incoming message */
  ETERM *fromp, *clientIdp, *protfuncp, *argsp, *resp;
  /* erl_interface stuff */

  char *clientId;
  int i;

  PROCESS pid;
//  loose_init(100, 0);
  clients = calloc(MAX_CLIENTS, sizeof(struct Client));
  struct Monitor_flag *msg = (struct Monitor_flag *) malloc(sizeof(struct Monitor_flag));

  /* erl_interface stuff */
  /* port = atoi(argv[1]); */
  port = 3456;
  erl_init(NULL, 0);
  //if (ei_connect_init(&cnode, "proxy", "secretcookie", 0) == -1)
  if (erl_connect_init(1, "secretcookie", 0) == -1)
    erl_err_quit("erl_connect_init");
  /* Make a listen socket */
  if ((listen = my_listen(port)) <= 0)
    erl_err_quit("my_listen");
  if (erl_publish(port) == -1)
    erl_err_quit("erl_publish");
 
  printf("Stupid erlang ssh does not support pty_req\n");
  fflush(stdout);

 if ((fd = erl_accept(listen, &conn)) == ERL_ERROR)
    erl_err_quit("erl_accept");
  fprintf(stderr, "Connected to %s\n\r", conn.nodename);
  /* erl_interface stuff */

  mypid = current_process();
  printf("master monitor_flag %p\n",monitor_flag);
  fflush(stdout);
  fd_set readmask;
  struct timeval tv;


  while(1) {
    resp = NULL;
    tv.tv_sec = (time_t) (0);
    tv.tv_usec = (time_t) (1000);
    FD_ZERO(&readmask);
    FD_SET(fd,&readmask);
    i = select(fd+1, &readmask, NULL, NULL, &tv);
    if (i) {
      //for (i = 0; i < MAX_CLIENTS; i++)
      //{printf("i: %i id: %s prot: %i pid: %i\n",i,clients[i].clientId,clients[i].prot,clients[i].pid);}
      got = erl_receive_msg(fd, buf, BUFSIZE, &emsg);
      if (got == ERL_TICK) {
	/* ignore */
      } else if (got == ERL_ERROR) {
	loop = 0;
      } else {
	if (emsg.type == ERL_REG_SEND) {
	  printf("1\n");
	  fromp     = erl_element(1, emsg.msg);
	  if (strcmp(emsg.to_name,"exit") == 0) {
	      printf("MASTER exit\n");
	      resp = erl_format("{ok, exited}");
	      erl_send(fd, fromp, resp);
	      erl_close_connection(fd);
	      exit(1);
	  }
	  clientIdp = erl_element(2, emsg.msg);
	  protfuncp = erl_element(3, emsg.msg);
	  argsp     = erl_element(4, emsg.msg);
	  clientId  = ERL_ATOM_PTR(clientIdp);
	  if (strcmp(emsg.to_name,"start") == 0) {
	    if (check_if_clientId_in_use(clientId,&i)) {
	      printf("MASTER clientId_already_in_use\n");
	      erl_send(fd, fromp, erl_format("{error, clientId_already_in_use}"));
	    } else if (find_free_client(&i)) {
	      printf("MASTER start child\n");
	      pid = create_child_process(clientId);
	      msg->fromp   = fromp;
	      msg->frompid = mypid;
	      msg->topid   = pid;
	      send_msg(msg);
	      print_msgs(monitor_flag);
	      clients[i].pid = pid;
	      clients[i].clientId = strdup(clientId);
	      clients[i].prot = ERL_INT_UVALUE(protfuncp);
	    }
	    else {
	      resp = erl_format("{error, max_nof_clients_reached}");
	      erl_send(fd, fromp, resp);
	      printf("no start %i\n",i);
	    }
	  } else if (strcmp(emsg.to_name,"send") == 0) {
	    if (check_if_clientId_in_use(clientId,&i)) {
	      msg->fromp = fromp;
	      msg->topid = clients[i].pid;
	      msg->prot  = clients[i].prot;
	      msg->func  = ERL_INT_UVALUE(protfuncp);
	      msg->args  = argsp;
	      printf("MASTER send msg\n");
	      send_msg(msg);
	      print_msgs(monitor_flag);
	    } else {
	      resp = erl_format("{error, client_not_started}");
	      erl_send(fd, fromp, resp);
	    }
	  } else if (strcmp(emsg.to_name,"stop") == 0) {
	    if (check_if_clientId_in_use(clientId,&i)) {
	      msg->fromp = fromp;
	      msg->topid = clients[i].pid;
	      msg->prot  = DIE;
	      send_msg(msg);
	      print_msgs(monitor_flag);
	      clients[i].pid = 0;
	      free(clients[i].clientId);
	      clients[i].clientId = NULL;
	      clients[i].prot = 0;
	      resp = erl_format("{ok, client_stopped}");
	      erl_send(fd, fromp, resp);
	    } else {
	      resp = erl_format("{ok, client_already_stopped}");
	      erl_send(fd, fromp, resp);
	    }
	  }
	  erl_free_term(clientIdp);
	  erl_free_term(protfuncp);
	  erl_free_term(emsg.from);
	  erl_free_term(emsg.msg);
	  erl_free_term(resp);
	}
      }
    }
    printf("MASTER check master received msg\n");
    if (search_and_delete_msg(msg, mypid)) {
      printf("MASTER received msg\n");
      print_msgs(monitor_flag);
      erl_send(fd, msg->fromp, msg->childdata);
      erl_free_term(msg->childdata);
    }
  }
}

int my_listen(int port) {
  int listen_fd;
  struct sockaddr_in addr;
  int on = 1;
  if ((listen_fd = socket(AF_INET, SOCK_STREAM, 0)) < 0)
    return (-1);
  setsockopt(listen_fd, SOL_SOCKET, SO_REUSEADDR, &on, sizeof(on));
  memset((void*) &addr, 0, (size_t) sizeof(addr));
  addr.sin_family = AF_INET;
  addr.sin_port = htons(port);
  addr.sin_addr.s_addr = htonl(INADDR_ANY);
  if (bind(listen_fd, (struct sockaddr*) &addr, sizeof(addr)) < 0)
    return (-1);
  listen(listen_fd, 5);
  return listen_fd;
}

PROCESS create_child_process(char *id)
{
    PROCESS Block;
    PROCESS pid;
    char client_name[20];
    char block_name[10];
    int i;
    
    for (i = 0; i < MAX_CLIENTS; i++)
    {
      if (clients[i].clientId == id)
	{
	  return 0;
	}
    }

    sprintf(client_name, "client_%s", id);
    sprintf(block_name, "block_%s", id);   
    
    /* start ose proc */
    //    Block = create_block(block_name, 0, 0, 0, 0);
    Block = create_block(block_name, 0, 0, 0, 0);
    pid = create_process(OS_PRI_PROC,
			 client_name,
			 client,
			 1024, 4, 0, Block,
			 NULL, 0, 0);

    start(pid);
    
    return pid;
}

int find_free_client(int *client) {
  int i = *client;
  for (i = 0; i < MAX_CLIENTS; i++) {
    if (clients[i].clientId == NULL) {
      *client = i;
      return(1);
    }
  }
  return(0);
}
    
//int check_if_clientId_in_use 
int check_if_clientId_in_use(char *clientId,int *client) {
  int i = *client;
  for (i = 0; i < MAX_CLIENTS; i++) {
    if (clients[i].clientId != NULL) {
      if (((int)strlen(clients[i].clientId) == (int)strlen(clientId)) && (strcmp(clients[i].clientId, clientId) == 0)) {
	*client = i;
	return(1);
      }
    }
  }
  return(0);
}
  
void send_msg(struct Monitor_flag *new_msg) {
  struct Monitor_flag *temp_msg;
  if (monitor_flag == NULL) {
    monitor_flag = (struct Monitor_flag *) malloc(sizeof(struct Monitor_flag));
    *monitor_flag = *new_msg;
    monitor_flag->next = NULL;
  } else {
    temp_msg = monitor_flag;
    while (temp_msg->next != 0) 
      temp_msg = temp_msg->next;
    temp_msg->next = (struct Monitor_flag *) malloc(sizeof(struct Monitor_flag));
    *temp_msg->next = *new_msg;
    temp_msg->next->next = NULL;
  }
}

int search_and_delete_msg(struct Monitor_flag *msg, PROCESS pid) {
  struct Monitor_flag *temp_msg, *prev_msg;
  printf("MESSAGE search_and_delete_msg\n");
  if (monitor_flag == NULL) 
    return 0;
  temp_msg = monitor_flag;
  if (temp_msg->topid == pid) {
    monitor_flag = temp_msg->next;
    *msg = *temp_msg;
    free(temp_msg);
    return(1);
  } else {
    prev_msg = temp_msg;
    temp_msg = temp_msg->next;
    while (temp_msg != NULL) {
      if (temp_msg->topid == pid) {
	prev_msg->next = temp_msg->next;
	*msg = *temp_msg;
	free(temp_msg);
	return(1);
      } else {
	prev_msg = temp_msg;
	temp_msg = temp_msg->next;
      }
    }
  }
  return(0);
}

void print_msgs(struct Monitor_flag *temp_msg) {  
  while (temp_msg != NULL) {
    printf("MESSAGE Me: %p Frompid: %i Topid: %i Fromp: %p Prot: %i Func: %i Args: %p Childdata: %p Next: %p\n",
	   temp_msg,
	   (int)temp_msg->frompid,
	   (int)temp_msg->topid,
	   temp_msg->fromp,
	   (int)temp_msg->prot,
	   (int)temp_msg->func,
	   temp_msg->args,
	   temp_msg->childdata,
	   temp_msg->next);
    temp_msg = temp_msg->next;
  }
}
