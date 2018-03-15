#ifndef __CMD_H
#define __CMD_H

#include "state.h"

extern int open_cmd_server_socket();
extern int accept_cmd_client(int server_cmd_sockfd);
extern int service_cmd_client(int *client_cmd_sockfdp, struct state *s);

#endif /* __CMD_H */
