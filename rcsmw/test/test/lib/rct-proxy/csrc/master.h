#include <osetypes.h>
#include <ose.h>
#include "socket.h"
#include <netinet/in.h>
#include "erl_interface.h"
#include "ei.h"
#include <stdlib.h>
#include <string.h>

#define MAX_ENDPOINTS 129
#define MAX_ASSOCIATIONS 513
#define START 0
#define SEND 0
#define STOP 0

#define DIE  0
#define LICI 1
#define BPAI 2
#define SCTP 3
struct Monitor_flag
{
  PROCESS frompid;     // start
  PROCESS topid;       // start send stop childmsg
  ETERM *fromp;        // start send stop childmsg
  uint32_t prot;       //       send stop
  uint32_t func;       //       send
  ETERM *args;         //       send
  ETERM *childdata;     //                childmsg
  struct Monitor_flag *next;
  //char clientId[50];
  //char *clientId;
};

struct Client
{
  char *clientId;
  //  char clientId[50];
  PROCESS pid;
  uint32_t prot;
};
