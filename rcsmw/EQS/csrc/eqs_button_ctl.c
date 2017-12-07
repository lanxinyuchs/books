/*
 *
 * Copyright (c) Ericsson AB 2014-2015 All rights reserved.
 *
 * The information in this document is the property of Ericsson. Except
 * as specifically authorized in writing by Ericsson,the receiver of this
 * document shall keep the information contained herein confidential
 * and shall protect the same in whole or in part from disclosure and
 * dissemination to third parties. Disclosure and disseminations to the
 * receiver's employees shall only be made on a strict need to know basis.
 *
 */

#include <unistd.h>
#include <stdint.h>
#include <sys/types.h>
#include <signal.h>
#include <arpa/inet.h>
#include <syslog.h>

#include "rhai-mmi.h"

#define EVENT_IND (1)

static int child(void);
static int parent(pid_t child);

int
main()
{
  pid_t pid;

  openlog("eqs_button_ctl", LOG_PID, LOG_USER);

  pid = fork();

  if (pid == 0)
    return child();
  else
    return parent(pid);
}

static int
child(void)
{
  pid_t parent;
  char tmp;

  for (;;) {
    if (read(STDIN_FILENO, &tmp, 1) < 1)
      break;
  }

  parent = getppid();
  kill(parent, SIGTERM);

  return 0;
}

static int
parent(pid_t child)
{
  void *handle = NULL;
  int button_events = 0;
  char buffer[3*sizeof(uint32_t)];
  char *ptr = buffer;
  int len = 2*sizeof(uint32_t);
  int ret;

  *(uint32_t*)ptr = htonl(len);
  ptr += sizeof(uint32_t);
  *(uint32_t*)ptr = EVENT_IND;
  ptr += sizeof(uint32_t);

  ret = rhai_mmi_button_init(&handle, 0);
  if (ret < 0) {
    syslog(LOG_ERR, "rhai_mmi_button_init() returned %i\n", ret);
    goto error;
  }

  for (;;) {
    ret = rhai_mmi_button_get_event(handle, &button_events);
    if (ret < 0) {
      syslog(LOG_ERR, "rhai_mmi_button_get_event() returned %i\n", ret);
      break;
    }
    *(uint32_t*)ptr = button_events;
    write(STDOUT_FILENO, buffer, sizeof(buffer));
  }

  rhai_mmi_button_shutdown(handle);

 error:
  kill(child, SIGTERM);

  return 0;
}
