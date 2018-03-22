
#include "ose.h"
#include "osetypes.h"

#include "cello_cri.h"
#include "cello_te_ose.h"
#include "cello_te_trace.h"

static const char *pongCmdHelp[] = {
  "Pong commands:",
  "Start Pong Server process: pong -s <name>",
  "Start Pong Client process: pong -c <name> <path> <min=4> <max=4> <burst=1>",
  "Kill Pong process: pong -k <name>",
  0
};

static const char *pongCmdLazy[] = {
  "Pong lazy:",
  "pong -s clayman",
  "pong -c aluvian port_0_dev_1/clayman",
  "pong -c halaster clayman",
  "pong -k clayman",
  0
};

union SIGNAL {
  SIGSELECT sigNo;
};

OS_PROCESS(pongServer)
{
  SIGSELECT     all[] = { 0 };
  union SIGNAL *sig;

  for (;;) {
    sig = receive(all);
    send(&sig, sender(&sig));
  }
}

OS_PROCESS(pongClient)
{
  SIGSELECT     sel1[] = { 1, 0x00000000 };
  SIGSELECT     sel2[] = { 2, 0x00000000, 0xdeadbabe };
  union SIGNAL *sig;
  int           i, j, k, min, max, burst;
  PROCESS       pid;
  char         *name;

  name = get_env(current_process(), "SERVER");
  if (sscanf(get_env(current_process(), "MIN"), "0x%x", &min) != 1) {
    if (sscanf(get_env(current_process(), "MIN"), "%d", &min) != 1) {
      kill_proc(current_process());
    }
  }
  if (sscanf(get_env(current_process(), "MAX"), "0x%x", &max) != 1) {
    if (sscanf(get_env(current_process(), "MAX"), "%d", &max) != 1) {
      kill_proc(current_process());
    }
  }
  if (sscanf(get_env(current_process(), "BURST"), "%d", &burst) != 1) {
    kill_proc(current_process());
  }

  if (max < min) {
    i = max;
    max = min;
    min = i;
  }

  INFO(STR("Minimum signal size...: %d", min));
  INFO(STR("Maximum signal size...: %d", max));
  INFO(STR("Signals per burst.....: %d", burst));

deadbabe:
  INFO(STR("Hunting for \"%s\"...", name));
  sig = alloc(sizeof(SIGSELECT), 0x00000000);
  (void) hunt(name, 0, 0, &sig);
  sig = receive(sel1);
  pid = sender(&sig);
  free_buf(&sig);
  INFO(STR("Successful hunt for \"%s\"", name));
  sig = alloc(sizeof(SIGSELECT), 0xdeadbabe);
  (void) attach(&sig, pid);

  for (;;) {

    for (i = min; i <= max; i++) {

      for (k = 1; k <= burst; k++) {
        sig = alloc(i, 0x00000000);
        for (j = 4; j < i; j++) {
          ((U8*) sig)[j] = (U8) (j & 0xff);
        }
        TRACE(1, STR("Sending %d bytes", i));
        send(&sig, pid);
      }

      for (k = 1; k <= burst; k++) {
        sig = receive(sel2);
        if (sig->sigNo == 0xdeadbabe) {
          INFO(STR("Server \"%s\" was killed", name));
          free_buf(&sig);
          goto deadbabe;
        } else {
          if (sigsize(&sig) != i) {
            TRACE_ERROR(STR("Received bad signal size: %d  Correct signal size: %d",
                            sigsize(&sig), i));
            stop(current_process());
          }
          for (j = 4; j < i; j++) {
            if (((U8*) sig)[j] != (U8) (j & 0xff)) {
              TRACE_ERROR(STR("Received bad data in signal of size: %d", i));
              stop(current_process());
            }
          }
          free_buf(&sig);
        }
        TRACE(1, STR("Received %d bytes", i));
      }
    }
  }
}

static int
pongStartServer(const char *name)
{
  PROCESS pid;

  if (hunt(name, 0, &pid, 0)) {
    printf("Process \"%s\" already exists\r\n", name);
    return 1;
  }

  pid = create_process(OS_PRI_PROC, name, pongServer,
                       1024, 31, 0, 0, 0, 0, 0);
  start(pid);
  printf("Pong Server process \"%s\" (0x%x) was started\r\n", name, pid);
  return 0;
}

static int
pongStartClient(const char *name, const char *path,
                const char *min, const char *max, const char *burst)
{
  int     dummy;
  PROCESS pid;

  if (hunt(name, 0, &pid, 0)) {
    printf("Process \"%s\" already exists\r\n", name);
    return 1;
  }

  if (sscanf(min, "0x%x", &dummy) != 1) {
    if (sscanf(min, "%d", &dummy) != 1) {
      printf("Invalid minimum signal size \"%s\"\r\n", min);
      return 1;
    }
  }
  if (dummy < 4) {
    printf("Invalid minimum signal size \"%s\"\r\n", min);
    return 1;
  }

  if (sscanf(max, "0x%x", &dummy) != 1) {
    if (sscanf(max, "%d", &dummy) != 1) {
      printf("Invalid maximum signal size \"%s\"\r\n", max);
      return 1;
    }
  }
  if (dummy < 4) {
    printf("Invalid maximum signal size \"%s\"\r\n", max);
    return 1;
  }

  if (sscanf(burst, "%d", &dummy) != 1 || dummy < 1) {
    printf("Invalid burst value \"%s\"\r\n", burst);
    return 1;
  }

  pid = create_process(OS_PRI_PROC, name, pongClient,
                       1024, 31, 0, 0, 0, 0, 0);
  set_env(pid, "SERVER", path);
  set_env(pid, "MIN", min);
  set_env(pid, "MAX", max);
  set_env(pid, "BURST", burst);
  start(pid);
  printf("Pong Client process \"%s\" (0x%x) was started\r\n", name, pid);
  return 0;
}

static int
pongKill(const char *name)
{
  PROCESS pid;

  if (!hunt(name, 0, &pid, 0)) {
    printf("Process \"%s\" not found\r\n", name);
    return 1;
  }

  kill_proc(pid);
  printf("Process \"%s\" (0x%x) was killed\r\n", name, pid);

  return 0;
}

int
pongCmd(int argc, char **argv)
{
  int i;

  if (argc == 2) {
    if (strcmp(argv[1], "-help") == 0) {
baba:
      for (i = 0; pongCmdHelp[i]; i++) {
        printf("%s\r\n", pongCmdHelp[i]);
      }
      return 0;
    } else if (strcmp(argv[1], "-lazy") == 0) {
      for (i = 0; pongCmdLazy[i]; i++) {
        printf("%s\r\n", pongCmdLazy[i]);
      }
      return 0;
    }
  } else if (argc == 3) {
    if (strcmp(argv[1], "-s") == 0) {
      return pongStartServer(argv[2]);
    } else if (strcmp(argv[1], "-k") == 0) {
      return pongKill(argv[2]);
    }
  } else if (argc == 4 || argc == 5 || argc == 6 || argc == 7) {
    if (strcmp(argv[1], "-c") == 0) {
      if (argc == 4) {
        return pongStartClient(argv[2], argv[3],     "4",     "4",     "1");
      } else if (argc == 5) {
        return pongStartClient(argv[2], argv[3], argv[4],     "4",     "1");
      } else if (argc == 6) {
        return pongStartClient(argv[2], argv[3], argv[4], argv[5],     "1");
      } else {
        return pongStartClient(argv[2], argv[3], argv[4], argv[5], argv[6]);
      }
    }
  }
  goto baba;

  return 1;
}

void
addCmd_pong(void)
{
  CelloCri_addShellCommand("pong",
                           "pong -s <name> | -c <name> <path> <min=4> <max=4> <burst=1> | -k <name>",
                           "Start or kill a pong server or client",
                           pongCmd);
}
