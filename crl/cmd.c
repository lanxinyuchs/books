/**
 *   Handling of local commands.
 *
 *   @file
 *
 *   This file is a part of the COLI shell.
 *
 *   Copyright (C) 2011-2012 by Ericsson AB. All rights reserved. The
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
 *   Revised : 2015-07-06 Henrik Wallin
 *   Change  : Fixes in shell_run_cmd:
 *             If command executes successfully, it should
 *             return SHELL_SUCCESS regardless of the status value.
 *             Return the status value in the RUNCMD_RETURN_STATUS tag.
 *
 *   Revised : 2015-04-28 Henrik Wallin
 *   Change  : Fix handling of command process killing itself.
 *             The code in start_cmd_proc did not handle the case of
 *             the command function killing itself properly.
 *             If it happend the code in start_cmd_proc got stuck in
 *             receive.
 *
 *   Revised : 2015-04-27 Henrik Wallin
 *   Change  : Fix handling of attach signals in cri_server.
 *             The loop in start_cmd_proc() handles attach signals from the
 *             shell_pid and the coli command pid. But the cri_server have also
 *             attached to colid earlier.
 *             If that attach signal arrives while a command is executing, the
 *             code in start_cmd_proc() will not be able to handle it.
 *             Fix by making the colid attach another signal number so it
 *             isn't handled until the command execution is over. The command
 *             execution is not dependent on colid being present.
 *
 *   Revised : 2015-03-20 Henrik Wallin
 *   Change  : Fix one-off bug in malloc(). Missing space for the '\0' character.
 *
 *   Revised : 2014-12-10 Henrik Wallin
 *   Change  : Make colid_pid a local variable in cri_server process.
 *
 *   Revised : 2014-12-10 Henrik Wallin
 *   Change  : Adjust to changes in open_socket. The path passed into
 *             open_socket needs to end with XXXXXX
 *             (as path is passed into mkstemp)
 *
 *   Revised : 2014-12-05 Ravineet Singh EAB/FJP/HB
 *   Change  : Global variables, argc/argv/cmd_res replaced with
 *             signals to/from cmd_proc.
 *             Make sure that caller is alive before receiving fd via socket.
 *
 *   Revised : 2014-11-26 Ravineet Singh EAB/FJP/HB
 *   Change  : Blocked SIGPIPE in cmd_proc  while executing command,
 *             in case the command is piped to/from some other command
 *             that closes the pipe prematurely.
 *
 *   Revised : 2014-10-30 Henrik Wallin
 *   Change  : Fix shell_remove_cmd implementation.
 *
 *   Revised : 2014-09-13 Ravineet Singh EAB/FJP/HB
 *   Change  : Adapt to the fact that set_std_stream_socket now returns.
 *             Synchronize cmd_proc exit.
 *
 *   Revised : 2014-07-14 Christoffer Cederwall EAB/FJP/HB
 *   Change  : Added shell_remove_cmd.
 *
 *   Revised : 2014-06-30 Ravineet Singh EAB/FJP/HB
 *   Change  : Moved set/reset of std streams to start_cmd_proc() where we
 *             detect that colish has died (ex. via CTRL-C)
 *
 *   Revised : 2014-01-21 Ravineet Singh EAB/FJP/HB
 *   Change  : Replaced local cmd_tree implementattion with common/shared
 *             cmd_tree implementation.
 *             Replaced local std stream push/pop with new common std stream
 *             API calls(std_stream.h)
 *
 *   Revised : 2013-11-28 Ravineet Singh EAB/FJP/HB
 *   Change  : Changed implementation of shell_run_command from usage of
 *             system()  to popen(), since system is not reentrant, popen is.
 *             Enhanced stack from cmd process, from 16k to 64k.
 *
 *   Revised : 2013-04-25 Sridhar K.M
 *   Change  : Remote coli chnages.
 *
 *   Revised : 2012-02-28 Lars Jönsson EAB/FJP/TB
 *   Change  : Added environment variable PARENT to the process that
 *             executes the command. Note that the parent process only
 *             lives during the command and thus not compatible with the
 *             PARENT environment variable in OSE.
 *
 *   Revised : 2011-11-29 Lars Jönsson EAB/FJP/TB
 *   Change  : All signals are now using variable sized string instead of
 *             fix sized strings.
 *
 *   Revised : 2011-11-23 Lars Jönsson EAB/FJP/TB
 *   Change  : First version.
 * ========================================================================
 */

/* ========================================================================
 *   INCLUDE FILES
 * ========================================================================
 */
#define _GNU_SOURCE
#include <assert.h>
#include <errno.h>
#include <ose.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

#include <cmd_tree.h>
#include <coli.sig>
#include <fdh.h>
#include <std_stream.h>

/* ========================================================================
 *   DEFINITIONS
 * ========================================================================
 */
#define	HUNT_SIG	  1
#define	COLI_REG_LOC_SIG  0xeeaabbcc
#define	COLI_RM_LOC_SIG	  0xeeaabbdd
#define	COLI_ATTACH_SIG   0xeeaabbee
#define BUFF_SIZE         1024
/* ========================================================================
 *   TYPE DEFINITIONS
 * ========================================================================
 */

struct coli_reg_loc_sig {
   SIGSELECT sig_no;
   cmd_t *cmd;
};

struct coli_rm_loc_sig {
   SIGSELECT sig_no;
   char name[1];
};

union SIGNAL {
   SIGSELECT                   sig_no;
   struct coli_reg_cmd_sig     coli_reg_cmd;
   struct coli_reg_cmd_r_sig   coli_reg_cmd_r;
   struct coli_exec_cmd_sig    coli_exec_cmd;
   struct coli_exec_cmd_r_sig  coli_exec_cmd_r;
   struct coli_cmd_exit_sig    coli_cmd_exit;
   struct coli_reg_loc_sig     coli_reg_loc;
   struct coli_rm_loc_sig      coli_rm_loc;
   struct coli_rm_cmd_sig      coli_rm_cmd;
   struct coli_start_cmd_sig   coli_start_cmd;
   struct coli_start_cmd_r_sig coli_start_cmd_r;
   struct coli_finish_cmd_sig  coli_finish_cmd;
};

/* ========================================================================
 *   DATA DECLARATIONS
 * ========================================================================
 */ 
static void *lib_cmd_tree = NULL;

/* ========================================================================
 *   FUNCTION PROTOTYPES
 * ========================================================================
 */ 

/** ==================================================================== */
/** 
 *   Returns number of digits in val.
 * 
 *   @param val        The value
 * 
 *   @return           Number of digits
 *
 *   @par Globals:     
 *                     --
 */
/* ===================================================================== */
static int
num_digits(int val)
{
   int num = 0;

   while ( val > 0 )
   {
      num++;
      val /= 10;
   }

   return num > 0 ? num : 1;
}

/** ==================================================================== */
/** 
 *   Creates the socket name for the local command server and allocates
 *   memory for the string.
 * 
 *   @param            -
 * 
 *   @return           Socket name string (must be freed after use)
 *
 *   @par Globals:     
 *                     --
 */
/* ===================================================================== */
static char *
get_sock_name(void)
{
   char *name;
   char *dir = P_tmpdir;

   name = malloc(strlen(dir) + 1
                 + strlen(SOCK_PREFIX) + 1
                 + num_digits(getpid()) + 1
                 + 6
                 + 1);

   if ( name != NULL )
      sprintf(name, "%s/%s.%u.XXXXXX", dir, SOCK_PREFIX, getpid());

   return name;
}

/** ==================================================================== */
/** 
 *   Locate the COLI server.
 * 
 *   @return           - Pid of the COLI server.
 *
 *   @par Globals:     
 *                     --
 */
/* ===================================================================== */
static PROCESS
register_cri(void)
{
   PROCESS       pid;
   SIGSELECT     sel_hunt[] = {1, HUNT_SIG};
   union SIGNAL  *sig;

   sig = alloc(sizeof(union SIGNAL), HUNT_SIG);
   hunt("coli_server", 0, NULL, &sig);

   sig = receive(sel_hunt);

   pid = sender(&sig);
   free_buf(&sig);

   return pid;
}

/** ==================================================================== */
/** 
 *   Registers a command to the COLI server.
 * 
 *   @param colid_pid  PID of COLI server
 *   @param crid_pid   PID of local command process
 *   @param name       Command name
 *   @param usage      Usage string
 *   @param descr      Description string
 * 
 *   @return           o if OK, otherwise non-zero
 *
 *   @par Globals:     
 *                     --
 */
/* ===================================================================== */
static int
register_command(PROCESS colid_pid, PROCESS crid_pid,
		 const char *name, const char *usage, const char *descr)
{
   SIGSELECT     sel[] = {1, COLI_REG_CMD_R};
   union SIGNAL  *sig;
   int           ret;
   int           offset;
   int           name_size = strlen(name) + 1;
   int           usage_size = strlen(usage) + 1;
   int           descr_size = strlen(descr) + 1;

   sig = alloc(sizeof(struct coli_reg_cmd_sig) - 1 +
	       name_size + usage_size + descr_size,
	       COLI_REG_CMD);

   sig->coli_reg_cmd.pid = crid_pid;

   sig->coli_reg_cmd.name  = (offset  = 0);
   sig->coli_reg_cmd.usage = (offset += name_size);
   sig->coli_reg_cmd.descr = (offset += usage_size);

   strcpy(&sig->coli_reg_cmd.str[sig->coli_reg_cmd.name], name);
   strcpy(&sig->coli_reg_cmd.str[sig->coli_reg_cmd.usage], usage);
   strcpy(&sig->coli_reg_cmd.str[sig->coli_reg_cmd.descr], descr);

   send(&sig, colid_pid);
   sig = receive(sel);
   ret = sig->coli_reg_cmd_r.status;
   free_buf(&sig);

   return ret;
}

/** ==================================================================== */
/** 
 *   Remove a command from the COLI server.
 * 
 *   @param name       Command name
 *
 *   @par Globals:      
 *                     --
 */
/* ===================================================================== */
void
remove_command(PROCESS colid_pid, const char *name)
{
   union 	SIGNAL *sig;
   int		name_size = strlen(name) + 1;

   sig = alloc(sizeof(struct coli_rm_cmd_sig) - 1 + name_size, COLI_RM_CMD);
   strcpy(&sig->coli_rm_cmd.name[0], name);
   send(&sig, colid_pid);
}

/** ==================================================================== */
/** 
 *   Unpacks the null terminated strings in buf into argv and puts the
 *   nuber of strings in argc.
 * 
 *   @param buf        Buffer with null terminated strings
 *   @param size       Size of buf
 *   @param argc       Number of arguments
 *   @param argv       Pointer to argument strings
 * 
 *   @return           O if OK, otherwise non-zero
 *
 *   @par Globals:     
 *                     --
 */
/* ===================================================================== */
static int
unpack_args(char *buf, int size, int *argc, char ***argv)
{
  int   i;
  int   num = 0;
  char  *ptr = buf;

  if ( (size == 0) ||
       (buf[size-1] != '\0') ||
       ((size > 1) && (buf[size-2] != '\0')) )
    return -1;

  while ( size > 1 ) {
     i = strlen(ptr);
     size -= i + 1;
     ptr += i + 1;
     num++;
  }

  *argc = num;
  *argv = malloc(*argc * sizeof(char *));
  ptr   = buf;

  for (i = 0; i < *argc; i++) {
    (*argv)[i] = ptr;
    ptr = &ptr[strlen(ptr) + 1];
  }

  return 0;
}

/** ==================================================================== */
/**
 *   Process that runs the command.
 *   SIGPIPE is blocked during cmd execution, in case any
 *   of the fd:s is a pipe/socket.
 *   Fixme: The blocking of the signal will prevent signal generation
 *          inside the cmd function aswell. Ex. write functiond in
 *          cmd->func errors might not be detected.
 *
 *   @param            -
 *
 *   @return           -
 *
 *   @par Globals:
 *                     -
 */
/* ===================================================================== */
OS_PROCESS(cmd_proc)
{
   SIGSELECT     sel_start[] = {1, COLI_START_CMD};
   SIGSELECT     sel_die[]   = {1, COLI_FINISH_CMD};
   union SIGNAL  *sig;
   cmd_t *cmd;
   int fd[]={STDIN_FILENO, STDOUT_FILENO, STDERR_FILENO};
   int i;
   int cmd_res = -1;
   int argc;
   char **argv;
   PROCESS master;

   /* Wait for start. */
   sig = receive(sel_start);
   master = sender(&sig);
   argc = sig->coli_start_cmd.argc;
   argv = sig->coli_start_cmd.argv;
   free_buf(&sig);

   /* Legacy code does not handle SIGPIPE, block it for this thread.*/
   for(i=0;i<3;i++)
   {
      int ret;
      struct stat buf;
      sigset_t sigpipe_mask;

      ret = fstat(fd[i], &buf);
      assert(0 == ret);
      if ( S_ISFIFO(buf.st_mode) ||  S_ISSOCK(buf.st_mode))
      {
         sigemptyset(&sigpipe_mask);
         sigaddset(&sigpipe_mask, SIGPIPE);
         ret = pthread_sigmask(SIG_BLOCK, &sigpipe_mask, NULL);
         if ( -1 == ret )
         {
            perror("pthread_sigmask failed:");
            abort();
         }
         break;
      }
   }

   /* Find and run the command. */
   cmd = get_cmd(&lib_cmd_tree, argv[0]);
   if ( (cmd != NULL) && (cmd->func != NULL) )
   {
      cmd_res = cmd->func(argc, argv);
   }

   /* Send command reply */
   sig = alloc(sizeof(struct coli_start_cmd_r_sig), COLI_START_CMD_R);
   sig->coli_start_cmd_r.status = cmd_res;
   send(&sig, master);

   /* Wait here to die. */
   sig = receive(sel_die);
   free_buf(&sig);
   kill_proc(current_process());
}

/** ==================================================================== */
/**
 *   Starts the process that runs the command.
 *
 *   @param sock_fd    Socket file descriptor to retreive std streams
 *
 *   @param shell_pid  PID of command shell
 *
 *   @param argc       argc
 *
 *   @param argv       argv
 *
 *   @return           Result from the command, 0 if OK
 *
 *   @par Globals:
 *                     -
 *
 *   The command process is killed (command terminated) if the process
 *   that issued the command dies, when e.g. Ctrl-C is pressed.
 */
/* ===================================================================== */
static int
start_cmd_proc(int sock_fd, PROCESS shell_pid, int argc, char **argv)
{
   SIGSELECT     sel[] = {2, OS_ATTACH_SIG, COLI_START_CMD_R};
   union SIGNAL  *sig;
   PROCESS       pid;
   char          str[9];
   stdiofd_t     fds;
   int           ret;
   int           cmd_res = -1;

   save_std_stream(&fds);

   /* Is caller still alive? */
   sig = receive_w_tmo(1, sel);
   if (sig)
   {
         assert(sender(&sig) == shell_pid);
         free_buf(&sig);
         return cmd_res;
   }

   ret = set_std_stream_socket(sock_fd);
   if( 0 != ret)
   {
      return ret;
   }

   pid = create_process(OS_PRI_PROC, "cmd_proc", cmd_proc, 65536, 16,
                        0, 0, NULL, 0, 0);

   sprintf(str, "%x", shell_pid);
   set_env(pid, "PARENT", str);
   attach(NULL, pid);
   start(pid);

   /* Start command. */
   sig = alloc(sizeof(struct coli_start_cmd_sig), COLI_START_CMD);
   sig->coli_start_cmd.argc = argc;
   sig->coli_start_cmd.argv = argv;
   send(&sig, pid);

   /* Get reply */
   sig = receive(sel);

   if (sig->sig_no == OS_ATTACH_SIG)
   {
      /*
       * Reset fd:s before killing cmd_proc, this may leak output to the
       * registerator process' stdout/stderr.
       */
      if (sender(&sig) == shell_pid)
      {
         set_std_stream(&fds);
         kill_proc(pid);
         free_buf(&sig);

         /* Collect attach. (from cmd_proc) */
         sig = receive(sel);
         assert(sender(&sig) == pid);
         free_buf(&sig);
      }

      /*
       * If it wasn't the shell proc that died, then it is cmd_proc,
       * weird, but could happen. I.e command executes
       * kill_proc(current_process())
       */
      else
      {
         assert(sender(&sig) == pid);
         fprintf(stderr, "Warning: command %s died unexpectedly.\n", argv[0]);

         /* fflush and restore fd:s*/
         fflush(stdout);
         fflush(stderr);
         set_std_stream(&fds);
      }
   }
   else
   {
      /*
       * cmd_proc normal exit
       */
      cmd_res = sig->coli_start_cmd_r.status;
      free_buf(&sig);

      /* fflush and restore fd:s*/
      fflush(stdout);
      fflush(stderr);
      set_std_stream(&fds);

      /* Send suicide sig. */
      sig = alloc(sizeof(struct coli_finish_cmd_sig), COLI_FINISH_CMD);
      send(&sig, pid);

      /* Collect attach. (from cmd_proc) */
      sig = receive(sel);
      assert(sender(&sig) == pid);
      free_buf(&sig);
   }

   return cmd_res;
}

/** ==================================================================== */
/**
 *   Spawns the command specified in the first string of buf.
 *
 *   @param sock_fd    Socket file descriptor to retreive std streams
 *
 *   @param buf        Buffer with null terminated strings, that
 *                     corresponds to argv
 *   @param len        Size of buf
 *   @param shell_pid  PID of command shell
 *
 *   @return           Result from the command, 0 if OK
 *
 *   @par Globals:
 *                     -
 */
/* ===================================================================== */
static int
spawn_cmd(int sock_fd, char *buf, int len, PROCESS shell_pid)
{
   cmd_t *cmd;
   int    ret;
   int    argc;
   char **argv;

   ret = unpack_args(buf, len, &argc, &argv);
   if ( ret == 0 )
   {
      cmd = get_cmd(&lib_cmd_tree, buf);
      if ( (cmd != NULL) &&  (cmd->func != NULL) )
      {
         ret = start_cmd_proc(sock_fd, shell_pid, argc, argv);
      }
      else
      {
         ret = -1;
      }

      free(argv);
   }

   return ret;
}

/** ==================================================================== */
/** 
 *   Executes the command in the COLI execute command signal. The COLI
 *   exit command signal is sent when the command is finished.
 * 
 *   @param sig        COLI execute command signal
 * 
 *   @return           -
 *
 *   @par Globals:     
 *                     --
 */
/* ===================================================================== */
static void
exec_command(union SIGNAL *sig)
{
   char          *sock_name;
   union SIGNAL  *reply;
   int           ret;
   OSATTREF      ref;
   int           sock_fd;
   PROCESS       shell_pid;

   sock_name = get_sock_name();
   sock_fd = open_socket(sock_name);
   assert(-1 != sock_fd);

   shell_pid = sender(&sig);

   reply = alloc(sizeof(struct coli_exec_cmd_r_sig) + strlen(sock_name),
                 COLI_EXEC_CMD_R);
   strcpy(reply->coli_exec_cmd_r.socket, sock_name);
   reply->coli_exec_cmd_r.status = COLI_OK;
   send(&reply, shell_pid);

   ref = attach(NULL, shell_pid);
   ret = spawn_cmd(sock_fd,
                   sig->coli_exec_cmd.cmd,
                   sig->coli_exec_cmd.len,
                   shell_pid);
   detach(&ref);

   if ( sock_name )
      free(sock_name);

   reply = alloc(sizeof(struct coli_cmd_exit_sig), COLI_CMD_EXIT);
   reply->coli_cmd_exit.status = ret;
   send(&reply, shell_pid);
}

/** ==================================================================== */
/** 
 *   Callback function that is used when registering all commands to the
 *   COLI command server.
 * 
 *   @param cmd        Command struct with all information about the
 *                     command
 * 
 *   @return           -
 *
 *   @par Globals:     
 *                     --
 *
 *   The command is deleted locally if the registration at the COLI
 *   command server fails.
 */
/* ===================================================================== */
static void
reg_cmd(cmd_t *cmd, void *arg)
{
   PROCESS *colid_pid = arg;

   if ( register_command(*colid_pid, current_process(),
			 cmd->name, cmd->usage, cmd->description) != 0 )
      delete_cmd(&lib_cmd_tree, (char *)cmd->name);
}

/** ==================================================================== */
/** 
 *   Local command server process.
 * 
 *   @param            -
 * 
 *   @return           -
 *
 *   @par Globals:     
 *                     --
 */
/* ===================================================================== */
OS_PROCESS(cri_server)
{
   PROCESS       colid_pid;
   SIGSELECT     selAll[] = {0};
   union SIGNAL  *sig;
   int           connected;

   while (1)
   {
      /*
      **  Fix problems with race conditions
      */
      colid_pid = register_cri();
      sig = alloc(sizeof(union SIGNAL), COLI_ATTACH_SIG);
      attach(&sig, colid_pid);
      connected = 1;

      walk_cmds(&lib_cmd_tree, reg_cmd, &colid_pid);

      while (connected) {
	 sig = receive(selAll);

	 switch ( sig->sig_no )
	 {
	    case COLI_EXEC_CMD:
	       exec_command(sig);
	       break;
	    case COLI_ATTACH_SIG:
	       if ( sender(&sig) == colid_pid )
		  connected = 0;
	       break;
	    case COLI_REG_LOC_SIG:
               if (add_cmd(&lib_cmd_tree, sig->coli_reg_loc.cmd) == 0)
                  reg_cmd(sig->coli_reg_loc.cmd, &colid_pid);
	       break;
	    case COLI_RM_LOC_SIG:
		delete_cmd(&lib_cmd_tree, sig->coli_rm_loc.name);
		remove_command(colid_pid, sig->coli_rm_loc.name);
		break;
	    default:
	       break;
	 }

	 free_buf(&sig);
      }
   }

   kill_proc(current_process());
}

/** ==================================================================== */
/** 
 *   Returns the PID of the local command process. The process is started
 *   if it is not started yet.
 * 
 *   @param            -
 * 
 *   @return           PID of the local command process
 *
 *   @par Globals:     
 *                     --
 */
/* ===================================================================== */
static PROCESS
get_crid_pid(void)
{
   static PROCESS pid = 0;
   static pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;

   char proc_name[255];
 
   /*
   **  Chek if the cri server is started
   */
   if ( pid )
      return pid;

   pthread_mutex_lock(&mutex);

   /*
   **  Ensure that nobody else has started the cri server
   **  before the mutex was taken
   */
   if ( !pid )
   {
      sprintf(proc_name,"cri_server_%d",getpid());
      start(pid = create_process(OS_PRI_PROC,
				 proc_name,
				 cri_server,
				 16384,
				 16,
				 0, 0, NULL, 0, 0));
   }   pthread_mutex_unlock(&mutex);

   return pid;
}


/** ==================================================================== */
/** 
 *   Add a command to the shell.
 * 
 *   @param name       Command name
 *   @param usage      Usage string
 *   @param description Description
 *   @param func       Name of the function that handler the command
 * 
 *   @return           -
 *
 *   @par Globals:     
 *                     --
 *
 *   First line of description os treated as a short description. The
 *   rest (if more than one line) is the long description.
 */
/* ===================================================================== */
void
shell_add_cmd(const char *name, const char *usage,
	      const char *description, cmd_func_t func)
{
   union SIGNAL *sig;
   cmd_t    *cmd = malloc(sizeof(cmd_t));
   PROCESS  crid_pid = get_crid_pid();

   if ( cmd != 0 )
      memset(cmd, 0, sizeof(cmd_t));

   if ( (cmd != 0) &&
	((cmd->name = malloc(strlen(name)+1)) != 0) &&
	((cmd->usage = malloc(strlen(usage)+1)) != 0) &&
	((cmd->description = malloc(strlen(description)+1)) != 0) )
   {
      strcpy(cmd->name, name);
      strcpy(cmd->usage, usage);
      strcpy(cmd->description, description);
      cmd->func = func;

      sig = alloc(sizeof(struct coli_reg_loc_sig), COLI_REG_LOC_SIG);
      sig->coli_reg_loc.cmd = cmd;
      send(&sig, crid_pid);
   }
}

/** ==================================================================== */
/** 
 *   Remove a command from the shell.
 * 
 *   @param name       Command name
 *   @param func       Name of the function that handler the command.
 *                     Not used for now. Must be used for de-registering
 *                     commands with the same name.
 * 
 *   @return           -
 *
 *   @par Globals:     
 *                     --
 */
/* ===================================================================== */

void
shell_remove_cmd(const char *name, cmd_func_t func)
{
   union SIGNAL *sig;
   PROCESS crid_pid = get_crid_pid();

   assert(name);
   (void)func;

   sig = alloc(sizeof(struct coli_rm_loc_sig) + strlen(name) + 1,
               COLI_RM_LOC_SIG);
   strcpy(sig->coli_rm_loc.name, name);
   send(&sig, crid_pid);
}

/** ==================================================================== */
/** 
 *   Executes the specified shell command.
 *   OSE manual says: Only registered commands can be run with 
 *   shell_run_cmd(), not built-in shell commands. 
 *   But this implementation allows both, if intergrated shell is used, or 
 *   only 'built-in' cmds, otherwise.
 * 
 *   @param name       Command name
 *   @param tagname    Discarded.
 * 
 *   @return           -
 *
 *   @par Globals:     
 *                     --
 *
 */
/* ===================================================================== */
U32 
shell_run_cmd(const char *cmd, const OSADDRESS *taglist)
{
   char buf[256];
   FILE *ptr;
   int status;
   const OSADDRESS *tag;
   int *tag_status = NULL;

   /* Only handle tag RETURN_STATUS for now.
    * Most of the other tags are not applicable for Linux
    * and / or the implementation of COLI.
    */
   if (taglist) {
       for (tag = taglist; *tag != RUNCMD_TAGEND; tag++) {
           if (*tag == RUNCMD_RETURN_STATUS) {
               if (tag[1] == RUNCMD_TAGEND) {
                   return SHELL_EMISSING_ARG;
               }
               tag_status = (int *) tag[1];
           }
       }
   }

   ptr = popen(cmd, "r");
   if ( ptr== NULL){
      perror("popen failed:");
      return SHELL_EUNKNOWN_CMD;
   }

   while (fgets(buf, 256, ptr) != NULL){
      (void) fprintf(stdout, "%s", buf);
   }

   status = pclose(ptr);

   if (!WIFEXITED(status))
      return SHELL_ETERMINATED;

   if (tag_status) {
       *tag_status = WEXITSTATUS(status);
   }

   return SHELL_SUCCESS;
}

