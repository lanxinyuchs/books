/**
 *   Handling of local commands.
 *
 *   @file
 *
 *   This file is a part of the COLI command server.
 *
 *   Copyright (C) 2011 by Ericsson AB. All rights reserved. The
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
 *   Revised : 2014-12-04 Ravineet Singh EAB/FJP/HB
 *   Change  : Fixed typos in daemonize_colid
 *
 *   Revised : 2014-10-30 Henrik Wallin
 *   Change  : Report no error if trying to remove a non-existing command (file).
 *
 *   Revised : 2014-10-17 Ravineet Singh EAB/FJP/HB
 *   Change  : Removed check of parent pid
 *             Added support for running colid in foreground.
 *
 *   Revised : 2014-10-14 Ravineet Singh EAB/FJP/HB
 *   Change  : Daemonize colid
 *
 *   Revised : 2014-09-09 Ravineet Singh EAB/FJP/HB
 *   Change  : Removed colish as a command line shell
 *
 *   Revised : 2014-07-14 Christoffer Cederwall EAB/FJP/HB
 *   Change  : Added shell_remove_cmd.
 *
 *   Revised : 2014-06-30 Ravineet Singh EAB/FJP/HB
 *   Change  : Added command 'coli-help' for integrated shell env.
 *
 *   Revised : 2014-01-23 Ravineet Singh EAB/FJP/HB
 *   Change  : Replaced local cmd_tree implementattion with common/shared
 *             cmd_tree implementation.
 *             Sockets directory creation removed. crid will create sockets
 *             in P_tmpdir dir. umask not needed, hence removed.
 *             Removed constructor/destructor (they are inherited at fork),
 *             instead signal handlers are
 *             installed at start of coli_server.
 *
 *   Revised : 2014-01-10 Stanislav Vovk
 *   Change  : Setting umask(000) for colid to make sure that permissions
 *             requested by e.g. mkdir() are also applied.
 *
 *   Revised : 2013-12-13 Ravineet Singh EAB/FJP/HB
 *   Change  : Removed autotool variable to find colish, instead use
 *             'which colish' to find colish.
 *
 *   Revised : 2013-11-27 Ravineet Singh EAB/FJP/HB
 *   Change  : Added support for coli commands integrated into host shell.
 *             Soft links are created/removed (add/remove command) for coli
 *             commands.
 *             Restructured register_commad function.
 *             Added constructor/desturctor and signal handlers to be able
 *             to cleanup.
 *
 *   Revised : 2013-04-25 Sridhar K.M
 *   Change  : find command response is modified to provide command process name in addition
 *             to commnd pid.
 *
 *   Revised : 2011-11-30 Lars Jönsson EAB/FJP/TB
 *   Change  : All command names are now allocated dynamically.
 *
 *   Revised : 2011-11-29 Lars Jönsson EAB/FJP/TB
 *   Change  : All signals are now using variable sized string instead of
 *             fix sized strings.
 *
 *   Revised : 2011-11-28 Lars Jönsson EAB/FJP/TB
 *   Change  : Cleanup of help, usage and description strings retrieval.
 *
 *   Revised : 2011-11-23 Lars Jönsson EAB/FJP/TB
 *   Change  : First version.
 * ========================================================================
 */

/* ========================================================================
 *   INCLUDE FILES
 * ========================================================================
 */
#ifndef _GNU_SOURCE
  #define _GNU_SOURCE
#endif
#include <assert.h>
#include <cmd_tree.h>
#include <coli.sig>
#include <errno.h>
#include <fcntl.h>
#include <linux/limits.h>
#include <ose.h>
#include <signal.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h> 
#include <syslog.h>
#include <unistd.h>

/* ========================================================================
 *   DEFINITIONS
 * ========================================================================
 */
/*
**  Attach signal declaration
*/
#define COLI_ATTACH	(COLI_SIGBASE - 1) /*!- SIGNO(struct coli_attach_sig) -!*/

struct coli_attach_sig {
      SIGSELECT  sig_no;
      char       name[1];
};

/* ========================================================================
 *   TYPE DEFINITIONS
 * ========================================================================
 */
union SIGNAL {
   SIGSELECT                         sig_no;
   struct coli_attach_sig            coli_attach;
   struct coli_reg_cmd_sig           coli_reg_cmd;
   struct coli_reg_cmd_r_sig         coli_reg_cmd_r;
   struct coli_find_cmd_sig          coli_find_cmd;
   struct coli_find_cmd_r_sig        coli_find_cmd_r;
   struct coli_part_cmd_sig          coli_part_cmd;
   struct coli_part_cmd_r_sig        coli_part_cmd_r;
   struct coli_help_cmd_sig          coli_help_cmd;
   struct coli_help_cmd_r_sig        coli_help_cmd_r;
   struct coli_usage_cmd_sig         coli_usage_cmd;
   struct coli_usage_cmd_r_sig       coli_usage_cmd_r;
   struct coli_descr_cmd_sig         coli_descr_cmd;
   struct coli_descr_cmd_r_sig       coli_descr_cmd_r;
   struct coli_rm_cmd_sig            coli_rm_cmd;
};

/* ========================================================================
 *   DATA DECLARATIONS
 * ========================================================================
 */
static void *colid_cmd_tree = NULL;

/* ========================================================================
 *   FUNCTION PROTOTYPES
 * ========================================================================
 */

static char*  coli_dir_default = DEFAULT_DIR;

/** ==================================================================== */
/**
 *   Report error.
 *
 *   @param   format 	Format string
 *
 *   @return  -
 *
 *   @par Globals:
 *                   -
 */
/* ===================================================================== */
static void
report_error(const char *format, ...)
{
   va_list ap;
   char* buf;
   int ret;

   va_start(ap, format);
   ret = vasprintf(&buf, format, ap);
   assert(-1 != ret);
   va_end(ap);

   openlog ("colid", LOG_CONS | LOG_PID | LOG_NDELAY, LOG_USER);
   syslog (LOG_ERR, "%s", buf);
   closelog ();

   free(buf);
}

/** ==================================================================== */
/**
 *   Get full path of colish binary by executing "which colish".
 *
 *   @return  path to full path of colish binary.
 *
 *   @par Globals:
 *                   -
 */
/* ===================================================================== */

static char*
get_colish_path(void)
{
   static char * path;
   const char cmd[] = "which colish";
   char * ret;
   FILE *ptr;
   int status;
   
   if (path)
      return path;

   path = malloc(PATH_MAX);
   assert(0 != path);

   ptr = popen((char*)cmd, "r");
   assert(NULL != ptr);
   ret = fgets(path, PATH_MAX, ptr);

   status = pclose(ptr);
   assert(WIFEXITED(status));
   status =  WEXITSTATUS(status);

   if (0 == status){
      assert( ret == path);
      path[strlen(path) - 1 ] = '\0';    /* Remove '\n'*/
   }
   else{
      report_error("Fatal error: colish not found. Aborting!");
      abort();
   }
   return path;
}

/** ==================================================================== */
/**
 *   Get coli base working dir.
 *
 *   @return  path to working dir
 *
 *   @par Globals:
 *                   coli_dir_default
 */
/* ===================================================================== */

static char*
get_coli_dir(void)
{
   static char*  cmds_path;
   if(!cmds_path)
   {
      cmds_path = getenv("COLI_CMDS_PATH");
      if(!cmds_path)
      {
	 cmds_path = coli_dir_default;
      }
      assert(cmds_path);
   }
   return cmds_path;
}

/* Forward declarations */
static void remove_command(char *name);
static void remove_coli_help(void);
/** ==================================================================== */
/**
 *   Colid cleanup, remove all commads.
 *
 *   @param signo       Signal nr, != 0 in case called by signal handler.
 *
 *   @return  -
 *
 *   @par Globals:
 *                   -
 */
/* ===================================================================== */
static void
colid_cleanup(int signo)
{
   cmd_t * cmd;

   /* Remove all commands. */
   do
   {
      cmd = get_cmd_partial(&colid_cmd_tree, "", NULL);
      if(cmd){
	 remove_command(cmd->name);
      }
   }while(cmd);

   /* Remove help command*/
   remove_coli_help();

   /* Leave the cmd install dir be, it might be other than oss polluting it. */

   if(signo){
      signal(signo, SIG_DFL);
      raise(signo);
   }
}

/** ==================================================================== */
/**
 *   Colid install signal handlers.
 *
 *   @param   -
 *
 *   @return  -
 *
 *   @par Globals:
 *                   -
 */
/* ===================================================================== */
static void
colid_signal_handler (void)
{
  struct sigaction cleanup;
 
  cleanup.sa_handler = colid_cleanup;
  sigemptyset (&cleanup.sa_mask);
  cleanup.sa_flags = 0;

  sigaction (SIGINT,  &cleanup, NULL);
  sigaction (SIGILL,  &cleanup, NULL);
  sigaction (SIGABRT, &cleanup, NULL);
  sigaction (SIGFPE,  &cleanup, NULL);
  sigaction (SIGSEGV, &cleanup, NULL);
  sigaction (SIGTERM, &cleanup, NULL);
}

/** ==================================================================== */
/**
 *   Removes a command.
 *
 *   @param name       Command name
 *
 *   @return           -
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
static void
remove_command(char *name)
{
   char*  buf;
   char* path;
   int ret; 

   path = get_coli_dir();

   ret = asprintf(&buf, "%s/%s", path, name);
   assert(-1 != ret);

   ret = remove(buf);
   if(0 != ret && errno != ENOENT)
   {
      report_error("coli_server: failed to remove cmd '%s', %s",
		   buf, strerror(errno));
   }
   free(buf);
   (void)delete_cmd(&colid_cmd_tree, name);
}

/** ==================================================================== */
/**
 *   Adds a command.
 *
 *   @param name       Command name
 *   @param usage      command usage
 *   @param Descr      Command description
 *   @param PID        Command owner
 *
 *   @return           -
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
static int
add_command(char* name, const char* usage, const char* descr, PROCESS pid)
{
   int   ret = -1;
   cmd_t *cmd = calloc(1, sizeof(cmd_t));

   assert(cmd);
   cmd->name = malloc(strlen(name)+1);
   cmd->usage = malloc(strlen(usage)+1);
   cmd->description = malloc(strlen(descr)+1);
   assert(cmd->name);
   assert(cmd->usage);
   assert(cmd->description);

   strcpy(cmd->name, name);
   strcpy(cmd->usage, usage);
   strcpy(cmd->description, descr);
   cmd->pid = pid;
   ret = add_cmd(&colid_cmd_tree, cmd);
   if (0 != ret)
   {
      free(cmd->name);
      free(cmd->usage);
      free(cmd->description);
      free(cmd);
   }
   else
   {
      /*
       * Note: A command name is only allowed to be installed once.
       *       This is in line with the binary tree implementation.
       *       It may however override a 'system' command, ex 'ls'
       *       which is not in colid colid_cmd_tree.
       */
      char* buf;
      int ret;

      ret = asprintf(&buf, "%s/%s", get_coli_dir(), name);
      assert(-1 != ret);

      ret = symlink(get_colish_path(), buf);
      if(0 != ret)
      {
	 (void)delete_cmd(&colid_cmd_tree, name);
	 report_error("coli_server: failed to add cmd '%s', %s",
		      buf, strerror(errno));
      }
      free(buf);
   }
   return ret;
}

/** ==================================================================== */
/**
 *   Adds a command. A COLI command registration reply signal is sent to
 *   the sender.
 *
 *   @param sig        COLI command registration signal
 *
 *   @return           -
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
static void
register_command(union SIGNAL *sig)
{
   union SIGNAL  *reply;
   union SIGNAL  *attach_sig;
   char          *name = &sig->coli_reg_cmd.str[sig->coli_reg_cmd.name];
   char          *usage = &sig->coli_reg_cmd.str[sig->coli_reg_cmd.usage];
   char          *descr = &sig->coli_reg_cmd.str[sig->coli_reg_cmd.descr];
   PROCESS       pid = sig->coli_reg_cmd.pid;
   int ret;

   reply = alloc(sizeof(struct coli_reg_cmd_r_sig), COLI_REG_CMD_R);
   ret = add_command(name, usage, descr, pid);
   if ( ret == 0 )
   {
      attach_sig = alloc(sizeof(struct coli_attach_sig) + strlen(name),
			 COLI_ATTACH);
      strcpy(attach_sig->coli_attach.name, name);
      attach(&attach_sig, pid);
   }
   reply->coli_reg_cmd_r.status = ret;
   send(&reply, sender(&sig));
}

/** ==================================================================== */
/**
 *   Searches for a command. A COLI find command reply signal is sent to
 *   the sender.
 *
 *   @param sig        COLI find command signal
 *
 *   @return           -
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
static void
find_command(union SIGNAL *sig)
{
   union SIGNAL  *reply;
   cmd_t         *cmd=get_cmd(&colid_cmd_tree, sig->coli_find_cmd.name);

   reply = alloc(sizeof(struct coli_find_cmd_r_sig), COLI_FIND_CMD_R);

   if ( cmd != 0 )
   {
      reply->coli_find_cmd_r.pid    = cmd->pid;
      reply->coli_find_cmd_r.status = COLI_OK;
   }
   else
   {
      reply->coli_find_cmd_r.status = -1;
   }

   send(&reply, sender(&sig));
}


/** ==================================================================== */
/**
 *   Searches for a command, where only the first part of the command is
 *   specified. A COLI part command reply signal is sent to the sender.
 *
 *   @param sig        COLI part command signal
 *
 *   @return           -
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
static void
part_command(union SIGNAL *sig)
{
   union SIGNAL  *reply;
   cmd_t         *cmd;
   char          *name = &sig->coli_part_cmd.str[sig->coli_part_cmd.name];
   char          *prev = &sig->coli_part_cmd.str[sig->coli_part_cmd.prev];

   if ( strlen(prev) )
     cmd = get_cmd_partial(&colid_cmd_tree, name, prev);
   else
     cmd = get_cmd_partial(&colid_cmd_tree, name, NULL);

   if ( cmd != 0 )
   {
      reply = alloc(sizeof(struct coli_part_cmd_r_sig) + strlen(cmd->name),
		    COLI_PART_CMD_R);
      strcpy(reply->coli_part_cmd_r.name, cmd->name);
      reply->coli_part_cmd_r.status = COLI_OK;
   }
   else
   {
      reply = alloc(sizeof(struct coli_part_cmd_r_sig), COLI_PART_CMD_R);
      reply->coli_part_cmd_r.status = -1;
   }

   send(&reply, sender(&sig));
}

/** ==================================================================== */
/**
 *   Calculates the length of the short help inside the description string.
 *
 *   @param src        Description string
 *
 *   @return           Length
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
static int
help_len(const char *src)
{
   char  *ptr = strchr(src, '\n');

   if ( ptr )
      return (int)(ptr - src);

   return strlen(src);
}

/** ==================================================================== */
/**
 *   Extracts and copies the short command string from the description
 *   string.
 *
 *   @param dest       Where to put the short help string
 *   @param src        Description string
 *
 *   @return           Pointer to the short help string (dest)
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
static char *
help_copy(char *dest, const char *src)
{
   int  len = help_len(src);

   strncpy(dest, src, len);
   dest[len] = '\0';

   return dest;
}

/** ==================================================================== */
/**
 *   Retrieves the short help string for a command. A COLI short help
 *   command reply signal is sent to the sender.
 *
 *   @param sig        COLI short help command signal
 *
 *   @return           -
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
static void
help_command(union SIGNAL *sig)
{
   union SIGNAL  *reply;
   cmd_t         *cmd=get_cmd(&colid_cmd_tree, sig->coli_help_cmd.name);

   if ( cmd != 0 )
   {

      int   help_size = help_len(cmd->description) + 1;

      reply = alloc(sizeof(struct coli_help_cmd_r_sig) - 1 +
		    help_size, COLI_HELP_CMD_R);

      reply->coli_help_cmd_r.status = COLI_OK;

      help_copy((char*)&reply->coli_help_cmd_r.help, cmd->description);
   }
   else
   {
      reply = alloc(sizeof(struct coli_help_cmd_r_sig), COLI_HELP_CMD_R);
      reply->coli_help_cmd_r.status = -1;
   }

   send(&reply, sender(&sig));
}

/** ==================================================================== */
/**
 *   Retrieves the usage string for a command. A COLI usage command reply
 *   signal is sent to the sender.
 *
 *   @param sig        COLI usage command signal
 *
 *   @return           -
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
static void
usage_command(union SIGNAL *sig)
{
   union SIGNAL  *reply;
   cmd_t         *cmd=get_cmd(&colid_cmd_tree, sig->coli_usage_cmd.name);

   if ( cmd != 0 )
   {
      int   usage_size = strlen(cmd->usage) + 1;

      reply = alloc(sizeof(struct coli_usage_cmd_r_sig) - 1 +
		    usage_size, COLI_USAGE_CMD_R);

      reply->coli_usage_cmd_r.status = COLI_OK;

      strcpy(reply->coli_usage_cmd_r.usage, cmd->usage);
   }
   else
   {
      reply = alloc(sizeof(struct coli_usage_cmd_r_sig), COLI_USAGE_CMD_R);
      reply->coli_usage_cmd_r.status = -1;
   }

   send(&reply, sender(&sig));
}

/** ==================================================================== */
/**
 *   Retrieves the description string for a command. A COLI description
 *   command reply signal is sent to the sender.
 *
 *   @param sig        COLI description command signal
 *
 *   @return           -
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
static void
descr_command(union SIGNAL *sig)
{
   union SIGNAL  *reply;
   cmd_t         *cmd=get_cmd(&colid_cmd_tree, sig->coli_descr_cmd.name);

   if ( cmd != 0 )
   {
      int   descr_size = strlen(cmd->description) + 1;

      reply = alloc(sizeof(struct coli_descr_cmd_r_sig) - 1 +
		    descr_size, COLI_DESCR_CMD_R);

      reply->coli_descr_cmd_r.status = COLI_OK;

      strcpy(reply->coli_descr_cmd_r.descr, cmd->description);
   }
   else
   {
      reply = alloc(sizeof(struct coli_descr_cmd_r_sig), COLI_DESCR_CMD_R);
      reply->coli_descr_cmd_r.status = -1;
   }

   send(&reply, sender(&sig));
}

/** ==================================================================== */
/**
 *   Create COLI default directories.
 *
 *   @param            -
 *
 *   @return           -
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
static void
create_cmds_dir(void)
{
   char* path;
   int ret;

   path = get_coli_dir();
   ret = mkdir(path, 0777);
   if (0 != ret && errno != EEXIST){
      report_error("Error %s: failed to create dir '%s', %s",
	 __func__, path, strerror(errno));
      exit(ret);
   }
   ret = access(path, R_OK|W_OK|X_OK);
   if(0 != ret){
      report_error("Error %s: failed to access dir '%s', %s",
		   __func__, path, strerror(errno));
      exit(ret);
   }
}

/** ==================================================================== */
/**
 *   Add help commad for intergrated shell env.
 *   Since (at least) bash has a builtin commad 'help' and has
 *   precedence, install coli_help in the file system.
 *
 *   @param            -
 *
 *   @return           -
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
static void
add_coli_help(void)
{
   char* buf;
   int ret;

   ret = asprintf(&buf, "%s/%s", get_coli_dir(), "coli-help");
   assert(-1 != ret);

   ret = symlink(get_colish_path(), buf);
   assert(-1 != ret);
}

/** ==================================================================== */
/**
 *   Remove coli help command.
 *
 *   @param            -
 *
 *   @return           -
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
static void
remove_coli_help(void)
{
   char* buf;
   int ret;

   ret = asprintf(&buf, "%s/%s", get_coli_dir(), "coli-help");
   assert(-1 != ret);

   ret = remove(buf);
   if(0 != ret && errno != ENOENT)
   {
      report_error("coli_server: failed to remove cmd '%s', %s",
		   buf, strerror(errno));
   }
}

/** ==================================================================== */
/**
 *   Daemonize COLI server.
 *
 *   @param            -
 *
 *   @return           -
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
static void
daemonize_colid(void)
{
   pid_t pid;
   pid_t sid;
   int fd;
   int ret;

   /* Fork off the parent process */
   pid = fork();
   if (pid < 0)
   {
      perror("colid: fork() failed");
      exit(-1);
   }

   /* Decouple. */
   if (pid > 0)
   {
      exit(0);
   }

   sid = setsid();
   if (sid < 0)
   {
      perror("colid: setsid() failed");
      exit(-2);
   }

   if ((chdir("/")) < 0)
   {
      perror("colid: chdir('/') failed");
      exit(-3);
   }

   fd = open("/dev/null",O_RDWR, 0);
   if ( -1 == fd)
   {
      perror("colid: open('/dev/null') failed");
      exit(-4);
   }

   ret = dup2(fd, STDIN_FILENO);
   if( -1 == ret )
   {
      perror("colid: dup2() failed");
      exit(-5);
   }
   
   ret = dup2(fd, STDOUT_FILENO);
   if( -1 == ret )
   {
      perror("colid: dup2() failed");
      exit(-5);
   }

   ret = dup2(fd, STDERR_FILENO);
   if( -1 == ret )
   {
      perror("colid: dup2() failed");
      exit(-5);
   }

   close (fd);
}

/** ==================================================================== */
/**
 *   main
 *   We need to daemonize before initializing LITS, otherwise ITC
 *   descriptors will be created in wrong process/sid
 *
 *   @param            argc
 *
 *   @param            argv
 *                     For debug purpose, '-f' can be provided, in
 *                     which case colid is not daemonized.
 *
 *   @return           ret val from __wrap_main in osemain.c
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
int
main(int argc, char **argv)
{
   extern int __wrap_main(int argc, char **argv);
   if (! (argc > 1 && strcmp(argv[1], "-f") == 0)) {
      daemonize_colid();
   }
   return  __wrap_main(argc, argv);
}

/** ==================================================================== */
/**
 *   COLI server process.
 *
 *   @param            -
 *
 *   @return           -
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
OS_PROCESS(coli_server)
{
   SIGSELECT     selAll[] = {0};
   union SIGNAL  *sig;

   colid_signal_handler();
   create_cmds_dir();
   add_coli_help();

   while (1) {
      sig = receive(selAll);

      switch ( sig->sig_no )
      {
	 case COLI_ATTACH:
	    remove_command(sig->coli_attach.name);
	    break;

	 case COLI_REG_CMD:
	    register_command(sig);
	    break;

	 case COLI_FIND_CMD:
	    find_command(sig);
	    break;

	 case COLI_PART_CMD:
	    part_command(sig);
	    break;

	 case COLI_HELP_CMD:
	    help_command(sig);
	    break;

	 case COLI_USAGE_CMD:
	    usage_command(sig);
	    break;

	 case COLI_DESCR_CMD:
	    descr_command(sig);
	    break;

	 case COLI_RM_CMD:
	    remove_command(sig->coli_rm_cmd.name);
	    break;	
	 default:
	    break;
      }

      free_buf(&sig);
   }

   kill_proc(current_process());
}
