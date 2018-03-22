/**
 *   Routines for sending and retrieving file descriptors via unix
 *   sockets.
 *
 *   @file
 *
 *   This file is common to the COLI shell and command lib.
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
 *   Revised : 2016-01-20 Ove Vesterlund
 *   Change  : Fixed checking of cmsg_type when receiving fd.
 *
 *   Revised : 2014-12-10 Henrik Wallin
 *   Change  : Changed open_socket to use mkstemp. This will decrease
 *             the risk that the filename generated will be in use already.
 *
 *   Revised : 2014-12-05 Ravineet Singh EAB/FJP/HB
 *   Change  : receive_fd blocks, again, and is signal interrupable.
 *
 *   Revised : 2014-09-13 Ravineet Singh EAB/FJP/HB
 *   Change  : receive_fd no longer blocks, instead returns an error
 *             after a few tries.
 *
 *   Revised : 2014-06-30 Ravineet Singh EAB/FJP/HB
 *   Change  : send_connection() does not return anything.
 *
 *   Revised : 2014-06-26 Ravineet Singh EAB/FJP/HB
 *   Change  : STDIN/STDOUT/STDERR wrongfully closed in send_connection().
 *
 *   Revised : 2014-01-09 Ravineet Singh EAB/FJP/HB
 *   Change  : Merged lib/fdh.c and colish/fdh.c into this file.
 *
 *   Revised : 2012-02-13 Lars Jönsson EAB/FJP/TB
 *   Change  : Fixed problem with writing to the socket from another user.
 *             Everyone is now allowed to use the socket.
 *
 *   Revised : 2011-11-23 Lars Jönsson EAB/FJP/TB
 *   Change  : First version.
 * ========================================================================
 */

/* ========================================================================
 *   INCLUDE FILES
 * ========================================================================
 */
#include <assert.h>
#include <errno.h>
#include <stdio.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/un.h>
#include <unistd.h>
#include <stdlib.h>

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

/** ==================================================================== */
/** 
 *   Opens a unix socket
 * 
 *   @param path       Socket name, should end with XXXXXX
 * 
 *   @return           Socket handler, -1 if error
 *
 *   @par Globals:     
 *                     --
 */
/* ===================================================================== */
int
open_socket(char *path)
{
   struct sockaddr_un unix_socket_name = {0};
   int fd;

   if (strlen(path) >= sizeof(unix_socket_name.sun_path))
      return -1;

   /* Use mkstemp to get a unique filename.
    * As the file opened is closed and removed before it is used in bind,
    * it is in theory possibile that someone steals it in between.
    * But that is a very small chance.
    */
   fd = mkstemp(path);
   if (fd == -1) {
      fprintf(stderr, "%s: ", path);
      perror("mkstemp");
      return -1;
   }
   close(fd);

   if (unlink(path) != 0) {
      fprintf(stderr, "%s: ", path);
      perror("unlink");
      return -1;
   }

   unix_socket_name.sun_family = AF_UNIX;
   strcpy(unix_socket_name.sun_path, path);

   fd = socket(PF_UNIX, SOCK_DGRAM, 0);
   if ( fd == -1 ) {
      perror("socket");
      return -1;
   }

   /*lint -save -e64 */
   if (bind(fd, (struct sockaddr *)&unix_socket_name, sizeof(unix_socket_name))) {
   /*lint -restore */
      perror("bind");
      close(fd);
      return -1;
   }

   /*
   **  Allow everyone to use the socket
   */
   if ( chmod(path, S_IRWXU|S_IRWXG|S_IRWXO) != 0 ) {
      perror("chmod");
      close(fd);
      unlink(path);
      return -1;
   }

   return fd;
}

/** ==================================================================== */
/** 
 *   Closes a socket
 * 
 *   @param fd         Socket descriptor
 * 
 *   @return           -
 *
 *   @par Globals:     
 *                     --
 */
/* ===================================================================== */
void
close_socket(int fd)
{
   struct sockaddr_un usn;
   socklen_t          len = sizeof(usn);
   
   /*lint -save -e64 */
   if ( getsockname(fd, (struct sockaddr *)&usn, &len) == 0 )
   /*lint -restore */
      unlink(usn.sun_path);

   close(fd);
}

/** ==================================================================== */
/**
 *   Receive file descriptor(s) via the socket.
 *
 *   @param fd         Socket descriptor
 *
 *   @return           File descriptor, -1 if error
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
int
receive_fd(int fd)
{
   struct msghdr msg;
   struct iovec iov;
   char buf[1];
   int rv;
   int connfd = -1;
   char ccmsg[CMSG_SPACE(sizeof(connfd))];
   struct cmsghdr *cmsg;

   iov.iov_base = buf;
   iov.iov_len = 1;

   msg.msg_name = 0;
   msg.msg_namelen = 0;
   msg.msg_iov = &iov;
   msg.msg_iovlen = 1;
   /* old BSD implementations should use msg_accrights instead of
    * msg_control; the interface is different. */
   msg.msg_control = ccmsg;
   msg.msg_controllen = sizeof(ccmsg); /* ? seems to work... */
   msg.msg_flags = 0;

   /**
    * Fixme:
    * The caller may die while we are in blocking socket receive, leading
    * to unresponsive cri_server...
    * Maybe have a nonblocking forever loop and report an error after x retries?
    */
   do
   {
      rv = recvmsg(fd, &msg, 0);
   }
   while(rv == -1 && errno == EINTR);

   if (rv == -1) {
      perror("recvmsg:");
      return -1;
   }

   cmsg = CMSG_FIRSTHDR(&msg);

   if (cmsg->cmsg_type != SCM_RIGHTS) {
      fprintf(stderr, "got control message of unknown type %d\n",
              cmsg->cmsg_type);
      return -1;
   }

   return *(int*)CMSG_DATA(cmsg);
}

/** ==================================================================== */
/** 
 *   Sends a file descriptor on the Unix socket.
 *   Aborts (asserts) in case send is unsuccessful.
 * 
 *   @param sock_fd    Unix socket
 *   @param fd         File descriptor
 * 
 *   @return           -
 *
 *   @par Globals:     
 *
 */
/* ===================================================================== */
static void
send_connection(struct sockaddr_un* unix_socket_name, int sock_fd, int fd)
{
   struct msghdr msg;
   char ccmsg[CMSG_SPACE(sizeof(fd))];
   struct cmsghdr *cmsg;
   struct iovec vec;  /* stupidity: must send/receive at least one byte */
   char *str = "x";
   int rv;
   
   msg.msg_name = (struct sockaddr*)unix_socket_name;
   msg.msg_namelen = sizeof(*unix_socket_name);
   
   vec.iov_base = str;
   vec.iov_len = 1;
   msg.msg_iov = &vec;
   msg.msg_iovlen = 1;
   
   /* old BSD implementations should use msg_accrights instead of 
    * msg_control; the interface is different. */
   msg.msg_control = ccmsg;
   msg.msg_controllen = sizeof(ccmsg);
   cmsg = CMSG_FIRSTHDR(&msg);
   cmsg->cmsg_level = SOL_SOCKET;
   cmsg->cmsg_type = SCM_RIGHTS;
   cmsg->cmsg_len = CMSG_LEN(sizeof(fd));
   *(int*)CMSG_DATA(cmsg) = fd;
   msg.msg_controllen = cmsg->cmsg_len;
   
   msg.msg_flags = 0;
   
   rv = sendmsg(sock_fd, &msg, 0);
   assert(-1 != rv);
}

/** ==================================================================== */
/** 
 *   Opens a Unix socket with the specified name.
 * 
 *   @param path       Socket name/path
 * 
 *   @return           Socket decsriptor if OK, otherwise 0
 *
 *   @par Globals:     
 *                     --
 */
/* ===================================================================== */
static int
open_unix_fd(struct sockaddr_un* unix_socket_name, char *path)
{
   int  sock_fd = -1;

   unix_socket_name->sun_family = AF_UNIX;

   if (strlen(path) >= sizeof(unix_socket_name->sun_path) - 1) return 0;

   strcpy(unix_socket_name->sun_path, path);
   sock_fd = socket(PF_UNIX, SOCK_DGRAM, 0);

   assert(sock_fd != -1);

   /* doesn't do anything at the moment:
    * connect(unix_socket_fd, (sockaddr*)unix_socket_name); */

   return sock_fd;
}

/** ==================================================================== */
/** 
 *   Send file descriptor(s) via the socket
 * 
 *   @param path       Socket name
 * 
 *   @return           -
 *
 *   @par Globals:     
 *                     --
 */
/* ===================================================================== */
void
send_fd(char *path)
{
   int  sock_fd;
   struct sockaddr_un unix_socket_name = {0};

   sock_fd = open_unix_fd(&unix_socket_name, path);
   send_connection(&unix_socket_name, sock_fd, STDIN_FILENO);
   send_connection(&unix_socket_name, sock_fd, STDOUT_FILENO);
   send_connection(&unix_socket_name, sock_fd, STDERR_FILENO);
   close(sock_fd);
}
