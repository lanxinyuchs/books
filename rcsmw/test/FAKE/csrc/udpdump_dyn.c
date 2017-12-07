/* ----------------------------------------------------------------------
 * %CCaseFile:	udpdump_dyn.c %
 * %CCaseRev:	/main/R3A/3 %
 * %CCaseDate:	2015-02-19 %
 * %CCaseDocNo: %
 * Author:	erarafo
 * Author: <name>, <e-mail address>
 *
 * Short description: Dynamic program that listens to a UDP port and dumps
 * incoming datagrams to file.
 *
 * Currently this program depends on the log facility in ugt_common.
 * This program is however not upgrade related. Consider putting the
 * log facility in a more general library.
 *
 * ----------------------------------------------------------------------
 * %CCaseTemplateFile:  template.c %
 * %CCaseTemplateId: CCver: /main/1 %
 *
 * %CCaseCopyrightBegin%
 * Copyright (c) Ericsson AB 2015 All rights reserved.
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
 * ----------------------------------------------------------------------
 *
 * Revision history:
 *
 * Rev        Date       Name        What
 * -----      -------    --------    --------------------------
 * R3A/1      2015-02-18 erarafo     First version
 * R3A/2      2015-02-18 erarafo     Corrections
 * R3A/3      2015-02-19 erarafo     Minor adjustments
 * ----------------------------------------------------------------------
 */

#include "ugt_common.h"

#include <stdbool.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <string.h>
#include <errno.h>

#define LOG "udpdump_dyn"

#define BUFFER_SIZE 1024


static void
nothingMore() {
  for (; true; ) {
    sleep(60);
  }
}


/**
 * Listens to datagrams on the given port. Received datagrams are
 * dumped as printable text lines to the application log file.
 *
 * This code is a modified version of the code example in the
 * 'getaddrinfo' manpage.
 */
static void
udpDump(const char *port) {

  struct addrinfo hints;
  memset(&hints, 0, sizeof(struct addrinfo));
  hints.ai_flags = AI_PASSIVE|AI_NUMERICSERV;
  hints.ai_family = AF_INET;
  hints.ai_socktype = SOCK_DGRAM;
  hints.ai_protocol = 0;
  hints.ai_canonname = NULL;
  hints.ai_addr = NULL;
  hints.ai_next = NULL;

  struct addrinfo *result;
  int addrInfoRes = getaddrinfo(NULL, port, &hints, &result);
  if (addrInfoRes != 0) {
    APPLOG_T(LOG, "FATAL: failed to create address info, %s", gai_strerror(addrInfoRes));
    nothingMore();
  }

  bool success = false;
  int sfd;
  for (struct addrinfo *rp = result; rp != NULL; rp = rp->ai_next) {
    sfd = socket(rp->ai_family, rp->ai_socktype, rp->ai_protocol);
    if (sfd == -1) {
      APPLOG_T(LOG, "INFO: failed to create socket");
    }
    else {
      int bindResult = bind(sfd, rp->ai_addr, rp->ai_addrlen);
      if (bindResult != 0) {
        int errorNumber = errno;
        APPLOG_T(LOG, "INFO: failed to bind socket: %s", strerror(errorNumber));
      }
      else {
        success = true;
        break;
      }
    }
  }

  freeaddrinfo(result);

  if (!success) {
    APPLOG_T(LOG, "FATAL: failed to bind a socket");
    nothingMore();
  }

  char string[BUFFER_SIZE+1];
  for (; true;) {
    char buf[BUFFER_SIZE];
    struct sockaddr_storage peer_addr;
    socklen_t peer_addr_len = sizeof(struct sockaddr_storage);
    ssize_t nread =
        recvfrom(
            sfd,
            buf,
            BUFFER_SIZE,
            0,
            (struct sockaddr *)&peer_addr,
            &peer_addr_len);
    if (nread == -1) {
      APPLOG_T(LOG, "WARNING: receive failed");
    }
    else {
      for (int k = 0; k < nread; k++) {
        if (buf[k] >= 32 && buf[k] < 127) {
          string[k] = buf[k];
        }
        else {
          string[k] = '~';
        }
      }
      string[nread] = '\0';
      APPLOG_T(LOG, "UDP: %s", string);
    }
  }
}


/**
 * Expected arguments are:
 *     argc == 3
 *     argv[0] executable abspath
 *     argv[1] program name as set by caller (or taken from appdata?)
 *     argv[2] port (string containing decimal integer
 */
int
main(int argc, char *argv[]) {
  APPLOG_T(LOG,
      "INFO: --- %s starting, version: %s, arguments -",
      LOG,
      "%CCaseRev:	/main/R3A/3 %");
  for (int k = 0; k < argc; k++) {
    APPLOG_T(LOG, "INFO: argv[%d]: %s", k, argv[k]);
  }
  if (argc < 3) {
    APPLOG_T(LOG, "FATAL, too few arguments, argc: %d", argc);
    nothingMore();
  }
  const char *port = argv[2];
  udpDump(port);
}
