/* ----------------------------------------------------------------------
 * %CCaseFile:	sys_sysctl.c %
 * %CCaseRev:	/main/R5A/2 %
 * %CCaseDate:	2016-02-15 %
 * %CCaseDocNo: %
 * Author:      
 * Author: Lars Carlsson, <lars.carlsson@ericsson.com>
 *
 * Short description:
 * This is called from comsaFirewall (using appmServer:start_internal_lm)
 * to enable IPv6 on the veth devices created to connect default- and Oam-
 * network namespaces.
 *
 * Usage: ./comsa_sysctl <filepath> <value> 
 * requires two arguments
 * arg1 filepath relative to /proc/sys
 * arg2 string to write into path
 * Example: comsa_sysctl net/ipv6/conf/veth0/disable_ipv6 0
 *
 * ----------------------------------------------------------------------
 * %CCaseTemplateFile:  template.c %
 * %CCaseTemplateId: CCver: /main/1 %
 *
 * %CCaseCopyrightBegin%
 * Copyright (c) Ericsson AB 2016 All rights reserved.
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
 *
 * ----------------------------------------------------------------------
 *
 * Revision history:
 *
 * Rev        Date       Name        What
 * -----      -------    --------    --------------------------
 * R5         2016-01-20 etxlg       Created
 * R5/1       2016-01-20 etxlg       Got rid of %m warnings
 * R5/2       2016-02-15 etxlg       Always(almost) exit 0 to avoid llog errors
 * ----------------------------------------------------------------------
 */


#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <syslog.h>
#include <errno.h>


#define PROCSYS_PATH "/proc/sys/"

int
main(int argc, char **argv)
{
    int fd, res;
    char path[256];

    /*
    syslog(LOG_USER | LOG_ERR, "%s", "vorpal blade");
    syslog(LOG_USER | LOG_ERR, "real: %u effective: %u", getuid(), geteuid());
    */

    if(argc != 3) {
	syslog(LOG_USER | LOG_ERR, "%s", "need two arguments");
	return 1;
    }

    strcpy(path, PROCSYS_PATH);
    strncat(path, argv[1], sizeof path - 1);

    fd = open(path, O_WRONLY);
    if(fd == -1){
	syslog(LOG_USER | LOG_ERR, "open(%s): %s", path, strerror(errno));
	return 0;
    }
    res = write(fd, argv[2], strlen(argv[2]));
    if(res != strlen(argv[2])){
	syslog(LOG_USER | LOG_ERR, "write(%s): %s", path, strerror(errno));
	close(fd);
	return 0;
    }

    close(fd);
    return 0;
}
