/**
 *   ITC command main file.
 *
 *   @file itc_cmd.c
 *
 *   ITC command implementation main file. Contains the main function for
 *   the native ITC command.
 *
 *   Copyright (C) 2013 by Ericsson AB. All rights reserved. The
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
 *   Revised : 2013-08-25 Magnus Lindberg, magnus.k.lindberg@ericsson.com
 *   Change  : First version.
 * ========================================================================
 */

/* ========================================================================
 *   INCLUDE FILES
 * ========================================================================
 */
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <getopt.h>

#include "itc.h"
#include "itc_system.h"

#include "itc_gen_cmd.h"

/* ========================================================================
 *   DEFINITIONS
 * ========================================================================
 */
#define CMD_TMO 5000

/* ========================================================================
 *   TYPE DEFINITIONS
 * ========================================================================
 */
typedef enum {
        LIST_BRIEF = 0,
        LIST_NORMAL,
        LIST_VERBOSE,
        PRINT_MBOX,
        PRINT_POOL,
} listformat;

/* ========================================================================
 *   DATA DECLARATIONS
 * ========================================================================
 */

/* ========================================================================
 *   FUNCTION PROTOTYPES
 * ========================================================================
 */

/* ===================================================================== */
/**
 *   itc command main function
 *
 *   @param argc       Number if arguments
 *
 *   @param argv       Array of argument strings
 *
 *   @return           Return code of program, 0 for success.
 *
 *   @par Globals:     --
 *
 *   ITC command main function.
 */
/* ===================================================================== */
int main(int argc, char *argv[])
{
        int opt;
        itc_mbox_id_t mbox_id, infombox = ITC_NO_ID;
        listformat listformat = LIST_NORMAL;
        sortmethod sm = SORT_MBOXID;

        while ((opt = getopt(argc, argv, "slnhm:p:")) != -1) {
                switch(opt) {
                case 's':
                        listformat = LIST_BRIEF;
                        break;
                case 'l':
                        listformat = LIST_VERBOSE;
                        break;
                case 'n':
                        sm = SORT_MBOXNAME;
                        break;
                case 'm':
                        infombox = strtoul(optarg, NULL, 0);
                        listformat = PRINT_MBOX;
                        break;
                case 'p':
                        infombox = strtoul(optarg, NULL, 0);
                        listformat = PRINT_POOL;
                        break;
                case 'h':
                        printhelp();
                        exit(0);
                default:
                        printhelp();
                        exit(-1);
                        break;
                }
        }

        if(itc_init(1, ITC_MALLOC, NULL, ITC_NO_NAMESPACE, 0) != 0) {
                printf("itc_init failed, exiting\n");
                exit(-1);
        }
        mbox_id = itc_create_mailbox("itc_cmd", 0);


        switch(listformat) {
        case LIST_BRIEF:
                list_brief(sm);
                break;

        case LIST_NORMAL:
                list_normal(sm);
                break;

        case LIST_VERBOSE:
                list_verbose(sm);
                break;

        case PRINT_MBOX:
                print_mbox(infombox);
                break;

        case PRINT_POOL:
                print_allocinfo(infombox);
                break;

        default:
                exit(-1);
                break;
        }

        itc_delete_mailbox(mbox_id);
        itc_exit();

        return 0;
}
