/**
 *   ITC command main file.
 *
 *   @file itc_gen_cmd.c
 *
 *   ITC command implementation support functions. These functions
 *   implement the meat of the command implementation and can be
 *   used in a variety of environments.
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
 *
 *   Revised : 2014-01-16 Magnus Lindberg, magnus.k.lindberg@ericsson.com
 *   Change  : Corrected itccmd -l printout of rx and tx statistics.
 *
 *   Revised : 2015-02-06 Magnus Lindberg, magnus.k.lindberg@ericsson.com
 *   Change  : Added printout of thread id in itccmd -l and itccmd -m.
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
} listformat;


union itc_msg {
        uint32_t              msgno;

        struct itc_mbox_added itc_mbox_added;
};

struct mbox_info {
        struct mbox_info    *next;
        struct mbox_info    *prev;
        int                  root;

        struct itc_mbox_info mbox_info;
};

/* ========================================================================
 *   DATA DECLARATIONS
 * ========================================================================
 */
static struct mbox_info *mboxlist = NULL;

static const  char *statetext[] = { "unused ",
                                    " mbox  ",
                                    " clone ",
                                    "shared ",
                                    "deleted"
};

static const char *rxtext[] = { "no ",
                                "yes"
};

static const  char *alloctext[] = { "malloc",
                                    "pool",
                                    "poolflex",
                                    "user defined"
};

static char helptext[] =
        "itccmd [-s] [-l] [-n] [-m <mbox id>] [-h] itc mailbox information command\n"
        "-s           brief listing\n"
        "-l           verbose listing\n"
        "-n           sort on mailbox name\n"
        "-m <mbox id> Print detailed mailbox information\n"
        "-p <mbox id> Print ITC allocation (pool) information\n"
        "-h           Print help text\n";

/* ========================================================================
 *   FUNCTION PROTOTYPES
 * ========================================================================
 */

/* ===================================================================== */
/**
 *   Prints command helptext
 *
 *   @return           -
 *
 *   @par Globals:     --
 *
 *   Prints ITC command helptext.
 */
/* ===================================================================== */
void printhelp(void)
{
        printf("%s", helptext);
}

/* ===================================================================== */
/**
 *   Inserts an mailbox into the mailbox list
 *
 *   @param sm         Method for sorting the mailboxes in the list.
 *
 *   @param mboxlist   List of mailboxes.
 *
 *   @param new        New mailbox to add to the list.
 *
 *   @return           -
 *
 *   @par Globals:     mboxlist
 *                     Global mailbox list.
 *
 *   This function inserts a new mailbox (struct mbox_info) into the
 *   global mailbox list. The mailboxes are sorted into the list according
 *   to the choosen sort method.
 */
/* ===================================================================== */
static void insert_mbox(sortmethod sm,
                        struct mbox_info *mboxlist,
                        struct mbox_info *new)
{
        struct mbox_info *ml;

        for(ml = ML_FIRST(mboxlist) ; ml != NULL ; ML_NEXT(ml)) {
                if(sm == SORT_MBOXID) {
                        if(new->mbox_info.mbox_id <
                           ml->mbox_info.mbox_id) {
                                break;
                        }
                } else if(sm == SORT_MBOXNAME) {
                        if(strcmp(new->mbox_info.name,
                                  ml->mbox_info.name) < 0) {
                                break;
                        }
                } else {
                        break;
                }
        }

        if(ml != NULL) {
                ML_BEFORE(ml, new);
        } else {
                ML_AT_END(mboxlist, new);
        }
}

/* ===================================================================== */
/**
 *   Get info about the mailboxes in the global mailbox list.
 *
 *   @return           -
 *
 *   @par Globals:     mboxlist
 *                     Global mailbox list.
 *
 *   This function gets all information about all the mailboxes in the
 *   global mailbox list.
 */
/* ===================================================================== */
static void get_mboxinfo()
{
        struct mbox_info *ml, *tmp;
        struct itc_mbox_info *mi;

        for(ml = ML_FIRST(mboxlist) ; ml != NULL ; ML_NEXT(ml)) {
                mi = itc_get_mailbox_info(ml->mbox_info.mbox_id);
                if(mi == NULL                 ||
                   mi->state == MBOX_UNUSED   ||
                   mi->state == MBOX_DELETED) {
                        tmp = ml;
                        ml = ml->prev;
                        ML_OUT(tmp);
                        free(tmp);
                } else {
                        ml->mbox_info = *mi;
                        memcpy(ml->mbox_info.name, mi->name,
                               (strlen(mi->name) + 1));
                }
        }
}

/* ===================================================================== */
/**
 *   Gets all mailboxes from ITC and adds them to list.
 *
 *   @param sm         Method for sorting the mailboxes in the list.
 *
 *   @return           -
 *
 *   @par Globals:     mboxlist
 *                     Global mailbox list.
 *
 *   This function retrieves all mailboxes from ITC and adds them to the
 *   global mailbox list according to the chosen sort method.
 */
/* ===================================================================== */
static void get_mboxlist(sortmethod sm)
{
        union itc_msg    *msg;
        struct mbox_info *mi;
        int last = 0;

        mboxlist = malloc(sizeof(struct mbox_info));
        if(mboxlist == NULL) {
                printf("Not enough memory for command itc\n");
                exit(-1);
        }
        ML_NEW(mboxlist);

        itc_get_mboxes();
        while(!last) {
                msg = itc_receive(ITC_NOFILTER, CMD_TMO, ITC_FROM_ALL);
                if(msg == NULL) {
                        printf("Timeout while waiting for mailbox list\n");
                        exit(-1);
                } else if(msg->msgno != ITC_MBOX_ADDED) {
                        printf("Unexpected message (0x%x) received while waiting for mailbox list\n",
                                msg->msgno);
                        itc_free(&msg);
                        exit(-1);
                }

                mi = malloc(sizeof(struct mbox_info) + strlen(msg->itc_mbox_added.mbox_name));
                if(mi == NULL) {
                        printf("Not enough memory for command itc\n");
                        exit(-1);
                }
                memset(mi, 0, sizeof(struct mbox_info) + (strlen(msg->itc_mbox_added.mbox_name)));

                mi->mbox_info.mbox_id = msg->itc_mbox_added.mbox_id;
                memcpy(mi->mbox_info.name, msg->itc_mbox_added.mbox_name,
                       (strlen(msg->itc_mbox_added.mbox_name) + 1));
                if(msg->itc_mbox_added.last) {
                        last = 1;
                }

                itc_free(&msg);

                insert_mbox(sm, mboxlist, mi);
        }

        while(1) {
                msg = itc_receive(ITC_NOFILTER, 0, ITC_FROM_ALL);
                if(msg == NULL) {
                        break;
                } else if(msg->msgno != ITC_MBOX_ADDED) {
                        printf("Unexpected message (0x%x) received while waiting for mailbox list\n",
                               msg->msgno);
                        itc_free(&msg);
                        exit(-1);
                } else {
                        mi = malloc(sizeof(struct mbox_info) + strlen(msg->itc_mbox_added.mbox_name));
                        if(mi == NULL) {
                                printf("Not enough memory for command itc\n");
                                exit(-1);
                        }

                        mi->mbox_info.mbox_id = msg->itc_mbox_added.mbox_id;
                        memcpy(mi->mbox_info.name, msg->itc_mbox_added.mbox_name,
                               (strlen(msg->itc_mbox_added.mbox_name) + 1));
                        itc_free(&msg);

                        insert_mbox(sm, mboxlist, mi);
                }
        }
}

/* ===================================================================== */
/**
 *   Clear and free global mailbox list
 *
 *   @return           -
 *
 *   @par Globals:     mboxlist
 *                     Global mailbox list.
 *
 *   Clears the global mailbox list and frees all entries in it.
 */
/* ===================================================================== */
static void free_mboxlist(void)
{
        struct mbox_info *ml;

        while((ml = ML_FIRST(mboxlist)) != NULL) {
                ML_OUT(ml);
                free(ml);
        }

        free(mboxlist);
        mboxlist = NULL;
}

/* ===================================================================== */
/**
 *   Print a brief output of all ITC mailboxes.
 *
 *   @param sm         Method for sorting the mailboxes in the list.
 *
 *   @return           -
 *
 *   @par Globals:     mboxlist
 *                     Global mailbox list.
 *
 *   Prints a list with all ITC mailboxes sorted according to sort
 *   methology. The list only contains mailbox id and name.
 */
/* ===================================================================== */
void list_brief(sortmethod sm)
{
        struct mbox_info *ml;

        get_mboxlist(sm);

        printf("Mailbox list\n");
        printf("---------------\n");
        printf("Mbox id    Name\n");
        for(ml = ML_FIRST(mboxlist) ; ml != NULL ; ML_NEXT(ml)) {
                printf("0x%08x %s\n", ml->mbox_info.mbox_id,
                       ml->mbox_info.name);
        }

        free_mboxlist();
}

/* ===================================================================== */
/**
 *   Print the default output list for all ITC mailboxes.
 *
 *   @param sm         Method for sorting the mailboxes in the list.
 *
 *   @return           -
 *
 *   @par Globals:     mboxlist
 *                     Global mailbox list.
 *
 *   Prints a list with all ITC mailboxes sorted according to sort
 *   methology. The list contains the default information.
 */
/* ===================================================================== */
void list_normal(sortmethod sm)
{
        struct mbox_info *ml;

        get_mboxlist(sm);

        get_mboxinfo();

        printf("Mailbox list\n");
        printf("--------------------------------------------\n");
        printf("Mbox id     state    in RX    RX queue  Name\n");
        for(ml = ML_FIRST(mboxlist) ; ml != NULL ; ML_NEXT(ml)) {
                printf("0x%08x %s    %s     %8ld  %s\n",
                       ml->mbox_info.mbox_id,
                       statetext[ml->mbox_info.state],
                       rxtext[ml->mbox_info.in_rx],
                       ml->mbox_info.rx_qlen,
                       ml->mbox_info.name);
        }

        free_mboxlist();
}

/* ===================================================================== */
/**
 *   Print the verbose output list for all ITC mailboxes.
 *
 *   @param sm         Method for sorting the mailboxes in the list.
 *
 *   @return           -
 *
 *   @par Globals:     mboxlist
 *                     Global mailbox list.
 *
 *   Prints a list with all ITC mailboxes sorted according to sort
 *   methology. The list contains detailed information.
 */
/* ===================================================================== */
void list_verbose(sortmethod sm)
{
        struct mbox_info *ml;

        get_mboxlist(sm);

        get_mboxinfo();

        printf("Mailbox list\n");

#ifdef ITC_STATS
        printf("---------------------------------------------------------------------------------------------------------\n");
        printf("Mbox id     Thread id  state                tx          rx       alloc        free  in RX    RX queue    RX FD  Name\n");
#else
        printf("------------------------------------------------------------------\n");
        printf("Mbox id      Thread id  state      in RX    RX queue   RX FD   Name\n");
#endif
        for(ml = ML_FIRST(mboxlist) ; ml != NULL ; ML_NEXT(ml)) {
                printf("0x%08x %8d    %s     ",
                       ml->mbox_info.mbox_id,
                       ml->mbox_info.tid,
                       statetext[ml->mbox_info.state]);
#ifdef ITC_STATS
                printf(" %11ld %11ld %11ld %11ld  ",
                       ml->mbox_info.stats.tx_msg,
                       ml->mbox_info.stats.rx_msg,
                       ml->mbox_info.stats.allocs,
                       ml->mbox_info.stats.frees);
#endif
                if(ml->mbox_info.rxfd == -1) {
                        printf(" %s     %8ld           %s\n",
                               rxtext[ml->mbox_info.in_rx],
                               ml->mbox_info.rx_qlen,
                               ml->mbox_info.name);
                } else {
                        printf(" %s     %8ld  %7d  %s\n",
                               rxtext[ml->mbox_info.in_rx],
                               ml->mbox_info.rx_qlen,
                               ml->mbox_info.rxfd,
                               ml->mbox_info.name);
                }
        }

        free_mboxlist();
}

/* ===================================================================== */
/**
 *   Print detailed information about one mailbox.
 *
 *   @param mbox_id    Mailbox ID to print information about.
 *
 *   @return           -
 *
 *   @par Globals:     --
 *
 *   Prints detailed information about one mailbox.
 */
/* ===================================================================== */
void print_mbox(itc_mbox_id_t mbox_id)
{
        struct itc_mbox_info *mi;
        int n_filter, i;

        mi = itc_get_mailbox_info(mbox_id);

        if(mi == NULL) {
                printf("Mailbox: 0x%08x\n", mbox_id);
                printf("Mailbox ID does not exist\n");
                return;
        }

        if(mi->state == MBOX_UNUSED) {
                printf("Mailbox: 0x%08x\n", mbox_id);
                printf("Mailbox ID not used yet\n");
        } else if(mi->state == MBOX_INUSE ||
                mi->state == MBOX_CLONE) {
                printf("Mailbox: 0x%08x\n", mbox_id);
		printf("Thread id: %d\n", mi->tid);
                printf("Mailbox: 0x%08x\n", mbox_id);
                printf("Name: %s\n", mi->name);
                if(mi->state == MBOX_CLONE) {
                        printf("Mailbox Clone, parent: 0x%08x\n", mi->parent_id);
                }
                printf("Currently receiving: %s\n", rxtext[mi->in_rx]);
                if(mi->in_rx) {
                        if(mi->filter[0] == 0) {
                                printf("Receives any\n");
                        } else {
                                n_filter = abs(mi->filter[0]);
                                if(n_filter > 4) {
                                        n_filter = 4;
                                }

                                if(mi->filter[0] & 0x80000000) {
                                        printf("Receives any messages but: ");
                                } else {
                                        printf("Receives messages: ");
                                }
                                for(i=1 ; i<=n_filter ; i++) {
                                        printf("0x%08x ", mi->filter[i]);
                                }
                                printf("\n");
                        }
                }
                printf("Receive queue length: %ld\n", mi->rx_qlen);
                if(mi->rxfd != -1) {
                        printf("Receive filedescriptor: %d", mi->rxfd);
                }

#ifdef ITC_STATS
                printf("\nMailbox statistics\n");
                printf("---------------------------------\n");
                printf("Transmitted messages: %11ld\n", mi->stats.tx_msg);
                printf("Received messages:    %11ld\n", mi->stats.rx_msg);
                printf("Receives with msg:    %11ld\n", mi->stats.rx_wmsg);
                printf("Receive timeouts:     %11ld\n", mi->stats.rx_tmo);
                printf("Allocations:          %11ld\n", mi->stats.allocs);
                printf("Frees:                %11ld\n", mi->stats.frees);
                printf("Locates:              %11ld\n", mi->stats.locates);
                printf("Pending locates:      %11ld\n", mi->stats.pend_locates);
                printf("Monitored by:         %11ld\n", mi->stats.monitors);
                printf("Monitors:             %11ld\n", mi->stats.mymonitors);
                printf("Pending monitors:     %11ld\n", mi->stats.pend_mymonitors);
#endif

        } else if(mi->state == MBOX_DELETED) {
                printf("Mailbox: 0x%08x\n", mbox_id);
                printf("Mailbox deleted\n");
        } else {
                printf("Mailbox: 0x%08x\n", mbox_id);
                printf("Mailbox illegal state(%d) BUG!\n",
                       mi->state);
       }
}

/* ===================================================================== */
/**
 *   Print information about ITC allocations for an ITC process.
 *
 *   @param mbox_id    Mailbox ID for an mailbox in the desired process.
 *
 *   @return           -
 *
 *   @par Globals:     --
 *
 *   Print information about ITC allocations for an ITC process.
 */
/* ===================================================================== */
void print_allocinfo(itc_mbox_id_t mbox_id)
{
        struct itc_alloc_info *ai;
        int i;

        ai = itc_get_alloc_info(mbox_id);
        if(ai == NULL) {
                printf("Requested mailbox process not found for id: 0x%08x\n",
                       mbox_id);
                return;
        }

        switch(ai->scheme) {
        case ITC_MALLOC:
                printf("ITC allocation scheme %s\n", alloctext[ai->scheme]);
                printf("ITC current total allocations from heap: %lu\n",
                       ai->info.malloc_info.tot_malloc);
                break;
        case ITC_POOL:
                printf("ITC allocation scheme %s\n", alloctext[ai->scheme]);
                printf("Pool total size: %10lu bytes\n", ai->info.pool_info.totsize);
                printf("Pool total free: %10lu bytes\n", ai->info.pool_info.totfree);
                for(i=0 ; i<ITC_POOL_BUFF_VALS ; i++) {
                        printf("Message size %5u   allocated %6u   free %6u\n",
                               ai->info.pool_info.size[i],
                               ai->info.pool_info.allocated[i],
                               ai->info.pool_info.free[i]);
                }
                break;
        case ITC_POOL_FLEX:
                printf("ITC allocation scheme %s\n", alloctext[ai->scheme]);
                printf("Pool total size: %10lu bytes\n", ai->info.pool_flex_info.totsize);
                printf("Pool total free: %10lu bytes\n", ai->info.pool_flex_info.totfree);
                for(i=0 ; i<ITC_POOL_BUFF_VALS ; i++) {
                        printf("Message size %5u   allocated %6u   free %6u\n",
                               ai->info.pool_flex_info.size[i],
                               ai->info.pool_flex_info.allocated[i],
                               ai->info.pool_flex_info.free[i]);
                }
                break;
        default:
                printf("itc command unable to interpret this (%d) allocation scheme\n",
                       ai->scheme);
                break;
        }
}
