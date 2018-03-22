/**
 *   Test of error handling.
 *
 *   @file
 *
 *   This file is a part of the test suite for the Legacy IPC and Task
 *   Support (lits) library.
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
 *   Revised : 2011-04-12 Lars Jönsson EAB/FJP/TB
 *   Change  : Added support for test of static error handlers.
 *
 *   Revised : 2011-04-05 Lars Jönsson EAB/FJP/TB
 *   Change  : First version.
 * ========================================================================
 */

/* ========================================================================
 *   INCLUDE FILES
 * ========================================================================
 */
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ose.h>

/* ========================================================================
 *   DEFINITIONS
 * ========================================================================
 */
#define	HANDLE_SPERROR	0x00123456
#define	EXTRA_SPERROR	4250

#define	HANDLE_SBERROR	0x00234567
#define	EXTRA_SBERROR	4260

#define	HANDLE_PERROR	0x12345678
#define	EXTRA_PERROR	425

#define	HANDLE_BERROR	0x23456789
#define	EXTRA_BERROR	426

#define	HANDLE_SERROR	0x34567890
#define	EXTRA_SERROR	427

/* ========================================================================
 *   TYPE DEFINITIONS
 * ========================================================================
 */
typedef  struct error_info_t_
{
    OSBOOLEAN  user_called;
    OSERRCODE  ecode;
    OSERRCODE  extra;

} error_info_t;

/* ========================================================================
 *   DATA DECLARATIONS
 * ========================================================================
 */
/*
** System error handler. May only be changed by changed
** at compile time configuration.
*/
extern OSERRH *lits_system_error_handler;

/*
** The static error test process
*/
extern PROCESS test_static_errh_;

union SIGNAL
{
    SIGSELECT   sig_no;
};

/*
**  Storage of errorhandler info
*/
static error_info_t  seinfo;
static error_info_t  beinfo;
static error_info_t  peinfo;
static error_info_t  sbeinfo;
static error_info_t  speinfo;

/* ========================================================================
 *   FUNCTION PROTOTYPES
 * ========================================================================
 */

/** ==================================================================== */
/**
 *   System error handler that is used during test of error handling.
 *
 *   @param user_called
 *                     Set to 0 if called from lits. Set to non-zero if
 *                     called via error() and error2().
 *   @param ecode      Error code
 *   @param extra      Extra information that is error code depenedent
 *
 *   @return           Set to non-zero to indicate that the error is
 *                     handled
 *
 *   @par Globals:
 *                     seinfo
 */
/* ===================================================================== */
static OSADDRESS
system_error_handler(OSBOOLEAN user_called, OSERRCODE ecode, OSERRCODE extra)
{
    seinfo.user_called = user_called;
    seinfo.ecode       = ecode;
    seinfo.extra       = extra;

    return 1;
}

/** ==================================================================== */
/**
 *   Block error handler that is used during test of error handling.
 *
 *   @param user_called
 *                     Set to 0 if called from lits. Set to non-zero if
 *                     called via error() and error2().
 *   @param ecode      Error code
 *   @param extra      Extra information that is error code depenedent
 *
 *   @return           Set to non-zero if ecode is HANDLE_BERROR, to
 *                     indicate that the error is handled. Otherwise 0
 *
 *   @par Globals:
 *                     beinfo
 */
/* ===================================================================== */
static OSADDRESS
block_error_handler(OSBOOLEAN user_called, OSERRCODE ecode, OSERRCODE extra)
{
    beinfo.user_called = user_called;
    beinfo.ecode       = ecode;
    beinfo.extra       = extra;

    return ecode == HANDLE_BERROR ? 1 : 0;
}

/** ==================================================================== */
/**
 *   Process error handler that is used during test of error handling.
 *
 *   @param user_called
 *                     Set to 0 if called from lits. Set to non-zero if
 *                     called via error() and error2().
 *   @param ecode      Error code
 *   @param extra      Extra information that is error code depenedent
 *
 *   @return           Set to non-zero if ecode is HANDLE_PERROR, to
 *                     indicate that the error is handled. Otherwise 0
 *
 *   @par Globals:
 *                     peinfo
 */
/* ===================================================================== */
static OSADDRESS
process_error_handler(OSBOOLEAN user_called, OSERRCODE ecode, OSERRCODE extra)
{
    peinfo.user_called = user_called;
    peinfo.ecode       = ecode;
    peinfo.extra       = extra;

    return ecode == HANDLE_PERROR ? 1 : 0;
}

/** ==================================================================== */
/**
 *   Block error handler that is used during test of error handling.
 *
 *   @param user_called
 *                     Set to 0 if called from lits. Set to non-zero if
 *                     called via error() and error2().
 *   @param ecode      Error code
 *   @param extra      Extra information that is error code depenedent
 *
 *   @return           Set to non-zero if ecode is HANDLE_BERROR, to
 *                     indicate that the error is handled. Otherwise 0
 *
 *   @par Globals:
 *                     beinfo
 */
/* ===================================================================== */
OSADDRESS
static_block_error_handler(OSBOOLEAN user_called, OSERRCODE ecode,
                           OSERRCODE extra)
{
    sbeinfo.user_called = user_called;
    sbeinfo.ecode       = ecode;
    sbeinfo.extra       = extra;

    return ecode == HANDLE_SBERROR ? 1 : 0;
}

/** ==================================================================== */
/**
 *   Process error handler that is used during test of error handling.
 *
 *   @param user_called
 *                     Set to 0 if called from lits. Set to non-zero if
 *                     called via error() and error2().
 *   @param ecode      Error code
 *   @param extra      Extra information that is error code depenedent
 *
 *   @return           Set to non-zero if ecode is HANDLE_PERROR, to
 *                     indicate that the error is handled. Otherwise 0
 *
 *   @par Globals:
 *                     peinfo
 */
/* ===================================================================== */
OSADDRESS
static_process_error_handler(OSBOOLEAN user_called, OSERRCODE ecode,
                             OSERRCODE extra)
{
    speinfo.user_called = user_called;
    speinfo.ecode       = ecode;
    speinfo.extra       = extra;

    return ecode == HANDLE_SPERROR ? 1 : 0;
}

/** ==================================================================== */
/**
 *   Checks the result of error handling
 *
 *   @param str        String to precede the result string
 *   @param einfo      Error information to be checked
 *   @param user_called
 *                     Expected value of user_called
 *   @param ecode      Expected error code
 *   @param extra      Expected extra information
 *
 *   @return           Non-zero is result is OK, otherwise 0
 *
 *   @par Globals:
 *                     --
 *
 *   The einfo struct is reset after check.
 */
/* ===================================================================== */
static int
check_error_handling(char *str, error_info_t *einfo, OSBOOLEAN user_called,
                     OSERRCODE ecode, OSERRCODE extra)
{
    int  passed = 1;

    passed &= user_called == einfo->user_called;
    passed &= ecode == einfo->ecode;
    passed &= extra == einfo->extra;

    printf("%s [ecode=0x%lx, extra=%lu]: %s\n",
           str, einfo->ecode, einfo->extra, passed ? "OK" : "Fail");

    memset(einfo, 0, sizeof(error_info_t));

    return passed;
}

/** ==================================================================== */
/**
 *   Process for testing static block and process error handlers.
 *
 *   @param            -
 *
 *   @return           -
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
OS_PROCESS(test_static_errh)
{
    SIGSELECT     selAll[] = {0};
    union SIGNAL  *sig;
    int           passed = 1;

    sig = receive(selAll);

    printf("Test of static process error handler\n");
    error2(HANDLE_SPERROR, EXTRA_SPERROR);
    passed &= check_error_handling(" - Static process error handler",
                                   &speinfo, 1, HANDLE_SPERROR, EXTRA_SPERROR);
    passed &= check_error_handling(" - Static block error handler", &sbeinfo, 0, 0,
                                   0);
    passed &= check_error_handling(" - Process error handler", &peinfo, 0, 0, 0);
    passed &= check_error_handling(" - Block error handler", &beinfo, 0, 0, 0);
    passed &= check_error_handling(" - System error handler", &seinfo, 0, 0, 0);

    printf("Test of static block error handler\n");
    error2(HANDLE_SBERROR, EXTRA_SBERROR);
    passed &= check_error_handling(" - Static process error handler",
                                   &speinfo, 1, HANDLE_SBERROR, EXTRA_SBERROR);
    passed &= check_error_handling(" - Static block error handler",
                                   &sbeinfo, 1, HANDLE_SBERROR, EXTRA_SBERROR);
    passed &= check_error_handling(" - Process error handler", &peinfo, 0, 0, 0);
    passed &= check_error_handling(" - Block error handler", &beinfo, 0, 0, 0);
    passed &= check_error_handling(" - System error handler", &seinfo, 0, 0, 0);

    sig->sig_no = passed;
    send(&sig, sender(&sig));

    stop(current_process());
}

/** ==================================================================== */
/**
 *   Main test process
 *
 *   @param            -
 *
 *   @return           -
 *
 *   @par Globals:
 *                     --
 */
/* ===================================================================== */
OS_PROCESS(test)
{
    SIGSELECT     selAll[] = {0};
    union SIGNAL  *sig;
    int           passed = 1;

    /*
    **  A poosible enhancement to this program is to start a separate program
    **  that ends up in the default error handler and catch termination of
    **  that program to check if the defaault error works as expected.
    */

    lits_system_error_handler = system_error_handler;

    /* Ensure that einfo is reset */
    memset(&seinfo, 0, sizeof(error_info_t));
    memset(&sbeinfo, 0, sizeof(error_info_t));
    memset(&speinfo, 0, sizeof(error_info_t));
    memset(&beinfo, 0, sizeof(error_info_t));
    memset(&peinfo, 0, sizeof(error_info_t));

    /*
    **  Start and run test of static error handlers
    */
    sig = alloc(sizeof(union SIGNAL), 0);
    send(&sig, test_static_errh_);
    sig = receive(selAll);
    passed &= sig->sig_no;
    free_buf(&sig);

    printf("Test of static process error handler for another process\n");
    error2(HANDLE_SPERROR, EXTRA_SPERROR);
    passed &= check_error_handling(" - Static process error handler", &speinfo, 0,
                                   0, 0);
    passed &= check_error_handling(" - Static block error handler",
                                   &sbeinfo, 1, HANDLE_SPERROR, EXTRA_SPERROR);
    passed &= check_error_handling(" - Process error handler", &peinfo, 0, 0, 0);
    passed &= check_error_handling(" - Block error handler", &beinfo, 0, 0, 0);
    passed &= check_error_handling(" - System error handler",
                                   &seinfo, 1, HANDLE_SPERROR, EXTRA_SPERROR);

    printf("Test of static block error handler without a process error handler\n");
    error2(HANDLE_SBERROR, EXTRA_SBERROR);
    passed &= check_error_handling(" - Static process error handler", &speinfo, 0,
                                   0, 0);
    passed &= check_error_handling(" - Static block error handler",
                                   &sbeinfo, 1, HANDLE_SBERROR, EXTRA_SBERROR);
    passed &= check_error_handling(" - Process error handler", &peinfo, 0, 0, 0);
    passed &= check_error_handling(" - Block error handler", &beinfo, 0, 0, 0);
    passed &= check_error_handling(" - System error handler", &seinfo, 0, 0, 0);

    create_error_handler(get_bid(current_process()), block_error_handler, 200);
    create_error_handler(current_process(), process_error_handler, 200);

    printf("Test of system error handler\n");
    error2(HANDLE_SERROR, EXTRA_SERROR);
    passed &= check_error_handling(" - Static process error handler", &speinfo, 0,
                                   0, 0);
    passed &= check_error_handling(" - Static block error handler", &sbeinfo, 0, 0,
                                   0);
    passed &= check_error_handling(" - Process error handler", &peinfo, 1,
                                   HANDLE_SERROR, EXTRA_SERROR);
    passed &= check_error_handling(" - Block error handler", &beinfo, 1,
                                   HANDLE_SERROR, EXTRA_SERROR);
    passed &= check_error_handling(" - System error handler", &seinfo, 1,
                                   HANDLE_SERROR, EXTRA_SERROR);

    printf("Test of block error handler\n");
    error2(HANDLE_BERROR, EXTRA_BERROR);
    passed &= check_error_handling(" - Static process error handler", &speinfo, 0,
                                   0, 0);
    passed &= check_error_handling(" - Static block error handler", &sbeinfo, 0, 0,
                                   0);
    passed &= check_error_handling(" - Process error handler", &peinfo, 1,
                                   HANDLE_BERROR, EXTRA_BERROR);
    passed &= check_error_handling(" - Block error handler", &beinfo, 1,
                                   HANDLE_BERROR, EXTRA_BERROR);
    passed &= check_error_handling(" - System error handler", &seinfo, 0, 0, 0);

    printf("Test of process error handler\n");
    error2(HANDLE_PERROR, EXTRA_PERROR);
    passed &= check_error_handling(" - Static process error handler", &speinfo, 0,
                                   0, 0);
    passed &= check_error_handling(" - Static block error handler", &sbeinfo, 0, 0,
                                   0);
    passed &= check_error_handling(" - Process error handler", &peinfo, 1,
                                   HANDLE_PERROR, EXTRA_PERROR);
    passed &= check_error_handling(" - Block error handler", &beinfo, 0, 0, 0);
    passed &= check_error_handling(" - System error handler", &seinfo, 0, 0, 0);

    /*
    **  Remove process error handler and check that the block
    **  error handler gets the errors
    */
    create_error_handler(current_process(), NULL, 200);

    printf("Test of block error handler (without process error handler)\n");
    error2(HANDLE_BERROR, EXTRA_BERROR);
    passed &= check_error_handling(" - Static process error handler", &speinfo, 0,
                                   0, 0);
    passed &= check_error_handling(" - Static block error handler", &sbeinfo, 0, 0,
                                   0);
    passed &= check_error_handling(" - Process error handler", &peinfo, 0, 0, 0);
    passed &= check_error_handling(" - Block error handler", &beinfo, 1,
                                   HANDLE_BERROR, EXTRA_BERROR);
    passed &= check_error_handling(" - System error handler", &seinfo, 0, 0, 0);

    /*
    **  Remove block (and process) error handler and check that the system
    **  error handler gets the errors
    */
    create_error_handler(get_bid(current_process()), NULL, 200);

    printf("Test of system error handler (without other handlers)\n");
    error2(HANDLE_SERROR, EXTRA_SERROR);
    passed &= check_error_handling(" - Static process error handler", &speinfo, 0,
                                   0, 0);
    passed &= check_error_handling(" - Static block error handler", &sbeinfo, 0, 0,
                                   0);
    passed &= check_error_handling(" - Process error handler", &peinfo, 0, 0, 0);
    passed &= check_error_handling(" - Block error handler", &beinfo, 0, 0, 0);
    passed &= check_error_handling(" - System error handler", &seinfo, 1,
                                   HANDLE_SERROR, EXTRA_SERROR);

    printf("\n Test suite result: %s\n\n", passed ? "PASSED" : "FAILED");

    exit(passed ? 0 : -1);
}
