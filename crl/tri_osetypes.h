/**
 *   @COPYRIGHT
 *   COPYRIGHT (C) 2015 BY ERICSSON AB. ALL RIGHTS RESERVED. THE
 *   INFORMATION IN THIS DOCUMENT IS THE PROPERTY OF ERICSSON. EXCEPT
 *   AS SPECIFICALLY AUTHORIZED IN WRITING BY ERICSSON, THE RECEIVER
 *   OF THIS DOCUMENT SHALL KEEP THE INFORMATION CONTAINED HEREIN
 *   CONFIDENTIAL AND SHALL PROTECT THE SAME IN WHOLE OR IN PART FROM
 *   DISCLOSURE AND DISSEMINATION TO THIRD PARTIES. DISCLOSURE AND
 *   DISSEMINATIONS TO THE RECEIVER'S EMPLOYEES SHALL ONLY BE MADE ON
 *   A STRICT NEED TO KNOW BASIS.
 */

/**
 *   @FILE TRI_OSETYPES.H
 *   THE CONTENTS OF THIS FILE DECLARES OSE TYPES USED WHEN USING TRI
 *   WITHOUT LITS SUPPORT.
 */

/* ========================================================================
 *   HISTORY OF DEVELOPMENT:
 *   -----------------------
 *
 *   Revised : 2015-11-03 Anette Schött
 *   Change  : First version.
 *
 *   Revised : 2015-11-23 Anette Schött
 *   Change  : Addition of some typedefs, struct type and function
 *             declarations.
 * ========================================================================
 */

#ifdef NO_LITS

#ifndef TRI_OSETYPES_H
#define TRI_OSETYPES_H

/* ========================================================================
 *   INCLUDE FILES
 * ========================================================================
 */

#include <stdint.h>
#include <stdio.h>
#include "itc.h"

#ifdef __cplusplus
extern "C" {
#endif

/* ========================================================================
 *   DEFINITIONS
 * ========================================================================
 */

typedef enum { False = 0, True = 1 } Boolean;
typedef uint32_t U32;
typedef uint16_t U16;
typedef uint8_t U8;
typedef uint32_t SIGSELECT;

typedef unsigned long OSADDRESS;
typedef unsigned long OSUSER;
typedef signed long OSFSEMVAL;
typedef itc_mbox_id_t PROCESS;

struct OS_pcb
{
    OSADDRESS type;     /* enum PROCESS_TYPE */
    OSADDRESS status;   /* enum PROCESS_STATUS */
    OSADDRESS priority; /* OSPRIORITY */
    OSUSER user;
    OSFSEMVAL fsemvalue;
    OSADDRESS sigqueue;
    OSADDRESS attach_list;
    OSADDRESS stack_top;
    OSADDRESS stack_limit;
    PROCESS remote_server;
    OSADDRESS sig_cnt_in_q;
    OSADDRESS sig_cnt_owned;
    OSADDRESS max_sigsize;
    OSADDRESS sigsel_size;
    OSADDRESS line;
    OSADDRESS file;
    OSADDRESS name;
    OSADDRESS cpuregs;
    OSADDRESS wanted;
    char strings[1];
};

extern struct OS_pcb* zzget_pcb (PROCESS pid);

extern PROCESS zzcurrent_process(void);

union SIGNAL;   /* Forward declaration */
extern void zzfree_buf(union SIGNAL **sig);


#ifdef __cplusplus
}
#endif

#endif /* TRI_OSETYPES_H */

#endif /* NO_LITS */
