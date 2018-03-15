/**
 * Copyright Ericsson AB 2009-2014 - All Rights Reserved
 *
 * No part of this software may be reproduced in any form without the written
 * permission of the copyright owner.
 */

#ifndef _OSETYPES_H
#define _OSETYPES_H

#ifndef OSE_LEGACY_SUPPORT
#include <stdint.h>
#endif

#ifdef OSE_CK_COMPAT_MODE
#include <itc.h>
#endif

/*
**  Note! LITS is not verified on a 64 bit machine, but it is currently
**        assumed to work anyway (except for U64).
*/
#ifndef OSE_LEGACY_SUPPORT
typedef uint32_t U32;
typedef uint16_t U16;
typedef uint8_t  U8;
typedef int32_t  S32;
typedef int16_t  S16;
typedef int8_t   S8;
#else
typedef unsigned int U32;
typedef unsigned short int U16;
typedef unsigned char U8;
typedef int S32;
typedef short int S16;
typedef signed char S8;
#endif

/*
**  Definition of U64 depends on is OSEck is in use of not. Default is
*   OSE Delta defintion but this can be overridden using a define
*/

#ifdef OSE_CK_COMPAT_MODE

typedef uint64_t U64;
typedef  int64_t S64;

#else

#define MOV64(dest, src) (dest).lsw = (src).lsw, (dest).msw = (src).msw
#define EQU64(x, y)  ((x).lsw == (y).lsw && (x).msw == (y).msw)
#define NEQ64(x, y)  ((x).lsw != (y).lsw || (x).msw != (y).msw)
#define MAKEU64(dest, mswMA, lswMA) (dest).lsw = lswMA, (dest).msw = mswMA
#define GETU32LSW(l) ((l).lsw)
#define GETU32MSW(l) ((l).msw)

/*
**  Note! U64 is not compatible with Little endian
*/
typedef struct
{
    U32 msw;
    U32 lsw;
} U64;

#endif

typedef enum { False = 0, True = 1 } Boolean;

#ifdef OSE_CK_COMPAT_MODE
typedef unsigned long OSADDRESS;
typedef unsigned char OSBOOLEAN;
typedef unsigned long OSERRCODE;
typedef signed long OSFSEMVAL;
typedef unsigned long OSPPDKEY;
typedef unsigned char OSPRIORITY;
typedef unsigned long OSREPORTID;
typedef signed long OSREPORTVAL;
typedef signed long OSSEMVAL;
typedef unsigned long OSTICK;
typedef unsigned long OSUSER;
typedef unsigned short OSVECTOR;
typedef OSADDRESS (OSERRH)(OSBOOLEAN, OSERRCODE, OSERRCODE);
typedef void (OSENTRYPOINT)(void);
typedef unsigned long OSPOOLID;
typedef itc_monitor_id_t OSATTREF;
typedef size_t OSBUFSIZE;
typedef uint32_t OSTIME;
typedef uint32_t OSTMOREF;
typedef uint32_t SIGSELECT;
typedef itc_mbox_id_t PROCESS;

#ifndef NULL
# if defined(__cplusplus)
# define NULL 0
# else
# define NULL ((void *)0)
# endif
#endif

#define NIL NULL
union SIGNAL;	/* Forward declaration */
#endif /* OSE_CK_COMPAT_MODE */

#endif /* _OSETYPES_H */
