/**
 * Copyright Ericsson AB 2009-2014 - All Rights Reserved
 *
 * No part of this software may be reproduced in any form without the written
 * permission of the copyright owner.
 */

/**
 * This file includes function prototypes and data type definitions that
 * implement same interface as OSE header files.
 *
 * As ITC is used for inter-process communication, certain data types are
 * defined as ITC types where appropriate.
 */

#ifndef _LITS_OSE_H
#define _LITS_OSE_H

/* Make sure that ose headers are not included before this one. */
#ifdef OSE
# error "Other OSE headers have been already included."
#endif

#ifdef OSE_CK_COMPAT_MODE
#include <osetypes.h>
#endif
#include <osarch.h>

#ifndef OSE_LEGACY_SUPPORT
#include <itc.h>
#endif

#define _LITS_IS_ITC_OSE_ 1

#ifdef __cplusplus
extern "C" {
#endif

#define OSE

#ifndef OSE_CK_COMPAT_MODE
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

#ifndef OSE_LEGACY_SUPPORT
typedef itc_monitor_id_t OSATTREF;
typedef size_t OSBUFSIZE;
typedef uint32_t OSTIME;
typedef uint32_t OSTMOREF;
typedef uint32_t SIGSELECT;
typedef itc_mbox_id_t PROCESS;
typedef uint32_t cpuid_t;
#else
typedef unsigned long OSATTREF;
typedef unsigned long OSBUFSIZE;
typedef unsigned long OSTIME;
typedef OSADDRESS OSTMOREF;
typedef unsigned long SIGSELECT;
typedef unsigned long PROCESS;
#endif /* OSE_LEGACY_SUPPORT */
#endif /* OSE_CK_COMPAT_MODE */

#if defined(__cplusplus) && !defined(OSE_CK_COMPAT_MODE)
#define OS_PROCESS(name) extern "C" void name(void)
#else
#define OS_PROCESS(name) void name(void)
#endif

#ifndef OSE_CK_COMPAT_MODE
# ifndef NULL
#  if defined(__cplusplus)
#   define NULL 0
#  else
#   define NULL ((void *)0)
#  endif
# endif

# define NIL NULL
union SIGNAL;	/* Forward declaration */
#endif /* OSE_CK_COMPAT_MODE */

#define OS_ATTACH_SIG (252)     /* !-SIGNO(SIGSELECT)-! */

#ifndef OSE_LEGACY_SUPPORT
#define OSE_ILLEGAL_ATTREF ((itc_mbox_id_t)0)
#define OSE_ILLEGAL_PROCESS ((itc_mbox_id_t)0)
#define OSE_ILLEGAL_TMOREF ((itc_mbox_id_t)0)
#else
#define OSE_ILLEGAL_ATTREF ((unsigned long)0)
#define OSE_ILLEGAL_PROCESS ((unsigned long)0)
#define OSE_ILLEGAL_TMOREF ((unsigned long)0)
#endif

#define PROC_ATTR_PID 1
#define PROC_ATTR_POOLID 2

enum PROCESS_TYPE
{
    OS_PRI_PROC = 0,
    OS_BG_PROC  = 64,
    OS_INT_PROC = 128,
    OS_TI_PROC  = 256,
    OS_PHANTOM  = 512,
    OS_BLOCK    = 1024,
    OS_ZOOMBIE  = 2048,
    OS_ILLEGAL  = 4096
};

enum PROCESS_STATUS
{
    OS_RECEIVE     = 1,
    OS_DELAY       = 2,
    OS_SEMAPHORE   = 4,
    OS_FSEMAPHORE  = 8,
    OS_REMOTE      = 16,
    OS_STOPPED     = 32,
    OS_INTERCEPTED = 64
};

struct OS_redir_entry
{
    SIGSELECT sig;
    PROCESS pid;
};


/* This way we made the SEMAPHORE structure an incomplete type.
   The purpose of this is to prevent users to use statically defined
   ose semaphores which can cause issues since previous semaphore
   structure was incompatible with posix's sem_t.  */

struct semaphore;
typedef struct semaphore SEMAPHORE;

struct OS_pid_list
{
    OSBUFSIZE count;
    PROCESS list[1]; /* Variable size */
};

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

struct OS_sig_info
{
    SIGSELECT sig_no;
    PROCESS owner;
    PROCESS sender_pid;
    PROCESS addressee_pid;
    OSBUFSIZE sig_size;
    OSBUFSIZE size_in_pool;
    OSADDRESS next;
};

/**
 * Macro to avoid warning when freeing a non union SIGNAL pointer
 */

#define free_buf_type(t, b)                     \
    do                                          \
    {                                           \
        union U                                 \
        {                                       \
            union SIGNAL **s;                   \
            t *x;                               \
        } u;                                    \
        u.x = (b);                              \
        free_buf(u.s);                          \
    } while(0)

/**
 * This creates a wrapper function to call "zz" prefixed functions to prevent
 * namespace clashes with native system functions.
 *
 * Code that this generates looks like following:
 *
 * void zzsend(union SIGNAL **sig, PROCESS to);
 * static inline
 * void send(union SIGNAL **sig, PROCESS to)
 * ...
 *
 * This and when compiler creates bytecode from static inline functions,
 * it will directly create the code that is inside this wrapper function.
 */
#pragma GCC system_header

#define LITS_WRAP_FUNC(type, name, ...)         \
    type zz ##name(__VA_ARGS__);                \
    static inline                               \
    type name(__VA_ARGS__)

LITS_WRAP_FUNC(PROCESS, addressee, union SIGNAL **sig)
{ return zzaddressee(sig); }

LITS_WRAP_FUNC(union SIGNAL*, alloc, OSBUFSIZE size, SIGSELECT signo)
{ return zzalloc(size, signo); }

LITS_WRAP_FUNC(void, cancel_tmo, OSTMOREF *tmoref)
{ zzcancel_tmo(tmoref); }

LITS_WRAP_FUNC(union SIGNAL*, cancel_tmo_sig, OSTMOREF *tmoref)
{ return zzcancel_tmo_sig(tmoref); }

LITS_WRAP_FUNC(PROCESS, current_process, void)
{ return zzcurrent_process(); }

LITS_WRAP_FUNC(PROCESS, get_bid, PROCESS pid)
{ return zzget_bid(pid); }

LITS_WRAP_FUNC(OSFSEMVAL, get_fsem, PROCESS pid)
{ return zzget_fsem(pid); }

LITS_WRAP_FUNC(struct OS_pcb*, get_pcb, PROCESS id)
{ return zzget_pcb(id); }

LITS_WRAP_FUNC(enum PROCESS_TYPE, get_ptype, PROCESS pid)
{ return zzget_ptype(pid); }

LITS_WRAP_FUNC(OSSEMVAL, get_sem, SEMAPHORE *sem)
{ return zzget_sem(sem); }

LITS_WRAP_FUNC(OSBOOLEAN, get_sig_info,
               struct OS_sig_info *sig_info,
               PROCESS pool_id,
               PROCESS pid,
               OSADDRESS sig
    )
{ return zzget_sig_info(sig_info, pool_id, pid, sig); }

LITS_WRAP_FUNC(OSTICK, get_systime, OSTICK *microsecs)
{ return zzget_systime(microsecs); }

LITS_WRAP_FUNC(OSTICK, get_ticks, void)
{ return zzget_ticks(); }

LITS_WRAP_FUNC(void, intercept, PROCESS id)
{ zzintercept(id); }

LITS_WRAP_FUNC(OSTMOREF, request_tmo, OSTIME timeout, SIGSELECT tmosigno)
{ return zzrequest_tmo(timeout, tmosigno); }

LITS_WRAP_FUNC(OSTMOREF, request_tmo_sig, OSTIME timeout, union SIGNAL **tmosig)
{ return zzrequest_tmo_sig(timeout, tmosig); }

LITS_WRAP_FUNC(void, restart_tmo, OSTMOREF *tmoref, OSTIME timeout)
{ zzrestart_tmo(tmoref, timeout); }

LITS_WRAP_FUNC(void, restore, union SIGNAL *sig)
{ zzrestore(sig); }

LITS_WRAP_FUNC(void, resume, PROCESS id)
{ zzresume(id); }

LITS_WRAP_FUNC(PROCESS, sender, union SIGNAL **sig)
{ return zzsender(sig); }

LITS_WRAP_FUNC(OSBUFSIZE, sigsize, union SIGNAL **sig)
{ return zzsigsize(sig); }

LITS_WRAP_FUNC(void, signal_fsem, PROCESS pid)
{ zzsignal_fsem(pid); }

LITS_WRAP_FUNC(void, signal_sem, SEMAPHORE *sem)
{ zzsignal_sem(sem); }

LITS_WRAP_FUNC(OSTIME, system_tick, void)
{ return zzsystem_tick(); }

LITS_WRAP_FUNC(void, tick, void)
{ zztick(); }

LITS_WRAP_FUNC(int, wake_up, void)
{ return zzwake_up(); }

LITS_WRAP_FUNC(void, error, OSERRCODE ecode)
{ zzerror(ecode); }

LITS_WRAP_FUNC(void, free_buf, union SIGNAL **sig)
{ zzfree_buf(sig); }

LITS_WRAP_FUNC(union SIGNAL*, receive, const SIGSELECT *sigsel)
{ return zzreceive(sigsel); }

LITS_WRAP_FUNC(void, send, union SIGNAL **sig, PROCESS to)
{ zzsend(sig, to); }

LITS_WRAP_FUNC(void, send_w_s, union SIGNAL **sig, PROCESS from, PROCESS to)
{ zzsend_w_s(sig, from, to); }

LITS_WRAP_FUNC(void, start, PROCESS id)
{ zzstart(id); }

LITS_WRAP_FUNC(void, stop, PROCESS id)
{ zzstop(id); }

LITS_WRAP_FUNC(void, error2, OSERRCODE ecode, OSERRCODE extra)
{ zzerror2(ecode, extra); }

LITS_WRAP_FUNC(union SIGNAL*, alloc_nil, OSBUFSIZE size, SIGSELECT signo)
{ return zzalloc_nil(size, signo); }

LITS_WRAP_FUNC(OSBOOLEAN, set_sigsize, union SIGNAL **sig, OSBUFSIZE newsize)
{ return zzset_sigsize(sig, newsize); }

LITS_WRAP_FUNC(union SIGNAL*, s_alloc, OSPOOLID pool,
               OSBUFSIZE size, SIGSELECT signo)
{ return zzs_alloc(pool, size, signo); }

LITS_WRAP_FUNC(union SIGNAL*, s_alloc_nil, OSPOOLID pool,
               OSBUFSIZE size, SIGSELECT signo)
{ return zzs_alloc_nil(pool, size, signo); }

/* Define ose_i.h functions in this file. */
#ifndef OSE_I

LITS_WRAP_FUNC(OSBOOLEAN, assign_linkhandler, char *pathname, PROCESS handler)
{ return zzassign_linkhandler(pathname, handler); }

LITS_WRAP_FUNC(OSATTREF, attach, union SIGNAL **sig, PROCESS id)
{ return zzattach(sig, id); }

LITS_WRAP_FUNC(OSBOOLEAN, clear_bp, PROCESS pid, OSADDRESS addr)
{ return zzclear_bp(pid, addr); }

LITS_WRAP_FUNC(PROCESS, create_block,
               char *name,
               OSUSER user,
               OSBOOLEAN use_remote_calls,
               PROCESS remote_call_server,
               OSBOOLEAN supervisor_mode
    )
{ return zzcreate_block(
        name,
        user,
        use_remote_calls,
        remote_call_server,
        supervisor_mode
        ); }

LITS_WRAP_FUNC(OSERRH*, create_error_handler,
               PROCESS id,
               OSERRH *entrypoint,
               OSADDRESS stack_size
    )
{ return zzcreate_error_handler(id, entrypoint, stack_size); }

LITS_WRAP_FUNC(void, create_pool,
               PROCESS bid,
               OSADDRESS base,
               OSADDRESS size,
               OSADDRESS sigsize_tab[],
               OSADDRESS stacksize_tab[]
    )
{ zzcreate_pool(bid, base, size, sigsize_tab, stacksize_tab); }

LITS_WRAP_FUNC(OSPOOLID, s_create_pool,
	       OSPOOLID poolid,
	       OSADDRESS base,
	       OSADDRESS size,
	       OSBUFSIZE const *sigsizes
    )
{ return zzs_create_pool(poolid, base, size, sigsizes); }

LITS_WRAP_FUNC(PROCESS, create_process,
               enum PROCESS_TYPE proc_type,
               const char *name,
               OSENTRYPOINT *entrypoint,
               OSADDRESS stack_size,
               OSPRIORITY priority,
               OSTIME timeslice,
               PROCESS block,
               struct OS_redir_entry *router_table,
               OSVECTOR vector,
               OSUSER user
    )
{ return zzcreate_process(
        proc_type,
        name,
        entrypoint,
        stack_size,
        priority,
        timeslice,
        block,
        router_table,
        vector,
        user
        ); }

LITS_WRAP_FUNC(SEMAPHORE*, create_sem, OSSEMVAL initial_val)
{ return zzcreate_sem(initial_val); }

LITS_WRAP_FUNC(void, delay, OSTIME timeout)
{ zzdelay(timeout); }

LITS_WRAP_FUNC(void, detach, OSATTREF *attRef)
{ zzdetach(attRef); }

LITS_WRAP_FUNC(void, flush, PROCESS *psel, PROCESS pid)
{ zzflush(psel, pid); }


LITS_WRAP_FUNC(struct OS_pid_list*, get_bid_list, OSUSER user)
{ return zzget_bid_list(user); }

LITS_WRAP_FUNC(char*, get_cpu, PROCESS pid)
{ return zzget_cpu(pid); }

LITS_WRAP_FUNC(char*, get_env, PROCESS id, const char *name)
{ return zzget_env(id, name); }

LITS_WRAP_FUNC(char*, get_env_list, PROCESS id, const char *first_name)
{ return zzget_env_list(id, first_name); }

LITS_WRAP_FUNC(OSADDRESS, get_envp, PROCESS id, const char *name)
{ return zzget_envp(id, name); }

LITS_WRAP_FUNC(OSBOOLEAN, get_mem,
               PROCESS id,
               OSADDRESS from,
               void *to,
               OSADDRESS size
    )
{ return zzget_mem(id, from, to, size); }

LITS_WRAP_FUNC(struct OS_pid_list*, get_pid_list, PROCESS bid)
{ return zzget_pid_list(bid); }

LITS_WRAP_FUNC(struct OS_pid_list*, get_pool_list, OSUSER user)
{ return zzget_pool_list(user); }

LITS_WRAP_FUNC(struct OS_poolcb*, get_poolcb, PROCESS pool_id)
{ return zzget_poolcb(pool_id); }

LITS_WRAP_FUNC(OSPRIORITY, get_pri, PROCESS pid)
{ return zzget_pri(pid); }

LITS_WRAP_FUNC(PROCESS, get_segid, PROCESS pid)
{ return zzget_segid(pid); }

LITS_WRAP_FUNC(PROCESS, get_sig_poolid, PROCESS bid)
{ return zzget_sig_poolid(bid); }

LITS_WRAP_FUNC(OSBOOLEAN, get_signal,
               PROCESS id,
               OSADDRESS mailptr,
               union SIGNAL **sig_copy,
               OSADDRESS *nextsig
    )
{ return zzget_signal(id, mailptr, sig_copy, nextsig); }

LITS_WRAP_FUNC(PROCESS, get_stk_poolid, PROCESS bid)
{ return zzget_stk_poolid(bid); }

LITS_WRAP_FUNC(OSUSER, get_uid, PROCESS id)
{ return zzget_uid(id); }

LITS_WRAP_FUNC(OSBOOLEAN, hunt,
               const char *name,
               OSUSER user,
               PROCESS *name_,
               union SIGNAL **hunt_sig
    )
{ return zzhunt(name, user, name_, hunt_sig); }

LITS_WRAP_FUNC(OSBOOLEAN, hunt_remote,
               const char *link,
               const char *name,
               PROCESS *pid
    )
{ return zzhunt_remote(link, name, pid); }

LITS_WRAP_FUNC(OSBOOLEAN, hunt_from,
               char *name,
               OSUSER user,
               PROCESS *name_,
               union SIGNAL **hunt_sig,
               PROCESS from
    )
{ return zzhunt_from(name, user, name_, hunt_sig, from); }

LITS_WRAP_FUNC(void, kill_proc, PROCESS id)
{ zzkill_proc(id); }

LITS_WRAP_FUNC(void, kill_sem, SEMAPHORE *sem)
{ zzkill_sem(sem); }

LITS_WRAP_FUNC(union SIGNAL*, receive_from,
               OSTIME timeout,
               const SIGSELECT *sel,
               PROCESS from
    )
{ return zzreceive_from(timeout, sel, from); }

LITS_WRAP_FUNC(union SIGNAL*, receive_sport,
               OSTIME timeout,
               SIGSELECT *sel,
               PROCESS *sport_list
    )
{ return zzreceive_sport(timeout, sel, sport_list); }

LITS_WRAP_FUNC(union SIGNAL*, receive_w_tmo, OSTIME timeout, const SIGSELECT *sel)
{ return zzreceive_w_tmo(timeout, sel); }

LITS_WRAP_FUNC(OSBOOLEAN, set_bp,
               PROCESS pid,
               OSADDRESS address,
               OSADDRESS attribute,
               union SIGNAL **trapsig
    )
{ return zzset_bp(pid, address, attribute, trapsig); }

LITS_WRAP_FUNC(OSBOOLEAN, set_env, PROCESS id, const char *name, const char *value)
{ return zzset_env(id, name, value); }

LITS_WRAP_FUNC(OSBOOLEAN, set_envp, PROCESS id, const char *name, OSADDRESS value)
{ return zzset_envp(id, name, value); }

LITS_WRAP_FUNC(void, set_fsem, OSFSEMVAL value, PROCESS pid)
{ zzset_fsem(value, pid); }

LITS_WRAP_FUNC(OSBOOLEAN, set_mem,
               PROCESS pid,
               void *from,
               OSADDRESS to,
               OSADDRESS size
    )
{ return zzset_mem(pid, from, to, size); }

LITS_WRAP_FUNC(OSBOOLEAN, set_pcb, PROCESS pid, char *cpuregs)
{ return zzset_pcb(pid, cpuregs); }

LITS_WRAP_FUNC(OSPRIORITY, set_pri, OSPRIORITY newpri)
{ return zzset_pri(newpri); }

LITS_WRAP_FUNC(OSPRIORITY, set_pri_for, PROCESS pid, OSPRIORITY newpri)
{ return zzset_pri_for(pid, newpri); }

LITS_WRAP_FUNC(void, set_redirection,
               PROCESS pid,
               struct OS_redir_entry *redir
    )
{ zzset_redirection(pid, redir); }

LITS_WRAP_FUNC(void, wait_fsem, OSFSEMVAL count)
{ zzwait_fsem(count); }

LITS_WRAP_FUNC(void, wait_sem, SEMAPHORE *sem)
{ zzwait_sem(sem); }

#ifndef OSE_CK_COMPAT_MODE
LITS_WRAP_FUNC(cpuid_t, ose_cpu_id, void)
{return zzose_cpu_id(); }

LITS_WRAP_FUNC(cpuid_t, ose_num_cpus, void)
{return zzose_num_cpus(); }
#endif

#endif /* OSE_I */

#ifdef __cplusplus
}
#endif


#endif /* _LITS_OSE_H */
