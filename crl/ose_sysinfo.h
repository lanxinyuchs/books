/**
 * Copyright Ericsson AB 2013 - All Rights Reserved
 *
 * No part of this software may be reproduced in any form without the written
 * permission of the copyright owner.
 */

#ifndef _LITS_OSE_SYSINFO_H
#define _LITS_OSE_SYSINFO_H

#include <ose.h>

#ifdef __cplusplus
extern "C" {
#endif

#define SYSINFO_EILLEGAL_PID (0)
#define SYSINFO_ILLEGAL_ATTREF ((OSATTREF)0xFFFFFFFF)
#define SYSINFO_NO_ERR       (1)

#define SYSINFO_POOL_BUFFER_SIZES (8)

enum sysinfo_attref_state
{
    SYSINFO_ATTREF_UNAVAILABLE = 0,
    SYSINFO_ATTREF_ATTACHED = 1,
    SYSINFO_ATTREF_UNATTACHED = 2,
    SYSINFO_ATTREF_DETACHED = 3
};

enum sysinfo_proc_state_value
{
    SYSINFO_STATE_STOPPED = 0,
    SYSINFO_STATE_READY = 1,
    SYSINFO_STATE_RUNNING = 2,
    SYSINFO_STATE_WAITING = 3
};

struct sysinfo_system_attref_info
{
    enum sysinfo_attref_state state;
    PROCESS attacher;
    PROCESS attached;
    union SIGNAL *sig;
};

LITS_WRAP_FUNC(int, sysinfo_proc_name, PROCESS pid,
	       unsigned int size, char *name)
{ return zzsysinfo_proc_name(pid, size, name); }

LITS_WRAP_FUNC(int, sysinfo_proc_state, PROCESS pid,
	       unsigned int *state, unsigned int *substate)
{ return zzsysinfo_proc_state(pid, state, substate); }

LITS_WRAP_FUNC(int, sysinfo_proc_prio, PROCESS pid, OSPRIORITY *prio)
{ return zzsysinfo_proc_prio(pid, prio); }

LITS_WRAP_FUNC(int, sysinfo_system_attref_info, OSATTREF attref,
	       struct sysinfo_system_attref_info *info)
{ return zzsysinfo_system_attref_info(attref, info); }

LITS_WRAP_FUNC(int, sysinfo_signal_size, const union SIGNAL *sigbuf,
	       OSBUFSIZE *size)
{ return zzsysinfo_signal_size(sigbuf, size); }

LITS_WRAP_FUNC(int, sysinfo_pool_unused, OSPOOLID poolid, OSADDRESS *unused)
{ return zzsysinfo_pool_unused(poolid, unused); }

LITS_WRAP_FUNC(int, sysinfo_pool_used, OSPOOLID poolid, OSADDRESS *used)
{ return zzsysinfo_pool_used(poolid, used); }

LITS_WRAP_FUNC(int, sysinfo_signal_poolid, const union SIGNAL *sigbuf,
	       OSPOOLID *poolid)
{ return zzsysinfo_signal_poolid(sigbuf, poolid); }

LITS_WRAP_FUNC(int, sysinfo_signal_size_index, const union SIGNAL *sigbuf,
	       unsigned int *index)
{ return zzsysinfo_signal_size_index(sigbuf, index); }

LITS_WRAP_FUNC(int, sysinfo_pool_buffer_sizes, OSPOOLID poolid,
	       OSBUFSIZE *sizes)
{ return zzsysinfo_pool_buffer_sizes(poolid, sizes); }

#ifdef __cplusplus
}
#endif

#endif /* _LITS_OSE_SYSINFO_H */
