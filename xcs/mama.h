/******************************************************************************
 * Copyright Ericsson AB
 *
 * The copyright to the computer programs herein is the property of Ericsson AB.
 * The programs may be used and/or copied only with the written permission from
 * Ericsson AB or in accordance with the terms conditions stipulated in the
 * agreement/contract under which the programs have been supplied.
 *
 *****************************************************************************/

/**
 * @file mama.h
 * @brief Mama interface for children.
 *
 * This file defines the programming interface to Mama used by the children
 * launched by Mama. For the system integration interface, see
 * @ref libmama.h.
 *
 *******************************************************************************/

#ifndef __MAMA_H
#define __MAMA_H

#include <signal.h>

/* sync has higher priority than ping due to lower number */
#define SIGSYNC (SIGRTMIN+1)
#define SIGALIVE (SIGRTMIN+2)

/**
 * @brief Send sync to mama.
 *
 * @return 0 on success.
 *
 * Sends a sync signal to mama. The child should call this function when
 * it has completed initialization on which subsequent children may depend.
 *
 * See the @ref mamachildwaittimeoutms for more information.
 */
static inline int mama_send_sync(void)
{
    union sigval dummy = { 0 };
    return sigqueue(getppid(), SIGSYNC, dummy);
}
/**
 * @brief Tell Mama that you are alive.
 *
 * @return 0 on success.
 *
 * Sends an alive signal to mama. The child should call this function regularly
 * to notify Mama that the child is alive. This feature is available to detect
 * hanging processes. If Mama is configured to expect an alive signal from
 * a child within a certain interval, the child needs to send it.
 * Otherwise Mama will think the child has hung and will take action
 * accordingly.
 *
 * See the @ref mamachildalivetimeoutms for more information.
 */
static inline int mama_send_alive(void)
{
    union sigval dummy = { 0 };
    return sigqueue(getppid(), SIGALIVE, dummy);
}

/**
 * @brief Tell Mama to shut down gracefully.
 *
 * @return 0 on success.
 *
 * Calling this function will tell Mama to shut down the system gracefully and
 * restart.
 *
 * See the @ref mamaglobalshutdowntimeoutms and
 * @ref mamaglobalonlymanagedchildrencanshutdown for more information.
 */
static inline int mama_send_shutdown(void)
{
    return kill(getppid(), SIGTERM);
}

#endif /* __MAMA_H */
