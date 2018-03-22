#ifndef LNH_API_H_
#define LNH_API_H_

#include <stdint.h>

/* Initialize the Tiny Link Handler. */
extern int lnh_init();

/*

  TODO: Encapsulate TLNH API to enable safe usage in thread mode without
        having to disable interrupts during calls.

 */

#endif /* LNH_API_H_ */
