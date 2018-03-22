
#ifndef _MY_TMO_SERVER_H_
#define _MY_TMO_SERVER_H_
#include <itc.h>

/**********************************************************************
 ***** My little timeout server
 **********************************************************************/
#define TIMESPEC_TO_MS(x) (x.tv_sec * 1000ULL + x.tv_nsec / 1000000)


/* Create a tmo_server thread;
     "server_name", also the mailbox name.
   Returns non-zero if fail.
*/
extern int init_tmo_server( char *server_name );


/* Register a new timeout ;
     "tmo" in milliseconds.
     "tmo_msg" address to pointer to signal that will be returned when timeout expires.
   Returns a "handle" used to cancel timeout before it expires.
*/
extern void *register_tmo(uint32_t tmo, union itc_msg **tmo_msg );

/* Register a new timeout ;
     "tmo" in absolute milliseconds (minimizing drift when repeated timeouts).
     "tmo_msg" address to pointer to signal that will be returned when timeout expires.
   Returns a "handle" used to cancel timeout before it expires.

   #include <time.h>
   struct timespec ts_now;
   unsigned long long base_time;
   void * tmo_handle;

   clock_gettime(CLOCK_MONOTONIC, &ts_now);
   base_time = TIMESPEC_TO_MS(ts_now);
   tmo_handle = register_tmo_absolute(base_time + <offset_to_next_tmo>, ....)
*/
extern void *register_tmo_absolute(unsigned long long tmo,
                                   union itc_msg **tmo_msg );

extern void *register_tmo_absolute_with_sender(unsigned long long tmo,
                                               union itc_msg **tmo_msg,
                                               itc_mbox_id_t mbox);

/* Cancel timeout before it expires.
     "handle" is what you got back when you registered the timout.
     The signal that was provided in register_tmo will be thrown away.
 */
extern void cancel_tmo(void *handle);

#endif
