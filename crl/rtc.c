#include <assert.h>
#include <unistd.h>
#include <sys/time.h>
#include "ose.h"
#include "osetypes.h"
/* XXX #include "cello_te_trace.h"*/
#include "ose_spi/rtc.h"

void rtc_request_alarm(ALARM_INFO *alarmInfo, const time_t *expires,
		       unsigned long period, union SIGNAL **alarmSig)
{
   (void)alarmInfo;
   (void)expires;
   (void)period;
   (void)alarmSig;
}

void rtc_get_time (struct TimePair *tvp)
{
   struct timeval tv;
   int ret;

   ret = gettimeofday(&tv, NULL);
   assert(0 == ret);
   tvp->seconds = tv.tv_sec;
   tvp->micros = (unsigned long)tv.tv_usec;
}

void rtc_set_time (const struct TimePair *tvp)
{
   struct timeval tv;
   int ret;

   tv.tv_sec = tvp->seconds;
   tv.tv_usec = tvp->micros;
   ret = settimeofday(&tv, NULL);

   assert(0 == ret);
}


