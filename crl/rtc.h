/* COPYRIGHT-ENEA-SRC-R2 *
 **************************************************************************
 * Copyright (C) 2004-2006 by Enea Software AB.
 * All rights reserved.
 *
 * This Software is furnished under a software license agreement and
 * may be used only in accordance with the terms of such agreement.
 * Any other use or reproduction is prohibited. No title to and
 * ownership of the Software is hereby transferred.
 *
 * PROPRIETARY NOTICE
 * This Software consists of confidential information.
 * Trade secret law and copyright law protect this Software.
 * The above notice of copyright on this Software does not indicate
 * any actual or intended publication of such Software.
 **************************************************************************
 * COPYRIGHT-END */
/**
 * @toc OSE:
 *
 * @file ose_spi/rtc.h
 *
 * @brief Specification of the real-time clock's (RTC) system programs
 *       interface (SPI) functionality for OSE RTC.
 *
 * @long RTC implements Alarm functionality.
 * Alarms are similar to time-outs but with a lower resolution (one second).
 * They are absolute in time, i.e. dependent on the real-time clock and
 * recalculated if the real-time clock is changed.
 */

#ifndef _RTC_H
#define _RTC_H

#include "ose.h"
#include "osetypes.h"
#include "time.h"

#ifdef __cplusplus
extern "C" {
#endif

/**
 * @type TimePair
 *
 * @brief The struct TimePair type specifies number of seconds and
 *        microseconds since Jan 1 1970 00:00:00.
 * @field seconds Seconds since Jan 1 1970 00:00:00.
 * @field micros  And microseconds.
 */
struct TimePair
{
  time_t seconds;
  unsigned long micros;
};

struct alarm_info;    /* Internal RTC structure. */

/**
 * @macro RTC_SCALE_NORMAL
 *
 * @brief Normal scale factor (no adjustment) for RTC adjustment.
 *
 * @long Constant used when setting the RTC scale factor with
 * rtc_get_scale/rtc_set_scale().
 *
 * @seealso rtc_set_scale
 */
#define RTC_SCALE_NORMAL      1000000000

/**
 * @macro RTC_SCALE_RANGE_TOP
 *
 * @brief Largest scale factor for RTC adjustment.
 *
 * @long Constant used when setting the RTC scale factor with
 * rtc_get_scale/rtc_set_scale().
 *
 * @seealso rtc_set_scale
 */
#define RTC_SCALE_RANGE_TOP 2000000000

/**
 * @macro RTC_SCALE_RANGE_BOT
 *
 * @brief Smallest scale factor for RTC adjustment.
 *
 * @long Constant used when setting the RTC scale factor with
 * rtc_get_scale/rtc_set_scale().
 *
 * @seealso rtc_set_scale
*/
#define RTC_SCALE_RANGE_BOT 1

/**
 * @type RtcStatus
 *
 * @include "ose_spi/rtc.h"
 *
 * @brief RTC status code.
 */
typedef S32 RtcStatus;

/**
 * @macro RTC_ENORMAL
 *
 * @include "ose_spi/rtc.h"
 *
 * @brief Status successful.
 */
#define RTC_ENORMAL 0

/**
 * @macro RTC_EZONE_NAME_NOT_FOUND
 *
 * @include "ose_spi/rtc.h"
 *
 * @brief No rule found for the given zone name.
 * @long Status codes specific to the rtc_tz_compile() call.
 */
#define RTC_EZONE_NAME_NOT_FOUND 1

/**
 * @macro RTC_EERROR_IN_RULES
 *
 * @include "ose_spi/rtc.h"
 *
 * @brief Syntax error or logical error in rules.
 */
#define RTC_EERROR_IN_RULES 2

/**
 * @macro RTC_ECOMPILATION_FAILED
 *
 * @include "ose_spi/rtc.h"
 *
 * @brief Illegal pointer, timeout or internal error.
 */
#define RTC_ECOMPILATION_FAILED 3

/**
 * @macro RTC_EINVALID_STACK_SIZE
 *
 * @include "ose_spi/rtc.h"
 *
 * @brief Invalid value of environment variable RTC_TZ_COMPILE_STACKSIZE.
 */
#define RTC_EINVALID_STACK_SIZE 4

/**
 * @macro RTC_EINVALID_TZ_DATA
 *
 * @include "ose_spi/rtc.h"
 *
 * @brief Status codes specific to the RTC_TZ_CONFIG_REPLY signal
 */
#define RTC_EINVALID_TZ_DATA      1

/**
 * @type ALARM_INFO
 *
 * @brief Contains vital information for identification of correct
 *        alarm request at cancellation.
 *
 * @field sigNo    Alarm signal number.
 * @field instance Unique instance number for alarm request.
 * @field pointer  Direct access pointer to alarm request.
 */
typedef struct
{
  SIGSELECT             sigNo;
  SIGSELECT             instance;
  void                 *pointer;
}
ALARM_INFO;

/**
 * @type RtcAlarm
 *
 * @brief Template for the returned alarm signal.
 *
 * @field sigNo    Signal number specified by user.
 * @field instance Unique instance number for unique identification.
 * @field time     Time alarm expired.
 */
struct RtcAlarm
{
  SIGSELECT             sigNo;
  SIGSELECT             instance;
  time_t                time;
  /* user data follows... */
};

/**
 * @function rtc_request_alarm
 *
 * @brief Request one or multiple alarms with application specified signal.
 *
 * @field alarmInfo Identification of alarm for cancellation.
 * @field expires   Time when alarm (first) expires.
 * @field period    Time interval (in seconds) between multiple
 *                  alarms, specify 0 for single alarm.
 * @field alarmSig  User supplied signal to return at expiration.
 *
 * @return void
 */
void rtc_request_alarm(ALARM_INFO *alarmInfo, const time_t *expires,
		       unsigned long period, union SIGNAL **alarmSig);

/**
 * @function rtc_cancel_alarm
 *
 * @brief Cancel an alarm request. Remove all alarm signals from signal queue.
 *
 * @restriction Must only be called once for each requested alarm.
 *
 * @field alarmInfo Identification of alarm to cancel.
 *
 * @return Signal specified in request_alarm call.
 */
union SIGNAL *rtc_cancel_alarm(ALARM_INFO *alarmInfo);

/**
 * @function rtc_get_time
 *
 * @brief Get the system's real time.
 *
 * @field tvp System's time will be inserted here.
 *
 * @return void
 */
void rtc_get_time (struct TimePair *tvp);

/**
 * @function rtc_set_time
 *
 * @brief Set the system's real time.
 *
 * @field tvp System's new time.
 *
 * @return void
 *
 * @restriction  Note, periodic alarms can become historic when
 *               using this call. Each periodic alarm will then expire,
 *               generating an alarm. Also if several periods are passed,
 *               only one alarm is given for each requested periodic alarm.
 */
void rtc_set_time (const struct TimePair *tvp);

/*
 * @function rtc_adjust_time
 *
 * @brief Gracefully adjust the system's real time.
 *
 * @field micros Number of micros to adjust.
 *
 * @return void
 *
 * @restriction Obsolete, use rtc_set_scale().
 */

/* void rtc_adjust_time (long micros); */

/**
 * @function rtc_get_scale
 *
 * @brief Get the OSE RTC's scale factor, for compensating clock skew
 *        between hardware timer and wall-time.
 *
 * @return RTC scale factor. RTC_SCALE_NORMAL (1000000000) is the scale
 *         normal (no clock skew).
 *
 * @seealso rtc_set_scale
 */
U32 rtc_get_scale(void);

/**
 * @function rtc_set_scale
 *
 * @brief Set the OSE RTC's scale factor, for compensating clock skew
 *        between hardware timer and wall time.
 *
 * @long The hardware timer-driven clock is multiplied with the scale
 * factor when RTC time is calculated. RTC_SCALE_NORMAL (1000000000)
 * means scale factor 1 and is the scale normal (no clock skew).
 *
 * @field new_scale The new value of the scale.
 *
 * @return RTC scale factor.
 *
 * @restriction Valid scale numbers range from 1 up to 2000000000.
 *              If there is more drift than can be handled, the hardware
 *              timer clock needs to be setup differently.
 */
U32 rtc_set_scale(U32 new_scale);

/**
 * @function rtc_tz_compile
 *
 * @brief Prepare for time zone configuration by compiling the text
 *        format time zone rules into a binary format. Return a signal
 *        containing the resulting data.
 *
 * @field      zoneName         The name of the time zone, as it appears in
 *                              the rules, e.g. Europe/Stockholm
 * @field      timeZoneRules    A pointer to a zero terminated text string
 *                              describing the time zones.
 * @field      leapSecondRules  A pointer to a zero terminated text string
 *                              describing when leap seconds occurs.
 *                              In most systems, it should be set to NULL,
 *                              indicating that leap seconds are not used.
 * @field      errorCode        Where to store the resulting status code.
 * @field      errorMessage     Where to store any resulting error message
 *                              string. May be set to NULL if no message is
 *                              wanted.
 * @field      errorMessageSize Maximum size of errorMessage. May be set to
 *                              zero if no message is wanted.
 *
 * @return A pointer to signal buffer containing the signal
 *         RTC_CONFIG_REQUEST, with all its data filled in. The signal
 *         shall be sent to the ose_rtc process to complete the
 *         configuration.
 *
 *         If the compilation fails for any reason, NIL will be returned.
 *         The status code will indicate the type of error. The error
 *         message string will give the location and a description.
 *
 *         Possible status codes are RTC_ENORMAL,RTC_EZONE_NAME_NOT_FOUND
 *         RTC_EERROR_IN_RULES, RTC_ECOMPILATION_FAILED
 *
 * @seealso RTC_CONFIG_REQUEST
 */
union SIGNAL *
rtc_tz_compile(const char  * zoneName,
               const char  * timeZoneRules,
               const char  * leapSecondRules,
               RtcStatus   * errorCode,
               char        * errorMessage,
               unsigned int  errorMessageSize);

/**
 * @function rtc_mkgmtime
 *
 * @brief Convert from broken down (GMT) time to arithmetic time.
 *        Works as mktime, but uses GMT instead of local time.
 *
 * @field time Calendar time.
 *
 * @return Arithmetic time.
 */
time_t rtc_mkgmtime (struct tm *time);


#ifdef __cplusplus
}
#endif

#endif /* _RTC_H */
