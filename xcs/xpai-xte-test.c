#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <time.h>
#include <signal.h>
#include "xpai_xte_if.h"

#define XPAI_XTE_TEST_SECONDS 1446720654
#define XPAI_XTE_TEST_USECONDS 874643
#define XPAI_XTE_HOUR 3600
#define XPAI_XTE_TIMEOUT_SEC 10

static int test_set_utc_time()
{
	S32 result = 0;
	struct timespec time;
	long diff;

	result = XPAI_SetUtcTime(XPAI_XTE_TEST_SECONDS, XPAI_XTE_TEST_USECONDS);
	if (result != XPAI_SET_UTC_TIME_OK) {
		return 1;
	}

	if (clock_gettime(CLOCK_REALTIME, &time)) {
		return -1;
	}

	/* Some time passed between setting the clock and checking the value,
	 * instead of measuring it correctly, it's enough to check if the
	 * difference is within some tolerance interval */
	diff = time.tv_sec - XPAI_XTE_TEST_SECONDS;
	if (diff < 0 || diff > XPAI_XTE_HOUR) {
		return 1;
	}

	return 0;
}

static int test_timeout_not_released()
{
	timer_t timer_id;
	struct sigevent sev;
	struct itimerspec its;
	S32 result = 0;

	/* Reset time to epoch time */
	result = XPAI_SetUtcTime(0, 0);
	if (result != XPAI_SET_UTC_TIME_OK) {
		return 1;
	}

	/* Create the timer */
	memset(&sev, 0, sizeof(sev)); /* Initialize to avoid coverity complaining */
	sev.sigev_notify = SIGEV_NONE;
	if (timer_create(CLOCK_MONOTONIC, &sev, &timer_id) == -1) {
		return -1;
	}

	/* Start the timer */
	its.it_value.tv_sec = XPAI_XTE_TIMEOUT_SEC;
	its.it_value.tv_nsec = 0;
	/* Timer expires once */
	its.it_interval.tv_sec = 0;
	its.it_interval.tv_nsec = 0;
	if (timer_settime(timer_id, 0, &its, NULL) == -1) {
		return -1;
	}

	/* Set time to some time in 2015 */
	result = XPAI_SetUtcTime(XPAI_XTE_TEST_SECONDS, XPAI_XTE_TEST_USECONDS);
	if (result != XPAI_SET_UTC_TIME_OK) {
		return 1;
	}

	if (timer_gettime(timer_id, &its) == -1) {
		return -1;
	}

	if (its.it_value.tv_sec != 0) {
		printf("We don't have timeout yet, time until next expiration: %d\n",
			   (int)its.it_value.tv_sec);
	} else {
		printf("Timeout too early!\n");
		return 1;
	}

	/* Sanity check - we get timeout after ~XPAI_XTE_TIMEOUT_SEC */
	sleep(XPAI_XTE_TIMEOUT_SEC);
	if (timer_gettime(timer_id, &its) == -1) {
		return -1;
	}
	if (its.it_value.tv_sec == 0) {
		printf("We got expected timeout!\n");
	} else {
		return 1;
	}

	return 0;
}

static void print_usage()
{
	printf("Usage: xpai_xte_test -tc <test case number>\n\n"
		   "\twhere <test case number> is one of:\n"
		   "\t1: Set UTC time\n"
		   "\t2: Test timeout not released\n"
		   );
}

int main(int argc, char **argv)
{
	int result = 0;
	int tc = 0;

	if (argc < 3) {
		print_usage();
		exit(-1);
	}

	if (argc >= 3) {
		if (strstr(argv[1], "-tc") != NULL) {
			tc = atoi(argv[2]);
		}
	}

	switch (tc) {
	case 1:
		printf("Set UTC time test...\n");
		result = test_set_utc_time();
		break;
	case 2:
		printf("Test timeout not released...\n");
		result = test_timeout_not_released();
		break;
	default:
		printf("Wrong tc number %d\n", tc);
		print_usage();
		exit(-1);
	}

	if (result) {
		printf("ERROR!\n");
	} else {
		printf("SUCCESS!\n");
	}
	return result;
}
