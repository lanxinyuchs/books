/*
 * Copyright (C) 2012 - Christian Babeux <christian.babeux@efficios.com>
 * Copyright (C) 2014 - Mathieu Desnoyers <mathieu.desnoyers@efficios.com>
 * Copyright (C) 2016 - Ericsson AB
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License, version 2 only, as
 * published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
 * more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <pthread.h>
#include <signal.h>
#include <time.h>
#include <unistd.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <stdarg.h>

#include <lttng/health.h>
#include <lttng/lttng.h> /* lttng_strerror() */


/* Default setting of global parameters */
#define DEFAULT_TIMEOUT_IN_MIN (1)
#define DEFAULT_OUTPUT_PATH    "/tmp"
#define LOG_FILE_MAX_SIZE      (1000000)

/* File pointer to log file of health check information */
static FILE *fp;
/* Full path to the LTTng health check log */
static char full_path[128] = {0};
/* Flag indication if test shall be daemonized */
static int daemonized = 0;
/* Timeout between health check tests */
static int time_out;
/* Path to directory to store log file */
static char output_path[128];
/* Path to LTTng health check socket */
static char relayd_path[128] = {0};
/* Flag indicates if the health check should be run only once. */
static int singleshot = 0;

static void start_timer(void);

/* ===================================================================== */
/**
 *  This function checks the size of the health check log file and write
 *  the requested string to it. If size exceed the limit, the file is
 *  truncated.
 *
 *   @param      lh                LTTng health check structure
 *
 *   @return             -
 *
 *   @par Globals:
 *                       full_path, fp
 */
/* ===================================================================== */
static
void write_to_file(char *format, ...)
{
    struct stat buf;
    int ret;
    va_list args;

    ret = stat(full_path, &buf);
    if (ret < 0) {
       abort();
    }

    if (buf.st_size > LOG_FILE_MAX_SIZE) {
       fp = fopen(full_path, "w");
       if (fp == NULL) {
          abort();
       }
    }

    va_start(args, format);
    vfprintf(fp, format, args);
    va_end(args);

}

/* ===================================================================== */
/**
 *  This function requests LTTng for the health status of a component and
 *  evalutaes the result of the query.
 *
 *   @param      lh                LTTng health check structure
 *   @param      componant_name    Name of the component to check
 *   @param      ok_if_not_running 1 if ok that query is not succefully
 *                                 performed, otherwise 0.
 *
 *   @return             0 at ok health check status
 *                       -1 at failing health check query
 *                       otherwise LTTng error code.
 *
 *   @par Globals:
 *                       fp
 */
/* ===================================================================== */
static
int check_component(struct lttng_health *lh, const char *component_name,
                int ok_if_not_running)
{
        const struct lttng_health_thread *thread;
        int nr_threads, i, status;

        if ((status = lttng_health_query(lh))) {
                if (ok_if_not_running) {
                        return 0;
                }
                write_to_file("Error querying %s health\n", component_name);
                return -1;
        }

        status = lttng_health_state(lh);
        if (!status) {
                return status;
        }
        else if (status < 0) {
           write_to_file("Health state for %s: %s (%d)\n", component_name,
                         lttng_strerror(status), status);
           return status;
        }
        /*
         * Health check indicate an component error, continue
         * to get more information
         */
        nr_threads = lttng_health_get_nr_threads(lh);
        if (nr_threads < 0) {
                write_to_file("Error getting number of threads for %s\n",
                              component_name);
                return -1;
        }

        write_to_file("Component \"%s\" is in error.\n", component_name);

        for (i = 0; i < nr_threads; i++) {
                int thread_state;

                thread = lttng_health_get_thread(lh, i);
                if (!thread) {
                        write_to_file("Error getting thread no. %d\n", i);
                        return -1;
                }
                thread_state = lttng_health_thread_state(thread);
                if (thread_state < 0) {
                        write_to_file("Thread %s (%d) reports error %s (%d)\n",
                                      lttng_health_thread_name(thread), i,
                                      lttng_strerror(thread_state),
                                      thread_state);
                        continue;
                }
                write_to_file
                   ("Thread \"%s\" (%d) is not responding in component \"%s\".\n",
                    lttng_health_thread_name(thread), i, component_name);
        }
        return status;
}

/* ===================================================================== */
/**
 *  This function request LTTng for health check of session daemon.
 *
 *   @param              -
 *
 *   @return             0 at ok health check status
 *                       -1 at failing health check query
 *                       otherwise LTTng error code.
 *
 *   @par Globals:
 *                       -
 */
/* ===================================================================== */
static
int check_sessiond(void)
{
        struct lttng_health *lh;
        int status;

        lh = lttng_health_create_sessiond();
        if (!lh) {
                write_to_file("lttng_health_create_sessiond failed");
                return -1;
        }

        status = check_component(lh, "sessiond", 0);

        lttng_health_destroy(lh);

        return status;
}

/* ===================================================================== */
/**
 *  This function request LTTng for health check of consumer daemon/daemons.
 *
 *   @param              hc   Consumer daemon to check
 *
 *   @return             0 at ok health check status
 *                       -1 at failing health check query
 *                       otherwise LTTng error code.
 *
 *   @par Globals:
 *                       -
 */
/* ===================================================================== */
static
int check_consumerd(enum lttng_health_consumerd hc)
{
        struct lttng_health *lh;
        int status;
        static const char *cnames[NR_LTTNG_HEALTH_CONSUMERD] = {
                "ust-consumerd-32",
                "ust-consumerd-64",
                "kernel-consumerd",
        };

        lh = lttng_health_create_consumerd(hc);
        if (!lh) {
                write_to_file("lttng_health_create_consumerd failed");
                return -1;
        }

        status = check_component(lh, cnames[hc], 1);

        lttng_health_destroy(lh);

        return status;
}

/* ===================================================================== */
/**
 *  This function request LTTng for health check of consumer daemon/daemons.
 *
 *   @param              path  Path to the relayd health socket.
 *
 *   @return             0 at ok health check status
 *                       -1 at failing health check query
 *                       otherwise LTTng error code.
 *
 *   @par Globals:
 *                       -
 */
/* ===================================================================== */
static
int check_relayd(const char *path)
{
        struct lttng_health *lh;
        int status;

        lh = lttng_health_create_relayd(path);
        if (!lh) {
                write_to_file("lttng_health_create_relayd failed");
                return -1;
        }

        status = check_component(lh, "relayd", 1);

        lttng_health_destroy(lh);

        return status;
}

/* ===================================================================== */
/**
 *  This function is called when timeout ellapses and performs a health
 *  check of all available LTTng components.
 *
 *   @param              -
 *
 *   @return             -
 *
 *   @par Globals:
 *                       fd
 */
/* ===================================================================== */
static
void check_all_components(union sigval val)
{
        int i, status = 0;

        status |= check_sessiond();

        for (i = 0; i < NR_LTTNG_HEALTH_CONSUMERD; i++) {
                status |= check_consumerd(i);
                fflush(fp);
        }

        if (relayd_path[0] != '\0') {
           status |= check_relayd(&(relayd_path[0]));
           fflush(fp);
        }

        if (!status) {
           write_to_file("Health check ok\n");
           fflush(fp);
        }

        /* Start/restart the timer, only when daemon */
        if (daemonized && !singleshot) {
           start_timer();
        }

        return;
}

/* ===================================================================== */
/**
 *  This function starts the single shot timer for the specified timeout
 *  value and at timeout calls the notify function.
 *
 *   @param              -
 *
 *   @return             -
 *
 *   @par Globals:
 *                       time_out, fd
 */
/* ===================================================================== */
static void
start_timer(void)
{
        struct sigevent sig;
        timer_t timerid;
        struct itimerspec in;
        pthread_attr_t attr;
        struct sched_param parm;

        pthread_attr_init( &attr );
        parm.sched_priority = 255;
        pthread_attr_setschedparam(&attr, &parm);

        sig.sigev_notify = SIGEV_THREAD;
        sig.sigev_signo = 0;
        sig.sigev_notify_function = check_all_components;
        sig.sigev_notify_attributes = &attr;

        if (!timer_create(CLOCK_REALTIME, &sig, &timerid)) {

                in.it_value.tv_sec = 60 * time_out;
                in.it_value.tv_nsec = 0;
                in.it_interval.tv_sec = 0;
                in.it_interval.tv_nsec = 0;
                if (timer_settime(timerid, 0, &in, NULL) == -1) {
                   write_to_file("Setting up of timer failed\n");
                }
        }
        else {
           write_to_file("Setting up of timer failed\n");
        }
        fflush(fp);
}

/* ===================================================================== */
/**
 *  This function creates a daemon process and kills the parent process.
 *
 *   @param              -
 *
 *   @return             0 at successfull parsing, otherwise 1
 *
 *   @par Globals:
 *                       -
 */
/* ===================================================================== */
static
int daemonize_me(void)
{
       pid_t process_id, sid;
       int ret;

       /* create child process */
       process_id = fork();

       /* indication of fork() failure */
       if (process_id < 0) {
          fprintf(stderr, "fork failed, errno=%d\n", errno);
          return 1;
       }

       /* parent process, kill it */
       if (process_id > 0) {
          fprintf(stderr, "process_id of child process %d\n", process_id);
          /* Return success in exit status */
          exit(0);
       }

       /* unmask the file mode. */
       umask(0);

       /* set new session. */
       sid = setsid();
       if(sid < 0) {
          fprintf(stderr, "setsid failed, errno=%d\n", errno);
          return 1;
       }

       /* Change the current working directory to root */
       ret = chdir("/");
       if(ret < 0) {
          fprintf(stderr, "chdir failed, errno=%d\n", errno);
          return 1;
       }

       /* close stdin. stdout and stderr */
       close(STDIN_FILENO);
       close(STDOUT_FILENO);
       close(STDERR_FILENO);

       return 0;
}

/* ===================================================================== */
/**
 *  Open health check log file.
 *
 *   @param              -
 *
 *   @return             -
 *
 *   @par Globals:
 *                       fp, full_path, output_path
 */
/* ===================================================================== */
static void open_health_log(void)
{
   snprintf(full_path, sizeof(full_path), "%s/%s",
            &(output_path[0]), "lttng-health.log");
   fp = fopen(full_path, "w");
   if (fp == NULL) {
      fprintf(stderr, "fopen failed, errno=%d\n", errno);
      abort();
   }
}

/* ===================================================================== */
/**
 *  Close health check log file.
 *
 *   @param              -
 *
 *   @return             -
 *
 *   @par Globals:
 *                       fp
 */
/* ===================================================================== */
static void close_health_log(void)
{
   fclose(fp);
}

/* ===================================================================== */
/**
 *  This function printf the syntax for lttng_healthchk application.
 *
 *   @param              -
 *
 *   @return             -
 *
 *   @par Globals:
 *                       -
 */
/* ===================================================================== */
static
void usage(void)
{
        fprintf(
                stderr,
                "LTTng health test\n"
                "\n"
                "Usage: lttng_healthchk <options> ...\n"
                "\t -d\n"
                "\t\t daemonize the LTTng health check\n"
                "\t -t\n"
                "\t\t time in minutes between each health check\n"
                "\t\t default: 60 minutes\n"
                "\t -o\n"
                "\t\t output path for health check information\n"
                "\t\t default: /tmp\n"
                "\t -r\n"
                "\t\t output path for LTTng relayd health check sockets\n"
                "\t\t default: if not set, no health check is done for relayd\n"
                "\t -s\n"
                "\t\t run the health check once and exit\n"
                "\t\t singleshot cannot be combined with daemonize\n"
                "\n");
}

/* ===================================================================== */
/**
 *  This function parses the std input and sets the global varibales
 *  accordingly.
 *
 *   @param              argc  Number of arguments
 *   @param              argv  Array of arguments strings
 *
 *   @return             0 at successfull parsing, otherwise 1
 *
 *   @par Globals:
 *                       time_out, relayd_path, output_path
 */
/* ===================================================================== */
int parse_args(int argc, char *argv[])
{
        int opt;
        int optind = 0;
        char *endptr;

        time_out = DEFAULT_TIMEOUT_IN_MIN;
        sprintf(&(output_path[0]), DEFAULT_OUTPUT_PATH);

        while ((opt = getopt(argc, argv, "dt:o:r:s")) != -1) {

           switch (opt) {
               case 'd':
                  if (singleshot) {
                     fprintf(stderr, "Error: daemonize cannot be combined with singleshot\n");
                     return 1;
                  }
                  if (daemonize_me()) {
                     abort();
                  }
                  daemonized = 1;
                  break;
               case 't':
                  time_out = strtol(optarg, &endptr, 10);
                  if (endptr != NULL && *endptr != '\0') {
                     fprintf(stderr, "Error: Timeout value must be a decimal number\n");
                     return 1;
                  }
                  break;
               case 'o':
                  sprintf(output_path, optarg);
                  break;
               case 'r':
                  sprintf(&(relayd_path[0]), optarg);
                  break;
              case 's':
                  if (daemonized) {
                     fprintf(stderr, "Error: singleshot cannot be combined with daemonize\n");
                     return 1;
                  }
                  singleshot = 1;
                  break;
               default: /* '?' */
                   return 1;
               }
        }

        if (optind > argc) {
           fprintf(stderr, "Error: Expected argument after options\n");
           return 1;
        }

        open_health_log();
        check_all_components((union sigval)0);

        return 0;
}

/* ===================================================================== */
/**
 *  This is the main function for the LTTng health test daemon.
 *
 *   @param              -
 *
 *   @return             -
 *
 *   @par Globals:
 *                       full_path, fd, daemonized
 */
/* ===================================================================== */
int main(int argc, char *argv[])
{
        /* Parse arguments */
        if (parse_args(argc, argv) != 0) {
           usage();
           return 0;
        }

        while (daemonized) {
           /*
            * Dont block context switches, let the process sleep
            * for some time
            */
           sleep(1);
        }

        close_health_log();

        return (0);
}
