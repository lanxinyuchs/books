#include <unistd.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <dirent.h>
#include <assert.h>

#include <pthread.h>
#include <sys/wait.h>
#include "itc.h"
#include "itc_ct.h"

#define UNSET_POLICY       -1
#define UNSET_PRIO         -1
#define UNSET_HIGHPRIO     -1
#define DEFAULT_HIGH_PRIO  40

static int
confSchedulingPolicyEnv(int policy, int priority, int highpriority);

static void
getTwoLatestProcTasks(pid_t process_id,pid_t *latest_pid,pid_t *penultimate_pid);

static bool
checkItcThreadsSchedParams(int policy,
                           int coor_priority,
                           int sysv_priority)
{
   struct sched_param  coor_sch_param;
   struct sched_param  sysv_sch_param;
   pid_t coor_pid;
   pid_t sysv_pid;
   int ret;
   bool result = true;

   ret = itc_init(32, ITC_MALLOC, NULL, ITC_NO_NAMESPACE, 0);
   if ((ret != 0) && (ret != ITC_EALREADY_INITIALISED)) {
      printf("itc_init failed, ret=%d \n", ret);
   }

   getTwoLatestProcTasks(getpid(),&sysv_pid,&coor_pid);

   /* let's give itc threads time to be scheduled */
   usleep(500*1000);

   if (sched_getscheduler(coor_pid) != policy ||
       sched_getscheduler(sysv_pid) != policy)
   {
      result = false;
   }

   (void)sched_getparam(coor_pid,&coor_sch_param);
   (void)sched_getparam(sysv_pid,&sysv_sch_param);

   if (coor_sch_param.sched_priority != coor_priority ||
       sysv_sch_param.sched_priority != sysv_priority)
   {
      result = false;
   }

   itc_exit();

   return result;
}


/* ===================================================================== */
/**
 *   Function to set or unset wanted policy enviromnment variables
 *   in the process.
 *
 *   @param    policy        - scheduling policy to set. If -1 unset
 *                             the variable.
 *             piority       - scheduling piority to set. If -1 unset
 *                             the variable.
 *             highpriority  - highpriority to set. If -1 unset
 *                             the variable.
 *
 *   @return   0     -  If environment is set successfully.
 *             -1    -  Otherwise.
 *
 *
 *   @par Globals: -
 */
/* ===================================================================== */
static int
confSchedulingPolicyEnv(int policy, int priority, int highpriority)
{
   char prio_char[5];
   char *env_policy = NULL;
   char *env_prio = NULL;
   char *env_highprio = NULL;

   if (priority == -1)
   {
      unsetenv("CRL_SCHED_PRIO");
   }
   else
   {
      sprintf(prio_char,"%d",priority);
      setenv("CRL_SCHED_PRIO",prio_char,1);
   }

   if (highpriority == -1)
   {
      unsetenv("CRL_SCHED_HIGH_PRIO");
   }
   else
   {
      sprintf(prio_char,"%d",highpriority);
      setenv("CRL_SCHED_HIGH_PRIO",prio_char,1);
   }

   if (policy == SCHED_FIFO)
   {
      setenv("CRL_SCHED_POLICY","FIFO",1);
   }
   else if (policy == SCHED_OTHER)
   {
      setenv("CRL_SCHED_POLICY","OTHER",1);
   }
   else if (policy == SCHED_RR)
   {
      setenv("CRL_SCHED_POLICY","RR",1);
   }
   else
   {
      unsetenv("CRL_SCHED_POLICY");
   }

   env_policy = getenv("CRL_SCHED_POLICY");
   env_prio = getenv("CRL_SCHED_PRIO");
   env_highprio =  getenv("CRL_SCHED_HIGH_PRIO");

   if (policy != -1 && !env_policy)
   {
      return -1;
   }

   if (highpriority != -1 && !env_highprio)
   {
      return -1;
   }

   if (priority != -1 && !env_prio)
   {
      return -1;
   }

   return 0;
}


/* ===================================================================== */
/**
 *   Function that gets two latest threads created in the process.
 *
 *   @param    process_id       - id of the process.
 *             latest_pid       - Out: pid of the last created thread.
 *             penultimate_pid  - Out: pid of the thread created before
 *                                the last one.
 *
 *   @return     -
 *
 *
 *   @par Globals: -
 */
/* ===================================================================== */
static void
getTwoLatestProcTasks(pid_t process_id,
                      pid_t *latest_pid,
                      pid_t *penultimate_pid)
{

   DIR *dirPath = NULL;
   struct dirent *next_dir;
   char dirname[100];
   snprintf(dirname, sizeof(dirname), "/proc/%d/task", process_id);

   *latest_pid = 0;
   *penultimate_pid = 0;

   dirPath = opendir(dirname);

   if (dirPath)
   {
      while ((next_dir = readdir(dirPath)) != NULL)
      {
         /* find the task (thread) with the biggest id
          and one before that */
         if (*latest_pid < atoi(next_dir->d_name))
         {
            *penultimate_pid = *latest_pid;
            *latest_pid = atoi(next_dir->d_name);
         }
      }

      closedir(dirPath);
   }

   assert(*latest_pid != 0);
}


static int
test_other_policy(void)
{
   int result = 0;

   confSchedulingPolicyEnv(SCHED_OTHER,
                           sched_get_priority_min(SCHED_OTHER),
                           sched_get_priority_max(SCHED_OTHER));

   if (!checkItcThreadsSchedParams(SCHED_OTHER,
                                  sched_get_priority_min(SCHED_OTHER),
                                  sched_get_priority_max(SCHED_OTHER)))
   {
      result = -1;
      goto out;
   }

   confSchedulingPolicyEnv(UNSET_POLICY,
                           UNSET_PRIO,
                           UNSET_HIGHPRIO);

   if (!checkItcThreadsSchedParams(SCHED_OTHER,
                                  sched_get_priority_min(SCHED_OTHER),
                                  sched_get_priority_max(SCHED_OTHER)))
   {
      result = -2;
      goto out;
   }

   confSchedulingPolicyEnv(SCHED_OTHER,
                           UNSET_PRIO,
                           UNSET_HIGHPRIO);

   if (!checkItcThreadsSchedParams(SCHED_OTHER,
                                  sched_get_priority_min(SCHED_OTHER),
                                  sched_get_priority_max(SCHED_OTHER)))
   {
      result = -3;
      goto out;
   }

   confSchedulingPolicyEnv(UNSET_POLICY,
                           sched_get_priority_min(SCHED_OTHER),
                           UNSET_HIGHPRIO);

   if (!checkItcThreadsSchedParams(SCHED_OTHER,
                                  sched_get_priority_min(SCHED_OTHER),
                                  sched_get_priority_max(SCHED_OTHER)))
   {
      result = -4;
      goto out;
   }

   confSchedulingPolicyEnv(UNSET_POLICY,
                           55,
                           UNSET_HIGHPRIO);

   if (!checkItcThreadsSchedParams(SCHED_OTHER,
                                  sched_get_priority_min(SCHED_OTHER),
                                  sched_get_priority_max(SCHED_OTHER)))
   {
      result = -5;
      goto out;
   }


out:
   return result;
}

static int
test_rr_policy(void)
{
   int result = 0;

   confSchedulingPolicyEnv(SCHED_RR,
                          sched_get_priority_min(SCHED_RR),
                          sched_get_priority_max(SCHED_RR));

   if (!checkItcThreadsSchedParams(SCHED_RR,
                                  sched_get_priority_min(SCHED_RR),
                                  sched_get_priority_max(SCHED_RR)))
   {
      result = -1;
      goto out;
   }

   confSchedulingPolicyEnv(SCHED_RR,
                          sched_get_priority_max(SCHED_RR),
                          sched_get_priority_max(SCHED_RR));

   if (!checkItcThreadsSchedParams(SCHED_RR,
                                  sched_get_priority_max(SCHED_RR),
                                  sched_get_priority_max(SCHED_RR)))
   {
      result = -2;
      goto out;
   }

   confSchedulingPolicyEnv(SCHED_RR,22,33);

   if (!checkItcThreadsSchedParams(SCHED_RR,22,33))
   {
      result = -3;
      goto out;
   }

   confSchedulingPolicyEnv(SCHED_RR,
                          UNSET_PRIO,
                          11);

   if (!checkItcThreadsSchedParams(SCHED_RR,
                                   sched_get_priority_min(SCHED_RR),
                                   11))
   {
      result = -4;
      goto out;
   }

   confSchedulingPolicyEnv(SCHED_RR,
                          UNSET_PRIO,
                          UNSET_HIGHPRIO);

   if (!checkItcThreadsSchedParams(SCHED_RR,
                                   sched_get_priority_min(SCHED_RR),
                                   DEFAULT_HIGH_PRIO))
   {
      result = -5;
      goto out;
   }

out:
   return result;
}

static int
test_fifo_policy(void)
{
   int result = 0;

   confSchedulingPolicyEnv(SCHED_FIFO,
                           sched_get_priority_max(SCHED_FIFO),
                           sched_get_priority_max(SCHED_FIFO));

   if (!checkItcThreadsSchedParams(SCHED_FIFO,
                                   sched_get_priority_max(SCHED_FIFO),
                                   sched_get_priority_max(SCHED_FIFO)))
   {
      result = -1;
      goto out;
   }

   confSchedulingPolicyEnv(SCHED_FIFO,
                           sched_get_priority_min(SCHED_FIFO),
                           sched_get_priority_max(SCHED_FIFO));

   if (!checkItcThreadsSchedParams(SCHED_FIFO,
                                   sched_get_priority_min(SCHED_FIFO),
                                   sched_get_priority_max(SCHED_FIFO)))
   {
      result = -2;
      goto out;
   }

   confSchedulingPolicyEnv(SCHED_FIFO,33,44);

   if (!checkItcThreadsSchedParams(SCHED_FIFO,33,44))
   {
      result = -3;
      goto out;
   }

   confSchedulingPolicyEnv(SCHED_FIFO,UNSET_PRIO,22);

   if (!checkItcThreadsSchedParams(SCHED_FIFO,
                                   sched_get_priority_min(SCHED_FIFO),
                                   22))
   {
      result = -4;
      goto out;
   }

   confSchedulingPolicyEnv(SCHED_FIFO,UNSET_PRIO,UNSET_HIGHPRIO);

   if (!checkItcThreadsSchedParams(SCHED_FIFO,
                                   sched_get_priority_min(SCHED_FIFO),
                                   DEFAULT_HIGH_PRIO))
   {
      result = -5;
      goto out;
   }


out:
   return result;
}

int run_sched_params_test(void)
{
   int result = 0;

   if ((result = test_other_policy()) != 0)
   {
      return result - 1000 ;
   }

   if ((result = test_fifo_policy()) != 0)
   {
      return result - 2000 ;
   }

   if ((result = test_rr_policy()) != 0)
   {
      return result - 3000 ;
   }

   return 0;
}
