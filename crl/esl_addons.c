#include <assert.h>
#include <unistd.h>
#include <semaphore.h>
#include <sys/time.h>
#include <osetypes.h>
#include <ose4/tosv.h>

#define LOG(a,b)

/* A really poor mans hunt with tmo ... */
PROCESS
ose_hunt_w_tmo(const char *hunt_path, OSUSER user, OSTIME tmo)
{
   PROCESS pid = 0;

   /* ESL does not support user != 0 */
   assert(user == 0);
   if(0 == hunt(hunt_path, user, &pid, NULL))
   {
      delay(tmo);
      (void)hunt(hunt_path, user, &pid, NULL);
   }

   return pid;
}

PROCESS huntWithTimeout(OSTIME tmo, const char *hunt_path)
{
   return ose_hunt_w_tmo(hunt_path, 0, tmo);
}

PROCESS osaHuntWithTimeout(OSTIME tmo, const char *hunt_path)
{
   return ose_hunt_w_tmo(hunt_path, 0, tmo);
}


/*******************************************************************************
 *
 *  Name  : Cello_getOwnInstanceId
 *
 *  Descr.: Returns the instance identity for the program instance that the
 *          calling process belongs to.
 *
 *  Args  : -
 *
 *  Return: The instance identity or zero if the calling process does not belong
 *          to an instantiated program.
 *
 ******************************************************************************/
U32
Cello_getOwnInstanceId(void)
{
   return 0;
}
/*
 * Fixme
 * ESL unimplemented funtions, put here for the moment.
 */
#warning "zzcreate_sem untested"
SEMAPHORE* zzcreate_sem(OSSEMVAL initial_val) {
    sem_t* _sem;
    int rv;

    _sem = malloc(sizeof(sem_t));
    assert(_sem != 0);

    rv = sem_init(_sem, 0, initial_val);
    assert(rv == 0);
    return (SEMAPHORE*)_sem;
}

#warning "zzsignal_sem untested"
void
zzsignal_sem(SEMAPHORE *sem)
{
    int rv;

    rv = sem_post((sem_t*)sem);
    assert(rv == 0);
}

#warning "zzwait_sem untested"
void
zzwait_sem(SEMAPHORE *sem)
{
    int rv;

    rv = sem_wait((sem_t*)sem);
    assert(rv == 0);
}

void
zzkill_sem(SEMAPHORE *sem)
{
// _sem free
}

#warning "zzget_pid_list untested"
struct OS_pid_list*
zzget_pid_list(PROCESS bid)
{
   LOG(LOG_ERR, STR("%s not implemented!!\n", __FUNCTION__));
   return (struct OS_pid_list*)0;
}

PROCESS
zzget_segid(PROCESS pid)
{
   LOG(LOG_ERR, STR("%s not implemented!!\n", __FUNCTION__));
   return 0;
}

#define ESL_SYSTEM_TICK (1000) /* 1 ms ok ? */
OSTICK
zzget_ticks(void)
{
   struct timeval tv;
   int ret;

   ret = gettimeofday(&tv, NULL);
   assert(0 == ret);
   return (tv.tv_sec * 1000 * 1000 + tv.tv_usec)/ESL_SYSTEM_TICK;
}

OSTIME
zzsystem_tick(void)
{
   return ESL_SYSTEM_TICK;
}

#warning "requestTmo untested"
void requestTmo(CANCEL_INFO* cancelInfo, OSTIME timeOut, union SIGNAL **tmoSig)
{
    cancelInfo->pointer = (void*)zzrequest_tmo_sig(timeOut, tmoSig);
}

#warning "cancelTmo untested"
union SIGNAL *cancelTmo(CANCEL_INFO *cancelInfo)
{
    return zzcancel_tmo_sig((OSTMOREF*)&cancelInfo->pointer);
}

#warning "fRequestTmo untested"
void fRequestTmo(CANCEL_INFO *cancelInfo, OSTIME timeOut, PROCESS pid, SIGSELECT tmoSigNo)
{
    assert(pid == current_process());
    cancelInfo->pointer = (void*)zzrequest_tmo(timeOut, tmoSigNo);
}

#warning "fCancelTmo untested"
void fCancelTmo(CANCEL_INFO *cancelInfo)
{
    if (!cancelInfo || !cancelInfo->pointer)
       return;
    zzcancel_tmo((OSTMOREF*)&cancelInfo->pointer);
}

#warning "resetTmo untested"
void resetTmo(CANCEL_INFO *cancelInfo, OSTIME timeOut)
{
  OSTMOREF tr = cancelInfo->instance;
  restart_tmo(&tr, timeOut);
  cancelInfo->instance = tr;
}

void heap_init_flib(void) {
    /* Unnecessary on Linux */
}

void* heap_alloc_unsafe(size_t size) {
    return malloc(size);
}

void heap_free_shared(void* ptr) {
    free(ptr);
}



#warning "zzget_systime untested"
OSTICK zzget_systime(OSTICK* microsecs) {
   struct timeval tv;
   int ret;
   ret = gettimeofday(&tv, NULL);
   assert(0 == ret);
   *microsecs = (tv.tv_sec * 1000 * 1000 + tv.tv_usec) % ESL_SYSTEM_TICK;
   return (tv.tv_sec * 1000 * 1000 + tv.tv_usec)/ESL_SYSTEM_TICK;
}

OSBOOLEAN zzhunt_from(char *name,
		      OSUSER user,
		      PROCESS *name_,
		      union SIGNAL **hunt_sig,
		      PROCESS from)
{
    #warning "zzhunt_from needs to be implemented"
    assert("zzhunt_from needs to be implemented");
    return 1;
}

void zzresume(PROCESS id)
{
   //TODO
}

void zzrestore(union SIGNAL *sig)
{
   //TODO
}
