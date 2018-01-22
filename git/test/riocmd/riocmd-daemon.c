#include "riocmd-internal.h"

static void sighup_handler(int s, siginfo_t *info, void *ucontext)
{
        (void)ucontext;

        /* Ignore recursive self sent signals */
        if (info->si_pid == getpid())
                return;

        if (restart_flag == 0)
                restart_flag = 1;
}
static void sigterm_handler(int s)
{
        kill_flag++;
}
static void sigusr1_handler(int s)
{
        exit(EXIT_SUCCESS);
}
static void sigalrm_handler(int s)
{
        exit(EXIT_FAILURE);
}

static void init_sig_handler(void)
{
        struct sigaction sachild;
        struct sigaction saalrm;
        struct sigaction sausr1;
        struct sigaction sahup;
        struct sigaction saterm;


        sachild.sa_handler = SIG_IGN;
        sigemptyset(&sachild.sa_mask);
        sachild.sa_flags = SA_RESTART | SA_NOCLDSTOP | SA_NOCLDWAIT;
        if (sigaction(SIGCHLD, &sachild, NULL) == -1) {
                syslog(LOG_ERR, "SIGCHLD failed with %d", errno);
                terminate(NULL, EXIT_FAILURE);
        }
        sahup.sa_sigaction = sighup_handler;
        sigfillset(&sahup.sa_mask);
        sahup.sa_flags = SA_SIGINFO;
        if (sigaction(SIGHUP, &sahup, NULL) == -1) {
                syslog(LOG_ERR, "SIGHUP failed with %d", errno);
                terminate(NULL, EXIT_FAILURE);
        }
        saterm.sa_handler = sigterm_handler;
        sigemptyset(&saterm.sa_mask);
        if (sigaction(SIGTERM, &saterm, NULL) == -1) {
                syslog(LOG_ERR, "SIGTERM failed with %d", errno);
                terminate(NULL, EXIT_FAILURE);
        }
        saalrm.sa_handler = sigalrm_handler;
        sigemptyset(&saalrm.sa_mask);
        if (sigaction(SIGALRM, &saalrm, NULL) == -1) {
                syslog(LOG_ERR, "SIGALRM failed with %d", errno);
                terminate(NULL, EXIT_FAILURE);
        }
        sausr1.sa_handler = sigusr1_handler;
        sigemptyset(&sausr1.sa_mask);
        if (sigaction(SIGUSR1, &sausr1, NULL) == -1) {
                syslog(LOG_ERR, "SIGUSR1 failed with %d", errno);
                terminate(NULL, EXIT_FAILURE);
        }
}

static void __null_fds(void)
{
        struct fp {
                FILE *file;
                const char *mode;
        } files[] = {
                { .file = stdin , .mode = "r" },
                { .file = stdout, .mode = "w" },
                { .file = stderr, .mode = "w" }
        };
        __s32 i;

        for(i = 0; i<3; i++) {
                FILE *f = freopen("/dev/null", files[i].mode, files[i].file);
                if(f == NULL) {
                        syslog(LOG_ERR, "(%s) FATAL: freopen: %d\n",
                               __func__, i);
                }
        }
}

void daemonize()
{
        pid_t pid, sid, parent;

        init_sig_handler();

        /* Get pid and fork */
        pid = fork();

        if(pid < 0)
                exit(-1);

        if (pid == 0) {
                parent = getppid();
                umask(0);
                sid = setsid();
                if (sid < 0)
                        exit(-1);

                /* open syslog */
                openlog ("riocmd", LOG_PID | LOG_ODELAY, LOG_DAEMON);

                /* change wdir to root */
                if(chdir("/") < 0)
                        exit(-1);

                __null_fds();
                kill(parent, SIGUSR1);
                return;
        }
        alarm(2);
        pause();
        exit(-1);
}

void block_sighup(struct riocmd_control *c)
{
        sigset_t hup_set;

        sigemptyset(&hup_set);
        sigaddset(&hup_set, SIGHUP);
        if (sigprocmask(SIG_BLOCK, &hup_set, NULL))
                terminate(c, EXIT_FAILURE);
}
void unblock_sighup(struct riocmd_control *c)
{
        sigset_t hup_set;

        sigemptyset(&hup_set);
        sigaddset(&hup_set, SIGHUP);
        if (sigprocmask(SIG_UNBLOCK, &hup_set, NULL))
                terminate(c, EXIT_FAILURE);
}
