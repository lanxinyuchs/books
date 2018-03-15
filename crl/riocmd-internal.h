#ifndef __RIOCMD_INTERNAL_H_
#define __RIOCMD_INTERNAL_H_

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <unistd.h>
#include <string.h>
#include <syslog.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/socket.h>
#include <sys/epoll.h>
#include <sys/ioctl.h>
#include <sys/un.h>
#include <signal.h>
#include <poll.h>
#include <linux/types.h>
#include <linux/rbs/af_rbs.h>
#include <linux/rbs/rbs-sys.h>

#include <itc.h>

#include <ulh_dl_list.h>
#include <ulh_ref.h>
#include <ulh_cm.h>
#include <ulh_rio.h>
#include <ulh_lnh_msg.h>

#include <riocmd-api.h>

#define RIOCMD_LFD_MAGIC          (void *)(0x19641005)
#define RIOCMD_IFD_MAGIC          (void *)(0x19920916)
#define MAX_CLIENTS               6
#define RIO_EPOLL_MAX             (MAX_CLIENTS + 2) /* 6 * EMCA + 1 itc mbox + listen_fd */

enum riocmd_bt {
	DUS = 0,
	DUW = 1
};

#define NOT_CLEAN                 0
#define CLEANUP_STARTED           1
#define CLEANUP_DONE              2


struct riocmd_control {
        struct ulh_ref            ref;
        struct dl_list            epoll_list;
        struct dl_list            req_list;
	struct dl_list            client_list;
        __u32                     req_seq;
        int                       clients;
        itc_mbox_id_t             mbox;
        itc_mbox_id_t             lnh_mbox;
	itc_monitor_id_t          lnh_mon_id;
        int                       mbox_fd;
        int                       listen_fd;
        int                       epoll_fd;
        char                      src_mac[6];  /* used only on DUW HW */
        enum riocmd_bt            board_type;
};

struct riocmd_client {
        struct dl_list            node;
        struct ulh_ref            ref;
        struct riocmd_control    *control;
        struct dl_list            lnh_obj_list;
	union riocmd_msg          msg;
        int                       msg_sz;
        int                       state;
        char                      name[16];
        int                       fd;
};

struct riocmd_preq {
        struct dl_list            node;
        struct ulh_ref            ref;
        struct riocmd_client     *client;
	__u32                     pending;
        __u32                     lnh_seq;
        __u32                     api_seq;
        __u32                     api_id;
	__u32                     snid;
	__u32                     cmid;
	__u32                     lid;
	__u32                     ret;
	char                      link_name[ULH_LNHNAMESIZ];
};


/* riocmd-res.c */
extern struct riocmd_control *alloc_control(void);
extern struct riocmd_client *alloc_client(struct riocmd_control *c);
extern struct riocmd_preq *alloc_preq(struct riocmd_client *client, __u32 snid);
extern struct riocmd_lnh_obj *alloc_lnh_obj(struct riocmd_client *client);

/* riocmd-daemon.c */

extern void block_sighup(struct riocmd_control *c);
extern void unblock_sighup(struct riocmd_control *c);
extern void daemonize(void);

/* riocmd-client-api.c */

extern int handle_client_fd(__u32 events, void *data);
extern void client_rsp(struct riocmd_preq *preq);
extern void client_event(struct riocmd_preq *preq, __u32 state);
extern void client_cleanup(struct riocmd_client *client);

/* riocmd-lnh-api.c */

extern int handle_itc_fd(__u32 events, void *data);
extern void create_cm_req(struct riocmd_preq *preq, struct ulh_cm_rio_config *cfg);
extern void destroy_link_req(struct riocmd_preq *preq);

/* riocmd.c */

extern int restart_flag;
extern int kill_flag;
extern int state_clean;

extern struct riocmd_preq *locate_link_obj(struct riocmd_control *c, __u32 lid);
extern void cb_destroy(struct riocmd_control *c, void *magic, void *data);
extern void terminate(struct riocmd_control *c, int return_code);

/* riocmd-cfg.c */

#define LINE_SIZE                 80
#define MAX_NAME_LEN              LINE_SIZE

struct cfg_data
{
        char   cfg_file_name[MAX_NAME_LEN];
        char   lnh_mbox_name[MAX_NAME_LEN];
	__u32  board_type;
	__u16  master_destid;
};

void read_config_file(char *file_name, struct cfg_data *cfg);

#endif
