#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <unistd.h>

#include "itc.h"

#ifndef __ITC_CT_H
#define __ITC_CT_H

struct thread_data {
        char name[32];
        bool have_mutex;
        pthread_mutex_t lock;
};

typedef enum {
        TC_MB_DELETE = 0,
        TC_THREAD_RETURNS,
        TC_THREAD_CANCEL,
        TC_KILL_PROC,
        TC_LEAVE_PROC,
} locmon_tcs;

#define MAX_NAME_LEN 32

#define STOP_THREAD 0xA000
#define KILL_THREAD 0xA001
#define PING_THREAD 0xA002

#define LOC_BASE    0xB000
#define MON_BASE    0xC000

#define CREATE_LOOP      0xD000
struct create_loop {
        uint32_t      msgno;
        bool          send_repl;
        char          name[1];
};

#define CREATE_LOOP_REPL 0xD001
struct create_loop_repl {
        uint32_t      msgno;
        bool          success;
        pthread_t     tid;
        itc_mbox_id_t mbox_id;
};

#define STOP_LOOP        0xD002
struct stop_loop {
        uint32_t      msgno;
        bool          send_repl;
        itc_mbox_id_t mbox_id;
};

#define STOP_LOOP_REPL   0xD003
struct stop_loop_repl {
        uint32_t      msgno;
        bool          success;
};

#define KILL_LOOP        0xD004
struct kill_loop {
        uint32_t      msgno;
        bool          send_repl;
        itc_mbox_id_t mbox_id;
};

#define KILL_LOOP_REPL   0xD005
struct kill_loop_repl {
        uint32_t      msgno;
        bool          success;
};

#define CANCEL_LOOP        0xD006
struct cancel_loop {
        uint32_t                msgno;
        bool send_repl;
        pthread_t tid;
};

#define CANCEL_LOOP_REPL   0xD007
struct cancel_loop_repl {
        uint32_t                msgno;
        bool success;
};

#define KILL_PROC         0xD008
#define LEAVE_PROC        0xD009


#define CREATE_CLONE      0xD00A
struct create_clone {
        uint32_t      msgno;
        bool          send_repl;
        char          name[1];
};

#define CREATE_CLONE_REPL 0xD00B
struct create_clone_repl {
        uint32_t      msgno;
        bool          success;
        itc_mbox_id_t clone_id;
};

#define DELETE_CLONE      0xD00C
struct delete_clone {
        uint32_t      msgno;
        bool          send_repl;
        itc_mbox_id_t clone_id;
};

#define DELETE_CLONE_REPL 0xD00D
struct delete_clone_repl {
        uint32_t      msgno;
        bool          success;
};

#define ADD_NAME          0xD00E
struct add_name {
        uint32_t      msgno;
        bool          send_repl;
        char          name[1];
};

#define ADD_NAME_REPL     0xD00F
struct add_name_repl {
        uint32_t      msgno;
        bool          success;
};

#define CREATESHARED_MBOX 0xD010
struct createshared_mbox {
        uint32_t      msgno;
        char          name[1];
};

#define DELETESHARED_MBOX 0xD011
struct deleteshared_mbox {
        uint32_t      msgno;
        itc_mbox_id_t mbox_id;
};

#define RXSHARED          0xD012
struct rxshared {
        uint32_t      msgno;
        itc_mbox_id_t mbox_id;
        int           nr;
};

#define RXSHARED_REPL     0xD013
struct rxshared_repl {
        uint32_t      msgno;
        bool          success;
};

#define GENMSG            0xD014
struct genmsg {
        uint32_t      msgno;
        uint32_t      genmsgno;
        int           nr;
        int           dly;
};

#define REM_LOCATE        0xD014
struct rem_locate {
        uint32_t      msgno;
        itc_mbox_id_t loc_mbox;
        itc_mbox_id_t rx_mbox;
        itc_mbox_id_t from_mbox;
};

#define MBOX_TEST_MSG 0xE000

#define ASS_LNH   0xF000
#define DEASS_LNH 0xF001
struct ass_lnh {
        uint32_t      msgno;
        char          lnhname[1];
};

#define ADD_LNH_MBOX 0xF002
#define REM_LNH_MBOX 0xF003
struct lnh_addrem_mbox {
        uint32_t      msgno;
        char          name[1];
};

#define ASS_LNH_REPL      0xF004
#define DEASS_LNH_REPL    0xF005
#define ADD_LNH_MBOX_REPL 0xF006
#define REM_LNH_MBOX_REPL 0xF007
struct lnh_repl {
        uint32_t      msgno;
        int           result;
};

#define LNH_TESTMSG 0xF008
struct lnh_testmsg {
        uint32_t      msgno;
        int           seq_no;
};

int run_pooltest(void);
int run_mbox_tests(void);
int loc_mon_test(void);
int run_lnhtest(void);
int run_sched_params_test(void);

#endif
