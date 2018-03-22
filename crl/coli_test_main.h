#ifndef COLI_TEST_MAIN_H
#define COLI_TEST_MAIN_H

#include <ose.h>

#define COLI_ATTACH       (0xEEFF00)

#define COLI_TEST_SIG     (0xEEFF01)
struct coli_test_sig {
      SIGSELECT sig_no;
      uint32_t  type;
};

#define COLI_TEST_SIG_RSP (0xEEFF02)
struct coli_test_sig_rsp {
      SIGSELECT sig_no;
};

#define COLI_KILL           (0)
#define COLI_ADD_CMD_1      (1)
#define COLI_ADD_CMD_2      (2)
#define COLI_REMOVE_CMD_1   (3)
#define COLI_REMOVE_CMD_2   (4)

#define READ  0
#define WRITE 1

PROCESS startCommandServer();
void killCommandServer();

void sendTestSigRsp(PROCESS pid);
void waitForTestSigRsp(void);
int waitForCommand(char *cmd);
int waitForUnregisterCommand(char *cmd);

#endif /* COLI_TEST_MAIN_H */
