/* ---------------------------------------------------------------------------
 *
 * @copyright Ericsson AB 2016 All rights reserved.
 * The information in this document is the property of Ericsson.  Except
 * as specifically authorized in writing by Ericsson, the receiver of
 * this document shall keep the information contained herein confidential
 * and shall protect the same in whole or in part from disclosure and
 * dissemination to third parties.  Disclosure and disseminations to the
 * receivers employees shall only be made on a strict need to know basis.
 *
 * ---------------------------------------------------------------------------
 */

#ifndef TEST_HELPER_FUNCTIONS_H
#define TEST_HELPER_FUNCTIONS_H

#include "stdio.h"
#include "pthread.h"
#include "stdlib.h"
#include "string.h"
#include "itc.h"
#include "test_sigs/lhsh_shelld_test.sig"
#include "unistd.h"
#include "client/client.h"

typedef enum {PASSED, FAILED, ABORTED} tc_result;

#define NUM_OF_CLIENTS 3
#define NUM_OF_CLIENT_MODES 3
#define ITC_TEST_TMO 5000

static int aborted, passed, failed;

/*************************************Helper methods***************************************/


/**
 * @brief
 *
 * Translates client mode id to string
 *
 * @param clientmode  Mode of working
 *
 * @return mode name string
 *
 */
static const char *translatemode(int clientmode)
{
        switch (clientmode) {
        case 0:
                return "ITC";
        case 1:
                return "RUMA";
        case 2:
                return "RCMD";
        default:
                /*client will run in default itc mode!*/
                return "ITC";

        }
}


/**
 * @brief
 *
 * Hunt client mailbox
 *
 * @param client_id  Client id
 *
 * @return mailbox id
 *
 */
itc_mbox_id_t hunt_client(int client_id)
{
        char client_box_name[32];
        sprintf(client_box_name, "dummy_mbox_%d", client_id);
        return itc_locate(client_box_name);
}


/**
 * @brief
 *
 * Checks received signal
 *
 * @param sig  Received signal
 * @param expected Expected sigNo
 *
 * @return comparison result
 *
 */
static inline int check_recv_signal(union itc_msg *sig,
                                    uint32_t expect)
{
        if (sig == NULL) {
                printf("Timeout occured. Test app didn't receive wanted signal: %d\n", expect);
                return 0;
        }

        if (sig->sigNo == expect)
                return 1;

        printf("Test app received unexpected signal 0x%x\n", sig->sigNo);
        return 0;
}


/**
 * @brief
 *
 * Checks received signal data
 *
 * @param sent_bytes Number of bytes sent
 * @param received_bytes Number of bytes received
 * @param buffer Received buffer
 * @param requested_buffer Requested buffer
 *
 * @return comaprison result
 *
 */
static inline int comp_rec_data(int sent_bytes, int received_bytes,
                                char *buffer, char *requsted_buffer)
{
        if (sent_bytes != received_bytes) {
                printf("Received incorrect number of bytes. Expected %d, received %d\n",
                       sent_bytes, received_bytes);
                return -1;
        }
        /* we must tolerate weird characters at the end so we are
           comparing only printable chars*/
        int ret = strncmp(buffer, requsted_buffer, strlen(requsted_buffer));

        if (ret != 0) {
                printf("Expected and provided output don't match.\n");
                return -1;
        }
        printf("Received data is correct.\n");
        return 0;

}


/**
 * @brief
 *
 * Starts client process
 *
 * @param clientmode  Mode of working for client process
 * @param client_id Id for client process
 *
 * @return client status
 *
 */
int start_client(int clientmode, int client_id)
{
        char command[RCMD_REQ_BUFFER_SIZE];
        int status;
        printf("Starting client %d in %s mode\n", client_id,
               translatemode(clientmode));
        sprintf(command, "./lhsh_shelld_client -c %d -n %d &", clientmode, client_id);
        status = system(command);
        if (status != 0) {
                printf("Unable to start client. This will mark TC execution as failed. \n");
                return status;
        }

        /* We must wait for the client to wake up */
        sleep(5);

        return status;
}


/**
 * @brief
 *
 * Stops client process
 *
 * @param myBox Test app mailbox
 * @param client_id Client id
 *
 * @return mailbox id
 *
 */
int stop_client(itc_mbox_id_t myBox, int client_id)
{
        itc_mbox_id_t client_box;
        char client_box_name[32];
        sprintf(client_box_name, "dummy_mbox_%d", client_id);
        client_box = itc_locate(client_box_name);
        if (client_box == ITC_NO_ID) {
                printf("Unable to find client. Client seems to be already dead. Aborting.\n");
                return -1;
        }
        union itc_msg *stop_client_request;
        stop_client_request = itc_alloc(sizeof(struct TestControlClientStop),
                                        STOP_TEST_CLIENT);
        printf("Sending stop signal to client.\n");
        itc_send(&stop_client_request, client_box, myBox);
        return 0;

}

/**
 * @brief
 *
 * Search for active sleep processes, if everything
 * went fine we expect only greep sleep to be active.
 *
 * @return number of matches for sleep
 *
 */
int check_if_child_active()
{
        FILE *fp;
        int size = 0;
        char line[4]; /* we want to check how many sleep instances are active */
        fp = popen("ps -ef | grep \"sleep 2000\" | wc -l", "r");

        if (fp == NULL) {
                printf("Handle error.\n");
                return -1;
        }

        if (fgets(line, 4, fp) != NULL) {
                sscanf(line, "%d", &size);
        }

        if (pclose(fp) == -1) {
                printf("Error reported by pclose.\n");
                return -1;
        }
        return size;
}

/**
 * @brief
 *
 * Check if we produced some zobies by killing
 * shelld's kids
 *
 * @return status (0 - OK; 1 - NOK)
 *
 */
int check_if_defunct()
{
        FILE *fp;
        char line[80]; /* we want to check if there are remaing zombies */
        int size = 0; /* we should have 2 hits only! */

        fp = popen("ps -ef | grep defunct", "r");


        if (fp == NULL) {
                printf("Handle error.\n");
                return 1;

        }
        while (fgets(line, 80, fp) != NULL) {
                size++;
        }

        if (pclose(fp) == -1) {
                printf("Error reported by pclose.\n");
                return 1;
        }

        if (size != 2) {
                printf("Remote process did not terminate correctly.\n");
                return 1;
        }

        return 0;
}


/**
 * @brief
 *
 * Clean hanging clients
 *
 * @param myBox Test app mailbox
 *
 */
void clean_hanging_clients(itc_mbox_id_t myBox)
{
        itc_mbox_id_t client_box;
        int h_clients = 0;

        printf("Cleaning called.\n");

        for (int i = 0; i < NUM_OF_CLIENTS; i++) {
                client_box = hunt_client(i);

                if (client_box != ITC_NO_ID) {
                        printf("Cleaning client id %d\n", i);
                        stop_client(myBox, i);
                        h_clients++;
                }

        }

        if (h_clients > 0)
                printf("Cleaned %d hanging clients\n", h_clients);
        else
                printf("No hanging clients to clean...\n");
}


/**
 * @brief
 *
 * Sends remote command to shelld
 *
 * @param remote_cmd Remote command that will be executed by remote shell
 * @param client_id Client id
 * @param myBox Test app mailbox
 *
 * @return mailbox id
 *
 */
int send_command(char *remote_cmd, int client_id, itc_mbox_id_t myBox)
{

        union itc_msg *shelld_simple_command_request;
        itc_mbox_id_t client_box;

        /* Sending remote command to shelld via client */
        shelld_simple_command_request = itc_alloc(sizeof(struct SimpleRemoteCmd) +
                                        RCMD_REQ_BUFFER_SIZE, SHELLD_SIMPLE_COMMAND_REQUEST);

        if (shelld_simple_command_request == NULL) {
                printf("Unable to allocate cmd.");
                return -1;
        }

        sprintf(shelld_simple_command_request->simple_remote_command.cmd_string,
                remote_cmd);

        /* Hunt for client mailbox */

        client_box = hunt_client(client_id);

        if (client_box == ITC_NO_ID) {
                printf("Unable to find client. Client seems to be dead. Aborting.\n");
                itc_free(&shelld_simple_command_request);
                return -1;
        }

        printf("Client box found, id: %d\n", client_box);
        itc_send(&shelld_simple_command_request, client_box, myBox);

        return 0;

}

#endif /* TEST_HELPER_FUNCTIONS_H */
