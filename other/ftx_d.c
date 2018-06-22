/* ---------------------------------------------------------------------------
 *
 * © Ericsson AB 2017 All rights reserved.
 * The information in this document is the property of Ericsson.  Except
 * as specifically authorized in writing by Ericsson, the receiver of
 * this document shall keep the information contained herein confidential
 * and shall protect the same in whole or in part from disclosure and
 * dissemination to third parties.  Disclosure and disseminations to the
 * receivers employees shall only be made on a strict need to know basis.
 *
 * ---------------------------------------------------------------------------
 */
#include <stdio.h>
#include <errno.h>
#include <stdlib.h>
#include <fcntl.h>
#include <unistd.h>
#include <string.h>
#include <signal.h>
#include <arpa/inet.h>
#include <pthread.h>
#include <itc.h>
#include <itc_system.h>
#include "ftx.sig"
#include "log.h"

#define FILE_DIR_DEFAULT "/root"
#define FILE_NAME_LENGTH (FILE_NAME_MAX_LENGTH + 6)
/* internal message definitation */
#define EXIT_SIGNAL     0xdeadbeef

/******************************************************************************/

union itc_msg {
        uint32_t sigNo;

        struct FtxStartReqS    ftx_start_req;
        struct FtxStartCfmS    ftx_start_cfm;
        struct FtxStartRejS    ftx_start_rej;
        struct FtxDataS        ftx_data;
        struct FtxDataAckS     ftx_data_ack;
        struct FtxEndReqS      ftx_end_req;
        struct FtxEndCfmS      ftx_end_cfm;
        struct FtxEndRejS      ftx_end_rej;
};

/******************************************************************************/

struct file_info {
        uint32_t fd;
        char name[FILE_NAME_LENGTH]; 
        uint32_t size;
        uint32_t seqno;
        uint32_t pos;
        char md5[FILE_MD5SUM_STRING_LENGTH];
};

/* trx state */
static enum {
    IDLE,   
    BUSY
} ftx_state = IDLE;

static itc_mbox_id_t ftx_server_mbox = ITC_NO_ID;
static struct file_info fi;

/******************************************************************************/
static void prepare_ftx()
{
    ftx_state = IDLE;
    fi.fd = 0;
    fi.name[0] = '\0';
    fi.seqno = 0;
    fi.pos = 0;
}

/******************************************************************************/

static int
trx_d_itc_errorhandler(const char *itc_call, const char *buffer, uint32_t flags,
                             const char *file, int line)
{
    const char *basename __attribute__((unused)) = file;

    /* If itc is not compiled with file and line support we insert this function's
       file and line in order to get some idea of where to start digging. */
    if (!basename || line == 0) {
        line = __LINE__;
        file = basename = __FILE__;
    }

    while (*file) {
        if (*file++ == '/')
            basename = file;
    }

    log_err(
        "%s:%d ERROR: %s in %s with %x flags (errno = %d)",
        basename, line, buffer, itc_call, flags, errno);

    return 1;
}

/******************************************************************************/
static int getMd5(char* fileName, char *md5string)
{
    char cmd[60]; 
    sprintf(cmd, "md5sum %s | cut -c1-%d", fileName, FILE_MD5SUM_STRING_LENGTH);

    FILE *fp;

    if ((fp = popen(cmd, "r")) == NULL) {
        log_err("Error opening pipe!");
        return -1;
    }

    if(fgets(md5string, FILE_MD5SUM_STRING_LENGTH, fp) != NULL) {
        log_trace2("getMd5: calc output: %s", md5string);
    }

    if(pclose(fp))  {
        log_err("Command not found or exited with error status\n");
        return -1;
    }
    return 0;
}

/******************************************************************************/

static void
handle_ftx_start_request(itc_mbox_id_t sender,
                                 struct FtxStartReqS *req)
{
    union itc_msg *reply;
    uint32_t error_code = FTX_RESULT_SUCCESS;
    uint32_t fd;

    log_trace2("FTX_START_REQ: fileName: %s, fileSize: %lu", 
                req->fileName, req->fileSize);

    prepare_ftx();

    sprintf(fi.name, "%s/%s", FILE_DIR_DEFAULT, req->fileName);
    fi.size = req->fileSize;
    /* open a file to write */
    fd = open(fi.name, O_RDWR | O_TRUNC | O_CREAT);

    if(fd < 0){
        log_err("file open failed with errno %d", errno);     
        error_code = FTX_RESULT_INTERNAL_ERROR;
        goto start_req_error;
    }

    log_info("open file %s succeed!", fi.name);
    fi.fd = fd;
    ftx_state = BUSY;

    log_info("Send FTX_START_CFM msg to Client");
    reply = itc_alloc(sizeof(struct FtxStartCfmS),
                      FTX_START_CFM);
    itc_send(&reply, sender, ftx_server_mbox);
    return;

start_req_error:
    log_info("Send FTX_START_REJ msg to Client, error_code: %d", error_code);
    reply = itc_alloc(sizeof(struct FtxStartRejS),
                  FTX_START_REJ);
    reply->ftx_start_rej.error_code = error_code;
    itc_send(&reply, sender, ftx_server_mbox);
    return;
}

/******************************************************************************/

static void
handle_ftx_data(itc_mbox_id_t sender,
                                 struct FtxDataS *req)
{
    union itc_msg *reply;
    uint32_t error_code = FTX_RESULT_SUCCESS;
    uint32_t ret;

    log_trace2("FTX_DATA: seqno= %d, fileBlockSize= %lu",
                    req->seqNr, req->dataBlockSize);

    if(ftx_state == IDLE)
    {
        log_err("Wrong state!");
        error_code = FTX_RESULT_WRONG_STATE;
        goto ftx_data_error;
    }

    if(req->dataBlockSize == 0)
    {
        log_err("data block size is 0");
        error_code = FTX_RESULT_INVALID_PARAM;
        goto ftx_data_error;
    }
    if(req->seqNr != fi.seqno)
    {
        log_err("unexpected data sequence: waiting for %d, got %d",
                 fi.seqno, req->seqNr);
        error_code = FTX_RESULT_INVALID_PARAM;
        goto ftx_data_error;
    }
    
    /* Update sequence number first */
    fi.seqno++;

    /* write data to file */
    ret = lseek(fi.fd, fi.pos, SEEK_SET);
    if(ret < 0)
    {
        log_err("file lseek failed with errno %d", errno);                 
        error_code = FTX_RESULT_INTERNAL_ERROR;
        goto ftx_data_error;
    }
    log_trace2("file lseek ok, pos= 0x%x", fi.pos);

    ret = write(fi.fd, req->dataBlock, req->dataBlockSize);
    if(ret < 0)
    {
        log_err("file write failed with errno %d", errno);                 
        error_code = FTX_RESULT_INTERNAL_ERROR;
        goto ftx_data_error;
    }
    /* Update file offset */
    fi.pos += req->dataBlockSize;
    log_trace2("file write ok, pos= 0x%x", fi.pos);

ftx_data_error:
    log_info("Send FTX_DATA_ACK msg to Client, seqNr: %d", req->seqNr);
    reply = itc_alloc(sizeof(struct FtxDataAckS),
              FTX_DATA_ACK);
    reply->ftx_data_ack.seqNr = req->seqNr;
    reply->ftx_data_ack.error_code = error_code;
    itc_send(&reply, sender, ftx_server_mbox);
    return;

}

/******************************************************************************/

static void
handle_ftx_end_req(itc_mbox_id_t sender,
                                 struct FtxEndReqS *req)
{
    union itc_msg *reply;
    uint32_t error_code = FTX_RESULT_SUCCESS;
    char md5_server[FILE_MD5SUM_STRING_LENGTH];
    char md5_client[FILE_MD5SUM_STRING_LENGTH];

    log_trace2("FTX_END_REQ: md5= %s", req->md5String);

    if(ftx_state == IDLE)
    {
        log_err("Wrong state!");
        error_code = FTX_RESULT_WRONG_STATE;
        goto ftx_end_error;
    }

    memcpy(md5_client, req->md5String, sizeof(md5_client));
    if( strlen(md5_client) != (FILE_MD5SUM_STRING_LENGTH - 1))
    {
        log_err("wrong md5sum lenght from client!");
        error_code = FTX_RESULT_INVALID_PARAM;
        goto ftx_end_error;
    }

    /* Close file first */
    if(close(fi.fd) < 0)
    {
        log_err("file close failed with errno: %d", errno);
        error_code = FTX_RESULT_INTERNAL_ERROR;
        goto ftx_end_error;
    }
    log_trace2("file close ok");

    /* get md5sum of file */
    if(getMd5(fi.name, md5_server) == -1)
    {
        log_err("getMd5 error!");
        error_code = FTX_RESULT_INTERNAL_ERROR;
        goto ftx_end_error;
    }
  
    /* check if transfter ok */
    if(strcmp(md5_server, md5_client) != 0)
    {
        log_err("server md5sum not equal to client md5sum!");
        error_code = FTX_RESULT_INTERNAL_ERROR;
        goto ftx_end_error;       
    }else{
        log_info("Send FTX_END_CFM msg to Client");
        reply = itc_alloc(sizeof(struct FtxEndCfmS),
              FTX_END_CFM);
        reply->ftx_end_cfm.result = error_code;
        memcpy(reply->ftx_end_cfm.md5String, md5_server, FILE_MD5SUM_STRING_LENGTH);
        itc_send(&reply, sender, ftx_server_mbox);

        return;
    }

ftx_end_error:
        log_info("Send FTX_END_REJ msg to Client, error_code: %d", error_code);
        reply = itc_alloc(sizeof(struct FtxEndRejS),
              FTX_END_REJ);
        reply->ftx_end_rej.error_code = error_code;
        memcpy(reply->ftx_end_rej.md5String, md5_server, FILE_MD5SUM_STRING_LENGTH);
        itc_send(&reply, sender, ftx_server_mbox);

        /* file transfer goes wrong, delete it */
        if(fi.fd > 0)
            close(fi.fd);
        if(access(fi.name, F_OK) == 0)
        {
            log_info("bad file! deleting...");
            if( remove(fi.name) < 0)
            {
                log_err("file remove failed. errno = %d", errno);
                return;
            }
            log_info("file %s deleted!", fi.name);
        }
        return;
}


/******************************************************************************/

int main_loop(void)
{

    while (1) {
            union itc_msg *sig;
            itc_mbox_id_t sender;
            sig = itc_receive(ITC_NOFILTER, ITC_NO_TMO, ITC_FROM_ALL);

            sender = itc_sender(sig);

            switch (sig->sigNo) {
                case FTX_START_REQ:
                        log_trace2("FTX_START_REQ from 0x%08x", sender);
                        handle_ftx_start_request(sender, 
                                                 &sig->ftx_start_req);
                        break;

                case FTX_DATA:
                        log_trace2("FTX_DATA from 0x%08x", sender);
                        handle_ftx_data(sender,
                                        &sig->ftx_data);
                        break;

                case FTX_END_REQ:
                        log_trace2("FTX_END_REQ from 0x%08x", sender);
                        handle_ftx_end_req(sender,
                                           &sig->ftx_end_req);
                        break;

                case EXIT_SIGNAL:
                        log_trace2("ftx server exiting as ordered");
                        itc_free(&sig);
                        return 0;
                            
                default:
                       log_err("Unrecognized signal 0x%08x from 0x%08x", sig->sigNo, sender);
                 }

             itc_free(&sig);
     }

     itc_delete_mailbox(ftx_server_mbox);
     itc_exit();

     exit(1);
}

/*****************************************************************************/
static void print_usage()
{
    printf("Usage: trx_d <options>\n\n"
           "Options:\n"
           "    -h  Display usage information (this message).\n"
           "    -d  Daemonize the program.\n\n");
}

/*****************************************************************************/
static void exit_handler(int sig)
{
    union itc_msg *msg;

    log_info("Received signal 0x%X, terminating", sig);
    msg = itc_alloc(sizeof(uint32_t), EXIT_SIGNAL);
    itc_send(&msg, ftx_server_mbox, ITC_MY_MBOX);
}

/******************************************************************************/

int main(int argc, char *argv[])
{
    int daemonize = 0;
    int exit_code = 0;
    struct sigaction act;

    if (argc > 1) {
        if (strcmp("-d", argv[1]) == 0) {
            daemonize = 1;
        } else if (strcmp("-h", argv[1]) == 0) {
            print_usage();
            exit(0);
        } else {
            print_usage();
            exit(1);
        }
    }

    log_info("trx_d server started.");

    if (!daemonize || !daemon(0, 0)) {
        /* Initialize ITC */
        if(itc_init(16, ITC_MALLOC, NULL, ITC_NO_NAMESPACE, 0))
        {
            log_err("Unable to inizalize ITC!");
            return -1;
        }

        #ifdef HAVE_SYSLOG_H
            /* Initialize syslog */
            setlogmask(LOG_UPTO(LOG_INFO));
            openlog("ftxd", LOG_CONS | LOG_PID, LOG_USER);
        #endif

            /* Register itc error handler */
        itc_register_errorhandler(&trx_d_itc_errorhandler);

            /* Create our mailbox. */
        ftx_server_mbox = itc_create_mailbox("ftx_server", 0);
        if (ftx_server_mbox == ITC_NO_ID)
        {
            log_err("Unable to create ITC mailbox!");
            return -1;
        }
            /* Handle term signal */
        memset(&act, '\0', sizeof(act));
        act.sa_handler = &exit_handler;
        if (sigaction(SIGTERM, &act, NULL) < 0) {
            log_err("Failed to install signal exit handler");
            exit(1);
        }
        
        exit_code = main_loop();
    }
    return exit_code;
}
