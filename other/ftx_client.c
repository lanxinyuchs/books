/* ---------------------------------------------------------------------------
 *
 * @ Ericsson AB 2017 All rights reserved.
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
#include <unistd.h>
#include <errno.h>
#include <stdarg.h>
#include <string.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <itc.h>
#include "ftx.sig"

#define DATA_SIZE 128

union itc_msg {
	uint32_t sigNo;

	struct FtxStartReqS ftx_startReqs;
	struct FtxStartCfmS ftx_startCfms;
	struct FtxStartRejS ftx_startRejs;
	struct FtxDataS ftx_dataS;
	struct FtxDataAckS ftx_dataAsks;
	struct FtxEndReqS ftx_endReqs;
	struct FtxEndCfmS ftx_endCfms;
	struct FtxEndRejS ftx_endRejs;
};

enum state {
	FTX_SATRT = 0,
	FTX_DATAR,
	FTX_END
};

struct ftx_arg {
	enum state f_state;
	itc_mbox_id_t server_mbox;
	int fd;
	int count;
	int pre_size;
	char * filename;
	U32 seqNr;
};

static itc_mbox_id_t hunt_shelld(char *link)
{
	char name[128];
	union itc_msg *msg;
	itc_mbox_id_t server_mbox;
	char str_shelld[] = {"ftx_server"};

	if (link && strlen(link)) {
		snprintf(name, sizeof(name), "%s/%s", link, str_shelld);
	} else {
		printf("the board nr can't be empty\n");
		return 0;
	}

	itc_locate_async(name, NULL, ITC_MY_MBOX);

	msg = itc_receive(ITC_NOFILTER, 3000, ITC_FROM_ALL);

	if (msg == NULL)
		return 0;

	if (msg->sigNo!= ITC_LOCATE_DEFAULT_NO) {
		printf("unexpected signal 0x%x\n", msg->sigNo);
		return 0;
	}

	server_mbox = itc_sender(msg);
	itc_free(&msg);

	return server_mbox;
}

static int getMd5(char* fileName, char *md5string)
{
	char cmd[60];
	sprintf(cmd, "md5sum %s | cut -c1-%d", fileName, FILE_MD5SUM_STRING_LENGTH);

	FILE *fp;

	if ((fp = popen(cmd, "r")) == NULL) {
		printf("Error opening pipe!");
		return -1;
	}

	if(fgets(md5string, FILE_MD5SUM_STRING_LENGTH, fp) != NULL) {
		printf("getMd5: calc output: %s\n", md5string);
	}

	if(pclose(fp))  {
		printf("Command not found or exited with error status\n");
		return -1;
	}
	return 0;
}


static int ftx_transfer_end(struct ftx_arg * ftxp,char *md5string)
{
	int size,ret = 0;
	union itc_msg *msg = NULL;

	if(ftxp->f_state == FTX_DATAR)
		ftxp->f_state = FTX_END;
	else
		return -1;

	printf("ftx end require msg send \n");

	msg = itc_alloc(sizeof(struct FtxEndReqS),FTX_END_REQ);
	if (!msg){
		printf("itc alloc failed \n");
		return -1;
	}

	memset(msg->ftx_endReqs.md5String,0,FILE_MD5SUM_STRING_LENGTH);

	if(md5string != NULL)
		strncpy(msg->ftx_endReqs.md5String,md5string,FILE_MD5SUM_STRING_LENGTH);

	itc_send(&msg, ftxp->server_mbox, ITC_MY_MBOX);

	return 0;
}

static int ftx_data_transfer(struct ftx_arg * ftxp)
{
	int size,ret = 0;
	union itc_msg *msg = NULL;
	U8 md5string[FILE_MD5SUM_STRING_LENGTH] = {};

	if(ftxp->f_state != FTX_DATAR)
		return -1;

	msg = itc_alloc(sizeof(struct FtxDataS) + sizeof(U8)*(DATA_SIZE-1),FTX_DATA);
	if (!msg){
		printf("itc alloc failed \n");
		return -1;
	}

	memset(msg->ftx_dataS.dataBlock,0,DATA_SIZE);

	if(ftxp->count > 0)
		lseek(ftxp->fd, -ftxp->pre_size, SEEK_CUR);

	size = read(ftxp->fd,(U8*)msg->ftx_dataS.dataBlock,DATA_SIZE);
	if (size < 0) {
		perror("read file failed");
		return -1;
	}
	else if(size == 0){
		ret = getMd5(ftxp->filename,md5string);
		if(ret)
			return -1;
		else
			return ftx_transfer_end(ftxp,md5string);
	}
	else{
		msg->ftx_dataS.dataBlockSize = size;
		msg->ftx_dataS.seqNr = ftxp->seqNr;
	}

	ftxp->pre_size = size;
	itc_send(&msg, ftxp->server_mbox, ITC_MY_MBOX);

	return 0;
}


static int ftx_server_reject(struct ftx_arg *ftxp, U32 error,union itc_msg *msg)
{
	int ret = 0;

	switch(error){

	case FTX_RESULT_WRONG_STATE:
		printf("reject cause FTX_RESULT_WRONG_STATE,");
		break;
	case FTX_RESULT_INVALID_PARAM:
		printf("reject cause FTX_RESULT_INVALID_PARAM,");
		break;
	case FTX_RESULT_INTERNAL_ERROR:
		printf("reject cause FTX_RESULT_INTERNAL_ERROR\n");
		break;
	default:
		printf("invalid reject number\n");
		return -1;
	}

	if(ftxp->f_state == FTX_SATRT){
		printf("at stage start\n");
		ret = -1;
	}
	else if(ftxp->f_state == FTX_DATAR){
		printf("at stage data transfer\n");
		if(ftxp->count++ < 3 && ftxp->seqNr == msg->ftx_dataAsks.seqNr)
			ret = ftx_data_transfer(ftxp);
		else
			ret = ftx_transfer_end(ftxp,NULL);
	}
	else if(ftxp->f_state == FTX_END){
		printf("at stage end\n");
		ret = -1;
	}
	return ret;
}

static int ftx_date_ask(struct ftx_arg *ftxp,union itc_msg * msg)
{
	int ret = 0;

	if(msg->ftx_dataAsks.error_code == FTX_RESULT_SUCCESS){
		ftxp->seqNr++;
		ret = ftx_data_transfer(ftxp);
	}
	else if(msg->ftx_dataAsks.error_code == FTX_RESULT_INTERNAL_ERROR){
		ret = ftx_transfer_end(ftxp,NULL);
	}
	else
		ret = ftx_server_reject(ftxp,msg->ftx_dataAsks.error_code,msg);

	return ret;
}


static int ftx_transfer(char *board, char* file_name)
{
	int ret = 0;
	union itc_msg *msg;
	struct stat info;
	U32 file_size = 0;
	struct ftx_arg ftx ={FTX_SATRT,0,0,0,0,NULL,0};

	ftx.server_mbox = hunt_shelld(board);
	if (!ftx.server_mbox) {
		printf("unable to hunt %s/ftx_server\n", board);
		return -1;
	}

	ret = stat(file_name, &info);
	if(ret){
		perror("fstat filed :");
		return -1;
	}

	if(S_ISREG(info.st_mode) && info.st_size > 0)
		file_size = (unsigned int)info.st_size;
	else{
		printf("the file is incorrect or empty\n");
		return -1;
	}

	ftx.fd = open(file_name,O_RDONLY | O_NONBLOCK);
	if( ftx.fd < 0 ){
		perror("file open failed :");
		return -1;
	}

	ftx.filename = file_name;

	msg = itc_alloc(sizeof(struct FtxStartReqS),FTX_START_REQ);
	if (!msg){
		printf("itc alloc failed \n");
		return -1;
	}

	memset(msg->ftx_startReqs.fileName,0,FILE_NAME_MAX_LENGTH);
	strcpy(msg->ftx_startReqs.fileName,file_name);
	msg->ftx_startReqs.fileSize = file_size;

	itc_send(&msg, ftx.server_mbox, ITC_MY_MBOX);

	printf("ftx start require msg send \n");

	while(1){
		msg = itc_receive(ITC_NOFILTER, 1000, ITC_FROM_ALL);

		if(msg == NULL){
			if(ftx.f_state == FTX_SATRT)
				printf("receive start ask timeout , go exit\n");
			else if(ftx.f_state == FTX_DATAR){
				printf("receive data ask timeout\n");
				if(ftx.count++ < 3)
					ret = ftx_data_transfer(&ftx);
				else
					ret = ftx_transfer_end(&ftx,NULL);
				if(ret == 0)
					continue;
			}
			else
				printf("receive end ask timeout\n");

			return -1;
		}

		switch(msg->sigNo){
		case FTX_START_CFM:
			ftx.f_state = FTX_DATAR;
			ret = ftx_data_transfer(&ftx);
			break;
		case FTX_START_REJ:
			ret = ftx_server_reject(&ftx,msg->ftx_startRejs.error_code,NULL);
			break;
		case FTX_DATA_ACK:
			ret = ftx_date_ask(&ftx,msg);
			break;
		case FTX_END_CFM:
			close(ftx.fd);
			return 0;
		case FTX_END_REJ:
			ret = ftx_server_reject(&ftx,msg->ftx_endRejs.error_code,NULL);
			break;
		default:
			break;
		}

		if (msg)
			itc_free(&msg);

		if(ret)
			goto do_error;

	}

do_error:
	close(ftx.fd);
	return -1;
}


int main(int argc, char **argv)
{
	char* proc_name;
	itc_mbox_id_t client;
	char file_name[FILE_NAME_MAX_LENGTH] = "";
	int ret, i;

	ret = itc_init(16, ITC_MALLOC, NULL, NULL, 0);
	if (ret) {
		printf("itc_init failure: %d\n", ret);
		return -1;
	}

	client = itc_create_mailbox("ftx_client", 0);
	if (client == ITC_NO_ID) {
		printf("create mailbox failure: %d\n", client);
		itc_exit();
		return -1;
	}

	if (argc != 3 ) {
		printf("Usage: %s <board> <filename> \n", argv[0]);
		return 0;
	}

	snprintf(file_name, sizeof(file_name), "%s", argv[2]);

	ret = ftx_transfer(argv[1],file_name);
	if(!ret)
		printf("ftx transfer file %s succeed\n",file_name);

	itc_delete_mailbox(client);
	itc_exit();
	return 0;
}


