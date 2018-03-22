#include "xpai_xmh_if.h"
#include "xpai_xcbc_loadable_au_if.h"
#include "common.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <itc.h>
#include <stdbool.h>
#include <pthread.h>
#include <unistd.h>

#define UNUSED __attribute__((__unused__))
#define MAX_MAILBOX_NUM    32
#define MBOX "XMH_TEST"

static itc_mbox_id_t main_mbox;


union itc_msg {
	uint32_t msgno;
	struct XPAI_LoadFileDataIndS data_ind;
	struct XPAI_LoadFileDeleteIndS delete_ind;
};

static int handle_init(void)
{
	if(itc_init(MAX_MAILBOX_NUM, ITC_MALLOC, NULL, ITC_NO_NAMESPACE, 0)) {
		printf("%s: Unable to initialize ITC!\n", __func__);
		return 1;
	}

	main_mbox = itc_create_mailbox(MBOX, 0);
	if (main_mbox == ITC_NO_ID) {
		printf("%s: Cannot create ITC mailbox \"%s\"!\n",
		       __func__, MBOX);
		return 1;
	}

	return 0;
}

static uint32_t conn_test(void)
{
	int32_t  res;
	uint32_t index = 1;
	uint16_t load_res;
	uint32_t buf;
	struct XPAI_LoadFileList2S *lmc_list;
	struct XPAI_SubFileListS *lm_list;

	lmc_list = XPAI_LoadFileList2();
	if (lmc_list != NULL) {
		printf("Connection test failed for XPAI_LoadFileList2()\n");
		itc_free((union itc_msg **) &lmc_list);
		goto conn_test_failed;
	}

	lm_list = XPAI_LoadSubFileList2(index);
	if (lm_list != NULL) {
		printf("Connection test failed for XPAI_LoadSubFileList2()\n");
		itc_free((union itc_msg **) &lm_list);
		goto conn_test_failed;
	}

	res = XPAI_LoadSubFileOpen2(index, index);
	if (res >= 0) {
		printf("Connection test failed for XPAI_LoadSubFileOpen2()\n");
		goto conn_test_failed;
	}

	res = XPAI_LoadSubFileRead2(0, (uint8_t *)&buf, 0, 0);
	if (res >= 0) {
		printf("Connection test failed for XPAI_LoadSubFileRead2()\n");
		goto conn_test_failed;
	}

	res = XPAI_LoadSubFileClose(0);
	if (res >= 0) {
		printf("Connection test failed for XPAI_LoadSubFileClose()\n");
		goto conn_test_failed;
	}

	res = XPAI_LoadFileInit(NULL, &buf, 0);
	if (res != XPAI_LOAD_FILE_INIT_NOK_SERVER) {
		printf("Connection test failed for XPAI_LoadFileInit()\n");
		goto conn_test_failed;
	}

	res = XPAI_LoadFileData(0, 0, NULL);
	if (res != XPAI_LOAD_FILE_DATA_NOK_SERVER) {
		printf("Connection test failed for XPAI_LoadFileData()\n");
		goto conn_test_failed;
	}

	res = XPAI_LoadFileEnd(&load_res);
	if (res != XPAI_LOAD_FILE_END_NOK_SERVER) {
		printf("Connection test failed for XPAI_LoadFileEnd()\n");
		goto conn_test_failed;
	}

	res = XPAI_LoadFileDelete(NULL, 0);
	if (res != XPAI_LOAD_FILE_DELETE_NOK_SERVER) {
		printf("Connection test failed for XPAI_LoadFileDelete()\n");
		goto conn_test_failed;
	}

	return 0;

conn_test_failed:
	return 1;
}


static struct XPAI_LoadFileList2S *get_lmc_list(void)
{
	struct XPAI_LoadFileList2S *lmc_list = NULL;
	
	lmc_list = XPAI_LoadFileList2();
	if (lmc_list == NULL)
		goto get_lmc_list_end;

	if (lmc_list->result != XPAI_XMH_SUCCEED) {
		printf("XPAI_LoadFileList2 failed with %d\n", lmc_list->result);
		itc_free((union itc_msg **) &lmc_list);
		goto get_lmc_list_end;
	}

get_lmc_list_end:
	return lmc_list;
}

static void find_index(int32_t *valid_index, int32_t *invalid_index,
                       struct XPAI_LoadFileList2S *lmc_list)
{
	uint32_t i;
	for (i = 0; i < XPAI_XMH_MAX_LOADFILES; i++) {
		if (lmc_list->info[i].state == XPAI_XMH_STATE_VALID)
			*valid_index = i;
		else
			*invalid_index = i;
	}
	return;
}

static uint32_t check_delete_ind(uint32_t ind)
{
	union itc_msg *rec = NULL;
	uint32_t delete_filter[] = {1, XPAI_LOAD_FILE_DELETE_IND};

	rec = itc_receive(delete_filter, 40000, ITC_FROM_ALL);
	if (!rec) {
		printf("Server didn't reply in 15s, quit test\n");
		goto check_ind_failed;
	}
	if (rec->delete_ind.result != ind) {
		printf("Delete LMC failed, error = %d\n",
		       rec->delete_ind.result);
		goto check_ind_failed;
	}
	itc_free(&rec);

	return 0;
check_ind_failed:
	if (rec)
		itc_free(&rec);
	return 1;
}

static uint32_t clean_flash(char *sw_pid, struct XPAI_LoadFileList2S *lmc_list)
{
	uint32_t res;
	uint32_t do_delete = 0;

	for (int i = 0; i < XPAI_XMH_MAX_LOADFILES; i++) {
		if (lmc_list->info[i].state != XPAI_XMH_STATE_VALID)
			continue;
		if (!strcmp(lmc_list->info[i].swPid, sw_pid)) {
			do_delete = 1;
			break;
		}
	}
	if (do_delete) {
		res = XPAI_LoadFileDelete(sw_pid, main_mbox);
		if (res != XPAI_LOAD_FILE_DELETE_OK) {
			printf("Delete test lmc failed with error %d\n", res);
			goto clean_failed;
		}
		if (check_delete_ind(XPAI_LOAD_FILE_DELETE_RESULT_OK))
			goto clean_failed;
	}

	return 0;

clean_failed:
	return 1;

}
static void test1(struct XPAI_LoadFileList2S *lmc_list)
{
	uint32_t i;

	printf("start test 1, get LMC list\n");

	for (i = 0; i < XPAI_XMH_MAX_LOADFILES; i++) {
		printf("LMC%d:\n state=%d\n isCurrentLoadFile=%d\n size=%d\n lmc_size=%d\n "
			"seqNo=%d\n fileType=%d\n prodDateTime=%d\n swPid=%s\n "
			"noLoadSubFiles=%d\n working=%d\n locked=%d\n",
			i+1,
			lmc_list->info[i].state,
			lmc_list->info[i].isCurrentLoadFile,
			lmc_list->info[i].size,
                        lmc_list->info[i].lmc_size,
			lmc_list->info[i].seqNo,
			lmc_list->info[i].fileType,
			lmc_list->info[i].prodDateTime,
			lmc_list->info[i].swPid,
			lmc_list->info[i].noLoadSubFiles,
			lmc_list->info[i].working,
			lmc_list->info[i].locked);
	}

}

static uint32_t test2(int32_t valid_index, int32_t invalid_index)
{
	uint32_t res = 1;
	uint32_t i;
	struct XPAI_SubFileListS *lm_list = NULL;

	printf("start test2, get LM list");
	lm_list = XPAI_LoadSubFileList2(valid_index);
	if (lm_list == NULL) {
		printf("XPAI_LoadSubFileList2 returned NULL\n");
		goto test2_failed;
	}
	if (lm_list->result != XPAI_XMH_SUCCEED) {
		printf("XPAI_LoadSubFileList2 failed with %d\n",
		       lm_list->result);
		goto test2_failed;
	}
	for (i = 0; i < lm_list->noLoadSubFiles; i++) {
		printf("LM%d magic no: %s\n", i, lm_list->info[i]);
	}

	itc_free((union itc_msg **) &lm_list);
	/* Invalid LMC for negative test */
	lm_list = XPAI_LoadSubFileList2(invalid_index);
	if (lm_list == NULL) {
		printf("XPAI_LoadSubFileList2 returned NULL\n");
		goto test2_failed;
	}
	if (lm_list->result != XPAI_XMH_NOT_VALID) {
		printf("negative test failed\n");
		goto test2_failed;
	}
	itc_free((union itc_msg **) &lm_list);
	/* Illegal index */
	lm_list = XPAI_LoadSubFileList2(XPAI_XMH_MAX_LOADFILES);
	if (lm_list == NULL) {
		printf("XPAI_LoadSubFileList2 returned NULL\n");
		goto test2_failed;
	}
	if (lm_list->result != XPAI_XMH_ILLEGAL_INDEX) {
		printf("negative test failed\n");
		goto test2_failed;
	}
	res = 0;

test2_failed:
	if (lm_list)
		itc_free((union itc_msg **) &lm_list);
	return res;
}

static uint32_t test3(struct XPAI_LoadFileList2S *lmc_list,
                      char *lmc_path, char *sw_pid)
{
	uint32_t retval = 0;
	uint32_t block_size = 0;
	uint16_t load_res = 0;
	FILE    *fp = NULL;
	int      i, n;
	uint32_t do_close = 0;
	uint32_t bytes_send = 0;
	char    *buf = NULL;
	union itc_msg *msg = NULL;
	uint32_t load_filter[] = {1, XPAI_LOAD_FILE_DATA_IND};
    uint32_t seq_num = 0;
    uint32_t retval1 = 0;

	printf("start test 3, load %s to flash\n", lmc_path);
	fp = fopen(lmc_path, "r");
	if (fp == NULL) {
		printf("Counld not find file %s\n", lmc_path);
		goto test3_failed;
	}

	/* Check if we have a lmc called lmc_test, if so,delete it */
	if (clean_flash(sw_pid, lmc_list))
		goto test3_failed;

	/* Load lmc_test to flash */
	retval = XPAI_LoadFileInit(sw_pid, &block_size, main_mbox);
	if (retval != XPAI_LOAD_FILE_INIT_OK) {
		printf("XPAI_LoadFileInit failed with %d\n", retval);
		goto test3_failed;
	}
	do_close = 1;
	/* Negative test, try to init again */
	retval = XPAI_LoadFileInit(sw_pid, &block_size, main_mbox);
	if (retval != XPAI_LOAD_FILE_INIT_NOK_WRONG_STATE) {
		printf("XPAI_LoadFileInit should failed with %d, not get %d\n",
		       XPAI_LOAD_FILE_INIT_NOK_WRONG_STATE, retval);
		goto test3_failed;
	}
	/* start loading */
	buf = malloc(block_size);
	if (buf == NULL) {
		printf("failed to alloc buf with block size %u\n", block_size);
		goto test3_failed;
	}
	i = 0;
	n = fread(buf, sizeof(uint8_t), block_size, fp);
	if (n > 0) {
		bytes_send += n;
	} else if (n < 0) {
		printf("Read got negative value %d", n);
		goto test3_failed;
	} else if (n == 0) {
		printf("Can't read LMC\n");
		goto test3_failed;
	}

       /*XPAI_LoadFileDataGetSeq test*/
    retval1 = XPAI_LoadFileDataGetSeq(&seq_num);
    if(retval1 == XPAI_LOAD_FILE_DATA_GET_SEQ_OK){
        printf("XPAI_LoadFileDataGetSeq return OK, seqNr = %d\n", seq_num);
    }else{
        printf("XPAI_LoadFileDataGetSeq return NOK, errorCode = %d\n", retval1);
    }
    
	retval = XPAI_LoadFileData(block_size, i, buf); 

	while (retval == XPAI_LOAD_FILE_DATA_OK) {
		msg = itc_receive(load_filter, 15000, ITC_FROM_ALL);
		if (!msg) {
			printf("Server didn't reply in 15s, quit test\n");
			goto test3_failed;
		}
		if (msg->data_ind.lmSeqNr != i) {
			printf("Wrong sequence numbers, expected %d, got %d\n",
			       i, msg->data_ind.lmSeqNr);
			goto test3_failed;
		}
		itc_free(&msg);

		if ((i % 100) == 0)
			printf("Wrting to flash, block #%d\n", i/100);
		n = fread(buf, sizeof(uint8_t), block_size, fp);
		if (n > 0) {
			bytes_send += n;
		} else if (n < 0) {
			printf("Got negative read value %d\n", n);
			goto test3_failed;
		} else if (n == 0) {
			break;
		}
		i++;
           /*XPAI_LoadFileDataGetSeq test*/
	    retval1 = XPAI_LoadFileDataGetSeq(&seq_num);
	    if(retval1 == XPAI_LOAD_FILE_DATA_GET_SEQ_OK){
	        printf("XPAI_LoadFileDataGetSeq return OK, seqNr = %d\n", seq_num);
	    }else{
	        printf("XPAI_LoadFileDataGetSeq return NOK, errorCode = %d\n", retval1);
	    }
		retval = XPAI_LoadFileData(block_size, i, buf);
	}

	fclose(fp);
	fp = NULL;

	printf("Call load file end\n");
	retval = XPAI_LoadFileEnd(&load_res);
	if (retval != XPAI_LOAD_FILE_END_OK) {
		printf("XPAI_LoadFileEnd failed with %d\n", retval);
		goto test3_failed;
	}
	if (load_res == XPAI_LOAD_FILE_END_RESULT_LOAD_COMPLETED) {
		printf("LOAD complete\n");
		/* Try to load again, should fail */
		retval = XPAI_LoadFileInit(sw_pid, &block_size, main_mbox);
		if (retval != XPAI_LOAD_FILE_INIT_NOK_WRONG_CONFIG_DATA) {
			printf("XPAI_LoadFileInit should failed with %d,"
			       " not get %d\n",
			       XPAI_LOAD_FILE_INIT_NOK_WRONG_CONFIG_DATA,
			       retval);
			goto test3_failed;
		}
	} else {
		printf("LOAD abort\n");
		goto test3_failed;
	}

	/* some nagetive test */
	retval = XPAI_LoadFileData(block_size, i, buf);
	if (retval != XPAI_LOAD_FILE_DATA_NOK_WRONG_STATE) {
		printf("XPAI_LoadFileData should fail with %d, now get %d\n",
		       XPAI_LOAD_FILE_DATA_NOK_WRONG_STATE, retval);
		goto test3_failed;
	}
	retval = XPAI_LoadFileEnd(&load_res);
	if (retval != XPAI_LOAD_FILE_END_NOK_WRONG_STATE) {
		printf("XPAI_LoadFileEnd should fail with %d not get %d\n",
		       XPAI_LOAD_FILE_END_NOK_WRONG_STATE, retval);
		goto test3_failed;
	}
	free(buf);
	return 0;
test3_failed:
	if (do_close) {
		printf("Call load file end\n");
		retval = XPAI_LoadFileEnd(&load_res);
		if (retval != XPAI_LOAD_FILE_END_OK)
			printf("XPAI_LoadFileEnd failed with %d\n", retval);
		if (load_res == XPAI_LOAD_FILE_END_RESULT_LOAD_COMPLETED)
			printf("LOAD complete\n");
		else
			printf("LOAD abort\n");
	}
	if (msg)
		itc_free(&msg);
	if (fp)
		fclose(fp);
	if (buf)
		free(buf);

	return 1;
}

static uint32_t test4(int32_t invalid_index,
                      struct XPAI_LoadFileList2S *lmc_list,
		      char *sw_pid)
{
	int32_t  res;
	int32_t  handle;
	uint32_t valid_index = 4;
	uint32_t size = 4;
	uint32_t pos = 0x0;
	uint8_t	 buf[size];
	uint32_t lm_index = 0;

	memset(buf, 0, size);

	printf("start test4, open read and close\n");
	/* XPAI_LoadSubFileOpen2() test */
	for (int i = 0; i < XPAI_XMH_MAX_LOADFILES; i++) {
		if (lmc_list->info[i].state != XPAI_XMH_STATE_VALID)
			continue;
		if (!strcmp(lmc_list->info[i].swPid, sw_pid)) {
			valid_index = i;
			break;
		}
	}
	if (valid_index == 4) {
		printf("Please load a LMC before run test4\n");
		goto test4_failed;
	}
	res = XPAI_LoadSubFileOpen2(valid_index, lm_index);	
	if (res < 0) {
		printf("XPAI_LoadSubFileOpen2 failed with %d\n", res);
		goto test4_failed;
	}
	handle = res;

	res = XPAI_LoadSubFileOpen2(XPAI_XMH_MAX_LOADFILES, lm_index);
	if (res != XPAI_XMH_ILLEGAL_INDEX) {
		printf("XPAI_LoadSubFileOpen2 negative test failed,"
		       "returned %d while expecting %d\n",
		       res, XPAI_XMH_ILLEGAL_INDEX);
		goto test4_failed;
	}

	res = XPAI_LoadSubFileOpen2(valid_index, XPAI_XMH_MAX_SUBFILES);
	if (res != XPAI_XMH_ILLEGAL_INDEX) {
		printf("XPAI_LoadSubFileOpen2 negative test failed,"
		       "returned %d while expecting %d\n",
		       res, XPAI_XMH_ILLEGAL_INDEX);
		goto test4_failed;
	}

	res = XPAI_LoadSubFileOpen2(invalid_index, lm_index);
	if (res != XPAI_XMH_NOT_VALID) {
		printf("XPAI_LoadSubFileOpen2 negative test failed,"
		       "returned %d while expecting %d\n",
		       res, XPAI_XMH_NOT_VALID);
		goto test4_failed;
	}
	/* XPAI_LoadSubFileRead2() test*/
	res = XPAI_LoadSubFileRead2(handle, buf, pos, size);
	if (res < 0) {
		printf("XPAI_LoadSubFileRead2 failed with %d\n", res);
		goto test4_failed;
	}

	res = XPAI_LoadSubFileRead2(-1, buf, pos, size);
	if (res != XPAI_XMH_ILLEGAL_HANDLE) {
		printf("XPAI_LoadSubFileRead2 negative test failed,"
		       "returned %d while expecting %d\n",
		       res, XPAI_XMH_ILLEGAL_HANDLE);
		goto test4_failed;
	}

	res = XPAI_LoadSubFileRead2(handle + 1, buf, pos, size);
	if (res != XPAI_XMH_ILLEGAL_HANDLE) {
		printf("XPAI_LoadSubFileRead2 negative test failed,"
		       "returned %d while expecting %d\n",
		       res, XPAI_XMH_ILLEGAL_HANDLE);
		goto test4_failed;
	}

	res = XPAI_LoadSubFileRead2(handle, buf, 0xFFFFFFFF, size);
	if (res != XPAI_XMH_FAIL) {
		printf("XPAI_LoadSubFileRead2 negative test failed,"
		       "returned %d while expecting %d\n",
		       res, XPAI_XMH_FAIL);
		goto test4_failed;
	}
	/* XPAI_LoadSubFileClose() test */
	res = XPAI_LoadSubFileClose(handle);
	if (res != XPAI_XMH_SUCCEED) {
		printf("XPAI_LoadSubFileclose failed with %d\n", res);
		goto test4_failed;
	}

	res = XPAI_LoadSubFileClose(handle);
	if (res != XPAI_XMH_ILLEGAL_HANDLE) {
		printf("XPAI_LoadSubFileClose negative test failed,"
		       "returned %d while expecting %d\n",
		       res, XPAI_XMH_ILLEGAL_HANDLE);
		goto test4_failed;
	}

	res = XPAI_LoadSubFileClose(-1);
	if (res != XPAI_XMH_ILLEGAL_HANDLE) {
		printf("XPAI_LoadSubFileClose negative test failed,"
		       "returned %d while expecting %d\n",
		       res, XPAI_XMH_ILLEGAL_HANDLE);
		goto test4_failed;
	}
	return 0;

test4_failed:
	return 1;
}

static uint32_t test5(char *sw_pid)
{
	uint32_t retval;

	printf("XPAI_LoadFileDelete test 1\n");	
	retval = XPAI_LoadFileDelete(NULL, main_mbox);
	if (retval != XPAI_LOAD_FILE_DELETE_OK) {
		printf("XPAI_LoadFileDelete failed with %d\n", retval);
		goto test5_failed;
	}
	if (check_delete_ind(XPAI_LOAD_FILE_DELETE_RESULT_OK))
		goto test5_failed;
	printf("XPAI_LoadFileDelete test 2\n");	
	/* remove lmc_test loaded by test3 */
	retval = XPAI_LoadFileDelete(sw_pid, main_mbox);
	if (retval != XPAI_LOAD_FILE_DELETE_OK) {
		printf("XPAI_LoadFileDelete failed with %d\n", retval);
		goto test5_failed;
	}
	if (check_delete_ind(XPAI_LOAD_FILE_DELETE_RESULT_OK))
		goto test5_failed;
	printf("XPAI_LoadFileDelete test 3\n");	
	/* try to delete the same lmc again */
	retval = XPAI_LoadFileDelete(sw_pid, main_mbox);
	if (retval != XPAI_LOAD_FILE_DELETE_NOK_WRONG_PARAM) {
		printf("XPAI_LoadFileDelete should fail with %d while get %d\n",
		       XPAI_LOAD_FILE_DELETE_NOK_WRONG_PARAM, retval);
		goto test5_failed;
	}
	return 0;
test5_failed:
	return 1;
}

static void print_usage(void)
{
	printf("Usage: xpai-xmh-test -c <test case number>"
	       " [-p <lmc_path> -n <lmc_pid>]\n\n"
	       "\twhere <test case number> is one of:\n"
	       "\t1: Get LMC list test\n"
	       "\t2: Get LM list test\n"
	       "\t3: Load LMC to flash test\n"
	       "\t4: Read from LMC test\n"
	       "\t5: Delete lmc test\n"
	      );
}

static uint32_t get_option(int argc, char **argv, char *lmc_path, char *sw_pid)
{
	uint32_t tc = 0;
	int opt;

	while ((opt = getopt(argc, argv, "c:p:n:")) != -1) {
		switch (opt) {
		case 'c':
			tc = atoi(optarg);
			printf("run testcase %u\n", tc);
			break;
		case 'p':
			strncpy(lmc_path, optarg, strlen(optarg) + 1);
			break;
		case 'n':
			strncpy(sw_pid, optarg, strlen(optarg) + 1);
			break;
		default:
			tc = 0;
			return tc;
		}
	}

	return tc;
}

int main(int argc, char **argv)
{
	int res;
	int tc;
	char     lmc_path[50];
	char     sw_pid[33];
	int32_t  valid_index = -1;
	int32_t  invalid_index = -1;
	struct XPAI_LoadFileList2S *lmc_list = NULL;

	if ((argc < 3) || (argc > 7)) {
		printf("Wrong argument number\n");
		print_usage();
		return EXIT_FAILURE;
	}
	tc = get_option(argc, argv, lmc_path, sw_pid);
	if (tc == 0) {
		printf("Cannot find testcase argument\n");
		print_usage();
		return EXIT_FAILURE;
	}

	res = handle_init();
	if (res)
		return -1;

	/* Run connection test */
	res = conn_test();
	if (res) {
		printf("Negative connection test failed\n");
		return -1;
	}

	res = xpai_lmc_init();
	if (res != INIT_OK) {
		printf("LMC init failed, return %d\n", res);
		return -1;
	}
	lmc_list = get_lmc_list();
	if (!lmc_list) {
		printf("failed to get LMC list\n");
		return -1;
	}
	find_index(&valid_index, &invalid_index, lmc_list);
	if (valid_index < 0) {
		printf("We don't have a valid LMC for test\n");
		itc_free((union itc_msg **) &lmc_list);
		return -1;
	}
	switch (tc) {
	/* Get LMC list */
	case 1:
		test1(lmc_list);
		break;

	/* Get LM list */
	case 2:
		if (test2(valid_index, invalid_index))
			goto stop_test;
		break;
	/* load lmc to flash */
	case 3:
		if (test3(lmc_list, lmc_path, sw_pid))
			goto stop_test;
		break;

	/* load subfile open read and close */
	case 4:
		if (test4(invalid_index, lmc_list, sw_pid))
			goto stop_test;
		break;

		/* delete lmc */
	case 5:
		if (test5(sw_pid))
			goto stop_test;
		break;

	/* */
	default:
		print_usage();
		if (lmc_list)
			itc_free((union itc_msg **) &lmc_list);
		return EXIT_FAILURE;
	}

	printf("\n*** LMC test case %d have passed ***\n", tc);
stop_test:
	if (lmc_list)
		itc_free((union itc_msg **) &lmc_list);
	printf("Quit test\n");

	return EXIT_SUCCESS;
}
