#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "itc.h"
#include "xpai_xhl_if.h"
#include "common.h"

#define NODEID_DELETE_SHELL_CMD   "nodeid delete"
#define MAX_NUM_NODEID            128


static void print_usage(char *pgm)
{
	printf("Usage: %s -tc <test case number>\n\n"
	       "\twhere <test case number> is one of:\n"
	       "\t1: Write/read first basic entry\n"
	       "\t2: Write/read max entry length\n"
	       "\t3: Write/read too big entry\n"
	       "\t4: Write/read multiple nodeid\n"
	       "\t5: Write/read max num of nodeid\n"
	       "\t6: Read empty nodeid\n",
	       pgm);
}

static int compare_buf(uint8_t *b1, uint8_t *b2, uint32_t len)
{
	uint32_t i;

	for (i = 0; i < len; i++) {
	/*	printf("b1[%d]=0x%x, b2[%d]=0x%x\n", i, b1[i], i, b2[i]);*/
		if (b1[i] != b2[i]) {
			return 1;
		}
	}

	return 0;
}

static int test_nodeid_basic_first_entry(int tc_no)
{
	uint32_t res, status = 0, read_len = 0;
	uint8_t  read_buf[50];

	uint8_t  write_arr[] = { 0xDE, 0xAD, 0xBE, 0xEF };
	uint32_t write_len = 4;

	printf("\t%d. test_nodeid_basic_first_entry\n", tc_no);

	printf("Calling XPAI_WriteNodeId(%d, %p)\n", write_len, (void*)write_arr);
	res = XPAI_WriteNodeId(write_len, write_arr);
	if (res != XPAI_WRITE_NODE_ID_OK) {
		printf("XPAI_WriteNodeId returned an error code: %d\n", res);
		status = -1;
		goto out;
	}

	printf("Calling XPAI_ReadNodeId(%d, %p)\n", read_len, (void*)read_buf);
	res = XPAI_ReadNodeId(&read_len, read_buf);
	if (res != XPAI_READ_NODE_ID_OK) {
		printf("XPAI_ReadNodeId returned an error code: %d\n", res);
		status = -1;
		goto out;
	}

	if (read_len != write_len) {
		printf("Mismatch in read (%d) and write (%d) length for Node ID\n",
		       read_len, write_len);
		status = -1;
		goto out;
	}

	if (compare_buf(write_arr, read_buf, read_len)) {
		printf("Mismatch when comparing written and read Node ID\n");
		status = -1;
		goto out;
	}

out:
	return status;

}

static int test_nodeid_max_len_entry(int tc_no)
{
	uint32_t res, status = 0, read_len = 0;
	uint8_t  read_buf[50];

	uint8_t max_write_arr[] = { 0xFF, 0xEE, 0xDD, 0xCC, 0xBB, 0xAA, 0x09, 0x08,
	                0x07, 0x06, 0x05, 0x04, 0x03, 0x02, 0x01, 0x00, 0x0F, 0x0E,
	                0x0D, 0x0C, 0x0B, 0x0A, 0x09, 0x08, 0x07, 0x06, 0x05, 0x04,
	                0x03, 0x02, 0x01, 0x00, 0x0F, 0x0E, 0x0D, 0x0C, 0x0B, 0x0A,
	                0x09, 0x08, 0x07, 0x06, 0x05, 0x04, 0x03, 0x02, 0x01, 0x00,
	                0x0F, 0x0E };

	uint32_t max_write_len = 50;

	printf("\t%d. test_nodeid_max_len_entry\n", tc_no);

	printf("Calling XPAI_WriteNodeId(%d, %p)\n", max_write_len, (void*)max_write_arr);
	res = XPAI_WriteNodeId(max_write_len, max_write_arr);
	if (res != XPAI_WRITE_NODE_ID_OK) {
		printf("XPAI_WriteNodeId returned an error code: %d\n", res);
		status = -1;
		goto out;
	}

	printf("Calling XPAI_ReadNodeId(%d, %p)\n", read_len, (void*)read_buf);
	res = XPAI_ReadNodeId(&read_len, read_buf);
	if (res != XPAI_READ_NODE_ID_OK) {
		printf("XPAI_ReadNodeId returned an error code: %d\n", res);
		status = -1;
		goto out;
	}

	if (read_len != max_write_len) {
		printf("Mismatch in read (%d) and write (%d) length for Node ID\n",
		       read_len, max_write_len);
		status = -1;
		goto out;
	}

	if (compare_buf(max_write_arr, read_buf, read_len)) {
		printf("Mismatch when comparing written and read Node ID\n");
		status = -1;
		goto out;
	}

out:
	return status;

}

static int test_nodeid_too_big_entry(int tc_no)
{
	uint32_t res, status = 0, read_len = 0;
	uint8_t  read_buf[50];

	uint8_t  too_big_arr[] = { 0xFF, 0xEE, 0xDD, 0xCC, 0xBB, 0xAA, 0xFF, 0xEE, 0xDD, 0xCC,
			     0xBB, 0xAA, 0xFF, 0xEE, 0xDD, 0xCC, 0xBB, 0xAA, 0xFF, 0xEE,
			     0xDD, 0xCC, 0xBB, 0xAA, 0xFF, 0xEE, 0xDD, 0xCC, 0xBB, 0xAA,
			     0xFF, 0xEE, 0xDD, 0xCC, 0xBB, 0xAA, 0xFF, 0xEE, 0xDD, 0xCC,
			     0xBB, 0xAA, 0xFF, 0xEE, 0xDD, 0xCC, 0xBB, 0xAA, 0xFF, 0xEE,
			     0xDD };
	uint32_t too_big_write_len = 51;

	uint8_t  check_arr[] = { 0xAC, 0xEE, 0xDD, 0xCC, 0xBB, 0xAA, 0xFF, 0xEE, 0xDD, 0xCC,
			     0xBB, 0xAA, 0xFF, 0xEE, 0xDD, 0xCC, 0xBB, 0xAA, 0xFF, 0xEE,
			     0xDD, 0xCC, 0xBB, 0xAA, 0xFF, 0xEE, 0xDD, 0xCC, 0xBB, 0xAA,
			     0xFF, 0xEE, 0xDD, 0xCC, 0xBB, 0xAA, 0xFF, 0xEE, 0xDD, 0xCC,
			     0xBB, 0xAA, 0xFF, 0xEE, 0xDD, 0xCC, 0xBB, 0xAA, 0xFF, 0xEE};

	uint32_t check_write_len = 50;


	printf("\t%d. test_nodeid_too_big_entry\n", tc_no);

	/*write correct array so we can check
	 * that it is not corrupted later*/
	printf("Calling XPAI_WriteNodeId(%d, %p)\n",
	       check_write_len,
	       (void*) check_arr);
	res = XPAI_WriteNodeId(check_write_len, check_arr);
	if (res != XPAI_WRITE_NODE_ID_OK) {
		printf("XPAI_WriteNodeId returned an error code: %d\n", res);
		status = -1;
		goto out;
	}

	/*write too big nodeid, expect error code*/
	printf("Calling XPAI_WriteNodeId(%d, %p)\n", too_big_write_len, (void*)too_big_arr);
	res = XPAI_WriteNodeId(too_big_write_len, too_big_arr);
	if (res == XPAI_WRITE_NODE_ID_OK) {
		printf("XPAI_WriteNodeId should fail\n");
		status = -1;
		goto out;
	}

	/* Expect last written Node ID to be intact */
	printf("Calling XPAI_ReadNodeId(%d, %p)\n", read_len, (void*)read_buf);
	res = XPAI_ReadNodeId(&read_len, read_buf);
	if (res != XPAI_READ_NODE_ID_OK) {
		printf("XPAI_ReadNodeId returned an error code: %d\n", res);
		status = -1;
		goto out;
	}

	if (read_len != check_write_len) {
		printf("Mismatch in read (%d) and write (%d) length for Node ID\n",
		       read_len, check_write_len);
		status = -1;
		goto out;
	}

	if (compare_buf(check_arr, read_buf, read_len)) {
		printf("Mismatch when comparing written and read Node ID\n");
		status = -1;
		goto out;
	}

out:
	return status;

}

static int test_several_node_id_write(int tc_no)
{
	uint32_t res, status = 0, read_len = 0, i;
	uint8_t  read_buf[50];

	uint8_t  write_arr[] = { 0xDE, 0xAD, 0xBE, 0xEF };
	uint32_t write_len = 4;

	printf("\t%d. test_several_node_id_write\n", tc_no);

	/* Write several Node IDs, verify that the last written Node ID is the one */
	/* is returned when Node ID is read */
	for (i = 0; i < 10; i++) {
		printf("Calling XPAI_WriteNodeId(%d, %p)\n",
		       write_len,
		       (void*) write_arr);
		write_arr[write_len-1] = i;
		res = XPAI_WriteNodeId(write_len, write_arr);
		if (res != XPAI_WRITE_NODE_ID_OK) {
			printf("XPAI_WriteNodeId returned an error code: %d\n",
			       res);
			status = -1;
			goto out;
		}
	}
	printf("Calling XPAI_ReadNodeId(%d, %p)\n", read_len, (void*)read_buf);
	res = XPAI_ReadNodeId(&read_len, read_buf);
	if (res != XPAI_READ_NODE_ID_OK) {
		printf("XPAI_ReadNodeId returned an error code: %d\n", res);
		status = -1;
		goto out;
	}

	if (read_len != write_len) {
		printf("Mismatch in read (%d) and write (%d) length for Node ID\n",
		       read_len, write_len);
		status = -1;
		goto out;
	}

	if (compare_buf(write_arr, read_buf, read_len)) {
		printf("Mismatch when comparing written and read Node ID\n");
		status = -1;
		goto out;
	}

out:
	return status;

}

static int test_max_num_nodeid_entries(int tc_no)
{
	uint32_t res, status = 0, read_len = 0, i;
	uint8_t  read_buf[50];
	int      exit_stat;

	uint8_t max_write_arr[] = { 0xFF, 0xEE, 0xDD, 0xCC, 0xBB, 0xAA, 0x09, 0x08,
	                0x07, 0x06, 0x05, 0x04, 0x03, 0x02, 0x01, 0x00, 0x0F, 0x0E,
	                0x0D, 0x0C, 0x0B, 0x0A, 0x09, 0x08, 0x07, 0x06, 0x05, 0x04,
	                0x03, 0x02, 0x01, 0x00, 0x0F, 0x0E, 0x0D, 0x0C, 0x0B, 0x0A,
	                0x09, 0x08, 0x07, 0x06, 0x05, 0x04, 0x03, 0x02, 0x01, 0x00,
	                0x0F, 0x0E };

	uint32_t max_write_len = 50;

	printf("\t%d. test_max_num_nodeid_entries\n", tc_no);

	/*clear nodeid*/

	exit_stat = system(NODEID_DELETE_SHELL_CMD);
	if (!WIFEXITED(exit_stat)) {
		printf("System call terminated abnormally!\n");

		if (WIFSIGNALED(exit_stat)) {
			printf("The system call was terminated with the signal %d",
			       WTERMSIG(exit_stat));
		}
		status = -1;
		goto out;
	}

	/* Write several Node IDs, verify that the last written Node ID is the one */
	/* is returned when Node ID is read */

	for (i = 0; i < MAX_NUM_NODEID; i++) {
		max_write_arr[49] = i;
		res = XPAI_WriteNodeId(max_write_len, max_write_arr);
		if (res != XPAI_WRITE_NODE_ID_OK) {
			printf("XPAI_WriteNodeId returned an error code: %d\n",
			       res);
			status = -1;
			goto out;
		}

		res = XPAI_ReadNodeId(&read_len, read_buf);
		if (res != XPAI_READ_NODE_ID_OK) {
			printf("XPAI_ReadNodeId returned an error code: %d\n",
			       res);
			status = -1;
			goto out;
		}
		if (read_len != max_write_len) {
			printf("Mismatch in read (%d) and write (%d) length for Node ID\n",
			       read_len,
			       max_write_len);
			status = -1;
			goto out;
		}

		if (compare_buf(max_write_arr, read_buf, read_len)) {
			printf("Mismatch when comparing written and read Node ID\n");
			status = -1;
			goto out;
		}
	}

	/* Try to write to full NODE ID */
	res = XPAI_WriteNodeId(max_write_len, max_write_arr);
	if (res == XPAI_WRITE_NODE_ID_OK) {
		printf("Expected XPAI_WriteNodeId to fail");
		status = -1;
		goto out;
	}

	/* Expect last written Node ID to be intact */
	printf("Calling XPAI_ReadNodeId(%d, %p)\n", read_len, (void*)read_buf);
	res = XPAI_ReadNodeId(&read_len, read_buf);
	if (res != XPAI_READ_NODE_ID_OK) {
		printf("XPAI_ReadNodeId returned an error code: %d\n", res);
		status = -1;
		goto out;
	}
	if (read_len != max_write_len) {
		printf("Mismatch in read (%d) and write (%d) length for Node ID\n",
		       read_len, max_write_len);
		status = -1;
		goto out;
	}

	if (compare_buf(max_write_arr, read_buf, read_len)) {
		printf("Mismatch when comparing written and read Node ID\n");
		status = -1;
		goto out;
	}

	/*clean after*/
	exit_stat = system(NODEID_DELETE_SHELL_CMD);
	if (!WIFEXITED(exit_stat)) {
		printf("System call terminated abnormally!\n");
		if (WIFSIGNALED(exit_stat)) {
			printf("The system call was terminated with the signal %d",
			       WTERMSIG(exit_stat));
		}
		status = -1;
	}

out:
	return status;

}

static int test_read_empty_nodeid(int tc_no)
{
	uint32_t res, status = 0, read_len = 0;
	uint8_t  read_buf[50];
	int      exit_stat;

	printf("\t%d. test_read_empty_nodeid\n", tc_no);

	/*clear nodeid*/

	exit_stat = system(NODEID_DELETE_SHELL_CMD);
	if (!WIFEXITED(exit_stat)) {
		printf("System call terminated abnormally!\n");

		if (WIFSIGNALED(exit_stat)) {
			printf("The system call was terminated with the signal %d",
			       WTERMSIG(exit_stat));
		}
		status = -1;
		goto out;
	}

	/* try to read empty nodeid partition */
	printf("Calling XPAI_ReadNodeId(%d, %p)\n", read_len, (void*)read_buf);
	res = XPAI_ReadNodeId(&read_len, read_buf);
	if (res == XPAI_READ_NODE_ID_OK) {
		printf("Expected XPAI_ReadNodeId to fail\n");
		status = -1;
		goto out;
	}

out:
	return status;
}

static int init_itc(void)
{
	/* Initialize ITC and create mailbox */
	itc_init(1 + 1, ITC_MALLOC, NULL, ITC_NO_NAMESPACE, 0);
	if(itc_create_mailbox("nodeid_test", 0) == ITC_NO_ID) {
		printf("failed to create nodeid_test mailbox\n");
		return 1;
	}
	return 0;
}

static void delete_itc(void)
{
	itc_mbox_id_t mbox = itc_current_mbox();
	if( mbox != ITC_NO_ID) {
		itc_delete_mailbox(mbox);
	}
}


int main(int argc, char **argv)
{
	int status = EXIT_SUCCESS;
	int result = 0;
	int tc = 0;

	if (argc < 3) {
		print_usage(argv[0]);
		exit(-1);
	}

	if (argc >= 3) {
		if (strstr(argv[1], "-tc") != NULL) {
			tc = atoi(argv[2]);
		}
	}

	/* Set up */
	if(init_itc()) {
		return 1;
	}

	result = xpai_nodeid_init();
	if(result != INIT_OK) {
		printf("Nodeid init failed, return %d\n", result);
		return -1;
	}

	switch (tc) {
		case 1:
			fflush(stdout);
			result = test_nodeid_basic_first_entry(tc);
			break;
		case 2:
			fflush(stdout);
			result = test_nodeid_max_len_entry(tc);
			break;
		case 3:
			fflush(stdout);
			result = test_nodeid_too_big_entry(tc);
			break;
		case 4:
			fflush(stdout);
			result = test_several_node_id_write(tc);
			break;
		case 5:
			fflush(stdout);
			result = test_max_num_nodeid_entries(tc);
			break;
		case 6:
			fflush(stdout);
			result = test_read_empty_nodeid(tc);
			break;
		default:
			printf("Invalid tc number %d\n", tc);
			print_usage(argv[0]);
			result = -1;
			goto exit;
	}

exit:
	/* Tear down */
	delete_itc();

	if(result) {
		printf("ERROR!\n");
		status = EXIT_FAILURE;
	} else {
		printf("SUCCESS!\n");
	}

	return status;
}
