/* ---------------------------------------------------------------------------
 *
 * © Ericsson AB 2015 All rights reserved.
 * The information in this document is the property of Ericsson.  Except
 * as specifically authorized in writing by Ericsson, the receiver of
 * this document shall keep the information contained herein confidential
 * and shall protect the same in whole or in part from disclosure and
 * dissemination to third parties.  Disclosure and dissemination to the
 * receivers employees shall only be made on a strict need to know basis.
 *
 * ---------------------------------------------------------------------------
 */
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "client.h"

#define TRACEPOINT_PROVIDER com_ericsson_xcs_trace_test
#include "tpt_create.h" /* This creates the tracepoints, should only be done once for each tracepoint provider */
#include "tpt.h"

static void setup_lttng_session(void)
{
	char *commands_setup =
			"lttng -n create s101 --snapshot -o /tmp/s101_lttng-traces;"
			"lttng -n enable-event -u com_ericsson_xcs_trace_test*;"
			"lttng -n start s101";
	FILE *output = popen(commands_setup, "r");
	if (output != NULL)  {
		pclose(output);
	} else {
		printf("popen failed sending (%s)", commands_setup);
	}
}

static void destroy_lttng_session(void)
{
	char *commands_destroy = "lttng -n destroy s101";
	FILE *output= popen(commands_destroy, "r");
	if (output != NULL)  {
		pclose(output);
	}
}

static int get_lttng_trace(char *check)
{
	FILE *output;
	int retval = -1;
	char buffer[1024];
	char *commands_record = "lttng snapshot record -s s101";
	char *commands_view   = "babeltrace /tmp/s101_lttng-traces/*";
	char *commands_clear  = "rm -rf /tmp/s101_lttng-traces/";

	output = popen(commands_record, "r");
	if (output != NULL)  {
		pclose(output);
	} else {
		printf("popen failed sending (%s)", commands_record);
	}

	output = popen(commands_view, "r");
	if (output == NULL)  {
		printf("popen failed sending (%s)", commands_view);
		return -1;
	}
	while(fgets(buffer, sizeof(buffer), output) != NULL){
		printf("\t  [%s] %s", check == NULL ? "NULL" : check, buffer);
		if(check != NULL) {
			if(strstr(buffer, check) != NULL) {
				retval = 0;
				break;
			}
		}
	}

	pclose(output);

	output = popen(commands_clear, "r");
	if (output != NULL)  {
		pclose(output);
	} else {
		printf("popen failed sending (%s)", commands_clear);
	}

	return retval;
}

static int error_trace_macros(int test_no)
{
	int sub_test = 0;

	setup_lttng_session();
	printf("\t%d.%d: TRACE ERROR\n", test_no, sub_test++);
	TPT_ERROR("trace error message");
	if (get_lttng_trace(":ERROR") == -1) {
		destroy_lttng_session();
		return -1;
	}
	destroy_lttng_session();

	return 0;
}

static int info_trace_macros(int test_no)
{
	int sub_test = 0;

	setup_lttng_session();
	printf("\t%d.%d: TPT_INFO\n", test_no, sub_test++);
	TPT_INFO("trace info message");
	if (get_lttng_trace(":INFO") == -1) {
		destroy_lttng_session();
		return -1;
	}
	destroy_lttng_session();

	return 0;
}

static int state_trace_macros(int test_no)
{
	int sub_test = 0;

	setup_lttng_session();
	printf("\t%d.%d: TPT_STATE\n", test_no, sub_test++);
	TPT_STATE("trace state message");
	if (get_lttng_trace(":STATE_CHANGE") == -1) {
		destroy_lttng_session();
		return -1;
	}
	destroy_lttng_session();

	setup_lttng_session();
	printf("\t%d.%d: TPT_OBJ_STATE\n", test_no, sub_test++);
	TPT_OBJ_STATE("myObj", "trace state message");
	if (get_lttng_trace(":STATE_CHANGE") == -1) {
		destroy_lttng_session();
		return -1;
	}
	if (get_lttng_trace("myObj") == -1) {
		destroy_lttng_session();
		return -1;
	}
	destroy_lttng_session();

	return 0;
}

static int tracen_trace_macros(int test_no)
{
	int sub_test = 0;

	/* Sadly, we need to duplicate code here as I can not give
	 * a iterator to the TRACE macro since it expands the number
	 * as a name...
	 */

	setup_lttng_session();
	printf("\t%d.%d: TRACE1\n", test_no, sub_test++);
	TPT_TRACE(1,"trace message");
	if (get_lttng_trace(":TRACE1") == -1) {
		destroy_lttng_session();
		return -1;
	}
	destroy_lttng_session();
	setup_lttng_session();
	printf("\t%d.%d: TPT_OBJ1\n", test_no, sub_test++);
	TPT_TRACE_OBJ(1, "myObj", "trace message");
	if (get_lttng_trace(":TRACE1") == -1) {
		destroy_lttng_session();
		return -1;
	}
	if (get_lttng_trace("myObj") == -1) {
		destroy_lttng_session();
		return -1;
	}
	destroy_lttng_session();


	setup_lttng_session();
	printf("\t%d.%d: TRACE2\n", test_no, sub_test++);
	TPT_TRACE(2,"trace message");
	if (get_lttng_trace(":TRACE2") == -1) {
		destroy_lttng_session();
		return -1;
	}
	destroy_lttng_session();
	setup_lttng_session();
	printf("\t%d.%d: TPT_OBJ2\n", test_no, sub_test++);
	TPT_TRACE_OBJ(2, "myObj", "trace message");
	if (get_lttng_trace(":TRACE2") == -1) {
		destroy_lttng_session();
		return -1;
	}
	if (get_lttng_trace("myObj") == -1) {
		destroy_lttng_session();
		return -1;
	}
	destroy_lttng_session();

	setup_lttng_session();
	printf("\t%d.%d: TRACE3\n", test_no, sub_test++);
	TPT_TRACE(3,"trace message");
	if (get_lttng_trace(":TRACE3") == -1) {
		destroy_lttng_session();
		return -1;
	}
	destroy_lttng_session();
	setup_lttng_session();
	printf("\t%d.%d: TPT_OBJ3\n", test_no, sub_test++);
	TPT_TRACE_OBJ(3, "myObj", "trace message");
	if (get_lttng_trace(":TRACE3") == -1) {
		destroy_lttng_session();
		return -1;
	}
	if (get_lttng_trace("myObj") == -1) {
		destroy_lttng_session();
		return -1;
	}
	destroy_lttng_session();

	setup_lttng_session();
	printf("\t%d.%d: TRACE4\n", test_no, sub_test++);
	TPT_TRACE(4,"trace message");
	if (get_lttng_trace(":TRACE4") == -1) {
		destroy_lttng_session();
		return -1;
	}
	destroy_lttng_session();
	setup_lttng_session();
	printf("\t%d.%d: TPT_OBJ4\n", test_no, sub_test++);
	TPT_TRACE_OBJ(4, "myObj", "trace message");
	if (get_lttng_trace(":TRACE4") == -1) {
		destroy_lttng_session();
		return -1;
	}
	if (get_lttng_trace("myObj") == -1) {
		destroy_lttng_session();
		return -1;
	}
	destroy_lttng_session();

	setup_lttng_session();
	printf("\t%d.%d: TRACE5\n", test_no, sub_test++);
	TPT_TRACE(5,"trace message");
	if (get_lttng_trace(":TRACE5") == -1) {
		destroy_lttng_session();
		return -1;
	}
	destroy_lttng_session();
	setup_lttng_session();
	printf("\t%d.%d: TPT_OBJ5\n", test_no, sub_test++);
	TPT_TRACE_OBJ(5, "myObj", "trace message");
	if (get_lttng_trace(":TRACE5") == -1) {
		destroy_lttng_session();
		return -1;
	}
	if (get_lttng_trace("myObj") == -1) {
		destroy_lttng_session();
		return -1;
	}
	destroy_lttng_session();

	setup_lttng_session();
	printf("\t%d.%d: TRACE6\n", test_no, sub_test++);
	TPT_TRACE(6,"trace message");
	if (get_lttng_trace(":TRACE6") == -1) {
		destroy_lttng_session();
		return -1;
	}
	destroy_lttng_session();
	setup_lttng_session();
	printf("\t%d.%d: TPT_OBJ6\n", test_no, sub_test++);
	TPT_TRACE_OBJ(6, "myObj", "trace message");
	if (get_lttng_trace(":TRACE6") == -1) {
		destroy_lttng_session();
		return -1;
	}
	if (get_lttng_trace("myObj") == -1) {
		destroy_lttng_session();
		return -1;
	}
	destroy_lttng_session();

	setup_lttng_session();
	printf("\t%d.%d: TRACE7\n", test_no, sub_test++);
	TPT_TRACE(7,"trace message");
	if (get_lttng_trace(":TRACE7") == -1) {
		destroy_lttng_session();
		return -1;
	}
	destroy_lttng_session();
	setup_lttng_session();
	printf("\t%d.%d: TPT_OBJ7\n", test_no, sub_test++);
	TPT_TRACE_OBJ(7, "myObj", "trace message");
	if (get_lttng_trace(":TRACE7") == -1) {
		destroy_lttng_session();
		return -1;
	}
	if (get_lttng_trace("myObj") == -1) {
		destroy_lttng_session();
		return -1;
	}
	destroy_lttng_session();

	return 0;
}

static int sig_trace_macros(int test_no)
{
	int sub_test = 0;
	unsigned int sig_no = 0x100b;
	unsigned int pid = 0xffffffff;

	setup_lttng_session();
	printf("\t%d.%d: TPT_REC_SIG\n", test_no, sub_test++);
	TPT_REC_SIG(sig_no,"trace receive sinal message");
	if (get_lttng_trace(":REC_SIG") == -1) {
		destroy_lttng_session();
		return -1;
	}
	destroy_lttng_session();
	setup_lttng_session();
	printf("\t%d.%d: TPT_OBJ_REC_SIG\n", test_no, sub_test++);
	TPT_OBJ_REC_SIG("myObj", sig_no, "trace receive signal message");
	if (get_lttng_trace(":REC_SIG") == -1) {
		destroy_lttng_session();
		return -1;
	}
	if (get_lttng_trace("myObj") == -1) {
		destroy_lttng_session();
		return -1;
	}
	destroy_lttng_session();

	setup_lttng_session();
	printf("\t%d.%d: TPT_SEND_SIG\n", test_no, sub_test++);
	TPT_SEND_SIG(sig_no, pid, "trace send sinal message");
	if (get_lttng_trace(":SEND_SIG") == -1) {
		destroy_lttng_session();
		return -1;
	}
	destroy_lttng_session();
	setup_lttng_session();
	printf("\t%d.%d: TPT_OBJ_SEND_SIG\n", test_no, sub_test++);
	TPT_OBJ_SEND_SIG("myObj", sig_no, pid, "trace send sinal message");
	if (get_lttng_trace(":SEND_SIG") == -1) {
		destroy_lttng_session();
		return -1;
	}
	if (get_lttng_trace("myObj") == -1) {
		destroy_lttng_session();
		return -1;
	}
	destroy_lttng_session();

	return 0;
}

static int data_trace_macros(int test_no)
{
	int sub_test = 0;
	char data[16] = {0};

	setup_lttng_session();
	printf("\t%d.%d: TPT_DATA\n", test_no, sub_test++);
	TPT_DATA("trace data message", data, sizeof(data));
	if (get_lttng_trace(":DATA") == -1) {
		destroy_lttng_session();
		return -1;
	}
	destroy_lttng_session();
	setup_lttng_session();
	printf("\t%d.%d: TPT_OBJ_DATA\n", test_no, sub_test++);
	TPT_OBJ_DATA("myObj", "trace data message", data, sizeof(data));
	if (get_lttng_trace(":DATA") == -1) {
		destroy_lttng_session();
		return -1;
	}
	if (get_lttng_trace("myObj") == -1) {
		destroy_lttng_session();
		return -1;
	}
	destroy_lttng_session();

	return 0;
}

static int uts_trace_macros(int test_no)
{
	int sub_test = 0;
	char data[16] = {0};

	setup_lttng_session();
	printf("\t%d.%d: TPT_UTS1\n", test_no, sub_test++);
	TPT_UTS(1, 1000, 101, "file.c", 110, "proc", "trace uts message", data, sizeof(data));
	if (get_lttng_trace(":TRACE_UTS1") == -1) {
		destroy_lttng_session();
		return -1;
	}
	destroy_lttng_session();

	setup_lttng_session();
	printf("\t%d.%d: TPT_UTS2\n", test_no, sub_test++);
	TPT_UTS(2, 1000, 101, "file.c", 110, "proc", "trace uts message", data, sizeof(data));
	if (get_lttng_trace(":TRACE_UTS2") == -1) {
		destroy_lttng_session();
		return -1;
	}
	destroy_lttng_session();

	setup_lttng_session();
	printf("\t%d.%d: TPT_UTS3\n", test_no, sub_test++);
	TPT_UTS(3, 1000, 101, "file.c", 110, "proc", "trace uts message", data, sizeof(data));
	if (get_lttng_trace(":TRACE_UTS3") == -1) {
		destroy_lttng_session();
		return -1;
	}
	destroy_lttng_session();

	setup_lttng_session();
	printf("\t%d.%d: TPT_UTS4\n", test_no, sub_test++);
	TPT_UTS(4, 1000, 101, "file.c", 110, "proc", "trace uts message", data, sizeof(data));
	if (get_lttng_trace(":TRACE_UTS4") == -1) {
		destroy_lttng_session();
		return -1;
	}
	destroy_lttng_session();

	setup_lttng_session();
	printf("\t%d.%d: TPT_UTS5\n", test_no, sub_test++);
	TPT_UTS(5, 1000, 101, "file.c", 110, "proc", "trace uts message", data, sizeof(data));
	if (get_lttng_trace(":TRACE_UTS5") == -1) {
		destroy_lttng_session();
		return -1;
	}
	destroy_lttng_session();

	setup_lttng_session();
	printf("\t%d.%d: TPT_UTS6\n", test_no, sub_test++);
	TPT_UTS(6, 1000, 101, "file.c", 110, "proc", "trace uts message", data, sizeof(data));
	if (get_lttng_trace(":TRACE_UTS6") == -1) {
		destroy_lttng_session();
		return -1;
	}
	destroy_lttng_session();

	setup_lttng_session();
	printf("\t%d.%d: TPT_UTS7\n", test_no, sub_test++);
	TPT_UTS(7, 1000, 101, "file.c", 110, "proc", "TPT_UTS", data, sizeof(data));
	if (get_lttng_trace(":TRACE_UTS7") == -1) {
		destroy_lttng_session();
		return -1;
	}
	destroy_lttng_session();

	return 0;
}

static int str_macro(int test_no)
{
	int sub_test = 0;
	char *str_null = NULL;
	char *long_message =
			"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
			"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
			"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
			"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
			"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
			"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
			"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
			"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
			"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
			"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
			"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
			"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
			"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
			"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
			"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
			"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
			"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
			"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
			"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
			"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
			"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
			"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
			"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa";

	printf("\t%d.%d: Too long message for STR2() (%d char)\n", test_no, sub_test++, strlen(long_message));
	setup_lttng_session();
	TPT_ERROR(STR2("%s",long_message));
	if (get_lttng_trace(":ERROR") == -1) {
		destroy_lttng_session();
		return -1;
	}
	destroy_lttng_session();

	printf("\t%d.%d: Test x and X\n", test_no, sub_test++);
	setup_lttng_session();
	TPT_ERROR(STR2(" 0x%x 0x%X", 10 , 10));
	if (get_lttng_trace("0xa") == -1) {
		destroy_lttng_session();
		return -1;
	}
	if (get_lttng_trace("0xA") == -1) {
		destroy_lttng_session();
		return -1;
	}
	destroy_lttng_session();

	printf("\t%d.%d: Test u and d\n", test_no, sub_test++);
	setup_lttng_session();
	TPT_ERROR(STR2("%u %d", 10 , -10));
	if (get_lttng_trace("10") == -1) {
		destroy_lttng_session();
		return -1;
	}
	if (get_lttng_trace("-10") == -1) {
		destroy_lttng_session();
		return -1;
	}
	destroy_lttng_session();

	printf("\t%d.%d: Test f 10.10\n", test_no, sub_test++);
	setup_lttng_session();
	TPT_ERROR(STR2("%f", 10.10));
	if (get_lttng_trace("10.100000") == -1) {
		destroy_lttng_session();
		return -1;
	}
	destroy_lttng_session();

	printf("\t%d.%d: Test c\n", test_no, sub_test++);
	setup_lttng_session();
	TPT_ERROR(STR2("%c", 'Q'));
	if (get_lttng_trace("Q") == -1) {
		destroy_lttng_session();
		return -1;
	}
	destroy_lttng_session();

	printf("\t%d.%d: Test precision 4 of 12.3456789\n", test_no, sub_test++);
	setup_lttng_session();
	TPT_ERROR(STR2("%.4f", 12.3456789));
	if (get_lttng_trace("12.3457") == -1) {
		destroy_lttng_session();
		return -1;
	}
	destroy_lttng_session();

	printf("\t%d.%d: Test null to s\n", test_no, sub_test++);
	setup_lttng_session();
	TPT_ERROR(STR2("%s", str_null));
	if (get_lttng_trace("(null)") == -1) {
		destroy_lttng_session();
		return -1;
	}
	destroy_lttng_session();

	printf("\t%d.%d: Test %%s\n", test_no, sub_test++);
	setup_lttng_session();
	TPT_ERROR(STR2("%%s"));
	if (get_lttng_trace("%s") == -1) {
		destroy_lttng_session();
		return -1;
	}
	destroy_lttng_session();

	printf("\t%d.%d: Test no precision given for 10.5 and 10.4\n", test_no, sub_test++);
	setup_lttng_session();
	TPT_ERROR(STR2("%.f", 10.5));
	/* snprintf does not round this up... */
	if (get_lttng_trace("11") == -1) {
		destroy_lttng_session();
		return -1;
	}
	destroy_lttng_session();
	setup_lttng_session();
	TPT_ERROR(STR2("%.f", 10.4));
	if (get_lttng_trace("10") == -1) {
		destroy_lttng_session();
		return -1;
	}
	destroy_lttng_session();

	printf("\t%d.%d: Test width\n", test_no, sub_test++);
	setup_lttng_session();
	TPT_ERROR(STR2("%10s %3c", "happy", 'q'));
	if (get_lttng_trace("     happy") == -1) {
		destroy_lttng_session();
		return -1;
	}
	if (get_lttng_trace("  q") == -1) {
		destroy_lttng_session();
		return -1;
	}
	destroy_lttng_session();
	setup_lttng_session();
	TPT_ERROR(STR2("%04d 0x%08x", 10, 11));
	if (get_lttng_trace("0010") == -1) {
		destroy_lttng_session();
		return -1;
	}
	if (get_lttng_trace("0x0000000b") == -1) {
		destroy_lttng_session();
		return -1;
	}
	destroy_lttng_session();

	return 0;
}

static int client_server(int test_no)
{
	int sub_test = 0;

	printf("\t%d.%d: Test send one message\n", test_no, sub_test++);
	setup_lttng_session();
	TPT_INFO("send_one_message");
	send_one_message();
	TPT_INFO("send_one_message done");

	TPT_INFO("send_two_message");
	send_two_message();
	TPT_INFO("send_two_message done");

	if (get_lttng_trace("send_one_message") == -1) {
		/* If this is not found it might be the compiler bug that is
		 * back again. See:
		 * https://gcc.gnu.org/ml/gcc-help/2014-05/msg00028.html
		 * https://lists.lttng.org/pipermail/lttng-dev/2014-June/023167.html
		 */
		destroy_lttng_session();
		return -1;
	}
	if (get_lttng_trace("TPT_TEST_ONE_REQ") == -1) {
		destroy_lttng_session();
		return -1;
	}
	if (get_lttng_trace("TPT_TEST_TWO_REQ") == -1) {
		destroy_lttng_session();
		return -1;
	}

	destroy_lttng_session();
	return 0;
}

static void print_usage(char *pgm)
{
	printf("Usage: %s -tc <test case number>\n\n"
	       "\twhere <test case number> is one of:\n"
	       "\t1: Test ERROR trace macros\n"
	       "\t2: Test INFO trace macros\n"
	       "\t3: Test STATE trace macros\n"
	       "\t4: Test TRACE<n> trace macros\n"
	       "\t5: Test SEND_SIG/REC_SIG trace macros\n"
	       "\t6: Test DATA trace macros\n"
	       "\t7: Test UTS trace macros\n"
	       "\t8: Test STR macro\n"
	       "\t9: Test client/server\n",
	       pgm);
}

int main(int argc, char **argv)
{
	int tc = 0;
	int result = 0;
	int status = EXIT_SUCCESS;
	(void)argc; (void)argv;
	TPT_INIT();

	if (argc < 3) {
		print_usage(argv[0]);
		exit(-1);
	}

	if (argc >= 3) {
		if (strstr(argv[1], "-tc") != NULL) {
			tc = atoi(argv[2]);
		}
	}

	switch (tc) {
	case 1:
		printf("%d Test ERROR trace macros:\n", tc);
		fflush(stdout);
		result = error_trace_macros(tc);
		break;
	case 2:
		printf("%d Test INFO trace macros:\n", tc);
		fflush(stdout);
		result = info_trace_macros(tc);
		break;
	case 3:
		printf("%d Test STATE trace macros:\n", tc);
		fflush(stdout);
		result = state_trace_macros(tc);
		break;
	case 4:
		printf("%d Test TRACE<n> trace macros:\n", tc);
		fflush(stdout);
		result = tracen_trace_macros(tc);
		break;
	case 5:
		printf("%d Test SEND_SIG/REC_SIG trace macros:\n", tc);
		fflush(stdout);
		result = sig_trace_macros(tc);
		break;
	case 6:
		printf("%d Test DATA trace macros:\n", tc);
		fflush(stdout);
		result = data_trace_macros(tc);
		break;
	case 7:
		printf("%d Test UTS trace macros:\n", tc);
		fflush(stdout);
		result = uts_trace_macros(tc);
		break;
	case 8:
		printf("%d Test STR macro:\n", tc);
		fflush(stdout);
		result = str_macro(tc);
		break;
	case 9:
		printf("%d Test client/server:\n", tc);
		fflush(stdout);
		result = client_server(tc);
		break;
	default:
		printf("Invalid tc number %d\n", tc);
		print_usage(argv[0]);
		result = -1;
		break;
	}

	if(result) {
		printf("ERROR!\n");
		status = EXIT_FAILURE;
	} else {
		printf("SUCCESS!\n");
	}

	return status;
}
