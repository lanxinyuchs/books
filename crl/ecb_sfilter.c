
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <unistd.h>
#include <errno.h>
#include <sys/stat.h>

#include "ecb_fcs.h"


#define HDLC_I_FORMAT(c) (((c) & 0x01) == 0x00)
#define HDLC_U_FORMAT(c) (((c) & 0x03) == 0x03)
#define HDLC_S_FORMAT(c) (((c) & 0x03) == 0x01)
#define HDLC_NS(c)       (((c) & 0x0e) >> 1)
#define HDLC_NR(c)       (((c) & 0xe0) >> 5)
#define HDLC_U_CR(c)     ((c) & 0xef)
#define HDLC_S_CR(c)     ((c) & 0x0f)
#define HDLC_XID         0xaf
#define HDLC_SNRM        0x83
#define HDLC_DISC        0x43
#define HDLC_UA          0x63
#define HDLC_UI          0x03
#define HDLC_DM          0x0f
#define HDLC_FRMR        0x87
#define HDLC_RR          0x01
#define HDLC_RNR         0x05
#define HDLC_PF          0x10

#define MAX_DATA_SIZE    512
#define MAX_PATH_SIZE    512

struct buffer {
	char     *name;
	uint32_t  size;
	uint8_t   data[MAX_DATA_SIZE];
};


static struct buffer rx_buf = { .name = "RX", .size = 0 };
static struct buffer tx_buf = { .name = "TX", .size = 0 };

static char *device_file = NULL;

static const char syntax[] =
	"Syntax:  %s <device file>\n\n";

static const char purpose[] =
	"Purpose: This program together with strace provide a method for\n"
	"         listening on what SW reads and writes to a specified\n"
	"         device file. The collected octets are scanned for HDLC\n"
	"         communication, made human-readable and then written to\n"
	"         stdout.\n\n";

static const char usage[] =
	"Usage:   This program expects the output from strace to be on a\n"
	"         certain format. Therefore the following strace arguments\n"
	"         are mandatory:\n"
	"         -xx for hexadecimal string format\n"
	"         -s set for maximum frame size to fit in hexadecimal string\n"
	"         -t, -tt or -ttt for time stamps\n"
	"         -f for tracing child processes\n\n"
	"         You may use -e trace=write,read to reduce the CPU load.\n\n"
	"         You may use many -p arguments to trace many processes.\n\n"
	"         The recommended usage is to redirect stderr of strace\n"
	"         to stdout and then use a pipeline to supply stdin of\n"
	"         this program.\n\n"
	"         The ps command should provide PID and device file.\n\n"
	"         You may use 'stdbuf -oL' to avoid buffering stdout.\n\n";

static const char example[] =
	"Example: strace -xx -s 512 -tt -f -p 42 -p 101 -e trace=write,read "
	"2>&1 | %s /dev/ttyS1\n"
	"         strace -xx -s 512 -tt -f -p 42 -e trace=write,read 2>&1 | "
	"stdbuf -oL %s /dev/ttyS1 | grep FCS_GOOD\n\n";


static void parse_hdlc(struct buffer *buf)
{
	uint8_t addr, ctrl;

	addr = buf->data[0];
	ctrl = buf->data[1];

	printf("%u ; ", addr);

	if (HDLC_I_FORMAT(ctrl)) {
		printf("I %u,%u", HDLC_NS(ctrl), HDLC_NR(ctrl));
	} else if (HDLC_U_FORMAT(ctrl)) {
		switch (HDLC_U_CR(ctrl)) {
		case HDLC_XID: printf("XID"); break;
		case HDLC_SNRM: printf("SNRM"); break;
		case HDLC_DISC: printf("DISC"); break;
		case HDLC_DM: printf("DM"); break;
		case HDLC_FRMR: printf("FRMR"); break;
		case HDLC_UA: printf("UA"); break;
		case HDLC_UI: printf("UI"); break;
		default:
			printf("???");
			break;
		}
	} else if (HDLC_S_FORMAT(ctrl)) {
		switch (HDLC_S_CR(ctrl)) {
		case HDLC_RR: printf("RR %u", HDLC_NR(ctrl)); break;
		case HDLC_RNR: printf("RNR %u", HDLC_NR(ctrl)); break;
		default:
			printf("???");
			break;
		}
	} else {
		printf("???");
	}

	if (ctrl & HDLC_PF) {
		printf(" P/F");
	}
}

static void parse_frame(char *ts, struct buffer *buf)
{
	uint32_t src, dst;
	uint16_t fcs;

	/* Name of buffer */
	printf("%s ; ", buf->name);

	/* Time stamp */
	printf("%s ; ", ts);

	/* Octet transparency encoded data */
	for (src = 0; src < buf->size; src++) {
		printf("%02x%c",
		       buf->data[src],
		       ((src + 1) < buf->size) ? '-' : ' ');
	}
	printf("; ");

	/* Octet transparency decoded data */
	src = dst = 0;
	fcs = ECB_FCS_INIT;
	while (src < buf->size && buf->data[src] != 0x7e) {
		if (buf->data[src] == 0x7d) {
			src++;
			if (src >= buf->size) {
				break;
			}
			buf->data[dst] = buf->data[src++] ^ 0x20;
		} else {
			buf->data[dst] = buf->data[src++];
		}
		printf("%02x%c",
		       buf->data[dst],
		       ((src + 1) < buf->size) ? '-' : ' ');
		fcs = ECB_FCS(fcs, buf->data[dst++]);
	}
	buf->size = dst;
	printf("; ");

	/* FCS */
	if (fcs == ECB_FCS_GOOD) {
		printf("FCS_GOOD ; ");
		parse_hdlc(buf);
	} else {
		printf("FCS_BAD ; ;");
	}

	printf("\n");
	buf->size = 0;
}

static void parse_data(char *ptr, char *ts, struct buffer *buf)
{
	while ((ptr = strstr(ptr, "\\x")) != NULL) {

		if (buf->size >= MAX_DATA_SIZE) {
			buf->size = 0;
		}

		*ptr = '0';
		if (sscanf(ptr, "%hhx", &buf->data[buf->size]) != 1) {
			break;
		}
		buf->size++;

		if (buf->data[buf->size - 1] == 0x7e) {
			parse_frame(ts, buf);
		}
	}
}

static int check_fd(int pid, int fd)
{
	char    linkname[100], target[MAX_PATH_SIZE];
	ssize_t size;

	snprintf(linkname, sizeof(linkname), "/proc/%d/fd/%d", pid, fd);

	size = readlink(linkname, target, sizeof(target));
	if (size < 0) {
		fprintf(stderr, "readlink(\"%s\") failed (%d)\n",
			linkname, errno);
		exit(EXIT_FAILURE);
	} else if (size >= MAX_PATH_SIZE) {
		return -1; /* Any string truncated will not match. */
	}

	target[size] = '\0';

	return strcmp(target, device_file);
}

static void parse_read(char *line, char *ptr)
{
	char ts[100];
	int  pid, fd;

	if (sscanf(line, "[pid %d] %100s read(%d", &pid, ts, &fd) == 3 &&
	    check_fd(pid, fd) == 0) {
		parse_data(ptr, ts, &rx_buf);
	}
}

static void parse_write(char *line, char *ptr)
{
	char ts[100];
	int  pid, fd;

	if (sscanf(line, "[pid %d] %100s write(%d", &pid, ts, &fd) == 3 &&
	    check_fd(pid, fd) == 0) {
		parse_data(ptr, ts, &tx_buf);
	}
}

static void parse_line(char *line)
{
	char *ptr;

	if ((ptr = strstr(line, " read(")) != NULL) {
		parse_read(line, ptr);
	} else if ((ptr = strstr(line, " write(")) != NULL) {
		parse_write(line, ptr);
	}
}

static void check_device_file(char *path)
{
	struct stat st_buf;

	if (stat(path, &st_buf) == -1) {
		fprintf(stderr, "Could not find \"%s\"\n", path);
		exit(EXIT_FAILURE);
	} else if (!S_ISCHR(st_buf.st_mode)) {
		fprintf(stderr, "File \"%s\" is not a device file\n", path);
		exit(EXIT_FAILURE);
	}

	device_file = path;
}

int main(int argc, char *argv[])
{
	char   *line = NULL;
	size_t  size;

	if (argc != 2 || strcmp(argv[1], "--help") == 0) {
		fprintf(stderr,	syntax, argv[0]);
		fprintf(stderr, purpose);
		fprintf(stderr, usage);
		fprintf(stderr, example, argv[0], argv[0]);
		exit(EXIT_FAILURE);
	} else if (strlen(argv[1]) >= MAX_PATH_SIZE) {
		fprintf(stderr, "Too long path for device file");
		exit(EXIT_FAILURE);
	}

	check_device_file(argv[1]);
	while ((getline(&line, &size, stdin)) != -1) {
		parse_line(line);
	}

	return 0;
}
