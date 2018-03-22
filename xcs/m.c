#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include <getopt.h>
#include <libgen.h>
#include <ctype.h>
#include <limits.h>
#include <stdbool.h>

union value {
	int8_t int8;
	int16_t int16;
	int32_t int32;
	int64_t int64;
	uint8_t uint8;
	uint16_t uint16;
	uint32_t uint32;
	uint64_t uint64;
};

typedef enum {
	OPERATION_READ = 0,
	OPERATION_WRITE,
	OPERATION_BITSET,
	OPERATION_BITCLEAR,
	OPERATION_MASKSET,
	OPERATION_MASKCLEAR,
} operation_t;

struct memory {
	uintptr_t phys_addr;
	void *virt_addr;
	void *pages_start;
	size_t pages_size;
};

struct args {
	uintptr_t address;
	long int count;
	int size_in_bytes;
	operation_t operation;
	bool sign;
	unsigned int bit;
	uint64_t mask;
	bool quiet;
	union value v;
};

void usage()
{
	printf("Usage: m [options]\n"
	       "    -a|--address   The address in hex to do operation on [required].\n"
	       "    -o|--operation The operation, either 'write', 'read', 'bitset', 'bitclear',\n"
	       "                   'maskset' or 'maskclear'. Default is 'read'.\n"
	       "    -c|--count     The number of units to operate on [default: 1].\n"
	       "    -s|--size      The number of bytes per unit, either 1, 2, 4 or 8.\n"
	       "                   Default is '4'.\n"
	       "    -u|--uvalue    The unsigned value to write (start with 0x for hex).\n"
	       "    -i|--ivalue    The signed value to write (start with 0x for hex).\n"
	       "    -v|--bvalue    The binary value to write.\n"
	       "    -b|--bit       The bit to set for 'bitset' operation, or clear\n"
	       "                   for 'bitclear' operation, where 0 is LSB [default: 0].\n"
	       "    -m|--mask      The mask to set for 'maskset' operation, or clear\n"
	       "                   for 'maskclear' operation (in hex).\n"
	       "    -q|--quiet     Quiet operation. No unnecessary print outs or\n"
	       "                   formatting. Useful for scripting.\n"
	       "    -h|--help      Show this text.\n\n");
}

void usage_md()
{
	printf("Usage: md[.b .w .l .q] <address in hex> [count]\n"
	       "    This command reads memory at a specified address\n"
	       "    The suffix to md, e.g. md.b means:\n"
	       "        .b = bytes (8 bits)\n"
	       "        .w = word  (16 bits)\n"
	       "        .l = long  (32 bits)\n"
	       "        .q = quad  (64 bits)\n\n");
}

void usage_mw()
{
	printf("Usage: mw[.b .w .l .q] <address in hex> <value> [count]\n"
	       "    This command writes a value to  memory at a specified address\n"
	       "    If the value starts with 0x it will be interpreted as hexadecimal,\n"
	       "    otherwise decimal\n"
	       "    The suffix to md, e.g. md.b means:\n"
	       "        .b = bytes (8 bits)\n"
	       "        .w = word  (16 bits)\n"
	       "        .l = long  (32 bits)\n"
	       "        .q = quad  (64 bits)\n\n");
}

int parse_args(int argc, char *argv[], struct args *a)
{
	static char short_options[] = "hqo:u:i:v:b:m:a:c:s:";
	static struct option long_options[] = {
		{ "help",       no_argument,       NULL, 'h' },
		{ "quiet",      no_argument,       NULL, 'q' },
		{ "operation",  required_argument, NULL, 'o' },
		{ "uvalue",     required_argument, NULL, 'u' },
		{ "ivalue",     required_argument, NULL, 'i' },
		{ "bvalue",     required_argument, NULL, 'v' },
		{ "bit",        required_argument, NULL, 'b' },
		{ "mask",       required_argument, NULL, 'm' },
		{ "address",    required_argument, NULL, 'a' },
		{ "count",      required_argument, NULL, 'c' },
		{ "size",       required_argument, NULL, 's' },
		{ 0, 0, 0, 0 }
	};
	int argid, index;
	char *endptr;

	for (;;) {
		argid = getopt_long(argc, argv, short_options, long_options, &index);

		if (argid == -1)
			break;

		switch (argid) {
			case 'h':
				usage();
				exit(EXIT_SUCCESS);

			case 'q':
				a->quiet = true;
				break;

			case 'o':
				if (strcmp(optarg, "write") == 0)
					a->operation = OPERATION_WRITE;
				else if (strcmp(optarg, "read") == 0)
					a->operation = OPERATION_READ;
				else if (strcmp(optarg, "bitset") == 0)
					a->operation = OPERATION_BITSET;
				else if (strcmp(optarg, "bitclear") == 0)
					a->operation = OPERATION_BITCLEAR;
				else if (strcmp(optarg, "maskset") == 0)
					a->operation = OPERATION_MASKSET;
				else if (strcmp(optarg, "maskclear") == 0)
					a->operation = OPERATION_MASKCLEAR;
				else {
					fprintf(stderr, "Unknown operation %s\n", optarg);
					return -1;
				}
				break;

			case 'b':
				a->bit = strtoul(optarg, &endptr, 0);
				if (*endptr != '\0') {
					fprintf(stderr, "Invalid bit %s\n", optarg);
					usage();
					return -1;
				}
				break;

			case 'm':
				a->mask = strtoull(optarg, &endptr, 16);
				if (*endptr != '\0') {
					fprintf(stderr, "Invalid mask %s\n", optarg);
					usage();
					return -1;
				}
				break;

			case 'u':
				a->v.uint64 = strtoull(optarg, &endptr, 0);
				if (*endptr != '\0') {
					fprintf(stderr, "Invalid value %s\n", optarg);
					usage();
					return -1;
				}
				a->sign = false;
				break;

			case 'i':
				a->v.int64 = strtoll(optarg, &endptr, 0);
				if (*endptr != '\0') {
					fprintf(stderr, "Invalid value %s\n", optarg);
					usage();
					return -1;
				}
				a->sign = true;
				break;

			case 'v':
				a->v.uint64 = strtoull(optarg, &endptr, 2);
				if (*endptr != '\0') {
					fprintf(stderr, "Invalid value %s\n", optarg);
					usage();
					return -1;
				}
				a->sign = false;
				break;

			case 'a':
				a->address = strtoul(optarg, &endptr, 16);
				if (*endptr != '\0') {
					fprintf(stderr, "Invalid address %s\n", optarg);
					usage();
					return -1;
				}
				break;

			case 'c':
				a->count = strtoul(optarg, &endptr, 10);
				if (*endptr != '\0') {
					fprintf(stderr, "Invalid count %s\n", optarg);
					usage();
					return -1;
				}
				break;

			case 's':
				a->size_in_bytes = strtoul(optarg, &endptr, 10);
				if (*endptr != '\0') {
					fprintf(stderr, "Invalid size %s\n", optarg);
					usage();
					return -1;
				}
				break;

			default:
				usage();
				return -1;
		}
	}

	if (!a->address) {
		fprintf(stderr, "Address is NULL, not supplied?\n");
		usage();
		return -1;
	}

	if ((a->operation == OPERATION_MASKSET ||
		a->operation == OPERATION_MASKCLEAR) && !a->mask) {
		fprintf(stderr, "No mask supplied!\n");
		usage();
	}

	return 0;
}

int parse_md(int argc, char *argv[], struct args *a)
{
	char *cmd = basename(argv[0]);
	char *endptr;

	a->operation = OPERATION_READ;
	if (strcmp(cmd, "md.b") == 0)
		a->size_in_bytes = sizeof(uint8_t);
	else if (strcmp(cmd, "md.w") == 0)
		a->size_in_bytes = sizeof(uint16_t);
	else if (strcmp(cmd, "md.l") == 0)
		a->size_in_bytes = sizeof(uint32_t);
	else if (strcmp(cmd, "md.q") == 0)
		a->size_in_bytes = sizeof(uint64_t);
	else {
		usage_md();
		return 1;
	}

	if (argc < 2) {
		usage_md();
		return 1;
	}

	a->address = (uintptr_t) strtoul(argv[1], &endptr, 16);
	if (*endptr != '\0') {
		fprintf(stderr, "Invalid address %s\n", argv[1]);
		usage_md();
		return 1;
	}

	if (argc > 2) {
		a->count = strtoul(argv[2], &endptr, 10);
		if (*endptr != '\0') {
			fprintf(stderr, "Invalid count %s\n", argv[2]);
			usage_md();
			return 1;
		}
	}

	return 0;
}

int parse_mw(int argc, char *argv[], struct args *a)
{
	char *cmd = basename(argv[0]);
	char *endptr;

	a->operation = OPERATION_WRITE;
	if (strcmp(cmd, "mw.b") == 0)
		a->size_in_bytes = sizeof(uint8_t);
	else if (strcmp(cmd, "mw.w") == 0)
		a->size_in_bytes = sizeof(uint16_t);
	else if (strcmp(cmd, "mw.l") == 0)
		a->size_in_bytes = sizeof(uint32_t);
	else if (strcmp(cmd, "mw.q") == 0)
		a->size_in_bytes = sizeof(uint64_t);
	else {
		usage_md();
		return 1;
	}

	if (argc < 3) {
		usage_md();
		return 1;
	}

	a->address = (uintptr_t) strtoul(argv[1], &endptr, 16);
	if (*endptr != '\0') {
		fprintf(stderr, "Invalid address %s\n", argv[1]);
		usage_md();
		return 1;
	}

	a->v.uint64 = strtoull(argv[2], &endptr, 0);
	if (*endptr != '\0') {
		fprintf(stderr, "Invalid value %s\n", argv[1]);
		usage_md();
		return 1;
	}

	if (argc > 3) {
		a->count = strtoul(argv[3], &endptr, 10);
		if (*endptr != '\0') {
			fprintf(stderr, "Invalid count %s\n", argv[2]);
			usage_md();
			return 1;
		}
	}

	return 0;
}

int map_address(struct memory *m, operation_t operation, size_t size)
{
	uintptr_t phys_page;
	long page_size;
	int fd = -1;
	int open_flags = O_SYNC;
	int mmap_flags = 0;
	int status = -1;

	page_size = sysconf(_SC_PAGE_SIZE);
	if (page_size == -1) {
		fprintf(stderr, "Failed to get page size (%s)\n", strerror(errno));
		goto exit;
	}

	phys_page = m->phys_addr & ~(page_size - 1);
	m->pages_size = size + m->phys_addr - phys_page;

	if (operation == OPERATION_READ) {
		open_flags |= O_RDONLY;
		mmap_flags |= PROT_READ;
	}
	else if (operation == OPERATION_WRITE) {
		open_flags |= O_RDWR;
		mmap_flags |= PROT_WRITE;
	}
	else {
		open_flags |= O_RDWR;
		mmap_flags |= PROT_READ | PROT_WRITE;
	}

	fd = open("/dev/mem", open_flags);
	if (fd == -1) {
		fprintf(stderr, "Failed to open /dev/mem (%s)\n", strerror(errno));
		goto exit;
	}

	m->pages_start = mmap(NULL, m->pages_size, mmap_flags, MAP_SHARED,
	                      fd, phys_page);
	if (m->pages_start == MAP_FAILED) {
		fprintf(stderr, "Failed to mmap page (%s)\n", strerror(errno));
		goto exit;
	}

	m->virt_addr = m->pages_start + m->phys_addr - phys_page;
	status = 0;

exit:
	if (fd >= 0) {
		if (close(fd))
			status = -1;
	}

	return status;
}

void printascii(char *ptr, int count)
{
	char c;

	putchar('|');
	while (count--) {
		c = *ptr++;
		if (isprint(c))
			putchar(c);
		else
			putchar('.');
	}
	putchar('|');
}

uintptr_t getphys(struct memory *m, void *ptr)
{
	return m->phys_addr + (uintptr_t) (ptr - m->virt_addr);
}

void print_address(void *ptr, struct memory *m)
{
	uintptr_t addr = getphys(m, ptr);
#if (__WORDSIZE == 64)
	printf("%016x:  ", addr);
#else
	printf("%08x:  ", addr);
#endif
}

void printu8(struct memory *m, int count)
{
	int i = 0;
	int j = 0;
	int len = 0;
	uint8_t *ptr = m->virt_addr;
	uint8_t *ptr2 = m->virt_addr;

	while (count--) {
		if (len == 0)
			print_address(ptr, m);

		printf("%02x ", *ptr++);
		len++;

		if (i++ == 7) {
			putchar(' ');
			i = 0;

			if (j++ == 1) {
				printascii((char *) ptr2, len);
				putchar('\n');
				j = 0;
				len = 0;
				ptr2 = ptr;
			}
		}
	}

	if (len) {
		int num;
		if (len < 8) {
			for (i = len; i < 8; i++) {
				for (j = 0; j < 3; j++)
					putchar(' ');
			}
			putchar(' ');
			num = 0;
		}
		else
			num = len - 8;

		for (i = num; i < 8; i++) {
			for (j = 0; j < 3; j++)
				putchar(' ');
		}
		putchar(' ');

		printascii((char *) ptr2, len);
		putchar('\n');
	}
}

void printu16(struct memory *m, int count)
{
	int len = 0;
	uint16_t *ptr = m->virt_addr;

	while (count--) {
		if (len == 0)
			print_address(ptr, m);

		printf("%04x  ", *ptr++);

		if (len++ == 7) {
			putchar('\n');
			len = 0;
		}
	}

	if (len)
		putchar('\n');
}

void printu32(struct memory *m, int count)
{
	int len = 0;
	uint32_t *ptr = m->virt_addr;

	while (count--) {
		if (len == 0)
			print_address(ptr, m);

		printf("%08x  ", *ptr++);

		if (len++ == 7) {
			putchar('\n');
			len = 0;
		}
	}

	if (len)
		putchar('\n');
}

void printu64(struct memory *m, int count)
{
	int len = 0;
	uint64_t *ptr = m->virt_addr;

	while (count--) {
		if (len == 0)
			print_address(ptr, m);

		printf("%016llx  ", *ptr++);

		if (len++ == 3) {
			putchar('\n');
			len = 0;
		}
	}

	if (len)
		putchar('\n');
}

#define READ_LOOP(count, type, address) do {                          \
	type##_t *ptr = (address);                                        \
	while(count--)                                                    \
		printf("%0*llx ", sizeof(type##_t) * 2, (uint64_t) *ptr++);   \
} while(0)

void read_mem(struct memory *m, int size_in_bytes, int count, bool quiet)
{
	switch (size_in_bytes) {
	case sizeof(uint8_t):
		if (quiet)
			READ_LOOP(count, uint8, m->virt_addr);
		else
			printu8(m, count);
		break;
	case sizeof(uint16_t):
		if (quiet)
			READ_LOOP(count, uint16, m->virt_addr);
		else
			printu16(m, count);
		break;
	case sizeof(uint32_t):
		if (quiet)
			READ_LOOP(count, uint32, m->virt_addr);
		else
			printu32(m, count);
		break;
	case sizeof(uint64_t):
		if (quiet) {
			READ_LOOP(count, uint64, m->virt_addr);
		}
		else
			printu64(m, count);
		break;
	default:
		break;
	}
}

#define WRITE_LOOP(val, count, type, memory, quiet) do {                   \
	type##_t *ptr = (memory)->virt_addr;                                   \
	while(count--) {                                                       \
		if (!quiet)                                                        \
			printf("Writing %#0*llx to %08x\n",                            \
			       sizeof(type##_t) * 2, (uint64_t) val.type,              \
			       getphys(memory, ptr));                                  \
		*ptr++ = val.type;                                                 \
	}                                                                      \
} while(0)

void write_mem(struct memory *m, union value v, int size_in_bytes, int count,
               bool sign, bool quiet)
{
	switch (size_in_bytes) {
	case sizeof(uint8_t):
		if (sign)
			WRITE_LOOP(v, count, int8, m, quiet);
		else
			WRITE_LOOP(v, count, uint8, m, quiet);
		break;
	case sizeof(uint16_t):
		if (sign)
			WRITE_LOOP(v, count, int16, m, quiet);
		else
			WRITE_LOOP(v, count, uint16, m, quiet);
		break;
	case sizeof(uint32_t):
		if (sign)
			WRITE_LOOP(v, count, int32, m, quiet);
		else
			WRITE_LOOP(v, count, uint32, m, quiet);
		break;
	case sizeof(uint64_t):
		if (sign)
			WRITE_LOOP(v, count, int64, m, quiet);
		else
			WRITE_LOOP(v, count, uint64, m, quiet);
		break;
	default:
		break;
	}
}

#define MASKSET(mask, count, type, memory, quiet) do {                       \
	type##_t *ptr = (memory)->virt_addr;                                     \
	int len = count;                                                         \
	while(len--) {                                                           \
		if (!quiet)                                                          \
			printf("Setting mask %0*llx on %0*llx at %#8x ",                 \
			       sizeof(type##_t) * 2, (uint64_t) mask,                    \
			       sizeof(type##_t) * 2, (uint64_t) *ptr,                    \
			       getphys(memory, ptr));                                    \
		*ptr |= (mask);                                                      \
		if (!quiet)                                                          \
			printf("now %0*llx\n", sizeof(type##_t) * 2, (uint64_t) *ptr++); \
	}                                                                        \
} while(0)

void maskset(struct memory *m, uint64_t mask, int size_in_bytes, int count,
             bool quiet)
{
	switch (size_in_bytes) {
	case sizeof(uint8_t):
		MASKSET(mask, count, uint8, m, quiet);
		break;
	case sizeof(uint16_t):
		MASKSET(mask, count, uint16, m, quiet);
		break;
	case sizeof(uint32_t):
		MASKSET(mask, count, uint32, m, quiet);
		break;
	case sizeof(uint64_t):
		MASKSET(mask, count, uint64, m, quiet);
		break;
	default:
		break;
	}
}

#define MASKCLEAR(mask, count, type, memory, quiet) do {                     \
	type##_t *ptr = (memory)->virt_addr;                                     \
	int len = count;                                                         \
	while(len--) {                                                           \
		if (!quiet)                                                          \
			printf("Clearing mask %0*llx on %0*llx at %#8x ",                \
			       sizeof(type##_t) * 2, (uint64_t) mask,                    \
			       sizeof(type##_t) * 2, (uint64_t) *ptr,                    \
			       getphys(memory, ptr));                                    \
		*ptr &= ~(mask);                                                     \
		if (!quiet)                                                          \
			printf("now %0*llx\n", sizeof(type##_t) * 2, (uint64_t) *ptr++); \
	}                                                                        \
} while(0)


void maskclear(struct memory *m, uint64_t mask, int size_in_bytes, int count,
               bool quiet)
{
	switch (size_in_bytes) {
	case sizeof(uint8_t):
		MASKCLEAR(mask, count, uint8, m, quiet);
		break;
	case sizeof(uint16_t):
		MASKCLEAR(mask, count, uint16, m, quiet);
		break;
	case sizeof(uint32_t):
		MASKCLEAR(mask, count, uint32, m, quiet);
		break;
	case sizeof(uint64_t):
		MASKCLEAR(mask, count, uint64, m, quiet);
		break;
	default:
		break;
	}
}

int main(int argc, char *argv[])
{
	struct args a =
	    { NULL, 1, sizeof(uint32_t), OPERATION_READ, false, 0, 0, false };
	struct memory m;
	char *cmd = basename(argv[0]);

	if (strcmp(cmd, "m") == 0) {
		if (parse_args(argc, argv, &a))
			return 1;
	}
	else if (strncmp(cmd, "md", 2) == 0) {
		if (parse_md(argc, argv, &a))
			return 1;
	}
	else if (strncmp(cmd, "mw", 2) == 0) {
		if (parse_mw(argc, argv, &a))
			return 1;
	}

	if (a.address % a.size_in_bytes) {
		fprintf(stderr, "Address %#x incorrectly aligned for size %d\n",
		        a.address, a.size_in_bytes);
		return 1;
	}

	m.phys_addr = a.address;
	if (map_address(&m, a.operation, a.size_in_bytes * a.count))
		return 1;

	switch (a.operation) {
	case OPERATION_READ:
		read_mem(&m, a.size_in_bytes, a.count, a.quiet);
		break;
	case OPERATION_WRITE:
		write_mem(&m, a.v, a.size_in_bytes, a.count, a.sign, a.quiet);
		break;
	case OPERATION_BITSET:
		maskset(&m, 1 << a.bit, a.size_in_bytes, a.count, a.quiet);
		break;
	case OPERATION_BITCLEAR:
		maskclear(&m, 1 << a.bit, a.size_in_bytes, a.count, a.quiet);
		break;
	case OPERATION_MASKSET:
		maskset(&m, a.mask, a.size_in_bytes, a.count, a.quiet);
		break;
	case OPERATION_MASKCLEAR:
		maskclear(&m, a.mask, a.size_in_bytes, a.count, a.quiet);
		break;
	default:
		break;
	}

	if (munmap(m.pages_start, m.pages_size)) {
		fprintf(stderr, "Failed to unmap %p of size %d (%s)\n",
		        m.pages_start, m.pages_size, strerror(errno));
		return 1;
	}

	return 0;
}
