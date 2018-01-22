#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <stdint.h>
#include <string.h>
#include <errno.h>
#include <time.h>
#include <arpa/inet.h>

#include "ulh_rlnh.h"
#include "ulh_cm.h"

#ifdef ULH_RLNH_DEBUG

static char *ulh_rlnh_type2str(uint16_t type)
{
	switch (type) {
	case RLNH_INIT: return "INIT";
	case RLNH_INIT_REPLY: return "INIT_REPLY";
	case RLNH_PUBLISH: return "PUBLISH";
	case RLNH_QUERY_NAME: return "QUERY_NAME";
	case RLNH_UNPUBLISH: return "UNPUBLISH";
	case RLNH_UNPUBLISH_ACK: return "UNPUBLISH_ACK";
	default:
		break;
	}

	return "????";
}

void ulh_rlnh_print_header(const char *name, const char *prfx,
		struct ulh_cm_msghdr *hdr, uint16_t type)
{
	struct timespec ts;

	clock_gettime(CLOCK_MONOTONIC, &ts);
	type = ntohs(type);

	if (!hdr->src && !hdr->dst)
		printf("[%9ld.%9ld] %s %s size=%u %s(%u) src=0x%x dst=0x%x\n",
				ts.tv_sec, ts.tv_nsec,
				name, prfx,
				hdr->size,
				ulh_rlnh_type2str(type),
				type,
				hdr->src,
				hdr->dst);
	else
		printf("[%9ld.%9ld] %s %s size=%u SIGNAL (0x%x) src=0x%x "
				"dst=0x%x\n",
				ts.tv_sec, ts.tv_nsec,
				name, prfx,
				hdr->size,
				type,
				hdr->src,
				hdr->dst);
}

#endif /* ULH_RLNH_DEBUG */
