#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <stdint.h>

#include <net/if.h>
#include <netinet/in.h>
#include <sys/ioctl.h>
#include <sys/socket.h>
#include <arpa/inet.h>
#include <pthread.h>
#include <errno.h>

#include <itc.h>

#include "ecm-link-api.h"

#include "ulh_trace.h"
#ifdef LTTNG
#include "ulh_lttng.h"
#include "../libitclnh/ulh_rlnh.h"
#endif

#define ECM_TEST_LINK  "ecmtest_link"
#define ECM_TEST_MBOX  "ecmtest_mbox"
#define ECM_TEST_MSG   "hello ecm"
#define ECM_TEST_MSGNO 0
#define IF_NAME        "eth0"

union itc_msg
{
	uint32_t msgno;
	char content[1];
};

struct ecm_if
{
	int magic;
};

static uint8_t board1_mac[6] = {0xB2,0xBD,0xBF,0x68,0xD2,0xAC};
static uint8_t board2_mac[6] = {0x7A,0xC8,0xBC,0xE1,0x34,0xD2};
static char *board1_ip = "10.163.201.122";
static char *board2_ip = "10.163.201.123";

int main(int argc, char **argv)
{
	struct ecm_if handle;
	struct ecm_if *p_handle = &handle;
	int sockfd;
	struct ifreq ifr;
	struct sockaddr_in sin;
	char local_ip[16];
	struct ecm_link_config cfg;
	uint32_t link_id;
	int ret;
	char remote_mbox[32];
	itc_mbox_id_t peer_id;
   	union itc_msg *msg;
   	uint32_t locate_rsp_filter[] = {1, ITC_LOCATE_DEFAULT_NO};
    uint32_t ecm_test_filter[] = {1, ECM_TEST_MSGNO};

    /** initialization **/

	itc_init(1, ITC_MALLOC, NULL, ITC_NO_NAMESPACE, 0);

	itc_create_mailbox(ECM_TEST_MBOX, 0);

	ecm_link_init(&p_handle);

	/* get the ip address */

    sockfd = socket(PF_INET, SOCK_DGRAM, IPPROTO_IP);

    strcpy(ifr.ifr_name, IF_NAME);
	if(ioctl(sockfd, SIOCGIFADDR, &ifr) == -1) 
	{
		ULH_TRACE_ERROR("ioctl SIOCGIFADDR failed: %d(%s)",
				         errno, strerror(errno));
		return -1;
	}

	close(sockfd);

	memcpy(&sin, &ifr.ifr_addr, sizeof(sin));
	strcpy(local_ip, inet_ntoa(sin.sin_addr)); 
    printf("local ip is %s\n", local_ip); 

    if (strcmp(local_ip, board1_ip) == 0)
    {
    	for (int i=0; i<MAC_ADDR_LEN; i++)
    		cfg.dst_mac[i] = board2_mac[i];
    }
    else if (strcmp(local_ip, board2_ip) == 0)
    {
    	for (int i=0; i<MAC_ADDR_LEN; i++)
    		cfg.dst_mac[i] = board1_mac[i];
    }
	cfg.dst_vlan = 0;
	strcpy(cfg.device, IF_NAME);

	ret = ecm_link_create(p_handle, ECM_TEST_LINK, &cfg, &link_id);
	if (ret < 0)
	{
		ULH_TRACE_ERROR("ecm link create failed: %d", ret);
		return -1;
	}
	else
	{
		ULH_TRACE_INFO("ecm link id is: %d", link_id);
	}	

	/** sending message **/

	snprintf(remote_mbox, sizeof(remote_mbox), 
			 "%s/%s", ECM_TEST_LINK, ECM_TEST_MBOX);

	itc_locate_async(remote_mbox, NULL, ITC_MY_MBOX);
	msg = itc_receive(locate_rsp_filter, 2000, ITC_FROM_ALL);
	if(msg == NULL) 
	{
		ULH_TRACE_ERROR("peer %s not found\n", remote_mbox);
		abort();		
	}
	else
	{
		itc_free(&msg);
	}

	msg = itc_alloc(sizeof(uint32_t) + sizeof(ECM_TEST_MSG), ECM_TEST_MSGNO);
	strcpy(msg->content, ECM_TEST_MSG);

	itc_send(&msg, peer_id, ITC_MY_MBOX);

	msg = itc_receive(ecm_test_filter, ITC_NO_TMO, ITC_FROM_ALL);
    printf("%s\n", msg->content);

    itc_free(&msg); 

    return 0;
} 