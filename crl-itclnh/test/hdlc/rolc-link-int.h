/* ---------------------------------------------------------------------------
 *
 * © Ericsson AB 2013 All rights reserved.
 * The information in this document is the property of Ericsson.  Except
 * as specifically authorized in writing by Ericsson, the receiver of
 * this document shall keep the information contained herein confidential
 * and shall protect the same in whole or in part from disclosure and
 * dissemination to third parties.  Disclosure and disseminations to the
 * receivers employees shall only be made on a strict need to know basis.
 *
 * ---------------------------------------------------------------------------
 */

#ifndef _ROLC_LINK_INT_H_
#define _ROLC_LINK_INT_H_

#include "rolc-link-api.h"


#define ROLC_DAEMON_NAME			"rolc_daemon"


#define ROLC_CREATE_REQ				(0x01900407)
struct rolc_create_req {
	uint32_t	msgno;
	void 		*owner;
	uint32_t 	ri_port;
	uint32_t 	ri_addr;
	uint32_t 	mode;
	char	 	name[ROLC_LINKNAME_SIZE];
};

#define ROLC_CREATE_RSP				(0x01900408)
struct rolc_create_rsp {
	uint32_t 	msgno;
	uint32_t 	linkid;
	int			result;
	char	 	name[ROLC_LINKNAME_SIZE];
};

#define ROLC_DELETE_REQ				(0x01900409)
struct rolc_delete_req {
	uint32_t	msgno;
	int			reserved;
	void 		*owner;
	uint32_t 	linkid;
};

#define ROLC_DELETE_RSP				(0x0190040a)
struct rolc_delete_rsp {
	uint32_t	msgno;
	int			result;
	void 		*owner;
	uint32_t 	linkid;
};


/**********************************************************
**  Test signals
**********************************************************/

struct test_args {
	struct rolc_if*	rolc_handle;
	itc_mbox_id_t	parent_mbox;
	itc_mbox_id_t	peer_mbox;
	itc_mbox_id_t	test_mbox;
	itc_mbox_id_t	work_mbox;
	int				size;
	int				nlink;
	int				nthread;
	int 			master;
};



#define ROLC_TEST_BASE					(0xbabe0000)

#define ROLC_TEST_DONE					(ROLC_TEST_BASE + 0)
struct rolc_test_done {
	uint32_t	msgno;
	int			result;
};

#define ROLC_TEST_CREATE_LINK			(ROLC_TEST_BASE + 1)
#define ROLC_TEST_CREATE_LINK_RSP		(ROLC_TEST_BASE + 2)
struct rolc_test_create_link {
	uint32_t	msgno;
	uint32_t	linkid;
	uint32_t	addr;
	uint32_t	mode;
	char		link_name[ROLC_LINKNAME_SIZE];
};

#define ROLC_TEST_DESTROY_LINK			(ROLC_TEST_BASE + 3)
#define ROLC_TEST_DESTROY_LINK_RSP		(ROLC_TEST_BASE + 4)
struct rolc_test_destroy_link {
	uint32_t	msgno;
	uint32_t	linkid;
	int			result;
};


#define ROLC_TEST_CREATE_MBOX			(ROLC_TEST_BASE + 5)
#define ROLC_TEST_CREATE_MBOX_RSP		(ROLC_TEST_BASE + 6)
struct rolc_test_create_mbox {
	uint32_t		msgno;
	itc_mbox_id_t	mail_box;
	char			mbox_name[ROLC_LINKNAME_SIZE];
};


#define ROLC_TEST_DESTROY_MBOX			(ROLC_TEST_BASE + 7)
#define ROLC_TEST_DESTROY_MBOX_RSP		(ROLC_TEST_BASE + 8)
struct rolc_test_destroy_mbox {
	uint32_t		msgno;
	itc_mbox_id_t	mail_box;
};


#define ROLC_TEST_LOCATE_MBOX			(ROLC_TEST_BASE + 9)
#define ROLC_TEST_LOCATE_MBOX_RSP		(ROLC_TEST_BASE + 10)
struct rolc_test_locate_mbox {
	uint32_t		msgno;
	char			mbox_name[ROLC_LINKNAME_SIZE];
};


#define ROLC_TEST_PING					(ROLC_TEST_BASE + 11)
#define	ROLC_TEST_PONG					(ROLC_TEST_BASE + 12)
struct rolc_test_ping_pong {
	uint32_t		msgno;
	char			data[1];
};

#define ROLC_SPRAY_DONE					(ROLC_TEST_BASE + 13)
struct rolc_spray_done {
	uint32_t	msgno;
	int			result;
};


extern void set_rx_skip_mode(uint8_t addr, uint8_t mode);
extern void set_tx_skip_mode(uint8_t addr, uint8_t mode);


extern void handle_rolc_create_link(struct test_args*, union itc_msg *);
extern void handle_rolc_destroy_link(struct test_args*, union itc_msg*);
extern void handle_rolc_create_mbox(struct test_args*, union itc_msg*);
extern void handle_rolc_destroy_mbox(struct test_args*, union itc_msg*);
extern void handle_rolc_locate_mbox(struct test_args*, union itc_msg*);


extern unsigned long crc32(char* p, int len);
//extern void print_data(char *p, int size);
extern void *test_prog_m(void*);
extern void *test_prog_s(void*);
extern void *test_spray(void*);

#endif
