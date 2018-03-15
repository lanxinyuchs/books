#ifndef _NODEID_SERVER_H_
#define _NODEID_SERVER_H_

/*----------------------------  Include files  ------------------------------*/
#include "conn-establish.h"

#define NODEID_SERVER_NAME     "nodeid_server"
#define NODEID_SERVER_VERSIONS              1

#define NODEID_RESULT_SUCCESS               0
#define NODEID_RESULT_INVALID_PARAM         1
#define NODEID_RESULT_NOT_FOUND             2
#define NODEID_RESULT_RESOURCE_SHORTAGE     3
#define NODEID_RESULT_ACCESS_DENIED         4
#define NODEID_RESULT_OTHER_ERROR           5

/* Signal define */
#ifndef NODEID_SIGBASE
#define NODEID_SIGBASE 0x0111E00
#endif

#define NODEID_CONN_ESTABLISH_MSG_STRUCT(name) \
	struct conn_establish_msg_numbers name = \
	{ \
		NODEID_CONN_ESTABLISH_REQ, \
		NODEID_CONN_ESTABLISH_CFM, \
		NODEID_CONN_ESTABLISH_REJ, \
		NODEID_CONN_DISCONNECT_REQ, \
		NODEID_CONN_DISCONNECT_CFM, \
		NODEID_CONN_DISCONNECT_REJ, \
		NODEID_CONN_MONITOR_FWD \
	}

#define NODEID_STRUCTS \
	conn_any_msg_t                    any_msg;         \
	struct nodeid_read_req     nodeid_read_req; \
	struct nodeid_read_cfm     nodeid_read_cfm; \
	struct nodeid_read_rej     nodeid_read_rej; \
	struct nodeid_write_req    nodeid_write_req; \
	struct nodeid_write_cfm    nodeid_write_cfm; \
	struct nodeid_write_rej    nodeid_write_rej; \
	struct nodeid_erase_req    nodeid_erase_req; \
	struct nodeid_erase_cfm    nodeid_erase_cfm; \
	struct nodeid_erase_rej    nodeid_erase_rej;


#define NODEID_CONN_ESTABLISH_REQ    (NODEID_SIGBASE + 0x1)
#define NODEID_CONN_ESTABLISH_CFM    (NODEID_SIGBASE + 0x2)
#define NODEID_CONN_ESTABLISH_REJ    (NODEID_SIGBASE + 0x3)
#define NODEID_CONN_DISCONNECT_REQ   (NODEID_SIGBASE + 0x4)
#define NODEID_CONN_DISCONNECT_CFM   (NODEID_SIGBASE + 0x5)
#define NODEID_CONN_DISCONNECT_REJ   (NODEID_SIGBASE + 0x6)
#define NODEID_CONN_MONITOR_FWD      (NODEID_SIGBASE + 0x7)

#define NODE_ID_READ_REQ    (NODEID_SIGBASE + 0x9)
#define NODE_ID_READ_CFM    (NODEID_SIGBASE + 0xA)
#define NODE_ID_READ_REJ    (NODEID_SIGBASE + 0xB)

#define NODE_ID_WRITE_REQ   (NODEID_SIGBASE + 0xC)
#define NODE_ID_WRITE_CFM   (NODEID_SIGBASE + 0xD)
#define NODE_ID_WRITE_REJ   (NODEID_SIGBASE + 0xE)

#define NODE_ID_ERASE_REQ  (NODEID_SIGBASE + 0xF)
#define NODE_ID_ERASE_CFM  (NODEID_SIGBASE + 0x10)
#define NODE_ID_ERASE_REJ  (NODEID_SIGBASE + 0x11)

#define NODEID_MSG_HEADER \
	uint32_t msgno;         \
	uint32_t procedure_ref; \
	uint32_t connection_ref;

struct nodeid_read_req {
	NODEID_MSG_HEADER
};

struct nodeid_read_cfm {
	NODEID_MSG_HEADER
	uint32_t length;
	/*must be last*/
	uint8_t  node_id[];
};

struct nodeid_read_rej {
	NODEID_MSG_HEADER
	uint32_t  error_code;
};

struct nodeid_write_req {
	NODEID_MSG_HEADER
	uint32_t length;
	/*must be last*/
	uint8_t  node_id[];
};

struct nodeid_write_cfm {
	NODEID_MSG_HEADER
};

struct nodeid_write_rej {
	NODEID_MSG_HEADER
	uint32_t error_code;
};

struct nodeid_erase_req {
	NODEID_MSG_HEADER
};

struct nodeid_erase_cfm {
	NODEID_MSG_HEADER
};

struct nodeid_erase_rej {
	NODEID_MSG_HEADER
	uint32_t error_code;
};

#endif
