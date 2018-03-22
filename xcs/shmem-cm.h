#ifndef __SHMEM_CM_H
#define __SHMEM_CM_H

#include <stdint.h>


/*
** Shared Memory Connection Manager Messages definitions
*/

#define SHMEM_CM_MSG_TYPE_CONN 0
#define SHMEM_CM_MSG_TYPE_ACK  1
#define SHMEM_CM_MSG_TYPE_NACK 2
#define SHMEM_CM_MSG_TYPE_DISC 3
#define SHMEM_CM_MSG_TYPE_PING 4
#define SHMEM_CM_MSG_TYPE_PONG 5
#define SHMEM_CM_MSG_TYPE_DATA 6

#define SHMEM_CM_MSG_VERSION   1

struct shmem_cm_msg_conn {
	uint32_t type;
	uint32_t version;
};

struct shmem_cm_msg_ack {
	uint32_t type;
};

struct shmem_cm_msg_nack {
	uint32_t type;
	uint32_t version;
};

struct shmem_cm_msg_ping {
	uint32_t type;
};

struct shmem_cm_msg_pong {
	uint32_t type;
};

struct shmem_cm_msg_disc {
	uint32_t type;
};

struct shmem_cm_msg_data {
	uint32_t type;
	uint32_t src;
	uint32_t dst;
	uint32_t size;
	uint32_t data[];
};

union shmem_cm_msg {
	uint32_t                 type;
	struct shmem_cm_msg_conn conn;
	struct shmem_cm_msg_ack  ack;
	struct shmem_cm_msg_nack nack;
	struct shmem_cm_msg_ping ping;
	struct shmem_cm_msg_pong pong;
	struct shmem_cm_msg_data data;
};


/*
** Shared Memory Connection Manager FIFO definitions
*/

typedef void (*shmem_cm_fifo_uc)(void *object, uint32_t *data, uint32_t size);

/* This structure is exposed for memory allocation purposes (not for access). */
struct shmem_cm_fifo {
	void              *object;
	shmem_cm_fifo_uc   uc;
	uint32_t          *mbox;
	uint32_t           write;
	uint32_t           end;
	uint32_t          *head;
	uint32_t          *start;
};

void shmem_cm_fifo_init(struct shmem_cm_fifo *fifo,
                        void *object, shmem_cm_fifo_uc uc,
                        uint32_t *mbox, uint32_t *start, uint32_t size);
void shmem_cm_fifo_tx(struct shmem_cm_fifo *fifo);
void shmem_cm_fifo_rx(struct shmem_cm_fifo *fifo);
int shmem_cm_fifo_enqueue(struct shmem_cm_fifo *fifo,
                          const void *data0, uint32_t size0,
                          const void *data1, uint32_t size1);

#endif /* SHMEM_CM_H */
