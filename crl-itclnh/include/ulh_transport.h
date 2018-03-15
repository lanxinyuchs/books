#ifndef ULH_TRANSPORT_H__
#define ULH_TRANSPORT_H__

#include <stdint.h>
#include <itc.h>

#include <ulh_dl_list.h>
#include <ulh_ref.h>

#define ULH_TRANS_NOCONN	((uint32_t) -1)
#define ULH_TRANS_MAX_ADDR_LEN  128

struct ulh_trans_addr {
	uint8_t type;
	uint8_t len;
	uint8_t data[ULH_TRANS_MAX_ADDR_LEN]; /* TODO: rename */
};

struct ulh_tbuff_rbuf {
	struct ulh_ref ref;
	uint8_t *buf;
};

struct ulh_tbuff {
	uint32_t size;
	uint8_t *data;
	struct dl_list link;
	struct ulh_tbuff_rbuf *rbuf;
};

struct ulh_tbuff_queue {
	struct dl_list queue;
};

struct ulh_tbuff_pool {
	struct ulh_tbuff **pool;
	struct dl_list f_list;
	uint32_t size;
	uint32_t limit;
};

#define ULH_TRANSMSG_DATA	(0x0190041f)
struct ulh_transmsg_data {
	uint32_t msg_no;
	struct ulh_tbuff data;
	uint32_t cid;
	unsigned long uref;
};

#define ULH_TRANSMSG_STATE	(0x01900420)
struct ulh_transmsg_state {
	uint32_t msg_no;
	uint32_t state;
	uint32_t cid;
	unsigned long uref;
};

typedef enum ulh_trans_info {
	TRANS_HEADING = 0,
	TRANS_SUMMARY,
	TRANS_DETAILED
} ulh_trans_info;

int ulh_trans_init(uint32_t max_conns);
uint32_t ulh_trans_create_conn(const char *tr_name, struct ulh_trans_addr *src,
		struct ulh_trans_addr *dst);
int ulh_trans_destroy_conn(uint32_t cid);
uint32_t ulh_trans_getmtu(uint32_t cid);
int ulh_trans_conn_info(uint32_t cid, ulh_trans_info, char *text, int maxtextlen);

int ulh_trans_attach(uint32_t cid, itc_mbox_id_t mbox, unsigned long uref);
int ulh_trans_detach(uint32_t cid, itc_mbox_id_t mbox, unsigned long uref);

int ulh_trans_transmit(uint32_t cid, struct ulh_tbuff *buff);

int ulh_tbuff_alloc(uint32_t cid, uint32_t size,
		struct ulh_tbuff *tbuff);

static inline void ulh_tbuff_free(struct ulh_tbuff *tbuff)
{
	ulh_unhold_ref(&tbuff->rbuf->ref);
}

static inline uint8_t *ulh_tbuff_get(struct ulh_tbuff *tbuff)
{
	return tbuff->data;
}

static inline uint32_t ulh_tbuff_len(struct ulh_tbuff *tbuff)
{
	return tbuff->size;
}

static inline void ulh_tbuff_hold(struct ulh_tbuff *buff)
{
	ulh_hold_ref(&buff->rbuf->ref);
}

static inline uint32_t ulh_tbuff_headroom(struct ulh_tbuff *buff)
{
	return buff->data - buff->rbuf->buf;
}

static inline void ulh_tbuff_push(struct ulh_tbuff *buff, uint32_t len)
{
	buff->data -= len;
	buff->size += len;
}

static inline void ulh_tbuff_pop(struct ulh_tbuff *buff, uint32_t len)
{
	buff->data += len;
	buff->size -= len;
}

static inline void ulh_tbuff_queue_init(struct ulh_tbuff_queue *q)
{
	dl_list_init(&q->queue);
}

static inline void ulh_tbuff_queue(struct ulh_tbuff_queue *q,
		struct ulh_tbuff *tbuff)
{
	dl_list_init(&tbuff->link);
	dl_list_insert_tail(&q->queue, &tbuff->link);
}

static inline void ulh_tbuff_queue_head(struct ulh_tbuff_queue *q,
		struct ulh_tbuff *tbuff)
{
	dl_list_init(&tbuff->link);
	dl_list_insert_head(&q->queue, &tbuff->link);
}

static inline struct ulh_tbuff *ulh_tbuff_dequeue(struct ulh_tbuff_queue *q)
{
	struct ulh_tbuff *tbuff;
	if (dl_list_empty(&q->queue))
		return NULL;
	tbuff = dl_list_first_entry(&q->queue, struct ulh_tbuff, link);
	dl_list_remove(&tbuff->link);
	return tbuff;
}

static inline int ulh_tbuff_queue_count(struct ulh_tbuff_queue *q)
{
	int count = 0;
	struct ulh_tbuff *buf;

	dl_list_foreach(buf, &q->queue, link)
		count++;

	return count;
}

int ulh_tbuff_pool_init(struct ulh_tbuff_pool *p, uint32_t limit);
void ulh_tbuff_pool_free(struct ulh_tbuff_pool *p);
int ulh_tbuff_pool_expand(struct ulh_tbuff_pool *p);
int ulh_tbuff_pool_shrink(struct ulh_tbuff_pool *p);

static inline struct ulh_tbuff *ulh_tbuff_pool_get(struct ulh_tbuff_pool *p)
{
	struct ulh_tbuff *tbuff;

	if (dl_list_empty(&p->f_list) && ulh_tbuff_pool_expand(p))
		return NULL;
	tbuff = dl_list_first_entry(&p->f_list, struct ulh_tbuff, link);
	dl_list_remove(&tbuff->link);
	return tbuff;
}

static inline void ulh_tbuff_pool_put(struct ulh_tbuff_pool *p,
		struct ulh_tbuff *tbuff)
{
	dl_list_remove(&tbuff->link);
	dl_list_insert_tail(&p->f_list, &tbuff->link);
}

/* ................. */
struct ulh_trans_ops {
	int (*create_conn)(void *, uint32_t, struct ulh_trans_addr *,
			struct ulh_trans_addr *, void **);
	int (*destroy_conn)(void *, void *);
	int (*enable)(void *, void *);
	int (*disable)(void *, void *);
	int (*transmit)(void *, void *, struct ulh_tbuff *);
	int (*getmtu)(void *, void *);
	int (*alloc_buff)(void *, void *, uint32_t, struct ulh_tbuff *);
	int (*conn_info)(void *, void *, ulh_trans_info, char *, int);

	void (*destroy)(void *);
};

int ulh_trans_register(const char *name, struct ulh_trans_ops *ops, void *ref);
int ulh_trans_unregister(const char *name);

void ulh_trans_deliver(uint32_t cid, struct ulh_tbuff *tbuff);

int ulh_tbuff_alloc_def(struct ulh_tbuff *tbuff, uint32_t size);

#endif /* ULH_TRANSPORT_H__ */
