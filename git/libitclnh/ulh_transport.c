#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <pthread.h>
#include <errno.h>

#include <ulh_dl_list.h>
#include <itc.h>
#include "ulh_ref.h"
#include "ulh_transport.h"

struct ulh_trans_conn_listener {
	struct dl_list link;
	itc_mbox_id_t mbox;
	unsigned long uref;
};

struct ulh_trans_conn {
	struct dl_list link;
	int state;
	uint32_t cid;
	struct ulh_transport *trans; /* XXX copy to speed up?*/
	void *conn_ref;
	struct ulh_ref ref;

	pthread_mutex_t lock;
	struct dl_list listeners;

	struct ulh_trans_addr src;
	struct ulh_trans_addr dst;
};

struct ulh_transport {
	struct dl_list link;
	char *name;
	struct ulh_trans_ops *ops;
	void *param;
	struct dl_list conns;
	struct ulh_ref ref;

	pthread_mutex_t wait_lock;
	pthread_cond_t wait;
};

struct ulh_transport_global {
	struct dl_list trans;
	pthread_mutex_t lock;

	uint32_t max_conns;
	struct ulh_trans_conn *conns;
	struct dl_list f_conns;
};

union itc_msg {
	uint32_t msg_no;
	struct ulh_transmsg_data data;
};

static struct ulh_transport_global *tr = NULL;


static struct ulh_transport *get_trans(const char *name)
{
	struct ulh_transport *it;

	dl_list_foreach(it, &tr->trans, link) {
		if (!strcmp(it->name, name))
			return it;
	}

	return NULL;
}

static inline struct ulh_trans_conn *__get_conn(uint32_t cid)
{
	struct ulh_trans_conn *conn;

	if (cid >= tr->max_conns)
		return NULL;
	conn = &tr->conns[cid];
	if (!conn->state)
		return NULL;

	return conn;
}

int ulh_trans_init(uint32_t max_conns)
{
	uint32_t i;

	if (tr)
		return -EALREADY;
	if (!max_conns)
		return -EINVAL;

	tr = malloc(sizeof(*tr));
	if (!tr)
		return -ENOMEM;
	dl_list_init(&tr->trans);
	pthread_mutex_init(&tr->lock, NULL);
	dl_list_init(&tr->f_conns);
	tr->max_conns = max_conns;

	tr->conns = malloc(sizeof(struct ulh_trans_conn) * tr->max_conns);
	if (!tr->conns) {
		free(tr);
		tr = NULL;
		return -ENOMEM;
	}

	for (i = 0; i < tr->max_conns; i++) {
		tr->conns[i].state = 0;
		tr->conns[i].cid = i;
		dl_list_insert_tail(&tr->f_conns, &tr->conns[i].link);
		pthread_mutex_init(&tr->conns[i].lock, NULL);
	}

	return 0;
}

static void __ulh_trans_free(struct ulh_ref *ref)
{
	struct ulh_transport *trans = container_of(ref, struct ulh_transport,
			ref);

	pthread_mutex_lock(&trans->wait_lock);
	pthread_cond_broadcast(&trans->wait);
	pthread_mutex_unlock(&trans->wait_lock);
}

int ulh_trans_register(const char *name, struct ulh_trans_ops *ops, void *ref)
{
	struct ulh_transport *trans, *it;

	if (!tr)
		return -EFAULT;
	if (!name || !ops)
		return -EINVAL;

	trans = malloc(sizeof(*trans));
	if (!trans) {
		return -ENOMEM;
	}
	trans->name = strdup(name);
	if (!trans->name) {
		free(trans);
		return -ENOMEM;
	}

	trans->ops = ops;
	trans->param = ref;
	dl_list_init(&trans->conns);
	pthread_mutex_init(&trans->wait_lock, NULL);
	pthread_cond_init(&trans->wait, NULL);
	ulh_init_ref(&trans->ref, 1, __ulh_trans_free);

	pthread_mutex_lock(&tr->lock);
	it = get_trans(name);
	if (it) {
		pthread_mutex_unlock(&tr->lock);
		free(trans->name);
		free(trans);
		return -EBUSY;
	}

	dl_list_insert_tail(&tr->trans, &trans->link);
	pthread_mutex_unlock(&tr->lock);

	return 0;
}

int ulh_trans_unregister(const char *name)
{
	struct ulh_transport *trans;

	if (!tr)
		return -EFAULT;
	if (!name)
		return -EINVAL;

	pthread_mutex_lock(&tr->lock);
	trans = get_trans(name);
	if (!trans) {
		pthread_mutex_unlock(&tr->lock);
		return 0;
	}

	dl_list_remove(&trans->link);
	pthread_mutex_unlock(&tr->lock);

	ulh_unhold_ref(&trans->ref);

	pthread_mutex_lock(&trans->wait_lock);
	while (ulh_read_ref(&trans->ref))
		pthread_cond_wait(&trans->wait, &trans->wait_lock);
	pthread_mutex_unlock(&trans->wait_lock);

	if (trans->ops->destroy)
		trans->ops->destroy(trans->param);

	free(trans->name);
	free(trans);
	return 0;
}

static void __ulh_trans_conn_free(struct ulh_ref *ref)
{
	struct ulh_transport *trans;
	struct ulh_trans_conn *conn = container_of(ref, struct ulh_trans_conn,
			ref);

	conn->state = 0;
	trans = conn->trans;
	if (trans->ops->destroy_conn)
		trans->ops->destroy_conn(trans->param,
				conn->conn_ref);

	pthread_mutex_lock(&tr->lock);
	dl_list_remove(&conn->link);
	dl_list_insert_tail(&tr->f_conns, &conn->link);
	pthread_mutex_unlock(&tr->lock);

	ulh_unhold_ref(&trans->ref);
}

uint32_t ulh_trans_create_conn(const char *tr_name, struct ulh_trans_addr *src,
		struct ulh_trans_addr *dst)
{
	struct ulh_transport *trans;
	struct ulh_trans_conn *conn;
	int ret;
	void *conn_ref;

	if (!tr)
		return -EFAULT;
	if (!tr_name)
		return -EINVAL;

	pthread_mutex_lock(&tr->lock);
	trans = get_trans(tr_name);
	if (!trans || !trans->ops->create_conn) {
		pthread_mutex_unlock(&tr->lock);
		return -EINVAL;
	}

	dl_list_foreach(conn, &trans->conns, link) {
		if (!memcmp(&conn->src, src, sizeof(*src)) &&
		    !memcmp(&conn->dst, dst, sizeof(*dst))) {
			ulh_hold_ref(&conn->ref);
			pthread_mutex_unlock(&tr->lock);
			return conn->cid;
		}
	}

	if (dl_list_empty(&tr->f_conns)) {
		pthread_mutex_unlock(&tr->lock);
		return -EFAULT; /* XXX */
	}

	conn = dl_list_first_entry(&tr->f_conns, struct ulh_trans_conn, link);
	dl_list_remove(&conn->link);
	dl_list_init(&conn->listeners);

	dl_list_insert_tail(&trans->conns, &conn->link);
	ulh_hold_ref(&trans->ref);

	ret = trans->ops->create_conn(trans->param, conn->cid, src, dst,
			&conn_ref);
	if (!ret) {
		ulh_init_ref(&conn->ref, 1, __ulh_trans_conn_free);
		conn->conn_ref = conn_ref;
		conn->trans = trans;
		memcpy(&conn->src, src, sizeof(*src));
		memcpy(&conn->dst, dst, sizeof(*dst));
		conn->state = 1;
		pthread_mutex_unlock(&tr->lock);
		return conn->cid;
	}

	dl_list_remove(&conn->link);
	dl_list_insert_head(&tr->f_conns, &conn->link);
	pthread_mutex_unlock(&tr->lock);

	ulh_unhold_ref(&trans->ref);

	return ULH_TRANS_NOCONN;
}

int ulh_trans_destroy_conn(uint32_t cid)
{
	struct ulh_trans_conn *conn;

	if (!tr)
		return -EFAULT;

	conn = __get_conn(cid);
	if (!conn)
		return -EINVAL;

	ulh_unhold_ref(&conn->ref);
	return 0;
}

uint32_t ulh_trans_getmtu(uint32_t cid)
{
	struct ulh_trans_conn *conn;
	struct ulh_transport *trans;

	if (!tr)
		return -EFAULT;

	conn = __get_conn(cid);
	if (!conn)
		return -EINVAL;

	trans = conn->trans;
	if (trans->ops->getmtu)
		return trans->ops->getmtu(trans->param, conn->conn_ref);

	return 512; /* XXX */
}

int ulh_trans_conn_info(uint32_t cid, ulh_trans_info lvl, char *text, int maxtextlen)
{
	struct ulh_trans_conn *conn;
	struct ulh_transport *trans;

	if (!tr)
		return -EFAULT;

	conn = __get_conn(cid);
	if (!conn)
		return -EINVAL;

	trans = conn->trans;
	if (trans->ops->conn_info)
		return trans->ops->conn_info(trans->param, conn->conn_ref,
					     lvl, text, maxtextlen);

	text[0] = '\0';
	return -ENOENT;
}

int ulh_trans_attach(uint32_t cid, itc_mbox_id_t mbox, unsigned long uref)
{
	struct ulh_trans_conn *conn;
	struct ulh_trans_conn_listener *lis, *it;
	int enable = 0;

	if (!tr)
		return -EFAULT;

	conn = __get_conn(cid);
	if (!conn)
		return -EINVAL;

	lis = malloc(sizeof(*lis));
	if (!lis)
		return -ENOMEM;
	dl_list_init(&lis->link);
	lis->mbox = mbox;
	lis->uref = uref;

	pthread_mutex_lock(&conn->lock);
	if (dl_list_empty(&conn->listeners))
		enable = 1;
	dl_list_foreach(it, &conn->listeners, link) {
		if (it->mbox == mbox && it->uref == uref) {
			pthread_mutex_unlock(&conn->lock);
			free(lis);
			return -EALREADY;
		}
	}
	dl_list_insert_tail(&conn->listeners, &lis->link);
	ulh_hold_ref(&conn->ref);
	pthread_mutex_unlock(&conn->lock);

	if (enable && conn->trans->ops->enable)
		conn->trans->ops->enable(conn->trans->param, conn->conn_ref);

	return 0;
}

int ulh_trans_detach(uint32_t cid, itc_mbox_id_t mbox, unsigned long uref)
{
	struct ulh_trans_conn *conn;
	struct ulh_trans_conn_listener *it, *lis = NULL;
	int disable = 0;

	if (!tr)
		return -EFAULT;

	conn = __get_conn(cid);
	if (!conn)
		return -EINVAL;

	pthread_mutex_lock(&conn->lock);
	dl_list_foreach(it, &conn->listeners, link) {
		if (it->mbox == mbox && it->uref == uref) {
			dl_list_remove(&it->link);
			lis = it;
			break;
		}
	}
	if (lis && dl_list_empty(&conn->listeners))
		disable = 1;
	pthread_mutex_unlock(&conn->lock);

	if (disable && conn->trans->ops->disable)
		conn->trans->ops->disable(conn->trans->param, conn->conn_ref);

	if (lis) {
		free(lis);
		ulh_unhold_ref(&conn->ref);
	}
	return 0;
}

int ulh_trans_transmit(uint32_t cid, struct ulh_tbuff *buff)
{
	struct ulh_trans_conn *conn;
	struct ulh_transport *trans;
	int ret;

	if (!tr)
		return -EFAULT;

	conn = __get_conn(cid);
	if (!conn)
		return -EINVAL;

	trans = conn->trans;
	if (!trans->ops->transmit)
		return -EINVAL;

	ret = trans->ops->transmit(trans->param, conn->conn_ref, buff);
	return ret;
}

void ulh_trans_deliver(uint32_t cid, struct ulh_tbuff *tbuff)
{
	struct ulh_trans_conn *conn;
	struct ulh_trans_conn_listener *it;
	union itc_msg *msg;

	if (!tr || cid == ULH_TRANS_NOCONN) {
		ulh_tbuff_free(tbuff);
		return;
	}
	conn = __get_conn(cid);
	if (!conn) {
		ulh_tbuff_free(tbuff);
		return;
	}

	pthread_mutex_lock(&conn->lock);
	dl_list_foreach(it, &conn->listeners, link) {
		msg = itc_alloc(sizeof(struct ulh_transmsg_data),
				ULH_TRANSMSG_DATA);
		msg->data.data = *tbuff;
		msg->data.cid = cid;
		msg->data.uref = it->uref;

		ulh_tbuff_hold(tbuff);
		itc_send(&msg, it->mbox, ITC_MY_MBOX);
	}
	pthread_mutex_unlock(&conn->lock);

	ulh_tbuff_free(tbuff);
}

#define ULH_TRANS_HDRSIZ	32

static void __ulh_tbuff_free(struct ulh_ref *ref)
{
	struct ulh_tbuff_rbuf *dbuf = container_of(ref,
			struct ulh_tbuff_rbuf, ref);
	free(dbuf);
}

int ulh_tbuff_alloc_def(struct ulh_tbuff *tbuff, uint32_t size)
{
	struct ulh_tbuff_rbuf *dbuf;

	dbuf = malloc(sizeof(*dbuf) + size + ULH_TRANS_HDRSIZ);
	if (!dbuf)
		return -ENOMEM;
	ulh_init_ref(&dbuf->ref, 1, __ulh_tbuff_free);

	dbuf->buf = (uint8_t *) (dbuf + 1);
	tbuff->rbuf = dbuf;
	tbuff->data = dbuf->buf + ULH_TRANS_HDRSIZ;
	tbuff->size = size;

	return 0;
}

int ulh_tbuff_alloc(uint32_t cid, uint32_t size,
		struct ulh_tbuff *buff)
{
	struct ulh_trans_conn *conn;
	struct ulh_transport *trans;
	int ret;

	if (!tr)
		return -EFAULT;

	conn = __get_conn(cid);
	if (!conn)
		return -EINVAL;

	trans = conn->trans;
	if (!trans->ops->alloc_buff)
		return ulh_tbuff_alloc_def(buff, size);

	ret = trans->ops->alloc_buff(trans->param, conn->conn_ref,
			size, buff);
	return ret;
}

int ulh_tbuff_pool_init(struct ulh_tbuff_pool *pool, uint32_t limit)
{
	dl_list_init(&pool->f_list);
	pool->limit = limit;
	pool->size = 0;
	pool->pool = NULL;
	if (limit)
		ulh_tbuff_pool_expand(pool);

	return 0;
}

void ulh_tbuff_pool_free(struct ulh_tbuff_pool *pool)
{
	uint32_t i;
	struct ulh_tbuff *tbuff;

	for (i = 0; i < pool->size; i++) {
		tbuff = pool->pool[i];
		if (!tbuff)
			continue;
		free(tbuff);
	}

	free(pool->pool);
}

int ulh_tbuff_pool_expand(struct ulh_tbuff_pool *pool)
{
	uint32_t new_size, i;
	struct ulh_tbuff **new_pool;

	if (pool->size == pool->limit)
		return -1;

	if (pool->size)
		new_size = pool->size * 2;
	else
		new_size = 2;
	if (new_size > pool->limit)
		new_size = pool->limit;

	if (pool->pool)
		new_pool = realloc(pool->pool,
				sizeof(struct ulh_tbuff *) * new_size);
	else
		new_pool = malloc(sizeof(struct ulh_tbuff *) * new_size);

	if (!new_pool)
		return -1;

	pool->pool = new_pool;
	for (i = pool->size; i < new_size; i++) {
		pool->pool[i] = malloc(sizeof(struct ulh_tbuff));
		if (!pool->pool[i]) {
			if (i == pool->size)
				return -1;
			break;
		}
		memset(pool->pool[i], 0, sizeof(struct ulh_tbuff));
		dl_list_insert_head(&pool->f_list, &pool->pool[i]->link);
	}

	pool->size = new_size;
	return 0;
}

int ulh_tbuff_pool_shrink(struct ulh_tbuff_pool *pool)
{
	/* XXX */
	return 0;
}
