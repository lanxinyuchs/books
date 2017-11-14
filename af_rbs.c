/*
 * AF_RBS sockets
 *
 * Copyright 2011 Ericsson AB
 * Andrey Panteleev <andrey.xx.panteleev@ericsson.com>
 *
 * This program is free software; you can redistribute  it and/or modify it
 * under  the terms of  the GNU General  Public License as published by the
 * Free Software Foundation;  either version 2 of the  License, or (at your
 * option) any later version.
  *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 */

#include <linux/module.h>

#include <linux/capability.h>
#include <linux/kernel.h>
#include <linux/init.h>
#include <linux/sched.h>
#include <linux/errno.h>
#include <linux/string.h>
#include <linux/stat.h>
#include <linux/socket.h>
#include <linux/sockios.h>
#include <linux/net.h>
#include <linux/slab.h>
#include <asm/uaccess.h>
#include <linux/skbuff.h>
#include <linux/types.h>
#include <linux/kfifo.h>
#include <net/sock.h>

#include <linux/rbs/rbs-fn.h>
#include <linux/rbs/af_rbs.h>

struct rbs_mcgrp_member {
	struct list_head entry;
	struct rbs_sock *rsk;
};

struct rbs_proto_mcgrp {
	struct mutex lock;
	struct list_head list;
	int protocol;

	/* message queue */
	struct kfifo queue;
	spinlock_t queue_lock;
	int queue_pending;
	struct work_struct queue_work;
	int (*sk_cb)(struct rbs_sock *, void *);
};

struct rbs_protocol {
	spinlock_t lock;
	int releasing;
	struct rbs_proto_ops *ops;
	void *init_param;
	atomic_t ops_refcnt;
	struct hlist_head sk_list;
	struct mutex sk_list_lock;
	wait_queue_head_t wait;
	wait_queue_head_t oob_wait;
};

static struct rbs_protocol *rbs_proto_table;

static inline struct rbs_sock *rbs_sk(struct sock *sk)
{
	return container_of(sk, struct rbs_sock, sk);
}

static inline struct rbs_protocol *rbs_proto_get(int protocol)
{
	if (protocol < 0 || protocol >= RBS_PROTO_MAX)
		return NULL;
	return &rbs_proto_table[protocol];
}

static inline int __rbs_proto_alive(struct rbs_protocol *proto)
{
	if (!proto->ops || proto->releasing)
		return -EUNATCH;
	return 0;
}

static inline struct rbs_proto_ops *rbs_ops_get(struct rbs_protocol *proto)
{
	struct rbs_proto_ops *ops = NULL;

	if (!proto)
		return NULL;

	spin_lock(&proto->lock);
	if (!proto->releasing)
		ops = proto->ops;
	atomic_inc(&proto->ops_refcnt);
	spin_unlock(&proto->lock);

	return ops;
}

static inline void rbs_ops_put(struct rbs_protocol *proto)
{
	if (!proto)
		return;
	if (atomic_dec_and_test(&proto->ops_refcnt))
		wake_up(&proto->wait);
}

static struct proto rbs_proto = {
	.name	  = "RBS",
	.owner	  = THIS_MODULE,
	.obj_size = sizeof(struct rbs_sock),
};

static int rbs_sock_release(struct socket *sock)
{
	struct sock *sk = sock->sk;
	struct rbs_protocol *proto;
	struct rbs_proto_ops *ops;
	struct rbs_sock *rsk = rbs_sk(sk);

	if (!sk)
		return 0;

	proto = rbs_proto_get(sk->sk_protocol);

	sock_orphan(sk);

	mutex_lock(&proto->sk_list_lock);
	ops = proto->ops;
	if (ops && ops->done && !rsk->dead)
		ops->done(rsk);
	rsk->dead = 1;
	__sk_del_bind_node(sk);
	mutex_unlock(&proto->sk_list_lock);

	sock->sk = NULL;

	skb_queue_purge(&sk->sk_write_queue);

	local_bh_disable();
	sock_prot_inuse_add(sock_net(sk), &rbs_proto, -1);
	local_bh_enable();

	sock_put(sk);
	return 0;
}

/* mostly copy of datagram_poll() */
static unsigned int rbs_sock_poll(struct file *file, struct socket *sock,
		poll_table *wait)
{
	struct sock *sk = sock->sk;
	struct rbs_sock *rsk = rbs_sk(sk);
	struct rbs_protocol *proto = rbs_proto_get(sk->sk_protocol);
	unsigned int mask;
	struct rbs_proto_ops *ops;

	ops = rbs_ops_get(proto);

	sock_poll_wait(file, sk_sleep(sk), wait);
	if (ops && ops->oob_avail)
		sock_poll_wait(file, &proto->oob_wait, wait);
	mask = 0;

	/* exceptional events? */
	if (sk->sk_err || !skb_queue_empty(&sk->sk_error_queue))
		mask |= POLLERR;
	if (sk->sk_shutdown & RCV_SHUTDOWN)
		mask |= POLLRDHUP | POLLIN | POLLRDNORM;
	if (sk->sk_shutdown == SHUTDOWN_MASK)
		mask |= POLLHUP;

	/* readable? */
	if (!skb_queue_empty(&sk->sk_receive_queue))
		mask |= POLLIN | POLLRDNORM;

	/* writable? */
	if (sock_writeable(sk))
		mask |= POLLOUT | POLLWRNORM | POLLWRBAND;
	else
		set_bit(SOCK_ASYNC_NOSPACE, &sk->sk_socket->flags);

	if (ops && ops->oob_avail && ops->oob_avail(rsk))
		mask |= POLLPRI;

	rbs_ops_put(proto);

	return mask;
}

static int rbs_sock_ioctl(struct socket *sock, unsigned int cmd,
		unsigned long arg)
{
	struct sock *sk = sock->sk;
	struct rbs_sock *rsk = rbs_sk(sk);
	struct rbs_protocol *proto = rbs_proto_get(sk->sk_protocol);
	struct rbs_proto_ops *ops;
	int ret = 0;

	if (_IOC_DIR(cmd) & _IOC_READ)
		ret = !access_ok(VERIFY_WRITE, (void __user *)arg,
				_IOC_SIZE(cmd));
	if (!ret && (_IOC_DIR(cmd) & _IOC_WRITE))
		ret = !access_ok(VERIFY_READ, (void __user *)arg,
				_IOC_SIZE(cmd));
	if (ret)
		return -EFAULT;

	if (rsk->dead)
		return -EUNATCH;

	ret = -EOPNOTSUPP;

	ops = rbs_ops_get(proto);
	/* check if unregistration of the protocol is ongoing */
	if (!ops)
		ret = -EUNATCH;
	else if (ops->ioctl)
		ret = ops->ioctl(rsk, cmd, arg);
	rbs_ops_put(proto);

	return ret;
}

#if defined(CONFIG_COMPAT)
static int rbs_sock_compat_ioctl(struct socket *sock, unsigned int cmd,
		unsigned long arg)
{
	struct sock *sk = sock->sk;
	struct rbs_sock *rsk = rbs_sk(sk);
	struct rbs_protocol *proto = rbs_proto_get(sk->sk_protocol);
	struct rbs_proto_ops *ops;
	int ret = 0;

	if (_IOC_DIR(cmd) & _IOC_READ)
		ret = !access_ok(VERIFY_WRITE, (void __user *)arg,
				_IOC_SIZE(cmd));
	if (!ret && (_IOC_DIR(cmd) & _IOC_WRITE))
		ret = !access_ok(VERIFY_READ, (void __user *)arg,
				_IOC_SIZE(cmd));
	if (ret)
		return -EFAULT;

	if (rsk->dead)
		return -EUNATCH;

	ret = -EOPNOTSUPP;

	ops = rbs_ops_get(proto);
	/* check if unregistration of the protocol is ongoing */
	if (!ops)
		ret = -EUNATCH;
	else if (ops->compat_ioctl)
		ret = ops->compat_ioctl(rsk, cmd, arg);
	else if (ops->ioctl)
		ret = ops->ioctl(rsk, cmd, arg);
	rbs_ops_put(proto);

	return ret;
}
#endif /* CONFIG_COMPAT */

static int __rbs_sock_wakeoob(wait_queue_t *wait, unsigned mode, int sync,
				  void *key)
{
	return autoremove_wake_function(wait, mode, sync, key);
}

static int rbs_sock_waitoob(struct sock *sk, struct rbs_proto_ops *ops,
		int *err, long *timeo_p)
{
	int error;
	struct rbs_sock *rsk = rbs_sk(sk);
	struct rbs_protocol *proto = rbs_proto_get(sk->sk_protocol);
	DEFINE_WAIT_FUNC(wait, __rbs_sock_wakeoob);

	prepare_to_wait_exclusive(&proto->oob_wait, &wait,
			TASK_INTERRUPTIBLE);

	error = sock_error(sk);
	if (error)
		goto out_err;
	if (rsk->dead) {
		error = -EUNATCH;
		goto out_err;
	}
	error = __rbs_proto_alive(proto);
	if (error)
		goto out_err;
	if (!ops->oob_avail) {
		error = -EAGAIN;
		goto out_err;
	}
	if (ops->oob_avail(rsk))
		goto out_noerr;

	/* handle signals */
	if (signal_pending(current))
		goto interrupted;

	error = 0;
	*timeo_p = schedule_timeout(*timeo_p);

out:
	finish_wait(&proto->oob_wait, &wait);
	return error;
interrupted:
	error = sock_intr_errno(*timeo_p);
out_err:
	*err = error;
	goto out;
out_noerr:
	*err = 0;
	error = 1;
	goto out;
}

static int rbs_sock_recvoob(struct sock *sk, struct msghdr *msg,
		size_t len, int flags)
{
	int err;
	long timeout;
	struct rbs_sock *rsk = rbs_sk(sk);
	struct rbs_protocol *proto = rbs_proto_get(sk->sk_protocol);
	struct rbs_proto_ops *ops;
	void *oob_data;
	size_t copied = 0;
	int oob_len;

	err = sock_error(sk);
	if (err)
		return err;
	if (rsk->dead)
		return -EUNATCH;

	ops = rbs_ops_get(proto);
	if (!ops)
		err = -EUNATCH;
	else if (!ops->oob_recv)
		err = -EOPNOTSUPP;

	if (err)
		goto out;

	timeout = sock_rcvtimeo(sk, flags & MSG_DONTWAIT);
	sock_hold(sk);
	do {
		oob_data = ops->oob_recv(rsk, &oob_len);
		if (oob_data) {
			copied = oob_len;
			if (copied > len) {
				msg->msg_flags |= MSG_TRUNC;
				copied = len;
			}
			err = memcpy_to_msg(msg, oob_data, (int) copied);
			break;
		}

		err = -EAGAIN;
		if (!timeout)
			goto out;

	} while (!rbs_sock_waitoob(sk, ops, &err, &timeout));

out:
	sock_put(sk);
	rbs_ops_put(proto);
	return err ? : copied;
}
static int rbs_sock_recvmsg(struct socket *sock, struct msghdr *msg,
			    size_t len, int flags)
{
	struct sock *sk = sock->sk;
	struct rbs_sock *rsk = rbs_sk(sk);
	int noblock;
	size_t copied;
	struct sk_buff *skb, *data_skb;
	int err;
	struct sk_buff *frag_iter;

	if (flags & MSG_OOB)
		return rbs_sock_recvoob(sk, msg, len, flags);

	copied = 0;

	/* allow to receive data if unattached but only non-blocking */
	if (rsk->dead)
		noblock = 1;
	else
		noblock = flags & MSG_DONTWAIT;

	skb = skb_recv_datagram(sk, flags, noblock, &err);
	if (skb == NULL) {
		if (rsk->dead && err == -EAGAIN)
			err = -EUNATCH;
		goto out;
	}

	data_skb = skb;

	msg->msg_namelen = 0;

	copied = data_skb->len;
	skb_walk_frags(skb, frag_iter)
		copied += frag_iter->len;

	if (len < copied) {
		msg->msg_flags |= MSG_TRUNC;
		copied = len;
	}
	skb_reset_transport_header(data_skb);
	err = skb_copy_datagram_iter(data_skb, 0, &msg->msg_iter, copied);

	if (flags & MSG_TRUNC)
		copied = data_skb->len;

	skb_free_datagram(sk, skb);
out:
	return err ? : copied;
}

static int rbs_sock_bind(struct socket *sock, struct sockaddr *saddr, int len)
{
	struct sock *sk = sock->sk;
	struct rbs_sock *rsk = rbs_sk(sk);
	struct rbs_protocol *proto = rbs_proto_get(sk->sk_protocol);
	struct rbs_proto_ops *ops;
	int rc = -EOPNOTSUPP;

	if (rsk->dead)
		return -EUNATCH;

	ops = rbs_ops_get(proto);
	if (!ops)
		return -EUNATCH;

	if (!ops->bind)
		goto done;

	rc = ops->bind(rsk, saddr, len);
done:
	rbs_ops_put(proto);
	return rc;
}

static int rbs_sock_sendmsg(struct socket *sock, struct msghdr *msg,
			    size_t len)
{
	struct sock *sk = sock->sk;
	struct rbs_sock *rsk = rbs_sk(sk);
	int noblock = msg->msg_flags & MSG_DONTWAIT;
	struct rbs_protocol *proto = rbs_proto_get(sk->sk_protocol);
	struct rbs_proto_ops *ops;
	int  max_hdr_size = 0;
	struct sk_buff *skb;
	int rc;

	if (rsk->dead)
		return -EUNATCH;

	ops = rbs_ops_get(proto);
	if (!ops)
		return -EUNATCH;

	if (!ops->sendmsg) {
		rc = -EOPNOTSUPP;
		goto out;
	}
	sock_hold(sk);
	if (ops->sockaddr)
		if ((rc = ops->sockaddr(rsk, msg->msg_name,
					msg->msg_namelen, &max_hdr_size)))
			goto done;

	skb = sock_alloc_send_skb(sk, len + max_hdr_size, noblock, &rc);

	if (!skb)
		goto done;

	skb_reserve(skb, max_hdr_size);
	skb_reset_transport_header(skb);
	skb_put(skb, len);

	if ((rc = memcpy_from_msg(skb_transport_header(skb), msg, len))) {
		kfree_skb(skb);
		goto done;
	}

	rc = ops->sendmsg(rsk, msg->msg_name, msg->msg_namelen, skb);
done:
	sock_put(sk);
out:
	rbs_ops_put(proto);
	if (rc < 0)
		pr_warn("%s: fail ret %d\n", __func__, rc);
	return rc;
}

static int rbs_sock_mmap(struct file *file, struct socket *sock,
		struct vm_area_struct *vma)
{
	struct sock *sk = sock->sk;
	struct rbs_sock *rsk = rbs_sk(sk);
	struct rbs_protocol *proto = rbs_proto_get(sk->sk_protocol);
	struct rbs_proto_ops *ops;
	int ret = -EOPNOTSUPP;

	if (rsk->dead)
		return -EUNATCH;

	ops = rbs_ops_get(proto);
	if (!ops)
		return -EUNATCH;

	if (!ops->mmap)
		goto done;

	ret = ops->mmap(rsk, vma);
done:
	rbs_ops_put(proto);
	return ret;
}

static const struct proto_ops rbs_proto_ops = {
	.family =	PF_RBS,
	.owner =	THIS_MODULE,
	.release =	rbs_sock_release,
	.bind =		rbs_sock_bind,
	.connect =	sock_no_connect,
	.socketpair =	sock_no_socketpair,
	.accept =	sock_no_accept,
	.getname =	sock_no_getname,
	.poll =		rbs_sock_poll,
	.ioctl =	rbs_sock_ioctl,
#if defined(CONFIG_COMPAT)
	.compat_ioctl =	rbs_sock_compat_ioctl,
#endif
	.listen =	sock_no_listen,
	.shutdown =	sock_no_shutdown,
	.setsockopt =	sock_no_setsockopt,
	.getsockopt =	sock_no_getsockopt,
	.sendmsg =	rbs_sock_sendmsg,
	.recvmsg =	rbs_sock_recvmsg,
	.mmap =		rbs_sock_mmap,
	.sendpage =	sock_no_sendpage,
};

static void rbs_sock_destruct(struct sock *sk)
{
	skb_queue_purge(&sk->sk_receive_queue);

	if (!sock_flag(sk, SOCK_DEAD)) {
		printk(KERN_ERR "Freeing alive rbs socket %p\n", sk);
		return;
	}

	WARN_ON(atomic_read(&sk->sk_rmem_alloc));
	WARN_ON(atomic_read(&sk->sk_wmem_alloc));
}

static int rbs_sock_create(struct net *net, struct socket *sock, int protocol,
		       int kern)
{
	struct sock *sk;
	struct rbs_sock *rsk;
	int ret = -EUNATCH;
	struct rbs_protocol *proto = rbs_proto_get(protocol);
	struct rbs_proto_ops *ops;

	if (!proto)
		return -EPROTONOSUPPORT;

	sock->state = SS_UNCONNECTED;
	sock->ops = &rbs_proto_ops;

	sk = sk_alloc(net, PF_RBS, GFP_KERNEL, &rbs_proto);
	if (!sk)
		return -ENOMEM;

	sock_init_data(sock, sk);
	sk->sk_destruct = rbs_sock_destruct;
	sk->sk_protocol = protocol;

	rsk = rbs_sk(sk);
	rsk->dead = 0;

	ops = rbs_ops_get(proto);
	if (!ops)
		goto ops_init_fail;
	else if (ops->init) {
		ret = ops->init(rsk, proto->init_param);
		if (ret)
			goto ops_init_fail;
	}
	mutex_lock(&proto->sk_list_lock);
	sk_add_bind_node(sk, &proto->sk_list);
	mutex_unlock(&proto->sk_list_lock);
	rbs_ops_put(proto);

	local_bh_disable();
	sock_prot_inuse_add(net, &rbs_proto, 1);
	local_bh_enable();

	return 0;

ops_init_fail:
	rbs_ops_put(proto);
	sock_orphan(sk);
	sk_free(sk);
	return ret;
}

int rbs_proto_register(int protocol, struct rbs_proto_ops *proto_ops,
		void *init_param)
{
	struct rbs_protocol *proto = rbs_proto_get(protocol);

	if (!proto || !proto_ops)
		return -EINVAL;

	spin_lock(&proto->lock);
	if (proto->ops) {
		spin_unlock(&proto->lock);
		return -EBUSY;
	}
	proto->ops = proto_ops;
	proto->init_param = init_param;
	spin_unlock(&proto->lock);

	return 0;
}
EXPORT_SYMBOL(rbs_proto_register);

void rbs_proto_unregister(int protocol, struct rbs_proto_ops *proto_ops)
{
	struct sock *sk;
	struct rbs_sock *rsk;
	struct rbs_protocol *proto = rbs_proto_get(protocol);
	struct rbs_proto_ops *ops;

	if (!proto || !proto_ops)
		return;

	spin_lock(&proto->lock);
	ops = proto->ops;
	if (ops != proto_ops) {
		spin_unlock(&proto->lock);
		return;
	}
	proto->init_param = NULL;
	proto->releasing = 1;
	spin_unlock(&proto->lock);

	/* wake up oob waiters */
	if (waitqueue_active(&proto->oob_wait))
		wake_up_interruptible_all(&proto->oob_wait);

	/* wait ops to be released */
	wait_event(proto->wait, (!atomic_read(&proto->ops_refcnt)));

	mutex_lock(&proto->sk_list_lock);
	sk_for_each_bound(sk, &proto->sk_list) {
		rsk = rbs_sk(sk);
		if (rsk->dead)
			continue;

		rsk->dead = 1;
		if (ops->done)
			ops->done(rsk);

		sk->sk_err = EUNATCH;
		sk->sk_error_report(sk);
	}
	mutex_unlock(&proto->sk_list_lock);

	spin_lock(&proto->lock);
	proto->releasing = 0;
	proto->ops = NULL;
	spin_unlock(&proto->lock);
}
EXPORT_SYMBOL(rbs_proto_unregister);

int rbs_proto_send(struct rbs_sock *rsk, struct sk_buff *skb)
{
	int ret = -EUNATCH;

	if (!rsk || !skb)
		return -EINVAL;

	if (rsk->dead || sock_flag(&rsk->sk, SOCK_DEAD))
		goto fail;

	ret = sock_queue_rcv_skb(&rsk->sk, skb);
	if (ret)
		goto fail;

	return 0;
fail:
	consume_skb(skb);
	return ret;
}
EXPORT_SYMBOL(rbs_proto_send);


void rbs_proto_oobavail(int protocol)
{
	struct rbs_protocol *proto = rbs_proto_get(protocol);

	if (!proto)
		return;

	/* wake up oob waiters */
	if (waitqueue_active(&proto->oob_wait))
		wake_up_interruptible_all(&proto->oob_wait);
}
EXPORT_SYMBOL(rbs_proto_oobavail);

/*
 * Multicast
 */
static int __rbs_proto_mcgrp_send(struct rbs_proto_mcgrp *mcgrp,
		struct sk_buff *skb,
		int (*filter)(struct rbs_sock *, void *),
		void *fparam)
{
	struct rbs_mcgrp_member *member;
	struct sk_buff *skb2;
	int mcast_member = 1;

	list_for_each_entry(member, &mcgrp->list, entry) {
		if (skb_shared(skb))
			skb2 = skb_clone(skb, GFP_KERNEL);
		else {
			skb2 = skb_get(skb);
			skb_orphan(skb2);
		}
		if (!skb2) {
			pr_err("%s: unable to clone skb\n", __func__);
			consume_skb(skb);
			return -ENOMEM;
		}
		if (filter)
			mcast_member = filter(member->rsk, fparam);
		if (mcast_member)
			rbs_proto_send(member->rsk, skb2);
	}

	consume_skb(skb);

	return 0;
}

static void rbs_proto_mcgrp_work(struct work_struct *work)
{
	int pevents, ret;
	unsigned int rlen;
	struct sk_buff *skb;
	int len;
	void *p;
	struct rbs_proto_mcgrp *mcgrp = container_of(work,
			struct rbs_proto_mcgrp, queue_work);

	spin_lock(&mcgrp->queue_lock);
	pevents = mcgrp->queue_pending;
	mcgrp->queue_pending = 0;
	spin_unlock(&mcgrp->queue_lock);

	for (; pevents > 0; pevents--) {
		spin_lock(&mcgrp->queue_lock);
		rlen = kfifo_out_peek(&mcgrp->queue, &len, sizeof(len));
		spin_unlock(&mcgrp->queue_lock);

		if (!rlen || rlen != sizeof(len) || len <= 0)
			goto fetch_fail;

		len += sizeof(len);

		skb = alloc_skb(len, GFP_KERNEL);
		if (!skb) {
			spin_lock(&mcgrp->queue_lock);
			mcgrp->queue_pending += pevents;
			spin_unlock(&mcgrp->queue_lock);
			return;
		}
		p = skb_put(skb, len);

		rlen = kfifo_out_spinlocked(&mcgrp->queue, p, len,
					    &mcgrp->queue_lock);
		if (!rlen || rlen != len) {
			consume_skb(skb);
			goto fetch_fail;
		}

		skb_pull(skb, sizeof(len));

		ret = rbs_proto_mcgrp_send(mcgrp, skb);
		if (ret)
			pr_warn("%s: unable to delivery event\n", __func__);
	}

	return;

fetch_fail:
	WARN_ONCE(1, "%s: incorrect message queue state\n", __func__);
	spin_lock(&mcgrp->queue_lock);
	kfifo_reset(&mcgrp->queue);
	mcgrp->queue_pending = 0;
	spin_unlock(&mcgrp->queue_lock);
}

struct rbs_proto_mcgrp *rbs_proto_mcgrp_alloc(int protocol, int queue_size)
{
	struct rbs_proto_mcgrp *mcgrp;
	int ret;

	if (!rbs_proto_get(protocol))
		return NULL;

	mcgrp = kmalloc(sizeof(*mcgrp), GFP_KERNEL);
	if (!mcgrp)
		return NULL;

	if (queue_size > 0) {
		ret = kfifo_alloc(&mcgrp->queue, queue_size, GFP_KERNEL);
		if (ret) {
			kfree(mcgrp);
			return NULL;
		}
		mcgrp->queue_pending = 0;
	} else
		mcgrp->queue_pending = -1; /* no queue */

	mcgrp->sk_cb = NULL;
	mutex_init(&mcgrp->lock);
	spin_lock_init(&mcgrp->queue_lock);
	mcgrp->protocol = protocol;

	INIT_WORK(&mcgrp->queue_work, rbs_proto_mcgrp_work);
	INIT_LIST_HEAD(&mcgrp->list);

	return mcgrp;
}
EXPORT_SYMBOL(rbs_proto_mcgrp_alloc);

int rbs_proto_mcgrp_cb_add(struct rbs_proto_mcgrp *mcgrp,
			   int (*sk_cb)(struct rbs_sock *, void *))
{
	if (!mcgrp)
		return -EINVAL;

	mutex_lock(&mcgrp->lock);
	mcgrp->sk_cb = sk_cb;
	mutex_unlock(&mcgrp->lock);

	return 0;
}
EXPORT_SYMBOL(rbs_proto_mcgrp_cb_add);

void rbs_proto_mcgrp_free(struct rbs_proto_mcgrp *mcgrp)
{
	if (!mcgrp)
		return;

	flush_work(&mcgrp->queue_work);
	if (mcgrp->queue_pending != -1)
		kfifo_free(&mcgrp->queue);

	if (!list_empty(&mcgrp->list)) {
		pr_err("%s: mcgrp is not empty\n", __func__);
	}

	kfree(mcgrp);
}
EXPORT_SYMBOL(rbs_proto_mcgrp_free);

static struct rbs_mcgrp_member *__rbs_proto_mcgrp_get(
		struct rbs_proto_mcgrp *mcgrp,
		struct rbs_sock *rsk)
{
	struct rbs_mcgrp_member *member;
	list_for_each_entry(member, &mcgrp->list, entry) {
		if (member->rsk == rsk)
			return member;
	}
	return NULL;
}

int rbs_proto_mcgrp_add(struct rbs_proto_mcgrp *mcgrp, struct rbs_sock *rsk)
{
	struct rbs_mcgrp_member *member;

	if (!mcgrp || !rsk)
		return -EINVAL;

	mutex_lock(&mcgrp->lock);
	if (__rbs_proto_mcgrp_get(mcgrp, rsk)) {
		mutex_unlock(&mcgrp->lock);
		return 0;
	}

	member = kmalloc(sizeof(*member), GFP_KERNEL);
	if (!member)
		return -ENOMEM;

	INIT_LIST_HEAD(&member->entry);
	member->rsk = rsk;
	sock_hold(&rsk->sk);

	list_add_tail(&member->entry, &mcgrp->list);
	mutex_unlock(&mcgrp->lock);

	return 0;
}
EXPORT_SYMBOL(rbs_proto_mcgrp_add);

void rbs_proto_mcgrp_remove(struct rbs_proto_mcgrp *mcgrp,
	struct rbs_sock *rsk)
{
	struct rbs_mcgrp_member *member;

	if (!mcgrp || !rsk)
		return;

	mutex_lock(&mcgrp->lock);
	member = __rbs_proto_mcgrp_get(mcgrp, rsk);
	if (!member)
		goto out;

	list_del(&member->entry);
	sock_put(&member->rsk->sk);

	kfree(member);
out:
	mutex_unlock(&mcgrp->lock);
}
EXPORT_SYMBOL(rbs_proto_mcgrp_remove);

int rbs_proto_mcgrp_send(struct rbs_proto_mcgrp *mcgrp, struct sk_buff *skb)
{
	int ret;

	if (!mcgrp || !skb)
		return -EINVAL;

	mutex_lock(&mcgrp->lock);
	ret = __rbs_proto_mcgrp_send(mcgrp, skb, mcgrp->sk_cb, skb);
	mutex_unlock(&mcgrp->lock);

	return ret;
}
EXPORT_SYMBOL(rbs_proto_mcgrp_send);

int rbs_proto_mcgrp_send_wf(struct rbs_proto_mcgrp *mcgrp, struct sk_buff *skb,
		int (*filter)(struct rbs_sock *, void *),
		void *fparam) 
{
	int ret;

	if (!mcgrp || !skb)
		return -EINVAL;

	mutex_lock(&mcgrp->lock);
	ret = __rbs_proto_mcgrp_send(mcgrp, skb, filter, fparam);
	mutex_unlock(&mcgrp->lock);

	return ret;
}
EXPORT_SYMBOL(rbs_proto_mcgrp_send_wf);

int rbs_proto_mcgrp_post(struct rbs_proto_mcgrp *mcgrp, void *data, int len)
{
	int ret = 0;

	if (!mcgrp || !data || !len || mcgrp->queue_pending == -1)
		return -EINVAL;

	spin_lock(&mcgrp->queue_lock);
	if (kfifo_avail(&mcgrp->queue) >= len + sizeof(len)) {
		kfifo_in(&mcgrp->queue, &len, sizeof(len));
		kfifo_in(&mcgrp->queue, data, len);
		mcgrp->queue_pending++;
	} else
		ret = -ENOMEM;
	spin_unlock(&mcgrp->queue_lock);

	schedule_work(&mcgrp->queue_work);

	return ret;
}
EXPORT_SYMBOL(rbs_proto_mcgrp_post);

static const struct net_proto_family rbs_family_ops = {
	.family = PF_RBS,
	.create = rbs_sock_create,
	.owner	= THIS_MODULE,
};

static int __init rbs_proto_init(void)
{
	int i, ret;
	struct rbs_protocol *proto;

	ret = proto_register(&rbs_proto, 0);
	if (ret)
		panic("%s: unable to register RBS proto\n", __func__);

	rbs_proto_table = kcalloc(RBS_PROTO_MAX, sizeof(*rbs_proto_table),
			GFP_KERNEL);
	if (!rbs_proto_table)
		panic("%s: failed to allocate proto_table\n", __func__);

	for (i = 0; i < RBS_PROTO_MAX; i++) {
		proto = &rbs_proto_table[i];

		proto->ops = NULL;
		proto->releasing = 0;
		atomic_set(&proto->ops_refcnt, 0);
		init_waitqueue_head(&proto->wait);
		init_waitqueue_head(&proto->oob_wait);
		spin_lock_init(&proto->lock);
		mutex_init(&proto->sk_list_lock);
	}

	ret = sock_register(&rbs_family_ops);
	if (ret)
		panic("%s: unable to register rbs family\n", __func__);

	return ret;
}
postcore_initcall(rbs_proto_init);

/*
 * Debug 
 */
static ssize_t rbs_proto_sysfs_protocols(struct rbs_fn *fn,
		struct rbs_fn_attribute *attr, char *buf)
{
	int i, o, off = 0;

	for (i = 0; i < RBS_PROTO_MAX; i++) {
		spin_lock(&rbs_proto_table[i].lock);
		o = rbs_proto_table[i].ops && !rbs_proto_table[i].releasing;
		spin_unlock(&rbs_proto_table[i].lock);

		if (o)
			off += sprintf(buf + off, "%d ", i);
	}

	off += sprintf(buf + off, "\n");

	return off;
}

static struct rbs_fn_attribute rbs_proto_fn_attrs[] = {
        _RBS_FN_ATTR(protocols, S_IRUGO, rbs_proto_sysfs_protocols, NULL),
        _RBS_FN_ATTR_NULL,
};

static struct rbs_fn rbs_proto_fn = {
	.name = "rbs-proto",
	.owner = THIS_MODULE,
	.attrs = rbs_proto_fn_attrs,
};

static int __init rbs_proto_dbg_init(void)
{
	return rbs_fn_add(&rbs_proto_fn);
}
late_initcall(rbs_proto_dbg_init);
