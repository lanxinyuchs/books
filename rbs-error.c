/*
 * RBS error reporting framework 
 *
 * Copyright 2012 Ericsson AB
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

#include <linux/kernel.h>
#include <linux/init.h>
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
#include <net/sock.h>

#include <linux/rbs/af_rbs.h>
#include <linux/rbs/rbs-fn.h>
#include <linux/rbs/rbs-error.h>
#include <linux/rbs/rbs-smem.h>

#ifdef CONFIG_RBSERR_ENTRY_SIZE 
#define RBS_ERRENT_SIZE	CONFIG_RBSERR_ENTRY_SIZE
#else
#define RBS_ERRENT_SIZE 256
#endif

#ifndef CONFIG_RBSERR_BUFFER_SIZE
#define CONFIG_RBSERR_BUFFER_SIZE 0x10000
#endif

#define eb_entry_data_size \
	(RBS_ERRENT_SIZE - sizeof(struct rbs_error))

#define RBS_ERROR_MAGIC	0x25223526

struct error_buff_head {
	unsigned int magic;
	int size; /* max. number of entries */
	int entry_size;
	char *buf;
};
#define eb_head_size \
	 ((sizeof(struct error_buff_head) + 3) & ~0x3)

struct error_entry {
	struct list_head entry;
	struct rbs_error *data;
};

struct error_buff_ctrl {
	struct list_head write_queue;
	unsigned int write_drops;
	struct list_head preread_queue;
	atomic_t preread_queue_size;
	struct list_head read_queue;
	/* write_lock protects write_queue and preread_queue */
	raw_spinlock_t writer_lock;
	/* reader_lock protects read_queue */
	struct mutex reader_lock;
	rbs_error_seq_t last_seq;

	struct error_entry *entries;
	int entry_size;
	int recovered;
	rbs_error_seq_t prev_last_seq;
	int proto_avail;
};

#define eb_reader_for_each(_ctrl, _ptr) \
	list_for_each_entry((_ptr), &(_ctrl)->read_queue, entry)

static struct error_buff_ctrl *error_ctrl;

static inline struct error_entry *eb_reserve(struct error_buff_ctrl *eb)
{
	unsigned long flags;
	struct error_entry *entry = NULL;

	raw_spin_lock_irqsave(&eb->writer_lock, flags);
	eb->last_seq++;
	if (!list_empty(&eb->write_queue)) {
		entry = list_first_entry(&eb->write_queue,
				struct error_entry,
				entry);
		entry->data->seq = eb->last_seq;
		entry->data->cpu = smp_processor_id();
		entry->data->ts = sched_clock_cpu(entry->data->cpu);
		entry->data->dropped = eb->write_drops;
		eb->write_drops = 0;
		list_del_init(&entry->entry);
	} else
		eb->write_drops++;
	raw_spin_unlock_irqrestore(&eb->writer_lock, flags);

	return entry;
}

static inline void eb_commit(struct error_buff_ctrl *eb,
		struct error_entry *entry)
{
	unsigned long flags;

	entry->data->flags |= RBS_ERRENT_COMMITTED;

	raw_spin_lock_irqsave(&eb->writer_lock, flags);
	list_add_tail(&entry->entry, &eb->preread_queue);
	raw_spin_unlock_irqrestore(&eb->writer_lock, flags);

	atomic_inc(&eb->preread_queue_size);

	if (likely(eb->proto_avail))
		rbs_proto_oobavail(RBS_PROTO_ERR);
}

static inline void eb_return(struct error_buff_ctrl *eb,
		struct error_entry *entry)
{
	unsigned long flags;

	entry->data->flags = 0;
	entry->data->seq = 0;

	raw_spin_lock_irqsave(&eb->writer_lock, flags);
	list_add_tail(&entry->entry, &eb->write_queue);
	raw_spin_unlock_irqrestore(&eb->writer_lock, flags);
}

static void eb_read_queue_insert(struct error_buff_ctrl *eb,
		struct error_entry *entry)
{
	struct error_entry *p;

	list_for_each_entry_reverse(p, &eb->read_queue, entry) {
		if (entry->data->seq >= p->data->seq) {
			list_add(&entry->entry, &p->entry);
			return;
		}
	}

	list_add(&entry->entry, &eb->read_queue);
}

static void eb_flush_preread_queue(struct error_buff_ctrl *eb)
{
	unsigned long flags;
	struct list_head tmp_head;
	struct error_entry *entry;

	if (!atomic_read(&eb->preread_queue_size))
		return;

	atomic_set(&eb->preread_queue_size, 0);

	INIT_LIST_HEAD(&tmp_head);
	raw_spin_lock_irqsave(&eb->writer_lock, flags);
	list_splice_tail_init(&eb->preread_queue, &tmp_head);
	raw_spin_unlock_irqrestore(&eb->writer_lock, flags);

	while (!list_empty(&tmp_head)) {
		entry = list_first_entry(&tmp_head,
				struct error_entry,
				entry);
		list_del_init(&entry->entry);
		eb_read_queue_insert(eb, entry);
	}
}

static inline void eb_rd_begin(struct error_buff_ctrl *eb)
{
	mutex_lock(&eb->reader_lock);
	eb_flush_preread_queue(eb);
}

static inline void eb_rd_end(struct error_buff_ctrl *eb)
{
	mutex_unlock(&eb->reader_lock);
}

static inline void eb_copy_data(struct error_buff_ctrl *eb,
		struct error_entry *entry,
		void *dst)
{
	memcpy(dst, entry->data, eb->entry_size);
}

static struct error_entry *eb_find_next(struct error_buff_ctrl *eb,
		rbs_error_seq_t seq)
{
	struct error_entry *entry;
	
	seq++;
	list_for_each_entry(entry, &eb->read_queue, entry) {
		if (entry->data->seq >= seq)
			return entry;
	}
	return NULL;
}

static struct error_entry *eb_find(struct error_buff_ctrl *eb,
		rbs_error_seq_t seq)
{
	struct error_entry *entry;
	
	list_for_each_entry(entry, &eb->read_queue, entry) {
		if (entry->data->seq == seq)
			return entry;
	}
	return NULL;
}

static void eb_ack(struct error_buff_ctrl *eb,
		struct error_entry *entry)
{
	list_del_init(&entry->entry);
	eb_return(eb, entry);
}

static void eb_clear(struct error_buff_ctrl *eb)
{
	struct error_entry *entry;

	eb_rd_begin(eb);
	while (!list_empty(&eb->read_queue)) {
		entry = list_first_entry(&eb->read_queue,
				struct error_entry,
				entry);
		eb_ack(eb, entry);
	}
	eb_rd_end(eb);
}

struct error_buff_ctrl *eb_alloc(int nentries, int entry_size,
		char *mem, int recover)
{
	struct error_buff_ctrl *eb;
	struct error_entry *entry;
	int i;

	eb = kmalloc(sizeof(*eb), GFP_KERNEL);
	if (!eb)
		return NULL;
	INIT_LIST_HEAD(&eb->write_queue);
	INIT_LIST_HEAD(&eb->preread_queue);
	INIT_LIST_HEAD(&eb->read_queue);
	raw_spin_lock_init(&eb->writer_lock);
	mutex_init(&eb->reader_lock);
	eb->write_drops = 0;
	eb->last_seq = 0;
	eb->entry_size = entry_size;
	eb->prev_last_seq = 0;
	atomic_set(&eb->preread_queue_size, 0);
	eb->recovered = recover;
	eb->proto_avail = 0;

	if (!nentries)
		return eb;

	eb->entries = kmalloc(sizeof(struct error_entry) * nentries,
			GFP_KERNEL);
	if (!eb->entries) {
		kfree(eb);
		return NULL;
	}

	for (i = 0; i < nentries; i++) {
		entry = &eb->entries[i];
		INIT_LIST_HEAD(&entry->entry);
		entry->data = (struct rbs_error *) (mem + i * eb->entry_size);

		if (!recover) {
			entry->data->seq = 0;
			entry->data->flags = 0;
		}

		if (entry->data->seq != 0 && 
		    (entry->data->flags & RBS_ERRENT_COMMITTED)) {
			list_add_tail(&entry->entry, &eb->preread_queue);
			atomic_inc(&eb->preread_queue_size);
			if (entry->data->seq > eb->last_seq)
				eb->last_seq = entry->data->seq;
		} else
			list_add_tail(&entry->entry, &eb->write_queue);
	}

	eb->prev_last_seq = eb->last_seq;

	return eb;
}

void eb_free(struct error_buff_ctrl *eb)
{
	if (!eb)
		return;
	if (eb->entries)
		kfree(eb->entries);
	kfree(eb);
}

int rbs_error_b(unsigned int source, unsigned int code,
		void *data, unsigned int data_len)
{
	struct rbs_error *err;
	struct error_entry *entry;
	unsigned int len;

	entry = eb_reserve(error_ctrl);
	if (!entry)
		return -EAGAIN;
	err = entry->data;

	len = data_len;
	if (len > eb_entry_data_size) {
		len = eb_entry_data_size;
		err->flags |= RBS_ERRENT_TRUNCATED;
	}

	memcpy((void *) (err + 1), data, len);
	err->size = len;
	err->source = source;
	err->code = code;
	err->flags |= RBS_ERRENT_BINDATA;
	
	eb_commit(error_ctrl, entry);

	return 0;
}
EXPORT_SYMBOL(rbs_error_b);

int rbs_error_t(unsigned int source, unsigned int code,
		const char *fm, ...)
{
	struct rbs_error *err;
	struct error_entry *entry;
	va_list args;

	entry = eb_reserve(error_ctrl);
	if (!entry)
		return -EAGAIN;
	err = entry->data;

	va_start(args, fm);
	err->size = 
		vscnprintf((char *) (err + 1), eb_entry_data_size, fm, args);
	va_end(args);

	err->source = source;
	err->code = code;
	
	eb_commit(error_ctrl, entry);

	return 0;
}
EXPORT_SYMBOL(rbs_error_t);

static void rbs_err_mem_init(void *buf, unsigned int size)
{
	struct error_buff_head *head = buf;

	memset(buf, 0, size);

	head->magic = RBS_ERROR_MAGIC;
	head->size = (size - eb_head_size) / RBS_ERRENT_SIZE;
	head->buf = buf + eb_head_size;
	head->entry_size = RBS_ERRENT_SIZE;
}

static int rbs_err_is_mem_valid(void *buf, unsigned int size)
{
	struct error_buff_head *head = buf;

	if (head->magic != RBS_ERROR_MAGIC)
		return 0;
	if (head->buf != buf + eb_head_size)
		return 0;
	if (head->entry_size != RBS_ERRENT_SIZE)
		return 0;
	if (head->size != ((size - eb_head_size) / RBS_ERRENT_SIZE))
		return 0;

	return 1;
}

static int __init rbs_err_init(void)
{
	void *buf = NULL;
	int nentries;
	unsigned int size;
	int recover = 0;
	int ret;

	size = CONFIG_RBSERR_BUFFER_SIZE;
	nentries = (size - eb_head_size) / RBS_ERRENT_SIZE;

	ret = rbs_smem_alloc_kernel("rbs_error", size, &buf, RBS_SMEM_PERSIST);
	if (ret >= 0)
		recover = rbs_err_is_mem_valid(buf, size);
	else
		buf = kmalloc(size, GFP_KERNEL);
	if (!buf) {
		pr_err("%s: Unable to allocate buffer\n", __func__);
		return -ENOMEM;
	}

	if (!recover)
		rbs_err_mem_init(buf, size);
	
	error_ctrl = eb_alloc(nentries, RBS_ERRENT_SIZE,
			((struct error_buff_head *)buf)->buf, recover);
	if (!error_ctrl) {
		pr_err("Unable to initialize rbs-error\n");
		return -ENOMEM;
	}
	
	rbs_error_t(RBS_ERRSRC_SYS, RBS_SYS_RESTART, "Restart");

	return 0;
}
postcore_initcall(rbs_err_init);

/*
 *
 * User API
 *
 */

struct rbs_error_sock {
	struct error_buff_ctrl *eb;
	rbs_error_seq_t seq;

	char oob_data[RBS_ERRENT_SIZE];
};

static int rbs_err_sock_init(struct rbs_sock *rsk, void *init_param)
{
	struct rbs_error_sock *sdata;

	sdata = kmalloc(sizeof(*sdata), GFP_KERNEL);
	if (!sdata)
		return -ENOMEM;

	sdata->eb = init_param;
	sdata->seq = 0;
	rsk->proto_data = sdata;

	return 0;
}

static void rbs_err_sock_done(struct rbs_sock *rsk)
{
	struct rbs_error_sock *sdata;

	sdata = rsk->proto_data;
	rsk->proto_data = NULL;

	if (!sdata)
		return;

	kfree(sdata);
}

static int rbs_err_sock_oobavail(struct rbs_sock *rsk)
{
	int ret = 0;
	struct rbs_error_sock *sdata = rsk->proto_data;

	eb_rd_begin(sdata->eb);
	if (eb_find_next(sdata->eb, sdata->seq))
		ret = 1;
	eb_rd_end(sdata->eb);

	return ret;
}

static void *rbs_err_sock_oobrecv(struct rbs_sock *rsk, int *size)
{
	struct error_entry *entry;
	struct rbs_error_sock *sdata = rsk->proto_data;
	void *buf = NULL;

	eb_rd_begin(sdata->eb);
	entry = eb_find_next(sdata->eb, sdata->seq);
	if (entry) {
		sdata->seq = entry->data->seq;
		eb_copy_data(sdata->eb, entry, sdata->oob_data);
		buf = sdata->oob_data;
		*size = sdata->eb->entry_size;
	}
	eb_rd_end(sdata->eb);

	return buf;
}

static int rbs_err_sock_ioctl_ack(struct error_buff_ctrl *eb,
		void __user *arg)
{
	struct rbs_error_ack ack;
	struct error_entry *entry;

	if (copy_from_user(&ack, arg, sizeof(ack)))
		return -EFAULT;

	switch (ack.what) {
	case RBS_ERRACK_THIS:
		eb_rd_begin(eb);
		entry = eb_find(eb, ack.seq);
		if (entry)
			eb_ack(eb, entry);
		eb_rd_end(eb);
		if (!entry)
			return -ENOENT;

		return 0;
	case RBS_ERRACK_TO:
		break;
	default:
		return -EINVAL;
	}

	eb_rd_begin(eb);
	while (!list_empty(&eb->read_queue)) {
		entry = list_first_entry(&eb->read_queue,
				struct error_entry,
				entry);
		if (entry->data->seq > ack.seq)
			break;
		eb_ack(eb, entry);
	}
	eb_rd_end(eb);

	return 0;
}

static int rbs_err_sock_ioctl_read(struct rbs_error_sock *sdata,
		void __user *arg)
{
	struct error_entry *entry;
	int out_size;
	struct rbs_error_read *out;
	int ret = 0;
	struct error_buff_ctrl *eb = sdata->eb;

	out_size = sizeof(struct rbs_error_read) + eb_entry_data_size;
	if (!access_ok(VERIFY_WRITE, arg, out_size))
		return -EFAULT;
	out = kmalloc(out_size, GFP_KERNEL);
	if (!out)
		return -ENOMEM;

	if (copy_from_user(out, arg, sizeof(struct rbs_error_read))) {
		kfree(out);
		return -EFAULT;
	}

	eb_rd_begin(eb);
	switch (out->what) {
	case RBS_ERRREAD_THIS:
		entry = eb_find(eb, out->seq);
		break;
	case RBS_ERRREAD_NEXT:
		entry = eb_find_next(eb, out->seq);
		break;
	default:
		ret = -EINVAL;
		goto go_out;
	}

	if (!entry) {
		ret = -ENOENT;
		goto go_out;
	}

	eb_copy_data(eb, entry, &out->error);

	if (copy_to_user(arg, out, out_size)) {
		ret = -EFAULT;
		goto go_out;
	}

	sdata->seq = entry->data->seq;

go_out:
	eb_rd_end(eb);
	if (out)
		kfree(out);
	return ret;
}

static int rbs_err_sock_ioctl_state(struct error_buff_ctrl *eb,
		void __user *arg)
{
	struct rbs_error_state state;

	memset(&state, 0, sizeof(state));
	state.recovered = eb->recovered;
	state.entry_size = eb->entry_size;
	state.restart_seq = eb->prev_last_seq;

	if (copy_to_user(arg, &state, sizeof(state)))
		return -EFAULT;

	return 0;
}

static int rbs_err_sock_ioctl(struct rbs_sock *rsk, unsigned int cmd,
		unsigned long arg)
{
	struct rbs_error_sock *sdata = rsk->proto_data;
	struct error_buff_ctrl *eb = sdata->eb;
	int ret = 0;

	switch (cmd) {
	case RBS_ERRIOC_STATE:
		ret = rbs_err_sock_ioctl_state(eb, (void __user *) arg);
		break;
	case RBS_ERRIOC_ACK:
		ret = rbs_err_sock_ioctl_ack(eb, (void __user *) arg);
		break;
	case RBS_ERRIOC_READ:
		ret = rbs_err_sock_ioctl_read(sdata, (void __user *) arg);
		break;
	default:
		ret = -ENOSYS;
		break;
	}

	return ret;
}

static struct rbs_proto_ops rbs_err_proto_ops = {
	.init = rbs_err_sock_init,
	.done = rbs_err_sock_done,
	.oob_avail = rbs_err_sock_oobavail,
	.oob_recv = rbs_err_sock_oobrecv,
	.ioctl = rbs_err_sock_ioctl,
};

static ssize_t rbs_err_read(struct rbs_fn *fn,
		struct rbs_fn_attribute *attr,
		char *buf)
{
	int offset = 0;
	int count = 20; /* first X entries to avoid buf overflow */
	struct rbs_error *err;
	struct error_entry *entry;

	eb_rd_begin(error_ctrl);
	eb_reader_for_each(error_ctrl, entry) {
		err = entry->data;
		offset += sprintf(buf + offset,
				"%llu %d %llu 0x%.8x 0x%.8x ",
				err->seq, err->cpu,
				err->ts,
				err->source,
				err->code);
		if (err->flags & RBS_ERRENT_BINDATA)
			offset += sprintf(buf + offset,
					"data (%u bytes)\n", err->size);
		else
			offset += sprintf(buf + offset,
					"%s\n", (char *) (err + 1));
		if (!--count)
			break;
	}
	eb_rd_end(error_ctrl);

	return offset;
}

static ssize_t rbs_err_clear(struct rbs_fn *fn,
		struct rbs_fn_attribute *attr,
		const char *buf,
		size_t count)
{
	eb_clear(error_ctrl);
	return count;
}

static ssize_t rbs_err_add(struct rbs_fn *fn,
		struct rbs_fn_attribute *attr,
		const char *buf,
		size_t count)
{
	if (rbs_error_t(0xeaebeced, 0xffeeddcc, buf))
		return -EAGAIN;
	return count;
}

static ssize_t rbs_err_ack(struct rbs_fn *fn,
		struct rbs_fn_attribute *attr,
		const char *buf,
		size_t count)
{
	unsigned int num = 0;
	struct error_entry *entry;

	(void) sscanf(buf, "%u", &num);
	if (!num)
		return count;

	eb_rd_begin(error_ctrl);
	while (!list_empty(&error_ctrl->read_queue)) {
		entry = list_first_entry(&error_ctrl->read_queue,
				struct error_entry,
				entry);
		list_del_init(&entry->entry);
		eb_return(error_ctrl, entry);
		if (!--num)
			break;
	}
	eb_rd_end(error_ctrl);

	return count;
}

static struct rbs_fn_attribute rbs_err_attrs[] = {
	_RBS_FN_ATTR(read, 0444, rbs_err_read, NULL),
	_RBS_FN_ATTR(ack, 0200, NULL, rbs_err_ack),
	_RBS_FN_ATTR(clear, 0200, NULL, rbs_err_clear),
	_RBS_FN_ATTR(add, 0200, NULL, rbs_err_add),
	_RBS_FN_ATTR_NULL,
};

static struct rbs_fn rbs_err_fn = {
	.name = "rbs-error",
	.owner = THIS_MODULE,
	.attrs = rbs_err_attrs,
};

static int __init rbs_err_lateinit(void)
{
	int ret;

	ret = rbs_proto_register(RBS_PROTO_ERR, &rbs_err_proto_ops,
			error_ctrl);
	if (ret) {
		pr_err("%s: unable to register protocol\n", __func__);
		return ret;
	}

	error_ctrl->proto_avail = 1;

	return rbs_fn_add(&rbs_err_fn);
}
late_initcall(rbs_err_lateinit);
