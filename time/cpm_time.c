/*
 * BFN Time driver
 *
 * Copyright 2013-2017 Ericsson AB
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
#include <linux/file.h>
#include <linux/fs.h>
#include <linux/poll.h>
#include <linux/spinlock.h>
#include <linux/time.h>
#include <linux/errno.h>
#include <linux/slab.h>
#include <linux/init.h>
#include <linux/interrupt.h>
#include <linux/anon_inodes.h>
#include <linux/kthread.h>
#include <linux/skbuff.h>
#include <linux/sched.h>
#include <linux/sched/rt.h>
#include <linux/platform_device.h>
#include <linux/if_vlan.h>
#include <linux/udp.h>
#include <linux/ptp_classify.h>
#include <linux/npu/adk_netd_phy_tstamp.h>

#include <linux/rbs/rbs-fn.h>
#include <linux/rbs/af_rbs.h>
#include <linux/rbs/rbs-time.h>

/* PTP generic header size */
#define PTP_HLEN	34

/* single shot timer */
struct rbs_sstimer {
	struct hrtimer timer;
	raw_spinlock_t lock;
	int cancelled;
	struct task_struct *task;
};

/* user socket data */
struct cpm_sk_data {
	struct cpm_time *cpm_time;
};

struct cpm_time_tstable_entry {
	struct list_head link;

	uint32_t msgkey;
	uint64_t tstamp;
	struct sk_buff *skb;

	uint64_t best_before;
};

#define CPMTIME_TSTABLESIZ_PKT		32
#define CPMTIME_TSTABLESIZ_TS		8
#define CPMTIME_TSTABLE_LSPAN_ATC	10240000 /* 40ms */

#define CPMTIME_TSPORT_PTP	(RBS_TIMETSPORT_PTP_EPHY | \
		RBS_TIMETSPORT_PTP_C2I)
#define CPMTIME_TSPORT_C2I	(RBS_TIMETSPORT_PTP_C2I | \
		RBS_TIMETSPORT_PTP_C2I_TP)

struct cpm_time_ts_port {
	uint32_t pidx;
	uint32_t chan;
	uint32_t type;
	uint32_t dto_ntp;
	uint32_t dto_ptp;
	uint32_t ptp_mode;
	struct rbs_time_cpm1_offset *offset_rgn;

	struct rbs_time_ts_port_stats stats;

	struct cpm_sk_data *owner;

	raw_spinlock_t tstable_lock;
	struct list_head tstable_ts, tstable_ts_f;
	struct list_head tstable_pkt, tstable_pkt_f;

	struct cpm_time_tstable_entry *tstable;

	uint32_t c2i_refs;
};

struct cpm_time_c2i_port {
	uint32_t enabled;
	uint32_t pid;
	uint32_t dto;
	uint8_t port_mac[6];
	struct rbs_time_cpm1_offset *offset_rgn;

	uint32_t magic_rnd;

	struct cpm_sk_data *owner;

	uint32_t chan;
	uint32_t tp_chan;

	uint32_t tsp_pidx;
	uint32_t tp_tsp_pidx;
};

#define CPMTIME_TSPORT_MAX	32
#define CPMTIME_C2IPORT_MAX	128

struct cpm_time {
	struct rbs_fn *fn;
	unsigned long shared_data;

	struct mutex tsport_lock;
	struct adk_netd_phy_tstamp_pdata_t *adk_netd_pdata;
	struct cpm_time_ts_port *tsport[CPMTIME_TSPORT_MAX];
	struct cpm_time_c2i_port *c2iport[CPMTIME_C2IPORT_MAX];
	int tsport_free;
};

static struct cpm_time *cpm_time = NULL;

/*
 *
 * SS timer
 *
 */
static enum hrtimer_restart rbs_sstimer_timeout(struct hrtimer *hrtimer)
{
	struct rbs_sstimer *timer = container_of(hrtimer, struct rbs_sstimer,
						 timer);

	if (likely(timer->task))
		wake_up_process(timer->task);

	return HRTIMER_NORESTART;
}

static long rbs_sstimer_ioctl_cancel(struct rbs_sstimer *timer,
		void __user *arg)
{
	raw_spin_lock(&timer->lock);
	timer->cancelled = 1;
	if (timer->task)
		wake_up_process(timer->task);
	raw_spin_unlock(&timer->lock);

	return 0;
}

static long rbs_sstimer_ioctl_wait(struct rbs_sstimer *timer,
		void __user *arg)
{
	u64 ns;
	ktime_t exp_time;
	enum hrtimer_mode mode = HRTIMER_MODE_REL_PINNED;

	if (copy_from_user(&ns, arg, sizeof(ns)))
		return -EFAULT;

	exp_time = ns_to_ktime(ns);

	set_current_state(TASK_INTERRUPTIBLE);

	raw_spin_lock(&timer->lock);
	if (unlikely(timer->cancelled))
		goto cancelled;
	timer->task = current;
	raw_spin_unlock(&timer->lock);

	hrtimer_init(&timer->timer, CLOCK_MONOTONIC, mode);
#if defined(CONFIG_PREEMPT_RT_FULL)
	timer->timer.irqsafe = 1;
#endif
	timer->timer.function = rbs_sstimer_timeout;

	hrtimer_start_range_ns(&timer->timer, exp_time, 0, mode);
	if (!hrtimer_active(&timer->timer))
		goto in_the_past;

	schedule();

in_the_past:
	hrtimer_cancel(&timer->timer);

	raw_spin_lock(&timer->lock);
cancelled:
	timer->cancelled = 0;
	timer->task = NULL;
	raw_spin_unlock(&timer->lock);

	__set_current_state(TASK_RUNNING);
	return 0;
}

static long rbs_sstimerfd_ioctl(struct file *file, unsigned int cmd,
		unsigned long arg)
{
	struct rbs_sstimer *timer = file->private_data;
	long ret = 0;

	switch (cmd) {
	case RBS_SSTIOC_CANCEL:
		return rbs_sstimer_ioctl_cancel(timer, (void __user *) arg);
	case RBS_SSTIOC_WAIT:
		return rbs_sstimer_ioctl_wait(timer, (void __user *) arg);
	default:
		ret = -ENOSYS;
		break;
	}

	return ret;
}

#ifdef CONFIG_COMPAT
static long rbs_sstimerfd_compat_ioctl(struct file *file, unsigned int cmd,
		unsigned long arg)
{
	/* all cmd's are compatible */
	return rbs_sstimerfd_ioctl(file, cmd, arg);
}
#endif /* CONFIG_COMPAT */

static int rbs_sstimerfd_release(struct inode *inode, struct file *file)
{
	struct rbs_sstimer *timer = file->private_data;

	hrtimer_cancel(&timer->timer);
	kfree(timer);
	return 0;
}

static const struct file_operations rbs_sstimerfd_fops = {
	.owner		= THIS_MODULE,
	.release	= rbs_sstimerfd_release,
	.unlocked_ioctl	= rbs_sstimerfd_ioctl,
#ifdef CONFIG_COMPAT
	.compat_ioctl	= rbs_sstimerfd_compat_ioctl,
#endif
};

static int rbs_timer_createsst(void)
{
	struct rbs_sstimer *timer;
	int ret;

	timer = kmalloc(sizeof(*timer), GFP_KERNEL);
	if (!timer)
		return -ENOMEM;

	timer->cancelled = 0;
	raw_spin_lock_init(&timer->lock);
	timer->task = NULL;

	hrtimer_init(&timer->timer, CLOCK_MONOTONIC, HRTIMER_MODE_REL);
#if defined(CONFIG_PREEMPT_RT_FULL)
	timer->timer.irqsafe = 1;
#endif
	timer->timer.function = rbs_sstimer_timeout;

	ret = anon_inode_getfd("[rbstimefd]", &rbs_sstimerfd_fops,
			timer, O_RDWR);
	if (ret < 0) {
		kfree(timer);
		return ret;
	}

	return ret;
}

static int rbs_time_ioctl_createsst(void __user *arg)
{
	int ret;

	ret = rbs_timer_createsst();
	if (ret < 0)
		return ret;

	if (copy_to_user(arg, &ret, sizeof(ret)))
		return -EFAULT;
	return 0;
}

/*
 *
 * TS ports
 *
 */

static inline int64_t __cpm_time_offset_ns(struct rbs_time_cpm1_offset *o)
{
	uint32_t ptr = o->ptr;
	return o->slots[ptr].offset_kernel;
}

static inline int64_t __cpm_time_dto_cpm_offset_ns(
		struct rbs_time_cpm1_offset *o, uint32_t dto)
{
	uint32_t ptr = o->dto[dto].ptr;
	return o->dto[dto].slots[ptr].offset_cpm_ns;
}

static inline int64_t __cpm_time_dto_offset_ns(struct rbs_time_cpm1_offset *o,
		uint32_t dto)
{
	uint32_t ptr = o->dto[dto].ptr;
	return o->dto[dto].slots[ptr].offset_bfn_ns;
}

static inline int32_t __cpm_time_dto_dly_ingress_ns(
		struct rbs_time_cpm1_offset *o, uint32_t dto)
{
	return o->dto[dto].ingress_delay_ns;
}

static inline int32_t __cpm_time_dto_dly_egress_ns(
		struct rbs_time_cpm1_offset *o, uint32_t dto)
{
	return o->dto[dto].egress_delay_ns;
}

static inline __u64 __get_cpm_time(void)
{
	__u64 cval = 0;

#if defined(__arm__)
	__u32 cvall, cvalh;

	asm volatile("mrrc p15, 0, %0, %1, c14" : "=r" (cvall), "=r" (cvalh));

	cval = ((__u64) cvalh << 32) | cvall;

#elif defined (__aarch64__)
	asm volatile("mrs %0, cntpct_el0" : "=r" (cval));

#endif
	return cval;
}

static inline __u64 __get_cpm_time_ns(void)
{
	__u64 t = __get_cpm_time();
	return (t * 1000) >> 8;
}

static inline int __cpm_time_after(uint64_t now, uint64_t deadline)
{
	if ((int64_t) deadline - (int64_t) now < 0)
		return 1;
	return 0;
}

/*
 * Timestamp table handling
 */
static void cpm_time_ts_tbl_add_ts(struct cpm_time_ts_port *tsp,
		uint32_t msgkey, uint64_t ts)
{
	struct list_head *it, *tmp;
	struct cpm_time_tstable_entry *entry;
	struct skb_shared_hwtstamps ssh;
	struct sk_buff *skb = NULL;
	struct sk_buff *skb_free = NULL;
	unsigned long flags;
	uint64_t now;

	tsp->stats.tx_ptp_ts++;

	raw_spin_lock_irqsave(&tsp->tstable_lock, flags);
	now = __get_cpm_time();

	/* check if we have matching packet */
	list_for_each_safe(it, tmp, &tsp->tstable_pkt) {
		entry = list_entry(it, struct cpm_time_tstable_entry, link);

		/* check if packets life has to end, we don't
		 * want to free skb while holding raw spinlock,
		 * free one entry at a time */
		if (__cpm_time_after(now, entry->best_before)) {
			if (!skb_free) {
				list_del(&entry->link);
				skb_free = entry->skb;
				entry->skb = NULL;
				list_add(&entry->link, &tsp->tstable_pkt_f);
				tsp->stats.tx_ptp_pktdrop++;
			}
			continue;
		}

		if (entry->msgkey == msgkey) {
			list_del(&entry->link);
			skb = entry->skb;
			entry->skb = NULL;
			list_add(&entry->link, &tsp->tstable_pkt_f);
			break;
		}
	}

	/* put packet timestamp in the list if no match */
	if (!skb) {
		if (list_empty(&tsp->tstable_ts_f)) {
			if (list_empty(&tsp->tstable_ts))
				goto done;

			/* reuse oldest timestamp entry */
			entry = list_first_entry(&tsp->tstable_ts,
					struct cpm_time_tstable_entry, link);
			list_del(&entry->link);
			tsp->stats.tx_ptp_tsdrop++;
		} else {
			entry = list_first_entry(&tsp->tstable_ts_f,
					struct cpm_time_tstable_entry, link);
			list_del(&entry->link);
		}

		entry->msgkey = msgkey;
		entry->tstamp = ts;
		entry->best_before = now + CPMTIME_TSTABLE_LSPAN_ATC;

		list_add_tail(&entry->link, &tsp->tstable_ts);
	}

done:
	raw_spin_unlock_irqrestore(&tsp->tstable_lock, flags);

	if (skb) {
		tsp->stats.tx_ptp_match++;
		memset(&ssh, 0, sizeof(ssh));
		ssh.hwtstamp = ns_to_ktime(ts);
		skb_tstamp_tx(skb, &ssh);
		dev_kfree_skb_any(skb);
	}

	if (skb_free)
		dev_kfree_skb_any(skb_free);
}

static int cpm_time_ts_tbl_add_pkt(struct cpm_time_ts_port *tsp,
		uint32_t msgkey, struct sk_buff *skb)
{
	struct list_head *it, *tmp;
	struct cpm_time_tstable_entry *entry;
	struct skb_shared_hwtstamps ssh;
	struct sk_buff *skb_free = NULL;
	uint64_t ts, now;
	unsigned long flags;
	int match = 0;
	int ret = 0;

	raw_spin_lock_irqsave(&tsp->tstable_lock, flags);

	now = __get_cpm_time();

	list_for_each_safe(it, tmp, &tsp->tstable_ts) {
		entry = list_entry(it, struct cpm_time_tstable_entry, link);
		if (__cpm_time_after(now, entry->best_before)) {
			list_del(&entry->link);
			list_add(&entry->link, &tsp->tstable_ts_f);
			tsp->stats.tx_ptp_tsdrop++;
			continue;
		}
		if (entry->msgkey == msgkey) {
			match = 1;
			ts = entry->tstamp;
			list_del(&entry->link);
			list_add(&entry->link, &tsp->tstable_ts_f);
			break;
		}
	}

	if (!match) {
		if (list_empty(&tsp->tstable_pkt_f)) {
			if (list_empty(&tsp->tstable_pkt))
				goto done;

			entry = list_first_entry(&tsp->tstable_pkt,
					struct cpm_time_tstable_entry, link);
			list_del(&entry->link);
			skb_free = entry->skb;
			entry->skb = NULL;
			tsp->stats.tx_ptp_pktdrop++;
		} else {
			entry = list_first_entry(&tsp->tstable_pkt_f,
					struct cpm_time_tstable_entry, link);
			list_del(&entry->link);
		}

		entry->msgkey = msgkey;
		entry->best_before =  now + CPMTIME_TSTABLE_LSPAN_ATC;
		entry->skb = skb;
		skb = NULL;

		list_add_tail(&entry->link, &tsp->tstable_pkt);

		/* TODO: spawn periodic job to free obsolete skbs */
	}
done:
	raw_spin_unlock_irqrestore(&tsp->tstable_lock, flags);

	if (match) {
		tsp->stats.tx_ptp_match++;
		memset(&ssh, 0, sizeof(ssh));
		ssh.hwtstamp = ns_to_ktime(ts);
		skb_tstamp_tx(skb, &ssh);
		dev_kfree_skb_any(skb);
		skb = NULL;
	}

	if (unlikely(skb)) {
		/* XXX give adk chance to return CPM packet timestamp,
		 * better than nothing... or?!? */
		ret = -ENOENT;
	}
	if (skb_free)
		dev_kfree_skb_any(skb_free);

	return ret;
}

/*
 * Classify PTP packet
 */
#define CPMTIME_PTPCL_EHPULLED	0x1

static int cpm_time_ptp_classify(struct sk_buff *skb, uint32_t modes,
		uint32_t flags, uint8_t **out_ptp)
{
	__u16 proto, port;
	uint8_t *p;
	uint8_t *ptphdr = NULL;
	uint32_t len = 0;
	int max_vlans = 4;

	if (!modes)
		return -ENOENT;

	if (flags & CPMTIME_PTPCL_EHPULLED) {
		proto = skb->protocol;
		p = (uint8_t *) skb->data;
		len = 0;
	} else {
		proto = ((struct ethhdr *) skb->data)->h_proto;
		p = ((uint8_t *) skb->data) + ETH_HLEN;
		len = ETH_HLEN;
	}

skip_vlan:
	if (proto == cpu_to_be16(ETH_P_8021Q) ||
	    proto == cpu_to_be16(ETH_P_8021AD)) {
		if (unlikely(!pskb_may_pull(skb, len + VLAN_HLEN)))
			return -ENOENT;

		if (!max_vlans--)
			return -ENOENT;

		proto = ((struct vlan_hdr *) p)->h_vlan_encapsulated_proto;
		p += VLAN_HLEN;
		len += VLAN_HLEN;
		goto skip_vlan;
	}

	/* PTP over Ethernet */
	if (proto == cpu_to_be16(ETH_P_1588)) {
		if (!(modes & RBS_TIMEPTPMODE_ETHER))
			return -ENOENT;
		ptphdr = p;
		goto ptp_header;
	}

	/* PTP over IPv4/UDP */
	if (proto == cpu_to_be16(ETH_P_IP)) {
		if (!(modes & RBS_TIMEPTPMODE_UDP4))
			return -ENOENT;
		len += ((struct iphdr *) p)->ihl << 2;
		if (unlikely(!pskb_may_pull(skb, len + UDP_HLEN)))
			return -ENOENT;
		/* UDP */
		if (p[9] != 17)
			return -ENOENT;
		/* only first fragment */
		if (p[7] || (p[6] & 0x1f))
			return -ENOENT;

		/* jump to UDP header */
		p += ((struct iphdr *) p)->ihl << 2;

		/* PTP event UDP port */
		port = ((struct udphdr *) p)->dest;
		if (port != cpu_to_be16(PTP_EV_PORT))
			return -ENOENT;

		ptphdr = p + UDP_HLEN;
		len += UDP_HLEN;
		goto ptp_header;
	}

	/* PTP over IPv6/UDP */
	if (proto == cpu_to_be16(ETH_P_IPV6)) {
		if (!(modes & RBS_TIMEPTPMODE_UDP6))
			return -ENOENT;
		len += IP6_HLEN;
		if (unlikely(!pskb_may_pull(skb, len + UDP_HLEN)))
			return -ENOENT;
		/* UDP */
		if (p[6] != 17)
			return -ENOENT;

		/* jump to UDP header */
		p += IP6_HLEN;

		/* PTP event UDP port */
		port = ((struct udphdr *) p)->dest;
		if (port != cpu_to_be16(PTP_EV_PORT))
			return -ENOENT;

		ptphdr = p + UDP_HLEN;
		len += UDP_HLEN;
		goto ptp_header;
	}

	return -ENOENT;

ptp_header:
	if (unlikely(!pskb_may_pull(skb, len + PTP_HLEN)))
		return -ENOENT;

	/* only PTPv2 Event messages */
	if (ptphdr[0] & 0x8)
		return -ENOENT;

	if (out_ptp)
		*out_ptp = ptphdr;
	return 0;
}

/*
 * Egress PHY callback
 */
static int __cpm_time_ptp_ephy_egress(struct cpm_time_ts_port *tsp,
		struct sk_buff *skb)
{
	uint32_t msgtype, msgseq, msgkey;
	uint8_t *ptphdr;
	int ret;

	ret = cpm_time_ptp_classify(skb, tsp->ptp_mode, 0, &ptphdr);
	if (ret)
		return -ENOENT;

	if ((ptphdr[1] & 0xf) != 2)
		return -ENOENT;

	tsp->stats.tx_ptp++;

	msgtype = ptphdr[0] & 0xf;
	msgseq = be16_to_cpup((const __be16 *) &ptphdr[OFF_PTP_SEQUENCE_ID]);
	msgkey = ((uint32_t) msgtype << 16) | msgseq;

	return cpm_time_ts_tbl_add_pkt(tsp, msgkey, skb);
}

static int __cpm_time_ptp_c2i_egress(struct cpm_time_ts_port *tsp,
		struct sk_buff *skb)
{
	uint8_t *ptphdr;
	const struct ethhdr *ehdr;
	uint32_t msgtype, msgseq, msgkey;
	int ret;

	ret = cpm_time_ptp_classify(skb, tsp->ptp_mode, 0, &ptphdr);
	if (ret)
		return -ENOENT;

	if ((ptphdr[1] & 0xf) != 2)
		return -ENOENT;

	tsp->stats.tx_ptp++;

	ehdr = (struct ethhdr *) skb->data;

	msgtype = ptphdr[0] & 0xf;
	msgseq = be16_to_cpup((const __be16 *) &ptphdr[OFF_PTP_SEQUENCE_ID]);
	msgkey = ((uint32_t) ehdr->h_source[5] << 24) |
			((uint32_t) msgtype << 16) | msgseq;

	return cpm_time_ts_tbl_add_pkt(tsp, msgkey, skb);
}

static int cpm_time_ts_phy_cb(int port, struct sk_buff *skb, void *drv_data)
{
	struct cpm_time_ts_port *tsp = (struct cpm_time_ts_port *) drv_data;

	if (unlikely(!skb || !drv_data))
		return -EINVAL;
	if (unlikely(!(skb_shinfo(skb)->tx_flags & SKBTX_IN_PROGRESS)))
		return -EINVAL;

	if (tsp->type & RBS_TIMETSPORT_PTP_EPHY)
		return __cpm_time_ptp_ephy_egress(tsp, skb);

	if (tsp->type & RBS_TIMETSPORT_PTP_C2I)
		return __cpm_time_ptp_c2i_egress(tsp, skb);

	return -ENODEV;
}

/*
 * Ingress/egress TS correction callback
 */
static int __cpm_time_ptp_ephy_ingress(struct cpm_time_ts_port *tsp,
		unsigned char *ptphdr, u64 *ts_out)
{
	uint32_t mts_ns;
	uint32_t mts_s;
	uint64_t cpm_time_ns;
	uint64_t phy_time;

	/* PTPv2 */
	if ((ptphdr[1] & 0xf) != 2)
		return -ENOENT;

	tsp->stats.rx_ptp++;

	/* PHY sets bit 2 of Reserved(1) field */
	if (unlikely(!(ptphdr[5] & 0x4))) {
		tsp->stats.rx_ptp_no_ets++;
		return -ENOENT;
	}

	cpm_time_ns = __get_cpm_time_ns();

	/* nanosecond part is stored in Reserved(2) */
	mts_ns = be32_to_cpup((const __be32 *) &ptphdr[16]);
	/* 4 bits of seconds part is stored in Reserve(1) */
	mts_s = ptphdr[5] >> 4;

	phy_time = cpm_time_ns + __cpm_time_dto_cpm_offset_ns(
			tsp->offset_rgn, tsp->dto_ptp);
	do_div(phy_time, 1000000000);

	if ((phy_time & 0xf) >= mts_s)
		mts_s += phy_time & ~0xf;
	else
		mts_s += (phy_time & ~0xf) - 16;

	*ts_out = (u64) mts_s * 1000000000 + mts_ns +
		__cpm_time_dto_offset_ns(tsp->offset_rgn, tsp->dto_ptp) -
		__cpm_time_dto_dly_ingress_ns(tsp->offset_rgn, tsp->dto_ptp);
	return 0;
}

static int __cpm_time_ptp_c2i_ingress(struct cpm_time_ts_port *tsp,
		struct sk_buff *skb, unsigned char *ptphdr, u64 *ts_out)
{
	uint32_t mts_ns, mts_s;
	uint64_t ts;
	uint32_t magic, pid, payload_size;
	uint32_t msgtype, msgseq, msgkey;
	struct cpm_time_c2i_port *c2i;
	struct cpm_time_ts_port *c2i_tsp;
	struct ethhdr *ehdr;

	payload_size = skb->len - (ptphdr - skb->data);
	if (unlikely(payload_size < PTP_HLEN + 8 /* TS */))
		return -ENOENT;

	/* check if C2I TX timestamp custom frame */
	if ((ptphdr[1] & 0xf) != 2) {
		if (!(tsp->type & CPMTIME_TSPORT_C2I))
			return -ENOENT;

		magic = be32_to_cpup((const __be32 *) ptphdr);
		if (unlikely((magic & RBS_TIMEC2I_MAGIC_STC_MASK) !=
					RBS_TIMEC2I_MAGIC_STC))
			return -ENOENT;

		pid = RBS_TIMEC2I_MAGIC_PID(magic);
		c2i = cpm_time->c2iport[pid];
		if (unlikely(!c2i || !c2i->enabled))
			return -ENOENT;

		if (unlikely((magic & RBS_TIMEC2I_MAGIC_RND_MASK) !=
				c2i->magic_rnd))
			return 1;

		/* check that both source and destination addresses
		 * are local */
		if (unlikely(!skb_mac_header_was_set(skb)))
			return 1;
		ehdr = (struct ethhdr *) skb_mac_header(skb);
		if (!(ehdr->h_dest[0] & 0x2) || !(ehdr->h_source[0] & 0x2))
			return 1;

		/* jump into real PTP header */
		ptphdr += 4;

		/* fetch timestamp and properties */
		mts_s = be32_to_cpup((const __be32 *) &ptphdr[PTP_HLEN]);
		mts_ns = be32_to_cpup((const __be32 *) &ptphdr[PTP_HLEN + 4]);

		ts = (uint64_t) mts_s * 1000000000 + mts_ns +
			__cpm_time_dto_offset_ns(c2i->offset_rgn, c2i->dto) +
			__cpm_time_dto_dly_egress_ns(c2i->offset_rgn,
					c2i->dto);

		msgtype = ptphdr[0] & 0xf;
		msgseq = be16_to_cpup(
				(const __be16 *) &ptphdr[OFF_PTP_SEQUENCE_ID]);
		msgkey = ((uint32_t) c2i->port_mac[5] << 24) |
			((uint32_t) msgtype << 16) | msgseq;

		if (unlikely(c2i->tsp_pidx >= CPMTIME_TSPORT_MAX))
			return 1;

		c2i_tsp = cpm_time->tsport[c2i->tsp_pidx];
		if (unlikely(!c2i_tsp))
			return 1;

		cpm_time_ts_tbl_add_ts(c2i_tsp, msgkey, ts);
		return 1;
	}

	if (!(tsp->type & RBS_TIMETSPORT_PTP_C2I))
		return -ENOENT;

	tsp->stats.rx_ptp++;

	pid = ptphdr[5] & 0x7f;
	c2i = cpm_time->c2iport[pid];
	if (unlikely(!c2i || !c2i->enabled))
		return -ENOENT;

	/* fetch timestamp */
	mts_s = be32_to_cpup((const __be32 *) &ptphdr[PTP_HLEN]);
	mts_ns = be32_to_cpup((const __be32 *) &ptphdr[PTP_HLEN + 4]);

	*ts_out = (u64) mts_s * 1000000000 + mts_ns +
		__cpm_time_dto_offset_ns(c2i->offset_rgn, c2i->dto) -
		__cpm_time_dto_dly_ingress_ns(c2i->offset_rgn, c2i->dto);

	/* remove timestamp */
	memmove(ptphdr + PTP_HLEN, ptphdr + PTP_HLEN + 8,
			payload_size - PTP_HLEN - 8);
	skb->len -= 8;
	return 0;
}

static int cpm_time_tscorr_cb(int port, int direction, struct sk_buff *skb,
		u64 ts_in, u64 *ts_out, void *drv_data)
{
	struct cpm_time_ts_port *tsp = (struct cpm_time_ts_port *) drv_data;
	uint8_t *ptphdr;
	int ret;

	if (unlikely(!skb || !ts_out || !drv_data))
		return -EINVAL;

	/* egress is only for NTP */
	if (direction == ADK_NETD_TSTAMP_EGRESS)
		goto ntp_corr_ts;

	/* check if PTP is enabled */
	if (!(tsp->type & (CPMTIME_TSPORT_PTP | CPMTIME_TSPORT_C2I)))
		goto ntp_corr_ts;

	ret = cpm_time_ptp_classify(skb, tsp->ptp_mode, CPMTIME_PTPCL_EHPULLED,
			&ptphdr);
	if (ret)
		goto ntp_corr_ts;

	ret = -ENOENT;

	if (tsp->type & RBS_TIMETSPORT_PTP_EPHY) {
		ret = __cpm_time_ptp_ephy_ingress(tsp, ptphdr, ts_out);
		if (unlikely(ret < 0))
			goto ntp_corr_ts;
	} else if (tsp->type & RBS_TIMETSPORT_PTP_C2I) {
		ret = __cpm_time_ptp_c2i_ingress(tsp, skb, ptphdr, ts_out);
		if (unlikely(ret < 0))
			goto ntp_corr_ts;
	} else if (tsp->type & RBS_TIMETSPORT_PTP_C2I_TP) {
		ret = __cpm_time_ptp_c2i_ingress(tsp, skb, ptphdr, ts_out);
		if (unlikely(ret < 0))
			goto ntp_corr_ts;
	}

	return ret;

ntp_corr_ts:
	if (unlikely(!(tsp->type & RBS_TIMETSPORT_NTP) ||
	             !tsp->offset_rgn->status))
		return -ENODEV;

	if (direction == ADK_NETD_TSTAMP_EGRESS) {
		ts_in += __cpm_time_dto_dly_egress_ns(tsp->offset_rgn,
						      tsp->dto_ntp);
		tsp->stats.tx_cpmts++;
	} else {
		ts_in -= __cpm_time_dto_dly_ingress_ns(tsp->offset_rgn,
						       tsp->dto_ntp);
		tsp->stats.rx_cpmts++;
	}
	*ts_out = ts_in + __cpm_time_offset_ns(tsp->offset_rgn);
	return 0;
}

static struct cpm_time_ts_port *__rbs_time_get_tsport(struct cpm_time *c1t,
		uint32_t chan)
{
	int i;

	for (i = 0; i < CPMTIME_TSPORT_MAX; i++) {
		if (!c1t->tsport[i])
			continue;
		if (c1t->tsport[i]->chan == chan)
			return c1t->tsport[i];
	}

	return NULL;
}

static void __rbs_time_tstable_flush(struct cpm_time_ts_port *tsp)
{
	struct list_head t_pkt, t_pkt_f;
	struct list_head t_ts, t_ts_f;
	struct cpm_time_tstable_entry *entry;
	unsigned long flags;

	if (!tsp->tstable)
		return;

	/* detach queues to get clean result */
	raw_spin_lock_irqsave(&tsp->tstable_lock, flags);
	list_replace_init(&tsp->tstable_pkt, &t_pkt);
	list_replace_init(&tsp->tstable_pkt_f, &t_pkt_f);
	list_replace_init(&tsp->tstable_ts, &t_ts);
	list_replace_init(&tsp->tstable_ts_f, &t_ts_f);
	raw_spin_unlock_irqrestore(&tsp->tstable_lock, flags);

	while (!list_empty(&t_pkt)) {
		entry = list_first_entry(&t_pkt,
				struct cpm_time_tstable_entry, link);
		list_del(&entry->link);

		if (entry->skb)
			dev_kfree_skb_any(entry->skb);
		entry->skb = NULL;
		tsp->stats.tx_ptp_pktdrop++;
		list_add(&entry->link, &t_pkt_f);
	}
	while (!list_empty(&t_ts)) {
		entry = list_first_entry(&t_ts,
				struct cpm_time_tstable_entry, link);
		list_del(&entry->link);
		list_add(&entry->link, &t_ts_f);
		tsp->stats.tx_ptp_tsdrop++;
	}

	raw_spin_lock_irqsave(&tsp->tstable_lock, flags);
	list_replace(&t_pkt_f, &tsp->tstable_pkt_f);
	list_replace(&t_ts_f, &tsp->tstable_ts_f);
	raw_spin_unlock_irqrestore(&tsp->tstable_lock, flags);
}

static int __rbs_time_tstable_init(struct cpm_time *c1t,
		struct cpm_time_ts_port *tsp)
{
	int i;

	if (tsp->tstable)
		return 0;

	tsp->tstable = kzalloc(sizeof(*tsp->tstable) *
			(CPMTIME_TSTABLESIZ_PKT +
			 CPMTIME_TSTABLESIZ_TS), GFP_KERNEL);
	if (!tsp->tstable) {
		rbs_fn_info(c1t->fn, "Failed to allocate TS table for "
				"chan %u\n", tsp->chan);
		return -ENOMEM;
	}

	for (i = 0; i < CPMTIME_TSTABLESIZ_PKT; i++)
		list_add(&tsp->tstable[i].link, &tsp->tstable_pkt_f);
	for (i = 0; i < CPMTIME_TSTABLESIZ_TS; i++)
		list_add(&tsp->tstable[CPMTIME_TSTABLESIZ_PKT + i].link,
				&tsp->tstable_ts_f);

	return 0;
}

static int __rbs_time_add_tsport(struct cpm_time *c1t,
		struct cpm_time_ts_port *tsp, uint32_t type)
{
	int ret;
	int a_phy_cb = 0;
	int a_ts_cb = 0;
	uint32_t set = ~tsp->type & type;
	uint32_t old = tsp->type;

	if (!set)
		return 0;

	tsp->type |= set;

	if (!c1t->adk_netd_pdata)
		goto out;

	/* PTP port requires both phycb and tscb start with it */
	if (set & CPMTIME_TSPORT_PTP) {
		/* init&clean TS table first */
		ret = __rbs_time_tstable_init(c1t, tsp);
		if (ret)
			goto fail;
		__rbs_time_tstable_flush(tsp);

		ret = c1t->adk_netd_pdata->register_cb(tsp->chan,
				cpm_time_ts_phy_cb, tsp);
		if (ret)
			goto fail;

		a_phy_cb = 1;

		/* ts_cb could be added already */
		if (!(old & RBS_TIMETSPORT_NTP)) {
			ret = c1t->adk_netd_pdata->register_ts_cb(tsp->chan,
					cpm_time_tscorr_cb, tsp);
			if (ret)
				goto fail;

			a_ts_cb = 1;
		}
	}

	if (set & RBS_TIMETSPORT_NTP) {
		if ((old | set) & CPMTIME_TSPORT_PTP)
			goto out;

		ret = c1t->adk_netd_pdata->register_ts_cb(tsp->chan,
				cpm_time_tscorr_cb, tsp);
		if (ret)
			goto fail;

		a_ts_cb = 1;
	}

	if (set & RBS_TIMETSPORT_PTP_C2I_TP) {
		if ((old | set) & (CPMTIME_TSPORT_PTP | RBS_TIMETSPORT_NTP))
			goto out;

		ret = c1t->adk_netd_pdata->register_ts_cb(tsp->chan,
				cpm_time_tscorr_cb, tsp);
		if (ret)
			goto fail;

		a_ts_cb = 1;
	}

out:
	return 0;

fail:
	if (a_phy_cb)
		c1t->adk_netd_pdata->unregister_cb(tsp->chan, NULL);
	if (a_ts_cb)
		c1t->adk_netd_pdata->unregister_ts_cb(tsp->chan, NULL);

	tsp->type &= ~set;

	rbs_fn_info(c1t->fn, "Failed to add/configure chan %u, set 0x%x\n",
			tsp->chan, set);
	return ret;
}

static void __rbs_time_rm_tsport(struct cpm_time *c1t,
		struct cpm_time_ts_port *tsp, uint32_t type)
{
	if (!type)
		return;

	if (!c1t->adk_netd_pdata) {
		tsp->type &= ~type;
		return;
	}

	if ((type & CPMTIME_TSPORT_PTP) && (tsp->type & CPMTIME_TSPORT_PTP)) {
		c1t->adk_netd_pdata->unregister_cb(tsp->chan, NULL);

		if (!(tsp->type & RBS_TIMETSPORT_NTP))
			c1t->adk_netd_pdata->unregister_ts_cb(tsp->chan, NULL);

		tsp->type &= ~CPMTIME_TSPORT_PTP;

		__rbs_time_tstable_flush(tsp);
	}

	if ((type & RBS_TIMETSPORT_NTP) && (tsp->type & RBS_TIMETSPORT_NTP)) {
		if (!(tsp->type & CPMTIME_TSPORT_PTP))
			c1t->adk_netd_pdata->unregister_ts_cb(tsp->chan, NULL);

		tsp->type &= ~RBS_TIMETSPORT_NTP;
	}

	if ((type & RBS_TIMETSPORT_PTP_C2I_TP) &&
	    (tsp->type & RBS_TIMETSPORT_PTP_C2I_TP)) {
		if (!(tsp->type & (CPMTIME_TSPORT_PTP | RBS_TIMETSPORT_NTP)))
			c1t->adk_netd_pdata->unregister_ts_cb(tsp->chan, NULL);

		tsp->type &= ~RBS_TIMETSPORT_PTP_C2I_TP;
	}
}

static int rbs_time_ioctl_pktstamp(void __user *arg, struct rbs_sock *rsk)
{
	struct cpm_sk_data *skd = rsk->proto_data;
	struct cpm_time_ts_port *tsp;
	struct rbs_time_ts_ioc_pktstamp ioc;
	int ret = copy_from_user(&ioc, arg, sizeof(ioc));
	struct cpm_time *c1t = skd->cpm_time;
	uint32_t msgkey;

	if (unlikely(ret))
		return ret;

	if (unlikely(ioc.pidx >= CPMTIME_TSPORT_MAX))
		return -ENODEV;

	tsp = c1t->tsport[ioc.pidx];
	if (unlikely(!tsp || (c1t->tsport_free & (1 << ioc.pidx))))
		return -ENODEV;

	msgkey = ((uint32_t) (ioc.stamp.ptp_msgtype & 0xf) << 16) |
		(ioc.stamp.ptp_msgseq & 0xffff);

	cpm_time_ts_tbl_add_ts(tsp, msgkey, ioc.stamp.tstamp);
	return 0;
}

static int cpm_time_alloc_port(struct cpm_time *c1t, uint32_t chan,
		struct cpm_time_ts_port **out)
{
	struct cpm_time_ts_port *tsp;

	if (!c1t->tsport_free)
		return -ENOSPC;

	tsp = kzalloc(sizeof(*tsp), GFP_KERNEL);
	if (!tsp) {
		return -ENOMEM;
	}

	tsp->chan = chan;
	tsp->offset_rgn = (struct rbs_time_cpm1_offset *)
		c1t->shared_data;
	tsp->type = 0;
	tsp->owner = NULL;
	INIT_LIST_HEAD(&tsp->tstable_pkt_f);
	INIT_LIST_HEAD(&tsp->tstable_pkt);
	INIT_LIST_HEAD(&tsp->tstable_ts_f);
	INIT_LIST_HEAD(&tsp->tstable_ts);

	tsp->pidx = ffs(c1t->tsport_free) - 1;
	c1t->tsport_free &= ~(1 << tsp->pidx);
	raw_spin_lock_init(&tsp->tstable_lock);

	mb();
	c1t->tsport[tsp->pidx] = tsp;

	if (out)
		*out =tsp;
	return 0;
}

static int cpm_time_add_port(struct cpm_time *c1t, uint32_t chan,
		uint32_t type, uint32_t dto, uint32_t ptp_mode,
		struct cpm_sk_data *owner,
		uint32_t *pidx_out)
{
	struct cpm_time_ts_port *tsp;
	int owner_set = 0;
	int ret = 0;

	tsp = __rbs_time_get_tsport(c1t, chan);

	/* return directly if no change, but set valid port index */
	if (!type || (tsp && (tsp->type & type)))
		goto done;
	if (!type && !tsp)
		return -ENODEV;

	switch (type) {
	case RBS_TIMETSPORT_NTP:
		if (dto >= RBS_TIME_DTOSIZ)
			return -EINVAL;
		break;
	case RBS_TIMETSPORT_PTP_EPHY:
		if (dto >= RBS_TIME_DTOSIZ)
			return -EINVAL;
		if (tsp && (tsp->type & (RBS_TIMETSPORT_PTP_C2I |
						RBS_TIMETSPORT_PTP_C2I_TP)))
			return -EBUSY;
		break;
	case RBS_TIMETSPORT_PTP_C2I:
	case RBS_TIMETSPORT_PTP_C2I_TP:
		if (tsp && (tsp->type & RBS_TIMETSPORT_PTP_EPHY))
			return -EBUSY;
		break;
	default:
		return -EINVAL;
	}

	if (!tsp) {
		ret = cpm_time_alloc_port(c1t, chan, &tsp);
		if (ret)
			return ret;
	}

	/* set/check ownership */
	if (!tsp->owner) {
		tsp->owner = owner;
		owner_set = 1;
	}
	if (tsp->owner != owner)
		return -EPERM;

	if (type & (CPMTIME_TSPORT_PTP | CPMTIME_TSPORT_C2I))
		tsp->ptp_mode = ptp_mode;

	if (type & RBS_TIMETSPORT_PTP_EPHY)
		tsp->dto_ptp = dto;
	if (type & RBS_TIMETSPORT_NTP)
		tsp->dto_ntp = dto;

	ret = __rbs_time_add_tsport(c1t, tsp, type);
	if (ret && owner_set)
		tsp->owner = NULL;

done:
	if (pidx_out && tsp)
		*pidx_out = tsp->pidx;
	if (!ret && tsp && (type & CPMTIME_TSPORT_C2I))
		tsp->c2i_refs++;

	return ret;
}

static int cpm_time_rm_port(struct cpm_time *c1t, uint32_t chan,
		uint32_t type, struct cpm_sk_data *owner)
{
	struct cpm_time_ts_port *tsp;

	tsp = __rbs_time_get_tsport(c1t, chan);
	if (!tsp)
		return -ENODEV;
	if (tsp->owner != owner)
		return -EPERM;

	if (type & CPMTIME_TSPORT_C2I) {
		if (--tsp->c2i_refs)
			type &= ~CPMTIME_TSPORT_C2I;
	}

	__rbs_time_rm_tsport(c1t, tsp, type);

	if (!tsp->type) {
		memset(&tsp->stats, 0, sizeof(tsp->stats));
		tsp->owner = NULL;
	}

	return 0;
}

static int rbs_time_ioctl_add_port(void __user *arg, struct rbs_sock *rsk)
{
	struct cpm_sk_data *skd = rsk->proto_data;
	struct rbs_time_ts_ioc_add_port ioc;
	struct cpm_time *c1t = skd->cpm_time;
	int ret = copy_from_user(&ioc, arg, sizeof(ioc));

	if (ret)
		return ret;
	if (ioc.type & CPMTIME_TSPORT_C2I)
		return -EINVAL;

	mutex_lock(&c1t->tsport_lock);
	ret = cpm_time_add_port(c1t, ioc.chan, ioc.type,
			ioc.offset_location, RBS_TIMEPTPMODE_ETHER, skd,
			&ioc.pidx);
	mutex_unlock(&c1t->tsport_lock);

	if (ret)
		return ret;

	if (copy_to_user(arg, &ioc, sizeof(ioc)))
		return -EFAULT;
	return 0;
}

static int rbs_time_ioctl_add_port_e(void __user *arg, struct rbs_sock *rsk)
{
	struct cpm_sk_data *skd = rsk->proto_data;
	struct rbs_time_ts_ioc_add_port_e ioc;
	struct cpm_time *c1t = skd->cpm_time;
	int ret = copy_from_user(&ioc, arg, sizeof(ioc));

	if (ret)
		return ret;
	if (ioc.type & CPMTIME_TSPORT_C2I)
		return -EINVAL;

	mutex_lock(&c1t->tsport_lock);
	ret = cpm_time_add_port(c1t, ioc.chan, ioc.type,
			ioc.offset_location, ioc.ptp_mode, skd,
			&ioc.pidx);
	mutex_unlock(&c1t->tsport_lock);

	if (ret)
		return ret;

	if (copy_to_user(arg, &ioc, sizeof(ioc)))
		return -EFAULT;
	return 0;
}

static int rbs_time_ioctl_rm_port(void __user *arg, struct rbs_sock *rsk)
{
	struct cpm_sk_data *skd = rsk->proto_data;
	struct cpm_time *c1t = skd->cpm_time;
	struct rbs_time_ts_ioc_rm_port ioc;
	int ret = copy_from_user(&ioc, arg, sizeof(ioc));

	if (ret)
		return ret;

	mutex_lock(&c1t->tsport_lock);
	ret = cpm_time_rm_port(c1t, ioc.chan, ioc.type, skd);
	mutex_unlock(&c1t->tsport_lock);

	return ret;
}

static int rbs_time_ioctl_get_stats(void __user *arg, struct rbs_sock *rsk)
{
	struct cpm_sk_data *skd = rsk->proto_data;
	struct cpm_time_ts_port *tsp;
	struct rbs_time_ts_ioc_getstats ioc;
	int ret = copy_from_user(&ioc, arg, sizeof(ioc));
	struct cpm_time *c1t = skd->cpm_time;

	if (ret)
		return ret;

	mutex_lock(&c1t->tsport_lock);
	tsp = __rbs_time_get_tsport(c1t, ioc.chan);
	if (tsp)
		memcpy(&ioc.stats, &tsp->stats, sizeof(ioc.stats));
	else
		ret = -ENODEV;
	mutex_unlock(&c1t->tsport_lock);

	if (ret)
		return ret;

	if (copy_to_user(arg, &ioc, sizeof(ioc)))
		return -EFAULT;
	return 0;
}

static int rbs_time_ioctl_add_c2i_port(void __user *arg, struct rbs_sock *rsk)
{
	struct cpm_sk_data *skd = rsk->proto_data;
	struct rbs_time_ts_ioc_add_c2i_port ioc;
	struct cpm_time_c2i_port *c2i;
	struct cpm_time *c1t = skd->cpm_time;
	int ret = copy_from_user(&ioc, arg, sizeof(ioc));

	if (ret)
		return ret;

	if (ioc.port_id >= CPMTIME_C2IPORT_MAX)
		return -EINVAL;
	if (ioc.offset_location >= RBS_TIME_DTOSIZ)
		return -EINVAL;

	mutex_lock(&c1t->tsport_lock);
	c2i = c1t->c2iport[ioc.port_id];
	if (!c2i) {
		c2i = kzalloc(sizeof(*c2i), GFP_KERNEL);
		if (!c2i) {
			mutex_unlock(&c1t->tsport_lock);
			return -ENOMEM;
		}

		c2i->pid = ioc.port_id;
		c2i->offset_rgn = (struct rbs_time_cpm1_offset *)
			c1t->shared_data;

		c1t->c2iport[ioc.port_id] = c2i;
	}

	if (c2i->enabled) {
		mutex_unlock(&c1t->tsport_lock);
		return 0;
	}

	c2i->dto = ioc.offset_location;
	memcpy(c2i->port_mac, ioc.port_mac, 6);
	c2i->magic_rnd = ioc.magic_rnd;
	c2i->chan = ioc.mac_chan;
	c2i->tp_chan = ioc.tp_mac_chan;

	ret = cpm_time_add_port(c1t, c2i->chan, RBS_TIMETSPORT_PTP_C2I,
			~0, RBS_TIMEPTPMODE_ETHER, skd, &c2i->tsp_pidx);
	if (ret) {
		mutex_unlock(&c1t->tsport_lock);
		return ret;
	}

	if (ioc.tp_mac_chan != (uint32_t) ~0) {
		ret = cpm_time_add_port(c1t, c2i->tp_chan,
				RBS_TIMETSPORT_PTP_C2I_TP, ~0,
				RBS_TIMEPTPMODE_ETHER, skd,
				&c2i->tp_tsp_pidx);
		if (ret) {
			cpm_time_rm_port(c1t, c2i->chan,
					RBS_TIMETSPORT_PTP_C2I, skd);
			mutex_unlock(&c1t->tsport_lock);
			return ret;
		}
	} else
		c2i->tp_tsp_pidx = ~0;

	c2i->owner = skd;
	c2i->enabled = 1;
	mutex_unlock(&c1t->tsport_lock);

	if (copy_to_user(arg, &ioc, sizeof(ioc)))
		return -EFAULT;
	return 0;
}

static int rbs_time_ioctl_rm_c2i_port(void __user *arg, struct rbs_sock *rsk)
{
	struct cpm_sk_data *skd = rsk->proto_data;
	struct rbs_time_ts_ioc_add_c2i_port ioc;
	struct cpm_time_c2i_port *c2i;
	struct cpm_time *c1t = skd->cpm_time;
	int ret = copy_from_user(&ioc, arg, sizeof(ioc));

	if (ret)
		return ret;

	if (ioc.port_id >= CPMTIME_C2IPORT_MAX)
		return -EINVAL;

	mutex_lock(&c1t->tsport_lock);
	c2i = c1t->c2iport[ioc.port_id];
	if (!c2i || !c2i->enabled) {
		mutex_unlock(&c1t->tsport_lock);
		return -ENODEV;
	}

	cpm_time_rm_port(c1t, c2i->chan, RBS_TIMETSPORT_PTP_C2I, skd);
	cpm_time_rm_port(c1t, c2i->tp_chan, RBS_TIMETSPORT_PTP_C2I_TP,
				skd);

	c2i->enabled = 0;
	mutex_unlock(&c1t->tsport_lock);

	return 0;
}

static int rbs_time_ioctl_set_ptpmode(void __user *arg, struct rbs_sock *rsk)
{
	struct cpm_sk_data *skd = rsk->proto_data;
	struct rbs_time_ts_ioc_set_ptpmode ioc;
	struct cpm_time *c1t = skd->cpm_time;
	struct cpm_time_ts_port *tsp;
	int ret = copy_from_user(&ioc, arg, sizeof(ioc));

	if (ret)
		return ret;

	mutex_lock(&c1t->tsport_lock);
	tsp = __rbs_time_get_tsport(c1t, ioc.chan);
	if (!tsp) {
		mutex_unlock(&c1t->tsport_lock);
		return -ENODEV;
	}
	if (tsp->owner != skd) {
		mutex_unlock(&c1t->tsport_lock);
		return -EPERM;
	}

	tsp->ptp_mode = ioc.ptp_mode;
	mutex_unlock(&c1t->tsport_lock);

	return ret;
}

static int cpm_time_sock_init(struct rbs_sock *rsk, void *init_param)
{
	struct cpm_sk_data *skd = kzalloc(sizeof(*skd), GFP_KERNEL);

	if (!skd)
		return -ENOMEM;

	skd->cpm_time = (struct cpm_time *) init_param;
	rsk->proto_data = skd;

	return 0;
}

static void cpm_time_sock_done(struct rbs_sock *rsk)
{
	struct cpm_sk_data *skd = (struct cpm_sk_data *) rsk->proto_data;
	struct cpm_time *c1t = skd->cpm_time;
	struct cpm_time_c2i_port *c2i;
	int i;

	if (!skd)
		return;

	mutex_lock(&c1t->tsport_lock);
	for (i = 0; i < CPMTIME_C2IPORT_MAX; i++) {
		if (!c1t->c2iport[i] ||
		    c1t->c2iport[i]->owner != skd)
			continue;
		c2i = c1t->c2iport[i];
		if (!c2i->enabled)
			continue;

		cpm_time_rm_port(c1t, c2i->chan, RBS_TIMETSPORT_PTP_C2I, skd);
		cpm_time_rm_port(c1t, c2i->tp_chan, RBS_TIMETSPORT_PTP_C2I_TP,
				skd);

		c1t->c2iport[i]->enabled = 0;
	}

	for (i = 0; i < CPMTIME_TSPORT_MAX; i++) {
		if (!c1t->tsport[i] ||
		    c1t->tsport[i]->owner != skd)
			continue;

		__rbs_time_rm_tsport(c1t, c1t->tsport[i], ~0);

		memset(&c1t->tsport[i]->stats, 0,
				sizeof(struct rbs_time_ts_port_stats));
		c1t->tsport[i]->owner = NULL;
	}
	mutex_unlock(&c1t->tsport_lock);

	kfree(skd);
}

static int cpm_time_sock_ioctl(struct rbs_sock *rsk, unsigned int cmd,
		unsigned long arg)
{
	int ret = 0;

	switch (cmd) {
	case RBS_TIMEIOC_CREATESST:
		return rbs_time_ioctl_createsst((void __user *) arg);
	case RBS_TIMEIOC_ADD_PORT:
		return rbs_time_ioctl_add_port((void __user *) arg, rsk);
	case RBS_TIMEIOC_ADD_PORT_E:
		return rbs_time_ioctl_add_port_e((void __user *) arg, rsk);
	case RBS_TIMEIOC_RM_PORT:
		return rbs_time_ioctl_rm_port((void __user *) arg, rsk);
	case RBS_TIMEIOC_PKTSTAMP:
		return rbs_time_ioctl_pktstamp((void __user *) arg, rsk);
	case RBS_TIMEIOC_GETSTATS:
		return rbs_time_ioctl_get_stats((void __user *) arg, rsk);
	case RBS_TIMEIOC_ADD_C2I_PORT:
		return rbs_time_ioctl_add_c2i_port((void __user *) arg, rsk);
	case RBS_TIMEIOC_RM_C2I_PORT:
		return rbs_time_ioctl_rm_c2i_port((void __user *) arg, rsk);
	case RBS_TIMEIOC_SET_PTPMODE:
		return rbs_time_ioctl_set_ptpmode((void __user *) arg, rsk);
	default:
		ret = -ENOSYS;
		break;
	}
	return ret;
}

static void cpm_time_vma_open(struct vm_area_struct *vm)
{
	struct cpm_sk_data *sk_data = vm->vm_private_data;

	if (!try_module_get(THIS_MODULE))
		rbs_fn_err(sk_data->cpm_time->fn,
				"try_module_get() failure\n");
}

static void cpm_time_vma_close(struct vm_area_struct *vm)
{
	module_put(THIS_MODULE);
}

static const struct vm_operations_struct cpm_time_vm_ops = {
	.open = cpm_time_vma_open,
	.close = cpm_time_vma_close,
};

static int cpm_time_sock_mmap(struct rbs_sock *rsk, struct vm_area_struct *vm)
{
	struct cpm_sk_data *sk_data = rsk->proto_data;
	int ret;

	if ((vm->vm_end - vm->vm_start) > PAGE_SIZE)
		return -EINVAL;

	vm->vm_private_data = sk_data;
	vm->vm_ops = &cpm_time_vm_ops;
	ret = remap_pfn_range(vm, vm->vm_start,
			__pa(sk_data->cpm_time->shared_data) >> PAGE_SHIFT,
			vm->vm_end - vm->vm_start, vm->vm_page_prot);
	if (!ret)
		cpm_time_vma_open(vm);

	return ret;
}

static struct rbs_proto_ops cpm_time_proto_ops = {
	.init = cpm_time_sock_init,
	.done = cpm_time_sock_done,
	.ioctl = cpm_time_sock_ioctl,
	.mmap = cpm_time_sock_mmap,
};

static ssize_t dbg_tsports_show(struct rbs_fn *fn,
				struct rbs_fn_attribute *attr,
				char *buf)
{
	struct cpm_time *c1t = rbs_fn_getdata(fn);
	int i;
	ssize_t len = 0;

	mutex_lock(&c1t->tsport_lock);
	for (i = 0; i < CPMTIME_TSPORT_MAX; i++) {
		if (!c1t->tsport[i])
			continue;

		len += sprintf(buf + len, "%u %u 0x%x %u\n",
				c1t->tsport[i]->pidx, c1t->tsport[i]->chan,
				c1t->tsport[i]->type,
				c1t->tsport[i]->c2i_refs);
	}
	mutex_unlock(&c1t->tsport_lock);

	return len;
}

static ssize_t dbg_c2iports_show(struct rbs_fn *fn,
				struct rbs_fn_attribute *attr,
				char *buf)
{
	struct cpm_time *c1t = rbs_fn_getdata(fn);
	int i;
	ssize_t len = 0;

	mutex_lock(&c1t->tsport_lock);
	for (i = 0; i < CPMTIME_C2IPORT_MAX; i++) {
		if (!c1t->c2iport[i])
			continue;
		if (!c1t->c2iport[i]->enabled)
			continue;

		len += sprintf(buf + len, "%u %u %u\n",
				c1t->c2iport[i]->pid, c1t->c2iport[i]->chan,
				c1t->c2iport[i]->tp_chan);
	}
	mutex_unlock(&c1t->tsport_lock);

	return len;
}

static int cpm_time_ts_probe(struct platform_device *pdev)
{
	struct device *dev = &pdev->dev;
	int i, ret;
	uint32_t cheat;

	mutex_lock(&cpm_time->tsport_lock);
	cpm_time->adk_netd_pdata =
		(struct adk_netd_phy_tstamp_pdata_t *)dev_get_platdata(dev);

	/* register ports if any */
	for (i = 0; i < CPMTIME_TSPORT_MAX; i++) {
		if (!cpm_time->tsport[i])
			continue;
		if (!cpm_time->tsport[i]->type)
			continue;

		cheat = cpm_time->tsport[i]->type;
		cpm_time->tsport[i]->type = 0;
		ret = __rbs_time_add_tsport(cpm_time, cpm_time->tsport[i],
				cheat);
		if (ret) {
			rbs_fn_err(cpm_time->fn, "Failed to add "
					"TS port %u when adk (re-)appeared, "
					"%d\n", cpm_time->tsport[i]->chan,
					ret);
		}
	}
	mutex_unlock(&cpm_time->tsport_lock);

	return 0;
}

static int cpm_time_ts_remove(struct platform_device *pdev)
{
	mutex_lock(&cpm_time->tsport_lock);
	cpm_time->adk_netd_pdata = NULL;
	mutex_unlock(&cpm_time->tsport_lock);
	return 0;
}

static const struct platform_device_id cpm_time_ts_id_table[] = {
	{"adk_netd_phy_tstamp" },
	{},
};

static struct platform_driver cpm_time_ts_driver = {
	.driver = {
		.name = "cpm_time_ts",
	},
	.id_table = cpm_time_ts_id_table,
	.probe = cpm_time_ts_probe,
	.remove = cpm_time_ts_remove,
};

static int cpm_time_fn_setup(struct rbs_fn *fn)
{
	struct cpm_time *cpm_time = rbs_fn_getdata(fn);
	int ret;

	cpm_time->adk_netd_pdata = NULL;
	for (ret = 0; ret < CPMTIME_TSPORT_MAX; ret++) {
		cpm_time->tsport[ret] = NULL;
		cpm_time->tsport_free |= 1 << ret;
	}
	for (ret = 0; ret < CPMTIME_C2IPORT_MAX; ret++)
		cpm_time->c2iport[ret] = NULL;

	platform_driver_register(&cpm_time_ts_driver);

	ret = rbs_proto_register(RBS_PROTO_TIME, &cpm_time_proto_ops,
			cpm_time);
	if (ret) {
		rbs_fn_err(fn, "unable to register protocol\n");
		platform_driver_unregister(&cpm_time_ts_driver);
		return ret;
	}

	return 0;
}

static void cpm_time_fn_release(struct rbs_fn *fn)
{
	struct cpm_time *cpm_time = rbs_fn_getdata(fn);
	int i;

	rbs_proto_unregister(RBS_PROTO_TIME, &cpm_time_proto_ops);

	if (!cpm_time)
		return;

	mutex_lock(&cpm_time->tsport_lock);
	/* disable C2I ports */
	for (i = 0; i < CPMTIME_C2IPORT_MAX; i++) {
		if (!cpm_time->c2iport[i])
			continue;
		if (!cpm_time->c2iport[i]->enabled)
			continue;

		cpm_time_rm_port(cpm_time, cpm_time->c2iport[i]->chan,
				RBS_TIMETSPORT_PTP_C2I,
				cpm_time->c2iport[i]->owner);
		cpm_time_rm_port(cpm_time, cpm_time->c2iport[i]->tp_chan,
				RBS_TIMETSPORT_PTP_C2I_TP,
				cpm_time->c2iport[i]->owner);

		cpm_time->c2iport[i]->enabled = 0;
	}

	/* remove and free TS ports */
	for (i = 0; i < CPMTIME_TSPORT_MAX; i++) {
		if (!cpm_time->tsport[i])
			continue;

		__rbs_time_rm_tsport(cpm_time, cpm_time->tsport[i], ~0);
		if (cpm_time->tsport[i]->tstable) {
			__rbs_time_tstable_flush(cpm_time->tsport[i]);
			kfree(cpm_time->tsport[i]->tstable);
		}
		kfree(cpm_time->tsport[i]);

		cpm_time->tsport[i] = NULL;
	}

	/* free C2I ports */
	for (i = 0; i < CPMTIME_C2IPORT_MAX; i++) {
		if (cpm_time->c2iport[i])
			kfree(cpm_time->c2iport[i]);
		cpm_time->c2iport[i] = NULL;
	}
	mutex_unlock(&cpm_time->tsport_lock);

	platform_driver_unregister(&cpm_time_ts_driver);
}


static struct rbs_fn_ops cpm_time_fn_ops = {
	.setup = cpm_time_fn_setup,
	.release = cpm_time_fn_release,
};

static struct rbs_fn_attribute rbs_time_attr[] = {
	_RBS_FN_ATTR(tsports, S_IRUGO, dbg_tsports_show, NULL),
	_RBS_FN_ATTR(c2iports, S_IRUGO, dbg_c2iports_show, NULL),
	_RBS_FN_ATTR_NULL,
};

static struct rbs_fn cpm_time_fn = {
	.name = "rbs-time",
	.owner = THIS_MODULE,
	.ops = &cpm_time_fn_ops,
	.attrs = rbs_time_attr,
};

static __init int cpm_time_init(void)
{
	if (cpm_time)
		return -EBUSY;

	cpm_time = kzalloc(sizeof(*cpm_time), GFP_KERNEL);
	if (!cpm_time)
		return -ENOMEM;

	if (sizeof(struct rbs_time_cpm1_offset) > PAGE_SIZE) {
		pr_err("CPM1-TIME : Build failure");
		kfree(cpm_time);
		return -EFAULT;
	}
	cpm_time->shared_data = get_zeroed_page(GFP_KERNEL);
	if (!cpm_time->shared_data) {
		kfree(cpm_time);
		return -ENOMEM;
	}

	mutex_init(&cpm_time->tsport_lock);

	cpm_time->fn = &cpm_time_fn;
	rbs_fn_setdata(&cpm_time_fn, cpm_time);
	return rbs_fn_add(&cpm_time_fn);
}

static void __exit cpm_time_exit(void)
{
	struct cpm_time *cpm_time = rbs_fn_getdata(&cpm_time_fn);

	rbs_fn_remove(&cpm_time_fn);
	rbs_fn_setdata(&cpm_time_fn, NULL);
	if (cpm_time) {
		if (cpm_time->shared_data)
			free_page(cpm_time->shared_data);
		kfree(cpm_time);
	}
}

module_init(cpm_time_init);
module_exit(cpm_time_exit);

MODULE_LICENSE("GPL");

