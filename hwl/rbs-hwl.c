/*
 * This file implements RBS HW Log access interface
 * Simple module that allows to access HW Log from user-space
 *
 * Design Rules for using the HW LOG are described in the following document:
 * 39/102 60-CSX 101 09 (HW Log Design Rules)
 *
 * COPYRIGHT (c) Ericsson AB 2015
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston,
 * MA 02111-1307 USA
 */

#include <linux/module.h>
#include <linux/kernel.h>
#include <linux/init.h>
#include <linux/sched.h>
#include <linux/errno.h>
#include <linux/string.h>
#include <linux/time.h>
#include <linux/rtc.h>
#include <linux/err.h>
#include <linux/types.h>

#include <linux/workqueue.h>

#include <mtd/mtd-abi.h>
#include <linux/mtd/mtd.h>

#include <linux/rbs/af_rbs.h>
#include <linux/rbs/rbs-hwl.h>

/* --------------------------------------------------------------------------
 *   CONSTANT DEFINITIONS
 * --------------------------------------------------------------------------
 */
#ifdef CONFIG_RBS_HWL_DEVNAME
#define HWL_DEV_NAME		CONFIG_RBS_HWL_DEVNAME
#else
#define HWL_DEV_NAME		"hw_log"
#endif


#define HWL_FORMAT_NORMAL	(0x0)
#define HWL_FORMAT_LEGACY	(0x1)

#define HWL_TIME_SZ		(6)  /* Not null terminated */
#define HWL_DATE_SZ		(6)  /* Not null terminated */
#define HWL_TIMEDATE_SZ		(HWL_TIME_SZ + HWL_DATE_SZ)
#define HWL_LOGENTRY_LEN	(128)

/* Maximum number of invalid writes to HW log before failing */
#define HWL_WRITE_MAX_CNT	(512)

#define ERASED_FLASH_CONTENT	(0xff)

#define ADM_STATE_FREE		(0xff)
#define ADM_STATE_WRITTEN	(0x3f)
#define ADM_STATE_DELETED	(0x00)
#define ADM_STATE_UNDEFINED	(0x7f)

/*
 * Log IDs
 * Reserved in:
 *   1/155 18-CRX901265, RCS EE HW Log Id Code Description
 */
#define HWLOGID_HWUPDATE	"000"
#define HWLOGID_SWUPDATE	"003"
#define HWLOGID_POWERCYCLE	"004"

/* --------------------------------------------------------------------------
 *   MACRO DEFINITIONS
 * --------------------------------------------------------------------------
 */
#define IS_FREE(e) (e.adm_state == ADM_STATE_FREE)

#define GET_LOGID(e) e.logid
#define GET_DATE(e) e.date
#define GET_TIME(e) (e.date + HWL_DATE_SZ)
#define GET_MSG(text) \
	(strstr(text, "; ")? (strstr(text, "; ")) + 2: \
		(strstr(text, ";*")? (strstr(text, ";*")) + 2: text))

#define GET_FREETXT(e) e.freetext

/* --------------------------------------------------------------------------
 *   TYPE DEFINITIONS
 * --------------------------------------------------------------------------
 */

struct rbs_hwl;

struct hwl_work_task {
	struct rbs_hwl_ioc_write ioc;
	struct work_struct work;
};

struct hwl_ops {
	int (*write_entry)(struct rbs_hwl*, const struct rbs_hwl_ioc_write*);
	void (*erase_sector)(struct rbs_hwl*, unsigned int);
};

struct rbs_hwl {
	struct mutex lock;
	struct mtd_info *mtd;

	int time;
	struct hwl_ops *ops;

	size_t wr_idx;
	size_t rd_idx;
};

/**
 * Log format
 *      1             1            12         3         111
 * ! adm state ! filter value ! date time ! log ID ! free text !
 */
typedef union hwlog_entry {
	struct {
		char adm_state;
		char filter_value;
		char date[HWL_TIMEDATE_SZ];    /* Not null terminated */
		char logid[RBS_HWL_ID_SZ];     /* Not null terminated */
		char freetext[RBS_HWL_MESSAGE_SZ]; /* Null terminated */
	};
} hwlog_entry;

/* --------------------------------------------------------------------------
 *   DATA DECLARATIONS
 * --------------------------------------------------------------------------
 */
static struct rbs_hwl *hwl;

/* --------------------------------------------------------------------------
 *   FUNCTION DEFINITIONS [LOCAL]
 * --------------------------------------------------------------------------
 */
inline static int __mtd_read(
		struct mtd_info *mtd,
		size_t from,
		size_t len,
		void *buf)
{
	int ret; 
	size_t retlen = 0;

	ret = mtd->_read(mtd, from, len, &retlen, buf);
	if (ret) {
		pr_err("hwl: Warning: Failed to read entry\n");
		return (ret);
	}
	if (retlen != len) {
		pr_err("hwl: Failed to read expected len=%zd, retlen=%zd\n",
				len, retlen);
		return -(EIO);
	}
	return 0;
}

inline static int __mtd_write(
		struct mtd_info *mtd,
		size_t to,
		size_t len,
		const void *buf)
{
	int ret; 
	size_t retlen = 0;

	ret = mtd->_write(mtd, to, len, &retlen, buf);
	if (ret) {
		pr_err("hwl: Warning: Failed to write entry\n");
		return (ret);
	}
	if (retlen != len) {
		pr_err("hwl: Failed to write expected len=%zd, retlen=%zd\n",
				len, retlen);
		return -(EIO);
	}
	return 0;
}

static void __mtd_write_work(
		struct work_struct *work)
{
	struct hwl_work_task *tsk =
			container_of(work, struct hwl_work_task, work);

	mutex_lock(&hwl->lock);
	(void) hwl->ops->write_entry(hwl, &tsk->ioc);
	mutex_unlock(&hwl->lock);

	kfree(tsk);
}

static void __mtd_erase_callback(
		struct erase_info *done)
{
	wake_up((wait_queue_head_t *)done->priv);
}

static void __mtd_erase_sector(
		struct mtd_info *mtd,
		unsigned int offset)
{
	int ret;
	struct erase_info ei = {0};
	wait_queue_head_t waitq;
	DECLARE_WAITQUEUE(wait, current);

	init_waitqueue_head(&waitq);
	ei.addr = offset;
	ei.len = mtd->erasesize;
	ei.mtd = mtd;
	ei.callback = __mtd_erase_callback;
	ei.priv = (unsigned long)&waitq;
	ret = mtd->_erase(mtd, &ei);
	if(!ret) {
		set_current_state(TASK_UNINTERRUPTIBLE);
		add_wait_queue(&waitq, &wait);
		if (ei.state != MTD_ERASE_DONE && ei.state != MTD_ERASE_FAILED)
			schedule();
		remove_wait_queue(&waitq, &wait);
		set_current_state(TASK_RUNNING);
		ret = (ei.state == MTD_ERASE_FAILED)? -(EIO): 0;
	}
}

static void hwl_get_datetime(char *d, char *t)
{
	struct timeval time;
	struct rtc_time tm;
	unsigned long local_time;

	do_gettimeofday(&time);
	local_time = (u32)(time.tv_sec - (sys_tz.tz_minuteswest * 60));
	rtc_time_to_tm(local_time, &tm);

	sprintf(d, "%02d%02d%02d", tm.tm_year% 100, tm.tm_mon + 1, tm.tm_mday);
	sprintf(t, "%02d%02d%02d", tm.tm_hour, tm.tm_min, tm.tm_sec);
}

/*
 * finds write_idx and start_idx
 */
static int hwl_set_idx(
		struct rbs_hwl *hwl)
{
	hwlog_entry e;
	size_t offset = 0;
	int ret = 0;

	while (offset < hwl->mtd->size) {
		if ((__mtd_read(hwl->mtd, offset, sizeof(e), &e))) {
			goto out;
		}

		if (IS_FREE(e)) {
			break;
		}
		offset += HWL_LOGENTRY_LEN;
		if (offset >= hwl->mtd->size) {
			/* No free entry found, erase first sector */
			hwl->ops->erase_sector(hwl, 0);
			hwl->wr_idx = 0;
			hwl->rd_idx = hwl->mtd->erasesize;
			goto out;
		}
	}
	hwl->wr_idx = offset;

	/* start of log */
	while (IS_FREE(e)) {
		offset += HWL_LOGENTRY_LEN;
		if (offset >= hwl->mtd->size) {
			/* No old entries found */
			hwl->rd_idx = 0;
			goto out;
		}

		if (((ret =__mtd_read(hwl->mtd, offset, sizeof(e), &e)))<0) {
			goto out;
		}
	}
	hwl->rd_idx = offset;
 out:
	pr_info("hwl: set context: write=%zd(%zd), read=%zd(%zd)\n",
			hwl->wr_idx, hwl->wr_idx/HWL_LOGENTRY_LEN,
			hwl->rd_idx, hwl->rd_idx/HWL_LOGENTRY_LEN);
	return (ret);
}

static void hwl_update_idx(
		struct rbs_hwl *hwl)
{
	hwlog_entry entry;

	__mtd_read(hwl->mtd, hwl->wr_idx, sizeof(entry), &entry);
	/* No stepping needed */
	if (IS_FREE(entry)) {
		return;
 	}

	/* check if log is wrapped */
	if (hwl->wr_idx + HWL_LOGENTRY_LEN >= hwl->mtd->size) {
		/* erase first sector and update indexes to the beginning */
		hwl->ops->erase_sector(hwl, 0);
		hwl->wr_idx = 0;
		hwl->rd_idx = hwl->mtd->erasesize;
		return;
	}

	/* check if log is full -> erase next block */
	if (!((hwl->wr_idx + HWL_LOGENTRY_LEN) % hwl->mtd->erasesize)) {
		__mtd_read(hwl->mtd, hwl->wr_idx + HWL_LOGENTRY_LEN,
			sizeof(entry), &entry);

		if (!IS_FREE(entry)) {
			/* erase sector and update start_idx to next sector */
			hwl->ops->erase_sector(hwl,
				hwl->wr_idx + HWL_LOGENTRY_LEN);
			hwl->rd_idx = hwl->wr_idx +
				HWL_LOGENTRY_LEN + hwl->mtd->erasesize;
			if (hwl->rd_idx >= hwl->mtd->size) hwl->rd_idx = 0;
		}
	}

	hwl->wr_idx += HWL_LOGENTRY_LEN;
	return;
}

static int hwl_filter_entry(
		struct rbs_hwl *hwl,
		const struct rbs_hwl_ioc_write *ioc)
{
	hwlog_entry e;
	size_t offset = hwl->rd_idx;
	int filter = RBS_HWL_GET_LOGID_FILTER(ioc->filter);
	int filter_on_msg = RBS_HWL_GET_MSG_FILTER(ioc->filter);
	int num = 0;

	while (offset !=  hwl->wr_idx) {
		if ((__mtd_read(hwl->mtd, offset, sizeof(e), &e))) {
			break;
		}

		if (!IS_FREE(e)) {
		/*
		 * The filtering function is restarted if:
		 * - the HW configuration ( LogId = 000) is changed;
		 * - new UP is loaded (LogId = 003);
		 * - after power on, power cycling (LogId = 004);
		 * */
			char *id = GET_LOGID(e);
			if (!strncmp(HWLOGID_HWUPDATE, id, RBS_HWL_ID_SZ) ||
			    !strncmp(HWLOGID_SWUPDATE, id, RBS_HWL_ID_SZ) ||
			    !strncmp(HWLOGID_POWERCYCLE, id, RBS_HWL_ID_SZ)) {
				num = 0;
			}
			if (!strncmp(ioc->id, id, RBS_HWL_ID_SZ)) {
				if (!filter_on_msg ||
					!strncmp(GET_MSG(ioc->msg),
						GET_MSG(e.freetext),
						(filter_on_msg == 1?
							strlen(ioc->msg):
							filter_on_msg)))
					++num;
			}
		}

		offset = (offset + HWL_LOGENTRY_LEN) >= hwl->mtd->size?
			0: offset + HWL_LOGENTRY_LEN;
	}
	pr_info("hwl: filter entry logID=\'%s\' "
		"filter=0x%x filter_id=%d, filter_on_msg=%d num=%d mode=%d\n",
		ioc->id, ioc->filter, filter, filter_on_msg, num, ioc->mode);
	return num;
}

static void hwl_erase_sector(
		struct rbs_hwl *hwl,
		unsigned int start)
{
	__mtd_erase_sector(hwl->mtd, start);
}

/*
 * Write entry to next free location and set e->adm_state to:
 *	- WRITTEN if validation OK
 *	- DELETED if validation failed
 *
 * @return non-zero if error during log write occured, 0 otherwise
 */
static int hwl_logentry_write(
		struct rbs_hwl *hwl,
		hwlog_entry *e)
{
	hwlog_entry ev;
	int ret;

	e->adm_state = ADM_STATE_UNDEFINED;

	/* write next free entry */
	if ((ret = __mtd_write(hwl->mtd, hwl->wr_idx, sizeof(*e), e)))
		return (ret);

	/* read back entry */
	if ((ret = __mtd_read(hwl->mtd, hwl->wr_idx, sizeof(ev), &ev)))
		return (ret);

	/* validate write */
	if (strncmp(ev.date, e->date, HWL_LOGENTRY_LEN - 2)) {
		/* validation failed */
		e->adm_state = ADM_STATE_DELETED;
	} else {
		/* validation OK */
		e->adm_state = ADM_STATE_WRITTEN;
	}

	/* write back adm_state */
	ret = __mtd_write(hwl->mtd,
		hwl->wr_idx, sizeof(e->adm_state), &e->adm_state);
	if (ret) return (ret);

	/* step write pointer */
	hwl_update_idx(hwl);

	return 0;
}

static int hwl_logentry(
		struct rbs_hwl *hwl,
		const struct rbs_hwl_ioc_write *ioc)
{
	hwlog_entry e;
	char *semicolon_delimiter;
	char date[7], time[7];
	int occurrence = 0, ret, i;

	if (RBS_HWL_GET_LOGID_FILTER(ioc->filter)) {
		occurrence = hwl_filter_entry(hwl, ioc) + 1;
		if (occurrence > RBS_HWL_GET_LOGID_FILTER(ioc->filter)) {
			pr_info("hwl: Too many entries with same logID="
				"\'%s\', info not stored in HW log.\n",
				ioc->id);
			return 0;
		}
	}

	memset(&e, ERASED_FLASH_CONTENT, sizeof(e));

	hwl_get_datetime(date, time);
	sprintf(e.date, "%s%s%s%s", date, time, ioc->id, ioc->msg);

	e.filter_value = occurrence;
	semicolon_delimiter = strstr(e.freetext, ";");
	if (semicolon_delimiter)
		*(++semicolon_delimiter) =
			hwl->time == RBS_HWLTIME_TRUSTED? ' ': '*';

	for (i = 0; i < HWL_WRITE_MAX_CNT; i++) {
		/* write entry */
		ret = hwl_logentry_write(hwl, &e);
		if (ret) return ret;

		if (e.adm_state == ADM_STATE_WRITTEN) {
			/* log validation OK */
			return 0;
		}

		pr_info("hwl: logentry validation failed, trying at next location\n");
	}

	/* write validation failed on more than HWL_WRITE_MAX_CNT locations,
	 * don't try again */
	pr_err("hwl: logentry validation failed, logID=\'%s\', "
		"msg=\'%s\'\n", ioc->id, ioc->msg);

	return -(EINVAL);
}

static int rbs_hwl_ioctl_logentry(
		struct rbs_hwl *hwl,
		void __user *arg)
{
	struct rbs_hwl_ioc_write ioc;
	int ret = -1;

	if (!hwl)
		return -(EINVAL);
	if (copy_from_user(&ioc, arg, sizeof(ioc)))
		return -(EFAULT);

	if (hwl->wr_idx >= hwl->mtd->size) {
		pr_err("hwl: write index invalid, %zu\n", hwl->wr_idx);
		return -(EINVAL);
	}

	if ((ioc.mode & RBS_HWLMODE_BLOCKING) == RBS_HWLMODE_NONBLOCK) {
		/* Allocate & initialize our private structure */
		struct hwl_work_task *tsk = kzalloc(sizeof(*tsk), GFP_KERNEL);
		if (!tsk) {
			pr_err("hwl: unable to allocate work\n");
			return -(ENOMEM);
		}
		tsk->ioc = ioc;
		INIT_WORK(&tsk->work, __mtd_write_work);
		/* queue work on a workqueue */
		queue_work(system_long_wq, &tsk->work);
		return 0;
	}

	/* Ensure that any scheduled work has run to completion */
	flush_workqueue(system_long_wq);
	ret = hwl->ops->write_entry(hwl, &ioc);
	return (ret);
}

/*
 * Read hwlog entry.
 *
 * Return value:
 *	- ENOMSG if entry is empty
 *	- 0 if adm state is WRITTEN
 *	- ENOENT if adm_state is UNDEFINED or DELETED
 *	- EINVAL if adm_state is unknown
 *	- EFAULT for other errors during hwlog read
 */
static int rbs_hwl_ioctl_readentry(
		struct rbs_hwl *hwl,
		void __user *arg)
{
	struct rbs_hwl_ioc_read ioc;
	hwlog_entry e;
	int ix, ret = 0;

	if (!hwl)
		return -(EFAULT);

	if (copy_from_user(&ioc, arg, sizeof(ioc)))
		return -(EFAULT);

	/* adjust read pointer */
	if ((ix = hwl->rd_idx+ioc.offset*HWL_LOGENTRY_LEN) >= hwl->mtd->size)
		ix %= (size_t) hwl->mtd->size;

	if ((ret = __mtd_read(hwl->mtd, ix, sizeof(e), &e)))
		return (ret);

	if (IS_FREE(e)) {
		return -(ENOMSG);
	}
	memcpy(ioc.id, GET_LOGID(e), RBS_HWL_ID_SZ);
	ioc.id[RBS_HWL_ID_SZ] = 0;

	memcpy(ioc.time, GET_DATE(e), HWL_DATE_SZ);
	memcpy(ioc.time + HWL_DATE_SZ, GET_TIME(e), HWL_TIME_SZ);
	ioc.time[HWL_TIMEDATE_SZ] = 0;
	ioc.filter = (int)e.filter_value;

	memcpy(ioc.msg, GET_FREETXT(e), RBS_HWL_MESSAGE_SZ);

	if (copy_to_user(arg, &ioc, sizeof(ioc)))
		return -(EFAULT);

	switch (e.adm_state) {
	case ADM_STATE_WRITTEN:
		ret = 0;
		break;
	case ADM_STATE_UNDEFINED:
	case ADM_STATE_DELETED:
		ret = -(ENOENT);
		break;
	default:
		ret = -(EINVAL);
		break;
	}
	return (ret);
}

static int rbs_hwl_ioctl_eraselog(
		struct rbs_hwl *hwl,
		void __user *arg)
{
	unsigned int offset;

	if (!hwl)
		return -(EINVAL);

	for(offset = 0; offset < hwl->mtd->size; offset += hwl->mtd->erasesize)
		hwl->ops->erase_sector(hwl, offset);

	hwl->wr_idx = 0;
	hwl->rd_idx = 0;

	return 0;
}

static int rbs_hwl_ioctl_settime(
	struct rbs_hwl *hwl,
	void __user *arg)
{
	void __user *param = (void __user *) arg;
	unsigned int time;

	if (!hwl)
		return -(EINVAL);

	if (copy_from_user(&time, param, sizeof(time)))
		return -EFAULT;

	if (time > RBS_HWLTIME_TRUSTED)
		return -EINVAL;

	hwl->time = time;
	pr_info("hwl: set trusted time: %u\n", hwl->time);

	return 0;
}

static int rbs_hwl_sock_init(
		struct rbs_sock *rsk,
		void *init_param)
{
	rsk->proto_data = hwl;
	return 0;
}

static void rbs_hwl_sock_done(
		struct rbs_sock *rsk)
{
	rsk->proto_data = NULL;
}

static int rbs_hwl_sock_ioctl(
		struct rbs_sock *rsk,
		unsigned int cmd,
		unsigned long arg)
{
	int ret = 0;

	mutex_lock(&hwl->lock);

	switch (cmd) {
	case RBS_HWLIOC_WRITE:
		ret = rbs_hwl_ioctl_logentry(hwl, (void __user *) arg);
		break;
	case RBS_HWLIOC_READ:
		ret = rbs_hwl_ioctl_readentry(hwl, (void __user *) arg);
		break;
	case RBS_HWLIOC_ERASE:
		ret = rbs_hwl_ioctl_eraselog(hwl, (void __user *) arg);
		break;
	case RBS_HWLIOC_TIME:
		ret = rbs_hwl_ioctl_settime(hwl, (void __user *) arg);
		break;
	default:
		ret = -(ENOSYS);
		break;
	}

	mutex_unlock(&hwl->lock);
	return (ret);
}

static struct rbs_proto_ops hwl_proto_ops = {
	.init = rbs_hwl_sock_init,
	.done = rbs_hwl_sock_done,
	.ioctl = rbs_hwl_sock_ioctl,
};

static struct hwl_ops hwl_ops = {
	.erase_sector = hwl_erase_sector,
	.write_entry = hwl_logentry,
};

static int __init rbs_hwl_init(void)
{
	hwl = kzalloc(sizeof(*hwl), GFP_KERNEL);
	if (!hwl) {
		pr_err("hwl: out of memory\n");
		return -(ENOMEM);
	}

	hwl->mtd = get_mtd_device_nm(HWL_DEV_NAME);
	if (IS_ERR(hwl->mtd)) {
		pr_err("hwl: no mtd device %s\n", HWL_DEV_NAME);
		return -(ENODEV);
	}
	if (hwl->mtd->type == MTD_ABSENT) {
		put_mtd_device(hwl->mtd);
		return -(ENODEV);
	}

	hwl->ops = &hwl_ops;

	/* find out where the firdt free entry exists in the log and set idx */
	if (hwl_set_idx(hwl)) {
		pr_err("hwl: Failed to find the first free entry in the HW log\n");
		return -(ENOSYS);
	}

	mutex_init(&hwl->lock);

	/* register API */
	if (rbs_proto_register(RBS_PROTO_HWL, &hwl_proto_ops, NULL)) {
		pr_err("hwl: unable to register protocol\n");
		return -(EBUSY);
	}

	pr_info("hwl: mtd dev : %s, type : %u\n", HWL_DEV_NAME, hwl->mtd->type);
	pr_info("hwl: mtd total size : %u Kb\n", (uint32_t)(hwl->mtd->size/1024));
	pr_info("hwl: mtd erase size : %u Kb\n", hwl->mtd->erasesize/1024);
	pr_info("hwl: mtd write size : %u bytes\n", hwl->mtd->writebufsize);
	return 0;
}

static void __exit rbs_hwl_exit(void)
{
	if (hwl->mtd)
		put_mtd_device(hwl->mtd);

	kfree(hwl);
	rbs_proto_unregister(RBS_PROTO_HWL, &hwl_proto_ops);
}

/* --------------------------------------------------------------------------
 *   FUNCTION DEFINITIONS [GLOBAL]
 * --------------------------------------------------------------------------
 */
late_initcall(rbs_hwl_init);
module_exit(rbs_hwl_exit);

MODULE_LICENSE("GPL");
