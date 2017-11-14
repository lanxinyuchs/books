/*
 * Simple module that implements an interface to change MTD
 * partition permissions from user-space.
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
#include <linux/rbs/rbs-mtdpart.h>

/* --------------------------------------------------------------------------
 *   CONSTANT DEFINITIONS
 * --------------------------------------------------------------------------
 */


/* --------------------------------------------------------------------------
 *   MACRO DEFINITIONS
 * --------------------------------------------------------------------------
 */


/* --------------------------------------------------------------------------
 *   TYPE DEFINITIONS
 * --------------------------------------------------------------------------
 */

struct rbs_mtdpart {
	struct mutex lock;
	char buf[RBS_MAX_PARTNAME_LENGTH];
};

/* --------------------------------------------------------------------------
 *   DATA DECLARATIONS
 * --------------------------------------------------------------------------
 */

static struct rbs_mtdpart *mtdpart;

/* --------------------------------------------------------------------------
 *   FUNCTION DEFINITIONS [LOCAL]
 * --------------------------------------------------------------------------
 */

static int rbs_mtdpart_ioctl_enable_wp(
			struct rbs_mtdpart *mtdpart,
			void __user *arg)
{
	struct mtd_info *mtd;

	if (!mtdpart)
		return -(EINVAL);

	if (copy_from_user(&(mtdpart->buf), (char *) arg, sizeof(mtdpart->buf)))
		return -(EFAULT);

	mtdpart->buf[RBS_MAX_PARTNAME_LENGTH-1] = '\0';
	mtd = get_mtd_device_nm(mtdpart->buf);

	if (IS_ERR(mtd)) {
		pr_info("rbs-mtdpart: no mtd device %s\n", mtdpart->buf);
		return -(ENODEV);
	}

        if (mtd->type == MTD_ABSENT) {
		put_mtd_device(mtd);
		return -(ENODEV);
	}

	mtd->flags &= ~MTD_WRITEABLE;
	pr_info("Partition %s not writable\n", mtd->name);
        put_mtd_device(mtd);

	return 0;
}

static int rbs_mtdpart_ioctl_disable_wp(
			struct rbs_mtdpart *mtdpart,
			void __user *arg)
{
	struct mtd_info *mtd;

	if (!mtdpart)
		return -(EINVAL);

	if (copy_from_user(&(mtdpart->buf), (char *) arg, sizeof(mtdpart->buf)))
		return -(EFAULT);

	mtdpart->buf[RBS_MAX_PARTNAME_LENGTH-1] = '\0';
	mtd = get_mtd_device_nm(mtdpart->buf);

        if (IS_ERR(mtd)) {
		pr_info("rbs-mtdpart: no mtd device %s\n", mtdpart->buf);
		return -(ENODEV);
	}

        if (mtd->type == MTD_ABSENT) {
		put_mtd_device(mtd);
		return -(ENODEV);
	}

	mtd->flags |= MTD_WRITEABLE;
	pr_info("Partition %s writable\n", mtd->name);
	put_mtd_device(mtd);

	return 0;
}

static int rbs_mtdpart_sock_ioctl(
			struct rbs_sock *rsk,
			unsigned int cmd,
			unsigned long arg)
{
	int ret = 0;

	mutex_lock(&mtdpart->lock);

	switch (cmd) {
	case RBS_MTDPART_ENABLE_WP:
		ret = rbs_mtdpart_ioctl_enable_wp(mtdpart,(void __user *) arg);
		break;
	case RBS_MTDPART_DISABLE_WP:
		ret = rbs_mtdpart_ioctl_disable_wp(mtdpart,(void __user *) arg);
		break;
	default:
		ret = -(ENOSYS);
		break;
	}

	mutex_unlock(&mtdpart->lock);

	return (ret);
}

static struct rbs_proto_ops mtdpart_proto_ops = {
	.ioctl = rbs_mtdpart_sock_ioctl,
};


static int __init rbs_mtdpart_init(void)
{
	mtdpart = kzalloc(sizeof(*mtdpart), GFP_KERNEL);
	if (!mtdpart) {
		pr_err("rbs-mtdpart: out of memory\n");
		return -(ENOMEM);
	}

	mutex_init(&mtdpart->lock);

	/* register API */
	if (rbs_proto_register(RBS_PROTO_MTDPART, &mtdpart_proto_ops, NULL)) {
		pr_err("rbs-mtdpart: unable to register protocol\n");
		return -(EBUSY);
	}

	return 0;
}

static void __exit rbs_mtdpart_exit(void)
{
	rbs_proto_unregister(RBS_PROTO_MTDPART, &mtdpart_proto_ops);
	kfree(mtdpart);
}

/* --------------------------------------------------------------------------
 *   FUNCTION DEFINITIONS [GLOBAL]
 * --------------------------------------------------------------------------
 */

late_initcall(rbs_mtdpart_init);
module_exit(rbs_mtdpart_exit);

MODULE_LICENSE("GPL");

