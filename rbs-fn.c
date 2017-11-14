#include <linux/kernel.h>
#include <linux/version.h>
#include <linux/module.h>
#include <linux/init.h>
#include <linux/slab.h>
#include <linux/uaccess.h>
#include <linux/klist.h>
#include <linux/workqueue.h>
#include <linux/kallsyms.h>
#include <linux/kthread.h>
#include <net/netlink.h>
#include <linux/net.h>
#include <net/sock.h>

#include <linux/rbs/rbs-fn.h>

struct rbs_fn_head {
	char name[RBS_FN_NAME_SIZE];
	struct rbs_fn *fn;
	struct list_head entry;
	struct list_head dep_list;
};

struct rbs_fn_dep_entry {
	struct list_head entry;
	struct rbs_fn *fn;
	struct rbs_fn_id id;
};

struct rbs_fn_task {
	struct list_head entry;
	int task;
	struct rbs_fn *fn;

	int internal;
	int param;
	int done;
	wait_queue_head_t done_event;
	int result;

	struct work_struct work;
	int refs;

	int parents_size; /* parents array size */
	int nparents; /* actual number of parents */
	struct rbs_fn_task **parents;
};

/* tasks requested by user */
#define TASK_FN_ADD	0x0
#define TASK_FN_REMOVE	0x1
#define TASK_FN_LOCK	0x2
/* internal tasks */
#define TASK_FN_RELEASE	0x3
#define TASK_FN_SETUP	0x4

static struct list_head rbs_fn_taskq;
static atomic_t rbs_fn_taskq_pending;
DEFINE_MUTEX(rbs_fn_taskq_lock);
static wait_queue_head_t rbs_fn_taskq_event;
static struct task_struct *rbs_fn_task_worker;
static struct workqueue_struct *rbs_fn_workqueue;

static struct kset *rbs_fn_kset;
static struct kobject rbs_fn_unattached;
static struct list_head rbs_fn_list;

static int rbs_fn_num = 0;
static int rbs_fn_mod_ready = 0;

static struct class *rbs_fn_class;
static unsigned int rbs_fn_dev_ma;
static unsigned long rbs_fn_dev_mi[(MAX_RBS_FNS / BITS_PER_LONG) + 1] = {0,};

struct rbs_fn_private {
	struct rbs_fn *fn;
	int ready;
	int active;
	int depcnt;
	struct rbs_fn_head *dep_head;
	struct rbs_fn_task *setup_task;
	struct rbs_fn_task *release_task;
};
#define to_rbs_fn(_obj) container_of(_obj, struct rbs_fn, kobj)
#define to_rbs_fn_attr(_attr) container_of(_attr, struct rbs_fn_attribute, attr)

static ssize_t rbs_fn_attr_show(struct kobject *kobj, struct attribute *attr,
			        char *buf)
{
	struct rbs_fn_attribute *fn_attr = to_rbs_fn_attr(attr);
	struct rbs_fn *fn = to_rbs_fn(kobj);
	ssize_t ret = -EIO;

	if (fn_attr->show)
		ret = fn_attr->show(fn, fn_attr, buf);
	if (ret >= (ssize_t)PAGE_SIZE) {
		print_symbol("fn_attr_show: %s returned bad count\n",
				(unsigned long)fn_attr->show);
	}
	return ret;
}

static ssize_t rbs_fn_attr_store(struct kobject *kobj, struct attribute *attr,
			         const char *buf, size_t count)
{
	struct rbs_fn_attribute *fn_attr = to_rbs_fn_attr(attr);
	struct rbs_fn *fn = to_rbs_fn(kobj);
	ssize_t ret = -EIO;

	if (fn_attr->store)
		ret = fn_attr->store(fn, fn_attr, buf, count);
	return ret;
}

static const struct sysfs_ops rbs_fn_sysfs_ops = {
	.show	= rbs_fn_attr_show,
	.store	= rbs_fn_attr_store,
};

static void rbs_fn_kobj_release(struct kobject *kobj)
{
	struct rbs_fn *fn = to_rbs_fn(kobj);

	if (fn->p)
		kfree(fn->p);
	fn->p = NULL;
}

static struct kobj_type rbs_fn_ktype = {
	.release	= rbs_fn_kobj_release,
	.sysfs_ops	= &rbs_fn_sysfs_ops,
	.namespace	= NULL,
};

static struct rbs_fn_head *rbs_fn_head_get(const char *name)
{
	struct rbs_fn_head *head;

	list_for_each_entry(head, &rbs_fn_list, entry) {
		if (!strcmp(head->name, name))
			return head;
	}
	return NULL;
}

static struct rbs_fn_head *rbs_fn_head_add(const char *name,
		struct rbs_fn *fn)
{
	struct rbs_fn_head *head;

	head = rbs_fn_head_get(name);
	if (!head) {
		head = kzalloc(sizeof(*head), GFP_KERNEL);
		if (!head)
			return NULL;

		INIT_LIST_HEAD(&head->dep_list);
		list_add_tail(&head->entry, &rbs_fn_list);

		strcpy(head->name, name);
	}

	if (fn)
		head->fn = fn;

	return head;
}

static void rbs_fn_head_del(const char *name, struct rbs_fn *fn)
{
	struct rbs_fn_head *head;

	head = rbs_fn_head_get(name);
	if (!head)
		return;

	if (head->fn == fn)
		head->fn = NULL;

	/* don't delete if there are dependent functions */
	if (!list_empty(&head->dep_list))
		return;

	list_del_init(&head->entry);

	kfree(head);
}

static int rbs_fn_dep_add(const char *name, struct rbs_fn *fn,
		struct rbs_fn_id *id)
{
	struct rbs_fn_head *head;
	struct rbs_fn_dep_entry *de;

	head = rbs_fn_head_get(name);
	if (!head) {
		head = rbs_fn_head_add(name, NULL);
	}
	if (!head)
		return -ENOMEM;

	de = kzalloc(sizeof(*de), GFP_KERNEL);
	if (!de) {
		rbs_fn_head_del(name, NULL);
		return -ENOMEM;
	}

	de->fn = fn;
	de->id = *id;

	list_add_tail(&de->entry, &head->dep_list);

	if (head->fn && head->fn->p->active) {
		de->id.fn = head->fn;
		if (fn->ops && de->fn->ops && de->fn->ops->save_ops)
			de->fn->ops->save_ops(de->fn, &de->id);
	} else
		fn->p->depcnt++;

	return 0;
}

static void rbs_fn_dep_del(const char *name, struct rbs_fn *fn)
{
	struct rbs_fn_head *head;
	struct rbs_fn_dep_entry *dep_entry;

	head = rbs_fn_head_get(name);
	if (!head)
		return;

	list_for_each_entry(dep_entry, &head->dep_list, entry) {
		if (dep_entry->fn == fn) {
			list_del_init(&dep_entry->entry);
			kfree(dep_entry);
			return;
		}
	}
}

static inline void rbs_fn_task_init(struct rbs_fn_task *tsk,
		int task, struct rbs_fn *fn)
{
	INIT_LIST_HEAD(&tsk->entry);
	tsk->task = task;
	tsk->fn = fn;
	tsk->done = 0;
	tsk->result = -EFAULT;
	init_waitqueue_head(&tsk->done_event);
	tsk->refs = 0;
	tsk->nparents = 0;
	tsk->parents_size = 0;
	tsk->parents = 0;
	tsk->internal = 0;
}

static inline struct rbs_fn_task *rbs_fn_task_alloc(int task,
		struct rbs_fn *fn, int internal)
{
	struct rbs_fn_task *tsk;

	/* FIXME: pre-allocate memory for the internal tasks
	 *  (maximum number of tasks - MAX_RBS_FNS*2) */
	tsk = kmalloc(sizeof(*tsk), GFP_KERNEL);
	if (!tsk)
		return NULL;

	rbs_fn_task_init(tsk, task, fn);
	tsk->internal = internal;

	return tsk;
}

static inline void rbs_fn_task_free(struct rbs_fn_task *task)
{
	if (task->parents)
		kfree(task->parents);
	if (task->internal)
		kfree(task);
}

static inline void rbs_fn_task_queue(struct rbs_fn_task *task)
{
	mutex_lock(&rbs_fn_taskq_lock);
	list_add_tail(&task->entry, &rbs_fn_taskq);
	atomic_inc(&rbs_fn_taskq_pending);
	mutex_unlock(&rbs_fn_taskq_lock);

	wake_up(&rbs_fn_taskq_event);
}

static inline void rbs_fn_task_add_parent(struct rbs_fn_task *task,
		struct rbs_fn_task *parent)
{
	BUG_ON(!task->internal);

	if (!parent)
		return;

	if (task->nparents == task->parents_size) {
		if (!task->parents_size)
			task->parents_size = 2;
		else
			task->parents_size *= 2;
		task->parents = krealloc(task->parents,
				task->parents_size * sizeof(task), GFP_KERNEL);
	}

	task->parents[task->nparents++] = parent;
	parent->refs++;
}

static inline void rbs_fn_task_ext_completed(struct rbs_fn_task *task)
{
	spin_lock(&task->done_event.lock);
	task->done = 1;
	wake_up_locked(&task->done_event);
	spin_unlock(&task->done_event.lock);
}

static void rbs_fn_setup_worker(struct work_struct *work)
{
	struct rbs_fn_task *task =
		container_of(work, struct rbs_fn_task, work);

	if (!task->fn->p->ready && task->fn->ops && task->fn->ops->setup)
		task->result = task->fn->ops->setup(task->fn);
	else
		task->result = 0;

	rbs_fn_task_queue(task);
}

static void rbs_fn_setup_start(struct rbs_fn *fn, struct rbs_fn_task *parent)
{
	struct rbs_fn_private *p = fn->p;
	struct rbs_fn_task *task;

	if (p->depcnt)
		return;
	if (p->setup_task) {
		rbs_fn_task_add_parent(p->setup_task, parent);
		return;
	}

	if (fn->lock_level == RBS_FN_LOCK_HARD)
		return;

	task = rbs_fn_task_alloc(TASK_FN_SETUP, fn, 1);
	INIT_WORK(&task->work, rbs_fn_setup_worker);
	rbs_fn_task_add_parent(task, parent);

	p->setup_task = task;

	if (!p->release_task) {
		queue_work(rbs_fn_workqueue, &task->work);
	} else
		rbs_fn_task_add_parent(p->release_task, task);
}

static void rbs_fn_setup_finish(struct rbs_fn_task *task)
{
	struct rbs_fn_dep_entry *de;
	struct rbs_fn *fn = task->fn;
	struct rbs_fn_private *p = fn->p;

	p->setup_task = NULL;

	if (task->result) {
		fn->lock_level = RBS_FN_LOCK_HARD;
		return;
	} else
		p->ready = 1;

	if (p->depcnt)
		return;

	if (fn->lock_level == RBS_FN_LOCK_SOFT)
		return;

	p->active = 1;

	list_for_each_entry(de, &p->dep_head->dep_list, entry) {
		de->id.fn = fn;
		if (fn->ops && de->fn->ops && de->fn->ops->save_ops)
			de->fn->ops->save_ops(de->fn, &de->id);

		if (!--de->fn->p->depcnt)
			rbs_fn_setup_start(de->fn, NULL);
	}
}

static void rbs_fn_release_worker(struct work_struct *work)
{
	struct rbs_fn_task *task =
		container_of(work, struct rbs_fn_task, work);

	if (task->fn->p->ready && task->fn->ops && task->fn->ops->release)
		task->fn->ops->release(task->fn);
	task->result = 0;

	rbs_fn_task_queue(task);
}

static void rbs_fn_release_start(struct rbs_fn *fn, int dep_only,
		struct rbs_fn_task *parent)
{
	struct rbs_fn_private *p = fn->p;
	struct rbs_fn_dep_entry *de;
	struct rbs_fn_task *task;

	if (!p->ready && !p->setup_task)
		return;
	if (p->release_task) {
		rbs_fn_task_add_parent(p->release_task, parent);
		return;
	}

	p->active = 0;

	task = rbs_fn_task_alloc(TASK_FN_RELEASE, fn, 1);
	INIT_WORK(&task->work, rbs_fn_release_worker);

	list_for_each_entry(de, &p->dep_head->dep_list, entry) {
		de->id.fn = NULL;
		if (fn->ops && de->fn->ops &&
				de->fn->ops->release_ops)
			de->fn->ops->release_ops(de->fn, &de->id);

		de->fn->p->depcnt++;
		rbs_fn_release_start(de->fn, 0, task);
	}

	if (p->setup_task) {
		rbs_fn_task_add_parent(p->setup_task, task);
	} else if (!task->refs){
		if (dep_only)
			goto nothing_to_do;
		queue_work(rbs_fn_workqueue, &task->work);
	}

	p->release_task = task;
	rbs_fn_task_add_parent(task, parent);

	return;

nothing_to_do:
	rbs_fn_task_free(task);
	return;
}

static void rbs_fn_release_finish(struct rbs_fn_task *task)
{
	struct rbs_fn *fn = task->fn;
	struct rbs_fn_private *p = fn->p;

	p->release_task = NULL;
	p->ready = 0;
}

static ssize_t rbs_fn_sysfs_lock_show(struct rbs_fn *fn,
		struct rbs_fn_attribute *attr, char *buf)
{
	return sprintf(buf, "%d\n", fn->lock_level);
}

static ssize_t rbs_fn_sysfs_lock_store(struct rbs_fn *fn,
		struct rbs_fn_attribute *attr, const char *buf, size_t count)
{
	int level;
	int ret;

	ret = sscanf(buf, "%d", &level);
	if (ret == 1) {
		rbs_fn_lock(fn, level);
		return count;
	}
	return -EINVAL;
}

static RBS_FN_ATTR(lock, 0600, rbs_fn_sysfs_lock_show,
		rbs_fn_sysfs_lock_store);

static int rbs_fn_attrs_add(struct rbs_fn *fn)
{
	int error = 0;
	int i;

	if (fn->owner && sysfs_create_link(&fn->kobj,
				&fn->owner->mkobj.kobj, "module")) {
		pr_info("(%s) sysfs_create_link failed\n", __func__);
	}

	(void) rbs_fn_create_file(fn, &rbs_fn_attr_lock);

	if (!fn->attrs)
		return 0;
	for (i = 0; fn->attrs[i].attr.name; i++) {
		error = rbs_fn_create_file(fn, &fn->attrs[i]);
		if (error) {
			while (--i >= 0)
				rbs_fn_remove_file(fn, &fn->attrs[i]);
			break;
		}
	}

	return error;
}

static void rbs_fn_attrs_remove(struct rbs_fn *fn)
{
	int i;

	if (fn->owner)
		sysfs_remove_link(&fn->kobj, "module");

	rbs_fn_remove_file(fn, &rbs_fn_attr_lock);

	if (fn->attrs) {
		for (i = 0; fn->attrs[i].attr.name; i++)
			rbs_fn_remove_file(fn, &fn->attrs[i]);
	}
}

static int rbs_fn_reserve_dev_minor(void)
{
	unsigned long dev_m;

	dev_m = find_first_zero_bit(rbs_fn_dev_mi, MAX_RBS_FNS);
	if (dev_m >= MAX_RBS_FNS)
		return -1;
	set_bit(dev_m, rbs_fn_dev_mi);

	return (int) dev_m;
}

static void rbs_fn_return_dev_minor(int dev_minor)
{
	if (dev_minor >= 0)
		clear_bit(dev_minor, rbs_fn_dev_mi);
}

static int rbs_fn_add_start(struct rbs_fn_task *task)
{
	struct rbs_fn *fn = task->fn;
	struct rbs_fn_private *priv;
	struct rbs_fn_head *head = NULL;
	struct rbs_fn_dep_entry *dep_entry;
	struct kobject *parent_kobj = NULL;
	int i, ret, dev_mi = -1;

	head = rbs_fn_head_get(fn->name);
	if (head && head->fn)
		return -EEXIST;

	if (rbs_fn_num >= MAX_RBS_FNS) {
		WARN_ONCE(1, "rbs_fn: maximum number of FNs reached\n");
		return -EFAULT;
	}

	priv = kzalloc(sizeof(*priv), GFP_KERNEL);
	if (!priv)
		return -ENOMEM;

	priv->fn = fn;
	fn->p = priv;

	if (fn->parent) {
		head = rbs_fn_head_get(fn->parent);
		if (head && head->fn)
			parent_kobj = &head->fn->kobj;
		else
			parent_kobj = &rbs_fn_unattached;
	}

	/* init kobject */
	memset(&fn->kobj, 0, sizeof(struct kobject));
	fn->kobj.kset = rbs_fn_kset;
	ret = kobject_init_and_add(&fn->kobj, &rbs_fn_ktype,
			parent_kobj, "%s", fn->name);
	if (ret)
		goto out;

	/* create device */
	if (fn->has_device) {
		dev_mi = rbs_fn_reserve_dev_minor();
		if (dev_mi < 0) 
			goto out_release_kobj;
		fn->dev = device_create(rbs_fn_class, NULL,
				MKDEV(rbs_fn_dev_ma, dev_mi), &fn,
				(fn->device_name ? fn->device_name :
				 fn->name));
		if (IS_ERR(fn->dev)) {
			fn->dev = NULL;
			rbs_fn_return_dev_minor(dev_mi);
			goto out_release_kobj;
		}
	}

	/* add to list and attach children */
	head = rbs_fn_head_add(fn->name, fn);
	if (!head) {
		ret = -ENOMEM;
		goto out_release_device;
	}
	list_for_each_entry(dep_entry, &head->dep_list, entry) {
		if (dep_entry->fn->parent &&
		    !strcmp(dep_entry->fn->parent, fn->name)) {
			i = kobject_move(&dep_entry->fn->kobj,
					&fn->kobj);
		}
	}
	priv->dep_head = head;

	/* add dependencies */
	if (fn->parent) {
		struct rbs_fn_id parent_id;

		parent_id.name = fn->parent;
		parent_id.id = RBS_FN_PARENT_FNID;
		rbs_fn_dep_add(fn->parent, fn, &parent_id);
	}

	for (i = 0; fn->depend && fn->depend[i].name; i++) {
		rbs_fn_dep_add(fn->depend[i].name, fn, &fn->depend[i]);
	}

	/* add sysfs attributes*/
	if (rbs_fn_attrs_add(fn)) {
		pr_err("%s: unable to add %s attributes\n", __func__,
				fn->name);
	}

	rbs_fn_num++;
	rbs_fn_setup_start(fn, NULL);

	return 0;

out_release_device:
	rbs_fn_return_dev_minor(dev_mi);
	if (fn->dev)
		device_destroy(rbs_fn_class, fn->dev->devt);
	fn->dev = NULL;
out_release_kobj:
	kobject_put(&fn->kobj);
out:
	if (priv)
		kfree(priv);
	fn->p = NULL;

	return ret;
}

static void rbs_fn_add_finish(struct rbs_fn_task *task)
{
}

static int rbs_fn_remove_start(struct rbs_fn_task *task)
{
	int i;
	struct rbs_fn_dep_entry *dep_entry;
	struct rbs_fn *fn = task->fn;
	struct rbs_fn_private *p = fn->p;

	if (!p)
		return -EFAULT;

	list_for_each_entry(dep_entry, &p->dep_head->dep_list, entry) {
		if (dep_entry->fn->parent &&
		    !strcmp(dep_entry->fn->parent, fn->name)) {
			i = kobject_move(&dep_entry->fn->kobj,
					&rbs_fn_unattached);
		}
	}

	if (fn->parent)
		rbs_fn_dep_del(fn->parent, fn);

	for (i = 0; fn->depend && fn->depend[i].name; i++) {
		rbs_fn_dep_del(fn->depend[i].name, fn);
	}
	rbs_fn_attrs_remove(fn);

	rbs_fn_release_start(fn, 0, task);

	return 0;
}

static void rbs_fn_remove_finish(struct rbs_fn_task *task)
{
	struct rbs_fn *fn = task->fn;

	/* destroy device */
	if (fn->has_device && fn->dev) {
		rbs_fn_return_dev_minor(MINOR(fn->dev->devt));
		device_destroy(rbs_fn_class, fn->dev->devt);
		fn->dev = NULL;
	}

	rbs_fn_head_del(fn->name, fn);
	fn->p->dep_head = NULL;
	rbs_fn_num--;
	kobject_put(&fn->kobj);
}

static int rbs_fn_lock_start(struct rbs_fn_task *task)
{
	struct rbs_fn *fn = task->fn;
	struct rbs_fn_private *p = fn->p;
	int level = task->param;

	if (!p)
		return -EINVAL;

	fn->lock_level = level;

	switch (fn->lock_level) {
	case RBS_FN_LOCK_NO:
		rbs_fn_setup_start(fn, NULL);
		break;
	case RBS_FN_LOCK_SOFT:
		rbs_fn_release_start(fn, 1, task);
		break;
	case RBS_FN_LOCK_HARD:
	default:
		rbs_fn_release_start(fn, 0, task);
		break;
	}

	return 0;
}

static void rbs_fn_lock_finish(struct rbs_fn_task *task)
{
}

static void rbs_fn_task_int_completed(struct rbs_fn_task *task)
{
	int i;
	struct rbs_fn_task *tbe;

	for (i = 0; i < task->nparents; i++) {
		tbe = task->parents[i];
		if (--tbe->refs)
			continue;

		switch (tbe->task) {
		/* external tasks ready to be finalized */
		case TASK_FN_ADD:
			rbs_fn_add_finish(tbe);
			rbs_fn_task_ext_completed(tbe);
			break;
		case TASK_FN_REMOVE:
			rbs_fn_remove_finish(tbe);
			rbs_fn_task_ext_completed(tbe);
			break;
		case TASK_FN_LOCK:
			rbs_fn_lock_finish(tbe);
			rbs_fn_task_ext_completed(tbe);
			break;
		/* internal tasks ready to be scheduled */
		case TASK_FN_SETUP:
		case TASK_FN_RELEASE:
			queue_work(rbs_fn_workqueue, &tbe->work);
			break;
		default:
			break;
		}
	}
}

static int rbs_fn_worker(void *unused)
{
	struct rbs_fn_task *task;

	while (!kthread_should_stop()) {
		wait_event_interruptible(rbs_fn_taskq_event,
				(atomic_read(&rbs_fn_taskq_pending)));

		mutex_lock(&rbs_fn_taskq_lock);
		if (list_empty(&rbs_fn_taskq)) {
			mutex_unlock(&rbs_fn_taskq_lock);
			continue;
		}
		task = list_first_entry(&rbs_fn_taskq,
				struct rbs_fn_task, entry);
		list_del(&task->entry);
		atomic_dec(&rbs_fn_taskq_pending);
		mutex_unlock(&rbs_fn_taskq_lock);

		switch (task->task) {
		/* internal tasks to be finalized */
		case TASK_FN_SETUP:
			rbs_fn_setup_finish(task);
			rbs_fn_task_int_completed(task);
			rbs_fn_task_free(task);
			break;
		case TASK_FN_RELEASE:
			rbs_fn_release_finish(task);
			rbs_fn_task_int_completed(task);
			rbs_fn_task_free(task);
			break;
		/* external tasks to be executed */
		case TASK_FN_ADD:
			task->result = rbs_fn_add_start(task);
			if (!task->refs) {
				rbs_fn_add_finish(task);
				rbs_fn_task_ext_completed(task);
			}
			break;
		case TASK_FN_REMOVE:
			task->result = rbs_fn_remove_start(task);
			if (!task->refs) {
				rbs_fn_remove_finish(task);
				rbs_fn_task_ext_completed(task);
			}
			break;
		case TASK_FN_LOCK:
			task->result = rbs_fn_lock_start(task);
			if (!task->refs) {
				rbs_fn_lock_finish(task);
				rbs_fn_task_ext_completed(task);
			}
			break;
		default:
			pr_err("%s: undefined task type - %d\n", __func__,
					task->task);
			break;
		}
	}

	return 0;
}

static inline void rbs_fn_task_execute(struct rbs_fn_task *task)
{
	/* put task in the queue and wake up worker */
	mutex_lock(&rbs_fn_taskq_lock);
	list_add_tail(&task->entry, &rbs_fn_taskq);
	atomic_inc(&rbs_fn_taskq_pending);
	mutex_unlock(&rbs_fn_taskq_lock);

	wake_up(&rbs_fn_taskq_event);
	
	/* wait for task completion */
	spin_lock(&task->done_event.lock);
	while (wait_event_interruptible_locked(task->done_event, task->done));
	spin_unlock(&task->done_event.lock);
}

/**
 * rbs_fn_add - add rbs-function
 * @fn: rbs-function descriptor.
 */
int rbs_fn_add(struct rbs_fn *fn)
{
	struct rbs_fn_task task;

	if (!rbs_fn_mod_ready)
		return -EFAULT;

	if (!fn->name || strlen(fn->name) + 1 > RBS_FN_NAME_SIZE)
		return -EINVAL;

	if (current == rbs_fn_task_worker)
		return -EFAULT;

	/* post a new task */
	rbs_fn_task_init(&task, TASK_FN_ADD, fn);
	rbs_fn_task_execute(&task);

	return task.result;
}
EXPORT_SYMBOL(rbs_fn_add);

/**
 * rbs_fn_remove - remove rbs-function
 * @fn: rbs-function descriptor
 */
int rbs_fn_remove(struct rbs_fn *fn)
{
	struct rbs_fn_task task;

	if (!rbs_fn_mod_ready)
		return -EFAULT;

	if (current == rbs_fn_task_worker)
		return -EFAULT;

	/* post a new task */
	rbs_fn_task_init(&task, TASK_FN_REMOVE, fn);
	rbs_fn_task_execute(&task);

	return task.result;
}
EXPORT_SYMBOL(rbs_fn_remove);

/**
 * rbs_fn_lock - lock rbs-function
 * @fn: rbs-function descriptor.
 * @level: lock level
 */
void rbs_fn_lock(struct rbs_fn *fn, int level)
{
	struct rbs_fn_task task;

	if (level < RBS_FN_LOCK_NO || level > RBS_FN_LOCK_HARD)
		return;

	if (!fn->name)
		return;

	if (current == rbs_fn_task_worker)
		return;

	if (!rbs_fn_mod_ready || !fn->p) {
		fn->lock_level = level;
		return;
	}

	/* post a new task */
	rbs_fn_task_init(&task, TASK_FN_LOCK, fn);
	task.param = level;
	rbs_fn_task_execute(&task);
}
EXPORT_SYMBOL(rbs_fn_lock);

/**
 * rbs_fn_getclass - return RBS device class
 */
struct class *rbs_fn_getclass(void)
{
	return rbs_fn_class;
}
EXPORT_SYMBOL(rbs_fn_getclass);

/**
 * rbs_fn_create_file - create sysfs attribute file
 * @fn: rbs-function.
 * @attr: attribute descriptor.
 */
int rbs_fn_create_file(struct rbs_fn *fn,
		       const struct rbs_fn_attribute *attr)
{
	int error = -EINVAL;
	if (fn && fn->p)
		error = sysfs_create_file(&fn->kobj, &attr->attr);
	return error;
}
EXPORT_SYMBOL(rbs_fn_create_file);

/**
 * rbs_fn_remove_bin_file - remove sysfs attribute file
 * @fn: rbs-function.
 * @attr: attribute descriptor.
 */
void rbs_fn_remove_file(struct rbs_fn *fn,
			const struct rbs_fn_attribute *attr)
{
	if (fn && fn->p)
		sysfs_remove_file(&fn->kobj, &attr->attr);
}
EXPORT_SYMBOL(rbs_fn_remove_file);

/**
 * rbs_fn_create_bin_file - create sysfs binary attribute file
 * @fn: rbs-function.
 * @attr: binary attribute descriptor.
 */
int rbs_fn_create_bin_file(struct rbs_fn *fn,
			   const struct bin_attribute *attr)
{
	int error = -EINVAL;
	if (fn && fn->p)
		error = sysfs_create_bin_file(&fn->kobj, attr);
	return error;
}
EXPORT_SYMBOL(rbs_fn_create_bin_file);

/**
 * rbs_fn_remove_bin_file - remove sysfs binary attribute file
 * @fn: rbs-function.
 * @attr: binary attribute descriptor.
 */
void rbs_fn_remove_bin_file(struct rbs_fn *fn,
			    const struct bin_attribute *attr)
{
	if (fn && fn->p)
		sysfs_remove_bin_file(&fn->kobj, attr);
}
EXPORT_SYMBOL(rbs_fn_remove_bin_file);

static int __init rbs_fn_init(void)
{
	int ret;
	dev_t devt;

	rbs_fn_kset = kset_create_and_add("rbs-fn", NULL, NULL);
	if (!rbs_fn_kset)
		return -ENOMEM;

	memset(&rbs_fn_unattached, 0, sizeof(rbs_fn_unattached));
	rbs_fn_unattached.kset = rbs_fn_kset;

	ret = kobject_init_and_add(&rbs_fn_unattached, &rbs_fn_ktype,
			NULL, "unattached");
	if (ret) {
		kset_unregister(rbs_fn_kset);
		pr_err("%s: unable to create unattached kobject\n", __func__);
		return -ENOMEM;
	}

	rbs_fn_class = class_create(THIS_MODULE, "rbsd");
	if (IS_ERR(rbs_fn_class)) {
		pr_err("%s: unable to create class\n", __func__);
		return -EFAULT;
	}
	if (alloc_chrdev_region(&devt, 0, MAX_RBS_FNS, "rbs")) {
		pr_err("%s: unable to allocate device region\n", __func__);
		return -EFAULT;
	}
	rbs_fn_dev_ma = MAJOR(devt);

	INIT_LIST_HEAD(&rbs_fn_list);
	INIT_LIST_HEAD(&rbs_fn_taskq);
	init_waitqueue_head(&rbs_fn_taskq_event);
	atomic_set(&rbs_fn_taskq_pending, 0);

	rbs_fn_workqueue = alloc_workqueue("rbs-fn", WQ_UNBOUND, MAX_RBS_FNS);
	if (!rbs_fn_workqueue) {
		pr_err("%s: unable to allocate workqueue\n", __func__);
		return -ENOMEM;
	}

	rbs_fn_task_worker = kthread_run(rbs_fn_worker, NULL, "krbsfnd");
	if (IS_ERR(rbs_fn_task_worker)) {
		pr_err("%s: unable to spawn worker thread\n", __func__);
		return -ENOMEM;
	}

	rbs_fn_mod_ready = 1;

	return 0;
}
postcore_initcall(rbs_fn_init);

#ifdef CONFIG_PRINTK

static int __rbs_fn_printk(const char *level, const struct rbs_fn *fn,
			   struct va_format *vaf)
{
	if (!fn || !fn->name)
		return printk("%s(NULL device *): %pV", level, vaf);

	return printk("%sRBS %s: %pV",
		      level, fn->name, vaf);
}

int rbs_fn_printk(const char *level, const struct rbs_fn *fn,
		  const char *fmt, ...)
{
	struct va_format vaf;
	va_list args;
	int r;

	va_start(args, fmt);

	vaf.fmt = fmt;
	vaf.va = &args;

	r = __rbs_fn_printk(level, fn, &vaf);
	va_end(args);

	return r;
}
EXPORT_SYMBOL(rbs_fn_printk);

#define define_rbs_fn_printk_level(func, kern_level)		\
	int func(const struct rbs_fn *fn, const char *fmt, ...)	\
{								\
	struct va_format vaf;					\
	va_list args;						\
	int r;							\
								\
	va_start(args, fmt);					\
								\
	vaf.fmt = fmt;						\
	vaf.va = &args;						\
								\
	r = __rbs_fn_printk(kern_level, fn, &vaf);		\
	va_end(args);						\
								\
	return r;						\
}								\
EXPORT_SYMBOL(func);

define_rbs_fn_printk_level(rbs_fn_emerg, KERN_EMERG);
define_rbs_fn_printk_level(rbs_fn_alert, KERN_ALERT);
define_rbs_fn_printk_level(rbs_fn_crit, KERN_CRIT);
define_rbs_fn_printk_level(rbs_fn_err, KERN_ERR);
define_rbs_fn_printk_level(rbs_fn_warn, KERN_WARNING);
define_rbs_fn_printk_level(rbs_fn_notice, KERN_NOTICE);
define_rbs_fn_printk_level(_rbs_fn_info, KERN_INFO);

#endif
