#ifndef RBS_FN_H_
#define RBS_FN_H_

#define RBS_FN_NAME_SIZE	32
#define MAX_RBS_FNS		128

#include <linux/device.h>

#define RBS_FN_PARENT_FNID	0x0

struct rbs_fn;
struct rbs_fn_id;
struct rbs_fn_private;
struct rbs_fn_attribute;

struct rbs_fn_ops {
	int (*setup)(struct rbs_fn *);
	void (*release)(struct rbs_fn *);
	void (*save_ops)(struct rbs_fn *, struct rbs_fn_id *);
	void (*release_ops)(struct rbs_fn *, struct rbs_fn_id *);
};

struct rbs_fn_id {
	const char	*name;
	unsigned int	id;
	struct rbs_fn	*fn;
};
#define RBS_FN_ID(_name, _id) \
	{ \
		.name = _name, \
		.id   = _id, \
	}
#define RBS_FN_ID_NULL {.name = NULL,}

/* FN lock levels */
#define RBS_FN_LOCK_NO		0
#define RBS_FN_LOCK_SOFT	1
#define RBS_FN_LOCK_HARD	2

/* rbs_fn */
struct rbs_fn {
	const char		*name;
	struct module		*owner;
	const char		*parent;

	/* sysfs attributes */
	struct rbs_fn_attribute	*attrs;

	/* operations */
	struct rbs_fn_ops	*ops;

	struct kobject		kobj;

	/* fn lock level */
	int			lock_level;

	void    		*data;
	struct rbs_fn_id	*depend;

	struct rbs_fn_private	*p;

	/* device */
	int			has_device;
	struct device   	*dev;
	const char		*device_name;
};

extern int rbs_fn_add(struct rbs_fn *fn);
extern int rbs_fn_remove(struct rbs_fn *);

extern void rbs_fn_lock(struct rbs_fn *fn, int level);
#define rbs_fn_unlock(_fn) rbs_fn_lock((_fn), RBS_FN_LOCK_NO)

extern struct class *rbs_fn_getclass(void);

static inline void rbs_fn_setdata(struct rbs_fn *fn, void *data)
{
	if (fn)
		fn->data = data;
}

static inline void *rbs_fn_getdata(struct rbs_fn *fn)
{
	if (fn)
		return fn->data;
	return NULL;
}

static inline void *rbs_fn_opsget(struct rbs_fn *fn)
{
	return fn->ops;
}
#define rbs_fn_ops_of(_fn, _type) ((_type *)rbs_fn_opsget(_fn))

struct rbs_fn_attribute {
	struct attribute attr;
	ssize_t (*show)(struct rbs_fn *fn, struct rbs_fn_attribute *attr,
			char *buf);
	ssize_t (*store)(struct rbs_fn *fn, struct rbs_fn_attribute *attr,
			const char *buf, size_t count);
};

#define _RBS_FN_ATTR(_name, _mode, _show, _store) \
	__ATTR(_name, _mode, _show, _store)
#define _RBS_FN_ATTR_NULL __ATTR_NULL

#define RBS_FN_ATTR(_name, _mode, _show, _store) \
struct rbs_fn_attribute rbs_fn_attr_##_name = \
	__ATTR(_name, _mode, _show, _store)

extern int rbs_fn_create_file(struct rbs_fn *fn,
			      const struct rbs_fn_attribute *attr);
extern void rbs_fn_remove_file(struct rbs_fn *fn,
			       const struct rbs_fn_attribute *attr);
extern int rbs_fn_create_bin_file(struct rbs_fn *fn,
			       const struct bin_attribute *attr);
extern void rbs_fn_remove_bin_file(struct rbs_fn *fn,
				   const struct bin_attribute *attr);

#ifdef CONFIG_PRINTK

extern int rbs_fn_printk(const char *level, const struct rbs_fn *fn,
		      const char *fmt, ...)
	__attribute__ ((format (printf, 3, 4)));
extern int rbs_fn_emerg(const struct rbs_fn *fn, const char *fmt, ...)
	__attribute__ ((format (printf, 2, 3)));
extern int rbs_fn_alert(const struct rbs_fn *fn, const char *fmt, ...)
	__attribute__ ((format (printf, 2, 3)));
extern int rbs_fn_crit(const struct rbs_fn *fn, const char *fmt, ...)
	__attribute__ ((format (printf, 2, 3)));
extern int rbs_fn_err(const struct rbs_fn *fn, const char *fmt, ...)
	__attribute__ ((format (printf, 2, 3)));
extern int rbs_fn_warn(const struct rbs_fn *fn, const char *fmt, ...)
	__attribute__ ((format (printf, 2, 3)));
extern int rbs_fn_notice(const struct rbs_fn *fn, const char *fmt, ...)
	__attribute__ ((format (printf, 2, 3)));
extern int _rbs_fn_info(const struct rbs_fn *fn, const char *fmt, ...)
	__attribute__ ((format (printf, 2, 3)));

#else

static inline int rbs_fn_printk(const char *level, const struct rbs_fn *fn,
		      const char *fmt, ...)
	__attribute__ ((format (printf, 3, 4)));
static inline int rbs_fn_printk(const char *level, const struct rbs_fn *fn,
		      const char *fmt, ...)
	 { return 0; }

static inline int rbs_fn_emerg(const struct rbs_fn *fn, const char *fmt, ...)
	__attribute__ ((format (printf, 2, 3)));
static inline int rbs_fn_emerg(const struct rbs_fn *fn, const char *fmt, ...)
	{ return 0; }
static inline int rbs_fn_crit(const struct rbs_fn *fn, const char *fmt, ...)
	__attribute__ ((format (printf, 2, 3)));
static inline int rbs_fn_crit(const struct rbs_fn *fn, const char *fmt, ...)
	{ return 0; }
static inline int rbs_fn_alert(const struct rbs_fn *fn, const char *fmt, ...)
	__attribute__ ((format (printf, 2, 3)));
static inline int rbs_fn_alert(const struct rbs_fn *fn, const char *fmt, ...)
	{ return 0; }
static inline int rbs_fn_err(const struct rbs_fn *fn, const char *fmt, ...)
	__attribute__ ((format (printf, 2, 3)));
static inline int rbs_fn_err(const struct rbs_fn *fn, const char *fmt, ...)
	{ return 0; }
static inline int rbs_fn_warn(const struct rbs_fn *fn, const char *fmt, ...)
	__attribute__ ((format (printf, 2, 3)));
static inline int rbs_fn_warn(const struct rbs_fn *fn, const char *fmt, ...)
	{ return 0; }
static inline int rbs_fn_notice(const struct rbs_fn *fn, const char *fmt, ...)
	__attribute__ ((format (printf, 2, 3)));
static inline int rbs_fn_notice(const struct rbs_fn *fn, const char *fmt, ...)
	{ return 0; }
static inline int _rbs_fn_info(const struct rbs_fn *fn, const char *fmt, ...)
	__attribute__ ((format (printf, 2, 3)));
static inline int _rbs_fn_info(const struct rbs_fn *fn, const char *fmt, ...)
	{ return 0; }

#endif

#define rbs_fn_info(dev, fmt, arg...) _rbs_fn_info(dev, fmt, ##arg)

#if defined(DEBUG)
#define rbs_fn_dbg(dev, format, arg...)		\
	rbs_fn_printk(KERN_DEBUG, dev, format, ##arg)
#else
#define rbs_fn_dbg(dev, format, arg...)				\
({								\
	if (0)							\
		rbs_fn_printk(KERN_DEBUG, dev, format, ##arg);	\
	0;							\
})
#endif

#endif /* !RBS_FN_H_ */
