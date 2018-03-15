#ifndef ULH_REF_H__
#define ULH_REF_H__

#include <pthread.h>

struct ulh_ref {
	int refs;
	void (*free)(struct ulh_ref *);
};

static inline int __chgref(struct ulh_ref *ref, int step)
{
	int ret, old;
	do {
		old = *((volatile int *) &ref->refs);
		ret = __sync_bool_compare_and_swap(&ref->refs, old,
				old + step);
	} while (!ret);

	return old + step;
}

static inline int __readref(struct ulh_ref *ref)
{
	return *((volatile int *) &ref->refs);
}

static inline void ulh_init_ref(struct ulh_ref *ref, int val,
		void (*free)(struct ulh_ref *))
{
	ref->refs = val;
	ref->free = free;
}

static inline int ulh_hold_ref(struct ulh_ref *ref)
{
	return __chgref(ref, 1);
}

static inline int ulh_unhold_ref(struct ulh_ref *ref)
{
	int val;

	val = __chgref(ref, -1);
	if (!val && ref->free)
		ref->free(ref);

	return val;
}

static inline int ulh_read_ref(struct ulh_ref *ref)
{
	return __readref(ref);
}

#endif /* ULH_REF_H__ */
