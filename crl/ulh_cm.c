#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <pthread.h>

#include <ulh_dl_list.h>

#include "ulh_cm.h"
#include "ulh_ref.h"

struct ulh_cm {
	char *name;
	struct ulh_cm_ops *ops;
	void *param;
	struct ulh_ref ref;
	struct dl_list link;

	pthread_mutex_t wait_lock;
	pthread_cond_t wait;
};

struct ulh_cm_global {
	struct dl_list cms;
	pthread_mutex_t lock;
};

static struct ulh_cm_global *gcm;

static struct ulh_cm *__get_cm(const char *name)
{
	struct ulh_cm *cm;

	dl_list_foreach(cm, &gcm->cms, link) {
		if (!strcmp(cm->name, name))
			return cm;
	}
	return NULL;
}

int ulh_cm_create_instance(const char *cm_name, const char *name,
		struct ulh_cm_instance *instance,
		struct ulh_cm_config *config, struct ulh_timerqueue *tqueue)
{
	struct ulh_cm *cm = NULL;
	int ret;
	
	if (!gcm)
		return -EFAULT;
	if (!name || !instance)
		return -EINVAL;

	pthread_mutex_lock(&gcm->lock);
	cm = __get_cm(cm_name);
	if (!cm || !cm->ops->create_instance) {
		pthread_mutex_unlock(&gcm->lock);
		return -ENOENT;
	}
	ulh_hold_ref(&cm->ref);
	instance->cm_ref = cm;
	pthread_mutex_unlock(&gcm->lock);

	ret = cm->ops->create_instance(cm->param, name, instance, config,
			tqueue);
	if (ret)
		ulh_unhold_ref(&cm->ref);

	return ret;
}

int ulh_cm_destroy_instance(struct ulh_cm_instance *instance)
{
	struct ulh_cm *cm;
	int ret = 0;

	if (!gcm)
		return -EFAULT;
	if (!instance || !instance->cm_ref)
		return -EINVAL;

	cm = instance->cm_ref;
	if (cm->ops->destroy_instance)
		ret = cm->ops->destroy_instance(cm->param, instance);
	if (!ret) {
		instance->cm_ref = NULL;
		ulh_unhold_ref(&cm->ref);
	}

	return ret;
}

static void __free_cm(struct ulh_ref *ref)
{
	struct ulh_cm *cm = container_of(ref, struct ulh_cm, ref);

	pthread_mutex_lock(&cm->wait_lock);
	pthread_cond_broadcast(&cm->wait);
	pthread_mutex_unlock(&cm->wait_lock);
}

int ulh_cm_init(void)
{
	if (gcm)
		return -EALREADY;
	gcm = malloc(sizeof(*gcm));
	if (!gcm)
		return -ENOMEM;
	pthread_mutex_init(&gcm->lock, NULL);
	dl_list_init(&gcm->cms);

	return 0;
}

int ulh_cm_register(const char *name, struct ulh_cm_ops *ops, void *param)
{
	struct ulh_cm *cm = NULL;
	int ret = -ENOMEM;

	if (!gcm)
		return -EFAULT;

	pthread_mutex_lock(&gcm->lock);
	cm = __get_cm(name);
	if (cm) {
		ret = -EBUSY;
		goto out;
	}
	cm = malloc(sizeof(*cm));
	if (!cm)
		goto out;
	cm->name = strdup(name);
	if (!cm->name)
		goto out;

	pthread_mutex_init(&cm->wait_lock, NULL);
	pthread_cond_init(&cm->wait, NULL);

	ulh_init_ref(&cm->ref, 1, __free_cm);
	cm->ops = ops;
	cm->param = param;
	dl_list_insert_tail(&gcm->cms, &cm->link);

	ret = 0;
	cm = NULL;

out:
	if (cm) {
		if (cm->name)
			free(cm->name);
		free(cm);
	}

	pthread_mutex_unlock(&gcm->lock);
	return ret;
}

int ulh_cm_unregister(const char *name)
{
	struct ulh_cm *cm = NULL;

	if (!gcm)
		return -EFAULT;

	pthread_mutex_lock(&gcm->lock);
	cm = __get_cm(name);
	if (!cm) {
		pthread_mutex_unlock(&gcm->lock);
		return 0;
	}

	dl_list_remove(&cm->link);
	pthread_mutex_unlock(&gcm->lock);

	ulh_unhold_ref(&cm->ref);

	pthread_mutex_lock(&cm->wait_lock);
	while (ulh_read_ref(&cm->ref))
		pthread_cond_wait(&cm->wait, &cm->wait_lock);
	pthread_mutex_unlock(&cm->wait_lock);

	if (cm->ops->destroy)
		cm->ops->destroy(cm->param);

	free(cm);
	return 0;
}

