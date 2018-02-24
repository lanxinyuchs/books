#include <stdlib.h>
#include <stdint.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <string.h>
#include <limits.h>
#include <stdio.h>
#include <dirent.h>
#include <string.h>
#include <errno.h>
#include <pthread.h>
#include <semaphore.h>
#include "uio_helper.h"

#define UIO_ROOT "/sys/class/uio"

#define UIO_DEV_SIZE 16
#define UIO_DEV_PATH_SIZE 32

/* This should be the same as in include/linux/uio_driver.h! */
#define MAX_UIO_MAPS 5

struct uio_notify_context {
        pthread_t thread;
        /* It is tempting to use pthread_self() as an invalid value
         * of 'thread'. However, this assumes bind/unbind calls are
         * always made from the same thread! Using a flag is a lot
         * less error prone at the cost of a few bytes. */
        int thread_started;
        uio_irq_notifier_func irq_cb;
        uio_irq_init_func init_cb;
        uio_irq_destroy_func destroy_cb;
        void *irq_data; /* for irq_cb user data */
        void *bind_data; /* for init_cb and destroy_cb user data */
        sem_t mutex;
};

struct uio_handle {
        char *dev;
        int fd;
        int maps;       /* Number of actual maps in use */
        void *maddr[MAX_UIO_MAPS];
        size_t msize[MAX_UIO_MAPS];
        char *mname[MAX_UIO_MAPS];
        struct uio_notify_context *context;
};

#ifdef USE_MM_TABLE_

#define MM_TABLE_SZ 64

struct mm_info {
        uintptr_t addr;      /* mmap() address */
        size_t size;         /* mmap() size in bytes */
        uint32_t users;      /* reference counter */
        int obsolete;        /* scheduled for removal or not by compact_table() */
};

static int next_mm_idx = 0;
static struct mm_info mm_table[MM_TABLE_SZ] = {{0,},};

/******************************************************************************
 *
 * Local function:
 *
 * Description:
 *
 * Side effects:
 *
 *****************************************************************************/
static void remap_table(int fd)
{
        int i;

        for (i = 0; i < next_mm_idx; i++) {
                struct mm_info *m = &mm_table[i];

                if (m->obsolete) {
                        /* remap and reinstate entry */
                        (void)mmap((void *)m->addr, m->size, PROT_READ |
                                PROT_WRITE, MAP_SHARED, fd, 0);
                        m->obsolete = 0;
                }
        }
}

/******************************************************************************
 *
 * Local function:
 *
 * Description:
 *
 * Side effects:
 *
 *****************************************************************************/
static void compact_table()
{
        int i, j;

        j = next_mm_idx;
        next_mm_idx = 0;
        for (i = 0; i < j; i++) {
                struct mm_info *m = &mm_table[i];

                if (!m->obsolete) {
                        mm_table[next_mm_idx].addr = m->addr;
                        mm_table[next_mm_idx].size = m->size;
                        mm_table[next_mm_idx].users = m->users;
                        mm_table[next_mm_idx].obsolete = 0;
                        ++next_mm_idx;
                } else {
                        mm_table[i].users = 0;
                        mm_table[i].obsolete = 0;
                }
        }
}

#endif

/******************************************************************************
 *
 * Local function:
 *
 * Description:
 *
 * Side effects:
 *
 *****************************************************************************/
static const char *get_dev_name(char (*dev)[], const char *name)
{
        DIR *dir;
        struct dirent *dir_e;

        dir = opendir(UIO_ROOT);
        dir_e = readdir(dir);
        while ((dir_e = readdir(dir))) {
                if (dir_e) {
                        FILE *fdd;
                        char name_path[255];
                        char name_buf[64];

                        if (strstr(dir_e->d_name, "."))
                                continue;

                        snprintf(name_path, sizeof(name_path),
                                 UIO_ROOT "/%s/name",
                                 dir_e->d_name);

                        fdd = fopen(name_path, "r");
                        if (fdd == NULL) {
                                closedir(dir);
                                return NULL;
                        }
                        size_t n = fread(name_buf, 1, sizeof(name_buf), fdd);
                        /* Strip last character from buffer in case data was
                         * read since it is most likely a line feed (LF). */
                        name_buf[(n ? n - 1 : 0)] = 0;
                        if (!strcmp(name, name_buf)) {
                                sprintf(*dev, "%s", dir_e->d_name);
                                closedir(dir);
                                fclose(fdd);
                                return *dev;
                        }
                        fclose(fdd);
                }
        }
        closedir(dir);
        return NULL;
}

/******************************************************************************
 *
 * Local function:
 *
 * Description:
 *
 * Side effects:
 *
 *****************************************************************************/
static const char *get_map_property(char (*prop_out)[], const char *prop,
                        const char *dev, int map_no)
{
        FILE *fp;
        char path[255];

        snprintf(path, sizeof(path), UIO_ROOT "/%s/maps/map%u/%s", dev,
                        map_no, prop);
        fp = fopen(path, "r");
        if (fp == NULL)
                return NULL;
        fgets(*prop_out, 32, fp);
        fclose(fp);

        return *prop_out;
}

/******************************************************************************
 *
 * Local function:
 *
 * Description:
 *
 * Side effects:
 *
 *****************************************************************************/
static int get_map_by_name(struct uio_handle *hdl, const char *name)
{
        int i;

        for (i = 0; i < hdl->maps; i++) {
                if (!strcmp(name, hdl->mname[i]))
                        return i;
        }
        return -1;
}

/******************************************************************************
 *
 * Global function:
 *
 * Description:
 *
 * Side effects:
 *
 *****************************************************************************/
UIO_HANDLE_ uio_open(const char *name)
{
        char dev[UIO_DEV_SIZE];
        char dev_path[UIO_DEV_PATH_SIZE];
        int fd = 0;
        struct uio_handle *hdl = NULL;
        int i;

        if (get_dev_name(&dev, name) == NULL) {
                fprintf(stderr, "uio_helper: failed to open \"%s\"\n", name);
                goto open_error;
        }
        snprintf(dev_path, UIO_DEV_PATH_SIZE, "/dev/%s", dev);
        dev_path[UIO_DEV_PATH_SIZE - 1] = '\0';

        fd = open(dev_path, O_RDWR | O_SYNC);
        if (fd == -1) {
                fprintf(stderr, "uio_helper: failed to open \"%s\"\n", dev_path);
                goto open_error;
        }
        hdl = malloc(sizeof(struct uio_handle));
        if (hdl == NULL) {
                fprintf(stderr, "uio_helper: out of memory\n");
                goto open_error;
        }
        hdl->context = malloc(sizeof(struct uio_notify_context));
        if (hdl->context == NULL) {
                fprintf(stderr, "uio_helper: out of memory\n");
                goto open_error;
        }

        hdl->fd = fd;
        hdl->dev = strdup(dev);
        for (i = 0; i <= MAX_UIO_MAPS; i++) {
                char map_name[32];
                if (get_map_property(&map_name, "name", dev, i) == NULL)
                        break;
                hdl->maddr[i] = NULL;
                hdl->msize[i] = 0;
                hdl->mname[i] = strdup(map_name);
        }
        hdl->maps = i;
        hdl->context->thread_started = 0;
        hdl->context->irq_cb = NULL;
        hdl->context->init_cb = NULL;
        hdl->context->destroy_cb = NULL;
        hdl->context->irq_data = NULL;
        hdl->context->bind_data = NULL;

        return (UIO_HANDLE_)hdl;

open_error:

        if (fd)
                close(fd);
        if (hdl) {
                if (hdl->context)
                        free(hdl->context);
                free(hdl);
        }
        return UIO_OPEN_FAILED;
}

/******************************************************************************
 *
 * Global function:
 *
 * Description:
 *
 * Side effects:
 *
 *****************************************************************************/
int uio_getfd(UIO_HANDLE_ uio_handle)
{
        struct uio_handle *hdl = (struct uio_handle *)uio_handle;
        if (!hdl || hdl == UIO_OPEN_FAILED) {
                errno = EINVAL;
                return -1;
        }
        return hdl->fd;
}

/******************************************************************************
 *
 * Global function:
 *
 * Description:
 *
 * Side effects:
 *
 *****************************************************************************/
void uio_close(UIO_HANDLE_ uio_handle)
{
        int i;
        struct uio_handle *hdl = (struct uio_handle *)uio_handle;

        if (hdl && hdl != UIO_OPEN_FAILED) {
                uio_unbind_irq(hdl);
                uio_munmap(hdl);
                if (hdl->context)
                        free(hdl->context);
                if (hdl->dev)
                        free(hdl->dev);
                for (i = 0; i < hdl->maps; i++) {
                        if (hdl->mname[i])
                                free(hdl->mname[i]);
                }
                close(hdl->fd);
                free(hdl);
        }
}

/******************************************************************************
 *
 * Local function:
 *
 * Description:
 *
 * Side effects:
 *
 *****************************************************************************/

#define WITHIN_(b, s, a) ((a) >= (b) && (a) <= ((b) + (s)))

static void *__uio_mmap(struct uio_handle *hdl, int map)
{
        uint32_t base;
        uint32_t offset;
        uint32_t size;
        size_t page_size;
        size_t page_offset;
        size_t map_size;
        uintptr_t map_addr;
        char tmp[32];

        if (get_map_property(&tmp, "addr", hdl->dev, map) == NULL) {
                errno = EINVAL;
                return MAP_FAILED;
        }
        base = strtoul(tmp, NULL, 16);
        if (get_map_property(&tmp, "offset", hdl->dev, map) == NULL) {
                errno = EINVAL;
                return MAP_FAILED;
        }
        offset = strtoul(tmp, NULL, 16);
        if (get_map_property(&tmp, "size", hdl->dev, map) == NULL) {
                errno = EINVAL;
                return MAP_FAILED;
        }
        size = strtoul(tmp, NULL, 16);

        /* XXX Maybe 'offset' can be used instead of below calculation? */
        page_size = sysconf(_SC_PAGESIZE);
        page_offset = base % page_size;

        /* Calculate actual size spanning over the whole required page range */
        map_size = (page_offset + size + page_size - 1) & ~(page_size - 1);
        map_addr = base - page_offset;

#ifdef USE_MM_TABLE_
        if (next_mm_idx == MM_TABLE_SZ) {
                errno = ENOMEM;
                return MAP_FAILED;
        }

        int i;
        uint32_t users = 1;
        for (i = 0; i < next_mm_idx; ++i) {
                struct mm_info *m = &mm_table[i];

                if ((map_addr >= m->addr) &&
                    (map_addr + map_size) <= (m->addr + m->size)) {
                        /* No mmap() needed */
                        m->users++;          /* step reference count only */
                        hdl->maddr[map] = (void *)map_addr;
                        hdl->msize[map] = map_size;
                        return (void *)base;
                } else if ((m->addr >= map_addr) &&
                           (m->addr + m->size) <= (map_addr + map_size)) {
                        /* munmap() needed */
                        munmap((void *) m->addr, m->size);
                        m->obsolete = 1;
                        users += m->users;    /* propagate reference count */
                } else if (WITHIN_(map_addr, map_size, m->addr) ||
                           WITHIN_(map_addr, map_size, m->addr + m->size)) {
                        /* munmap() needed */
                        munmap((void *) m->addr, m->size);
                        if (m->addr > map_addr) {
                                map_size += ((m->addr + m->size) - (map_addr + map_size));
                        } else {
                                map_size += (map_addr - m->addr);
                                map_addr = m->addr;
                        }
                        m->obsolete = 1;
                        users += m->users;    /* propagate reference count */
                }
        }
#endif

        map_addr = (uintptr_t)mmap((void *)map_addr, map_size, PROT_READ |
                        PROT_WRITE, MAP_SHARED, hdl->fd, 0);
        if (MAP_FAILED == (void *)map_addr) {
                /* Failed to map memory region.
                   If the MM table is used this is an even more severe condition
                   since we might have already unmapped some overlapping areas!
                   Try to fix that. */
#ifdef USE_MM_TABLE_
                remap_table(hdl->fd);
#endif
                return MAP_FAILED;
        }

#ifdef USE_MM_TABLE_
        mm_table[next_mm_idx].addr = map_addr;
        mm_table[next_mm_idx].size = map_size;
        mm_table[next_mm_idx].users = users;
        next_mm_idx++;
        compact_table();
#endif
        hdl->maddr[map] = (void *)map_addr;
        hdl->msize[map] = map_size;
        /* coverity[overflow_sink] Overflow very unlikely if mmap() succeeds. */
        return (void *)(map_addr + page_offset);
}

#undef WITHIN_

/******************************************************************************
 *
 * Global function:
 *
 * Description:
 *
 * Side effects:
 *
 *****************************************************************************/
void *uio_mmap(UIO_HANDLE_ uio_handle)
{
        struct uio_handle *hdl = (struct uio_handle *)uio_handle;

        if (!hdl || hdl == UIO_OPEN_FAILED) {
                errno = EINVAL;
                return MAP_FAILED;
        }

        return __uio_mmap(hdl, 0);
}

/******************************************************************************
 *
 * Global function:
 *
 * Description:
 *
 * Side effects:
 *
 *****************************************************************************/
void *uio_mmap_by_name(UIO_HANDLE_ uio_handle, const char *name)
{
        int map;
        struct uio_handle *hdl = (struct uio_handle *)uio_handle;

        if (!hdl || hdl == UIO_OPEN_FAILED) {
                errno = EINVAL;
                return MAP_FAILED;
        }

        map = get_map_by_name(hdl, name);
        if (map == -1) {
                errno = EINVAL;
                return MAP_FAILED;
        }

        return __uio_mmap(hdl, map);
}

/******************************************************************************
 *
 * Local function:
 *
 * Description:
 *
 * Side effects:
 *
 *****************************************************************************/
static int __uio_munmap(struct uio_handle *hdl, int map)
{
        if (!hdl->maddr[map]) {
                errno = EINVAL;
                return -1;
        }

        size_t map_size = hdl->msize[map];
        uintptr_t map_addr = (uintptr_t)hdl->maddr[map];

#ifdef USE_MM_TABLE_
        int i;
        for (i = 0; i < next_mm_idx; i++) {
                struct mm_info *m = &mm_table[i];

                if ((map_addr >= (uintptr_t)m->addr) &&
                    (map_addr + map_size) <= (m->addr + m->size)) {
                        if (m->users == 1) {
                                if (munmap((void *)m->addr, m->size) == -1)
                                        return -1;
                                m->obsolete = 1;
                                compact_table();
                                hdl->maddr[map] = NULL;
                                hdl->msize[map] = 0;
                                return 0;
                        }
                        m->users--;
                        hdl->maddr[map] = NULL;
                        hdl->msize[map] = 0;
                        return 0;
                }
        }
        /* Mapping was not found! */
        errno = EINVAL;
        return -1;
#else
        if (munmap((void *)map_addr, map_size) == -1)
                return -1;
        hdl->maddr[map] = NULL;
        hdl->msize[map] = 0;
        return 0;
#endif
}

/******************************************************************************
 *
 * Global function:
 *
 * Description:
 *
 * Side effects:
 *
 *****************************************************************************/
int uio_munmap(UIO_HANDLE_ uio_handle)
{
        struct uio_handle *hdl = (struct uio_handle *)uio_handle;

        if (hdl == NULL || hdl == UIO_OPEN_FAILED) {
                errno = EINVAL;
                return -1;
        }

        return __uio_munmap(hdl, 0);
}

/******************************************************************************
 *
 * Global function:
 *
 * Description:
 *
 * Side effects:
 *
 *****************************************************************************/
int uio_munmap_by_name(UIO_HANDLE_ uio_handle, const char *name)
{
        int map;
        struct uio_handle *hdl = (struct uio_handle *)uio_handle;

        if (!hdl || hdl == UIO_OPEN_FAILED) {
                errno = EINVAL;
                return -1;
        }

        map = get_map_by_name(hdl, name);
        if (map == -1) {
                errno = EINVAL;
                return -1;
        }

        return __uio_munmap(hdl, map);
}

#define no_warn_result_ void*ignore_result_;ignore_result_=(void*)(uintptr_t)

void uio_enable_irq(UIO_HANDLE_ uio_handle)
{
        struct uio_handle *hdl = (struct uio_handle *)uio_handle;
        uint32_t tmp = 0x1;

        if (!hdl || hdl == UIO_OPEN_FAILED)
                return;

        no_warn_result_ write(hdl->fd, &tmp, sizeof(uint32_t));
}

void uio_disable_irq(UIO_HANDLE_ uio_handle)
{
        struct uio_handle *hdl = (struct uio_handle *)uio_handle;
        uint32_t tmp = 0x0;

        if (!hdl || hdl == UIO_OPEN_FAILED)
                return;

        no_warn_result_ write(hdl->fd, &tmp, sizeof(uint32_t));
}

static void __wait_irq(struct uio_handle *hdl)
{
        int nfsd = hdl->fd + 1;

        while (1) {
                fd_set rd;

                FD_ZERO(&rd);
                FD_SET(hdl->fd, &rd);

                int retval = select(nfsd, &rd, NULL, NULL, NULL);
                if (retval == -1)
                        perror("select()");
                else if (retval) {
                        if (FD_ISSET(hdl->fd, &rd)) {
                                uint32_t tmp = 0;
                                no_warn_result_ read(hdl->fd, &tmp, 4);
                                if (tmp)
                                        return;
                        }
                }
        }
}

static void *__thread_loop(struct uio_handle *hdl)
{
         while (1) {
                 __wait_irq(hdl);
                 if (hdl->context->irq_cb)
                         hdl->context->irq_cb(hdl->context->irq_data);
         }
}

static void *uio_irq_thread(void *data)
{
        struct uio_handle *hdl = (struct uio_handle *)data;

        if (hdl->context->init_cb) {
                if (hdl->context->init_cb(hdl->context->bind_data)) {
                        sem_post(&hdl->context->mutex);
                        return NULL;
                }
        }
        hdl->context->thread_started = 1;
        sem_post(&hdl->context->mutex);
        if (hdl->context->destroy_cb) {
                 /* POSIX.1 permits pthread_cleanup_push() and
                  * pthread_cleanup_pop() to be implemented as macros that
                  * expand to text containing '{' and  '}', respectively.
                  * For this reason, the caller must ensure that calls to these
                  * functions are paired within the same function, and at the
                  * same lexical nesting level. (In other words, a clean-up
                  * handler is only established during the execution of a
                  * specified section of code.) */
                pthread_cleanup_push(hdl->context->destroy_cb,
                                hdl->context->bind_data);
                __thread_loop(hdl);
                pthread_cleanup_pop(0);
         }

         __thread_loop(hdl);
         return NULL;
}

/**
 * This function must be called with UIO top interrupt disabled.
 */
int uio_irq_set_notifier(UIO_HANDLE_ uio_handle, uio_irq_notifier_func cb,
                         void *data)
{
        struct uio_handle *hdl = (struct uio_handle *)uio_handle;
        if (!hdl || hdl == UIO_OPEN_FAILED || !hdl->context) {
                errno = EINVAL;
                return -1;
        }
        hdl->context->irq_cb = cb;
        hdl->context->irq_data = data;
        return 0;
}

static int __uio_bind_irq(UIO_HANDLE_ uio_handle, int rt, int prio,
                uio_irq_init_func init_cb, uio_irq_destroy_func destroy_cb,
                void *data)
{
        struct uio_handle *hdl = (struct uio_handle *)uio_handle;
        pthread_attr_t attr;

        if (!hdl || hdl == UIO_OPEN_FAILED || !hdl->context) {
                errno = EINVAL;
                return -1;
        }

        hdl->context->thread_started = 0;

        if (pthread_attr_init(&attr)) {
                perror("pthread_attr_init: failed to initialize thread");
                return -1;
        }
        /* For portability, explicitly create threads in a joinable state */
        pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_JOINABLE);

        if (rt && prio >= sched_get_priority_min(SCHED_FIFO) &&
            prio <= sched_get_priority_max(SCHED_FIFO)) {
                struct sched_param param;

                pthread_attr_setschedpolicy(&attr, SCHED_FIFO);
                param.sched_priority = prio;
                pthread_attr_setschedparam(&attr, &param);
                pthread_attr_setinheritsched (&attr, PTHREAD_EXPLICIT_SCHED);
        }

        hdl->context->init_cb = init_cb;
        hdl->context->destroy_cb = destroy_cb;
        hdl->context->bind_data = data;

        if (sem_init(&hdl->context->mutex, 0, 0)) {
                perror("sem_init: failed to initialize semaphore");
                return -1;
        }

        if (pthread_create(&hdl->context->thread, &attr, uio_irq_thread,
                           (void *)hdl)) {
                perror("pthread_create: failed to start thread");
                return -1;
        }
        sem_wait(&hdl->context->mutex);
        sem_destroy(&hdl->context->mutex);
        if (!hdl->context->thread_started) {
                errno = EFAULT;
                return -1;
        }

        return 0;
}

int uio_bind_irq(UIO_HANDLE_ uio_handle)
{
        return __uio_bind_irq(uio_handle, 0, 0, NULL, NULL, NULL);
}

int uio_bind_irq2(UIO_HANDLE_ uio_handle, uio_irq_init_func init_cb,
                uio_irq_destroy_func destroy_cb, void *data)

{
        return __uio_bind_irq(uio_handle, 0, 0, init_cb, destroy_cb, data);
}

int uio_bind_irq_rt(UIO_HANDLE_ uio_handle, int prio)
{
        return __uio_bind_irq(uio_handle, 1, prio, NULL, NULL, NULL);
}

int uio_bind_irq2_rt(UIO_HANDLE_ uio_handle, int prio,
                uio_irq_init_func init_cb, uio_irq_destroy_func destroy_cb,
                void *data)
{
        return __uio_bind_irq(uio_handle, 1, prio, init_cb, destroy_cb, data);
}

int uio_unbind_irq(UIO_HANDLE_ uio_handle)
{
        /* Stop thread */
        struct uio_handle *hdl = (struct uio_handle *)uio_handle;

        if (!hdl || hdl == UIO_OPEN_FAILED || !hdl->context) {
                errno = EINVAL;
                return -1;
        }
        if (hdl->context->thread_started) {
                pthread_cancel(hdl->context->thread);
                pthread_join(hdl->context->thread, NULL);
                hdl->context->thread_started = 0;
        }
        return 0;
}

int uio_wait_irq(UIO_HANDLE_ uio_handle)
{
        struct uio_handle *hdl = (struct uio_handle *)uio_handle;

        if (!hdl || hdl == UIO_OPEN_FAILED || !hdl->context) {
                errno = EINVAL;
                return -1;
        }
        __wait_irq(uio_handle);
        return 0;
}

