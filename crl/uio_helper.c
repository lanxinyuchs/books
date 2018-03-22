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
        size_t moffset[MAX_UIO_MAPS];
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
 *****************************************************************************/
static const char *get_dev_name(char *dev, const char *name)
{
        DIR *dir;
        struct dirent *dir_e;

        dir = opendir(UIO_ROOT);
        while ((dir_e = readdir(dir)) != NULL) {
                FILE *fdd;
                char name_path[255];
                char name_buf[64];

                if (strncmp(dir_e->d_name, "uio", 3))
                        continue;

                snprintf(name_path, sizeof(name_path),
                         UIO_ROOT "/%s/name",
                         dir_e->d_name);

                fdd = fopen(name_path, "r");
                if (fdd == NULL) {
                        (void)closedir(dir);
                        return NULL;
                }
                size_t n = fread(name_buf, 1, sizeof(name_buf), fdd);
                /* Strip last character from buffer in case data was
                 * read since it is most likely a line feed (LF). */
                name_buf[(n ? n - 1 : 0)] = 0;
                if (!strcmp(name, name_buf)) {
                        sprintf(dev, "%s", dir_e->d_name);
                        (void)closedir(dir);
                        fclose(fdd);
                       return dev;
                }
                fclose(fdd);
        }
        (void)closedir(dir);
        return NULL;
}

/******************************************************************************
 *
 *****************************************************************************/
static const char *get_map_property(char *prop_out, const char *prop,
                        const char *dev, int map_no)
{
        FILE *fp;
        char path[255];
        char prop_buf[32];

        snprintf(path, sizeof(path), UIO_ROOT "/%s/maps/map%u/%s", dev,
                        map_no, prop);
        fp = fopen(path, "r");
        if (fp == NULL)
                return NULL;

        size_t n = fread(prop_buf, 1, sizeof(prop_buf), fp);
        /* Strip last character from buffer in case data was
         * read since it is most likely a line feed (LF). */
        prop_buf[(n ? n - 1 : 0)] = 0;
        strcpy(prop_out, prop_buf);

        fclose(fp);

        return prop_out;
}

/******************************************************************************
 *
 *****************************************************************************/
static int get_map_by_name(const struct uio_handle *hdl, const char *name)
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
 *****************************************************************************/
static ssize_t get_map_offset(const char *desc)
{
        size_t s_pos = strlen(desc);
        while (--s_pos) {
                if (desc[s_pos] == '@') {
                        char *end_ptr;
                        errno = 0;
                        long off = strtol(&desc[s_pos + 1], &end_ptr, 16);
                        if (*end_ptr || end_ptr == &desc[s_pos + 1] || errno)
                                break;
                        if (off >= sysconf(_SC_PAGESIZE))
                                break;
                        return (ssize_t)off;
                }
        }
        return -1;
}

/******************************************************************************
 *
 *****************************************************************************/
static const char *get_map_name(const char *desc, char *name)
{
        size_t s_pos = strlen(desc);
        while (--s_pos) {
                if (desc[s_pos] == '@') {
                        (void)strncpy(name, desc, s_pos);
                        name[s_pos] = 0;
                        return name;
                }
        }
        return desc;
}

/******************************************************************************
 *
 *****************************************************************************/
UIO_HANDLE_ uio_open(const char *name)
{
        char dev[UIO_DEV_SIZE];
        char dev_path[UIO_DEV_PATH_SIZE];
        int fd = 0;
        struct uio_handle *hdl = NULL;
        int i;

        if (get_dev_name(dev, name) == NULL) {
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
        for (i = 0; i < MAX_UIO_MAPS; i++) {
                char map_desc[32];
                char map_name[32];
                int offset;
                if (get_map_property(map_desc, "name", dev, i) == NULL)
                        break;
                hdl->maddr[i] = NULL;
                hdl->msize[i] = 0;
                /* Get the page offset from the special '@' pattern in the
                 * map name/descriptor. Only strip off the offset from the
                 * descriptor if extraction succeeds. */
                offset = get_map_offset(map_desc);
                if (offset >= 0) {
                        hdl->mname[i] = strdup(get_map_name(map_desc, map_name));
                } else {
                        hdl->mname[i] = strdup(map_desc);
                        offset = 0;
                }
                hdl->moffset[i] = (size_t)offset;
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
 *****************************************************************************/
void uio_close(UIO_HANDLE_ uio_handle)
{
        int i;
        struct uio_handle *hdl = (struct uio_handle *)uio_handle;

        if (hdl && hdl != UIO_OPEN_FAILED) {
                (void)uio_unbind_irq(hdl);
                (void)uio_munmap(hdl);
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
 *****************************************************************************/

#define WITHIN_(b, s, a) ((a) >= (b) && (a) <= ((b) + (s)))

static void *__uio_mmap(struct uio_handle *hdl, int map)
{
        uint32_t base;
        uint32_t size;
        size_t page_size;
        size_t page_offset;
        size_t map_size;
        uintptr_t map_addr;
        char tmp[32];
        char *end_ptr;

        if (get_map_property(tmp, "addr", hdl->dev, map) == NULL) {
                errno = EINVAL;
                return MAP_FAILED;
        }
        errno = 0;
        base = strtoul(tmp, &end_ptr, 16);
        if (*end_ptr || end_ptr == tmp || errno) {
                errno = EINVAL;
                return MAP_FAILED;
        }
        if (get_map_property(tmp, "size", hdl->dev, map) == NULL) {
                errno = EINVAL;
                return MAP_FAILED;
        }
        errno = 0;
        size = strtoul(tmp, &end_ptr, 16);
        if (*end_ptr || end_ptr == tmp || errno) {
                errno = EINVAL;
                return MAP_FAILED;
        }

        page_size = (size_t)sysconf(_SC_PAGESIZE);
        if ((ssize_t)page_size < 0)
                page_size = 4096; /* Try a page size of 4k to mmap() */
        page_offset = base % page_size;

        /* This used to be a calculation of the actual size spanning over the
         * whole required page range. However, more recent versions of UIO
         * has changed semantics regarding the provided size of the area to
         * map and now check things like:
         *
         *    if (vma->vm_end - vma->vm_start > mem->size) {
         *            return -EINVAL;
         *    }
         *
         * This check effectively destroys any possibilities for the user
         * to stay unaware of the actual size required by mmap() since the
         * size given must match the actual size that is caculated below
         * and must also be a multiple of the page size! For that reason
         * we must fully trust that the user provided a size big enough
         * (including possible passing of page boundaries). All we can do
         * here is making sure it is a multiple of the page size, which will
         * make it work also in the case for UIO version not applying these
         * strict rules. But it still requires that the user takes page
         * boundaries into consideration due to the relaxed calculation. */
        map_size = (size + page_size - 1) & ~(page_size - 1);

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
                        PROT_WRITE, MAP_SHARED, hdl->fd, map * (long)page_size);
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
        return (void *)(map_addr +
                        (page_offset ? page_offset :
                        hdl->maps ? hdl->moffset[map] :
                        0));
}

#undef WITHIN_

/******************************************************************************
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

/*lint -esym(438, ignore_result_) Last value assigned to variable not used */
/*lint -esym(550, ignore_result_) Symbol not accessed */
#define no_warn_result_ void*ignore_result_;ignore_result_=(void*)(uintptr_t)

/******************************************************************************
 *
 *****************************************************************************/
void uio_enable_irq(UIO_HANDLE_ uio_handle)
{
        struct uio_handle *hdl = (struct uio_handle *)uio_handle;
        uint32_t tmp = 0x1;

        if (!hdl || hdl == UIO_OPEN_FAILED)
                return;

        no_warn_result_ write(hdl->fd, &tmp, sizeof(uint32_t));
}

/******************************************************************************
 *
 *****************************************************************************/
void uio_disable_irq(UIO_HANDLE_ uio_handle)
{
        struct uio_handle *hdl = (struct uio_handle *)uio_handle;
        uint32_t tmp = 0x0;

        if (!hdl || hdl == UIO_OPEN_FAILED)
                return;

        no_warn_result_ write(hdl->fd, &tmp, sizeof(uint32_t));
}

/******************************************************************************
 *
 *****************************************************************************/
static void __wait_irq(const struct uio_handle *hdl)
{
        int nfsd = hdl->fd + 1;

        for (;;) {
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

/******************************************************************************
 *
 *****************************************************************************/
static void __thread_loop(const struct uio_handle *hdl)
{
         for (;;) {
                 __wait_irq(hdl);
                 if (hdl->context->irq_cb)
                         hdl->context->irq_cb(hdl->context->irq_data);
         }
}

/******************************************************************************
 *
 *****************************************************************************/
static void *uio_irq_thread(void *data)
{
        struct uio_handle *hdl = (struct uio_handle *)data;

        if (hdl->context->init_cb) {
                if (hdl->context->init_cb(hdl->context->bind_data)) {
                        (void)sem_post(&hdl->context->mutex);
                        return NULL;
                }
        }
        hdl->context->thread_started = 1;
        (void)sem_post(&hdl->context->mutex);
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
                /*lint -e{725} Expected positive indentation from line N */
                __thread_loop(hdl);
                pthread_cleanup_pop(0);
        }

        __thread_loop(hdl);
        return NULL;
}

/******************************************************************************
 * This function must be called with UIO top interrupt disabled.
 *****************************************************************************/
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

/******************************************************************************
 *
 *****************************************************************************/
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
        (void)sem_wait(&hdl->context->mutex);
        (void)sem_destroy(&hdl->context->mutex);
        if (!hdl->context->thread_started) {
                errno = EFAULT;
                return -1;
        }

        return 0;
}

/******************************************************************************
 *
 *****************************************************************************/
int uio_bind_irq(UIO_HANDLE_ uio_handle)
{
        return __uio_bind_irq(uio_handle, 0, 0, NULL, NULL, NULL);
}

/******************************************************************************
 *
 *****************************************************************************/
int uio_bind_irq2(UIO_HANDLE_ uio_handle, uio_irq_init_func init_cb,
                uio_irq_destroy_func destroy_cb, void *data)

{
        return __uio_bind_irq(uio_handle, 0, 0, init_cb, destroy_cb, data);
}

/******************************************************************************
 *
 *****************************************************************************/
int uio_bind_irq_rt(UIO_HANDLE_ uio_handle, int prio)
{
        return __uio_bind_irq(uio_handle, 1, prio, NULL, NULL, NULL);
}

/******************************************************************************
 *
 *****************************************************************************/
int uio_bind_irq2_rt(UIO_HANDLE_ uio_handle, int prio,
                uio_irq_init_func init_cb, uio_irq_destroy_func destroy_cb,
                void *data)
{
        return __uio_bind_irq(uio_handle, 1, prio, init_cb, destroy_cb, data);
}

/******************************************************************************
 *
 *****************************************************************************/
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

/******************************************************************************
 *
 *****************************************************************************/
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
