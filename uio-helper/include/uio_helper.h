#ifndef UIO_HELPER_H_
#define UIO_HELPER_H_

#include <sys/mman.h>

#ifdef __cplusplus
extern "C" {
#endif

#define UIO_HANDLE_ void *
#define UIO_OPEN_FAILED ((UIO_HANDLE_)-1)

typedef void (*uio_irq_notifier_func)(void *);
typedef int (*uio_irq_init_func)(void *);
typedef void (*uio_irq_destroy_func)(void *);

UIO_HANDLE_ uio_open(const char *name);
int uio_getfd(UIO_HANDLE_);
void uio_close(UIO_HANDLE_);
void *uio_mmap(UIO_HANDLE_);
void *uio_mmap_by_name(UIO_HANDLE_, const char *name);
int uio_munmap(UIO_HANDLE_);
int uio_munmap_by_name(UIO_HANDLE_, const char *name);
void uio_enable_irq(UIO_HANDLE_);
void uio_disable_irq(UIO_HANDLE_);
int uio_bind_irq(UIO_HANDLE_);
int uio_bind_irq_rt(UIO_HANDLE_, int prio);
int uio_bind_irq2(UIO_HANDLE_, uio_irq_init_func, uio_irq_destroy_func, void *);
int uio_bind_irq2_rt(UIO_HANDLE_, int prio, uio_irq_init_func, uio_irq_destroy_func, void *);
int uio_unbind_irq(UIO_HANDLE_);
int uio_irq_set_notifier(UIO_HANDLE_, uio_irq_notifier_func, void *);
int uio_wait_irq(UIO_HANDLE_);

#ifdef __cplusplus
}
#endif
#endif /* UIO_HELPER_H_ */
