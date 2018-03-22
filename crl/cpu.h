#ifndef CPU_H_
#define CPU_H_

#define LOCK_SAVE(a)
#define LOCK_RESTORE(a)

/*
 * Count leading zeros.
 */
static inline int
cntlzw(int x)
{
        int a;

        __asm volatile ("cntlzw %0,%1" : "=r"(a) : "r"(x));

        return a;
}

#endif
