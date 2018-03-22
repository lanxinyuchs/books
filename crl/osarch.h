#ifndef _LITS_OSARCH_H
#define _LITS_OSARCH_H

#define _LITS_OSARCH_ 1
#define OSE_VERSION 500

#if !(defined(OSARM)      \
      || defined(OSMIPS)  \
      || defined(OSPP)    \
      || defined(OSSPARC) \
      || defined(OSX86))

#if defined(__arm) || defined(__arm__)
# define OSARM
#elif defined(__mips) || defined(__mips__)
# define OSMIPS
#elif defined(__ppc) || defined(__ppc__)
# define OSPP
#elif defined(__sparc) || defined(__sparc__)
# define OSSPARC
#elif defined(__i386) || defined(__i386__)
# define OSX86
#else
# define softOSE
#endif

#endif /* defined(... */

#endif /* _LITS_OSARCH_H */
