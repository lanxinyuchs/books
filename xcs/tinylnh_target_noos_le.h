#define T_ASSERT(c) if (!(c)) { while (1) { asm("wfe"); } }

#if defined(__BYTE_ORDER__) && (__BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__)
#define T_HTONL(u32)               \
  ((((u32) << 24) & 0xff000000) |  \
   (((u32) <<  8) & 0x00ff0000) |  \
   (((u32) >>  8) & 0x0000ff00) |  \
   (((u32) >> 24) & 0x000000ff))
#define T_NTOHL(u32) T_HTONL(u32)
#define T_HTONS(u16)          \
  ((((u16) << 8) & 0xff00) |  \
   (((u16) >> 8) & 0x00ff))
#define T_NTOHS(u16) T_HTONS(u16)
#elif defined(__BYTE_ORDER__) && (__BYTE_ORDER__ == __ORDER_BIG_ENDIAN__)
#define T_HTONL(u32) (u32)
#define T_NTOHL(u32) (u32)
#define T_HTONS(u16) (u16)
#define T_NTOHS(u16) (u16)
#else
#error "Please define a valid __BYTE_ORDER__!"
#endif

#define T_INFO(...)
#define T_TRACE(...)
#define T_ERROR(...)

#define T_MUTEX_STRUCT int
#define T_MUTEX_INIT(mutex)
#define T_MUTEX_LOCK(mutex)
#define T_MUTEX_UNLOCK(mutex)
