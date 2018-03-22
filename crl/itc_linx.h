#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <unistd.h>

#include "itc.h"

#ifndef __ITC_LINX_H
#define __ITC_LINX_H

#define ADD_LINX_LNH  (ITC_MSG_BASE + 0xF00)
struct add_linx_lnh {
        uint32_t      msgno;
        char          lnhpath[1];
};

extern void *linxlnh_main(void *data);

#endif
