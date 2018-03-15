#ifndef _RHD_MSG_BASE_H_
#define _RHD_MSG_BASE_H_

#ifndef RHD_MSG_BASE
/*  as we use -Werror the line below will result in an error.
  #  pragma GCC warning "WARNING: You should set RHD_MSG_BASE to avoid default values"*/
#  pragma message ( "WARNING: You should set RHD_MSG_BASE to avoid default values" )
#  define RHD_MSG_BASE 0x12340000
#endif

#define RHD_BOARD_MSG_BASE    (RHD_MSG_BASE + 0x000)
#define RHD_I2C_MSG_BASE      (RHD_MSG_BASE + 0x100)
#define RHD_SPI_MSG_BASE      (RHD_MSG_BASE + 0x200)
#define RHD_VII_MSG_BASE      (RHD_MSG_BASE + 0x300)
#define RHD_MMI_MSG_BASE      (RHD_MSG_BASE + 0x400)
#define RHD_PINMUX_MSG_BASE   (RHD_MSG_BASE + 0x500)
#define RHD_LH_MSG_BASE       (RHD_MSG_BASE + 0x600)
#endif
