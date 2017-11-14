#
# RBS core Makefile
#

obj-y += rbs-fn.o
obj-y += rbs-error.o
obj-y += rbs-smem.o
obj-y += rbs-rlimit.o
obj-y += af_rbs.o
obj-y += sys/
obj-y += gpio/
obj-y += misc/
obj-y += time/
obj-y += sec/

ifeq ($(CONFIG_RBS_DEBUG),y)
EXTRA_CFLAGS += -DDEBUG
endif

obj-$(CONFIG_RBS_FPGA)          += fpga/
obj-$(CONFIG_RBS_HWL)           += hwl/
obj-$(CONFIG_RBS_MTDPART)       += mtdpart/



