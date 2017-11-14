#
# RBS time Makefile
#

obj-$(CONFIG_CPM_TIME) += cpm_time.o
# adk_netd_phy_tstamp.h now pulls in ncp_release_id.h
ccflags-y += -Iinclude/uapi/linux/npu

# This is needed for include/linux/npu/ncp_sal_types_linux.h (also
# pulled i through adk_netd_phy_tstamp.h). We used to set this through
# a wrapping ncp_sal_types_linux.h in include/linux/npu
ccflags-y += -DNCP_KERNEL -DNCP_LINUX
