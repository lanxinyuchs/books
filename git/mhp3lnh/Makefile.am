AM_CPPFLAGS = -I$(top_srcdir)/include

bin_PROGRAMS = mhp3lnh_old

mhp3lnh_old_SOURCES  = mhp3lnh.c ulh_mhp.c ulh_ebcom.c ulh_lttng.c
mhp3lnh_old_SOURCES += ../com_ericsson_system_start_tp.c

mhp3lnh_old_LDADD = $(top_builddir)/libitclnh/libitclnh.la
mhp3lnh_old_LDFLAGS = -litc -llttng-ust -lpthread
