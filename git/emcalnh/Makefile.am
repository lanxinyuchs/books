AM_CPPFLAGS = -I$(top_srcdir)/include

bin_PROGRAMS = mhp3lnh

mhp3lnh_SOURCES  = cm.c lh.c lh_lttng.c
mhp3lnh_SOURCES += ../com_ericsson_system_start_tp.c

mhp3lnh_LDADD = $(top_builddir)/libitclnh/libitclnh.la
mhp3lnh_LDFLAGS  = -litc -lpthread -lrhai_mhp -lrt
