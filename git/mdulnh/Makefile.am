AM_CPPFLAGS = -I$(top_srcdir)/include

bin_PROGRAMS = mdulnh

mdulnh_SOURCES  = cm.c lh.c mdu_lttng.c
mdulnh_SOURCES += ../com_ericsson_system_start_tp.c

mdulnh_LDADD = $(top_builddir)/libitclnh/libitclnh.la
mdulnh_LDFLAGS = -litc -lpthread
