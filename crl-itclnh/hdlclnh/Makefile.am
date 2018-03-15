AM_CPPFLAGS = -I$(top_srcdir)/include

bin_PROGRAMS = hdlclnh

hdlclnh_SOURCES  = hdlclnh.c
hdlclnh_SOURCES += hdlclnh_lttng.c hdlclnh_dbg.c
hdlclnh_SOURCES += ../com_ericsson_system_start_tp.c

hdlclnh_LDADD = $(top_builddir)/libitclnh/libitclnh.la
hdlclnh_LDADD += $(top_builddir)/libhdlccm/libhdlccm.la
hdlclnh_LDFLAGS = -lpthread -lrt -litc -lrhai_ecp -llttng-ust -ldl