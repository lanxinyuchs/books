AM_CPPFLAGS = -I$(top_srcdir)/include

bin_PROGRAMS = ecblnh

ecblnh_SOURCES  = a4ci_server.c atfi_server.c ecb_lttng.c hdlc_dbg.c
ecblnh_SOURCES += ../com_ericsson_system_start_tp.c

ecblnh_LDADD = $(top_builddir)/libitclnh/libitclnh.la
ecblnh_LDADD += $(top_builddir)/libecbcm/libecbcm.la
ecblnh_LDFLAGS = -lpthread -lrt -litc -lrhai_ecb -lrhai_sys -llttng-ust -ldl
