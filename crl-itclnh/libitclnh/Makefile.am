if CMLIB
lib_LTLIBRARIES =	libitclnh.la
else
noinst_LTLIBRARIES =	libitclnh.la
endif

if MULTITHREAD
libitclnh_la_SOURCES  = ulh_cm.c ulh_lnh_multithread.c ulh_rlnh_dbg.c
else
libitclnh_la_SOURCES  = ulh_cm.c ulh_lnh.c ulh_rlnh_dbg.c
endif
libitclnh_la_SOURCES += itclnh_lttng.c ulh_timer.c ulh_transport.c


libitclnh_la_CPPFLAGS =   -I$(top_srcdir)/include

libitclnh_la_CFLAGS =     -D__EXTENSIONS__ \
			-fno-strict-aliasing @AM_CFLAGS@

libitclnh_la_LDFLAGS =    -version-info 2:0:0
