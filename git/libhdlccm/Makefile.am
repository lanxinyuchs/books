if CMLIB
lib_LTLIBRARIES =	libhdlccm.la
include_HEADERS       = $(top_srcdir)/include/ulh_hdlc.h
else
noinst_LTLIBRARIES =	libhdlccm.la
endif

libhdlccm_la_SOURCES  = ulh_hdlc.c ulh_hdlc_dbg.c
libhdlccm_la_SOURCES += hdlc_lttng.c


libhdlccm_la_CPPFLAGS =   -I$(top_srcdir)/include

libhdlccm_la_CFLAGS =     -D__EXTENSIONS__ \
			-fno-strict-aliasing @AM_CFLAGS@

libhdlccm_la_LDFLAGS =    --version-script=versions.ldscript \
				-version-info 1:1:0
