lib_LTLIBRARIES       =	libecmif.la

include_HEADERS       = $(top_srcdir)/include/ecm-link-api.h

libecmif_la_SOURCES  = ecmif.c

libecmif_la_CPPFLAGS = -I$(top_srcdir)/include

libecmif_la_CFLAGS   = -D__EXTENSIONS__ \
			-fno-strict-aliasing @AM_CFLAGS@

libecmif_la_LDFLAGS  = -version-info 1:0:0
