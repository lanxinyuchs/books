lib_LTLIBRARIES = libmduif.la

include_HEADERS = $(top_srcdir)/include/mdu-link-api.h

libmduif_la_SOURCES = mduif.c

libmduif_la_CPPFLAGS = -I$(top_srcdir)/include

libmduif_la_CFLAGS = -D__EXTENSIONS__ -fno-strict-aliasing @AM_CFLAGS@

libmduif_la_LDFLAGS = --version-script=versions.ldscript \
			-version-info 0:0:0
