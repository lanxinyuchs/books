lib_LTLIBRARIES       =	libeolcif.la

include_HEADERS       = $(top_srcdir)/include/eolc-link-api.h

libeolcif_la_SOURCES  = eolcif.c

libeolcif_la_CPPFLAGS = -I$(top_srcdir)/include

libeolcif_la_CFLAGS   = -D__EXTENSIONS__ \
			-fno-strict-aliasing @AM_CFLAGS@

libeolcif_la_LDFLAGS  = --version-script=versions.ldscript \
			-version-info 0:0:0
