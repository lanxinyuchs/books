lib_LTLIBRARIES =	libemcaif.la

include_HEADERS       = $(top_srcdir)/include/emca-link-api.h

libemcaif_la_SOURCES  = emcaif.c


libemcaif_la_CPPFLAGS =   -I$(top_srcdir)/include

libemcaif_la_CFLAGS =     -D__EXTENSIONS__ \
			-fno-strict-aliasing @AM_CFLAGS@

libemcaif_la_LDFLAGS =    --version-script=versions.ldscript \
				-version-info 1:0:0
