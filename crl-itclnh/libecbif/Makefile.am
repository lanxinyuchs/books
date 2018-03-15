lib_LTLIBRARIES =	libecbif.la

include_HEADERS       = $(top_srcdir)/include/ecb-link-api.h

libecbif_la_SOURCES  = ecbif.c


libecbif_la_CPPFLAGS =   -I$(top_srcdir)/include

libecbif_la_CFLAGS =     -D__EXTENSIONS__ \
			-fno-strict-aliasing @AM_CFLAGS@

libecbif_la_LDFLAGS =    --version-script=versions.ldscript \
				-version-info 0:0:0
