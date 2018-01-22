lib_LTLIBRARIES =	librolcif.la

include_HEADERS       = $(top_srcdir)/include/rolc-link-api.h

librolcif_la_SOURCES  = rolcif.c


librolcif_la_CPPFLAGS =   -I$(top_srcdir)/include

librolcif_la_CFLAGS =     -D__EXTENSIONS__ \
			-fno-strict-aliasing @AM_CFLAGS@

librolcif_la_LDFLAGS =    --version-script=versions.ldscript \
				-version-info 0:0:0
