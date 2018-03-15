if CMLIB
lib_LTLIBRARIES =	libecbcm.la
include_HEADERS       = $(top_srcdir)/include/ulh_ecb.h
else
noinst_LTLIBRARIES =	libecbcm.la
endif

libecbcm_la_SOURCES  = ulh_ecb.c


libecbcm_la_CPPFLAGS =   -I$(top_srcdir)/include

libecbcm_la_CFLAGS =     -D__EXTENSIONS__ \
			-fno-strict-aliasing @AM_CFLAGS@

libecbcm_la_LDFLAGS =    --version-script=versions.ldscript \
				-version-info 1:0:0
