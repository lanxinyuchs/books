noinst_LTLIBRARIES =	libriocm.la

libriocm_la_SOURCES  = ulh_rio.c


libriocm_la_CPPFLAGS =   -I$(top_srcdir)/include

libriocm_la_CFLAGS =     -D__EXTENSIONS__ \
			-fno-strict-aliasing @AM_CFLAGS@

libriocm_la_LDFLAGS =    --version-script=versions.ldscript \
				-version-info 1:0:0
