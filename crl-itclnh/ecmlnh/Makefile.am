# what flags you want to pass to the C compiler & linker
AM_CPPFLAGS = -I$(top_srcdir)/include

include_HEADERS       = ecmlnh.h

# this lists the binaries to produce, the (non-PHONY, binary) targets in
# the previous manual Makefile
lib_LTLIBRARIES =	libecmlnh.la

libecmlnh_la_SOURCES  = ecmlnh.c ulh_ecm.c ulh_ecmt.c ulh_lttng.c

if MULTITHREAD
libecmlnh_la_CFLAGS = -DPER_CONN_TXFD @AM_CFLAGS@
endif

libecmlnh_la_LDFLAGS = -L$(top_builddir)/libitclnh -litclnh -version-info 1:0:0

bin_PROGRAMS = ecmlnh


ecmlnh_SOURCES  = ecmmain.c ecmlnh.c ulh_ecm.c ulh_ecmt.c ulh_lttng.c

ecmlnh_LDADD = $(top_builddir)/../install/lib/libitclnh.a
ecmlnh_LDFLAGS = -litc -llttng-ust -lpthread
