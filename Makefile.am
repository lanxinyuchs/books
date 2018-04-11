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

bin_PROGRAMS = ecmlnh ecmtest 

ecmlnh_SOURCES  = ecmmain.c

ecmlnh_LDADD = libecmlnh.la
ecmlnh_LDADD += $(top_builddir)/libitclnh/libitclnh.la
ecmlnh_LDFLAGS = -litc -llttng-ust -lpthread

ecmtest_SOURCES  = ecm_test.c

ecmtest_LDADD = $(top_builddir)/libecmif/libecmif.la
ecmtest_LDFLAGS = -litc -llttng-ust -lpthread
#ecmtest_CFLAGS = $(AM_CFLAGS) -O0 -pipe -D__MACHINE_ZYNQMP_MIMO
#ftrace_SOURCES = ftrace.c
