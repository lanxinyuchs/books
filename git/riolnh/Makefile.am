# what flags you want to pass to the C compiler & linker
AM_CPPFLAGS = -I$(top_srcdir)/include

# this lists the binaries to produce, the (non-PHONY, binary) targets in
# the previous manual Makefile
bin_PROGRAMS = riolnh

riolnh_SOURCES  = riolnh.c

riolnh_LDADD = $(top_builddir)/libitclnh/libitclnh.la
riolnh_LDFLAGS = -litc -lpthread
