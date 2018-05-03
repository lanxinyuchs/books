lib_LTLIBRARIES =	libecp.la

# Common Flags

COMMON_SRC_CPPFLAGS =	$(COMMON_CPPFLAGS)

COMMON_SRC_CFLAGS =	$(COMMON_CFLAGS)

COMMON_SRC_CXXFLAGS =	$(COMMON_CXXFLAGS)

################################################################################
# libecp.la
################################################################################

libecp_la_SOURCES = libecp.c

libecp_la_CPPFLAGS =	$(COMMON_SRC_CPPFLAGS) \
			-I$(top_srcdir)/include \
			-I$(top_srcdir)/src/log

if ECP_XENON
libecp_la_SOURCES  += xenon/ecp.c
libecp_la_CPPFLAGS += -I$(top_srcdir)/include/xenon
endif

if ECP_XMU03
libecp_la_SOURCES  += xmu03/ecp.c
libecp_la_CPPFLAGS += -I$(top_srcdir)/include/xmu03
endif

if LTTNG
libecp_la_CPPFLAGS += -D_LTTNG_
libecp_la_SOURCES += log/log_lttng.c
libecp_la_LIBADD = $(LTTNG_LIBS)
else
libecp_la_CPPFLAGS += -pedantic
libecp_la_SOURCES += log/log_syslog.c
endif

libecp_la_CFLAGS =	$(COMMON_SRC_CFLAGS)

libecp_la_LDFLAGS =	-version-info 0:6:0

TESTS =
CLEANFILES =

include $(top_srcdir)/Makefile.lint
include $(top_srcdir)/Makefile.coverity
