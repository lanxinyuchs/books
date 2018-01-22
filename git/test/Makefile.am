AM_CPPFLAGS = -I$(top_srcdir)/include

bin_PROGRAMS = ecbt prim sec hdlct

ecbt_SOURCES  = ecb/a4ci.c ecb/g1_a4ci.c ecb/g1_a4ci_mcab.c
ecbt_SOURCES += ecb/spray.c ecb/test.c
ecbt_SOURCES += ../com_ericsson_system_start_tp.c
ecbt_CFLAGS = -Iecb
ecbt_LDADD = $(top_builddir)/libecbif/libecbif.la
ecbt_LDFLAGS = -litc -la4ci -lpthread -lrt -llttng-ust -ldl

prim_SOURCES = ecblnh/primary/client.c
prim_LDADD = $(top_builddir)/libecbif/libecbif.la
prim_LDFLAGS = -litc -lpthread -lrt -llttng-ust -ldl

sec_SOURCES = ecblnh/secondary/server.c
sec_LDADD = $(top_builddir)/libecbif/libecbif.la
sec_LDFLAGS = -litc -lpthread -lrt -llttng-ust -ldl

hdlct_SOURCES = hdlc/test.c hdlc/rolcif.c hdlc/test_m.c hdlc/rolclnh.c hdlc/test_s.c
hdlct_SOURCES += ../com_ericsson_system_start_tp.c
hdlct_LDADD = $(top_builddir)/libhdlccm/libhdlccm.la
hdlct_LDADD += $(top_builddir)/libitclnh/libitclnh.la
hdlct_LDFLAGS = -litc -lpthread -lrt -llttng-ust -ldl
