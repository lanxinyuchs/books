AUTOMAKE_OPTIONS = foreign
ACLOCAL_AMFLAGS = -I m4
SUBDIRS =

if DOCS
SUBDIRS += doc
endif

SUBDIRS += include
SUBDIRS += libitclnh

include_HEADERS = include/ulh_cm.h \
                  include/ulh_hdlc.h \
                  include/ulh_lnh_msg.h \
                  include/ulh_lnh.h \
                  include/ulh_transport.h \
                  include/ulh_ref.h \
                  include/ulh_timer.h \
                  include/ulh_dl_list.h

if EOLCIF
SUBDIRS += libeolcif
endif

if HDLCCM
SUBDIRS += libhdlccm
endif

if ECBIF
SUBDIRS += libecbif
SUBDIRS += test
endif

if ROLCIF
SUBDIRS += librolcif
endif

if ECMIF
SUBDIRS += libecmif
endif

if EMCAIF
SUBDIRS += libemcaif
endif

if MDUIF
SUBDIRS += libmduif
endif

if ECBCM
SUBDIRS += libecbcm
endif

if RIOCM
SUBDIRS += libriocm
endif

if MHP3
SUBDIRS += mhp3lnh
endif

if ECB
SUBDIRS += ecblnh
endif

if HDLC
SUBDIRS += hdlclnh
endif

if RIO
SUBDIRS += riolnh
endif

if ECM
SUBDIRS += ecmlnh
endif

# Yes, mhp3 ...
if MHP3
SUBDIRS += emcalnh
endif

if MDU
SUBDIRS += mdulnh
endif

if RIOSTUB
SUBDIRS += riostub
endif

if LHSH
SUBDIRS += lhsh
endif

if INFOCOMMAND
SUBDIRS += command
endif
