lib_LIBRARIES = libglms.a
libglms_a_SOURCES = \
		   com_ericsson_glms.h \
		   trace_probe.c \
		   glms_internal.sig \
		   clients.h \
		   clients.c \
		   data_common.h \
		   data_common.c \
		   data_featureState.h \
		   data_featureState.c \
		   data_featureKey.h \
		   data_featureKey.c \
		   data_capacityState.h \
		   data_capacityState.c \
		   data_capacityKey.h \
		   data_capacityKey.c \
		   data_keyFile.h \
		   data_keyFile.c \
		   data_lm.h \
		   data_lm.c \
		   data_unlocks.h \
		   data_unlocks.c \
		   data_licenseSupport.h \
		   data_licenseSupport.c \
		   glms_main.h \
		   glms_main.c \
		   lihi_feature.h \
		   lihi_feature.c \
		   lihi_capacity.h \
		   lihi_capacity.c \
		   persistentStorage.h \
		   persistentStorage.c \
		   glms_xmlParser.h \
		   glms_xmlParser.c \
		   osa_xmlParser.h \
		   osa_xmlParser.c \
		   glmsUtils.h \
		   glmsUtils.c \
		   glms_timi.h \
		   glms_timi.c \
		   kfParser.h


libglms_a_CPPFLAGS = -I$(top_srcdir)/include -I$(top_srcdir)/src
if GTEST_ENABLED
   libglms_a_CPPFLAGS += -DGLMS_TEST_RUN
endif


libglms_a_CFLAGS = -Wall -Werror -Wextra -pedantic -std=gnu99 -pthread
if GCOV_ENABLED
   libglms_a_CFLAGS += -fprofile-arcs -ftest-coverage
endif

glms_includedir = $(includedir)/glmsadpi
glms_include_HEADERS = $(top_srcdir)/include/glmsadpi/glms.h \
                       $(top_srcdir)/include/glmsadpi/glmsDataTypes.h \
                       $(top_srcdir)/include/glmsadpi/glms_adpi.sig

lihi_includedir = $(includedir)/lihi
lihi_include_HEADERS = $(top_srcdir)/include/lihi/licenseFeatureControlICommon.h \
		       $(top_srcdir)/include/lihi/licenseFeatureControlIUnion.h \
		       $(top_srcdir)/include/lihi/licenseFeatureControlIUnionContent.h \
                       $(top_srcdir)/include/lihi/licenseFeatureControlI.sig \
                       $(top_srcdir)/include/lihi/licenseCapacityControlICommon.h \
		       $(top_srcdir)/include/lihi/licenseCapacityControlIUnion.h \
		       $(top_srcdir)/include/lihi/licenseCapacityControlIUnionContent.h \
                       $(top_srcdir)/include/lihi/licenseCapacityControlI.sig
