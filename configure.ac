AC_INIT([libecp],
	m4_esyscmd([build-aux/git-version-gen .tarball-version]),
	[no-reply@ericsson.com])
AC_CONFIG_AUX_DIR([build-aux])
AC_CONFIG_MACRO_DIR([m4])
AM_INIT_AUTOMAKE([foreign -Wall -Werror subdir-objects])

AM_PROG_AR
AC_PROG_CC
AC_PROG_CXX
LT_INIT

# The config.h is not needed yet!
#AC_CONFIG_HEADERS([config.h])

# Handle pthread in a portable way.
# Note that these tests must be performed before checking for availability of
# libraries since they may depend on pthread functions.
AX_PTHREAD
AC_SUBST(PTHREAD_LIBS)
AC_SUBST(PTHREAD_CFLAGS)
CC="$PTHREAD_CC"
CFLAGS="$CFLAGS $PTHREAD_CFLAGS"
CXXFLAGS="$CXXFLAGS $PTHREAD_CFLAGS"

# Check for availability of required libraries.
# The order is important.

# Checks for Lint.
AC_CHECK_PROG([HAVE_LINT], [lint], [yes], [no])
AM_CONDITIONAL([LINT], [test x$HAVE_LINT = xyes])

# Checks for Coverity.
AC_CHECK_PROG([HAVE_COV_CONFIGURE], [cov-configure], [yes], [no])
AC_CHECK_PROG([HAVE_COV_BUILD], [cov-build], [yes], [no])
AC_CHECK_PROG([HAVE_COV_ANALYZE], [cov-analyze], [yes], [no])
AC_CHECK_PROG([HAVE_COV_FORMAT_ERRORS], [cov-format-errors], [yes], [no])
AM_CONDITIONAL([COVERITY], [test x$HAVE_COV_CONFIGURE = xyes && test x$HAVE_COV_BUILD = xyes && test x$HAVE_COV_ANALYZE = xyes && test x$HAVE_COV_FORMAT_ERRORS = xyes])

# sysroot support
AC_MSG_CHECKING([for sysroot])
AC_ARG_WITH([libtool-sysroot],
  [AS_HELP_STRING([--libtool-sysroot=DIR],
    [Search for headers within DIR/usr/include (default: prefix)])],
  [],
  [AS_VAR_SET([with_libtool_sysroot],[yes])])
AS_CASE([$with_libtool_sysroot],
  [no],[],
  [yes],[AS_VAR_SET([with_libtool_sysroot],[$prefix])],
  [])
AC_MSG_RESULT([$with_libtool_sysroot])
AS_VAR_IF([with_libtool_sysroot],[no],[],[AS_VAR_SET([TARGET_SYSTEM_ROOT],[$with_libtool_sysroot])])
AC_SUBST([TARGET_SYSTEM_ROOT])
AM_CONDITIONAL([WITHLIBTOOLSYSROOT],[AS_VAR_TEST_SET([TARGET_SYSTEM_ROOT])])


# Lint configuration files
AC_MSG_CHECKING([for lint configuration])
AC_ARG_WITH([lint-configdir],
  [AS_HELP_STRING([--with-lint-configdir=DIR],
    [Search for Flexelint configuration files within DIR])],
  [],
  [AS_VAR_SET([with_lint_configdir],[no])])
AM_CONDITIONAL([HAS_LINT_CONFIGDIR], [test "x$with_lint_configdir" != "xno"])
AM_COND_IF([HAS_LINT_CONFIGDIR],[
		AC_MSG_RESULT([$with_lint_configdir])
		AS_VAR_SET([LINT_CONFIGDIR],[$with_lint_configdir])
		AC_SUBST([LINT_CONFIGDIR])
	], [
		AC_MSG_RESULT([not provided])
	])

COMMON_CFLAGS="$COMMON_CFLAGS -std=gnu99"
COMMON_CFLAGS="$COMMON_CFLAGS -ggdb3"
COMMON_CFLAGS="$COMMON_CFLAGS -Werror"
COMMON_CFLAGS="$COMMON_CFLAGS -Wall"
COMMON_CFLAGS="$COMMON_CFLAGS -Wextra"
COMMON_CFLAGS="$COMMON_CFLAGS -Wmissing-declarations"
COMMON_CFLAGS="$COMMON_CFLAGS -Winit-self"

COMMON_CXXFLAGS="$COMMON_CXXFLAGS -std=gnu++98"
COMMON_CXXFLAGS="$COMMON_CXXFLAGS -ggdb3"
COMMON_CXXFLAGS="$COMMON_CXXFLAGS -Werror"
COMMON_CXXFLAGS="$COMMON_CXXFLAGS -Wall"
COMMON_CXXFLAGS="$COMMON_CXXFLAGS -Wextra"
COMMON_CXXFLAGS="$COMMON_CXXFLAGS -pedantic"

AC_MSG_CHECKING([whether to compile for XENON])
AC_ARG_ENABLE([ecp-xenon],
    [AS_HELP_STRING([--enable-ecp-xenon],
    [enable to compile module for XENON [default=no]])],
    [ecp_xenon="$enableval"],
    [ecp_xenon=no])

# The use of '_' in the enable argument is wrong, it should be '-'.
# The below should removed but must be done by XENON users when they
# are prepared to migrate their build environment.
# :begin: deprecated
AC_ARG_ENABLE([ecp_xenon],
    [AS_HELP_STRING([--enable-ecp_xenon],
    [enable to compile module for XENON [default=no]])],
    [ecp_xenon="$enableval"],
    [ecp_xenon=$ecp_xenon])
# :end: deprecated

AC_MSG_RESULT([$ecp_xenon])
AM_CONDITIONAL([ECP_XENON], [test x$ecp_xenon = xyes])
if test "$ecp_xenon" = yes; then
  ecp_xmu03=no
else
  ecp_xmu03=yes
fi

# feature name nDmC-*-compact follows below rules:
#  n: number of ECP devices (i.e. ports)
#  D: ECP _D_evices
#  m: number of ECP channels (i.e. buffers)
#  C: ECP _C_hannels
#  CPRI/ECP-compact: no padding between ECP CPRI interrupt blocks
#                    or ECP channel blocks
#
AC_MSG_CHECKING([whether to configure 2D2C-CPRI-compact])
AC_ARG_ENABLE([ecp-xenon-2D2C-CPRI-compact],
    [AS_HELP_STRING([--enable-ecp-xenon-2D2C-CPRI-compact],
    [enable to configure 2D2C-CPRI-compact[default=no]])],
    [ecp_xenon_2D2C_CPRI_compact="$enableval"],
    [ecp_xenon_2D2C_CPRI_compact=no])
AC_MSG_RESULT([$ecp_xenon_2D2C_CPRI_compact])

AC_MSG_CHECKING([whether to configure 7D2C-CPRI-compact])
AC_ARG_ENABLE([ecp-xenon-7D2C-CPRI-compact],
    [AS_HELP_STRING([--enable-ecp-xenon-7D2C-CPRI-compact],
    [enable to configure 7D2C-CPRI-compact[default=no]])],
    [ecp_xenon_7D2C_CPRI_compact="$enableval"],
    [ecp_xenon_7D2C_CPRI_compact=no])
AC_MSG_RESULT([$ecp_xenon_7D2C_CPRI_compact])

if test "$ecp_xenon_2D2C_CPRI_compact" = yes && test "$ecp_xenon" = yes; then
  AC_DEFINE(ECP_DEVS, 2, [Define the number of ECP devices])
  AC_DEFINE(ECP_DEV_OFFSET, 0x50, [Define the offset of adjacent ECP devies])
  AC_DEFINE(ECP_CPRI_OFFSET, 0x18, [Define the offset of adjacent ECP CPRI interrupt blocks])
  AC_DEFINE(ECP_CIDS, 2, [Define the number of ECP channels])
fi

if test "$ecp_xenon_7D2C_CPRI_compact" = yes && test "$ecp_xenon" = yes; then
  AC_DEFINE(ECP_DEVS, 7, [Define the number of ECP devices])
  AC_DEFINE(ECP_DEV_OFFSET, 0x50, [Define the offset of adjacent ECP devies])
  AC_DEFINE(ECP_CPRI_OFFSET, 0x18, [Define the offset of adjacent ECP CPRI interrupt blocks])
  AC_DEFINE(ECP_CIDS, 2, [Define the number of ECP channels])
fi

if test "$ecp_xenon_2D2C_CPRI_compact" = no && test "$ecp_xenon_7D2C_CPRI_compact" = no && test "$ecp_xenon" = yes; then
  AC_DEFINE(ECP_DEVS, 2, [Define the number of ECP devices])
  AC_DEFINE(ECP_DEV_OFFSET, 0x50, [Define the offset of adjacent ECP devies])
  AC_DEFINE(ECP_CPRI_OFFSET, 0x28, [Define the offset of adjacent ECP CPRI interrupt blocks])
  AC_DEFINE(ECP_CIDS, 5, [Define the number of ECP channels])
fi

AC_MSG_CHECKING([whether to compile for XMU03])
AC_ARG_ENABLE([ecp-xmu03],
    [AS_HELP_STRING([--enable-ecp-xmu03],
    [enable to compile module for XMU03 [default=yes]])],
    [ecp_xmu03="$enableval"],
    [ecp_xmu03=$ecp_xmu03])
AC_MSG_RESULT([$ecp_xmu03])
AM_CONDITIONAL([ECP_XMU03], [test x$ecp_xmu03 = xyes])

if test "$ecp_xmu03$ecp_xenon" = yesyes; then
  AC_MSG_ERROR([cannot enable ecp-xenon and ecp-xmu03 simultaneously])
fi

if test "$ecp_xmu03$ecp_xenon" = nono; then
  AC_MSG_ERROR([must enable one of ecp-xenon or ecp-xmu03])
fi

AC_CACHE_CHECK([whether ulh_hold_ref() returns a value],
pac_cv_have_ulh_hold_ref_ret,[
    AC_TRY_COMPILE(
        [#include <ulh_ref.h>],
        [int v = ulh_hold_ref(NULL);],
        pac_cv_have_ulh_hold_ref_ret=yes,
        pac_cv_have_ulh_hold_ref_ret=no)])
if test "$pac_cv_have_ulh_hold_ref_ret" = yes ; then
    AC_DEFINE(HAVE_ULH_HOLD_REF_RET,1,[Define if ulh_hold_ref() returns a value])
fi

AC_MSG_CHECKING([whether to use lttng for logging])
AC_ARG_ENABLE([lttng],
	[AS_HELP_STRING([--enable-lttng],
	[enable lttng support [default=no]])],
	[lttng="$enableval"],
	[lttng=no])
AC_MSG_RESULT([$lttng])
AM_CONDITIONAL([LTTNG], [test x$lttng = xyes])
AM_COND_IF([LTTNG],
	AC_CHECK_LIB([lttng-ust],
		[lttng_probe_register],
		[LTTNG_LIBS=" -llttng-ust -ldl"],
		[echo "We need the lttng-ust library"; exit -1])
	AC_SUBST([LTTNG_LIBS])
)

AC_SUBST([COMMON_CPPFLAGS])
AC_SUBST([COMMON_CFLAGS])
AC_SUBST([COMMON_CXXFLAGS])

AC_CONFIG_FILES([Makefile
		 src/Makefile])

AC_OUTPUT
