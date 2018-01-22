#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.
AC_CONFIG_MACRO_DIR([m4])
AC_PREREQ([2.63])
AC_INIT([itclnh],
	m4_esyscmd([build-aux/git-version-gen .tarball-version]),
        [no-reply@ericsson.com])
AM_INIT_AUTOMAKE
LT_INIT

# Some additional CFLAGS to default setting "-g -O2"
AM_CFLAGS="$AM_CFLAGS -std=gnu99 -Wall"

# Checks for programs.
AC_PROG_CC
AC_PROG_LIBTOOL
AC_PROG_INSTALL

# Checks for libraries.
AC_CHECK_LIB([pthread], [pthread_create])
AC_CHECK_LIB([itc], [main])

# Checks for header files.
AC_CHECK_HEADERS([stddef.h stdint.h stdlib.h string.h unistd.h])

AC_MSG_CHECKING([whether to build an MHP3 linkhandler])
AC_ARG_WITH([cmlib],
        [AS_HELP_STRING([--with-cmlib],
        [build cm/lnh as libs [default=no]])],
        [cmlib=yes],
        [cmlib=no])
AC_MSG_RESULT([$cmlib])
AM_CONDITIONAL([CMLIB], [test "$cmlib" = "yes"])

AC_MSG_CHECKING([whether to build an MHP3 linkhandler])
AC_ARG_WITH([mhp3],
        [AS_HELP_STRING([--with-mhp3],
        [enable mhp3 cm [default=no]])],
        [mhp3=yes],
        [mhp3=no])
AC_MSG_RESULT([$mhp3])
AM_CONDITIONAL([MHP3], [test "$mhp3" = "yes"])
AM_CONDITIONAL([EOLCIF], [test "$rio" = "yes" -o "$mhp3" = "yes"])

if test x"$mhp3" = x"yes"; then
        AC_CHECK_LIB([lttng-ust], [main])
fi


AC_MSG_CHECKING([whether to build an ECB linkhandler])
AC_ARG_WITH([ecb],
        [AS_HELP_STRING([--with-ecb],
        [enable ecb cm [default=no]])],
        [ecb=yes],
        [ecb=no])
AC_MSG_RESULT([$ecb])
AM_CONDITIONAL([ECB], [test "$ecb" = "yes"])
AM_CONDITIONAL([ECBCM], [test "$ecb" = "yes"])
AM_CONDITIONAL([ECBIF], [test "$ecb" = "yes"])

if test x"$ecb" = x"yes"; then
        AC_CHECK_LIB([lttng-ust], [main])
fi


AC_MSG_CHECKING([whether to build an HDLC linkhandler])
AC_ARG_WITH([hdlc],
        [AS_HELP_STRING([--with-hdlc],
        [enable hdlc cm [default=no]])],
        [hdlc=yes],
        [hdlc=no])
AC_MSG_RESULT([$hdlc])
AM_CONDITIONAL([HDLC],   [test "$hdlc" = "yes"])
AM_CONDITIONAL([HDLCCM], [test "$hdlccm" = "yes" -o "$hdlc" = "yes"])
AM_CONDITIONAL([ROLCIF], [test "$hdlc" = "yes"])

AC_MSG_CHECKING([whether to build HDLC CM])
AC_ARG_WITH([hdlccm],
        [AS_HELP_STRING([--with-hdlccm],
        [build hdlccm lib  [default=no]])],
        [hdlccm=yes],
        [hdlccm=no])
AC_MSG_RESULT([$hdlccm])
AM_CONDITIONAL([HDLCCM], [test "$hdlccm" = "yes" -o "$hdlc" = "yes"])

AC_MSG_CHECKING([whether to build an RIO linkhandler])
AC_ARG_WITH([rio],
        [AS_HELP_STRING([--with-rio],
        [enable rio cm [default=no]])],
        [rio=yes],
        [rio=no])
AC_MSG_RESULT([$rio])
AM_CONDITIONAL([RIO], [test "$rio" = "yes"])
AM_CONDITIONAL([RIOCM], [test "$rio" = "yes"])
AM_CONDITIONAL([EOLCIF], [test "$rio" = "yes" -o "$mhp3" = "yes"])

AC_MSG_CHECKING([whether to build an ECM linkhandler binary])
AC_ARG_WITH([ecm],
        [AS_HELP_STRING([--with-ecm],
        [enable ethernet cm [default=no]])],
        [ecm=yes],
        [ecm=no])
AC_MSG_RESULT([$ecm])
AM_CONDITIONAL([ECM], [test "$ecm" = "yes"])
AM_CONDITIONAL([ECMIF], [test "$ecm" = "yes"])

AC_MSG_CHECKING([whether to build an MDU linkhandler binary])
AC_ARG_WITH([mdu],
        [AS_HELP_STRING([--with-mdu],
        [enable mdu [default=no]])],
        [mdu=yes],
        [mdu=no])
AC_MSG_RESULT([$mdu])
AM_CONDITIONAL([MDU], [test "$mdu" = "yes"])
AM_CONDITIONAL([MDUIF], [test "$mdu" = "yes"])

AC_MSG_CHECKING([whether to build with EMCA if support])
AC_ARG_WITH([emcaif],
        [AS_HELP_STRING([--with-emcaif],
        [enable EMCA if [default=no]])],
        [emcaif=yes],
        [emcaif=no])
AC_MSG_RESULT([$emcaif])
AM_CONDITIONAL([EMCAIF], [test "$emcaif" = "yes"])

AC_MSG_CHECKING([whether to build with a RIO stub])
AC_ARG_WITH([riostub],
        [AS_HELP_STRING([--with-riostub],
        [enable rio stub [default=no]])],
        [riostub=yes],
        [riostub=no])
AC_MSG_RESULT([$riostub])
AM_CONDITIONAL([RIOSTUB], [test "$riostub" = "yes"])

AC_MSG_CHECKING([whether to generate lhsh])
AC_ARG_WITH([lhsh],
        [AS_HELP_STRING([--with-lhsh],
        [enable lhsh [default=no]])],
        [lhsh=yes],
        [lhsh=no])
AC_MSG_RESULT([$lhsh])
AM_CONDITIONAL([LHSH], [test "$lhsh" = "yes"])

AC_MSG_CHECKING([whether to build with lttng traces])
AC_ARG_ENABLE([lttng],
        [AS_HELP_STRING([--enable-lttng],
        [enable lttng traces [default=no]])],
        [lttng="$enableval"],
        [lttng=no])
AC_MSG_RESULT([$lttng])
if test x"$lttng" = x"yes"; then
        AM_CFLAGS="$AM_CFLAGS -DLTTNG"
        AC_CHECK_LIB([lttng-ust], [ltt_probe_register])
fi

AC_MSG_CHECKING([whether to build with itclnh command])
AC_ARG_ENABLE([infocmd],
        [AS_HELP_STRING([--disable-infocmd],
        [generate itclnh info command [default=yes]])],
        [infocmd=no],
        [infocmd=yes])
AC_MSG_RESULT([$infocmd])
AM_CONDITIONAL([INFOCOMMAND], [test "$infocmd" = "yes"])

AC_MSG_CHECKING([whether to build the documentation])
AC_ARG_WITH([docs],
        [AS_HELP_STRING([--with-docs],
        [build documentation [default=yes]])],
        [docs=no],
        [docs=yes])
AC_MSG_RESULT([$docs])
AM_CONDITIONAL([DOCS], [test "$docs" = "yes"])

AC_SUBST([AM_CFLAGS])

AC_CONFIG_FILES([Makefile
		 doc/Makefile
		 include/Makefile
		 libitclnh/Makefile
		 libeolcif/Makefile
		 libecbif/Makefile
		 libemcaif/Makefile
		 librolcif/Makefile
		 libecmif/Makefile
		 libhdlccm/Makefile
		 libecbcm/Makefile
		 libriocm/Makefile
		 ecblnh/Makefile
		 riolnh/Makefile
		 hdlclnh/Makefile
		 mhp3lnh/Makefile
		 ecmlnh/Makefile
		 riostub/Makefile
		 emcalnh/Makefile
		 libmduif/Makefile
		 mdulnh/Makefile
		 lhsh/Makefile
		 command/Makefile
		 test/Makefile])
AC_OUTPUT

