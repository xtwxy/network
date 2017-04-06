AC_INIT([ipmid], [1.0.0], [xtwxy@hotmail.com], [ipmid], [https://github.com/xtwxy])
LT_PREREQ([2.4.6])
AC_ENABLE_SHARED

AM_INIT_AUTOMAKE([-Wall -Werror foreign])
AM_PROG_AR

LT_INIT

AC_PROG_CC
AC_PROG_LIBTOOL

AC_CONFIG_MACRO_DIRS([m4])
AC_CONFIG_HEADERS([include/config.h])
AC_CONFIG_FILES([
    Makefile
    event_queue/Makefile
    ipmid/Makefile
    pkginfo/Makefile
])
AC_OUTPUT

