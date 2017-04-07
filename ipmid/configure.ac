AC_PREREQ(2.59)
LT_PREREQ([2.4.6])

AC_INIT([ipmi server daemon], [1.0.0], [xtwxy@hotmail.com], [ipmid], [https://github.com/xtwxy])

AC_PROG_CC
AC_ENABLE_SHARED

AC_CANONICAL_SYSTEM

AM_INIT_AUTOMAKE([-Wall -Werror foreign])
AM_PROG_AR
AM_PROG_CC_C_O 
#AC_PROG_LIBTOOL
LT_INIT


AC_CONFIG_MACRO_DIRS([m4])


AC_CONFIG_HEADERS([include/config.h])

AC_CONFIG_FILES([
    Makefile
    event_queue/Makefile
    ipmid/Makefile
    pkginfo/Makefile
])

AC_OUTPUT

