#######################################
# The list of executables we are building seperated by spaces
# the 'bin_' indicates that these build products will be installed
# in the $(bindir) directory. For example /usr/bin
#bin_PROGRAMS=example_udp_echo

# Because a.out is only a sample program we don't want it to be installed.
# The 'noinst_' prefix indicates that the following targets are not to be
# installed.
noinst_PROGRAMS=example-udp-echo

#######################################
# Build information for each executable. The variable name is derived
# by use the name of the executable with each non alpha-numeric character is
# replaced by '_'. So a.out becomes a_out and the appropriate suffex added.
# '_SOURCES' for example.

ACLOCAL_AMFLAGS=-I ../m4

# Sources for the a.out 
example_udp_echo_SOURCES= example-udp-echo.c

# Libraries for a.out
example_udp_echo_LDADD = $(top_builddir)/libepoll/libepoll.la

# Linker options for a.out
example_udp_echo_LDFLAGS = -rpath `cd $(top_builddir);pwd`/libepoll/.libs

# Compiler options for a.out
example_udp_echo_CPPFLAGS = -I$(top_srcdir)/include -I$(top_builddir)/include
