#
# src/c/libmicrohttpd/Makefile
# =============================================================================
# DNS Resolver Daemon (dnsresolvd). Version 0.1
# =============================================================================
# A daemon that performs DNS lookups for the given hostname
# passed in an HTTP request, with the focus on its implementation
# using various programming languages. (GNU libmicrohttpd-boosted impl.)
# =============================================================================
# Copyright (C) 2017-2020 Radislav (Radicchio) Golubtsov
#
# (See the LICENSE file at the top of the source tree.)
#

EXEC = dnsresolvd
DEPS = $(EXEC).o

# Specify flags and other vars here.
# The following standards-compliance options work well with gcc 4.9.4
# (OpenBSD 6.5, from packages):
#C_STD = c90 # Same as c89, same as -ansi .
C_STD = c99

CFLAGS = -Wall -pedantic -std=$(C_STD) -O3 \
         -march=x86-64 -mtune=generic -pipe -fstack-protector-strong

# Telling the compiler to use additional set of features including those
# from BSD and SVID, e.g. this macro explicitly declares the gethostbyname2()
# function -- very important when building for Linux.
CFLAGS += -D_DEFAULT_SOURCE

LDLIBS = -lmicrohttpd

# If we're on OpenBSD, invoke its gcc from packages
# instead of implicitly using its system default gcc or clang.
ifeq ($(shell uname), OpenBSD)
    CC = egcc
    CFLAGS += -I/usr/local/include
endif

# Making the target.
$(DEPS): %.o: %.c
$(EXEC): $(DEPS)

.PHONY: all clean

all: $(EXEC)

clean:
	$(RM) $(EXEC) $(DEPS)

# vim:set nu ts=4 sw=4:
