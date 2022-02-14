#
# src/c/libsoup/Makefile
# =============================================================================
# DNS Resolver Daemon (dnsresolvd). Version 0.1
# =============================================================================
# A daemon that performs DNS lookups for the given hostname
# passed in an HTTP request, with the focus on its implementation
# using various programming languages. (GNOME libsoup-boosted impl.)
# =============================================================================
# Copyright (C) 2017-2022 Radislav (Radicchio) Golubtsov
#
# (See the LICENSE file at the top of the source tree.)
#

EXEC = dnsresolvd
DEPS = $(EXEC).o

# Specify flags and other vars here.
C_STD = c99

CFLAGS = -Wall -pedantic -std=$(C_STD) -O3 \
         -march=x86-64 -mtune=generic -pipe -fstack-protector-strong

# Telling the compiler to use additional set of features including those
# from BSD and SVID, e.g. this macro explicitly declares the gethostbyname2()
# function -- very important when building for Linux.
CFLAGS += -D_DEFAULT_SOURCE

CFLAGS += `pkg-config --cflags-only-I libsoup-2.4 json-glib-1.0`
LDLIBS  = `pkg-config   --libs-only-l libsoup-2.4 json-glib-1.0`

# If we're on OpenBSD, invoke its gcc from packages
# instead of implicitly using its system default gcc or clang.
ifeq ($(shell uname), OpenBSD)
    CC = egcc
else
    CC = tcc
#   CC = cc
endif

# Making the target.
$(DEPS): %.o: %.c
$(EXEC): $(DEPS)

.PHONY: all clean

all: $(EXEC)

clean:
	$(RM) $(EXEC) $(DEPS)

# vim:set nu ts=4 sw=4:
