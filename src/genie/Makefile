#
# src/genie/Makefile
# =============================================================================
# DNS Resolver Daemon (dnsresolvd). Version 0.9.9
# =============================================================================
# A daemon that performs DNS lookups for the given hostname
# passed in an HTTP request, with the focus on its implementation
# using various programming languages. (libsoup-boosted impl.)
# =============================================================================
# Copyright (C) 2017-2023 Radislav (Radicchio) Golubtsov
#
# (See the LICENSE file at the top of the source tree.)
#

PREF = dnsresolv
EXEC = $(PREF)d
SRCS = $(PREF)d.gs \
       $(PREF)h.vala
#                ^ ^
#                | |
#                | +----- Helper module, in Vala.
#                +------- Main   module, in Genie.

# Specify flags and other vars here.
# If we're on OpenBSD, invoke its gcc from packages
# instead of implicitly using its system default gcc or clang.
ifeq ($(shell uname), OpenBSD)
    CC = egcc
endif

VC      = valac
VCFLAGS = --target-glib=2.40 --cc=$(CC) -X -s                       \
                                        -X -O3                      \
                                        -X -mtune=generic           \
                                        -X -pipe                    \
                                        -X -fstack-protector-strong \
          --pkg=posix                                               \
          --pkg=libsoup-2.4                                         \
          --pkg=json-glib-1.0           -o

# Making the target.
$(EXEC): $(SRCS)
	$(VC) $(VCFLAGS) $@ $^

.PHONY: all clean

all: $(EXEC)

clean:
	$(RM) $(EXEC)

# vim:set nu ts=4 sw=4:
