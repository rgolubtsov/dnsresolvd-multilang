#
# src/vala/Makefile
# =============================================================================
# DNS Resolver Daemon (dnsresolvd). Version 0.1
# =============================================================================
# A daemon that performs DNS lookups for the given hostname
# passed in an HTTP request, with the focus on its implementation
# using various programming languages. (libsoup-boosted impl.)
# =============================================================================
# Copyright (C) 2017-2018 Radislav (Radicchio) Golubtsov
#
# (See the LICENSE file at the top of the source tree.)
#

EXEC = dnsresolvd
SRCS = $(EXEC).vala

# Specify flags and other vars here.
VC      = valac
VCFLAGS = --target-glib=2.40 -X -s -X -O3 --pkg=posix -o

# Making the target.
$(EXEC): $(SRCS)
	$(VC) $(VCFLAGS) $(EXEC) $(SRCS)

.PHONY: all clean

all: $(EXEC)

clean:
	$(RM) $(EXEC)

# vim:set nu ts=4 sw=4:
