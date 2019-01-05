#
# src/elixir/Makefile
# =============================================================================
# DNS Resolver Daemon (dnsresolvd). Version 0.1
# =============================================================================
# A daemon that performs DNS lookups for the given hostname
# passed in an HTTP request, with the focus on its implementation
# using various programming languages. (Cowboy-boosted impl.)
# =============================================================================
# Copyright (C) 2017-2019 Radislav (Radicchio) Golubtsov
#
# (See the LICENSE file at the top of the source tree.)
#

____ = lib
BEAM = $(____)/*.beam

# Specify flags and other vars here.
EC      = elixirc
ECFLAGS = -o

# Making the target.
$(BEAM): $(____)/*.ex
	$(EC) $(ECFLAGS) $(____) $(____)

.PHONY: all clean

all: $(BEAM)

clean:
	$(RM) $(BEAM)

# vim:set nu ts=4 sw=4:
