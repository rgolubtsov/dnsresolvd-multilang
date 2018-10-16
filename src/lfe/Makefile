#
# src/lfe/Makefile
# =============================================================================
# DNS Resolver Daemon (dnsresolvd). Version 0.1
# =============================================================================
# A daemon that performs DNS lookups for the given hostname
# passed in an HTTP request, with the focus on its implementation
# using various programming languages. (Cowboy-boosted impl.)
# =============================================================================
# Copyright (C) 2017-2018 Radislav (Radicchio) Golubtsov
#
# (See the LICENSE file at the top of the source tree.)
#

____ = lib
BEAM = $(____)/ebin
SRCS = $(____)/*.lfe

# Specify flags and other vars here.
MKDIR   = mkdir
LC      = lfec
LCFLAGS = -o
RMFLAGS = -vR

# Making the target.
$(BEAM): $(SRCS)
	if [ ! -d "$(BEAM)" ]; then               \
		$(MKDIR) $(BEAM);                 \
		$(LC) $(LCFLAGS) $(BEAM) $(SRCS); \
	fi

.PHONY: all clean

all: $(BEAM)

clean:
	$(RM) $(RMFLAGS) $(BEAM)

# vim:set nu ts=4 sw=4:
