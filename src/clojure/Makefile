#
# src/clojure/Makefile
# =============================================================================
# DNS Resolver Daemon (dnsresolvd). Version 0.1
# =============================================================================
# A daemon that performs DNS lookups for the given hostname
# passed in an HTTP request, with the focus on its implementation
# using various programming languages. (HTTP Kit-boosted impl.)
# =============================================================================
# Copyright (C) 2017-2019 Radislav (Radicchio) Golubtsov
#
# (See the LICENSE file at the top of the source tree.)
#

____ = lib
BEAM = $(____)/ebin
SRCS = $(____)/*.clj

# Specify flags and other vars here.
MKDIR   = mkdir
CL      = clojure
CLFLAGS = -o
RMFLAGS = -vR

# Making the target.
$(BEAM): $(SRCS)
	if [ ! -d "$(BEAM)" ]; then               \
		$(MKDIR) $(BEAM);                 \
		$(CL) $(CLFLAGS) $(BEAM) $(SRCS); \
	fi

.PHONY: all clean

all: $(BEAM)

clean:
	$(RM) $(RMFLAGS) $(BEAM)

# vim:set nu ts=4 sw=4:
