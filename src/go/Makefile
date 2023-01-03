#
# src/go/Makefile
# =============================================================================
# DNS Resolver Daemon (dnsresolvd). Version 0.9.9
# =============================================================================
# A daemon that performs DNS lookups for the given hostname
# passed in an HTTP request, with the focus on its implementation
# using various programming languages. (net/http-boosted impl.)
# =============================================================================
# Copyright (C) 2017-2023 Radislav (Radicchio) Golubtsov
#
# (See the LICENSE file at the top of the source tree.)
#

PREF = dnsresolv
EXEC = $(PREF)d
DEPS = $(EXEC).o
SRCS = $(PREF)d.go \
       $(PREF)h.go

# Specify flags and other vars here.
GO_TOOL       = go tool
GO_COMP_FLAGS = compile -complete -o $(DEPS)
GO_LINK_FLAGS = link -s -w        -o $(EXEC)

# Making the target.
$(DEPS): $(SRCS)
	$(GO_TOOL) $(GO_COMP_FLAGS) $(SRCS)

$(EXEC): $(DEPS)
	$(GO_TOOL) $(GO_LINK_FLAGS) $(DEPS)

.PHONY: all clean

all: $(EXEC)

clean:
	$(RM) $(EXEC) $(DEPS)

# vim:set nu ts=4 sw=4:
