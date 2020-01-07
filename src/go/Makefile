#
# src/go/Makefile
# =============================================================================
# DNS Resolver Daemon (dnsresolvd). Version 0.1
# =============================================================================
# A daemon that performs DNS lookups for the given hostname
# passed in an HTTP request, with the focus on its implementation
# using various programming languages. (net/http-boosted impl.)
# =============================================================================
# Copyright (C) 2017-2020 Radislav (Radicchio) Golubtsov
#
# (See the LICENSE file at the top of the source tree.)
#

EXEC = dnsresolvd

# Making the target.
$(EXEC): $(EXEC).go
	go run $(EXEC).go

.PHONY: all clean

all: $(EXEC)

clean:
	$(RM) $(EXEC)

# vim:set nu ts=4 sw=4:
