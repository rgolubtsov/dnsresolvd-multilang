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

LIB = lib
SRC = src
NS  = dnsresolvd

# Specify flags and other vars here.
MKDIR   = mkdir
CL      = clojure
CLFLAGS = -e
RMFLAGS = -vR

# Making the target.
$(LIB): $(SRC)/*
	if [ ! -d "$(LIB)" ]; then                                   \
		$(MKDIR) $(LIB);                                     \
		$(CL) $(CLFLAGS) '(binding [*compile-path* "$(LIB)"] \
                                  (println  *compile-path*)          \
                                  (compile (symbol "$(NS)")))';      \
	fi

.PHONY: all clean

all: $(LIB)

clean:
	$(RM) $(RMFLAGS) $(LIB)

# vim:set nu ts=4 sw=4:
