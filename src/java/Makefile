#
# src/java/Makefile
# =============================================================================
# DNS Resolver Daemon (dnsresolvd). Version 0.1
# =============================================================================
# A daemon that performs DNS lookups for the given hostname
# passed in an HTTP request, with the focus on its implementation
# using various programming languages. (Vertosphere-boosted impl.)
# =============================================================================
# Copyright (C) 2017-2019 Radislav (Radicchio) Golubtsov
#
# (See the LICENSE file at the top of the source tree.)
#

DEPS = DnsResolvd.class                  \
       dns_resolv/ControllerHelper.class

# Specify flags and other vars here.
# If we're on OpenBSD, set the JAVA_HOME env. var. and the path to java exec.
ifeq ($(shell uname), OpenBSD)
    JAVA_HOME = /usr/local/jdk-1.8.0
    JAVA_PATH = $(JAVA_HOME)/bin/
endif

JC = $(JAVA_PATH)javac

# Making the target.
$(DEPS): %.class: %.java
	$(JC) $^

.PHONY: all clean

all: $(DEPS)

clean:
	$(RM) $(DEPS)

# vim:set nu ts=4 sw=4:
