#
# src/java/Makefile
# =============================================================================
# DNS Resolver Daemon (dnsresolvd). Version 0.9.9
# =============================================================================
# A daemon that performs DNS lookups for the given hostname
# passed in an HTTP request, with the focus on its implementation
# using various programming languages. (Vertosphere-boosted impl.)
# =============================================================================
# Copyright (C) 2017-2025 Radislav (Radicchio) Golubtsov
#
# (See the LICENSE file at the top of the source tree.)
#

DEPS = DnsResolvd.class                     \
       dns_resolv/DnsLookupController.class \
       dns_resolv/ControllerHelper.class

# Specify flags and other vars here.
DEP_PREF = $(HOME)/.m2/repository/

# If we're on OpenBSD, build the proper path to javac exec. and extra libs.
ifeq ($(shell uname), OpenBSD)
    JAVA_HOME = /usr/local/jdk-1.8.0
    JAVA_PATH = $(JAVA_HOME)/bin/

    DEP_URL0 = $(DEP_PREF)org/graylog2/syslog4j/0.9.61-SNAPSHOT/syslog4j-0.9.61-SNAPSHOT.jar
else
    DEP_URL0 = $(DEP_PREF)org/graylog2/syslog4j/0.9.60/syslog4j-0.9.60.jar
endif

DEP_URL1 = $(DEP_PREF)io/vertx/vertx-core/3.5.0/vertx-core-3.5.0.jar

JC      = $(JAVA_PATH)javac
JCFLAGS = -cp .:$(DEP_URL0):$(DEP_URL1)

# Making the target.
$(DEPS): %.class: %.java
	$(JC) $(JCFLAGS) $^

.PHONY: all clean

all: $(DEPS)

clean:
	$(RM) $(DEPS)

# vim:set nu ts=4 sw=4:
