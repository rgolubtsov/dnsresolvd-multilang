#
# content/dev/misc/dnsresolvd/c/Makefile
# =============================================================================
# DNS Resolver Daemon (dnsresolvd). Version 0.1
# =============================================================================
# A GNU libmicrohttpd-boosted daemon for performing DNS lookups.
# =============================================================================
# Copyright (C) 2017 Radislav (Radicchio) Golubtsov
#
# (See the LICENSE file at the top of the source tree.)
#

EXEC = dnsresolvd
DEPS = $(EXEC).o

# Specify flags and other vars here.
# The following standards-compliance options work well with gcc 4.9.4
# (OpenBSD 6.1, in packages):
#C_STD = c90 # Same as c89, same as -ansi .
#C_STD = c99
C_STD = c11

CFLAGS = -Wall -pedantic -std=$(C_STD) -O3 \
         -march=x86-64 -mtune=generic -pipe -fstack-protector-strong

LDLIBS = -lmicrohttpd

# Making the target.
$(DEPS): %.o: %.c
$(EXEC): $(DEPS)

.PHONY: all clean

all: $(EXEC)

clean:
	$(RM) $(EXEC) $(DEPS)

# vim:set nu:et:ts=4:sw=4:
