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
NSS = dnsresolvh \
      dnsresolvd

# Specify flags and other vars here.
MV      = mv
MVFLAGS = -v
MKDIR   = mkdir
ECHO    = echo
CL      = clojure
CLFLAGS = -e
RMFLAGS = -vR

# Making the target.
$(LIB): $(SRC)s/*
	$(MV) $(MVFLAGS) $(SRC)s $(SRC);                                                                      \
	if [ ! -d "$(LIB)" ]; then                                                                            \
		$(MKDIR) $(LIB);                                                                              \
		$(ECHO) "------------ Start  ahead-of-time compilation for modules in $(SRC)/ -------------"; \
		for ns in $(NSS); do                                                                          \
			$(CL) $(CLFLAGS) "(defmacro DEP-URL [] (str                                           \
                                         \"file:\" (System/getProperty \"user.home\") \"/.m2/repository/\"    \
                                         \"http-kit/http-kit/2.3.0/http-kit-2.3.0.jar\"))                     \
                (let [acl (ClassLoader/getSystemClassLoader)                   ]                              \
                (let [fld (aget (.getDeclaredFields java.net.URLClassLoader) 0)] (.setAccessible fld true)    \
                (let [ucp (.get    fld acl)                                    ]                              \
                          (.addURL ucp (java.net.URL. (DEP-URL)))                                             \
                )))                                                                                           \
                                          (binding [*compile-path* \"$(LIB)\"]                                \
                                          (compile (symbol \"$$ns\")))";                                      \
		done;                                                                                         \
		$(ECHO) "------------ Finish ahead-of-time compilation for modules in $(SRC)/ -------------"; \
	fi;                                                                                                   \
	$(MV) $(MVFLAGS) $(SRC) $(SRC)s

.PHONY: all clean

all: $(LIB)

clean:
	$(RM) $(RMFLAGS) $(LIB)

# vim:set nu ts=4 sw=4:
