/*
 * src/c/dnsresolvd.h
 * ============================================================================
 * DNS Resolver Daemon (dnsresolvd). Version 0.1
 * ============================================================================
 * A daemon that performs DNS lookups for the given hostname
 * passed in an HTTP request, with the focus on its implementation
 * using various programming languages. (GNU libmicrohttpd-boosted impl.)
 * ============================================================================
 * Copyright (C) 2017-2018 Radislav (Radicchio) Golubtsov
 *
 * (See the LICENSE file at the top of the source tree.)
 */

#ifndef __DNSRESOLVD_H
#define __DNSRESOLVD_H

#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <syslog.h>
#include <netdb.h>
#include <arpa/inet.h>

#include <microhttpd.h>

/* Helper constants. */
#define _EMPTY_STRING       ""
#define _ONE_SPACE_STRING  " "
#define _COLON_SPACE_SEP  ": "
#define _COMMA_SPACE_SEP  ", "
#define _NEW_LINE         "\n"
#define _PRINT_BANNER_OPT "-V"

/* Common error messages. */
#define _ERR_PREFIX                    "error"
#define _ERR_PORT_MUST_BE_POSITIVE_INT "%s: <port_number> must be "      \
                                       "a positive integer value, "      \
                                       "in the range 1024-49151."
#define _ERR_CANNOT_START_SERVER       "%s: FATAL: Cannot start server " \
                                       "for an unknown reason. Exiting..."
#define _ERR_COULD_NOT_LOOKUP          "could not lookup hostname"

/* Print this error message when there are no any args passed. */
#define _ERR_MUST_BE_ONE_TWO_ARGS "%s: There must be one or two args " \
                                  "passed: %u args found"

/* Print this usage info just after any inappropriate input. */
#define _MSG_USAGE_TEMPLATE "Usage: %s <port_number> [-V]"

/** Constant: The minimum port number allowed. */
#define _MIN_PORT 1024

/** Constant: The maximum port number allowed. */
#define _MAX_PORT 49151

/* Common notification messages. */
#define _MSG_SERVER_STARTED_1 "Server started on port %u"
#define _MSG_SERVER_STARTED_2 "=== Hit Ctrl+C to terminate it."

/* HTTP response headers. */
#define _HDR_CONTENT_TYPE  "text/html; charset=UTF-8"
#define _HDR_CACHE_CONTROL "no-cache, no-store, must-revalidate"
#define _HDR_EXPIRES       "Thu, 01 Dec 1994 16:00:00 GMT"
#define _HDR_PRAGMA        "no-cache"

/* Daemon name, version, and copyright banners. */
#define _DMN_NAME        "DNS Resolver Daemon (dnsresolvd)"
#define _DMN_DESCRIPTION "Performs DNS lookups for the given hostname " \
                         "passed in an HTTP request"
#define _DMN_VERSION_S__ "Version"
#define _DMN_VERSION     "0.1"
#define _DMN_COPYRIGHT__ "Copyright (C) 2017-2018"
#define _DMN_AUTHOR      "Radislav Golubtsov <ragolubtsov@my.com>"

/** Constant: The default hostname to look up for. */
#define _DEF_HOSTNAME "openbsd.org"

/*
 * Performs DNS lookup action for the given hostname,
 * i.e. (in this case) IP address retrieval by hostname.
 */
char *dns_lookup(char *, const char *);

#endif /* __DNSRESOLVD_H */

/* vim:set nu et ts=4 sw=4: */
