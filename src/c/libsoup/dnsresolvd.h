/*
 * src/c/libsoup/dnsresolvd.h
 * ============================================================================
 * DNS Resolver Daemon (dnsresolvd). Version 0.9.9
 * ============================================================================
 * A daemon that performs DNS lookups for the given hostname
 * passed in an HTTP request, with the focus on its implementation
 * using various programming languages. (GNOME libsoup-boosted impl.)
 * ============================================================================
 * Copyright (C) 2017-2025 Radislav (Radicchio) Golubtsov
 *
 * (See the LICENSE file at the top of the source tree.)
 */

#ifndef __DNSRESOLVD_H
#define __DNSRESOLVD_H

#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdio.h>
#include <syslog.h>
#include <stdbool.h>

#include <libsoup/soup.h>
#include <glib.h>
#include <glib-unix.h>
#include <json-glib/json-glib.h>

#include <netdb.h>
#include <arpa/inet.h>
#include <sys/socket.h>

/* Helper constants. */
#define _EMPTY_STRING       ""
#define _COLON_SPACE_SEP  ": "
#define _COMMA_SPACE_SEP  ", "
#define _NEW_LINE         "\n"
#define _AMPER             "&"
#define _ONE_SPACE_STRING  " "
#define _PRINT_BANNER_OPT "-V"

/* Common error messages. */
#define _ERR_PREFIX                    "error"
#define _ERR_PORT_MUST_BE_POSITIVE_INT "%s: <port_number> must be "           \
                                       "a positive integer value, "           \
                                       "in the range 1024-49151."
#define _ERR_CANNOT_START_SERVER       "%s: FATAL: Cannot start server "
#define _ERR_SRV_UNKNOWN_REASON        "for an unknown reason. Exiting..."
#define _ERR_SRV_PORT_IS_IN_USE        "due to the port requested is in use. "\
                                       "Exiting..."
#define _ERR_COULD_NOT_LOOKUP          "could not lookup hostname"
#define _ERR_ADDR_ALREADY_IN_USE       "Address already in use"

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

/* HTTP request params. */
#define _PRM_FMT_HTML "html"
#define _PRM_FMT_JSON "json"

/* HTTP response headers. */
#define _HDR_CONTENT_TYPE_N      "Content-Type"
#define _HDR_CONTENT_TYPE_V_HTML "text/html; charset=UTF-8"
#define _HDR_CONTENT_TYPE_V_JSON "application/json"
#define _HDR_CACHE_CONTROL_N     "Cache-Control"
#define _HDR_CACHE_CONTROL_V     "no-cache, no-store, must-revalidate"
#define _HDR_EXPIRES_N           "Expires"
#define _HDR_EXPIRES_V           "Thu, 01 Dec 1994 16:00:00 GMT"
#define _HDR_PRAGMA_N            "Pragma"
#define _HDR_PRAGMA_V            "no-cache"

/* Response data names. */
#define _DAT_HOSTNAME_N "hostname"
#define _DAT_ADDRESS_N  "address"
#define _DAT_VERSION_N  "version"
#define _DAT_VERSION_V  "IPv"

/* Daemon name, version, and copyright banners. */
#define _DMN_NAME        "DNS Resolver Daemon (dnsresolvd)"
#define _DMN_DESCRIPTION "Performs DNS lookups for the given hostname " \
                         "passed in an HTTP request"
#define _DMN_VERSION_S__ "Version"
#define _DMN_VERSION     "0.9.9"
#define _DMN_COPYRIGHT__ "Copyright (C) 2017-2025"
#define _DMN_AUTHOR      "Radislav Golubtsov <radicchio@vk.com>"

/** Constant: The default hostname to look up for. */
#define _DEF_HOSTNAME "openbsd.org"

/**
 * The structure to hold IP address of the analyzed host/service
 * and a corresponding IP version (family) used to look up in DNS:
 * <code>4</code> for IPv4-only hosts,
 * <code>6</code> for IPv6-capable hosts.
 */
typedef struct {
    char           *addr;
    unsigned short  ver;
} ADDR_VER;

/* Performs DNS lookup action for the given hostname. */
ADDR_VER *dns_lookup(ADDR_VER *, const char *);

/* Adds headers to the response. */
char *add_response_headers(SoupMessageHeaders *, const char *);

/* Helper protos. */
void _cleanups_fixate(const gpointer);
void _separator_draw(const char *);

#endif /* __DNSRESOLVD_H */

/* vim:set nu et ts=4 sw=4: */
