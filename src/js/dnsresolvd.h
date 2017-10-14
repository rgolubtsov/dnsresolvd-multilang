/*
 * content/dev/misc/dnsresolvd/js/dnsresolvd.h
 * ============================================================================
 * DNS Resolver Daemon (dnsresolvd). Version 0.1
 * ============================================================================
 * A Node.js-boosted daemon for performing DNS lookups.
 * ============================================================================
 * Copyright (C) 2017 Radislav (Radicchio) Golubtsov
 *
 * (See the LICENSE file at the top of the source tree.)
 */

"use strict";

var os = require("os");

/**
 * The helper class for the main daemon module.
 * It aimed at accumulating such pieces like constants, globals, etc.,
 * just like it behaves in its C counterpart. Yep, that's a little funny
 * to make it named like in C, but definitely feasible nevertheless!))
 */
var __DNSRESOLVD_H = function() {
    /* Helper constants. */
    this._EXIT_FAILURE     =    1; //    Failing exit status.
    this._EXIT_SUCCESS     =    0; // Successful exit status.
    this._ONE_SPACE_STRING =  " ";
    this._COLON_SPACE_SEP  = ": ";
    this._COMMA_SPACE_SEP  = ", ";
    this._NEW_LINE         = os.EOL;

    /* Common error messages and codes. */
    this._ERR_PREFIX                    = "Error";
    this._ERR_PORT_MUST_BE_POSITIVE_INT = ": <port_number> must be "
                                        + "a positive integer value, "
                                        + "in the range 1024-49151.";
    this._ERR_CANNOT_START_SERVER       = ": FATAL: Cannot start server ";
    this._ERR_SRV_UNKNOWN_REASON        = "for an unknown reason. Exiting...";
    this._ERR_SRV_PORT_IS_IN_USE        = "due to the port requested "
                                        + "is in use. Exiting...";
    this._ERR_COULD_NOT_LOOKUP          = "Could not lookup hostname.";
    this._ERR_EADDRINUSE                = "EADDRINUSE";

    /* Print this error message when there are no any args passed. */
    this._ERR_MUST_BE_THE_ONLY_ARG_1 = ": There must be exactly one arg passed"
                                     + ": ";
    this._ERR_MUST_BE_THE_ONLY_ARG_2 = " args found";

    /* Print this usage info just after any inappropriate input. */
    this._MSG_USAGE_TEMPLATE_1 = "Usage: ";
    this._MSG_USAGE_TEMPLATE_2 = " <port_number>";

    /* Syslog-related constants. */
    this._LOG_DAEMON_EXT      = ".js";
    this._LOG_FACILITY_DAEMON = "daemon";
    this._LOG_PRIORITY_ERR    = "err";
    this._LOG_PRIORITY_INFO   = "info";

    /* Daemon class events. */
    this._EVE_ERROR     = "error";
    this._EVE_LISTENING = "listening";

    /** Constant: The minimum port number allowed. */
    this._MIN_PORT = 1024;

    /** Constant: The maximum port number allowed. */
    this._MAX_PORT = 49151;

    /* Common notification messages. */
    this._MSG_SERVER_STARTED_1 = "Server started on port ";
    this._MSG_SERVER_STARTED_2 = "=== Hit Ctrl+C to terminate it.";

    /* HTTP response headers and status codes. */
    this._HDR_CONTENT_TYPE  = "text/html; charset=UTF-8";
    this._HDR_CACHE_CONTROL = "no-cache, no-store, must-revalidate";
    this._HDR_EXPIRES       = "Thu, 01 Dec 1994 16:00:00 GMT";
    this._HDR_PRAGMA        = "no-cache";
    this._RSC_HTTP_200_OK   = 200;

    /* Daemon name, version, and copyright banners. */
    this._DMN_NAME        = "DNS Resolver Daemon (dnsresolvd)";
    this._DMN_DESCRIPTION = "Performs DNS lookups for the given hostname "
                          + "passed in an HTTP request";
    this._DMN_VERSION_S__ = "Version";
    this._DMN_VERSION     = "0.1";
    this._DMN_COPYRIGHT__ = "Copyright (C) 2017";
    this._DMN_AUTHOR      = "Radislav Golubtsov <ragolubtsov@my.com>";

    /** Constant: The default hostname to look up for. */
    this._DEF_HOSTNAME = "openbsd.org";
};

module.exports = exports = __DNSRESOLVD_H;

// vim:set nu:et:ts=4:sw=4:
