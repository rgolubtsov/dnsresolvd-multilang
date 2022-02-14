/*
 * src/js/dnsresolvd.h
 * ============================================================================
 * DNS Resolver Daemon (dnsresolvd). Version 0.1
 * ============================================================================
 * A daemon that performs DNS lookups for the given hostname
 * passed in an HTTP request, with the focus on its implementation
 * using various programming languages. (Node.js-boosted impl.)
 * ============================================================================
 * Copyright (C) 2017-2022 Radislav (Radicchio) Golubtsov
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
    this._EXIT_FAILURE     =      1; //    Failing exit status.
    this._EXIT_SUCCESS     =      0; // Successful exit status.
    this._EMPTY_STRING     =     "";
    this._ONE_SPACE_STRING =    " ";
    this._COLON_SPACE_SEP  =   ": ";
    this._COMMA_SPACE_SEP  =   ", ";
    this._NEW_LINE         = os.EOL;
    this._PRINT_BANNER_OPT =   "-V";
    this._QUESTION_MARK    =    "?";

    /* Common error messages and codes. */
    this._ERR_PREFIX                    = "error";
    this._ERR_PORT_MUST_BE_POSITIVE_INT = ": <port_number> must be "
                                        + "a positive integer value, "
                                        + "in the range 1024-49151.";
    this._ERR_CANNOT_START_SERVER       = ": FATAL: Cannot start server ";
    this._ERR_SRV_UNKNOWN_REASON        = "for an unknown reason. Exiting...";
    this._ERR_SRV_PORT_IS_IN_USE        = "due to the port requested "
                                        + "is in use. Exiting...";
    this._ERR_COULD_NOT_LOOKUP          = "could not lookup hostname";
    this._ERR_EADDRINUSE                = "EADDRINUSE";

    /* Print this error message when there are no any args passed. */
    this._ERR_MUST_BE_ONE_TWO_ARGS_1 = ": There must be one or two args passed"
                                     + ": ";
    this._ERR_MUST_BE_ONE_TWO_ARGS_2 = " args found";

    /* Print this usage info just after any inappropriate input. */
    this._MSG_USAGE_TEMPLATE_1 = "Usage: ";
    this._MSG_USAGE_TEMPLATE_2 = " <port_number> [-V]";

    /* Syslog-related constants. */
    this._LOG_DAEMON_EXT      = ".js";
    this._LOG_FACILITY_DAEMON = "daemon";
    this._LOG_PRIORITY_ERR    = "err";
    this._LOG_PRIORITY_INFO   = "info";

    /* Daemon class events. */
    this._EVE_ERROR     = "error";
    this._EVE_LISTENING = "listening";
    this._EVE_DATA      = "data";
    this._EVE_END       = "end";

    /** Constant: The minimum port number allowed. */
    this._MIN_PORT = 1024;

    /** Constant: The maximum port number allowed. */
    this._MAX_PORT = 49151;

    /* Common notification messages. */
    this._MSG_SERVER_STARTED_1 = "Server started on port ";
    this._MSG_SERVER_STARTED_2 = "=== Hit Ctrl+C to terminate it.";

    /* HTTP request methods and params. */
    this._MTD_HTTP_GET  = "GET";
    this._MTD_HTTP_POST = "POST";
    this._PRM_FMT_HTML  = "html";
    this._PRM_FMT_JSON  = "json";

    /* HTTP response headers and status codes. */
    this._HDR_CONTENT_TYPE_N      = "Content-Type";
    this._HDR_CONTENT_TYPE_V_HTML = "text/html; charset=UTF-8";
    this._HDR_CONTENT_TYPE_V_JSON = "application/json";
    this._HDR_CONTENT_LENGTH_N    = "Content-Length";
    this._HDR_CACHE_CONTROL_N     = "Cache-Control";
    this._HDR_CACHE_CONTROL_V     = "no-cache, no-store, must-revalidate";
    this._HDR_EXPIRES_N           = "Expires";
    this._HDR_EXPIRES_V           = "Thu, 01 Dec 1994 16:00:00 GMT";
    this._HDR_PRAGMA_N            = "Pragma";
    this._HDR_PRAGMA_V            = "no-cache";
    this._RSC_HTTP_200_OK         = 200;

    /* Response data names. */
    this._DAT_HOSTNAME_N = "hostname";
    this._DAT_ADDRESS_N  = "address";
    this._DAT_VERSION_N  = "version";
    this._DAT_VERSION_V  = "IPv";

    /* Daemon name, version, and copyright banners. */
    this._DMN_NAME        = "DNS Resolver Daemon (dnsresolvd)";
    this._DMN_DESCRIPTION = "Performs DNS lookups for the given hostname "
                          + "passed in an HTTP request";
    this._DMN_VERSION_S__ = "Version";
    this._DMN_VERSION     = "0.1";
    this._DMN_COPYRIGHT__ = "Copyright (C) 2017-2022";
    this._DMN_AUTHOR      = "Radislav Golubtsov <ragolubtsov@my.com>";

    /** Constant: The default hostname to look up for. */
    this._DEF_HOSTNAME = "openbsd.org";
};

module.exports = exports = __DNSRESOLVD_H;

// vim:set nu et ts=4 sw=4:
