/*
 * src/genie/dnsresolvh.vala
 * ============================================================================
 * DNS Resolver Daemon (dnsresolvd). Version 0.1
 * ============================================================================
 * A daemon that performs DNS lookups for the given hostname
 * passed in an HTTP request, with the focus on its implementation
 * using various programming languages. (libsoup-boosted impl.)
 * ============================================================================
 * Copyright (C) 2017-2018 Radislav (Radicchio) Golubtsov
 *
 * (See the LICENSE file at the top of the source tree.)
 */

/** The helper class for the daemon. */
class AUX {
    // Helper constants.
    const string EMPTY_STRING     =   "";
    const string COLON_SPACE_SEP  = ": ";
    const string COMMA_SPACE_SEP  = ", ";
    const string NEW_LINE         = "\n";
    const string C_FMT            = "%c";
    const string AMPER            =  "&";
    const string SPACE            =  " ";
    const string PRINT_BANNER_OPT = "-V";

    // Common error messages.
    const string ERR_PREFIX                    = "error";
    const string ERR_PORT_MUST_BE_POSITIVE_INT = "%s: <port_number> must "
                                               + "be a positive integer "
                                               + "value, in the range "
                                               + "1024-49151.";
    const string ERR_CANNOT_START_SERVER       = "%s: FATAL: "
                                               + "Cannot start server ";
    const string ERR_SRV_UNKNOWN_REASON        = "for an unknown reason. "
                                               + "Exiting...";
    const string ERR_SRV_PORT_IS_IN_USE        = "due to the port "
                                               + "requested is in use. "
                                               + "Exiting...";
    const string ERR_COULD_NOT_LOOKUP          = "could not lookup hostname";
    const string ERR_ADDR_ALREADY_IN_USE       = "^.*(\\ is\\ |\\ in\\ ).*$";

    // Print this error message when there are no any args passed.
    const string ERR_MUST_BE_ONE_TWO_ARGS = "%s: There must be one or two "
                                          + "args passed: %u args found";

    // Print this usage info just after any inappropriate input.
    const string MSG_USAGE_TEMPLATE = "Usage: %s <port_number> [-V]";

    /** Constant: The minimum port number allowed. */
    const uint MIN_PORT = 1024;

    /** Constant: The maximum port number allowed. */
    const uint MAX_PORT = 49151;

    // Common notification messages.
    const string MSG_SERVER_STARTED_1 = "Server started on port %u";
    const string MSG_SERVER_STARTED_2 = "=== Hit Ctrl+C to terminate it.";

    // HTTP request methods and params.
    const string MTD_HTTP_GET  = "GET";
    const string MTD_HTTP_POST = "POST";
    const string PRM_FMT_HTML  = "html";
    const string PRM_FMT_JSON  = "json";

    // HTTP response headers.
    const string HDR_CONTENT_TYPE_N      = "Content-Type";
    const string HDR_CONTENT_TYPE_V_HTML = "text/html; charset=UTF-8";
    const string HDR_CONTENT_TYPE_V_JSON = "application/json";
    const string HDR_CACHE_CONTROL_N     = "Cache-Control";
    const string HDR_CACHE_CONTROL_V     = "no-cache, no-store, "
                                         + "must-revalidate";
    const string HDR_EXPIRES_N           = "Expires";
    const string HDR_EXPIRES_V           = "Thu, 01 Dec 1994 16:00:00 GMT";
    const string HDR_PRAGMA_N            = "Pragma";
    const string HDR_PRAGMA_V            = "no-cache";

    // Response data names.
    const string DAT_HOSTNAME_N = "hostname";
    const string DAT_ADDRESS_N  = "address";
    const string DAT_VERSION_N  = "version";
    const string DAT_VERSION_V  = "IPv";

    // Daemon name, version, and copyright banners.
    const string DMN_NAME        = "DNS Resolver Daemon (dnsresolvd)";
    const string DMN_DESCRIPTION = "Performs DNS lookups for the given "
                                 + "hostname passed in an HTTP request";
    const string DMN_VERSION_S__ = "Version";
    const string DMN_VERSION     = "0.1";
    const string DMN_COPYRIGHT__ = "Copyright (C) 2017-2018";
    const string DMN_AUTHOR      = "Radislav Golubtsov <ragolubtsov@my.com>";

    /** Constant: The default hostname to look up for. */
    const string DEF_HOSTNAME = "openbsd.org";

    /** Default constructor. */
    public AUX() {}
}

// vim:set nu et ts=4 sw=4:
