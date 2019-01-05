/*
 * src/genie/dnsresolvh.vala
 * ============================================================================
 * DNS Resolver Daemon (dnsresolvd). Version 0.1
 * ============================================================================
 * A daemon that performs DNS lookups for the given hostname
 * passed in an HTTP request, with the focus on its implementation
 * using various programming languages. (libsoup-boosted impl.)
 * ============================================================================
 * Copyright (C) 2017-2019 Radislav (Radicchio) Golubtsov
 *
 * (See the LICENSE file at the top of the source tree.)
 */

/** The helper class for the daemon. */
class AUX {
    // Helper constants.
    public const string EMPTY_STRING     =   "";
    public const string COLON_SPACE_SEP  = ": ";
    public const string COMMA_SPACE_SEP  = ", ";
    public const string NEW_LINE         = "\n";
    public const string C_FMT            = "%c";
    public const string AMPER            =  "&";
    public const string SPACE            =  " ";
    public const string PRINT_BANNER_OPT = "-V";

    // Common error messages.
    public const string ERR_PREFIX                    = "error";
    public const string ERR_PORT_MUST_BE_POSITIVE_INT = "%s: <port_number> must "
                                                      + "be a positive integer "
                                                      + "value, in the range "
                                                      + "1024-49151.";
    public const string ERR_CANNOT_START_SERVER       = "%s: FATAL: "
                                                      + "Cannot start server ";
    public const string ERR_SRV_UNKNOWN_REASON        = "for an unknown reason. "
                                                      + "Exiting...";
    public const string ERR_SRV_PORT_IS_IN_USE        = "due to the port "
                                                      + "requested is in use. "
                                                      + "Exiting...";
    public const string ERR_COULD_NOT_LOOKUP          = "could not lookup hostname";
    public const string ERR_ADDR_ALREADY_IN_USE       = "^.*(\\ is\\ |\\ in\\ ).*$";

    // Print this error message when there are no any args passed.
    public const string ERR_MUST_BE_ONE_TWO_ARGS = "%s: There must be one or two "
                                                 + "args passed: %u args found";

    // Print this usage info just after any inappropriate input.
    public const string MSG_USAGE_TEMPLATE = "Usage: %s <port_number> [-V]";

    /** Constant: The minimum port number allowed. */
    public const uint MIN_PORT = 1024;

    /** Constant: The maximum port number allowed. */
    public const uint MAX_PORT = 49151;

    // Common notification messages.
    public const string MSG_SERVER_STARTED_1 = "Server started on port %u";
    public const string MSG_SERVER_STARTED_2 = "=== Hit Ctrl+C to terminate it.";

    // HTTP request methods and params.
    public const string MTD_HTTP_GET  = "GET";
    public const string MTD_HTTP_POST = "POST";
    public const string PRM_FMT_HTML  = "html";
    public const string PRM_FMT_JSON  = "json";

    // HTTP response headers.
    public const string HDR_CONTENT_TYPE_N      = "Content-Type";
    public const string HDR_CONTENT_TYPE_V_HTML = "text/html; charset=UTF-8";
           const string HDR_CONTENT_TYPE_V_JSON = "application/json";
           const string HDR_CACHE_CONTROL_N     = "Cache-Control";
           const string HDR_CACHE_CONTROL_V     = "no-cache, no-store, "
                                                + "must-revalidate";
           const string HDR_EXPIRES_N           = "Expires";
           const string HDR_EXPIRES_V           = "Thu, 01 Dec 1994 16:00:00 GMT";
           const string HDR_PRAGMA_N            = "Pragma";
           const string HDR_PRAGMA_V            = "no-cache";

    // Response data names.
    public const string DAT_HOSTNAME_N = "hostname";
    public const string DAT_ADDRESS_N  = "address";
    public const string DAT_VERSION_N  = "version";
    public const string DAT_VERSION_V  = "IPv";

    // Daemon name, version, and copyright banners.
    public const string DMN_NAME        = "DNS Resolver Daemon (dnsresolvd)";
    public const string DMN_DESCRIPTION = "Performs DNS lookups for the given "
                                        + "hostname passed in an HTTP request";
    public const string DMN_VERSION_S__ = "Version";
    public const string DMN_VERSION     = "0.1";
    public const string DMN_COPYRIGHT__ = "Copyright (C) 2017-2019";
    public const string DMN_AUTHOR      = "Radislav Golubtsov <ragolubtsov@my.com>";

    /** Constant: The default hostname to look up for. */
    public const string DEF_HOSTNAME = "openbsd.org";

    /**
     * Adds headers to the response.
     *
     * @param resp_hdrs The response headers object.
     * @param fmt       The response format selector.
     *
     * @return The <code>"Content-Type"</code> response header value
     *         used in the caller's <code>msg.set_response()</code> method.
     */
    public string add_response_headers(Soup.MessageHeaders resp_hdrs,
                                       string              fmt) {

        resp_hdrs.append(HDR_CACHE_CONTROL_N, HDR_CACHE_CONTROL_V);
        resp_hdrs.append(HDR_EXPIRES_N,       HDR_EXPIRES_V      );
        resp_hdrs.append(HDR_PRAGMA_N,        HDR_PRAGMA_V       );

        var HDR_CONTENT_TYPE_V = EMPTY_STRING;

               if (fmt == PRM_FMT_HTML) {
            HDR_CONTENT_TYPE_V = HDR_CONTENT_TYPE_V_HTML;
        } else if (fmt == PRM_FMT_JSON) {
            HDR_CONTENT_TYPE_V = HDR_CONTENT_TYPE_V_JSON;
        }

        return HDR_CONTENT_TYPE_V;
    }

    // Helper method. Makes final buffer cleanups, closes streams, etc.
    public void cleanups_fixate(MainLoop loop) {
        // Stopping the daemon.
        if ((loop != null) && (loop.is_running())) {
            loop.quit();
        }

        // Closing the system logger.
        Posix.closelog();
    }

    // Helper method. Draws a horizontal separator banner.
    public void separator_draw(string banner_text) {
        int i = banner_text.length;

        do { stdout.putc('='); i--; } while (i > 0); stdout.puts(NEW_LINE);
    }

    /** Default constructor. */
    public AUX() {}
}

// vim:set nu et ts=4 sw=4:
