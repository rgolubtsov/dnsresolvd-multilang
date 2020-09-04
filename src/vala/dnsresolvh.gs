[indent=4]
/* src/vala/dnsresolvh.gs
 * ============================================================================
 * DNS Resolver Daemon (dnsresolvd). Version 0.1
 * ============================================================================
 * A daemon that performs DNS lookups for the given hostname
 * passed in an HTTP request, with the focus on its implementation
 * using various programming languages. (libsoup-boosted impl.)
 * ============================================================================
 * Copyright (C) 2017-2020 Radislav (Radicchio) Golubtsov
 *
 * (See the LICENSE file at the top of the source tree.)
 */

/** The helper class for the daemon. */
class AUX
    // Helper constants.
    const EMPTY_STRING     : string =   ""
    const COLON_SPACE_SEP  : string = ": "
    const COMMA_SPACE_SEP  : string = ", "
    const NEW_LINE         : string = "\n"
    const C_FMT            : string = "%c"
    const AMPER            : string =  "&"
    const SPACE            : string =  " "
    const PRINT_BANNER_OPT : string = "-V"

    // Common error messages.
    const ERR_PREFIX                    : string =  "error"
    const ERR_PORT_MUST_BE_POSITIVE_INT : string = ("%s: <port_number> must "
                                                 +  "be a positive integer "
                                                 +  "value, in the range "
                                                 +  "1024-49151.")
    const ERR_CANNOT_START_SERVER       : string = ("%s: FATAL: "
                                                 +  "Cannot start server ")
    const ERR_SRV_UNKNOWN_REASON        : string = ("for an unknown reason. "
                                                 +  "Exiting...")
    const ERR_SRV_PORT_IS_IN_USE        : string = ("due to the port "
                                                 +  "requested is in use. "
                                                 +  "Exiting...")
    const ERR_COULD_NOT_LOOKUP          : string =  "could not lookup hostname"
    const ERR_ADDR_ALREADY_IN_USE       : string =  "^.*(\\ is\\ |\\ in\\ ).*$"

    // Print this error message when there are no any args passed.
    const ERR_MUST_BE_ONE_TWO_ARGS : string = ("%s: There must be one or two "
                                            +  "args passed: %u args found")

    // Print this usage info just after any inappropriate input.
    const MSG_USAGE_TEMPLATE : string = "Usage: %s <port_number> [-V]"

    /** Constant: The minimum port number allowed. */
    const MIN_PORT : uint = 1024

    /** Constant: The maximum port number allowed. */
    const MAX_PORT : uint = 49151

    // Common notification messages.
    const MSG_SERVER_STARTED_1 : string = "Server started on port %u"
    const MSG_SERVER_STARTED_2 : string = "=== Hit Ctrl+C to terminate it."

    // HTTP request methods and params.
    const MTD_HTTP_GET  : string = "GET"
    const MTD_HTTP_POST : string = "POST"
    const PRM_FMT_HTML  : string = "html"
    const PRM_FMT_JSON  : string = "json"

    // HTTP response headers.
    const HDR_CONTENT_TYPE_N      : string =  "Content-Type"
    const HDR_CONTENT_TYPE_V_HTML : string =  "text/html; charset=UTF-8"
    const HDR_CONTENT_TYPE_V_JSON : string =  "application/json"
    const HDR_CACHE_CONTROL_N     : string =  "Cache-Control"
    const HDR_CACHE_CONTROL_V     : string = ("no-cache, no-store, "
                                           +  "must-revalidate")
    const HDR_EXPIRES_N           : string =  "Expires"
    const HDR_EXPIRES_V           : string =  "Thu, 01 Dec 1994 16:00:00 GMT"
    const HDR_PRAGMA_N            : string =  "Pragma"
    const HDR_PRAGMA_V            : string =  "no-cache"

    // Response data names.
    const DAT_HOSTNAME_N : string = "hostname"
    const DAT_ADDRESS_N  : string = "address"
    const DAT_VERSION_N  : string = "version"
    const DAT_VERSION_V  : string = "IPv"

    // Daemon name, version, and copyright banners.
    const DMN_NAME        : string =  "DNS Resolver Daemon (dnsresolvd)"
    const DMN_DESCRIPTION : string = ("Performs DNS lookups for the given "
                                   +  "hostname passed in an HTTP request")
    const DMN_VERSION_S__ : string =  "Version"
    const DMN_VERSION     : string =  "0.1"
    const DMN_COPYRIGHT__ : string =  "Copyright (C) 2017-2020"
    const DMN_AUTHOR      : string =  "Radislav Golubtsov <ragolubtsov@my.com>"

    /** Constant: The default hostname to look up for. */
    const DEF_HOSTNAME : string = "openbsd.org"

    /**
     * Adds headers to the response.
     *
     * @param resp_hdrs The response headers object.
     * @param fmt       The response format selector.
     *
     * @return The <code>"Content-Type"</code> response header value
     *         used in the caller's <code>msg.set_response()</code> method.
     */
    def add_response_headers(resp_hdrs : Soup.MessageHeaders,
                             fmt       : string) : string

        resp_hdrs.append(HDR_CACHE_CONTROL_N, HDR_CACHE_CONTROL_V)
        resp_hdrs.append(HDR_EXPIRES_N,       HDR_EXPIRES_V      )
        resp_hdrs.append(HDR_PRAGMA_N,        HDR_PRAGMA_V       )

        var HDR_CONTENT_TYPE_V = EMPTY_STRING

        if      (fmt == PRM_FMT_HTML)
            HDR_CONTENT_TYPE_V = HDR_CONTENT_TYPE_V_HTML
        else if (fmt == PRM_FMT_JSON)
            HDR_CONTENT_TYPE_V = HDR_CONTENT_TYPE_V_JSON

        return HDR_CONTENT_TYPE_V

    // Helper method. Makes final buffer cleanups, closes streams, etc.
    def cleanups_fixate(loop : MainLoop) : void
        // Stopping the daemon.
        if ((loop != null) && (loop.is_running()))
            loop.quit();

        // Closing the system logger.
        Posix.closelog();

    // Helper method. Draws a horizontal separator banner.
    def separator_draw(banner_text : string) : void
        i : int = banner_text.length

        while (i > 0)
            stdout.putc('='); i--

        stdout.puts(NEW_LINE)

    /** Default constructor. */
    construct()
        pass

// vim:set nu et ts=4 sw=4:
