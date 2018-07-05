[indent=4]
/* src/vala/dnsresolvh.gs
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
class AUX
    // Helper constants.
    const EMPTY_STRING     : string =   ""
    const COMMA_SPACE_SEP  : string = ", "
    const NEW_LINE         : string = "\n"
    const S_FMT            : string = "%s"
    const V_BAR            : string =  "|"
    const SPACE            : string =  " "
    const PRINT_BANNER_OPT : string = "-V"

    // Common error messages.
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

    // Daemon name, version, and copyright banners.
    const DMN_NAME        : string =  "DNS Resolver Daemon (dnsresolvd)"
    const DMN_DESCRIPTION : string = ("Performs DNS lookups for the given "
                                   +  "hostname passed in an HTTP request")
    const DMN_VERSION_S__ : string =  "Version"
    const DMN_VERSION     : string =  "0.1"
    const DMN_COPYRIGHT__ : string =  "Copyright (C) 2017-2018"
    const DMN_AUTHOR      : string =  "Radislav Golubtsov <ragolubtsov@my.com>"

    // Helper method. Makes final buffer cleanups, closes streams, etc.
    def cleanups_fixate() : void
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
