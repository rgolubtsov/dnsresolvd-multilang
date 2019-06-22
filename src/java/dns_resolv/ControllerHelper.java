/*
 * src/java/dns_resolv/ControllerHelper.java
 * ============================================================================
 * DNS Resolver Daemon (dnsresolvd). Version 0.1
 * ============================================================================
 * A daemon that performs DNS lookups for the given hostname
 * passed in an HTTP request, with the focus on its implementation
 * using various programming languages. (Vertosphere-boosted impl.)
 * ============================================================================
 * Copyright (C) 2017-2019 Radislav (Radicchio) Golubtsov
 *
 * (See the LICENSE file at the top of the source tree.)
 */

package dns_resolv;

/** The helper for the controller class and related ones. */
public class ControllerHelper {
    // Helper constants.
    public static final int    EXIT_FAILURE     =    1; //    Failing exit status.
    public static final int    EXIT_SUCCESS     =    0; // Successful exit status.
    public static final String EMPTY_STRING     =   "";
    public static final String ONE_SPACE_STRING =  " ";
    public static final String COLON_SPACE_SEP  = ": ";
    public static final String COMMA_SPACE_SEP  = ", ";
    public static final String PRINT_BANNER_OPT = "-V";
    public static final String NEW_LINE         = System.lineSeparator();

    // Common error messages.
    public static final String ERR_PORT_MUST_BE_POSITIVE_INT = ": <port_number> must be "
                                                             + "a positive integer value, "
                                                             + "in the range 1024-49151.";

    // Print this error message when there are no any args passed.
    public static final String ERR_MUST_BE_ONE_TWO_ARGS_1 = ": There must be one or two args passed: ";
    public static final String ERR_MUST_BE_ONE_TWO_ARGS_2 = " args found";

    // Print this usage info just after any inappropriate input.
    public static final String MSG_USAGE_TEMPLATE_1 = "Usage: ";
    public static final String MSG_USAGE_TEMPLATE_2 = " <port_number> [-V]";

    /** Constant: The minimum port number allowed. */
    public static final int MIN_PORT = 1024;

    /** Constant: The maximum port number allowed. */
    public static final int MAX_PORT = 49151;

    // Daemon name, version, and copyright banners.
    public static final String DMN_NAME        = "DNS Resolver Daemon (dnsresolvd)";
    public static final String DMN_DESCRIPTION = "Performs DNS lookups for the given "
                                               + "hostname passed in an HTTP request";
    public static final String DMN_VERSION_S__ = "Version";
    public static final String DMN_VERSION     = "0.1";
    public static final String DMN_COPYRIGHT__ = "Copyright (C) 2017-2019";
    public static final String DMN_AUTHOR      = "Radislav Golubtsov <ragolubtsov@my.com>";

    // Helper method. Makes final buffer cleanups, closes streams, etc.
    public static void cleanups_fixate() {
        // TODO: Implement the cleanups_fixate method.
    }

    // Helper method. Draws a horizontal separator banner.
    public static void separator_draw(String banner_text) {
        int i = banner_text.length();

        do { System.out.print('='); i--; } while (i > 0); System.out.println();
    }
}

// vim:set nu et ts=4 sw=4:
