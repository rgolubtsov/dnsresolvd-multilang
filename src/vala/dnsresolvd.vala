/*
 * src/vala/dnsresolvd.vala
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

/** The main class of the daemon. */
class DnsResolvd {
    /**
     * Starts up the daemon.
     *
     * @param port_number The server port number to listen on.
     *
     * @return The server exit code when interrupted.
     */
    public int startup(uint64 port_number) {
        int ret = Posix.EXIT_SUCCESS;

//      stdout.puts(port_number.to_string() + AUX.NEW_LINE);

        // -------------------------------------------
        // TODO: Implement starting up the HTTP server
        //       using the libsoup GNOME library.
        // -------------------------------------------

        return ret;
    }

    /** Default constructor. */
    public DnsResolvd() {}
}

// The daemon entry point.
public static int main(string[] args) {
    int ret = Posix.EXIT_SUCCESS;

    int argc = args.length - 1;

    // Instantiating the daemon helper class.
    var aux = new AUX();

    var    daemon_name = args[0];
    uint64 port_number;

    string print_banner_opt = AUX.EMPTY_STRING;

    if (argc > 0) {
        port_number = uint64.parse(args[1]);

        if (argc > 1) {
            print_banner_opt = args[2];
        }
    } else {
        port_number = 0;
    }

//  stdout.printf(AUX.S_FMT,  AUX.V_BAR + AUX.SPACE + argc.to_string()
//              + AUX.SPACE + AUX.V_BAR + AUX.SPACE + daemon_name
//              + AUX.SPACE + AUX.V_BAR + AUX.SPACE + port_number.to_string()
//              + AUX.SPACE + AUX.V_BAR + AUX.SPACE + print_banner_opt
//              + AUX.SPACE + AUX.V_BAR + AUX.NEW_LINE);

    if (print_banner_opt == AUX.PRINT_BANNER_OPT) {
        aux.separator_draw(AUX.DMN_DESCRIPTION);

        stdout.puts(AUX.DMN_NAME + AUX.COMMA_SPACE_SEP
           + AUX.DMN_VERSION_S__ + AUX.SPACE + AUX.DMN_VERSION + AUX.NEW_LINE
           + AUX.DMN_DESCRIPTION                               + AUX.NEW_LINE
           + AUX.DMN_COPYRIGHT__ + AUX.SPACE + AUX.DMN_AUTHOR  + AUX.NEW_LINE);

        aux.separator_draw(AUX.DMN_DESCRIPTION);
    }

    // Checking for args presence.
    if (argc == 0) {
        ret = Posix.EXIT_FAILURE;

        stderr.printf(AUX.ERR_MUST_BE_ONE_TWO_ARGS
                    + AUX.NEW_LINE + AUX.NEW_LINE, daemon_name, argc);

        stderr.printf(AUX.MSG_USAGE_TEMPLATE
                    + AUX.NEW_LINE + AUX.NEW_LINE, daemon_name);

        aux.cleanups_fixate();

        return ret;
    }

    // Checking for port correctness.
    if ((port_number < AUX.MIN_PORT) || (port_number > AUX.MAX_PORT)) {
        ret = Posix.EXIT_FAILURE;

        stderr.printf(AUX.ERR_PORT_MUST_BE_POSITIVE_INT
                    + AUX.NEW_LINE + AUX.NEW_LINE, daemon_name);

        stderr.printf(AUX.MSG_USAGE_TEMPLATE
                    + AUX.NEW_LINE + AUX.NEW_LINE, daemon_name);

        aux.cleanups_fixate();

        return ret;
    }

    // Instantiating the main daemon class.
    var dmn = new DnsResolvd();

    // Starting up the daemon.
    ret = dmn.startup(port_number);

    // Making final cleanups.
    aux.cleanups_fixate();

    return ret;
}

// vim:set nu et ts=4 sw=4:
