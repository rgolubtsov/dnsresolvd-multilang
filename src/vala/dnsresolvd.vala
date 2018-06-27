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

// The daemon entry point.
public static int main(string[] args) {
    int ret = Posix.EXIT_SUCCESS;
//  int ret = Posix.EXIT_FAILURE;

    int argc = args.length - 1;

    // Instantiating the daemon helper class.
//  var aux = new AUX();

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

    stdout.printf(AUX.S_FMT,  AUX.V_BAR + AUX.SPACE + argc.to_string()
                + AUX.SPACE + AUX.V_BAR + AUX.SPACE + daemon_name
                + AUX.SPACE + AUX.V_BAR + AUX.SPACE + port_number.to_string()
                + AUX.SPACE + AUX.V_BAR + AUX.SPACE + print_banner_opt
                + AUX.SPACE + AUX.V_BAR + AUX.NEW_LINE);

    return ret;
}

// vim:set nu et ts=4 sw=4:
