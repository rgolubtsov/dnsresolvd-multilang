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

    int argc = args.length;

    var    daemon_name = args[0];
    uint64 port_number;

    string print_banner_opt = "";

    if (argc > 1) {
        port_number = uint64.parse(args[1]);

        if (argc > 2) {
            print_banner_opt = args[2];
        }
    } else {
        port_number = 0;
    }

    stdout.printf("%s", "| " + argc.to_string()
                     + " | " + daemon_name
                     + " | " + port_number.to_string()
                     + " | " + print_banner_opt
                     + " |"  + "\n");

    return ret;
}

// vim:set nu et ts=4 sw=4:
