/*
 * src/java/DnsResolvd.java
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

import dnsresolvd.ControllerHelper;

/** The startup class of the daemon. */
public class DnsResolvd {
    /**
     * The daemon entry point.
     *
     * @param argv The array of command-line arguments.
     */
    public static void main(final String[] argv) {
        int ret = ControllerHelper.EXIT_SUCCESS;
//      int ret = ControllerHelper.EXIT_FAILURE;

        System.exit(ret);
    }
}

// vim:set nu et ts=4 sw=4:
