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
    public static final int EXIT_FAILURE = 1; //    Failing exit status.
    public static final int EXIT_SUCCESS = 0; // Successful exit status.
}

// vim:set nu et ts=4 sw=4:
