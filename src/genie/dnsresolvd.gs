[indent=4]
/* src/genie/dnsresolvd.gs
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
init//(string[] args)
    ret : int = Posix.EXIT_SUCCESS
//  ret : int = Posix.EXIT_FAILURE

    Posix.exit(ret)

// vim:set nu et ts=4 sw=4:
