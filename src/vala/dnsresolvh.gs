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
    const EMPTY_STRING : string =   ""
    const NEW_LINE     : string = "\n"
    const S_FMT        : string = "%s"
    const V_BAR        : string =  "|"
    const SPACE        : string =  " "

    /** Default constructor. */
    construct()
        pass

// vim:set nu et ts=4 sw=4:
