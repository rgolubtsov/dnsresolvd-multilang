/*
 * src/go/dnsresolvh.go
 * ============================================================================
 * DNS Resolver Daemon (dnsresolvd). Version 0.1
 * ============================================================================
 * A daemon that performs DNS lookups for the given hostname
 * passed in an HTTP request, with the focus on its implementation
 * using various programming languages. (net/http-boosted impl.)
 * ============================================================================
 * Copyright (C) 2017-2020 Radislav (Radicchio) Golubtsov
 *
 * (See the LICENSE file at the top of the source tree.)
 */

package main

import (
    "fmt"
    "log/syslog"
)

// Helper constants.
const (
    _EXIT_FAILURE     int    =    1 //    Failing exit status.
    _EXIT_SUCCESS     int    =    0 // Successful exit status.
    _EMPTY_STRING     string =   ""
    _COMMA_SPACE_SEP  string = ", "
    _NEW_LINE         string = "\n"
    _ONE_SPACE_STRING string =  " "
    _PRINT_BANNER_OPT string = "-V"
)

// Common error messages.
const (
    _ERR_PORT_MUST_BE_POSITIVE_INT string = ": <port_number> must be "   +
                                            "a positive integer value, " +
                                            "in the range 1024-49151."
)

// Print this error message when there are no any args passed.
const (
    _ERR_MUST_BE_ONE_TWO_ARGS_1 string = ": There must be one or two args " +
                                         "passed: "
    _ERR_MUST_BE_ONE_TWO_ARGS_2 string = " args found"
)

// Print this usage info just after any inappropriate input.
const (
    _MSG_USAGE_TEMPLATE_1 string = "Usage: "
    _MSG_USAGE_TEMPLATE_2 string = " <port_number> [-V]"
)

// Constant: The minimum port number allowed.
const _MIN_PORT uint = 1024

// Constant: The maximum port number allowed.
const _MAX_PORT uint = 49151

// Daemon name, version, and copyright banners.
const (
    _DMN_NAME        string = "DNS Resolver Daemon (dnsresolvd)"
    _DMN_DESCRIPTION string = "Performs DNS lookups for the given " +
                              "hostname passed in an HTTP request"
    _DMN_VERSION_S__ string = "Version"
    _DMN_VERSION     string = "0.1"
    _DMN_COPYRIGHT__ string = "Copyright (C) 2017-2020"
    _DMN_AUTHOR      string = "Radislav Golubtsov <ragolubtsov@my.com>"
)

// Helper function. Makes final buffer cleanups, closes streams, etc.
func _cleanups_fixate(log *syslog.Writer) {
    // Closing the system logger.
    log.Close()
}

// Helper function. Draws a horizontal separator banner.
func _separator_draw(banner_text string) {
    i := len(banner_text)

    for { fmt.Printf("%c", '='); i--; if (i == 0) { break } }; fmt.Println()
}

// vim:set nu et ts=4 sw=4:
