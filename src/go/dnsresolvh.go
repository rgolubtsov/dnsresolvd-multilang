/*
 * src/go/dnsresolvh.go
 * ============================================================================
 * DNS Resolver Daemon (dnsresolvd). Version 0.9.9
 * ============================================================================
 * A daemon that performs DNS lookups for the given hostname
 * passed in an HTTP request, with the focus on its implementation
 * using various programming languages. (net/http-boosted impl.)
 * ============================================================================
 * Copyright (C) 2017-2024 Radislav (Radicchio) Golubtsov
 *
 * (See the LICENSE file at the top of the source tree.)
 */

package main

import (
    "net/http"
    "log/syslog"
    "fmt"
)

// Helper constants.
const (
    _EXIT_FAILURE     int    =    1 //    Failing exit status.
    _EXIT_SUCCESS     int    =    0 // Successful exit status.
    _EMPTY_STRING     string =   ""
    _COLON_SPACE_SEP  string = ": "
    _COMMA_SPACE_SEP  string = ", "
    _NEW_LINE         string = "\n"
    _AMPER            string =  "&"
    _ONE_SPACE_STRING string =  " "
    _PRINT_BANNER_OPT string = "-V"
    _COLON            string =  ":"
)

// Common error messages.
const (
    _ERR_PREFIX                    string = "error"
    _ERR_PORT_MUST_BE_POSITIVE_INT string = ": <port_number> must be "   +
                                            "a positive integer value, " +
                                            "in the range 1024-49151."
    _ERR_CANNOT_START_SERVER       string = ": FATAL: Cannot start server "
    _ERR_SRV_UNKNOWN_REASON        string = "for an unknown reason. Exiting..."
    _ERR_SRV_PORT_IS_IN_USE        string = "due to the port requested " +
                                            "is in use. Exiting..."
    _ERR_COULD_NOT_LOOKUP          string = "could not lookup hostname"
    _ERR_ADDR_ALREADY_IN_USE       string = "address already in use"
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

// Common notification messages.
const _MSG_SERVER_STARTED_1 string = "Server started on port "
const _MSG_SERVER_STARTED_2 string = "=== Hit Ctrl+C to terminate it."

// HTTP request params.
const (
    _PRM_FMT_HTML string = "html"
    _PRM_FMT_JSON string = "json"
)

// HTTP response headers.
const (
    _HDR_CONTENT_TYPE_N      string = "Content-Type"
    _HDR_CONTENT_TYPE_V_HTML string = "text/html; charset=UTF-8"
    _HDR_CONTENT_TYPE_V_JSON string = "application/json"
    _HDR_CACHE_CONTROL_N     string = "Cache-Control"
    _HDR_CACHE_CONTROL_V     string = "no-cache, no-store, must-revalidate"
    _HDR_EXPIRES_N           string = "Expires"
    _HDR_EXPIRES_V           string = "Thu, 01 Dec 1994 16:00:00 GMT"
    _HDR_PRAGMA_N            string = "Pragma"
    _HDR_PRAGMA_V            string = "no-cache"
)

// Response data names.
const (
    _DAT_HOSTNAME_N string = "hostname"
    _DAT_ADDRESS_N  string = "address"
    _DAT_VERSION_N  string = "version"
    _DAT_VERSION_V  string = "IPv"
)

// Daemon name, version, and copyright banners.
const (
    _DMN_NAME        string = "DNS Resolver Daemon (dnsresolvd)"
    _DMN_DESCRIPTION string = "Performs DNS lookups for the given " +
                              "hostname passed in an HTTP request"
    _DMN_VERSION_S__ string = "Version"
    _DMN_VERSION     string = "0.9.9"
    _DMN_COPYRIGHT__ string = "Copyright (C) 2017-2024"
    _DMN_AUTHOR      string = "Radislav Golubtsov <radicchio@vk.com>"
)

// Constant: The default hostname to look up for.
const _DEF_HOSTNAME string = "openbsd.org"

/**
 * Adds headers to the response.
 *
 * @param headers The HTTP header object.
 * @param frt     The response format selector.
 */
func add_response_headers(headers http.Header, frt string) {
    var _HDR_CONTENT_TYPE_V string = _EMPTY_STRING

           if (frt == _PRM_FMT_HTML) {
        _HDR_CONTENT_TYPE_V = _HDR_CONTENT_TYPE_V_HTML
    } else if (frt == _PRM_FMT_JSON) {
        _HDR_CONTENT_TYPE_V = _HDR_CONTENT_TYPE_V_JSON
    }

    headers.Add(_HDR_CONTENT_TYPE_N,  _HDR_CONTENT_TYPE_V )
    headers.Add(_HDR_CACHE_CONTROL_N, _HDR_CACHE_CONTROL_V)
    headers.Add(_HDR_EXPIRES_N,       _HDR_EXPIRES_V      )
    headers.Add(_HDR_PRAGMA_N,        _HDR_PRAGMA_V       )
}

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
