/*
 * src/go/dnsresolvd.go
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
    "os"
    "strconv"
    "strings"
    "fmt"
    "log/syslog"
    "path/filepath"
    "net/http"
)

// The daemon entry point.
func main() {
    var ret int = _EXIT_SUCCESS

    var argc uint = uint(len(os.Args) - 1)

        daemon_name := os.Args[0]
    var port_number uint

    var print_banner_opt string = _EMPTY_STRING

    if (argc > 0) {
        port_number_, e := strconv.Atoi(os.Args[1])

        if (e == nil) { port_number = uint(port_number_) }

        if (argc > 1) {
            print_banner_opt = strings.ToUpper(os.Args[2])
        }
    } else {
        port_number = 0
    }

    if (print_banner_opt == _PRINT_BANNER_OPT) {
        _separator_draw(_DMN_DESCRIPTION)

        fmt.Printf(_DMN_NAME   + _COMMA_SPACE_SEP                             +
              _DMN_VERSION_S__ + _ONE_SPACE_STRING + _DMN_VERSION + _NEW_LINE +
              _DMN_DESCRIPTION +                                    _NEW_LINE +
              _DMN_COPYRIGHT__ + _ONE_SPACE_STRING + _DMN_AUTHOR  + _NEW_LINE)

        _separator_draw(_DMN_DESCRIPTION)
    }

    // Opening the system logger.
    log, _ := syslog.Dial(_EMPTY_STRING,   _EMPTY_STRING,
                          syslog.LOG_ERR | syslog.LOG_DAEMON,
                          filepath.Base(daemon_name))

    // Checking for args presence.
    if (argc == 0) {
        ret = _EXIT_FAILURE

        var argc_str string = strconv.Itoa(int(argc))

        fmt.Fprintf(os.Stderr, daemon_name      +
                    _ERR_MUST_BE_ONE_TWO_ARGS_1 + argc_str  +
                    _ERR_MUST_BE_ONE_TWO_ARGS_2 + _NEW_LINE + _NEW_LINE)

        log.Err(               daemon_name      +
                    _ERR_MUST_BE_ONE_TWO_ARGS_1 + argc_str  +
                    _ERR_MUST_BE_ONE_TWO_ARGS_2 + _NEW_LINE)

        fmt.Fprintf(os.Stderr, _MSG_USAGE_TEMPLATE_1 + daemon_name +
                               _MSG_USAGE_TEMPLATE_2 + _NEW_LINE   + _NEW_LINE)

        _cleanups_fixate(log)

        os.Exit(ret)
    }

    // Checking for port correctness.
    if ((port_number < _MIN_PORT) || (port_number > _MAX_PORT)) {
        ret = _EXIT_FAILURE

        fmt.Fprintf(os.Stderr, daemon_name         +
                    _ERR_PORT_MUST_BE_POSITIVE_INT + _NEW_LINE + _NEW_LINE)

        log.Err(               daemon_name         +
                    _ERR_PORT_MUST_BE_POSITIVE_INT + _NEW_LINE)

        fmt.Fprintf(os.Stderr, _MSG_USAGE_TEMPLATE_1 + daemon_name +
                               _MSG_USAGE_TEMPLATE_2 + _NEW_LINE   + _NEW_LINE)

        _cleanups_fixate(log)

        os.Exit(ret)
    }

    var port_number_str string = strconv.Itoa(int(port_number))

    fmt.Printf(_MSG_SERVER_STARTED_1 + port_number_str + _NEW_LINE +
               _MSG_SERVER_STARTED_2 +                   _NEW_LINE)

    log.Info(  _MSG_SERVER_STARTED_1 + port_number_str + _NEW_LINE +
               _MSG_SERVER_STARTED_2)

    // Creating the HTTP server.
    e := http.ListenAndServe(_COLON + port_number_str, nil)

    if (e != nil) {
        fmt.Println(e)
    }

    // Making final cleanups.
    _cleanups_fixate(log)

    os.Exit(ret)
}

// vim:set nu et ts=4 sw=4:
