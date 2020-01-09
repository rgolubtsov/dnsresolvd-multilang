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

import "os"
import "strconv"
import "strings"
import "fmt"
import "log/syslog"
import "path/filepath"

const _EXIT_FAILURE     int    =    1 //    Failing exit status.
const _EXIT_SUCCESS     int    =    0 // Successful exit status.
const _EMPTY_STRING     string =   ""
const _COMMA_SPACE_SEP  string = ", "
const _NEW_LINE         string = "\n"
const _ONE_SPACE_STRING string =  " "
const _PRINT_BANNER_OPT string = "-V"
const _ERR_PORT_MUST_BE_POSITIVE_INT string = ": <port_number> must be a positive integer value, in the range 1024-49151."
const _ERR_MUST_BE_ONE_TWO_ARGS_1 string = ": There must be one or two args passed: "
const _ERR_MUST_BE_ONE_TWO_ARGS_2 string = " args found"
const _MSG_USAGE_TEMPLATE_1 string = "Usage: "
const _MSG_USAGE_TEMPLATE_2 string = " <port_number> [-V]"
const _MIN_PORT uint = 1024
const _MAX_PORT uint = 49151
const _DMN_NAME        string = "DNS Resolver Daemon (dnsresolvd)"
const _DMN_DESCRIPTION string = "Performs DNS lookups for the given hostname passed in an HTTP request"
const _DMN_VERSION_S__ string = "Version"
const _DMN_VERSION     string = "0.1"
const _DMN_COPYRIGHT__ string = "Copyright (C) 2017-2020"
const _DMN_AUTHOR      string = "Radislav Golubtsov <ragolubtsov@my.com>"

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

    fmt.Println(port_number)

    if (print_banner_opt == _PRINT_BANNER_OPT) {
        _separator_draw(_DMN_DESCRIPTION)

        fmt.Printf(_DMN_NAME   + _COMMA_SPACE_SEP                             +
              _DMN_VERSION_S__ + _ONE_SPACE_STRING + _DMN_VERSION + _NEW_LINE +
              _DMN_DESCRIPTION +                                    _NEW_LINE +
              _DMN_COPYRIGHT__ + _ONE_SPACE_STRING + _DMN_AUTHOR  + _NEW_LINE)

        _separator_draw(_DMN_DESCRIPTION)
    }

    // Opening the system logger.
    log, e := syslog.Dial(_EMPTY_STRING,   _EMPTY_STRING,
                          syslog.LOG_ERR | syslog.LOG_DAEMON,
                          filepath.Base(daemon_name))

    if (e == nil) {}

    // Checking for args presence.
    if (argc == 0) {
        ret = _EXIT_FAILURE

        fmt.Fprintf(os.Stderr, daemon_name      +
                    _ERR_MUST_BE_ONE_TWO_ARGS_1 + strconv.Itoa(int(argc)) +
                    _ERR_MUST_BE_ONE_TWO_ARGS_2 + _NEW_LINE + _NEW_LINE)

        log.Err(               daemon_name      +
                    _ERR_MUST_BE_ONE_TWO_ARGS_1 + strconv.Itoa(int(argc)) +
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

    // Making final cleanups.
    _cleanups_fixate(log)

    os.Exit(ret)
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
