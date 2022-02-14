/*
 * src/go/dnsresolvd.go
 * ============================================================================
 * DNS Resolver Daemon (dnsresolvd). Version 0.9.9
 * ============================================================================
 * A daemon that performs DNS lookups for the given hostname
 * passed in an HTTP request, with the focus on its implementation
 * using various programming languages. (net/http-boosted impl.)
 * ============================================================================
 * Copyright (C) 2017-2022 Radislav (Radicchio) Golubtsov
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
    "io/ioutil"
    "encoding/json"
    "net"
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

    // Defining the default request handler.
    _request_handler := func(resp http.ResponseWriter, req *http.Request) {
        var mtd string = req.Method

        var params []string

        type json_object_s struct {
            Hostname string `json:"hostname"`
            Address  string `json:"address"`
            Version  string `json:"version"`
        }

        type json_object_e struct {
            Hostname string `json:"hostname"`
            Error    string `json:"error"`
        }

        var jobj []byte
        var resp_buffer string = _EMPTY_STRING

               if (mtd == http.MethodGet ) {
            params = strings.Split(req.URL.RawQuery, _AMPER)
        } else if (mtd == http.MethodPost) {
            req_body, _ := ioutil.ReadAll(req.Body)
            params = strings.Split(string(req_body), _AMPER)
        }

        // Parsing and validating request params.
        hostname, frt := _parse_and_validate(params)

        // Performing DNS lookup for the given hostname.
        addr, ver := dns_lookup(hostname)

        ver_str := strconv.Itoa(int(ver))

        if (frt == _PRM_FMT_HTML) {
            resp_buffer = "<!DOCTYPE html>"                                                   + _NEW_LINE +
"<html lang=\"en-US\" dir=\"ltr\">"                                                       + _NEW_LINE +
"<head>"                                                                                  + _NEW_LINE +
"<meta http-equiv=\"" + _HDR_CONTENT_TYPE_N      +                     "\"    content=\"" +
                        _HDR_CONTENT_TYPE_V_HTML +                     "\"           />"  + _NEW_LINE +
"<meta http-equiv=\"X-UA-Compatible\" content=\"IE=edge\"                            />"  + _NEW_LINE +
"<meta       name=\"viewport\"        content=\"width=device-width,initial-scale=1\" />"  + _NEW_LINE +
"<title>" + _DMN_NAME + "</title>"                                                        + _NEW_LINE +
"</head>"                                                                                 + _NEW_LINE +
"<body>"                                                                                  + _NEW_LINE +
"<div>"   +  hostname + _ONE_SPACE_STRING
        }

        // If lookup error occurred.
        if (addr == _ERR_PREFIX) {
                   if (frt == _PRM_FMT_HTML) {
                resp_buffer += _ERR_PREFIX       +
                               _COLON_SPACE_SEP  +
                               _ERR_COULD_NOT_LOOKUP
            } else if (frt == _PRM_FMT_JSON) {
                jobj, _ = json.Marshal(json_object_e {
                    hostname,
                    _ERR_COULD_NOT_LOOKUP,
                })
            }
        } else {
                   if (frt == _PRM_FMT_HTML) {
                resp_buffer += addr              +
                               _ONE_SPACE_STRING +
                               _DAT_VERSION_V    +
                               ver_str
            } else if (frt == _PRM_FMT_JSON) {
                jobj, _ = json.Marshal(json_object_s {
                    hostname,
                    addr,
                    _DAT_VERSION_V + ver_str,
                })
            }
        }

               if (frt == _PRM_FMT_HTML) {
            resp_buffer += "</div>"  + _NEW_LINE +
                           "</body>" + _NEW_LINE +
                           "</html>" + _NEW_LINE
        } else if (frt == _PRM_FMT_JSON) {
            resp_buffer  = string(jobj)
        }

        // Adding headers to the response.
        add_response_headers(resp.Header(), frt)

        fmt.Fprintf(resp, resp_buffer)
    }

    /*
     * Attaching HTTP request handlers to process incoming requests
     * and producing the response.
     */
    http.HandleFunc("/", _request_handler)

    // Starting up the HTTP listener on <port_number>.
    e := http.ListenAndServe(_COLON + port_number_str, nil)

    // Handling errors during start up of the listener.
    if (e != nil) {
        ret = _EXIT_FAILURE

        if (strings.Contains(e.Error(), _ERR_ADDR_ALREADY_IN_USE)) {
            fmt.Fprintf(os.Stderr, daemon_name   +
                        _ERR_CANNOT_START_SERVER +
                        _ERR_SRV_PORT_IS_IN_USE  + _NEW_LINE + _NEW_LINE)

            log.Err(               daemon_name   +
                        _ERR_CANNOT_START_SERVER +
                        _ERR_SRV_PORT_IS_IN_USE  + _NEW_LINE)
        } else {
            fmt.Fprintf(os.Stderr, daemon_name   +
                        _ERR_CANNOT_START_SERVER +
                        _ERR_SRV_UNKNOWN_REASON  + _NEW_LINE + _NEW_LINE)

            log.Err(               daemon_name   +
                        _ERR_CANNOT_START_SERVER +
                        _ERR_SRV_UNKNOWN_REASON  + _NEW_LINE)
        }

        _cleanups_fixate(log)

        os.Exit(ret)
    }

    // Making final cleanups.
    _cleanups_fixate(log)

    os.Exit(ret)
}

// Helper function. Parses and validates request params.
func _parse_and_validate(params []string) (string, string) {
    var hostname, frt string

    const _h string = "h="
    const _f string = "f="

    // ------------------------------------------------------------------------
    // --- Parsing and validating request params - Begin ----------------------
    // ------------------------------------------------------------------------
    for i := 0; i < len(params); i++ {
               if (strings.HasPrefix(     params[i], _h)) {
            hostname = strings.TrimPrefix(params[i], _h) // <-----------+
        } else if (strings.HasPrefix(     params[i], _f)) { //          |
            frt      = strings.TrimPrefix(params[i], _f) // <-------+   |
        }                                                   //      |   |
    }   /*                                  +-----------------------+---+
                                            |                       |   |
                                            |            +----------+   |
                                            |            |          |   |
    $ curl 'http://localhost:<port-number>/?h=<hostname>&f=<fmt>'   |   |
    $ curl -d 'h=<hostname>&f=<fmt>' http://localhost:<port_number> |   |
               |            |                                       |   |
               |            +---------------------------------------+   |
               |                                                        |
               +--------------------------------------------------------+ */

    if (hostname == _EMPTY_STRING) {
        hostname  = _DEF_HOSTNAME
    }

    if (frt      == _EMPTY_STRING) {
        frt       = _PRM_FMT_JSON
    } else {
        frt       = strings.ToLower(frt)

        frt_ := []string {
            _PRM_FMT_HTML,
            _PRM_FMT_JSON,
        }

        var _frt bool = false

        for i := 0; i < len(frt_); i++ {
            if (frt == frt_[i]) {
               _frt = true; break
            }
        }

        if (!_frt) {
            frt   = _PRM_FMT_JSON
        }
    }
    // ------------------------------------------------------------------------
    // --- Parsing and validating request params - End ------------------------
    // ------------------------------------------------------------------------

    return hostname, frt
}

/**
 * Performs DNS lookup action for the given hostname,
 * i.e. (in this case) IP address retrieval by hostname.
 *
 * @param hostname The effective hostname to look up for.
 *
 * @return The IP address of the analyzing host/service
 *         and the corresponding IP version (family)
 *         used to look up in DNS:
 *         <code>4</code> for IPv4-only hosts,
 *         <code>6</code> for IPv6-capable hosts.
 */
func dns_lookup(hostname string) (string, uint) {
    var addr string
    var ver  uint

    addrs, e := net.LookupHost(hostname)

    if ((e == nil) || (len(addrs) > 0)) {
        addr = addrs[0]

        if (strings.Contains(addr, _COLON)) {
            ver = 6
        } else {
            ver = 4
        }
    } else {
        addr = _ERR_PREFIX
        ver  = 0
    }

    return addr, ver
}

// vim:set nu et ts=4 sw=4:
