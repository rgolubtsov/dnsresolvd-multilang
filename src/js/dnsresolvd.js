#!/usr/bin/env node
/* content/dev/misc/dnsresolvd/js/dnsresolvd.js
 * ============================================================================
 * DNS Resolver Daemon (dnsresolvd). Version 0.1
 * ============================================================================
 * A Node.js-boosted daemon for performing DNS lookups.
 * ============================================================================
 * Copyright (C) 2017 Radislav (Radicchio) Golubtsov
 *
 * (See the LICENSE file at the top of the source tree.)
 */

"use strict";

var path = require("path");
var http = require("http");
var url  = require("url");
var dns  = require("dns");

var posix = require("posix");

var __DNSRESOLVD_H = require("./dnsresolvd.h");

var aux = new __DNSRESOLVD_H();

/**
 * Performs DNS lookup action for the given hostname,
 * i.e. (in this case) IP address retrieval by hostname.
 *
 * It first prepares and runs the server instance, then does all the rest.
 *
 * @param _ret        The status code passed in from the caller.
 * @param port_number The server port number to listen on.
 * @param daemon_name The daemon name (executable/script name).
 *
 * @return The status code indicating the daemon overall execution outcome.
 */
var dns_lookup = function(_ret, port_number, daemon_name) {
    var ret = _ret;

    /*
     * Creating, configuring, and starting the server.
     *
     * @param req  The HTTP request object.
     * @param resp The HTTP response object.
     *
     * @return The HTTP server object.
     */
    var daemon = http.createServer(function(req, resp) {
        // Parsing and validating query params.
        var query = url.parse(req.url, true).query;

        // http://localhost:<port_number>/?h=<hostname>
        //                                 |
        var hostname = query.h; // <-------+

        if (!hostname) {
            hostname = aux._DEF_HOSTNAME;
        }

        /*
         * Performing DNS lookup for the given hostname
         * and writing the response out.
         *
         * @param hostname The effective hostname to look up for.
         * @param e        The Error object (if any error occurs).
         * @param addr     The IP address retrieved.
         * @param ver      The IP version (family) used to look up in DNS.
         */
        dns.lookup(hostname, function(e, addr, ver) {
            var resp_buffer = "<!DOCTYPE html>"                                                                              + aux._NEW_LINE
+ "<html lang=\"en-US\" dir=\"ltr\">" + aux._NEW_LINE + "<head>"                                                             + aux._NEW_LINE
+ "<meta http-equiv=\"Content-Type\"    content=\"" + aux._HDR_CONTENT_TYPE + "\" />"                                        + aux._NEW_LINE
+ "<meta http-equiv=\"X-UA-Compatible\" content=\"IE=edge,chrome=1\" />"                                                     + aux._NEW_LINE
+ "<!-- No caching at all for:                                                                       -->"                    + aux._NEW_LINE
+ "<meta http-equiv=\"Cache-Control\"   content=\"" + aux._HDR_CACHE_CONTROL + "\" /> <!-- HTTP/1.1 -->"                     + aux._NEW_LINE
+ "<meta http-equiv=\"Expires\"         content=\"" + aux._HDR_EXPIRES + "\"       /> <!-- Proxies  -->"                     + aux._NEW_LINE
+ "<meta http-equiv=\"Pragma\"          content=\"" + aux._HDR_PRAGMA + "\"                            /> <!-- HTTP/1.0 -->" + aux._NEW_LINE
+ "<meta       name=\"viewport\"        content=\"width=device-width,initial-scale=1\" />"                                   + aux._NEW_LINE
+ "<meta       name=\"description\"     content=\"" + aux._DMN_DESCRIPTION + "\" />"                                         + aux._NEW_LINE
+ "<title>" + aux._DMN_NAME + "</title>" + aux._NEW_LINE + "</head>"                                                         + aux._NEW_LINE
+ "<body id=\"dnsresolvd\">"             + aux._NEW_LINE + "<p>"
+ hostname + " ==&gt; ";

            if (e) {
                resp_buffer += aux._ERR_PREFIX
                            +  aux._COLON_SPACE_SEP
                            +  aux._ERR_COULD_NOT_LOOKUP;
            } else {
                resp_buffer += addr + " (IPv" + ver + ")";
            }

            resp_buffer += "</p>"    + aux._NEW_LINE
                        +  "</body>" + aux._NEW_LINE
                        +  "</html>" + aux._NEW_LINE;

            // Adding headers to the response.
            resp.writeHead(aux._RSC_HTTP_200_OK, {
                "Content-Type"  : aux._HDR_CONTENT_TYPE,
                "Cache-Control" : aux._HDR_CACHE_CONTROL,
                "Expires"       : aux._HDR_EXPIRES,
                "Pragma"        : aux._HDR_PRAGMA
            });

            // Writing the response out.
            resp.write(resp_buffer);

            // Closing the response stream.
            resp.end();
        });
    }).listen(port_number);

    daemon.on(aux._EVE_ERROR, function(e) {
        ret = aux._EXIT_FAILURE;

        if (e.code === aux._ERR_EADDRINUSE) {
            console.error(daemon_name + aux._ERR_CANNOT_START_SERVER
                                      + aux._ERR_SRV_PORT_IS_IN_USE
                                      + aux._NEW_LINE);

            posix.syslog(aux._LOG_PRIORITY_ERR,
                          daemon_name + aux._ERR_CANNOT_START_SERVER
                                      + aux._ERR_SRV_PORT_IS_IN_USE
                                      + aux._NEW_LINE);
        } else {
            console.error(daemon_name + aux._ERR_CANNOT_START_SERVER
                                      + aux._ERR_SRV_UNKNOWN_REASON
                                      + aux._NEW_LINE);

            posix.syslog(aux._LOG_PRIORITY_ERR,
                          daemon_name + aux._ERR_CANNOT_START_SERVER
                                      + aux._ERR_SRV_UNKNOWN_REASON
                                      + aux._NEW_LINE);
        }

        _cleanups_fixate();

        return ret;
    });

    daemon.on(aux._EVE_LISTENING, function(e) {
        console.log(aux._MSG_SERVER_STARTED_1 + port_number + aux._NEW_LINE
                  + aux._MSG_SERVER_STARTED_2);

        posix.syslog(aux._LOG_PRIORITY_INFO, aux._MSG_SERVER_STARTED_1
             + port_number + aux._NEW_LINE + aux._MSG_SERVER_STARTED_2);
    });

    return ret;
};

/* Helper function. Makes final buffer cleanups, closes streams, etc. */
var _cleanups_fixate = function() {
    // Closing the system logger.
    posix.closelog();
}

/* Helper function. Draws a horizontal separator banner. */
var _separator_draw = function(banner_text) {
    var i = banner_text.length;

    do { process.stdout.write('='); i--; } while (i); console.log();
};

/** The daemon entry point. */
var main = function(argc, argv) {
    var ret = aux._EXIT_SUCCESS;

    var daemon_name = path.basename(argv[1]);
    var port_number = parseInt(argv[2], 10);

    // Opening the system logger.
    posix.openlog(path.basename(daemon_name, aux._LOG_DAEMON_EXT),
                  {cons : true, pid : true}, aux._LOG_FACILITY_DAEMON);

    _separator_draw(aux._DMN_DESCRIPTION);

    console.log(aux._DMN_NAME + aux._COMMA_SPACE_SEP  + aux._DMN_VERSION_S__
      + aux._ONE_SPACE_STRING + aux._DMN_VERSION      + aux._NEW_LINE
      + aux._DMN_DESCRIPTION                          + aux._NEW_LINE
      + aux._DMN_COPYRIGHT__  + aux._ONE_SPACE_STRING + aux._DMN_AUTHOR);

    _separator_draw(aux._DMN_DESCRIPTION);

    // Checking for args presence.
    if (argc !== 3) {
        ret = aux._EXIT_FAILURE;

        console.error(daemon_name + aux._ERR_MUST_BE_THE_ONLY_ARG_1
                     + (argc - 2) + aux._ERR_MUST_BE_THE_ONLY_ARG_2
                     + aux._NEW_LINE);

        posix.syslog(aux._LOG_PRIORITY_ERR,
                      daemon_name + aux._ERR_MUST_BE_THE_ONLY_ARG_1
                     + (argc - 2) + aux._ERR_MUST_BE_THE_ONLY_ARG_2
                     + aux._NEW_LINE);

        console.error(aux._MSG_USAGE_TEMPLATE_1 + daemon_name
                    + aux._MSG_USAGE_TEMPLATE_2 + aux._NEW_LINE);

        _cleanups_fixate();

        return ret;
    }

    // Checking for port correctness.
    if (isNaN(port_number) || (port_number < aux._MIN_PORT)
                           || (port_number > aux._MAX_PORT)) {

        ret = aux._EXIT_FAILURE;

        console.error(daemon_name + aux._ERR_PORT_MUST_BE_POSITIVE_INT
                    + aux._NEW_LINE);

        posix.syslog(aux._LOG_PRIORITY_ERR,
                      daemon_name + aux._ERR_PORT_MUST_BE_POSITIVE_INT
                    + aux._NEW_LINE);

        console.error(aux._MSG_USAGE_TEMPLATE_1 + daemon_name
                    + aux._MSG_USAGE_TEMPLATE_2 + aux._NEW_LINE);

        _cleanups_fixate();

        return ret;
    }

    /*
     * Preparing and running the server instance,
     * then making DNS lookup upon request
     * for the hostname provided.
     */
    ret = dns_lookup(ret, port_number, daemon_name);

    // Making final cleanups. */
    _cleanups_fixate();

    return ret;
};

var argv = process.argv;
var argc = argv.length;

// Starting up the daemon.
var ret = main(argc, argv);

process.exitCode = ret;

// vim:set nu:et:ts=4:sw=4:
