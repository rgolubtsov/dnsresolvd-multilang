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

/** The main class of the daemon. */
class DnsResolvd : Soup.Server
    /** Default constructor. */
    construct()
        pass

// The daemon entry point.
init//(string[] args)
    ret : int = Posix.EXIT_SUCCESS

    argc : int = args.length - 1

    // Instantiating the daemon helper class.
    var aux = new AUX()

    var daemon_name = args[0]
    port_number     : uint

    print_banner_opt : string = AUX.EMPTY_STRING

    if (argc > 0)
        port_number = int.parse(args[1])

        if (argc > 1)
            print_banner_opt = args[2]
    else
        port_number = 0

    if (print_banner_opt == AUX.PRINT_BANNER_OPT)
        aux.separator_draw(AUX.DMN_DESCRIPTION)

        stdout.puts(AUX.DMN_NAME + AUX.COMMA_SPACE_SEP
           + AUX.DMN_VERSION_S__ + AUX.SPACE + AUX.DMN_VERSION + AUX.NEW_LINE
           + AUX.DMN_DESCRIPTION                               + AUX.NEW_LINE
           + AUX.DMN_COPYRIGHT__ + AUX.SPACE + AUX.DMN_AUTHOR  + AUX.NEW_LINE)

        aux.separator_draw(AUX.DMN_DESCRIPTION)

    // Opening the system logger.
    Posix.openlog((string) null,
                  (Posix.LOG_CONS | Posix.LOG_PID), Posix.LOG_DAEMON)

    // Checking for args presence.
    if (argc == 0)
        ret = Posix.EXIT_FAILURE

        stderr.printf(AUX.ERR_MUST_BE_ONE_TWO_ARGS
                    + AUX.NEW_LINE + AUX.NEW_LINE, daemon_name, argc)

        Posix.syslog(Posix.LOG_ERR,
                      AUX.ERR_MUST_BE_ONE_TWO_ARGS
                    + AUX.NEW_LINE + AUX.NEW_LINE, daemon_name, argc)

        stderr.printf(AUX.MSG_USAGE_TEMPLATE
                    + AUX.NEW_LINE + AUX.NEW_LINE, daemon_name)

        aux.cleanups_fixate()

        Posix.exit(ret)

    // Checking for port correctness.
    if ((port_number < AUX.MIN_PORT) || (port_number > AUX.MAX_PORT))
        ret = Posix.EXIT_FAILURE

        stderr.printf(AUX.ERR_PORT_MUST_BE_POSITIVE_INT
                    + AUX.NEW_LINE + AUX.NEW_LINE, daemon_name)

        Posix.syslog(Posix.LOG_ERR,
                      AUX.ERR_PORT_MUST_BE_POSITIVE_INT
                    + AUX.NEW_LINE + AUX.NEW_LINE, daemon_name)

        stderr.printf(AUX.MSG_USAGE_TEMPLATE
                    + AUX.NEW_LINE + AUX.NEW_LINE, daemon_name)

        aux.cleanups_fixate()

        Posix.exit(ret)

    // Instantiating the main daemon class.
    var dmn = new DnsResolvd()

    // Creating the main loop instance.
    var loop = new MainLoop()

    if ((dmn == null) || (loop == null))
        ret = Posix.EXIT_FAILURE

        stderr.printf(AUX.ERR_CANNOT_START_SERVER
                    + AUX.ERR_SRV_UNKNOWN_REASON
                    + AUX.NEW_LINE + AUX.NEW_LINE, daemon_name)

        Posix.syslog(Posix.LOG_ERR,
                      AUX.ERR_CANNOT_START_SERVER
                    + AUX.ERR_SRV_UNKNOWN_REASON
                    + AUX.NEW_LINE + AUX.NEW_LINE, daemon_name)

        aux.cleanups_fixate()

        Posix.exit(ret)

    // ------------------------------------------------------------------------
    // --- Attaching Unix signal handlers to ensure daemon clean shutdown -----
    // --- Begin --------------------------------------------------------------
    SIGINT_callback  : SourceFunc = def() // <== SIGINT  handler.
        aux.cleanups_fixate(loop)

        return new Unix.SignalSource(Posix.SIGINT ).REMOVE

    SIGTERM_callback : SourceFunc = def() // <== SIGTERM handler.
        aux.cleanups_fixate(loop)

        return new Unix.SignalSource(Posix.SIGTERM).REMOVE

    Unix.signal_add(Posix.SIGINT,  (owned) SIGINT_callback )
    Unix.signal_add(Posix.SIGTERM, (owned) SIGTERM_callback)
    // ------------------------------------------------------------------------
    // --- Attaching Unix signal handlers to ensure daemon clean shutdown -----
    // --- End ----------------------------------------------------------------

    /*
     * Attaching HTTP request handlers to process incoming requests
     * and producing the response. ---+------+------+------+------+
     *                                |      |      |      |      |
     *                                v      v      v      v      v
     *
     * Default request handler.
     *
     * @param _dmn The daemon instance running.
     * @param  msg The HTTP message being processed.
     * @param  pth The path  component of the <code>msg</code> request URI.
     * @param  qry The query component of the <code>msg</code> request URI.
     */
    DEF_REQ_HNDLR_callback : Soup.ServerCallback = def(_dmn, msg, pth, qry)
        var resp_buffer = AUX.EMPTY_STRING

        var mtd      = msg.method
        var req_body = msg.request_body

        var hostname = (string) null // The effective hostname to look up for.
        var fmt      = (string) null // The response format selector.

        // --------------------------------------------------------------------
        // --- Parsing and validating request params - Begin ------------------
        // --------------------------------------------------------------------
        if      (mtd == AUX.MTD_HTTP_GET )
            if (qry      != null)
                hostname  = qry.get("h") // <------+
                //                                 |
                // http://localhost:<port_number>/?h=<hostname>&f=<fmt>
                //                                              |
                fmt       = qry.get("f") // <-------------------+
        else if (mtd == AUX.MTD_HTTP_POST)
            if((req_body != null) && (req_body.length > 0))
                var req_body_data = AUX.EMPTY_STRING

                for i : uint = 0 to (req_body.length - 1)
                    req_body_data += req_body.data[i].to_string(AUX.C_FMT)

                var qry_ary = req_body_data.split(AUX.AMPER)

            // $ curl -d 'h=<hostname>&f=<fmt>' http://localhost:<port_number>
            //            |            |
            //            |            +----------------------------+
            //            +---------------------------------------+ |
            //                                                    | |

                for i : uint = 0 to (qry_ary.length - 1)   //     | |
                    if      (qry_ary[i].has_prefix("h="))  // <---+ |
                        hostname = qry_ary[i].substring(2) //       |
                    else if (qry_ary[i].has_prefix("f="))  // <-----+
                        fmt      = qry_ary[i].substring(2)

        if((hostname == null) || (hostname.length == 0))
            hostname  = AUX.DEF_HOSTNAME

        if((fmt      == null) || (fmt.length      == 0))
            fmt       = AUX.PRM_FMT_JSON
        else
            fmt       = fmt.down()

            fmt_ : array of string = { AUX.PRM_FMT_HTML, AUX.PRM_FMT_JSON }

            _fmt : bool = false

            for i : uint = 0 to (fmt_.length - 1)
                if (fmt == fmt_[i])
                   _fmt  = true; break

            if (!_fmt)
                fmt   = AUX.PRM_FMT_JSON
        // --------------------------------------------------------------------
        // --- Parsing and validating request params - End --------------------
        // --------------------------------------------------------------------

        // Adding headers to the response.
        var HDR_CONTENT_TYPE_V = aux.add_response_headers(msg.response_headers,
                                 fmt)

        msg.set_status(Soup.Status.OK);msg.set_response(HDR_CONTENT_TYPE_V,
                       Soup.MemoryUse.COPY,    resp_buffer.data)

    /*
     * The first argument in this call is the URI path for serving requests.
     * Values <code>null</code> or &quot;/&quot; mean that this will be
     * the default handler for all requests.
     * The second argument is the default handler callback itself.
     */
    dmn.add_handler(null, (owned) DEF_REQ_HNDLR_callback)

    // Trying to start up the daemon.
    try
        // Setting up the daemon to listen on all TCP IPv4 interfaces.
        if (dmn.listen_all(port_number, Soup.ServerListenOptions.IPV4_ONLY))
            stdout.printf(AUX.MSG_SERVER_STARTED_1 + AUX.NEW_LINE
                        + AUX.MSG_SERVER_STARTED_2 + AUX.NEW_LINE,port_number)

            Posix.syslog(Posix.LOG_INFO,
                          AUX.MSG_SERVER_STARTED_1 + AUX.NEW_LINE
                        + AUX.MSG_SERVER_STARTED_2 + AUX.NEW_LINE,port_number)

            // Starting up the daemon by running the main loop.
            loop.run()
    except e : Error
        ret = Posix.EXIT_FAILURE

        try
            var re = new Regex(AUX.ERR_ADDR_ALREADY_IN_USE)

            if (re.match(e.message))
                stderr.printf(AUX.ERR_CANNOT_START_SERVER
                            + AUX.ERR_SRV_PORT_IS_IN_USE
                            + AUX.NEW_LINE + AUX.NEW_LINE, daemon_name)

                Posix.syslog(Posix.LOG_ERR,
                              AUX.ERR_CANNOT_START_SERVER
                            + AUX.ERR_SRV_PORT_IS_IN_USE
                            + AUX.NEW_LINE + AUX.NEW_LINE, daemon_name)
            else
                stderr.printf(AUX.ERR_CANNOT_START_SERVER
                            + AUX.ERR_SRV_UNKNOWN_REASON
                            + AUX.NEW_LINE + AUX.NEW_LINE, daemon_name)

                Posix.syslog(Posix.LOG_ERR,
                              AUX.ERR_CANNOT_START_SERVER
                            + AUX.ERR_SRV_UNKNOWN_REASON
                            + AUX.NEW_LINE + AUX.NEW_LINE, daemon_name)
        except e : Error
            stderr.printf(AUX.ERR_CANNOT_START_SERVER
                        + AUX.ERR_SRV_UNKNOWN_REASON
                        + AUX.NEW_LINE + AUX.NEW_LINE, daemon_name)

            Posix.syslog(Posix.LOG_ERR,
                          AUX.ERR_CANNOT_START_SERVER
                        + AUX.ERR_SRV_UNKNOWN_REASON
                        + AUX.NEW_LINE + AUX.NEW_LINE, daemon_name)

        aux.cleanups_fixate(loop)

        Posix.exit(ret)

    // Making final cleanups.
    aux.cleanups_fixate(loop)

    Posix.exit(ret)

// vim:set nu et ts=4 sw=4:
