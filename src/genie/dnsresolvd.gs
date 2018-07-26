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
