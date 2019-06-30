/*
 * src/java/DnsResolvd.java
 * ============================================================================
 * DNS Resolver Daemon (dnsresolvd). Version 0.1
 * ============================================================================
 * A daemon that performs DNS lookups for the given hostname
 * passed in an HTTP request, with the focus on its implementation
 * using various programming languages. (Vertosphere-boosted impl.)
 * ============================================================================
 * Copyright (C) 2017-2019 Radislav (Radicchio) Golubtsov
 *
 * (See the LICENSE file at the top of the source tree.)
 */

import static dns_resolv.ControllerHelper.*;
import        dns_resolv.DnsLookupController;

import org.graylog2.syslog4j.impl.unix.UnixSyslog;
import org.graylog2.syslog4j.impl.unix.UnixSyslogConfig;
import org.graylog2.syslog4j.              SyslogIF;

/** The startup class of the daemon. */
public class DnsResolvd {
    /**
     * The daemon entry point.
     *
     * @param argv The array of command-line arguments.
     */
    public static void main(final String[] argv) {
        int ret = EXIT_SUCCESS;

        int    argc        = argv.length - 1;
        String daemon_name = argv[0];
        int    port_number;

        String print_banner_opt = EMPTY_STRING;

        if (argc > 0) {
            // Validating the port number and discarding any rubbish
            // it may contain.
            try {
                port_number = new Integer(argv[1]).intValue();
            } catch (NumberFormatException e) {
                port_number = 0;
            }

            if (argc > 1) {
                print_banner_opt = argv[2].toUpperCase();
            }
        } else {
            port_number = 0;
        }

        if (print_banner_opt.compareTo(PRINT_BANNER_OPT) == 0) {
            separator_draw(DMN_DESCRIPTION);

            System.out.println(DMN_NAME + COMMA_SPACE_SEP  + DMN_VERSION_S__
                     + ONE_SPACE_STRING + DMN_VERSION      + NEW_LINE
                     + DMN_DESCRIPTION                     + NEW_LINE
                     + DMN_COPYRIGHT__  + ONE_SPACE_STRING + DMN_AUTHOR);

            separator_draw(DMN_DESCRIPTION);
        }

//      add_classpath();

        // Opening the system logger.
        // --- Calling <syslog.h> openlog(NULL, LOG_CONS | LOG_PID, LOG_DAEMON); ---
        UnixSyslog       log = new UnixSyslog();
        UnixSyslogConfig cfg = new UnixSyslogConfig();
        cfg.setIdent(null);
        cfg.setFacility(SyslogIF.FACILITY_DAEMON );
        log.initialize (SyslogIF.UNIX_SYSLOG, cfg);

        // Checking for args presence.
        if (argc == 0) {
            ret = EXIT_FAILURE;

            System.err.println(daemon_name + ERR_MUST_BE_ONE_TWO_ARGS_1
                                    + argc + ERR_MUST_BE_ONE_TWO_ARGS_2
                                           + NEW_LINE);

            log.error         (daemon_name + ERR_MUST_BE_ONE_TWO_ARGS_1
                                    + argc + ERR_MUST_BE_ONE_TWO_ARGS_2
                                           + NEW_LINE);

            System.err.println(MSG_USAGE_TEMPLATE_1 + daemon_name
                             + MSG_USAGE_TEMPLATE_2 + NEW_LINE);

            cleanups_fixate(log);

            System.exit(ret);
        }

        // Checking for port correctness.
        if ((port_number < MIN_PORT) || (port_number > MAX_PORT)) {
            ret = EXIT_FAILURE;

            System.err.println(daemon_name + ERR_PORT_MUST_BE_POSITIVE_INT
                                           + NEW_LINE);

            log.error         (daemon_name + ERR_PORT_MUST_BE_POSITIVE_INT
                                           + NEW_LINE);

            System.err.println(MSG_USAGE_TEMPLATE_1 + daemon_name
                             + MSG_USAGE_TEMPLATE_2 + NEW_LINE);

            cleanups_fixate(log);

            System.exit(ret);
        }

        // Starting up the daemon.
        new DnsLookupController().startup(port_number, daemon_name, log);

        // Making final cleanups.
        cleanups_fixate(log);
    }
}

// vim:set nu et ts=4 sw=4:
