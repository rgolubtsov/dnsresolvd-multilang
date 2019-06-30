/*
 * src/java/dns_resolv/DnsLookupController.java
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

package dns_resolv;

import static dns_resolv.ControllerHelper.*;

import org.graylog2.syslog4j.impl.unix.UnixSyslog;

import io.vertx.core.http.HttpServer;
import io.vertx.core.Vertx;

/** The controller class of the daemon. */
public class DnsLookupController {
    /**
     * Starts up the daemon.
     *
     * @param port_number The server port number to listen on.
     * @param daemon_name The name of the daemon startup script.
     * @param log         The system logger object instance.
     */
    public void startup(final int        port_number,
                        final String     daemon_name,
                        final UnixSyslog log) {

        HttpServer server = Vertx.vertx().createHttpServer();

        server.requestHandler(req -> {
            System.out.println("--- server.requestHandler() called ---");
        });

        server.listen(port_number, res -> {
            int ret = EXIT_SUCCESS;

            if (res.succeeded()) {
                System.out.println(MSG_SERVER_STARTED_1 + port_number + NEW_LINE
                                 + MSG_SERVER_STARTED_2);

                log.info(MSG_SERVER_STARTED_1 + port_number + NEW_LINE
                       + MSG_SERVER_STARTED_2);
            } else {
                ret = EXIT_FAILURE;

                if (res.cause().getMessage()
                   .compareTo(ERR_ADDR_ALREADY_IN_USE) == 0) {

                    System.err.println(daemon_name + ERR_CANNOT_START_SERVER
                                                   + ERR_SRV_PORT_IS_IN_USE
                                                   + NEW_LINE);

                    log.error         (daemon_name + ERR_CANNOT_START_SERVER
                                                   + ERR_SRV_PORT_IS_IN_USE
                                                   + NEW_LINE);
                } else {
                    System.err.println(daemon_name + ERR_CANNOT_START_SERVER
                                                   + ERR_SRV_UNKNOWN_REASON
                                                   + NEW_LINE);

                    log.error         (daemon_name + ERR_CANNOT_START_SERVER
                                                   + ERR_SRV_UNKNOWN_REASON
                                                   + NEW_LINE);
                }

                System.exit(ret);
            }
        });
    }
}

// vim:set nu et ts=4 sw=4:
