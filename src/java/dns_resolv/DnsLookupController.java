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
import io.vertx.core.http.HttpMethod;

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
            HttpMethod mtd = req.method();

            String hostname = null; // The effective hostname to look up for.
            String fmt      = null; // The response format selector.

            // Used in the HTTP POST handler's lambda as a wrapper
            // to hold hostname and fmt values.
            String[] params = {
                null,               // The effective hostname to look up for.
                null                // The response format selector.
            };

            // ----------------------------------------------------------------
            // --- Parsing and validating request params - Begin --------------
            // ----------------------------------------------------------------
                   if (mtd == HttpMethod.GET ) {
                hostname = req.getParam("h");//<---+
                //                                 |
                // http://localhost:<port_number>/?h=<hostname>&f=<fmt>
                //                                              |
                fmt      = req.getParam("f");//<----------------+
            } else if (mtd == HttpMethod.POST) {
                req.bodyHandler(body -> {
                    if (body != null) {
                        String req_body_data = body.toString();

                        if (!req_body_data.isEmpty()) {
                            String[] qry_ary = req_body_data.split(AMPER);

             // $ curl -d 'h=<hostname>&f=<fmt>' http://localhost:<port_number>
             //            |            |
             //            |            +-------------------------------------+
             //            +------------------------------------------------+ |
             //                                                             | |
                            for (int i = 0; i < qry_ary.length; i++) { //   | |
                                       if(qry_ary[i].startsWith("h=")){//<--+ |
                                    params[0]=qry_ary[i].substring(2); //     |
                                } else if(qry_ary[i].startsWith("f=")){//<----+
                                    params[1]=qry_ary[i].substring(2);
                                }
                            }
                        }
                    }
                });

                hostname = params[0];
                fmt      = params[1];
            }
            // ----------------------------------------------------------------
            // --- Parsing and validating request params - End ----------------
            // ----------------------------------------------------------------

            req.response().end(hostname + NEW_LINE + fmt);
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