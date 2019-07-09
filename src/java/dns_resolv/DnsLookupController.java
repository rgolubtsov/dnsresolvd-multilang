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

import io.vertx.core.Vertx;
import io.vertx.core.http.HttpServer;
import io.vertx.core.http.HttpMethod;

import java.net.InetAddress;
import java.net.Inet4Address;
import java.net.Inet6Address;
import java.net.UnknownHostException;

/** The controller class of the daemon. */
public class DnsLookupController {
    /**
     * Performs DNS lookup action for the given hostname,
     * i.e. (in this case) IP address retrieval by hostname.
     *
     * @param hostname The effective hostname to look up for.
     *
     * @return The array containing IP address of the analyzing host/service
     *         and corresponding IP version (family) used to look up in DNS:
     *         <code>4</code> for IPv4-only hosts,
     *         <code>6</code> for IPv6-capable hosts.
     */
    public String[] dns_lookup(final String hostname) {
        String[] addr_ver = new String[2];

        // Trying to get an A record (IPv4) for the host
        // or its AAAA record (IPv6).
        try {
            InetAddress hostent = InetAddress.getByName(hostname);

            addr_ver[0] = hostent.getHostAddress();

                   if (hostent instanceof Inet4Address) {
                addr_ver[1] = new Integer(4).toString();
            } else if (hostent instanceof Inet6Address) {
                addr_ver[1] = new Integer(6).toString();
            }
        } catch (UnknownHostException e) {
            addr_ver[0] = ERR_PREFIX;
            addr_ver[1] = EMPTY_STRING;
        }

        return addr_ver;
    }

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

            if((hostname == null) || (hostname.isEmpty())) {
                hostname  = DEF_HOSTNAME;
            }

            if((fmt      == null) || (fmt     .isEmpty())) {
                fmt       = PRM_FMT_JSON;
            } else {
                fmt       = fmt.toLowerCase();

                String[] fmt_ = {
                    PRM_FMT_HTML,
                    PRM_FMT_JSON
                };

                boolean _fmt = false;

                for (int i  = 0; i < fmt_.length; i++) {
                    if (fmt.compareTo(fmt_[i]) == 0) {
                       _fmt = true; break;
                    }
                }

                if (!_fmt) {
                    fmt   = PRM_FMT_JSON;
                }
            }
            // ----------------------------------------------------------------
            // --- Parsing and validating request params - End ----------------
            // ----------------------------------------------------------------

            // Performing DNS lookup for the given hostname.
            String[] addr_ver = dns_lookup(hostname);

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
