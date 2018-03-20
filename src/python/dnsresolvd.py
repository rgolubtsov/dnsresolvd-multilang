# -*- coding: utf-8 -*-
# src/python/dnsresolvd.py
# =============================================================================
# DNS Resolver Daemon (dnsresolvd). Version 0.1
# =============================================================================
# A daemon that performs DNS lookups for the given hostname
# passed in an HTTP request, with the focus on its implementation
# using various programming languages. (Twisted-boosted impl.)
# =============================================================================
# Copyright (C) 2017-2018 Radislav (Radicchio) Golubtsov
#
# (See the LICENSE file at the top of the source tree.)
#

from twisted.web.resource       import Resource
from twisted.internet.endpoints import TCP4ServerEndpoint
from twisted.internet           import reactor
from twisted.web.server         import Site

from dns_resolv.dns_lookup_controller import DnsLookupController

class DnsResolvd:
    """The main class of the daemon."""

    def startup(self, port_number, aux):
        """Starts up the daemon.

        Args:
            port_number: The server port number to listen on.
            aux:         The controller helper object instance.

        Returns:
            The server exit code when interrupted.
        """

        ret = aux._EXIT_SUCCESS

        class Daemon(Resource):
            """The server class of the daemon.
            Used by Twisted engine to run an event loop.

            Extends:
                Resource: The Twisted web-accessible resource class.
            """

            isLeaf = True

            def render_GET(self, req):
                """Renders the HTTP response based on the incoming HTTP
                request.
                It also calls the method to perform DNS lookup for a hostname
                passed in the HTTP request.

                Args:
                    req: The incoming HTTP request object.

                Returns:
                    The HTTP response has to be rendered.
                """

                query = req.args
                h     = b"h"

                # Parsing and validating query params.
                if (h in query):
                    hostname = query[h][0].decode()
                    #                ^
                    #                |
                    #                +----------------+
                    #                                 |
                    # http://localhost:<port_number>/?h=<hostname>
                else:
                    hostname = aux._DEF_HOSTNAME

                # Instantiating the controller class.
                ctrl = DnsLookupController()

                # Performing DNS lookup for the given hostname.
                (addr, ver) = ctrl.dns_lookup(hostname, aux)

                resp_buffer = ("<!DOCTYPE html>"                                            + aux._NEW_LINE
+ "<html lang=\"en-US\" dir=\"ltr\">"                                                       + aux._NEW_LINE
+ "<head>"                                                                                  + aux._NEW_LINE
+ "<meta http-equiv=\""     + aux._HDR_CONTENT_TYPE_N +                  "\"    content=\""
                            + aux._HDR_CONTENT_TYPE_V +                  "\"           />"  + aux._NEW_LINE
+ "<meta http-equiv=\"X-UA-Compatible\" content=\"IE=edge\"                            />"  + aux._NEW_LINE
+ "<meta       name=\"viewport\"        content=\"width=device-width,initial-scale=1\" />"  + aux._NEW_LINE
+ "<title>" + aux._DMN_NAME + "</title>"                                                    + aux._NEW_LINE
+ "</head>"                                                                                 + aux._NEW_LINE
+ "<body>"                                                                                  + aux._NEW_LINE
+ "<p>"     + hostname      + " ==&gt; ")

                if (addr == aux._ERR_PREFIX):
                    resp_buffer += (aux._ERR_PREFIX
                                +   aux._COLON_SPACE_SEP
                                +   aux._ERR_COULD_NOT_LOOKUP)
                else:
                    resp_buffer +=  str(addr) + " (IPv" + str(ver) + ")"

                resp_buffer += ("</p>"    + aux._NEW_LINE
                            +   "</body>" + aux._NEW_LINE
                            +   "</html>" + aux._NEW_LINE)

                # Adding headers to the response.
                aux.add_response_headers(req)

                return resp_buffer.encode()

            ## Default constructor.
            def __init__(self):
                self = []

                return None

        # Setting up the TCP IPv4-configured server.
        TCP4ServerEndpoint(reactor, port_number).listen(Site(Daemon()))

        # Running the event loop.
        ret = reactor.run()

        return ret

    def __init__(self):
        """Default constructor."""

        self = []

        return None

# vim:set nu et ts=4 sw=4:
