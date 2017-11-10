# -*- coding: utf-8 -*-
# src/python/dnsresolvd.py
# =============================================================================
# DNS Resolver Daemon (dnsresolvd). Version 0.1
# =============================================================================
# A daemon that performs DNS lookups for the given hostname
# passed in an HTTP request, with the focus on its implementation
# using various programming languages. (Twisted-boosted impl.)
# =============================================================================
# Copyright (C) 2017 Radislav (Radicchio) Golubtsov
#
# (See the LICENSE file at the top of the source tree.)
#

from twisted.web.resource       import Resource
from twisted.internet.endpoints import TCP4ServerEndpoint
from twisted.internet           import reactor
from twisted.web.server         import Site

from dns_resolv.dns_lookup_controller import DnsLookupController

## The main class of the daemon.
class DnsResolvd:
    ##
    # Starts up the daemon.
    #
    # @param port_number The server port number to listen on.
    # @param aux         The controller helper object instance.
    #
    # @return The server exit code when interrupted.
    #
    def startup(self, port_number, aux):
        ret = aux._EXIT_SUCCESS

        ##
        # The server class of the daemon.
        # Used by Twisted engine to run an event loop.
        #
        # @extends Resource The Twisted web-accessible resource class.
        #
        class Daemon(Resource):
            isLeaf = True

            ##
            # Renders the HTTP response based on the incoming HTTP request.
            # It also calls the method to perform DNS lookup for a hostname
            # passed in the HTTP request.
            #
            # @param req The incoming HTTP request object.
            #
            # @return The HTTP response has to be rendered.
            #
            def render_GET(self, req):
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

                resp_buffer = ("<!DOCTYPE html>"                                                                                                  + aux._NEW_LINE
+ "<html lang=\"en-US\" dir=\"ltr\">" + aux._NEW_LINE + "<head>"                                                                                  + aux._NEW_LINE
+ "<meta http-equiv=\"" + aux._HDR_CONTENT_TYPE_N + "\"    content=\"" + aux._HDR_CONTENT_TYPE_V + "\" />"                                        + aux._NEW_LINE
+ "<meta http-equiv=\"X-UA-Compatible\" content=\"IE=edge,chrome=1\" />"                                                                          + aux._NEW_LINE
+ "<!-- No caching at all for:                                                                       -->"                                         + aux._NEW_LINE
+ "<meta http-equiv=\"" + aux._HDR_CACHE_CONTROL_N + "\"   content=\"" + aux._HDR_CACHE_CONTROL_V + "\" /> <!-- HTTP/1.1 -->"                     + aux._NEW_LINE
+ "<meta http-equiv=\"" + aux._HDR_EXPIRES_N + "\"         content=\"" + aux._HDR_EXPIRES_V + "\"       /> <!-- Proxies  -->"                     + aux._NEW_LINE
+ "<meta http-equiv=\"" + aux._HDR_PRAGMA_N + "\"          content=\"" + aux._HDR_PRAGMA_V + "\"                            /> <!-- HTTP/1.0 -->" + aux._NEW_LINE
+ "<meta       name=\"viewport\"        content=\"width=device-width,initial-scale=1\" />"                                                        + aux._NEW_LINE
+ "<meta       name=\"description\"     content=\"" + aux._DMN_DESCRIPTION + "\" />"                                                              + aux._NEW_LINE
+ "<title>" + aux._DMN_NAME + "</title>" + aux._NEW_LINE + "</head>"                                                                              + aux._NEW_LINE
+ "<body id=\"dnsresolvd\">"             + aux._NEW_LINE + "<p>"
+ hostname + " ==&gt; ")

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

    ## Default constructor.
    def __init__(self):
        self = []

        return None

# vim:set nu:et:ts=4:sw=4:
