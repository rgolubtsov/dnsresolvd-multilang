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

from dns_resolv.controller_helper     import ControllerHelper
from dns_resolv.dns_lookup_controller import DnsLookupController

## The main class of the daemon.
class DnsResolvd:
    ##
    # Starts up the daemon.
    #
    # @param port_number The server port number to listen on.
    # @param daemon_name The daemon name (executable/script name).
    #
    # @return The server exit code when interrupted.
    #
    def startup(self, port_number, daemon_name):
        # Instantiating the controller helper class.
        aux = ControllerHelper()

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
            #
            # @param req The incoming HTTP request object.
            #
            # @return The HTTP response has to be rendered.
            #
            def render_GET(self, req):
                # Instantiating the controller class.
                ctrl = DnsLookupController()

                # Performing DNS lookup for the given hostname
                # and writing the response out.
                dbg = ctrl.dns_lookup(port_number, daemon_name)

                # -------------------------------------------------------------
                # --- Debug output - Begin ------------------------------------
                # -------------------------------------------------------------
                resp_buffer = ("<!DOCTYPE html><html><head><title>" + dbg
                            +  "</title></head><body><p>"           + dbg
                            +  "</p></body></html>").encode()
                # -------------------------------------------------------------
                # --- Debug output - End --------------------------------------
                # -------------------------------------------------------------

                # Adding headers to the response.
                aux.add_response_headers(req)

                return resp_buffer

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
