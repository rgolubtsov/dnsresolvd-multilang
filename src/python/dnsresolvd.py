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
    ## Starts up the daemon.
    def startup(self, port_number, daemon_name):
        class Daemon(Resource):
            isLeaf = True

            def render_GET(self, request):
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
                            +  "</p></body></html>")
                # -------------------------------------------------------------
                # --- Debug output - End --------------------------------------
                # -------------------------------------------------------------

                return resp_buffer

            ## Default constructor.
            def __init__(self):
                self = []

                return None

        TCP4ServerEndpoint(reactor, port_number).listen(Site(Daemon()))
        reactor.run()

    ## Default constructor.
    def __init__(self):
        self = []

        return None

# vim:set nu:et:ts=4:sw=4:
