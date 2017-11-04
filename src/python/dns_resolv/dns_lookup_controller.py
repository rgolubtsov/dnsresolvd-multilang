# -*- coding: utf-8 -*-
# src/python/dns_resolv/dns_lookup_controller.py
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

## The controller class of the daemon.
class DnsLookupController:
    ##
    # Performs DNS lookup action for the given hostname,
    # i.e. (in this case) IP address retrieval by hostname.
    #
    def dns_lookup(self, port_number, daemon_name):
        # TODO: Implement performing DNS lookup.

        # ---------------------------------------------------------------------
        # --- Debug output - Begin --------------------------------------------
        # ---------------------------------------------------------------------
        dbg = daemon_name + ":" + str(port_number)

        print(dbg)
        # ---------------------------------------------------------------------
        # --- Debug output - End ----------------------------------------------
        # ---------------------------------------------------------------------

        return dbg

    ## Default constructor.
    def __init__(self):
        self = []

        return None

# vim:set nu:et:ts=4:sw=4:
