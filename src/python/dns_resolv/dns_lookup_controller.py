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

from dns.resolver import Resolver, NoAnswer

class DnsLookupController:
    """The controller class of the daemon."""

    def dns_lookup(self, hostname, aux):
        """Performs DNS lookup action for the given hostname,
        i.e. (in this case) IP address retrieval by hostname.

        Args:
            hostname: The effective hostname to look up for.
            aux:      The controller helper object instance.

        Returns:
            The IP address retrieved for the host analyzed
            and the IP version (family) used to look up in DNS.
        """

        resolver = Resolver()

        # If the host doesn't have the A record (IPv4),
        # trying to find its AAAA record (IPv6).
        try:
            addr     = resolver.query(hostname,    "A")[0] # <---+
            ver      = 4       #                                 |
        except Exception as e: # From the dnspython lib. --------+
            try:               #                                 |
                addr = resolver.query(hostname, "AAAA")[0] # <---+
                ver  = 6
            except Exception as e:
                addr = ver = aux._ERR_PREFIX

        return (addr, ver)

    def __init__(self):
        """Default constructor."""

        self = []

        return None

# vim:set nu et ts=4 sw=4:
