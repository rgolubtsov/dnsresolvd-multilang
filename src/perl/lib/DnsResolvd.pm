#
# src/perl/lib/DnsResolvd.pm
# =============================================================================
# DNS Resolver Daemon (dnsresolvd). Version 0.1
# =============================================================================
# A daemon that performs DNS lookups for the given hostname
# passed in an HTTP request, with the focus on its implementation
# using various programming languages. (Mojolicious-boosted impl.)
# =============================================================================
# Copyright (C) 2017-2018 Radislav (Radicchio) Golubtsov
#
# (See the LICENSE file at the top of the source tree.)
#

package DnsResolvd;

use strict;
use warnings;
use utf8;
use v5.10;

use Mojo::Base "Mojolicious";

## Constant: The root route to make GET requests.
use constant ROOT_ROUTE => "/";

## Constant: The controller route to make DNS lookup actions.
use constant DNS_LOOKUP_CONTROLLER_ROUTE => "dns_lookup_controller";

## Constant: The DNS lookup action route.
use constant DNS_LOOKUP_ACTION_ROUTE => "#dns_lookup";

## Starts up the daemon.
sub startup {
    my $self = shift();

    # Getting the router object.
    my $router = $self->routes();

    # Routing to the DNS lookup controller and action route.
    $router->get(ROOT_ROUTE)->to(DNS_LOOKUP_CONTROLLER_ROUTE
                               . DNS_LOOKUP_ACTION_ROUTE);
}

1;

# vim:set nu et ts=4 sw=4:
