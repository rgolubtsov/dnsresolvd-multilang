#
# content/dev/misc/dnsresolvd/perl/lib/DnsResolvd/DnsLookupController.pm
# =============================================================================
# DNS Resolver Daemon (dnsresolvd). Version 0.1
# =============================================================================
# A Mojolicious-boosted daemon for performing DNS lookups.
# =============================================================================
# Copyright (C) 2017 Radislav (Radicchio) Golubtsov
#
# (See the LICENSE file at the top of the source tree.)
#

package DnsResolvd::DnsLookupController;

use strict;
use warnings;
use utf8;
use v5.10;

use Mojo::Base "Mojolicious::Controller";
use Net::DNS::Native;
use IO::Select;
use Socket;

#use Data::Dumper;

use DnsResolvd::ControllerHelper
    "_EXIT_SUCCESS",
    "_COLON_SPACE_SEP",
    "_NEW_LINE",
# -----------------------------------------------------------------------------
    "_ERR_PREFIX",
    "_ERR_COULD_NOT_LOOKUP",
# -----------------------------------------------------------------------------
    "_HDR_CONTENT_TYPE",
    "_HDR_CACHE_CONTROL",
    "_HDR_EXPIRES",
    "_HDR_PRAGMA",
# -----------------------------------------------------------------------------
    "_DMN_NAME",
    "_DMN_DESCRIPTION",
# -----------------------------------------------------------------------------
    "_DEF_HOSTNAME";

# HTTP response buffer template chunks.
use constant RESP_TEMPLATE_1 => "<!DOCTYPE html>"                                                                        . _NEW_LINE
. "<html lang=\"en-US\" dir=\"ltr\">" . _NEW_LINE . "<head>"                                                             . _NEW_LINE
. "<meta http-equiv=\"Content-Type\"    content=\"" . _HDR_CONTENT_TYPE . "\" />"                                        . _NEW_LINE
. "<meta http-equiv=\"X-UA-Compatible\" content=\"IE=edge,chrome=1\" />"                                                 . _NEW_LINE
. "<!-- No caching at all for:                                                                       -->"                . _NEW_LINE
. "<meta http-equiv=\"Cache-Control\"   content=\"" . _HDR_CACHE_CONTROL . "\" /> <!-- HTTP/1.1 -->"                     . _NEW_LINE
. "<meta http-equiv=\"Expires\"         content=\"" . _HDR_EXPIRES . "\"       /> <!-- Proxies  -->"                     . _NEW_LINE
. "<meta http-equiv=\"Pragma\"          content=\"" . _HDR_PRAGMA . "\"                            /> <!-- HTTP/1.0 -->" . _NEW_LINE
. "<meta       name=\"viewport\"        content=\"width=device-width,initial-scale=1\" />"                               . _NEW_LINE
. "<meta       name=\"description\"     content=\"" . _DMN_DESCRIPTION . "\" />"                                         . _NEW_LINE
. "<title>" . _DMN_NAME . "</title>" . _NEW_LINE . "</head>"                                                             . _NEW_LINE
. "<body id=\"dnsresolvd\">"         . _NEW_LINE . "<p>";

use constant RESP_TEMPLATE_2A => " ==&gt; ";
use constant RESP_TEMPLATE_2B => " (IPv";
use constant RESP_TEMPLATE_2C => ")";

use constant RESP_TEMPLATE_3 => _ERR_PREFIX . _COLON_SPACE_SEP
                              . _ERR_COULD_NOT_LOOKUP;

use constant RESP_TEMPLATE_4 => "</p>" . _NEW_LINE . "</body>" . _NEW_LINE
                                                   . "</html>" . _NEW_LINE;

##
# Performs DNS lookup action for the given hostname,
# i.e. (in this case) IP address retrieval by hostname.
#
# @return The HTTP response status code indicating the result
#         of rendering the response buffer.
#
sub dns_lookup {
    my $self = shift();

    my $ret = _EXIT_SUCCESS;

    # Parsing and validating query params.
    my $query = $self->req()->query_params();

    # http://localhost:<port_number>/?h=<hostname>
    #                                 |
    #                             +---+
    #                             |
    #                             V
    my $hostname = $query->param("h");

    if (!$hostname) {
        $hostname = _DEF_HOSTNAME;
    }

    # Performing DNS lookup for the given hostname
    # and writing the response out.
    my $dns = Net::DNS::Native->new();

    my $sock = $dns->getaddrinfo($hostname);

    # Instantiating the select() system call wrapper class.
    my $sel = IO::Select->new($sock);

    # Waiting until resolving done.
    $sel->can_read();

#   print(Dumper($sel));

    my $addr;
    my $ver;

    if (!$sock) {
        $addr = _ERR_PREFIX;
    } else {
        my ($err, @res) = $dns->get_result($sock);

        if ($err) {
            $addr = _ERR_PREFIX;
        } else {
            $ver  = $res[0]->{family};
            $addr = $res[0]->{ addr };

            # If the host doesn't have the A record (IPv4),
            # trying to find its AAAA record (IPv6).
            if ($ver == AF_INET) {
                $ver  = 4;
                $addr = Socket::inet_ntop(AF_INET,
                                         (unpack_sockaddr_in ($addr))[1]);
            } else {
                $ver  = 6;
                $addr = Socket::inet_ntop(AF_INET6,
                                         (unpack_sockaddr_in6($addr))[1]);
            }   #                                   ^
        }       #                                   |
    }           # From the Socket lib. -------------+

    my $resp_buffer = RESP_TEMPLATE_1 . $hostname . RESP_TEMPLATE_2A;

    if ($addr eq _ERR_PREFIX) {
        $resp_buffer .= RESP_TEMPLATE_3;
    } else {
        $resp_buffer .= $addr . RESP_TEMPLATE_2B . $ver . RESP_TEMPLATE_2C;
    }

    $resp_buffer .= RESP_TEMPLATE_4;

    # Instantiating the controller helper class.
    my $aux = DnsResolvd::ControllerHelper->new();

    # Adding headers to the response.
    $aux->add_response_headers($self);

    # Rendering the response buffer.
    $ret = $self->render(inline => $resp_buffer);

    return $ret;
}

1;

# vim:set nu:et:ts=4:sw=4:
