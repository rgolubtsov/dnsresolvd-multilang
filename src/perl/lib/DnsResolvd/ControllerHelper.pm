#
# content/dev/misc/dnsresolvd/perl/lib/DnsResolvd/ControllerHelper.pm
# =============================================================================
# DNS Resolver Daemon (dnsresolvd). Version 0.1
# =============================================================================
# A Mojolicious-boosted daemon for performing DNS lookups.
# =============================================================================
# Copyright (C) 2017 Radislav (Radicchio) Golubtsov
#
# (See the LICENSE file at the top of the source tree.)
#

package DnsResolvd::ControllerHelper;

use strict;
use warnings;
use utf8;
use v5.10;

use Exporter "import";

# Helper constants.
use constant _EXIT_FAILURE     =>    1; #    Failing exit status.
use constant _EXIT_SUCCESS     =>    0; # Successful exit status.
use constant _EMPTY_STRING     =>   "";
use constant _ONE_SPACE_STRING =>  " ";
use constant _COLON_SPACE_SEP  => ": ";
use constant _COMMA_SPACE_SEP  => ", ";
use constant _NEW_LINE         => "\n";

# Common error messages and codes.
use constant _ERR_PREFIX                    => "Error";
use constant _ERR_PORT_MUST_BE_POSITIVE_INT => ": <port_number> must be "
                                             . "a positive integer value, "
                                             . "in the range 1024-49151.";
use constant _ERR_CANNOT_START_SERVER       => ": FATAL: Cannot start server ";
use constant _ERR_SRV_UNKNOWN_REASON        => "for an unknown reason. "
                                             . "Exiting...";
use constant _ERR_SRV_PORT_IS_IN_USE        => "due to the port requested "
                                             . "is in use. Exiting...";
use constant _ERR_COULD_NOT_LOOKUP          => "Could not lookup hostname.";
use constant _ERR_ADDR_ALREADY_IN_USE       =>
                                          qr/^.*Address\ already\ in\ use.*$/;

# Print this error message when there are no any args passed.
use constant _ERR_MUST_BE_THE_ONLY_ARG_1 => ": There must be exactly one arg "
                                          . "passed: ";
use constant _ERR_MUST_BE_THE_ONLY_ARG_2 => " args found";

# Print this usage info just after any inappropriate input.
use constant _MSG_USAGE_TEMPLATE_1 => "Usage: ";
use constant _MSG_USAGE_TEMPLATE_2 => " <port_number>";

## Constant: The minimum port number allowed.
use constant _MIN_PORT => 1024;

## Constant: The maximum port number allowed.
use constant _MAX_PORT => 49151;

# Common notification messages.
use constant _MSG_SERVER_STARTED_1 => "Server started on port ";
use constant _MSG_SERVER_STARTED_2 => "=== Hit Ctrl+C to terminate it.";

# HTTP response headers and status codes.
use constant _HDR_CONTENT_TYPE  => "text/html; charset=UTF-8";
use constant _HDR_CACHE_CONTROL => "no-cache, no-store, must-revalidate";
use constant _HDR_EXPIRES       => "Thu, 01 Dec 1994 16:00:00 GMT";
use constant _HDR_PRAGMA        => "no-cache";
use constant _RSC_HTTP_200_OK   => 200;

# Daemon name, version, and copyright banners.
use constant _DMN_NAME        => "DNS Resolver Daemon (dnsresolvd)";
use constant _DMN_DESCRIPTION => "Performs DNS lookups for the given hostname "
                               . "passed in an HTTP request";
use constant _DMN_VERSION_S__ => "Version";
use constant _DMN_VERSION     => "0.1";
use constant _DMN_COPYRIGHT__ => "Copyright (C) 2017";
use constant _DMN_AUTHOR      => "Radislav Golubtsov <ragolubtsov\@my.com>";

## Constant: The default hostname to look up for.
use constant _DEF_HOSTNAME => "openbsd.org";

## Props to export.
our @EXPORT_OK = (
    "_EXIT_FAILURE",
    "_EXIT_SUCCESS",
    "_ONE_SPACE_STRING",
    "_COLON_SPACE_SEP",
    "_COMMA_SPACE_SEP",
    "_NEW_LINE",
# -----------------------------------------------------------------------------
    "_ERR_PREFIX",
    "_ERR_PORT_MUST_BE_POSITIVE_INT",
    "_ERR_CANNOT_START_SERVER",
    "_ERR_SRV_UNKNOWN_REASON",
    "_ERR_SRV_PORT_IS_IN_USE",
    "_ERR_COULD_NOT_LOOKUP",
    "_ERR_ADDR_ALREADY_IN_USE",
# -----------------------------------------------------------------------------
    "_ERR_MUST_BE_THE_ONLY_ARG_1",
    "_ERR_MUST_BE_THE_ONLY_ARG_2",
# -----------------------------------------------------------------------------
    "_MSG_USAGE_TEMPLATE_1",
    "_MSG_USAGE_TEMPLATE_2",
# -----------------------------------------------------------------------------
    "_MIN_PORT",
    "_MAX_PORT",
# -----------------------------------------------------------------------------
    "_MSG_SERVER_STARTED_1",
    "_MSG_SERVER_STARTED_2",
# -----------------------------------------------------------------------------
    "_HDR_CONTENT_TYPE",
    "_HDR_CACHE_CONTROL",
    "_HDR_EXPIRES",
    "_HDR_PRAGMA",
    "_RSC_HTTP_200_OK",
# -----------------------------------------------------------------------------
    "_DMN_NAME",
    "_DMN_DESCRIPTION",
    "_DMN_VERSION_S__",
    "_DMN_VERSION",
    "_DMN_COPYRIGHT__",
    "_DMN_AUTHOR",
# -----------------------------------------------------------------------------
    "_DEF_HOSTNAME",
);

##
# Adds headers to the response.
#
# @param ctrl The controller instance object.
#
sub add_response_headers {
    my  $self  = shift();
    my ($ctrl) = @_;

    my $headers = $ctrl->res()->headers();

    $headers->content_type (          _HDR_CONTENT_TYPE );
    $headers->cache_control(          _HDR_CACHE_CONTROL);
    $headers->expires      (          _HDR_EXPIRES      );
    $headers->header       (Pragma => _HDR_PRAGMA       );
}

# Helper method. Draws a horizontal separator banner.
sub _separator_draw {
    my  $self         = shift();
    my ($banner_text) = @_;

    my $i = length($banner_text);

    do { print('='); $i--; } while ($i); say(_EMPTY_STRING);
}

## Default constructor.
sub new {
    my $class = shift();
    my $self  = [];

    bless($self, $class);

    return $self;
}

1;

# vim:set nu:et:ts=4:sw=4:
