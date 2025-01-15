#
# src/perl/lib/DnsResolvd/ControllerHelper.pm
# =============================================================================
# DNS Resolver Daemon (dnsresolvd). Version 0.9.9
# =============================================================================
# A daemon that performs DNS lookups for the given hostname
# passed in an HTTP request, with the focus on its implementation
# using various programming languages. (Mojolicious-boosted impl.)
# =============================================================================
# Copyright (C) 2017-2025 Radislav (Radicchio) Golubtsov
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
use constant {
    _EXIT_FAILURE     =>    1, #    Failing exit status.
    _EXIT_SUCCESS     =>    0, # Successful exit status.
    _EMPTY_STRING     =>   "",
    _ONE_SPACE_STRING =>  " ",
    _COLON_SPACE_SEP  => ": ",
    _COMMA_SPACE_SEP  => ", ",
    _NEW_LINE         => "\n",
    _PRINT_BANNER_OPT => "-V",
};

# Common error messages and codes.
use constant {
    _ERR_PREFIX                    => "error",
    _ERR_PORT_MUST_BE_POSITIVE_INT => ": <port_number> must be "
                                    . "a positive integer value, "
                                    . "in the range 1024-49151.",
    _ERR_CANNOT_START_SERVER       => ": FATAL: Cannot start server ",
    _ERR_SRV_UNKNOWN_REASON        => "for an unknown reason. Exiting...",
    _ERR_SRV_PORT_IS_IN_USE        => "due to the port requested is in use. "
                                    . "Exiting...",
    _ERR_COULD_NOT_LOOKUP          => "could not lookup hostname",
    _ERR_ADDR_ALREADY_IN_USE       => qr/^.*Address\ already\ in\ use.*$/,
};

# Print this error message when there are no any args passed.
use constant {
    _ERR_MUST_BE_ONE_TWO_ARGS_1 => ": There must be one or two args passed: ",
    _ERR_MUST_BE_ONE_TWO_ARGS_2 => " args found",
};

# Print this usage info just after any inappropriate input.
use constant {
    _MSG_USAGE_TEMPLATE_1 => "Usage: ",
    _MSG_USAGE_TEMPLATE_2 => " <port_number> [-V]",
};

## Constant: The minimum port number allowed.
use constant _MIN_PORT => 1024;

## Constant: The maximum port number allowed.
use constant _MAX_PORT => 49151;

# Common notification messages.
use constant {
    _MSG_SERVER_STARTED_1 => "Server started on port ",
    _MSG_SERVER_STARTED_2 => "=== Hit Ctrl+C to terminate it.",
};

# HTTP request params.
use constant {
    _PRM_FMT_HTML => "html",
    _PRM_FMT_JSON => "json",
};

# HTTP response headers and status codes.
use constant {
    _HDR_CONTENT_TYPE_HTML => "text/html; charset=UTF-8",
    _HDR_CONTENT_TYPE_JSON => "application/json",
    _HDR_CACHE_CONTROL     => "no-cache, no-store, must-revalidate",
    _HDR_EXPIRES           => "Thu, 01 Dec 1994 16:00:00 GMT",
    _HDR_PRAGMA            => "no-cache",
    _RSC_HTTP_200_OK       => 200,
};

# Response data names.
use constant _DAT_VERSION_V  => "IPv";

# Daemon name, version, and copyright banners.
use constant {
    _DMN_NAME        => "DNS Resolver Daemon (dnsresolvd)",
    _DMN_DESCRIPTION => "Performs DNS lookups for the given hostname "
                      . "passed in an HTTP request",
    _DMN_VERSION_S__ => "Version",
    _DMN_VERSION     => "0.9.9",
    _DMN_COPYRIGHT__ => "Copyright (C) 2017-2025",
    _DMN_AUTHOR      => "Radislav Golubtsov <ragolubtsov\@my.com>",
};

## Constant: The default hostname to look up for.
use constant _DEF_HOSTNAME => "openbsd.org";

## Props to export.
our @EXPORT_OK = (
    "_EXIT_FAILURE",
    "_EXIT_SUCCESS",
    "_EMPTY_STRING",
    "_ONE_SPACE_STRING",
    "_COLON_SPACE_SEP",
    "_COMMA_SPACE_SEP",
    "_NEW_LINE",
    "_PRINT_BANNER_OPT",
# -----------------------------------------------------------------------------
    "_ERR_PREFIX",
    "_ERR_PORT_MUST_BE_POSITIVE_INT",
    "_ERR_CANNOT_START_SERVER",
    "_ERR_SRV_UNKNOWN_REASON",
    "_ERR_SRV_PORT_IS_IN_USE",
    "_ERR_COULD_NOT_LOOKUP",
    "_ERR_ADDR_ALREADY_IN_USE",
# -----------------------------------------------------------------------------
    "_ERR_MUST_BE_ONE_TWO_ARGS_1",
    "_ERR_MUST_BE_ONE_TWO_ARGS_2",
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
    "_PRM_FMT_HTML",
    "_PRM_FMT_JSON",
# -----------------------------------------------------------------------------
    "_HDR_CONTENT_TYPE_HTML",
    "_HDR_CONTENT_TYPE_JSON",
    "_HDR_CACHE_CONTROL",
    "_HDR_EXPIRES",
    "_HDR_PRAGMA",
    "_RSC_HTTP_200_OK",
# -----------------------------------------------------------------------------
    "_DAT_VERSION_V",
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
# @param fmt  The response format selector.
#
sub add_response_headers {
    my  $self        = shift();
    my ($ctrl, $fmt) = @_;

    my $headers = $ctrl->res()->headers();

    my $HDR_CONTENT_TYPE_V;

         if ($fmt eq _PRM_FMT_HTML) {
        $HDR_CONTENT_TYPE_V = _HDR_CONTENT_TYPE_HTML;
    } elsif ($fmt eq _PRM_FMT_JSON) {
        $HDR_CONTENT_TYPE_V = _HDR_CONTENT_TYPE_JSON;
    }

    $headers->content_type (          $HDR_CONTENT_TYPE_V);
    $headers->cache_control(          _HDR_CACHE_CONTROL );
    $headers->expires      (          _HDR_EXPIRES       );
    $headers->header       (Pragma => _HDR_PRAGMA        );
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

# vim:set nu et ts=4 sw=4:
