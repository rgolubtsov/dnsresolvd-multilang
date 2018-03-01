# -*- coding: utf-8 -*-
# src/python/dns_resolv/controller_helper.py
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

import syslog

class ControllerHelper:
    """The helper for the controller class and related ones."""

    # Helper constants.
    _EXIT_FAILURE     =    1 #    Failing exit status.
    _EXIT_SUCCESS     =    0 # Successful exit status.
    _EMPTY_STRING     =   ""
    _ONE_SPACE_STRING =  " "
    _COLON_SPACE_SEP  = ": "
    _COMMA_SPACE_SEP  = ", "
    _NEW_LINE         = "\n"

    # Common error messages and codes.
    _ERR_PREFIX                    =  "Error"
    _ERR_PORT_MUST_BE_POSITIVE_INT = (": <port_number> must be "
                                   +  "a positive integer value, "
                                   +  "in the range 1024-49151.")
    _ERR_CANNOT_START_SERVER       =  ": FATAL: Cannot start server "
    _ERR_SRV_UNKNOWN_REASON        = ("for an unknown reason. "
                                   +  "Exiting...")
    _ERR_SRV_PORT_IS_IN_USE        = ("due to the port requested "
                                   +  "is in use. Exiting...")
    _ERR_COULD_NOT_LOOKUP          =  "Could not lookup hostname."
#   _ERR_ADDR_ALREADY_IN_USE       =  "Address already in use."
    _ERR_ADDR_ALREADY_IN_USE       =  r"^.*(is|in).*$"
    _ERR_EADDRINUSE                =  "EADDRINUSE"

    # Print this error message when there are no any args passed.
    _ERR_MUST_BE_THE_ONLY_ARG_1 = (": There must be exactly one arg "
                                +  "passed: ")
    _ERR_MUST_BE_THE_ONLY_ARG_2 =  " args found"

    # Print this usage info just after any inappropriate input.
    _MSG_USAGE_TEMPLATE_1 = "Usage: "
    _MSG_USAGE_TEMPLATE_2 = " <port_number>"

    ## Constant: The minimum port number allowed.
    _MIN_PORT = 1024

    ## Constant: The maximum port number allowed.
    _MAX_PORT = 49151

    # Common notification messages.
    _MSG_SERVER_STARTED_1 = "Server started on port "
    _MSG_SERVER_STARTED_2 = "=== Hit Ctrl+C to terminate it."

    # HTTP response headers.
    _HDR_CONTENT_TYPE_N  = "Content-Type"
    _HDR_CONTENT_TYPE_V  = "text/html; charset=UTF-8"
    _HDR_CACHE_CONTROL_N = "Cache-Control"
    _HDR_CACHE_CONTROL_V = "no-cache, no-store, must-revalidate"
    _HDR_EXPIRES_N       = "Expires"
    _HDR_EXPIRES_V       = "Thu, 01 Dec 1994 16:00:00 GMT"
    _HDR_PRAGMA_N        = "Pragma"
    _HDR_PRAGMA_V        = "no-cache"

    # Daemon name, version, and copyright banners.
    _DMN_NAME        =  "DNS Resolver Daemon (dnsresolvd)"
    _DMN_DESCRIPTION = ("Performs DNS lookups for the given hostname "
                     +  "passed in an HTTP request")
    _DMN_VERSION_S__ =  "Version"
    _DMN_VERSION     =  "0.1"
    _DMN_COPYRIGHT__ =  "Copyright (C) 2017"
    _DMN_AUTHOR      =  "Radislav Golubtsov <ragolubtsov@my.com>"

    ## Constant: The default hostname to look up for.
    _DEF_HOSTNAME = "openbsd.org"

    def add_response_headers(self, req):
        """Adds headers to the response.

        Args:
            req: The incoming HTTP request object.
        """

        req.setHeader(self._HDR_CONTENT_TYPE_N,  self._HDR_CONTENT_TYPE_V )
        req.setHeader(self._HDR_CACHE_CONTROL_N, self._HDR_CACHE_CONTROL_V)
        req.setHeader(self._HDR_EXPIRES_N,       self._HDR_EXPIRES_V      )
        req.setHeader(self._HDR_PRAGMA_N,        self._HDR_PRAGMA_V       )

    # Helper method. Makes final buffer cleanups, closes streams, etc.
    def _cleanups_fixate(self):
        # Closing the system logger.
        syslog.closelog()

    # Helper method. Draws a horizontal separator banner.
    def _separator_draw(self, banner_text):
#       my $i = length($banner_text); # <== Perl 5.
        i = len(banner_text)

#       do { print('='); $i--; } while ($i); say(_EMPTY_STRING); # <== Perl 5.
# -----------------------------------------------------------------------------
#       while (True):
#           print('=', end=self._EMPTY_STRING)
#           i -= 1
#           if (not i):
#               break
#       print()
# -----------------------------------------------------------------------------
        exec("while (True):\n\tprint('=', end=self._EMPTY_STRING)\n\ti -= 1\n"
           + "\tif (not i):\n\t\tbreak\nprint()")

    def __init__(self):
        """Default constructor."""

        self = []

        return None

# vim:set nu et ts=4 sw=4:
