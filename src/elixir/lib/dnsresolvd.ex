#
# src/elixir/lib/dnsresolvd.ex
# =============================================================================
# DNS Resolver Daemon (dnsresolvd). Version 0.1
# =============================================================================
# A daemon that performs DNS lookups for the given hostname
# passed in an HTTP request, with the focus on its implementation
# using various programming languages. (Cowboy-boosted impl.)
# =============================================================================
# Copyright (C) 2017-2018 Radislav (Radicchio) Golubtsov
#
# (See the LICENSE file at the top of the source tree.)
#

defmodule AUX do
    @moduledoc "The helper module for the daemon."

    # Helper constants.
    def _EXIT_FAILURE    , do:    1
    def _EXIT_SUCCESS    , do:    0
    def _EMPTY_STRING    , do:   ""
    def _COLON_SPACE_SEP , do: ": "
    def _COMMA_SPACE_SEP , do: ", "
    def _NEW_LINE        , do: "\n"
    def _C_FMT           , do: "%c"
    def _AMPER           , do:  "&"
    def _ONE_SPACE_STRING, do:  " "
    def _PRINT_BANNER_OPT, do: "-V"

    # Daemon name, version, and copyright banners.
    def _DMN_NAME       , do: "DNS Resolver Daemon (dnsresolvd)"
    def _DMN_DESCRIPTION, do: "Performs DNS lookups for the given "
                          <>  "hostname passed in an HTTP request"
    def _DMN_VERSION_S__, do: "Version"
    def _DMN_VERSION    , do: "0.1"
    def _DMN_COPYRIGHT__, do: "Copyright (C) 2017-2018"
    def _DMN_AUTHOR     , do: "Radislav Golubtsov <ragolubtsov@my.com>"

    # Constant: The default hostname to look up for.
    def _DEF_HOSTNAME, do: "openbsd.org"

    # Helper function. Draws a horizontal separator banner.
    def _separator_draw(banner_text) do
        i = String.length(banner_text)

        for (_ <- i..1), do: IO.write('=')

        IO.puts(_EMPTY_STRING())
    end
end

defmodule Dnsresolvd do
    @moduledoc "The main module of the daemon."

    @doc """
    Performs DNS lookup action for the given hostname,
    i.e. (in this case) IP address retrieval by hostname.

    **Args:**<br />
        `hostname`: The effective hostname to look up for.

    **Returns:**<br />
        The array containing IP address of the analyzing host/service
        and corresponding IP version (family) used to look up in DNS:
        `4` for IPv4-only hosts, `6` for IPv6-capable hosts.
    """
    def dns_lookup(hostname) do
        # TODO: Implement performing DNS lookup action for the given hostname.
        #       This function currently is a dummy thing and hence
        #       it should be populated with the actual code.
        IO.puts("=== " <> hostname)
    end
end

# vim:set nu ts=4 sw=4:
