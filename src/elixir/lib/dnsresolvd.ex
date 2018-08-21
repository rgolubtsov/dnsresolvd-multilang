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

defmodule Dnsresolvd do
    @moduledoc "The main module of the daemon."

    @doc """
    Starts up the daemon.

    **Args:**<br />
        `port_number`: The server port number to listen on.

    **Returns:**<br />
        The server exit code when interrupted.
    """
    def startup(port_number) do
        dns_lookup(to_string(port_number) <> AUX._ONE_SPACE_STRING
                                          <> AUX._DEF_HOSTNAME)
    end

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

        AUX._EXIT_SUCCESS
    end
end

# vim:set nu ts=4 sw=4:
