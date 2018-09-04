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

defmodule DnsResolvd do
    @moduledoc "The main Application module of the daemon."

    use Application

    @doc """
    Starts up the daemon.

    **Args:**<br />
        `args`: A list containing the server port number to listen on
                as the first element.

    **Returns:**<br />
        The server exit code when interrupted.
    """
    def start(_, args) do
        [port_number | _] = args

        # Using this call instead of passing "--app dnsresolvd"
        # to the Elixir interpreter. See the file "dnsresolvd.app"
        # for explanation.
        {:ok, _} = Application.ensure_all_started(:cowboy)

        dispatch = :cowboy_router.compile([
            {:_, [
                {'/', :cowboy_static, []}
            ]}
        ])

        # Starting up the plain HTTP listener on <port_number>.
        {:ok, _} = :cowboy.start_clear(:http_listener, [
            port: port_number
        ], %{
            env: [dispatch: dispatch]
        })

        # Starting up the daemon's provided Supervisor (optional).
        DnsResolvs.start_link()

        # Using this call instead of passing "--no-halt"
        # to the Elixir interpreter. See the file "dnsresolvd.app"
        # for explanation.
        Process.sleep(:infinity)
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

defmodule DnsResolvs do
    @moduledoc "The main Supervisor module of the daemon."

    use Supervisor

    def start_link() do
        Supervisor.start_link(__MODULE__, [])
    end

    def init(_) do
        child_procs = [] # <== No child processes do we need.

        Supervisor.init(child_procs, strategy: :one_for_one)
    end
end

# vim:set nu ts=4 sw=4:
