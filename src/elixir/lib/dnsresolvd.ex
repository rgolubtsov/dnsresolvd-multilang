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
    Starts up the daemon.<br />
    It has to be the application module callback, but used directly
    from the startup script of the daemon.

    **Args:**<br />
        `args`: A list containing the server port number to listen on
                as the first element.

    **Returns:**<br />
        The tuple containing the PID of the top supervisor process
        and the state of the running application (or an empty list).
    """
    @impl (true)
    def start(_, args) do
        [port_number | _] = args

        # Using this call instead of passing "--app dnsresolvd"
        # to the Elixir interpreter. See the file "dnsresolvd.app"
        # for explanation.
        {:ok, _} = Application.ensure_all_started(:cowboy)

        dispatch = :cowboy_router.compile([
            {:_, [
                {'/', :cowboy_static, {:file, 'Makefile'}}
            ]}
        ])

        # Starting up the plain HTTP listener on <port_number>.
        {:ok, _} = :cowboy.start_clear(:http_listener, [
            port: port_number
        ], %{
            env: %{:dispatch => dispatch}
        })

        # Starting up the daemon's provided Supervisor (optional).
        DnsResolvs.start_link()

        # Trapping exit signals, i.e. transforming them into {:EXIT} message.
        Process.flag(:trap_exit, true)

        # Inspecting the daemon's Application process message queue
        # for the incoming {:EXIT} message, i.e. doing same things
        # as the Process.sleep(:infinity) inactive call below
        # until the message received.
        receive do
            {:EXIT, from, reason} ->
                IO.inspect(from)
                IO.inspect(reason)
        end

        # --- Use message queue (as above) instead of sleep() call below ------
        # Using this call instead of passing "--no-halt"
        # to the Elixir interpreter. See the file "dnsresolvd.app"
        # for explanation.
        # Process.sleep(:infinity)
        # ---------------------------------------------------------------------
    end

    ###
    # Performs DNS lookup action for the given hostname,
    # i.e. (in this case) IP address retrieval by hostname.
    #
    # Args:
    #     hostname: The effective hostname to look up for.
    #
    # Returns:
    #     The array containing IP address of the analyzing host/service
    #     and corresponding IP version (family) used to look up in DNS:
    #     "4" for IPv4-only hosts, "6" for IPv6-capable hosts.
    #
    defp dns_lookup(hostname) do
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

    @doc """
    Supervisor startup helper.<br />
    Used to directly start up the `DnsResolvs` supervisor process.

    **Returns:**<br />
        The PID of the `DnsResolvs` supervisor process.
    """
    def start_link() do
        Supervisor.start_link(__MODULE__, [])
    end

    @doc """
    Supervisor init/1 callback.<br />
    Gets called when the `DnsResolvs` supervisor is about to be started up.

    **Returns:**<br />
        The tuple containing supervisor flags
        and child processes' specifications.
    """
    @impl (true)
    def init(_) do
        child_procs = [] # <== No child processes do we need.

        Supervisor.init(child_procs, strategy: :one_for_one)
    end
end

# vim:set nu ts=4 sw=4:
