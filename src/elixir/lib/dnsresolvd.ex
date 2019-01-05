#
# src/elixir/lib/dnsresolvd.ex
# =============================================================================
# DNS Resolver Daemon (dnsresolvd). Version 0.1
# =============================================================================
# A daemon that performs DNS lookups for the given hostname
# passed in an HTTP request, with the focus on its implementation
# using various programming languages. (Cowboy-boosted impl.)
# =============================================================================
# Copyright (C) 2017-2019 Radislav (Radicchio) Golubtsov
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
        {port_number, daemon_name, log} = args

        # Using this call instead of passing "--app dnsresolvd"
        # to the Elixir interpreter. See the file "dnsresolvd.app"
        # for explanation.
        {:ok, _} = Application.ensure_all_started(:cowboy)

        dispatch = :cowboy_router.compile([
            {:_, [
                {'/', ReqHandler, []}
            ]}
        ])

        # Starting up the plain HTTP listener on <port_number>.
        ret_ = :cowboy.start_clear(:http_listener, [
            port: port_number
        ], %{
            env: %{:dispatch => dispatch}
        })

        # Handling errors during start up of the listener.
        if (elem(ret_, 0) === :error) do
            ret = AUX._EXIT_FAILURE

            if (elem(ret_, 1) === :eaddrinuse) do
                IO.puts(:stderr, daemon_name <> AUX._ERR_CANNOT_START_SERVER
                                             <> AUX._ERR_SRV_PORT_IS_IN_USE
                                             <> AUX._NEW_LINE)

                :syslog.log(log, :err,
                                 daemon_name <> AUX._ERR_CANNOT_START_SERVER
                                             <> AUX._ERR_SRV_PORT_IS_IN_USE
                                             <> AUX._NEW_LINE)
            else
                IO.puts(:stderr, daemon_name <> AUX._ERR_CANNOT_START_SERVER
                                             <> AUX._ERR_SRV_UNKNOWN_REASON
                                             <> AUX._NEW_LINE)

                :syslog.log(log, :err,
                                 daemon_name <> AUX._ERR_CANNOT_START_SERVER
                                             <> AUX._ERR_SRV_UNKNOWN_REASON
                                             <> AUX._NEW_LINE)
            end

            AUX._cleanups_fixate(log)

            System.halt(ret)
        end

        IO.puts(AUX._MSG_SERVER_STARTED_1
             <> to_string(port_number) <> AUX._NEW_LINE
             <> AUX._MSG_SERVER_STARTED_2)

        :syslog.log(log, :info,
                AUX._MSG_SERVER_STARTED_1
             <> to_string(port_number) <> AUX._NEW_LINE
             <> AUX._MSG_SERVER_STARTED_2)

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
end

defmodule ReqHandler do
    @moduledoc "The default HTTP request handler."

    @doc """
    The request handler `init/2` callback.<br />
    Gets called when a new incoming HTTP request is received.

    **Args:**<br />
        `req`:   The incoming HTTP request object.
        `state`: The initial state of the HTTP handler.

    **Returns:**<br />
        The tuple containing the HTTP response to be rendered
        and a new state of the HTTP handler.
    """
    def init(req, state) do
        mtd = :cowboy_req.method(req)

        # ---------------------------------------------------------------------
        # --- Parsing and validating request params - Begin -------------------
        # ---------------------------------------------------------------------
        {:ok, params, req} = cond do
            (mtd === AUX._MTD_HTTP_GET ) ->
                {:ok, :cowboy_req.parse_qs(req), req}
            (mtd === AUX._MTD_HTTP_POST) ->
                :cowboy_req.read_urlencoded_body(req)
        end

        hostname = for (param <- params) do
            {k, v} = param; if (k === "h"), do: v # <---+-----------------+
        end #     +----GET----+-----+-----+             |                 |
        #         |     |     |     |     |       +-----+      +----------+-+
        #         v     v     v     v     v       |            |          | |
        # $ curl 'http://localhost:<port_number>/?h=<hostname>&f=<fmt>'   | |
        # $                                                               | |
        # $ curl -d 'h=<hostname>&f=<fmt>' http://localhost:<port_number> | |
        #         ^  |            |                                       | |
        #         |  +------------+---------------------------------------+ |
        #         |               |                                         |
        # POST----+               +---------------------+                   |
        #                                               |                   |
        fmt      = for (param <- params) do #           |                   |
            {k, v} = param; if (k === "f"), do: v # <---+-------------------+
        end

        hostname = hostname |> Enum.filter(fn(v) -> (v !== nil) end)
        hostname = hostname |> Enum.at(length(hostname) - 1)

        fmt      = fmt      |> Enum.filter(fn(v) -> (v !== nil) end)
        fmt      = fmt      |> Enum.at(length(fmt     ) - 1)

        hostname = if ((              hostname  === nil )
                   or  (              hostname  === true)
                   or  (String.length(hostname) === 0   )) do
            AUX._DEF_HOSTNAME
        else
            hostname
        end

        fmt      = if ((              fmt       === nil )
                   or  (              fmt       === true)
                   or  (String.length(fmt     ) === 0   )) do
            AUX._PRM_FMT_JSON
        else
            String.downcase(fmt, :ascii)
        end

        # This if-construct is almost identically ported
        # from its Pythonic equivalent ! :-)
        fmt = if (fmt not in [
            AUX._PRM_FMT_HTML,
            AUX._PRM_FMT_JSON,
        ]) do
            AUX._PRM_FMT_JSON
        else
            fmt
        end
        # ---------------------------------------------------------------------
        # --- Parsing and validating request params - End ---------------------
        # ---------------------------------------------------------------------

        # Performing DNS lookup for the given hostname.
        addr_ver = dns_lookup(hostname)

        addr = elem(addr_ver, 0)
        ver  = elem(addr_ver, 1)

        resp_buffer = if (fmt === AUX._PRM_FMT_HTML) do
            "<!DOCTYPE html>"                                                                <> AUX._NEW_LINE
<> "<html lang=\"en-US\" dir=\"ltr\">"                                                       <> AUX._NEW_LINE
<> "<head>"                                                                                  <> AUX._NEW_LINE
<> "<meta http-equiv=\""      <> AUX._HDR_CONTENT_TYPE_N      <>          "\"    content=\""
                              <> AUX._HDR_CONTENT_TYPE_V_HTML <>          "\"           />"  <> AUX._NEW_LINE
<> "<meta http-equiv=\"X-UA-Compatible\" content=\"IE=edge\"                            />"  <> AUX._NEW_LINE
<> "<meta       name=\"viewport\"        content=\"width=device-width,initial-scale=1\" />"  <> AUX._NEW_LINE
<> "<title>" <> AUX._DMN_NAME <> "</title>"                                                  <> AUX._NEW_LINE
<> "</head>"                                                                                 <> AUX._NEW_LINE
<> "<body>"                                                                                  <> AUX._NEW_LINE
<> "<div>"   <> hostname      <> AUX._ONE_SPACE_STRING
        else          if (fmt === AUX._PRM_FMT_JSON) do
               AUX._CB1
            <> AUX._DAT_HOSTNAME_N
            <> AUX._DQ1
            <> hostname
            <> AUX._DQ2
        end
            # --- Block-separator-prettifier ---
        end

        # If lookup error occurred.
        resp_buffer = if (addr === AUX._ERR_PREFIX) do
                 if (fmt === AUX._PRM_FMT_HTML) do
                resp_buffer <> AUX._ERR_PREFIX
                            <> AUX._COLON_SPACE_SEP
                            <> AUX._ERR_COULD_NOT_LOOKUP
            else if (fmt === AUX._PRM_FMT_JSON) do
                resp_buffer <> AUX._ERR_PREFIX
                            <> AUX._DQ1
                            <> AUX._ERR_COULD_NOT_LOOKUP
            end
                # --- Block-separator-prettifier ---
            end
        else
                 if (fmt === AUX._PRM_FMT_HTML) do
                resp_buffer <> to_string(addr)
                            <> AUX._ONE_SPACE_STRING
                            <> AUX._DAT_VERSION_V
                            <> to_string(ver )
            else if (fmt === AUX._PRM_FMT_JSON) do
                resp_buffer <> AUX._DAT_ADDRESS_N
                            <> AUX._DQ1
                            <> to_string(addr)
                            <> AUX._DQ2
                            <> AUX._DAT_VERSION_N
                            <> AUX._DQ1
                            <> AUX._DAT_VERSION_V
                            <> to_string(ver )
            end
                # --- Block-separator-prettifier ---
            end
        end

        resp_buffer = if (fmt === AUX._PRM_FMT_HTML) do
            resp_buffer <> "</div>"  <> AUX._NEW_LINE
                        <> "</body>" <> AUX._NEW_LINE
                        <> "</html>" <> AUX._NEW_LINE
        else          if (fmt === AUX._PRM_FMT_JSON) do
            resp_buffer <> AUX._CB2
        end
            # --- Block-separator-prettifier ---
        end

        # Adding headers to the response.
        req = AUX._add_response_headers(fmt, req)

        req = :cowboy_req.set_resp_body(resp_buffer , req)
        req = :cowboy_req.reply(AUX._RSC_HTTP_200_OK, req)

        {:ok,
            req,
            state # <== The state of the handler doesn't need to be changed.
        }
    end

    ###
    # Performs DNS lookup action for the given hostname,
    # i.e. (in this case) IP address retrieval by hostname.
    #
    # Args:
    #     hostname: The effective hostname to look up for.
    #
    # Returns:
    #     The tuple containing IP address of the analyzing host/service
    #     and corresponding IP version (family) used to look up in DNS:
    #     "4" for IPv4-only hosts, "6" for IPv6-capable hosts.
    #
    defp dns_lookup(hostname) do
        hostent4 = :inet.gethostbyname(String.to_charlist(hostname), :inet )

        # If the host doesn't have an A record (IPv4),
        # trying to find its AAAA record (IPv6).
        hostent6 = if (elem(hostent4, 0) === :error) do
                   :inet.gethostbyname(String.to_charlist(hostname), :inet6)
        end

             if (elem(hostent4, 0) === :ok) do
            {
                elem(hostent4, 1)
                    |> elem(5)
                    |> hd()
                    |> :inet.ntoa(),
                4
            }
        else if (elem(hostent6, 0) === :ok) do
            {
                elem(hostent6, 1)
                    |> elem(5)
                    |> hd()
                    |> :inet.ntoa(),
                6
            }
        else
            {AUX._ERR_PREFIX, nil}
        end
            # --- Block-separator-prettifier ---
        end
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
    Supervisor `init/1` callback.<br />
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

# vim:set nu et ts=4 sw=4:
