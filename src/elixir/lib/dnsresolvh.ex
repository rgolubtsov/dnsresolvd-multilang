#
# src/elixir/lib/dnsresolvh.ex
# =============================================================================
# DNS Resolver Daemon (dnsresolvd). Version 0.9.9
# =============================================================================
# A daemon that performs DNS lookups for the given hostname
# passed in an HTTP request, with the focus on its implementation
# using various programming languages. (Cowboy-boosted impl.)
# =============================================================================
# Copyright (C) 2017-2023 Radislav (Radicchio) Golubtsov
#
# (See the LICENSE file at the top of the source tree.)
#

defmodule AUX do
    @moduledoc "The helper module for the daemon."

    # Helper constants.
    def _EXIT_FAILURE    , do:    1 #    Failing exit status.
    def _EXIT_SUCCESS    , do:    0 # Successful exit status.
    def _EMPTY_STRING    , do:   ""
    def _COLON_SPACE_SEP , do: ": "
    def _COMMA_SPACE_SEP , do: ", "
    def _NEW_LINE        , do: "\n"
    def _ONE_SPACE_STRING, do:  " "
    def _PRINT_BANNER_OPT, do: "-V"

    # JSON entities :-).
    def _CB1, do:   "{\""
    def _CB2, do:   "\"}"
    def _DQ1, do: "\":\""
    def _DQ2, do: "\",\""

    # Common error messages.
    def _ERR_PREFIX                   , do: "error"
    def _ERR_PORT_MUST_BE_POSITIVE_INT, do: ": <port_number> must be "
                                        <>  "a positive integer value, "
                                        <>  "in the range 1024-49151."
    def _ERR_CANNOT_START_SERVER      , do: ": FATAL: Cannot start server "
    def _ERR_SRV_UNKNOWN_REASON       , do: "for an unknown reason. "
                                        <>  "Exiting..."
    def _ERR_SRV_PORT_IS_IN_USE       , do: "due to the port requested "
                                        <>  "is in use. Exiting..."
    def _ERR_COULD_NOT_LOOKUP         , do: "could not lookup hostname"

    # Print this error message when there are no any args passed.
    def _ERR_MUST_BE_ONE_TWO_ARGS_1, do: ": There must be one or two args "
                                     <>  "passed: "
    def _ERR_MUST_BE_ONE_TWO_ARGS_2, do: " args found"

    # Print this usage info just after any inappropriate input.
    def _MSG_USAGE_TEMPLATE_1, do: "Usage: "
    def _MSG_USAGE_TEMPLATE_2, do: " <port_number> [-V]"

    ## Constant: The minimum port number allowed.
    def _MIN_PORT, do: 1024

    ## Constant: The maximum port number allowed.
    def _MAX_PORT, do: 49151

    # Common notification messages.
    def _MSG_SERVER_STARTED_1, do: "Server started on port "
    def _MSG_SERVER_STARTED_2, do: "=== Hit Ctrl+\\ to terminate it."

    # HTTP request methods and params.
    def _MTD_HTTP_GET , do: "GET"
    def _MTD_HTTP_POST, do: "POST"
    def _PRM_FMT_HTML , do: "html"
    def _PRM_FMT_JSON , do: "json"

    # HTTP response headers and status codes.
    def _HDR_CONTENT_TYPE_N     , do: "content-type"
    def _HDR_CONTENT_TYPE_V_HTML, do: "text/html; charset=UTF-8"
    def _HDR_CONTENT_TYPE_V_JSON, do: "application/json"
    def _HDR_CACHE_CONTROL_N    , do: "cache-control"
    def _HDR_CACHE_CONTROL_V    , do: "no-cache, no-store, "
                                  <>  "must-revalidate"
    def _HDR_EXPIRES_N          , do: "expires"
    def _HDR_EXPIRES_V          , do: "Thu, 01 Dec 1994 16:00:00 GMT"
    def _HDR_PRAGMA_N           , do: "pragma"
    def _HDR_PRAGMA_V           , do: "no-cache"
    def _RSC_HTTP_200_OK        , do: 200

    # Response data names.
    def _DAT_HOSTNAME_N, do: "hostname"
    def _DAT_ADDRESS_N , do: "address"
    def _DAT_VERSION_N , do: "version"
    def _DAT_VERSION_V , do: "IPv"

    # Daemon name, version, and copyright banners.
    def _DMN_NAME       , do: "DNS Resolver Daemon (dnsresolvd)"
    def _DMN_DESCRIPTION, do: "Performs DNS lookups for the given "
                          <>  "hostname passed in an HTTP request"
    def _DMN_VERSION_S__, do: "Version"
    def _DMN_VERSION    , do: "0.9.9"
    def _DMN_COPYRIGHT__, do: "Copyright (C) 2017-2023"
    def _DMN_AUTHOR     , do: "Radislav Golubtsov <radicchio@vk.com>"

    ## Constant: The default hostname to look up for.
    def _DEF_HOSTNAME, do: "openbsd.org"

    @doc """
    Adds headers to the response.

    **Args:**<br />
        `fmt`: The response format selector.
        `req`: The consolidated HTTP request/response object.

    **Returns:**<br />
        The new consolidated HTTP request/response object.
    """
    def _add_response_headers(fmt, req) do
        hDR_CONTENT_TYPE_V = if (fmt === _PRM_FMT_HTML()) do
            _HDR_CONTENT_TYPE_V_HTML()
        else                 if (fmt === _PRM_FMT_JSON()) do
            _HDR_CONTENT_TYPE_V_JSON()
        end
            # --- Block-separator-prettifier ---
        end

        :cowboy_req.set_resp_headers(%{
            _HDR_CONTENT_TYPE_N()  =>  hDR_CONTENT_TYPE_V,
            _HDR_CACHE_CONTROL_N() => _HDR_CACHE_CONTROL_V(),
            _HDR_EXPIRES_N()       => _HDR_EXPIRES_V(),
            _HDR_PRAGMA_N()        => _HDR_PRAGMA_V()
        }, req)
    end

    # Helper function. Makes final buffer cleanups, closes streams, etc.
    def _cleanups_fixate(log) do
        # Closing the system logger.
        if (log !== nil) do
            # ----- Calling Erlang -----+---+
            :syslog.close(log) # <------+   |
            :syslog.stop()     # <----------+
        end
    end

    # Helper function. Draws a horizontal separator banner.
    def _separator_draw(banner_text) do
        i = String.length(banner_text)

        for (_ <- i..1), do: IO.write('=')

        IO.puts(_EMPTY_STRING())
    end
end

# vim:set nu et ts=4 sw=4:
