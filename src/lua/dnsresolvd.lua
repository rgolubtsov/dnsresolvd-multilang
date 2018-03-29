#!/usr/bin/env luvit
--[[ src/lua/dnsresolvd.lua
 * ============================================================================
 * DNS Resolver Daemon (dnsresolvd). Version 0.1
 * ============================================================================
 * A daemon that performs DNS lookups for the given hostname
 * passed in an HTTP request, with the focus on its implementation
 * using various programming languages. (Luvit-boosted impl.)
 * ============================================================================
 * Copyright (C) 2017-2018 Radislav (Radicchio) Golubtsov
 *
 * (See the LICENSE file at the top of the source tree.)
--]]

local path  = require("path" )
local http  = require("http" )
local url   = require("url"  )
local dns   = require("dns"  )
local json  = require("json" )
local posix = require("posix")

local aux = require("dnsresolvh")

--[[
 * Performs DNS lookup action for the given hostname,
 * i.e. (in this case) IP address retrieval by hostname.
 *
 * It first prepares and runs the server instance, then does all the rest.
 *
 * @param _ret        The status code passed in from the caller.
 * @param port_number The server port number to listen on.
 * @param daemon_name The daemon name (executable/script name).
 *
 * @return The status code indicating the daemon overall execution outcome.
--]]
local dns_lookup = function(_ret, port_number, daemon_name)
    local ret = _ret

    --[[
     * Creating, configuring, and starting the server.
     *
     * @param req  The HTTP request  object.
     * @param resp The HTTP response object.
     *
     * @return The HTTP server object.
    --]]
    local daemon = http.createServer(function(req, resp)
        local params
        local hostname
        local fmt

        -- Parsing and validating request params.
            if (req.method == aux._MTD_HTTP_GET ) then
            params = _request_params_parse(req.url)

            hostname = params.h
            fmt      = params.f

            --[[
             * Calling the lookup wrapper for GET requests:
             * all the logic is implemented there.
            --]]
            dns_lookup_wrapper(hostname, fmt, resp)
        elseif (req.method == aux._MTD_HTTP_POST) then
            req:on(aux._EVE_DATA, function(body)
                params = _request_params_parse(aux._QUESTION_MARK .. body)
            end)

            req:on(aux._EVE_END, function()
                hostname = params.h
                fmt      = params.f

                --[[
                 * Calling the lookup wrapper for POST requests:
                 * all the logic is implemented there.
                --]]
                dns_lookup_wrapper(hostname, fmt, resp)
            end)
        end
    end):listen(port_number)

    daemon:on(aux._EVE_ERROR, function(e)
        ret = aux._EXIT_FAILURE

        if (e == aux._ERR_EADDRINUSE) then
            print(daemon_name .. aux._ERR_CANNOT_START_SERVER
                              .. aux._ERR_SRV_PORT_IS_IN_USE
                              .. aux._NEW_LINE)

            posix.syslog(posix.LOG_ERR,
                  daemon_name .. aux._ERR_CANNOT_START_SERVER
                              .. aux._ERR_SRV_PORT_IS_IN_USE
                              .. aux._NEW_LINE)
        else
            print(daemon_name .. aux._ERR_CANNOT_START_SERVER
                              .. aux._ERR_SRV_UNKNOWN_REASON
                              .. aux._NEW_LINE)

            posix.syslog(posix.LOG_ERR,
                  daemon_name .. aux._ERR_CANNOT_START_SERVER
                              .. aux._ERR_SRV_UNKNOWN_REASON
                              .. aux._NEW_LINE)
        end

        _cleanups_fixate()

        return ret
    end)

    daemon:on(aux._EVE_LISTENING, function(e)
        print(aux._MSG_SERVER_STARTED_1 .. port_number .. aux._NEW_LINE
           .. aux._MSG_SERVER_STARTED_2)

        posix.syslog(posix.LOG_INFO,          aux._MSG_SERVER_STARTED_1
           .. port_number .. aux._NEW_LINE .. aux._MSG_SERVER_STARTED_2)
    end)

    daemon_address = {daemon:address()}

    if (not daemon_address[1]) then
        if (daemon_address[3] == aux._ERR_EADDRINUSE) then
            daemon:emit(aux._EVE_ERROR, aux._ERR_EADDRINUSE)
        else
            daemon:emit(aux._EVE_ERROR)
        end
    else
        daemon:emit(aux._EVE_LISTENING)
    end

    return ret
end

--[[
 * Wraps writing the response with the DNS record retrieved.
 *
 * @param hostname The effective hostname to look up for.
 * @param fmt      The response format selector.
 * @param resp     The HTTP response object.
--]]
dns_lookup_wrapper = function(hostname, fmt, resp)
    --[[
     * Writes the response with the DNS record retrieved.
     *
     * @param hostname The effective hostname to look up for.
     * @param fmt      The response format selector.
     * @param e        The Error object (if any error occurs).
     * @param rec      The DNS record retrieved.
     * @param resp     The HTTP response object.
    --]]
    local resp_write = function(hostname, fmt, e, rec, resp)
        local resp_buffer

        if (fmt == aux._PRM_FMT_HTML) then
            resp_buffer = "<!DOCTYPE html>"                                                  .. aux._NEW_LINE
.. "<html lang=\"en-US\" dir=\"ltr\">"                                                       .. aux._NEW_LINE
.. "<head>"                                                                                  .. aux._NEW_LINE
.. "<meta http-equiv=\""      .. aux._HDR_CONTENT_TYPE_N      ..          "\"    content=\""
                              .. aux._HDR_CONTENT_TYPE_V_HTML ..          "\"           />"  .. aux._NEW_LINE
.. "<meta http-equiv=\"X-UA-Compatible\" content=\"IE=edge\"                            />"  .. aux._NEW_LINE
.. "<meta       name=\"viewport\"        content=\"width=device-width,initial-scale=1\" />"  .. aux._NEW_LINE
.. "<title>" .. aux._DMN_NAME .. "</title>"                                                  .. aux._NEW_LINE
.. "</head>"                                                                                 .. aux._NEW_LINE
.. "<body>"                                                                                  .. aux._NEW_LINE
.. "<div>"   .. hostname      .. aux._ONE_SPACE_STRING
        end

        if (e) then
                if (fmt == aux._PRM_FMT_HTML) then
                resp_buffer = resp_buffer .. aux._ERR_PREFIX
                                          .. aux._COLON_SPACE_SEP
                                          .. aux._ERR_COULD_NOT_LOOKUP
            elseif (fmt == aux._PRM_FMT_JSON) then
                resp_buffer = json.encode({
                    [aux._DAT_HOSTNAME_N] = hostname,
                    [aux._ERR_PREFIX    ] = aux._ERR_COULD_NOT_LOOKUP,
                })
            end
        else
            if (#rec == 0) then
                ret = aux._EXIT_FAILURE

                return ret
            else
                local addr = rec[1].address --> The IP address for host.
                local ver  = rec[1].type    --> The IP version (family).

                    if (ver == dns.TYPE_A   ) then
                    ver = 4
                elseif (ver == dns.TYPE_AAAA) then
                    ver = 6
                end

                    if (fmt == aux._PRM_FMT_HTML) then
                    resp_buffer = resp_buffer .. addr
                                              .. aux._ONE_SPACE_STRING
                                              .. aux._DAT_VERSION_V
                                              .. ver
                elseif (fmt == aux._PRM_FMT_JSON) then
                    resp_buffer = json.encode({
                        [aux._DAT_HOSTNAME_N] = hostname,
                        [aux._DAT_ADDRESS_N ] = addr,
                        [aux._DAT_VERSION_N ] = aux._DAT_VERSION_V .. ver,
                    })
                end
            end
        end

        if (fmt == aux._PRM_FMT_HTML) then
            resp_buffer = resp_buffer .. "</div>"  .. aux._NEW_LINE
                                      .. "</body>" .. aux._NEW_LINE
                                      .. "</html>" .. aux._NEW_LINE
        end

        -- Adding headers to the response.
        local HDR_CONTENT_TYPE_V

            if (fmt == aux._PRM_FMT_HTML) then
            HDR_CONTENT_TYPE_V = aux._HDR_CONTENT_TYPE_V_HTML
        elseif (fmt == aux._PRM_FMT_JSON) then
            HDR_CONTENT_TYPE_V = aux._HDR_CONTENT_TYPE_V_JSON
        end

        resp:writeHead(aux._RSC_HTTP_200_OK, {
            [aux._HDR_CONTENT_TYPE_N ] =      HDR_CONTENT_TYPE_V,
            [aux._HDR_CACHE_CONTROL_N] = aux._HDR_CACHE_CONTROL_V,
            [aux._HDR_EXPIRES_N      ] = aux._HDR_EXPIRES_V,
            [aux._HDR_PRAGMA_N       ] = aux._HDR_PRAGMA_V,
        })

        -- Writing the response out.
        resp:write(resp_buffer)

        -- Closing the response stream.
        resp:finish()
    end

    --[[
     * Performing DNS lookup for the given hostname
     * and writing the response out.
     *
     * Since Luvit actually doesn't have the dns.lookup() method,
     * it needs to perform a so-called two-stage lookup operation:
     *
     *     If the host doesn't have the A record (IPv4),
     *     trying to find its AAAA record (IPv6).
    --]]
    dns.resolve4        (hostname, function(e, rec      )
        ret = resp_write(hostname, fmt,     e, rec, resp)

        if (ret == aux._EXIT_FAILURE) then
            dns.resolve6  (hostname, function(e, rec      )
                resp_write(hostname, fmt,     e, rec, resp)
            end)
        end
    end)
end

-- Helper function. Parses and validates request params.
_request_params_parse = function(url_or_body)
    local query = url.parse(url_or_body, true).query

    local hostname = query.h -- <------+
    --                                 |
    -- http://localhost:<port_number>/?h=<hostname>&f=<fmt>
    --                                              |
    local fmt      = query.f -- <-------------------+

    if (not hostname) then
        hostname = aux._DEF_HOSTNAME
    end

    if (not fmt) then
        fmt = aux._PRM_FMT_JSON
    else
        local fmt_ = {
            aux._PRM_FMT_HTML,
            aux._PRM_FMT_JSON,
        }

         fmt = fmt:lower()
        _fmt = false

        for i = 1, #fmt_ do
            if (fmt == fmt_[i]) then
                _fmt = true

                break
            end
        end

        if (not _fmt) then
            fmt = aux._PRM_FMT_JSON
        end
    end

    return {
        h = hostname,
        f = fmt,
    }
end

-- Helper function. Makes final buffer cleanups, closes streams, etc.
_cleanups_fixate = function()
    -- Closing the system logger.
    posix.closelog()
end

-- Helper function. Draws a horizontal separator banner.
_separator_draw = function(banner_text)
    local i = banner_text:len()
    local s = aux._EMPTY_STRING

    repeat s = s .. '=' i = i - 1 until (i == 0) print(s)
end

-- The daemon entry point.
local main = function(argc, argv)
    local ret = aux._EXIT_SUCCESS

    local daemon_name = path.basename(argv[1])
    local port_number = tonumber(argv[2], 10)

    -- Opening the system logger.
    posix.openlog(path.basename(daemon_name,  aux._LOG_DAEMON_EXT),
                  {cons = true, pid = true}, posix.LOG_DAEMON)

    local print_banner_opt = aux._EMPTY_STRING

    if (argc > 2) then
        print_banner_opt = argv[3]
    end

    if (print_banner_opt == aux._PRINT_BANNER_OPT) then
        _separator_draw(aux._DMN_DESCRIPTION)

        print(aux._DMN_NAME     .. aux._COMMA_SPACE_SEP .. aux._DMN_VERSION_S__
           .. aux._ONE_SPACE_STRING .. aux._DMN_VERSION     .. aux._NEW_LINE
           .. aux._DMN_DESCRIPTION                          .. aux._NEW_LINE
           .. aux._DMN_COPYRIGHT__ .. aux._ONE_SPACE_STRING .. aux._DMN_AUTHOR)

        _separator_draw(aux._DMN_DESCRIPTION)
    end

    -- Checking for args presence.
    if (argc == 1) then
        ret = aux._EXIT_FAILURE

        print(daemon_name .. aux._ERR_MUST_BE_ONE_TWO_ARGS_1
            .. (argc - 1) .. aux._ERR_MUST_BE_ONE_TWO_ARGS_2
            .. aux._NEW_LINE)

        posix.syslog(posix.LOG_ERR,
              daemon_name .. aux._ERR_MUST_BE_ONE_TWO_ARGS_1
            .. (argc - 1) .. aux._ERR_MUST_BE_ONE_TWO_ARGS_2
            .. aux._NEW_LINE)

        print(aux._MSG_USAGE_TEMPLATE_1 .. daemon_name
           .. aux._MSG_USAGE_TEMPLATE_2 .. aux._NEW_LINE)

        _cleanups_fixate()

        return ret
    end

    -- Checking for port correctness.
    if ((not port_number) or (port_number < aux._MIN_PORT)
                          or (port_number > aux._MAX_PORT)) then

        ret = aux._EXIT_FAILURE

        print(daemon_name .. aux._ERR_PORT_MUST_BE_POSITIVE_INT
           .. aux._NEW_LINE)

        posix.syslog(posix.LOG_ERR,
              daemon_name .. aux._ERR_PORT_MUST_BE_POSITIVE_INT
           .. aux._NEW_LINE)

        print(aux._MSG_USAGE_TEMPLATE_1 .. daemon_name
           .. aux._MSG_USAGE_TEMPLATE_2 .. aux._NEW_LINE)

        _cleanups_fixate()

        return ret
    end

    --[[
     * Preparing and running the server instance,
     * then making DNS lookup upon request
     * for the hostname provided.
    --]]
    ret = dns_lookup(ret, port_number, daemon_name)

    -- Making final cleanups.
    _cleanups_fixate()

    return ret
end

local argv = process.argv
local argc = #argv

-- Starting up the daemon.
local ret = main(argc, argv)

process.exitCode = ret

-- vim:set nu et ts=4 sw=4:
