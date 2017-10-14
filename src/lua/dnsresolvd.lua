#!/usr/bin/env luvit
--[[ content/dev/misc/dnsresolvd/lua/dnsresolvd.lua
 * ============================================================================
 * DNS Resolver Daemon (dnsresolvd). Version 0.1
 * ============================================================================
 * A Luvit (Lua)-boosted daemon for performing DNS lookups.
 * ============================================================================
 * Copyright (C) 2017 Radislav (Radicchio) Golubtsov
 *
 * (See the LICENSE file at the top of the source tree.)
--]]

local path = require("path")
local http = require("http")
local url  = require("url")
local dns  = require("dns")

local aux  = require("dnsresolvd_h")

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
     * @param req  The HTTP request object.
     * @param resp The HTTP response object.
     *
     * @return The HTTP server object.
    --]]
    local daemon = http.createServer(function(req, resp)
        -- Parsing and validating query params.
        local query = url.parse(req.url, true).query

        -- http://localhost:<port_number>/?h=<hostname>
        --                                 |
        local hostname = query.h -- <------+

        if (hostname == nil) then
            hostname = aux._DEF_HOSTNAME
        end

        --[[
         * Performing DNS lookup for the given hostname
         * and writing the response out.
         *
         * @param hostname The effective hostname to look up for.
         * @param e        The Error object (if any error occurs).
         * @param addr     The IP address retrieved.
         * @param ver      The IP version (family) used to look up in DNS.
        --]]
--      dns.lookup(hostname, function(e, addr, ver)
        --- Debug vars - Begin ------------------------------------------------
            local e    = nil
            local addr = "129.128.5.194"
            local ver  = 4
        --- Debug vars - End --------------------------------------------------

            local resp_buffer = "<!DOCTYPE html>"                                                                               .. aux._NEW_LINE
.. "<html lang=\"en-US\" dir=\"ltr\">" .. aux._NEW_LINE .. "<head>"                                                             .. aux._NEW_LINE
.. "<meta http-equiv=\"Content-Type\"    content=\"" .. aux._HDR_CONTENT_TYPE .. "\" />"                                        .. aux._NEW_LINE
.. "<meta http-equiv=\"X-UA-Compatible\" content=\"IE=edge,chrome=1\" />"                                                       .. aux._NEW_LINE
.. "<!-- No caching at all for:                                                                       -->"                      .. aux._NEW_LINE
.. "<meta http-equiv=\"Cache-Control\"   content=\"" .. aux._HDR_CACHE_CONTROL .. "\" /> <!-- HTTP/1.1 -->"                     .. aux._NEW_LINE
.. "<meta http-equiv=\"Expires\"         content=\"" .. aux._HDR_EXPIRES .. "\"       /> <!-- Proxies  -->"                     .. aux._NEW_LINE
.. "<meta http-equiv=\"Pragma\"          content=\"" .. aux._HDR_PRAGMA .. "\"                            /> <!-- HTTP/1.0 -->" .. aux._NEW_LINE
.. "<meta       name=\"viewport\"        content=\"width=device-width,initial-scale=1\" />"                                     .. aux._NEW_LINE
.. "<meta       name=\"description\"     content=\"" .. aux._DMN_DESCRIPTION .. "\" />"                                         .. aux._NEW_LINE
.. "<title>" .. aux._DMN_NAME .. "</title>" .. aux._NEW_LINE .. "</head>"                                                       .. aux._NEW_LINE
.. "<body id=\"dnsresolvd\">"             .. aux._NEW_LINE .. "<p>"
.. hostname .. " ==&gt; "

            if (e ~= nil) then
                resp_buffer = resp_buffer .. aux._ERR_PREFIX
                                          .. aux._COLON_SPACE_SEP
                                          .. aux._ERR_COULD_NOT_LOOKUP
            else
                resp_buffer = resp_buffer .. addr .. " (IPv" .. ver .. ")"
            end

            resp_buffer = resp_buffer .. "</p>"    .. aux._NEW_LINE
                                      .. "</body>" .. aux._NEW_LINE
                                      .. "</html>" .. aux._NEW_LINE

            -- Adding headers to the response.
            resp:writeHead(aux._RSC_HTTP_200_OK, {
                ["Content-Type"]  = aux._HDR_CONTENT_TYPE,
                ["Cache-Control"] = aux._HDR_CACHE_CONTROL,
                ["Expires"]       = aux._HDR_EXPIRES,
                ["Pragma"]        = aux._HDR_PRAGMA
            })

            -- Writing the response out.
            resp:write(resp_buffer)

            -- Closing the response stream.
            resp:finish()
--      end)
    end):listen(port_number)

    -- FIXME: Ported one-to-one from Node.js impl. Not working.
    daemon:on(aux._EVE_ERROR, function(e)
        ret = aux._EXIT_FAILURE

        if (e.code == aux._ERR_EADDRINUSE) then
            print(daemon_name .. aux._ERR_CANNOT_START_SERVER
                              .. aux._ERR_SRV_PORT_IS_IN_USE
                              .. aux._NEW_LINE)
        else
            print(daemon_name .. aux._ERR_CANNOT_START_SERVER
                              .. aux._ERR_SRV_UNKNOWN_REASON
                              .. aux._NEW_LINE)
        end

        _cleanups_fixate()

        return ret
    end)

    -- FIXME: Ported one-to-one from Node.js impl. Not working.
    daemon:on(aux._EVE_LISTENING, function(e)
        print(aux._MSG_SERVER_STARTED_1 .. port_number .. aux._NEW_LINE
           .. aux._MSG_SERVER_STARTED_2)
    end)

    print(aux._MSG_SERVER_STARTED_1 .. port_number .. aux._NEW_LINE
       .. aux._MSG_SERVER_STARTED_2)

    return ret
end

-- Helper function. Makes final buffer cleanups, closes streams, etc.
local _cleanups_fixate = function()
    -- TODO: Implement cleanup stuff.
end

-- Helper function. Draws a horizontal separator banner.
local _separator_draw = function(banner_text)
    local i = banner_text:len()

    while (i > 0) do
        process.stdout:write('='); i = i - 1
    end

    print()
end

-- The daemon entry point.
local main = function(argc, argv)
    local ret = aux._EXIT_SUCCESS

    local daemon_name = path.basename(argv[1])
    local port_number = tonumber(argv[2], 10)

--  _separator_draw(aux._DMN_DESCRIPTION)

    print(aux._DMN_NAME         .. aux._COMMA_SPACE_SEP .. aux._DMN_VERSION_S__
       .. aux._ONE_SPACE_STRING .. aux._DMN_VERSION      .. aux._NEW_LINE
       .. aux._DMN_DESCRIPTION                           .. aux._NEW_LINE
       .. aux._DMN_COPYRIGHT__  .. aux._ONE_SPACE_STRING .. aux._DMN_AUTHOR)

--  _separator_draw(aux._DMN_DESCRIPTION)

    -- Checking for args presence.
    if (argc ~= 2) then
        ret = aux._EXIT_FAILURE

        print(daemon_name .. aux._ERR_MUST_BE_THE_ONLY_ARG_1
            .. (argc - 1) .. aux._ERR_MUST_BE_THE_ONLY_ARG_2
            .. aux._NEW_LINE)

        print(aux._MSG_USAGE_TEMPLATE_1 .. daemon_name
           .. aux._MSG_USAGE_TEMPLATE_2 .. aux._NEW_LINE)

        _cleanups_fixate()

        return ret
    end

    -- Checking for port correctness.
    if ((port_number == nil) or (port_number < aux._MIN_PORT)
                             or (port_number > aux._MAX_PORT)) then

        ret = aux._EXIT_FAILURE

        print(daemon_name .. aux._ERR_PORT_MUST_BE_POSITIVE_INT
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

-- vim:set nu:et:ts=4:sw=4:
