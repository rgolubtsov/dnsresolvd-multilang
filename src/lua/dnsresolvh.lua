--[[
 * src/lua/dnsresolvh.lua
 * ============================================================================
 * DNS Resolver Daemon (dnsresolvd). Version 0.9.9
 * ============================================================================
 * A daemon that performs DNS lookups for the given hostname
 * passed in an HTTP request, with the focus on its implementation
 * using various programming languages. (Luvit-boosted impl.)
 * ============================================================================
 * Copyright (C) 2017-2025 Radislav (Radicchio) Golubtsov
 *
 * (See the LICENSE file at the top of the source tree.)
--]]

--[[
 * The helper module for the main daemon module.
 * It aimed at accumulating such pieces like constants, globals, etc.,
 * just like it behaves in its C counterpart.
--]]
local _h = {}
    -- Helper constants.
    _h._EXIT_FAILURE     =    1 --    Failing exit status.
    _h._EXIT_SUCCESS     =    0 -- Successful exit status.
    _h._EMPTY_STRING     =   ""
    _h._ONE_SPACE_STRING =  " "
    _h._COLON_SPACE_SEP  = ": "
    _h._COMMA_SPACE_SEP  = ", "
    _h._NEW_LINE         = "\n"
    _h._PRINT_BANNER_OPT = "-V"
    _h._QUESTION_MARK    =  "?"

    -- Common error messages and codes.
    _h._ERR_PREFIX                    = "error"
    _h._ERR_PORT_MUST_BE_POSITIVE_INT = ": <port_number> must be "
                                     .. "a positive integer value, "
                                     .. "in the range 1024-49151."
    _h._ERR_CANNOT_START_SERVER       = ": FATAL: Cannot start server "
    _h._ERR_SRV_UNKNOWN_REASON        = "for an unknown reason. Exiting..."
    _h._ERR_SRV_PORT_IS_IN_USE        = "due to the port requested "
                                     .. "is in use. Exiting..."
    _h._ERR_COULD_NOT_LOOKUP          = "could not lookup hostname"
    _h._ERR_EADDRINUSE                = "EADDRINUSE"

    -- Print this error message when there are no any args passed.
    _h._ERR_MUST_BE_ONE_TWO_ARGS_1 = ": There must be one or two args passed: "
    _h._ERR_MUST_BE_ONE_TWO_ARGS_2 = " args found"

    -- Print this usage info just after any inappropriate input.
    _h._MSG_USAGE_TEMPLATE_1 = "Usage: "
    _h._MSG_USAGE_TEMPLATE_2 = " <port_number> [-V]"

    -- Syslog-related constants.
    _h._LOG_DAEMON_EXT = ".lua"

    -- Daemon module events.
    _h._EVE_ERROR     = "error"
    _h._EVE_LISTENING = "listening"
    _h._EVE_DATA      = "data"
    _h._EVE_END       = "end"

    -- Constant: The minimum port number allowed.
    _h._MIN_PORT = 1024

    -- Constant: The maximum port number allowed.
    _h._MAX_PORT = 49151

    -- Common notification messages.
    _h._MSG_SERVER_STARTED_1 = "Server started on port "
    _h._MSG_SERVER_STARTED_2 = "=== Hit Ctrl+C to terminate it."

    -- HTTP request methods and params.
    _h._MTD_HTTP_GET  = "GET"
    _h._MTD_HTTP_POST = "POST"
    _h._PRM_FMT_HTML  = "html"
    _h._PRM_FMT_JSON  = "json"

    -- HTTP response headers and status codes.
    _h._HDR_CONTENT_TYPE_N      = "Content-Type"
    _h._HDR_CONTENT_TYPE_V_HTML = "text/html; charset=UTF-8"
    _h._HDR_CONTENT_TYPE_V_JSON = "application/json"
    _h._HDR_CONTENT_LENGTH_N    = "Content-Length"
    _h._HDR_CACHE_CONTROL_N     = "Cache-Control"
    _h._HDR_CACHE_CONTROL_V     = "no-cache, no-store, must-revalidate"
    _h._HDR_EXPIRES_N           = "Expires"
    _h._HDR_EXPIRES_V           = "Thu, 01 Dec 1994 16:00:00 GMT"
    _h._HDR_PRAGMA_N            = "Pragma"
    _h._HDR_PRAGMA_V            = "no-cache"
    _h._RSC_HTTP_200_OK         = 200

    -- Response data names.
    _h._DAT_HOSTNAME_N = "hostname"
    _h._DAT_ADDRESS_N  = "address"
    _h._DAT_VERSION_N  = "version"
    _h._DAT_VERSION_V  = "IPv"

    -- Daemon name, version, and copyright banners.
    _h._DMN_NAME        = "DNS Resolver Daemon (dnsresolvd)"
    _h._DMN_DESCRIPTION = "Performs DNS lookups for the given hostname "
                       .. "passed in an HTTP request"
    _h._DMN_VERSION_S__ = "Version"
    _h._DMN_VERSION     = "0.9.9"
    _h._DMN_COPYRIGHT__ = "Copyright (C) 2017-2025"
    _h._DMN_AUTHOR      = "Radislav Golubtsov <radicchio@vk.com>"

    -- Constant: The default hostname to look up for.
    _h._DEF_HOSTNAME = "openbsd.org"
return _h

-- vim:set nu et ts=4 sw=4:
