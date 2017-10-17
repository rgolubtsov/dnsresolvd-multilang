--[[
 * src/lua/dnsresolvh.lua
 * ============================================================================
 * DNS Resolver Daemon (dnsresolvd). Version 0.1
 * ============================================================================
 * A daemon that performs DNS lookups for the given hostname
 * passed in an HTTP request, with the focus on its implementation
 * using various programming languages. (Luvit-boosted impl.)
 * ============================================================================
 * Copyright (C) 2017 Radislav (Radicchio) Golubtsov
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

    -- Common error messages and codes.
    _h._ERR_PREFIX                    = "Error"
    _h._ERR_PORT_MUST_BE_POSITIVE_INT = ": <port_number> must be "
                                     .. "a positive integer value, "
                                     .. "in the range 1024-49151."
    _h._ERR_CANNOT_START_SERVER       = ": FATAL: Cannot start server "
    _h._ERR_SRV_UNKNOWN_REASON        = "for an unknown reason. Exiting..."
    _h._ERR_SRV_PORT_IS_IN_USE        = "due to the port requested "
                                     .. "is in use. Exiting..."
    _h._ERR_COULD_NOT_LOOKUP          = "Could not lookup hostname."
    _h._ERR_EADDRINUSE                = "EADDRINUSE"

    -- Print this error message when there are no any args passed.
    _h._ERR_MUST_BE_THE_ONLY_ARG_1 = ": There must be exactly one arg passed: "
    _h._ERR_MUST_BE_THE_ONLY_ARG_2 = " args found"

    -- Print this usage info just after any inappropriate input.
    _h._MSG_USAGE_TEMPLATE_1 = "Usage: "
    _h._MSG_USAGE_TEMPLATE_2 = " <port_number>"

    -- Syslog-related constants.
    _h._LOG_DAEMON_EXT      = ".lua"
    _h._LOG_FACILITY_DAEMON = "daemon"
    _h._LOG_PRIORITY_ERR    = "err"
    _h._LOG_PRIORITY_INFO   = "info"

    -- Daemon module events.
    _h._EVE_ERROR     = "error"
    _h._EVE_LISTENING = "listening"

    -- Constant: The minimum port number allowed.
    _h._MIN_PORT = 1024

    -- Constant: The maximum port number allowed.
    _h._MAX_PORT = 49151

    -- Common notification messages.
    _h._MSG_SERVER_STARTED_1 = "Server started on port "
    _h._MSG_SERVER_STARTED_2 = "=== Hit Ctrl+C to terminate it."

    -- HTTP response headers and status codes.
    _h._HDR_CONTENT_TYPE  = "text/html; charset=UTF-8"
    _h._HDR_CACHE_CONTROL = "no-cache, no-store, must-revalidate"
    _h._HDR_EXPIRES       = "Thu, 01 Dec 1994 16:00:00 GMT"
    _h._HDR_PRAGMA        = "no-cache"
    _h._RSC_HTTP_200_OK   = 200

    -- Daemon name, version, and copyright banners.
    _h._DMN_NAME        = "DNS Resolver Daemon (dnsresolvd)"
    _h._DMN_DESCRIPTION = "Performs DNS lookups for the given hostname "
                       .. "passed in an HTTP request"
    _h._DMN_VERSION_S__ = "Version"
    _h._DMN_VERSION     = "0.1"
    _h._DMN_COPYRIGHT__ = "Copyright (C) 2017"
    _h._DMN_AUTHOR      = "Radislav Golubtsov <ragolubtsov@my.com>"

    -- Constant: The default hostname to look up for.
    _h._DEF_HOSTNAME = "openbsd.org"
return _h

-- vim:set nu:et:ts=4:sw=4:
