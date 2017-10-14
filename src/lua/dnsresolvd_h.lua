--[[
 * content/dev/misc/dnsresolvd/lua/dnsresolvd_h.lua
 * ============================================================================
 * DNS Resolver Daemon (dnsresolvd). Version 0.1
 * ============================================================================
 * A Luvit (Lua)-boosted daemon for performing DNS lookups.
 * ============================================================================
 * Copyright (C) 2017 Radislav (Radicchio) Golubtsov
 *
 * (See the LICENSE file at the top of the source tree.)
--]]

--[[
 * The helper module for the main daemon module.
 * It aimed at accumulating such pieces like constants, globals, etc.,
 * just like it behaves in its C counterpart. Yep, that's a little funny
 * to make it named like in C, but definitely feasible nevertheless!))
--]]
local __dh = {}
    -- Helper constants.
    __dh._EXIT_FAILURE     =    1 --    Failing exit status.
    __dh._EXIT_SUCCESS     =    0 -- Successful exit status.
    __dh._ONE_SPACE_STRING =  " "
    __dh._COLON_SPACE_SEP  = ": "
    __dh._COMMA_SPACE_SEP  = ", "
    __dh._NEW_LINE         = "\n"

    -- Common error messages and codes.
    __dh._ERR_PREFIX                    = "Error"
    __dh._ERR_PORT_MUST_BE_POSITIVE_INT = ": <port_number> must be "
                                       .. "a positive integer value, "
                                       .. "in the range 1024-49151."
    __dh._ERR_CANNOT_START_SERVER       = ": FATAL: Cannot start server "
    __dh._ERR_SRV_UNKNOWN_REASON        = "for an unknown reason. Exiting..."
    __dh._ERR_SRV_PORT_IS_IN_USE        = "due to the port requested "
                                       .. "is in use. Exiting..."
    __dh._ERR_COULD_NOT_LOOKUP          = "Could not lookup hostname."
    __dh._ERR_EADDRINUSE                = "EADDRINUSE"

    -- Print this error message when there are no any args passed.
    __dh._ERR_MUST_BE_THE_ONLY_ARG_1 = ": There must be exactly one arg passed"
                                    .. ": "
    __dh._ERR_MUST_BE_THE_ONLY_ARG_2 = " args found"

    -- Print this usage info just after any inappropriate input.
    __dh._MSG_USAGE_TEMPLATE_1 = "Usage: "
    __dh._MSG_USAGE_TEMPLATE_2 = " <port_number>"

    -- Syslog-related constants.
    __dh._LOG_DAEMON_EXT      = ".lua"
    __dh._LOG_FACILITY_DAEMON = "daemon"
    __dh._LOG_PRIORITY_ERR    = "err"
    __dh._LOG_PRIORITY_INFO   = "info"

    -- Daemon module events.
    __dh._EVE_ERROR     = "error"
    __dh._EVE_LISTENING = "listening"

    -- Constant: The minimum port number allowed.
    __dh._MIN_PORT = 1024

    -- Constant: The maximum port number allowed.
    __dh._MAX_PORT = 49151

    -- Common notification messages.
    __dh._MSG_SERVER_STARTED_1 = "Server started on port "
    __dh._MSG_SERVER_STARTED_2 = "=== Hit Ctrl+C to terminate it."

    -- HTTP response headers and status codes.
    __dh._HDR_CONTENT_TYPE  = "text/html; charset=UTF-8"
    __dh._HDR_CACHE_CONTROL = "no-cache, no-store, must-revalidate"
    __dh._HDR_EXPIRES       = "Thu, 01 Dec 1994 16:00:00 GMT"
    __dh._HDR_PRAGMA        = "no-cache"
    __dh._RSC_HTTP_200_OK   = 200

    -- Daemon name, version, and copyright banners.
    __dh._DMN_NAME        = "DNS Resolver Daemon (dnsresolvd)"
    __dh._DMN_DESCRIPTION = "Performs DNS lookups for the given hostname "
                         .. "passed in an HTTP request"
    __dh._DMN_VERSION_S__ = "Version"
    __dh._DMN_VERSION     = "0.1"
    __dh._DMN_COPYRIGHT__ = "Copyright (C) 2017"
    __dh._DMN_AUTHOR      = "Radislav Golubtsov <ragolubtsov@my.com>"

    -- Constant: The default hostname to look up for.
    __dh._DEF_HOSTNAME = "openbsd.org"
return __dh

-- vim:set nu:et:ts=4:sw=4:
