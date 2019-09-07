#
# src/bash/dnsresolvd.h
# =============================================================================
# DNS Resolver Daemon (dnsresolvd). Version 0.1
# =============================================================================
# A daemon that performs DNS lookups for the given hostname
# passed in an HTTP request, with the focus on its implementation
# using various programming languages. (Netcat-boosted impl.)
# =============================================================================
# Copyright (C) 2017-2019 Radislav (Radicchio) Golubtsov
#
# (See the LICENSE file at the top of the source tree.)
#

# The helper script ("header file") for the daemon.
# It aimed at accumulating such pieces like constants, helper functions,
# just like it behaves in its C counterpart. Yep, that's a little funny
# to make it named like in C, but definitely feasible nevertheless!))

# Helper constants.
declare -r _EXIT_FAILURE=1 #    Failing exit status.
declare -r _EXIT_SUCCESS=0 # Successful exit status.
declare -r _EMPTY_STRING=""
declare -r _COMMA=","
declare -r _NEW_LINE="\n"
declare -r _PRINT_BANNER_OPT="-V"

# Common error messages.
declare -r _ERR_PORT_MUST_BE_POSITIVE_INT=": <port_number> must be "`
                                         `"a positive integer value, "`
                                         `"in the range 1024-49151."
declare -r _ERR_CANNOT_START_SERVER=": FATAL: Cannot start server "
declare -r _ERR_SRV_PORT_IS_IN_USE="due to the port requested is in use. "`
                                  `"Exiting..."
declare -r _ERR_ADDR_ALREADY_IN_USE="Address already in use"

# Print this error message when there are no any args passed.
declare -r _ERR_MUST_BE_ONE_TWO_ARGS_1=": There must be one or two args "`
                                      `"passed: "
declare -r _ERR_MUST_BE_ONE_TWO_ARGS_2=" args found"

# Print this usage info just after any inappropriate input.
declare -r _MSG_USAGE_TEMPLATE_1="Usage: "
declare -r _MSG_USAGE_TEMPLATE_2=" <port_number> [-V]"

# Constant: The minimum port number allowed.
declare -r _MIN_PORT=1024

# Constant: The maximum port number allowed.
declare -r _MAX_PORT=49151

# Common notification messages.
declare -r _MSG_SERVER_STARTED_1="Server started on port "
declare -r _MSG_SERVER_STARTED_2="=== Hit Ctrl+C to terminate it."

# HTTP request params.
declare -r _PRM_FMT_HTML="html"
declare -r _PRM_FMT_JSON="json"

# HTTP response headers and status codes.
declare -r _HDR_CONTENT_TYPE_N="Content-Type: "
declare -r _HDR_CONTENT_TYPE_V_HTML="text/html; charset=UTF-8"
declare -r _HDR_CONTENT_TYPE_V_JSON="application/json"
declare -r _HDR_CONTENT_LENGTH_N="Content-Length: "
declare -r _HDR_DATE_N="Date: "
declare -r _HDR_CACHE_CONTROL_N="Cache-Control: "
declare -r _HDR_CACHE_CONTROL_V="no-cache, no-store, must-revalidate"
declare -r _HDR_EXPIRES_N="Expires: "
declare -r _HDR_EXPIRES_V="Thu, 01 Dec 1994 16:00:00 GMT"
declare -r _HDR_PRAGMA_N="Pragma: "
declare -r _HDR_PRAGMA_V="no-cache"
declare -r _HDR_SERVER_N="Server: "
declare -r _RSC_HTTP_200_OK="HTTP/1.1 200 OK"

# Response data names.
declare -r _DAT_HOSTNAME_N="hostname"
declare -r _DAT_ADDRESS_N="address"
declare -r _DAT_VERSION_N="version"
declare -r _DAT_VERSION_V="IPv"

# Daemon name, version, and copyright banners.
declare -r _DMN_NAME="DNS Resolver Daemon (dnsresolvd)"
declare -r _DMN_DESCRIPTION="Performs DNS lookups for the given hostname "`
                           `"passed in an HTTP request"
declare -r _DMN_VERSION_S__="Version"
declare -r _DMN_VERSION="0.1"
declare -r _DMN_COPYRIGHT__="Copyright (C) 2017-2019"
declare -r _DMN_AUTHOR="Radislav Golubtsov <ragolubtsov@my.com>"

# Constant: The default hostname to look up for.
declare -r _DEF_HOSTNAME="openbsd.org"

##
# Adds headers to the response.
#
# @param fmt                  The response format selector.
# @param HDR_CONTENT_LENGTH_V The length of the response buffer.
#
add_response_headers() {
    fmt=$1; HDR_CONTENT_LENGTH_V=$2

      if [ ${fmt} == ${_PRM_FMT_HTML} ]; then
        HDR_CONTENT_TYPE_V=${_HDR_CONTENT_TYPE_V_HTML}
    elif [ ${fmt} == ${_PRM_FMT_JSON} ]; then
        HDR_CONTENT_TYPE_V=${_HDR_CONTENT_TYPE_V_JSON}
    fi

    HDR_DATE_V=`date -Ru | sed -e "s/\+0000/GMT/g"`

    # Returning the compound string containing response headers.
    echo "${_HDR_CONTENT_TYPE_N}${HDR_CONTENT_TYPE_V}${_NEW_LINE}"`
        `"${_HDR_CONTENT_LENGTH_N}${HDR_CONTENT_LENGTH_V}${_NEW_LINE}"`
        `"${_HDR_DATE_N}${HDR_DATE_V}${_NEW_LINE}"`
        `"${_HDR_CACHE_CONTROL_N}${_HDR_CACHE_CONTROL_V}${_NEW_LINE}"`
        `"${_HDR_EXPIRES_N}${_HDR_EXPIRES_V}${_NEW_LINE}"`
        `"${_HDR_PRAGMA_N}${_HDR_PRAGMA_V}${_NEW_LINE}"`
        `"${_HDR_SERVER_N}${_DMN_NAME}${_NEW_LINE}"
}

# Helper function. Draws a horizontal separator banner.
_separator_draw() { banner_text=$1 # <== C-ished "(const char *banner_text)":-)
    i=${#banner_text} # <== C-ished "unsigned char i = strlen(banner_text);":-)

    while ((i > 0)); do echo -n '='; let i--; done; echo
}

# vim:set nu et ts=4 sw=4:
