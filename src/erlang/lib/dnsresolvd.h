%
% src/erlang/lib/dnsresolvd.h
% =============================================================================
% DNS Resolver Daemon (dnsresolvd). Version 0.1
% =============================================================================
% A daemon that performs DNS lookups for the given hostname
% passed in an HTTP request, with the focus on its implementation
% using various programming languages. (Cowboy-boosted impl.)
% =============================================================================
% Copyright (C) 2017-2018 Radislav (Radicchio) Golubtsov
%
% (See the LICENSE file at the top of the source tree.)
%

%% @doc The helper header file for the daemon.
%%      It aimed at accumulating such pieces like constants, helper functions,
%%      just like it behaves in its C counterpart. Yep, that's a little funny
%%      to make it named like in C, but definitely feasible nevertheless!))
%% @end

% Helper constants.
-define(_EXIT_FAILURE,          1). %    Failing exit status.
-define(_EXIT_SUCCESS,          0). % Successful exit status.
-define(_EMPTY_STRING,         "").
-define(_COMMA_SPACE_SEP,    ", ").
-define(_NEW_LINE,           "\n").
-define(_ONE_SPACE_STRING,    " ").
-define(_PRINT_BANNER_OPT,   "-V").
-define(_DBG_PREF,         "==> ").

% Common error messages.
-define(_ERR_PORT_MUST_BE_POSITIVE_INT, ": <port_number> must be "
                                     ++ "a positive integer value, "
                                     ++ "in the range 1024-49151.").

% Print this error message when there are no any args passed.
-define(_ERR_MUST_BE_ONE_TWO_ARGS_1, ": There must be one or two args "
                                  ++ "passed: ").
-define(_ERR_MUST_BE_ONE_TWO_ARGS_2, " args found").

% Print this usage info just after any inappropriate input.
-define(_MSG_USAGE_TEMPLATE_1, "Usage: ").
-define(_MSG_USAGE_TEMPLATE_2, " <port_number> [-V]").

%% Constant: The minimum port number allowed.
-define(_MIN_PORT, 1024).

%% Constant: The maximum port number allowed.
-define(_MAX_PORT, 49151).

% Daemon name, version, and copyright banners.
-define(_DMN_NAME,        "DNS Resolver Daemon (dnsresolvd)").
-define(_DMN_DESCRIPTION, "Performs DNS lookups for the given "
                       ++ "hostname passed in an HTTP request").
-define(_DMN_VERSION_S__, "Version").
-define(_DMN_VERSION,     "0.1").
-define(_DMN_COPYRIGHT__, "Copyright (C) 2017-2018").
-define(_DMN_AUTHOR,      "Radislav Golubtsov <ragolubtsov@my.com>").

% Helper function. Makes final buffer cleanups, closes streams, etc.
cleanups_fixate(Log) ->
    % Closing the system logger.
    if (Log =/= nil) ->
        syslog:close(Log),
        syslog:stop();
       (true       ) ->
        false
    end.

% vim:set nu et ts=4 sw=4: