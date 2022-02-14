%
% src/erlang/lib/dnsresolvd.erl
% =============================================================================
% DNS Resolver Daemon (dnsresolvd). Version 0.9.9
% =============================================================================
% A daemon that performs DNS lookups for the given hostname
% passed in an HTTP request, with the focus on its implementation
% using various programming languages. (Cowboy-boosted impl.)
% =============================================================================
% Copyright (C) 2017-2022 Radislav (Radicchio) Golubtsov
%
% (See the LICENSE file at the top of the source tree.)
%

%% @doc The main --application-- module of the daemon.
-module(dnsresolvd).

-behaviour(application).

-export([start/2, stop/1]).

-include("dnsresolvd.h").

%% @doc Starts up the daemon.<br />
%%      It has to be the application module callback, but used directly
%%      from the startup script of the daemon.
%%
%% @param Args A list containing the server port number to listen on
%%             as the first element.
%%
%% @returns The tuple containing the PID of the top supervisor process
%%          and the state of the running application (or an empty list).
start(_, Args) ->
    {Port_number, Daemon_name, Log} = Args,

    {ok, _} = application:ensure_all_started(cowboy),

    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", reqhandler, []}
        ]}
    ]),

    % Starting up the plain HTTP listener on <port_number>.
    Ret_ = cowboy:start_clear(http_listener, [{
        port, Port_number
    }], #{
        env => #{dispatch => Dispatch}
    }),

    % Handling errors during start up of the listener.
    if (element(1, Ret_) =:= error) ->
        Ret0 = ?_EXIT_FAILURE,

        if (element(2, Ret_) =:= eaddrinuse) ->
            io:put_chars(standard_error, Daemon_name
                      ++ ?_ERR_CANNOT_START_SERVER
                      ++ ?_ERR_SRV_PORT_IS_IN_USE
                      ++ ?_NEW_LINE ++ ?_NEW_LINE),

            syslog:log(Log, err, Daemon_name
                      ++ ?_ERR_CANNOT_START_SERVER
                      ++ ?_ERR_SRV_PORT_IS_IN_USE
                      ++ ?_NEW_LINE);
           (true                           ) ->
            io:put_chars(standard_error, Daemon_name
                      ++ ?_ERR_CANNOT_START_SERVER
                      ++ ?_ERR_SRV_UNKNOWN_REASON
                      ++ ?_NEW_LINE ++ ?_NEW_LINE),

            syslog:log(Log, err, Daemon_name
                      ++ ?_ERR_CANNOT_START_SERVER
                      ++ ?_ERR_SRV_UNKNOWN_REASON
                      ++ ?_NEW_LINE)
        end,

        cleanups_fixate(Log),

        halt(Ret0);
       (true                      ) ->
        false
    end,

    io:put_chars(?_MSG_SERVER_STARTED_1
              ++ integer_to_list(Port_number) ++ ?_NEW_LINE
              ++ ?_MSG_SERVER_STARTED_2       ++ ?_NEW_LINE),

    syslog:log(Log, info,
                 ?_MSG_SERVER_STARTED_1
              ++ integer_to_list(Port_number) ++ ?_NEW_LINE
              ++ ?_MSG_SERVER_STARTED_2),

    % Starting up the daemon's provided --supervisor-- (optional).
    dnsresolvs:start_link(),

    % Trapping exit signals, i.e. transforming them into {'EXIT'} message.
    process_flag(trap_exit, true),

    % Inspecting the daemon's --application-- process message queue
    % for the incoming {'EXIT'} message until the message received.
    receive
        {'EXIT', _, _} -> []
    end.

% Does nothing. Required to satisfy the --application-- behaviour
%               callback module design only.
stop(_) ->
    ok.

% vim:set nu et ts=4 sw=4:
